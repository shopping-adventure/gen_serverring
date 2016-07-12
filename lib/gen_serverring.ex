defmodule GenServerring do
  use GenServer
  require Crdtex.Set

  defstruct(
    node_set: Crdtex.Set.new,
    up_set: MapSet.new,
    forced_down: Crdtex.Set.new,
    payload: nil,
    counter: 0,
    callback: nil)

  defmacro __using__(_), do: []

  def start_link({name, callback}) do
    {:ok, payload} = callback.init([])
    GenServer.start_link(__MODULE__, {payload, callback}, [{:name, name}])
  end

  def init({payload, callback}) do
    :erlang.send_after(1_000, self(), :send_gossip)
    case File.read(ring_path) do
      {:ok, bin} ->
        set = :erlang.binary_to_term(bin)
        monitor(get_set(set))
        # should we call callback.handle_ring_change in such a situation ?
        {:ok, gen_ring(set, MapSet.new(get_set(set)), payload, 0, callback)}
      _ ->
        set = Crdtex.Set.new
        {:ok, set} = add(set, {node(), 1}, node())
        {:ok, gen_ring(set, MapSet.new([node()]), payload, 1, callback)}
    end
  end

  # payload management, mimicking a GenServer
  def call(server, action), do: GenServer.call(server, action)

  def cast(server, action), do: GenServer.cast(server, action)

  def reply(client, term), do: GenServer.reply(client, term)

  # cluster_management
  # we have to stop all nodes
  def stop({name, n} = server, reason \\ :normal, timeout \\ :infinity) do
    {:ok, ring} = get_ring(server)
    others = ring.up_set |> MapSet.delete(n)
    Enum.each(others, fn(n) -> GenServer.stop({name, n}, reason, timeout) end)
    Genserver.stop(server, reason, timeout)
  end

  def add_node(server, node) when is_binary(node) do
    add_node(server, :"#{node}")
  end
  def add_node(server, node) when is_atom(node) do
    GenServer.cast(server, {:add_node, node})
  end

  def del_node(server, node) when is_binary(node) do
    del_node(server, :"#{node}")
  end
  def del_node(server, node) when is_atom(node) do
    GenServer.cast(server, {:del_node, node})
  end

  # put the specified node in a specific kind of state: it can't be used by the
  # cluster, so it will not received gossip and gossip received from it
  # will be silently ignored ignored but the node can still be up and will
  # continue to be monitored as long as needed. Even if a node get down, it will
  # be removed from up_set but kept in the forced_down state until explicitly
  # removed from there.
  def force_down(server, n), do:  GenServer.call(server, {:forced_down, n})

  # remove a node of the forced_down state
  def unforce_down(server, n), do:  GenServer.call(server, {:unforced_down, n})

  def all(server), do: GenServer.call(server, :get_all)
  def up(server), do: GenServer.call(server, :get_up)

  # classic GenServer callbacks
  def handle_call(:get_all, _, ring), do: {:reply, get_set(ring.node_set), ring}
  def handle_call(:get_up, _, ring) do
    {:reply, MapSet.to_list(up_nodes(ring)), ring}
  end
  def handle_call(:get_ring, _, ring), do: {:reply, {:ok, ring}, ring}
  def handle_call({:forced_down, n}, _, ring) do
    counter = ring.counter + 1
    {:ok, forced_down} = add(ring.forced_down, {node(), counter}, n)
    {:reply, get_set(forced_down),
      %{ring | forced_down: forced_down, counter: counter}}
  end
  def handle_call({:unforced_down, n}, _, ring) do
    counter = ring.counter + 1
    {:ok, forced_down} = delete(ring.forced_down, {node(), counter}, n)
    up_set = ring.up_set
    up_set =
      case MapSet.member(up_set, n) do
        true -> up_set
        false ->
          Node.monitor(n, true)
          MapSet.put(up_set, n)
      end
    {:reply, MapSet.to_list(forced_down),
      %{ring | forced_down: forced_down, up_set: up_set, counter: counter}}
  end
  def handle_call(other, from, ring) do
    payload = ring.payload
    up_load = fn(load) -> update_payload(ring, load) end
    case ring.callback.handle_call(other, from, payload) do
      {:reply, reply, new} -> {:reply, reply, up_load.(new)}
      {:reply, reply, new, timeout} -> {:reply, reply, up_load.(new), timeout}
      {:noreply, new} -> {:noreply, up_load.(new)}
      {:noreply, new, timeout} -> {:noreply, up_load.(new), timeout}
      {:stop, reason, reply, new} -> {:stop, reason, reply, up_load.(new)}
      {:stop, reason, new} -> {:stop, reason, up_load.(new)}
    end
  end

  def handle_cast({:reconcile, gossip}, ring) do
    from_node = gossip.from_node
    up_set_reconcile(from_node, ring.up_set)
    ring_reconcile(from_node, gossip, ring)
  end
  def handle_cast({:add_node, n}, ring) do
    case contain?(ring.node_set, n) do
      true ->
        {:noreply, ring}
      false ->
        new_up_set = MapSet.put(ring.up_set, n)
        {:ok, new_node_set} = add(ring.node_set, {node(), ring.counter + 1}, n)
        {ring, _} =
          update_ring(%GenServerring{ring | up_set: new_up_set},
           %{node_set: new_node_set, payload: ring.payload, from_node: []})
        {:noreply, ring}
    end
  end
  def handle_cast({:del_node, n}, ring) do
    case contain?(ring.node_set, n) do
      false ->
        {:noreply, ring}
      true ->
        new_up_set = MapSet.delete(ring.up_set, n)
        {:ok, new_node_set} =
          delete(ring.node_set, {node(), ring.counter + 1}, n)
        {ring, _} =
         update_ring(%GenServerring{ring | up_set: new_up_set},
           %{node_set: new_node_set, payload: ring.payload, from_node: []})
        {:noreply, ring}
    end
  end
  def handle_cast(other, ring) do
    payload = ring.payload
    up_load = fn(load) -> update_payload(ring, load) end
    case ring.callback.handle_cast(other, payload) do
      {:noreply, new} -> {:noreply, up_load.(new)}
      {:noreply, new, timeout} -> {:noreply, up_load.(new), timeout}
      {:stop, reason, new} -> {:stop, reason, up_load.(new)}
    end
  end

  def handle_info(:send_gossip, %GenServerring{node_set: node_set} = ring) do
    :erlang.send_after(1_000, self(), :send_gossip)
    if not contain?(node_set, node()) do
      :erlang.send_after(5_000, self(), :halt_node)
    end
    case up_nodes(ring) |> MapSet.delete(node()) |> MapSet.to_list do
      [] -> {:noreply, ring}
      active_nodes ->
        {:registered_name, name} = Process.info(self(), :registered_name)
        random_node =
          Enum.at(active_nodes, :random.uniform(length(active_nodes)) - 1)
        GenServer.cast(
          {name, random_node},
          {:reconcile,
            %{node_set: node_set,
              forced_down: ring.forced_down,
              payload: ring.payload,
              from_node: [node()]}})
      {:noreply, ring}
    end
  end
  def handle_info({:nodedown, n}, %GenServerring{up_set: up_set} = ring) do
    new_up_set =
      case MapSet.member(up_set, n) do
        true ->
          set = MapSet.delete(up_set, n)
          ring.callback.handle_ring_change(MapSet.to_list(set))
          set
        false -> up_set
      end
    {:noreply, %{ring | up_set: new_up_set}}
  end
  def handle_info(:halt_node, s) do
    File.rm(ring_path)
    :init.stop()
    {:noreply, s}
  end
  def handle_info(other, ring) do
    up_load = fn(load) -> update_payload(ring, load) end
    case ring.callback.handle_info(other, ring.payload) do
      {:noreply, new} -> {:noreply, up_load.(new)}
      {:noreply, new, timeout} -> {:noreply, up_load.(new), timeout}
      {:stop, reason, new} -> {:stop, reason, up_load.(new)}
    end
  end

  # small utilities functions
  defp value(payload), do: Crdtex.value(payload)

  defp get_set(set), do: Crdtex.value(set)

  defp contain?(set, e), do: Crdtex.value(set, {:contains, e})

  defp add(set, actor, e), do: Crdtex.update(set, actor, {:add, e})

  defp delete(set, actor, e), do: Crdtex.update(set, actor, {:remove, e})

  defp merge(nil, crdt), do: crdt
  defp merge(crdt, crdt), do: crdt
  defp merge(crdt1, crdt2), do: Crdtex.merge(crdt1, crdt2)

  defp get_ring(server), do: GenServer.call(server, :get_ring)

  defp up_nodes(ring) do
    MapSet.difference(ring.up_set, MapSet.new(get_set(ring.forced_down)))
  end

  defp monitor(list) do
    list = List.delete(list, node())
    Enum.each(list, fn(n) -> Node.monitor(n, :true) end)
  end

  defp gen_ring(set, up, payload, counter, callback) do
    gen_ring(set, up, payload, counter, callback, Crdtex.Set.new)
  end
  defp gen_ring(set, up, payload, counter, callback, forced_down) do
    %GenServerring{node_set: set, up_set: up, forced_down: forced_down,
      payload: payload, counter: counter, callback: callback}
  end

  defp update_payload(ring, payload) do
    {ring, _} =
      update_ring(ring,
        %{node_set: ring.node_set, payload: payload, from_node: [],
          forced_down: ring.forced_down})
    ring
  end

  defp update_ring(ring, changes) do
    merged_node_set = merge(ring.node_set, changes.node_set)
    merged_payload = merge(ring.payload, changes.payload)
    merged_forced_down = merge(ring.forced_down, changes.forced_down)
    updated_counter = update_counter(merged_node_set, merged_payload, ring)

    up_set =
      notify_up_set(get_set(ring.node_set), get_set(merged_node_set),
        MapSet.to_list(ring.up_set) ++ changes.from_node, ring.callback)
    notify_node_set(ring.node_set, merged_node_set, changes.node_set)

    old_payload = ring.payload
    ring =
      gen_ring(merged_node_set, up_set, merged_payload, updated_counter,
        ring.callback, merged_forced_down)
    {ring, notify_payload(value(old_payload), value(changes.payload), ring)}
  end

  defp update_counter(merged_node_set, merged_payload, old_ring) do
    old_nodes = old_ring.node_set
    old_payload = old_ring.payload
    case {merged_node_set, merged_payload} do
      {^old_nodes, ^old_payload} -> old_ring.counter
      _ -> old_ring.counter + 1
    end
  end

  defp notify_up_set(set, set, old_up, _), do: MapSet.new(old_up)
  defp notify_up_set(old_set, merged_set, old_up, callback) do
    new_up = MapSet.difference(MapSet.new(merged_set), MapSet.new(old_set))
    Enum.each(new_up, fn(n) -> Node.monitor(n, :true) end)
    new_set = old_up ++ MapSet.to_list(new_up)
    GenEvent.notify(GenServerring.Events, {:new_up_set, old_up, new_set})
    callback.handle_ring_set(new_set)
    MapSet.new(new_set)
  end

  defp notify_node_set(set, set, _), do: :nothingtodo
  defp notify_node_set(old_set, _, new_set) do
    GenEvent.notify(GenServerring.Events, {:new_node_set, get_set(old_set), get_set(new_set)})
      File.write!(ring_path, new_set |> :erlang.term_to_binary)
  end

  defp notify_payload(payload, payload, _), do: :no_payload_change
  defp notify_payload(_, _, ring), do: ring.payload

  defp ring_path do
    "#{Application.get_env(:gen_serverring, :data_dir, "./data")}/ring"
  end

  defp up_set_reconcile([], _), do: :nothingtodo
  defp up_set_reconcile([n], up_set) do
    case MapSet.member?(up_set, n) do
      true -> :nothingtodo
      false -> Node.monitor(n, true)
    end
  end

  defp ring_reconcile([n], gossip, ring) do
    case contain?(ring.forced_down, n) do
      true -> {:noreply, ring} # must ignore gossips from forced_down nodes
      false -> ring_reconcile([], gossip, ring)
    end
  end
  defp ring_reconcile([], gossip, ring) do
    {ring, payload} = update_ring(ring, gossip)
    case payload do
      :no_payload_change -> :nothingtodo
      _ -> ring.callback.handle_state_change(payload)
    end
    {:noreply, ring}
  end
end

defmodule GenServerring.App do
  use Application

  def start(type, []) do
    name = Application.get_env(:gen_serverring, :name, :demo_ring)
    callback = Application.get_env(:gen_serverring, :callback, Demo)
    start(type, [{name, callback}])
  end
  def start(_type, args) do
    Supervisor.start_link(GenServerring.App.Sup, args)
  end

  defmodule Sup do
    use Supervisor
    def init(arg) do
      children =
        [worker(:gen_event, [{:local, GenServerring.Events}], id: GenServerring.Events),
         worker(GenServerring, arg)]
      supervise(children, strategy: :one_for_one)
    end
  end
end

