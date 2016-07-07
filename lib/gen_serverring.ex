defmodule GenServerring do
  use GenServer
  require Crdtex.Set

  defstruct node_set: Crdtex.Set.new, up_set: MapSet.new, payload: nil, counter: 0, callback: nil

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

  def all(server) , do: GenServer.call(server, :get_all)
  def up(server), do: GenServer.call(server, :get_up)

  # classic GenServer callbacks
  def handle_call(:get_all, _, ring) do
    {:reply, get_set(ring.node_set), ring}
  end
  def handle_call(:get_up, _, ring) do
    {:reply, MapSet.to_list(ring.up_set), ring}
  end
  def handle_call(:get_ring, _, ring) do
    {:reply, {:ok, ring}, ring}
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
    case gossip.from_node do
      [] -> :nothingtodo
      [n] ->
        case MapSet.member?(ring.up_set, n) do
          false -> :nothingtodo
          true ->
            # TODO add this info in the gossip to speedup the convergence of UPs
            Node.monitor(n, true)
        end
    end
    {:noreply, update_ring(ring, gossip)}
  end
  def handle_cast({:add_node, n}, ring) do
    case contain?(ring.node_set, n) do
      true ->
        {:noreply, ring}
      false ->
        new_up_set = MapSet.put(ring.up_set, n)
        {:ok, new_node_set} = add(ring.node_set, {node(), ring.counter + 1}, n)
        {:noreply,
         update_ring(%GenServerring{ring | up_set: new_up_set},
           %{node_set: new_node_set, payload: ring.payload, from_node: []})}
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
        {:noreply,
         update_ring(%GenServerring{ring | up_set: new_up_set},
           %{node_set: new_node_set, payload: ring.payload, from_node: []})}
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
    case ring.up_set |> MapSet.delete(node()) |> MapSet.to_list do
      [] -> {:noreply, ring}
      active_nodes ->
        {:registered_name, name} = Process.info(self(), :registered_name)
        random_node =
          Enum.at(active_nodes, :random.uniform(length(active_nodes)) - 1)
        GenServer.cast({name, random_node},
          {:reconcile, %{node_set: node_set, payload: ring.payload, from_node: [node()]}})
      {:noreply, ring}
    end
  end
  def handle_info({:nodedown, n}, %GenServerring{up_set: up_set} = ring) do
    {:noreply, %{ring | up_set: MapSet.delete(up_set, n)}}
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
  defp monitor(list) do
    list = List.delete(list, node())
    Enum.each(list, fn(n) -> Node.monitor(n, :true) end)
  end

  defp gen_ring(set, up, payload, counter, callback) do
    %GenServerring{node_set: set, up_set: up, payload: payload,
      counter: counter, callback: callback}
  end

  defp get_ring(server), do: GenServer.call(server, :get_ring)

  defp update_payload(ring, payload) do
    update_ring(ring,
      %{node_set: ring.node_set, payload: payload, from_node: []})
  end

  defp update_ring(ring, changes) do
    merged_node_set = merge(ring.node_set, changes.node_set)
    merged_payload = merge(ring.payload, changes.payload)
    updated_counter = update_counter(merged_node_set, merged_payload, ring)

    up_set = notify_up_set(get_set(ring.node_set), get_set(merged_node_set),
    MapSet.to_list(ring.up_set) ++ changes.from_node)
    notify_node_set(ring.node_set, merged_node_set, changes.node_set)

    ring =
      gen_ring(merged_node_set, up_set, merged_payload, updated_counter,
        ring.callback)
    notify_payload(value(ring.payload), value(changes.payload), ring)
    ring
  end

  defp update_counter(merged_node_set, merged_payload, old_ring) do
    old_nodes = old_ring.node_set
    old_payload = old_ring.payload
    case {merged_node_set, merged_payload} do
      {^old_nodes, ^old_payload} -> old_ring.counter
      _ -> old_ring.counter + 1
    end
  end

  defp notify_up_set(set, set, old_up) do
    MapSet.new(old_up)
  end
  defp notify_up_set(old_set, merged_set, old_up) do
    new_up = MapSet.difference(MapSet.new(merged_set), MapSet.new(old_set))
    Enum.each(new_up, fn(n) -> Node.monitor(n, :true) end)
    new_set = old_up ++ MapSet.to_list(new_up)
    GenEvent.notify(GenServerring.Events, {:new_up_set, old_up, new_set})
    MapSet.new(new_set)
  end

  defp notify_node_set(set, set, _) do
    :nothingtodo
  end
  defp notify_node_set(old_set, _, new_set) do
    GenEvent.notify(GenServerring.Events, {:new_node_set, get_set(old_set), get_set(new_set)})
      File.write!(ring_path, new_set |> :erlang.term_to_binary)
  end

  defp notify_payload(payload, payload, _) do
    :nothingtodo
  end
  defp notify_payload(_, _, ring) do
    ring.callback.handle_change(ring.payload)
  end

  defp ring_path do
    "#{Application.get_env(:gen_serverring, :data_dir, "./data")}/ring"
  end

  defp value(payload), do: Crdtex.value(payload)

  defp get_set(set), do: Crdtex.value(set)

  defp contain?(set, e), do: Crdtex.value(set, {:contains, e})

  defp add(set, actor, e), do: Crdtex.update(set, actor, {:add, e})

  defp delete(set, actor, e), do: Crdtex.update(set, actor, {:remove, e})

  defp merge(nil, crdt), do: crdt
  defp merge(crdt, crdt), do: crdt
  defp merge(crdt1, crdt2), do: Crdtex.merge(crdt1, crdt2)
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
