defmodule MonoCounterTest do
  use ExUnit.Case, async: true

  setup_all do
    Application.start(:crdtex)
    File.mkdir("data")

    on_exit fn() ->
      Application.stop(:crdtex)
    end

    :ok
  end

  setup context do
    Application.put_env(:gen_serverring, :name, context.name)
    Application.put_env(:gen_serverring, :callback, context.callback)
    Application.start(:gen_serverring)

    on_exit fn -> Application.stop(:gen_serverring) end

    :ok
  end

  defp update_state(state, action) do
    {mega, second, micro} = :erlang.now()
    dot = {node(), (mega * 1_000_000 + second) * 1_000_000 + micro}
    {:ok, state} = Crdtex.update(state, dot, action)
    state
  end

  defp inc(server), do: GenServerring.cast(server, :inc)

  defp dec(server), do: GenServerring.cast(server, :dec)

  defp get(server), do: GenServerring.call(server, :get)

  # callbacks for tests
  def init([]), do: {:ok, Crdtex.Counter.new}

  def handle_call(:get,_,counter), do: {:reply,Crdtex.value(counter),counter}

  def handle_cast(:inc,counter), do: {:noreply,update_state(counter,:increment)}
  def handle_cast(:dec,counter), do: {:noreply,update_state(counter,:decrement)}

  def handle_info(msg, counter) do
    case Enum.member?(Process.registered(), :handle_info_ring) do
      true -> send(:tester_handle_info, {:received, msg})
      false -> :ok
    end
    {:noreply, counter}
  end

  def handle_state_change(counter) do
    case Enum.member?(Process.registered(), :handle_state_change_ring) do
      true ->
        send(:tester_handle_state_change,
          {:state_changed, Crdtex.value(counter)})
      false -> :ok
    end
  end

  def handle_ring_change({up_set, _, _}) do
    case Enum.member?(Process.registered(), :handle_ring_change_ring) do
      true -> send(:tester_handle_ring_change, {:ring_changed, up_set})
      false -> :ok
    end
  end

  @moduletag callback: __MODULE__
  
  @tag name: :call_cast_ring
  # counter on one node only, this is just checking that the interaction between
  # the callback and GenServerring is working as expected.
  test "counter on one node, casts and calls", context do
    name = context.name

    assert get(name) == 0
    assert inc(name) == :ok
    assert get(name) == 1
    dec(name)
    assert get(name) == 0
  end

  @tag name: :handle_info_ring
  test "counter on one node, handle_info", context do
    name = context.name

    true = Process.register(self(), :tester_handle_info)
    send(name, :weird_message)
    assert_receive({:received, :weird_message})
    true = Process.unregister(:tester_handle_info)
  end

  @tag name: :handle_state_change_ring
  test "counter on one node, handle_state_change", context do
    # testing handle_state_change with only one node, I have to cheat...
    # handle_state_change is trigerred if the payload change during a reconcile,
    # let's simulate that
    name = context.name

    {:ok, counter} = __MODULE__.init([])
    {:ok, ring} = GenServerring.init({counter, __MODULE__})
    # ring and the state of the GenServerring are identical, let's modify ring
    counter = update_state(counter, :increment)
    gossip =
      %{node_set: ring.node_set, payload: counter, from_node: [],
        forced_down: ring.forced_down}
    true = Process.register(self(), :tester_handle_state_change)
    GenServerring.cast(name, {:reconcile, gossip})

    assert_receive({:state_changed, 1})
    true = Process.unregister(:tester_handle_state_change)
  end

  @tag name: :handle_ring_change_ring
  test "change of up_set on a single node", context do
    name = context.name
    Process.register(self(), :tester_handle_ring_change)
    first_up_set = GenServerring.up(name)

    # adding a non existant node should trigger 2 ring_changes, the fantasy node
    # will be added to up_set and immediately after it will be detected as down
    # require that ct is launched with --sname

    GenServerring.add_node(name, :fantasy@localhost)
    assert_receive({:ring_changed, second_up_set})
    assert_receive({:ring_changed, ^first_up_set})

    assert Enum.member?(second_up_set, :fantasy@localhost)
  end
end
