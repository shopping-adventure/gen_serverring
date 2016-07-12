defmodule GenServerringCounterTest do
  use ExUnit.Case
  doctest GenServerring

  defp update_state(state, action) do
    {mega, second, micro} = :erlang.now()
    dot = {node(), (mega * 1_000_000 + second) * 1_000_000 + micro}
    {:ok, state} = Crdtex.update(state, dot, action)
    state
  end

  defp setup_env(name) do
    Application.put_env(:gen_serverring, :name, name)
    Application.put_env(:gen_serverring, :callback, __MODULE__)
    :ok = Application.stop(:gen_serverring)
    :ok = Application.start(:gen_serverring)
  end

  defp inc(server), do: GenServerring.cast(server, :inc)

  defp dec(server), do: GenServerring.cast(server, :dec)

  defp get(server), do: GenServerring.call(server, :get)

  # callbacks for tests
  def init([]), do: {:ok, Crdtex.Counter.new}

  def handle_call(:get, _, counter), do: {:reply, Crdtex.value(counter), counter}

  def handle_cast(:inc,counter), do: {:noreply,update_state(counter,:increment)}
  def handle_cast(:dec,counter), do: {:noreply,update_state(counter,:decrement)}

  def handle_info(msg, counter) do
    send(:tester_handle_info, {:received, msg})
    {:noreply, counter}
  end

  def handle_state_change(counter) do
    send(:tester_handle_state_change, {:state_changed, Crdtex.value(counter)})
  end

  def handle_ring_change(up_set) do
    send(:tester_handle_ring_change, {:ring_changed, up_set})
  end

  # counter on one node only, this is just checking that the interaction between
  # the callback and GenServerring is working as expected.
  test "counter on one node, casts and calls" do
    name = :gen_serverring_counter_test
    setup_env(name)

    assert get(name) == 0
    assert inc(name) == :ok
    assert get(name) == 1
    dec(name)
    assert get(name) == 0
  end

  test "counter on one node, handle_info" do
    name = :gen_serverring_counter_test
    setup_env(name)

    true = Process.register(self(), :tester_handle_info)
    send(name, :weird_message)
    assert_receive({:received, :weird_message})
    true = Process.unregister(:tester_handle_info)
  end

  test "counter on one node, handle_state_change" do
    # testing handle_state_change with only one node, I have to cheat...
    # handle_state_change is trigerred if the payload change during a reconcile,
    # let's simulate that
    name = :gen_serverring_counter_test
    setup_env(name)

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

end
