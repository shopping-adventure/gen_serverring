defmodule MonitorTest do
  use ExUnit.Case, async: true

  setup context do
    case File.dir?("./data") do
      true -> File.rm_rf("./data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    :ok = Application.start(:crdtex)
    {:ok, _} = GenServerring.start_link({context.name, context.callback})

    on_exit fn() ->
      Application.stop(:gen_serverring)
      Application.stop(:crdtex)
    end

    :ok
  end

  def init([]), do: Demo.init([])

  def handle_call(msg, from, state) do
    :ct.log(:info, 75, 'handle_call ~p~n', [msg], [])
    Demo.handle_call(msg, from, state)
  end

  def handle_cast(msg, state) do
    :ct.log(:info, 75, 'handle_cast ~p~n', [msg], [])
    Demo.handle_cast(msg, state)
  end

  def handle_info(msg, state) do
    :ct.log(:info, 75, 'handle_info ~p~n', [msg], [])
    {:noreply, state}
  end

  def handle_state_change(change) do
    :ct.log(:info, 75, 'handle_state ~p~n', [change], [])
    :ok
  end

  def handle_ring_change(change) do
    :ct.log(:info, 75, 'handle_ring ~p~n', [change], [])
    :ok
  end

  @tag name: :ct_test_ring
  @tag callback: __MODULE__
  test "monitoring ct_test_ring", context do
    ring = context.name
    assert length(GenServerring.all(ring)) == 1
    assert length(GenServerring.up(ring)) == 1

    # wait to be part of the ring
    :ct.sleep(13_000)
    all = GenServerring.all(ring)
    up = GenServerring.up(ring)
    assert length(all) == 3
    assert length(up) == 3

    # n4 added and enough gossips should have occured
    :ct.sleep(6_000) # 19 sec
    all2 = GenServerring.all(ring)
    up2 = GenServerring.up(ring)
    assert length(all2) == 4
    assert length(up2) == 4

    # n4 has crashed
    :ct.sleep(5_000) # 24 sec
    ^all2 = GenServerring.all(ring)
    assert length(GenServerring.up(ring)) == 3

    # forcing n2 down during 10 seconds
    n2 = :"n2@#{hostname()}"
    :ct.sleep(3_000) # 27 sec
    assert Enum.member?(GenServerring.all(ring), n2)
    assert Enum.member?(GenServerring.up(ring), n2)
    GenServerring.force_down(ring, n2)
    assert Enum.member?(GenServerring.all(ring), n2)
    assert false == Enum.member?(GenServerring.up(ring), n2)
    :ct.sleep(10_000) # 37 sec
    GenServerring.unforce_down(ring, n2)
    assert Enum.member?(GenServerring.all(ring), n2)
    assert Enum.member?(GenServerring.up(ring), n2)

    :ct.sleep(3_000) # give time to n3 to gossip that n2 is back
  end

  defp hostname do
    {name, 0} = System.cmd("hostname", [])
    String.strip(name)
  end
end
