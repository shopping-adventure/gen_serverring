defmodule MonitorTest do
  use ExUnit.Case, async: true

  setup context do
    case File.dir?("./data") do
      true -> File.rm_rf("./data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    Application.start(:crdtex)
    Application.put_env(:gen_serverring, :name, context.name)
    Application.put_env(:gen_serverring, :callback, context.callback)
    Application.start(:gen_serverring)

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
    :ct.sleep(13_000) # wait to be part of the ring
    all = GenServerring.all(ring)
    up = GenServerring.up(ring)
    assert length(all) == 3
    assert length(up) == 3
    :ct.sleep(5_000) # n4 added and enough gossips should have occured
    all2 = GenServerring.all(ring)
    up2 = GenServerring.up(ring)
    assert length(all2) == 4
    assert length(up2) == 4
    :ct.sleep(6_000) # n4 has crashed
    ^all2 = GenServerring.all(ring)
    assert length(GenServerring.up(ring)) == 3
  end
end
