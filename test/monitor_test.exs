defmodule MonitorTest do
  use ExUnit.Case, async: true

  setup do
    case File.dir?("./data") do
      true -> File.rm_rf("./data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    Application.start(:crdtex)
    Application.start(:gen_serverring)

    on_exit fn() ->
      Application.stop(:gen_serverring)
      Application.stop(:crdtex)
    end

    :ok
  end

  test "monitoring the default ring" do
    assert length(GenServerring.all(:demo_ring)) == 1
    assert length(GenServerring.up(:demo_ring)) == 1
    :timer.sleep(13_000) # wait to be part of the ring
    all = GenServerring.all(:demo_ring)
    up = GenServerring.up(:demo_ring)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(1000)
    :ct.pal(:debug, 50, 'up is ~p~n', [GenServerring.up(:demo_ring)])
    :timer.sleep(5_000) # n4 added and enough gossips should have occured
    all2 = GenServerring.all(:demo_ring)
    up2 = GenServerring.up(:demo_ring)
    assert length(all2 -- all) == 1
    assert length(up2 -- up) == 1
    :timer.sleep(6_000) # n4 has crashed
    ^all2 = Genserverring.all(:demo_ring)
    ^up = Genserverring.up(:demo_ring)
  end
end
