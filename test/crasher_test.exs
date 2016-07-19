defmodule CrasherTest do
  use ExUnit.Case, async: true

  setup context do
    case File.dir?("./data") do
      true -> File.rm_rf("./data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    Application.start(:crdtex)
    Application.put_env(:gen_serverring, :name, context.name)
    Application.start(:gen_serverring)

    on_exit fn() ->
      Application.stop(:gen_serverring)
      Application.stop(:crdtex)
    end

    :ok
  end

  @tag name: :ct_test_ring
  test "just a stupid crasher...", context do
    ring = context.name
    :ct.sleep(18_000) # node 4 added to ring and 3 gossips occured
    Demo.get(ring)
    :ct.sleep(5_000)
    :init.stop
  end
end
