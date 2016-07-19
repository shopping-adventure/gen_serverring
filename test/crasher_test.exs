defmodule CrasherTest do
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

  test "just a stupid crasher..." do
    :ct.sleep(18_000) # node 4 added to ring and 3 gossips occured
    Demo.get
    :ct.sleep(5_000)
    Demo.get
    :init.stop
  end
end
