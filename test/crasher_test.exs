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
    :timer.sleep(18_000)
    Demo.get
    :timer.sleep(5_000)
    :init.stop
  end
end
