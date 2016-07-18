defmodule ReaderTest do
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

  test "reading from the default ring" do
    :timer.sleep(7_000) # wait to be part of the ring
    v = Demo.get # v should be 2
    2 = v
    :timer.sleep(7_000)
    t = Demo.get
    assert t == (v + 1)
    :timer.sleep(5_000)
    t = Demo.get
    assert t == (v + 2)
    :timer.sleep(5_000)
    t = Demo.get
    assert t == (v + 3)
  end
end
