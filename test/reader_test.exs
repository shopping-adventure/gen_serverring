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
    :ct.sleep(7_000)
    v = Demo.get
    2 = v
    :ct.sleep(7_000)
    t = Demo.get
    assert t == (v + 1)
    :ct.sleep(5_000)
    t = Demo.get
    assert t == (v + 2)
    :ct.sleep(5_000)
    t = Demo.get
    assert t == (v + 3)
  end
end
