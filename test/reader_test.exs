defmodule ReaderTest do
  use ExUnit.Case, async: true

  setup context do
    case File.dir?("./data") do
      true -> File.rm_rf("./data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    :ok = Application.start(:crdtex)
    Application.put_env(:gen_serverring, :name, context.name)
    :ok = Application.start(:gen_serverring)

    on_exit fn() ->
      Application.stop(:gen_serverring)
      Application.stop(:crdtex)
    end

    :ok
  end

  @tag name: :ct_test_ring
  # using the default callback: Demo
  test "reading from ring", context do
    ring = context.name
    :ct.sleep(7_000)
    v = Demo.get(ring)
    2 = v
    :ct.sleep(7_000)
    t = Demo.get(ring)
    assert t == (v + 1)
    :ct.sleep(5_000)
    t = Demo.get(ring)
    assert t == (v + 2)
    :ct.sleep(5_000)
    t = Demo.get(ring)
    assert t == (v + 3)
  end
end
