defmodule WriterTest do
  use ExUnit.Case, async: true

  @host "cantor"

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
  # using Demo as callback
  test "writing to ring", context do
    ring = context.name
    Demo.inc(ring)
    :ct.sleep(5_000)
    GenServerring.add_node(ring, :"n2@#{@host}")
    Demo.inc(ring)
    :ct.sleep(5_000)
    GenServerring.add_node(ring, :"n3@#{@host}")
    Demo.inc(ring)
    :ct.sleep(5_000)
    GenServerring.add_node(ring, :"n4@#{@host}")
    Demo.inc(ring)
    :ct.sleep(5_000)
    Demo.inc(ring)

    :ct.sleep(10_000)
  end
end

