defmodule WriterTest do
  use ExUnit.Case, async: true

  setup context do
    case File.dir?("./data") do
      true -> File.rm_rf("./data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    :ok = Application.start(:crdtex)
    {:ok, _} = GenServerring.start_link({context.name, Demo})

    on_exit fn() ->
      Application.stop(:crdtex)
    end

    :ok
  end

  @tag name: :ct_test_ring
  # using Demo as callback
  test "writing to ring", context do
    host = hostname()
    ring = context.name
    Demo.inc(ring)

    :ct.sleep(5_000) # 5 sec
    GenServerring.add_node(ring, :"n2@#{host}")
    Demo.inc(ring)

    :ct.sleep(5_000) # 10 sec
    GenServerring.add_node(ring, :"n3@#{host}")
    Demo.inc(ring)

    :ct.sleep(5_000) # 15 sec
    GenServerring.add_node(ring, :"n4@#{host}")
    Demo.inc(ring)

    :ct.sleep(5_000) # 20 sec
    Demo.inc(ring)

    # n4 has crashed already
    :ct.sleep(5_000) # 25 sec
    Demo.inc(ring)

    # n2 has been forced down by n3
    :ct.sleep(5_000) # 30 sec
    Demo.inc(ring)

    :ct.sleep(5_000) # 35 sec
    Demo.inc(ring)

    #n2 has been unforced down by n3
    :ct.sleep(5_000) # 40 sec
    Demo.inc(ring)

    :ct.sleep(6_000)
  end

  defp hostname do
    {name, 0} = System.cmd("hostname", [])
    String.strip(name)
  end
end
