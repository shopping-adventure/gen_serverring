defmodule ReaderTest do
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
  # using the default callback: Demo
  test "reading from ring", context do
    ring = context.name
    :ct.sleep(7_000)
    v = Demo.get(ring)
    :ct.sleep(7_000)
    t = Demo.get(ring)
    assert t == (v + 1)
    :ct.sleep(5_000)
    t = Demo.get(ring)
    assert t == (v + 2)
    :ct.sleep(5_000) # 24 sec
    t = Demo.get(ring)
    assert t == (v + 3)

    # between 27 and 37 seconds from the start of the test, n2 is forced down by
    # n3 so it should not see all the changes of payload made by n1, the first
    # one might slip through if n1 has not already been gossip notified of n2
    # being forced_down
    :ct.sleep(4_000) # 28 sec
    during = Demo.get(ring)
    assert during <= (v + 4)
    :ct.sleep(5_000) # 33 sec
    assert during == Demo.get(ring)
    :ct.sleep(3_500) # 36.5 sec
    assert during == Demo.get(ring)

    :ct.sleep(7_500) # 44 sec
    t = Demo.get(ring)
    assert t == (v + 7)
    assert v == 2
  end
end
