# the goal of this test is to ensure that a gen_serverring start correctly in
# the presence of a ring file

defmodule RestartTest do
  use ExUnit.Case, async: true

  setup context do
    case File.dir?(".data") do
      true -> File.rm_rf(".data")
      false -> :ok
    end
    assert :ok == File.mkdir("./data")
    :ok = Application.start(:crdtex)
    
    # we crete a ring file before starting the ring
    set = Crdtex.Set.new
    {:ok, set} = Crdtex.update(set, {node(), 1}, {:add, node()})
    {:ok, set} =
      Crdtex.update(set, {:"node42@localhost", 2}, {:add, :"node43@localhost"})
    {:ok, set} =
      Crdtex.update(set, {:"node43@localhost", 2}, {:add, :"node42@localhost"})
    File.write("./data/ring", :erlang.term_to_binary(set))

    on_exit fn() ->
      Application.stop(:gen_serverring)
      Application.stop(:crdtex)
    end

    :ok
  end

  @tag name: :restart_ring
  test "initialisation from a ring file", context do
    ring = context.name
    {:ok, _} = GenServerring.start_link({ring, Demo})
    assert [node(), :"node42@localhost", :"node43@localhost"] ==
      GenServerring.all(ring)
    assert [node()] == GenServerring.up(ring)
  end
end
