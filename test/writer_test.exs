defmodule WriterTest do
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

  test "writing to the default ring" do
    Demo.inc
    :timer.sleep(5000)
    GenServerring.add_node(:demo_ring, :n2@localhost)
    Demo.inc
    :timer.sleep(5000)
    GenServerring.add_node(:demo_ring, :n3@localhost)
    Demo.inc
    :timer.sleep(5000)
    GenServerring.add_node(:demo_ring, :n4@localhost)
    Demo.inc
    :timer.sleep(5000)
    Demo.inc

    :timer.sleep(10_000)
  end
end
