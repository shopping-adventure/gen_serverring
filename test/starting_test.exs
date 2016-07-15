defmodule StartingTest do
  use ExUnit.Case, async: false

  setup_all do
    Application.start(:crdtex)
    
    on_exit fn() ->
      Application.stop(:crdtex)
    end

    :ok
  end

  #@tag :pending
  test "default params" do
    assert :ok == Application.start(:gen_serverring)
    assert true == Enum.member?(Process.registered(), :demo_ring)
    assert :ok == Application.stop(:gen_serverring)
  end

  @tag name: :my_name
  @tag callback: __MODULE__

  test "arbitrary name and callback", context do
    Application.put_env(:gen_serverring, :name, context.name)
    Application.put_env(:gen_serverring, :callback, context.callback)
    assert :ok == Application.start(:gen_serverring)
    assert true == Enum.member?(Process.registered(), :my_name)
    assert :ok == Application.stop(:gen_serverring)
  end

  # act as a minimal callback
  def init([]), do: {:ok, Crdtex.Counter.new}
end
