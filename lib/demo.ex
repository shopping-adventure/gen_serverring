defmodule Demo do
  use GenServerring
  require Crdtex
  require Crdtex.Counter

  def init([]), do: {:ok, Crdtex.Counter.new}

  def inc(server \\ :demo_ring), do: GenServerring.cast(server, :inc)

  def dec(server \\ :demo_ring), do: GenServerring.cast(server, :dec)

  def get(server \\ :demo_ring), do: GenServerring.call(server, :get)

  # almost like using a GenServer :)
  # note that for the moment GenServerring.stop is not calling
  # callback.terminate :-(

  def handle_call(:get, _, counter), do: {:reply, Crdtex.value(counter), counter}

  def handle_cast(:inc, counter) do
    {:ok, counter} = Crdtex.update(counter, {node(), dot()}, :increment)
    {:noreply, counter}
  end
  def handle_cast(:dec, counter) do
    {:ok, counter} = Crdtex.update(counter, {node(), dot()}, :decrement)
    {:noreply, counter}
  end

  def handle_info(msg, counter) do
    IO.puts("got this weird message #{msg}")
    {:noreply, counter}
  end

  # specific to GenServerring and mandatory
  # useful only for side effects (such as persisting the payload)  as it does
  # allow the payload to be changed (at least not for time being).
  def handle_change(_) do
    :nothingtodo
  end

  # {:erlang.monotic_time(), :erlang.unique_integer([:monotonic]} might be needed
  defp dot(), do: :erlang.unique_integer([:monotonic, :positive])
end
