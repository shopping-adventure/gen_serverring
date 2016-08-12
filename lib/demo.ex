defmodule Demo do
  use GenServerring
  require Crdtex
  require Crdtex.Counter

  def inc(server \\ :demo_ring), do: GenServerring.cast(server, :inc)

  def dec(server \\ :demo_ring), do: GenServerring.cast(server, :dec)

  def get(server \\ :demo_ring), do: GenServerring.call(server, :get)

  # almost like using a GenServer :)
  # note that for the moment GenServerring.stop is not calling
  # callback.terminate
  def init([]), do: {:ok, Crdtex.Counter.new}

  def handle_call(:get, _, counter),
    do: {:reply, Crdtex.value(counter), counter}

  def handle_cast(:inc, counter) do
    {:ok, counter} = Crdtex.update(counter, {node, dot}, :increment)
    {:noreply, counter}
  end
  def handle_cast(:dec, counter) do
    {:ok, counter} = Crdtex.update(counter, {node, dot}, :decrement)
    {:noreply, counter}
  end

  def handle_info(msg, counter) do
    IO.puts("got this weird message #{msg}")
    {:noreply, counter}
  end

  # specific to GenServerring and mandatory
  # useful only for side effects (such as persisting the payload) as it does
  # not allow the payload to be changed (at least not for time being).
  # It is triggered by a handle_cast which is itself triggered by a
  # handle_info, so it could make sense to let it modify the payload by
  # returning {:noreply, new_payload} | {:noreply, new_payload, :hibernate} |
  # {:no_reply, new_payload, timeout} | {:stop, reason, new_payload}
  def handle_state_change(state),
    do: IO.puts("new state #{Crdtex.value(state)}")

  def handle_ring_change(nodes), do: IO.inspect(nodes)

  defp dot() do
    try do
      :erlang.unique_integer([:monotonic, :positive]) # erlang 18 and up
    rescue
      _ ->
        {mega, second, micro} = :erlang.now()
        (mega * 1_000_000 + second) * 1_000_000 + micro
    end
  end
end
