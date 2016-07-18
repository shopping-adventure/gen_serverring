-module(writer_SUITE).

-export([all/0]).

-export([writer_test/1]).

all() -> [writer_test].

writer_test(_) -> ct_elixir_wrapper:run_elixir_test('writer_test.exs').
