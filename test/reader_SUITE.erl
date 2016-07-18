-module(reader_SUITE).

-export([all/0]).

-export([reader_test/1]).

all() -> [reader_test].

reader_test(_) -> ct_elixir_wrapper:run_elixir_test('reader_test.exs').
