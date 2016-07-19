-module(crasher_SUITE).

-export([all/0]).

-export([crasher_test/1]).

all() -> [crasher_test].

crasher_test(_) -> ct_elixir_wrapper:run_elixir_test('crasher_test.exs').
