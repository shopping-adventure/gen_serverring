-module(restart_SUITE).

-export([all/0, restart_test/1]).

all() -> [restart_test].

restart_test(_) -> ct_elixir_wrapper:run_elixir_test('restart_test.exs').
