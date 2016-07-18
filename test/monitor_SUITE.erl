-module(monitor_SUITE).

-export([all/0]).

-export([monitor_test/1]).

all() -> [monitor_test].

monitor_test(_) -> ct_elixir_wrapper:run_elixir_test('monitor_test.exs').
