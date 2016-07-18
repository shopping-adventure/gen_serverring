-module(crasher_SUITE).

-export([all/0]).

-export([crasher_test/1]).

all() -> [crasher_test].

crasher_test(_) ->
    timer:sleep(11000),
		init:stop().
