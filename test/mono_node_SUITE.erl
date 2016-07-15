-module(mono_node_SUITE).

-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, all/0]).
-export([undefined_function/3]).

init_per_suite(Config) ->
    code:add_pathz(filename:join(test_dir(), "../include")),
		ct_elixir_wrapper:init(),
    Config.

end_per_suite(_) ->
    ct_elixir_wrapper:stop().

init_per_testcase(_Test_case, Config) ->
    process_flag(error_handler, ?MODULE),
    Config.

all() ->
    Dir = test_dir(),
		code:add_pathz(Dir),
		['starting_test.exs', 'mono_counter_test.exs'].

undefined_function(?MODULE, Func, Args) ->
    case lists:suffix(".exs", atom_to_list(Func)) of
        true -> ct_elixir_wrapper:run_elixir_test(Func);
        false -> error_handler:undefined_function(?MODULE, Func, Args)
    end;
undefined_function(Module, Func, Args) ->
    error_handler:undefined_function(Module, Func,Args).

test_dir() ->
    {ok, CWD} = file:get_cwd(),
    filename:join(CWD, "../../test").
