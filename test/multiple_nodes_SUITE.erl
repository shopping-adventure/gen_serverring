%%% @author Laurent Picouleau <laurent@kbrwadventure>
%%% @doc
%%% largely inspired from
%%% <https://github.com/processone/ejabberd/blob/master/test/elixir_SUITE.erl>
%%% @end

-module(multiple_nodes_SUITE).

%-include_lib("common_test/include/ct.hrl").

-export([init_per_suite/1, init_per_testcase/2, all/0]).
-export([undefined_function/3]).

init_per_suite(Config) ->
    code:add_pathz(filename:join(test_dir(), "../include")),
    Config.

init_per_testcase(_Test_case, Config) ->
    % TODO: set ring_name and callback according to Test_case
    process_flag(error_handler, ?MODULE),
    Config.

all() -> % might be better to explicitly set the list of tests
    Dir = test_dir(),
    Fun = fun(File, Acc) -> [list_to_atom(filename:basename(File)) | Acc] end,
    filelib:fold_files(Dir, "ct_.*_test\.exs$", false, Fun, []).

undefined_function(?MODULE, Func, Args) ->
    case lists:suffix(".exs", atom_to_list(Func)) of
        true -> run_elixir_test(Func);
        false -> error_handler:undefined_function(?MODULE, Func, Args)
    end;
undefined_function(Module, Func, Args) ->
    error_handler:undefined_function(Module, Func,Args).

run_elixir_test(Func) ->
    %% Elixir tests can be tagged as follow to be ignored (place before test
    %% start)
    %% @tag pending: true

		% TODO: a better solution for paths...
		true = code:add_patha(elixir_dir() ++ "elixir/ebin"),
		true = code:add_patha(elixir_dir() ++ "ex_unit/ebin"),
		true = code:add_patha(elixir_dir() ++ "iex/ebin"),
		true = code:add_patha(elixir_dir() ++ "logger/ebin"),
		true = code:add_patha(test_dir() ++ "/../_build/test/lib/crdtex/ebin"),
		true =
		    code:add_patha(test_dir() ++ "/../_build/test/lib/gen_serverring/ebin"),
    'Elixir.ExUnit':start([
        {exclude, [{pending, true}]},
        {formatters, ['Elixir.ExUnit.CLIFormatter']}
%            ['Elixir.ExUnit.CLIFormatter', 'Elixir.ExUnit.CTFormatter']}
    ]),

    'Elixir.Code':load_file(list_to_binary(filename:join(test_dir(),
        atom_to_list(Func)))),
    ResultMap = 'Elixir.ExUnit':run(),

    case maps:find(failures, ResultMap) of
        {ok, 0} -> ok;
        {ok, Failures} ->
            ct:print("Tests failed in module '~s': ~.10B failures.~nSee logs",
                [Func, Failures]),
            ct:fail(elixir_test_failure),
            error
    end.

elixir_dir() -> "/usr/lib/elixir/lib/".

test_dir() ->
    {ok, CWD} = file:get_cwd(),
    filename:join(CWD, "../../../../test").
