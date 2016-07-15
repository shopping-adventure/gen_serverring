%%% @author Laurent Picouleau <laurent@kbrwadventure>
%%% @doc
%%% inspired from
%%% <https://github.com/processone/ejabberd/blob/master/test/elixir_SUITE.erl>
%%% @end

-module(ct_elixir_wrapper).

-export([init/0, stop/0, run_elixir_test/1, test_dir/0]).

init() ->
		% TODO: a better solution for paths...
		true = code:add_patha(elixir_dir() ++ "elixir/ebin"),
		true = code:add_patha(elixir_dir() ++ "ex_unit/ebin"),
		true = code:add_patha(elixir_dir() ++ "iex/ebin"),
		true = code:add_patha(elixir_dir() ++ "logger/ebin"),
		true = code:add_patha(test_dir() ++ "/../_build/dev/lib/crdtex/ebin"),
		true =
		    code:add_patha(test_dir() ++ "/../_build/dev/lib/gen_serverring/ebin"),
		ok = application:start(compiler),
		ok = application:start(elixir),
		ok = 'Elixir.Application':start(iex),
		ok = 'Elixir.Application':start(logger).

stop() ->
    'Elixir.Application':stop(logger),
	  'Elixir.Application':stop(iex),
		application:stop(elixir),
		application:stop(compiler).

run_elixir_test(Func) ->
    %% Elixir tests can be tagged as follow to be ignored (place before test
    %% start)
    %% @tag pending: true

    'Elixir.ExUnit':start([
        {exclude, [{pending, true}]},
        {formatters, ['Elixir.ExUnit.CLIFormatter']}
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
    filename:join(CWD, "../../test").
