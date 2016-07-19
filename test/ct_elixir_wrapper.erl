%%% @author Laurent Picouleau <laurent@kbrwadventure>
%%% @doc
%%% inspired from
%%% <https://github.com/processone/ejabberd/blob/master/test/elixir_SUITE.erl>
%%% @end

-module(ct_elixir_wrapper).

-export([elixir_init/0, init/0, stop/0, run_elixir_test/1, test_dir/0]).

-export([apps_path/0]).

apps_path() ->
    {ok, Cwd} = file:get_cwd(),
    Build = lists:append(Cwd, "/_build/dev/lib/"),
    code:add_patha(lists:append(Build, "crdtex/ebin")),
    code:add_patha(lists:append(Build, "gen_serverring/ebin")).

elixir_init() ->
    code:add_patha(elixir_dir() ++ "elixir/ebin"),
    code:add_patha(elixir_dir() ++ "ex_unit/ebin"),
    code:add_patha(elixir_dir() ++ "iex/ebin"),
    code:add_patha(elixir_dir() ++ "logger/ebin"),
    application:start(compiler),
    application:start(elixir),
    'Elixir.Application':start(iex),
    'Elixir.Application':start(logger).


init() ->
    elixir_init(),
    true = code:add_patha(base_dir() ++ "/_build/dev/lib/crdtex/ebin"),
    true = code:add_patha(base_dir() ++ "/_build/dev/lib/gen_serverring/ebin").

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

base_dir() ->
    {ok, CWD} = file:get_cwd(),
    filename:join(CWD, "../..").

test_dir() -> base_dir() ++ "/test".
