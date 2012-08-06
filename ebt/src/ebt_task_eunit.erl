-module(ebt_task_eunit).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/3]).

-spec perform/3 :: (file:name(), ebt_config:config(), ebt_config:defaults()) ->
    error_m:monad(ok).
perform(Dir, Config, Defaults) ->
    do([error_m ||
        TestDir <- ebt_config:app_test_outdir(Dir, Config, Defaults),
        EbinTestDir <- return(filename:join(TestDir, "ebin")),
        return(code:add_path(EbinTestDir)),
        case filelib:wildcard(EbinTestDir ++ "/*.beam") of
            [] ->
                io:format("no tests in ~s~n", [EbinTestDir]),
                ok;
            L ->
                strikead_lists:eforeach(fun(Module) ->
                    io:format("test ~p~n", [Module]),
                    case eunit:test(Module) of
                        error -> {error, {test_failed, Module}};
                        ok -> ok
                    end
                end, lists:map(fun(F) ->
                    io:format("~s~n", [F]),
                    list_to_atom(filename:basename(F, "_tests.beam"))
                end, lists:filter(fun(F) ->
                    lists:suffix("_tests.beam", F)
                end, L)))
        end
    ]).

