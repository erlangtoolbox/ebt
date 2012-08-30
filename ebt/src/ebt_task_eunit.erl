-module(ebt_task_eunit).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/3]).

-spec perform/3 :: (atom(), file:name(), ebt_config:config()) -> error_m:monad(ok).
perform(_Target, Dir, Config) ->
    do([error_m ||
        TestDir <- ebt_config:app_outdir(test, Dir, Config),
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinTestDir <- return(filename:join(TestDir, "ebin")),
        case filelib:wildcard(EbinTestDir ++ "/*.beam") of
            [] ->
                io:format("no tests in ~s~n", [EbinTestDir]),
                ok;
            L ->
                do([error_m ||
                    ebt:load_library(TestDir),
                    ebt:load_library(ProdDir),
                    ebt_xl_lists:eforeach(fun(Module) ->
                        io:format("test ~p~n", [Module]),
                        case eunit:test(Module) of
                            error -> {error, {test_failed, Module}};
                            ok -> ok
                        end
                    end, lists:map(fun(F) ->
                        list_to_atom(filename:basename(F, "_tests.beam"))
                    end, lists:filter(fun(F) ->
                        lists:suffix("_tests.beam", F)
                    end, L)))
                ])
        end
    ]).

