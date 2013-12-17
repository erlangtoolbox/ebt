%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2013, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_cover).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).


-spec(perform(atom(), file:name(), ebt_config:config()) -> ebt__error_m:monad(ok)).
perform(Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinProdDir <- return(filename:join(ProdDir, "ebin")),
        case ebt_config:value(Target, Config, enabled, true) of
            true ->
                Results = cover:compile_beam_directory(EbinProdDir),
                lists:foreach(fun
                    ({ok, Module}) -> io:format("~s instrumented~n", [Module]);
                    ({error, E}) -> io:format("WARNING: ~p~n", [E])
                end, Results);
            false -> ok
        end]).
