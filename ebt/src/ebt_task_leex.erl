-module(ebt_task_leex).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, _Config) ->
    SrcDir = Dir ++ "/src",
    Files = filelib:wildcard(SrcDir ++ "/*.xrl"),
    ebt_strikead_lists:eforeach(fun(F) ->
        io:format("generating lexer from ~p~n", [F]),
        leex:file(F)
    end, Files).

