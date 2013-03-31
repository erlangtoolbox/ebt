-module(ebt_task_leex).

-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, _Config) ->
    SrcDir = Dir ++ "/src",
    Files = filelib:wildcard(SrcDir ++ "/*.xrl"),
    ebt__xl_lists:eforeach(fun(F) ->
        io:format("generating lexer from ~p~n", [F]),
        leex:file(F)
    end, Files).

