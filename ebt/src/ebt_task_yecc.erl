-module(ebt_task_yecc).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, _Config) ->
    SrcDir = Dir ++ "/src",
    Files = filelib:wildcard(SrcDir ++ "/*.yrl"),
    ebt_strikead_lists:eforeach(fun(F) ->
        io:format("generating parser from ~p~n", [F]),
        yecc:file(F)
    end, Files).

