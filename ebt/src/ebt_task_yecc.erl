-module(ebt_task_yecc).

-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, _Config) ->
    SrcDir = Dir ++ "/src",
    Files = filelib:wildcard(SrcDir ++ "/*.yrl"),
    ebt__xl_lists:eforeach(fun(F) ->
        io:format("generating parser from ~p~n", [F]),
        yecc:file(F)
    end, Files).

