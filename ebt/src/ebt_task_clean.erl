-module(ebt_task_clean).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(_Dir, Config) ->
    do([ error_m ||
        delete(ebt_config:outdir(production, Config)),
        delete(ebt_config:outdir(dist, Config)),
        delete(ebt_config:outdir(test, Config))
    ]).

delete({ok, Dir}) ->
    io:format("delete ~s~n", [Dir]),
    ebt_strikead_file:delete(Dir);
delete(E) -> E.