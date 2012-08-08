-module(ebt_task_clean).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Dir, Config, Defaults) ->
    do([ error_m ||
        delete(ebt_config:outdir(production, Config, Defaults)),
        delete(ebt_config:outdir(dist, Config, Defaults)),
        delete(ebt_config:outdir(test, Config, Defaults))
    ]).

delete({ok, Dir}) ->
    io:format("delete ~s~n", [Dir]),
    strikead_file:delete(Dir);
delete(E) -> E.