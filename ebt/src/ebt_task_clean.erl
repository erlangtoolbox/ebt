-module(ebt_task_clean).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(_Dir, Config) ->
    do([error_m ||
        delete(ebt_config:outdir(Config))
    ]).

delete({ok, Dir}) ->
    io:format("delete ~s~n", [Dir]),
    ebt_xl_file:delete(Dir);
delete(E) -> E.