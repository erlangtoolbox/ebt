-module(ebt_task_clean).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, _Dir, Config) ->
    ebt__do([ebt__error_m ||
        delete(ebt_config:outdir(Config))
    ]).

delete({ok, Dir}) ->
    io:format("delete ~s~n", [Dir]),
    ebt__xl_file:delete(Dir);
delete(E) -> E.