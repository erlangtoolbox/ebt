-module(ebt_task_depends).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Dir, _Config, _Defaults) ->
    do([ error_m ||
        return(ok)
    ]).

