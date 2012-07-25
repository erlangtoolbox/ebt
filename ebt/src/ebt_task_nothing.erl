-module(ebt_task_nothing).

-behaviour(ebt_task).

-export([perform/3]).

perform(Dir, _Config, _Defaults) ->
    ebt:report("doing nothing in ~s", [Dir]).
