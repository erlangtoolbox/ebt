-module(ebt_task_erlc).

-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, _Config) ->
    ebt:report("Compiling...~s", [Dir]).
