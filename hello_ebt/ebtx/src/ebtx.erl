-module(ebtx).

-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, _Dir, _Config) ->
    io:format("Hello from EBT!~n").
