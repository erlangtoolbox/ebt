-module(ebtx).

-export([perform/3]).

perform(_Target, _Dir, Config) ->
    io:format("Hello from EBT!~n"),
    {ok, Config}.
