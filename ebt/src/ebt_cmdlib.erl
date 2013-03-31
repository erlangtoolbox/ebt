%% Copyright
-module(ebt_cmdlib).
-author("volodymyr.kyrychenko@strikead.com").

%% API
-export([exec/2, exec/1]).

exec({Command, Params}) ->
    exec(ebt__xl_string:format(Command, Params));
exec(Command) ->
    io:format("exec ~s~n", [Command]),
    process_result(Command, ebt__xl_shell:command(Command)).

exec({Command, Params}, Dir) ->
    exec(ebt__xl_string:format(Command, Params), Dir);
exec(Command, Dir) ->
    io:format("exec ~s in ~s~n", [Command, Dir]),
    process_result(Command, ebt__xl_shell:command(Command, Dir)).

process_result(_Command, {ok, Stdout}) ->
    io:format("~s", [Stdout]);
process_result(Command, {error, Stdout}) ->
    io:format("~s", [Stdout]),
    {error, ebt__xl_string:format("failed: ~s", [Command])}.
