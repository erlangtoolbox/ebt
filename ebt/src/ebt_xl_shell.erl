-module(ebt_xl_shell).

-export([command/1, command/2, getenv/0]).

-spec(command(string()) -> error_m:monad(string())).
command(Command) ->
    case eunit_lib:command(Command) of
        {0, Out} -> {ok, Out};
        {_, Out} -> {error, Out}
    end.

-spec(command(string(), file:name()) -> error_m:monad(string())).
command(Command, Dir) ->
    case eunit_lib:command(Command, Dir) of
        {0, Out} -> {ok, Out};
        {_, Out} -> {error, Out}
    end.

-spec(getenv() -> [{atom(), string()}]).
getenv() ->
    lists:map(fun(V) ->
        {H, [_ | T]} = lists:splitwith(fun(X) -> X /= $= end, V),
        {list_to_atom(H), T}
    end, os:getenv()).