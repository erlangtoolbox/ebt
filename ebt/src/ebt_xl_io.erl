-module(ebt_xl_io).

-export([lines/1, parse_lines/1, posix_error/2, apply_io/3, is_posix_error/1]).
-type posix_error() :: {error, {atom(), atom() | string(), any()}}.
-export_types([posix_error/0]).

lines(IoDevice) -> ebt_xl_stream:map(fun({L, _}) -> L end, parse_lines(IoDevice)).

parse_lines(IoDevice) ->
    ebt_xl_stream:stream(IoDevice, fun(_) ->
        {ok, Pos} = file:position(IoDevice, {cur, 0}),
        case io:get_line(IoDevice, '') of
            eof -> empty;
            L -> {{L, Pos}, IoDevice}
        end
    end).

-spec posix_error/2 :: ({error, atom()}, any()) -> posix_error().
posix_error(E = {error, Code}, Target) ->
    case is_posix_error(E) of
        true -> {error, {Code, erl_posix_msg:message(Code), Target}};
        _ -> {error, {Code, Code, Target}}
    end.

-spec is_posix_error/1 :: ({error, atom()}) -> boolean().
is_posix_error({error, Code}) -> erl_posix_msg:message(Code) /= "unknown POSIX error".

-spec apply_io/3 :: (module(), atom(), [term()]) -> any() | posix_error().
apply_io(Module, Func, Args) ->
    case apply(Module, Func, Args) of
        E = {error, _} -> posix_error(E, Args);
        X -> X
    end.

