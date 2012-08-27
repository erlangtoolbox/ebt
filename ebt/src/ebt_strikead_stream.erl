-module(ebt_strikead_stream).

-export([stream/2, map/2, foreach/2, seq/2, foldl/3, filter/2, to_list/1,
    to_stream/1, to_pair/1, mapfind/2, empty/0]).
-export([ifoldl/3]).

-opaque stream() :: fun().

stream(Context, Next) ->
    fun() ->
        case Next(Context) of
            empty -> [];
            {R, C} -> [R | stream(C, Next)]
        end
    end.

map(F, S) ->
    fun() ->
        case S() of
            [] -> [];
            [H | T] -> [F(H) | map(F, T)]
        end
    end.

foreach(F, S) ->
    case S() of
        [] -> done;
        [H | T] -> F(H), foreach(F, T)
    end.

seq(From, To) ->
    stream(From, fun(X) ->
        if
                X =< To -> {X, X + 1};
                true -> empty
        end
    end).

foldl(F, Acc0, S) ->
    case S() of
        [] -> Acc0;
        [H | T] -> foldl(F, F(H, Acc0), T)
    end.


ifoldl(F, Acc0, S) -> ifoldl(F, Acc0, 1, S).

ifoldl(F, Acc0, Index, S) ->
    case S() of
        [] -> Acc0;
        [H | T] -> ifoldl(F, F(H, Acc0, Index), Index + 1, T)
    end.


filter(P, S) ->
    fun() -> filter_next(P, S) end.

filter_next(P, S) ->
    case S() of
        [] -> [];
        [H | T] ->
            case P(H) of
                true -> [H | filter(P, T)];
                _ -> filter_next(P, T)
            end
    end.

to_list(S) -> lists:reverse(foldl(fun(V, L) -> [V | L] end, [], S)).

to_pair(S) -> S().

to_stream(L) when is_list(L) ->
    stream(L, fun
            ([]) -> empty;
            ([H | T]) -> {H, T}
    end).

-spec mapfind/2 :: (fun((any()) -> option_m:monad(any())), stream()) -> option_m:monad(any()).
mapfind(F, S) ->
    case S() of
        [] -> undefined;
        [H | T] ->
            case F(H) of
                undefined -> mapfind(F, T);
                R -> R
            end
    end.

empty() -> fun() -> [] end.

