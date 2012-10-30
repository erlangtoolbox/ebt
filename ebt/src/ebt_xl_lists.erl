-module(ebt_xl_lists).

-export([find/2, first/1, emap/2, eforeach/2, mapfilter/2, index/2, split/2, keypsort/3,
    sublistmatch/2, substitute/3, keyfind/3, keyfind/4, keyreplace/3, kvfind/2,
    kvfind/3, keyreplace_or_add/3, eflatten/1]).

-type(kvlist(A, B) :: [{A, B}]).
-type(kvlist_at() :: kvlist(atom(), atom() | binary() | string() | integer() | float())).
-export_types([kvlist/2, kvlist_at/0]).

-spec(find(fun((term()) -> boolean()), [term()]) -> option_m:monad(term())).
find(_Pred, []) -> undefined;
find(Pred, [H | T]) ->
    case Pred(H) of
        true -> {ok, H};
        _ -> find(Pred, T)
    end.

-spec(first([term()]) -> option_m:monad(term())).
first([]) -> undefined;
first([H | _]) -> {ok, H}.

-spec(emap(fun((term()) -> error_m:monad(term())), [term()]) ->
    error_m:monad([term()])).
emap(F, List) -> emap(F, [], List).

-spec(emap(fun((term()) -> error_m:monad(term())), [term()], [term()]) ->
    error_m:monad([term()])).
emap(_F, Acc, []) -> {ok, lists:reverse(Acc)};
emap(F, Acc, [H | T]) ->
    case F(H) of
        {ok, R} -> emap(F, [R | Acc], T);
        X -> X
    end.

-spec(eforeach(fun((any()) -> error_m:monad(any())), []) -> error_m:monad(ok)).
eforeach(_F, []) -> ok;
eforeach(F, [H | T]) ->
    case F(H) of
        ok -> eforeach(F, T);
        {ok, _} -> eforeach(F, T);
        X -> X
    end.

-spec(mapfilter(fun((term()) -> false | term()), [term()]) -> [term()]).
mapfilter(F, L) -> mapfilter([], F, L).

-spec(mapfilter([term()],
    fun((term()) -> option_m:monad(term())), [term()]) -> [term()]).
mapfilter(Acc, _F, []) -> lists:reverse(Acc);
mapfilter(Acc, F, [H | T]) ->
    case F(H) of
        undefined -> mapfilter(Acc, F, T);
        {ok, X} -> mapfilter([X | Acc], F, T)
    end.

-spec(keypsort([term()], integer(), kvlist(term(), term()))
        -> [{term(), term()}]).
keypsort(Keys, N, L) ->
    C = fun(A, B) ->
        case {index(element(N, A), Keys), index(element(N, B), Keys)} of
            {undefined, _} -> true;
            {_, undefined} -> false;
            {{ok, I1}, {ok, I2}} -> I1 =< I2
        end
    end,
    lists:sort(C, L).

-spec(index(term(), [term()]) -> option_m:monad(integer())).
index(X, L) -> index(X, 1, L).

-spec(index(term(), integer(), [term()])
        -> option_m:monad(integer())).
index(_X, _I, []) -> undefined;
index(X, I, [X | _]) -> {ok, I};
index(X, I, [_ | T]) -> index(X, I + 1, T).

-spec(sublistmatch(kvlist_at(), kvlist_at()) -> boolean()).
sublistmatch(Pattern, Map) ->
    lists:all(fun({Pk, Pv}) ->
        case lists:keyfind(Pk, 1, Map) of
            {Pk, Pv} -> true;
            {Pk, V} when is_list(V) ->
                re:run(V, Pv, [anchored, {capture, none}]) == match;
            _ -> false
        end
    end, Pattern).

-spec(substitute([term()], kvlist_at(),
    StringHandler :: fun((string(), kvlist_at()) -> string())) -> [term()]).
substitute(Pattern, Map, StringHandler) ->
    lists:map(fun(X) ->
        case lists:keyfind(X, 1, Map) of
            {X, V} -> V;
            _ when is_list(X) -> StringHandler(X, Map);
            _ -> X
        end
    end, Pattern).

-spec(keyfind(term(), pos_integer(), [tuple()], tuple()) -> tuple()).
keyfind(Key, N, List, Default) ->
    case keyfind(Key, N, List) of
        {ok, X} -> X;
        undefined -> Default
    end.

-spec(keyfind(term(), pos_integer(), [tuple()]) -> option_m:monad(tuple())).
keyfind(Key, N, List) ->
    case lists:keyfind(Key, N, List) of
        false -> undefined;
        X -> {ok, X}
    end.

-spec(kvfind(term(), kvlist(any(), any()), any()) -> any()).
kvfind(Key, List, Default) ->
    case kvfind(Key, List) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

-spec(kvfind(term(), kvlist(any(), any())) -> option_m:monad(any())).
kvfind(Key, List) ->
    case keyfind(Key, 1, List) of
        {ok, {_, Value}} -> {ok, Value};
        X -> X
    end.

-spec(keyreplace_or_add(pos_integer(), [tuple()], tuple() | [tuple()]) -> [tuple()]).
keyreplace_or_add(N, List, List2) when is_list(List2) ->
    lists:foldl(fun(T, Acc) -> keyreplace_or_add(N, Acc, T) end, List, List2);
keyreplace_or_add(N, List, Tuple) when is_tuple(Tuple) ->
    case lists:keymember(element(N, Tuple), N, List) of
        true -> lists:keyreplace(element(N, Tuple), N, List, Tuple);
        false -> [Tuple | List]
    end.

-spec(keyreplace(pos_integer(), [tuple()], [tuple()]) -> [tuple()]).
keyreplace(_N, List, []) -> List;
keyreplace(N, List, [R | ReplList]) ->
    keyreplace(N, lists:keyreplace(element(N, R), N, List, R), ReplList).

-spec(split(pos_integer(), [term()]) -> {[term()], [term()]}).
split(Pos, List) when length(List) > Pos -> lists:split(Pos, List);
split(_, List) -> {List, []}.

-spec(eflatten(error_m:monad([term()])) -> error_m:monad([term()])).
eflatten({ok, List}) -> {ok, lists:flatten(List)};
eflatten(E) -> E.