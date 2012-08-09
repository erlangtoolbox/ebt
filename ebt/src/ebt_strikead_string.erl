-module(ebt_strikead_string).

-export([empty/1, not_empty/1, strip/1, quote/1, stripthru/1, format/2,
    to_float/1, substitute/2, to_string/1, mk_atom/1, to_upper/1, to_lower/1,
    equal_ignore_case/2, join/2, join/1, to_atom/1, to_binary/1, to_integer/1]).

-type iostring() :: string() | binary().
-export_type([iostring/0]).

-spec empty/1 :: (string()) -> boolean().
empty(S) -> S == "".

-spec not_empty/1 :: (string()) -> boolean().
not_empty(S) -> S /= "".

-spec strip/1 :: (string()) -> string().
strip(S) -> strip(S, forward).
strip("", _) -> "";
strip([$ | T], Dir) -> strip(T, Dir);
strip([$\t | T], Dir) -> strip(T, Dir);
strip([$\r | T], Dir) -> strip(T, Dir);
strip([$\n | T], Dir) -> strip(T, Dir);
strip(T, forward) -> lists:reverse(strip(lists:reverse(T), backward));
strip(T, backward) -> T.

-spec stripthru/1 :: (string()) -> string().
stripthru(S) -> [X || X <- S, X /= $\n andalso X /= $\r andalso X /= $\t].

-spec quote/1 :: (string()) -> string().
quote(Str) -> format("~5000p", [Str]).

-spec format/2 :: (io:format(), [term()]) -> string().
format(Pattern, Values) -> lists:flatten(io_lib:format(Pattern, Values)).

-spec to_float/1 :: (string()) -> float().
to_float(X) ->
    try
        list_to_float(X)
    catch
        _:_ -> float(list_to_integer(X))
    end.

-spec substitute/2 :: (string(), ebt_strikead_lists:kvlist_at()) -> string().
substitute(Str, Map) ->
    Parts = re:split(Str, "(\\\{[a-zA-Z\\\-_\\\.]+\\\})", [{return, list}, trim]),
    lists:flatten([replace_macro(X, Map) || X <- Parts]).

-spec replace_macro/2 :: (string(), ebt_strikead_lists:kvlist_at()) -> string().
replace_macro([${|T], Map) ->
    Key = list_to_atom(string:strip(T, right, $})),
    case lists:keyfind(Key, 1, Map) of
        {_, V} -> to_string(V);
        _ -> ""
    end;
replace_macro(X, _Map) -> X.

-spec to_string/1 :: (atom() | binary() | string() | float() | integer())
    -> string().
to_string(V) when is_binary(V) -> binary_to_list(V);
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_list(V) -> V;
to_string(V) when is_float(V); is_integer(V) -> format("~p", [V]);
to_string(V) -> format("~p", [V]).

-spec mk_atom/1 :: ([atom() | binary() | string() | float() | integer()]) ->
    atom().
mk_atom(L) when is_list(L) ->
    list_to_atom(string:join([to_string(X) || X <- L], "")).

-spec equal_ignore_case/2 :: (iostring(), iostring()) -> boolean().
equal_ignore_case(A, B) when is_list(A), is_list(B);
    is_binary(A), is_binary(B) ->
    string:equal(to_lower(A), to_lower(B));
equal_ignore_case(A, B) when is_list(A) ->
    string:equal(to_lower(list_to_binary(A)), to_lower(B));
equal_ignore_case(A, B) when is_list(B) ->
    string:equal(to_lower(A), to_lower(list_to_binary(B))).

-spec to_lower/1 :: (iostring()) -> iostring().
to_lower(S) when is_binary(S) -> list_to_binary(to_lower(binary_to_list(S)));
to_lower(S) when is_list(S) -> string:to_lower(S).

-spec to_upper/1 :: (iostring()) -> iostring().
to_upper(S) when is_binary(S) -> list_to_binary(to_upper(binary_to_list(S)));
to_upper(S) when is_list(S) -> string:to_upper(S).

%todo test performance of concatenating lists and binaries
-spec join/2 :: ([iostring()], iostring()) -> iostring().
join(List, Delim) when is_binary(Delim) ->
    list_to_binary(join(List, binary_to_list(Delim)));
join(List, Delim) -> string:join([to_string(X) || X <- List], Delim).

-spec join/1 :: ([iostring()]) -> string().
join(List) -> join(List, "").

-spec to_atom/1 :: (iostring() | atom()) -> atom().
to_atom(X) when is_binary(X) -> binary_to_atom(X, utf8);
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_atom(X) -> X.

-spec to_binary/1 :: (iostring() | atom()) -> binary().
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_list(X) -> list_to_binary(X);
to_binary(X) when is_binary(X) -> X.

-spec to_integer/1 :: (iostring()|atom()|binary()) -> integer().
to_integer(X) when is_list(X) ->  list_to_integer(X);
to_integer(X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to_integer(X) when is_binary(X) -> list_to_integer(binary_to_list(X)).

		      
