%% Copyright
-module(ebt_xl_convert).
-author("volodymyr.kyrychenko@strikead.com").

-type primitive_type() :: atom() | binary() | string() | float() | integer().

%% API
-export([to_string/1, to_atom/1, to_float/1, to_binary/1, to_integer/1,
    make_atom/1, to/2]).

-spec to_string/1 :: (primitive_type()) -> string().
to_string(X) -> to(string, X).

-spec to_atom/1 :: (binary() | string() | atom()) -> atom().
to_atom(X) -> to(atom, X).

-spec to_float/1 :: (string() | binary()) -> float().
to_float(X) -> to(float, X).

-spec to_binary/1 :: (primitive_type()) -> binary().
to_binary(X) -> to(binary, X).

-spec to_integer/1 :: (string() | atom() | binary()) -> integer().
to_integer(X) -> to(integer, X).

-spec make_atom/1 :: ([primitive_type()]) -> atom().
make_atom(L) -> list_to_atom(string:join([to_string(X) || X <- L], "")).

-spec to/2 :: (atom(), term()) -> term().
to(binary, X) when is_binary(X) -> X;
to(binary, X) when is_float(X) -> to(binary, io_lib:format("~p", [X]));
to(binary, X) when is_integer(X) -> to(binary, integer_to_list(X));
to(binary, X) when is_atom(X) -> atom_to_binary(X, utf8);
to(binary, X) when is_list(X) -> list_to_binary(X);

to(string, X) when is_binary(X) -> binary_to_list(X);
to(string, X) when is_atom(X) -> atom_to_list(X);
to(string, X) when is_list(X) -> X;
to(string, X) -> lists:flatten(io_lib:format("~p", [X]));

to(atom, X) when is_binary(X) -> binary_to_atom(X, utf8);
to(atom, X) when is_list(X) -> list_to_atom(X);
to(atom, X) when is_atom(X) -> X;

to(float, X) when is_float(X) -> X;
to(float, X) when is_integer(X) -> X + 0.0;
to(float, X) when is_binary(X) -> to(float, binary_to_list(X));
to(float, X) when is_list(X) ->
    try
        list_to_float(X)
    catch
        _:_ -> float(list_to_integer(X))
    end;


to(integer, X) when is_list(X) -> list_to_integer(X);
to(integer, X) when is_atom(X) -> list_to_integer(atom_to_list(X));
to(integer, X) when is_binary(X) -> list_to_integer(binary_to_list(X)).
