-module(ebt_xl_auto).

-export([using/3]).

-spec(using(Module, Params, Callback) -> term() when
    Module :: module(),
    Params :: term(),
    Callback :: fun()).

using(Module, Params, F) ->
    case Module:auto_open(Params) of
        {ok, Descriptor} ->
            try
                {ok, F(Descriptor)}
            after
                Module:auto_close(Descriptor)
            end;
        X -> X
    end.

