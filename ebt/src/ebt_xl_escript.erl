-module(ebt_xl_escript).

-compile({parse_transform, do}).

-export([read_file/2, read_file/1]).

-spec read_file/1 :: (file:name()) -> error_m:monad(binary()).
read_file(File) -> read_file(escript:script_name(), File).

-spec read_file/2 :: (file:name(), file:name()) -> error_m:monad(binary()).
read_file(EscriptPath, File) ->
    do([error_m ||
        Sections <- escript:extract(EscriptPath, []),
        case ebt_xl_lists:find(fun(Section) ->
            element(1, Section) == archive
        end, Sections) of
            undefined -> {ok, undefined};
            {ok, {_, Zip}} ->
                do([error_m ||
                    Handle <- zip:zip_open(Zip, [memory]),
                    try zip:zip_get(File, Handle) of
                        {ok, {_, Bin}} -> {ok, Bin};
                        {error, E} -> {error, {E, [EscriptPath, File]}}
                    after
                        zip:zip_close(Handle)
                    end
                ])
        end
    ]).

