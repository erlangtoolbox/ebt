-module(ebt_config).

-compile({parse_transform, do}).

-type config() :: ebt_strikead_lists:kvlist_at().
-export_types([config/0, defaults/0]).

-export([read/2, value/3, value/4, find_value/2, find_value/3,
    outdir/1, version/1, appname/2, app_outdir/3, outdir/2]).

-spec read/2 :: (file:name(), config()) -> error_m:monad(config()).
read(Filename, Defaults) ->
    case ebt_strikead_file:exists(Filename) of
        {ok, true} ->
            do([error_m||
                Config <- ebt_strikead_file:read_terms(Filename),
                return(ebt_strikead_lists:keyreplace_or_add(1, Defaults, Config))
            ]);
        {ok, false} -> {ok, Defaults};
        E -> E
    end.

-spec find_value/2 :: (atom(), config()) -> error_m:monad(any()).
find_value(Key, Config) ->
    maybe_m:to_error_m(ebt_strikead_lists:kvfind(Key, Config),
        ebt_strikead_string:format("~p not found", [Key])).

-spec find_value/3 :: (atom(), config(), atom()) -> error_m:monad(any()).
find_value(Key, Config, InnerKey) ->
    Error = {error, ebt_strikead_string:format("~p/~p not found", [Key, InnerKey])},
    case ebt_strikead_lists:kvfind(Key, Config) of
        {ok, V} ->
            case ebt_strikead_lists:kvfind(InnerKey, V) of
                undefined -> Error;
                X -> X
            end;
        undefined -> Error
    end.

-spec value/3 :: (atom(), config(), any()) -> any().
value(Key, Config, Default) -> ebt_strikead_lists:kvfind(Key, Config, Default).

-spec value/4 :: (atom(), config(), atom(), any()) -> any().
value(Key, Config, InnerKey, Default) ->
    case ebt_strikead_lists:kvfind(Key, Config) of
        {ok, V} -> ebt_strikead_lists:kvfind(InnerKey, V, Default);
        undefined -> Default
    end.

-spec outdir/1 :: (config()) -> error_m:monad(string()).
outdir(Config) ->
    ebt_config:find_value(outdir, Config).

-spec outdir/2 :: (atom(), config()) -> error_m:monad(string()).
outdir(Kind, Config) ->
        case outdir(Config) of
            {ok, Dir} -> {ok, filename:absname(
                filename:join(Dir, atom_to_list(Kind)))};
            E -> E
        end.


-spec app_outdir/3 :: (atom(), file:name(), config()) -> maybe_m:monad(string()).
app_outdir(Kind, Dir, Config) ->
    do([error_m ||
        App <- appname(Dir, Config),
        OutDir <- outdir(Kind, Config),
        return(filename:join(OutDir, App))
    ]).

-spec version/1 :: (config()) -> error_m:monad(string()).
version(Config) ->
    case ebt_strikead_lists:kvfind(version, Config) of
        {ok, {shell, Cmd}} ->
            case eunit_lib:command(Cmd) of
                {0, Out} -> {ok, Out};
                E -> {error, E}
            end;
        _ -> {ok, "0.0.1"}
    end.

-spec appname/2 :: (file:name(), config()) -> error_m:monad(string()).
appname(Dir, Config) ->
    do([error_m ||
        {_, Name, _} <- ebt_applib:load(Dir),
        Version <- ebt_config:version(Config),
        return(ebt_strikead_string:join([Name, Version], "-"))
    ]).
