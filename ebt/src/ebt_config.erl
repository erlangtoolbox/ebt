-module(ebt_config).

-compile({parse_transform, do}).

-type config() :: strikead_lists:kvlist_at().
-type defaults() :: strikead_lists:kvlist_at().
-export_types([config/0, defaults/0]).

-export([read/1, value/3, value/4, find_value/3, outdir/2, version/1, appname/2,
    app_outdir/4, outdir/3]).

-spec read/1 :: (file:name()) -> error_m:monad(config()).
read(Filename) ->
    case strikead_file:exists(Filename) of
        {ok, true} -> strikead_file:read_terms(Filename);
        {ok, false} -> {ok, []};
        E -> E
    end.

-spec find_value/3 :: (atom(), config(), defaults()) -> maybe_m:monad(any()).
find_value(Key, Config, Defaults) when is_list(Defaults)->
    do([search_m ||
        strikead_lists:kvfind(Key, Config),
        strikead_lists:kvfind(Key, Defaults)
    ]).

-spec value/3 :: (atom(), config(), any()) -> any().
value(Key, Config, Default) -> strikead_lists:kvfind(Key, Config, Default).

-spec value/4 :: (atom(), config(), atom(), any()) -> any().
value(Key, Config, InnerKey, Default) ->
    case strikead_lists:kvfind(Key, Config) of
        {ok, V} -> strikead_lists:kvfind(InnerKey, V, Default);
        undefined -> Default
    end.

-spec outdir/2 :: (config(), defaults()) -> error_m:monad(string()).
outdir(Config, Defaults) ->
    case ebt_config:find_value(outdir, Config, Defaults) of
        Ok = {ok, _} -> Ok;
        _ -> {error, "unknown output directory"}
    end.

-spec outdir/3 :: (atom(), config(), defaults()) -> error_m:monad(string()).
outdir(Kind, Config, Defaults) ->
        case outdir(Config, Defaults) of
            {ok, Dir} -> {ok, filename:absname(filename:join(Dir, atom_to_list(Kind)))};
            E -> E
        end.


-spec app_outdir/4 :: (atom(), file:name(), config(), defaults()) -> maybe_m:monad(string()).
app_outdir(Kind, Dir, Config, Defaults) ->
    do([error_m ||
        App <- appname(Dir, Config),
        OutDir <- outdir(Kind, Config, Defaults),
        return(filename:join(OutDir, App))
    ]).

-spec version/1 :: (config()) -> error_m:monad(string()).
version(Config) ->
    case strikead_lists:kvfind(version, Config) of
        {ok, {shell, X}} -> {ok, os:cmd(X)};
        _ -> {ok, "0.0.1"}
    end.

-spec appname/2 :: (file:name(), ebt_config:config()) -> error_m:monad(string()).
appname(Dir, Config) ->
    do([error_m ||
        {_, Name, _} <- ebt_applib:load(Dir),
        Version <- ebt_config:version(Config),
        return(strikead_string:join([Name, Version],"-"))
    ]).
