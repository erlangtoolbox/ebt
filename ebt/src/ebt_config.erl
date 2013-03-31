-module(ebt_config).

-compile({parse_transform, ebt__do}).

-type(config() :: ebt__xl_lists:kvlist_at()).
-export_types([config/0, defaults/0]).

-export([read/2, value/3, value/4, find_value/2, find_value/3,
    outdir/1, outdir/3, version/1, appname_full/2, appname/1, app_outdir/3,
    outdir/2, build_number/1, info_outdir/2]).

-spec(read(file:name(), config()) -> ebt__error_m:monad(config())).
read(Filename, Defaults) ->
    case ebt__xl_file:exists(Filename) of
        {ok, true} ->
            ebt__do([ebt__error_m ||
                Config <- ebt__xl_file:read_terms(Filename),
                return(ebt__xl_lists:keyreplace_or_add(1, Defaults, Config))
            ]);
        {ok, false} -> {ok, Defaults};
        E -> E
    end.

-spec(find_value(atom(), config()) -> ebt__error_m:monad(any())).
find_value(Key, Config) ->
    ebt__option_m:to_ebt__error_m(ebt__xl_lists:kvfind(Key, Config),
            ebt__xl_string:format("~p not found", [Key])).

-spec(find_value(atom(), config(), atom()) -> ebt__error_m:monad(any())).
find_value(Key, Config, InnerKey) ->
    Error = {error, ebt__xl_string:format("~p/~p not found", [Key, InnerKey])},
    case ebt__xl_lists:kvfind(Key, Config) of
        {ok, V} ->
            case ebt__xl_lists:kvfind(InnerKey, V) of
                undefined -> Error;
                X -> X
            end;
        undefined -> Error
    end.

-spec(value(atom(), config(), any()) -> any()).
value(Key, Config, Default) -> ebt__xl_lists:kvfind(Key, Config, Default).

-spec(value(atom(), config(), atom(), any()) -> any()).
value(Key, Config, InnerKey, Default) ->
    case ebt__xl_lists:kvfind(Key, Config) of
        {ok, V} -> ebt__xl_lists:kvfind(InnerKey, V, Default);
        undefined -> Default
    end.

-spec(outdir(config()) -> ebt__error_m:monad(string())).
outdir(Config) ->
    ebt__do([ebt__error_m ||
        Dir <- ebt_config:find_value(outdir, Config),
        ebt__xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(outdir(atom(), config()) -> ebt__error_m:monad(string())).
outdir(Kind, Config) ->
    ebt__do([ebt__error_m ||
        Parent <- outdir(Config),
        Dir <- return(filename:absname(ebt__xl_string:join([Parent, Kind], "/"))),
        ebt__xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(outdir(atom(), config(), string()) -> ebt__error_m:monad(string())).
outdir(Kind, Config, Suffix) ->
    ebt__do([ebt__error_m ||
        Parent <- outdir(Kind, Config),
        Dir <- return(ebt__xl_string:join([Parent, Suffix], "/")),
        ebt__xl_file:mkdirs(Dir),
        return(Dir)
    ]).

-spec(app_outdir(atom(), file:name(), config()) -> ebt__error_m:monad(string())).
app_outdir(Kind, Dir, Config) ->
    ebt__do([ebt__error_m ||
        App <- appname_full(Dir, Config),
        outdir(Kind, Config, App)
    ]).

-spec(info_outdir(file:name(), config()) -> ebt__error_m:monad(string())).
info_outdir(Dir, Config) ->
    ebt__do([ebt__error_m ||
        AppProdDir <- app_outdir(production, Dir, Config),
        InfoDir <- return(ebt__xl_string:join([AppProdDir, ".ebt-info"], "/")),
        ebt__xl_file:mkdirs(InfoDir),
        return(InfoDir)
    ]).

-spec(version(config()) -> ebt__error_m:monad(string())).
version(Config) ->
    case ebt__xl_lists:kvfind(version, Config) of
        {ok, {shell, Cmd}} -> ebt__xl_shell:command(Cmd);
        _ -> {ok, "0.0.1"}
    end.

-spec(build_number(config()) -> ebt__error_m:monad(string())).
build_number(Config) ->
    case ebt__xl_lists:kvfind(build, Config) of
        {ok, {shell, Cmd}} -> ebt__xl_shell:command(Cmd);
        _ -> {ok, "0"}
    end.

-spec(appname_full(file:name(), config()) -> ebt__error_m:monad(string())).
appname_full(Dir, Config) ->
    ebt__do([ebt__error_m ||
        {_, Name, _} <- ebt_applib:load(Dir ++ "/src"),
        Version <- ebt_config:version(Config),
        return(ebt__xl_string:join([Name, Version], "-"))
    ]).

-spec(appname(file:name()) -> ebt__error_m:monad(string())).
appname(Dir) ->
    ebt__do([ebt__error_m ||
        {_, Name, _} <- ebt_applib:load(Dir ++ "/src"),
        return(atom_to_list(Name))
    ]).

