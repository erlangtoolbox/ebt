-module(ebt_task_compile).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/3]).

-spec perform/3 :: (file:name(), ebt_config:config(), ebt_config:defaults()) ->
    error_m:monad(ok).
perform(Dir, Config, Defaults) ->
    load_libs(Config),
    SrcDir = Dir ++ "/src",
    do([error_m ||
        AppOutDir <- ebt_config:app_production_outdir(Dir, Config, Defaults),
        OutDir <- return(AppOutDir ++ "/ebin"),
        strikead_file:mkdirs(OutDir),
        compile(SrcDir, OutDir, ebt_config:value(compile, Config, flags, [])),
        AppSpec <- ebt_applib:load(Dir),
        update_app(AppSpec, OutDir),
        strikead_file:copy_if_exists(Dir ++ "/include", AppOutDir),
        strikead_file:copy_if_exists(Dir ++ "/priv", AppOutDir),
        strikead_file:copy_if_exists(Dir ++ "/bin", AppOutDir)
    ]).

-spec load_libs/1 :: (ebt_config:config()) -> ok.
load_libs(Config) ->
    Libs = ebt_config:value(libs, Config, []),
    [code:add_path(Dir ++ "/ebin") || Dir <- Libs],
    ok.

-spec update_app/2 :: ({application, atom(), strikead_lists:kvlist_at()}, file:name()) ->
    error_m:monad(ok).
update_app(AppSpec = {_, App, _}, OutDir) ->
    Filename = strikead_string:join([OutDir, "/", App, ".app"], ""),
    Modules = [list_to_atom(filename:basename(F, ".beam")) ||
        F <- filelib:wildcard(OutDir ++ "/*.beam")],
    strikead_file:write_terms(Filename,
            ebt_applib:update(AppSpec, {modules, Modules})).

-spec compile/3 :: (file:name(), file:name(), [any()]) -> error_m:monad(ok).
compile(SrcDir, OutDir, Flags) ->
    case make:files(filelib:wildcard(SrcDir ++ "/*.erl"), [{outdir, OutDir}, {i, SrcDir ++ "/../include"} | Flags]) of
        up_to_date -> io:format("...compiled~n");
        error -> {error, "Compilation failed!"}
    end.
