-module(ebt_task_compile).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/3]).

-spec perform/3 :: (file:name(), ebt_config:config(), ebt_config:defaults()) ->
    error_m:monad(ok).
perform(Dir, Config, Defaults) ->
    SrcDir = Dir ++ "/src",
    TestDir = Dir ++ "/test",
    do([error_m ||
        AppProdDir <- ebt_config:app_production_outdir(Dir, Config, Defaults),
        EbinProdDir <- return(AppProdDir ++ "/ebin"),
        compile(SrcDir, EbinProdDir, Config),
        AppSpec <- ebt_applib:load(Dir),
        update_app(AppSpec, EbinProdDir, Config),
        strikead_file:copy_if_exists(Dir ++ "/include", AppProdDir),
        strikead_file:copy_if_exists(Dir ++ "/priv", AppProdDir),
        strikead_file:copy_if_exists(Dir ++ "/bin", AppProdDir),
        copy_resources(SrcDir,
            ebt_config:value(compile, Config, resources, []), EbinProdDir),
        AppTestDir <- ebt_config:app_test_outdir(Dir, Config, Defaults),
        EbinTestDir <- return(AppTestDir ++ "/ebin"),
        case strikead_file:exists(Dir ++ "/test") of
            {ok, true} ->
                do([error_m ||
                    compile(TestDir, EbinTestDir, Config),
                    copy_resources(TestDir,
                        ebt_config:value(compile, Config, resources, []), EbinTestDir)
                ]);
            {ok, false} -> ok;
            E -> E
        end
    ]).

-spec update_app/3 :: (application:application_spec(), file:name(), ebt_config:config()) ->
    error_m:monad(ok).
update_app(AppSpec = {_, App, _}, EbinProdDir, Config) ->
    Filename = strikead_string:join([EbinProdDir, "/", App, ".app"], ""),
    Modules = [list_to_atom(filename:basename(F, ".beam")) ||
        F <- filelib:wildcard(EbinProdDir ++ "/*.beam")],
    {ok, Version} = ebt_config:version(Config),
    strikead_file:write_terms(Filename,
            ebt_applib:update(AppSpec, [{modules, Modules}, {vsn, Version}])).

-spec compile/3 :: (file:name(), file:name(), ebt_config:config()) -> error_m:monad(ok).
compile(SrcDir, OutDir, Config) ->
    do([error_m ||
        ebt:load_libraries(Config),
        io:format("compiling ~s to ~s~n", [SrcDir, OutDir]),
        Includes <- return([{i, Lib} || Lib <- ebt_config:value(libraries, Config, [])]),
        Flags <- return(ebt_config:value(compile, Config, flags, []) ++ Includes),
        strikead_file:mkdirs(OutDir),
        case make:files(filelib:wildcard(SrcDir ++ "/*.erl"), [{outdir, OutDir}, {i, SrcDir ++ "/../include"} | Flags]) of
            up_to_date -> io:format("...compiled~n");
            error -> {error, "Compilation failed!"}
        end
    ]).

-spec copy_resources(file:name(), [string()], file:name()) -> error_m:monad(ok).
copy_resources(BaseDir, Wildcards, DestDir) ->
    strikead_lists:eforeach(fun(F) ->
        io:format("copy ~s to ~s~n", [F, DestDir]),
        strikead_file:copy(F, DestDir)
    end, [F || WC <- Wildcards, F <- filelib:wildcard(BaseDir ++ "/" ++ WC)]).
