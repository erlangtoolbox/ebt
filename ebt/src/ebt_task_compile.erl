-module(ebt_task_compile).

-compile({parse_transform, do}).

-behaviour(ebt_task).

-export([perform/3]).

-spec perform/3 :: (atom(), file:name(), ebt_config:config()) ->
    error_m:monad(ok).
perform(Target, Dir, Config) ->
    SrcDir = Dir ++ "/src",
    TestDir = Dir ++ "/test",
    do([error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        EbinProdDir <- return(AppProdDir ++ "/ebin"),
        compile(Target, SrcDir, EbinProdDir, Config),
        AppSpec <- ebt_applib:load(Dir),
        update_app(AppSpec, EbinProdDir, Config),
        ebt_xl_file:copy_if_exists(Dir ++ "/include", AppProdDir),
        ebt_xl_file:copy_if_exists(Dir ++ "/priv", AppProdDir),
        ebt_xl_file:copy_if_exists(Dir ++ "/bin", AppProdDir),
        ebt_xl_file:copy_filtered(SrcDir,
            ebt_config:value(Target, Config, resources, []), EbinProdDir),
        AppTestDir <- ebt_config:app_outdir(test, Dir, Config),
        EbinTestDir <- return(AppTestDir ++ "/ebin"),
        HasTests <- ebt_xl_file:exists(Dir ++ "/test"),
        case HasTests of
            true ->
                do([error_m ||
                    compile(Target, TestDir, EbinTestDir, Config),
                    ebt_xl_file:copy_filtered(TestDir,
                        ebt_config:value(Target, Config, resources, []), EbinTestDir)
                ]);
            false -> ok
        end
    ]).

-spec update_app/3 :: (application:application_spec(), file:name(), ebt_config:config()) ->
    error_m:monad(ok).
update_app(AppSpec = {_, App, _}, EbinProdDir, Config) ->
    Filename = ebt_xl_string:join([EbinProdDir, "/", App, ".app"], ""),
    Modules = [list_to_atom(filename:basename(F, ".beam")) ||
        F <- filelib:wildcard(EbinProdDir ++ "/*.beam")],
    {ok, Version} = ebt_config:version(Config),
    ebt_xl_file:write_terms(Filename,
        ebt_applib:update(AppSpec, [{modules, Modules}, {vsn, Version}])).

-spec compile/4 :: (atom(), file:name(), file:name(), ebt_config:config()) ->
    error_m:monad(ok).
compile(Target, SrcDir, OutDir, Config) ->
    do([error_m ||
        io:format("compiling ~s to ~s~n", [SrcDir, OutDir]),
        Includes <- return([{i, Lib} || Lib <- ebt_config:value(libraries, Config, [])]),
        Flags <- return(ebt_config:value(Target, Config, flags, []) ++ Includes),
        ebt_xl_file:mkdirs(OutDir),
        FirstFiles <- return(lists:filter(fun(F) ->
            ebt_xl_file:exists(F) == {ok, true}
        end, [SrcDir ++ "/" ++ F || F <- ebt_config:value(Target, Config, first, [])])),
        compile(FirstFiles, SrcDir, Flags, OutDir, Config),
        compile(filelib:wildcard(SrcDir ++ "/*.erl"), SrcDir, Flags, OutDir, Config)
    ]).

compile([], _SrcDir, _Flags, _OutDir, _Config) -> ok;
compile(Files, SrcDir, Flags, OutDir, Config) ->
    do([error_m ||
        ebt:load_libraries(Config),
        case make:files(Files, [{outdir, OutDir}, {i, SrcDir ++ "/../include"} | Flags]) of
            up_to_date -> io:format("...compiled~n");
            error -> {error, "Compilation failed!"}
        end
    ]).