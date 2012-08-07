-module(ebt).

-compile({parse_transform, do}).

-export([main/1, load_libraries/1, load_library/1]).

-define(OPTS, [
    {output, $o, outdir, {string, "out"}, "output directory"}
]).
-spec main/1 :: ([string()]) -> ok.
main(Args) ->
    ok = application:load(ebt),
    case getopt:parse(?OPTS, Args) of
        {ok, {Opts, _}} -> build(Opts);
        {error, X} ->
            io:format(standard_error, "~p~n", [X]),
            getopt:usage(?OPTS, "ebt"),
            halt(2)
    end.

build([{output, OutDir}]) ->
    Defaults = [{output, filename:absname(OutDir)}],
    case build(".", Defaults) of
        {ok, _} ->
            io:format("BUILD SUCCESSFUL!~n");
        {error, E} ->
            io:format(standard_error, "BUILD FAILED: ~p~n", [E]),
            halt(1)
    end.

-spec build/2 :: (file:name(), strikead_lists:kvlist_at()) -> error_m:monad(any()).
build(ContextDir, Defaults) ->
    ConfigFile = filename:join(ContextDir, "build.ebt"),
    do([error_m ||
        Config <- ebt_config:read(ConfigFile),
        build(ContextDir, Config, Defaults)
    ]).

-spec build/3 :: (file:name(), ebt_config:config(), strikead_lists:kvlist_at()) ->
    error_m:monad(any()).
build(ContextDir, Config, Defaults) ->
    do([error_m ||
        OutDir <- ebt_config:outdir(Config, Defaults),
        ebt_task:perform(prepare, ebt_config:value(targets, Config, prepare, []), ContextDir, Config, Defaults),
        strikead_lists:eforeach(
            fun(Dir) ->
                io:format("==> entering ~s~n", [Dir]),
                {ExitCode, Stdout} = eunit_lib:command(
                    filename:absname(escript:script_name()) ++ " -o " ++ OutDir, Dir),
                io:format(Stdout),
                io:format("==> leaving ~s~n", [Dir]),
                case ExitCode of
                    0 -> ok;
                    _ -> {error, "build in directory " ++ Dir ++ " failed"}
                end
            end,
            ebt_config:value(subdirs, Config, [])
        ),
        ebt_task:perform(perform, ebt_config:value(targets, Config, perform, ['otp-app']), ContextDir, Config, Defaults)
    ]).

-spec load_libraries/1 :: (ebt_config:config()) -> [file:name()].
load_libraries(Config) ->
    strikead_lists:eforeach(fun load_library/1, [Lib ||
        LibDir <- ebt_config:value(libraries, Config, []),
        Lib <- filelib:wildcard(LibDir ++ "/*")]).

-spec load_library/1 :: (file:name()) -> error_m:monad(ok).
load_library(Path) ->
    case code:add_path(filename:join(Path, "ebin")) of
        true -> ok;
        {error, bad_directory} -> ok; % ignore
        {error, E} -> {error, {load_library, E, Path}}
    end.
