-module(ebt).

-compile({parse_transform, do}).

-export([main/1, load_libraries/1, load_library/1]).

-define(OPTS, [
    {outdir, $o, outdir, {string, "out"}, "output directory"}
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

build([{outdir, OutDir}]) ->
    Defaults = [{outdir, filename:absname(OutDir)}],
    case build(".", Defaults) of
        {ok, _} ->
            io:format("BUILD SUCCESSFUL!~n");
        {error, E} ->
            io:format(standard_error, "BUILD FAILED: ~p~n", [E]),
            halt(1)
    end.

-spec build/2 :: (file:name(), ebt_config:config()) -> error_m:monad(any()).
build(ContextDir, Defaults) ->
    ConfigFile = filename:join(ContextDir, "ebt.config"),
    do([error_m ||
        Config <- ebt_config:read(ConfigFile, Defaults),
        OutDir <- ebt_config:outdir(Config),
        ebt_task:perform(prepare, ebt_config:value(targets, Config, prepare, []), ContextDir, Config),
        ebt_strikead_lists:eforeach(
            fun(Dir) ->
                io:format("==> entering ~s~n", [Dir]),
                {Status, {_, Stdout}} = ebt_strikead_shell:command(
                    filename:absname(escript:script_name()) ++ " -o " ++ OutDir, Dir),
                io:format("~s", [Stdout]),
                io:format("==> leaving ~s~n", [Dir]),
                case Status of
                    ok -> ok;
                    error -> {error, "build in directory " ++ Dir ++ " failed"}
                end
            end,
            ebt_config:value(subdirs, Config, [])
        ),
        ebt_task:perform(perform, ebt_config:value(targets, Config, perform, ['otp-app']), ContextDir, Config)
    ]).

-spec load_libraries/1 :: (ebt_config:config()) -> [file:name()].
load_libraries(Config) ->
    ebt_strikead_lists:eforeach(fun load_library/1, [Lib ||
        LibDir <- ebt_config:value(libraries, Config, []),
        Lib <- filelib:wildcard(LibDir ++ "/*")]).

-spec load_library/1 :: (file:name()) -> error_m:monad(ok).
load_library(Path) ->
    case code:add_patha(filename:join(Path, "ebin")) of
        true -> ok;
        {error, bad_directory} ->
            io:format("failed to load ~s~n", [Path]),
            ok; % ignore
        {error, E} -> {error, {load_library, E, Path}}
    end.
