-module(ebt).

-compile({parse_transform, do}).

-export([main/1, load_libraries/1, load_library/1]).

-define(OPTS, [
    {outdir, $o, outdir, {string, "out"}, "output directory"}
]).
-spec main/1 :: ([string()]) -> ok.
main(Args) ->
    do([error_m ||
        application:load(ebt),
        Vsn <- application:get_key(ebt, vsn),
        io:format("Erlang Build Tool, v.~s~n", [Vsn]),
        {Opts, _} <- getopt:parse(?OPTS, Args),
        case build(Opts) of
            {error, X} ->
                io:format(standard_error, "~s~n", [X]),
                halt(1);
            {ok, X} ->
                io:format("~s~n", [X])
        end
    ]).

build([{outdir, OutDir}]) ->
    Defaults = [{outdir, filename:absname(OutDir)}],
    case build(".", Defaults) of
        {ok, _} ->
            {ok, "BUILD SUCCESSFUL"};
        {error, E} when is_list(E) ->
            {error, ebt_xl_string:format("BUILD FAILED: ~s~n", [E])};
        {error, E} ->
            {error, ebt_xl_string:format("BUILD FAILED: ~p~n", [E])}
    end.

-spec build/2 :: (file:name(), ebt_config:config()) -> error_m:monad(any()).
build(ContextDir, Defaults) ->
    ConfigFile = filename:join(ContextDir, "ebt.config"),
    do([error_m ||
        Config <- ebt_config:read(ConfigFile, Defaults),
        OutDir <- ebt_config:outdir(Config),
        ebt_task:perform(prepare, ebt_config:value(targets, Config, prepare, []), ContextDir, Config),
        ebt_xl_lists:eforeach(
            fun(Dir) ->
                io:format("==> entering ~s~n", [Dir]),
                {Status, {_, Stdout}} = ebt_xl_shell:command(
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
        ebt_task:perform(perform, ebt_config:value(targets, Config, perform, [otpapp]), ContextDir, Config)
    ]).

-spec load_libraries/1 :: (ebt_config:config()) -> [file:name()].
load_libraries(Config) ->
    ebt_xl_lists:eforeach(fun load_library/1, [Lib ||
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
