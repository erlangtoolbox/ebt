-module(ebt).

-compile({parse_transform, do}).

-export([main/1]).

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
