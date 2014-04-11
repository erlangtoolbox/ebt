%%  The MIT License (MIT)
%%
%%  Copyright (c) 2014 Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%
%%  Permission is hereby granted, free of charge, to any person obtaining a copy of
%%  this software and associated documentation files (the "Software"), to deal in
%%  the Software without restriction, including without limitation the rights to
%%  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
%%  the Software, and to permit persons to whom the Software is furnished to do so,
%%  subject to the following conditions:
%%
%%  The above copyright notice and this permission notice shall be included in all
%%  copies or substantial portions of the Software.
%%
%%  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
%%  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
%%  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
%%  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%%  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(ebtool).

-compile({parse_transform, do}).

-export([main/1, load_libraries/1, load_library/1]).

-define(OPTS, [
    {file, $f, file, {string, "ebt.config"}, "config file"},
    {outdir, $o, outdir, {string, "out"}, "output directory"},
    {profile, $p, profile, {atom, default}, "build profile"},
    {define, $D, define, string, "define parameters"}
]).
-spec(main([string()]) -> ok).
main(Args) ->
    ebt_tty:initialize(),
    try do([error_m ||
        xl_application:start(ebtool),
        Vsn <- application:get_key(ebtool, vsn),
        ebt_tty:format("Erlang Build Tool, v.~s~n", [Vsn]),
        ebt_tty:format("Erlang ~p~n", [erlang:system_info(otp_release)]),
        {Opts, _} <- getopt:parse(?OPTS, Args),
        case build(Opts) of
            {error, X} when is_list(X) ->
                ebt_tty:format(standard_error, "~s~n", [X]),
                halt(1);
            {error, X} ->
                ebt_tty:format(standard_error, "~p~n", [X]),
                halt(1);
            {ok, X} ->
                ebt_tty:format("~s~n", [X])
        end
    ]) of
        {error, X} ->
            ebt_tty:format(standard_error, "~p~n", [X]),
            halt(1);
        _ -> ok
    catch
        _:X ->
            ebt_tty:format(standard_error, "~p~n~p~n", [X, erlang:get_stacktrace()]),
            halt(1)
    end.

build(Opts) ->
    {ok, OutDir} = xl_lists:kvfind(outdir, Opts),
    {ok, Profile} = xl_lists:kvfind(profile, Opts),
    {ok, EbtConfig} = xl_lists:kvfind(file, Opts),
    Defines = lists:map(fun({define, X}) ->
        case string:tokens(X, "=") of
            [K, V] -> {define, list_to_atom(K), V};
            [K] -> {define, list_to_atom(K), true}
        end
    end, xl_lists:keyfilter(1, define, Opts)),
    Defaults = [{outdir, filename:absname(OutDir)}],
    case build(Profile, ".", Defaults, Defines, EbtConfig) of
        {ok, _} -> {ok, "BUILD SUCCESSFUL"};
        {error, E} when is_list(E) -> {error, xl_string:format("BUILD FAILED: ~s~n", [E])};
        {error, E} -> {error, xl_string:format("BUILD FAILED: ~p~n", [E])}
    end.

-spec(build(atom(), file:name(), ebt_config:config(), [{define, atom(), term()}], file:name()) -> error_m:monad(any())).
build(Profile, ContextDir, Defaults, Defines, EbtConfig) ->
    ebt_tty:format("==> build file: ~s, profile: ~p~n", [EbtConfig, Profile]),
    ConfigFile = filename:join(ContextDir, EbtConfig),
    do([error_m ||
        Config <- ebt_config:read(ConfigFile, Defaults, Defines),
        OutDir <- ebt_config:outdir(Config),
        ProfileConfig <- return(ebt_config:value(profiles, Config, Profile, ebt_config:value(profiles, Config, default, []))),
        {_, PreparedConfig} <- ebt_task:perform(prepare, xl_lists:kvfind(prepare, ProfileConfig, []), ContextDir, Config),
        xl_lists:eforeach(
            fun(Dir) ->
                Definitions = xl_string:join(lists:map(fun({K, V}) ->
                    "-D'" ++ xl_string:join([K, V], "=") ++ "'"
                end, ebt_config:definitions(PreparedConfig)), " "),
                ebt_tty:format("==> entering ~s~n", [Dir]),
                Result = ebt_cmdlib:exec({"~s -f ~p -o ~p -p ~s ~s", [filename:absname(escript:script_name()), EbtConfig, OutDir, Profile, Definitions]}, Dir),
                ebt_tty:format("==> leaving ~s~n", [Dir]),
                case Result of
                    ok -> ok;
                    {error, _} -> {error, "build in directory " ++ Dir ++ " failed"}
                end
            end,
            xl_lists:kvfind(subdirs, ProfileConfig, [])
        ),
        ebt_task:perform(perform, xl_lists:kvfind(perform, ProfileConfig, [package]), ContextDir, PreparedConfig)
    ]).

-spec(load_libraries(ebt_config:config()) -> [file:name()]).
load_libraries(Config) ->
    lists:foreach(fun load_library/1, [Lib || Lib <- ebt_config:libraries(Config)]),
    code:rehash().

-spec(load_library(file:name() | {file:name(), string(), string()}) -> error_m:monad(ok)).
load_library(Path) ->
    EbinPath = filename:join(Path, "ebin"),
    case code:add_patha(EbinPath) of
        true -> ok;
        {error, bad_directory} -> io:format("WARNING: failed to load ~s~n", [EbinPath])
    end.