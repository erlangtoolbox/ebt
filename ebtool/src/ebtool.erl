%%  Copyright (c) 2012-2013 StrikeAd LLC http://www.strikead.com
%%  Copyright (c) 2012-2014 Vladimir Kirichenko vladimir.kirichenko@gmail.com
%%
%%  All rights reserved.
%%
%%  Redistribution and use in source and binary forms, with or without
%%  modification, are permitted provided that the following conditions are met:
%%
%%      Redistributions of source code must retain the above copyright
%%  notice, this list of conditions and the following disclaimer.
%%      Redistributions in binary form must reproduce the above copyright
%%  notice, this list of conditions and the following disclaimer in the
%%  documentation and/or other materials provided with the distribution.
%%      Neither the name of the EBT nor the names of its
%%  contributors may be used to endorse or promote products derived from
%%  this software without specific prior written permission.
%%
%%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
%%  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
%%  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
%%  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%%  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
%%  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
%%  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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