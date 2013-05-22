%%  Copyright (c) 2012-2013
%%  StrikeAd LLC http://www.strikead.com
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
%%      Neither the name of the StrikeAd LLC nor the names of its
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

-module(ebt).

-compile({parse_transform, do}).

-export([main/1, load_libraries/1, load_library/1]).

-define(OPTS, [
    {outdir, $o, outdir, {string, "out"}, "output directory"},
    {profile, $p, profile, {atom, default}, "build profile"}
]).
-spec(main([string()]) -> ok).
main(Args) ->
    R = do([error_m ||
        xl_application:start(ebt),
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
    ]),
    case R of
        {error, X} ->
            io:format(standard_error, "~p~n", [X]),
            halt(1);
        _ -> ok
    end.

build(Opts) ->
    {ok, OutDir} = xl_lists:kvfind(outdir, Opts),
    {ok, Profile} = xl_lists:kvfind(profile, Opts),
    Defaults = [{outdir, filename:absname(OutDir)}],
    case build(Profile, ".", Defaults) of
        {ok, _} ->
            {ok, "BUILD SUCCESSFUL"};
        {error, E} when is_list(E) ->
            {error, xl_string:format("BUILD FAILED: ~s~n", [E])};
        {error, E} ->
            {error, xl_string:format("BUILD FAILED: ~p~n", [E])}
    end.

-spec(build(atom(), file:name(), ebt_config:config()) -> error_m:monad(any())).
build(Profile, ContextDir, Defaults) ->
    io:format("==> build profile: ~p~n", [Profile]),
    ConfigFile = filename:join(ContextDir, "ebt.config"),
    do([error_m ||
        Config <- ebt_config:read(ConfigFile, Defaults),
        OutDir <- ebt_config:outdir(Config),
        ProfileConfig <- return(ebt_config:value(profiles, Config, Profile, ebt_config:value(profiles, Config, default, []))),
        ebt_task:perform(prepare, xl_lists:kvfind(prepare, ProfileConfig, []), ContextDir, Config),
        xl_lists:eforeach(
            fun(Dir) ->
                io:format("==> entering ~s~n", [Dir]),
                Result = ebt_cmdlib:exec({"~s -o ~p -p ~s", [filename:absname(escript:script_name()), OutDir, Profile]}, Dir),
                io:format("==> leaving ~s~n", [Dir]),
                case Result of
                    ok -> ok;
                    {error, _} -> {error, "build in directory " ++ Dir ++ " failed"}
                end
            end,
            ebt_config:value(subdirs, Config, [])
        ),
        ebt_task:perform(perform, xl_lists:kvfind(perform, ProfileConfig, [package]), ContextDir, Config)
    ]).

-spec(load_libraries(ebt_config:config()) -> [file:name()]).
load_libraries(Config) ->
    LibMasks = lists:map(fun(LibDir) -> LibDir ++ "/*" end, ebt_config:value(libraries, Config, [])),
    SortedLibs = lists:sort(fun compare/2, lists:map(fun libinfo/1, xl_file:wildcards(LibMasks))),
    Libs = lists:foldl(fun(L = {_, Name, _}, Libraries) ->
        case lists:keymember(Name, 2, Libraries) of
            false -> [L | Libraries];
            true -> Libraries
        end
    end, [], SortedLibs),
    xl_lists:eforeach(fun load_library/1, [Lib || Lib <- Libs]).

-spec(load_library(file:name() | {file:name(), string(), string()}) -> error_m:monad(ok)).
load_library({Dir, Name, Version}) -> load_library(Dir ++ "/" ++ Name ++ "-" ++ xl_string:join(Version, "."));
load_library(Path) ->
    case code:add_patha(filename:join(Path, "ebin")) of
        true -> ok;
        {error, bad_directory} -> io:format("WARNING: failed to load ~s~n", [Path])
    end.

libinfo(Path) ->
    AbsPath = xl_file:absolute(Path),
    Dir = filename:dirname(AbsPath),
    LibName = filename:basename(AbsPath),
    [Name, Version] = string:tokens(LibName, "-"),
    VersionList = string:tokens(Version, "."),
    {Dir, Name, VersionList}.

compare({_, Name1, Version1}, {_, Name2, Version2}) ->
    case Name1 of
        Name2 -> Version1 > Version2;
        _ -> Name1 > Name2
    end.