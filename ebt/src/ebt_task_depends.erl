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

%% @doc Dependencies configuration
%%
%% == Configuration ==
%% <ul>
%% <li>dir - downloaded libraries goes there</li>
%% <li>repositaries - repositary configuration</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {depends, [
%%     {dir, "./lib"},
%%     {repositories, [
%%         {"http://erlang-build-tool.googlecode.com/files", [
%%             {erlandox, "1.0.5"},
%%             {xl_stdlib, "1.2.0.301"},
%%             {xl_io, "1.2.0.301"},
%%             {getopt, "0.7.1"}
%%         ]}
%%     ]}
%% ]}
%% </pre>
-module(ebt_task_depends).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, _Dir, Config) ->
    DepsDir = ebt_config:value(Target, Config, dir, "./lib"),
    inets:start(),
    do([error_m ||
        xl_file:mkdirs(DepsDir),
        xl_lists:eforeach(fun({Url, Libs}) ->
            xl_lists:eforeach(fun({Name, Version}) ->
                Lib = xl_string:join([Name, "-", Version, ".ez"]),
                LocalFile = filename:join(DepsDir, Lib),
                case xl_file:exists(xl_string:join([DepsDir, "/", Name, "-", Version])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        do([error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [], [{stream, LocalFile}]),
                            xl_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            xl_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(Target, Config, repositories, []))
    ]).


