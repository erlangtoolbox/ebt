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
%%             {getopt, "0.7.1"}
%%         ]}
%%     ]}
%% ]}
%% </pre>
-module(ebt_task_depends).

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, _Dir, Config) ->
    DepsDir = ebt_config:value(Target, Config, dir, "./lib"),
    Escape = ebt_config:value(Target, Config, escape, ''),
    inets:start(),
    do([error_m ||
        xl_file:mkdirs(DepsDir),
        xl_lists:eforeach(fun({Url, Libs}) ->
            xl_lists:eforeach(fun({Name, Version}) ->
                LibName = xl_string:join([Name, "-", Version]),
                Lib = xl_string:join([LibName, ".ez"]),
                case xl_file:exists(xl_string:join([DepsDir, "/", Escape, LibName])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        LocalFile = filename:join(DepsDir, Lib),
                        do([error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [], [{stream, LocalFile}]),
                            xl_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            xl_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(Target, Config, repositories, [])),
        return(Config)
    ]).
