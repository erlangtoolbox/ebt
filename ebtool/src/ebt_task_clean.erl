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
%% @doc Clean
%%
%% == Configuration ==
%% List of wildcards to clean up.
%%
%% == Example ==
%% <pre>
%% {clean, ["*.fprof"]}
%% </pre>
-module(ebt_task_clean).

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, _Dir, Config) ->
    do([error_m ||
        OutDir <- ebt_config:outdir(Config),
        xl_lists:eforeach(fun(Path) ->
            io:format("delete ~s~n", [Path]),
            xl_file:delete(Path)
        end, xl_file:wildcards(lists:map(fun
            (outdir) -> OutDir;
            (X) -> X
        end, ebt_config:value(Target, Config, [OutDir])))),
        return(Config)
    ]).
