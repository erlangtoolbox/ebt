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
%% @doc Build PLT
%%
%% == Configuration ==
%% <ul>
%% <li>plt_path - path to PLT</li>
%% <li>options - building plt options</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {build_plt, [
%%     {plt_path, "out/dislyzer/erlang.plt"},
%%     {options, [{apps, [kernel, stdlib]}]}
%% ]}
%% </pre>
-module(ebt_task_build_plt).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-export([perform/3, initial_plt_path/2]).

perform(Target, _Dir, Config) ->
    do([error_m ||
        Plt <- initial_plt_path(Target, Config),
        case xl_file:exists(Plt) of
            {ok, false} ->
                io:format("build PLT: ~s~n", [Plt]),
                Options = [{analysis_type, plt_build}, {output_plt, Plt} |
                    ebt_config:value(Target, Config, options, [{apps, [kernel, stdlib]}])],
                ebt_task_dialyze:display_warnings(dialyzer:run(Options));
            {ok, true} -> io:format("PLT is already built: ~s~n", [Plt]);
            E -> E
        end,
        return(Config)
    ]).

initial_plt_path(Target, Config) ->
    do([error_m ||
        OutDir <- ebt_config:outdir(dialyzer, Config),
        return(ebt_config:value(Target, Config, plt_path, filename:join(OutDir, "erlang.plt")))
    ]).

