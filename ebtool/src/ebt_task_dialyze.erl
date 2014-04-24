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
%% @doc Dialyze
%%
%% == Configuration ==
%% <ul>
%% <li>plt_path - initial PLT path</li>
%% <li>files - include/exclude files</li>
%% <li>options - dialyzer options</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {dialyze, [
%%     {plt_path, "out/dislyzer/erlang.plt"},
%%     {options, [{warnings, [error_handling, race_conditions, unmatched_returns]}]
%%     {files, [
%%          {include, ["src/*.erl"]},
%%          {exclude, []}
%%     ]},
%% ]}
%% </pre>
-module(ebt_task_dialyze).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-export([perform/3, display_warnings/1]).

perform(Target, Dir, Config) ->
    do([error_m ||
        Plt <- ebt_task_build_plt:initial_plt_path(Target, Config),
%%         AppPlt <- app_plt_path(Dir, Config),
        Includes <- case xl_file:exists(Dir ++ "/include") of
            {ok, true} -> {ok, [{include_dirs, [Dir ++ "/include"]}]};
            {ok, false} -> {ok, []};
            E -> E
        end,
        display_warnings(dialyzer:run(Includes ++ [
            {init_plt, Plt},
            {from, src_code},
            {files, ebt_config:files(Target, Config, ["src/*.erl"], [])} |
            ebt_config:value(Target, Config, options, [
                {warnings, [error_handling, race_conditions, unmatched_returns]}
            ])
        ])),
        return(Config)
    ]).

display_warnings([]) -> ok;
display_warnings(Warnings) ->
    lists:foreach(fun(Warning) -> io:format(dialyzer:format_warning(Warning)) end, Warnings).

%% app_plt_path(Dir, Config) ->
%%     do([error_m ||
%%         OutDir <- ebt_config:app_outdir(dialyzer, Dir, Config),
%%         App <- ebt_config:appname(Dir),
%%         return(xl_string:join([OutDir, "/", App, ".plt"]))
%%     ]).
%%
%%
