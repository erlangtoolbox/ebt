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

-compile({parse_transform, ebt__do}).

-export([perform/3, display_warnings/1]).

perform(Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        Plt <- ebt_task_build_plt:initial_plt_path(Target, Config),
%%         AppPlt <- app_plt_path(Dir, Config),
        Includes <- case ebt__xl_file:exists(Dir ++ "/include") of
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
%%     ebt__do([ebt__error_m ||
%%         OutDir <- ebt_config:app_outdir(dialyzer, Dir, Config),
%%         App <- ebt_config:appname(Dir),
%%         return(ebt__xl_string:join([OutDir, "/", App, ".plt"]))
%%     ]).
%%
%%
