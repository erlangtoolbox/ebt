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
-module(ebt_task_build_plt).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).
%% API
-export([perform/3, initial_plt_path/2]).

perform(Target, _Dir, Config) ->
    ebt__do([ebt__error_m ||
        Plt <- initial_plt_path(Target, Config),
        case ebt__xl_file:exists(Plt) of
            {ok, false} ->
                io:format("building PLT: ~s~n", [Plt]),
                Options = [{analysis_type, plt_build}, {output_plt, Plt} |
                    ebt_config:value(Target, Config, options, [{apps, [kernel, stdlib]}])],
                ebt_task_dialyze:display_warnings(dialyzer:run(Options));
            {ok, true} -> io:format("PLT is already built: ~s~n", [Plt]);
            E -> E
        end
    ]).

initial_plt_path(Target, Config) ->
    ebt__do([ebt__error_m ||
        OutDir <- ebt_config:outdir(dialyzer, Config),
        return(ebt_config:value(Target, Config, plt_path, filename:join(OutDir, "erlang.plt")))
    ]).

