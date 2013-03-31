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
-module(ebt_task_rpm_depends).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).

perform(Target, _Dir, Config) ->
    ebt__do([ebt__error_m ||
        RPMBuildDir <- ebt_config:outdir(rpmbuild, Config),
        ebt_rpmlib:prepare_environment(RPMBuildDir),
        LibDir <- ebt_config:find_value(Target, Config, dir),
        Libs <- return(filelib:wildcard(LibDir ++ "/*")),
        ebt__xl_lists:eforeach(fun(Lib) ->
            build_rpm(Config, filename:absname(Lib))
        end, Libs)
    ]).

build_rpm(Config, Lib) ->
    SpecTemplatePath = Lib ++ "/.ebt-info/rpm.spec",
    case ebt__xl_file:exists(SpecTemplatePath) of
        {ok, true} ->
            AppName = filename:basename(Lib),
            ebt__do([ebt__error_m ||
                SpecFile <- ebt_task_rpm_spec:spec_file_path(Config, AppName),
                ebt_task_rpm_spec:generate_spec_for_build(Config, SpecTemplatePath, AppName, Lib),
                ebt_task_rpm:rpmbuild(SpecFile)
            ]);
        {ok, false} -> io:format("ignore: no ~p~n", [SpecTemplatePath]);
        E -> E
    end.
