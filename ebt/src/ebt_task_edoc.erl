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
-module(ebt_task_edoc).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, ebt__do}).
%% API
-export([perform/3]).

perform(Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        App <- ebt_config:appname(Dir),
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        DocDir <- return(filename:join(ProdDir, "doc")),
        ebt__xl_file:copy_if_exists("doc", ProdDir),
        OverviewPath <- return(filename:join(DocDir, "overview.edoc")),
        HasOverview <- ebt__xl_file:exists(OverviewPath),
        case HasOverview of
            true -> ebt_task_template:substitute_file(Config, OverviewPath, OverviewPath, [], {${, $}});
            _ -> ok
        end,
        FileMasks <- return(["src/*.erl" | ebt_config:value(Target, Config, files, [])]),
        ExcludeMasks <- return(ebt_config:value(Target, Config, exclude, [])),
        Files <- return(lists:subtract(ebt__xl_file:wildcards(FileMasks), ebt__xl_file:wildcards(ExcludeMasks))),
        edoc:run([], Files, [{dir, DocDir}, {application, App} | ebt_config:value(Target, Config, [])])
    ]).
