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

%% @doc Compile NIF/Port
%%
%% == Configuration ==
%% <ul>
%% <li>output_format - one supported by graphviz. default is png</li>
%% <li>options - additional options</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {graphviz, [
%%   {output_format, png}
%% ]}
%% </pre>
-module(ebt_task_graphviz).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    OutputFormat = ebt_config:value(Target, Dir, output_format, png),
    Options = ebt_config:value(Target, Dir, options, ""),
    Files = ebt_config:files(Target, Config, [], []),
    ebt__do([ebt__error_m ||
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        DocDir <- return(filename:join(ProdDir, "doc")),
        ebt__xl_file:mkdirs(DocDir),
        ebt__xl_lists:eforeach(fun(F) ->
            Command = ebt__xl_string:format("~s -T~s ~s ~s > ~s/~s.~s",
                [Target, OutputFormat, Options, F, DocDir, filename:basename(F, ".dot"), OutputFormat]),
            io:format("~s~n", [Command]),
            ebt__xl_shell:command(Command)
        end, Files)
    ]).
