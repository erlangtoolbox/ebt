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
%% @doc EDoc
%%
%% == Configuration ==
%% files - include/exclude files
%%
%% == Example ==
%% <pre>
%% {edoc, [
%%     {files, [
%%          {include, ["src/*.erl"]},
%%          {exclude, []}
%%     ]},
%% ]}
%% </pre>
-module(ebt_task_edoc).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        App <- ebt_config:appname(Dir),
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        DocDir <- return(filename:join(ProdDir, "doc")),
        xl_file:copy_if_exists("doc", ProdDir),
        OverviewPath <- return(filename:join(DocDir, "overview.edoc")),
        HasOverview <- xl_file:exists(OverviewPath),
        case HasOverview of
            true -> ebt_task_template:substitute_file(Config, OverviewPath, OverviewPath, [], {${, $}});
            _ -> ok
        end,
        edoc:run([], ebt_config:files(Target, Config, ["src/*.erl"], []),
            [{dir, DocDir}, {application, App} | ebt_config:value(Target, Config, [])]),
        return(Config)
    ]).
