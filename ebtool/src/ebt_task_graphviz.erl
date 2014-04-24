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

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    OutputFormat = ebt_config:value(Target, Dir, output_format, png),
    Options = ebt_config:value(Target, Dir, options, ""),
    Files = ebt_config:files(Target, Config, [], []),
    do([error_m ||
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        DocDir <- return(filename:join(ProdDir, "doc")),
        xl_file:mkdirs(DocDir),
        xl_lists:eforeach(fun(F) ->
            Command = xl_string:format("~s -T~s ~s ~s > ~s/~s.~s",
                [Target, OutputFormat, Options, F, DocDir, filename:basename(F, ".dot"), OutputFormat]),
            io:format("~s~n", [Command]),
            xl_shell:command(Command)
        end, Files),
        return(Config)
    ]).
