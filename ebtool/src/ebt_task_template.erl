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

-module(ebt_task_template).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([perform/3, substitute_file/5]).

perform(Target, _Dir, Config) ->
    case xl_lists:eforeach(fun({In, Out, Map, Braces}) ->
        substitute_file(Config, In, Out, Map, Braces)
    end, ebt_config:value(Target, Config, [])) of
        ok -> {ok, Config};
        E -> E
    end.

substitute_file(Config, Src, Dst, Map, Braces) ->
    do([error_m ||
        CommonMap <- return(xl_shell:getenv() ++ ebt_config:definitions(Config)),
        io:format("~s -> ~s~n", [Src, Dst]),
        InFile <- xl_file:read_file(Src),
        xl_file:write_file(Dst, xl_string:substitute(InFile, Map ++ CommonMap, Braces))
    ]).
