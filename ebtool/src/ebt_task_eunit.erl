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

-module(ebt_task_eunit).

-compile({parse_transform, do}).

-export([perform/3]).

%% @doc EUnit Tests
%%
%% == Configuration ==
%% files - optional
%%
%% == Example ==
%% <pre>
%% {eunit, [
%%     {files, [
%%          {include, ["src/*_tests.erl"]},
%%          {exclude, []}
%%     ]},%% ]}
%% </pre>
-spec(perform(atom(), file:name(), ebt_config:config()) -> error_m:monad(ok)).
perform(Target, Dir, Config) ->
    do([error_m ||
        TestDir <- ebt_config:app_outdir(test, Dir, Config),
        ProdDir <- ebt_config:app_outdir(production, Dir, Config),
        case lists:map(fun(F) ->
            list_to_atom(filename:basename(F, "_tests.erl"))
        end, ebt_config:files(Target, Config, [], ["test/*_tests.erl"])) of
            [] -> io:format("no eunit tests~n");
            List ->
                ebtool:load_library(TestDir),
                ebtool:load_library(ProdDir),
                xl_lists:eforeach(fun(Module) ->
                    io:format("test ~p~n", [Module]),
                    case eunit:test(Module) of
                        error -> {error, {test_failed, Module}};
                        ok -> ok
                    end
                end, List)
        end,
        return(Config)
    ]).
