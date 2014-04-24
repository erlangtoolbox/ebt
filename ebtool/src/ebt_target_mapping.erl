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

-module(ebt_target_mapping).

-compile({parse_transform, do}).

-export([get/2]).

-spec(get(atom(), ebt_config:config()) -> error_m:monad({module(), [atom()]})).
get(Target, Config) ->
    do([error_m ||
        {Modules, Targets} <- mapping_to_tuple(system_mapping()),
        {UserModules, UserTargets} <- mapping_to_tuple({ok,
            ebt_config:value(tasks, Config, [{modules, []}, {targets, []}])}),
        Module <- get_module(Target, Modules ++ UserModules),
        Depends <- return(xl_lists:kvfind(Target, Targets, [])),
        UserDepends <- return(xl_lists:kvfind(Target, UserTargets, [])),
        return({Module, Depends ++ UserDepends})
    ]).

system_mapping() -> xl_application:eget_env(ebtool, tasks).

mapping_to_tuple(E = {error, _}) -> E;
mapping_to_tuple({ok, Mapping}) ->
    {ok, {
        xl_lists:kvfind(modules, Mapping, []),
        xl_lists:kvfind(targets, Mapping, [])
    }}.

get_module(Target, Modules) ->
    option_m:to_error_m(
        xl_lists:kvfind(Target, Modules),
        xl_string:format("cannot find module for target ~p", [Target])
    ).

