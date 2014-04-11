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

-module(ebt_task).

-compile({parse_transform, do}).

-export([perform/4]).

-export_type([context/0]).

-type(context() :: [{atom(), term()}]).

-spec(perform(atom(), [atom()], file:name(), ebt_config:config()) ->
    error_m:monad({[atom()], ebt_config:config()})).
perform(Level, Targets, Dir, Config) ->
    perform(Level, Targets, Dir, Config, []).

perform(_Level, [], _Dir, Config, Acc) -> {ok, {Acc, Config}};
perform(Level, [Target | Targets], Dir, Config, Acc) ->
    case lists:member(Target, Acc) of
        true ->
            ebt_tty:format("~p => ~s at ~s already done~n", [Level, Target, Dir]),
            perform(Level, Targets, Dir, Config, Acc);
        false ->
            ebt_tty:format("~p => ~s at ~s~n", [Level, Target, Dir]),
            do([error_m ||
                {Module, Depends} <- ebt_target_mapping:get(Target, Config),
                {DoneTargets, DoneConfig} <- perform(Level, Depends, Dir, Config, Acc),
                ebt_tty:format("~s:~n", [Target]),
                ebt_tty:io_context(Target),
                ebtool:load_libraries(Config),
                NewConfig <- Module:perform(Target, Dir, DoneConfig),
                R <- perform(Level, Targets, Dir, NewConfig, [Target | Acc] ++ DoneTargets),
                ebt_tty:io_context(undefined),
                return(R)
            ])
    end.
