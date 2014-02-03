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
