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
-module(ebt_task).

-compile({parse_transform, ebt__do}).

-export([perform/4]).

-callback(perform(atom(), file:name(), ebt_config:config()) -> ebt__error_m:monad(any())).

-spec(perform(atom(), [atom()], file:name(), ebt_config:config()) ->
    ebt__error_m:monad([atom()])).
perform(Level, Targets, Dir, Config) ->
    perform(Level, Targets, Dir, Config, []).

perform(_Level, [], _Dir, _Config, Acc) -> {ok, Acc};
perform(Level, [Target | Targets], Dir, Config, Acc) ->
    case lists:member(Target, Acc) of
        true ->
            io:format("~p => ~s at ~s already done~n", [Level, Target, Dir]),
            perform(Level, Targets, Dir, Config, Acc);
        false ->
            io:format("~p => ~s at ~s~n", [Level, Target, Dir]),
            ebt__do([ebt__error_m ||
                {Module, Depends} <- ebt_target_mapping:get(Target, Config),
                DoneTargets <- perform(Level, Depends, Dir, Config, Acc),
                io:format("~s:~n", [Target]),
                ebt:load_libraries(Config),
                Module:perform(Target, Dir, Config),
                perform(Level, Targets, Dir, Config, [Target | Acc] ++ DoneTargets)
            ])
    end.
