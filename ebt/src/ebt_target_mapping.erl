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

system_mapping() -> xl_application:eget_env(ebt, tasks).

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

