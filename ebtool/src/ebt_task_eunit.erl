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
