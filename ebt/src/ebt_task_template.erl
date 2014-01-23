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
-module(ebt_task_template).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3, substitute_file/5]).

perform(Target, _Dir, Config) ->
    case ebt__xl_lists:eforeach(fun({In, Out, Map, Braces}) ->
        substitute_file(Config, In, Out, Map, Braces)
    end, ebt_config:value(Target, Config, [])) of
        ok -> {ok, Config};
        E -> E
    end.

substitute_file(Config, Src, Dst, Map, Braces) ->
    ebt__do([ebt__error_m ||
        CommonMap <- return(ebt__xl_shell:getenv() ++ ebt_config:definitions(Config)),
        io:format("~s -> ~s~n", [Src, Dst]),
        InFile <- ebt__xl_file:read_file(Src),
        ebt__xl_file:write_file(Dst, ebt__xl_string:substitute(InFile, Map ++ CommonMap, Braces))
    ]).
