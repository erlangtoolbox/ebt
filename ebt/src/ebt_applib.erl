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


-module(ebt_applib).

-compile({parse_transform, ebt__do}).

-export([load/1, update/2]).

-spec(load(file:name()) -> ebt__error_m:monad(application:application_spec())).
load(Dir) ->
    case filelib:wildcard(Dir ++ "/*.app") of
        [File] ->
            case ebt__xl_file:read_terms(File) of
                {ok, [App = {application, _, Params}]} when is_list(Params) ->
                    {ok, App};
                E = {error, _} -> E;
                _ -> {error, "wrong format of app file"}
            end;
        _ -> {error, "failed to locate single app file in " ++ Dir}
    end.

-spec(update(application:application_spec(), [application:application_opt()])
        -> application:application_spec()).
update({application, App, Params}, Updates) ->
    {application, App, lists:foldl(fun(P, Ps) -> ebt__xl_lists:keyreplace_or_add(1, Ps, P) end, Params, Updates)}.


