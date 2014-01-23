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

%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_task_cover_analyse).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    Output = ebt_config:value(Target, Config, output, file),
    Analysis = ebt_config:value(Target, Config, analysis, coverage),
    Level = ebt_config:value(Target, Config, level, function),
    Html = ebt_config:value(Target, Config, html, false),
    case cover:modules() of
        [] -> io:format("no cover-compiled modules~n");
        Modules ->
            lists:foreach(fun(Module) ->
                case Output of
                    console ->
                        case cover:analyse(Module, Analysis, Level) of
                            {ok, {Module, Value}} -> io:format("~p: ~p~n", [Module, Value]);
                            {ok, Answers} ->
                                lists:foreach(fun
                                    ({{M, N}, {Cov, NotCov}}) ->
                                        io:format("~p line ~p: covered ~p, not covered ~p~n", [M, N, Cov, NotCov]);
                                    ({{M, N}, Calls}) ->
                                        io:format("~p line ~p: calls ~p~n", [M, N, Calls]);
                                    ({{M, F, A, C}, {Cov, NotCov}}) ->
                                        io:format("~p:~p/~p clause ~p: covered ~p, not covered ~p~n", [M, F, A, C, Cov, NotCov]);
                                    ({{M, F, A, C}, Calls}) ->
                                        io:format("~p:~p/~p clause ~p: calls ~p~n", [M, F, A, C, Calls]);
                                    ({{M, F, A}, {Cov, NotCov}}) ->
                                        io:format("~p:~p/~p: covered ~p, not covered ~p~n", [M, F, A, Cov, NotCov]);
                                    ({{M, F, A}, Calls}) ->
                                        io:format("~p:~p/~p: calls ~p~n", [M, F, A, Calls])
                                end, Answers);
                            {error, E} -> io:format("ERROR: ~p~n", [E])
                        end;
                    file ->
                        case ebt_config:app_outdir(cover, Dir, Config) of
                            {ok, CoverDir} when Html ->
                                case cover:analyse_to_file(Module, ebt__xl_string:join([CoverDir, "/", Module, ".html"]), [html]) of
                                    {ok, _} -> ok;
                                    {error, E} -> io:format("ERROR: ~p~n", [E])
                                end;
                            {ok, CoverDir} ->
                                case cover:analyse_to_file(Module, ebt__xl_string:join([CoverDir, "/", Module, ".cover"])) of
                                    {ok, _} -> ok;
                                    {error, E} -> io:format("ERROR: ~p~n", [E])
                                end;
                            E -> E
                        end
                end
            end, Modules)
    end.
