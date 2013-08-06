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

%% @doc Dependencies configuration
%%
%% == Configuration ==
%% <ul>
%% <li>dir - downloaded libraries goes there</li>
%% <li>repositaries - repositary configuration</li>
%% </ul>
%%
%% == Example ==
%% <pre>
%% {depends, [
%%     {dir, "./lib"},
%%     {repositories, [
%%         {"http://erlang-build-tool.googlecode.com/files", [
%%             {erlandox, "1.0.5"},
%%             {xl_stdlib, "1.2.0.301"},
%%             {getopt, "0.7.1"}
%%         ]}
%%     ]}
%% ]}
%% </pre>
-module(ebt_task_depends).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, _Dir, Config) ->
    DepsDir = ebt_config:value(Target, Config, dir, "./lib"),
    Escape = ebt_config:value(Target, Config, escape, ''),
    inets:start(),
    ebt__do([ebt__error_m ||
        ebt__xl_file:mkdirs(DepsDir),
        ebt__xl_lists:eforeach(fun({Url, Libs}) ->
            ebt__xl_lists:eforeach(fun({Name, Version}) ->
                LibName = ebt__xl_string:join([Name, "-", Version]),
                Lib = ebt__xl_string:join([LibName, ".ez"]),
                case ebt__xl_file:exists(ebt__xl_string:join([DepsDir, "/", Escape, LibName])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        LocalFile = filename:join(DepsDir, Lib),
                        ebt__do([ebt__error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [], [{stream, LocalFile}]),
                            ebt__xl_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            ebt__xl_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(Target, Config, repositories, [])),
        Modules <- return(lists:map(fun(M) ->
            list_to_atom(filename:basename(M, ".beam"))
        end, filelib:wildcard(DepsDir ++ "/*/ebin/*.beam"))),
        Apps <- return(lists:map(fun(A) ->
            list_to_atom(filename:basename(A, ".app"))
        end, filelib:wildcard(DepsDir ++ "/*/ebin/*.app"))),
        ebt__xl_lists:eforeach(fun(Lib) ->
            LibName = filename:basename(Lib),
            {_, Name, _} = ebt_config:libinfo(Lib),
            escape(DepsDir, Name, LibName, Escape, Modules, ebt_config:value(Target, Config, escape_exclude, []), Apps)
        end, filelib:wildcard(DepsDir ++ "/*"))
    ]).

escape(_Dir, _Name, _Lib, '', _Atoms, _Excludes, _Apps) -> ok;
escape(Dir, Name, Lib, Escape, Atoms, Excludes, Apps) ->
    TargetName = ebt__xl_string:join([Dir, "/", Escape, Lib]),
    AppFile = ebt__xl_string:join([TargetName, "/ebin/", Name, ".app"]),
    ebt__do([ebt__error_m ||
        ebt__xl_file:rename(filename:join(Dir, Lib), TargetName),
        ebt__xl_lists:eforeach(fun(Mod) ->
            io:format("escape ~s~n", [Mod]),
            case beam_lib:chunks(Mod, [abstract_code]) of
                {error, beam_lib, Error} -> {error, {Mod, Error}};
                {ok, {_, [{abstract_code, missing_chunk}]}} -> {error, {Mod, no_abstract_code}};
                {ok, {_, [{abstract_code, no_abstract_code}]}} -> {error, {Mod, no_abstract_code}};
                {ok, {_, [{abstract_code, {_, Forms}}]}} ->
                    case compile:forms(escape_forms(Escape, Forms, Atoms, Excludes), [report, debug_info]) of
                        {ok, Module, Binary} ->
                            ebt__do([ebt__error_m ||
                                ebt__xl_file:delete(Mod),
                                ebt__xl_file:write_file(ebt__xl_string:join([TargetName, "/ebin/", Module, ".beam"]), Binary)
                            ]);
                        error -> error
                    end
            end
        end, filelib:wildcard(ebt__xl_string:join([TargetName, "/ebin/*.beam"]))),
        [{application, _, Params}] <- ebt__xl_file:read_terms(AppFile),
        ebt__xl_file:write_term(ebt__xl_string:join([TargetName, "/ebin/", Escape, Name, ".app"]),
            ebt_applib:update(
                {application, ebt__xl_convert:make_atom([Escape, Name]), Params},
                [
                    {modules, lists:map(fun(M) ->
                        ebt__xl_convert:make_atom([Escape, M])
                    end, ebt__xl_lists:kvfind(modules, Params, []))},
                    {applications, lists:map(fun(M) ->
                        case lists:member(M, Apps) of
                            true -> ebt__xl_convert:make_atom([Escape, M]);
                            false -> M
                        end end, ebt__xl_lists:kvfind(applications, Params, []))}
                ]
                    ++ case ebt__xl_lists:kvfind(mod, Params) of
                           {ok, {M, P}} -> [{mod, {ebt__xl_convert:make_atom([Escape, M]), P}}];
                           _ -> []
                       end
            )
        ),
        ebt__xl_file:delete(AppFile)
    ]).

escape_forms(Escape, Forms, Atoms, Excludes) -> [escape_form(Escape, Form, Atoms, Excludes) || Form <- Forms].

escape_form(Escape, A = {atom, Line, Name}, Atoms, Excludes) ->
    case lists:member(Name, Atoms) andalso not lists:member(A, Excludes) of
        true -> {atom, Line, ebt__xl_convert:make_atom([Escape, Name])};
        false -> A
    end;
escape_form(Escape, {attribute, Line, module, {Name, Params}}, _Atoms, _Excludes) ->
    {attribute, Line, module, {ebt__xl_convert:make_atom([Escape, Name]), Params}};
escape_form(Escape, {attribute, Line, module, Name}, _Atoms, _Excludes) ->
    {attribute, Line, module, ebt__xl_convert:make_atom([Escape, Name])};
escape_form(Escape, X, Atoms, Excludes) when is_tuple(X) -> list_to_tuple([escape_form(Escape, E, Atoms, Excludes) || E <- tuple_to_list(X)]);
escape_form(Escape, X, Atoms, Excludes) when is_list(X) -> [escape_form(Escape, E, Atoms, Excludes) || E <- X];
escape_form(_Escape, X, _Atoms, _Excludes) -> X.
