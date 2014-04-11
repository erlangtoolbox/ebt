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
%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_task_cover_analyse).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-export([perform/3]).

perform(Target, Dir, Config) ->
    Output = ebt_config:value(Target, Config, output, file),
    Analysis = ebt_config:value(Target, Config, analysis, coverage),
    Level = ebt_config:value(Target, Config, level, function),
    Html = ebt_config:value(Target, Config, html, false),
    R = case cover:modules() of
        [] -> io:format("no cover-compiled modules~n");
        Modules ->
            case ebt_config:app_outdir(cover, Dir, Config) of
                {ok, CoverReportDir} when Output == file ->
                    io:format("report ~s~n", [CoverReportDir]);
                _ -> ok
            end,
            lists:foreach(fun
                (Module) when Output == console ->
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
                (Module) when Output == file ->
                    case ebt_config:app_outdir(cover, Dir, Config) of
                        {ok, CoverDir} when Html ->
                            case cover:analyse_to_file(Module, xl_string:join([CoverDir, "/", Module, ".html"]), [html]) of
                                {ok, _} -> ok;
                                {error, E} -> io:format("ERROR: ~p~n", [E])
                            end;
                        {ok, CoverDir} ->
                            case cover:analyse_to_file(Module, xl_string:join([CoverDir, "/", Module, ".cover"])) of
                                {ok, _} -> ok;
                                {error, E} -> io:format("ERROR: ~p~n", [E])
                            end;
                        E -> E
                    end
            end, Modules)
    end,
    case R of
        ok -> {ok, Config};
        E -> E
    end.
