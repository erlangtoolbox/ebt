%%%-------------------------------------------------------------------
%%% @author Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>
%%% @copyright (C) 2013, strikead.com
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ebt_cover_analyse).
-author("Volodymyr Kyrychenko <vladimir.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).


-spec(perform(atom(), file:name(), ebt_config:config()) -> ebt__error_m:monad(ok)).
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
