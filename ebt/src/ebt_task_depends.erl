-module(ebt_task_depends).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, _Dir, Config) ->
    DepsDir = ebt_config:value(Target, Config, dir, "./lib"),
    inets:start(),
    ebt__do([ebt__error_m ||
        ebt__xl_file:mkdirs(DepsDir),
        ebt__xl_lists:eforeach(fun({Url, Libs}) ->
            ebt__xl_lists:eforeach(fun({Name, Version}) ->
                Lib = ebt__xl_string:join([Name, "-", Version, ".ez"]),
                LocalFile = filename:join(DepsDir, Lib),
                case ebt__xl_file:exists(ebt__xl_string:join([DepsDir, "/", Name, "-", Version])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        ebt__do([ebt__error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [], [{stream, LocalFile}]),
                            ebt__xl_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            ebt__xl_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(Target, Config, repositories, []))
    ]).


