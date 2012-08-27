-module(ebt_task_depends).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(_Dir, Config) ->
    DepsDir = ebt_config:value(depends, Config, dir, "./lib"),
    inets:start(),
    do([error_m ||
        ebt_xl_file:mkdirs(DepsDir),
        ebt_xl_lists:eforeach(fun({Url, Libs}) ->
            ebt_xl_lists:eforeach(fun({Name, Version}) ->
                Lib = ebt_xl_string:join([Name, "-", Version, ".ez"]),
                LocalFile = filename:join(DepsDir, Lib),
                case ebt_xl_file:exists(ebt_xl_string:join([DepsDir, "/", Name, "-", Version])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        do([error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [],
                                [{stream, LocalFile}]),
                            ebt_xl_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            ebt_xl_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(depends, Config, repositories, []))
    ]).


