-module(ebt_task_depends).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(_Dir, Config) ->
    DepsDir = ebt_config:value(depends, Config, dir, "./lib"),
    R = do([error_m ||
        inets:start(),
        ebt_strikead_file:mkdirs(DepsDir),
        ebt_strikead_lists:eforeach(fun({Url, Libs}) ->
            ebt_strikead_lists:eforeach(fun({Name, Version}) ->
                Lib = ebt_strikead_string:join([Name, "-", Version, ".ez"]),
                LocalFile = filename:join(DepsDir, Lib),
                case ebt_strikead_file:exists(ebt_strikead_string:join([DepsDir, "/", Name, "-", Version])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        do([error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [],
                                [{stream, LocalFile}]),
                            ebt_strikead_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            ebt_strikead_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(depends, Config, repositories, []))
    ]),
    inets:stop(),
    R.


