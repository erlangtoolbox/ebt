-module(ebt_task_depends).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Dir, Config, _Defaults) ->
    DepsDir = ebt_config:value(depends, Config, dir, "./lib"),
    R = do([error_m ||
        inets:start(),
        strikead_file:mkdirs(DepsDir),
        strikead_lists:eforeach(fun({Url, Libs}) ->
            strikead_lists:eforeach(fun({Name, Version}) ->
                Lib = strikead_string:join([Name, "-", Version, ".ez"]),
                LocalFile = filename:join(DepsDir, Lib),
                case strikead_file:exists(strikead_string:join([DepsDir, "/", Name, "-", Version])) of
                    {ok, true} ->
                        io:format("already downloaded ~s~n", [Lib]);
                    {ok, false} ->
                        io:format("download ~s/~s~n", [Url, Lib]),
                        do([error_m ||
                            httpc:request(get, {Url ++ "/" ++ Lib, []}, [],
                                [{stream, LocalFile}]),
                            strikead_zip:unzip(LocalFile, [{cwd, DepsDir}, verbose]),
                            strikead_file:delete(LocalFile)
                        ]);
                    E -> E
                end
            end, Libs)
        end, ebt_config:value(depends, Config, repositories, []))
    ]),
    inets:stop(),
    R.


