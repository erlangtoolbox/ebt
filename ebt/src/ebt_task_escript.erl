-module(ebt_task_escript).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(Dir, Config) ->
    do([ error_m ||
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        Libs <- return(lists:map(fun(L) -> L ++ "/*/ebin/*" end , ebt_config:value(libraries, Config, []))),
        Files <- ebt_strikead_file:read_files([AppProdDir ++ "/ebin/*" | Libs]),
        {"memory", Zip} <- zip:create("memory", Files, [memory]),
        Scripts <- ebt_config:find_value(escript, Config),
        ebt_strikead_lists:eforeach(fun({Name, Params}) ->
            Path = ebt_strikead_string:join([AppProdDir, "/bin/", Name]),
            do([error_m ||
                ebt_strikead_file:write_file(Path, iolist_to_binary([
                    "#!/usr/bin/env escript\n%%! " ++ Params ++ "\n", Zip
                ])),
                #file_info{mode = Mode} <- ebt_strikead_file:read_file_info(Path),
                ebt_strikead_file:change_mode(Path, Mode bor 8#00100),
                io:format("created ~s~n", [Path])
            ])
        end, Scripts)
    ]).
