-module(ebt_task_escript).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Dir, Config, Defaults) ->
    do([ error_m ||
        AppProdDir <- ebt_config:app_production_outdir(Dir, Config, Defaults),
        Libs <- return(lists:map(fun(L) -> L ++ "/*/ebin/*" end , ebt_config:value(libraries, Config, []))),
        Files <- strikead_file:read_files([AppProdDir ++ "/ebin/*" | Libs]),
        {"memory", Zip} <- zip:create("memory", Files, [memory]),
        Scripts <- maybe_m:to_error_m(
           ebt_config:find_value(escript, Config, Defaults),
            "no escript configuration"
        ),
        strikead_lists:eforeach(fun({Name, Params}) ->
            Path = strikead_string:join([AppProdDir, "/bin/", Name]),
            do([error_m ||
                strikead_file:write_file(Path, iolist_to_binary([
                    "#!/usr/bin/env escript\n%%! " ++ Params ++ "\n", Zip
                ])),
                #file_info{mode = Mode} <- strikead_file:read_file_info(Path),
                strikead_file:change_mode(Path, Mode bor 8#00100),
                io:format("created ~s~n", [Path])
            ])
        end, Scripts)
    ]).
