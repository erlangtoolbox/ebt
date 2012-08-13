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
        Scripts <- ebt_config:find_value(escript, Config),
        ebt_strikead_lists:eforeach(fun({Name, Params, Resources}) ->
            Path = ebt_strikead_string:join([AppProdDir, "/bin/", Name]),
            do([error_m ||
                ResourceFiles <- ebt_strikead_file:read_files(Resources, {base, Dir}),
                {"memory", Zip} <- zip:create("memory", Files ++ ResourceFiles, [memory]),
                ebt_strikead_file:ensure_dir(Path),
                escript:create(Path, [
                    {shebang, default},
                    {comment, default},
                    {emu_args, Params},
                    {archive, Zip}
                ]),
                #file_info{mode = Mode} <- ebt_strikead_file:read_file_info(Path),
                ebt_strikead_file:change_mode(Path, Mode bor 8#00100),
                io:format("created ~s~n", [Path])
            ])
        end, Scripts)
    ]).
