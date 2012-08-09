-module(ebt_task_release).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(_Dir, Config) ->
    do([error_m ||
        RelConfig <- ebt_config:find_value(release, Config, config),
        Name <- ebt_config:find_value(release, Config, name),
        DestDir <- ebt_config:outdir(releases, Config),
        ReleaseDir <- return(ebt_strikead_string:join([DestDir, Name], "/")),
        ebt_strikead_file:mkdirs(ReleaseDir),
        io:format("release ~p to ~s~n", [Name, ReleaseDir]),
        RelTool <- reltool:start_server([{config, RelConfig}]),
        try
            reltool:create_target(RelTool, ReleaseDir)
        after
            reltool:stop(RelTool)
        end
    ]).
