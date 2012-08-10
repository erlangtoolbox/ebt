-module(ebt_task_release).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/2]).

perform(_Dir, Config) ->
    do([error_m ||
        RelConfig <- ebt_config:find_value(release, Config, config),
        Name <- ebt_config:find_value(release, Config, name),
        DestDir <- ebt_config:outdir(releases, Config),
        ReleaseDir <- ebt_config:outdir(releases, Config, Name),
        io:format("release ~p to ~s~n", [Name, ReleaseDir]),
        RelTool <- reltool:start_server([{config, RelConfig}]),
        try
            reltool:create_target(RelTool, ReleaseDir)
        after
            reltool:stop(RelTool)
        end,
        DistDir <- ebt_config:outdir(dist, Config),
        Command <- return(ebt_strikead_string:join(["tar -czf ", DistDir, "/", Name, ".tar.gz ", Name])),
        io:format("cd ~s; ~s~n", [DestDir, Command]),
        ebt_strikead_shell:command(Command, DestDir)
    ]).
