-module(ebt_task_release).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        RelConfig <- ebt_config:find_value(Target, Config, config),
        Name <- ebt_config:find_value(Target, Config, name),
        ReleaseDir <- ebt_config:outdir(releases, Config, Name),
        io:format("release ~p to ~s~n", [Name, ReleaseDir]),
        RelTool <- reltool:start_server([{config, RelConfig}]),
        try
            reltool:create_target(RelTool, ReleaseDir)
        after
            reltool:stop(RelTool)
        end,
        ebt_xl_file:copy_filtered(Dir,
            ebt_config:value(Target, Config, resources, []), ReleaseDir),
        generate_runners(RelConfig, ReleaseDir),
        pack(Target, Config)
    ]).

generate_runners(RelConfig, ReleaseDir) ->
    do([error_m ||
        Runner <- ebt_xl_escript:read_file("priv/release/run"),
        [{sys, L}] <- ebt_xl_file:read_terms(RelConfig),
        ebt_xl_lists:eforeach(fun({rel, Name, _, _}) ->
            Path = ebt_xl_string:join([ReleaseDir, "bin", Name], "/"),
            do([error_m ||
                ebt_xl_file:write_file(Path, Runner),
                #file_info{mode = Mode} <- ebt_xl_file:read_file_info(Path),
                ebt_xl_file:change_mode(Path, Mode bor 8#00100)
            ])
        end, lists:filter(fun(X) -> element(1, X) == rel end, L))
    ]).


pack(Target, Config) ->
    do([error_m ||
        Name <- ebt_config:find_value(Target, Config, name),
        DestDir <- ebt_config:outdir(releases, Config),
        DistDir <- ebt_config:outdir(dist, Config),
        Command <- return(ebt_xl_string:join(["tar -czf ", DistDir, "/", Name, ".tar.gz ", Name])),
        io:format("cd ~s; ~s~n", [DestDir, Command]),
        ebt_xl_shell:command(Command, DestDir)
    ]).

