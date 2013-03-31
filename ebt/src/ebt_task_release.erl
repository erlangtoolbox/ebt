-module(ebt_task_release).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        RelConfigFile <- ebt_config:find_value(Target, Config, config),
        [RelConfig] <- ebt__xl_file:read_terms(RelConfigFile),
        RelConfigUpdated <- update_release_config(RelConfig, Config),
        Name <- ebt_config:find_value(Target, Config, name),
        ReleaseDir <- ebt_config:outdir(releases, Config, Name),
        io:format("release ~p to ~s~n", [Name, ReleaseDir]),
        RelTool <- reltool:start_server([{config, RelConfigUpdated}]),
        try
            reltool:create_target(RelTool, ReleaseDir)
        after
            reltool:stop(RelTool)
        end,
        ebt__xl_file:copy_filtered(Dir,
            ebt_config:value(Target, Config, resources, []), ReleaseDir),
        generate_runners(RelConfigUpdated, ReleaseDir),
        pack(Target, Config)
    ]).

generate_runners({sys, L}, ReleaseDir) ->
    ebt__do([ebt__error_m ||
        RunnerTemplate <- ebt__xl_escript:read_file("priv/release/run"),
        ebt__xl_lists:eforeach(fun({rel, Name, _, _}) ->
            Path = ebt__xl_string:join([ReleaseDir, "bin", Name], "/"),
            ebt__do([ebt__error_m ||
                Runner <- return(ebt__xl_string:substitute(binary_to_list(RunnerTemplate), [
                    {'APPNAME', Name}
                ], {$@, $@})),
                ebt__xl_file:write_file(Path, Runner),
                #file_info{mode = Mode} <- ebt__xl_file:read_file_info(Path),
                ebt__xl_file:change_mode(Path, Mode bor 8#00100)
            ])
        end, lists:filter(fun(X) -> element(1, X) == rel end, L))
    ]).


pack(Target, Config) ->
    ebt__do([ebt__error_m ||
        Name <- ebt_config:find_value(Target, Config, name),
        DestDir <- ebt_config:outdir(releases, Config),
        DistDir <- ebt_config:outdir(dist, Config),
        ebt_cmdlib:exec(ebt__xl_string:join(["tar -czf ", DistDir, "/", Name, ".tar.gz ", Name]), DestDir)
    ]).

update_release_config({sys, Opts}, Config) ->
    ebt__do([ebt__error_m ||
        Version <- ebt_config:version(Config),
        return({sys, lists:map(fun
            ({rel, Name, _, Apps}) -> {rel, Name, Version, Apps};
            (X) -> X
        end, Opts)})
    ]).
