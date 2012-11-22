-module(ebt_task_release).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        RelConfigFile <- ebt_config:find_value(Target, Config, config),
        [RelConfig] <- xl_file:read_terms(RelConfigFile),
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
        ebt_xl_file:copy_filtered(Dir,
            ebt_config:value(Target, Config, resources, []), ReleaseDir),
        generate_runners(RelConfigUpdated, ReleaseDir),
        pack(Target, Config)
    ]).

generate_runners({sys, L}, ReleaseDir) ->
    do([error_m ||
        RunnerTemplate <- ebt_xl_escript:read_file("priv/release/run"),
        ebt_xl_lists:eforeach(fun({rel, Name, _, _}) ->
            Path = ebt_xl_string:join([ReleaseDir, "bin", Name], "/"),
            do([error_m ||
                Runner <- return(ebt_xl_string:substitute(binary_to_list(RunnerTemplate), [
                    {'APPNAME', Name}
                ], {$@, $@})),
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
        ebt_cmdlib:exec(ebt_xl_string:join(["tar -czf ", DistDir, "/", Name, ".tar.gz ", Name]), DestDir)
    ]).

update_release_config({sys, Opts}, Config) ->
    do([error_m ||
        Version <- ebt_config:version(Config),
        return({sys, lists:map(fun
            ({rel, Name, _, Apps}) -> {rel, Name, Version, Apps};
            (X) -> X
        end, Opts)})
    ]).
