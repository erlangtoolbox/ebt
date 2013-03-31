-module(ebt_task_git_info).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        OutDir <- ebt_config:info_outdir(Dir, Config),
        case ebt__xl_shell:command("git --no-pager log -1 --pretty='format:%H'") of
            {ok, Commit} ->
                io:format("commit ~s~n", [Commit]),
                ebt__xl_file:write_file(filename:join(OutDir, "git.commit"), Commit);
            {error, E} ->
                io:format("failed to retrieve git commit: ~n~p~n", [E])
        end
    ]).

