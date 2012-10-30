-module(ebt_task_git_info).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, Config) ->
    do([error_m ||
        OutDir <- ebt_config:info_outdir(Dir, Config),
        case ebt_xl_shell:command("git --no-pager log -1 --pretty='format:%H'") of
            {ok, Commit} ->
                io:format("commit ~s~n", [Commit]),
                ebt_xl_file:write_file(filename:join(OutDir, "git.commit"), Commit);
            {error, E} ->
                io:format("failed to retrieve git commit: ~n~p~n", [E])
        end
    ]).

