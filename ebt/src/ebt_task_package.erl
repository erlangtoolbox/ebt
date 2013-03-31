-module(ebt_task_package).

-compile({parse_transform, ebt__do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        App <- ebt_config:appname_full(Dir, Config),
        ProdDir <- ebt_config:outdir(production, Config),
        DistDir <- ebt_config:outdir(dist, Config),
        Archive <- return(ebt__xl_string:join([DistDir, "/", App, ".ez"], "")),
        io:format("packing ~s~n", [Archive]),
        zip:create(Archive, [App], [
            {cwd, ProdDir},
            {compress, all}, {uncompress, [".beam", ".app"]}
        ])
    ]).
