-module(ebt_task_clean).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(_Dir, Config, Defaults) ->
    do([ error_m ||
        ProdDir <- ebt_config:production_outdir(Config, Defaults),
        TestDir <- ebt_config:test_outdir(Config, Defaults),
        DistDir <- ebt_config:dist_outdir(Config, Defaults),
        strikead_file:delete(ProdDir),
        strikead_file:delete(DistDir),
        strikead_file:delete(TestDir)
    ]).
