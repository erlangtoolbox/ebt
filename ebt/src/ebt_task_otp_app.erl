-module(ebt_task_otp_app).

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Dir, Config, Defaults) ->
	do([error_m ||
		App <- ebt_config:appname(Dir, Config),
		ProdDir <- ebt_config:production_outdir(Config, Defaults),
		DistDir <- ebt_config:dist_outdir(Config, Defaults),
		Archive <- return(strikead_string:join([DistDir, "/", App, ".ez"],"")),
		io:format("Packing ~s~n", [Archive]),
		zip:create(Archive, [App], [
				{cwd, ProdDir},
				{compress, all}, {uncompress, [".beam",".app"]}
		])
	]).
