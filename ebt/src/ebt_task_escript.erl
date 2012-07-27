-module(ebt_task_escript).

-include_lib("kernel/include/file.hrl").

-compile({parse_transform, do}).
-behaviour(ebt_task).

-export([perform/3]).

perform(Dir, Config, Defaults) ->
	do([ error_m ||
		AppProdDir <- ebt_config:app_production_outdir(Dir, Config, Defaults),
		Libs <- return(lists:map(fun(L) -> L ++ "/*/ebin" end , ebt_config:value(libraries, Config, []))),
		Files <- strikead_file:read_files([AppProdDir ++ "/ebin/*" | Libs]),
		return(Files)
	%		{"memory", Zip} <- zip:create("memory", Files, [memory]),
%		ProdDir <- ebt_config:production_outdir(Config, Defaults),
%		file:write_file(ProdDir ++ "/bin/x", iolist_to_binary([
%			"#!/usr/bin/env escript\n%%! -noshell -noinput\n", Zip
%		])),
%		#file_info{mode = Mode} <- stikead_file:read_file_info(ProdDir ++ "/bin/x"),
%		file:change_mode(ProdDir ++ "/bin/x", Mode bor 8#00100)
	]).
