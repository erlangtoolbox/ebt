-module(ebt_config).

-compile({parse_transform, do}).

-type config() :: strikead_lists:kvlist_at().
-type defaults() :: strikead_lists:kvlist_at().
-export_types([config/0, defaults/0]).

-export([read/1, value/3, find_value/3, outdir/2, app_production_outdir/3,
	production_outdir/2, dist_outdir/2, version/1, appname/2]).

-spec read/1 :: (file:name()) -> error_m:monad(config()).
read(Filename) ->
	case strikead_file:exists(Filename) of
		true -> strikead_file:read_terms(Filename);
		false -> {ok, []}
	end.

-spec find_value/3 :: (atom(), config(), defaults()) -> any().
find_value(Key, Config, Defaults) when is_list(Defaults)->
	do([search_m ||
		strikead_lists:kvfind(Key, Config),
		strikead_lists:kvfind(Key, Defaults)
	]).

-spec value/3 :: (atom(), config(), any()) -> any().
value(Key, Config, Default) -> strikead_lists:kvfind(Key, Config, Default).

-spec production_outdir/2 :: (config(), defaults()) -> error_m:monad(string()).
production_outdir(Config, Defaults) -> outdir(Config, Defaults, "production").

-spec dist_outdir/2 :: (config(), defaults()) -> error_m:monad(string()).
dist_outdir(Config, Defaults) -> outdir(Config, Defaults, "dist").

-spec outdir/2 :: (config(), defaults()) -> error_m:monad(string()).
outdir(Config, Defaults) -> outdir(Config, Defaults, "").

-spec outdir/3 :: (config(), defaults(), string()) -> error_m:monad(string()).
outdir(Config, Defaults, Suffix) ->
	case ebt_config:find_value(output, Config, Defaults) of
		{ok, Dir} -> {ok, filename:join(Dir, Suffix)};
		_ -> {error, "unknown output directory"}
	end.

-spec app_production_outdir/3 :: (file:name(), config(), defaults()) ->
	maybe_m:monad(string()).
app_production_outdir(Dir, Config, Defaults) ->
	do([error_m ||
		App <- appname(Dir, Config),
		ProdOutDir <- production_outdir(Config, Defaults),
		return(filename:join(ProdOutDir, App))
	]).

-spec version/1 :: (config()) -> error_m:monad(string()).
version(Config) ->
	case strikead_lists:kvfind(version, Config) of
		{ok, {shell, X}} -> {ok, os:cmd(X)};
		_ -> {ok, "0.0.1"}
	end.

-spec appname/2 :: (file:name(), ebt_config:config()) -> error_m:monad(string()).
appname(Dir, Config) ->
	do([error_m ||
		{_, Name, _} <- ebt_applib:load(Dir),
		Version <- ebt_config:version(Config),
		return(strikead_string:join([Name, Version],"-"))
	]).
