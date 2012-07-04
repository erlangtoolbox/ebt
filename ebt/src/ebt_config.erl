-module(ebt_config).

-opaque config() :: [term()].
-export_types([config/0]).

-export([read/1, target/1, subdirs/1]).

-spec read/1 :: (file:filename()) -> error_m:monad(config()).
read(Filename) ->
	case strikead_file:exists(Filename) of
		true -> strikead_file:read_terms(Filename);
		false -> {ok, []}
	end.

-spec target/1 :: (config()) -> atom().
target(Config) ->
	strikead_lists:kvfind(target, Config, 'otp-app').

-spec subdirs/1 :: (config()) -> [file:filename()].
subdirs(Config) ->
	strikead_lists:kvfind(subdirs, Config, []).
