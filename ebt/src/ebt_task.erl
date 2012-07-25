-module(ebt_task).

-compile({parse_transform, do}).

-type target() :: {Name :: atom(), Module :: atom(), Depends :: [atom()]}.
-export([behaviour_info/1]).

-export([perform/4]).

behaviour_info(callbacks) ->
	[{perform, 3}];

behaviour_info(_) ->
	undefined.

-spec perform/4 :: (atom(), file:name(), ebt_config:config(), strikead_lists:kvlist_at()) ->
	error_m:monad(any()).
perform(Target, Dir, Config, Defaults) ->
	do([error_m ||
		{_Target, Module, Depends} <- find_target(Target),
		strikead_lists:eforeach(fun(T) -> perform(T, Dir, Config, Defaults) end, Depends),
		ebt:report_target(Target),
		Module:perform(Dir, Config, Defaults)
	]).

-spec find_target/1 :: (atom()) -> error_m:monad(target()).
find_target(Target) ->
	do([error_m ||
		Targets <- maybe_m:to_error_m(
			application:get_env(ebt, targets),
			"cannot find target mapping"
		),
		maybe_m:to_error_m(
			strikead_lists:keyfind(Target, 1, Targets),
			strikead_string:format("unknown target: ~s" , [Target])
		)
	]).
