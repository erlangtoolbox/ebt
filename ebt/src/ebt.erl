-module(ebt).

-compile({parse_transform, do}).

-export([main/1, report/2, report/1, report_target/1, debug/2]).

-spec main/1 :: ([string()]) -> ok.
main(Args) ->
	ok = application:load(ebt),
	Defaults = [{target, 'otp-app'}, {output, filename:absname("./out")}],
	case build(Args, ".", Defaults) of
		ok ->
			io:format("BUILD SUCCESSFUL!~n");
		{error, E} ->
			io:format(standard_error, "BUILD FAILED: ~p~n", [E]),
			halt(1)
	end.

-spec build/3 :: ([string()], file:name(), strikead_lists:kvlist_at()) -> error_m:monad(any()).
build(Args, ContextDir, Defaults) ->
	ConfigFile = filename:join(ContextDir, "build.ebt"),
	do([error_m ||
		Config <- ebt_config:read(ConfigFile),
		build(Args, ContextDir, Config, Defaults)
	]).

-spec build/4 :: ([string()], file:name(), ebt_config:config(), strikead_lists:kvlist_at()) -> error_m:monad(any()).
build(Args, ContextDir, Config, Defaults) ->
	do([error_m ||
		strikead_lists:eforeach(
			fun(Dir) -> build(Args, filename:join(ContextDir, Dir), Defaults) end,
			ebt_config:value(subdirs, Config, [])
		),
		Target <- maybe_m:to_error_m(
			ebt_config:find_value(target, Config, Defaults),
			"no target"
		),
		io:format("==> ~s ~s~n", [Target, ContextDir]),
		ebt_task:perform(Target, ContextDir, Config, Defaults)
	]).


-spec report_target/1 :: (atom()) -> ok.
report_target(Target) ->
	io:format("~s:~n", [Target]).

-spec report/2 :: (string(), [term()]) -> ok.
report(Message, Params) ->
	io:format(string:join([Message, "~n"], ""), Params).

-spec report/1 :: (string()) -> ok.
report(Message) ->
	report(Message, []).

-spec debug/2 :: (string(), [term()]) -> ok.
debug(Message, Params) ->
	io:format(string:join(["debug: ", Message, "~n"], ""), Params).
