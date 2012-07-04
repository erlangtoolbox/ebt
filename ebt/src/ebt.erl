-module(ebt).

-compile({parse_transform, do}).

-export([main/1, report/2, report/1, report_target/1, debug/2]).

-spec main/1 :: ([string()]) -> ok.
main(Args) ->
	ok = application:load(ebt),
	case build(Args, ".") of
		ok ->
			io:format("BUILD SUCCESSFUL!~n");
		{error, E} ->
			io:format(standard_error, "BUILD FAILED: ~p~n", [E]),
			halt(1)
	end.

-spec build/2 :: ([string()], file:filename()) -> error_m:monad(any()).
build(Args, ContextDir) ->
	ConfigFile = filename:join(ContextDir, "build.ebt"),
	do([error_m ||
		Config <- ebt_config:read(ConfigFile),
		build(Args, ContextDir, Config)
	]).

-spec build/3 :: ([string()], file:filename(), ebt_config:config()) -> error_m:monad(any()).
build(Args, ContextDir, Config) ->
	do([error_m ||
		strikead_lists:eforeach(
			fun(Dir) -> build(Args, filename:join(ContextDir, Dir)) end,
			ebt_config:subdirs(Config)
		),
		io:format("==> ~s ~s~n", [ebt_config:target(Config), ContextDir]),
		ebt_task:perform(ebt_config:target(Config), ContextDir, Config)
	]).


-spec report_target/1 :: (atom()) -> ok.
report_target(Target) ->
	io:format("~s:~n", [Target]).

-spec report/2 :: (string(), [term()]) -> ok.
report(Message, Params) ->
	io:format(string:join(["\t", Message, "~n"], ""), Params).

-spec report/1 :: (string()) -> ok.
report(Message) ->
	report(Message, []).

-spec debug/2 :: (string(), [term()]) -> ok.
debug(Message, Params) ->
	io:format(string:join(["debug: ", Message, "~n"], ""), Params).
