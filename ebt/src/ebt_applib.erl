-module(ebt_applib).

-compile({parse_transform, do}).

-export([load/1, update/2, appname/2]).

-spec load/1 :: (file:name()) -> error_m:monad(application:application_spec()).
load(Dir) ->
	SrcDir = Dir ++ "/src",
	case filelib:wildcard(SrcDir ++ "/*.app") of
		[File] ->
			case strikead_file:read_terms(File) of
				{ok, [App = {application, _, Params}]} when is_list(Params) ->
					{ok, App};
				E = {error, _} -> E;
				_ -> {error, "wrong format of app file"}
			end;
		_ -> {error, "failed to locate single app file in " ++ SrcDir}
	end.

-spec update/2 :: (application:application_spec(), application:application_opt())
	-> application:application_spec().
update({application, App, Params}, Param) ->
    {application, App, strikead_lists:keyreplace_or_add(1, Params, Param)}.

-spec appname/2 :: (file:name(), ebt_config:config()) -> error_m:monad(string()).
appname(Dir, Config) ->
	do([error_m ||
		{_, Name, _} <- load(Dir),
		Version <- ebt_config:version(Config),
		return(strikead_string:join([Name, Version],"-"))
	]).
