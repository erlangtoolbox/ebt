-module(ebt_target_mapping).

-compile({parse_transform, do}).

-export([get/2]).

-spec get/2 :: (atom(), ebt_config:config()) -> error_m:monad({module(), [atom()]}).
get(Target, Config) ->
    do([error_m ||
        {Modules, Targets} <- mapping_to_tuple(system_mapping()),
        {UserModules, UserTargets} <- mapping_to_tuple({ok,
            ebt_config:value(tasks, Config, [{modules, []}, {targets, []}])}),
        Module <- get_module(Target, Modules ++ UserModules),
        Depends <- return(ebt_strikead_lists:kvfind(Target, Targets, [])),
        UserDepends <- return(ebt_strikead_lists:kvfind(Target, UserTargets, [])),
        return({Module, Depends ++ UserDepends})
    ]).

system_mapping() ->
        maybe_m:to_error_m(
            application:get_env(ebt, tasks),
            "cannot find task mapping"
        ).

mapping_to_tuple(E = {error, _}) -> E;
mapping_to_tuple({ok, Mapping}) ->
    {ok, {
        ebt_strikead_lists:kvfind(modules, Mapping, []),
        ebt_strikead_lists:kvfind(targets, Mapping, [])
    }}.

get_module(Target, Modules) ->
    maybe_m:to_error_m(
        ebt_strikead_lists:kvfind(Target, Modules),
        ebt_strikead_string:format("cannot find module of ~p", [Target])
    ).

