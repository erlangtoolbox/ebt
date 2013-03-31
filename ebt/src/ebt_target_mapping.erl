-module(ebt_target_mapping).

-compile({parse_transform, ebt__do}).

-export([get/2]).

-spec(get(atom(), ebt_config:config()) -> ebt__error_m:monad({module(), [atom()]})).
get(Target, Config) ->
    ebt__do([ebt__error_m ||
        {Modules, Targets} <- mapping_to_tuple(system_mapping()),
        {UserModules, UserTargets} <- mapping_to_tuple({ok,
            ebt_config:value(tasks, Config, [{modules, []}, {targets, []}])}),
        Module <- get_module(Target, Modules ++ UserModules),
        Depends <- return(ebt__xl_lists:kvfind(Target, Targets, [])),
        UserDepends <- return(ebt__xl_lists:kvfind(Target, UserTargets, [])),
        return({Module, Depends ++ UserDepends})
    ]).

system_mapping() ->
    ebt__option_m:to_ebt__error_m(
        application:get_env(ebt, tasks),
        "cannot find task mapping"
    ).

mapping_to_tuple(E = {error, _}) -> E;
mapping_to_tuple({ok, Mapping}) ->
    {ok, {
        ebt__xl_lists:kvfind(modules, Mapping, []),
        ebt__xl_lists:kvfind(targets, Mapping, [])
    }}.

get_module(Target, Modules) ->
    ebt__option_m:to_ebt__error_m(
        ebt__xl_lists:kvfind(Target, Modules),
        ebt__xl_string:format("cannot find module of ~p", [Target])
    ).

