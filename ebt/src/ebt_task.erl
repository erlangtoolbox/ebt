-module(ebt_task).

-compile({parse_transform, do}).

-export([behaviour_info/1]).

-export([perform/4]).

behaviour_info(callbacks) ->
    [{perform, 2}];

behaviour_info(_) ->
    undefined.

-spec perform/4 :: (atom(), [atom()], file:name(), ebt_config:config()) ->
    error_m:monad([atom()]).
perform(Level, Targets, Dir, Config) ->
    perform(Level, Targets, Dir, Config, []).

perform(_Level, [], _Dir, _Config, Acc) -> {ok, Acc};
perform(Level, [Target | Targets], Dir, Config, Acc) ->
    case lists:member(Target, Acc) of
        true ->
            io:format("~p => ~s at ~s already done~n", [Level, Target, Dir]),
            perform(Level, Targets, Dir, Config, Acc);
        false ->
            io:format("~p => ~s at ~s~n", [Level, Target, Dir]),
            do([error_m ||
                {Module, Depends} <- ebt_target_mapping:get(Target, Config),
                DoneTargets <- perform(Level, Depends, Dir, Config, Acc),
                io:format("~s:~n", [Target]),
                ebt:load_libraries(Config),
                Module:perform(Dir, Config),
                perform(Level, Targets, Dir, Config, [Target | Acc] ++ DoneTargets)
            ])
    end.
