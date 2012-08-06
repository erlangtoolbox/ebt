-module(ebt_task).

-compile({parse_transform, do}).

-type target() :: {Name :: atom(), Module :: atom(), Depends :: [atom()]}.
-export([behaviour_info/1]).

-export([perform/5]).

behaviour_info(callbacks) ->
    [{perform, 3}];

behaviour_info(_) ->
    undefined.

-spec perform/5 :: (atom(), [atom()], file:name(), ebt_config:config(), strikead_lists:kvlist_at()) ->
    error_m:monad([atom()]).
perform(Level, Targets, Dir, Config, Defaults) ->
    perform(Level, Targets, Dir, Config, Defaults, []).

perform(_Level, [], _Dir, _Config, _Defaults, Acc) -> {ok, Acc};
perform(Level, [Target | Targets], Dir, Config, Defaults, Acc) ->
    case lists:member(Target, Acc) of
        true ->
                io:format("~p => ~s at ~s already done~n", [Level, Target, Dir]),
                perform(Level, Targets, Dir, Config, Defaults, Acc);
        false ->
                io:format("~p => ~s at ~s~n", [Level, Target, Dir]),
                do([error_m ||
                    {_Target, Module, Depends} <- find_target(Target),
                    DoneTargets <- perform(Level, Depends, Dir, Config, Defaults, Acc),
                    io:format("~s:~n", [Target]),
                    Module:perform(Dir, Config, Defaults),
                    perform(Level, Targets, Dir, Config, Defaults, [Target | Acc] ++ DoneTargets)
                ])
    end.


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
