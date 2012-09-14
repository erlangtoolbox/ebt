%% Copyright
-module(ebt_task_shell).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

%% API
-export([perform/3]).

perform(Target, Dir, Config) ->
    do([error_m ||
        Command <- ebt_config:find_value(Target, Config, command),
        WorkingDir <- ebt_config:value(Target, Config, dir, Dir),
        ebt_cmdlib:exec(Command, WorkingDir)
    ]).


