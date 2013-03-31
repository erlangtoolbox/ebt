%% Copyright
-module(ebt_task_shell).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, ebt__do}).

%% API
-export([perform/3]).

perform(Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        Command <- ebt_config:find_value(Target, Config, command),
        ebt_cmdlib:exec(Command, ebt_config:value(Target, Config, dir, Dir))
    ]).


