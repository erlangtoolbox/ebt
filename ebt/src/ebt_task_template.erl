%% Copyright
-module(ebt_task_template).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).

perform(Target, _Dir, Config) ->
    do([error_m ||
        Version <- ebt_config:version(Config),
        Build <- ebt_config:build_number(Config),
        CommonMap <- return([{'EBT_VERSION', Version}, {'EBT_BUILD', Build} | ebt_xl_shell:getenv()]),
        ebt_xl_lists:eforeach(fun({In, Out, Map, Braces}) ->
            do([error_m ||
                InFile <- ebt_xl_file:read_file(In),
                ebt_xl_file:write_file(Out, ebt_xl_string:substitute(InFile, Map ++ CommonMap, Braces))
            ])
        end, ebt_config:value(Target, Config, []))
    ]).


