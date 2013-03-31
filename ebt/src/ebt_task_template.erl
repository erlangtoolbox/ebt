%% Copyright
-module(ebt_task_template).
-author("volodymyr.kyrychenko@strikead.com").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).

perform(Target, _Dir, Config) ->
    ebt__do([ebt__error_m ||
        Version <- ebt_config:version(Config),
        Build <- ebt_config:build_number(Config),
        CommonMap <- return([{'EBT_VERSION', Version}, {'EBT_BUILD', Build} | ebt__xl_shell:getenv()]),
        ebt__xl_lists:eforeach(fun({In, Out, Map, Braces}) ->
            ebt__do([ebt__error_m ||
                InFile <- ebt__xl_file:read_file(In),
                ebt__xl_file:write_file(Out, ebt__xl_string:substitute(InFile, Map ++ CommonMap, Braces))
            ])
        end, ebt_config:value(Target, Config, []))
    ]).


