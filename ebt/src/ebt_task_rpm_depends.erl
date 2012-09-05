%% Copyright
-module(ebt_task_rpm_depends).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).

perform(Target, _Dir, Config) ->
    do([error_m ||
        ebt_task_rpm:prepare_environment(Config),
        Specs <- return(ebt_config:value(Target, Config, specs, [])),
        ebt_xl_lists:eforeach(fun(Spec) -> build_rpm(Target, Config, Spec) end, Specs)
    ]).

build_rpm(Target, Config, {Lib, Spec}) ->
    do([error_m ||
        LibDir <- ebt_config:find_value(Target, Config, dir),
        case lists:reverse(lists:sort(filelib:wildcard(ebt_xl_string:join([LibDir, "/", Lib, "-*"])))) of
            [L | _] ->
                do([error_m ||
                    {_, AppName, Params} <- ebt_applib:load(L ++ "/ebin"),
                    ebt_task_rpm:prepare_spec(Config, Spec, filename:absname(L), atom_to_list(AppName), ebt_xl_lists:kvfind(vsn, Params, "unknown")),
                    ebt_task_rpm:rpmbuild(Config, atom_to_list(AppName))
                ]);
            _ -> {error, ebt_xl_string:format("cannot find library ~s", [Lib])}
        end
    ]).