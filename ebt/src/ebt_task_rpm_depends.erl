%% Copyright
-module(ebt_task_rpm_depends).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3]).

perform(Target, _Dir, Config) ->
    ebt__do([ebt__error_m ||
        RPMBuildDir <- ebt_config:outdir(rpmbuild, Config),
        ebt_rpmlib:prepare_environment(RPMBuildDir),
        LibDir <- ebt_config:find_value(Target, Config, dir),
        Libs <- return(filelib:wildcard(LibDir ++ "/*")),
        ebt__xl_lists:eforeach(fun(Lib) ->
            build_rpm(Config, filename:absname(Lib))
        end, Libs)
    ]).

build_rpm(Config, Lib) ->
    SpecTemplatePath = Lib ++ "/.ebt-info/rpm.spec",
    case ebt__xl_file:exists(SpecTemplatePath) of
        {ok, true} ->
            AppName = filename:basename(Lib),
            ebt__do([ebt__error_m ||
                SpecFile <- ebt_task_rpm_spec:spec_file_path(Config, AppName),
                ebt_task_rpm_spec:generate_spec_for_build(Config, SpecTemplatePath, AppName, Lib),
                ebt_task_rpm:rpmbuild(SpecFile)
            ]);
        {ok, false} -> io:format("ignore: no ~p~n", [SpecTemplatePath]);
        E -> E
    end.
