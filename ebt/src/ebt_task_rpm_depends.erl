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
        LibDir <- ebt_config:find_value(Target, Config, dir),
        Libs <- return(filelib:wildcard(LibDir ++ "/*")),
        ebt_xl_lists:eforeach(fun(Lib) -> build_rpm(Config, Lib) end, Libs)
    ]).

build_rpm(Config, Lib) ->
    do([error_m ||
        RPMBuildDir <- ebt_config:outdir(rpmbuild, Config),
        SpecTemplate <- ebt_xl_file:read_file(Lib ++ "/.ebt-info/rpm.spec"),
        SpecFile <- ebt_task_rpm:spec_file(Config, filename:basename(Lib)),
        ebt_xl_file:write_file(SpecFile, iolist_to_binary([
            "#Spec generated by Erlang Build Tool\n\n",
            "%define _topdir " ++ RPMBuildDir ++ "\n",
            "%define _builddir " ++ Lib ++ "\n",
            SpecTemplate
        ])),
        ebt_task_rpm:rpmbuild(SpecFile)
    ]).
