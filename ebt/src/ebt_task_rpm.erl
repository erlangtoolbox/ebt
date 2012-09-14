%% Copyright
-module(ebt_task_rpm).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, do}).

-behaviour(ebt_task).

%% API
-export([perform/3, rpmbuild/1]).

perform(_Target, Dir, Config) ->
    do([error_m ||
        RPMBuildDir <- ebt_config:outdir(rpmbuild, Config),
        ebt_rpmlib:prepare_environment(RPMBuildDir),
        AppName <- ebt_config:appname_full(Dir, Config),
        SpecFile <- ebt_task_rpm_spec:build_spec_file(Config, AppName),
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        SpecTemplatePath <- ebt_task_rpm_spec:info_spec_file(Dir, Config),
        ebt_task_rpm_spec:generate_spec_for_build(Config, SpecTemplatePath, AppName, AppProdDir),
        rpmbuild(SpecFile)
    ]).



rpmbuild(SpecFile) -> ebt_cmdlib:exec({"rpmbuild -v -bb ~p", [SpecFile]}).
