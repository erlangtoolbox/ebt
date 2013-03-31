%% Copyright
-module(ebt_task_rpm).
-author("Volodymyr Kyrychenko <vladimirk.kirichenko@gmail.com>").

-compile({parse_transform, ebt__do}).

-behaviour(ebt_task).

%% API
-export([perform/3, rpmbuild/1]).

perform(_Target, Dir, Config) ->
    ebt__do([ebt__error_m ||
        RPMBuildDir <- ebt_config:outdir(rpmbuild, Config),
        ebt_rpmlib:prepare_environment(RPMBuildDir),
        AppName <- ebt_config:appname_full(Dir, Config),
        AppProdDir <- ebt_config:app_outdir(production, Dir, Config),
        SpecTemplatePath <- ebt_task_rpm_spec:info_spec_file_path(Dir, Config),
        ebt_task_rpm_spec:generate_spec_for_build(Config, SpecTemplatePath, AppName, AppProdDir),
        SpecFile <- ebt_task_rpm_spec:spec_file_path(Config, AppName),
        rpmbuild(SpecFile)
    ]).



rpmbuild(SpecFile) -> ebt_cmdlib:exec({"rpmbuild -v -bb ~p", [SpecFile]}).
