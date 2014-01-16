%% Copyright
{application, ebt, [
    {description, "Erlang Build Tool"},
    {vsn, "0.0.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        ebt__xl_stdlib,
        ebt__getopt,
        ebt__erlandox
    ]},
    {env, [
        {tasks, [
            {modules, [
                {clean, ebt_task_clean},
                {leex, ebt_task_leex},
                {yecc, ebt_task_yecc},
                {compile, ebt_task_compile},
                {depends, ebt_task_depends},
                {release, ebt_task_release},
                {eunit, ebt_task_eunit},
                {package, ebt_task_package},
                {escript, ebt_task_escript},
                {protoc, ebt_task_protoc},
                {shell, ebt_task_shell},
                {template, ebt_task_template},
                {git_info, ebt_task_git_info},
                {edoc, ebt_task_edoc},
                {build_plt, ebt_task_build_plt},
                {dialyze, ebt_task_dialyze},
                {cc, ebt_task_cc},
                {cover, ebt_task_cover},
                {cover_analyse, ebt_task_cover_analyse},
                {dot, ebt_task_graphviz},
                {neato, ebt_task_graphviz},
                {twopi, ebt_task_graphviz},
                {circo, ebt_task_graphviz},
                {fdp, ebt_task_graphviz},
                {sfdp, ebt_task_graphviz},
                {patchwork, ebt_task_graphviz}
            ]},
            {targets, [
                {dialyze, [build_plt, compile]},
                {compile, [template, leex, yecc, protoc, cc]},
                {eunit, [compile, cover]},
                {cover_analyse, [eunit]},
                {edoc, [dot, neato, twopi, circo, fdp, sfdp, patchwork]},
                {package, [cover_analyse, edoc, git_info]},
                {escript, [cover_analyse]}
            ]}
        ]}
    ]}
]}.
