%% Copyright
{application, ebt, [
    {description, "Erlang Build Tool"},
    {vsn, "0.0.0"},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        xl_stdlib,
        xl_io,
        getopt,
        erlandox
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
                {cc, ebt_task_cc}
            ]},
            {targets, [
                {dialyze, [build_plt, compile]},
                {compile, [template, leex, yecc, protoc, cc]},
                {eunit, [compile]},
                {package, [eunit, edoc, git_info]},
                {escript, [eunit]}
            ]}
        ]}
    ]}
]}.
