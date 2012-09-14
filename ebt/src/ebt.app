{application, ebt, [
    {description, "Erlang Build Tool"},
    {vsn, "bootstrap"},
    {registered, []},
    {applications, [
        kernel,
        stdlib
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
                {rpm_spec, ebt_task_rpm_spec},
                {rpm, ebt_task_rpm},
                {rpm_depends, ebt_task_rpm_depends}
            ]},
            {targets, [
                {compile, [leex, yecc, protoc]},
                {eunit, [compile]},
                {package, [eunit, rpm_spec]},
                {escript, [eunit]},
                {rpm, [eunit, rpm_spec]},
                {rpm_depends, [depends]}
            ]}
        ]}
    ]}
]}.
