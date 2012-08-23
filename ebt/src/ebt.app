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
                {'otp-app', ebt_task_otp_app},
                {escript, ebt_task_escript}
            ]},
            {targets, [
                {compile, [leex, yecc]},
                {eunit, [compile]},
                {'otp-app', [eunit]},
                {escript, [eunit]}
            ]}
        ]}
    ]}
]}.
