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
                {otpapp, ebt_task_otpapp},
                {escript, ebt_task_escript},
                {protoc, ebt_protoc}
            ]},
            {targets, [
                {compile, [leex, yecc, protoc]},
                {eunit, [compile]},
                {otpapp, [eunit]},
                {escript, [eunit]}
            ]}
        ]}
    ]}
]}.
