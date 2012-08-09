{application, ebt, [
    {description, "Erlang Build Tool"},
    {vsn, "1"},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {env, [
        {targets, [
            {clean, ebt_task_clean, []},
            {compile, ebt_task_compile, []},
            {depends, ebt_task_depends, []},
            {release, ebt_task_release, []},
            {eunit, ebt_task_eunit, [compile]},
            {'otp-app', ebt_task_otp_app, [compile, eunit]},
            {escript, ebt_task_escript, [compile, eunit]}
        ]}
    ]}
]}.
