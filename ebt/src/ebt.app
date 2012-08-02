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
            {compile, ebt_task_compile, [clean]},
            {'otp-app', ebt_task_otp_app, [compile]},
            {escript, ebt_task_escript, [compile]}
        ]}
    ]}
]}.
