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
			{compile, ebt_task_compile, []},
			{'otp-app', ebt_task_otp_app, [compile]},
			{escript, ebt_task_escript, [compile]}
		]}
	]}
]}.
