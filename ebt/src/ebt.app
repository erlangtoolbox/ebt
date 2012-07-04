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
			{erlc, ebt_task_erlc, []},
			{'otp-app', ebt_task_otp_app, [erlc]}
		]}
	]}
]}.
