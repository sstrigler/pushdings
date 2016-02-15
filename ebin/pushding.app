{application, pushding, [
	{description, "Pushing Daisies"},
	{vsn, "0.0.1"},
	{modules, ['pushding','pushding_rest_handler','pushding_sup','pushding_ws_handler']},
	{registered, [pushding_sup]},
	{applications, [kernel,stdlib,cowboy,gproc,jsx]},
	{mod, {pushding, []}}
]}.
