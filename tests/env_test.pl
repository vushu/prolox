:- begin_tests(env).
:- use_module(prolog/prolox/environment).

test(setting_variable_env_with_enclosing) :-
	create_new_env(none, Env), 
	define_var(jumanji, "jumanji world", Env, UpdatedEnv), 
	create_new_env(UpdatedEnv, NewEnv), 
	get_var(jumanji, NewEnv, Var), 
	format("Variable is: ~w~n ", Var), Var = "jumanji world".
:- end_tests(env).