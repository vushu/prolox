:- begin_tests(env).
:- use_module(prolog/prolox/environment).

test(setting_variable) :-
	create_new_env(Env), 
	define_var(hello, "hello world", Env, UpdateEnv), 
	get_var(hello, UpdateEnv, Var), 
    format("Variable is: ~w~n ", Var),
    Var = "hello world".



:- end_tests(env).