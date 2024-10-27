:- begin_tests(env).
:- use_module(prolog/prolox/environment).

test(setting_variable_env_with_enclosing) :-
	create_new_env(none, Enclosing), 
	define_var(jumanji, "jumanji world", Enclosing, NewEnclosing), 
	create_new_env(NewEnclosing, Env), 
	get_var(jumanji, Env, "jumanji world").

test(assign_env) :-
	create_new_env(none, Env), 
	define_var(jumanji, "jumanji world", Env, UpdatedEnv), 
	assign_var(jumanji, "huhuhu", UpdatedEnv, NewerEnv), 
	get_var(jumanji, NewerEnv, "huhuhu").

test(assign_env_enclosing) :-
	create_new_env(none, Enclosing), 
	define_var(jumanji, "jumanji world", Enclosing, UpdatedEnclosing), 
	create_new_env(UpdatedEnclosing, Env), 
	assign_var(jumanji, "changed jumanji", Env, NewerEnv), 
	get_var(jumanji, NewerEnv, "changed jumanji").
	
 :- end_tests(env).