:- begin_tests(env).
:- use_module(prolog/prolox/environment).

test(setting_variable_env_with_enclosing) :-
	create_new_env(none, Enclosing), 
	define_var(jumanji, "jumanji world", Enclosing, NewEnclosing), 
	create_new_env(NewEnclosing, Env), 
	get_var(jumanji, Env, "jumanji world").

test(setting_variable_env) :-
	create_new_env(none, Env), 
	define_var(jumanji, "jumanji", Env, Env2), 
	define_var(world, "world", Env2, Env3), 
	get_var(jumanji, Env3, "jumanji"), 
	get_var(world, Env3, "world"), 
	assign_var(jumanji, "jumanjino", Env3, EnvLast), 
	get_var(jumanji, EnvLast, "jumanjino").

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

test(get_var_with_enclosing) :-
	create_new_env(none, Enclosing), 
	define_var(jumanji, "jumanji world", Enclosing, UpdatedEnclosing), 
	create_new_env(UpdatedEnclosing, Env), 
	define_var(jumanji, "hej med dig", Env, UpdatedEnv), 
	get_var(jumanji, UpdatedEnv, "hej med dig").

test(assign_env_more_enclosing) :-
	create_new_env(none, Initial), 
	define_var(a, "A monkey", Initial, InitialUpdated), 
	create_new_env(InitialUpdated, Second), 
	create_new_env(Second, Third), 
	assign_var(a, "A lonkey", Third, Last), 
	get_var(a, Last, "A lonkey").

test(extract_enclosing) :-
	Env = env([i-2],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)),
	extract_enclosing(Env, env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)).

test(extract_enclosing2) :-
	Env = env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none), 
	extract_enclosing(Env, env([], none)).

test(extract_enclosing3) :-
	Env = env([], env([],env([i-2],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)))),
	extract_enclosing(Env, Env1),
	extract_enclosing(Env1, Env2), 
	extract_enclosing(Env2, env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)).

test(extract_enclosing4) :-
	Env= env([],env([i-5],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))),
	extract_enclosing(Env,env([i-5],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))).

 :- end_tests(env).