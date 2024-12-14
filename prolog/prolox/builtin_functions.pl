:- module(builtin_functions,[define_builtins/2, clock_timer/3]).
:- use_module(environment).
:- use_module(library(system)).
:- use_module(interpreter).

clock_timer(_, Env, state(Env, TimeMs)) :-
	statistics(runtime, [Milliseconds, _]),
	TimeMs = Milliseconds,
	format("~d ms", TimeMs).

wrap_string_to_identifier([], []).

wrap_string_to_identifier([Arg|Args], [identifier(Arg)|Rest]) :-
	wrap_string_to_identifier(Args, Rest).

create_func(Name, Args, Func, Env, NewEnv) :-
	wrap_string_to_identifier(Args, IdentifiedArgs),
	define_var(Name,
		lox_function(IdentifiedArgs,
			builtin(Func),
			closure(Env)),
		Env,
		NewEnv).

define_builtins(Env, NewEnv) :-
	create_func("clock", ["arg"], clock_timer, Env, NewEnv).