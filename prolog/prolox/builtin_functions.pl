:- module(builtin_functions,[define_builtins/2, clock_timer/3]).
:- use_module(environment).
:- use_module(library(system)).
:- use_module(interpreter).

clock_timer([Arg|Args], Env, state(Env, TimeMs)) :-
	writeln(Arg),
	statistics(runtime, [Milliseconds, _]),
	TimeMs = Milliseconds,
	format("~d ms",TimeMs).

define_builtins(Env, NewEnv) :-
	define_var("clock",
		lox_function([identifier("arg")],
			builtin(clock_timer),
			closure(Env)),
		Env,
		NewEnv).