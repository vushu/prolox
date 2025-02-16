:- module(environment,[create_new_env/2, define_var/4, get_var/3, assign_var/4, extract_enclosing/2, env_enclose_with/3]).

create_new_env(Enclosing, env([], Enclosing)).

env_enclose_with(env(C, none), With, env(C, With)).
env_enclose_with(env(C, X), With, env(C, S)) :- writeln("Append to back"), env_enclose_with(X, With, S).

extract_enclosing(env(_, none), env([], none)) :- !.
extract_enclosing(env(_, C), C).

define_var(Key, Value, env(Store, EnclosingEnv), env([Key-Value| Store], EnclosingEnv)).

get_var(Key, env(Store, _), Value) :-
	member(Key - Value, Store), !.

get_var(Key, env(_, EnclosingEnv), Value) :-
	get_var(Key, EnclosingEnv, Value).

assign_var(Key, Value, env([Key-_|T], R), env([Key-Value|T], R)) :-
	!.

assign_var(Key, Value, env([Key2-Value2|T], Enc), env([Key2-Value2|NewTail], Enc)) :-
	Key \= Key2,!,
	assign_var(Key, Value, 
		env(T, Enc), 
		env(NewTail, Enc)).

assign_var(Key, Value, env(S, Enc), env(S, R)) :-
	!, 
	assign_var(Key, Value, Enc, R).

assign_var(_, _, env([], _), none) :-
	writeln("Failed to assign"), halt.