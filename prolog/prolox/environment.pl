:- module(environment,[create_new_env/2, define_var/4, get_var/3, assign_var/4, extract_enclosing/2, env_enclose_with/3, merge_envs/3]).

create_new_env(Enclosing, env([], Enclosing)).

env_enclose_with(env(C, _), With, env(C, With)).

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
	(Key \= Key2 ->
	assign_var(Key, Value, env(T, Enc), env(NewTail, Enc))).

assign_var(Key, Value, env(S, Enc), env(S, R)) :-
	!, 
	assign_var(Key, Value, Enc, R).

assign_var(_, _, env([], _), none) :-
	writeln("Failed to assign"), halt.

merge_envs(none, GEnv, GEnv).

merge_envs(env([Key-Value|_], _), env([Key-_|T2], Closure), env([Key-Value | T2], Closure)).

% merge_envs(env([Key-Value|_], _), env([Key-_|T2], Closure), env([Key-Value | T2], Closure)) :- 

% merge_envs(env([Key-Value|T1], _), env([Key2-Value2|T2]), env([Key2-Value2| NewTail], T2)) :-
	% (Key \= Key2 -> merge_envs(env([Key-Value], _), env(T1, T2), env(NewTail, T2))).


