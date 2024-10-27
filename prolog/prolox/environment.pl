:- module(environment,[create_new_env/2, define_var/4, get_var/3, assign_var/4]).

create_new_env(Enclosing, env([], Enclosing)).

extract_enclosing(env(_, Enclosing), Enclosing).

define_var(Key, Value, env(Store, EnclosingEnv), env([Key-Value| Store], EnclosingEnv)).

get_var(Key, env(Store, none), Value) :-
	member(Key - Value, Store), !.

get_var(Key, env(Store, _), Value) :-
	member(Key - Value, Store), !.

get_var(Key, env(_, EnclosingEnv), Value) :-
	get_var(Key, EnclosingEnv, Value).

assign_var(_ ,_ ,  env([], none), env([], none)).

assign_var(Key, Value, env([Key-_|T], none), env([Key-Value|T], none)) :-
	!.

assign_var(Key, Value, env(_, Enclosing), R) :-
	assign_var(Key, Value, Enclosing, R), !.

assign_var(Key, Value, env([Key2-Value2|T], none), env([Key2-Value2|NewTail])) :-
	Key \= Key2, 
	assign_var(Key, Value, 
		env(T, none), 
		env(NewTail, none)).
