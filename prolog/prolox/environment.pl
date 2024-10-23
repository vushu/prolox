:- module(environment,[create_new_env/2, define_var/4, get_var/3]).

create_new_env(Enclosing, env([], Enclosing)).

extract_enclosing(env(_, Enclosing), Enclosing).

define_var(Key, Value, env(Store, EnclosingEnv), env([Key-Value| Store], EnclosingEnv)).

get_var(Key, env(Store, none), Value) :-
	member(Key - Value, Store), !.

get_var(Key, env(_, EnclosingEnv), Value) :-
	get_var(Key, EnclosingEnv, Value).