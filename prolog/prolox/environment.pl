:- module(environment,[create_new_env/2, define_var/4, get_var/3, assign_var/4, extract_enclosing/2, env_enclose_with/3, merge_envs/3, flatten_env/2, update_env/3, assign_many_vars/3]).

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

merge_envs(env([], none), GEnv, GEnv) :- writeln("DONE").
merge_envs(env([], S), GEnv, R) :- 
	writeln("Reaching end"),
	merge_envs(S, GEnv, R).

% First match
merge_envs(env([Key-Value| T ], P), env([Key-_|T2], Closure), env([Key-Value | T3 ], R)) :-
	merge_envs(env(T, P), env(T2, Closure), env(T3, R)).

% no match keep trying
merge_envs(env([Key-Value|Tail], P), env([Key2-Value2|T2], Closure), env([Key2-Value2 | NewTail ], Closure)) :- 
	merge_envs(env([Key-Value|Tail], P), env(T2, Closure), env(NewTail, Closure)).

merge_envs(E, env(H, Closure), env(H, R)) :- 
	merge_envs(E, Closure, R).

% merge_envs(env([_], Closure), G, env(RH, P)) :- 
	% merge_envs(Closure, G, env(RH, P)).

flatten_env(env(C, none), C).

flatten_env(env([], P), T) :-
	flatten_env(P, T).

flatten_env(env([C], P), [C | T]) :-
	flatten_env(P, T).

update_env_aux([], _ , E, E).

%TODO doesn't quite work
update_env_aux([H|T], S , env([], none), E, rerun([H|T])):-
	writeln("None found, going next try through").

update_env_aux([Key-Value|T], S , env([], P), E, RR) :-
	writeln("Trying closure"),
	update_env_aux([Key-Value|T], S, P, E, RR).

update_env_aux([Key-Value|T], S , env([Key-_|T2], P), env(H,X), RR) :-
	writeln("Found, going next"),
	update_env_aux(T, S, env([Key-Value|T2], P), env(H,X), RR).

update_env_aux([Key-Value|T], S , env([Key2-Value2|T2], P), env([Key2-Value2|H2], L), RR) :-
	writeln("Skipping. not found"),
	update_env_aux([Key-Value|T], S, env(T2, P), env(H2, L), RR).

update_env(M, Env, X) :- 
	update_env_aux(M, Env, Env, R, rerun(RR)),
		length(RR, N), (N > 0 -> assign_many_vars(RR, R, X) ; X = R).

assign_many_vars([], X, X).
assign_many_vars([Key-Value|T], S, X) :- 
	assign_var(Key, Value, S, R), assign_many_vars(T, R, X).
