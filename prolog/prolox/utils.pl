:- module(utils,[print_stmts/1, print_env/1]).
:- use_module(library(pairs)).

print_stmts([block([])]).
print_stmts([block([Stmt|Stmts])]) :-
    format("~w~n~n", Stmt), print_stmts(Stmts).


print_stmts([]).

print_stmts([Stmt|Stmts]) :-
    format("~w~n~n", Stmt), print_stmts(Stmts).

print_env(Env) :- print_env_(Env, 0).

print_env_(env([], Some), Level) :-
    (Some = none -> writeln("Done"); print_env_(Some, Level)).

print_env_(env(Map, Closure), Level) :-
    keys(Map, Keys),
    print_keys(Keys, Level),
    (Closure = none -> true; tab(Level), writeln("[Closure] ------> "), Next is Level + 4, print_env_(Closure, Next)).

% Extract and print the keys
keys(Map, Keys) :-
    pairs_keys(Map, Keys).

print_keys([], _).
print_keys([Key|Rest], Tab) :-
    tab(Tab), writeln(Key),
    print_keys(Rest, Tab).
