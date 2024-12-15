:- module(utils,[print_stmts/1]).

print_stmts([block([])]).
print_stmts([block([Stmt|Stmts])]) :-
    format("~w~n~n", Stmt), print_stmts(Stmts).


print_stmts([]).

print_stmts([Stmt|Stmts]) :-
    format("~w~n~n", Stmt), print_stmts(Stmts).
