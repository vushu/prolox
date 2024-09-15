:- module(interpreter,[interpret/1]).

interpret([Stmt|Stmts]) :-
	evaluate(Stmt, _), 
	evaluate_rest(Stmts).

evaluate_rest([Stmt|Stmts]) :-
	evaluate(Stmt, _), 
	evaluate_rest(Stmts).

evaluate(print(X), _) :-
	evaluate(X, R), 
	format("Printing: ~w", R).

evaluate(primary(X), Y) :-
	evaluate(X, Y).

evaluate(number(X), X).


