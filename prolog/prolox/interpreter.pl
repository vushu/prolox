:- module(interpreter,[interpret/1]).

interpret([Stmt|Stmts]) :-
	evaluate(Stmt, _), 
	evaluate_rest(Stmts).

evaluate_rest([Stmt|Stmts]) :-
	evaluate(Stmt, _), 
	evaluate_rest(Stmts).

evaluate_rest([]).

evaluate(print(X), _) :-
	evaluate(X, R), 
	format("~w~n", R), !.

evaluate(primary(true), true).

evaluate(primary(false), false).

evaluate(primary(nil), nil).

evaluate(primary(X), Y) :-
	evaluate(X, Y).

%bool and nil

evaluate(number(X), X).
evaluate(string(X), X).

evaluate(expr_stmt(S), R) :-
	evaluate(S, R).

evaluate(factor(left(L), right(R), op(Op)), Res) :-
	evaluate(L, L2), 
	evaluate(R, R2), 
	(Op = star, Res is L2*R2;
	Res is L2/R2).

evaluate(term(left(L), right(R), op(Op)), Res) :-
	evaluate(L, L2), 
	evaluate(R, R2), 
	(Op = plus, Res is L2 + R2;
	Res is L2 - R2).

evaluate(or(left(L), right(R), op(token(or, _))), Res) :-
	evaluate(L, L2), 
	evaluate(R, R2), Res is L2;
	R2.

evaluate(and(left(L), right(R), op(token(and, _))), Res) :-
	evaluate(L, L2), 
	evaluate(R, R2), Res is L2, R2.

evaluate(_, _) :-
	writeln("Unknown stmt").