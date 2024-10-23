:- module(interpreter,[interpret/1]).
:- use_module(environment).

:- discontiguous evaluate/3.

interpret([Stmt|Stmts]) :-
	create_new_env(none, Env), 
	evaluate(Stmt, Env, C), 
	evaluate_rest(Stmts, Env, C).

evaluate_rest([Stmt|Stmts], Env, C) :-
	evaluate(Stmt, Env, C), 
	evaluate_rest(Stmts, Env, C).

evaluate_rest([],_, _).

evaluate(print(X), Env, _) :-
	evaluate(X, Env, R), 
	format("printing: ~w~n", R), !.

evaluate(primary(true),_, true).

evaluate(primary(false),_, false).

evaluate(primary(nil),_, nil).

evaluate(primary(X), Env, Y) :-
	evaluate(X, Env, Y).

%bool and nil

evaluate(number(X),_, X).
evaluate(string(X),_, X).

evaluate(expr_stmt(S), Env, R) :-
	evaluate(S, Env, R).

evaluate(factor(left(L), right(R), op(Op)), Env, Res) :-
	evaluate(L, Env, L2), 
	evaluate(R, Env, R2), 
	(Op = star, Res is L2*R2;
	Res is L2/R2).

evaluate(term(left(L), right(R), op(Op)), Env, Res) :-
	evaluate(L, Env, L2), 
	evaluate(R, Env, R2), 
	(Op = plus, Res is L2 + R2;
	Res is L2 - R2).

evaluate(or(left(L), right(R), op(token(or, _))), Env, Res) :-
	evaluate(L, Env, L2), 
	is_truthy(L2, T), 
	evaluate(R, Env, R2), 
	is_truthy(R2, T2), 
	(T = true, Res = L2;
	T2 = true, Res = R2;
	Res = false).

evaluate(and(left(L), right(R), op(token(and, _))), Env, Res) :-
	evaluate(L, Env, L2), 
	is_truthy(L2, T), 
	evaluate(R, Env, R2), 
	is_truthy(R2, T2), 
	(T = true, T2 = true, Res = true;
	Res = false).

evaluate(comparison(left(L), right(R), op(token(Op, _))), Env, Res) :-
	evaluate(L, Env, L2), 
	evaluate(R, Env, R2), 
	(Op = greater, L2 > R2, Res = true;
	Op = greater_equal, L2 >= R2, Res = true;
	Op = less, L2 < R2, Res = true;
	Op = less_equal, L2 =< R2, Res = true;
	Res = false).

evaluate(equality(left(L), right(R), op(token(Op, _))), Env, Res) :-
	evaluate(L, Env, L2), 
	evaluate(R, Env, R2), 
	(Op = bang_equal, 
		not(L2 = R2), Res = true;
		Op = equal_equal, L2 == R2, Res = true;
		Res = false).

evaluate(group(E), Env, R) :-
	evaluate(E, Env, R).

evaluate(unary(op(token(Op, _)), right(E)), Env, R) :-
	evaluate(E, Env, E2), 
	(Op = bang, 
		negate(E2, R);
		Op = minus, R is  - E2;
		format("Op is unknown: ~w~n", Op), !).

evaluate(expr_stmt(Expr), Env, R) :-
	evaluate(Expr, Env, R).

evaluate(block([Stmt|Stmts]),Env, S) :-
	% writeln("Block-stmt"),
	evaluate(Stmt, Env, S), 
	evaluate_block_rest(Stmts, Env, S).

evaluate_block_rest([Stmt|Stmts], Env, S) :-
	evaluate(Stmt,Env, S), 
	evaluate_block_rest(Stmts,Env, S).

evaluate_block_rest([], _, _).

evaluate(_, _, _) :-
	writeln("Unknown stmt"), halt.

% block_stmt(block(Stmts))-->


negate(true, false).
negate(false, true).

is_truthy(true, true).
is_truthy(false, false).
is_truthy(nil, false).
is_truthy(_, true).