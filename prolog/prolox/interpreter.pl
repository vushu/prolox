:- module(interpreter,[interpret/1]).
:- use_module(environment).

interpret([Stmt|Stmts]) :-
	C = context(
		res(_), _), 
	evaluate(Stmt, C), 
	evaluate_rest(Stmts, C).

evaluate_rest([Stmt|Stmts], C) :-
	evaluate(Stmt, C), 
	evaluate_rest(Stmts, C).

evaluate_rest([], _).

evaluate(print(X), _) :-
	evaluate(X, R), 
	format("printing: ~w~n", R), !.

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
	is_truthy(L2, T), 
	evaluate(R, R2), 
	is_truthy(R2, T2), 
	(T = true, Res = L2;
	T2 = true, Res = R2;
	Res = false).

evaluate(and(left(L), right(R), op(token(and, _))), Res) :-
	evaluate(L, L2), 
	is_truthy(L2, T), 
	evaluate(R, R2), 
	is_truthy(R2, T2), 
	(T = true, T2 = true, Res = true;
	Res = false).

evaluate(comparison(left(L), right(R), op(token(Op, _))), Res) :-
	evaluate(L, L2), 
	evaluate(R, R2), 
	(Op = greater, L2 > R2, Res = true;
	Op = greater_equal, L2 >= R2, Res = true;
	Op = less, L2 < R2, Res = true;
	Op = less_equal, L2 =< R2, Res = true;
	Res = false).

evaluate(equality(left(L), right(R), op(token(Op, _))), Res) :-
	evaluate(L, L2), 
	evaluate(R, R2), 
	(Op = bang_equal, 
		not(L2 = R2), Res = true;
		Op = equal_equal, L2 == R2, Res = true;
		Res = false).

evaluate(group(E), R) :-
	evaluate(E, R).

evaluate(unary(op(token(Op, _)), right(E)), R) :-
	evaluate(E, E2), 
	(Op = bang, 
		negate(E2, R);
		Op = minus, R is  - E2;
		format("Op is unknown: ~w~n", Op), !).

evaluate(expr_stmt(Expr), Env) :-
	create_new_env(Env), 
	evaluate(Expr, Env).

% evaluate(block([Stmt|Stmts]), S) :-
% 	evaluate(Stmt, S), 
% 	evaluate_block_rest(Stmts, S).

% evaluate_block_rest([Stmt|Stmts], S) :-
% 	evaluate(Stmt, S), 
% 	evaluate_block_rest(Stmts, S).

% evaluate_block_rest([], _).


evaluate(_, _) :-
	writeln("Unknown stmt"), halt.

% block_stmt(block(Stmts))-->


negate(true, false).
negate(false, true).

is_truthy(true, true).
is_truthy(false, false).
is_truthy(nil, false).
is_truthy(_, true).