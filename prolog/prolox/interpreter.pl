:- module(interpreter,[interpret/1]).
:- use_module(environment).

:- discontiguous evaluate/3.

%Single statement.

interpret([Stmt]) :-
	create_new_env(none, Env), !, 
	evaluate(Stmt, Env, _).

%Multiple statements.

interpret([Stmt|Stmts]) :-
	create_new_env(none, Env), 
	evaluate(Stmt, Env, 
		state(Env1, _)), 
	evaluate_rest(Stmts, Env1, _).

evaluate_rest([Stmt|Stmts], Env, C) :-
	evaluate(Stmt, Env, 
		state(Env1, _)), 
	evaluate_rest(Stmts, Env1, _).

evaluate_rest([],_, _).

evaluate(print(X), Env, state(Env1, _)) :-
	evaluate(X, Env, 
		state(Env1, R)), 
	writeln(R), !.
	% format("printing: ~w~n", R), !.

evaluate(primary(true), Env, state(Env,true)).

evaluate(primary(false), Env, state(Env,false)).

evaluate(primary(nil), Env, state(Env,nil)).

evaluate(primary(X), Env, Y) :-
	evaluate(X, Env, Y).

%bool and nil

evaluate(number(X), Env, state(Env, X)).
evaluate(string(X), Env, state(Env,X)).

evaluate(expr_stmt(S), Env, R) :-
	evaluate(S, Env, R).

evaluate(factor(left(L), right(R), op(Op)), Env, state(Env2, Res)) :-
	evaluate(L, Env, 
		state(Env1, L2)), 
	evaluate(R, Env1, 
		state(Env2, R2)), 
	(Op = star, Res is L2*R2;
	Res is L2/R2).

evaluate(term(left(L), right(R), op(Op)), Env, state(Env2, Res)) :-
	evaluate(L, Env, 
		state(Env1, L2)), 
	evaluate(R, Env1, 
		state(Env2, R2)), 
	(Op = plus, Res is L2 + R2;
	Res is L2 - R2).

evaluate(or(left(L), right(R), op(token(or, _))), Env, state(Env2, Res)) :-
	evaluate(L, Env, 
		state(Env1, L2)), 
	is_truthy(L2, T), 
	evaluate(R, Env1, 
		state(Env2, R2)), 
	is_truthy(R2, T2), 
	(T = true, Res = L2;
	T2 = true, Res = R2;
	Res = false).

evaluate(and(left(L), right(R), op(token(and, _))), Env, state(Env2, Res)) :-
	evaluate(L, Env, 
		state(Env1, L2)), 
	is_truthy(L2, T), 
	evaluate(R, Env1, 
		state(Env2, R2)), 
	is_truthy(R2, T2), 
	(T = true, T2 = true, Res = true;
	Res = false).

evaluate(comparison(left(L), right(R), op(token(Op, _))), Env, state(Env2, Res)) :-
	evaluate(L, Env, 
		state(Env1, L2)), 
	evaluate(R, Env1, 
		state(Env2, R2)), 
	(Op = greater, L2 > R2, Res = true;
	Op = greater_equal, L2 >= R2, Res = true;
	Op = less, L2 < R2, Res = true;
	Op = less_equal, L2 =< R2, Res = true;
	Res = false).

evaluate(equality(left(L), right(R), op(token(Op, _))), Env, state(Env2, Res)) :-
	evaluate(L, Env, 
		state(Env1, L2)), 
	evaluate(R, Env1, 
		state(Env2, R2)), 
	(Op = bang_equal, 
		not(L2 = R2), Res = true;
		Op = equal_equal, L2 == R2, Res = true;
		Res = false).

evaluate(group(E), Env, R) :-
	evaluate(E, Env, R).

evaluate(unary(op(token(Op, _)), right(E)), Env, state(Env1, Res)) :-
	evaluate(E, Env, 
		state(Env1, R)), 
	(Op = bang, 
		negate(R, Res);
		Op = minus, Res is  - R;
		format("Op is unknown: ~w~n", Op), !).

evaluate(expr_stmt(Expr), Env, R) :-
	evaluate(Expr, Env, R).

evaluate(var_decl(name(Token), initializer(Stmt)), Env, state(Env2, none)) :-
	evaluate(Stmt, Env, 
		state(Env1, V)), !, 
	define_var(Token, V, Env1, Env2).

evaluate(block([Stmt|Stmts]), Env, _) :-
	writeln("Block-stmt"), !, 
	evaluate(Stmt, Env, 
		state(Env1, _)), 
	evaluate_block_rest(Stmts, Env1, _).

evaluate_block_rest([Stmt|Stmts], Env, _) :-
	evaluate(Stmt, Env, 
		state(Env1, _)), 
	evaluate_block_rest(Stmts, Env1, _).

evaluate_block_rest([], _, _).

% evaluate([assigment(assign_name(E), value(E2))| Stms], Env, S) :-
	% evaluate(E, Env, S),
	% define_var()

evaluate(_, _, _) :-
	writeln("Unknown stmt"), halt.

% block_stmt(block(Stmts))-->

extract_result(state(_, Res), Res).
extract_result(state(Env, _), Env).
extract_result(Res, Res).

negate(true, false).
negate(false, true).

is_truthy(true, true).
is_truthy(false, false).
is_truthy(nil, false).
is_truthy(_, true).