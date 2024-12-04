:- module(interpreter,[interpret/1]).
:- use_module(environment).

:- discontiguous evaluate/3.

%Single statement.

interpret([Stmt]) :-
	create_new_env(none, Env),
	!,
	evaluate(Stmt, Env, _),
	!.

%Multiple statements.

interpret([Stmt|Stmts]) :-
	create_new_env(none, Env),
	evaluate(Stmt,
		Env,
		state(Env1, _)),
	evaluate_rest(Stmts, Env1, _).

evaluate_rest([Stmt|Stmts], Env, state(NewEnv, _)) :-
	evaluate(Stmt,
		Env,
		state(Env1, _)),
	evaluate_rest(Stmts,
		Env1,
		state(NewEnv, _)),
	!.

evaluate_rest([], Env, state(Env, _)).

evaluate(print(Stmt), Env, state(Env1, none)) :-
	evaluate(Stmt,
		Env,
		state(Env1, R)),
	writeln(R),
	!.

evaluate(primary(true), Env, state(Env, true)).

evaluate(primary(false), Env, state(Env, false)).

evaluate(primary(nil), Env, state(Env, nil)).

evaluate(primary(X), Env, Y) :-
	evaluate(X, Env, Y).

%bool and nil

evaluate(number(X), Env, state(Env, X)).

evaluate(string(X), Env, state(Env, X)).

evaluate(factor(left(L), right(R), op(Op)), Env, state(Env2, Res)) :-
	evaluate(L,
		Env,
		state(Env1, L2)),
	evaluate(R,
		Env1,
		state(Env2, R2)),
	(Op = star, Res is L2*R2;
Res is L2/R2).

evaluate(term(left(L), right(R), op(Op)), Env, state(Env2, Res)) :-
	evaluate(L,
		Env,
		state(Env1, L2)),
	evaluate(R,
		Env1,
		state(Env2, R2)),
	(Op = plus, Res is L2 + R2;
Res is L2 - R2).

evaluate(or(left(L), right(R), op(token(or, _))), Env, state(Env2, Res)) :-
	evaluate(L,
		Env,
		state(Env1, L2)),
	is_truthy(L2, T),
	evaluate(R,
		Env1,
		state(Env2, R2)),
	is_truthy(R2, T2),
	(T = true, Res = L2;
T2 = true, Res = R2;
Res = false).

evaluate(and(left(L), right(R), op(token(and, _))), Env, state(Env2, Res)) :-
	evaluate(L,
		Env,
		state(Env1, L2)),
	is_truthy(L2, T),
	evaluate(R,
		Env1,
		state(Env2, R2)),
	is_truthy(R2, T2),
	(T = true, T2 = true, Res = true;
Res = false).

evaluate(comparison(left(L), right(R), op(token(Op, _))), Env, state(Env2, Res)) :-
	evaluate(L,
		Env,
		state(Env1, L2)),
	evaluate(R,
		Env1,
		state(Env2, R2)),
	(Op = greater, L2 > R2, Res = true;
Op = greater_equal, L2 >= R2, Res = true;
Op = less, L2 < R2, Res = true;
Op = less_equal, L2 =< R2, Res = true;
Res = false).

evaluate(equality(left(L), right(R), op(token(Op, _))), Env, state(Env2, Res)) :-
	evaluate(L,
		Env,
		state(Env1, L2)),
	evaluate(R,
		Env1,
		state(Env2, R2)),
	(Op = bang_equal,
		not(L2 = R2),
		Res = true;
Op = equal_equal,
		L2 == R2,
		Res = true;
Res = false).

evaluate(group(E), Env, R) :-
	evaluate(E, Env, R).

evaluate(unary(op(token(Op, _)), right(E)), Env, state(Env1, Res)) :-
	evaluate(E,
		Env,
		state(Env1, R)),
	(Op = bang,
		negate(R, Res);
Op = minus,
		Res is  - R;
format("Op is unknown: ~w~n", Op),
		!).

evaluate(expr_stmt(Expr), Env, R) :-
	evaluate(Expr, Env, R).

evaluate(variable(T), Env, state(Env, R)) :-
	get_var(VarName, Env, R),
	!. 

evaluate(var_decl(name(token(identifier(Name), _)), initializer(Stmt)), Env, state(Env2, none)) :-
	evaluate(Stmt,
		Env,
		state(Env1, V)),
	define_var(Name, V, Env1, Env2),
	% writeln(Env2),
	!.

evaluate(assignment(assign_name(variable(token(identifier(VarName), _))), value(ValueExpr)), Env, state(EnvAssigned, none)) :-
	evaluate(ValueExpr,
		Env,
		state(Env1, Value)),
	assign_var(VarName, Value, Env1, EnvAssigned),
	!.

evaluate(if(condition(Expr), then(Stmt), else(none)), Env, state(Env2, none)) :-
	evaluate(Expr,
		Env,
		state(Env1, true)),
	evaluate(Stmt,
		Env1,
		state(Env2, _)),
	!.

evaluate(if(condition(Expr), then(_), else(ElseStmt)), Env, state(Env2, none)) :-
	evaluate(Expr,
		Env,
		state(Env1, false)),
	evaluate(ElseStmt,
		Env1,
		state(Env2, _)),
	!.

evaluate(while(condition(Expr), body(Stmt)), Env, state(Env2, none)) :-
	while_loop(Expr,
		Stmt,
		Env,
		state(Env2, none)),
	!.

while_loop(Expr, Stmt, Env, state(Env3, none)) :-
	evaluate(Expr,
		Env,
		state(Env1, Res)),
	(Res = true,
		evaluate(Stmt,
			Env1,
			state(Env2, _)),
		while_loop(Expr,
			Stmt,
			Env2,
			state(Env3, _));
true).

evaluate(block([Stmt|Stmts]), Env, state(NewEnv, none)) :-
	!,
	create_new_env(Env, Enclosed),
	evaluate(Stmt,
		Enclosed,
		state(Env1, _)),
	evaluate_block_rest(Stmts,
		Env1,
		state(Env2, _)),
	 remove_inner_env(Env2, NewEnv).% going out of scope, hence we remove the inner scope.
		evaluate_block_rest([],
		R,
		state(R, _)).

evaluate_block_rest([Stmt|Stmts], Env, state(NewEnv, none)) :-
	evaluate(Stmt,
		Env,
		state(Env1, _)),
	evaluate_block_rest(Stmts,
		Env1,
		state(NewEnv, _)).

evaluate(function(T, parameters(Params), body(B)), Env, state(NewEnv, none)) :-
	define_var(T, lox_function(Params, B, closure(Env)), Env, NewEnv),
	!.

bind_args_to_params([], Env, Env, []).

bind_args_to_params([Arg|Args], Env, UpdatedEnv, [R|NewArgs]) :-
	evaluate(Arg, Env, state(NewEnv, R)),
	bind_args_to_params(Args, NewEnv, UpdatedEnv, NewArgs).

evaluate(call(callee(T), paren(_), arguments(Args)), Env, state(UpdatedEnv, none)) :-
	writeln("JUJU MANJI"),
	writeln(T),
	evaluate(T, Env, state(E2, R)),
	writeln("0--0------"),
	writeln(R),
	writeln("0--0------"),

	% get_var(T, Env, lox_function(_, _, closure(ClosureEnv))),
	bind_args_to_params(Args, ClosureEnv, UpdatedEnv, EvaluatedArgs),
	writeln("----------- [][][[]] -----------------"),
	writeln(EvaluatedArgs),
	writeln("----------------------------"),!.
	% create_new_env(ClosureEnv, NewEnv),
	% writeln("Func: "),
	% writeln(NewEnv),

% get_var(Key, env(Store, _), Value) :-
	% member(Key - Value, Store), !.

evaluate(Expr, E, _) :-
	writeln("-----------------------------------------------------------"),
	writeln("Unknown stmt:"),
	writeln(Expr),
	writeln("Environment:"),
	writeln(E),
	writeln("-----------------------------------------------------------"),
	halt.

extract_result(state(_, Res), Res).

extract_result(state(Env, _), Env).

extract_result(Res, Res).

negate(true, false).

negate(false, true).

is_truthy(true, true).

is_truthy(false, false).

is_truthy(nil, false).

is_truthy(_, true).

