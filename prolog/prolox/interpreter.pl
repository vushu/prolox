:- module(interpreter,[interpret/1, interpret/2, evaluate/3]).
:- use_module(environment).
:- use_module(builtin_functions).

:- discontiguous evaluate/3.

interpret(Stmts) :- interpret(Stmts, _).
interpret(Stmts, R) :-
	create_new_env(none, Env),
	define_builtins(Env, Env2),
	once(evaluate_rest(Stmts, state(Env2, none), state(Env3, R))),
	writeln("---------------------------------- DONE INTERPRETING --------------------------"),
	writeln("Env: "), writeln(Env3),
	writeln("-------------------------------------------------------------------------------").

evaluate_rest([], state(Env, R), state(Env, [R])).

evaluate_rest([Stmt|Stmts], state(Env, R), state(Env2, [R|R2])) :-
	evaluate(Stmt, Env, S),
	evaluate_rest(Stmts, S, state(Env2, R2)).


evaluate(print(Stmt), Env, state(Env1, none)) :-
	evaluate(Stmt, Env, state(Env1, R)),
	format("~w~n", R).

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
	evaluate(L, Env, state(Env1, L2)),
	evaluate(R, Env1, state(Env2, R2)),
	(Op = plus, Res is L2 + R2; Res is L2 - R2).

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

evaluate(variable(token(identifier(VarName), _)), Env, state(Env, R)) :-
	get_var(VarName, Env, R), 
	% format("~nGetting: ~w~n~w~n", [R, Env]),
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

evaluate(if(condition(Expr), then(Stmt), else(none)), Env, State) :-
	evaluate(Expr, Env, state(Env1, X)),
	(X = true -> evaluate(Stmt, Env1, State); State = state(Env1, none)).

evaluate(if(condition(Expr), then(_), else(ElseStmt)), Env, state(Env2, R)) :-
	evaluate(Expr, Env, state(Env1, false)),
	evaluate(ElseStmt,
		Env1,
		state(Env2, R)),
	!.

evaluate(while(condition(Expr), body(Stmt)), Env, state(Env2, R)) :-
	while_loop(Expr, Stmt, Env, state(Env2, R)),
	!.

% Needs to handle return
while_loop(Expr, Stmt, Env, state(Env3, none)) :-
	evaluate(Expr, Env, state(Env1, Res)), (Res = true ->
		evaluate(Stmt, Env1, state(Env2, _)),
		while_loop(Expr,
			Stmt,
			Env2,
			state(Env3, _)); true).


evaluate(block(Stmts), Env, state(Env1, R)) :-
	create_new_env(Env, Enclosed),
	execute_block(Stmts, Enclosed, Env, state(Env1, R)).

execute_block(Stmts, CurrentEnv, _, state(UpdatedEnv, R)) :-
	evaluate_block_rest(Stmts, state(CurrentEnv, none), state(Env1, R)),
	extract_enclosing(Env1, UpdatedEnv).


evaluate_block_rest([], L, L).

evaluate_block_rest([Stmt|Stmts], state(Env, _), State) :-
	evaluate(Stmt, Env, state(Env1, Res)),
	(Res = return_value(_) -> State = state(Env1, Res); evaluate_block_rest(Stmts, state(Env1, Res), State)).

evaluate(function(token(identifier(Name), _), parameters(Params), body(B)), Env, state(NewEnv, none)) :-
	define_var(Name, lox_function(Params, B, closure(Env)), Env, NewEnv), !.

eval_args([], Env, Env, []).

eval_args([Arg|Args], Env, UpdatedEnv, [R|NewArgs]) :-
	evaluate(Arg,
		Env,
		state(NewEnv, R)),
	eval_args(Args, NewEnv, UpdatedEnv, NewArgs).

define_params([], [], Env, Env).

define_params([identifier(Param)|Params], [Arg|Args], Env, UpdatedEnv) :-
	define_var(Param, Arg, Env, Env2),
	define_params(Params, Args, Env2, UpdatedEnv).

check_arity(Args, Params) :-
	(length(Params, L),
	length(Args, L) -> true;
length(Params, L1),
	length(Args, L2),
	format(atom(S),
		"Expected ~d argument(s) but got ~d.",
		[L1, L2]),
	writeln(S),
	halt).

evaluate_params(Args, Params, ClosureEnv, EvaluatedArgs, Env3) :-
	(length(Args, L), L > 255 -> writeln("Too many arguments"), halt;
	check_arity(Args, Params)),
	eval_args(Args, ClosureEnv, Env, EvaluatedArgs),
	define_params(Params, EvaluatedArgs, Env, Env3).

evaluate(call(callee(V), paren(_), arguments(Args)), BlockEnv, state(Env3, R)) :-
	evaluate(V, BlockEnv, state(BlockEnv, lox_function(Params, Body, closure(ClosureEnv)))),
	env_enclose_with(ClosureEnv, BlockEnv, Enclosed),
	% create_new_env(ClosureEnv, Enclosed),
	evaluate_params(Args, Params, Enclosed, EvaluatedArgs, Env2),
	call_function(Body, EvaluatedArgs, Env2, BlockEnv, state(Env3, Res)), 
	% Extracting result.
	(Res = return_value(M) -> R = M; R = Res).

call_function(block(Stmts), _, CurrentEnv, Env, state(Env1, R)) :-
	execute_block(Stmts, CurrentEnv, Env, state(Env1, R)).

call_function(builtin(Func), Args, _, Env, State) :-
	call(Func, Args, Env, State).

evaluate(return(keyword(_), value(E)), Env, state(Env2, return_value(R))) :-
	evaluate(E, Env, state(Env2, R)).

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