:- module(parser,[parse/2]).

parse(Tokens, Exprs) :-
	once(phrase(parse_expr(Exprs),
			Tokens)).

parse_expr([Expr|Rest])-->
	declaration_stmt(Expr),
	parse_expr(Rest).

parse_expr([])-->
	[].
	% {throw("Failed to parse")}, [].

declaration_stmt(Stmt)-->
	fun_declaration_stmt(Stmt).

declaration_stmt(Stmt)-->
	var_declaration_stmt(Stmt).

convert_for_stmt_to_while_stmt(none, Cond, none, ForBody, while(condition(Cond), body(ForBody))).

convert_for_stmt_to_while_stmt(none, Cond, Inc, ForBody, while(condition(Cond), body(block([ForBody, Inc])))).

convert_for_stmt_to_while_stmt(Init, Cond, none, ForBody, block([Init, while(condition(Cond), body(ForBody))])).

convert_for_stmt_to_while_stmt(Init, Cond, Inc, ForBody, block([Init, while(condition(Cond), body(block([ForBody, Inc])))])).

expression_stmt(expr_stmt(Expr))-->
	expression(Expr),
	[token(semicolon, _)].

expression_stmt(_)-->
	expression(_),
	{
		throw("Expected ';' after expression.")
		},
	[].

print_stmt(print(Expr))-->
	[token(print, _)],
	expression(Expr),
	[token(semicolon, _)].

block_stmt(block(Stmts))-->
	[token(left_brace, _)],
	block_stmt_rest(Stmts),
	([token(right_brace, _)];
{
			throw("Expected '}' after block.")
			}).

% empty block

block_stmt(block([]))-->
	[token(left_brace, _)],
	([token(right_brace, _)];
{
			throw("Expected '}' after block.")
			}).

block_stmt_rest([Stmt|Stmts])-->
	declaration_stmt(Stmt),
	block_stmt_rest(Stmts).

block_stmt_rest([])-->
	[].

var_declaration_stmt(Stmt)-->
	statement(Stmt).

var_declaration_stmt(var_decl(name(T), initializer(Stmt)))-->
	[token(var, _)],
	[T],
	{
		T = token(identifier(_),
			_)
		},
	[token(equal, _)],
	expression(Stmt),
	([token(semicolon, _)]).

if_stmt(if(condition(Expr), then(Stmt), else(ElseBlock)))-->
	[token(if, _)],
	[token(left_paren, _)],
	expression(Expr),
	[token(right_paren, _)],
	block_stmt(Stmt),
	has_else_block(ElseBlock).

has_else_block(Res)-->
	[token(else, _)],
	block_stmt(Res);
{
		Res = none
		}.

while_stmt(while(condition(Cond), body(Stmt)))-->
	[token(while, _)],
	[token(left_paren, _)],
	expression(Cond),
	[token(right_paren, _)],
	block_stmt(Stmt).

return_stmt(return(keyword(T), value(V))) -->
	[T],
	{T = token(return, _)},
	expression_stmt(V).


for_cond(Cond)-->
	[token(semicolon, _)],
	{
		Cond = primary(true)
		};
expression_stmt(Cond);
{
		Cond = failed
		}.

for_inc(Inc)-->
	[token(right_paren, _)],
	{
		Inc = none
		};
expression(Inc);
{
		Inc = none
		}.

for_initializer(Init)-->
	[token(semicolon, _)],
	{
		Init = none
		};
var_declaration_stmt(Init);
{
		Init = none
		}.

for_body(Body)-->
	block_stmt(Body);
{
		Body = none
		}.

for_stmt(While)-->
	[token(for, _)],
	[token(left_paren, _)],
	for_initializer(Init),
	for_cond(Cond),
	for_inc(Inc),
	[token(right_paren, _)],
	for_body(ForBody),
	{
		convert_for_stmt_to_while_stmt(Init, Cond, Inc, ForBody, While)
		}.

get_expr_stmts([Stmt|Stmts])-->
	expression_stmt(Stmt),
	get_expr_stmts(Stmts).

get_expr_stmts([])-->
	[].

statement(Stmt)-->
	print_stmt(Stmt).

statement(Stmt)-->
	block_stmt(Stmt).

statement(Stmt)-->
	if_stmt(Stmt).

statement(Stmt)-->
	while_stmt(Stmt);
	for_stmt(Stmt).

statement(Stmt)-->
	return_stmt(Stmt).

statement(Stmt)-->
	expression_stmt(Stmt).

expression(Expr)-->
	assignment_expr(Expr).

%Unary expr

term_op(Op)-->
	[T],
	{
		T = token(Op, _),
		memberchk(Op, [plus, minus])
		}.

factor_op(Op)-->
	[T],
	{
		T = token(Op, _),
		memberchk(Op, [slash, star])
		}.

unary_op(T)-->
	[T],
	{
		T = token(Op, _),
		memberchk(Op, [bang, minus])
		}.

comparison_op(T)-->
	[T],
	{
		T = token(Op, _),
		memberchk(Op, [greater, greater_equal, less, less_equal])
		}.

equality_op(T)-->
	[T],
	{
		T = token(Op, _),
		memberchk(Op, [bang_equal, equal_equal])
		}.

assignment_expr(E)-->
	or_expr(E).

assignment_expr(assignment(assign_name(E), value(E2)))-->
	or_expr(E),
	[token(equal, _)],
	assignment_expr(E2).

or_expr(E)-->
	and_expr(E).

or_expr(or(left(E), right(E2), op(T)))-->
	and_expr(E),
	[T],
	{
		T = token(or, _)
		},
	and_expr(E2).

and_expr(E)-->
	equality_expr(E).

and_expr(and(left(E), right(E2), op(T)))-->
	equality_expr(E),
	[T],
	{
		T = token(and, _)
		},
	equality_expr(E2).

equality_expr(E)-->
	comparison_expr(E).

equality_expr(equality(left(E), right(E2), op(Op)))-->
	comparison_expr(E),
	equality_op(Op),
	comparison_expr(E2).

comparison_expr(E)-->
	term_expr(E).

comparison_expr(comparison(left(E), right(E2), op(Op)))-->
	term_expr(E),
	comparison_op(Op),
	term_expr(E2).

term_expr(E)-->
	factor_expr(E).

term_expr(term(left(E), right(E2), op(Op)))-->
	factor_expr(E),
	term_op(Op),
	factor_expr(E2).

factor_expr(E)-->
	unary_expr(E).

factor_expr(factor(left(E), right(E2), op(Op)))-->
	unary_expr(E),
	factor_op(Op),
	unary_expr(E2).

unary_expr(E)-->
	call_expr(E).

unary_expr(unary(op(Op), right(E)))-->
	unary_op(Op),
	unary_expr(E). 

call_expr(E)-->
	primary_expr(E).

call_expr(call(callee(Callee), paren(RParen), arguments(Args)))-->
	primary_expr(Callee),
	[token(left_paren, _)],
	argument_list(Args),
	[RParen],
	{
		RParen = token(right_paren, _)
		},
	!.

argument_list([Arg|Rest])-->
	expression(Arg),
	[token(comma, _)],
	argument_list(Rest).

argument_list([Arg])-->
	expression(Arg).

argument_list([])-->
	[].

parameter_list([identifier(Param)|Rest])-->
	[token(identifier(Param), _)],
	[token(comma, _)],
	parameter_list(Rest).

parameter_list([identifier(Param)])-->
	[token(identifier(Param), _)].

parameter_list([])-->
	[].

fun_declaration_stmt(function(token(identifier(Name), L), parameters(Params), body(B)))-->
	[token(fun, _)],
	[token(identifier(Name), L)],
	[token(left_paren, _)],
	parameter_list(Params),
	[token(right_paren, _)],
	block_stmt(B).

primary_expr(primary(number(Num)))-->
	[token(number(Num), _)].

primary_expr(primary(string(X)))-->
	[token(string(X), _)].

primary_expr(primary(true))-->
	[token(true, _)].

primary_expr(primary(false))-->
	[token(false, _)].

primary_expr(primary(nil))-->
	[token(nil, _)].

primary_expr(variable(T))-->
	[T],
	{
		T = token(identifier(_),
			_)
		}.

primary_expr(primary(group(E)))-->
	[token(left_paren, _)],
	expression(E),
	[token(right_paren, _)].


