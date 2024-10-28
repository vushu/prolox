:- module(parser,[parse/2]).

parse(Tokens, Exprs) :-
	once(
		phrase(
			parse_expr(Exprs), Tokens)).

parse_expr([Expr|Rest])-->
	var_declaration_stmt(Expr), 
	parse_expr(Rest).

parse_expr([])-->
	[].
	% {throw("Failed to parse")}, [].

expression_stmt(expr_stmt(Expr))-->
	expression(Expr), 
	[token(semicolon, _)].

expression_stmt(_)-->
	expression(_), 
	
	{throw("Expected ';' after expression.")}, [].

print_stmt(print(Expr))-->
	[token(print, _)], 
	expression(Expr), 
	[token(semicolon, _)].

block_stmt(block(Stmts))-->
	[token(left_brace, _)], 
	block_stmt_rest(Stmts), 
	(
		[token(right_brace, _)];
	
		{throw("Expected '}' after block.")}).

% empty block

block_stmt(block([]))-->
	[token(left_brace, _)], 
	(
		[token(right_brace, _)];
	
		{throw("Expected '}' after block.")}).

block_stmt_rest([Stmt|Stmts])-->
	var_declaration_stmt(Stmt), 
	block_stmt_rest(Stmts).

block_stmt_rest([])-->
	[].

%var_declaration

var_declaration_stmt(Stmt)-->
	statement(Stmt).

var_declaration_stmt(var_decl(name(T), initializer(Stmt)))-->
	[token(var, _)], [T], 
	
	{T = token(
		identifier(_), _)}, 
	[token(equal, _)], 
	expression(Stmt), 
	(
		[token(semicolon, _)];
	
		{throw("Expected ';' after var declaration.")}).

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
	{Res = none}.

while_stmt(while(condition(Cond), body(Stmt)))-->
	[token(while, _)], 
	[token(left_paren, _)], 
	expression(Cond), 
	[token(right_paren, _)], 
	block_stmt(Stmt).

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
	while_stmt(Stmt).

statement(Stmt)-->
	expression_stmt(Stmt).

expression(Expr)-->
	assignment_expr(Expr).

%Unary expr

term_op(Op)-->
	[T], 
	
	{T = token(Op, _), 
	memberchk(Op, [plus, minus])}.

factor_op(Op)-->
	[T], 
	
	{T = token(Op, _), 
	memberchk(Op, [slash, star])}.

unary_op(T)-->
	[T], 
	
	{T = token(Op, _), 
	memberchk(Op, [bang, minus])}.

comparison_op(T)-->
	[T], 
	
	{T = token(Op, _), 
	memberchk(Op, [greater, greater_equal, less, less_equal])}.

equality_op(T)-->
	[T], 
	
	{T = token(Op, _), 
	memberchk(Op, [bang_equal, equal_equal])}.

assignment_expr(E)-->
	or_expr(E).

assignment_expr(assigment(assign_name(E), value(E2)))-->
	or_expr(E), 
	[token(equal, _)], 
	assignment_expr(E2).

or_expr(E)-->
	and_expr(E).

or_expr(or(left(E), right(E2), op(T)))-->
	and_expr(E), [T], 
	
	{T = token(or, _)}, 
	and_expr(E2).

and_expr(E)-->
	equality_expr(E).

and_expr(and(left(E), right(E2), op(T)))-->
	equality_expr(E), [T], 
	
	{T = token(and, _)}, 
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
	primary_expr(E).

unary_expr(unary(op(Op), right(E)))-->
	unary_op(Op), 
	unary_expr(E). 

primary_expr(primary(number(Num)))-->
	[token(
		number(Num), _)].

primary_expr(primary(string(X)))-->
	[token(
		string(X), _)].

primary_expr(primary(true))-->
	[token(true, _)].

primary_expr(primary(false))-->
	[token(false, _)].

primary_expr(primary(nil))-->
	[token(nil, _)].

primary_expr(variable(T))-->
	[T], 
	
	{T = token(
		identifier(_), _)}.

primary_expr(primary(group(E)))-->
	[token(left_paren, _)], 
	expression(E), 
	[token(right_paren, _)].


