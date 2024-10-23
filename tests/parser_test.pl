:- begin_tests(parse_tokens).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).

print_list([]).

print_list([H|T]) :-
	format("AST -------------~n ~w~n", H), 
	print_list(T).

test(parse_term) :-
	scan("2+2;", Tokens), 
	parse(Tokens, 
		[expr_stmt(
			term(
				left(
					primary(
						number(2))), 
				right(
					primary(
						number(2))), 
				op(plus)))]).

test(parse_factor) :-
	scan("2*2;", Tokens), 
	parse(Tokens, 
		[expr_stmt(
			factor(
				left(
					primary(
						number(2))), 
				right(
					primary(
						number(2))), 
				op(star)))]).

test(parse_block) :-
	scan("{2*2;}", Tokens), 
	parse(Tokens, 
		[block(
			[expr_stmt(
				factor(
					left(
						primary(
							number(2))), 
					right(
						primary(
							number(2))), 
					op(star)))])]).

test(parse_var_decl) :-
	scan("{var a = b;}", Tokens), 
	parse(Tokens, 
		[block(
			[var_decl(
				name(
					token(
						identifier("a"), 1)), 
				intializer(
					primary(
						token(
							identifier("b"), 1))))])]).

test(parse_comparison) :-
	scan("42 >= 10;", Tokens), 
	parse(Tokens, 
		[expr_stmt(
			comparison(
				left(
					primary(
						number(42))), 
				right(
					primary(
						number(10))), 
				op(
					token(greater_equal, 1))))]).

test(parse_if_stmt) :-
	scan("if (true) { print 223;}", Tokens), 
	parse(Tokens, 
		[if(
			condition(
				primary(true)), 
			then(
				block(
					[print(
						primary(
							number(223)))])), 
			else(none))]).

test(parse_block_stmt) :-
	scan("{ 
		print (42 - 40); 
		print 1; 
		print 2; 
		print 3; 
		}", Tokens), 
	parse(Tokens, Stmts), 
	print_list(Stmts).

:- end_tests(parse_tokens).



