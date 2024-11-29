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

test(parse_var_decl_inside_block) :-
	scan("{var a = b;}", Tokens), 
	parse(Tokens, Stmts), 
	Stmts = [block(
		[var_decl(
			name(
				token(
					identifier("a"), 1)), 
			initializer(
				variable(
					token(
						identifier("b"), 1))))])].

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
	parse(Tokens, 
		[block(
			[print(
				primary(
					group(
						term(
							left(
								primary(
									number(42))), 
							right(
								primary(
									number(40))), 
							op(minus))))), 
			print(
				primary(
					number(1))), 
			print(
				primary(
					number(2))), 
			print(
				primary(
					number(3)))])]).
	% print_list(Stmts).

test(parse_var_decl) :-
	scan("var a = b;", Tokens), 
	parse(Tokens, 
		[var_decl(
			name(
				token(
					identifier("a"), 1)), 
			initializer(
				variable(
					token(
						identifier("b"), 1))))]).

test(parse_var_assignment) :-
	scan("a = 12;", Tokens), 
	parse(Tokens, 
		[expr_stmt(
			assignment(
				assign_name(
					variable(
						token(
							identifier("a"), 1))), 
				value(
					primary(
						number(12)))))]).

test(parse_while) :-
	scan("while(true) {print 23;}", Tokens), 
	parse(Tokens, 
		[while(
			condition(
				primary(true)), 
			body(
				block(
					[print(
						primary(
							number(23)))])))]).

test(parse_for) :-
	scan("for(var i = 0; i < 30; i = i + 1) {print 23;}", Tokens), 
	parse(Tokens, 
		[block(
			[var_decl(
				name(
					token(
						identifier("i"), 1)), 
				initializer(
					primary(
						number(0)))), 
			while(
				condition(
					expr_stmt(
						comparison(
							left(
								variable(
									token(
										identifier("i"), 1))), 
							right(
								primary(
									number(30))), 
							op(
								token(less, 1))))), 
				body(
					block(
						[block(
							[print(
								primary(
									number(23)))]), 
						assignment(
							assign_name(
								variable(
									token(
										identifier("i"), 1))), 
							value(
								term(
									left(
										variable(
											token(
												identifier("i"), 1))), 
									right(
										primary(
											number(1))), 
									op(plus))))])))])]).

test(parse_for_no_initializer) :-
	scan("for(; i < 30; i = i + 1) { print 23; }", Tokens), 
	parse(Tokens, 
		[while(
			condition(
				expr_stmt(
					comparison(
						left(
							variable(
								token(
									identifier("i"), 1))), 
						right(
							primary(
								number(30))), 
						op(
							token(less, 1))))), 
			body(
				block(
					[block(
						[print(
							primary(
								number(23)))]), 
					assignment(
						assign_name(
							variable(
								token(
									identifier("i"), 1))), 
						value(
							term(
								left(
									variable(
										token(
											identifier("i"), 1))), 
								right(
									primary(
										number(1))), 
								op(plus))))])))]).

test(parse_for_no_initializer_and_cond) :-
	scan("for(;; i = i + 1) {print 23;}", Tokens), 
	parse(Tokens, Stmts), 
	Stmts = [while(
		condition(
			primary(true)), 
		body(
			block(
				[block(
					[print(
						primary(
							number(23)))]), 
				assignment(
					assign_name(
						variable(
							token(
								identifier("i"), 1))), 
					value(
						term(
							left(
								variable(
									token(
										identifier("i"), 1))), 
							right(
								primary(
									number(1))), 
							op(plus))))])))].

% test(parse_for_only_semicolons) :-
% 	scan("for(;;) {}", Tokens), 
% 	parse(Tokens, 
% 		[for(
% 			initializer(none), 
% 			condition(none), 
% 			incrementer(none), 
% 			body(
% 				block([])))]).





:- end_tests(parse_tokens).




