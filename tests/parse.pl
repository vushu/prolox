:- begin_tests(parse_tokens).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).

test(parse) :-
	once(scan("2+2;", Tokens)), 
	once(parse(Tokens, 
		[expr_stmt(
			term(
				left(
					primary(
						number(2))), 
				right(
					primary(
						number(2))), 
				op(plus)))])).
:- end_tests(parse_tokens).



