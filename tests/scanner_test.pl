:- begin_tests(scan_tokens).
:- use_module(prolog/prolox/scanner).

test(scanning_tokens) :-
	scan("+-123", 
		[token(plus, 1), 
		token(minus, 1), 
		token(
			number(123), 1)]).

test(scan_decimals) :-
	scan("123.123", 
		[token(
			number(123.123), 1)]).

test(scan_string) :-
	scan("\"mystring\"", 
		[token(
			string("mystring"), 1)]).


:- end_tests(scan_tokens).



