:- begin_tests(scan_tokens).
:- use_module(prolog/prolox/scanner).

print_list([]).

print_list([H|T]) :-
	format("Tokens ~w~n", H), 
	print_list(T).

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

test(scan_string2) :-
	scan("\"foo\" \"bar\"", Tokens), assertion(Tokens = [token(string("foo"),1),token(string("bar"),1)]).


test(scan_string) :-
	scan("2> 2+ 2", Tokens), 
	writeln("----------------"), 
	print_list(Tokens), 
	writeln("----------------").

test(scan_block) :-
	scan("{2 > 2 + 2//Hello!//howareyoudoing!.
		 }", Tokens), 
	writeln("----------------"),
	print_list(Tokens),
	writeln("----------------").

test(scan_while) :-
	scan("while(true) { print 42;}", Tokens), 
	writeln("----------------"), 
	print_list(Tokens), 
	writeln("----------------").

test(scan_var) :-
	scan("print a * b;}", Tokens), 
	writeln("----------------"), 
	print_list(Tokens), 
	writeln("----------------").


:- end_tests(scan_tokens).



