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

test(scan_string) :-
	scan("2> 2+ 2", Tokens), 
	writeln("----------------"),
	print_list(Tokens),
	writeln("----------------").

test(scan_block) :-
	scan("{ 	2 >   2 + 2
		 // Hello!
		 // how are you doing!.
		 }", Tokens), 
	writeln("----------------"),
	print_list(Tokens),
	writeln("----------------").




:- end_tests(scan_tokens).



