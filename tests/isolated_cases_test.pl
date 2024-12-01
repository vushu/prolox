:- begin_tests(isolated_cases).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(parse_function) :-
	scan("fun hej(name, age){ print 1;}", Tokens),
	writeln(Tokens),
	parse(Tokens, Stmts),
	writeln(Stmts).


:- end_tests(isolated_cases).
