:- begin_tests(scoping_test).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(parse_function) :-
	scan("fun foo(){var i = 42; print i;} foo(); foo();", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).



:- end_tests(scoping_test).
