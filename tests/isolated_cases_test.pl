:- begin_tests(isolated_cases).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(parse_function) :-
	scan("fun hej(mama, papa) {}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).


:- end_tests(isolated_cases).
