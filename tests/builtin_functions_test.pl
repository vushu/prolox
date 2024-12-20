:- begin_tests(builtin_functions).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(parse_function) :-
	scan("var now = clock(); print now;", Tokens), 
	parse(Tokens, Stmts),
	interpret(Stmts).

:- end_tests(builtin_functions).
