:- begin_tests(isolated_cases).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(interpret_for) :-
	scan("for(var i = 0; i < 0; i = i + 1) { 2 + 2;}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).


:- end_tests(isolated_cases).
