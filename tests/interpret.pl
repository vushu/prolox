:- begin_tests(interpret_stmts).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(interpret_term) :-
	scan("print 2 + 2;", Tokens), 
	parse(Tokens, Stmts), 
	writeln(Stmts), 
	interpret(Stmts).

test(interpret_or) :-
	scan("print true or false;", Tokens), 
	parse(Tokens, Stmts), 
	writeln(Stmts), 
	interpret(Stmts).

:- end_tests(interpret_stmts).