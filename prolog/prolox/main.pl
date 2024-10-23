:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).

main :-
	scan("print 1 + 1;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).
