:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).

main :-
	scan("var foo = 0; foo = 42; print foo;", Tokens), % scan("print 1 + 1;", Tokens), 
	parse(Tokens, Stmts),
	interpret(Stmts).
