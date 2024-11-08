:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).

main :-
	scan("for(var i = 0; i < 30;) {print \"HelloWorld!\";}", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).
