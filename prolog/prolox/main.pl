:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).

main :-
	scan("for(var i = 0; i <= 5; i = i + 1) { print i;}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).
