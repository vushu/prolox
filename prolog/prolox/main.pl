:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).

main :-
	% scan("{2*4>=2 != 4 or 1 and (2+2); a = 2; mama + papa;}", Tokens), 
% scan("{2+2;2+3;print mama;{2+2;}}", Tokens), % writeln(Tokens),
% scan("var a = 2;", Tokens), % writeln(Tokens),
% scan("if (2 > 1) { print \"2 is greater!\" ;}", Tokens), % writeln(Tokens),
% scan("if (true) { print 223;}", Tokens),
% scan("if (2 > 1) { print (3 + 2); } else { 2+2;}", Tokens), % writeln(Tokens),

	scan("print 42;", Tokens), % writeln(Tokens),
	
	parse(Tokens, Stmts), 
	writeln(Stmts), 
	interpret(Stmts).
