:- use_module(scanner).
:- use_module(parser).

main :-
	% scan("{2*4>=2 != 4 or 1 and (2+2); a = 2; mama + papa;}", Tokens), 

	scan("{2+2;2+3;print mama;{2+2;}}", Tokens), % writeln(Tokens),
	
	parse(Tokens, Exprs), 
	writeln(Exprs).
