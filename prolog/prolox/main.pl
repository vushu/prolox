:- use_module(scanner).

main :-
	scan("2+2=", Tokens), 
	format("Tokens: ~w", Tokens).
