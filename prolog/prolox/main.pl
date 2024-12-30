:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).
:- use_module(library(dcg/basics)).


tokenize(Z) --> tokens(Z).
tokenize(eof) --> [].
tokens(Z) --> "func", tokens(Ts) ,{Z = [func | Ts]}.
tokens(Z) --> white, tokens(Z).
tokens(Z) --> [_], {Z = []}.


phrase_string_codes :-
	string_codes("   func    ", Input),
	phrase(tokenize(T), Input), writeln(T).


main :-
	scan("for(var i = 0; i <= 5; i = i + 1) { print i; }", Tokens),
	writeln(Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts), phrase_string_codes.
