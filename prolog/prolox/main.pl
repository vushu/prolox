:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).
:- use_module(library(dcg/basics)).


tokenize(Z)-->
	tokens(Z).
tokens(Z)-->
	"func",
	tokens(Ts),
	{
		Z = [func|Ts]
		}.
tokens(Z)-->
	white,
	tokens(Z).
tokens(Z)-->
	[_],
	{
		Z = []
		}.

fibonacci :-
	scan("
	fun fib(n) {
  		if (n <= 1) { return n; }
  		return fib(n - 2) + fib(n - 1);
	}

	for (var i = 0; i < 20; i = i + 1) {
  		print fib(i);
	}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts, R), writeln(R).

nested_functions :-
	Input = "fun makeCounter() {
			var fortytwo = 42;
			fun count() {
				return fortytwo + 45;
			}
			print 31;
			return count;
		}
		var counter = makeCounter();
		print counter();
		print 232;",
	scan(Input, Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

phrase_string_codes :-
	string_codes("   func    ", Input),
	phrase(tokenize(T),
		Input),
	writeln(T).

scoping_test :- 
	Input = "var a = \"Global\";
	{
		fun mama() {
			print a;
		}

		mama();
		var a = \"Block\";
		mama();
	}", scan(Input, Tokens), 
writeln(Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

main :-
	% scan("    for(var i = 0; i <= 5; i = i + 1) { print i; }    ", Tokens),
% parse(Tokens, Stmts),
% interpret(Stmts),
% nested_functions.
	scoping_test.
	% fibonacci.


msms : writeln("").
	% env([
	% 		counter - lox_function([],
	% 			block([print(primary(string(HEJ)))]),
	% 			closure([])),
	% 		makeCounter - lox_function([],
	% 			block([
	% 					function(token(identifier(count),
	% 							2),
	% 						parameters([]),
	% 						body(block([print(primary(string(HEJ)))]))),
	% 					return(keyword(token(return, 5)), value(expr_stmt(variable(token(identifier(count),
	% 										5)))))]),
	% 			closure([])),
	% 		clock - lox_function([],
	% 			builtin(clock_timer),
	% 			closure(env([], none)))],
	% 	none).
