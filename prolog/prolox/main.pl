:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).
:- use_module(library(dcg/basics)).
:- use_module(environment).



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

	for (var i = 0; i < 10; i = i + 1) {
  		print fib(i);
	}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

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

simple_nested :-
	Input = "
		var globalvar = 42;
		fun closureboo(lol) {
			globalvar = lol;
			print globalvar;
		} 
		closureboo(10000);
		print globalvar;
		",
	scan(Input, Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).


phrase_string_codes :-
	string_codes("   func    ", Input),
	phrase(tokenize(T),
		Input),
	writeln(T).

main :-
	% scan("    for(var i = 0; i <= 5; i = i + 1) { print i; }    ", Tokens),
% parse(Tokens, Stmts),
% interpret(Stmts),
% nested_functions.
% simple_nested.
	% Env1 = env([], env([i-400, p-100], none)),
	% Env2 = env([j-9], env([i-1, p-9], none)),


	% merge_envs(Env1, Env2, Env3), writeln(Env3).
	% FlattenEnv = [o-"Papa",j-9, j-98000, j-898989, i-1121, i-9000, j-"Lolex", i-"funcky"],
	% Env = env([j-111], env([i-23], env([o-1], none))),
	% assign_many_vars(FlattenEnv, Env, UpdatedEnv), writeln(UpdatedEnv).
	% Env = env([],env([n-0],env([],env([clock-lox_function([],builtin(clock_timer),closure(env([],none)))],none)))),
	% flatten_env(Env, F), writeln(F).



	fibonacci.


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
