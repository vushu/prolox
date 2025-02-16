:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).


nested_fibonacci :-
	scan("
	fun run() {  
		fun mama() {
			print \"MAMA!\";
		}
		var foo = 42;
		fun fib(n) {
			foo = foo + 1;
			if (n <= 1) { return n; }
			return fib(n - 2) + fib(n - 1);
		}

		// for (var i = 0; i < 20; i = i + 1) {
			// print fib(i);
		// }
		return fib;
	}
	var fib = run();
	fib(20);
	", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts, _).

fibonacci :-
	scan("
	fun mama() {
		print \"MAMA!\";
	}
	var foo = 42;
	fun fib(n) {
		foo = foo + 1;
		if (n <= 1) { return n; }
		return fib(n - 2) + fib(n - 1);
	}

	for (var i = 0; i < 20; i = i + 1) {
		print fib(i);
	}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts, _).

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

scoping_test :- 
	Input = "
	var a = \"Global\";
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
	% run.
	% scoping_test.
	% nested_fibonacci.
	fibonacci.