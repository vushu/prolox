:- begin_tests(fibonacci).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/interpreter).
:- use_module(prolog/prolox/utils).

test(fibonacci) :-
	writeln("----------------------------------------------"),
	scan("
	fun fib(n) {
  		if (n <= 1) { return n; }
  		return fib(n - 2) + fib(n - 1);
	}

	for (var i = 0; i < 3; i = i + 1) {
  		print fib(i);
	}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

:- end_tests(fibonacci).

