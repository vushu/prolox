:- begin_tests(builtin_functions).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(parse_function) :-
	scan("var now = clock(); print now;", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

test(local_function) :-
	scan("fun makeCounter() {
			var i = 0;
			fun count() {
				i = i + 1;
				print i;
			}
			return count;
		}
		var counter = makeCounter();
		counter();
		counter();
		", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

:- end_tests(builtin_functions).
