:- begin_tests(scoping_test).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).
do_stuff([N|Ns], R):-
    (N = 5 -> R = 5, format("Done ~w~n", R), writeln("Asdfasdfsa");
    writeln("Continue"),
    do_stuff(Ns, R)).
    

test(parse_function) :-
	scan("fun foo(){var i = 42; print i;} foo(); foo();", Tokens),
	parse(Tokens, Stmts),
    writeln(Stmts),
	interpret(Stmts).

test(do_stuff) :-
    do_stuff([1,2,3,4,5,6,7,8], R), writeln(R).




:- end_tests(scoping_test).
