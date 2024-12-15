:- begin_tests(fibonacci).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).
:- use_module(prolog/prolox/utils).

test(fibonacci) :-
	scan("
        fun fib(n) {

            if (n <= 1) {
                return n;
            }

            return n;
        }
        for (var i = 0; i < 2; i = i + 1) 
        { 
            print fib(i);
        }", Tokens),
	parse(Tokens, Stmts),
    % print_stmts(Stmts).
	interpret(Stmts).

:- end_tests(fibonacci).
