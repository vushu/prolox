:- begin_tests(interpret_stmts).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(interpret_term) :-
	scan("print 2 + 2;", Tokens), 
	parse(Tokens, Stmts), 
	once(
		interpret(Stmts)).

test(interpret_or) :-
	scan("print false or 42;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_and) :-
	scan("print true and 1;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_less_equal) :-
	scan("print 4 >= 1; print 4 > 4;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_greater_equal) :-
	scan("print 1 >= 1; print 2 > 5;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_equality) :-
	scan("print 1 == 1; print 1 != 42;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_group) :-
	scan("print (1 != 1);", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_unary_bang) :-
	scan("print !!!true;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_unary_minus) :-
	scan("print ---100;", Tokens), 
	parse(Tokens, Stmts), 
	once(
		interpret(Stmts)).

test(interpret_binary_minus) :-
	scan("print 100-1;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_block_stmt) :-
	scan("{ 
		print 1;
		print 2;
		print 3;
		print 4;
		print 5;
		 }", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_var_decl) :-
	scan("var hej = 1;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).


:- end_tests(interpret_stmts).