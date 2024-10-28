:- begin_tests(interpret_stmts).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).

test(interpret_term) :-
	scan("print 2 + 2;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

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
	interpret(Stmts).

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

test(interpret_assignment) :-
	scan("var foo = 32; var bar = 10; foo = 42;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_assignment_simple) :-
	scan("var foo = 0; foo = 42;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_if) :-
	scan("if (3 > 1) { print 42; }", Tokens), 
	parse(Tokens, Stmts), 
	writeln(Stmts), 
	interpret(Stmts).

test(interpret_if_with_else) :-
	scan("if (3 < 1) { print 42; } else { print \"Damn\"; }", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_while) :-
	scan("while (false) { print 42; }", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

test(interpret_variable) :-
	scan("var a = 21; print a * 2;", Tokens), 
	parse(Tokens, Stmts), 
	interpret(Stmts).

:- end_tests(interpret_stmts).