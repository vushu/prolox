:- use_module(scanner).
:- use_module(parser).
:- use_module(interpreter).

main :-
	% scan("for(var i = 0; i < 30;) {print \"HelloWorld!\";}", Tokens), 

	scan("
	{ 
		var c = 300;
		var a = 1;
		{
			var b = 2;
			a = 2;
			{
				a = 3;
				c = a * c;
			} 
		}
		print a;
		print c;
	}", Tokens), 
	parse(Tokens, Stmts), 
	writeln(Stmts), 
	interpret(Stmts).
