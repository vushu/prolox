:- begin_tests(loop).
:- use_module(prolog/prolox/parser).
:- use_module(prolog/prolox/scanner).
:- use_module(prolog/prolox/interpreter).
:- use_module(prolog/prolox/utils).

test(while_loop) :-
	scan("
	{
		var i = 0;
		while (i < 5) 
		{
			{
		  		print i;
			}
			i = i + 1;
		}
	}", Tokens),
	parse(Tokens, Stmts),
	interpret(Stmts).

:- end_tests(loop).

% Print: 0
% Print: 1
% Print: 1
% Print: 2
% Print: 3
% Print: 5
% Print: 8
% Print: 13
% Print: 21
% Print: 34
% Print: 55
% Print: 89
% Print: 144
% Print: 233
% Print: 377
% Print: 610
% Print: 987
% Print: 1597
% Print: 2584
% Print: 4181

% env([],env([],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)))
% env([],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))
% env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)


% env([],env([],env([i-1],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))))
% env([],env([i-1],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)))
% env([i-2],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))
% env([],none)


% Custom while loop

% env([],env([],env([i-1],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))))
% env([],env([i-1],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none)))
% env([i-2],env([clock-lox_function([identifier(arg)],builtin(clock_timer),closure(env([],none)))],none))
% env([],none)