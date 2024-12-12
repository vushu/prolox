:- module(builtin_functions,[define_builtins/2, clock_timer/2]).
:- use_module(environment).

clock_timer(Env, state(Env, "Hej")) :-
    writeln(Env),
    writeln("clock timer calls Jujuju man").

define_builtins(Env, Env2) :-
    define_var("clock", lox_function([], builtin_func(clock_timer), closure(Env)), Env , Env2).