% main.pl

% Load the scanner.pl file
use_module(scanner).
% Define the main predicate
main :-
    % Test 1
    scan("2", Tokens1),
    format('Tokens:": ~w~n', [Tokens1]).
