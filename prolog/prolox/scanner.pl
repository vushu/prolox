% scanner.pl

:- module(scanner,[scan/2]).

% Define the main predicate for scanning input with line numbers.
scan(Input, Tokens) :-
    atom_chars(Input, Chars),
    scan_tokens(Chars, 1, Tokens, []).

% Predicate to handle the list of characters and produce tokens with line numbers.
scan_tokens([], _, [], []).
scan_tokens([H|T], Line, [Token|Tokens], Rest) :-
    (   is_space(H)
    ->  (H = '\n' -> NewLine is Line + 1, scan_tokens(T, NewLine, Tokens, Rest)
       ;  scan_tokens(T, Line, Tokens, Rest))
    ;   is_alpha(H)
    ->  scan_identifier([H|T], Line, Token, Rest1),
        scan_tokens(Rest1, Line, Tokens, Rest)
    ;   is_digit(H)
    ->  scan_number([H|T], Line, Token, Rest1),
        scan_tokens(Rest1, Line, Tokens, Rest)
    ;   member(H, "+-*/=;()")
    ->  Token = token(H, operator, Line),
        scan_tokens(T, Line, Tokens, Rest)
    ;   throw(error(unrecognized_character(H), _))
    ).

% Predicate to recognize and extract identifiers (alphanumeric sequences).
scan_identifier(Chars, Line, Token, Rest) :-
    span(is_alpha_num, Chars, TokenChars, Rest),
    atom_chars(Atom, TokenChars),
    ( member(Atom, [var1, var2, var3]) % Example reserved keywords or identifiers
    ->  Token = token(Atom, identifier, Line)
    ;   Token = token(Atom, identifier, Line)
    ).

% Predicate to recognize and extract numbers (digit sequences).
scan_number(Chars, Line, Token, Rest) :-
    span(is_digit, Chars, NumberChars, Rest),
    atom_chars(Atom, NumberChars),
    number_atom(Number, Atom),
    Token = token(Number, number, Line).

% Helper predicate to check if a character is a space.
is_space(Char) :-
    member(Char, [' ', '\t', '\n', '\r']).

% Helper predicate to check if a character is an alphabetic character.
is_alpha(Char) :-
    char_type(Char, alpha).

% Helper predicate to check if a character is a digit.
is_digit(Char) :-
    char_type(Char, digit).

% Helper predicate to check if a character is alphanumeric.
is_alpha_num(Char) :-
    is_alpha(Char);
    is_digit(Char).

% Predicate to split a list of characters based on a condition.
span(_, [], [], []).
span(Pred, [H|T], [H|Chars], Rest) :-
    call(Pred, H),
    span(Pred, T, Chars, Rest).
span(_, L, [], L).

% Helper predicate to convert an atom to a number.
number_atom(Number, Atom) :-
    atom_number(Atom, Number).
