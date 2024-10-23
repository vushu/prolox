:- module(scanner,[scan/2]).

tokens([Token | RestTokens]) -->
    token_kind(Token, 1),
    tokens(RestTokens). 

tokens([]) --> [].

comment(Line, NextLine) --> ['/','/'], skip_until_newline(Line, NextLine).

skip_until_newline(Line, NextLine) --> {NextLine is Line + 1}, ['\n'].
skip_until_newline(Line, NextLine) --> [X],  {format("Skipping: ~w \n", X)}, skip_until_newline(Line, NextLine).
skip_until_newline(_, _) --> [],tokens([]), !.

% DCG rules to recognize specific tokens
token_kind(token(left_paren, Line), Line)  --> ['('].
token_kind(token(right_paren, Line), Line) --> [')'].
token_kind(token(left_brace, Line), Line)  --> ['{'].
token_kind(token(right_brace, Line), Line) --> ['}'].
token_kind(token(comma, Line), Line) --> [','].
token_kind(token(dot, Line), Line) --> ['.'].
token_kind(token(minus, Line), Line) --> ['-'].
token_kind(token(plus, Line ), Line) --> ['+'].
token_kind(token(semicolon, Line), Line) --> [';'].
token_kind(token(star, Line), Line) --> ['*'].
token_kind(token(bang_equal, Line), Line) --> ['!','='].
token_kind(token(bang, Line), Line) --> ['!'].
token_kind(token(equal_equal, Line), Line) --> ['=','='] .
token_kind(token(equal, Line), Line) --> ['='].
token_kind(token(less_equal, Line), Line) --> ['<','='].
token_kind(token(less, Line), Line) --> ['<'].
token_kind(token(greater_equal, Line), Line) --> ['>','='].
token_kind(token(greater, Line), Line) --> ['>'].
token_kind(X, Line) --> ['/', '/'],{ writeln("comment detected") }, skip_until_newline(Line, NextLine), token_kind(X, NextLine).
token_kind(token(slash, Line), Line) --> ['/'].
token_kind(X, Line) --> ['\n'], { writeln("newline detected"), UpdateLine is Line + 1 }, token_kind(X, UpdateLine).
token_kind(X, Line) --> ['\t'], token_kind(X, Line).
token_kind(X, Line) --> [' '], token_kind(X, Line).
token_kind(token(number(Value), Line), Line) --> initial_numbers(Chars), { number_chars(Value, Chars) }. % Convert to number
token_kind(token(Result, Line), Line) --> get_keyword_or_identifier(Result).
token_kind(token(string(Value), Line), Line) --> get_string(Chars), { string_chars(Value, Chars)}.
token_kind(token([], Line), Line) --> [Ch], {throw(format("Unknown character '~w'", Ch))}.

get_keyword_or_identifier(Result) --> get_alpha(Chars), {string_chars(Value, Chars), keyword(Value, Result)}.

keyword("and", and).
keyword("class", class).
keyword("else", else).
keyword("false", false).
keyword("for", for).
keyword("fun", fun).
keyword("if", if).
keyword("nil", nil).
keyword("or", or).
keyword("print", print).
keyword("return", return).
keyword("super", super).
keyword("this", this).
keyword("true", true).
keyword("var", var).
keyword("while", while).
keyword(X, identifier(X)).  % is then identifier

get_string(String) --> ['"'], extract_string(String).
extract_string([Ch| Rest]) --> [Ch], extract_string(Rest).
extract_string([]) --> ['"'].

get_alpha([Ch| Rest]) --> is_alphabet(Ch), extract_alpha(Rest).
extract_alpha([Ch | Rest]) --> is_alpha_numberic(Ch), extract_alpha(Rest).
extract_alpha([]) --> [].

initial_numbers([Digit | Rest]) --> is_digit(Digit), extract_numbers(Rest).
extract_numbers([Digit | Rest]) --> is_digit(Digit), extract_numbers(Rest).
extract_numbers(['.', Digit | Rest]) --> ['.'], is_digit(Digit), extract_decimals(Rest).
extract_numbers([]) --> [].

extract_decimals([Digit | Rest]) --> is_digit(Digit), extract_decimals(Rest).
extract_decimals([]) --> [].

% % DCG rule to match a single digit
is_alpha_numberic(A)  --> is_alphabet(A); is_digit(A).
is_digit(D) --> [D], { char_type(D, digit) }.
is_alphabet(Ch) --> [Ch], { is_alpha(Ch) }.


% DCG rule to match and skip whitespace characters
whitespace --> ['\t'], whitespace.
whitespace --> ['\r'], whitespace.
whitespace --> [' '], whitespace.
whitespace --> [].  % Base case: no more whitespace

newline(Line, NextLine) --> ['\n'], { NextLine is Line + 1  }, newline(NextLine, _).
newline(Line, Line) --> [].  % Base case

scan(String, Tokens) :-
    string_chars(String, CharList),  % Convert string to list of characters
    once(phrase(tokens(Tokens), CharList)).  % Apply the DCG to produce tokens
    
