:- module(scanner,[scan/2]).
:- use_module(library(dcg/basics)).
tokens(Tokens) --> token_kind(Tokens, 1).
tokens(eof) --> [].

comment(Line, NextLine) --> ['/','/'], skip_until_newline(Line, NextLine).

skip_until_newline(Line, NextLine) --> {NextLine is Line + 1}, ['\n'].
skip_until_newline(Line, NextLine) --> [X],  {format("Skipping: ~w \n", X)}, skip_until_newline(Line, NextLine).
skip_until_newline(_, _) --> [], tokens([]), !.

% DCG rules to recognize specific tokens
token_kind(Z, Line)  --> ['('], token_kind(Zs, Line), {Z = [token(left_paren, Line) | Zs]}.
token_kind(Z, Line) --> [')'], token_kind(Zs, Line), {Z = [token(right_paren, Line) | Zs]}.
token_kind(Z, Line)  --> ['{'], token_kind(Zs, Line), {Z = [token(left_brace, Line) | Zs]}.
token_kind(Z, Line) --> ['}'], token_kind(Zs, Line), {Z = [token(right_brace, Line) | Zs]}.
token_kind(Z, Line) --> [','], token_kind(Zs, Line), {Z = [token(comma, Line) | Zs]}.
token_kind(Z, Line) --> ['.'], token_kind(Zs, Line), {Z = [token(dot, Line)| Zs]}.
token_kind(Z, Line) --> ['-'], token_kind(Zs, Line), {Z = [token(minus, Line) | Zs]}.
token_kind(Z, Line) --> ['+'], token_kind(Zs, Line), {Z = [token(plus, Line) | Zs]}.
token_kind(Z, Line) --> [';'], token_kind(Zs, Line), {Z = [token(semicolon, Line) | Zs]}.
token_kind(Z, Line) --> ['*'], token_kind(Zs, Line), {Z = [token(star, Line) | Zs]}.
token_kind(Z, Line) --> ['!','='], token_kind(Zs, Line), {Z = [token(bang_equal, Line) | Zs]}.
token_kind(Z, Line) --> ['!'], token_kind(Zs, Line), {Z = [token(bang, Line)| Zs]}.
token_kind(Z, Line) --> ['=','='], token_kind(Zs, Line), {Z = [token(equal_equal, Line)| Zs]}.
token_kind(Z, Line) --> ['='], token_kind(Zs, Line), {Z = [token(equal, Line) | Zs]}.
token_kind(Z, Line) --> ['<','='], token_kind(Zs, Line), {Z = [token(less_equal, Line)| Zs]}.
token_kind(Z, Line) --> ['<'], token_kind(Zs, Line), {Z = [token(less, Line) | Zs]}.
token_kind(Z, Line) --> ['>','='], token_kind(Zs, Line), {Z = [token(greater_equal, Line) | Zs ]}.
token_kind(Z, Line) --> ['>'], token_kind(Zs, Line), {Z = [token(greater, Line)| Zs]}.
token_kind(Zs, Line) --> ['/', '/'], { writeln("comment detected") }, skip_until_newline(Line, NextLine), token_kind(Zs, NextLine).
token_kind(Z, Line) --> ['/'], token_kind(Zs, Line), {Z = [token(slash, Line) | Zs]}.
token_kind(Zs, Line) --> ['\n'], { UpdateLine is Line + 1 }, token_kind(Zs, UpdateLine).
token_kind(Zs, Line) --> ['\t'], token_kind(Zs, Line).
token_kind(Zs, Line) --> white, token_kind(Zs, Line).

token_kind(Z, Line) --> initial_numbers(Chars), token_kind(Zs, Line), { number_chars(Value, Chars), Z = [token(number(Value), Line)| Zs] }. % Convert to number
token_kind(Z, Line) --> get_keyword_or_identifier(Result), token_kind(Zs, Line), {Z = [token(Result, Line) | Zs]}.
token_kind(Z, Line) --> get_string(Chars), token_kind(Zs, Line),  { string_chars(Value, Chars), Z = [token(string(Value), Line) | Zs]}.
token_kind([], _) --> [].
% token_kind(_) --> [Ch], {format("Unknown character '~w'~n", [Ch]), halt}.

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

newline(Line, NextLine) --> ['\n'], { NextLine is Line + 1  }, newline(NextLine, _).
newline(Line, Line) --> [].  % Base case

scan(String, Tokens) :-
    string_chars(String, CharList),  % Convert string to list of characters
    once(phrase(tokens(Tokens), CharList)).  % Apply the DCG to produce tokens
    
