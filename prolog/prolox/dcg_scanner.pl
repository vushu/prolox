tokens(Tokens, Line) -->
    whitespace,
    comment(Line, NextLine), 
    whitespace,
    tokens(Tokens, NextLine).

tokens([Token | RestTokens], Line) -->
    whitespace,
    newline(Line, NextLine),
    token_kind(T),
    {Token  = token(T, NextLine)}, 
    tokens(RestTokens, NextLine). 

tokens([], _) --> whitespace, [].

comment(Line, NextLine) --> ['/','/'], skip_until_newline(Line, NextLine).

skip_until_newline(Line, NextLine) --> {NextLine is Line + 1}, ['\n'].
skip_until_newline(Line, NextLine) --> [X],  {format("Skipping: ~w \n", X)}, skip_until_newline(Line, NextLine).
skip_until_newline(_, _) --> [],tokens([], _), !.

% DCG rules to recognize specific tokens
token_kind(left_paren)  --> ['('].
token_kind(right_paren) --> [')'].
token_kind(left_brace)  --> ['{'].
token_kind(right_brace) --> ['}'].
token_kind(comma) --> [','].
token_kind(dot) --> ['.'].
token_kind(minus) --> ['-'].
token_kind(plus) --> ['+'].
token_kind(semicolon) --> [';'].
token_kind(star) --> ['*'].
token_kind(bang_equal) --> ['!','='].
token_kind(bang) --> ['!'].
token_kind(equal_equal) --> ['=','='] .
token_kind(equal) --> ['='].
token_kind(less_equal) --> ['<','='].
token_kind(less) --> ['<'].
token_kind(greater_equal) --> ['>','='].
token_kind(greater) --> ['>'].
token_kind(slash) --> ['/'].
token_kind(number(Value)) --> initial_numbers(Chars), { number_chars(Value, Chars) }. % Convert to number
token_kind(Result) --> get_keyword_or_identifier(Result).
token_kind(string(Value)) --> get_string(Chars), { string_chars(Value, Chars)}.
token_kind([]) --> [Ch], {throw(format("Unknown character '~w'", Ch))}.

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
    phrase(tokens(Tokens, 1), CharList).  % Apply the DCG to produce tokens