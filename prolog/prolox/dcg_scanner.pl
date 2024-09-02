% Main DCG rule to produce a list of tokens
tokens([Token | RestTokens], Line) -->
    whitespace,
    newline(Line, NextLine),
    token_kind(T),
    {Token  = token(T, NextLine)}, % Match a single token
    tokens(RestTokens, NextLine).    % Recursively match the rest of the tokens
tokens([], _) --> whitespace, [].          % Base case: no more tokens

% DCG rules to recognize specific tokens
token_kind(left_paren)  --> ['('].
token_kind(right_paren) --> [')'].
token_kind(left_brace)  --> ['{'].
token_kind(right_brace) --> ['}'].


% DCG rule to match and skip whitespace characters
whitespace --> ['\t'], whitespace.
whitespace --> ['\r'], whitespace.
whitespace --> [' '], whitespace.
whitespace --> [].  % Base case: no more whitespace

newline(Line, NextLine) --> ['\n'], { NextLine is Line + 1  }, newline(NextLine, _).
newline(Line, Line) --> [].  % Base case


% Helper predicate to tokenize a string
tokenize_string(String, Tokens) :-
    string_chars(String, CharList),  % Convert string to list of characters
    phrase(tokens(Tokens, 1), CharList).  % Apply the DCG to produce tokens