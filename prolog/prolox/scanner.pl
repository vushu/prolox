:- module(scanner,[scan/2, make_token/2]).

	% make_token(+Kind, +Line, -Token)
% +Kind: The type or kind of the token (e.g., identifier, keyword, operator).
% +Line: The line number where the token appears.
% -Token: The resulting token structure.

is_space(Char) :-
	member(Char, [' ', '\t', '\n', '\r']).

is_digit(Char) :-
	char_type(Char, digit).

get_char_at_index(String, Index, Char) :-
	sub_atom(String, Index, 1, _, Char).


% Base case: When the character is not a digit, stop and return the digits collected so far.

is_dot(Ch) :-
	char_code(Ch, 46).

current_is_digit(Code, Pos, Ch) :-
	get_char_at_index(Code, Pos, Ch), 
	is_digit(Ch).

% Recursive case: Process a digit and continue to the next character.

is_at_end(Atom, Pos) :-
	atom_length(Atom, Len), Pos > Len.

%Base

scan(Code, Result) :-
	atom_codes(A, Code), 
	atom_chars(A, CharList), 
	scan_aux(CharList, Result).

scan_aux([], _).

char_list_to_float(CharList, Float) :-
	atom_chars(Atom, CharList), % Convert the char list to an atom

	atom_number(Atom, Float).% Convert the atom to a number (float)
	
	scan_aux([C|Tail], [Tok|Result])
	  :- (
		is_space(C)
		 ->
				scan_aux(Tail, Result);
		is_digit(C)
		 ->
				scan_numbers(Tail, Numbers, Rest, false), Chars = [C|Numbers], 
		char_list_to_float(Chars, Float), 
		make_token(
			number(Float), 1, Tok), 
		write_ln("Done making"), 
		scan_aux(Rest, Result);
		C == '('
		 ->
				write_ln("Left Paren"), 
		make_token(left_paren, 1, Tok), 
		scan_aux(Tail, Result);
		C == ')'
		 ->
				write_ln("Right Paren"), 
		make_token(right_paren, 1, Tok), 
		scan_aux(Tail, Result);
		C == '{'
		 ->
				write_ln("Right Paren"), 
		make_token(left_brace, 1, Tok), 
		scan_aux(Tail, Result);
		C == '}'
		 ->
				write_ln("Right Paren"), 
		make_token(right_brace, 1, Tok), 
		scan_aux(Tail, Result);
		otherwise ->
			write_ln('Unexpected character'), fail).

scan_numbers([], [], [], _) :-
	write_ln("BASE Scan_numbers").

scan_numbers([H|T], [H|R], Rest, HasDot) :-
	is_digit(H), 
	write_ln("Consuming Digit"), 
	scan_numbers(T, R, Rest, HasDot).

scan_numbers([H|T], [], [H|T], _) :-
	 \+ is_digit(H), 
	write_ln("Not digits left terminate").

% incase of float number this rule must hold

scan_numbers([H, H2, H3|Tail], [H, H2, H3|R], Rest, false) :-
	is_digit(H), 
	is_dot(H2), 
	is_digit(H3), 
	write_ln("Floatnumber!"), 
	scan_numbers(Tail, R, Rest, true).



make_token(Kind, Line, token(Kind, line(Line))).