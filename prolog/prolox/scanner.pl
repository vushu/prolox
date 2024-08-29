:- module(scanner,[scan/2]).

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

scan_aux([], []).

scan_aux([H|T], Result) :-
	is_digit(H), 
	scan_numbers(T, Numbers, false), % write_ln(Numbers), H = Numbers, 
	append([H], Numbers, Result).
	scan_aux([], Result).


scan_numbers([], [], _).

scan_numbers([H|T], [H|R], HasDot) :-
	is_digit(H), 
	scan_numbers(T, R, HasDot).

% incase of float number this rule must hold

scan_numbers([H, H2, H3|T], [H, H2, H3|R], false) :-
	is_digit(H), 
	is_dot(H2), 
	is_digit(H3), 
	scan_numbers(T, R, true).


% is_number(Code, Pos, [Ch|Result]) :-
% 	current_is_digit(Code, Pos, Ch), 
% 	is_number(Code, Pos, Result).