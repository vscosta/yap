
:- object(character,
	implements(characterp),
	extends(atom)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Character predicates.']).


	is_alpha('_').

	is_alpha(Char) :-
		is_letter(Char).


	is_letter(Char) :-
		is_lower_case(Char).

	is_letter(Char) :-
		is_upper_case(Char).


	is_alphanumeric(Char) :-
		is_alpha(Char).

	is_alphanumeric(Char) :-
		is_dec_digit(Char).


	is_bin_digit(0).
	is_bin_digit(1).


	is_octal_digit(Digit) :-
		Digit @>= 0,
		Digit @=< 7.


	is_dec_digit(Digit) :-
		Digit @>= 0,
		Digit @=< 9.


	is_hex_digit(Digit) :-
		Digit @>= 0,
		Digit @=< 9.

	is_hex_digit(Digit) :-
		Digit @>= 'A',
		Digit @=< 'F'.

	is_hex_digit(Digit) :-
		Digit @>= a,
		Digit @=< f.


	is_lower_case(Char) :-
		Char @>= a,
		Char @=< z.


	is_upper_case(Char) :-
		Char @>= 'A',
		Char @=< 'Z'.


	is_vowel(a).
	is_vowel(e).
	is_vowel(i).
	is_vowel(o).
	is_vowel(u).

	is_vowel('A').
	is_vowel('E').
	is_vowel('I').
	is_vowel('O').
	is_vowel('U').


	is_layout(' ').


	lower_upper(a, 'A').
	lower_upper(b, 'B').
	lower_upper(c, 'C').
	lower_upper(d, 'D').
	lower_upper(e, 'E').
	lower_upper(f, 'F').
	lower_upper(g, 'G').
	lower_upper(h, 'H').
	lower_upper(i, 'I').
	lower_upper(j, 'J').
	lower_upper(k, 'K').
	lower_upper(l, 'L').
	lower_upper(m, 'M').
	lower_upper(n, 'N').
	lower_upper(o, 'O').
	lower_upper(p, 'P').
	lower_upper(q, 'Q').
	lower_upper(r, 'R').
	lower_upper(s, 'S').
	lower_upper(t, 'T').
	lower_upper(u, 'U').
	lower_upper(v, 'V').
	lower_upper(w, 'W').
	lower_upper(x, 'X').
	lower_upper(y, 'Y').
	lower_upper(z, 'Z').

	lower_upper(Char, Char) :-
		\+ (Char @>= a, Char @=< z),
		\+ (Char @>= 'A', Char @=< 'Z').


	valid(Character) :-
		atom(Character),
		atom_length(Character, 1).


:- end_object.
