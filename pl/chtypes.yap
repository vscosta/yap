/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		chtypes.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	implementation of SWI's code_type/2			 *
*									 *
*************************************************************************/

/*
  
In addition, there is the library library(ctype) providing compatibility to some other Prolog systems. The predicates of this library are defined in terms of code_type/2.

char_type(?Char, ?Type)
    Tests or generates alternative Types or Chars. The character-types are inspired by the standard C <ctype.h> primitives.

    alnum
        Char is a letter (upper- or lowercase) or digit.

    alpha
        Char is a letter (upper- or lowercase).

    csym
        Char is a letter (upper- or lowercase), digit or the underscore (_). These are valid C- and Prolog symbol characters.

    csymf
        Char is a letter (upper- or lowercase) or the underscore (_). These are valid first characters for C- and Prolog symbols

    ascii
        Char is a 7-bits ASCII character (0..127).

    white
        Char is a space or tab. E.i. white space inside a line.

    cntrl
        Char is an ASCII control-character (0..31).

    digit
        Char is a digit.

    digit(Weigth)
        Char is a digit with value Weigth. I.e. char_type(X, digit(6) yields X = '6'. Useful for parsing numbers.

    xdigit(Weigth)
        Char is a haxe-decimal digit with value Weigth. I.e. char_type(a, xdigit(X) yields X = '10'. Useful for parsing numbers.

    graph
        Char produces a visible mark on a page when printed. Note that the space is not included!

    lower
        Char is a lower-case letter.

    lower(Upper)
        Char is a lower-case version of Upper. Only true if Char is lowercase and Upper uppercase.

    to_lower(Upper)
        Char is a lower-case version of Upper. For non-letters, or letter without case, Char and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

    upper
        Char is an upper-case letter.

    upper(Lower)
        Char is an upper-case version of Lower. Only true if Char is uppercase and Lower lowercase.

    to_upper(Lower)
        Char is an upper-case version of Lower. For non-letters, or letter without case, Char and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

    punct
        Char is a punctuation character. This is a graph character that is not a letter or digit.

    space
        Char is some form of layout character (tab, vertical-tab, newline, etc.).

    end_of_file
        Char is -1.

    end_of_line
        Char ends a line (ASCII: 10..13).

    newline
        Char is a the newline character (10).

    period
        Char counts as the end of a sentence (.,!,?).

    quote
        Char is a quote-character (", ', `).

    paren(Close)
        Char is an open-parenthesis and Close is the corresponding close-parenthesis. 

code_type(?Code, ?Type)
    As char_type/2, but uses character-codes rather than one-character atoms. Please note that both predicates are as flexible as possible. They handle either representation if the argument is instantiated and only will instantiate with an integer code or one-character atom depending of the version used. See also the prolog-flag double_quotes, atom_chars/2 and atom_codes/2. 

*/
				  
char_type(A, Spec) :-
	var(A), !,
	(ground(Spec),
	 '$handle_special_char_type'(Code, Spec)
	->
	 true
	;
	 '$char_spec_code_from_spec'(Spec, SpecCode),
	 '$code_enum'(Code, SpecCode),
	 '$spec_code_to_char'(SpecCode, Spec)
	),
	atom_codes(A,[Code]).
char_type(A, Spec) :-
	atom(A), !,
	atom_codes(A,[Code]),
	'$code_type'(Code, SpecCode),
	'$spec_code_to_char'(SpecCode, Spec).
char_type(Code, Spec) :-
	number(Code), !,
	'$code_type'(Code, SpecCode),
	'$spec_code_to_char'(SpecCode, Spec).
char_type(Code, Spec) :-
	'$do_error'(type_error(character,Code),char_type(Code, Spec)).

'$char_spec_code_from_spec'(Spec, Spec) :- atom(Spec), !.
'$char_spec_code_from_spec'(digit(Weight), digit(Weight)).
'$char_spec_code_from_spec'(xdigit(Weight), xdigit(Weight)).
'$char_spec_code_from_spec'(lower(Upper), lower(_)).
'$char_spec_code_from_spec'(to_lower(Upper), to_lower(_)).
'$char_spec_code_from_spec'(upper(Upper), upper(_)).
'$char_spec_code_from_spec'(to_upper(Upper), to_upper(_)).

code_type(Code, Spec) :-
	var(Code), !,
	(ground(Spec),
	 '$handle_special_char_type'(Code, Spec)
	->
	 true
	;
	 '$code_enum'(Code, Spec)
	).
code_type(A, Spec) :-
	atom(A), !,
	atom_codes(A,[Code]),
	'$code_type'(Code, Spec).
code_type(Code, Spec) :-
	number(Code), !,
	'$code_type'(Code, Spec).
code_type(Code, Spec) :-
	'$do_error'(type_error(character,Code),char_type(Code, Spec)).

'$code_enum'(Code, Spec) :-
	'$for'(0, 256, Code),
	'$code_type'(Code, Spec).

'$for'(Min, Max, Min).
'$for'(Min, Max, I) :-
	Min < Max,
	Min1 is Min+1,
	'$for'(Min1, Max, I).


'$code_type'(Code, Spec) :-
	'$type_of_char'(Code, TypeCode),
	'$code_type_name'(TypeCode, Type),
	'$type_code'(Type, Code, Spec).

'$code_type_name'( 1,uc).       /* Upper case */
'$code_type_name'( 2,ul).       /* Underline */
'$code_type_name'( 3,lc).       /* Lower case */
'$code_type_name'( 4,nu).       /* digit */
'$code_type_name'( 5,qt).       /* single quote */
'$code_type_name'( 6,dc).	/* double quote */
'$code_type_name'( 7,sy).       /* Symbol character */
'$code_type_name'( 8,sl).       /* Solo character */
'$code_type_name'( 9,bk).       /* Brackets & friends */
'$code_type_name'(10,bs).       /* Blank */
'$code_type_name'(11,ef).	/* End of File marker */
'$code_type_name'(12,cc).	/* comment char %	*/

'$spec_code_to_char'(lower(Code), lower(Char)) :- !,
	atom_codes(Char, [Code]).
'$spec_code_to_char'(to_lower(Code), to_lower(Char)) :- !,
	atom_codes(Char, [Code]).
'$spec_code_to_char'(upper(Code), upper(Char)) :- !,
	atom_codes(Char, [Code]).
'$spec_code_to_char'(to_upper(Code), to_upper(Char)) :- !,
	atom_codes(Char, [Code]).
'$spec_code_to_char'(Spec, Spec).


'$type_code'(Type, _, alnum) :-
	'$type_code_alnum'(Type).
'$type_code'(Type, _, alpha) :-
	'$type_code_alpha'(Type).
'$type_code'(Type, _, csym) :-
	'$type_code_csym'(Type).
'$type_code'(Type, _, csymf) :-
	'$type_code_csymf'(Type).
'$type_code'(_, Code, ascii) :-
	'$type_code_ascii'(Code).
'$type_code'(_, Code, white) :-
	'$type_code_white'(Code).
'$type_code'(_, Code, cntrl) :-
	'$type_code_cntrl'(Code).
'$type_code'(Type, _, digit) :-
	'$type_code_digit'(Type).
'$type_code'(_, Code, digit(Weight)) :-
	'$type_code_digit'(Code, Weight).
'$type_code'(_, Code, xdigit(Weight)) :-
	'$type_code_xdigit'(Code, Weight).
'$type_code'(Type, _, graph) :-
	'$type_code_graph'(Type).
'$type_code'(Type, _, lower) :-
	'$type_code_lower'(Type).
'$type_code'(Type, Code, lower(UpCode)) :-
	'$type_code_lower'(Type, Code, UpCode).
'$type_code'(Type, Code, to_lower(UpCode)) :-
	'$type_code_to_lower'(Type,Code,UpCode).
'$type_code'(Type, _, upper) :-
	'$type_code_upper'(Type).
'$type_code'(Type, Code, upper(UpCode)) :-
	'$type_code_upper'(Type,Code,UpCode).
'$type_code'(Type, Code, to_upper(UpCode)) :-
	'$type_code_to_upper'(Type,Code,UpCode).
'$type_code'(Type, _, punct) :-
	'$type_code_punct'(Type).
'$type_code'(Type, _, space) :-
	'$type_code_space'(Type).
'$type_code'(Type, _, end_of_file) :-
	'$type_code_end_of_file'(Type).
'$type_code'(_, Code, end_of_line) :-
	'$type_code_end_of_line'(Code).
'$type_code'(_, Code, newline) :-
	'$type_code_newline'(Code).
'$type_code'(_, Code, period) :-
	'$type_code_period'(Code).
'$type_code'(_, Code, quote) :-
	'$type_code_quote'(Code).


'$type_code_alnum'(uc).
'$type_code_alnum'(lc).
'$type_code_alnum'(nu).

'$type_code_alpha'(uc).
'$type_code_alpha'(lc).

'$type_code_csym'(uc).
'$type_code_csym'(ul).
'$type_code_csym'(lc).
'$type_code_csym'(nu).

'$type_code_csymf'(uc).
'$type_code_csymf'(ul).
'$type_code_csymf'(lc).

'$type_code_ascii'(Cod) :- Cod < 128.

'$type_code_white'(0' ).
'$type_code_white'(0'	).

'$type_code_cntrl'(C) :- C < 32.

'$type_code_digit'(nu).

'$type_code_digit'(0'0, 0).
'$type_code_digit'(0'1, 1).
'$type_code_digit'(0'2, 2).
'$type_code_digit'(0'3, 3).
'$type_code_digit'(0'4, 4).
'$type_code_digit'(0'5, 5).
'$type_code_digit'(0'6, 6).
'$type_code_digit'(0'7, 7).
'$type_code_digit'(0'8, 8).
'$type_code_digit'(0'9, 9).

'$type_code_xdigit'(0'0, 0).
'$type_code_xdigit'(0'1, 1).
'$type_code_xdigit'(0'2, 2).
'$type_code_xdigit'(0'3, 3).
'$type_code_xdigit'(0'4, 4).
'$type_code_xdigit'(0'5, 5).
'$type_code_xdigit'(0'6, 6).
'$type_code_xdigit'(0'7, 7).
'$type_code_xdigit'(0'8, 8).
'$type_code_xdigit'(0'9, 9).
'$type_code_xdigit'(0'a, 10).
'$type_code_xdigit'(0'A, 10).
'$type_code_xdigit'(0'b, 11).
'$type_code_xdigit'(0'B, 11).
'$type_code_xdigit'(0'c, 12).
'$type_code_xdigit'(0'C, 12).
'$type_code_xdigit'(0'd, 13).
'$type_code_xdigit'(0'D, 13).
'$type_code_xdigit'(0'e, 14).
'$type_code_xdigit'(0'E, 14).
'$type_code_xdigit'(0'f, 15).
'$type_code_xdigit'(0'F, 15).

'$type_code_graph'(uc).
'$type_code_graph'(ul).
'$type_code_graph'(lc).
'$type_code_graph'(nu).
'$type_code_graph'(qt).
'$type_code_graph'(dc).
'$type_code_graph'(sy).
'$type_code_graph'(sl).
'$type_code_graph'(bk).
'$type_code_graph'(cc).

'$type_code_lower'(lc).

'$type_code_lower'(lc, Code, Upcode) :-
	'$toupper'(Code, Upcode).

'$type_code_to_lower'(uc, C, C).
'$type_code_to_lower'(ul, C, C).
'$type_code_to_lower'(lc, Code, Upcode) :-
	'$toupper'(Code, Upcode).
'$type_code_to_lower'(nu, C, C).
'$type_code_to_lower'(qt, C, C).
'$type_code_to_lower'(dc, C, C).
'$type_code_to_lower'(sy, C, C).
'$type_code_to_lower'(sl, C, C).
'$type_code_to_lower'(bk, C, C).
'$type_code_to_lower'(bs, C, C).
'$type_code_to_lower'(ef, C, C).
'$type_code_to_lower'(cc, C, C).

'$type_code_upper'(uc).

'$type_code_upper'(uc, Code, Upcode) :-
	'$tolower'(Code, Upcode).

'$type_code_to_upper'(uc, Code, Upcode) :-
	'$tolower'(Code, Upcode).
'$type_code_to_upper'(ul, C, C).
'$type_code_to_upper'(lc, C, C).
'$type_code_to_upper'(nu, C, C).
'$type_code_to_upper'(qt, C, C).
'$type_code_to_upper'(dc, C, C).
'$type_code_to_upper'(sy, C, C).
'$type_code_to_upper'(sl, C, C).
'$type_code_to_upper'(bk, C, C).
'$type_code_to_upper'(bs, C, C).
'$type_code_to_upper'(ef, C, C).
'$type_code_to_upper'(cc, C, C).

'$type_code_punct'(ul).
'$type_code_punct'(qt).
'$type_code_punct'(dc).
'$type_code_punct'(sy).
'$type_code_punct'(sl).
'$type_code_punct'(bk).
'$type_code_punct'(cc).

'$type_code_space'(bs).

'$type_code_end_of_file'(ef).

'$type_code_end_of_line'(10).
'$type_code_end_of_line'(11).
'$type_code_end_of_line'(12).
'$type_code_end_of_line'(13).

'$type_code_newline'(10).

'$type_code_period'(  0).
'$type_code_period'(0'!).
'$type_code_period'(0'.).
'$type_code_period'(0'?).

'$type_code_quote'(  0). %'
'$type_code_quote'(0'").
'$type_code_quote'(0'').
'$type_code_quote'(0'`).

'$type_code_paren'(0'{, 0'}).
'$type_code_paren'(0'[, 0']).
'$type_code_paren'(0'(, 0'(). %'

'$handle_special_char_type'(Spec, digit(N)) :-
	integer(N),
	N >= 0,
	N =< 9,
	Spec is "0"+N.
'$handle_special_char_type'(Spec, xdigit(N)) :-
	integer(N),
	N >= 0,
	(
	 N =< 9
	 ->
	 Spec is "0"+N
	;
	 N =< 15
	->
	 Spec is "a"+(N-10)
	).
'$handle_special_char_type'(Spec, lower(Upper)) :-
	Upper >= "A",
	Upper =< "Z",
	Spec is Upper + ("a"-"A").
'$handle_special_char_type'(Spec, to_lower(Upper)) :-
	( Upper >= "A",
	  Upper =< "Z"
	->
	  Spec is Upper + ("a"-"A")
	;
	  Spec = Upper
	).
'$handle_special_char_type'(Spec, upper(Lower)) :-
	Lower >= "a",
	Lower =< "z",
	Spec is Lower + ("A"-"a").
'$handle_special_char_type'(Spec, to_upper(Lower)) :-
	( Lower >= "a",
	  Lower =< "z"
	->
	  Spec is Lower + ("A"-"a")
	;
	  Spec = Lower
	).
	  

downcase_atom(U, D) :-
	atom_codes(U, Codes),
	'$downcase_codes'(Codes, DCodes),
	atom_codes(D, DCodes).

'$downcase_codes'([], []).
'$downcase_codes'(C.Codes, D.DCodes) :-
	code_type(D, to_lower(C)),	
	'$downcase_codes'(Codes, DCodes).

upcase_atom(U, D) :-
	atom_codes(U, Codes),
	'$upcase_codes'(Codes, DCodes),
	atom_codes(D, DCodes).

'$upcase_codes'([], []).
'$upcase_codes'(C.Codes, D.DCodes) :-
	code_type(D, to_upper(C)),	
	'$upcase_codes'(Codes, DCodes).
