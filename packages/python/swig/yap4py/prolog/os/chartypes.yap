:- module('$char_type',[
			  op(1150, fx, block)
				%dif/2,
				%when/2,
				%block/1,
				%wait/1,
				%frozen/2
			 ]).

/**
  @defgroup  CharacterCodes Character Encoding and Manipulation.
  @ingroup InputOutput
  @{

The Prolog library includes a set of built-in predicates designed to
support manipulation of sequences of text, represented either as
lists, atoms or strings.

The char_type family of predicates support manipulation of individual characters, represented either as numbers (codes) or atoms (chars).

YAP supports UNICODE through the library utf8proc for compatibility
across operating systems. The implementation extends the original
(ASCII-based) character classification used by early Prologs, but
supports UNICODE.As usual, YAP tried to follow SWI-Prolog as much as
possible:

  + char_type/2 and code_type/2 support all documented SWI-Prolog flags,
but are strict in argument checking.

  + letters with no case are considered as lower-case. Hence, a
  variable can only start with an underscore or a unicode point in
  category LU.

  + number letters are consideed numbers.

  + connectors, dashes, are considered solo characters.

  + YAP does currently distinguish opening and closing quotes.

  + Symbols are processed as Prolog symbols, exception are modifiers
  that are handled as lower-case letters.

  Predicates are:

  + @ref char_type/2
  + @ref code_type/2



*/

/** @predicate char_type(?_Char_ , ?Type)

The character _Char_ has type _Type_. The types included here are
based on SWI-Prolog's documentation, and they include several types
from the C-library. It it is possible for a character to have
different types.

  + alnum
  Char is a letter (upper- or lowercase) or digit.

  +alpha
  Char is a letter (upper- or lowercase).

  + csym
  Char is a letter (upper- or lowercase), digit or the underscore (_). These are valid C and Prolog symbol characters.

  + csymf
Char is a letter (upper- or lowercase) or the underscore (_). These are valid first characters for C and Prolog symbols.

  + ascii
  Char is a 7-bit ASCII character (0..127).

  + white
  Char is a space or tab, i.e. white space inside a line.

  + cntrl
  Char is an ASCII control character (0..31).

  + digit
  Char is a digit.

  + digit(Weight)
   Char is a digit with value Weight. I.e. char_type(X, digit(6) yields X = '6'. Useful for parsing numbers.

+ xdigit(Weight)
  Char is a hexadecimal digit with value Weight. I.e. char_type(a, xdigit(X) yields X = '10'. Useful for parsing numbers.

  + graph
  Char produces a visible mark on a page when printed. Note that the space is not included!

  + lower
  Char is a lowercase letter.

  + lower(Upper)
  Char is a lowercase version of Upper. Only true if Char is lowercase and Upper uppercase.

  + to_lower(Upper)
  Char is a lowercase version of Upper. For non-letters, or letter without case, Char and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

  + upper
  Char is an uppercase letter.

  + upper(Lower)
Char is an uppercase version of Lower. Only true if Char is uppercase and Lower lowercase.

  + to_upper(Lower)
  Char is an uppercase version of Lower. For non-letters, or letter without case, Char and Lower are the same. See also upcase_atom/2 and downcase_atom/2.

  + punct
  Char is a punctuation character. This is a graph character that is not a letter or digit.

  + space
  Char is some form of layout character (tab, vertical tab, newline, etc.).

  + end_of_file
  Char is -1.

  + end_of_line
  Char ends a line (ASCII: 10..13).

  + newline
  Char is a newline character (10).

  + period
  Char counts as the end of a sentence (.,!,?).

  + quote
  Char is a quote character (", ', `).

  + paren(Close)
  Char is an open parenthesis and Close is the corresponding close parenthesis.

  + prolog_var_start
  Char can start a Prolog variable name.

  + prolog_atom_start
  Char can start a unquoted Prolog atom that is not a symbol.

  + prolog_identifier_continue
  Char can continue a Prolog variable name or atom.

  + prolog_prolog_symbol
  Char is a Prolog symbol character. Sequences of Prolog symbol characters glue together to form an unquoted atom. Examples are =.., \=, etc.
*/

:- discontiguous digit_weight/2, digit_weight/3.

prolog:char_type( CH, TYPE) :-
	(nonvar( CH )
	->
		true
		;
	arg(1,TYPE, A),
	 atomic(A)
	->
	 true
	;
	 between(0,0x10FFFF,I),
	 atom_codes(CH, [I])
	 ),
	 p_char_type( CH, TYPE).


p_char_type( ALNUM, alnum) :-
    char_type_alnum( ALNUM ).
p_char_type( ALPHA, alpha) :-
    char_type_alpha( ALPHA).
p_char_type( CSYM, csym) :-
    char_type_csym( CSYM ).
p_char_type( CSYMF, csymf) :-
    char_type_csymf( CSYMF).
p_char_type( ASCII, ascii ) :-
    char_type_ascii( ASCII ).
p_char_type( WHITE, white) :-
    char_type_white( WHITE ).
p_char_type( CNTRL , cntrl) :-
    char_type_cntrl( CNTRL ).
p_char_type( DIGIT , digit) :-
    char_type_digit( DIGIT ).
p_char_type(  DIGIT, digit(Weight) ) :-
    char_type_digit( DIGIT ),
    digit_weight( DIGIT, Weight ).
p_char_type( XDIGIT, xdigit(Weight) ) :-
    char_type_digit( XDIGIT ),
     xdigit_weight( XDIGIT, Weight ).
p_char_type( GRAPH , graph) :-
    char_type_graph( GRAPH ).
p_char_type( LOWER , lower) :-
    char_type_lower( LOWER ).
p_char_type( LOWER, lower( Upper)) :-
	toupper( LOWER, Upper),
    char_type_lower( LOWER ).
p_char_type( LOWER, to_lower( Upper)) :-
	toupper( LOWER, Upper).
p_char_type( UPPER, upper  ) :-
     char_type_upper( UPPER ).
p_char_type( UPPER , upper( Lower)) :-
    char_type_upper( UPPER ),
    tolower( UPPER, Lower).
p_char_type( UPPER, to_upper( Lower) ) :-
	tolower( UPPER, Lower).
p_char_type( PUNCT , punct) :-
    char_type_punct( PUNCT ).
p_char_type( SPACE , space) :-
    char_type_space( SPACE ).
p_char_type( END_OF_FILE , end_of_file) :-
    char_type_end_of_file( END_OF_FILE ).
p_char_type( END_OF_LINE , end_of_line) :-
    char_type_end_of_line( END_OF_LINE ).
p_char_type( NEWLINE , newline) :-
    char_type_newline( NEWLINE ).
p_char_type( PERIOD , period) :-
    char_type_period( PERIOD ).
p_char_type( QUOTE , quote) :-
    char_type_quote( QUOTE ).
p_char_type( Parent_Open, paren( PAREN_CLOSE) ) :-
    paren_paren(Parent_Open, PAREN_CLOSE).
p_char_type( PROLOG_VAR_START , prolog_var_start) :-
    char_type_prolog_var_start( PROLOG_VAR_START ).
p_char_type( PROLOG_ATOM_START , prolog_atom_start) :-
    char_type_prolog_atom_start( PROLOG_ATOM_START ).
p_char_type( PROLOG_IDENTIFIER_CONTINUE , prolog_identifier_continue) :-
    char_type_prolog_identifier_continue( PROLOG_IDENTIFIER_CONTINUE ).
p_char_type( PROLOG_PROLOG_SYMBOL , prolog_prolog_symbol) :-
    char_type_prolog_prolog_symbol( PROLOG_PROLOG_SYMBOL ).

prolog:code_type(CH, TYPE) :-
		(nonvar( CH )
		->
			true
			;
		arg(1,TYPE, A),
		 atomic(A)
		->
		 true
		;
		 between(0,0x10FFFF,CH)
		 ),
		 p_code_type( CH, TYPE).

p_code_type( ALNUM, alnum) :-
	code_type_alnum( ALNUM ).
p_code_type( ALPHA, alpha) :-
	code_type_alpha( ALPHA).
p_code_type( CSYM, csym) :-
	code_type_csym( CSYM ).
p_code_type( CSYMF, csymf) :-
	code_type_csymf( CSYMF).
p_code_type( ASCII, ascii ) :-
	code_type_ascii( ASCII ).
p_code_type( WHITE, white) :-
	code_type_white( WHITE ).
p_code_type( CNTRL , cntrl) :-
	code_type_cntrl( CNTRL ).
p_code_type( DIGIT , digit) :-
	code_type_digit( DIGIT ).
p_code_type(  DIGIT, digit(Weight) ) :-
	code_type_digit( DIGIT ),
	digit_weight( DIGIT, Weight ).
p_code_type( XDIGIT, xdigit(Weight) ) :-
	code_type_digit( XDIGIT ),
	xdigit_weight( XDIGIT, Weight ).
p_code_type( GRAPH , graph) :-
	code_type_graph( GRAPH ).
p_code_type( LOWER , lower) :-
	code_type_lower( LOWER ).
p_code_type( LOWER, lower( Upper)) :-
	toupper( LOWER, Upper),
	code_type_lower( LOWER ).
p_code_type( LOWER, to_lower( Upper)) :-
	toupper( LOWER, Upper).
p_code_type( UPPER, upper  ) :-
	code_type_upper( UPPER ).
p_code_type( UPPER , upper( Lower)) :-
	tolower( UPPER, Lower).
p_code_type( UPPER, to_upper( Lower) ) :-
	tolower( UPPER, Lower),
	char_type_upper( UPPER).
p_code_type( PUNCT , punct) :-
	code_type_punct( PUNCT ).
p_code_type( SPACE , space) :-
	code_type_space( SPACE ).
p_code_type( END_OF_FILE , end_of_file) :-
	code_type_end_of_file( END_OF_FILE ).
p_code_type( END_OF_LINE , end_of_line) :-
	code_type_end_of_line( END_OF_LINE ).
p_code_type( NEWLINE , newline) :-
	code_type_newline( NEWLINE ).
p_code_type( PERIOD , period) :-
	code_type_period( PERIOD ).
p_code_type( QUOTE , quote) :-
	code_type_quote( QUOTE ).
p_code_type( Parent_Open, paren( PAREN_CLOSE) ) :-
	paren_paren(Parent_Open, PAREN_CLOSE).
p_code_type( PROLOG_VAR_START , prolog_var_start) :-
	code_type_prolog_var_start( PROLOG_VAR_START ).
p_code_type( PROLOG_ATOM_START , prolog_atom_start) :-
	code_type_prolog_atom_start( PROLOG_ATOM_START ).
p_code_type( PROLOG_IDENTIFIER_CONTINUE , prolog_identifier_continue) :-
	code_type_prolog_identifier_continue( PROLOG_IDENTIFIER_CONTINUE ).
p_code_type( PROLOG_PROLOG_SYMBOL , prolog_prolog_symbol) :-
	code_type_prolog_prolog_symbol( PROLOG_PROLOG_SYMBOL ).


/* numeric code sequences, obtained from
http://www.unicode.org/Public/8.0.0/ucd/extracted/DerivedNumericValues.txt

by using:

 grep '[ \t]\#' DerivedNumericValues.txt | awk '{ print "ch( 0x" $1 ", "$6 ")." }'

*/

digit_weight( 0x0F33, -1/2).
digit_weight( 0x0030, 0).
digit_weight( 0x0660, 0).
digit_weight( 0x06F0, 0).
digit_weight( 0x07C0, 0).
digit_weight( 0x0966, 0).
digit_weight( 0x09E6, 0).
digit_weight( 0x0A66, 0).
digit_weight( 0x0AE6, 0).
digit_weight( 0x0B66, 0).
digit_weight( 0x0BE6, 0).
digit_weight( 0x0C66, 0).
digit_weight( 0x0C78, 0).
digit_weight( 0x0CE6, 0).
digit_weight( 0x0D66, 0).
digit_weight( 0x0DE6, 0).
digit_weight( 0x0E50, 0).
digit_weight( 0x0ED0, 0).
digit_weight( 0x0F20, 0).
digit_weight( 0x1040, 0).
digit_weight( 0x1090, 0).
digit_weight( 0x17E0, 0).
digit_weight( 0x17F0, 0).
digit_weight( 0x1810, 0).
digit_weight( 0x1946, 0).
digit_weight( 0x19D0, 0).
digit_weight( 0x1A80, 0).
digit_weight( 0x1A90, 0).
digit_weight( 0x1B50, 0).
digit_weight( 0x1BB0, 0).
digit_weight( 0x1C40, 0).
digit_weight( 0x1C50, 0).
digit_weight( 0x2070, 0).
digit_weight( 0x2080, 0).
digit_weight( 0x2189, 0).
digit_weight( 0x24EA, 0).
digit_weight( 0x24FF, 0).
digit_weight( 0x3007, 0).
digit_weight( 0x96F6, 0).
digit_weight( 0xA620, 0).
digit_weight( 0xA6EF, 0).
digit_weight( 0xA8D0, 0).
digit_weight( 0xA900, 0).
digit_weight( 0xA9D0, 0).
digit_weight( 0xA9F0, 0).
digit_weight( 0xAA50, 0).
digit_weight( 0xABF0, 0).
digit_weight( 0xF9B2, 0).
digit_weight( 0xFF10, 0).
digit_weight( 0x1018A, 0).
digit_weight( 0x104A0, 0).
digit_weight( 0x11066, 0).
digit_weight( 0x110F0, 0).
digit_weight( 0x11136, 0).
digit_weight( 0x111D0, 0).
digit_weight( 0x112F0, 0).
digit_weight( 0x114D0, 0).
digit_weight( 0x11650, 0).
digit_weight( 0x116C0, 0).
digit_weight( 0x11730, 0).
digit_weight( 0x118E0, 0).
digit_weight( 0x16A60, 0).
digit_weight( 0x16B50, 0).
digit_weight( 0x1D7CE, 0).
digit_weight( 0x1D7D8, 0).
digit_weight( 0x1D7E2, 0).
digit_weight( 0x1D7EC, 0).
vdigit_weight( 0x1D7F6, 0).
digit_weight( 0x1F100, 0x1F101, 0).
digit_weight( 0x1F10B, 0x1F10C, 0).
digit_weight( 0x09F4, 1/16).
digit_weight( 0x0B75, 1/16).
digit_weight( 0xA833, 1/16).
digit_weight( 0x109F6, 1/12).
digit_weight( 0x2152, 1/10).
digit_weight( 0x2151, 1/9).
digit_weight( 0x09F5, 1/8).
digit_weight( 0x0B76, 1/8).
digit_weight( 0x215B, 1/8).
digit_weight( 0xA834, 1/8).
digit_weight( 0x1245F, 1/8).
digit_weight( 0x2150, 1/7).
digit_weight( 0x2159, 1/6).
digit_weight( 0x109F7, 1/6).
digit_weight( 0x12461, 1/6).
digit_weight( 0x09F6, 3/16).
digit_weight( 0x0B77, 3/16).
digit_weight( 0xA835, 3/16).
digit_weight( 0x2155, 1/5).
digit_weight( 0x00BC, 1/4).
digit_weight( 0x09F7, 1/4).
digit_weight( 0x0B72, 1/4).
digit_weight( 0x0D73, 1/4).
digit_weight( 0xA830, 1/4).
digit_weight( 0x10140, 1/4).
digit_weight( 0x1018B, 1/4).
digit_weight( 0x109F8, 1/4).
digit_weight( 0x10E7C, 1/4).
digit_weight( 0x12460, 1/4).
digit_weight( 0x12462, 0x12463, 1/4).
digit_weight( 0x2153, 1/3).
digit_weight( 0x109F9, 1/3).
digit_weight( 0x10E7D, 1/3).
digit_weight( 0x1245A, 1/3).
digit_weight( 0x1245D, 1/3).
digit_weight( 0x12465, 1/3).
digit_weight( 0x215C, 3/8).
digit_weight( 0x2156, 2/5).
digit_weight( 0x109FA, 5/12).
digit_weight( 0x00BD, 1/2).
digit_weight( 0x0B73, 1/2).
digit_weight( 0x0D74, 1/2).
digit_weight( 0x0F2A, 1/2).
digit_weight( 0x2CFD, 1/2).
digit_weight( 0xA831, 1/2).
digit_weight( 0x10141, 1/2).
digit_weight( 0x10175, 0x10176, 1/2).
digit_weight( 0x109BD, 1/2).
digit_weight( 0x109FB, 1/2).
digit_weight( 0x10E7B, 1/2).
digit_weight( 0x12464, 1/2).
digit_weight( 0x109FC, 7/12).
digit_weight( 0x2157, 3/5).
digit_weight( 0x215D, 5/8).
digit_weight( 0x2154, 2/3).
digit_weight( 0x10177, 2/3).
digit_weight( 0x109FD, 2/3).
digit_weight( 0x10E7E, 2/3).
digit_weight( 0x1245B, 2/3).
digit_weight( 0x1245E, 2/3).
digit_weight( 0x12466, 2/3).
digit_weight( 0x00BE, 3/4).
digit_weight( 0x09F8, 3/4).
digit_weight( 0x0B74, 3/4).
digit_weight( 0x0D75, 3/4).
digit_weight( 0xA832, 3/4).
digit_weight( 0x10178, 3/4).
digit_weight( 0x109FE, 3/4).
digit_weight( 0x2158, 4/5).
digit_weight( 0x215A, 5/6).
digit_weight( 0x109FF, 5/6).
digit_weight( 0x1245C, 5/6).
digit_weight( 0x215E, 7/8).
digit_weight( 0x109BC, 11/12).
digit_weight( 0x0031, 1).
digit_weight( 0x00B9, 1).
digit_weight( 0x0661, 1).
digit_weight( 0x06F1, 1).
digit_weight( 0x07C1, 1).
digit_weight( 0x0967, 1).
digit_weight( 0x09E7, 1).
digit_weight( 0x0A67, 1).
digit_weight( 0x0AE7, 1).
digit_weight( 0x0B67, 1).
digit_weight( 0x0BE7, 1).
digit_weight( 0x0C67, 1).
digit_weight( 0x0C79, 1).
digit_weight( 0x0C7C, 1).
digit_weight( 0x0CE7, 1).
digit_weight( 0x0D67, 1).
digit_weight( 0x0DE7, 1).
digit_weight( 0x0E51, 1).
digit_weight( 0x0ED1, 1).
digit_weight( 0x0F21, 1).
digit_weight( 0x1041, 1).
digit_weight( 0x1091, 1).
digit_weight( 0x1369, 1).
digit_weight( 0x17E1, 1).
digit_weight( 0x17F1, 1).
digit_weight( 0x1811, 1).
digit_weight( 0x1947, 1).
digit_weight( 0x19D1, 1).
digit_weight( 0x19DA, 1).
digit_weight( 0x1A81, 1).
digit_weight( 0x1A91, 1).
digit_weight( 0x1B51, 1).
digit_weight( 0x1BB1, 1).
digit_weight( 0x1C41, 1).
digit_weight( 0x1C51, 1).
digit_weight( 0x2081, 1).
digit_weight( 0x215F, 1).
digit_weight( 0x2160, 1).
digit_weight( 0x2170, 1).
digit_weight( 0x2460, 1).
digit_weight( 0x2474, 1).
digit_weight( 0x2488, 1).
digit_weight( 0x24F5, 1).
digit_weight( 0x2776, 1).
digit_weight( 0x2780, 1).
digit_weight( 0x278A, 1).
digit_weight( 0x3021, 1).
digit_weight( 0x3192, 1).
digit_weight( 0x3220, 1).
digit_weight( 0x3280, 1).
digit_weight( 0x4E00, 1).
digit_weight( 0x58F1, 1).
digit_weight( 0x58F9, 1).
digit_weight( 0x5E7A, 1).
digit_weight( 0x5F0C, 1).
digit_weight( 0xA621, 1).
digit_weight( 0xA6E6, 1).
digit_weight( 0xA8D1, 1).
digit_weight( 0xA901, 1).
digit_weight( 0xA9D1, 1).
digit_weight( 0xA9F1, 1).
digit_weight( 0xAA51, 1).
digit_weight( 0xABF1, 1).
digit_weight( 0xFF11, 1).
digit_weight( 0x10107, 1).
digit_weight( 0x10142, 1).
digit_weight( 0x10158, 0x1015A, 1).
digit_weight( 0x102E1, 1).
digit_weight( 0x10320, 1).
digit_weight( 0x103D1, 1).
digit_weight( 0x104A1, 1).
digit_weight( 0x10858, 1).
digit_weight( 0x10879, 1).
digit_weight( 0x108A7, 1).
digit_weight( 0x108FB, 1).
digit_weight( 0x10916, 1).
digit_weight( 0x109C0, 1).
digit_weight( 0x10A40, 1).
digit_weight( 0x10A7D, 1).
digit_weight( 0x10A9D, 1).
digit_weight( 0x10AEB, 1).
digit_weight( 0x10B58, 1).
digit_weight( 0x10B78, 1).
digit_weight( 0x10BA9, 1).
digit_weight( 0x10CFA, 1).
digit_weight( 0x10E60, 1).
digit_weight( 0x11052, 1).
digit_weight( 0x11067, 1).
digit_weight( 0x110F1, 1).
digit_weight( 0x11137, 1).
digit_weight( 0x111D1, 1).
digit_weight( 0x111E1, 1).
digit_weight( 0x112F1, 1).
digit_weight( 0x114D1, 1).
digit_weight( 0x11651, 1).
digit_weight( 0x116C1, 1).
digit_weight( 0x11731, 1).
digit_weight( 0x118E1, 1).
digit_weight( 0x12415, 1).
digit_weight( 0x1241E, 1).
digit_weight( 0x1242C, 1).
digit_weight( 0x12434, 1).
digit_weight( 0x1244F, 1).
digit_weight( 0x12458, 1).
digit_weight( 0x16A61, 1).
digit_weight( 0x16B51, 1).
digit_weight( 0x1D360, 1).
digit_weight( 0x1D7CF, 1).
digit_weight( 0x1D7D9, 1).
digit_weight( 0x1D7E3, 1).
digit_weight( 0x1D7ED, 1).
digit_weight( 0x1D7F7, 1).
digit_weight( 0x1E8C7, 1).
digit_weight( 0x1F102, 1).
digit_weight( 0x2092A, 1).
digit_weight( 0x0F2B, 3/2).
digit_weight( 0x0032, 2).
digit_weight( 0x00B2, 2).
digit_weight( 0x0662, 2).
digit_weight( 0x06F2, 2).
digit_weight( 0x07C2, 2).
digit_weight( 0x0968, 2).
digit_weight( 0x09E8, 2).
digit_weight( 0x0A68, 2).
digit_weight( 0x0AE8, 2).
digit_weight( 0x0B68, 2).
digit_weight( 0x0BE8, 2).
digit_weight( 0x0C68, 2).
digit_weight( 0x0C7A, 2).
digit_weight( 0x0C7D, 2).
digit_weight( 0x0CE8, 2).
digit_weight( 0x0D68, 2).
digit_weight( 0x0DE8, 2).
digit_weight( 0x0E52, 2).
digit_weight( 0x0ED2, 2).
digit_weight( 0x0F22, 2).
digit_weight( 0x1042, 2).
digit_weight( 0x1092, 2).
digit_weight( 0x136A, 2).
digit_weight( 0x17E2, 2).
digit_weight( 0x17F2, 2).
digit_weight( 0x1812, 2).
digit_weight( 0x1948, 2).
digit_weight( 0x19D2, 2).
digit_weight( 0x1A82, 2).
digit_weight( 0x1A92, 2).
digit_weight( 0x1B52, 2).
digit_weight( 0x1BB2, 2).
digit_weight( 0x1C42, 2).
digit_weight( 0x1C52, 2).
digit_weight( 0x2082, 2).
digit_weight( 0x2161, 2).
digit_weight( 0x2171, 2).
digit_weight( 0x2461, 2).
digit_weight( 0x2475, 2).
digit_weight( 0x2489, 2).
digit_weight( 0x24F6, 2).
digit_weight( 0x2777, 2).
digit_weight( 0x2781, 2).
digit_weight( 0x278B, 2).
digit_weight( 0x3022, 2).
digit_weight( 0x3193, 2).
digit_weight( 0x3221, 2).
digit_weight( 0x3281, 2).
digit_weight( 0x3483, 2).
digit_weight( 0x4E8C, 2).
digit_weight( 0x5169, 2).
digit_weight( 0x5F0D, 2).
digit_weight( 0x5F10, 2).
digit_weight( 0x8CAE, 2).
digit_weight( 0x8CB3, 2).
digit_weight( 0x8D30, 2).
digit_weight( 0xA622, 2).
digit_weight( 0xA6E7, 2).
digit_weight( 0xA8D2, 2).
digit_weight( 0xA902, 2).
digit_weight( 0xA9D2, 2).
digit_weight( 0xA9F2, 2).
digit_weight( 0xAA52, 2).
digit_weight( 0xABF2, 2).
digit_weight( 0xF978, 2).
digit_weight( 0xFF12, 2).
digit_weight( 0x10108, 2).
digit_weight( 0x1015B, 0x1015E, 2).
digit_weight( 0x102E2, 2).
digit_weight( 0x103D2, 2).
digit_weight( 0x104A2, 2).
digit_weight( 0x10859, 2).
digit_weight( 0x1087A, 2).
digit_weight( 0x108A8, 2).
digit_weight( 0x1091A, 2).
digit_weight( 0x109C1, 2).
digit_weight( 0x10A41, 2).
digit_weight( 0x10B59, 2).
digit_weight( 0x10B79, 2).
digit_weight( 0x10BAA, 2).
digit_weight( 0x10E61, 2).
digit_weight( 0x11053, 2).
digit_weight( 0x11068, 2).
digit_weight( 0x110F2, 2).
digit_weight( 0x11138, 2).
digit_weight( 0x111D2, 2).
digit_weight( 0x111E2, 2).
digit_weight( 0x112F2, 2).
digit_weight( 0x114D2, 2).
digit_weight( 0x11652, 2).
digit_weight( 0x116C2, 2).
digit_weight( 0x11732, 2).
digit_weight( 0x118E2, 2).
digit_weight( 0x12400, 2).
digit_weight( 0x12416, 2).
digit_weight( 0x1241F, 2).
digit_weight( 0x12423, 2).
digit_weight( 0x1242D, 2).
digit_weight( 0x12435, 2).
digit_weight( 0x1244A, 2).
digit_weight( 0x12450, 2).
digit_weight( 0x12456, 2).
digit_weight( 0x12459, 2).
digit_weight( 0x16A62, 2).
digit_weight( 0x16B52, 2).
digit_weight( 0x1D361, 2).
digit_weight( 0x1D7D0, 2).
digit_weight( 0x1D7DA, 2).
digit_weight( 0x1D7E4, 2).
digit_weight( 0x1D7EE, 2).
digit_weight( 0x1D7F8, 2).
digit_weight( 0x1E8C8, 2).
digit_weight( 0x1F103, 2).
digit_weight( 0x22390, 2).
digit_weight( 0x0F2C, 5/2).
digit_weight( 0x0033, 3).
digit_weight( 0x00B3, 3).
digit_weight( 0x0663, 3).
digit_weight( 0x06F3, 3).
digit_weight( 0x07C3, 3).
digit_weight( 0x0969, 3).
digit_weight( 0x09E9, 3).
digit_weight( 0x0A69, 3).
digit_weight( 0x0AE9, 3).
digit_weight( 0x0B69, 3).
digit_weight( 0x0BE9, 3).
digit_weight( 0x0C69, 3).
digit_weight( 0x0C7B, 3).
digit_weight( 0x0C7E, 3).
digit_weight( 0x0CE9, 3).
digit_weight( 0x0D69, 3).
digit_weight( 0x0DE9, 3).
digit_weight( 0x0E53, 3).
digit_weight( 0x0ED3, 3).
digit_weight( 0x0F23, 3).
digit_weight( 0x1043, 3).
digit_weight( 0x1093, 3).
digit_weight( 0x136B, 3).
digit_weight( 0x17E3, 3).
digit_weight( 0x17F3, 3).
digit_weight( 0x1813, 3).
digit_weight( 0x1949, 3).
digit_weight( 0x19D3, 3).
digit_weight( 0x1A83, 3).
digit_weight( 0x1A93, 3).
digit_weight( 0x1B53, 3).
digit_weight( 0x1BB3, 3).
digit_weight( 0x1C43, 3).
digit_weight( 0x1C53, 3).
digit_weight( 0x2083, 3).
digit_weight( 0x2162, 3).
digit_weight( 0x2172, 3).
digit_weight( 0x2462, 3).
digit_weight( 0x2476, 3).
digit_weight( 0x248A, 3).
digit_weight( 0x24F7, 3).
digit_weight( 0x2778, 3).
digit_weight( 0x2782, 3).
digit_weight( 0x278C, 3).
digit_weight( 0x3023, 3).
digit_weight( 0x3194, 3).
digit_weight( 0x3222, 3).
digit_weight( 0x3282, 3).
digit_weight( 0x4E09, 3).
digit_weight( 0x4EE8, 3).
digit_weight( 0x53C1, 0x53C4, 3).
digit_weight( 0x5F0E, 3).
digit_weight( 0xA623, 3).
digit_weight( 0xA6E8, 3).
digit_weight( 0xA8D3, 3).
digit_weight( 0xA903, 3).
digit_weight( 0xA9D3, 3).
digit_weight( 0xA9F3, 3).
digit_weight( 0xAA53, 3).
digit_weight( 0xABF3, 3).
digit_weight( 0xF96B, 3).
digit_weight( 0xFF13, 3).
digit_weight( 0x10109, 3).
digit_weight( 0x102E3, 3).
digit_weight( 0x104A3, 3).
digit_weight( 0x1085A, 3).
digit_weight( 0x1087B, 3).
digit_weight( 0x108A9, 3).
digit_weight( 0x1091B, 3).
digit_weight( 0x109C2, 3).
digit_weight( 0x10A42, 3).
digit_weight( 0x10B5A, 3).
digit_weight( 0x10B7A, 3).
digit_weight( 0x10BAB, 3).
digit_weight( 0x10E62, 3).
digit_weight( 0x11054, 3).
digit_weight( 0x11069, 3).
digit_weight( 0x110F3, 3).
digit_weight( 0x11139, 3).
digit_weight( 0x111D3, 3).
digit_weight( 0x111E3, 3).
digit_weight( 0x112F3, 3).
digit_weight( 0x114D3, 3).
digit_weight( 0x11653, 3).
digit_weight( 0x116C3, 3).
digit_weight( 0x11733, 3).
digit_weight( 0x118E3, 3).
digit_weight( 0x12401, 3).
digit_weight( 0x12408, 3).
digit_weight( 0x12417, 3).
digit_weight( 0x12420, 3).
digit_weight( 0x12424, 0x12425, 3).
digit_weight( 0x1242E, 0x1242F, 3).
digit_weight( 0x12436, 0x12437, 3).
digit_weight( 0x1243A, 0x1243B, 3).
digit_weight( 0x1244B, 3).
digit_weight( 0x12451, 3).
digit_weight( 0x12457, 3).
digit_weight( 0x16A63, 3).
digit_weight( 0x16B53, 3).
digit_weight( 0x1D362, 3).
digit_weight( 0x1D7D1, 3).
digit_weight( 0x1D7DB, 3).
digit_weight( 0x1D7E5, 3).
digit_weight( 0x1D7EF, 3).
digit_weight( 0x1D7F9, 3).
digit_weight( 0x1E8C9, 3).
digit_weight( 0x1F104, 3).
digit_weight( 0x20AFD, 3).
digit_weight( 0x20B19, 3).
digit_weight( 0x22998, 3).
digit_weight( 0x23B1B, 3).
digit_weight( 0x0F2D, 7/2).
digit_weight( 0x0034, 4).
digit_weight( 0x0664, 4).
digit_weight( 0x06F4, 4).
digit_weight( 0x07C4, 4).
digit_weight( 0x096A, 4).
digit_weight( 0x09EA, 4).
digit_weight( 0x0A6A, 4).
digit_weight( 0x0AEA, 4).
digit_weight( 0x0B6A, 4).
digit_weight( 0x0BEA, 4).
digit_weight( 0x0C6A, 4).
digit_weight( 0x0CEA, 4).
digit_weight( 0x0D6A, 4).
digit_weight( 0x0DEA, 4).
digit_weight( 0x0E54, 4).
digit_weight( 0x0ED4, 4).
digit_weight( 0x0F24, 4).
digit_weight( 0x1044, 4).
digit_weight( 0x1094, 4).
digit_weight( 0x136C, 4).
digit_weight( 0x17E4, 4).
digit_weight( 0x17F4, 4).
digit_weight( 0x1814, 4).
digit_weight( 0x194A, 4).
digit_weight( 0x19D4, 4).
digit_weight( 0x1A84, 4).
digit_weight( 0x1A94, 4).
digit_weight( 0x1B54, 4).
digit_weight( 0x1BB4, 4).
digit_weight( 0x1C44, 4).
digit_weight( 0x1C54, 4).
digit_weight( 0x2074, 4).
digit_weight( 0x2084, 4).
digit_weight( 0x2163, 4).
digit_weight( 0x2173, 4).
digit_weight( 0x2463, 4).
digit_weight( 0x2477, 4).
digit_weight( 0x248B, 4).
digit_weight( 0x24F8, 4).
digit_weight( 0x2779, 4).
digit_weight( 0x2783, 4).
digit_weight( 0x278D, 4).
digit_weight( 0x3024, 4).
digit_weight( 0x3195, 4).
digit_weight( 0x3223, 4).
digit_weight( 0x3283, 4).
digit_weight( 0x4E96, 4).
digit_weight( 0x56DB, 4).
digit_weight( 0x8086, 4).
digit_weight( 0xA624, 4).
digit_weight( 0xA6E9, 4).
digit_weight( 0xA8D4, 4).
digit_weight( 0xA904, 4).
digit_weight( 0xA9D4, 4).
digit_weight( 0xA9F4, 4).
digit_weight( 0xAA54, 4).
digit_weight( 0xABF4, 4).
digit_weight( 0xFF14, 4).
digit_weight( 0x1010A, 4).
digit_weight( 0x102E4, 4).
digit_weight( 0x104A4, 4).
digit_weight( 0x1087C, 4).
digit_weight( 0x108AA, 0x108AB, 4).
digit_weight( 0x109C3, 4).
digit_weight( 0x10A43, 4).
digit_weight( 0x10B5B, 4).
digit_weight( 0x10B7B, 4).
digit_weight( 0x10BAC, 4).
digit_weight( 0x10E63, 4).
digit_weight( 0x11055, 4).
digit_weight( 0x1106A, 4).
digit_weight( 0x110F4, 4).
digit_weight( 0x1113A, 4).
digit_weight( 0x111D4, 4).
digit_weight( 0x111E4, 4).
digit_weight( 0x112F4, 4).
digit_weight( 0x114D4, 4).
digit_weight( 0x11654, 4).
digit_weight( 0x116C4, 4).
digit_weight( 0x11734, 4).
digit_weight( 0x118E4, 4).
digit_weight( 0x12402, 4).
digit_weight( 0x12409, 4).
digit_weight( 0x1240F, 4).
digit_weight( 0x12418, 4).
digit_weight( 0x12421, 4).
digit_weight( 0x12426, 4).
digit_weight( 0x12430, 4).
digit_weight( 0x12438, 4).
digit_weight( 0x1243C, 0x1243F, 4).
digit_weight( 0x1244C, 4).
digit_weight( 0x12452, 0x12453, 4).
digit_weight( 0x12469, 4).
digit_weight( 0x16A64, 4).
digit_weight( 0x16B54, 4).
digit_weight( 0x1D363, 4).
digit_weight( 0x1D7D2, 4).
digit_weight( 0x1D7DC, 4).
digit_weight( 0x1D7E6, 4).
digit_weight( 0x1D7F0, 4).
digit_weight( 0x1D7FA, 4).
digit_weight( 0x1E8CA, 4).
digit_weight( 0x1F105, 4).
digit_weight( 0x20064, 4).
digit_weight( 0x200E2, 4).
digit_weight( 0x2626D, 4).
digit_weight( 0x0F2E, 9/2).
digit_weight( 0x0035, 5).
digit_weight( 0x0665, 5).
digit_weight( 0x06F5, 5).
digit_weight( 0x07C5, 5).
digit_weight( 0x096B, 5).
digit_weight( 0x09EB, 5).
digit_weight( 0x0A6B, 5).
digit_weight( 0x0AEB, 5).
digit_weight( 0x0B6B, 5).
digit_weight( 0x0BEB, 5).
digit_weight( 0x0C6B, 5).
digit_weight( 0x0CEB, 5).
digit_weight( 0x0D6B, 5).
digit_weight( 0x0DEB, 5).
digit_weight( 0x0E55, 5).
digit_weight( 0x0ED5, 5).
digit_weight( 0x0F25, 5).
digit_weight( 0x1045, 5).
digit_weight( 0x1095, 5).
digit_weight( 0x136D, 5).
digit_weight( 0x17E5, 5).
digit_weight( 0x17F5, 5).
digit_weight( 0x1815, 5).
digit_weight( 0x194B, 5).
digit_weight( 0x19D5, 5).
digit_weight( 0x1A85, 5).
digit_weight( 0x1A95, 5).
digit_weight( 0x1B55, 5).
digit_weight( 0x1BB5, 5).
digit_weight( 0x1C45, 5).
digit_weight( 0x1C55, 5).
digit_weight( 0x2075, 5).
digit_weight( 0x2085, 5).
digit_weight( 0x2164, 5).
digit_weight( 0x2174, 5).
digit_weight( 0x2464, 5).
digit_weight( 0x2478, 5).
digit_weight( 0x248C, 5).
digit_weight( 0x24F9, 5).
digit_weight( 0x277A, 5).
digit_weight( 0x2784, 5).
digit_weight( 0x278E, 5).
digit_weight( 0x3025, 5).
digit_weight( 0x3224, 5).
digit_weight( 0x3284, 5).
digit_weight( 0x3405, 5).
digit_weight( 0x382A, 5).
digit_weight( 0x4E94, 5).
digit_weight( 0x4F0D, 5).
digit_weight( 0xA625, 5).
digit_weight( 0xA6EA, 5).
digit_weight( 0xA8D5, 5).
digit_weight( 0xA905, 5).
digit_weight( 0xA9D5, 5).
digit_weight( 0xA9F5, 5).
digit_weight( 0xAA55, 5).
digit_weight( 0xABF5, 5).
digit_weight( 0xFF15, 5).
digit_weight( 0x1010B, 5).
digit_weight( 0x10143, 5).
digit_weight( 0x10148, 5).
digit_weight( 0x1014F, 5).
digit_weight( 0x1015F, 5).
digit_weight( 0x10173, 5).
digit_weight( 0x102E5, 5).
digit_weight( 0x10321, 5).
digit_weight( 0x104A5, 5).
digit_weight( 0x1087D, 5).
digit_weight( 0x108AC, 5).
digit_weight( 0x108FC, 5).
digit_weight( 0x109C4, 5).
digit_weight( 0x10AEC, 5).
digit_weight( 0x10CFB, 5).
digit_weight( 0x10E64, 5).
digit_weight( 0x11056, 5).
digit_weight( 0x1106B, 5).
digit_weight( 0x110F5, 5).
digit_weight( 0x1113B, 5).
digit_weight( 0x111D5, 5).
digit_weight( 0x111E5, 5).
digit_weight( 0x112F5, 5).
digit_weight( 0x114D5, 5).
digit_weight( 0x11655, 5).
digit_weight( 0x116C5, 5).
digit_weight( 0x11735, 5).
digit_weight( 0x118E5, 5).
digit_weight( 0x12403, 5).
digit_weight( 0x1240A, 5).
digit_weight( 0x12410, 5).
digit_weight( 0x12419, 5).
digit_weight( 0x12422, 5).
digit_weight( 0x12427, 5).
digit_weight( 0x12431, 5).
digit_weight( 0x12439, 5).
digit_weight( 0x1244D, 5).
digit_weight( 0x12454, 0x12455, 5).
digit_weight( 0x1246A, 5).
digit_weight( 0x16A65, 5).
digit_weight( 0x16B55, 5).
digit_weight( 0x1D364, 5).
digit_weight( 0x1D7D3, 5).
digit_weight( 0x1D7DD, 5).
digit_weight( 0x1D7E7, 5).
digit_weight( 0x1D7F1, 5).
digit_weight( 0x1D7FB, 5).
digit_weight( 0x1E8CB, 5).
digit_weight( 0x1F106, 5).
digit_weight( 0x20121, 5).
digit_weight( 0x0F2F, 11/2).
digit_weight( 0x0036, 6).
digit_weight( 0x0666, 6).
digit_weight( 0x06F6, 6).
digit_weight( 0x07C6, 6).
digit_weight( 0x096C, 6).
digit_weight( 0x09EC, 6).
digit_weight( 0x0A6C, 6).
digit_weight( 0x0AEC, 6).
digit_weight( 0x0B6C, 6).
digit_weight( 0x0BEC, 6).
digit_weight( 0x0C6C, 6).
digit_weight( 0x0CEC, 6).
digit_weight( 0x0D6C, 6).
digit_weight( 0x0DEC, 6).
digit_weight( 0x0E56, 6).
digit_weight( 0x0ED6, 6).
digit_weight( 0x0F26, 6).
digit_weight( 0x1046, 6).
digit_weight( 0x1096, 6).
digit_weight( 0x136E, 6).
digit_weight( 0x17E6, 6).
digit_weight( 0x17F6, 6).
digit_weight( 0x1816, 6).
digit_weight( 0x194C, 6).
digit_weight( 0x19D6, 6).
digit_weight( 0x1A86, 6).
digit_weight( 0x1A96, 6).
digit_weight( 0x1B56, 6).
digit_weight( 0x1BB6, 6).
digit_weight( 0x1C46, 6).
digit_weight( 0x1C56, 6).
digit_weight( 0x2076, 6).
digit_weight( 0x2086, 6).
digit_weight( 0x2165, 6).
digit_weight( 0x2175, 6).
digit_weight( 0x2185, 6).
digit_weight( 0x2465, 6).
digit_weight( 0x2479, 6).
digit_weight( 0x248D, 6).
digit_weight( 0x24FA, 6).
digit_weight( 0x277B, 6).
digit_weight( 0x2785, 6).
digit_weight( 0x278F, 6).
digit_weight( 0x3026, 6).
digit_weight( 0x3225, 6).
digit_weight( 0x3285, 6).
digit_weight( 0x516D, 6).
digit_weight( 0x9646, 6).
digit_weight( 0x9678, 6).
digit_weight( 0xA626, 6).
digit_weight( 0xA6EB, 6).
digit_weight( 0xA8D6, 6).
digit_weight( 0xA906, 6).
digit_weight( 0xA9D6, 6).
digit_weight( 0xA9F6, 6).
digit_weight( 0xAA56, 6).
digit_weight( 0xABF6, 6).
digit_weight( 0xF9D1, 6).
digit_weight( 0xF9D3, 6).
digit_weight( 0xFF16, 6).
digit_weight( 0x1010C, 6).
digit_weight( 0x102E6, 6).
digit_weight( 0x104A6, 6).
digit_weight( 0x109C5, 6).
digit_weight( 0x10E65, 6).
digit_weight( 0x11057, 6).
digit_weight( 0x1106C, 6).
digit_weight( 0x110F6, 6).
digit_weight( 0x1113C, 6).
digit_weight( 0x111D6, 6).
digit_weight( 0x111E6, 6).
digit_weight( 0x112F6, 6).
digit_weight( 0x114D6, 6).
digit_weight( 0x11656, 6).
digit_weight( 0x116C6, 6).
digit_weight( 0x11736, 6).
digit_weight( 0x118E6, 6).
digit_weight( 0x12404, 6).
digit_weight( 0x1240B, 6).
digit_weight( 0x12411, 6).
digit_weight( 0x1241A, 6).
digit_weight( 0x12428, 6).
digit_weight( 0x12440, 6).
digit_weight( 0x1244E, 6).
digit_weight( 0x1246B, 6).
digit_weight( 0x16A66, 6).
digit_weight( 0x16B56, 6).
digit_weight( 0x1D365, 6).
digit_weight( 0x1D7D4, 6).
digit_weight( 0x1D7DE, 6).
digit_weight( 0x1D7E8, 6).
digit_weight( 0x1D7F2, 6).
digit_weight( 0x1D7FC, 6).
digit_weight( 0x1E8CC, 6).
digit_weight( 0x1F107, 6).
digit_weight( 0x20AEA, 6).
digit_weight( 0x0F30, 13/2).
digit_weight( 0x0037, 7).
digit_weight( 0x0667, 7).
digit_weight( 0x06F7, 7).
digit_weight( 0x07C7, 7).
digit_weight( 0x096D, 7).
digit_weight( 0x09ED, 7).
digit_weight( 0x0A6D, 7).
digit_weight( 0x0AED, 7).
digit_weight( 0x0B6D, 7).
digit_weight( 0x0BED, 7).
digit_weight( 0x0C6D, 7).
digit_weight( 0x0CED, 7).
digit_weight( 0x0D6D, 7).
digit_weight( 0x0DED, 7).
digit_weight( 0x0E57, 7).
digit_weight( 0x0ED7, 7).
digit_weight( 0x0F27, 7).
digit_weight( 0x1047, 7).
digit_weight( 0x1097, 7).
digit_weight( 0x136F, 7).
digit_weight( 0x17E7, 7).
digit_weight( 0x17F7, 7).
digit_weight( 0x1817, 7).
digit_weight( 0x194D, 7).
digit_weight( 0x19D7, 7).
digit_weight( 0x1A87, 7).
digit_weight( 0x1A97, 7).
digit_weight( 0x1B57, 7).
digit_weight( 0x1BB7, 7).
digit_weight( 0x1C47, 7).
digit_weight( 0x1C57, 7).
digit_weight( 0x2077, 7).
digit_weight( 0x2087, 7).
digit_weight( 0x2166, 7).
digit_weight( 0x2176, 7).
digit_weight( 0x2466, 7).
digit_weight( 0x247A, 7).
digit_weight( 0x248E, 7).
digit_weight( 0x24FB, 7).
digit_weight( 0x277C, 7).
digit_weight( 0x2786, 7).
digit_weight( 0x2790, 7).
digit_weight( 0x3027, 7).
digit_weight( 0x3226, 7).
digit_weight( 0x3286, 7).
digit_weight( 0x3B4D, 7).
digit_weight( 0x4E03, 7).
digit_weight( 0x67D2, 7).
digit_weight( 0x6F06, 7).
digit_weight( 0xA627, 7).
digit_weight( 0xA6EC, 7).
digit_weight( 0xA8D7, 7).
digit_weight( 0xA907, 7).
digit_weight( 0xA9D7, 7).
digit_weight( 0xA9F7, 7).
digit_weight( 0xAA57, 7).
digit_weight( 0xABF7, 7).
digit_weight( 0xFF17, 7).
digit_weight( 0x1010D, 7).
digit_weight( 0x102E7, 7).
digit_weight( 0x104A7, 7).
digit_weight( 0x109C6, 7).
digit_weight( 0x10E66, 7).
digit_weight( 0x11058, 7).
digit_weight( 0x1106D, 7).
digit_weight( 0x110F7, 7).
digit_weight( 0x1113D, 7).
digit_weight( 0x111D7, 7).
digit_weight( 0x111E7, 7).
digit_weight( 0x112F7, 7).
digit_weight( 0x114D7, 7).
digit_weight( 0x11657, 7).
digit_weight( 0x116C7, 7).
digit_weight( 0x11737, 7).
digit_weight( 0x118E7, 7).
digit_weight( 0x12405, 7).
digit_weight( 0x1240C, 7).
digit_weight( 0x12412, 7).
digit_weight( 0x1241B, 7).
digit_weight( 0x12429, 7).
digit_weight( 0x12441, 0x12443, 7).
digit_weight( 0x1246C, 7).
digit_weight( 0x16A67, 7).
digit_weight( 0x16B57, 7).
digit_weight( 0x1D366, 7).
digit_weight( 0x1D7D5, 7).
digit_weight( 0x1D7DF, 7).
digit_weight( 0x1D7E9, 7).
digit_weight( 0x1D7F3, 7).
digit_weight( 0x1D7FD, 7).
digit_weight( 0x1E8CD, 7).
digit_weight( 0x1F108, 7).
digit_weight( 0x20001, 7).
digit_weight( 0x0F31, 15/2).
digit_weight( 0x0038, 8).
digit_weight( 0x0668, 8).
digit_weight( 0x06F8, 8).
digit_weight( 0x07C8, 8).
digit_weight( 0x096E, 8).
digit_weight( 0x09EE, 8).
digit_weight( 0x0A6E, 8).
digit_weight( 0x0AEE, 8).
digit_weight( 0x0B6E, 8).
digit_weight( 0x0BEE, 8).
digit_weight( 0x0C6E, 8).
digit_weight( 0x0CEE, 8).
digit_weight( 0x0D6E, 8).
digit_weight( 0x0DEE, 8).
digit_weight( 0x0E58, 8).
digit_weight( 0x0ED8, 8).
digit_weight( 0x0F28, 8).
digit_weight( 0x1048, 8).
digit_weight( 0x1098, 8).
digit_weight( 0x1370, 8).
digit_weight( 0x17E8, 8).
digit_weight( 0x17F8, 8).
digit_weight( 0x1818, 8).
digit_weight( 0x194E, 8).
digit_weight( 0x19D8, 8).
digit_weight( 0x1A88, 8).
digit_weight( 0x1A98, 8).
digit_weight( 0x1B58, 8).
digit_weight( 0x1BB8, 8).
digit_weight( 0x1C48, 8).
digit_weight( 0x1C58, 8).
digit_weight( 0x2078, 8).
digit_weight( 0x2088, 8).
digit_weight( 0x2167, 8).
digit_weight( 0x2177, 8).
digit_weight( 0x2467, 8).
digit_weight( 0x247B, 8).
digit_weight( 0x248F, 8).
digit_weight( 0x24FC, 8).
digit_weight( 0x277D, 8).
digit_weight( 0x2787, 8).
digit_weight( 0x2791, 8).
digit_weight( 0x3028, 8).
digit_weight( 0x3227, 8).
digit_weight( 0x3287, 8).
digit_weight( 0x516B, 8).
digit_weight( 0x634C, 8).
digit_weight( 0xA628, 8).
digit_weight( 0xA6ED, 8).
digit_weight( 0xA8D8, 8).
digit_weight( 0xA908, 8).
digit_weight( 0xA9D8, 8).
digit_weight( 0xA9F8, 8).
digit_weight( 0xAA58, 8).
digit_weight( 0xABF8, 8).
digit_weight( 0xFF18, 8).
digit_weight( 0x1010E, 8).
digit_weight( 0x102E8, 8).
digit_weight( 0x104A8, 8).
digit_weight( 0x109C7, 8).
digit_weight( 0x10E67, 8).
digit_weight( 0x11059, 8).
digit_weight( 0x1106E, 8).
digit_weight( 0x110F8, 8).
digit_weight( 0x1113E, 8).
digit_weight( 0x111D8, 8).
digit_weight( 0x111E8, 8).
digit_weight( 0x112F8, 8).
digit_weight( 0x114D8, 8).
digit_weight( 0x11658, 8).
digit_weight( 0x116C8, 8).
digit_weight( 0x11738, 8).
digit_weight( 0x118E8, 8).
digit_weight( 0x12406, 8).
digit_weight( 0x1240D, 8).
digit_weight( 0x12413, 8).
digit_weight( 0x1241C, 8).
digit_weight( 0x1242A, 8).
digit_weight( 0x12444, 0x12445, 8).
digit_weight( 0x1246D, 8).
digit_weight( 0x16A68, 8).
digit_weight( 0x16B58, 8).
digit_weight( 0x1D367, 8).
digit_weight( 0x1D7D6, 8).
digit_weight( 0x1D7E0, 8).
digit_weight( 0x1D7EA, 8).
digit_weight( 0x1D7F4, 8).
digit_weight( 0x1D7FE, 8).
digit_weight( 0x1E8CE, 8).
digit_weight( 0x1F109, 8).
digit_weight( 0x0F32, 17/2).
digit_weight( 0x0039, 9).
digit_weight( 0x0669, 9).
digit_weight( 0x06F9, 9).
digit_weight( 0x07C9, 9).
digit_weight( 0x096F, 9).
digit_weight( 0x09EF, 9).
digit_weight( 0x0A6F, 9).
digit_weight( 0x0AEF, 9).
digit_weight( 0x0B6F, 9).
digit_weight( 0x0BEF, 9).
digit_weight( 0x0C6F, 9).
digit_weight( 0x0CEF, 9).
digit_weight( 0x0D6F, 9).
digit_weight( 0x0DEF, 9).
digit_weight( 0x0E59, 9).
digit_weight( 0x0ED9, 9).
digit_weight( 0x0F29, 9).
digit_weight( 0x1049, 9).
digit_weight( 0x1099, 9).
digit_weight( 0x1371, 9).
digit_weight( 0x17E9, 9).
digit_weight( 0x17F9, 9).
digit_weight( 0x1819, 9).
digit_weight( 0x194F, 9).
digit_weight( 0x19D9, 9).
digit_weight( 0x1A89, 9).
digit_weight( 0x1A99, 9).
digit_weight( 0x1B59, 9).
digit_weight( 0x1BB9, 9).
digit_weight( 0x1C49, 9).
digit_weight( 0x1C59, 9).
digit_weight( 0x2079, 9).
digit_weight( 0x2089, 9).
digit_weight( 0x2168, 9).
digit_weight( 0x2178, 9).
digit_weight( 0x2468, 9).
digit_weight( 0x247C, 9).
digit_weight( 0x2490, 9).
digit_weight( 0x24FD, 9).
digit_weight( 0x277E, 9).
digit_weight( 0x2788, 9).
digit_weight( 0x2792, 9).
digit_weight( 0x3029, 9).
digit_weight( 0x3228, 9).
digit_weight( 0x3288, 9).
digit_weight( 0x4E5D, 9).
digit_weight( 0x5EFE, 9).
digit_weight( 0x7396, 9).
digit_weight( 0xA629, 9).
digit_weight( 0xA6EE, 9).
digit_weight( 0xA8D9, 9).
digit_weight( 0xA909, 9).
digit_weight( 0xA9D9, 9).
digit_weight( 0xA9F9, 9).
digit_weight( 0xAA59, 9).
digit_weight( 0xABF9, 9).
digit_weight( 0xFF19, 9).
digit_weight( 0x1010F, 9).
digit_weight( 0x102E9, 9).
digit_weight( 0x104A9, 9).
digit_weight( 0x109C8, 9).
digit_weight( 0x10E68, 9).
digit_weight( 0x1105A, 9).
digit_weight( 0x1106F, 9).
digit_weight( 0x110F9, 9).
digit_weight( 0x1113F, 9).
digit_weight( 0x111D9, 9).
digit_weight( 0x111E9, 9).
digit_weight( 0x112F9, 9).
digit_weight( 0x114D9, 9).
digit_weight( 0x11659, 9).
digit_weight( 0x116C9, 9).
digit_weight( 0x11739, 9).
digit_weight( 0x118E9, 9).
digit_weight( 0x12407, 9).
digit_weight( 0x1240E, 9).
digit_weight( 0x12414, 9).
digit_weight( 0x1241D, 9).
digit_weight( 0x1242B, 9).
digit_weight( 0x12446, 0x12449, 9).
digit_weight( 0x1246E, 9).
digit_weight( 0x16A69, 9).
digit_weight( 0x16B59, 9).
digit_weight( 0x1D368, 9).
digit_weight( 0x1D7D7, 9).
digit_weight( 0x1D7E1, 9).
digit_weight( 0x1D7EB, 9).
digit_weight( 0x1D7F5, 9).
digit_weight( 0x1D7FF, 9).
digit_weight( 0x1E8CF, 9).
digit_weight( 0x1F10A, 9).
digit_weight( 0x2F890, 9).
digit_weight( 0x0BF0, 10).
digit_weight( 0x0D70, 10).
digit_weight( 0x1372, 10).
digit_weight( 0x2169, 10).
digit_weight( 0x2179, 10).
digit_weight( 0x2469, 10).
digit_weight( 0x247D, 10).
digit_weight( 0x2491, 10).
digit_weight( 0x24FE, 10).
digit_weight( 0x277F, 10).
digit_weight( 0x2789, 10).
digit_weight( 0x2793, 10).
digit_weight( 0x3038, 10).
digit_weight( 0x3229, 10).
digit_weight( 0x3248, 10).
digit_weight( 0x3289, 10).
digit_weight( 0x4EC0, 10).
digit_weight( 0x5341, 10).
digit_weight( 0x62FE, 10).
digit_weight( 0xF973, 10).
digit_weight( 0xF9FD, 10).
digit_weight( 0x10110, 10).
digit_weight( 0x10149, 10).
digit_weight( 0x10150, 10).
digit_weight( 0x10157, 10).
digit_weight( 0x10160, 0x10164, 10).
digit_weight( 0x102EA, 10).
digit_weight( 0x10322, 10).
digit_weight( 0x103D3, 10).
digit_weight( 0x1085B, 10).
digit_weight( 0x1087E, 10).
digit_weight( 0x108AD, 10).
digit_weight( 0x108FD, 10).
digit_weight( 0x10917, 10).
digit_weight( 0x109C9, 10).
digit_weight( 0x10A44, 10).
digit_weight( 0x10A9E, 10).
digit_weight( 0x10AED, 10).
digit_weight( 0x10B5C, 10).
digit_weight( 0x10B7C, 10).
digit_weight( 0x10BAD, 10).
digit_weight( 0x10CFC, 10).
digit_weight( 0x10E69, 10).
digit_weight( 0x1105B, 10).
digit_weight( 0x111EA, 10).
digit_weight( 0x1173A, 10).
digit_weight( 0x118EA, 10).
digit_weight( 0x16B5B, 10).
digit_weight( 0x1D369, 10).
digit_weight( 0x216A, 11).
digit_weight( 0x217A, 11).
digit_weight( 0x246A, 11).
digit_weight( 0x247E, 11).
digit_weight( 0x2492, 11).
digit_weight( 0x24EB, 11).
digit_weight( 0x216B, 12).
digit_weight( 0x217B, 12).
digit_weight( 0x246B, 12).
digit_weight( 0x247F, 12).
digit_weight( 0x2493, 12).
digit_weight( 0x24EC, 12).
digit_weight( 0x246C, 13).
digit_weight( 0x2480, 13).
digit_weight( 0x2494, 13).
digit_weight( 0x24ED, 13).
digit_weight( 0x246D, 14).
digit_weight( 0x2481, 14).
digit_weight( 0x2495, 14).
digit_weight( 0x24EE, 14).
digit_weight( 0x246E, 15).
digit_weight( 0x2482, 15).
digit_weight( 0x2496, 15).
digit_weight( 0x24EF, 15).
digit_weight( 0x09F9, 16).
digit_weight( 0x246F, 16).
digit_weight( 0x2483, 16).
digit_weight( 0x2497, 16).
digit_weight( 0x24F0, 16).
digit_weight( 0x16EE, 17).
digit_weight( 0x2470, 17).
digit_weight( 0x2484, 17).
digit_weight( 0x2498, 17).
digit_weight( 0x24F1, 17).
digit_weight( 0x16EF, 18).
digit_weight( 0x2471, 18).
digit_weight( 0x2485, 18).
digit_weight( 0x2499, 18).
digit_weight( 0x24F2, 18).
digit_weight( 0x16F0, 19).
digit_weight( 0x2472, 19).
digit_weight( 0x2486, 19).
digit_weight( 0x249A, 19).
digit_weight( 0x24F3, 19).
digit_weight( 0x1373, 20).
digit_weight( 0x2473, 20).
digit_weight( 0x2487, 20).
digit_weight( 0x249B, 20).
digit_weight( 0x24F4, 20).
digit_weight( 0x3039, 20).
digit_weight( 0x3249, 20).
digit_weight( 0x5344, 20).
digit_weight( 0x5EFF, 20).
digit_weight( 0x10111, 20).
digit_weight( 0x102EB, 20).
digit_weight( 0x103D4, 20).
digit_weight( 0x1085C, 20).
digit_weight( 0x1087F, 20).
digit_weight( 0x108AE, 20).
digit_weight( 0x108FE, 20).
digit_weight( 0x10918, 20).
digit_weight( 0x109CA, 20).
digit_weight( 0x10A45, 20).
digit_weight( 0x10A9F, 20).
digit_weight( 0x10AEE, 20).
digit_weight( 0x10B5D, 20).
digit_weight( 0x10B7D, 20).
digit_weight( 0x10BAE, 20).
digit_weight( 0x10E6A, 20).
digit_weight( 0x1105C, 20).
digit_weight( 0x111EB, 20).
digit_weight( 0x1173B, 20).
digit_weight( 0x118EB, 20).
digit_weight( 0x1D36A, 20).
digit_weight( 0x3251, 21).
digit_weight( 0x3252, 22).
digit_weight( 0x3253, 23).
digit_weight( 0x3254, 24).
digit_weight( 0x3255, 25).
digit_weight( 0x3256, 26).
digit_weight( 0x3257, 27).
digit_weight( 0x3258, 28).
digit_weight( 0x3259, 29).
digit_weight( 0x1374, 30).
digit_weight( 0x303A, 30).
digit_weight( 0x324A, 30).
digit_weight( 0x325A, 30).
digit_weight( 0x5345, 30).
digit_weight( 0x10112, 30).
digit_weight( 0x10165, 30).
digit_weight( 0x102EC, 30).
digit_weight( 0x109CB, 30).
digit_weight( 0x10E6B, 30).
digit_weight( 0x1105D, 30).
digit_weight( 0x111EC, 30).
digit_weight( 0x118EC, 30).
digit_weight( 0x1D36B, 30).
digit_weight( 0x20983, 30).
digit_weight( 0x325B, 31).
digit_weight( 0x325C, 32).
digit_weight( 0x325D, 33).
digit_weight( 0x325E, 34).
digit_weight( 0x325F, 35).
digit_weight( 0x32B1, 36).
digit_weight( 0x32B2, 37).
digit_weight( 0x32B3, 38).
digit_weight( 0x32B4, 39).
digit_weight( 0x1375, 40).
digit_weight( 0x324B, 40).
digit_weight( 0x32B5, 40).
digit_weight( 0x534C, 40).
digit_weight( 0x10113, 40).
digit_weight( 0x102ED, 40).
digit_weight( 0x109CC, 40).
digit_weight( 0x10E6C, 40).
digit_weight( 0x1105E, 40).
digit_weight( 0x111ED, 40).
digit_weight( 0x118ED, 40).
digit_weight( 0x12467, 40).
digit_weight( 0x1D36C, 40).
digit_weight( 0x2098C, 40).
digit_weight( 0x2099C, 40).
digit_weight( 0x32B6, 41).
digit_weight( 0x32B7, 42).
digit_weight( 0x32B8, 43).
digit_weight( 0x32B9, 44).
digit_weight( 0x32BA, 45).
digit_weight( 0x32BB, 46).
digit_weight( 0x32BC, 47).
digit_weight( 0x32BD, 48).
digit_weight( 0x32BE, 49).
digit_weight( 0x1376, 50).
digit_weight( 0x216C, 50).
digit_weight( 0x217C, 50).
digit_weight( 0x2186, 50).
digit_weight( 0x324C, 50).
digit_weight( 0x32BF, 50).
digit_weight( 0x10114, 50).
digit_weight( 0x10144, 50).
digit_weight( 0x1014A, 50).
digit_weight( 0x10151, 50).
digit_weight( 0x10166, 0x10169, 50).
digit_weight( 0x10174, 50).
digit_weight( 0x102EE, 50).
digit_weight( 0x10323, 50).
digit_weight( 0x109CD, 50).
digit_weight( 0x10A7E, 50).
digit_weight( 0x10CFD, 50).
digit_weight( 0x10E6D, 50).
digit_weight( 0x1105F, 50).
digit_weight( 0x111EE, 50).
digit_weight( 0x118EE, 50).
digit_weight( 0x12468, 50).
digit_weight( 0x1D36D, 50).
digit_weight( 0x1377, 60).
digit_weight( 0x324D, 60).
digit_weight( 0x10115, 60).
digit_weight( 0x102EF, 60).
digit_weight( 0x109CE, 60).
digit_weight( 0x10E6E, 60).
digit_weight( 0x11060, 60).
digit_weight( 0x111EF, 60).
digit_weight( 0x118EF, 60).
digit_weight( 0x1D36E, 60).
digit_weight( 0x1378, 70).
digit_weight( 0x324E, 70).
digit_weight( 0x10116, 70).
digit_weight( 0x102F0, 70).
digit_weight( 0x109CF, 70).
digit_weight( 0x10E6F, 70).
digit_weight( 0x11061, 70).
digit_weight( 0x111F0, 70).
digit_weight( 0x118F0, 70).
digit_weight( 0x1D36F, 70).
digit_weight( 0x1379, 80).
digit_weight( 0x324F, 80).
digit_weight( 0x10117, 80).
digit_weight( 0x102F1, 80).
digit_weight( 0x10E70, 80).
digit_weight( 0x11062, 80).
digit_weight( 0x111F1, 80).
digit_weight( 0x118F1, 80).
digit_weight( 0x1D370, 80).
digit_weight( 0x137A, 90).
digit_weight( 0x10118, 90).
digit_weight( 0x102F2, 90).
digit_weight( 0x10341, 90).
digit_weight( 0x10E71, 90).
digit_weight( 0x11063, 90).
digit_weight( 0x111F2, 90).
digit_weight( 0x118F2, 90).
digit_weight( 0x1D371, 90).
digit_weight( 0x0BF1, 100).
digit_weight( 0x0D71, 100).
digit_weight( 0x137B, 100).
digit_weight( 0x216D, 100).
digit_weight( 0x217D, 100).
digit_weight( 0x4F70, 100).
digit_weight( 0x767E, 100).
digit_weight( 0x964C, 100).
digit_weight( 0x10119, 100).
digit_weight( 0x1014B, 100).
digit_weight( 0x10152, 100).
digit_weight( 0x1016A, 100).
digit_weight( 0x102F3, 100).
digit_weight( 0x103D5, 100).
digit_weight( 0x1085D, 100).
digit_weight( 0x108AF, 100).
digit_weight( 0x108FF, 100).
digit_weight( 0x10919, 100).
digit_weight( 0x109D2, 100).
digit_weight( 0x10A46, 100).
digit_weight( 0x10AEF, 100).
digit_weight( 0x10B5E, 100).
digit_weight( 0x10B7E, 100).
digit_weight( 0x10BAF, 100).
digit_weight( 0x10CFE, 100).
digit_weight( 0x10E72, 100).
digit_weight( 0x11064, 100).
digit_weight( 0x111F3, 100).
digit_weight( 0x16B5C, 100).
digit_weight( 0x1011A, 200).
digit_weight( 0x102F4, 200).
digit_weight( 0x109D3, 200).
digit_weight( 0x10E73, 200).
digit_weight( 0x1011B, 300).
digit_weight( 0x1016B, 300).
digit_weight( 0x102F5, 300).
digit_weight( 0x109D4, 300).
digit_weight( 0x10E74, 300).
digit_weight( 0x1011C, 400).
digit_weight( 0x102F6, 400).
digit_weight( 0x109D5, 400).
digit_weight( 0x10E75, 400).
digit_weight( 0x216E, 500).
digit_weight( 0x217E, 500).
digit_weight( 0x1011D, 500).
digit_weight( 0x10145, 500).
digit_weight( 0x1014C, 500).
digit_weight( 0x10153, 500).
digit_weight( 0x1016C, 0x10170, 500).
digit_weight( 0x102F7, 500).
digit_weight( 0x109D6, 500).
digit_weight( 0x10E76, 500).
digit_weight( 0x1011E, 600).
digit_weight( 0x102F8, 600).
digit_weight( 0x109D7, 600).
digit_weight( 0x10E77, 600).
digit_weight( 0x1011F, 700).
digit_weight( 0x102F9, 700).
digit_weight( 0x109D8, 700).
digit_weight( 0x10E78, 700).
digit_weight( 0x10120, 800).
digit_weight( 0x102FA, 800).
digit_weight( 0x109D9, 800).
digit_weight( 0x10E79, 800).
digit_weight( 0x10121, 900).
digit_weight( 0x102FB, 900).
digit_weight( 0x1034A, 900).
digit_weight( 0x109DA, 900).
digit_weight( 0x10E7A, 900).
digit_weight( 0x0BF2, 1000).
digit_weight( 0x0D72, 1000).
digit_weight( 0x216F, 1000).
digit_weight( 0x217F, 0x2180, 1000).
digit_weight( 0x4EDF, 1000).
digit_weight( 0x5343, 1000).
digit_weight( 0x9621, 1000).
digit_weight( 0x10122, 1000).
digit_weight( 0x1014D, 1000).
digit_weight( 0x10154, 1000).
digit_weight( 0x10171, 1000).
digit_weight( 0x1085E, 1000).
digit_weight( 0x109DB, 1000).
digit_weight( 0x10A47, 1000).
digit_weight( 0x10B5F, 1000).
digit_weight( 0x10B7F, 1000).
digit_weight( 0x10CFF, 1000).
digit_weight( 0x11065, 1000).
digit_weight( 0x111F4, 1000).
digit_weight( 0x10123, 2000).
digit_weight( 0x109DC, 2000).
digit_weight( 0x10124, 3000).
digit_weight( 0x109DD, 3000).
digit_weight( 0x10125, 4000).
digit_weight( 0x109DE, 4000).
digit_weight( 0x2181, 5000).
digit_weight( 0x10126, 5000).
digit_weight( 0x10146, 5000).
digit_weight( 0x1014E, 5000).
digit_weight( 0x10172, 5000).
digit_weight( 0x109DF, 5000).
digit_weight( 0x10127, 6000).
digit_weight( 0x109E0, 6000).
digit_weight( 0x10128, 7000).
digit_weight( 0x109E1, 7000).
digit_weight( 0x10129, 8000).
digit_weight( 0x109E2, 8000).
digit_weight( 0x1012A, 9000).
digit_weight( 0x109E3, 9000).
digit_weight( 0x137C, 10000).
digit_weight( 0x2182, 10000).
digit_weight( 0x4E07, 10000).
digit_weight( 0x842C, 10000).
digit_weight( 0x1012B, 10000).
digit_weight( 0x10155, 10000).
digit_weight( 0x1085F, 10000).
digit_weight( 0x109E4, 10000).
digit_weight( 0x16B5D, 10000).
digit_weight( 0x1012C, 20000).
digit_weight( 0x109E5, 20000).
digit_weight( 0x1012D, 30000).
digit_weight( 0x109E6, 30000).
digit_weight( 0x1012E, 40000).
digit_weight( 0x109E7, 40000).
digit_weight( 0x2187, 50000).
digit_weight( 0x1012F, 50000).
digit_weight( 0x10147, 50000).
digit_weight( 0x10156, 50000).
digit_weight( 0x109E8, 50000).
digit_weight( 0x10130, 60000).
digit_weight( 0x109E9, 60000).
digit_weight( 0x10131, 70000).
digit_weight( 0x109EA, 70000).
digit_weight( 0x10132, 80000).
digit_weight( 0x109EB, 80000).
digit_weight( 0x10133, 90000).
digit_weight( 0x109EC, 90000).
digit_weight( 0x2188, 100000).
digit_weight( 0x109ED, 100000).
digit_weight( 0x109EE, 200000).
digit_weight( 0x12432, 216000).
digit_weight( 0x109EF, 300000).
digit_weight( 0x109F0, 400000).
digit_weight( 0x12433, 432000).
digit_weight( 0x109F1, 500000).
digit_weight( 0x109F2, 600000).
digit_weight( 0x109F3, 700000).
digit_weight( 0x109F4, 800000).
digit_weight( 0x109F5, 900000).
digit_weight( 0x16B5E, 1000000).
digit_weight( 0x4EBF, 100000000).
digit_weight( 0x5104, 100000000).
digit_weight( 0x16B5F, 100000000).
digit_weight( 0x16B60, 10000000000).
digit_weight( 0x5146, 1000000000000).
digit_weight( 0x16B61, 1000000000000).

/* brackets and matching characters, obtained from

http://www.unicode.org/Public/UCD/latest/ucd/BidiBrackets.txt

by running

 grep '[ \t]\#' BidiBrackets.txt | awk '{ print "brackets( 0x" $1 ", 0x"$2 ")." }' |sed 's/;//g'
*/

paren_paren( 0x0028, 0x0029).
paren_paren( 0x0029, 0x0028).
paren_paren( 0x005B, 0x005D).
paren_paren( 0x005D, 0x005B).
paren_paren( 0x007B, 0x007D).
paren_paren( 0x007D, 0x007B).
paren_paren( 0x0F3A, 0x0F3B).
paren_paren( 0x0F3B, 0x0F3A).
paren_paren( 0x0F3C, 0x0F3D).
paren_paren( 0x0F3D, 0x0F3C).
paren_paren( 0x169B, 0x169C).
paren_paren( 0x169C, 0x169B).
paren_paren( 0x2045, 0x2046).
paren_paren( 0x2046, 0x2045).
paren_paren( 0x207D, 0x207E).
paren_paren( 0x207E, 0x207D).
paren_paren( 0x208D, 0x208E).
paren_paren( 0x208E, 0x208D).
paren_paren( 0x2308, 0x2309).
paren_paren( 0x2309, 0x2308).
paren_paren( 0x230A, 0x230B).
paren_paren( 0x230B, 0x230A).
paren_paren( 0x2329, 0x232A).
paren_paren( 0x232A, 0x2329).
paren_paren( 0x2768, 0x2769).
paren_paren( 0x2769, 0x2768).
paren_paren( 0x276A, 0x276B).
paren_paren( 0x276B, 0x276A).
paren_paren( 0x276C, 0x276D).
paren_paren( 0x276D, 0x276C).
paren_paren( 0x276E, 0x276F).
paren_paren( 0x276F, 0x276E).
paren_paren( 0x2770, 0x2771).
paren_paren( 0x2771, 0x2770).
paren_paren( 0x2772, 0x2773).
paren_paren( 0x2773, 0x2772).
paren_paren( 0x2774, 0x2775).
paren_paren( 0x2775, 0x2774).
paren_paren( 0x27C5, 0x27C6).
paren_paren( 0x27C6, 0x27C5).
paren_paren( 0x27E6, 0x27E7).
paren_paren( 0x27E7, 0x27E6).
paren_paren( 0x27E8, 0x27E9).
paren_paren( 0x27E9, 0x27E8).
paren_paren( 0x27EA, 0x27EB).
paren_paren( 0x27EB, 0x27EA).
paren_paren( 0x27EC, 0x27ED).
paren_paren( 0x27ED, 0x27EC).
paren_paren( 0x27EE, 0x27EF).
paren_paren( 0x27EF, 0x27EE).
paren_paren( 0x2983, 0x2984).
paren_paren( 0x2984, 0x2983).
paren_paren( 0x2985, 0x2986).
paren_paren( 0x2986, 0x2985).
paren_paren( 0x2987, 0x2988).
paren_paren( 0x2988, 0x2987).
paren_paren( 0x2989, 0x298A).
paren_paren( 0x298A, 0x2989).
paren_paren( 0x298B, 0x298C).
paren_paren( 0x298C, 0x298B).
paren_paren( 0x298D, 0x2990).
paren_paren( 0x298E, 0x298F).
paren_paren( 0x298F, 0x298E).
paren_paren( 0x2990, 0x298D).
paren_paren( 0x2991, 0x2992).
paren_paren( 0x2992, 0x2991).
paren_paren( 0x2993, 0x2994).
paren_paren( 0x2994, 0x2993).
paren_paren( 0x2995, 0x2996).
paren_paren( 0x2996, 0x2995).
paren_paren( 0x2997, 0x2998).
paren_paren( 0x2998, 0x2997).
paren_paren( 0x29D8, 0x29D9).
paren_paren( 0x29D9, 0x29D8).
paren_paren( 0x29DA, 0x29DB).
paren_paren( 0x29DB, 0x29DA).
paren_paren( 0x29FC, 0x29FD).
paren_paren( 0x29FD, 0x29FC).
paren_paren( 0x2E22, 0x2E23).
paren_paren( 0x2E23, 0x2E22).
paren_paren( 0x2E24, 0x2E25).
paren_paren( 0x2E25, 0x2E24).
paren_paren( 0x2E26, 0x2E27).
paren_paren( 0x2E27, 0x2E26).
paren_paren( 0x2E28, 0x2E29).
paren_paren( 0x2E29, 0x2E28).
paren_paren( 0x3008, 0x3009).
paren_paren( 0x3009, 0x3008).
paren_paren( 0x300A, 0x300B).
paren_paren( 0x300B, 0x300A).
paren_paren( 0x300C, 0x300D).
paren_paren( 0x300D, 0x300C).
paren_paren( 0x300E, 0x300F).
paren_paren( 0x300F, 0x300E).
paren_paren( 0x3010, 0x3011).
paren_paren( 0x3011, 0x3010).
paren_paren( 0x3014, 0x3015).
paren_paren( 0x3015, 0x3014).
paren_paren( 0x3016, 0x3017).
paren_paren( 0x3017, 0x3016).
paren_paren( 0x3018, 0x3019).
paren_paren( 0x3019, 0x3018).
paren_paren( 0x301A, 0x301B).
paren_paren( 0x301B, 0x301A).
paren_paren( 0xFE59, 0xFE5A).
paren_paren( 0xFE5A, 0xFE59).
paren_paren( 0xFE5B, 0xFE5C).
paren_paren( 0xFE5C, 0xFE5B).
paren_paren( 0xFE5D, 0xFE5E).
paren_paren( 0xFE5E, 0xFE5D).
paren_paren( 0xFF08, 0xFF09).
paren_paren( 0xFF09, 0xFF08).
paren_paren( 0xFF3B, 0xFF3D).
paren_paren( 0xFF3D, 0xFF3B).
paren_paren( 0xFF5B, 0xFF5D).
paren_paren( 0xFF5D, 0xFF5B).
paren_paren( 0xFF5F, 0xFF60).
paren_paren( 0xFF60, 0xFF5F).
paren_paren( 0xFF62, 0xFF63).
paren_paren( 0xFF63, 0xFF62).

/** @} */
