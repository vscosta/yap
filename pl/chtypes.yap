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

:- system_module( '$_chtypes', [], []).

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
				  
