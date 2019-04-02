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
* File:		charsio.yap						 *
* Last rev:	5/12/99							 *
* mods:									 *
* comments:	I/O on character strings				 *
*									 *
*************************************************************************/

/**
 * @file   charsio.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 01:17:33 2015
 * 
 * @brief  Several operations on text.
 * @{
 * 
*/
%%  @file charsio.yap
%%
%%
%%  @brief Input/Output to characters.

:- module(charsio, [
	format_to_chars/3,
	format_to_chars/4,
	write_to_chars/3,
	write_to_chars/2,
	atom_to_chars/3,
	atom_to_chars/2,
	number_to_chars/3,
	number_to_chars/2,
	read_from_chars/2,
	open_chars_stream/2,
	with_output_to_chars/2,
	with_output_to_chars/3,
	with_output_to_chars/4,
 term_to_atom/2 
    ]).

/** @defgroup charsio Operations on Sequences of Codes.
@ingroup library
@{

Term to sequence of codes conversion, mostly replaced by engine code.
You can use the following directive to load the files.


~~~~~~~
:- use_module(library(charsio)).
~~~~~~~

It includes the following predicates:
  - atom_to_chars/2
  - atom_to_chars/3
  - format_to_chars/3
  - format_to_chars/4
  - number_to_chars/2
  - number_to_chars/3
  - open_chars_stream/2
  - read_from_chars/2
  - term_to_atom/2
  - with_output_to_chars/2
  - with_output_to_chars/3
  - with_output_to_chars/4
  - write_to_chars/2
  - write_to_chars/3

*/

:- meta_predicate(with_output_to_chars(0,?)).
:- meta_predicate(with_output_to_chars(0,-,?)).
:- meta_predicate(with_output_to_chars(0,-,?,?)).

/** @pred format_to_chars(+ _Form_, + _Args_, - _Result_) 

Execute the built-in procedure format/2 with form  _Form_ and
arguments  _Args_ outputting the result to the string of character
codes  _Result_.
*/
format_to_chars(Format, Args, Codes) :-
	format(codes(Codes), Format, Args).

/** @pred format_to_chars(+ _Form_, + _Args_, - _Result_, - _Result0_)

Execute the built-in procedure format/2 with form  _Form_ and
arguments  _Args_ outputting the result to the difference list of
character codes  _Result-Result0_.

*/
format_to_chars(Format, Args, OUT, L0) :-
	format(codes(OUT, L0), Format, Args).

/** @pred write_to_chars(+ _Term_, - _Result_) 

Execute the built-in procedure write/1 with argument  _Term_
outputting the result to the string of character codes  _Result_. 
*/
write_to_chars(Term, Codes) :-
	format(codes(Codes), '~w', [Term]).

/** @pred write_to_chars(+ _Term_, - _Result0_, - _Result_)

Execute the built-in procedure write/1 with argument  _Term_
outputting the result to the difference list of character codes
 _Result-Result0_.
*/
write_to_chars(Term, Out, Tail) :-
	format(codes(Out,Tail),'~w',[Term]).

/** @pred atom_to_chars(+ _Atom_, - _Result_) 

Convert the atom  _Atom_ to the string of character codes
 _Result_.
*/
atom_to_chars(Atom, OUT) :-
	atom_codes(Atom, OUT).

/** @pred atom_to_chars(+ _Atom_, - _Result0_, - _Result_)

Convert the atom  _Atom_ to the difference list of character codes
 _Result-Result0_.
*/
atom_to_chars(Atom, L0, OUT) :-
	format(codes(L0, OUT), '~a', [Atom]).

/** @pred number_to_chars(+ _Number_, - _Result_) 

Convert the number  _Number_ to the string of character codes
 _Result_.
*/
number_to_chars(Number, OUT) :-
	number_codes(Number, OUT).

/** @pred number_to_chars(+ _Number_, - _Result0_, - _Result_)

Convert the atom  _Number_ to the difference list of character codes
 _Result-Result0_. 
*/
number_to_chars(Number, L0, OUT) :-
	var(Number), !,
	throw(error(instantiation_error,number_to_chars(Number, L0, OUT))).
number_to_chars(Number, L0, OUT) :-
	number(Number), !,
	format(codes(L0, OUT), '~w', [Number]).
number_to_chars(Number, L0, OUT) :-
	throw(error(type_error(number,Number),number_to_chars(Number, L0, OUT))).

/** @pred open_chars_stream(+ _Chars_, - _Stream_) 

Open the list of character codes  _Chars_ as a stream  _Stream_.
*/
open_chars_stream(Codes, Stream) :-
	open_chars_stream(Codes, Stream, '').

open_chars_stream(Codes, Stream, Postfix) :-
	predicate_property(memory_file:open_memory_file(_,_,_),_), !,
	memory_file:new_memory_file(MF),
	memory_file:open_memory_file(MF, write, Out),
	format(Out, '~s~w', [Codes, Postfix]),
	close(Out),
	memory_file:open_memory_file(MF, read, Stream,
			 [ free_on_close(true)
			 ]).
open_chars_stream(Codes, Stream, Postfix) :-
	ensure_loaded(library(memfile)),
	open_chars_stream(Codes, Stream, Postfix).

/** @pred with_output_to_chars(? _Goal_, - _Chars_) 

Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the list of character codes  _Chars_.
*/
with_output_to_chars(Goal, Codes) :-
	with_output_to(codes(Codes), Goal).

/** @pred with_output_to_chars(? _Goal_, ? _Chars0_, - _Chars_)

Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the difference list of character codes
 _Chars-Chars0_.
*/
with_output_to_chars(Goal, Codes, L0) :-
	with_output_to(codes(Codes, L0), Goal).
%%	with_output_to_chars(:Goal, -Stream, -Codes, ?Tail) is det.
%
%	As  with_output_to_chars/2,  but  Stream  is  unified  with  the
%	temporary stream.

/** @pred with_output_to_chars(? _Goal_, - _Stream_, ? _Chars0_, - _Chars_)


Execute goal  _Goal_ such that its standard output will be sent to a
memory buffer. After successful execution the contents of the memory
buffer will be converted to the difference list of character codes
 _Chars-Chars0_ and  _Stream_ receives the stream corresponding to
the memory buffer.

 */
with_output_to_chars(Goal, Stream, Codes, Tail) :-
	with_output_to(codes(Codes, Tail), with_stream(Stream, Goal)).

with_stream(Stream, Goal) :-
	current_output(Stream),
	call(Goal).

/** @pred read_from_chars( + Chars, - Term) 

Parse the list of character codes  _Chars_ and return the result in
the term  _Term_. The character codes to be read must terminate with
a dot character such that either (i) the dot character is followed by
blank characters; or (ii) the dot character is the last character in the
string.

@note	The SWI-Prolog version does not require Codes to end
		in a full-stop.
*/
read_from_chars("", end_of_file) :- !.
read_from_chars(List, Term) :-
	atom_to_term(List, Term, _).
/**
@}
*/

