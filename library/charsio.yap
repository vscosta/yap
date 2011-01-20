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

:- meta_predicate(with_output_to_chars(0,?)).
:- meta_predicate(with_output_to_chars(0,-,?)).
:- meta_predicate(with_output_to_chars(0,-,?,?)).

format_to_chars(Form, Args, OUT) :-
	format_to_chars(Form, Args, OUT, []).

format_to_chars(Form, Args, OUT, L0) :-
	open_mem_write_stream(Stream),
	format(Stream,Form,Args),
	peek_mem_write_stream(Stream, L0, O),
	close(Stream),
	O = OUT.

write_to_chars(Term, OUT) :-
	write_to_chars(Term, [], OUT).

atom_to_chars(Atom, OUT) :-
	atom_to_chars(Atom, [], OUT).

atom_to_chars(Atom, L0, OUT) :-
	var(Atom), !,
	throw(error(instantiation_error,atom_to_chars(Atom, L0, OUT))).
atom_to_chars(Atom, L0, OUT) :-
	atom(Atom), !,
	open_mem_write_stream(Stream),
	write(Stream, Atom),
	peek_mem_write_stream(Stream, L0, O),
	close(Stream),
	O = OUT.
atom_to_chars(Atom, L0, OUT) :-
	throw(error(type_error(atom,Atom),atom_to_chars(Atom, L0, OUT))).

number_to_chars(Number, OUT) :-
	number_to_chars(Number, [], OUT).

number_to_chars(Number, L0, OUT) :-
	var(Number), !,
	throw(error(instantiation_error,number_to_chars(Number, L0, OUT))).
number_to_chars(Number, L0, OUT) :-
	number(Number), !,
	open_mem_write_stream(Stream),
	write(Stream, Number),
	peek_mem_write_stream(Stream, L0, O),
	close(Stream),
	O = OUT.
number_to_chars(Number, L0, OUT) :-
	throw(error(type_error(number,Number),number_to_chars(Number, L0, OUT))).

open_chars_stream(Chars, Stream) :-
	open_mem_read_stream(Chars, Stream).

with_output_to_chars(Goal, Chars) :-
	with_output_to_chars(Goal, [], Chars).

with_output_to_chars(Goal, L0, Chars) :-
	with_output_to_chars(Goal, Stream, L0, Chars),
	close(Stream).

with_output_to_chars(Goal, Stream, L0, Chars) :-
	open_mem_write_stream(Stream),
	current_output(SO),
	set_output(Stream),
	do_output_to_chars(Goal, Stream, L0, Chars, SO).

do_output_to_chars(Goal, Stream, L0, Chars, SO) :-
	catch(Goal, Exception, handle_exception(Exception,Stream,SO)),
	!,
	set_output(SO),
	peek_mem_write_stream(Stream, L0, Chars).
do_output_to_chars(_Goal, Stream, _L0, _Chars, SO) :-
	set_output(SO),
	close(Stream),
	fail.

handle_exception(Exception, Stream, SO) :-
	close(Stream),
	current_output(SO),
	throw(Exception).




