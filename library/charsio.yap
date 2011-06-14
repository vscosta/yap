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

format_to_chars(Format, Args, Codes) :-
	format(codes(Codes), Format, Args).

format_to_chars(Format, Args, OUT, L0) :-
	format(codes(OUT, L0), Format, Args).

write_to_chars(Term, Codes) :-
	format(codes(Codes), '~w', [Term]).

write_to_chars(Term, Out, Tail) :-
	format(codes(Out,Tail),'~w',[Term]).


atom_to_chars(Atom, OUT) :-
	atom_codes(Atom, OUT).

atom_to_chars(Atom, L0, OUT) :-
	format(codes(L0, OUT), '~a', [Atom]).

number_to_chars(Number, OUT) :-
	number_codes(Number, OUT).

number_to_chars(Number, L0, OUT) :-
	var(Number), !,
	throw(error(instantiation_error,number_to_chars(Number, L0, OUT))).
number_to_chars(Number, L0, OUT) :-
	number(Number), !,
	format(codes(L0, OUT), '~w', [Number]).
number_to_chars(Number, L0, OUT) :-
	throw(error(type_error(number,Number),number_to_chars(Number, L0, OUT))).

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

with_output_to_chars(Goal, Codes) :-
	with_output_to(codes(Codes), Goal).

with_output_to_chars(Goal, Codes, L0) :-
	with_output_to(codes(Codes, L0), Goal).
%%	with_output_to_chars(:Goal, -Stream, -Codes, ?Tail) is det.
%
%	As  with_output_to_chars/2,  but  Stream  is  unified  with  the
%	temporary stream.

with_output_to_chars(Goal, Stream, Codes, Tail) :-
	with_output_to(codes(Codes, Tail), with_stream(Stream, Goal)).

with_stream(Stream, Goal) :-
	current_output(Stream),
	call(Goal).

%%	read_from_chars(+Codes, -Term) is det.
%
%	Read Codes into Term.
%
%	@compat	The SWI-Prolog version does not require Codes to end
%		in a full-stop.

read_from_chars("", end_of_file) :- !.
read_from_chars(List, Term) :-
	atom_to_term(List, Term, _).

