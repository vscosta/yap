/**
  * @file jupyter.yap4py
  *
  * @brief JUpyter support.
  */


  % :- module( verify,
  %           [all_clear/4,
  %            errors/2,
  %            ready/2,
s  %           completion/2,
  %         ]
%%            ).
:- use_module(library(hacks)).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).

:-	 use_module(library(python)).
:-	 use_module(library(yapi)).

:- python_import(sys).

p_errors( Errors, Cell) :-
  blank( Cell ),
  !.
p_errors( Errors, Cell) :-
    no_errors( Errors , Cell ).

no_errors( _Errors , Text ) :-
    blank(Text).
no_errors( Errors , Text ) :-
    setup_call_cleanup(
       	open_esh( Errors , Text, Stream),
       	esh(Errors , Stream),
       	close_esh( Errors , Stream )
    ).

syntax(_Errors , E) :- writeln(user_error, E), fail.
syntax(Errors , error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
    Errors.errors := [t(Cause,LN,CharPos,Details)] + Errors.errors,
							!.
syntax(_Errors , E) :- throw(E).

open_esh(_Errors , Text, Stream) :-
	     open_mem_read_stream( Text, Stream ).

esh(Errors , Stream) :-
    repeat,
  catch(
	read_clause(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
	Error,
	syntax(Errors , Error)
    ),
    Cl == end_of_file,
    !.



close_esh( _Errors , Stream ) :-
    close(Stream).
