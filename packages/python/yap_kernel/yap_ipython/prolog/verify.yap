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

all_clear( Self, _Cell, P, Q) :-
    no_errors( Self, P ),
    yap_flag(singleton_variables, Old, false),
    no_errors( Self, Q ),
    yap_flag(singleton_variables, _, Old).

no_errors( _Self, Text ) :-
    blank(Text).
no_errors( Self, Text ) :-
    setup_call_cleanup(
       	open_esh( Self, Text, Stream),
       	esh(Self, Stream),
       	close_esh( Self, Stream )
    ).

syntax(_Self, E) :- writeln(user_error, E), fail.
syntax(Self, error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
    Self.errors := [t(Cause,LN,CharPos,Details)] + Self.errors,
							!.
syntax(_Self, E) :- throw(E).

open_esh(Self, Text, Stream) :-
	Self.errors := [],
	     open_mem_read_stream( Text, Stream ).

esh(Self, Stream) :-
    repeat,
    catch(
	read_clause(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
	Error,
	syntax(Self, Error)
    ),
    Cl == end_of_file,
    !,
    V := Self.errors,
	      V == [].



close_esh( _Self, Stream ) :-
    close(Stream).

