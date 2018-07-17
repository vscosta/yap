/**
  * @file jupyter.yap4py
  *
  * @brief JUpyter support.
  */


  :- module( verify,
             [errors/2,
              ready/2]
                      ).
:- use_module(library(hacks)).
:- use_module(library(jupyter)).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).

:-	 use_module(library(python)).
:-	 use_module(library(yapi)).


ready( Engine, Query) :-
     errors( Engine , Cell ),
     Es := Engine.errors,
     Es == [].



errors( _Engine , Text ) :-
    blank(Text).
    !.
errors( Engine , Text ) :-
  b_setval(jupyter, Engine),
    setup_call_cleanup(
       	open_esh( Engine , Text, Stream, Name ),
       	 esh(Engine , Name, Stream),
       	close_esh( Engine , Stream )
    ),
    fail.
errors( _Engine , _Text ).

open_esh(Engine , Text, Stream, Name) :-
  	Engine.errors := [],
    b_setval( jupyter, Engine),
    Name := Engine.stream_name,
    open_mem_read_stream( Text, Stream ).

esh(Engine , Name, Stream) :-
  repeat,
  catch(
  read_clause(Stream, Cl,[]),
  E=error(C,E),
  p_message(C,E)
  ),
  Cl == end_of_file,
  !.


close_esh( _Engine , Stream ) :-
  close(Stream).


  p_message(Severity, Error) :-
      writeln((Severity->Error)),
      catch( b_getval(jupyter, Engine), _, fail ),
      p_message(Severity, Engine, Error).

p_message( _Severity,  Engine, error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
     %% 	nb_getval(jupyter_cell, on),
     %%         assert( syntax_error(Cause,LN,CharPos,Details) ).
     %% user:portray_message(_Severity, error(style_check(_),_) ) :-
     %% 	nb_getval(jupyter_cell, on).
    Engine.errors := [t(Cause,LN,CharPos,Details)] + Engine.errors,
      !.
      p_message(error, Engine, E) :-
        writeln(E),
        !.
        p_message(warning, Engine, E) :-
          writeln(E),
          !.
        p_message(error, Engine, E) :-
          Engine.errors := [E] + Engine.errors.
          p_message(warning, Engine, E) :-
              Engine.errors := [E] + Engine.errors.
    %% ready(_Self, Line ) :-
%%             blank( Line ),
%%             !.
%% ready(Self, Line ) :-
%%     errors( Self, Line ),
%%     \+ syntax_error(_,_).

%% errors( Self, Text ) :-
%%        	setup_call_cleanup(
%%        			   open_events( Self, Text, Stream),
%%        			   goals(Self, Stream),
%%        			   close_events( Self )
%%        	 	   ).

%% clauses(_Self, Stream) :-
%%     repeat,
%%     read_clause(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
%% %	command( Self, Cl ),
%%     Cl == end_of_file,
%%     !.

%% goals(_Self, Stream) :-
%%     repeat,
%%     read_term(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
%% %	command( Self, Cl ),
%%     Cl == end_of_file,
%%     !.

%% command(_, end_of_file) :- !.

%% command( _Self, ( :- op(Prio,Assoc,Name) ) ) :-
%% 	addop(Prio,Assoc,Name).

%% command( _Self, ( :- module(Name, Exports) )) :-
%% 	retract( active_module( M0 ) ),
%% 	atom_concat( '__m0_', Name, M ),
%% 	assert( active_module(M) ),
%% 	assert( undo( active_module(M0) ) ),
%% 	maplist( addop2(M), Exports).


%% addop(Prio,Assoc,Name) :-
%% 	(
%% 	current_op(OPrio, SimilarAssoc, Name),
%% 	op(Prio, Assoc, Name),
%% 	matched_op(Assoc, SimilarAssoc)
%% 	->
%% 		assertz( undo(op( OPrio, Assoc, Name ) ) )
%% 		;
%% 		assertz( undo(op( 0, Assoc, Name ) ) )
%% 		).

%% addop2(M, op(Prio, Assoc, Name)) :-
%% 	addop( Prio, Assoc, M:Name ).

%% matched_op(A, B) :-
%% 	optype( A, T),
%% 	optype( B, T).

%% optype(fx,pre).
%% optype(fy,pre).
%% optype(xfx,in).
%% optype(xfy,in).
%% optype(yfx,in).
%% optype(yfy,in).
%% optype(xf,pos).
%% optype(yf,pos).

%% :- dynamic syntax_error/4, undo/1.

%%
%% open_events(Self, Text, Stream) :-
%% 	Self.errors := [],
%% 	nb_setval( jupyter, on),
%%     open_mem_read_stream( Text, Stream ).

%% :- initialization( nb_setval( jupyter, off ) ).

%% close_events( _Self ) :-
%% 	nb_setval( jupyter, off ),
%% 	retract( undo(G) ),
%% 	call(G),
%% 	fail.
%% close_events( Self ) :-
%% 	retract( syntax_error( C, L, N, A )),
%%     Self.errors := [t(C,L,N,A)] + Self.errors,
%%     fail.
%% close_events( _ ).
