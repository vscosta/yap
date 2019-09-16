/**
  * @file verify.yap
  *
  * @brief JUpyter support.
  */


 %%  :- module( verify,
%%              [errors/2,
%%               ready/2]
%%                       ).
:- use_module(library(hacks)).
%% :- use_module(library(jupyter)).


:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).

%% :-	 use_module(library(python)).
%% :-	 use_module(library(yapi)).

:- dynamic jupyter/1.
jupyter( []).

ready( Engine, Query) :-
     errors( Engine , Query ),
     Es := Engine.errors,
      Es \== [].



errors( _Engine , Text ) :-
    blank(Text),
    !.
errors( Engine , Text ) :-
%start_low_level_trace,
    setup_call_cleanup(
       	open_esh( Engine , Text, Stream, Name ),
       	 esh(Engine , Name, Stream),
       	close_esh( Engine , Stream )
    ),
    fail.
errors( _Engine , _Text ).

open_esh(Engine , Text, Stream, Name) :-
	   retractall(jupyter(_)),
	   assertz(jupyter(Engine)),
    b_setval( jupyter, Engine),
    Name := Engine.stream_name,
    open_mem_read_stream( Text, Stream ).

esh(Engine , _Name, Stream) :-
  repeat,
  catch(
      read_clause(Stream, Cl, [ syntax_errors(dec10)]),
      error(C,E),
      p3_message(C,Engine,E)
  ),
  Cl == end_of_file,
  !.


:- multifile user:portray_message/2.

user:portray_message(S,E) :-
jupyter(En),
			   En \= [],
			   python_clear_errors,
			   p3_message(S,En,E).

close_esh( _Engine , Stream ) :-
	   retractall(jupyter(_)),
	   assertz(jupyter([])),
	   close(Stream),
	   python_clear_errors.



p3_message( _Severity,  Engine, error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
    python_clear_errors,
    !,
    Engine.errors := [t(Cause,LN,CharPos,Details)]+Engine.errors .
p3_message(error, _Engine, _E) :-
     python_clear_errors,
     !.
p3_message(warning, _Engine, _E) :-
    !.
p3_message(error, Engine, E) :-
    Engine.errors := [E] + Engine.errors.
p3_message(warning, Engine, E) :-
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
