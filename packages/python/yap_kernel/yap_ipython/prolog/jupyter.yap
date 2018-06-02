/**
  * @file jupyter.yap4py
  *
  * @brief JUpyter support.
  */

  
  % :- module( jupyter,
  %            [jupyter_query/3,
  %            errors/2,
  %            ready/2,
  %           completion/2,
  %         ]
%%            ).
:- use_module(library(hacks)).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).

:-	 use_module(library(python)).
:-	 use_module(library(yapi)).
:-	 use_module(library(complete)).

:- python_import(sys).

jupyter_query(Caller, Cell, Line ) :-
	jupyter_cell(Caller, Cell, Line).

jupyter_cell(_Caller, Cell, _) :-
	jupyter_consult(Cell),	%stack_dump,
	fail.
jupyter_cell( _Caller, _, Line ) :-
	blank( Line ),
	!.
jupyter_cell( _Caller, _, [] ) :- !.
jupyter_cell( Caller, _, Line ) :-
	Self := Caller.query,
		       python_query(Self,Line).

restreams(call) :-
    streams(true).
restreams(fail) :-
    streams(false).
restreams(answer).
restreams(exit) :-
    streams(false).
restreams(!).
restreams(external_exception(_)).
restreams(exception).

jupyter_consult(Text) :-
	blank( Text ),
	!.
jupyter_consult(Cell) :-
	open_mem_read_stream( Cell, Stream),
%	Name = 'Inp',
%	stream_property(Stream, file_name(Name) ),
	load_files(user:'jupyter cell',[stream(Stream)]), !.
	%should load_files  close?

blank(Text) :-
	atom_codes(Text, L),
	maplist( blankc, L).

blankc(' ').
blankc('\n').
blankc('\t').

:- dynamic cell_stream/1.

streams(false) :-
    nb_setval(jupyter_cell, false),
        retract(cell_stream(S)),
	close(S),
	fail.
streams(false).
streams(true) :-
    streams( false ),
    nb_setval(jupyter_cell, true),
%    \+ current_stream('/python/input',_,_),
    open('/python/input', read, Input, [alias(user_input),bom(false),script(false)]),
    assert( cell_stream( Input) ),
    set_prolog_flag(user_input,Input),
    fail.
streams(true) :-
%    \+ current_stream('/python/sys.stdout',_,_),
    open('/python/sys.stdout', append, Output, [alias(user_output)]),
    set_prolog_flag(user_output, Output),
    assert( cell_stream( Output) ),
    fail.
streams(true) :-
    %    \+ current_stream('/python/sys.stderr',_,_),
    open('/python/sys.stderr', append, Error, [alias(user_error)]),
    assert( cell_stream( Error) ),
    set_prolog_flag(user_error, Error),
    fail.
streams(true).

ready(_Self, Line ) :-
            blank( Line ),
            !.
ready(Self, Line ) :-
    errors( Self, Line ),
    \+ syntax_error(_,_).

errors( Self, Text ) :-
       	setup_call_cleanup(
       			   open_events( Self, Text, Stream),
       			   goals(Self, Stream),
       			   close_events( Self )
       	 	   ).

clauses(_Self, Stream) :-
    repeat,
    read_clause(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
%	command( Self, Cl ),
    Cl == end_of_file,
    !.

goals(_Self, Stream) :-
    repeat,
    read_term(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
%	command( Self, Cl ),
    Cl == end_of_file,
    !.

command(_, end_of_file) :- !.

command( _Self, ( :- op(Prio,Assoc,Name) ) ) :-
	addop(Prio,Assoc,Name).

command( _Self, ( :- module(Name, Exports) )) :-
	retract( active_module( M0 ) ),
	atom_concat( '__m0_', Name, M ),
	assert( active_module(M) ),
	assert( undo( active_module(M0) ) ),
	maplist( addop2(M), Exports).


addop(Prio,Assoc,Name) :-
	(
	current_op(OPrio, SimilarAssoc, Name),
	op(Prio, Assoc, Name),
	matched_op(Assoc, SimilarAssoc)
	->
		assertz( undo(op( OPrio, Assoc, Name ) ) )
		;
		assertz( undo(op( 0, Assoc, Name ) ) )
		).

addop2(M, op(Prio, Assoc, Name)) :-
	addop( Prio, Assoc, M:Name ).

matched_op(A, B) :-
	optype( A, T),
	optype( B, T).

optype(fx,pre).
optype(fy,pre).
optype(xfx,in).
optype(xfy,in).
optype(yfx,in).
optype(yfy,in).
optype(xf,pos).
optype(yf,pos).

:- dynamic user:portray_message/2.
:-  multifile user:portray_message/2.

:- dynamic syntax_error/4, undo/1.

user:portray_message(_Severity, error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
	nb_getval(jupyter_cell, on),
        assert( syntax_error(Cause,LN,CharPos,Details) ).
user:portray_message(_Severity, error(style_check(_),_) ) :-
	nb_getval(jupyter_cell, on).

open_events(Self, Text, Stream) :-
	Self.errors := [],
	nb_setval( jupyter, on),
    open_mem_read_stream( Text, Stream ).

:- initialization( nb_setval( jupyter, off ) ).

close_events( _Self ) :-
	nb_setval( jupyter, off ),
	retract( undo(G) ),
	call(G),
	fail.
close_events( Self ) :-
	retract( syntax_error( C, L, N, A )),
    Self.errors := [t(C,L,N,A)] + Self.errors,
    fail.
close_events( _ ).


:- if(  current_prolog_flag(apple, true) ).

:- putenv( 'LC_ALL', 'en_us:UTF-8').

plot_inline :-
	X := self.inline_plotting,
	nb_setval(inline, X ),
	X = true,
	!,
	:= (
	   import( matplotlib ),
	   matplotlib.use( `nbagg` )
	   ).

:- endif.

%:- ( start_low_level_trace ).
