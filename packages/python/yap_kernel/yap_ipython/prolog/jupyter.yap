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

jupyter_query(Caller, Prog, Query ) :-
	catch(
	   jupyter_cell(Caller, Prog, Query),
		 E,
		 '$Error'(E, top)
		 ).

jupyter_cell(_Caller, Cell, _Line) :-
	jupyter_consult(Cell),	%stack_dump,
	fail.
jupyter_cell( _Caller, _, '' ) :- !.
jupyter_cell( _Caller, _, Line ) :-
	blank( Line ),
	!.
jupyter_cell( Self, _, Line ) :-
				%Self := Caller.query,
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
  load_files(user:'jupyter cell',[stream(Stream)]).

	blank(Text) :-
		atom(Text),
		!,
		atom_codes(Text, L),
		maplist( code_type(space), L).
		blank(Text) :-
			string(Text),
			!,
			string_codes(Text, L),
			maplist( code_type(space), L).


	streams(false) :-
	nb_setval(jupyter_cell, false),
	close(user_input),
	close(user_output),
	close(user_error).
streams(true) :-
    nb_setval(jupyter_cell, true),
    open('/python/input', read, _Input, [alias(user_input),bom(false),script(false)]),
    open('/python/sys.stdout', append, _Output, [alias(user_output)]),
    open('/python/sys.stderr', append, _Error, [alias(user_error)]).

ready(Self, Cell, P, Q ) :-
	catch(
	all_clear(Self, Cell, P, Q)
 E,
	 system_error(error,E).

all_clear( Self, _Cell, P, Q) :-
		no_errors( Self, P ),
		yap_flag(singleton_variables, Old, false)
		no_errors( Self, Q ).

no_errors( _Self, Text ) :-
    blank(Text),
    no_errors( Self, Text ) :-
       	setup_call_cleanup(
       			   open_esh( Self, Text, Stream),
       			   esh(Self, Stream),
       			   close_esh( Self, Stream )
       	 	   ).

esh(Self, Stream) :-
    repeat,
    catch(
			read_clause(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
	    Error,
			syntax(Self, Error)
			),
    Cl == end_of_file,
    !.


		syntax(_Self, E) :- writeln(user_error, E), fail.
			syntax(Self, error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
				 Self.errors := [t(Cause,LN,CharPos,Details)] + Self.errors,
				 !.
			syntax(_Self, E) :- throw(E).

open_esh(Self, Text, Stream) :-
	Self.errors := [],
    open_mem_read_stream( Text, Stream ).

:- initialization( nb_setval( jupyter, off ) ).

close_esh( _Self, Stream ) :-
    close(Stream).

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
