
/**
  * @file jupyter.yap4py
  *
  * @brief JUpyter support.
  */

:- yap_flag(gc_trace,verbose).
/*
  :- module( jupyter,
              [jupyter_query/3,
               blank/1,
	       streams/1
           ]
            ).
*/
:- use_module(library(hacks)).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).

%% :-	 reexport(library(python)).
%% :-	 reexport(library(yapi)).
%% :-	 reexport(library(complete)).
%% :-	 reexport(library(verify)).


:- python_import(sys).

jupyter_query(Caller, Cell, Line ) :-
    jupyter_cell(Caller, Cell, Line).

jupyter_cell(_Caller, Cell, _Line) :-
	jupyter_consult(Cell),	%stack_dump,
	fail.
jupyter_cell( _Caller, _, ¨¨ ) :- !.
jupyter_cell( _Caller, _, Line ) :-
	blank( Line ),
	!.
jupyter_cell(Caller, _, Line ) :-
  Query = Caller,
    catch(
	python_query(Query,Line),
	E=error(A,B),
	 system_error(A,B)
    ).

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
%	Name = 'Inp',
%	stream_property(Stream, file_name(Name) ),
%	setup_call_cleanup(
    catch(
	(
	    Options = [],
	    open_mem_read_stream( Cell, Stream),
	    load_files(user:'jupyter cell',[stream(Stream)| Options])
	),
	E=error(A,B),
	(close(Stream), system_error(A,B))
    ),
    fail.
jupyter_consult(_Cell).

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
    close(user_input),
    close(user_output),
    close(user_error).
streams(true) :-
    open('/python/input', read, _Input, [alias(user_input),bom(false),script(false)]),
    open('/python/sys.stdout', append, _Output, [alias(user_output)]),
    open('/python/sys.stderr', append, _Error, [alias(user_error)]).


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

%y:- ( start_low_level_trace ).
