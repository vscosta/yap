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
:-	 use_module(library(verify)).

:- python_import(sys).

jupyter_query(Caller, Prog, Query ) :-
	catch(
	   jupyter_cell(Caller, Prog, Query),
		 error(L,E),
		 system_error(L,E)
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
