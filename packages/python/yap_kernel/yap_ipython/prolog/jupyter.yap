
/**
  * @file jupyter.yap
  *
  * @brief JUpyter support.
  */


%:- yap_flag(gc_trace,verbose).
:- module( jupyter,
             [
	        op(100,fy,('$')),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	   op(50, yf, []),
	   op(50, yf, '()'),
	   op(100, xfy, '.'),
	       	   op(100, fy, '.'),
		   jupyter_cell/3,
		   blank/1,
	        streams/1
            ]
            ).


:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
:-	 use_module(library(real)).

:- use_module(library(hacks)).

:- reexport(library(complete)).
:- reexport(library(verify)).
:- reexport(library(yapi)).



:- python_import(sys).

:- python_import(yap4py.yapi as yapi).
:- python_import(builtins as builtin_mod).

:- meta_predicate jupyter_cell(:,:,+).


next_streams( _Caller, exit, _Bindings ) :-
%    Caller.answer := Bindings,
    !.
next_streams( _Caller, answer, _Bindings ) :-
%    Caller.answer := Bindings,
    !.
next_streams(_, redo, _ ) :-
    !.
next_streams( _, _, _ ). % :-
   % streams(false).


jupyter_cell(MCell, MLine, Self ) :-
    Caller := Self,
    strip_module(MCell, M, Cell),
    strip_module(MLine, M1,Line),
    j_consult(M, Cell,Caller),
    j_call(M1:Line,Caller).

j_consult(M, Cell,Caller) :-
    Cell == ""
    ->
    true;
      blank(Cell)
      ->
	  true
      ;
      jupyter_consult(M:Cell,Caller).

j_call(Line,Caller) :-
	(
	  blank(Line)
	->
	  true
	;
		 python_query(Caller, Line)
				%		 Port,
%x		 restreams(Port,Caller)
%		)
    ).

restreams(call,_Caller) :-
    streams(true).
restreams(fail,_Caller) :-
    streams(false).
restreams(exit,_Caller) :-
    streams(false).
restreams(answer,_Caller) :-
    streams(false).
restreams(redo,_Caller) :-
    streams(true).
restreams(!, _).
restreams(external_exception(_E),_Caller) :-
    streams(false).
restreams(exception,_Caller).
%:- meta_predicate

jupyter_consult(_:Text,_) :-
	blank( Text ),
	!.
jupyter_consult(M:Cell,Caller) :-
	Name = 'Inp',
	stream_property(Stream, file_name(Name) ),%	setup_call_cleanup(
	catch(
	  gated_call(
		     (open_mem_read_stream( Cell, Stream),
	    Options = [],
	      jupyter:restreams(call,Caller)),
	    load_files(M:Stream,[stream(Stream)| Options]	),
		     EGate,
		     jupyter:restreams(EGate,Caller),
		     system_error(A,B)
		    )
	     ).

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


:- dynamic std_streams/3, python_streams/3.

:- stream_property(Input,alias(user_input)),
   stream_property(Output,alias(user_output)),
   stream_property(Error,alias(user_error)),
   assert( std_streams( Input, Output, Error) ).


:-
	Input = user_input,
%	open('/python/builtins.readline', read, Input, [alias(user_input),bom(false),script(false)]),
    open('/python/sys.stdout', append, Output, [alias(user_output)]),
    open('/python/sys.stderr', append, Error, [alias(user_error)]),
    assert( python_streams( Input, Output, Error) ).

streams(false) :-
    std_streams( Input, Output, Error),
    set_stream(Input,alias(user_input)),
   set_stream(Output,alias(user_output)),
   set_stream(Error,alias(user_error)).
streams( true) :-
    python_streams( Input, Output, Error),
%    set_stream(Input,alias(user_input)),
    set_stream(Output,alias(user_output)),
   set_stream(Error,alias(user_error)).


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

