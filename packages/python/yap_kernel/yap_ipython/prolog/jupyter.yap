
/**
  * @file jupyter.yap
  *
  * @brief JUpyter support.
  */


%:- yap_flag(gc_trace,verbose).
 :- module( jupyter,
             [jupyter_query/3,
  	       jupyter_query/4,
	        op(100,fy,('$')),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	   op(50, yf, []),
	   op(50, yf, '()'),
	   op(100, xfy, '.'),
	       	   op(100, fy, '.'),
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





:- python_import(sys).

:- python_import(yap4py.yapi as yapi).
:- python_import(builtins as builtin_mod).

:- meta_predicate jupyter_query(+,:,:).


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


jupyter_query(Caller, MCell, MLine ) :-
    strip_module(MCell, M, Cell),
    strip_module(MLine, M1,Line),
    j_consult(M, Cell,Caller),
    j_call(Caller,M1,Line).

j_consult(M, Cell,Caller) :-
    Cell == ""
    ->
    true;
      blank(Cell)
      ->
	  true
      ;
        jupyter_consult(M:Cell,Caller).

j_call(Caller,M1,Line) :-
    Line == ""
    ->
    true;
      blank(Line)
      ->
	  true
      ;
      catch(
	  gated_call(
	      jupyter:restreams(call,Caller,[]),
	      python_query(M1:Line,_, Bindings),
	      EGate,
	      jupyter:restreams(EGate,Caller,Bindings)
	  ),
	  error(A,B),
	 system_error(A,B)
      ).


restreams(call,Caller,_Bindings) :-
    Caller.q.port := call,
	     Caller.q.answer := [],
		      nl(user_error),
    streams(true).
restreams(fail,Caller,_Bindings) :-
    Caller.q.port := fail,
	     Caller.q.answer := [],
		      nl(user_error),
    streams(false).
restreams(exit,Caller,Bindings) :-
    Caller.q.port := exit,
	     Caller.q.answer := Bindings,
		      nl(user_error),
    streams(false).
restreams(redo,Caller,Bindings) :-
    Caller.q.port :=  redo,
	     Caller.q.answer := Bindings,
    streams(true).
restreams(!, _,_).
restreams(external_exception(E),Caller,_Bindings) :-
    Caller.q.port := exception,
	     Caller.q.answer := external_exception(E),
    streams(false).
restreams(exception,Caller,_Bindings) :-
    Caller.q.port := exception,
	     Caller.q.answer := exception.

%:- meta_predicate

jupyter_consult(_:Text,_) :-
	blank( Text ),
	!.
jupyter_consult(M:Cell,Caller) :-
%	Name = 'Inp',
%	stream_property(Stream, file_name(Name) ),%	setup_call_cleanup(
    catch(
	  gated_call(
	    (open_mem_read_stream( Cell, Stream),
	    Options = [],
	      jupyter:restreams(call,Caller,[])),
	    load_files(M:Stream,[stream(Stream)| Options]	),
	      EGate,
	      jupyter:restreams(EGate,Caller,[])
	  ),
	error(A,B),
  system_error(A,B)
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
    open('/python/builtins_mod.input', read, Input, [alias(user_input),bom(false),script(false)]),
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
    set_stream(Input,alias(user_input)),
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
