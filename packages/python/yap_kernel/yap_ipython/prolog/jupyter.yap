
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

:- set_prolog_flag(verbose_load, false).

:-	 use_module(library(lists)).
v:-	 use_module(library(maplist)).
:-	 use_module(library(real)).

:- use_module(library(hacks)).

:- reexport(library(complete)).
:- reexport(library(verify)).
:- reexport(library(yapi)).



:- python_import(sys).

:- python_import(yap4py.yapi as yapi).
:- python_import(builtins as builtin_mod).
:- python_import('IPython'.utils.capture).
:- meta_predicate jupyter_cell(:,:,+).

:- set_prolog_flag(verbose_load, true).

next_streams( _Caller, exit, _Bindings ) :-
    %    Caller.answer := Bindings,
    !.
next_streams( _Caller, answer, _Bindings ) :-
    %    Caller.answer := Bindings,
    !.
next_streams(_, redo, _ ) :-
    !.
next_streams( _, _, _ ).


user:jupyter_cell(MCell, MLine, Self ) :-
    Caller := Self.q,
    j_consult(MCell,Caller),
    j_call(MLine,Caller).
    %O := IO,outputs,
    %forall(O,(:= display(O))),

	  

j_consult(MCell,Caller) :-
    (
	Cell == ""
    ->
    true;
	Cell == ''
    ->
    true;
	blank(Cell)
	->
	    true
	;
	jupyter_consult(MCell,Caller)
    ).

j_call(Line,Caller) :-
    (
	blank(Line)
    ->
    true
    ;
    catch(
	query_through_jupyter(Line,Caller),
	Error,
	fail
    )
    ).

query_through_jupyter(Line,Caller) :-
    open('/python/sys.stdout', append, Output, [alias(user_output)]),
    open('/python/sys.stderr', append, Error, [alias(user_error)]),
    gated_call(
	atom_to_term(Line,G,Vs),
	call(user:G),
	P,
	jupyter:port(P,Caller,G,Vs,GVs,LGs)
    ).

port(exit,Self,G,Vs,GVs,LGs) :-
    attributes:delayed_goals(G, Vs, GVs, LGs),
    print_message(help, answer(Vs, GVs,LGs)),
    Self.port := exit,
    Self.answer := GVs,
	 Self.delays := LGs,
               close(user_output),
               close(user_error).
port(answer,Self,G,Vs,GVs,LGs) :-
    attributes:delayed_goals(G, Vs, GVs, LGs),
    print_message(help, answer(Vs, GVs,LGs)),
    Self.port := answer,
    Self.answer := GVs,
	 Self.delays := LGs,
	      close(user_output),
	      close(user_error).
port(_,Self,G,Vs,GVs,LGs) :-
    Self.port = fail,
    print_message(help,false),
               close(user_output),
               close(user_error).


jupyter_consult(MText,_) :-
    strip_module(MText,_,Text),
    blank( Text ),
    !.
jupyter_consult(M:Cell,Caller) :-
    Options = [],
    catch(
	(
	    open_mem_read_stream( Cell, Stream),
	    stream_property(Stream, file_name(Name) ),
	    load_files(Stream,[stream(Stream),module(M)| Options]
		      ),
	    _,fail).

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

