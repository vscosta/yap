/**
  * @file jupyter.yap
  *
  * @brief JUpyter support.
  */

%:- set_prolog_flag(gc_trace,verbose).

:- module( jupyter,
           [
 	       jupyter/2,
	       jupyter_query/2,
	       jupyter_consult/2,
	       op(100,fy,('$')),
	       op(950,fy,:=),
	       op(950,yfx,:=),
	       %	   op(950,fx,<-),
	       %	   op(950,yfx,<-),
	       op(50, yf, []),
	       op(50, yf, '()'),
	       op(100, xfy, '.'),
	       op(100, fy, '.')	  ,
	       streams/1
]).


:- set_prolog_flag(verbose_load,false).

:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
%:-	 use_module(library(real)).
:- use_module(library(hacks)).
:-	 reexport(library(yapi)).
:-	 reexport(library(python)).
:- reexport(library(completer)).
:- reexport(library(verify)).

:- python_import(sys).

:- python_import(yap4py.yapi as yapi).
:- python_import(builtins as builtin_mod).
:- python_import(yapkernel.yapk).
%:- python_import(yap_ipython.utils.capture).

:- ipython := yapkernel.yapk.get_ipython().

streams(_).

next_streams( _Caller, exit, _Bindings ) :-
    %    Caller := Bindings,
    !.
    next_streams( _Caller, answer, _Bindings ) :-
    %    Caller := Bindings,

    !.
next_streams(_, redo, _ ) :-
    !.
next_streams( _, _, _ ).


jupyter(Cell, Query ) :-
    self := Query,
    shell :=  self,
    current_source_module(_,user),
    demagify(Cell, NMCell, KindOfMagic),
    ( KindOfMagic == '%%' -> true
    ;
    atom_concat('#?',_,Cell)
    ->
     j_call(user:NMCell, Query)
    ;
    j_consult(user:NMCell, Query)
    ).
    %O := IO,outputs,
    %forall(O,(:= display(O))),

demagify( Text, '', '%%' ) :-
    atom_concat('%%',_, Text),
    !.
demagify( Text, Rest, '%') :-
    atom_concat(['%',_,'\n',Extra], Text),
    !,
    errors(Extra,Rest).
demagify( Text, Text, '').

j_consult(MCell, Self) :-
    (
	MCell == ""
    ->
    true;
	MCell == ''
    ->
	true;
	jupyter_consult(MCell, Self)
    ).
j_call(Cell,Caller) :-
   (
	Cell == ""
    ->
    true
   ;
	Cell == ''
    ->
    true;
   atom_concat(Trcell,'*',Cell)
   ->
       j_call(Trcell,Caller)
   ;
	jupyter_query(Cell,Caller)
    ).

jupyter_query( Line, Self ) :-
    user:top_query(Self,user:Line).
/*
    read_term_from_atomic(Line, G, [variable_names(Vs)]),
    (query_to_answer(user:G,Vs,Port, GVs, LGs)
    *->
	atom_string(Port,SPort),
	Self.q.port := SPort,
	   print_message(help, answer(Vs, GVs,LGs)),
    %( retract(pydisplay(Obj)) -> Self.display_in_callback := Obj ; true ),
	   flush_output
%    term_to_dict(GVs,LGs,Bindings,NGs),
%    Self.q:= {gate:SPort,bindings:Bindings,delays:NGs}
     %:= print("oo").
	   ;
	   Self.q.port := "fail" ,
	   fail	   
     ).
*/
/*
:-    open('/python/sys.stdout', append, Output, [alias(python_output)]),
    open('/python/sys.stderr', append, Error, [alias(python_error)]).
*/

/**
  * @pred jupyter_consult(Cell)
  *
  * how the YAP Jupyter kernels consults the text in cell.
  */

jupyter_consult(Cell, _Self) :-
    Cell='', !.
jupyter_consult(Cell, Self) :-
    jupyter_consult(Cell, Self, []).

:- dynamic j/1.

j(0).

jc(I) :-
    retract(j(I)),
    I1 is I+1,
    assert(j(I1)).
    
jupyter_consult(Cell, Self, Options) :-
   := print(Self),
self := Self,
    jc(I),
        writeln(I),  
    format(atom(Id),'cell ~d',[I]),
    writeln(Id),
    open(atom(Cell), read, Stream),
    load_files(Id,[stream(Stream),skip_unix_header(true),source_module(user),silent(false)| Options]).


