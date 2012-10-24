%%% -*- Mode: Prolog; -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Author:        Vitor Santos Costa
%    E-mail:        vsc@dcc.fc.up.pt
%    Copyright (C): Universidade do Porto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of the YAP Python Interface
%  distributed according to Perl Artistic License
%  check LICENSE file for distribution license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/** <module> python

A C-based  Prolog interface to python.

@author		Vitor Santos Costa
@version	0:0:5, 2012/10/8
@license	Perl Artistic License

*/

%%%

:- module(python, [
     init_python/0,
     end_python/0,
     python_command/1,
     python_assign/3,
     python/2,
     op(100,fy,$),
     op(950,fy,:=),
     op(950,yfx,:=),
     (:=)/2,
     (:=)/1
     ]).


/************************************************************************************************************

Python interface

Data types are

     Python		Prolog
     string             atoms
     numbers		numbers
     lists		lists
     generic objs	__pointer__(Address)

$(var) refers to the attribute __main__.var

*************************************************************************************************************/

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).

:- dynamic python_mref_cache/2.

:= F :- python(F,_).

V := F :- var(V), !, python(F,V).
'$'(V) := F :- atom(V), !, python(F,F1), python_assign(V, F1).
A^Key := F :- python(F,F1), python_set_item(A, Key, F1).

python_do_import(Module, MRef) :-
	python_mref_cache(Module, MRef), !.
python_do_import(Module, MRef) :-
        python_import(Module, MRef),
	assert( python_mref_cache(Module, MRef) ).

python_eval_term(Object:len, OArg) :- !,
	python_len(Object, OArg).	
python_eval_term(Module:Object:len, OArg) :- !,
	python_do_import(Module, MRef),
	python_o(MRef, Function, ORef),
	python_len(ORef, OArg).	
python_eval_term(Object:dir, OArg) :- !,
	python_dir(Object, OArg).	
python_eval_term(Module:Object:dir, OArg) :- !,
	python_do_import(Module, MRef),
	python_o(MRef, Function, ORef),
	python_dir(ORef, OArg).	
python_eval_term(Module:Object:Field, OArg) :-
	atom(Module),
	atom(Object), !,
	python_do_import(Module, MRef),
	python_o(MRef, Function, ORef),
	python_access(ORef, Field, OArg).
python_eval_term(Module:Function, OArg) :-
	atom(Module), !,
	python_do_import(Module, MRef),
	functor(Function, F, _),
	python_f(MRef, F, FRef),
	python_apply(FRef, Function, OArg).
python_eval_term(Obj:Field, OArg) :-
	python_access(Obj, Field, OArg).

python(Obj, Out) :-
	 python_eval_term(Obj, Out), !.
python(Obj, OArg) :-
	python_do_is(Obj, Obj1),
	python_is(Obj1, OArg).

python_do_is(A+B, NA+NB) :- !,
	python_do_is(A, NA),
	python_do_is(B, NB).
python_do_is(A-B, NA-NB) :- !,
	python_do_is(A, NA),
	python_do_is(B, NB).
python_do_is(A*B, NA*NB) :- !,
	python_do_is(A, NA),
	python_do_is(B, NB).
python_do_is(A/B, NA/NB) :- !,
	python_do_is(A, NA),
	python_do_is(B, NB).
python_do_is(A, NA) :-
	python_eval_term(A, NA), !.
python_do_is(A, A).

python_command(Cmd) :-
       python_run_command(Cmd).

start_python :-
	use_foreign_library(foreign(python)),
	init_python,
	python_command('import sys').

add_cwd_to_python :-
	unix(getcwd(Dir)),
	atom_concat(['sys.path.append(\"',Dir,'\")'], Command),
	python_command(Command).
	% done	

python_assign(Name, Exp, '$'(Name)) :-
	python_assign(Name, Exp).

:- initialization(start_python, now).

:- initialization(add_cwd_to_python).

