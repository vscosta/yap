
% % % -*-Mode : Prolog; -*-
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

/**
 @file python.pl
 @brief  Python to C Interface
*/

/** @addtogroup Py4YAP  Prolog interface to python.
  @ingroup Python

@{
*/



:- module(python,
	  [
	   end_python/0,
	   python_run_file/1,
	   python_run_command/1,
	   python_run_script/2,
	   python_assign/2,
	   python_assign_indexed/3,
	   python_represents/2,
	   python_import/1,
	   array_to_python_list/4,
	   array_to_python_tuple/4,
	   array_to_python_view/5,
	   python/2,
	   python_threaded/0,
	   prolog_list_to_python_list/3,
	   python_clear_errors/0,
	   python_string_to/1,
	   op(100,fy,$),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	   op(50, yf, []),
	   op(50, yf, '()'),
	   op(100, xfy, '.'),
	   op(100, fy, '.'), 
	   (:=)/2,
        (:=)/1,
	%        (<-)/1,
	%        (<-)/2,
	'()'/1, '{}'/1
	  ]).


:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).
:- dynamic python_mref_cache/2, python_obj_cache/2.

:-	   op(100,fy,'$'),
	   op(950,fy,:=),
	   op(950,yfx,:=),
%	   op(950,fx,<-),
%	   op(950,yfx,<-),
	   op(50, yf, []),
	   op(50, yf, '()'),
	   op(100, xfy, '.'),
	   op(100, fy, '.').

:-  multifile (<-)/1, (<-)/2,
			 '()'/1, '{}'/1,
			 (:=)/1,
			 (:=)/2.

:- initialization( load_foreign_files([],['YAPPython'], init_python_dll), now ).

/**
 * @pred import(File)
 *
 * load  a module file  into the Python scape.
 *
 */
import( F ) :- catch( python:python_import(F), _, fail ).

user:dot_qualified_goal(Fs) :- catch( python:python_proc(Fs), _, fail ).

/**
 * @prefixpred F ()
 *
 * evaluate F as a funtion without arguments.
 *
 */
F() :-
    python:python_proc(F() ).


/**
 * @infixpred V := F
 *
 * evaluate F as a Python exressiom_.
 *
 */
:= (P1 , P2 ) :- !,
     := P1,
    := P2.

:= F :- catch( python:python_proc(F), _, fail ).

/**
 * @infixpred V := F
 *
 * evaluate F as a Python exress and match the result to _V_.
 *
 */
V := F :-
    python:python_assign(F, V).

/*
user:(<- F) :-
	catch( python:python_proc(F), _, fail ).

user:(V <- F) :-
	V := F.
*/

/**
  @pred(E,V)

 Same as :=/2: call E and match the result to _V_.
*/
python(Exp, Out) :-
	Out := Exp.


start_python :-
	python:python_import('inspect'),
	at_halt(end_python).

/**
  * @pred add_cwd_to_python
  *
  * This utility adds the current directory to Python's search path.
*/
add_cwd_to_python :-
    unix(getcwd(Dir)),
    sys.path.append( Dir),
    sys.argv[0] := `yap`.

load_library(Lib,Module) :-
    load_files(library(Lib), [module(Module)]).

load_file(File,Lib) :-
    load_files(File, [module(Lib)]).


load_text(FileAtom,Module) :-
    atom(FileAtom),
    !,
    load_files(Module:atom(FileAtom), []).

load_text(FileString,Module) :-
    string(FileString),
    load_files(Module:string(FileString), []).


%% @}
