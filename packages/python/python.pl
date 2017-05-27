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

%%%

:- module(python,
	  [
	   init_python/0,
	   end_python/0,
	   python_command/1,
	   python_run_file/1,
	   python_run_command/1,
	   python_run_script/2,
	   python_assign/3,
	   python_import/1,
	   array_to_python_list/4,
	   array_to_python_tuple/4,
	   array_to_python_view/5,
	   python/2,
	   acquire_GIL/0,
	   release_GIL/0,
		 python_threaded/0,
		 prolog_list_to_python_list/3,
	   (:=)/2,
	   (:=)/1,
	%   (<-)/2,
	%   (<-)/1,
	   op(100,fy,$),
	   op(950,fy,:=),
	   op(950,yfx,:=),
	   op(950,fx,<-),
	   op(950,yfx,<-),
	   op(50, yf, []),
	   op(50, yf, '()'),
	   op(100, xfy, '.'),
	   op(100, fy, '.')
	  ]).


/** <module> python

  A C-based  Prolog interface to python.

  @author               Vitor Santos Costa
  @version      0:0:5, 2012/10/8
  @license      Perl Artistic License

This is an interface to allow calling Python from Prolog. Please look
at the SWIG package if you want to embedd Prolog with Python.

The interface should be activated by consulting the python lybrary. It
immediately boots a Python image.

To best define the interface, one has to address two opposite goals:
    - make it as similar to python as possible
    - make all embedded language interfaces (python, R, Java) as
similar as possible.

   YAP supports the following translation between Prolog and Python:

| *Prolog*      | *Pyhon*       |          *Prolog Examples*             |
|:-------------:|:-------------:|---------------------------------------:|
| Numbers       | Numbers       | 2.3
|               |               | 1545
|               |               |
| Atom          | Symbols       | var
| $Atom         |               | $var [ = var]
| `string`      | 'string'      | \`hello\`
| "string"      |        '      | "hello"
|               |               |
| Atom(...)     | Symb(...)     | f( a, b, named=v)
| E.F(...)      | E.F (...)     | mod.f( a) [ = [mod\|f(a)] ]
| Atom()        |               | f() [ = '()'(f) ]
| Lists         | Lists         | [1,2,3]
| t(....)       | Tuples        | t(1,2,3) to (1,2,3)
| (..., ...)    |               | (1,2,3)[ = (1,(2,3))]
| {.=., .=.}    | Dict          | {\`one\`: 1, \`two\`: 2, \`three\`: 3}

*/



/************************************************************************************************************


Python interface

Data types are

     Python                Prolog
     string                atoms
     numbers		       numbers
     lists		           lists
     tuples                t(...)
     generic objs	        __pointer__(Address)

     $var refers to the attribute __main__.var

*************************************************************************************************************/


:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).
:- dynamic python_mref_cache/2, python_obj_cache/2.

:= (P1,P2) :- !,
	:= P1,
	:= P2.
:= import( F ) :- !, python_import(F).
:= F :- python_proc(F).

V <- F :-
	V := F.

( V := F ) :-
    python_assign(F, V).

((<- F)) :-
	:= F.

python_import(Module) :-
    python_import(Module, _).


python(Exp, Out) :-
	Out := Exp.

python_command(Cmd) :-
       python_run_command(Cmd).


start_python :-
	python_import('inspect', _),
	at_halt(end_python).

add_cwd_to_python :-
	unix(getcwd(Dir)),
	atom_concat(['sys.path.append(\"',Dir,'\")'], Command),
	python_command(Command),
	python_command("sys.argv = [\"yap\"]").
	% done

:- initialization( load_foreign_files([libYAPPython], [], init_python), now ).
