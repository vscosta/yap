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
     python/3
     ]).

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).

python(Module:Function, IArgs, OArg) :-
	python_import(Module, MRef),
	python_f(MRef, Function, FRef),
	Args =.. [c|IArgs],
	python_apply(FRef, Args, OArg).

python_command(Cmd) :-
    python_run_command(Cmd).

start_python :-
	use_foreign_library(foreign(python)),
	init_python,
	python_command('import sys'),
	unix(getcwd(Dir)),
	atom_concat(['sys.path.append(\"',Dir,'\")'], Command),
	python_command(Command).
	% done	


:- initialization(start_python, now).
