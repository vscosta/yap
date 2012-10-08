%%% -*- Mode: Prolog; -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    Author:        Nicos Angelopoulos, Vitor Santos Costa, Jan Wielemaker
%    E-mail:        Nicos Angelopoulos <nicos@gmx.co.uk>
%    Copyright (C): Nicos Angelopoulos, Universidade do Porto, VU University Amsterdam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of r..eal
%  distributed according to Perl Artistic License
%  check LICENSE file for distribution license
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(python, [
     init_python/0,
     end_python/0,
     python_command/1
     ]).

:- use_module(library(shlib)).
:- use_module(library(lists)).
:- use_module(library(apply_macros)).
:- use_module(library(charsio)).

/** <module> python

A C-based  Prolog interface to python.


@author		Vitor Santos Costa
@version	0:0:5, 2012/09/12
@license	Perl Artistic License

*/

%%%

python_command(Cmd) :-
    python_run_command(Cmd).

start_python :-
	use_foreign_library(foreign(python)),
	init_python.
	% done	


:- initialization(start_python, now).
