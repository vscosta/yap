% This file has been included as an YAP library by Vitor Santos Costa, 2008

% it is based on the Quintus Prolog arg library

:- module(arg,
	  [
	   genarg/3,
	   arg0/3,
	   genarg0/3,
	   args/3,
	   args0/3,
%	   project/3
	   path_arg/3
	  ]).



arg0(0,T,A) :- !,
	functor(T,A,_).
arg0(I,T,A) :-
	arg(I,T,A).

genarg0(I,T,A) :-
	nonvar(I), !,
	arg0(I,T,A).
genarg0(0,T,A) :-
	functor(T,A,_).
genarg0(I,T,A) :-
	arg(I,T,A).

args(_,[],[]).
args(I,[T|List],[A|ArgList]) :-
	genarg(I, T, A),
	args(I, List, ArgList).

args0(_,[],[]).
args0(I,[T|List],[A|ArgList]) :-
	genarg(I, T, A),
	args0(I, List, ArgList).

project(Terms, Index, Args) :-
	args0(Index, Terms, Args).

% no error checking here!
path_arg([], Term, Term).
path_arg([Index|Indices], Term, SubTerm) :-
	genarg(Index, Term, Arg),
	path_arg(Indices, Arg, SubTerm).
