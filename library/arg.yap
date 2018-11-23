/**
 * @file   arg.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 01:08:55 2015
 *
 * @brief
*/


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




/**
 *
 *
 * @defgroup args Term Argument Manipulation.
 *
 * @ingroup library
 *
 * @{
 *
 *This library extends arg/3 by supporting backtracking through
 *arguments and access to sub-arguments,
 *
 *  - arg0/3
 *  - args/3
 *  - args0/3
 *  - genarg/3
 *  - genarg0/3
 *  - path_arg/3
 *
 *
 *It is based on the Quintus Prolog public domain library. Except for
 *project, all predicates use the arg/3 argument pattern.  This file has
 *been included in the YAP library by Vitor Santos Costa, 2008.
 *
 * No error checking is actuallly performed within the package: this
 *left to the C-code that implements arg/3 and genarg/3.
 */

/**
 * @pred arg0( +_Index_, +_Term_ , -_Arg_ )
 *
 * Similar to arg/3, but `arg0(0,_T_,_F_)` unifies _F_ with _T_'s principal functor:

~~~~~~~~~
?- arg0(0, f(a,b), A).
A = f.
?- arg0(1, f(a,b), A).
A = a.
?- arg0(2, f(a,b), A).
A = b.
~~~~~~~~~

*/
arg0(0,T,A) :- !,
	functor(T,A,_).
arg0(I,T,A) :-
	arg(I,T,A).

/**
 * @pred genarg0( +_Index_, +_Term_ , -_Arg_ )
 *
 * Similar to genarg/3, but `genarg0(0,_T_,_F_)` unifies _F_ with _T_'s principal functor:
~~~~~~~~~
?- genarg0(I,f(a,b),A).
A = f,
I = 0 ? ;
A = a,
I = 1 ? ;
A = b,
I = 2.
~~~~~~~~~

*/
genarg0(I,T,A) :-
	nonvar(I), !,
	arg0(I,T,A).
genarg0(0,T,A) :-
	functor(T,A,_).
genarg0(I,T,A) :-
	genarg(I,T,A).

/**
 * @pred args( +_Index_, +_ListOfTerms_ , -_ListOfArgs_ )
 *
 * Succeeds if _ListOfArgs_ unifies with the application of  genarg/3 to every element of _ListOfTerms_.

It corresponds to calling maplist/3 on genarg/3:
~~~~~~~~~
args( I, Ts, As) :-
    maplist( genarg(I), Ts, As).
~~~~~~~~~

Notice that unification allows  _ListOfArgs_ to be bound, eg:

~~~~~~~~~
?- args(1, [X1+Y1,X2-Y2,X3*Y3,X4/Y4], [1,1,1,1]).
X1 = X2 = X3 = X4 = 1.
~~~~~~~~~


*/
args(_,[],[]).
args(I,[T|List],[A|ArgList]) :-
	genarg(I, T, A),
	args(I, List, ArgList).

/**
 * @pred args0( +_Index_, +_ListOfTerms_ , -_ListOfArgs_ )
 *
 * Succeeds if _ListOfArgs_ unifies with the application of  genarg0/3 to every element of _ListOfTerms_.

It corresponds to calling maplist/3 on genarg0/3:
~~~~~~~~~
args( I, Ts, As) :-
    maplist( genarg0(I), Ts, As).
~~~~~~~~~

Notice that unification allows  _ListOfArgs_ to be bound, eg:

~~~~~~~~~
?- args(1, [X1+Y1,X2-Y2,X3*Y3,X4/Y4], [1,1,1,1]).
X1 = X2 = X3 = X4 = 1.
~~~~~~~~~


*/
args0(_,[],[]).
args0(I,[T|List],[A|ArgList]) :-
	genarg(I, T, A),
	args0(I, List, ArgList).

/**
 * @pred args0( +_ListOfTerms_ , +_Index_, -_ListOfArgs_ )
 *
 * Succeeds if _ListOfArgs_ unifies with the application of  genarg0/3 to every element of _ListOfTerms_.

It corresponds to calling args0/3 but with a different order.
*/
project(Terms, Index, Args) :-
	args0(Index, Terms, Args).

% no error checking here!
/**
 * @pred path_arg( +_Path_ , +_Term_, -_Arg_ )
 *
 * Succeeds if _Path_ is empty and _Arg unifies with _Term_, or if _Path_ is a list with _Head_ and _Tail_, genarg/3 succeeds on the current term, and path_arg/3 succeeds on its argument.
 *
 * Notice that it can be used to enumerate all possible paths in  a term.
*/
path_arg([], Term, Term).
path_arg([Index|Indices], Term, SubTerm) :-
	genarg(Index, Term, Arg),
	path_arg(Indices, Arg, SubTerm).

%%% @}

/** @} */
