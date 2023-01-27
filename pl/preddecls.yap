/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		preds.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Predicate Manipulation for YAP: declaration support	 *
*									 *
*************************************************************************/
:- system_module_( '$_preddecls', [(discontiguous)/1,
        (dynamic)/1,
        (multifile)/1,
        (discontiguous)/1], ['$check_multifile_pred'/3,
        '$discontiguous'/2,
        '$dynamic'/2]).

:- use_system_module( '$_consult', ['$add_multifile'/3]).

:- use_system_module( '$_errors', [throw_error/2]).

'$log_upd'(1).

/**
  @defgroup YAPPredDecls Declaring Properties of Predicates
  @ingroup YAPCompilerSettings

The YAP Compiler allows the programmer to include declarations with
important pproprties of predicates, such as where they can be modified
during execution time, whether they are meta-predicates, or whether they can be
defined  across multiple files. We next join some of these declarations.

*/

/** @pred public(  _P_ ) is iso

Instructs the compiler that the source of a predicate of a list of
predicates  _P_ must be kept. This source is then accessible through
the clause/2 procedure and through the `listing` family of
built-ins.

Note that all dynamic procedures are public. The `source` directive
defines all new or redefined predicates to be public.

**/
'$public'(X, _) :- var(X), !,
	throw_error(instantiation_error,public(X)).
'$public'(Mod:Spec, _) :- !,
	'$public'(Spec,Mod).
'$public'((A,B), M) :- !, '$public'(A,M), '$public'(B,M).
'$public'([],_) :- !.
'$public'([H|L], M) :- !, '$public'(H, M), '$public'(L, M).
'$public'(A//N1, Mod) :- integer(N1), !,
	N is N1+2,
	'$public'(A/N, Mod).
'$public'(A/N, Mod) :- integer(N), atom(A), !,
	functor(T,A,N),
	'$do_make_public'(T, Mod).
'$public'(X, Mod) :-
	throw_error(type_error(callable,X),dynamic(Mod:X)).

'$do_make_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$do_make_public'(T, Mod) :-
	'$predicate_flags'(T,Mod,F,F),
	NF is F\/0x00400000,
	'$predicate_flags'(T,Mod,F,NF).


discontiguous(V) :-
	var(V), !,
	throw_error(instantiation_error,discontiguous(V)).
discontiguous(M:F) :- !,
	'$discontiguous'(F,M).
discontiguous(F) :-
	'$current_module'(M),
	'$discontiguous'(F,M).

'$discontiguous'(V,M) :- var(V), !,
	throw_error(instantiation_error,M:discontiguous(V)).
'$discontiguous'((X,Y),M) :- !,
	'$discontiguous'(X,M),
	'$discontiguous'(Y,M).
'$discontiguous'(M:A,_) :- !,
	'$discontiguous'(A,M).
'$discontiguous'(N//A1, M) :- !,
	integer(A1), !,
	A is A1+2,
	'$discontiguous'(N/A, M).
'$discontiguous'(N/A, M) :- !,
	'$new_discontiguous'(N,A,M).
'$discontiguous'(P,M) :-
	throw_error(type_error(predicate_indicator,P),M:discontiguous(P)).

%
% did we declare multifile properly?
%
'$check_multifile_pred'(Hd, M, _) :-
	functor(Hd,Na,Ar),
	source_location(F, _),
	recorded('$multifile_defs','$defined'(F,Na,Ar,M),_), !.
% oops, we did not.
'$check_multifile_pred'(Hd, M, Fl) :-
	% so this is not a multi-file predicate any longer.
	functor(Hd,Na,Ar),
	NFl is \(0x20000000) /\ Fl,
	'$predicate_flags'(Hd,M,Fl,NFl),
	'$warn_mfile'(Na,Ar).

'$warn_mfile'(F,A) :-
	write(user_error,'% Warning: predicate '),
	write(user_error,F/A), write(user_error,' was a multifile predicate '),
	write(user_error,' (line '),
	'$start_line'(LN), write(user_error,LN),
	write(user_error,')'),
	nl(user_error).

/**
 @pred module_transparent( + _Preds_ ) is directive
   _Preds_ is a list of predicates that can access the calling context.

This predicate was implemented to achieve compatibility with the older
module expansion system in SWI-Prolog. Please use meta_predicate/1 for
new code.

_Preds_ is a comma separated sequence of name/arity predicate
indicators (like in dynamic/1). Each goal associated with a
transparent declared predicate will inherit the context module from
its caller.

*/
:- dynamic('$module_transparent'/4).

'$module_transparent'((P,Ps), M) :- !,
	'$module_transparent'(P, M),
	'$module_transparent'(Ps, M).
'$module_transparent'(M:D, _) :- !,
	'$module_transparent'(D, M).
'$module_transparent'(F/N, M) :-
	'$module_transparent'(F,M,N,_), !.
'$module_transparent'(F/N, M) :-
	functor(P,F,N),
	asserta(prolog:'$module_transparent'(F,M,N,P)),
	'$predicate_flags'(P, M, Fl, Fl),
	NFlags is Fl \/ 0x200004,
	'$predicate_flags'(P, M, Fl, NFlags).
