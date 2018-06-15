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
:- system_module( '$_preddecls', [(discontiguous)/1,
        (dynamic)/1,
        (multifile)/1,
        (discontiguous)/1], ['$check_multifile_pred'/3,
        '$discontiguous'/2,
        '$dynamic'/2]).

:- use_system_module( '$_consult', ['$add_multifile'/3]).

:- use_system_module( '$_errors', ['$do_error'/2]).


:- '$system_meta_predicates'([
			      discontiguous(:),
			      multifile(:)
			     ]
			    ).

'$log_upd'(1).

/**
  @defgroup YAPPredDecls Declaring Properties of Predicates
  @{
  @ingroup YAPCompilerSettings

The YAP Compiler allows the programmer to include declarations with
important pproprties of predicates, such as where they can be modified
during execution time, whether they are meta-predicates, or whether they can be
defined  across multiple files. We next join some of these declarations.

*/

%
% can only do as goal in YAP mode.
%
/** @pred  dynamic( + _P_ )


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate.  _P_ must be written as a predicate indicator, that is in form
 _Name/Arity_ or _Module:Name/Arity_.

~~~~~
:- dynamic god/1.
~~~~~


a more convenient form can be used:

~~~~~
:- dynamic son/3, father/2, mother/2.
~~~~~

or, equivalently,

~~~~~
:- dynamic [son/3, father/2, mother/2].
~~~~~

Note:

a predicate is assumed to be dynamic when
asserted before being defined.


*/
dynamic(X) :-
	current_prolog_flag(language, yap), !,
  '$current_module'(M),
	'$dynamic'(X, M).
dynamic(X) :-
	'$do_error'(context_error(dynamic(X),declaration),query).

'$dynamic'(X,M) :- var(X), !,
	'$do_error'(instantiation_error,dynamic(M:X)).
'$dynamic'(X,M) :- var(M), !,
	'$do_error'(instantiation_error,dynamic(M:X)).
'$dynamic'(Mod:Spec,_) :- !,
	'$dynamic'(Spec,Mod).
'$dynamic'([], _) :- !.
'$dynamic'([H|L], M) :- !, '$dynamic'(H, M), '$dynamic'(L, M).
'$dynamic'((A,B),M) :- !, '$dynamic'(A,M), '$dynamic'(B,M).
'$dynamic'(A//N,Mod) :- integer(N), !,
	N1 is N+2,
	'$dynamic'(A/N1,Mod).
'$dynamic'(A/N,Mod) :-
  functor(G, A, N),
  '$mk_dynamic'(G,Mod).

/** @pred public(  _P_ ) is iso

Instructs the compiler that the source of a predicate of a list of
predicates  _P_ must be kept. This source is then accessible through
the clause/2 procedure and through the `listing` family of
built-ins.

Note that all dynamic procedures are public. The `source` directive
defines all new or redefined predicates to be public.

**/
'$public'(X, _) :- var(X), !,
	'$do_error'(instantiation_error,public(X)).
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
	'$do_pi_error'(type_error(callable,X),dynamic(Mod:X)).

'$do_make_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$do_make_public'(T, Mod) :-
	'$predicate_flags'(T,Mod,F,F),
	NF is F\/0x00400000,
	'$predicate_flags'(T,Mod,F,NF).


/** @pred     multifile( _P_ ) is iso

Declares that a predicate or several predicates may be defined
throughout several files. _P_ is a collection of one or more predicate
indicators:

~~~~~~~
:- multifile user:portray_message/2, multifile user:message_hook/3.
~~~~~~~

Instructs the compiler about the declaration of a predicate  _P_ in
more than one file. It must appear in the first of the loaded files
where the predicate is declared, and before declaration of any of its
clauses.

Multifile declarations must be supported by reconsult/1 and
compile/1: when a multifile predicate is reconsulted,
only the clauses from the same file are removed.

Since YAP4.3.0 multifile procedures can be static or dynamic.

**/
multifile(P) :-
    strip_module(P, OM, Pred),
	'$multifile'(Pred, OM).

'$multifile'(V, _) :-
    var(V),
    !,
	'$do_error'(instantiation_error,multifile(V)).
'$multifile'((X,Y), M) :-
    !,
    '$multifile'(X, M),
    '$multifile'(Y, M).
'$multifile'(Mod:PredSpec, _) :-
    !,
	'$multifile'(PredSpec, Mod).
'$multifile'(N//A, M) :- !,
	integer(A),
	A1 is A+2,
	'$multifile'(N/A1, M).
'$multifile'(N/A, M) :-
	'$add_multifile'(N,A,M),
	fail.
'$multifile'(N/A, M) :-
    functor(S,N,A),
	'$new_multifile'(S, M), !.
'$multifile'([H|T], M) :- !,
	'$multifile'(H,M),
	'$multifile'(T,M).
'$multifile'(P, M) :-
	'$do_error'(type_error(predicate_indicator,P),multifile(M:P)).

discontiguous(V) :-
	var(V), !,
	'$do_error'(instantiation_error,discontiguous(V)).
discontiguous(M:F) :- !,
	'$discontiguous'(F,M).
discontiguous(F) :-
	'$current_module'(M),
	'$discontiguous'(F,M).

'$discontiguous'(V,M) :- var(V), !,
	'$do_error'(instantiation_error,M:discontiguous(V)).
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
	'$do_error'(type_error(predicate_indicator,P),M:discontiguous(P)).

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

'$is_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$is_public'(T, Mod) :-
	'$predicate_flags'(T,Mod,F,F),
	F\/0x00400000 =\= 0.

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
  /**
   * @}
   */
