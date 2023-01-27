/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
*									 *
**************************************************************************
*								         *
* File:		predtypes.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* commen    ts:	boot file for Prolog					 *
*									 *
*************************************************************************/

/**
  @file predtypes.yap
  @brief YAP bootstrap

  @defgroup YAPPredtypes Declaring a type of predicate: 
    - dynamic (logical_updates)
    - multifile,
    - meta_predicate.
    
      @ingroup predimpl

@{


*/


% These are pseudo declarations
% so that the user will get a redefining system predicate

%
% can only do as goal in YAP mode.
%



meta_predicate(P) :-
		strip_module(P, OM, Pred),
		'$meta_predicate'(Pred, OM).
		

'$meta_predicate'(P,M) :-
    (var(P);var(M)),
    !,
    throw_error(instantiation_error,meta_predicate(M:P)).
'$meta_predicate'((P,_Ps),M) :-
	'$meta_predicate'(P,M),
	fail.
'$meta_predicate'((_P,Ps),M) :-
	!,
	'$meta_predicate'(Ps,M).
'$meta_predicate'( M:D, _M ) :-
	'$yap_strip_module'( M:D, M1, P),
	P\==D,
	!,
	'$meta_predicate'( P, M1 ).
'$meta_predicate'( P, M ) :-
    '$new_meta_pred'(P, M),
    recordz('$m' , meta_predicate(M,P),_).
'$meta_predicate'( _D, _M ).


:- meta_predicate([(0,0)]).


:- '$meta_predicate'((
       abolish(:),
	abolish(:,+),
	alarm(+,0,-),
	all(?,0,-),
	assert(:),
	assert(:,+),
	assert_static(:),
	asserta(:),
	asserta(:,+),
	asserta_static(:),
	assertz(:),
	assertz(:,+),
	assertz_static(:),
	at_halt(0),
	bagof(?,0,-),
	bb_get(:,-),
	bb_put(:,+),
	bb_delete(:,?),
	bb_update(:,?,?),
	call(0),
	call(1,?),
	call(2,?,?),
	call(3,?,?,?),
	call_with_args(0),
	call_with_args(1,?),
	call_with_args(2,?,?),
	call_with_args(3,?,?,?),
	call_with_args(4,?,?,?,?),
	call_with_args(5,?,?,?,?,?),
	call_with_args(6,?,?,?,?,?,?),
	call_with_args(7,?,?,?,?,?,?,?),
	call_with_args(8,?,?,?,?,?,?,?,?),
	call_with_args(9,?,?,?,?,?,?,?,?,?),
	call_cleanup(0,0),
	call_cleanup(0,?,0),
	call_residue(0,?),
	call_residue_vars(0,?),
	call_shared_object_function(:,+),
	catch(0,?,0),
	clause(:,?),
	clause(:,?,?),
	current_predicate(:),
	current_predicate(?,:),
	depth_bound_call(0,+),
	findall(?,0,-),
	findall(?,0,-,?),
	forall(0,0),
	format(+,:),
	format(+,+,:),
	freeze(?,0),
	gated_call(0,0,-,0),
	hide_predicate(:),
	if(0,0,0),
	ignore(0),
	incore(0),
	initialization(0),
	initialization(0,+),
	nospy(:),
        not(0),
        notrace(0),
        once(0),
        phrase(2,?),
        phrase(2,?,+),
	predicate_property(:,?),
	predicate_statistics(:,-,-,-),
	on_exception(+,0,0),
	qsave_program(+,:),
	retract(:),
	retract(:,?),
	retractall(:),
	reconsult(:),
	setof(?,0,-),
	setup_call_cleanup(0,0,0),
	setup_call_catcher_cleanup(0,0,?,0),
	spy(:),
	stash_predicate(:),
	when(+,0),
	with_mutex(+,0),
	with_output_to(?,0),
	'->'(0 , 0),
	'*->'(0 , 0),
	';'(0 , 0),
	^(+,0),
	{}(0,?,?),
	','(2,2,?,?),
	';'(2,2,?,?),
	'|'(2,2,?,?),
			      ->(2,2,?,?),
			      \+(2,?,?),
			      \+( 0 )
       ),prolog).

%
% can only do as goal in YAP mode.
%
/** @pred  dynamic( + _P_ )


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate.  _P_ must be written as a predicate indicator, that is in form
 _Name/Arity_ or _Module:Name/Arity_.

```
:- dynamic god/1.
```


a more convenient form can be used:

```
:- dynamic son/3, father/2, mother/2.
```

or, equivalently,

```
:- dynamic [son/3, father/2, mother/2].
```

Note:

a predicate is assumed to be dynamic when
asserted before being defined.


*/



/** @pred     multifile( _P_ ) is iso

Declares that a predicate or several predicates may be defined
throughout several files. _P_ is a collection of one or more predicate
indicators:

```
:- multifile user:portray_message/2, multifile user:message_hook/3.
```

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
    throw_error(instantiation_error,multifile(V)).
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
    !,
    functor(S,N,A),
	'$new_multifile'(S,M),
	fail.
'$multifile'(N/A, M) :-
    functor(S,N,A),
	'$new_multifile'(S, M), !.
'$multifile'([H|T], M) :- !,
	'$multifile'(H,M),
	'$multifile'(T,M).
'$multifile'(P, M) :-
	throw_error(type_error(predicate_indicator,P),multifile(M:P)).



%
% did we declare multifile properly?
%
'$check_multifile_pred'(Hd, M, _) :-
      ( source_location(F, _)
        ->
          true
        ;
          F = user_input
        ),
	functor(Hd,Na,Ar),
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

:- multifile 
       '$inline'/2,
       '$full_clause_optimisation'/4.

'$full_clause_optimisation'(_H, _M, B, B).






/**
 * @}
 * */
