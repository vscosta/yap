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
* comments:	Predicate Manipulation for YAP				 *
*									 *
*************************************************************************/

/**
 * @file pl/preds.yap
 */
:- system_module( '$_preds', [abolish/1,
			      abolish/2,
			      assert/1,
			      assert/2,
			      assert_static/1,
			      asserta/1,
			      asserta/2,
			      asserta_static/1,
			      assertz/1,
			      assertz/2,
			      assertz_static/1,
			      clause/2,
			      clause/3,
			      clause_property/2,
			      compile_predicates/1,
			      current_key/2,
			      current_predicate/1,
			      current_predicate/2,
			      dynamic_predicate/2,
			      hide_predicate/1,
			      nth_clause/3,
			      predicate_erased_statistics/4,
			      predicate_property/2,
			      predicate_statistics/4,
			      retract/1,
			      retract/2,
			      retractall/1,
			      stash_predicate/1,
			      system_predicate/1,
			      system_predicate/2,
			      unknown/2], ['$assert_static'/5,
					   '$assertz_dynamic'/4,
					   '$clause'/4,
					   '$current_predicate'/4,
					   '$init_preds'/0,
					   '$noprofile'/2,
					   '$public'/2,
					   '$unknown_error'/1,
					   '$unknown_warning'/1]).

/**
 * @defgroup Database The Clausal Data Base
 * @{
 * @ingroup builtins

Predicates in YAP may be dynamic or static. By default, when
consulting or reconsulting, predicates are assumed to be static:
execution is faster and the code will probably use less space.
Static predicates impose some restrictions: in general there can be no
addition or removal of  clauses for a procedure if it is being used in the
current execution.

Dynamic predicates allow programmers to change the Clausal Data Base with
the same flexibility as in C-Prolog. With dynamic predicates it is
always possible to add or remove clauses during execution and the
semantics will be the same as for C-Prolog. But the programmer should be
aware of the fact that asserting or retracting are still expensive operations,
and therefore he should try to avoid them whenever possible.

*/

:- use_system_module( '$_boot', ['$check_head_and_body'/4,
				 '$check_if_reconsulted'/2,
				 '$head_and_body'/3,
				 '$inform_as_reconsulted'/2]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_init', ['$do_log_upd_clause'/6,
				 '$do_log_upd_clause0'/6,
				 '$do_log_upd_clause_erase'/6,
				 '$do_static_clause'/8]).

:- use_system_module( '$_modules', ['$imported_pred'/4,
				    '$meta_predicate'/4,
				    '$module_expansion'/5]).

:- use_system_module( '$_preddecls', ['$check_multifile_pred'/3,
				      '$dynamic'/2]).

:- use_system_module( '$_strict_iso', ['$check_iso_strict_clause'/1]).



/** @pred  assert_static(: _C_)


Adds clause  _C_ to a static procedure. Asserting a static clause
for a predicate while choice-points for the predicate are available has
undefined results.


*/
assert_static(C) :-
    '$assert'(C , assertz_static, _ ).

/** @pred  asserta_static(: _C_)


Adds clause  _C_ as the first clause for a static procedure.


*/
asserta_static(C) :-
    '$assert'(C , asserta_static, _ ).


/** @pred  assertz_static(: _C_)


Adds clause  _C_ to the end of a static procedure.  Asserting a
static clause for a predicate while choice-points for the predicate are
available has undefined results.



The following predicates can be used for dynamic predicates and for
static predicates, if source mode was on when they were compiled:




*/
assertz_static(C) :-
    '$assert'(C , assertz_static, _ ).

/** @pred  clause(+ _H_, _B_) is iso


A clause whose head matches  _H_ is searched for in the
program. Its head and body are respectively unified with  _H_ and
 _B_. If the clause is a unit clause,  _B_ is unified with
 _true_.

This predicate is applicable to static procedures compiled with
`source` active, and to all dynamic procedures.


*/
clause(V0,Q) :-
    '$yap_strip_module'(V0, M, V),
    must_be_callable( V ),
    '$clause'(V,M,Q,_).

/** @pred  clause(+ _H_, _B_,- _R_)

The same as clause/2, plus  _R_ is unified with the
reference to the clause in the database. You can use instance/2
to access the reference's value. Note that you may not use
erase/1 on the reference on static procedures.
*/
clause(P,Q,R) :-
    '$instance_module'(R,M0), !,
    instance(R,T0),
    ( T0 = (H :- B) -> Q = B ; H=T0, Q = true),
    '$yap_strip_module'(P, M, T),
    '$yap_strip_module'(M0:H, M1, H1),
    (
	M == M1
    ->
    H1 = T
    ;
    M1:H1 = T
    ).
clause(V0,Q,R) :-
    '$yap_strip_module'(V0, M, V),
    must_be_of_type( callable, V ),
    '$clause'(V,M,Q,R).

'$clause'(P,M,Q,R) :-
    '$is_log_updatable'(P, M), !,
    '$log_update_clause'(_,_,M,P,Q,R).
'$clause'(P,M,Q,R) :-
    '$is_exo'(P, M), !,
    Q = true,
    R = '$exo_clause'(M,P),
    '$execute0'(P, M).
'$clause'(P,M,Q,R) :-
    '$is_source'(P, M), !,
    '$static_clause'(_, _, M, P ,Q ,R).
'$clause'(P,M,Q,R) :-
    '$some_recordedp'(M:P), !,
    '$recordedp'(M:P,(P:-Q),R).
'$clause'(P,M,Q,R) :-
    \+ '$undefined'(P,M),
    ( '$is_system_predicate'(P,M) -> true ;
      '$number_of_clauses'(P,M,N), N > 0 ),
    functor(P,Name,Arity),
    '$do_error'(permission_error(access,private_procedure,Name/Arity),
		clause(M:P,Q,R)).

'$init_preds' :-
    once('$do_static_clause'(_,_,_,_,_,_)),
    fail.
'$init_preds' :-
    once('$do_log_upd_clause0'(_,_,_,_,_,_)),
    fail.
'$init_preds' :-
    once('$do_log_upd_clause'(_,_,_,_,_,_)),
    fail.
'$init_preds' :-
    once('$do_log_upd_clause_erase'(_,_,_,_,_,_)),
    fail.

'$init_preds'.

:- '$init_preds'.

/** @pred  nth_clause(+ _H_, _I_,- _R_)


Find the  _I_th clause in the predicate defining  _H_, and give
a reference to the clause. Alternatively, if the reference  _R_ is
given the head  _H_ is unified with a description of the predicate
and  _I_ is bound to its position.


*/
nth_clause(V,I,R) :-
    strip_module(V, M1, P),
    (var(I), var(R)
    ->
    '$number_of_clauses'(P,M1,N),
    between(1, N, I)
    ;
    true
    ),
    '$nth_clause'(P, M1, I, R).

/**
   @pred  abolish(+ _PredSpec_) is iso


Deletes the predicate given by _PredSpec_ from the database.  All
state on the predicate, including whether it is dynamic or static,
multifile, or meta-predicate, will be lost. The specification must
include the name and arity, and it may include module
information. Under <tt>iso</tt> language mode this built-in will only
abolish dynamic procedures. Under other modes it will abolish any
procedures.

Older versions of YAP would accept unbound arguments; please use
current_predicate/2 to enumerate the predicates you want to discard.

*/
abolish(X) :-
    get_predicate_indicator(X, M, Na, Ar),
    functor(H, Na, Ar),
    ( '$is_dynamic'(H, M) -> '$abolishd'(H, M) ;
      '$undefined'(H, M) -> true ;
      current_prolog_flag(language, iso) -> '$do_error'(permission_error(modify,static_procedure,Na/Ar),abolish(X)) ;
      '$abolishs'(H,M)
      ).
    
/** @pred  abolish(+ _P_,+ _N_)

Completely delete the predicate with name _P_ and arity _N_. It will
remove both static and dynamic predicates. All state on the predicate,
including whether it is dynamic or static, multifile, or
meta-predicate, will be lost.

abolish/2 is similar to abolish/1, but it always tries to erase static properties. It should not be confused with SICStus Prolog abolish/2, which is abolish/1 plus a list of options.

*/
abolish(N,A) :-
    get_predicate_indicator(N/A, M, Na, Ar),
    functor(H, Na, Ar),
    ( '$is_dynamic'(H, M) -> '$abolishd'(H, M) ;
      '$undefined'(H, M) -> true ;
      '$abolishs'(H,M)
    ).


'$abolishs'(G, M) :-
    '$system_predicate'(G,M), !,
    functor(G,Name,Arity),
    '$do_error'(permission_error(modify,static_procedure,Name/Arity),abolish(M:G)).
'$abolishs'(G, M) :-
    '$is_multifile'(G,M),
    functor(G,Name,Arity),
    recorded('$mf','$mf_clause'(_,Name,Arity,M,_Ref),R),
    erase(R),
    % no need	erase(Ref),
    fail.
'$abolishs'(T, M) :-
    recorded('$import','$import'(_,M,_,_,T,_,_),R),
    erase(R),
    fail.
'$abolishs'(G, M) :-
    '$purge_clauses'(G, M), fail.
'$abolishs'(_, _).


/**  @pred stash_predicate(+ _Pred_)
Make predicate  _Pred_ invisible to new code, and to `current_predicate/2`,
`listing`, and friends. New predicates with the same name and
functor can be declared.
 **/
stash_predicate(P0) :-
    strip_module(P0, M, P),
    '$stash_predicate2'(P, M).

'$stash_predicate2'(V, M) :- var(V), !,
			     '$do_error'(instantiation_error,stash_predicate(M:V)).
'$stash_predicate2'(N/A, M) :- !,
    functor(S,N,A),
    '$stash_predicate'(S, M) .
'$stash_predicate2'(PredDesc, M) :-
    '$do_error'(type_error(predicate_indicator,PredDesc),stash_predicate(M:PredDesc)).

/** @pred hide_predicate(+ _Pred_)
Make predicate  _Pred_ invisible to `current_predicate/2`,
`listing`, and friends.
 **/
hide_predicate(P0) :-
    strip_module(P0, _M, P),
    nonvar(P),
    P = N/A,
    !,
    functor(_S,N,A).
hide_predicate(P0) :-
    strip_module(P0, M, P),
    '$hide_predicate'(P, M).

/** @pred  predicate_property( _P_, _Prop_) is iso


For the predicates obeying the specification  _P_ unify  _Prop_
  with a property of  _P_. These properties may be:

+ `built_in `
true for built-in predicates,

+ `dynamic`
true if the predicate is dynamic

+ `static `
true if the predicate is static

+ `meta_predicate( _M_) `
true if the predicate has a meta_predicate declaration  _M_.

+ `multifile `
true if the predicate was declared to be multifile

+ `imported_from( _Mod_) `
true if the predicate was imported from module  _Mod_.

+ `exported `
true if the predicate is exported in the current module.

+ `public`
true if the predicate is public; note that all dynamic predicates are
public.

+ `tabled `
true if the predicate is tabled; note that only static predicates can
be tabled in YAP.

+ `source (predicate_property flag) `
true if source for the predicate is available.

+ `number_of_clauses( _ClauseCount_) `
Number of clauses in the predicate definition. Always one if external
or built-in.
r
*/
predicate_property(Pred,Prop) :-
    '$yap_strip_module'(Pred, Mod, TruePred),
    (nonvar(Mod) -> true ; current_module(Mod) ),
    '$import'(Mod:TruePred, M:NPred),
    (nonvar(Prop) ->
     '$predicate_property'(NPred,M,Mod,Prop),
     !
    ;
    '$predicate_property'(NPred,M,Mod,Prop)
    ).
'$predicate_property'(P,M,_,built_in) :-
    '$is_system_predicate'(P,M).
'$predicate_property'(P,M,_,source) :-
    '$predicate_flags'(P,M,F,F),
    F /\ 0x00400000 =\= 0.
'$predicate_property'(P,M,_,tabled) :-
    '$predicate_flags'(P,M,F,F),
    F /\ 0x00000040 =\= 0.
'$predicate_property'(P,M,_,dynamic) :-
    '$is_dynamic'(P,M).
'$predicate_property'(P,M,_,static) :-
    \+ '$is_dynamic'(P,M),
    \+ '$undefined'(P,M).
'$predicate_property'(P,M,_,meta_predicate(Q)) :-
    functor(P,Na,Ar),
    prolog:'$meta_predicate'(Na,M,Ar,Q).
'$predicate_property'(P,M,_,multifile) :-
    '$is_multifile'(P,M).
'$predicate_property'(P,M,_,public) :-
    '$is_public'(P,M).
'$predicate_property'(P,M,_,thread_local) :-
    '$is_thread_local'(P,M).
'$predicate_property'(P,M,M,exported) :-
    functor(P,N,A),
    once(recorded('$module','$module'(_TFN,M,_S,Publics,_L),_)),
    lists:memberchk(N/A,Publics).
'$predicate_property'(_P,M,M0,imported_from(M)) :-
    M \= M0.
'$predicate_property'(P,Mod,_,number_of_clauses(NCl)) :-
    '$number_of_clauses'(P,Mod,NCl).
'$predicate_property'(P,Mod,_,file(F)) :-
    '$owner_file'(P,Mod,F).


/**
  @pred  predicate_statistics( _P_, _NCls_, _Sz_, _IndexSz_)

Given predicate  _P_,  _NCls_ is the number of clauses for
 _P_,  _Sz_ is the amount of space taken to store those clauses
(in bytes), and  _IndexSz_ is the amount of space required to store
indices to those clauses (in bytes).
*/
predicate_statistics(V,NCls,Sz,ISz) :- var(V), !,
				       '$do_error'(instantiation_error,predicate_statistics(V,NCls,Sz,ISz)).
predicate_statistics(P0,NCls,Sz,ISz) :-
    strip_module(P0, M, P),
    '$predicate_statistics'(P,M,NCls,Sz,ISz).

'$predicate_statistics'(M:P,_,NCls,Sz,ISz) :- !,
    '$predicate_statistics'(P,M,NCls,Sz,ISz).
'$predicate_statistics'(P,M,NCls,Sz,ISz) :-
    '$is_log_updatable'(P, M), !,
    '$lu_statistics'(P,NCls,Sz,ISz,M).
'$predicate_statistics'(P,M,_,_,_) :-
    '$is_system_predicate'(P,M), !, fail.
'$predicate_statistics'(P,M,_,_,_) :-
    '$undefined'(P,M), !, fail.
'$predicate_statistics'(P,M,NCls,Sz,ISz) :-
    '$static_pred_statistics'(P,M,NCls,Sz,ISz).

/** @pred  predicate_erased_statistics( _P_, _NCls_, _Sz_, _IndexSz_)


Given predicate  _P_,  _NCls_ is the number of erased clauses for
 _P_ that could not be discarded yet,  _Sz_ is the amount of space
of space required to store indices to those clauses (in bytes).

 */
predicate_erased_statistics(P,NCls,Sz,ISz) :-
    var(P), !,
    current_predicate(_,P),
    predicate_erased_statistics(P,NCls,Sz,ISz).
predicate_erased_statistics(P0,NCls,Sz,ISz) :-
    strip_module(P0,M,P),
    '$predicate_erased_statistics'(M:P,NCls,Sz,_,ISz).

/** @pred  current_predicate( _A_, _P_)

Defines the relation:  _P_ is a currently defined predicate whose name is the atom  _A_.
*/
current_predicate(A0,T0) :-
    ( nonvar(T0) -> '$yap_strip_module'(T0, M, T) ; T0 = T ),
    ( nonvar(A0) -> '$yap_strip_module'(M:A0, MA0, A) ; A0 = A ),
    M = MA0,
    (
	nonvar(M)
    ->
    true
    ;
    '$all_current_modules'(M)
    ),
    % M is bound
    M \= prolog,
    (
    '$current_predicate'(A,M,T,_),
    functor(T, A, _)
    ;
    '$imports'(M:T,M1:_T1),
    M\=M1,
    functor(T, A, _)
    ).


/** @pred  system_predicate( ?_P_ )

Defines the relation:  indicator _P_ refers to a currently defined system predicate.
*/
system_predicate(P0) :-
    '$yap_strip_module'(P0, M0, P),
    ( M= M0 ; M0 \= user, M = user ; M0 \= prolog, M = prolog ),
    (
	var(P)
    ->
    P = A/Arity,
    '$current_predicate'(A, M, T, system),
    functor(T, A, Arity),
    '$is_system_predicate'( T,  M)
    ;
    ground(P), P = A/Arity
    ->
    functor(T, A, Arity),
    '$current_predicate'(A, M, T, system),
    '$is_system_predicate'( T, M)
    ;
    ground(P), P = A//Arity2
    ->
    Arity is Arity2+2,
    functor(T, A, Arity),
    '$current_predicate'(A, M, T, system),
    '$is_system_predicate'( T, M)
    ;
    P = A/Arity
    ->
    '$current_predicate'(A, M, T, system),
    '$is_system_predicate'( T,  M),
    functor(T, A, Arity)
    ;
    P = A//Arity2
    ->
    '$current_predicate'(A, M, T, system),
    '$is_system_predicate'( T,  M),
    functor(T, A, Arity),
    Arity >= 2,
    Arity2 is Arity-2
    ;
    '$do_error'(type_error(predicate_indicator,P),
                system_predicate(P0))
    ).

/** @pred  system_predicate( ?A, ?P )

  Succeeds if _A_ is the name of the system predicate _P_. It can be used to test and to enumerate all system predicates.

  YAP also supports the ISO standard built-in system_predicate/1, that
  provides similar functionality and is compatible with most other Prolog
  systems.

*/
system_predicate(A, P0) :-
    '$yap_strip_module'(P0, M, P),
    (
	nonvar(P)
    ->
    '$current_predicate'(A, M, P, system),
    '$is_system_predicate'( P,  M)
    ;
    '$current_predicate'(A, M, P, system)
    ).


/**
  @pred  current_predicate( F ) is iso

  True if _F_ is the predicate indicator for a currently defined user or
  library predicate.The indicator  _F_ is of the form _Mod_:_Na_/_Ar_ or _Na/Ar_,
  where the atom _Mod_ is the module of the predicate,
 _Na_ is the name of the predicate, and  _Ar_ its arity.
*/
current_predicate(F0) :-
    '$yap_strip_module'(F0, M, F),
    must_bind_to_type( predicate_indicator, F ),
    '$c_i_predicate'( F, M ).

'$c_i_predicate'( AN, M ) :-
    var(AN),
    !,
    AN = A/N,
    current_predicate(A, M:S),
    functor(S, A, N).
'$c_i_predicate'( A/N, M ) :-
    atom(A), integer(N),
    !,
    functor(S, A, N),
    current_predicate(A, M:S).
'$c_i_predicate'( A/N, M ) :-
    atom(A), var(N),
    !,
    current_predicate(A, M:S),
    functor(S, A, N).
'$c_i_predicate'( A/N, M ) :-
    var(A),
    !,
    current_predicate(A, M:S),
    functor(S, A, N).
'$c_i_predicate'( A//N, M ) :-
    var(N),
    !,
    '$c_i_predicate'( A/N0, M ),
    N is N0+2.
'$c_i_predicate'( A//N, M ) :-
    N0 is N-2,
    '$c_i_predicate'( A/N0, M ).


/** @pred  current_key(? _A_,? _K_)


Defines the relation:  _K_ is a currently defined database key whose
name is the atom  _A_. It can be used to generate all the keys for
  the internal data-base.
*/
current_key(A,K) :-
    '$current_predicate'(A,idb,K,user).

% do nothing for now.
'$noprofile'(_, _).

'$ifunctor'(Pred,Na,Ar) :-
    (Ar > 0 ->
         functor(Pred, Na, Ar)
    ;
    Pred = Na
    ).


/** @pred  compile_predicates(: _ListOfNameArity_)



Compile a list of specified dynamic predicates (see dynamic/1 and
assert/1 into normal static predicates. This call tells the
Prolog environment the definition will not change anymore and further
calls to assert/1 or retract/1 on the named predicates
raise a permission error. This predicate is designed to deal with parts
of the program that is generated at runtime but does not change during
the remainder of the program execution.
 */
compile_predicates(Ps) :-
    '$current_module'(Mod),
    '$compile_predicates'(Ps, Mod, compile_predicates(Ps)).

'$compile_predicates'(V, _, Call) :-
    var(V), !,
    '$do_error'(instantiation_error,Call).
'$compile_predicates'(M:Ps, _, Call) :-
    '$compile_predicates'(Ps, M, Call).
'$compile_predicates'([], _, _).
'$compile_predicates'([P|Ps], M, Call) :-
    '$compile_predicate'(P, M, Call),
    '$compile_predicates'(Ps, M, Call).

'$compile_predicate'(P, _M, Call) :-
    var(P), !,
    '$do_error'(instantiation_error,Call).
'$compile_predicate'(M:P, _, Call) :-
    '$compile_predicate'(P, M, Call).
'$compile_predicate'(Na/Ar, Mod, _Call) :-
    functor(G, Na, Ar),
    findall([G|B],clause(Mod:G,B),Cls),
    abolish(Mod:Na,Ar),
    '$add_all'(Cls, Mod).

'$add_all'([], _).
'$add_all'([[G|B]|Cls], Mod) :-
    assert_static(Mod:(G:-B)),
    '$add_all'(Cls, Mod).


clause_property(ClauseRef, file(FileName)) :-
    ( recorded('$mf','$mf_clause'(FileName,_Name,_Arity,_Module,ClauseRef),_R)
    -> true
    ;
    instance_property(ClauseRef, 2, FileName) ).
clause_property(ClauseRef, source(FileName)) :-
    ( recorded('$mf','$mf_clause'(FileName,_Name,_Arity,_Module,ClauseRef),_R)
    -> true
    ;
    instance_property(ClauseRef, 2, FileName) ).
clause_property(ClauseRef, line_count(LineNumber)) :-
    instance_property(ClauseRef, 4, LineNumber),
    LineNumber > 0.
clause_property(ClauseRef, fact) :-
    instance_property(ClauseRef, 3, true).
clause_property(ClauseRef, erased) :-
    instance_property(ClauseRef, 0, true).
clause_property(ClauseRef, predicate(PredicateIndicator)) :-
    instance_property(ClauseRef, 1, PredicateIndicator).

'$set_predicate_attribute'(M:N/Ar, Flag, V) :-
    functor(P, N, Ar),
    '$set_flag'(P, M, Flag, V).

%% '$set_flag'(P, M, trace, off) :-
% set a predicate flag
%
'$set_flag'(P, M, trace, off) :-
    '$predicate_flags'(P,M,F,F),
    FN is F \/ 0x400000000,
    '$predicate_flags'(P,M,F,FN).

/**
@}
*/
