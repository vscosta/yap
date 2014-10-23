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

/** @defgroup Database Using the Clausal Data Base
@ingroup YAPBuiltins
@{

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
        '$current_predicate_no_modules'/3,
        '$init_preds'/0,
        '$noprofile'/2,
        '$public'/2,
        '$unknown_error'/1,
        '$unknown_warning'/1]).

:- use_system_module( '$_boot', ['$check_head_and_body'/4,
        '$check_if_reconsulted'/2,
        '$handle_throw'/3,
        '$head_and_body'/3,
        '$inform_as_reconsulted'/2]).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( '$_init', ['$do_log_upd_clause'/6,
        '$do_log_upd_clause0'/6,
        '$do_log_upd_clause_erase'/6,
        '$do_static_clause'/5]).

:- use_system_module( '$_modules', ['$imported_pred'/4,
        '$meta_predicate'/4,
        '$module_expansion'/5]).

:- use_system_module( '$_preddecls', ['$check_multifile_pred'/3,
        '$dynamic'/2]).

:- use_system_module( '$_strict_iso', ['$check_iso_strict_clause'/1]).

% The next predicates are applicable only
% to dynamic code

/** @pred  asserta(+ _C_) is iso 


Adds clause  _C_ to the beginning of the program. If the predicate is
undefined, declare it as dynamic.

 
*/
asserta(Mod:C) :- !,
	'$assert'(C,Mod,first,_,asserta(Mod:C)).
asserta(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,first,_,asserta(C)).

/** @pred  assertz(+ _C_) is iso 


Adds clause  _C_ to the end of the program. If the predicate is
undefined, declare it as dynamic.

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP allows
asserting clauses for static predicates. The current version of YAP
supports this feature, but this feature is deprecated and support may go
away in future versions.

 
*/
assertz(Mod:C) :- !,
	'$assert'(C,Mod,last,_,assertz(Mod:C)).
assertz(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,last,_,assertz(C)).

/** @pred  assert(+ _C_) 


Same as assertz/1. Adds clause  _C_ to the program. If the predicate is undefined,
declare it as dynamic. New code should use assertz/1 for better portability.

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP allows
asserting clauses for static predicates, as long as the predicate is not
in use and the language flag is <tt>cprolog</tt>. Note that this feature is
deprecated, if you want to assert clauses for static procedures you
should use assert_static/1.

 
*/
assert(Mod:C) :- !,
	'$assert'(C,Mod,last,_,assert(Mod:C)).
assert(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,last,_,assert(C)).

'$assert'(V,Mod,_,_,_) :- var(V), !,
	'$do_error'(instantiation_error,assert(Mod:V)).
'$assert'(V,Mod,_,_,_) :- var(Mod), !,
	'$do_error'(instantiation_error,assert(Mod:V)).
'$assert'(I,Mod,_,_,_) :- number(I), !,
	'$do_error'(type_error(callable,I),assert(Mod:I)).
'$assert'(M:C,_,Where,R,P) :- !,
	'$assert'(C,M,Where,R,P).
'$assert'((H:-G),M1,Where,R,P) :- !,
	'$assert_clause'(H, G, M1, Where, R, P).
'$assert'(H,M1,Where,R,_) :-
	strip_module(M1:H, HM, H1),
	'$assert_fact'(H1, HM, Where, R).

'$assert_clause'(H, _, _, _, _, P) :-
	var(H), !, '$do_error'(instantiation_error,P).
'$assert_clause'(M1:C, G, M1, Where, R, P) :- !,
	'$assert_clause2'(C, G, M1, Where, R, P).
'$assert_clause'(H, G, M1, Where, R, P) :- !,
	'$assert_clause2'(H, G, M1, Where, R, P).

'$assert_fact'(H,Mod,Where,R) :-
	( '$is_log_updatable'(H, Mod) ->
	    '$compile_dynamic'(H, Where, H, Mod, R)
        ;
	 '$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where, H, true, H, Mod, R)
	;
	  '$undefined'(H,Mod) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar, Mod),
	    '$assert_fact'(H,Mod,Where,R)
	;
            '$access_yap_flags'(14, 1) -> % I can assert over static facts in YAP mode
	    '$assert1'(Where,H,H,Mod,H)
        ;
	    functor(H, Na, Ar),
            '$do_error'(permission_error(modify,static_procedure,Na/Ar),Mod:assert(H))
	).


'$assert_clause2'(HI,BI,Mod,Where,R,P) :-
	'$expand_clause'((HI :- BI),C0,C,Mod,HM),
	'$assert_clause3'(C0,C,HM,Where,R,P).

'$assert_clause3'(C0,C,Mod,Where,R,P) :-
	'$check_head_and_body'(C,H,B,P),
	( '$is_log_updatable'(H, Mod) ->
            '$compile_dynamic'((H :- B), Where, C0, Mod, R)
	;
          '$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where, H, B, C0, Mod, R)
	;
	  '$undefined'(H,Mod) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar, Mod),
	    '$assert_clause3'(C0,C,Mod,Where,R,P)
	;
            '$access_yap_flags'(14, 1) -> % I can assert over static facts in YAP mode
	    '$assert1'(Where,C,C0,Mod,H)
        ;
	    functor(H, Na, Ar),
            '$do_error'(permission_error(modify,static_procedure,Na/Ar),P)
	).


'$assert_dynamic'(V,Mod,_,_,_) :- var(V), !,
	'$do_error'(instantiation_error,assert(Mod:V)).
'$assert_dynamic'(M:C,_,Where,R,P) :- !,
	'$assert_dynamic'(C,M,Where,R,P).
'$assert_dynamic'((H:-_G),_M1,_Where,_R,P) :-
        var(H), !, '$do_error'(instantiation_error,P).
'$assert_dynamic'(CI,Mod,Where,R,P) :-
	'$expand_clause'(CI,C0,C,Mod,HM),
	'$assert_dynamic2'(C0,C,HM,Where,R,P).

'$assert_dynamic2'(C0,C,Mod,Where,R,P) :-
	'$check_head_and_body'(C,H,B,P),
	( '$is_log_updatable'(H, Mod) ->
	    '$compile_dynamic'(C, Where, C0, Mod, R)
	;
	  '$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where,H,B,C0,Mod,R)
	;
	  '$undefined'(H, Mod) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar, Mod),
	    '$assert_dynamic2'(C0,C,Mod,Where,R,P)
	;
	    functor(H,Na,Ar),
	    '$do_error'(permission_error(modify,static_procedure,Na/Ar),P)
	).

/** @pred  assert_static(: _C_) 


Adds clause  _C_ to a static procedure. Asserting a static clause
for a predicate while choice-points for the predicate are available has
undefined results.

 
*/
assert_static(Mod:C) :- !,
	'$assert_static'(C,Mod,last,_,assert_static(Mod:C)).
assert_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,last,_,assert_static(C)).

/** @pred  asserta_static(: _C_) 


Adds clause  _C_ to the beginning of a static procedure. 

 
*/
asserta_static(Mod:C) :- !,
	'$assert_static'(C,Mod,first,_,asserta_static(Mod:C)).
asserta_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,first,_,asserta_static(C)).

asserta_static(Mod:C) :- !,
	'$assert_static'(C,Mod,last,_,assertz_static(Mod:C)).
/** @pred  assertz_static(: _C_) 


Adds clause  _C_ to the end of a static procedure.  Asserting a
static clause for a predicate while choice-points for the predicate are
available has undefined results.



The following predicates can be used for dynamic predicates and for
static predicates, if source mode was on when they were compiled:



 
*/
assertz_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,last,_,assertz_static(C)).

'$assert_static'(V,M,_,_,_) :- var(V), !,
	'$do_error'(instantiation_error,assert(M:V)).
'$assert_static'(M:C,_,Where,R,P) :- !,
	'$assert_static'(C,M,Where,R,P).
'$assert_static'((H:-_G),_M1,_Where,_R,P) :-
	var(H), !, '$do_error'(instantiation_error,P).
'$assert_static'(CI,Mod,Where,R,P) :-
	'$expand_clause'(CI,C0,C,Mod, HM),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H, HM) ->
	    '$do_error'(permission_error(modify,dynamic_procedure,HM:Na/Ar),P)
	;
	  '$undefined'(H,HM), get_value('$full_iso',true) ->
	    functor(H,Na,Ar), '$dynamic'(Na/Ar, HM), '$assertat_d'(Where,H,B,C0,HM,R)
	;
	'$assert1'(Where,C,C0,HM,H)
        ).


'$assertat_d'(first,Head,Body,C0,Mod,R) :- !,
	'$compile_dynamic'((Head:-Body), first, C0, Mod, CR),
         ( get_value('$abol',true)
           ->
            '$flags'(Head,Mod,Fl,Fl),
	    ( Fl /\ 0x20000000 =\= 0 -> '$check_multifile_pred'(Head,Mod,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordap'(Mod:Head,(H0 :- B0),R,CR),
	( '$is_multifile'(Head, Mod) ->
	    source_location(F, _),
	    functor(H0, Na, Ar),
	    recorda('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,Mod,F,R), _) 
	;
	    true
	).
'$assertat_d'(last,Head,Body,C0,Mod,R) :-
	'$compile_dynamic'((Head:-Body), last, C0, Mod, CR),
         ( get_value('$abol',true)
           ->
            '$flags'(Head,Mod,Fl,Fl),
	    ( Fl /\ 0x20000000 =\= 0 -> '$check_multifile_pred'(Head,Mod,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordzp'(Mod:Head,(H0 :- B0),R,CR),
	( '$is_multifile'(H0, Mod) ->
	    source_location(F, _),
	    functor(H0, Na, Ar),
	    recordz('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,Mod,F,R), _) 
	;
	    true
	).

'$assert1'(last,C,C0,Mod,_) :- '$compile'(C,0,C0,Mod).
'$assert1'(first,C,C0,Mod,_) :- '$compile'(C,2,C0,Mod).

'$assertz_dynamic'(X, C, C0, Mod) :- (X/\4)=:=0, !,
	'$head_and_body'(C,H,B),
	'$assertat_d'(last,H,B,C0,Mod,_).
'$assertz_dynamic'(X,C,C0,Mod) :- 
	'$head_and_body'(C,H,B), 
	functor(H,N,A),
	('$check_if_reconsulted'(N,A) ->
		true
		 ;
	  (X/\8)=:=0 ->
		'$inform_as_reconsulted'(N,A),
		'$remove_all_d_clauses'(H,Mod)
		 ;
		true
	),
	'$assertat_d'(last,H,B,C0,Mod,_).

'$remove_all_d_clauses'(H,M) :-
	'$is_multifile'(H, M), !,
	functor(H, Na, A),
	'$erase_all_mf_dynamic'(Na,A,M).
'$remove_all_d_clauses'(H,M) :-
	'$recordedp'(M:H,_,R), erase(R), fail.
'$remove_all_d_clauses'(_,_).

'$erase_all_mf_dynamic'(Na,A,M) :-
	source_location( F , _),
	recorded('$multifile_dynamic'(_,_,_), '$mf'(Na,A,M,F,R), R1),
	erase(R1),
	erase(R),
	fail.
'$erase_all_mf_dynamic'(_,_,_).

/** @pred  asserta(+ _C_,- _R_)

The same as `asserta(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a 
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

 
*/
asserta(M:C,R) :- !,
	'$assert_dynamic'(C,M,first,R,asserta(M:C,R)).
asserta(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,first,R,asserta(C,R)).

/** @pred  assertz(+ _C_,- _R_)

The same as `assertz(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a 
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

 
*/
assertz(M:C,R) :- !,
	'$assert_dynamic'(C,M,last,R,assertz(M:C,R)).
assertz(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,last,R,assertz(C,R)).

/** @pred  assert(+ _C_,- _R_)

The same as `assert(C)` ( (see Modifying the Database)) but
unifies  _R_ with the  database reference that identifies the new
clause, in a one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

 
*/
assert(M:C,R) :- !,
	'$assert_dynamic'(C,M,last,R,assert(M:C,R)).
assert(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,last,R,assert(C,R)).

/** @pred  clause(+ _H_, _B_) is iso 


A clause whose head matches  _H_ is searched for in the
program. Its head and body are respectively unified with  _H_ and
 _B_. If the clause is a unit clause,  _B_ is unified with
 _true_.

This predicate is applicable to static procedures compiled with
`source` active, and to all dynamic procedures.

 
*/
clause(M:P,Q) :- !,
	'$clause'(P,M,Q,_).
clause(V,Q) :-
	'$current_module'(M),
	'$clause'(V,M,Q,_).

/** @pred  clause(+ _H_, _B_,- _R_)

The same as clause/2, plus  _R_ is unified with the
reference to the clause in the database. You can use instance/2
to access the reference's value. Note that you may not use
erase/1 on the reference on static procedures.

 
*/
clause(P,Q,R) :- var(P), !,
	'$current_module'(M),
	'$clause'(P,M,Q,R).
clause(M:P,Q,R) :- !,
	'$clause'(P,M,Q,R).
clause(V,Q,R) :-
	'$current_module'(M),
	'$clause'(V,M,Q,R).

'$clause'(P,M,Q,R) :-
	'$instance_module'(R,M0), !,
	M0 = M,
	instance(R,T),
	( T = (H :- B) -> P = H, Q = B ; P=T, Q = true).
'$clause'(V,M,Q,R) :- var(V), !, 
	'$do_error'(instantiation_error,clause(M:V,Q,R)).
'$clause'(C,M,Q,R) :- 
	number(C), !,
	'$do_error'(type_error(callable,C),clause(M:C,Q,R)).
'$clause'(C,M,Q,R) :-
	db_reference(C), !,
	'$do_error'(type_error(callable,C),clause(M:R,Q,R)).
'$clause'(M:P,_,Q,R) :- !,
	'$clause'(P,M,Q,R).
'$clause'(P,M,Q,R) :-
	'$is_exo'(P, M), !,
	Q = true,
	R = '$exo_clause'(M,P),
	'$execute0'(P, M).
'$clause'(P,M,Q,R) :-
	'$is_source'(P, M), !,
	'$static_clause'(P,M,Q,R).
'$clause'(P,M,Q,R) :-
	'$is_log_updatable'(P, M), !,
	'$log_update_clause'(P,M,Q,R).
'$clause'(P,M,Q,R) :-
	'$some_recordedp'(M:P), !,
	'$recordedp'(M:P,(P:-Q),R).
'$clause'(P,M,Q,R) :-
	\+ '$undefined'(P,M),
	( '$system_predicate'(P,M) -> true ;
	    '$number_of_clauses'(P,M,N), N > 0 ),
	functor(P,Name,Arity),
	'$do_error'(permission_error(access,private_procedure,Name/Arity),
	      clause(M:P,Q,R)).

'$init_preds' :- 
	once('$handle_throw'(_,_,_)),
	fail.
'$init_preds' :- 
	once('$do_static_clause'(_,_,_,_,_)),
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



The following predicates can only be used for dynamic predicates:



 
*/
nth_clause(V,I,R) :-
	'$current_module'(M),
	strip_module(M:V, M1, P), !,
	'$nth_clause'(P, M1, I, R).


'$nth_clause'(P,M,I,R) :-
	var(I), var(R), !,
	'$clause'(P,M,_,R),
	'$fetch_nth_clause'(P,M,I,R).
'$nth_clause'(P,M,I,R) :-
	'$fetch_nth_clause'(P,M,I,R).

/** @pred  retract(+ _C_) is iso 


Erases the first clause in the program that matches  _C_. This
predicate may also be used for the static predicates that have been
compiled when the source mode was `on`. For more information on
source/0 ( (see Setting the Compiler)).

 
*/
retract(M:C) :- !,
	'$retract'(C,M).
retract(C) :-
	'$current_module'(M),
	'$retract'(C,M).
	
	
'$retract'(V,_) :- var(V), !,
	'$do_error'(instantiation_error,retract(V)).
'$retract'(M:C,_) :- !,
	'$retract'(C,M).
'$retract'(C,M) :- 
	'$check_head_and_body'(C,H,B,retract(M:C)), !,
	'$flags'(H, M, F, F),
	'$retract2'(F, H,M,B,_).

'$retract2'(F, H, M, B, R) :-
	F /\ 0x08000000 =:= 0x08000000, !,
%	'$is_log_updatable'(H, M), !,
	'$log_update_clause'(H,M,B,R),
	( F /\ 0x20000000  =:= 0x20000000, recorded('$mf','$mf_clause'(_,_,_,_,R),MR), erase(MR), fail ; true),
	erase(R).
'$retract2'(F, H, M, B, R) :- 	
%	'$is_dynamic'(H,M), !,
	F /\ 0x00002000 =:= 0x00002000, !,
	'$recordedp'(M:H,(H:-B),R),
	( F /\ 0x20000000  =:= 0x20000000, recorded('$mf','$mf_clause'(_,_,_,_,MRef),MR), erase(MR), erase(MRef), fail ; true),
	erase(R).
'$retract2'(_, H,M,_,_) :- 	
	'$undefined'(H,M), !,
	functor(H,Na,Ar),
	'$dynamic'(Na/Ar,M),
	fail.
'$retract2'(_, H,M,B,_) :- 	
	functor(H,Na,Ar),
	'$do_error'(permission_error(modify,static_procedure,Na/Ar),retract(M:(H:-B))).

/** @pred  retract(+ _C_,- _R_)

Erases from the program the clause  _C_ whose 
database reference is  _R_. The predicate must be dynamic.




 */
retract(M:C,R) :- !,
	'$retract'(C,M,R).
retract(C,R) :-
	'$current_module'(M),
	'$retract'(C,M,R).

'$retract'(V,M,R) :- var(V), !,
	'$do_error'(instantiation_error,retract(M:V,R)).
'$retract'(M:C,_,R) :- !,
	'$retract'(C,M,R).
'$retract'(C, M, R) :-
	'$check_head_and_body'(C,H,B,retract(C,R)),
	db_reference(R), '$is_dynamic'(H,M), !,
	instance(R,(H:-B)), erase(R).
'$retract'(C,M,R) :-
	'$check_head_and_body'(C,H,B,retract(C,R)),
	var(R), !,
	'$retract2'(H, M, B, R).
'$retract'(C,M,_) :-
	'$fetch_predicate_indicator_from_clause'(C, PI),
	'$do_error'(permission_error(modify,static_procedure,PI),retract(M:C)).

'$fetch_predicate_indicator_from_clause'((C :- _), Na/Ar) :- !,
	functor(C, Na, Ar).
'$fetch_predicate_indicator_from_clause'(C, Na/Ar) :-
	functor(C, Na, Ar).
	

/** @pred  retractall(+ _G_) is iso 


Retract all the clauses whose head matches the goal  _G_. Goal
 _G_ must be a call to a dynamic predicate.




 */
retractall(M:V) :- !,
	'$retractall'(V,M).
retractall(V) :-
	'$current_module'(M),
	'$retractall'(V,M).

'$retractall'(V,M) :- var(V), !,
	'$do_error'(instantiation_error,retract(M:V)).
'$retractall'(M:V,_) :- !,
	'$retractall'(V,M).
'$retractall'(T,M) :-
	(
	  '$is_log_updatable'(T, M) ->
	 ( '$is_multifile'(T, M) ->
	   '$retractall_lu_mf'(T,M)
	 ;
	   '$retractall_lu'(T,M)
	 )
	;
	  \+ callable(T) ->
	  '$do_error'(type_error(callable,T),retractall(T))
	;
	  '$undefined'(T,M) ->
	  functor(T,Na,Ar),
	  '$dynamic'(Na/Ar,M), !
	;
	  '$is_dynamic'(T,M) ->
	  '$erase_all_clauses_for_dynamic'(T, M)
	;
	  functor(T,Na,Ar),
	  '$do_error'(permission_error(modify,static_procedure,Na/Ar),retractall(T))
	).
	
'$retractall_lu'(T,M) :-
	'$free_arguments'(T), !,
	( '$purge_clauses'(T,M), fail ; true ).
'$retractall_lu'(T,M) :-
	'$log_update_clause'(T,M,_,R),
	erase(R),
	fail.
'$retractall_lu'(_,_).

'$retractall_lu_mf'(T,M) :-
	'$log_update_clause'(T,M,_,R),
	( recorded('$mf','$mf_clause'(_,_,_,_,R),MR), erase(MR), fail ; true),
	erase(R),
	fail.
'$retractall_lu_mf'(_,_).

'$erase_all_clauses_for_dynamic'(T, M) :-
	'$recordedp'(M:T,(T :- _),R), erase(R), fail.
'$erase_all_clauses_for_dynamic'(T,M) :-
	'$recordedp'(M:T,_,_), fail.
'$erase_all_clauses_for_dynamic'(_,_).

/** @pred  abolish(+ _P_,+ _N_)

Completely delete the predicate with name _P_ and arity _N_. It will
remove both static and dynamic predicates. All state on the predicate,
including whether it is dynamic or static, multifile, or
meta-predicate, will be lost. 
*/
abolish(Mod:N,A) :- !,
	'$abolish'(N,A,Mod).
abolish(N,A) :-
	'$current_module'(Mod),
	'$abolish'(N,A,Mod).
	
'$abolish'(N,A,M) :- var(N), !,
	'$do_error'(instantiation_error,abolish(M:N,A)).
'$abolish'(N,A,M) :- var(A), !,
	'$do_error'(instantiation_error,abolish(M:N,A)).
'$abolish'(N,A,M) :-
	( recorded('$predicate_defs','$predicate_defs'(N,A,M,_),R) -> erase(R) ),
	fail.
'$abolish'(N,A,M) :- functor(T,N,A),
		( '$is_dynamic'(T, M) -> '$abolishd'(T,M) ;
	      	 /* else */	      '$abolishs'(T,M) ).

/** @pred  abolish(+ _PredSpec_) is iso 


Deletes the predicate given by  _PredSpec_ from the database. If
 _PredSpec_ is an unbound variable, delete all predicates for the
current module. The
specification must include the name and arity, and it may include module
information. Under <tt>iso</tt> language mode this built-in will only abolish
dynamic procedures. Under other modes it will abolish any procedures. 

 
*/
abolish(V) :- var(V), !,
	'$do_error'(instantiation_error,abolish(V)).
abolish(Mod:V) :- var(V), !,
	'$do_error'(instantiation_error,abolish(Mod:V)).
abolish(M:X) :- !,
	'$abolish'(X,M).
abolish(X) :- 
	'$current_module'(M),
	'$abolish'(X,M).

'$abolish'(X,M) :- 
	'$access_yap_flags'(8, 2), !,
	'$new_abolish'(X,M).
'$abolish'(X, M) :- 
	'$old_abolish'(X,M).

'$new_abolish'(V,M) :- var(V), !,
	'$abolish_all'(M).
'$new_abolish'(A,M) :- atom(A), !,
	'$abolish_all_atoms'(A,M).
'$new_abolish'(M:PS,_) :- !,
	'$new_abolish'(PS,M).
'$new_abolish'(Na//Ar1, M) :-
	integer(Ar1),
	!,
	Ar is Ar1+2,
	'$new_abolish'(Na//Ar, M).
'$new_abolish'(Na/Ar, M) :-
	functor(H, Na, Ar),
	'$is_dynamic'(H, M), !,
	'$abolishd'(H, M).
'$new_abolish'(Na/Ar, M) :- % succeed for undefined procedures.
	functor(T, Na, Ar),
	'$undefined'(T, M), !.
'$new_abolish'(Na/Ar, M) :-
	'$do_error'(permission_error(modify,static_procedure,Na/Ar),abolish(M:Na/Ar)).
'$new_abolish'(T, M) :-
	'$do_error'(type_error(predicate_indicator,T),abolish(M:T)).

'$abolish_all'(M) :-
        '$current_predicate'(M,Na,Ar),
	'$new_abolish'(Na/Ar, M),
	fail.
'$abolish_all'(_).

'$abolish_all_atoms'(Na, M) :-
        '$current_predicate_for_atom'(Na,M,Ar),
	'$new_abolish'(Na/Ar, M),
	fail.
'$abolish_all_atoms'(_,_).

'$check_error_in_predicate_indicator'(V, Msg) :-
	var(V), !,
	'$do_error'(instantiation_error, Msg).
'$check_error_in_predicate_indicator'(M:S, Msg) :- !,
	'$check_error_in_module'(M, Msg),
	'$check_error_in_predicate_indicator'(S, Msg).
'$check_error_in_predicate_indicator'(S, Msg) :-
	S \= _/_,
	S \= _//_, !,
	'$do_error'(type_error(predicate_indicator,S), Msg).
'$check_error_in_predicate_indicator'(Na/_, Msg) :-
	var(Na), !,
	'$do_error'(instantiation_error, Msg).
'$check_error_in_predicate_indicator'(Na/_, Msg) :-
	\+ atom(Na), !,
	'$do_error'(type_error(atom,Na), Msg).
'$check_error_in_predicate_indicator'(_/Ar, Msg) :-
	var(Ar), !,
	'$do_error'(instantiation_error, Msg).
'$check_error_in_predicate_indicator'(_/Ar, Msg) :-
	\+ integer(Ar), !,
	'$do_error'(type_error(integer,Ar), Msg).
'$check_error_in_predicate_indicator'(_/Ar, Msg) :-
	Ar < 0, !,
	'$do_error'(domain_error(not_less_than_zero,Ar), Msg).
% not yet implemented!
%'$check_error_in_predicate_indicator'(Na/Ar, Msg) :-
%	Ar < maxarity, !,
%	'$do_error'(type_error(representation_error(max_arity),Ar), Msg).

'$check_error_in_module'(M, Msg) :-
	var(M), !,
	'$do_error'(instantiation_error, Msg).
'$check_error_in_module'(M, Msg) :-
	\+ atom(M), !,
	'$do_error'(type_error(atom,M), Msg).

'$old_abolish'(V,M) :- var(V), !,
	( '$access_yap_flags'(8, 1) ->
	    '$do_error'(instantiation_error,abolish(M:V))
	;
	    '$abolish_all_old'(M)
	).
'$old_abolish'(N/A, M) :- !,
	'$abolish'(N, A, M).
'$old_abolish'(A,M) :- atom(A), !,
	( '$access_yap_flags'(8, 1) ->
	  '$do_error'(type_error(predicate_indicator,A),abolish(M:A))
	;
	    '$abolish_all_atoms_old'(A,M)
	).
'$old_abolish'(M:N,_) :- !,
	'$old_abolish'(N,M).
'$old_abolish'([], _) :- !.
'$old_abolish'([H|T], M) :- !,  '$old_abolish'(H, M), '$old_abolish'(T, M).
'$old_abolish'(T, M) :-
	'$do_error'(type_error(predicate_indicator,T),abolish(M:T)).
	
'$abolish_all_old'(M) :-
        '$current_predicate'(M, Na, Ar),
	'$abolish'(Na, Ar, M),
	fail.
'$abolish_all_old'(_).

'$abolish_all_atoms_old'(Na, M) :-
        '$current_predicate_for_atom'(Na, M, Ar),
	'$abolish'(Na, Ar, M),
	fail.
'$abolish_all_atoms_old'(_,_).

'$abolishd'(T, M) :-
	'$is_multifile'(T,M),
	functor(T,Name,Arity),
	recorded('$mf','$mf_clause'(_,Name,Arity,M,Ref),R),
	erase(R),
	erase(Ref),
	fail.
'$abolishd'(T, M) :-
	recorded('$import','$import'(_,M,_,T,_,_),R),
	erase(R),
	fail.
'$abolishd'(T, M) :-
	'$purge_clauses'(T,M), fail.
'$abolishd'(T, M) :-
	'$kill_dynamic'(T,M), fail.
'$abolishd'(_, _).

'$abolishs'(G, M) :- '$system_predicate'(G,M), !,
	functor(G,Name,Arity),
	'$do_error'(permission_error(modify,static_procedure,Name/Arity),abolish(M:G)).
'$abolishs'(G, Module) :-
	'$access_yap_flags'(8, 2), % only do this in sicstus mode
	'$undefined'(G, Module),
	functor(G,Name,Arity),
	print_message(warning,no_match(abolish(Module:Name/Arity))).
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

/** @pred  dynamic_predicate(+ _P_,+ _Semantics_) 


Declares predicate  _P_ or list of predicates [ _P1_,..., _Pn_]
as a dynamic predicate following either `logical` or
`immediate` semantics.

 
*/
dynamic_predicate(P,Sem) :-
	'$bad_if_is_semantics'(Sem, dynamic(P,Sem)).
dynamic_predicate(P,Sem) :-
	'$log_upd'(OldSem),
	( Sem = logical -> '$switch_log_upd'(1) ; '$switch_log_upd'(0) ),
	'$current_module'(M),
	'$dynamic'(P, M),
	'$switch_log_upd'(OldSem).

'$bad_if_is_semantics'(Sem, Goal) :-
	var(Sem), !,
	'$do_error'(instantiation_error,Goal).
'$bad_if_is_semantics'(Sem, Goal) :-
	Sem \= immediate, Sem \= logical, !,
	'$do_error'(domain_error(semantics_indicator,Sem),Goal).


'$expand_clause'((H:-B),C1,C2,Mod,HM) :- !,
	strip_module(Mod:H, HM, H1),
	% Mod has scope over the full clause
	'$module_expansion'((H1:-B), C1, C2, HM, Mod, Mod),
	( get_value('$strict_iso',on) ->
	    '$check_iso_strict_clause'(C1)
        ;
           true
        ).
'$expand_clause'(H,H1,H1,Mod,HM) :-
	strip_module(Mod:H, HM, H1).

'$is_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$is_public'(T, Mod) :-
	'$flags'(T,Mod,F,F),
	F\/0x00400000 =\= 0.

/**  @pred stash_predicate(+ _Pred_) @anchor stash_predicate
Make predicate  _Pred_ invisible to new code, and to `current_predicate/2`,
`listing`, and friends. New predicates with the same name and
functor can be declared.
 **/
stash_predicate(V) :- var(V), !,
	'$do_error'(instantiation_error,stash_predicate(V)).
stash_predicate(M:P) :- !,
	'$stash_predicate2'(P, M).
stash_predicate(P) :-
	'$current_module'(M),
	'$stash_predicate2'(P, M).

'$stash_predicate2'(V, M) :- var(V), !,
	'$do_error'(instantiation_error,stash_predicate(M:V)).
'$stash_predicate2'(N/A, M) :- !,
	functor(S,N,A),
	'$stash_predicate'(S, M) .
'$stash_predicate2'(PredDesc, M) :-
	'$do_error'(type_error(predicate_indicator,PredDesc),stash_predicate(M:PredDesc)).

/** @pred @pred hide_predicate(+ _Pred_)
Make predicate  _Pred_ invisible to `current_predicate/2`,
`listing`, and friends.

 **/
hide_predicate(V) :- var(V), !,
	'$do_error'(instantiation_error,hide_predicate(V)).
hide_predicate(M:P) :- !,
	'$hide_predicate2'(P, M).
hide_predicate(P) :-
	'$current_module'(M),
	'$hide_predicate2'(P, M).

'$hide_predicate2'(V, M) :- var(V), !,
	'$do_error'(instantiation_error,hide_predicate(M:V)).
'$hide_predicate2'(N/A, M) :- !,
	functor(S,N,A),
	'$hide_predicate'(S, M) .
'$hide_predicate2'(PredDesc, M) :-
	'$do_error'(type_error(predicate_indicator,PredDesc),hide_predicate(M:PredDesc)).

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


 
*/
predicate_property(Pred,Prop) :- var(Pred), !,
	'$current_module'(Mod),
	'$predicate_property2'(Pred,Prop,Mod).
predicate_property(Mod:Pred,Prop) :- !,
	'$predicate_property2'(Pred,Prop,Mod).
predicate_property(Pred,Prop) :- 
	'$current_module'(Mod),
	'$predicate_property2'(Pred,Prop,Mod).

'$predicate_property2'(Pred,Prop,M) :- var(M), !,
	'$all_current_modules'(M),
	'$predicate_property2'(Pred,Prop,M).
'$predicate_property2'(Pred,Prop,M0) :- var(Pred), !,
	(M = M0 ; M = prolog), % prolog mode is automatically incorporate in every other module
	'$generate_all_preds_from_mod'(Pred, SourceMod, M),
	'$predicate_property'(Pred,SourceMod,M,Prop).
'$predicate_property2'(M:Pred,Prop,_) :- !,
	'$predicate_property2'(Pred,Prop,M).
'$predicate_property2'(Pred,Prop,Mod) :- 
	'$pred_exists'(Pred,Mod), !,
	'$predicate_property'(Pred,Mod,Mod,Prop).
'$predicate_property2'(Pred,Prop,Mod) :- 
	'$imported_pred'(Pred, Mod, NPred, M),
	(
	 Prop = imported_from(M)
	;
	 '$predicate_property'(NPred,M,M,Prop),
	 Prop \= exported
	).

'$generate_all_preds_from_mod'(Pred, M, M) :-
	'$current_predicate'(M,Na,Ar),
	'$ifunctor'(Pred,Na,Ar).
'$generate_all_preds_from_mod'(Pred, SourceMod, Mod) :-
	recorded('$import','$import'(SourceMod, Mod, Orig, Pred,_,_),_),
	'$pred_exists'(Orig, SourceMod).

'$predicate_property'(P,M,_,built_in) :- 
	'$system_predicate'(P,M).
'$predicate_property'(P,M,_,source) :- 
	'$flags'(P,M,F,F),
	F /\ 0x00400000 =\= 0.
'$predicate_property'(P,M,_,tabled) :- 
	'$flags'(P,M,F,F),
	F /\ 0x00000040 =\= 0.
'$predicate_property'(P,M,_,dynamic) :-
	'$is_dynamic'(P,M).
'$predicate_property'(P,M,_,static) :-
	\+ '$is_dynamic'(P,M),
	\+ '$undefined'(P,M).
'$predicate_property'(P,M,_,meta_predicate(Q)) :-
	functor(P,Na,Ar),
	'$meta_predicate'(Na,M,Ar,Q).
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
'$predicate_property'(P,Mod,_,number_of_clauses(NCl)) :-
	'$number_of_clauses'(P,Mod,NCl).
'$predicate_property'(P,Mod,_,file(F)) :-
	'$owner_file'(P,Mod,F).


/** @pred  predicate_statistics( _P_, _NCls_, _Sz_, _IndexSz_)  


Given predicate  _P_,  _NCls_ is the number of clauses for
 _P_,  _Sz_ is the amount of space taken to store those clauses
(in bytes), and  _IndexSz_ is the amount of space required to store
indices to those clauses (in bytes).

 
*/
predicate_statistics(V,NCls,Sz,ISz) :- var(V), !,
	'$do_error'(instantiation_error,predicate_statistics(V,NCls,Sz,ISz)).
predicate_statistics(M:P,NCls,Sz,ISz) :- !,
	'$predicate_statistics'(P,M,NCls,Sz,ISz).
predicate_statistics(P,NCls,Sz,ISz) :-
	'$current_module'(M),
	'$predicate_statistics'(P,M,NCls,Sz,ISz).

'$predicate_statistics'(M:P,_,NCls,Sz,ISz) :- !,
	'$predicate_statistics'(P,M,NCls,Sz,ISz).
'$predicate_statistics'(P,M,NCls,Sz,ISz) :-
	'$is_log_updatable'(P, M), !,
	'$lu_statistics'(P,NCls,Sz,ISz,M).
'$predicate_statistics'(P,M,_,_,_) :-
	'$system_predicate'(P,M), !, fail.
'$predicate_statistics'(P,M,_,_,_) :-
	'$undefined'(P,M), !, fail.
'$predicate_statistics'(P,M,NCls,Sz,ISz) :-
	'$static_pred_statistics'(P,M,NCls,Sz,ISz).

/** @pred  predicate_erased_statistics( _P_, _NCls_, _Sz_, _IndexSz_)  


Given predicate  _P_,  _NCls_ is the number of erased clauses for
 _P_ that could not be discarded yet,  _Sz_ is the amount of space
taken to store those clauses (in bytes), and  _IndexSz_ is the amount
of space required to store indices to those clauses (in bytes).




 */
predicate_erased_statistics(P,NCls,Sz,ISz) :-
        var(P), !, 
	current_predicate(_,P),
	predicate_erased_statistics(P,NCls,Sz,ISz).
predicate_erased_statistics(M:P,NCls,Sz,ISz) :- !,
	'$predicate_erased_statistics'(M:P,NCls,Sz,_,ISz).
predicate_erased_statistics(P,NCls,Sz,ISz) :-
	'$current_module'(M),
	'$predicate_erased_statistics'(M:P,NCls,Sz,_,ISz).

/** @pred  current_predicate( _A_, _P_)

Defines the relation:  _P_ is a currently defined predicate whose
name is the atom  _A_.

 
*/
current_predicate(A,T) :-
	var(T), !,		% only for the predicate
	'$current_module'(M),
	'$current_predicate_no_modules'(M,A,T).
current_predicate(A,M:T) :-			% module unspecified
	var(M), !,
	'$current_predicate_var'(A,M,T).
current_predicate(A,M:T) :- % module specified
	nonvar(T),
	!,
	functor(T,A,_),
	'$pred_exists'(T,M).
current_predicate(A,M:T) :- % module specified
	!,
	'$current_predicate_no_modules'(M,A,T).
current_predicate(A,T) :-			% only for the predicate
	'$current_module'(M),
	'$current_predicate_no_modules'(M,A,T).

'$current_predicate_var'(A,M,T) :-
	var(T), !,
	current_module(M),
	M \= prolog,
	'$current_predicate_no_modules'(M,A,T).
'$current_predicate_var'(A,M,T) :-
	functor(T,A,_),
	current_module(M),
	M \= prolog,
	'$pred_exists'(T,M).

/** @pred  system_predicate( _A_, _P_) 


Defines the relation:   _P_ is a built-in predicate whose name
is the atom  _A_.

 
*/
system_predicate(A,P) :-
	'$current_predicate_no_modules'(prolog,A,P),
	\+ '$hidden'(A).

system_predicate(P) :-
	'$current_module'(M),
	'$system_predicate'(P,M).

'$current_predicate_no_modules'(M,A,T) :-
	'$current_predicate'(M,A,Arity),
	'$ifunctor'(T,A,Arity),
	'$pred_exists'(T,M).

/** @pred  current_predicate( _F_) is iso 


 _F_ is the predicate indicator for a currently defined user or
library predicate.  _F_ is of the form  _Na/Ar_, where the atom
 _Na_ is the name of the predicate, and  _Ar_ its arity.

 
*/
current_predicate(F0) :-
	'$yap_strip_module'(F0, M, F),
	'$$current_predicate'(F, M).

'$$current_predicate'(F, M) :-
        ( var(M) ->			% only for the predicate
	'$all_current_modules'(M)
	; true),
	M \= prolog,
	'$current_predicate3'(F,M).

'$current_predicate3'(A/Arity,M) :-
	nonvar(A), nonvar(Arity), !,
	( '$ifunctor'(T,A,Arity),
	 '$pred_exists'(T,M)
	->
	 true
	;
%	 '$current_predicate'(prolog,A,Arity)
%	->
%	 functor(T,A,Arity),
%	'$pred_exists'(T,M)
%	;
	 recorded('$import','$import'(NM,M,G,T,A,Arity),_)
	->
	'$pred_exists'(G,NM)
	).
'$current_predicate3'(A/Arity,M) :- !,
	(
	 '$current_predicate'(M,A,Arity),
	 '$ifunctor'(T,A,Arity),
	 '$pred_exists'(T,M)
	;
%	 '$current_predicate'(prolog,A,Arity),
%	 functor(T,A,Arity),
%	'$pred_exists'(T,M)
%	;
	 recorded('$import','$import'(NM,M,G,T,A,Arity),_),
	 functor(T,A,Arity),
	'$pred_exists'(G,NM)
	).
'$current_predicate3'(BadSpec,M) :-			% only for the predicate
	'$do_error'(type_error(predicate_indicator,BadSpec),current_predicate(M:BadSpec)).

/** @pred  current_key(? _A_,? _K_) 


Defines the relation:  _K_ is a currently defined database key whose
name is the atom  _A_. It can be used to generate all the keys for
the internal data-base.

 
*/
current_key(A,K) :-
	'$current_predicate'(idb,A,Arity),
	'$ifunctor'(K,A,Arity).

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
	findall((G.B),clause(Mod:G,B),Cls),
	abolish(Mod:Na,Ar),
	'$add_all'(Cls, Mod).

'$add_all'([], _).
'$add_all'((G.B).Cls, Mod) :-
	assert_static(Mod:(G:-B)),
	'$add_all'(Cls, Mod).


clause_property(ClauseRef, file(FileName)) :-
	( recorded('$mf','$mf_clause'(FileName,_Name,_Arity,_Module,ClauseRef),_R)
	-> true
	;
	'$instance_property'(ClauseRef, 2, FileName) ).
clause_property(ClauseRef, source(FileName)) :-
	( recorded('$mf','$mf_clause'(FileName,_Name,_Arity,_Module,ClauseRef),_R)
	-> true
	;
	'$instance_property'(ClauseRef, 2, FileName) ).
clause_property(ClauseRef, line_count(LineNumber)) :-
	'$instance_property'(ClauseRef, 4, LineNumber),
	LineNumber > 0.
clause_property(ClauseRef, fact) :-
	'$instance_property'(ClauseRef, 3, true).
clause_property(ClauseRef, erased) :-
	'$instance_property'(ClauseRef, 0, true).
clause_property(ClauseRef, predicate(PredicateIndicator)) :-
	'$instance_property'(ClauseRef, 1, PredicateIndicator).

'$set_predicate_attribute'(M:N/Ar, Flag, V) :-
	functor(P, N, Ar),
	'$set_flag'(P, M, Flag, V).


/**
@}
*/

