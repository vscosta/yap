% The next predicates are applicable only
% to dynamic code

/** @file preddyns.yap */

/**
 * @ingroup Database
 * @{

Next follow the main operations on dynamic predicates.

*/

/** @pred  asserta(+ _C_) is iso


Adds clause  _C_ to the beginning of the program. If the predicate is
undefined, it is declared  dynamic (see dynamic/1).

*/
asserta(C) :-
	strip_module(C, Mod, NC),
	'$assert'(NC,Mod,first,_,asserta(C)).

/** @pred  assertz(+ _C_) is iso


Adds clause  _C_ to the end of the program. If the predicate is
undefined, it is declared  dynamic (see dynamic/1).

Most Prolog systems only allow asserting clauses for dynamic
predicates. This is also as specified in the ISO standard. YAP also allows
asserting clauses for static predicates, under the restriction that the static predicate may not be live in the stacks.
*/
assertz(C) :-
	strip_module(C,Mod,C1),
	'$assert'(C1,Mod,last,_,assertz(C)).

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
assert(C) :-
	strip_module(C,Mod,C1),
	'$assert'(C1,Mod,last,_,assert(C)).

'$assert'(V,Mod,_,_,_) :- var(V), !,
	'$do_error'(instantiation_error,assert(Mod:V)).
'$assert'(V,Mod,_,_,_) :- var(Mod), !,
	'$do_error'(instantiation_error,assert(Mod:V)).
'$assert'(I,Mod,_,_,_) :- number(I), !,
	'$do_error'(type_error(callable,I),assert(Mod:I)).
'$assert'(M:C,_,Where,R,P) :- !,
	strip_module(M:C, M1, C1),
	'$assert'(C1,M1,Where,R,P).
'$assert'((H:-G),M,Where,R,P) :- !,
	'$assert_clause'(H, G, M, Where, R, P).
'$assert'(H,M,Where,R,_) :-
	'$assert_fact'(H, M, Where, R).

'$assert_fact'(H,Mod,Where,R) :-
	functor(H, Na, Ar),
	( '$undefined'(H,Mod) ->
	    '$dynamic'(Na/Ar, Mod)
		;
		true
	),
	( '$is_log_updatable'(H, Mod) ->
	    '$compile_dynamic'(H, Where, H, Mod, R)
    ;
	 	'$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where, H, true, H, Mod, R)
	;
	% try asserting as static, see what happens
		Where = last ->
		assert_static(Mod:H)
	;
		asserta_static(Mod:H)
	).

'$assert_clause'(H, _, _, _, _, P) :-
		var(H), !,
		'$do_error'(instantiation_error,P).
'$assert_clause'(M:C, G, MG, Where, R, P) :-
		!,
		strip_module(M:C, M1, C1),
		'$assert_clause2'(C1, MG:G, M1, Where, R, P).
'$assert_clause'(H1, B1, Mod, Where, R, P) :-
	'$expand_clause'((H1 :- B1),C0,C,Mod,Mod),
	'$check_head_and_body'(C,H,B,P),
	( '$is_log_updatable'(H, Mod) ->
            '$compile_dynamic'((H :- B), Where, C0, Mod, R)
	;
          '$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where, H, B, C0, Mod, R)
	;
		Where = last
		->
		assert_static(Mod:(H :- B))
	;
		asserta_static(Mod:(H :- B))
	).

/** @pred  asserta(+ _C_,- _R_)

The same as `asserta(C)` but unifying  _R_ with
the  database reference that identifies the new clause, in a
one-to-one way. Note that `asserta/2` only works for dynamic
predicates. If the predicate is undefined, it will automatically be
declared dynamic.

*/
asserta(C,R) :-
	strip_module(C, M, C1),
	'$assert'(C1,M,first,R,asserta(C,R)).

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

'$assertat_d'(first,Head,Body,C0,Mod,R) :- !,
	'$compile_dynamic'((Head:-Body), first, C0, Mod, CR),
         ( get_value('$abol',true)
           ->
            '$predicate_flags'(Head,Mod,Fl,Fl),
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
            '$predicate_flags'(Head,Mod,Fl,Fl),
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
	'$predicate_flags'(H, M, F, F),
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

/* support for abolish/1 */
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
