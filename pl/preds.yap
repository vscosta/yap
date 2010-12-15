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

% The next predicates are applicable only
% to dynamic code

asserta(Mod:C) :- !,
	'$assert'(C,Mod,first,_,asserta(Mod:C)).
asserta(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,first,_,asserta(C)).

assertz(Mod:C) :- !,
	'$assert'(C,Mod,last,_,assertz(Mod:C)).
assertz(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,last,_,assertz(C)).

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
	'$is_log_updatable'(H, Mod), !,
	'$compile_dynamic'(H, Where, H, Mod, R).
'$assert_fact'(H,Mod,Where,R) :-
	( '$is_dynamic'(H, Mod) ->
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
'$assert_dynamic'((H:-G),M1,Where,R,P) :-
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

assert_static(Mod:C) :- !,
	'$assert_static'(C,Mod,last,_,assert_static(Mod:C)).
assert_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,last,_,assert_static(C)).

asserta_static(Mod:C) :- !,
	'$assert_static'(C,Mod,first,_,asserta_static(Mod:C)).
asserta_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,first,_,asserta_static(C)).

asserta_static(Mod:C) :- !,
	'$assert_static'(C,Mod,last,_,assertz_static(Mod:C)).
assertz_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,last,_,assertz_static(C)).

'$assert_static'(V,M,_,_,_) :- var(V), !,
	'$do_error'(instantiation_error,assert(M:V)).
'$assert_static'(M:C,_,Where,R,P) :- !,
	'$assert_static'(C,M,Where,R,P).
'$assert_static'((H:-G),M1,Where,R,P) :-
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
	    ( Fl /\ 0x00400000 =\= 0 -> '$erase_source'(Head,Mod) ; true ),
	    ( Fl /\ 0x20000000 =\= 0 -> '$check_multifile_pred'(Head,Mod,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordap'(Mod:Head,(H0 :- B0),R,CR),
	( '$is_multifile'(Head, Mod) ->
	    nb_getval('$consulting_file',F),
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
            ( Fl /\ 0x00400000 =\= 0 -> '$erase_source'(Head,Mod) ; true ),
	    ( Fl /\ 0x20000000 =\= 0 -> '$check_multifile_pred'(Head,Mod,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordzp'(Mod:Head,(H0 :- B0),R,CR),
	( '$is_multifile'(H0, Mod) ->
	    get_value('$consulting_file',F),
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
	get_value('$consulting_file',F),
	recorded('$multifile_dynamic'(_,_,_), '$mf'(Na,A,M,F,R), R1),
	erase(R1),
	erase(R),
	fail.
'$erase_all_mf_dynamic'(_,_,_).

asserta(M:C,R) :- !,
	'$assert_dynamic'(C,M,first,R,asserta(M:C,R)).
asserta(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,first,R,asserta(C,R)).

assertz(M:C,R) :- !,
	'$assert_dynamic'(C,M,last,R,assertz(M:C,R)).
assertz(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,last,R,assertz(C,R)).

assert(M:C,R) :- !,
	'$assert_dynamic'(C,M,last,R,assert(M:C,R)).
assert(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,last,R,assert(C,R)).

clause(M:P,Q) :- !,
	'$clause'(P,M,Q,_).
clause(V,Q) :-
	'$current_module'(M),
	'$clause'(V,M,Q,_).

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
'$clause'(C,M,Q,R) :- number(C), !,
	'$do_error'(type_error(callable,C),clause(M:C,Q,R)).
'$clause'(R,M,Q,R) :- db_reference(R), !,
	'$do_error'(type_error(callable,R),clause(M:R,Q,R)).
'$clause'(M:P,_,Q,R) :- !,
	'$clause'(P,M,Q,R).
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

% just create a choice-point
% the 6th argument marks the time-stamp.
'$do_log_upd_clause'(_,_,_,_,_,_).
'$do_log_upd_clause'(A,B,C,D,E,_) :-
	'$continue_log_update_clause'(A,B,C,D,E).
'$do_log_upd_clause'(_,_,_,_,_,_).

:- '$do_log_upd_clause'(_,_,_,_,_,_), !.

'$do_log_upd_clause_erase'(_,_,_,_,_,_).
'$do_log_upd_clause_erase'(A,B,C,D,E,_) :-
	'$continue_log_update_clause_erase'(A,B,C,D,E).
'$do_log_upd_clause_erase'(_,_,_,_,_,_).

:- '$do_log_upd_clause_erase'(_,_,_,_,_,_), !.

'$do_log_upd_clause0'(_,_,_,_,_,_).
'$do_log_upd_clause0'(A,B,C,D,_,_) :-
	'$continue_log_update_clause'(A,B,C,D).
'$do_log_upd_clause0'(_,_,_,_,_,_).

:- '$do_log_upd_clause0'(_,_,_,_,_,_), !.

'$do_static_clause'(_,_,_,_,_).
'$do_static_clause'(A,B,C,D,E) :-
	'$continue_static_clause'(A,B,C,D,E).
'$do_static_clause'(_,_,_,_,_).

:- '$do_static_clause'(_,_,_,_,_), !.

nth_clause(V,I,R) :- var(V), var(R), !,
	'$do_error'(instantiation_error,nth_clause(V,I,R)).
nth_clause(M:V,I,R) :- !,
	'$nth_clause'(V,M,I,R).
nth_clause(V,I,R) :-
	'$current_module'(M),
	'$nth_clause'(V,M,I,R).


'$nth_clause'(V,M,I,R) :- var(V), var(R), !, 
	'$do_error'(instantiation_error,M:nth_clause(V,I,R)).
'$nth_clause'(P1,_,I,R) :- nonvar(P1), P1 = M:P, !,
	'$nth_clause'(P,M,I,R).
'$nth_clause'(P,M,I,R) :- nonvar(R), !,
	'$nth_clause_ref'(P,M,I,R).
'$nth_clause'(C,M,I,R) :- number(C), !,
	'$do_error'(type_error(callable,C),M:nth_clause(C,I,R)).
'$nth_clause'(R,M,I,R) :- db_reference(R), !,
	'$do_error'(type_error(callable,R),M:nth_clause(R,I,R)).
'$nth_clause'(P,M,I,R) :-
	( '$is_log_updatable'(P,M) ; '$is_source'(P,M) ), !,
	'$p_nth_clause'(P,M,I,R).
'$nth_clause'(P,M,I,R) :-
	'$is_dynamic'(P,M), !,
	'$nth_instancep'(M:P,I,R).
'$nth_clause'(P,M,I,R) :-
	( '$system_predicate'(P,M) -> true ;
	    '$number_of_clauses'(P,M,N), N > 0 ),
	functor(P,Name,Arity),
	'$do_error'(permission_error(access,private_procedure,Name/Arity),
	      nth_clause(M:P,I,R)).

'$nth_clause_ref'(Cl,M,I,R) :-
	'$pred_for_code'(R, _, _, M1, I), I > 0, !,
	instance(R, Cl),
	M1 = M.
'$nth_clause_ref'(P,M,I,R) :-
	'$nth_instancep'(M:P,I,R).

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
	( F /\ 0x20000000  =:= 0x20000000, recorded('$mf','$mf_clause'(_,_,_,_,MRef),MR), erase(MR), fail ; true),
	erase(R).
'$retract2'(_, H,M,_,_) :- 	
	'$undefined'(H,M), !,
	functor(H,Na,Ar),
	'$dynamic'(Na/Ar,M),
	fail.
'$retract2'(_, H,M,B,_) :- 	
	functor(H,Na,Ar),
	'$do_error'(permission_error(modify,static_procedure,Na/Ar),retract(M:(H:-B))).

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

abolish(V) :- var(V), !,
	'$do_error'(instantiation_error,abolish(V)).
abolish(Mod:V) :- var(V), !,
	'$do_error'(instantiation_error,abolish(M:V)).
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
	'$is_multifile'(G,M), !,
	functor(G,Name,Arity),
	recorded('$mf','$mf_clause'(_,Name,Arity,M,Ref),R),
	erase(R),
	erase(Ref),
	fail.
'$abolishs'(T, M) :-
	recorded('$import','$import'(_,M,_,_,T,_,_),R),
	erase(R),
	fail.
'$abolishs'(G, M) :-
	'$purge_clauses'(G, M), fail.
'$abolishs'(_, _).

%
% can only do as goal in YAP mode.
%
dynamic(X) :- '$access_yap_flags'(8, 0), !,
        '$current_module'(M),
	'$dynamic'(X, M).
dynamic(X) :-
	'$do_error'(context_error(dynamic(X),declaration),query).

'$dynamic'(X,M) :- var(X), !,
	'$do_error'(instantiation_error,dynamic(M:X)).
'$dynamic'(Mod:Spec,_) :- !,
	'$dynamic'(Spec,Mod).
'$dynamic'([], _) :- !.
'$dynamic'([H|L], M) :- !, '$dynamic'(H, M), '$dynamic'(L, M).
'$dynamic'((A,B),M) :- !, '$dynamic'(A,M), '$dynamic'(B,M).
'$dynamic'(X,M) :-
	'$dynamic2'(X,M).

'$dynamic2'(X, Mod) :- '$log_upd'(Stat), Stat\=0, !,
	'$logical_updatable'(X, Mod).
'$dynamic2'(A//N1, Mod) :-
	integer(N1),
	N is N1+2,
	'$dynamic2'(A/N, Mod).
'$dynamic2'(A/N, Mod) :-
	integer(N), atom(A), !,
	functor(T,A,N), '$flags'(T,Mod,F,F),
	% LogUpd,BinaryTest,Safe,C,Dynamic,Compiled,Standard,Asm,
	( F/\ 0x19D1FA80 =:= 0, '$undefined'(T,Mod) -> NF is F \/ 0x00002000, '$flags'(T, Mod, F, NF), '$mk_d'(T,Mod);
	    F /\ 0x00002000 =:= 0x00002000 -> '$mk_d'(T,Mod);                     % dynamic
	    F /\ 0x08000000 =:= 0x08000000 -> '$mk_d'(T,Mod) ;      % LU
	    F /\ 0x00000400 =:= 0x00000400, '$undefined'(T,Mod) -> F1 is F /\ \(0x400), N1F is F1 \/ 0x00002000, NF is N1F /\ \(0x00400000), '$flags'(T,Mod,F,NF), '$mk_d'(T,Mod);
	    '$do_error'(permission_error(modify,static_procedure,A/N),dynamic(Mod:A/N))
	).
'$dynamic2'(X,Mod) :- 
	'$do_error'(type_error(callable,X),dynamic(Mod:X)).


'$logical_updatable'(A//N,Mod) :- integer(N), !,
	N1 is N+2,
	'$logical_updatable'(A/N1,Mod).
'$logical_updatable'(A/N,Mod) :- integer(N), atom(A), !,
	functor(T,A,N), '$flags'(T,Mod,F,F),
	(
	    F/\ 0x19D1FA80 =:= 0, '$undefined'(T,Mod) -> NF is F \/ 0x08000400, '$flags'(T,Mod,F,NF), '$mk_d'(T,Mod);
	    F /\ 0x08000000 =:= 0x08000000 -> '$mk_d'(T,Mod) ;      % LU
	    F /\ 0x00002000 =:= 0x00002000 -> '$mk_d'(T,Mod);      % dynamic
	    F /\ 0x00000400 =:= 0x00000400 , '$undefined'(T,Mod) -> N1F is F \/ 0x08000000, NF is N1F /\ \(0x00400000), '$flags'(T,Mod,F,NF), '$mk_d'(T,Mod);
	    '$do_error'(permission_error(modify,static_procedure,A/N),dynamic(Mod:A/N))
	).
'$logical_updatable'(X,Mod) :- 
	'$do_error'(type_error(callable,X),dynamic(Mod:X)).


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
	'$module_expansion'((H1:-B), C1, C2, Mod, HM),
	( get_value('$strict_iso',on) ->
	    '$check_iso_strict_clause'(C1)
        ;
           true
        ).
'$expand_clause'(H,H1,H1,Mod,HM) :-
	strip_module(Mod:H, HM, H1).

'$public'(X, _) :- var(X), !,
	'$do_error'(instantiation_error,public(X)).
'$public'(Mod:Spec, _) :- !,
	'$public'(Spec,Mod).
'$public'((A,B), M) :- !, '$public'(A,M), '$public'(B,M).
'$public'([],_) :- !.
'$public'([H|L], M) :- !, '$public'(H, M), '$public'(L, M).
'$public'(A//N1, Mod) :- integer(N1), !,
	N is N1+2,
	'$public'(A//N, Mod).
'$public'(A/N, Mod) :- integer(N), atom(A), !,
	functor(T,A,N),
	'$do_make_public'(T, Mod).
'$public'(X, Mod) :- 
	'$do_error'(type_error(callable,X),dynamic(Mod:X)).

'$do_make_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$do_make_public'(T, Mod) :-
	'$flags'(T,Mod,F,F),
	NF is F\/0x00400000,
	'$flags'(T,Mod,F,NF).

'$is_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$is_public'(T, Mod) :-
	'$flags'(T,Mod,F,F),
	F\/0x00400000 =\= 0.

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
	recorded('$import','$import'(SourceMod,Mod,_,Pred,_,_),_).


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
'$predicate_property'(P,M,M,exported) :-
	functor(P,N,A),
	once(recorded('$module','$module'(_TFN,M,Publics),_)),
	lists:memberchk(N/A,Publics).
'$predicate_property'(P,Mod,_,number_of_clauses(NCl)) :-
	'$number_of_clauses'(P,Mod,NCl).


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

predicate_erased_statistics(P,NCls,Sz,ISz) :-
	current_predicate(_,P),
	predicate_erased_statistics(P,NCls,Sz,ISz).
predicate_erased_statistics(M:P,NCls,Sz,ISz) :- !,
	'$predicate_erased_statistics'(M:P,NCls,Sz,_,ISz).
predicate_erased_statistics(P,NCls,Sz,ISz) :-
	'$current_module'(M),
	'$predicate_erased_statistics'(M:P,NCls,Sz,_,ISz).

current_predicate(A,T) :-
	'$current_predicate_inside'(A,T).

'$current_predicate_inside'(A,T) :-
	var(T), !,		% only for the predicate
	'$current_module'(M),
	'$current_predicate_no_modules'(M,A,T).
'$current_predicate_inside'(A,M:T) :-			% module specified
	var(M), !,
	'$current_predicate_var'(A,M,T).
'$current_predicate_inside'(A,M:T) :- % module specified
	nonvar(T),
	!,
	functor(T,A,_),
	'$pred_exists'(T,M).
'$current_predicate_inside'(A,M:T) :- % module specified
	!,
	'$current_predicate_no_modules'(M,A,T).
'$current_predicate_inside'(A,T) :-			% only for the predicate
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

current_predicate(A) :-
	'$current_predicate_inside'(A).

'$current_predicate_inside'(F) :-
	var(F), !,		% only for the predicate
	'$current_module'(M),
	'$current_predicate3'(M,F).
'$current_predicate_inside'(M:F) :-			% module specified
	var(M), !,
	'$current_module'(M),
	M \= prolog,
	'$current_predicate3'(M,F).
'$current_predicate_inside'(M:F) :- % module specified
	!,
	'$current_predicate3'(M,F).
'$current_predicate_inside'(S) :-			% only for the predicate
	'$current_module'(M),
	'$current_predicate3'(M,S).
	
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

'$current_predicate3'(M,A/Arity) :-
	nonvar(M),
	nonvar(A),
	nonvar(Arity), !,
	'$ifunctor'(Pred,A,Arity),
	'$pred_exists'(Pred,M).
'$current_predicate3'(M,A/Arity) :-
	nonvar(A), nonvar(Arity), !,
	(
	 '$current_predicate'(M,A,Arity)
	->
	'$ifunctor'(T,A,Arity),
	'$pred_exists'(T,M)
%	;
%	 '$current_predicate'(prolog,A,Arity)
%	->
%	 functor(T,A,Arity),
%	'$pred_exists'(T,M)
%	;
%	 recorded('$import','$import'(NM,M,G,T,A,Arity),_)
%	->
%	'$pred_exists'(G,NM)
	).
'$current_predicate3'(M,A/Arity) :- !,
	(
	 '$current_predicate'(M,A,Arity),
	 '$ifunctor'(T,A,Arity),
	 '$pred_exists'(T,M)
%	;
%	 '$current_predicate'(prolog,A,Arity),
%	 functor(T,A,Arity),
%	'$pred_exists'(T,M)
%	;
%	 recorded('$import','$import'(NM,M,G,T,A,Arity),_),
%	 functor(T,A,Arity),
%	'$pred_exists'(G,NM)
	).
'$current_predicate3'(M,BadSpec) :-			% only for the predicate
	'$do_error'(type_error(predicate_indicator,BadSpec),current_predicate(M:BadSpec)).

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
