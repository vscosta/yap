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

asserta(V) :- var(V), !,
	throw(error(instantiation_error,asserta(V))).
asserta(C) :- '$assert'(C,first,_,asserta(C)).

assertz(V) :- var(V), !,
	throw(error(instantiation_error,assertz(V))).
assertz(C) :- '$assert'(C,last,_,assertz(C)).

assert(V) :- var(V), !,
	throw(error(instantiation_error,assert(V))).
assert(C) :- '$assert'(C,last,_,assert(C)).

'$assert'(V,_,_,_) :- var(V), !,
	throw(error(instantiation_error,assert(V))).
'$assert'(M:C,Where,R,P) :- !,
	'$mod_switch'(M,'$assert'(C,Where,R,P)).
'$assert'((H:-G),Where,R,P) :- (var(H) -> throw(error(instantiation_error,P)) ;  H=M:C), !,
	'$current_module'(M1),
	( M1 = M ->
	    '$assert'((C:-G),Where,R,P)
	;
	    '$preprocess_clause_before_mod_change'((C:-G),M1,M,C1),
	    '$mod_switch'(M,'$assert'(C1,Where,R,P))
	).
'$assert'(CI,Where,R,P) :-
	'$expand_clause'(CI,C0,C),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H) ->
	    '$assertat_d'(Where,H,B,C0,R)
	;
	  '$undefined'(H) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar),
	    '$assertat_d'(Where,H,B,C0,R)
	;
            '$access_yap_flags'(14, 1) -> % I can assert over static facts in YAP mode
	    '$assert1'(Where,C,C0,H)
        ;
	    functor(H, Na, Ar),
            throw(error(permission_error(modify,static_procedure,Na/Ar),P))
	).


'$assert_dynamic'(V,_,_,_) :- var(V), !,
	throw(error(instantiation_error,assert(V))).
'$assert_dynamic'(M:C,Where,R,P) :- !,
	'$mod_switch'(M,'$assert_dynamic'(C,Where,R,P)).
'$assert_dynamic'((H:-G),Where,R,P) :- (var(H) -> throw(error(instantiation_error,P)) ;  H=M:C), !,
	'$current_module'(M1),
	( M1 = M ->
	    '$assert_dynamic'((C:-G),Where,R,P)
	;
	    '$preprocess_clause_before_mod_change'((C:-G),M1,M,C1),
	    '$mod_switch'(M,'$assert_dynamic'(C1,Where,R,P))
	).
'$assert_dynamic'(CI,Where,R,P) :-
	'$expand_clause'(CI,C0,C),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H) ->
	    '$assertat_d'(Where,H,B,C0,R)
	;
	  '$undefined'(H) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar),
	    '$assertat_d'(Where,H,B,C0,R)
	;
	    functor(H,Na,Ar),
	    throw(error(permission_error(modify,static_procedure,Na/Ar),P))
	).

assert_static(V) :- var(V), !,
	throw(error(instantiation_error,assert_static(V))).
assert_static(C) :- '$assert_static'(C,last,_,assert_static(C)).

asserta_static(V) :- var(V), !,
	throw(error(instantiation_error,asserta_static(V))).
asserta_static(C) :- '$assert_static'(C,first,_,asserta_static(C)).

assertz_static(V) :- var(V), !,
	throw(error(instantiation_error,assertz_static(V))).
assertz_static(C) :-
	'$assert_static'(C,last,_,assertz_static(C)).

'$assert_static'(V,_,_,_) :- var(V), !,
	throw(error(instantiation_error,assert(V))).
'$assert_static'(M:C,Where,R,P) :- !,
	'$mod_switch'(M,'$assert_static'(C,Where,R,P)).
'$assert_static'((H:-G),Where,R,P) :- (var(H) -> throw(error(instantiation_error,P)) ;  H=M:C), !,
	'$current_module'(M1),
	( M1 = M ->
	    '$assert_static'((C:-G),Where,R,P)
	;
	    '$preprocess_clause_before_mod_change'((C:-G),M1,M,C1),
	    '$mod_switch'(M,'$assert_static'(C1,Where,R,P))
	).
'$assert_static'(CI,Where,R,P) :-
	'$expand_clause'(CI,C0,C),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H) ->
	    throw(error(permission_error(modify,dynamic_procedure,Na/Ar),P))
	;
	  '$undefined'(H), '$get_value'('$full_iso',true) ->
	    functor(H,Na,Ar), '$dynamic'(Na/Ar), '$assertat_d'(Where,H,B,C0,R)
	;
	    '$assert1'(Where,C,C0,H)
        ).


'$assertat_d'(first,Head,Body,C0,R) :- !,
	'$compile_dynamic'((Head:-Body),2,CR),
         ( '$get_value'('$abol',true)
           ->
            '$flags'(H,Fl,Fl),
	    ( Fl /\ 16'400000 =\= 0 -> '$erase_source'(H) ; true ),
	    ( Fl /\ 16'040000 =\= 0 -> '$check_multifile_pred'(H,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordap'(Head,(H0 :- B0),R,CR),
	functor(Head,Na,Ar),
	( '$is_multifile'(Na,Ar) ->
	    '$get_value'('$consulting_file',F),
	    '$current_module'(M),
	    '$recorda'('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,M,F,R), _) 
	;
	    true
	).
'$assertat_d'(last,Head,Body,C0,R) :-
	'$compile_dynamic'((Head:-Body),0,CR),
         ( '$get_value'('$abol',true)
           ->
            '$flags'(H,Fl,Fl),
            ( Fl /\ 16'400000 =\= 0 -> '$erase_source'(H) ; true ),
	    ( Fl /\ 16'040000 =\= 0 -> '$check_multifile_pred'(H,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordzp'(Head,(H0 :- B0),R,CR),
	functor(H0,Na,Ar),
	( '$is_multifile'(Na,Ar) ->
	    '$get_value'('$consulting_file',F),
	    '$current_module'(M),
	    '$recordz'('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,M,F,R), _) 
	;
	    true
	).

'$assert1'(last,C,C0,H) :- '$$compile_stat'(C,C0,0,H).
'$assert1'(first,C,C0,H) :- '$$compile_stat'(C,C0,2,H).

'$assertz_dynamic'(X,C,C0) :- (X/\4)=:=0, !,
	'$head_and_body'(C,H,B),
	'$assertat_d'(last,H,B,C0,_).
'$assertz_dynamic'(X,C,C0) :- 
	'$head_and_body'(C,H,B), functor(H,N,A),
	('$check_if_reconsulted'(N,A) ->
		true
		 ;
	  (X/\8)=:=0 ->
		'$inform_as_reconsulted'(N,A),
		'$remove_all_d_clauses'(H)
		 ;
		true
	),
	'$assertat_d'(last,H,B,C0,_).

'$remove_all_d_clauses'(H) :-
	functor(H, Na, A),
	'$is_multifile'(Na,A), !,
	'$erase_all_mf_dynamic'(Na,A).
'$remove_all_d_clauses'(H) :-
	'$recordedp'(H,_,R), erase(R), fail.
'$remove_all_d_clauses'(_).

'$erase_all_mf_dynamic'(Na,A) :-
	'$get_value'('$consulting_file',F),
	'$current_module'(M),
	'$recorded'('$multifile_dynamic'(_,_,_), '$mf'(Na,A,M,F,R), R1),
	erase(R1),
	erase(R),
	fail.
'$erase_all_mf_dynamic'(_,_).

asserta(V,R) :- var(V), !,
	throw(error(instantiation_error,asserta(V,R))).
asserta(C,R) :- '$assert_dynamic'(C,first,R,asserta(C,R)).

assertz(V,R) :- var(V), !,
	throw(error(instantiation_error,assertz(V,R))).
assertz(C,R) :- '$assert_dynamic'(C,last,R,assertz(C,R)).

assert(V,R) :- var(V), !,
	throw(error(instantiation_error,assert(V,R))).
assert(C,R) :- '$assert_dynamic'(C,last,R,assert(C,R)).

clause(V,Q) :- var(V), !, 
	throw(error(instantiation_error,clause(V,Q))).
clause(C,Q) :- number(C), !,
	throw(error(type_error(callable,C),clause(C,Q))).
clause(R,Q) :- db_reference(R), !,
	throw(error(type_error(callable,R),clause(R,Q))).
clause(M:P,Q) :- !,
	'$mod_switch'(M,clause(P,Q)).
clause(P,Q) :- '$is_dynamic'(P), !,
	 '$recordedp'(P,(P:-Q),_). 
clause(P,Q) :-	
	'$some_recordedp'(P), !,
	 '$recordedp'(P,(P:-Q),_).
clause(P,Q) :-
	( '$system_predicate'(P) -> true ;
	    '$number_of_clauses'(P,N), N > 0 ),
	functor(P,Name,Arity),
	throw(error(permission_error(access,private_procedure,Name/Arity),
	      clause(P,Q))).

clause(V,Q,R) :- var(V), !, 
	throw(error(instantiation_error,clause(V,Q,R))).
clause(C,Q,R) :- number(C), !,
	throw(error(type_error(callable,C),clause(C,Q,R))).
clause(R,Q,R1) :- db_reference(R), !,
	throw(error(type_error(callable,R),clause(R,Q,R1))).
clause(M:P,Q,R) :- !,
	'$mod_switch'(M,clause(P,Q,R)).
clause(P,Q,R) :-
	 ( '$is_dynamic'(P) ->
	 	'$recordedp'(P,(P:-Q),R)
	 ;
	        functor(P,N,A),
		throw(error(permission_error(access,private_procedure,N/A),
			clause(P,Q,R)))
	 ).

retract(V) :- var(V), !,
	throw(error(instantiation_error,retract(V))).
retract(M:C) :- !,
	'$mod_switch'(M,retract(C)).
retract(C) :- 
	'$check_head_and_body'(C,H,B,retract(C)),
	'$is_dynamic'(H), !,
	'$recordedp'(H,(H:-B),R), erase(R).
retract(C) :-
	'$fetch_predicate_indicator_from_clause'(C, PI),
	throw(error(permission_error(modify,static_procedure,PI),retract(C))).

retract(V,R) :- var(V), !,
	throw(error(instantiation_error,retract(V,R))).
retract(M:C,R) :- !,
	'$mod_switch'(M,retract(C,R)).
retract(C,R) :-
	'$check_head_and_body'(C,H,B,retract(C,R)),
	db_reference(R), '$is_dynamic'(H), !,
	instance(R,(H:-B)), erase(R).
retract(C,R) :-
	'$head_and_body'(C,H,B,retract(C,R)),
	'$is_dynamic'(H), !,
	var(R),
	'$recordedp'(H,(H:-B),R),
	erase(R).
retract(C,_) :-
	'$fetch_predicate_indicator_from_clause'(C, PI),
	throw(error(permission_error(modify,static_procedure,PI),retract(C))).

'$fetch_predicate_indicator_from_clause'((C :- _), Na/Ar) :- !,
	functor(C, Na, Ar).
'$fetch_predicate_indicator_from_clause'(C, Na/Ar) :-
	functor(C, Na, Ar).
	

retractall(V) :- var(V), !,
	throw(error(instantiation_error,retract(V))).
retractall(M:V) :- !,
	'$mod_switch'(M,retractall(V)).
retractall(T) :- '$undefined'(T),
	functor(T, Na, Ar),
	'$dynamic'(Na/Ar),
	fail.
retractall(T) :- \+ '$is_dynamic'(T), !,
	functor(T,Na,Ar),
	throw(error(permission_error(modify,static_procedure,Na/Ar),retractall(T))).
retractall(T) :-
	'$erase_all_clauses_for_dynamic'(T).

'$erase_all_clauses_for_dynamic'(T) :-
	'$recordedp'(T,(T :- _),R), erase(R), fail.
'$erase_all_clauses_for_dynamic'(T) :-
	'$recordedp'(T,_,_), fail.
'$erase_all_clauses_for_dynamic'(_).

abolish(N,A) :- var(N), !,
	throw(error(instantiation_error,abolish(N,A))).
abolish(N,A) :- var(A), !,
	throw(error(instantiation_error,abolish(N,A))).
abolish(M:N,A) :- !,
	'$mod_switch'(M,abolish(N,A)).
abolish(N,A) :- 
	( '$recorded'('$predicate_defs','$predicate_defs'(N,A,_),R) -> erase(R) ),
	fail.
abolish(N,A) :- functor(T,N,A),
		( '$is_dynamic'(T) -> '$abolishd'(T) ;
	      	 /* else */	      '$abolishs'(T) ).

abolish(X) :- 
	'$access_yap_flags'(8, 2), !,
	'$new_abolish'(X).
abolish(X) :- 
	'$old_abolish'(X).

'$new_abolish'(V) :- var(V), !,
	'$abolish_all'.
'$new_abolish'(M:PS) :- !,
	'$mod_switch'(M,'$new_abolish'(PS)).
'$new_abolish'(Na/Ar) :-
	functor(H, Na, Ar),
	'$is_dynamic'(H), !,
	'$abolishd'(H).
'$new_abolish'(Na/Ar) :- % succeed for undefined procedures.
	functor(T, Na, Ar),
	'$undefined'(T), !.
'$new_abolish'(Na/Ar) :-
	'$current_module'(M),
	throw(error(permission_error(modify,static_procedure,Na/Ar),abolish(M:Na/Ar))).

'$abolish_all' :-
        current_predicate(_,P),
	functor(P, Na, Ar),
	'$new_abolish'(Na/Ar),
	fail.
'$abolish_all'.

'$check_error_in_predicate_indicator'(V, Msg) :-
	var(V), !,
	throw(error(instantiation_error, Msg)).
'$check_error_in_predicate_indicator'(M:S, Msg) :- !,
	'$check_error_in_module'(M, Msg),
	'$check_error_in_predicate_indicator'(S, Msg).
'$check_error_in_predicate_indicator'(S, Msg) :-
	S \= _/_, !,
	throw(error(type_error(predicate_indicator,S), Msg)).
'$check_error_in_predicate_indicator'(Na/_, Msg) :-
	var(Na), !,
	throw(error(instantiation_error, Msg)).
'$check_error_in_predicate_indicator'(Na/_, Msg) :-
	\+ atom(Na), !,
	throw(error(type_error(atom,Na), Msg)).
'$check_error_in_predicate_indicator'(_/Ar, Msg) :-
	var(Ar), !,
	throw(error(instantiation_error, Msg)).
'$check_error_in_predicate_indicator'(_/Ar, Msg) :-
	\+ integer(Ar), !,
	throw(error(type_error(integer,Ar), Msg)).
'$check_error_in_predicate_indicator'(_/Ar, Msg) :-
	Ar < 0, !,
	throw(error(domain_error(not_less_than_zero,Ar), Msg)).
% not yet implemented!
%'$check_error_in_predicate_indicator'(Na/Ar, Msg) :-
%	Ar < maxarity, !,
%	throw(error(type_error(representation_error(max_arity),Ar), Msg)).

'$check_error_in_module'(M, Msg) :-
	var(M), !,
	throw(error(instantiation_error, Msg)).
'$check_error_in_module'(M, Msg) :-
	\+ atom(M), !,
	throw(error(type_error(atom,M), Msg)).

'$old_abolish'(V) :- var(V), !,
	'$abolish_all_old'.
'$old_abolish'(M:N) :- !,
	'$mod_switch'(M,'$old_abolish'(N)).
'$old_abolish'([]) :- !.
'$old_abolish'([H|T]) :- !,  abolish(H), abolish(T).
'$old_abolish'(N/A) :- abolish(N,A).
	
'$abolish_all_old' :-
        current_predicate(_,P),
	functor(P, Na, Ar),
	'$abolish_old'(Na/Ar),
	fail.
'$abolish_all_old'.


'$abolishd'(T) :- '$recordedp'(T,_,R), erase(R), fail.
'$abolishd'(T) :- '$kill_dynamic'(T), fail.
'$abolishd'(_).

'$abolishs'(G) :- '$in_use'(G), !,
	functor(G,Name,Arity),
	throw(error(permission_error(modify,static_procedure_in_use,Name/Arity),abolish(G))).
'$abolishs'(G) :- '$system_predicate'(G), !,
	functor(G,Name,Arity),
	throw(error(permission_error(modify,static_procedure,Name/Arity),abolish(G))).
'$abolishs'(G) :-
	'$access_yap_flags'(8, 2), % only do this in sicstus mode
	'$undefined'(G),
	functor(G,Name,Arity),
	'$current_module'(Module),
	format(user_error,'[ Warning: abolishing undefined predicate (~w:~w/~w) ]~n',[Module,Name,Arity]),
	fail.
% I cannot allow modifying static procedures in YAPOR
% this code has to be here because of abolish/2
'$abolishs'(G) :-
	'$has_yap_or', !,
        functor(G,A,N),
	throw(error(permission_error(modify,static_procedure,A/N),abolish(G))).
'$abolishs'(G) :-
	'$purge_clauses'(G),
	'$recordedp'(G,_,R), erase(R), fail.
'$abolishs'(_).

%
% can only do as goal in YAP mode.
%
dynamic(X) :- '$access_yap_flags'(8, 0), !,
	'$dynamic'(X).
dynamic(X) :-
	throw(error(context_error(dynamic(X),declaration),query)).

'$dynamic'(X) :- var(X), !,
	throw(error(instantiation_error,dynamic(X))).
'$dynamic'(Mod:Spec) :- !,
	'$mod_switch'(Mod,'$dynamic'(Spec)).
'$dynamic'((A,B)) :- !, '$dynamic'(A), '$dynamic'(B).
'$dynamic'([]) :- !.
'$dynamic'([H|L]) :- !, '$dynamic'(H), '$dynamic'(L).
'$dynamic'(A) :-
	'$dynamic2'(A).

'$dynamic2'(X) :- '$log_upd'(Stat), Stat\=0, !,
	'$logical_updatable'(X).
'$dynamic2'(A/N) :- integer(N), atom(A), !,
	functor(T,A,N), '$flags'(T,F,F),
	( F/\16'9bc88 =:= 0 -> NF is F \/ 16'2000, '$flags'(T, F, NF);
	    '$is_dynamic'(T)  -> true;
	    F /\ 16'400 =:= 16'400, '$undefined'(T) -> F1 is F /\ \(0x600), NF is F1 \/ 16'2000, '$flags'(T,F,NF);
	    F/\16'8 =:= 16'8 -> true ;
	    throw(error(permission_error(modify,static_procedure,A/N),dynamic(A/N)))
	), '$flags'(T,F1,F1).
'$dynamic2'(X) :- 
	throw(error(type_error(callable,X),dynamic(X))).


'$logical_updatable'(A/N) :- integer(N), atom(A), !,
	functor(T,A,N), '$flags'(T,F,F),
	( F/\16'9bc88 =:= 0 -> NF is F \/ 16'408, '$flags'(T,F,NF);
	    '$is_dynamic'(T)  -> true;
	    F /\ 16'400 =:= 16'400 , '$undefined'(T) -> NF is F \/ 0x8,  '$flags'(T,F,NF);
	    F /\ 16'8=:= 16'8 -> true ;
	    throw(error(permission_error(modify,static_procedure,A/N),dynamic(A/N)))
	).
'$logical_updatable'(X) :- 
	throw(error(type_error(callable,X),dynamic(X))).


dynamic_predicate(P,Sem) :-
	'$bad_if_is_semantics'(Sem, dynamic(P,Sem)).
dynamic_predicate(P,Sem) :-
	'$log_upd'(OldSem),
	( Sem = logical -> '$switch_log_upd'(1) ; '$switch_log_upd'(0) ),
	'$dynamic'(P),
	'$switch_log_upd'(OldSem).

'$bad_if_is_semantics'(Sem, Goal) :-
	var(Sem), !,
	throw(error(instantiation_error,Goal)).
'$bad_if_is_semantics'(Sem, Goal) :-
	Sem \= immediate, Sem \= logical, !,
	throw(error(domain_error(semantics_indicator,Sem),Goal)).


'$expand_clause'(C0,C1,C2) :-
	'$expand_term_modules'(C0, C1, C2),
	( '$get_value'('$strict_iso',on) ->
	    '$check_iso_strict_clause'(C1)
        ;
           true
        ).

'$public'(X) :- var(X), !,
	throw(error(instantiation_error,public(X))).
'$public'(Mod:Spec) :- !,
	'$mod_switch'(Mod,'$public'(Spec)).
'$public'((A,B)) :- !, '$public'(A), '$public'(B).
'$public'([]) :- !.
'$public'([H|L]) :- !, '$public'(H), '$public'(L).
'$public'(A/N) :- integer(N), atom(A), !,
	functor(T,A,N),
	'$do_make_public'(T).
'$public'(X) :- 
	throw(error(type_error(callable,X),dynamic(X))).

'$do_make_public'(T) :-
	'$is_dynamic'(T), !.  % all dynamic predicates are public.
'$do_make_public'(T) :-
	'$flags'(T,F,F),
	NF is F\/16'400000,
	'$flags'(T,F,NF).

'$is_public'(T) :-
	'$is_dynamic'(T), !.  % all dynamic predicates are public.
'$is_public'(T) :-
	'$flags'(T,F,F),
	F\/16'400000 \== 0.

