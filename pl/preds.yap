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
asserta(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,first,_,asserta(C)).

assertz(V) :- var(V), !,
	throw(error(instantiation_error,assertz(V))).
assertz(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,last,_,assertz(C)).

assert(V) :- var(V), !,
	throw(error(instantiation_error,assert(V))).
assert(C) :-
	'$current_module'(Mod),
	'$assert'(C,Mod,last,_,assert(C)).

'$assert'(V,Mod,_,_,_) :- var(V), !,
	throw(error(instantiation_error,assert(Mod:V))).
'$assert'(M:C,_,Where,R,P) :- !,
	'$assert'(C,M,Where,R,P).
'$assert'((H:-G),M1,Where,R,P) :-
	(var(H) -> throw(error(instantiation_error,P)) ;  H=M:C), !,
	( M1 = M ->
	    '$assert'((C:-G),M1,Where,R,P)
	;
	    '$preprocess_clause_before_mod_change'((C:-G),M1,M,C1),
	    '$assert'(C1,M,Where,R,P)
	).
'$assert'(CI,Mod,Where,R,P) :-
	'$expand_clause'(CI,C0,C,Mod),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where, H, B, C0, Mod, R)
	;
	  '$undefined'(H,Mod) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar, Mod),
	    '$assertat_d'(Where,H,B,C0,Mod,R)
	;
            '$access_yap_flags'(14, 1) -> % I can assert over static facts in YAP mode
	    '$assert1'(Where,C,C0,Mod,H)
        ;
	    functor(H, Na, Ar),
            throw(error(permission_error(modify,static_procedure,Na/Ar),P))
	).


'$assert_dynamic'(V,Mod,_,_,_) :- var(V), !,
	throw(error(instantiation_error,assert(Mod:V))).
'$assert_dynamic'(M:C,_,Where,R,P) :- !,
	'$assert_dynamic'(C,Mod,Where,R,P).
'$assert_dynamic'((H:-G),M1,Where,R,P) :-
        (var(H) -> throw(error(instantiation_error,P)) ;  H=M:C), !,
	( M1 = M ->
	    '$assert_dynamic'((C:-G),M1,Where,R,P)
	;
	    '$preprocess_clause_before_mod_change'((C:-G),M1,M,C1),
	    '$assert_dynamic'(C1,M,Where,R,P)
	).
'$assert_dynamic'(CI,Mod,Where,R,P) :-
	'$expand_clause'(CI,C0,C,Mod),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H, Mod) ->
	    '$assertat_d'(Where,H,B,C0,Mod,R)
	;
	  '$undefined'(H, Mod) -> 
	    functor(H, Na, Ar),
	    '$dynamic'(Na/Ar, Mod),
	    '$assertat_d'(Where,H,B,C0,Mod,R)
	;
	    functor(H,Na,Ar),
	    throw(error(permission_error(modify,static_procedure,Na/Ar),P))
	).

assert_static(V) :- var(V), !,
	throw(error(instantiation_error,assert_static(V))).
assert_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,last,_,assert_static(C)).

asserta_static(V) :- var(V), !,
	throw(error(instantiation_error,asserta_static(V))).
asserta_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,first,_,asserta_static(C)).

assertz_static(V) :- var(V), !,
	throw(error(instantiation_error,assertz_static(V))).
assertz_static(C) :-
	'$current_module'(Mod),
	'$assert_static'(C,Mod,last,_,assertz_static(C)).

'$assert_static'(V,M,_,_,_) :- var(V), !,
	throw(error(instantiation_error,assert(M:V))).
'$assert_static'(M:C,_,Where,R,P) :- !,
	'$assert_static'(C,M,Where,R,P).
'$assert_static'((H:-G),M1,Where,R,P) :-
	(var(H) -> throw(error(instantiation_error,P)) ;  H=M:C), !,
	( M1 = M ->
	    '$assert_static'((C:-G),M1,Where,R,P)
	;
	    '$preprocess_clause_before_mod_change'((C:-G),M1,M,C1),
	    '$assert_static'(C1,M,Where,R,P)
	).
'$assert_static'(CI,Mod,Where,R,P) :-
	'$expand_clause'(CI,C0,C,Mod),
	'$check_head_and_body'(C,H,B,P),
	( '$is_dynamic'(H, Mod) ->
	    throw(error(permission_error(modify,dynamic_procedure,Na/Ar),P))
	;
	  '$undefined'(H,Mod), '$get_value'('$full_iso',true) ->
	    functor(H,Na,Ar), '$dynamic'(Na/Ar, Mod), '$assertat_d'(Where,H,B,C0,Mod,R)
	;
	    '$assert1'(Where,C,C0,Mod,H)
        ).


'$assertat_d'(first,Head,Body,C0,Mod,R) :- !,
	'$compile_dynamic'((Head:-Body), 2, Mod, CR),
         ( '$get_value'('$abol',true)
           ->
            '$flags'(H,Mod,Fl,Fl),
	    ( Fl /\ 16'400000 =\= 0 -> '$erase_source'(H,Mod) ; true ),
	    ( Fl /\ 16'040000 =\= 0 -> '$check_multifile_pred'(H,Mod,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordap'(Mod:Head,(H0 :- B0),R,CR),
	( '$is_multifile'(Head, Mod) ->
	    '$get_value'('$consulting_file',F),
	    functor(H0, Na, Ar),
	    '$recorda'('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,Mod,F,R), _) 
	;
	    true
	).
'$assertat_d'(last,Head,Body,C0,Mod,R) :-
	'$compile_dynamic'((Head:-Body), 0, Mod, CR),
         ( '$get_value'('$abol',true)
           ->
            '$flags'(H,Mod,Fl,Fl),
            ( Fl /\ 16'400000 =\= 0 -> '$erase_source'(H,Mod) ; true ),
	    ( Fl /\ 16'040000 =\= 0 -> '$check_multifile_pred'(H,Mod,Fl) ; true )
          ;
            true
        ),	    
	'$head_and_body'(C0, H0, B0),
	'$recordzp'(Mod:Head,(H0 :- B0),R,CR),
	( '$is_multifile'(H0, Mod) ->
	    '$get_value'('$consulting_file',F),
	    functor(H0, Na, Ar),
	    '$recordz'('$multifile_dynamic'(_,_,_), '$mf'(Na,Ar,Mod,F,R), _) 
	;
	    true
	).

'$assert1'(last,C,C0,Mod,H) :- '$$compile_stat'(C,C0,0,H,Mod).
'$assert1'(first,C,C0,Mod,H) :- '$$compile_stat'(C,C0,2,H,Mod).

'$assertz_dynamic'(X, C, C0, Mod) :- (X/\4)=:=0, !,
	'$head_and_body'(C,H,B),
	'$assertat_d'(last,H,B,C0,Mod,_).
'$assertz_dynamic'(X,C,C0,Mod) :- 
	'$head_and_body'(C,H,B), functor(H,N,A),
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
	'$get_value'('$consulting_file',F),
	'$recorded'('$multifile_dynamic'(_,_,_), '$mf'(Na,A,M,F,R), R1),
	erase(R1),
	erase(R),
	fail.
'$erase_all_mf_dynamic'(_,_,_).

asserta(V,R) :- var(V), !,
	throw(error(instantiation_error,asserta(V,R))).
asserta(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,first,R,asserta(C,R)).

assertz(V,R) :- var(V), !,
	throw(error(instantiation_error,assertz(V,R))).
assertz(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,last,R,assertz(C,R)).

assert(V,R) :- var(V), !,
	throw(error(instantiation_error,assert(V,R))).
assert(C,R) :-
	'$current_module'(M),
	'$assert_dynamic'(C,M,last,R,assert(C,R)).

clause(V,Q) :-
	'$current_module'(M),
	'$clause'(V,M,Q).

'$clause'(V,M,Q) :- var(V), !, 
	throw(error(instantiation_error,M:clause(V,Q))).
'$clause'(C,M,Q) :- number(C), !,
	throw(error(type_error(callable,C),M:clause(C,Q))).
'$clause'(R,Q) :- db_reference(R), !,
	throw(error(type_error(callable,R),M:clause(R,Q))).
'$clause'(M:P,_,Q) :- !,
	'$clause'(P,M,Q).
'$clause'(P,Mod,Q) :- '$is_dynamic'(P, Mod), !,
	 '$recordedp'(Mod:P,(P:-Q),_). 
'$clause'(P,M,Q) :-
	'$some_recordedp'(M:P), !,
	 '$recordedp'(M:P,(P:-Q),_).
'$clause'(P,M,Q) :-
	( '$system_predicate'(P) -> true ;
	    '$number_of_clauses'(P,M,N), N > 0 ),
	functor(P,Name,Arity),
	throw(error(permission_error(access,private_procedure,Name/Arity),
	      clause(M:P,Q))).

clause(V,Q,R) :-
	'$current_module'(V,M,Q,R),
	'$clause'(V,M,Q,R).

'$clause'(V,M,Q,R) :- var(V), !, 
	throw(error(instantiation_error,M:clause(V,Q,R))).
'$clause'(C,M,Q,R) :- number(C), !,
	throw(error(type_error(callable,C),clause(C,M:Q,R))).
'$clause'(R,M,Q,R1) :- db_reference(R), !,
	throw(error(type_error(callable,R),clause(R,Q,R1))).
'$clause'(M:P,_,Q,R) :- !,
	'$clause'(P,M,Q,R).
'$clause'(P,Mod,Q,R) :-
	 ( '$is_dynamic'(P, Mod) ->
	 	'$recordedp'(Mod:P,(P:-Q),R)
	 ;
	        functor(P,N,A),
		throw(error(permission_error(access,private_procedure,N/A),
			clause(Mod:P,Q,R)))
	 ).

retract(C) :-
	'$current_module'(M),
	'$retract'(C,M).
	
	
'$retract'(V,_) :- var(V), !,
	throw(error(instantiation_error,retract(V))).
'$retract'(M:C,_) :- !,
	'$retract'(C,M).
'$retract'(C,M) :- 
	'$check_head_and_body'(C,H,B,retract(C)),
	'$is_dynamic'(H, M), !,
	'$recordedp'(M:H,(H:-B),R), erase(R).
'$retract'(C,M) :-
	'$fetch_predicate_indicator_from_clause'(C, PI),
	throw(error(permission_error(modify,static_procedure,PI),retract(M:C))).

retract(C,R) :- !,
	'$current_module'(M),
	'$retract'(C,M,R).


'$retract'(V,M,R) :- var(V), !,
	throw(error(instantiation_error,retract(M:V,R))).
'$retract'(M:C,_,R) :- !,
	'$retract'(C,M,R).
'$retract'(C, M, R) :-
	'$check_head_and_body'(C,H,B,retract(C,R)),
	db_reference(R), '$is_dynamic'(H,M), !,
	instance(R,(H:-B)), erase(R).
'$retract'(C,M,R) :-
	'$head_and_body'(C,H,B,retract(C,R)),
	'$is_dynamic'(H,M), !,
	var(R),
	'$recordedp'(M:H,(H:-B),R),
	erase(R).
'$retract'(C,M,_) :-
	'$fetch_predicate_indicator_from_clause'(C, PI),
	throw(error(permission_error(modify,static_procedure,PI),retract(M:C))).

'$fetch_predicate_indicator_from_clause'((C :- _), Na/Ar) :- !,
	functor(C, Na, Ar).
'$fetch_predicate_indicator_from_clause'(C, Na/Ar) :-
	functor(C, Na, Ar).
	

retractall(V) :- !,
	'$current_module'(M),
	'$retractall'(V,M).

'$retractall'(V,M) :- var(V), !,
	throw(error(instantiation_error,retract(M:V))).
'$retractall'(M:V,_) :- !,
	'$retractall'(V,M).
'$retractall'(T,M) :-
	'$undefined'(T,M),
	functor(T,Na,Ar),
	'$dynamic'(Na/Ar,M), !.
'$retractall'(T,M) :-
	\+ '$is_dynamic'(T,M), !,
	functor(T,Na,Ar),
	throw(error(permission_error(modify,static_procedure,Na/Ar),retractall(T))).
'$retractall'(T,M) :-
	'$erase_all_clauses_for_dynamic'(T, M).

'$erase_all_clauses_for_dynamic'(T, M) :-
	'$recordedp'(M:T,(T :- _),R), erase(R), fail.
'$erase_all_clauses_for_dynamic'(T,M) :-
	'$recordedp'(M:T,_,_), fail.
'$erase_all_clauses_for_dynamic'(_,_).

abolish(N,A) :-
	'$current_module'(Mod),
	'$abolish'(N,A,Mod).
	
'$abolish'(N,A,M) :- var(N), !,
	throw(error(instantiation_error,abolish(M:N,A))).
'$abolish'(N,A,M) :- var(A), !,
	throw(error(instantiation_error,abolish(M:N,A))).
	throw(error(instantiation_error,abolish(M:N,A))).
'$abolish'(N,A,M) :-
	( '$recorded'('$predicate_defs','$predicate_defs'(N,A,_),R) -> erase(R) ),
	fail.
'$abolish'(N,A,M) :- functor(T,N,A),
		( '$is_dynamic'(T, M) -> '$abolishd'(T,M) ;
	      	 /* else */	      '$abolishs'(T,M) ).

abolish(X) :- 
	'$access_yap_flags'(8, 2), !,
	'$current_module'(M),
	'$new_abolish'(X,M).
abolish(X) :- 
	'$current_module'(M),
	'$old_abolish'(X,M).

'$new_abolish'(V,M) :- var(V), !,
	'$abolish_all'(M).
'$new_abolish'(A,M) :- var(A), !,
	'$abolish_all_atoms'(A,M).
'$new_abolish'(M:PS,_) :- !,
	'$new_abolish'(PS,M).
'$new_abolish'(Na/Ar, M) :-
	functor(H, Na, Ar),
	'$is_dynamic'(H, M), !,
	'$abolishd'(H, M).
'$new_abolish'(Na/Ar, M) :- % succeed for undefined procedures.
	functor(T, Na, Ar),
	'$undefined'(T, M), !.
'$new_abolish'(Na/Ar, M) :-
	throw(error(permission_error(modify,static_procedure,Na/Ar),abolish(M:Na/Ar))).
'$new_abolish'(T, M) :-
	throw(error(type_error(predicate_indicator,T),abolish(M:T))).

'$abolish_all'(M) :-
        '$current_predicate'(M,Na,Ar),
	'$new_abolish'(Na/Ar, M),
	fail.
'$abolish_all'(_).

'$abolish_all_atoms'(Na, M) :-
        '$current_predicate'(M,Na,Ar),
	'$new_abolish'(Na/Ar, M),
	fail.
'$abolish_all_atoms'(_,_).

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

'$old_abolish'(V,M) :- var(V), !,
	'$abolish_all_old'(M).
'$old_abolish'(A,M) :- atom(A), !,
	'$abolish_all_atoms_old'(A,M).
'$old_abolish'(M:N,_) :- !,
	'$old_abolish'(N,M).
'$old_abolish'([], _) :- !.
'$old_abolish'([H|T], M) :- !,  '$old_abolish'(H, M), '$old_abolish'(T, M).
'$old_abolish'(N/A, M) :-
	'$abolish'(N, A, M).
'$old_abolish'(T, M) :-
	throw(error(type_error(predicate_indicator,T),abolish(M:T))).
	
'$abolish_all_old'(M) :-
        '$current_predicate'(Mod,Na,Ar),
	'$abolish'(Na, Ar, Mod),
	fail.
'$abolish_all_old'.

'$abolish_all_atoms_old'(Na, M) :-
        '$current_predicate'(M,Na,Ar),
	'$abolish'(Na, Ar, M),
	fail.
'$abolish_all_atoms_old'(_,_).

'$abolishd'(T, M) :- '$recordedp'(M:T,_,R), erase(R), fail.
'$abolishd'(T, M) :- '$kill_dynamic'(T,M), fail.
'$abolishd'(_, _).

'$abolishs'(G, M) :- '$in_use'(G, M), !,
	functor(G,Name,Arity),
	throw(error(permission_error(modify,static_procedure_in_use,Name/Arity),abolish(M:G))).
'$abolishs'(G, _) :- '$system_predicate'(G), !,
	functor(G,Name,Arity),
	throw(error(permission_error(modify,static_procedure,Name/Arity),abolish(M:G))).
'$abolishs'(G, Module) :-
	'$access_yap_flags'(8, 2), % only do this in sicstus mode
	'$undefined'(G, Module),
	functor(G,Name,Arity),
	format(user_error,'[ Warning: abolishing undefined predicate (~w:~w/~w) ]~n',[Module,Name,Arity]),
	fail.
% I cannot allow modifying static procedures in YAPOR
% this code has to be here because of abolish/2
'$abolishs'(G, Module) :-
	'$has_yap_or', !,
        functor(G,A,N),
	throw(error(permission_error(modify,static_procedure,A/N),abolish(Module:G))).
'$abolishs'(G, M) :-
	'$purge_clauses'(G, M),
	'$recordedp'(M:G,_,R), erase(R), fail.
'$abolishs'(_, _).

%
% can only do as goal in YAP mode.
%
dynamic(X) :- '$access_yap_flags'(8, 0), !,
        '$current_module'(M),
	'$dynamic'(X, M).
dynamic(X) :-
	throw(error(context_error(dynamic(X),declaration),query)).

'$dynamic'(X,_) :- var(X), !,
	throw(error(instantiation_error,dynamic(M:X))).
'$dynamic'(Mod:Spec,_) :- !,
	'$dynamic'(Spec,Mod).
'$dynamic'([], _) :- !.
'$dynamic'([H|L], M) :- !, '$dynamic'(H, M), '$dynamic'(L, M).
'$dynamic'((A,B),M) :- !, '$dynamic'(A,M), '$dynamic'(B,M).
'$dynamic'(X,M) :- !,
	'$dynamic2'(X,M).

'$dynamic2'(X, Mod) :- '$log_upd'(Stat), Stat\=0, !,
	'$logical_updatable'(X, Mod).
'$dynamic2'(A/N, Mod) :- integer(N), atom(A), !,
	functor(T,A,N), '$flags'(T,Mod,F,F),
	( F/\16'9bc88 =:= 0 -> NF is F \/ 16'2000, '$flags'(T, Mod, F, NF);
	    '$is_dynamic'(T,Mod)  -> true;
	    F /\ 16'400 =:= 16'400, '$undefined'(T,Mod) -> F1 is F /\ \(0x600), NF is F1 \/ 16'2000, '$flags'(T,Mod,F,NF);
	    F/\16'8 =:= 16'8 -> true ;
	    throw(error(permission_error(modify,static_procedure,A/N),dynamic(Mod:A/N)))
	).
'$dynamic2'(X,Mod) :- 
	throw(error(type_error(callable,X),dynamic(Mod:X))).


'$logical_updatable'(A/N,Mod) :- integer(N), atom(A), !,
	functor(T,A,N), '$flags'(T,Mod,F,F),
	( F/\16'9bc88 =:= 0 -> NF is F \/ 16'408, '$flags'(T,Mod,F,NF);
	    '$is_dynamic'(T,Mod)  -> true;
	    F /\ 16'400 =:= 16'400 , '$undefined'(T,Mod) -> NF is F \/ 0x8,  '$flags'(T,Mod,F,NF);
	    F /\ 16'8=:= 16'8 -> true ;
	    throw(error(permission_error(modify,static_procedure,A/N),dynamic(Mod:A/N)))
	).
'$logical_updatable'(X,Mod) :- 
	throw(error(type_error(callable,X),dynamic(Mod:X))).


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
	throw(error(instantiation_error,Goal)).
'$bad_if_is_semantics'(Sem, Goal) :-
	Sem \= immediate, Sem \= logical, !,
	throw(error(domain_error(semantics_indicator,Sem),Goal)).


'$expand_clause'(C0,C1,C2,Mod) :-
	'$expand_term_modules'(C0, C1, C2, Mod),
	( '$get_value'('$strict_iso',on) ->
	    '$check_iso_strict_clause'(C1)
        ;
           true
        ).

'$public'(X, _) :- var(X), !,
	throw(error(instantiation_error,public(X))).
'$public'(Mod:Spec, _) :- !,
	'$public'(Spec,Mod).
'$public'((A,B), M) :- !, '$public'(A,M), '$public'(B,M).
'$public'([],_) :- !.
'$public'([H|L], M) :- !, '$public'(H, M), '$public'(L, M).
'$public'(A/N, Mod) :- integer(N), atom(A), !,
	'$do_make_public'(T, Mod).
'$public'(X, Mod) :- 
	throw(error(type_error(callable,X),dynamic(Mod:X))).

'$do_make_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$do_make_public'(T, Mod) :-
	'$flags'(T,Mod,F,F),
	NF is F\/16'400000,
	'$flags'(T,Mod,F,NF).

'$is_public'(T, Mod) :-
	'$is_dynamic'(T, Mod), !.  % all dynamic predicates are public.
'$is_public'(T, Mod) :-
	'$flags'(T,Mod,F,F),
	F\/16'400000 \== 0.

