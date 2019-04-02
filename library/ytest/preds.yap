:- [library(hacks)].

'$predicate_flags'(P, M, Flags0, Flags1) :-
    var(Flags0),
    Flags0 ==  Flags1,
    !,
    (
     predicate_property(M:P, meta_predicate(_))
    ->
     Flags1 = 0x200000
    ).
'$predicate_flags'(P, M, Flags0, Flags1) :-
    ( Flags1 /\ 0x200000 =\= 0,
      Flags0 /\ 0x200000 =:= 0
    ->
      true
    ;
      Flags1 /\ 0x200000 =\= 0,
      Flags0 /\ 0x200000 =\= 0
    ).

'$get_undefined_pred'(G,M,G,M0) :-
    predicate_property(M:G, imported_from(M0)), !.
'$get_undefined_pred'(G,M,G,OM) :-
    functor(G,F,N),
    ( system_predicate(F/N), OM = prolog ; current_predicate(user:F/N), OM= user),  !.
'$get_undefined_pred'(G,M,G,M0) :-
    predicate_property(M:G, imported_from(M0)), !.
'$get_undefined_pred'(G,M,G,M).

'$is_metapredicate'( call(_), _M) :- !.
'$is_metapredicate'( call(_,_), _M) :- !.
'$is_metapredicate'( G, M) :-
    predicate_property(M:G, meta_predicate(_)).

'$is_imported_predicate'(G,M,G,M0) :-
    predicate_property(M:G, imported_from(M0)).

'$is_system_predicate'( call(_), _M) :- !.
'$is_system_predicate'( call(_,_), _M) :- !.
'$is_system_predicate'(G,M) :-
    predicate_property(M:G, built_in).

'$is_multifile'(G,M) :-
    predicate_property(M:G, multifile).

'$module_transparent'(_,_,_,_) :- fail.

'$meta_predicate'(call,_M,1,call(0)) :- !.
'$meta_predicate'(call,_M,2,call(1,?)) :- !.
'$meta_predicate'(F,M,N,P) :-
    functor(G, F, N),
     predicate_property(M:G, meta_predicate(P)).

/** user:term_expansion(+M:Cl,-M:NCl ) 

rule preprocessor
*/
user:term_expansion( ( :- '$meta_predicate'( _ ) ), [] ).

user:goal_expansion(_:'_user_expand_goal'(A, M, B), user:user_expand_goal(A, M, B) ).


user_expand_goal(A, M, B) :-
    (
     current_predicate(M:goal_expansion/2),
     M:goal_expansion(A,B) -> true ;
     system:goal_expansion(A,B) -> true ;
     user:goal_expansion(A,M,B) -> true ;
     user:goal_expansion(A,B) -> true
    ).

user:goal_expansion(prolog:'$meta_predicate'(N,M,A,D) , user:mt( N, M, A, D) ).

mt(N,M,A,D) :-
    functor(D,N,A),
    predicate_property(M:D, meta_predicate(D)).


'$full_clause_optimisation'(_H, _M, B, B).

'$c_built_in'(G, _SM, _H, G).


'$head_and_body'((H:-B),H,B) :- !.
'$head_and_body'(H,H,true).

'$yap_strip_module'(T,M,S) :-
    fully_strip_module(T,M,S).

:- hide_predicate( expand_goal ).

:- include(library(boot/meta)).
