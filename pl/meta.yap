/**

  @file meta.yap
*/

/**
   @defgroup YAPMetaPredicates Using Meta-Calls with Modules

   @{


 @ingroup YAPModules

*/

/**
  @pred meta_predicate( Gi ) is directive

Declares that this predicate manipulates references to predicates.
Each _Gi_ is a mode specification.

If the argument is `:`, it does not refer directly to a predicate
but must be module expanded. If the argument is an integer, the argument
is a goal or a closure and must be expanded. Otherwise, the argument is
not expanded. Note that the system already includes declarations for all
built-ins.

For example, the declaration for call/1 and setof/3 are:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:- meta_predicate call(0), setof(?,0,?).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The meta_predicate declaration is
 implemented by:

  - asserting `$meta_predicate(SourceModule,Functor,Arity,Declaration)`
  - setting up a `MetaPredicate` flag in the internal predicate descriptor.
*/

% directive now meta_predicate Ps :- $meta_predicate(Ps).

:- use_system_module( '$_arith', ['$c_built_in'/4]).

meta_predicate(P) :-
    source_module(SM),
    '$meta_predicate'(P, SM).

'$meta_predicate'(P,M) :-
    var(P),
    !,
    '$do_error'(instantiation_error,meta_predicate(M:P)).
'$meta_predicate'(P,M) :-
    var(M),
    !,
    '$do_error'(instantiation_error,meta_predicate(M:P)).
'$meta_predicate'((P,_Ps),M) :-
    '$meta_predicate'(P,M),
    fail.
'$meta_predicate'((_P,Ps),M) :-
    !,
    '$meta_predicate'(Ps,M).
'$meta_predicate'( D, M ) :-
    '$yap_strip_module'( M:D, M1, P),
    P\==D,
    !,
    '$meta_predicate'( P, M1 ).
'$meta_predicate'( D, M ) :-
    functor(D,F,N),
    '$install_meta_predicate'(D,M,F,N),
    fail.
'$meta_predicate'( _D, _M ).

'$install_meta_predicate'(P,M,_F,_N) :-
    '$new_meta_pred'(P, M),
    fail.

'$install_meta_predicate'(_P,M,F,N) :-
    ( M = prolog -> M2 = _ ; M2 = M),
    retractall(prolog:'$meta_predicate'(F,M2,N,_)),
    fail.
'$install_meta_predicate'(P,M,F,N) :-
    ( M = prolog -> M2 = _ ; M2 = M),
    assertz('$meta_predicate'(F,M2,N,P)).

% comma has its own problems.

%% handle module transparent predicates by defining a
%% new context module.
'$is_mt'(H, B, HM, _SM, M, (context_module(CM),B), CM) :-
    '$yap_strip_module'(HM:H, M, NH),
    '$module_transparent'(_, M, _, NH), !.
'$is_mt'(_H, B, _HM, _SM, BM, B, BM).



% I assume the clause has been processed, so the
% var case is long gone! Yes :)
'$clean_cuts'(G,('$current_choice_point'(DCP),NG)) :-
    '$conj_has_cuts'(G,DCP,NG,OK), OK == ok, !.
'$clean_cuts'(G,G).

'$clean_cuts'(G,DCP,NG) :-
    '$conj_has_cuts'(G,DCP,NG,OK), OK == ok, !.
'$clean_cuts'(G,_,G).

'$conj_has_cuts'(V,_,V, _) :- var(V), !.
'$conj_has_cuts'(!,DCP,'$$cut_by'(DCP), ok) :- !.
'$conj_has_cuts'((G1,G2),DCP,(NG1,NG2), OK) :- !,
    '$conj_has_cuts'(G1, DCP, NG1, OK),
    '$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1;G2),DCP,(NG1;NG2), OK) :- !,
    '$conj_has_cuts'(G1, DCP, NG1, OK),
    '$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1->G2),DCP,(G1;NG2), OK) :- !,
    % G1: the system must have done it already
    '$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1*->G2),DCP,(G1;NG2), OK) :- !,
    % G1: the system must have done it already
    '$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'(if(G1,G2,G3),DCP,if(G1,NG2,NG3), OK) :- !,
    % G1: the system must have done it already
    '$conj_has_cuts'(G2, DCP, NG2, OK),
    '$conj_has_cuts'(G3, DCP, NG3, OK).
'$conj_has_cuts'(G,_,G, _).

% return list of vars in expanded positions on the head of a clause.
%
% these variables should not be expanded by meta-calls in the body of the goal.
%
%  should be defined before caller.
%
'$module_u_vars'(M, H, UVars) :-
    functor(H,F,N),
    '$meta_predicate'(F,M,N,D), !,
    '$do_module_u_vars'(N,D,H,UVars).
'$module_u_vars'(_,_,[]).

'$do_module_u_vars'(0,_,_,[]) :- !.
'$do_module_u_vars'(I,D,H,LF) :-
    arg(I,D,X), ( nonvar(X), X=':' -> true ; integer(X)),
    arg(I,H,A), '$uvar'(A, LF, L), !,
    I1 is I-1,
    '$do_module_u_vars'(I1,D,H,L).
'$do_module_u_vars'(I,D,H,L) :-
    I1 is I-1,
    '$do_module_u_vars'(I1,D,H,L).

'$uvar'(Y, [Y|L], L)  :- var(Y), !.
% support all/3
'$uvar'(same( G, _), LF, L)  :-
    '$uvar'(G, LF, L).
'$uvar'('^'( _, G), LF, L)  :-
    '$uvar'(G, LF, L).

/**
 * @pred '$meta_expand'( _Input_, _HeadModule_, _BodyModule_, _SourceModule_, _HVars_-_Head_, _OutGoal_)
 *
 * expand Input if a metapredicate, otherwF,MI,Arity,PredDefise ignore
 *
 * @return
*/
'$meta_expand'(G, _, CM, HVars, OG) :-
    var(G),
    !,
    (
	lists:identical_member(G, HVars)
    ->
    OG = G
    ;
    OG = CM:G
    ).
% nothing I can do here:
'$meta_expand'(G0, PredDef, CM, HVars, NG) :-
    G0 =.. [Name|GArgs],
    PredDef =.. [Name|GDefs],
    functor(PredDef, Name, Arity ),
    length(NGArgs, Arity),
    NG =.. [Name|NGArgs],
    '$expand_args'(GArgs, CM, GDefs, HVars, NGArgs).

'$expand_args'([],  _, [], _, []).
'$expand_args'([A|GArgs], CM,   [M|GDefs], HVars, [NA|NGArgs]) :-
    ( M == ':' -> true ; number(M) ),
    !,
    '$expand_arg'(A, CM, HVars, NA),
    '$expand_args'(GArgs, CM, GDefs, HVars, NGArgs).
'$expand_args'([A|GArgs],  CM, [_|GDefs], HVars, [A|NGArgs]) :-
    '$expand_args'(GArgs, CM, GDefs, HVars, NGArgs).


'$meta_expansion'(GM, BM, HVars,M:GF) :-
    '$yap_strip_module'(BM:GM, M, G),
    functor(G, F, Arity ),
    '$meta_predicate'(F, M, Arity, PredDef),
    !,
    '$meta_expand'(G, PredDef, BM, HVars, GF).
'$meta_expansion'(GF, BM, _HVars, BM:GF).

% check if an argument should be expanded
'$expand_arg'(G,  CM, HVars, OG) :-
    var(G),
    !,
    ( lists:identical_member(G, HVars) -> OG = G; OG = CM:G).
'$expand_arg'(G,  CM, _HVars, NCM:NG) :-
    '$yap_strip_module'(CM:G, NCM, NG).

'$match_mod'(G, _HMod, _SMod, M, O) :-
    M = prolog,
    !,
    O = G.
'$match_mod'(G, M, M, M, G) :-    !.
'$match_mod'(G, _HM, _M, M, M:G).

% Call the import mechanism
%
'$import_expansion'(M:G, M1:G1) :-
    '$import'(M:G, M1:G1),
    !.
'$import_expansion'( MG, MG).


'$user_expansion'(G0, G) :-
    '_user_expand_goal'(G0, G1),
    !,
    ( G1 == G0
    ->
      G0 = G
    ;
    '$user_expansion'(G1, G)
    ).
'$user_expansion'(MG, MG).


'$expand_head'(G0, GF, SM, HM) :-
    '$yap_strip_module'( SM:G0, M0N, G0N),
    '$user_expansion'(M0N:G0N, HMGF),
    '$yap_strip_module'(HMGF,HM,GF).

'$expand_goal'(G0, GF, MOF:GOF, HM, SM, BM, HVars-H*Assert) :-
    '$yap_strip_module'( BM:G0, M0N, G0N),
    '$user_expansion'(M0N:G0N, M1:G1),
    '$import_expansion'(M1:G1, M2:G2),
    '$meta_expansion'(M2:G2, M1, HVars, M4:B4),
    '$end_goal_expansion'(B4, GF, GS, HM, SM, M4, H, Assert),
    !,
    '$yap_strip_module'( M4:GS, MOF, GOF).
'$expand_goal'(G0, G0, M:G0, _, M, _, _).

'$end_goal_expansion'(G, G1, BM:GO, HM, SM, BM, H, compile) :-
   !,
    '$match_mod'(G, HM, SM, BM, G1),
    '$c_built_in'(G1, BM, H, GO).
'$end_goal_expansion'(G, SM:G, SM:G, _HM, SM, SM, _H, assert).


/*
'$match_mod'(G, HMod, SMod, M, O) :-
    (
     % \+ '$is_multifile'(G1,M),
     %->
      '$is_system_predicate'(G,M)
     ->
      O = G
    ;
      M == HMod, M == SMod
    ->
     O = G
    ;
     O = M:G
    ).
*/

% expand module names in a body
% args are:
%       goals to expand
%       code to pass to listing
%       code to pass to compiler
%       head module   HM
%       source module  SM
%       current module for looking up preds  M
%
% to understand the differences, you can consider:
%
%  a:(d:b(X)) :- g:c(X), d(X), user:hello(X)).
%
% when we process meta-predicate c, HM=d, DM=a, BM=a, M=g and we should get:
%
%  d:b(X) :- g:c(g:X), a:d(X), user:hello(X).
%
% on the other hand,
%
%  a:(d:b(X) :- c(X), d(X), d:e(X)).
%
% will give
%
%  d:b(X) :- a:c(a:X), a:d(X), e(X).
%
%
%       head variable '$expand_goals'(M:G,G1,GO,HM,SM,,_M,HVars)les.
%       goals or arguments/sub-arguments?
% I cannot use call here because of format/3
% modules:
% A4: module for body of clause (this is the one used in looking up predicates)
% A5: context module (this is the current context
% A6: head module (this is the one used in compiling and accessing).
%
%
%'$expand_goals'(V,NG,NG,HM,SM,BM,HVars):- writeln(V), fail.
'$expand_goals'(V,NG,NGO,HM,SM,BM,HVars-H) :-
    var(V),
    !,
    (
	lists:identical_member(V, HVars)
    ->
    '$expand_goals'(call(V),NG,NGO,HM,SM,BM,HVars-H)
    ;
    atom(BM)
    ->
    NG = call(BM:V),
    NGO = '$execute_in_mod'(V,BM)
    ;
    '$expand_goals'(call(BM:V),NG,NGO,HM,SM,BM,HVars-H)
    ).
'$expand_goals'(BM:V,NG,NGO,HM,SM,_BM,HVarsH) :-
    '$yap_strip_module'( BM:V, CM, G),
    nonvar(CM),
    !,
    '$expand_goals'(G,NG,NGO,HM,SM,CM,HVarsH).

'$expand_goals'(CM0:V,NG,NGO,HM,SM,BM,HVarsH) :-
    strip_module( CM0:V, CM, G),
    !,
    '$expand_goals'(call(CM:G),NG,NGO,HM,SM,BM,HVarsH).
% if I don't know what the module is, I cannot do anything to the goal,
% so I just put a call for later on.
'$expand_goals'(V,NG,NGO,_HM,_SM,BM,_HVarsH) :-
    var(BM),
    !,
    NG = call(BM:V),
    NGO = '$execute_wo_mod'(V,BM).
'$expand_goals'(depth_bound_call(G,D),
                depth_bound_call(G1,D),
                ('$set_depth_limit_for_next_call'(D),GO),
                HM,SM,BM,HVars) :-
    '$expand_goals'(G,G1,GO,HM,SM,BM,HVars),
    '$composed_built_in'(GO), !.
'$expand_goals'((A,B),(A1,B1),(AO,BO),HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'((A;B),(A1;B1),(AO;BO),HM,SM,BM,HVars) :-
    var(A), !,
    '$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'(
    (A*->B),(A1*->B1),
    (
	yap_hacks:current_choicepoint(DCP),
	AO,
	yap_hacks:cut_at(DCP),BO
    ),
    HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
    '$clean_cuts'(AOO, AO),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'(
    (A*->B;C),(A1*->B1;C1),
    (
	yap_hacks:current_choicepoint(DCP),
	AO,
	yap_hacks:cut_at(DCP),BO
    ;
    CO
    ),
    HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
    '$clean_cuts'(AOO, AO),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
    '$expand_goals'(C,C1,CO,HM,SM,BM,HVars).
'$expand_goals'((A;B),(A1;B1),(AO;BO),HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'((A|B),(A1|B1),(AO|BO),HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'((A->B),(A1->B1),(AO->BO),HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
    '$clean_cuts'(AOO, AO),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'(\+G,\+G,A\=B,_HM,_BM,_SM,_HVars) :-
    nonvar(G),
    G = (A = B),
    !.
'$expand_goals'(\+A,\+A1,(AO-> false;true),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
    '$clean_cuts'(AOO, AO).
'$expand_goals'(not(G),not(G),A\=B,_HM,_BM,_SM,_HVars) :-
    nonvar(G),
    G = (A = B),
    !.
'$expand_goals'(not(A),not(A1),(AO-> false;true),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
    '$clean_cuts'(AOO, AO).
'$expand_goals'(once(A),once(A1),
		('$current_choice_point'(CP),AO,'$$cut_by'(CP)),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, CP, AO).
'$expand_goals'((:-A),(:-A1),
		(:-AO),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO,HM,SM,BM,HVars).
'$expand_goals'(ignore(A),ignore(A1),
		('$current_choice_point'(CP),AO,'$$cut_by'(CP)-> true ; true),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, AO).
'$expand_goals'(forall(A,B),forall(A1,B1),
		((AO, ( BO-> false ; true)) -> false ; true),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, AO).
'$expand_goals'(if(A,B,C),if(A1,B1,C1),
		('$current_choice_point'(DCP),AO,yap_hacks:cut_at(DCP),BO; CO),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
    '$expand_goals'(C,C1,CO,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, DCP, AO).
'$expand_goals'((A*->B;C),(A1*->B1;C1),
		('$current_choice_point'(DCP),AO,yap_hacks:cut_at(DCP),BO; CO),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
    '$expand_goals'(C,C1,CO,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, DCP, AO).
'$expand_goals'((A*->B),(A1*->B1),
		('$current_choice_point'(DCP),AO,BO),HM,SM,BM,HVars) :-
    !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, DCP, AO).
'$expand_goals'(true,true,true,_,_,_,_) :- !.
'$expand_goals'(fail,fail,fail,_,_,_,_) :- !.
'$expand_goals'([-H|L],NG,NG,_HM,_SM,BM,_) :-
    is_list(L),
    !,
    NG=reconsult(BM:H).
'$expand_goals'([H|L],NG,NG,_HM,_SM,BM,_) :-
    is_list(L),
    !,
    NG=consult(BM:H).
'$expand_goals'([],BM:true,BM:true,_HM,_SM,BM,_) :-
    !.

'$expand_goals'(G, G1, GO, HM, SM, BM, HVars) :-
    '$yap_strip_module'(BM:G,  NBM, GM),
    '$expand_goal'(GM, G1, GO, HM, SM, NBM, HVars).



'$build_up'(HM, NH, SM, true, NH, true, NH) :- HM == SM, !.
'$build_up'(HM, NH, _SM, true, HM:NH, true, HM:NH) :- !.
'$build_up'(HM, NH, SM, B1, (NH :- B1), BO, ( NH :- BO)) :- HM == SM, !.
'$build_up'(HM, NH, _SM, B1, (NH :- B1), BO, ( HM:NH :- BO)) :- !.

% expand arguments of a meta-predicate
% $meta_expansion(ModuleWhereDefined,CurrentModule,Goal,ExpandedGoal,MetaVariables)

expand_goal(G, MF:GF) :-
    source_module(SM),
    '$yap_strip_module'(G, M, IG),
    '$expand_goals'(IG, _G1, GO, M, SM, M, []-IG*assert ),
    !,
    '$yap_strip_module'(M:GO, MF, GF).




'$expand_clause_body'(V,_, _NH1, _HM1, _SM, M, call(M:V), call(M:V) ) :-
    var(V), !.
'$expand_clause_body'(true, _, _NH1, _HM1, _SM, _M, true, true ) :- !.

'$expand_clause_body'(B,Assert, H, HM, SM, M, B1, BO ) :-
    % expanded positions
    % support for SWI's meta primitive.
    '$is_mt'(H, B, HM, SM, M, IB, BM),
    !,
    '$expand_goals'(IB, B1, BO1, HM, SM, BM, Assert),
    (
	Assert == compile,
	'$full_clause_optimisation'(H, BM, BO1, BO)
%    ,    ('__NB_getval__'(verbose,normal,fail)->writeln(BO);true)
    ->
    true
    ;
    BO = BO1
    ).



% expand module names in a clause (interface predicate).
% A1: Input Clause
% A2: Output Class to Compiler (lives in module HM)
% A3: Output Class to clause/2 and listing (lives in module HM)
%
% modules:
% A6: head module (this is the one used in compiling and accessing).
% A5: context module (this is the current context
% A4: module for body of clause (this is the one used in looking up predicates)
%
% has to be last!!!
'$expand_clause'(HB, Asserting, Cl1, ClO) :- % MHB is the original clause, SM0 the current source, Cl1 and ClO output clauses
    source_module(SM0),
    '$head_and_body'(HB, MHH, B0),           % HB is H :- B.
    '$yap_strip_module'(MHH, HM0, H), % further module expansion
    '$module_u_vars'(HM0, H,  HVars),	 % collect head variables in
    '$expand_head'(H, H0, HM0, HM),
 %   ('__NB_getval__'(verbose,normal,fail)->writeln(B0);true),
    '$expand_clause_body'(B0,HVars-B0*Asserting, H, HM, SM0, SM0, B1, BO ),
    '$build_up'(HM, H0, SM0, B1, Cl1, BO, ClO).
    

%% @}
