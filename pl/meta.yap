/**

  @file meta.yap

  @defgroup YAPMetaPredicates Using Meta-Calls with Modules
 @ingroup YAPModules
 @{

  @pred meta_predicate(G1 , Gj , Gn) is directive

Declares that this predicate manipulates references to predicates.
Each _Gi_ is a mode specification.

If the argument is `:`, it does not refer directly to a predicate
but must be module expanded. If the argument is an integer, the argument
is a goal or a closure and must be expanded. Otherwise, the argument is
not expanded. Note that the system already includes declarations for all
built-ins.

For example, the declaration for call/1 and setof/3 are:

```
:- meta_predicate call(0), setof(?,0,?).
```

meta_predicate declaration
 implemented by asserting

meta_predicate(SourceModule,Declaration)

*/

% directive now meta_predicate Ps :- $meta_predicate(Ps).

:- use_system_module( '$_arith', ['$c_built_in'/4]).

%% handle module transparent predicates by defining a
%% new context module.
'$is_mt'(H, B, HM, _SM, M, (context_module(CM),B), CM) :-
    '$yap_strip_module'(HM:H, M, NH),
    '$module_transparent'(_, M, _, NH).


% I assume the clause has been processed, so the
% var case is long gone! Yes :)

'$clean_cuts'(!,true):- !.
'$clean_cuts'(G,(current_choice_point(DCP),NG)) :-
	'$conj_has_cuts'(G,DCP,NG,OK), OK == ok, !.
'$clean_cuts'(G,G).

'$clean_cuts'(!,_,true):- !.
'$clean_cuts'(G,DCP,NG) :-
	'$conj_has_cuts'(G,DCP,NG,OK), OK == ok, !.
'$clean_cuts'(G,_,G).

'$conj_has_cuts'(V,_,V, _) :- var(V), !.
'$conj_has_cuts'(!,DCP,cut_by(DCP), ok) :- !.
'$conj_has_cuts'((G1,G2),DCP,(NG1,NG2), OK) :- !,
	'$conj_has_cuts'(G1, DCP, NG1, OK),
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1;G2),DCP,(NG1;NG2), OK) :- !,
	'$conj_has_cuts'(G1, DCP, NG1, OK),
	'$conj_has_cuts'(G2, DCP, NG2, OK).
'$conj_has_cuts'((G1->G2),DCP,(NG1->G2), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G1, DCP, NG1, OK).
'$conj_has_cuts'((G1*->G2),DCP,(NG1,G2), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G1, DCP, NG1, OK).
'$conj_has_cuts'(if(G1,G2,G3),DCP,if(NG1,G2,G3), OK) :- !,
	% G1: the system must have done it already
	'$conj_has_cuts'(G1, DCP, NG1, OK).
'$conj_has_cuts'(G,_,G, _).

% return list of vars in expanded positions on the head of a clause.
%
% these variables should not be expanded by meta-calls in the body of the goal.
%
%  should be defined before caller.
%
'$module_u_vars'(M, H, UVars) :-
    '$do_module_u_vars'(M:H,UVars).

'$do_module_u_vars'(M:H,UVars) :-
    nonvar(H),
    nonvar(M),
	functor(H,F,N),
	functor(D,F,N),
	(
	    '$is_metapredicate'(D,M)
	->
	(
	    recorded('$m' , meta_predicate(M,D),_)
	;
	    recorded('$m' , meta_predicate(prolog,D),_)
	)
	),
	!,
	'$do_module_u_vars'(N,D,H,UVars).
'$do_module_u_vars'(_,[]).

'$do_module_u_vars'(0,_,_,[]) :- !.
'$do_module_u_vars'(I,D,H,LF) :-
	arg(I,D,X), ( X=':' -> true ; integer(X)),
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

'$expand_args'([],  _, [], _, []).
'$expand_args'([A0|GArgs], SM0,   [0|GDefs], HVars, [NA|NGArgs]) :-
    '$yap_strip_module'(SM0:A0,SM,A),    
    '$expand_0arg'(A, SM, HVars, NA),
    '$expand_args'(GArgs, SM0, GDefs, HVars, NGArgs).
'$expand_args'([A0|GArgs], SM0,   [N|GDefs], HVars, [NA|NGArgs]) :-
    number(N),
    !,
    '$yap_strip_module'(SM0:A0,SM,A),    
    '$expand_narg'(A, SM, N, HVars, NA),
    '$expand_args'(GArgs, SM0, GDefs, HVars, NGArgs).
'$expand_args'([A0|GArgs], SM0,   [':'|GDefs], HVars, [NA|NGArgs]) :-
    !,
    '$yap_strip_module'(SM0:A0,SM,A),    
    '$expand_marg'(A, SM, HVars, NA),
    '$expand_args'(GArgs, SM0, GDefs, HVars, NGArgs).
'$expand_args'([A|GArgs], SM,   [_N|GDefs], HVars, [A|NGArgs]) :-
    '$expand_args'(GArgs, SM, GDefs, HVars, NGArgs).



'$expand_marg'(A, _SM, HVars, NA) :-
    '$identical_member'(A, HVars),
    !,
    A=NA.
'$expand_marg'(A, SM, _HVars, SM:A) :-
    !.

'$expand_narg'(A, _SM,   _N, HVars, NA) :-
    '$identical_member'(A, HVars),
    !,
    A=NA.
'$expand_narg'(A, SM,   _N, _HVars, NA) :-
    var(A),
    !,
    NA=SM:A.
'$expand_narg'(A, SM,  _N, _HVars, SM:A):-
    var(SM),
    !.
'$expand_narg'(M:A, _SM,   HVars, NA) :-
    !,
    '$expand_narg'(A, M,   HVars, NA).
'$expand_narg'(V^A, SM,   N, HVars, V^NA) :-
    '$expand_narg'(A, SM,   N, HVars, NA).
'$expand_narg'(A, SM,   N, HVars, NA) :-
    functor(A,call,N),
    !,
    A =.. [call,V|LA],
    (
	'$identical_member'(A, HVars)
    -> A=NA
    ;
    '$yap_strip_module'(SM:V,M,NV)
    ),
    NA =.. [call,M:NV|LA].
'$expand_narg'(A, SM,   _N, _HVars, SM:A).

'$expand_0arg'(A, _SM,   HVars, NA) :-
    '$identical_member'(A, HVars),
    !,
    A=NA.
'$expand_0arg'(A, SM,   _HVars, NA) :-
    var(A),
    !,
    NA=SM:A.
'$expand_0arg'(A, SM,   _HVars,SM:A):-
    var(SM),
    !.
'$expand_0arg'(V^A, SM,   HVars, V^NA) :-
    '$expand_0arg'(A, SM,   HVars, NA).
'$expand_0arg'(call(A), _SM,   HVars, call(A)) :-
    '$identical_member'(A, HVars).
'$expand_0arg'(call(A), SM,   _HVars, call(M:NA)) :-
    '$yap_strip_module'(SM:A, M,NA),
    (var(M);var(NA)),
    !.
'$expand_0arg'(A, SM,   HVars, MG) :-
    '$meta_expansion'(A, SM, SM, HVars,MG).

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
%       head variab'$expand_goals'(M:G,G1,GO,HM,SM,,_M,HVars)les.
%       goals or arguments/sub-arguments?
% I cannot use call here because of format/3
% modules:
% A4: module for body of clause (this is the one used in looking up predicates)
% A5: context module (this is the current context
				% A6: head module (this is the one used in compiling and 
'$expand_goals'(V0,call(BM:V),call(BM:V),_HM,_SM,BM0,_HVarsH) :-
    '$yap_strip_module'(BM0:V0,  BM, V),
    \+ callable(BM:V),
    !.
'$expand_goals'((A*->B;C),(A1*->B1;C1),(AO*->BO;CO),
        HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
	'$clean_cuts'(AOO, AO),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
	'$expand_goals'(C,C1,CO,HM,SM,BM,HVars).
'$expand_goals'((A->B;C),(A1->B1;C1),
        (AO->BO;CO),
        HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
	'$clean_cuts'(AOO, AO),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
	'$expand_goals'(C,C1,CO,HM,SM,BM,HVars).
'$expand_goals'(if(A,B,C),O1,OO,HM,SM,BM,HVars) :- !,
    '$expand_goals'((A *-> B; C),O1,OO,HM,SM,BM,HVars).
'$expand_goals'((A,B),(A1,B1),(AO,BO),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'((A;B),(A1;B1),(AO;BO),HM,SM,BM,HVars) :-  !,
	'$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'((A|B),(A1|B1),(AO|BO),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO,HM,SM,BM,HVars),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'((A->B),(A1->B1),( AO-> BO),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
	'$clean_cuts'(AOO, AO),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars).
'$expand_goals'(\+G,\+G,A\=B,_HM,_BM,_SM,_HVars) :-
    nonvar(G),
    G = (A = B),
    !.
'$expand_goals'(\+A,\+A1,(AO-> fail;true),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AOO,HM,SM,BM,HVars),
	'$clean_cuts'(AOO, AO).
'$expand_goals'(once(A),once(A1),
	(AO->true),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
        '$clean_cuts'(AO0,AO).
'$expand_goals'((:-A),(:-A1),
	(:-AO),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO,HM,SM,BM,HVars).
'$expand_goals'(ignore(A),ignore(A1),
	(AO-> true ; true),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, AO).
'$expand_goals'(forall(A,B),forall(A1,B1),
		\+( (AO,\+ ( BO ) )),
		HM,SM,BM,HVars) :- !,
    '$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
	'$expand_goals'(B,B1,BO0,HM,SM,BM,HVars),
        '$clean_cuts'(AO0, AO),
        '$clean_cuts'(BO0, BO).
'$expand_goals'(not(A),not(A1),(current_choice_point(CP),AO,cut_by(CP) -> fail; true),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO,HM,SM,BM,HVars).
'$expand_goals'((A*->B),(A1*->B1),(AO,BO),HM,SM,BM,HVars) :- !,
	'$expand_goals'(A,A1,AO0,HM,SM,BM,HVars),
	'$expand_goals'(B,B1,BO,HM,SM,BM,HVars),
    '$clean_cuts'(AO0, AO).
'$expand_goals'(true,true,true,_,_,_,_) :- !.
'$expand_goals'(fail,fail,fail,_,_,_,_) :- !.
'$expand_goals'(false,false,false,_,_,_,_) :- !.
'$expand_goals'(G, G1, GO, HM, SM, BM, HVars) :-
    '$yap_strip_module'(BM:G,  NBM, GM),
    '$expand_goal'(GM, G1, GO, HM, SM, NBM, HVars).


'$import_expansion'(M:G, M1:G1) :-
    '$imported_predicate'(M:G, M1:G1),
     !.
'$import_expansion'(MG, MG).

'$meta_expansion'(G, GM, _SM, _HVars,M:G0) :-
    '$yap_strip_module'(GM:G, M, G0),
    (var(M);var(G0)),
    !.
'$meta_expansion'(goal_expansion(A,B), _GM, _SM, _HVars, goal_expansion(A,B)) :-
    !.
'$meta_expansion'(G, GM, _SM, HVars, OG) :-
    functor(G, F, Arity ),
	 functor(PredDef, F, Arity ),
	 '$is_metapredicate'(PredDef,GM),
	 recorded('$m' , meta_predicate(M0,PredDef),_),
	 (M0==GM->true;M0==prolog),
    !,
	 G =.. [F|LArgs],
	 PredDef =.. [F|LMs],
	 '$expand_args'(LArgs, GM, LMs, HVars, OArgs),
	 OG =.. [F|OArgs].
'$meta_expansion'(G, GM, _SM, _HVars, M:NG) :-
    '$yap_strip_module'(GM:G,M,NG).

 /**
 * @brief Perform meta-variable and user expansion on a goal _G_
 *
 * given the example
```
:- module(m, []).

o:p(B) :- n:g, X is 2+3, call(B).
```
 *
 * @param G input goal, without module quantification.
 * @param G1F output, non-optimised for debugging
 * @param GOF output, optimised, ie, `n:g`, `prolog:(X is 2+3)`, `call(m:B)`, where `prolog` does not  need to be explicit
 * @param GOF output, optimised, `n:g`, `prolog:(X=5)`,  `call(m:B)`
 * @param HM head module, input, o
 * @param HM source module, input, m
 * @param M current module, input, `n`, `m`, `m`
 * @param HVars-H, list of meta-variables and initial head, `[]` and `p(B)`
 *
 *
 */
'$expand_goal'(G0, G1F, GOF, HM, SM0, BM0, HVars-H) :-
     '$user_expansion'(G0 , NG0),
       % we have a context
     '$yap_strip_module'( BM0:NG0, M1, G1), % MON is both the source and goal module
      /* use the environments SM and HM$ */
      (G1== NG0
      ->
     SM=SM0,
      % we still may be using an imported predicate:
     '$import_expansion'(M1:G1, M2:G2),
     '$meta_expansion'(G2, M2, M1,   	HVars, G3),
    '$match_mod'(G3, HM, SM, M1, G1F),
    '$c_built_in'(G1F, M2, H, GOF)
      ;
      /* use the one(s) given by the user */
      SM = M1,
	   '$expand_goals'(NG0, G1F, GOF, HM, SM0, BM0, HVars-H)
      ).

'$user_expansion'(G0 , NG0) :-
    '$do_user_expansion'(G0 , IG0),
    IG0 \== G0,
    !,
    '$user_expansion'(IG0 , NG0).
'$user_expansion'(G0 , G0).

'$match_mod'(G0, HMod, SMod, M0, O) :-
    '$yap_strip_module'(M0:G0, M,G),
    (var(M)->
	 O=(M:G)
    ;var(G)->
	 O=(M:G)
    ;
	'$is_metapredicate'(G,M)
    ->
    O = M:G
    ;
      '$is_system_predicate'(G,prolog)
     ->
      O = G
    ;
      user == HMod, user == SMod, user == M
    ->
     O = G
    ;
    O=M:G
    ).

'$build_up'(HM, NH, SM, true, NH, true, NH) :- HM == SM, !.
'$build_up'(HM, NH, _SM, true, HM:NH, true, HM:NH) :- !.
'$build_up'(HM, NH, SM, B1, (NH :- B1), BO, ( NH :- BO)) :- HM == SM, !.
'$build_up'(HM, NH, _SM, B1, (NH :- B1), BO, ( HM:NH :- BO)) :- !.

'$expand_goals'(BM:G,H,HM,_SM,BM,B1,BO) :-
	    '$yap_strip_module'( BM:G, CM, G1),
	     !,
	     (var(CM) ->
	     B1=HM:call(CM:G1), BO = B1
	     ;
	     '$expand_goals'(G1,H,HM,CM,CM,B1,BO)
	     ).

'$expand_clause_body'(V, _NH1, _HM1, _SM, M, call(M:V), call(M:V) ) :-
    var(V), !.
'$expand_clause_body'(true, _NH1, _HM1, _SM, _M, true, true ) :- !.
'$expand_clause_body'(B, H, HM, SM, M, B1, BO ) :-
	'$module_u_vars'(HM , H, UVars),
				% collect head variables in
                                % expanded positions
                                % support for SWI's meta primitive.
	(
	  '$is_mt'(H, B, HM, SM, M, IB, BM)
	->
	  IB = B1, IB = BO0
	;
	  M = BM, '$expand_goals'(B, B1, BO0, HM, SM, BM, UVars-H)
	),
	(
	  '$full_clause_optimisation'(H, BM, BO0, BO)
	->
	  true
	;
	  BO = BO0
	).

%
% check if current module redefines an imported predicate.
% and remove import.
%
'$handle_import_conflict'(H, Mod) :-
    '$import'(NM,Mod,NH,H,_,_),
    NM \= Mod,
    functor(NH,N,Ar),
    print_message(warning,redefine_imported(Mod,NM,N/Ar)),
    retract('$import'(NM,Mod,NH,H,_,_)),
    fail.
'$handle_import_conflict'(_, _).


'$verify_import'(G, NG) :-
	'$imported_predicate'(G,NG).


'$expand_meta_call'(M0:G, HVars, M:GF ) :-
    !,
    '$yap_strip_module'(M0:G, M, IG),
    '$expand_goals'(IG, GF, _GF0, M, M, M, HVars-IG).
'$expand_meta_call'(G, HVars, M:GF ) :-
    current_source_module(SM0),
    '$yap_strip_module'(SM0:G, M, IG),
    '$expand_goals'(IG, GF, _GF0, SM, SM, M, HVars-IG).


'$expand_a_clause'(MHB, SM0, Cl1, ClO) :- % MHB is the original clause, SM0 the current source, Cl1 and ClO output clauses
     '$yap_strip_module'(SM0:MHB, SM, HB),  % remove layers of modules over the clause. SM is the source module.
    '$head_and_body'(HB, H, B),           % HB is H :- B.
    '$yap_strip_module'(SM:H, HM, NH), % further module expansion
    '$yap_strip_module'(SM:B, BM, B0), % further module expansion
    '$expand_clause_body'(B0, NH, HM, SM0, BM, B1, BO),
    !,
    '$build_up'(HM, NH, SM0, B1, Cl1, BO, ClO).
'$expand_a_clause'(Cl, _SM, Cl, Cl).
 



% expand arguments of a meta-predicate
% $meta_expansion(ModuleWhereDefined,CurrentModule,Goal,ExpandedGoal,MetaVariables)


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
expand_goal(Input, Output) :-
    '$expand_meta_call'(Input, [], Output ).


