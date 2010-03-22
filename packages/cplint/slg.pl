/***************************************************************************/
/*                                                                         */
/* The SLG System                                                          */
/* Authors: Weidong Chen and David Scott Warren                            */
/* Copyright (C) 1993 Southern Methodist University                        */
/*               1993 SUNY at Stony Brook                                  */
/* See file COPYRIGHT_SLG for copying policies and disclaimer.             */
/*                                                                         */
/***************************************************************************/

/*==========================================================================
  File               : slg.pl
  Last Modification  : November 14, 2007 by Fabrizio Riguzzi
===========================================================================*/

/* ----------- beginning of system dependent features ---------------------
   To run the SLG system under a version of Prolog other than Quintus,
   comment out the following Quintus-specific code, and include the code
   for the Prolog you are running.
*/

% Quintus
/* Begin Quintus specific code */
% :- use_module(library(basics)).
% :- dynamic 'slg$prolog'/1, 'slg$tab'/2.
% :- dynamic slg_expanding/0.
% :- dynamic wfs_trace/0.
/* End Quintus specific code */

% Sicstus
/* Begin Sicstus specific code */
/* append([],L,L).
 append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).

 member(X,[X|_]).
 member(X,[_|L]) :- member(X,L).

 memberchk(X,[X|_]) :- !.
 memberchk(X,[_|L]) :- memberchk(X,L).
*/
 :- dynamic 'slg$prolog'/1, 'slg$tab'/2.
 :- dynamic slg_expanding/0.
 :- dynamic wfs_trace/0.
/* End Sicstus specific code */

% XSB
/* Begin XSB specific code */
/* To compile this under xsb, you must allocate more than the default stack
   space when running xsb. E.g. use % xsb -m 2000
*/
%:- import member/2, memberchk/2, append/3, ground/1 from basics.
%:- import numbervars/3 from num_vars.
  
%:- dynamic slg_expanding/0.
%:- dynamic 'slg$prolog'/1, 'slg$tab'/2.
%:- dynamic wfs_trace/0.
/* End XSB specific code */

/* -------------- end of system dependent features ----------------------- */

/* -------------- beginning of slg_load routines -------------------------
  An input file may contain three kinds of directives (in addition to 
  regular Prolog clauses and commands):

  a) :- default(prolog).
     :- default(tabled).
     All predicates defined from now on are prolog (tabled) predicates
     unless specified otherwise later.
  b) :- tabled pred_name/arity.
     pred_name/arity is a tabled predicate. A comma separated list
     is also acceptable.

  c) :- prolog pred_name/arity.
     pred_name/arity is a prolog predicate. A comma separated list
     is also acceptable.

  Besides Prolog clauses, we allow general clauses where the body is a 
  universal disjunction of literals. Such clauses are specified in the form
         Head <-- Body.
  (Maybe <-- can be viewed as "All".) The head must be an atom of a tabled
  predicate and the body should be a disjunction of literals (separated by ';')
  and should not contain cut. The head must be ground whenever it is called. 
  All variables in the body that do not occur in the head are universally 
  quantified.

  There is NO support for module facilities. In particular, ALL TABLED
  PREDICATES SHOULD BE DEFINED IN MODULE 'user'.
*/

:- op(1200,xfx,<--).
:- op(1150,fx,[(tabled),(prolog),(default)]).
:- op(900,xfx,<-).

:- assert('slg$tabled'(0,0)), retractall('slg$tabled'(_,_)).
:- assert('slg$default'((prolog))).

do_term_expansion(end_of_file,_) :- !,
	retractall('slg$default'(_)),
	assert('slg$default'((prolog))),
	retractall('slg$prolog'(_)),
	retractall('slg$tab'(_,_)),
	fail.
do_term_expansion((:-Com),Clauses) :- !,
	expand_command(Com,Clauses).
do_term_expansion((H-->B),NewClause) :- !,
	\+ slg_expanding,
	assert(slg_expanding),
	expand_term((H-->B),Clause),
	retractall(slg_expanding),
	do_term_expansion(Clause,NewClause).
do_term_expansion((Head <-- Body),Clauses) :- !,
	functor(Head,P,A),
	Pred = P/A,
	( 'slg$tab'(P,A) ->
	  convert_univ_clause(Head,Body,Clauses)
	; 'slg$prolog'(Pred) ->
	  write('Error: Prolog predicate '), write(Pred),
	  write(' in clauses with universal disjunction.'),nl,
	  write('       Clause ignored: '), write((Head <-- Body)), nl,
	  Clauses = []
	; 'slg$default'(Default),
	  ( Default == (prolog) ->
	    write('Error: Prolog predicate '), write(Pred),
	    write(' in clauses with universal disjunction.'),nl,
	    write('       Clause ignored: '), write((Head <-- Body)), nl,
	    Clauses = []
	  ; assert('slg$tab'(P,A)),
	    retractall('slg$tabled'(P,A)),
	    assert('slg$tabled'(P,A)),
	    functor(NewHead,P,A),
	    Clauses = [(:- retractall('slg$tabled'(P,A)), assert('slg$tabled'(P,A))),
                         (NewHead :- slg(NewHead))|RestClauses],
            convert_univ_clause(Head,Body,RestClauses)
	  )
        ).
do_term_expansion(Clause,Clauses) :-
	( Clause = (Head :- Body) -> true; Head = Clause, Body = true ),
	functor(Head,P,A),
	Pred = P/A,
	( 'slg$tab'(P,A) ->
	  convert_tabled_clause(Head,Body,Clauses)
        ; 'slg$prolog'(Pred) ->
	  Clauses = Clause
        ; 'slg$default'(Default),
	  ( Default == (prolog) ->
	    Clauses = Clause
	  ; ( 'slg$tab'(P,A) ->
	      convert_tabled_clause(Head,Body,Clauses)
	    ; assert('slg$tab'(P,A)),
	      retractall('slg$tabled'(P,A)),
	      assert('slg$tabled'(P,A)),
	      functor(NewHead,P,A),
	      Clauses = [(:- retractall('slg$tabled'(P,A)), assert('slg$tabled'(P,A))),
			 (NewHead :- slg(NewHead))|RestClauses],
              convert_tabled_clause(Head,Body,RestClauses)
	    )
	  )
        ).
expand_command(tabled(Preds),Clauses) :-
	expand_command_table(Preds,Clauses,[]).
expand_command(prolog(Preds),Clauses) :-
	expand_command_prolog(Preds,Clauses,[]).
expand_command(multifile(Preds),(:-multifile(NewPreds))) :-
	add_table_preds(Preds,NewPreds,[]).
expand_command(dynamic(Preds),(:-dynamic(NewPreds))) :-
	add_table_preds(Preds,NewPreds,[]).
expand_command(default(D),[]) :-
	( (D == (prolog); D == (tabled)) ->
	  retractall('slg$default'(_)),
	  assert('slg$default'(D))
        ; write('Warning: illegal default '),
	  write(D),
	  write(' ignored.'),
	  nl
        ).

expand_command_table((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_table_one(Pred,Clauses0,Clauses1),
	expand_command_table(Preds,Clauses1,Clauses).
expand_command_table(Pred,Clauses0,Clauses) :-
	expand_command_table_one(Pred,Clauses0,Clauses).

expand_command_table_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Pred = P/A,
	  functor(H,P,A),
	  ( ( predicate_property(H,built_in); slg_built_in(H) ) ->
	    write('ERROR: Cannot table built_in '),
	    write(Pred), nl,
	    Clauses0 = Clauses
	  ; 'slg$prolog'(Pred) ->
	    write('ERROR: '),
	    write(Pred),
	    write(' assumed to be a Prolog predicate'),
	    nl,
	    tab(7),
	    write('But later declared a tabled predicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; 'slg$tab'(P,A) ->
	    Clauses0 = Clauses
	  ; assert('slg$tab'(P,A)),
	    retractall('slg$tabled'(P,A)),
	    assert('slg$tabled'(P,A)),
	    Clauses0 = [(:- retractall('slg$tabled'(P,A)), assert('slg$tabled'(P,A))),
	                (H :- slg(H))|Clauses]
	  ).

expand_command_prolog((Pred,Preds),Clauses0,Clauses) :- !,
	expand_command_prolog_one(Pred,Clauses0,Clauses1),
	expand_command_prolog(Preds,Clauses1,Clauses).
expand_command_prolog(Pred,Clauses0,Clauses) :-
	expand_command_prolog_one(Pred,Clauses0,Clauses).

expand_command_prolog_one(Pspec,Clauses0,Clauses) :-
	  ( Pspec = P/A -> true; P = Pspec, A = 0 ),
	  Pred = P/A,
	  ( 'slg$tab'(P,A) ->
	    write('ERROR: '),
	    write(Pred),
	    write(' assumed to be a tabled predicate'),
	    nl,
	    tab(7),
	    write('But later declared a Prolog predicate.'),
	    nl,
	    Clauses0 = Clauses
	  ; retractall('slg$tab'(P,A)),
	    retractall('slg$tabled'(P,A)),
	    ( 'slg$prolog'(Pred) ->
	      true
	    ; assert('slg$prolog'(Pred))
	    ),
	    Clauses0 = [(:- retractall('slg$tabled'(P,A)))|Clauses]
          ).

add_table_preds(Preds,NewPreds0,NewPreds) :-
	( Preds == [] ->
	  NewPreds0 = NewPreds
        ; Preds = [P|Ps] ->
	  add_table_preds(P,NewPreds0,NewPreds1),
	  add_table_preds(Ps,NewPreds1,NewPreds)
        ; Preds = (P,Ps) ->
	  add_table_preds(P,NewPreds0,NewPreds1),
	  add_table_preds(Ps,NewPreds1,NewPreds)
        ; ( Preds = P/A -> true; P = Preds, A = 0 ),
	  ( 'slg$tab'(P,A) ->
	    name(P,Pl),
	    name(NewP,[115,108,103,36|Pl]), % 'slg$'
	    NewA is A+1,
	    NewPreds0 = [P/A,NewP/NewA|NewPreds]
	  ; NewPreds0 = [P/A|NewPreds]
          )
        ).

convert_tabled_clause(Head,Body,Clauses0) :-
	  conj_to_list(Body,Blist),
	  extract_guard(Blist,Guard,[],Nbody,Clauses0,Clauses),
	  list_to_conj(Guard,Gconj),
	  new_slg_head(Head,Nbody,NewHead),
	  ( Gconj == true ->
	    Clauses = [NewHead]
	  ; Clauses = [(NewHead :- Gconj)]
          ).

convert_univ_clause(Head,Body,Clauses) :-
	disj_to_list(Body,Blist),
	new_slg_head(Head,all(Blist),NewHead),
	Clauses = [(NewHead :- ( ground0(Head) -> 
	                         true
			       ; write('Error: Non-ground call '),
			         write(Head),
				 write(' in a clause with universal disjunction.'),
				 nl
			       ))].

ground0(X) :- ground(X).

conj_to_list(Term,List) :-
	conj_to_list(Term,List,[]).
conj_to_list(Term,List0,List) :-
	( Term = (T1,T2) ->
	  conj_to_list(T1,List0,List1),
	  conj_to_list(T2,List1,List)
        ; Term == true ->
	  List0 = List
        ; List0 = [Term|List]
        ).

disj_to_list(Term,List) :-
	disj_to_list(Term,List,[]).
disj_to_list(Term,List0,List) :-
	( Term = (T1;T2) ->
	  disj_to_list(T1,List0,List1),
	  disj_to_list(T2,List1,List)
        ; Term == true ->
	  List0 = List
        ; List0 = [Term|List]
        ).

extract_guard([],G,G,[],Cls,Cls).
extract_guard([Lit|List],G0,G,Rest,Cls0,Cls) :-
	( Lit = (\+N) ->
	  Nlit = N
        ; Nlit = Lit
        ),
	( ( predicate_property(Nlit,built_in); slg_built_in(Nlit) ) ->
	  G0 = [Lit|G1],
	  extract_guard(List,G1,G,Rest,Cls0,Cls)
        ; functor(Nlit,P,A),
	  Pred = P/A,
	  ( 'slg$tab'(P,A) ->
	    G0 = G,
	    Rest = [Lit|List],
	    Cls0 = Cls
	  ; 'slg$prolog'(Pred) ->
	    G0 = [Lit|G1],
	    extract_guard(List,G1,G,Rest,Cls0,Cls)
	  ; 'slg$default'((prolog)) ->
	    G0 = [Lit|G1],
	    assert('slg$prolog'(Pred)),
	    Cls0 = [(:- 'slg$prolog'(Pred) -> true; assert('slg$prolog'(Pred)))|Cls1],
	    extract_guard(List,G1,G,Rest,Cls1,Cls)
	  ; 'slg$default'((tabled)) ->
	    G0 = G,
	    Rest = [Lit|List],
	    assert('slg$tab'(P,A)),
	    retractall('slg$tabled'(P,A)),
            assert('slg$tabled'(P,A)),
	    functor(Head,P,A),
	    Cls0 = [(:- retractall('slg$tabled'(P,A)), assert('slg$tabled'(P,A))),
                    (Head :- slg(Head))|Cls]
	  )
        ).

list_to_conj([],true).
list_to_conj([Lit|List],G0) :-
	( List == [] ->
	  G0 = Lit
        ; G0 = (Lit,G),
	  list_to_conj(List,G)
        ).

new_slg_head(Head,Body,NewHead) :-
	functor(Head,P,A),
	name(P,Pl),
	name(Npred,[115,108,103,36|Pl]), % 'slg$'
	Narity is A+1,
	functor(NewHead,Npred,Narity),
	arg(Narity,NewHead,Body),
	put_in_args(0,A,Head,NewHead).

put_in_args(A,A,_,_).
put_in_args(A0,A,Head,NewHead) :-
	A0 < A,
	A1 is A0+1,
	arg(A1,Head,Arg),
	arg(A1,NewHead,Arg),
	put_in_args(A1,A,Head,NewHead).

slg_built_in(slg(_)).
slg_built_in(_<-_).
slg_built_in(slgall(_,_)).
slg_built_in(slgall(_,_,_,_)).
slg_built_in(emptytable(_)).
slg_built_in(st(_,_)).
slg_built_in(stnot(_,_)).
slg_built_in(stall(_,_,_)).
slg_built_in(stall(_,_,_,_,_)).
slg_built_in(stselect(_,_,_,_)).
slg_built_in(stselect(_,_,_,_,_,_)).
slg_built_in(xtrace).
slg_built_in(xnotrace).

/* ----------------- end of slg_load routines --------------------------- */

/* SLG tracing:
   xtrace: turns SLG trace on, which prints out tables at various 
           points
   xnotrace: turns off SLG trace
*/
xtrace :- 
    ( wfs_trace -> 
      true 
    ; assert(wfs_trace)
    ).
xnotrace :- 
    ( wfs_trace -> 
      retractall(wfs_trace) 
    ; true
    ).

/* isprolog(Call): Call is a Prolog subgoal */
isprolog(Call) :-
        functor(Call,P,A),
        \+ 'slg$tabled'(P,A).

/* slg(Call):
   It returns all true answers of Call under the well-founded semantics
   one by one.
*/
slg(Call) :-
        ( isprolog(Call) ->
          call(Call)
        ; oldt(Call,Tab),
          ground(Call,Ggoal),
          find(Tab,Ggoal,Ent),
          ent_to_anss(Ent,Anss),
          member_anss(d(Call,[]),Anss)
        ).

/* Call<-Cons:
   It returns all true or undefined answers of Call one by one. In
   case of a true answer, Cons = []. For an undefined answer,
   Cons is a list of delayed literals.
*/
Call<-Cons :-
        ( isprolog(Call) ->
          call(Call),
          Cons = []
        ; oldt(Call,Tab),
          ground(Call,Ggoal),
          find(Tab,Ggoal,Ent),
          ent_to_anss(Ent,Anss),
          member_anss(d(Call,Cons),Anss)
        ).

/* emptytable(EmptTab): creates an initial empty stable.
*/
emptytable(0:[]).

/* slgall(Call,Anss):
   slgall(Call,Anss,N0-Tab0,N-Tab):
   If Call is a prolog call, findall is used, and Tab = Tab0;
   If Call is an atom of a tabled predicate, SLG evaluation
   is carried out.
*/
slgall(Call,Anss) :-
	slgall(Call,Anss,0:[],_).
slgall(Call,Anss,N0:Tab0,N:Tab) :-
        ( isprolog(Call) ->
          findall(Call,Call,Anss),
	  N = N0, Tab = Tab0
        ; ground(Call,Ggoal),
          ( find(Tab0,Ggoal,Ent) ->
            ent_to_anss(Ent,Answers),
            Tab = Tab0
          ; new_init_call(Call,Ggoal,Ent,[],S1,1,Dfn1),
            add_tab_ent(Ggoal,Ent,Tab0,Tab1),
            oldt(Call,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxint-maxint,_Dep,N0:[],N:_TP),
            find(Tab,Ggoal,NewEnt),
            ent_to_anss(NewEnt,Answers)
          ),
          ansstree_to_list(Answers,Anss,[])
        ).

/* st(Call,PSM):
   stnot(Call,PSM):
   It finds a stable model in which Call must be true (false).
   Call must be ground.
*/
st(Call,PSM) :-
	st_true_false(Call,true,PSM).
stnot(Call,PSM) :-
	st_true_false(Call,false,PSM).

st_true_false(Call,Val,PSM) :-
	( isprolog(Call) ->
	  PSM = [],
	  call(Call)
        ; ground(Call) ->
	  wfs_newcall(Call,[],Tab1,0,_),
	  find(Tab1,Call,Ent),
	  ent_to_anss(Ent,Anss),
	  ( succeeded(Anss) ->
	    ( Val == true ->
	      PSM = []
	    ; fail
	    )
	  ; failed(Anss) ->
	    ( Val == false ->
	      PSM = []
	    ; fail
	    )
	  ; assume_one(Call,Val,Tab1,Tab2,[],Abd1,A0,A1),
	    collect_unds(Anss,A1,A),
	    st(A0,A,Tab2,Tab3,Abd1,Abd,[],DAbd,[],_Plits),
	    final_check(Abd,Tab3,_Tab,DAbd,PSM)
	  )
        ; write('Error: non-ground call '),
	  write(Call),
	  write(' in st/2.'),
	  nl,
	  fail
        ).

/* stall(Call,Anss,PSM):
   stall(Call,Anss,PSM,Tab0,Tab):
   It computes a partial stable model PSM and collects all
   answers of Call in that model.
*/
stall(Call,Anss,PSM) :-
	stall(Call,Anss,PSM,0:[],_).

stall(Call,Anss,PSM,N0:Tab0,N:Tab) :-
	( isprolog(Call) ->
	  findall(Call,Call,Anss),
	  PSM = [], N = N0, Tab = Tab0
        ; ground(Call,Ggoal),
	  ( find(Tab0,Ggoal,Ent) ->
	    Tab1 = Tab0, N = N0
          ; wfs_newcall(Call,Tab0,Tab1,N0,N),
	    find(Tab1,Ggoal,Ent)
          ),
	  ent_to_delay(Ent,Delay),
	  ( Delay == false ->
	    Fent = Ent, PSM = [], Tab = Tab1
	  ; ent_to_anss(Ent,Anss0),
	    collect_unds(Anss0,A0,A),
	    st(A0,A,Tab1,Tab2,[],Abd,[],DAbd,[],_Plits),
	    final_check(Abd,Tab2,Tab,DAbd,PSM),
	    find(Tab,Ggoal,Fent)
	  ),
	  ent_to_anss(Fent,Anss1),
          ansstree_to_list(Anss1,Anss,[])
        ).

/* stselect(Call,PSM0,Anss,PSM):
   stselect(Call,PSM0,Anss,PSM,N0:Tab0,N:Tab):
   It computes a partial stable model PSM in which all ground
   literals in PSM0 are true, and returns all answers of Call
   in the partial stable model. Call must be an atom of a tabled
   or stable predicate.
*/
stselect(Call,PSM0,Anss,PSM) :-
	stselect(Call,PSM0,Anss,PSM,0:[],_).

stselect(Call,PSM0,Anss,PSM,N0:Tab0,N:Tab) :-
	( isprolog(Call) ->
	  write('Error: Prolog predicate '),
	  write(Call),
	  write('stselect.'),
	  fail
        ; wfsoldt(Call,PSM0,Ent,Tab0,Tab1,N0,N),
	  ent_to_delay(Ent,Delay),
	  assume_list(PSM0,true,Tab1,Tab2,[],Abd0,A0,A1),
	  ( Delay == false ->
	    A1 = A2
          ; ent_to_anss(Ent,Anss0),
	    collect_unds(Anss0,A1,A2)
          ),
	  st(A0,A2,Tab2,Tab3,Abd0,Abd,[],DAbd,[],_Plits),
	  final_check(Abd,Tab3,Tab,DAbd,PSM),
	  ground(Call,Ggoal),
	  find(Tab,Ggoal,Fent),
	  ent_to_anss(Fent,Anss1),
	  ansstree_to_list(Anss1,Anss,[])
        ).

wfsoldt(Call,PSM0,Ent,Tab0,Tab,N0,N) :-
	ground(Call,Ggoal),
	( find(Tab0,Ggoal,Ent) ->
	  Tab1 = Tab0, N1 = N0
        ; wfs_newcall(Call,Tab0,Tab1,N0,N1),
	  find(Tab1,Ggoal,Ent)
        ),
	wfsoldt_ground(PSM0,Tab1,Tab,N1,N).

wfsoldt_ground([],Tab,Tab,N,N).
wfsoldt_ground([A|PSM],Tab0,Tab,N0,N) :-
	( ground(A) ->
	  true
        ; write('Error: non-ground assumption in stable model selection: '),
	  write(A), nl, fail
        ),
	( A = (\+G) ->
	  true
        ; A = G
        ),
	( isprolog(G) ->
	  Tab1 = Tab0, N1 = N0,
	  call(A)
        ; find(Tab0,G,_) ->
	  Tab1 = Tab0, N1 = N0
        ; wfs_newcall(G,Tab0,Tab1,N0,N1)
        ),
	wfsoldt_ground(PSM,Tab1,Tab,N1,N).

wfs_newcall(Call,Tab0,Tab,N0,N) :-
	new_init_call(Call,Ggoal,Ent0,[],S1,1,Dfn1),
	add_tab_ent(Ggoal,Ent0,Tab0,Tab1),
	oldt(Call,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxint-maxint,_Dep,N0:[],N:_TP).
	
/* collect_unds(Anss,A0,A):
   collects all delayed literals in answers Anss in a open-ended difference
   list A0/A. These delayed literals are assumed either false or true in the
   stable model computation.
*/
collect_unds([],A,A).
collect_unds(l(_GH,Lanss),A1,A) :-
	collect_unds_lanss(Lanss,A1,A).
collect_unds(n2(T1,_,T2),A1,A) :-
	collect_unds(T1,A1,A2),
	collect_unds(T2,A2,A).
collect_unds(n3(T1,_,T2,_,T3),A1,A) :-
	collect_unds(T1,A1,A2),
	collect_unds(T2,A2,A3),
	collect_unds(T3,A3,A).

collect_unds_lanss([],A,A).
collect_unds_lanss([d(_,D)|Lanss],A1,A) :-
	collect_unds_list(D,A1,A2),
	collect_unds_lanss(Lanss,A2,A).

collect_unds_list([],A,A).
collect_unds_list([Lit|D],[Lit|A1],A) :-
	collect_unds_list(D,A1,A).

/* st(A0,A,Tab0,Tab,Abd0,Abd,DAbd0,DAbd,Plits0,Plits):
   A0/A is an open-ended difference list containing a list of
   delayed literals. st tries for each delayed literal to 
   assume that it is true or false and checks to see if 
   it leads to a partial stable model. Propagation of assumed
   truth values is carried out as much as possible. It will 
   fail if the relevant program contains p :- \+p.

   Abd0/Abd is an accumulator for a table of assumed truth 
   values. They are checked against the table Tab0/Tab for
   consistency later in check_consistency. DAbd0/DAbd is an 
   accumulator for truth values of undefined literals that
   are derived from assumed truth values of other literals.
   Plits0/Plits is an accumulator for avoiding positive 
   infinite loops in processing positive delayed literals.
*/
st(A0,A,Tab0,Tab,Abd0,Abd,DAbd0,DAbd,Plits0,Plits) :-
	( % empty difference list
	  A0 == A ->
	  Tab = Tab0, Abd = Abd0, DAbd = DAbd0, Plits = Plits0
        ; A0 = [Lit|A1],
	  ( % non-ground negative literals
	    Lit = (Ggoal - (\+GH)) ->
	    write('Error: cannot handle non-ground negative literals: '),
	    write(\+GH), nl, fail
	  ; % positive undefined literal
	    Lit = Ggoal-GH ->
	    ( % encountered before
	      find(Plits0,Lit,_) ->
	      st(A1,A,Tab0,Tab,Abd0,Abd,DAbd0,DAbd,Plits0,Plits)
	    ; % otherwise, process undefined literals it depends upon
	      addkey(Plits0,Lit,_,Plits1),
	      find(Tab0,Ggoal,Ent),
	      ent_to_anss(Ent,Anss),
	      find(Anss,GH,Lanss),
	      collect_unds_lanss(Lanss,A,NewA),
	      st(A1,NewA,Tab0,Tab,Abd0,Abd,DAbd0,DAbd,Plits1,Plits)
	    )
	  ; % negative undefined literal
	    Lit = (\+G) ->
	    ( % has been assumed or derived to be true or false
	      ( find(Abd0,G,_Val); find(DAbd0,G,_) ) -> 
	      st(A1,A,Tab0,Tab,Abd0,Abd,DAbd0,DAbd,Plits0,Plits)
	    ; find(Tab0,G,Gent),
	      ent_to_anss(Gent,Ganss),
	      ( % found to be false already
	        failed(Ganss) ->
		addkey(DAbd0,G,false,DAbd1),
	        st(A1,A,Tab0,Tab,Abd0,Abd,DAbd1,DAbd,Plits0,Plits)
	      ; % found to be true already 
	        succeeded(Ganss) ->
		addkey(DAbd0,G,true,DAbd1),
	        st(A1,A,Tab0,Tab,Abd0,Abd,DAbd1,DAbd,Plits0,Plits)
	      ; % create a choice point
	        addkey(Abd0,G,Val,Abd1),
		( Ganss = l(G,[d(G,Ds)]), memberchk(\+G,Ds) ->
		  Val = false
	        ; ( Val = false; Val = true )
	        ),
	        propagate_forward(G,Val,Tab0,Tab1,Abd1),
	        A = [G-G|NewA], % make sure delayed literals of G are checked
	        propagate_backward(G,Val,Ganss,Tab1,Tab2,Abd1,Abd2,NewA,NNA),
	        st(A1,NNA,Tab2,Tab,Abd2,Abd,DAbd0,DAbd,Plits0,Plits)
	      )
	    )
          )
        ).

/* propagate_forward(G,Val,Tab0,Tab,Abd):
   G has been assumed to be Val, and this information is propagated
   using simplification or forward chaining links as much as 
   possible.
*/
propagate_forward(G,Val,Tab0,Tab,Abd) :-
	updatevs(Tab0,G,Ent0,Ent,Tab1),
	Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn,Slist0),
	Ent = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn,Slist),
	extract_known_by_abd(Slist0,Val,[],Slist,[],Klist),
	simplify(Klist,Tab1,Tab,Abd).

/* The forward chaining is such that negative literals can fail 
   or succeed by assumption, and positive literals can fail 
   by assumption, but cannot succeed by assumption.
   This avoids the construction of supported models that are 
   not stable.
*/
extract_known_by_abd([],_,Slist,Slist,Klist,Klist).
extract_known_by_abd([Link|Links],Val,Slist0,Slist,Klist0,Klist) :-
	( Link = (_ : (\+ _)) ->
	  ( Val == false ->
	    Slist1 = Slist0, 
	    Klist1 = [succ-Link|Klist0]
	  ; Val == true ->
	    Slist1 = Slist0, 
	    Klist1 = [fail-Link|Klist0]
	  ; Slist1 = [Link|Slist0], 
	    Klist1 = Klist0
	  )
        ; % Link = (_ : _-GH) ->
	  ( Val = false ->
	    Slist1 = Slist0,
	    Klist1 = [fail-Link|Klist0]
	  ; % Val = true ->
	    Slist1 = [Link|Slist0],
	    Klist1 = Klist0
	  )
        ),
	extract_known_by_abd(Links,Val,Slist1,Slist,Klist1,Klist).

/* propagate_backward(G,Val,Ganss,Tab0,Tab,Abd0,Abd,A,NewA):
   It tried to propagate the Val of G backward through answers
   if possible. If G is assumed to be true, and G has only one
   answer clause, then all literals in the body of the answer
   clause must be true. If G is assumed to be false, then all
   literals in answer clauses of G that have a single literal
   are assumed to be false too. Otherwise, it is no-op.
*/
propagate_backward(G,Val,Ganss,Tab0,Tab,Abd0,Abd,A,NewA) :-
	( Ganss = l(G,Lanss) ->
	  ( Val == true, Lanss = [d(G,Ds)] ->
	    assume_list(Ds,true,Tab0,Tab,Abd0,Abd,A,NewA)
	  ; Val == false, findall(Lit,member(d(G,[Lit]),Lanss),Ds) ->
	    assume_list(Ds,false,Tab0,Tab,Abd0,Abd,A,NewA)
	  ; Tab = Tab0, Abd = Abd0, A = NewA
          )
        ; Tab = Tab0, Abd = Abd0, A = NewA
        ).

assume_list([],_Val,Tab,Tab,Abd,Abd,A,A).
assume_list([Lit|Lits],Val,Tab0,Tab,Abd0,Abd,A0,A) :-
	assume_one(Lit,Val,Tab0,Tab1,Abd0,Abd1,A0,A1),
	assume_list(Lits,Val,Tab1,Tab,Abd1,Abd,A1,A).

/* assume_one(Lit,Val,Tab0,Tab,Abd0,Abd,A0,A):
   Due to back propagation, Lit is assumed to be Val.
   However, this assumption is carried out only if 
   Lit is a delayed literal of a ground call or most
   general call.
*/
assume_one(Ggoal-GH,_Val,Tab0,Tab,Abd0,Abd,A0,A) :-
	Ggoal \== GH, 
	!,
	Tab = Tab0, Abd = Abd0, A = A0.
assume_one(Lit,Val,Tab0,Tab,Abd0,Abd,A0,A) :-
	( Lit = G-G ->
	  GVal = Val
        ; Lit = (\+G) ->
	  ( Val == true -> GVal = false; GVal = true )
        ; Lit = G ->
	  GVal = Val
        ),
	( find(Abd0,G,V) ->              % already assumed
	  ( V == GVal ->
	    Tab = Tab0, Abd = Abd0, A = A0
	  ; fail
          )
        ; find(Tab0,G,Gent),
	  ent_to_anss(Gent,Ganss),
	  ( failed(Ganss) ->             % already known
	    ( GVal == true -> 
	      fail
	    ; Tab = Tab0, Abd = Abd0, A = A0
	    )
	  ; succeeded(Ganss) ->          % already known
	    ( GVal == false -> 
	      fail
	    ; Tab = Tab0, Abd = Abd0, A = A0
            )
	  ; addkey(Abd0,G,GVal,Abd1),    % otherwise, propagate
	    propagate_forward(G,GVal,Tab0,Tab1,Abd1),
	    A0 = [G-G|A1],
	    propagate_backward(G,Ganss,GVal,Tab1,Tab,Abd1,Abd,A1,A)
	  )
        ).

final_check(Abd,Tab0,Tab,DAbd,Alist) :-
	check_consistency(Abd,Tab0,Tab,Alist0,Alist1),
	add_dabd(DAbd,Alist1,[]),
	sort(Alist0,Sorted),
	listval_to_listlit(Sorted,Alist).

listval_to_listlit([],[]).
listval_to_listlit([Val|Vlist],[Lit|Llist]) :-
	val_to_lit(Val,Lit),
	listval_to_listlit(Vlist,Llist).

val_to_lit(G-true,G).
val_to_lit(G-false,\+G).

/* check_consistency(Abd,Tab0,Tab,Alist0,Alist):
   A proposition may be assumed to be true, but no true answer
   is derived at the end, which is inconsistency. A proposition
   may be assumed to be false, but has a true answer. The latter
   case is checked when the true answer is derived. Here Abd 
   indicates the assumed truth values, and answers in Tab0
   indicate the derived values by a fixpoint computation of
   forward chaining.

   Also at the end of a fixpoint computation, a subgoal may
   have only delayed answers with positive literals. These
   have to be deleted in order for Tab0/Tab to be used
   correctly later.
*/
check_consistency([],Tab,Tab,Alist,Alist).
check_consistency(l(G,Val),Tab0,Tab,Alist0,Alist) :-
	updatevs(Tab0,G,Ent0,Ent,Tab),
	Ent0 = e(Nodes,ANegs,Anss0,_Delay,Comp,Dfn,Slist),
	Ent = e(Nodes,ANegs,Anss,false,Comp,Dfn,Slist),
	( Val == true ->
	  succeeded(Anss0),
	  Anss = l(G,[d(G,[])]), % delete answers with positive delays
	  Alist0 = [G-Val|Alist]
        ; % Val == false -> 
	  Anss = [],
	  Alist0 = [G-Val|Alist]
        ).
check_consistency(n2(T1,_,T2),Tab0,Tab,Alist0,Alist) :-
	check_consistency(T1,Tab0,Tab1,Alist0,Alist1),
	check_consistency(T2,Tab1,Tab,Alist1,Alist).
check_consistency(n3(T1,_,T2,_,T3),Tab0,Tab,Alist0,Alist) :-
	check_consistency(T1,Tab0,Tab1,Alist0,Alist1),
	check_consistency(T2,Tab1,Tab2,Alist1,Alist2),
	check_consistency(T3,Tab2,Tab,Alist2,Alist).

add_dabd([],Alist,Alist).
add_dabd(l(G,Val),[G-Val|Alist],Alist).
add_dabd(n2(T1,_,T2),Alist0,Alist) :-
	add_dabd(T1,Alist0,Alist1),
	add_dabd(T2,Alist1,Alist).
add_dabd(n3(T1,_,T2,_,T3),Alist0,Alist) :-
	add_dabd(T1,Alist0,Alist1),
	add_dabd(T2,Alist1,Alist2),
	add_dabd(T3,Alist2,Alist).

/* oldt(QueryAtom,Table): top level call for SLG resolution.
   It returns a table consisting of answers for each relevant
   subgoal. For stable predicates, it basically extract the 
   relevant set of ground clauses by solving Prolog predicates
   and other well-founded predicates.
*/
oldt(Call,Tab) :-
    new_init_call(Call,Ggoal,Ent,[],S1,1,Dfn1),
    add_tab_ent(Ggoal,Ent,[],Tab1),
    oldt(Call,Ggoal,Tab1,Tab,S1,_S,Dfn1,_Dfn,maxint-maxint,_Dep,0:[],_TP),
    ( wfs_trace -> 
      nl, write('Final '), display_table(Tab), nl
    ; true 
    ).

/* oldt(Call,Ggoal,Tab0,Tab,Stack0,Stack,DFN0,DFN,Dep0,Dep,TP0,TP)
   explores the initial set of edges, i.e., all the 
   program clauses for Call. Ggoal is of the form 
   Gcall-Gdfn, where Gcall is numbervar of Call and Gdfn
   is the depth-first number of Gcall. Tab0/Tab,Stack0/Stack,
   DFN0/DFN, and Dep0/Dep are accumulators for the table, 
   the stack of subgoals, the DFN counter, and the dependencies.
   TP0/TP is the accumulator for newly created clauses during
   the processing of general clauss with universal disjunctions
   in the body. These clauses are created in order to guarantee
   polynomial data complexity in processing clauses with
   universal disjuntions in the body of a clause. The newly 
   created propositions are represented by numbers.
*/
oldt(Call,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( number(Call) ->
      TP0 = (_ : Tcl),
      find(Tcl,Call,Clause),
      edge_oldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1)
    ; findall(rule(d(Call,[]),Body),
	      (new_slg_head(Call,Body,NewHead),call(NewHead)),
	      Frames),
      map_oldt(Frames,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1)
    ),
    comp_tab_ent(Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_oldt([],_Ggoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_oldt([Clause|Frames],Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
  edge_oldt(Clause,Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
  map_oldt(Frames,Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* edge_oldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Clause may be one of the following forms:
          rule(d(H,Dlist),Blist)
          rule(d(H,all(Dlist)),all(Blist))
   where the second form is for general clauses with a universal
   disjunction of literals in the body. Dlist is a list of delayed 
   literals, and Blist is the list of literals to be solved.
   Clause represents a directed edge from Ggoal to the left most 
   subgoal in Blist.
*/
edge_oldt(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(Ans,B),
    ( B == [] ->
      ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; B = [Lit|_] ->
      ( Lit = (\+N) ->
        neg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; pos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      )
    ; B = all(Bl) ->
      ( Bl == [] ->
        ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Bl = [Lit|_],
        ( Lit = (\+N) ->
          aneg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        ; apos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      )
    ).

ans_edge(Ans,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab1) -> 
      ( Mode = new_head -> 
        returned_ans(Ans,Ggoal,RAns),
        map_nodes(Nodes,RAns,Tab1,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      ; Mode = no_new_head ->
        Tab = Tab1, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
      )
    ; Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
    ).

neg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(_,[\+N|_]),
    ( ground(N) -> true
    ; write('Flounder: '), write(\+N), nl, fail
    ),
    Node = (Ggoal:Clause),
    Ngoal = N,                 % N is already ground
    ( isprolog(N) ->           % if N is a Prolog predicate
      ( call(N) ->             %    then just call
        Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
      ; apply_subst(Node,d(\+ N,[]),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
      )
    ; ( find(Tab0,Ngoal,Nent) ->
        Tab2 = Tab0, S2 = S0, Dfn2 = Dfn0, Dep1 = Dep0, TP1 = TP0
      ; new_init_call(N,Ngoal,Ent,S0,S1,Dfn0,Dfn1),
	add_tab_ent(Ngoal,Ent,Tab0,Tab1),
	oldt(N,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,maxint-maxint,Ndep,TP0,TP1),
	compute_mins(Dep0,Ndep,pos,Dep1),
        find(Tab2,Ngoal,Nent)
      ),
      ent_to_comp(Nent,Ncomp),
      ent_to_anss(Nent,Nanss),
      ( succeeded(Nanss) ->
	Tab = Tab2, S = S2, Dfn = Dfn2, Dep = Dep1, TP = TP1
      ; failed(Nanss), Ncomp == true ->
        apply_subst(Node,d(\+N,[]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep1,Dep,TP1,TP)
      ; apply_subst(Node,d(\+N,[\+N]),Tab2,Tab,S2,S,Dfn2,Dfn,Dep1,Dep,TP1,TP)
      )
    ).

pos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(_H,[N|_B]),
    Node = (Ggoal:Clause),
    ground(N,Ngoal),
    ( isprolog(N) ->
      findall(d(N,[]),call(N),Nanss),
      map_anss_list(Nanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; ( find(Tab0,Ngoal,Nent) ->
        ent_to_comp(Nent,Ncomp),
        ent_to_anss(Nent,Nanss),
        ( Ncomp \== true ->
          update_lookup_mins(Ggoal,Node,Ngoal,pos,Tab0,Tab1,Dep0,Dep1),
          map_anss(Nanss,Node,Ngoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep1,Dep,TP0,TP)
        ; % N is completed. 
          map_anss(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_pos_call(Ngoal,Node,Ent,S0,S1,Dfn0,Dfn1),
        add_tab_ent(Ngoal,Ent,Tab0,Tab1),
        oldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxint-maxint,Ndep,TP0,TP),
        update_solution_mins(Ggoal,Ngoal,pos,Tab2,Tab,Ndep,Dep0,Dep)
      )
    ).

aneg_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(_H,all([\+N|_B])),
    Node = (Ggoal:Clause),
    ground(N,Ngoal),
    ( isprolog(N) ->
      findall(d(N,[]),call(N),Nanss),
      return_to_disj_list(Nanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ; ( find(Tab0,Ngoal,Nent) ->
        ent_to_comp(Nent,Ncomp),
        ent_to_anss(Nent,Nanss),
        ( Ncomp \== true ->
          update_lookup_mins(Ggoal,Node,Ngoal,aneg,Tab0,Tab,Dep0,Dep),
          S = S0, Dfn = Dfn0, TP = TP0
        ; % N is completed. 
          return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
        )
      ; % otherwise N is new
        new_aneg_call(Ngoal,Node,Ent,S0,S1,Dfn0,Dfn1),
        add_tab_ent(Ngoal,Ent,Tab0,Tab1),
        oldt(N,Ngoal,Tab1,Tab2,S1,S,Dfn1,Dfn,maxint-maxint,Ndep,TP0,TP),
        update_solution_mins(Ggoal,Ngoal,pos,Tab2,Tab,Ndep,Dep0,Dep)
      )
    ).

apos_edge(Clause,Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Clause = rule(d(H,D),all([N|B])),
    ( ground(N) -> true
    ; write('Flounder in a universal disjunction: '), 
      write(N), 
      nl, 
      fail
    ),
    pos_edge(rule(d(H,[]),[N]),Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    edge_oldt(rule(d(H,D),all(B)),Ggoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

apply_subst(Ggoal:Cl,d(An,Vr),Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    copy_term(Cl,rule(d(Ac,Vc),Body)),
    ( Body = [Call|NBody] ->
      Call = An,
      append(Vr,Vc,Vn)
    ; Body = all([Call|Calls]),
      % Call = An,              % An is the numbervar-ed version of Call.
      ( Vc == [] ->
        Vn = all(Vr)
      ; Vc = all(Vc0),
        append(Vr,Vc0,Vn0),
        Vn = all(Vn0)
      ),
      NBody = all(Calls)
    ),
    edge_oldt(rule(d(Ac,Vn),NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP).

/* map_nodes(Nodes,Ans,....):
   return Ans to each of the waiting nodes in Nodes, where a node
   is of the form Ggoal:Clause.
*/  
map_nodes([],_Ans,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_nodes([Node|Nodes],Ans,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    apply_subst(Node,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_nodes(Nodes,Ans,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_anss([],_Node,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anss(l(_GH,Lanss),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Lanss == [] ->
      Tab = Tab0, S = S0, Dfn = Dfn0, Dep = Dep0, TP = TP0
    ; Lanss = [Ans|_],
      returned_ans(Ans,Ngoal,RAns),
      apply_subst(Node,RAns,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
    ).
map_anss(n2(T1,_,T2),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anss(T1,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anss(T2,Node,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).
map_anss(n3(T1,_,T2,_,T3),Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anss(T1,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anss(T2,Node,Ngoal,Tab1,Tab2,S1,S2,Dfn1,Dfn2,Dep1,Dep2,TP1,TP2),
    map_anss(T3,Node,Ngoal,Tab2,Tab,S2,S,Dfn2,Dfn,Dep2,Dep,TP2,TP).

map_anss_list([],_Node,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anss_list([Ans|Lanss],Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    apply_subst(Node,Ans,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anss_list(Lanss,Node,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   Nanss: an answer table for Ngoal
   Node: is of the form (Ggoal:Clause), where Clause is of the form
         rule(d(H,D),all([\+N|B]))
   It carries out resolution of each answer with Clause, and constructs
   a new clause rule(Head,NBody), where the body is basically a 
   conjunction of all the resolvents. If a resolvent is a disjunction
   or a non-ground literal, a new proposition is created (which is 
   actually represented by a number), which has a clause whose body
   is the resolvent.
*/
return_to_disj(Nanss,Node,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Node = (Ggoal : Clause),
    Clause = rule(Head,all(Body)),
    TP0 = (N0 : Tcl0),
    negative_return_all(Nanss,Body,Ngoal,NBody,[],N0,N,Tcl0,Tcl),
    TP1 = (N : Tcl),
    edge_oldt(rule(Head,NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_return_all([],_Body,_Ngoal,NBody,NBody,N,N,Tcl,Tcl).
negative_return_all(l(_GH,Lanss),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    ( Lanss == [] ->
      NBody0 = NBody, N = N0, Tcl = Tcl0
    ; Lanss = [Ans|_],
      negative_return_one(Ans,Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl)
    ).
negative_return_all(n2(T1,_,T2),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    negative_return_all(T1,Body,Ngoal,NBody0,NBody1,N0,N1,Tcl0,Tcl1),
    negative_return_all(T2,Body,Ngoal,NBody1,NBody,N1,N,Tcl1,Tcl).
negative_return_all(n3(T1,_,T2,_,T3),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    negative_return_all(T1,Body,Ngoal,NBody0,NBody1,N0,N1,Tcl0,Tcl1),
    negative_return_all(T2,Body,Ngoal,NBody1,NBody2,N1,N2,Tcl1,Tcl2),
    negative_return_all(T3,Body,Ngoal,NBody2,NBody,N2,N,Tcl2,Tcl).

negative_return_one(d(H,Tv),Body,Ngoal,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    copy_term(Body,[\+Call|Bs]),
    H = Call,
    ( Tv == [] ->                    % no delay
      ( (Bs = [Lit], ground(Lit)) -> % resovlent is a ground literal
        NBody0 = [Lit|NBody],
        N = N0, Tcl = Tcl0
      ; Lit = N0,                    % otherwise, replace it with a number
        N is N0+1,
        NBody0 = [Lit|NBody],
        Clause = rule(d(Lit,[]),all(Bs)),
        add_tab_ent(Lit,Clause,Tcl0,Tcl)
      )
    ; ( ground(H) ->                 % if there is delay, always replace with number
	NewTv = [\+H]
      ; ground(H,GH),
	NewTv = [Ngoal - (\+GH)]
      ),
      Lit = N0,
      N is N0+1,
      NBody0 = [Lit|NBody],
      Clause = rule(d(Lit,all(NewTv)),all(Bs)),
      add_tab_ent(Lit,Clause,Tcl0,Tcl)
    ).

return_to_disj_list(Lanss,Node,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    Node = (Ggoal : Clause),
    Clause = rule(Head,all(Body)),
    TP0 = (N0 : Tcl0),
    negative_return_list(Lanss,Body,NBody,[],N0,N,Tcl0,Tcl),
    TP1 = (N : Tcl),
    edge_oldt(rule(Head,NBody),Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP1,TP).

negative_return_list([],_Body,NBody,NBody,N,N,Tcl,Tcl).
negative_return_list([d(H,[])|Lanss],Body,NBody0,NBody,N0,N,Tcl0,Tcl) :-
    copy_term(Body,[\+Call|Bs]),
    H = Call,
    ( Bs = [Lit], ground(Lit) ->
      NBody0 = [Lit|NBody1],
      N1 = N0, Tcl1 = Tcl0
    ; Lit = N0,
      N1 is N0+1,
      NBody0 = [Lit|NBody1],
      Clause = rule(d(Lit,[]),all(Bs)),
      add_tab_ent(Lit,Clause,Tcl0,Tcl1)
    ),
    negative_return_list(Lanss,Body,NBody1,NBody,N1,N,Tcl1,Tcl).

/* comp_tab_ent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP)
   check if Ggoal and subgoals on top of it on the stack are
   completely evaluated.
*/
comp_tab_ent(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    ( Dep0 == maxint-maxint ->
      process_pos_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
    ; update_mins(Ggoal,Dep0,pos,Tab0,Tab1,Gdfn,Gdep),
      Gdep = Gpmin-Gnmin,
      ( Gdfn @=< Gpmin, Gnmin == maxint ->
        process_pos_scc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Gdfn @=< Gpmin, Gdfn @=< Gnmin ->
        process_neg_scc(Ggoal,Tab1,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP)
      ; Tab = Tab1, S0 = S, Dfn = Dfn0, Dep = Gdep, TP = TP0
      )
    ).

process_pos_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    ( wfs_trace ->
      write('Stack: '), nl, display_stack(S0,Tab0),
      write('Completed call found: '), write(Ggoal), nl, 
      display_table(Tab0),
      write('Completing calls ......'), nl, nl
    ; true
    ),
    pop_subgoals(Ggoal,S0,S1,[],Scc),
    complete_comp(Scc,Tab0,Tab1,Alist,[]),
    return_aneg_nodes(Alist,Tab1,Tab,S1,S,Dfn0,Dfn,maxint-maxint,Dep,TP0,TP).

/* pop_subgoals(Ggoal,S0,S,Scc0,Scc)
   pop off the stack subgoals up to and including Ggoal
*/
pop_subgoals(Ggoal,S0,S,Scc0,Scc) :-
    S0 = [Sent|S1],
    ( Ggoal == Sent ->
      S = S1, 
      Scc = [Sent|Scc0]
    ; pop_subgoals(Ggoal,S1,S,[Sent|Scc0],Scc)
    ).

/* complete_comp(Scc,Tab0,Tab,Alist0,Alist):
   process the list Scc of subgoals that are 
   completely evaluated.
*/
complete_comp([],Tab,Tab,Alist,Alist).
complete_comp([Ggoal|Scc],Tab0,Tab,Alist0,Alist) :-
    complete_one(Ggoal,Tab0,Tab1,Alist0,Alist1),
    complete_comp(Scc,Tab1,Tab,Alist1,Alist).

/* complete_one(Ggoal,Tab0,Tab,Alist0,Alist)
   process one subgoal that has been completely 
   evaluated:
   1. set its Nodes and Negs to [] and Comp to true;
   2. simplify its answers and set up links
      for further simplification later;
   3. use the truth value of Ggoal to simplify
      answers of other complete subgoals (possibly 
      including itself).
   4. set Alist0/Alist: a list of negation nodes with
      universal disjunctions with associated answers
      for the selected negative literal.
*/
complete_one(Ggoal,Tab0,Tab,Alist0,Alist) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab1),
    Ent0 = e(_Nodes,ANegs,Anss0,Delay,_Comp,Gdfn,Slist0),
    Ent = e([],[],Anss,Delay,true,Gdfn,Slist),
    ( Delay == true ->
      reduce_ans(Anss0,Anss,Tab0),
      setup_simp_links(Anss,Ggoal,Slist0,Slist1,Tab1,Tab2)
    ; % Delay == false
      Anss = Anss0,
      Tab2 = Tab1,
      Slist1 = Slist0
    ),
    extract_known(Ggoal,Anss,Slist1,Slist,Klist),
    simplify(Klist,Tab2,Tab,[]),
    ( ANegs == [] ->
      Alist0 = Alist
    ; Alist0 = [(Anss,Ggoal)-ANegs|Alist]
    ).

setup_simp_links([],_,Slist,Slist,Tab,Tab).
setup_simp_links(l(GH,Lanss),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links_list(Lanss,Ggoal-GH,Ggoal,Slist0,Slist,Tab0,Tab).
setup_simp_links(n2(T1,_,T2),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links(T1,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links(T2,Ggoal,Slist1,Slist,Tab1,Tab).
setup_simp_links(n3(T1,_,T2,_,T3),Ggoal,Slist0,Slist,Tab0,Tab) :-
    setup_simp_links(T1,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links(T2,Ggoal,Slist1,Slist2,Tab1,Tab2),
    setup_simp_links(T3,Ggoal,Slist2,Slist,Tab2,Tab).

/* setup_simp_link_list(Lanss,Ggoal-GH,Ggoal,Slist0,Slist,Tab0,Tab)
   Ggoal-GH is to tell what portion of answers of Ggoal can be 
   simplified.
*/
setup_simp_links_list([],_,_,Slist,Slist,Tab,Tab).
setup_simp_links_list([d(_,D)|Anss],GHead,Ggoal,Slist0,Slist,Tab0,Tab) :-
    ( D = all(Ds) ->
      true
    ; Ds = D
    ),
    links_from_one_delay(Ds,GHead,Ggoal,Slist0,Slist1,Tab0,Tab1),
    setup_simp_links_list(Anss,GHead,Ggoal,Slist1,Slist,Tab1,Tab).

/* A link ((Ggoal-GH):Lit) in an entry for Ngoal means that 
   the literal Lit in an answer with head GH in Ggoal can 
   be potentially simplified if we know answers for Ngoal.
*/
links_from_one_delay([],_,_,Slist,Slist,Tab,Tab).
links_from_one_delay([D|Ds],GHead,Ggoal,Slist0,Slist,Tab0,Tab) :-
    ( D = (\+ Ngoal) ->
      ( Ggoal == Ngoal ->
        Tab1 = Tab0,
	Slist1 = [GHead:D|Slist0]
      ; add_link_to_ent(Tab0,Ngoal,GHead:D,Tab1),
	Slist1 = Slist0
      )
    ; D = (Ngoal-_) ->
      ( Ggoal == Ngoal ->
        Slist1 = [GHead:D|Slist0],
        Tab1 = Tab0
      ; Slist1 = Slist0,
        add_link_to_ent(Tab0,Ngoal,GHead:D,Tab1)
      )
    ),
    links_from_one_delay(Ds,GHead,Ggoal,Slist1,Slist,Tab1,Tab).

/* extract_known(Ggoal,Anss,Links,Slist,Klist):
   Given Ggoal and its answers Anss, and its 
   simplification Links, it partitioned Links 
   into Slist and Klist of links, where Klist 
   is a list of links that are known to be either
   true or false.

   Klist is either of the form Val-Links, or a
   list of the form Val-Link. In case of non-ground
   calls, the corresponding portion of Anss has to 
   be searched.
*/
extract_known(Ggoal,Anss,Links,Slist,Klist) :-
    ( failed(Anss) ->
      Klist = fail-Links,
      Slist = []
    ; Anss = l(GH,Lanss) ->
      ( Ggoal == GH ->       % Ground or most general call
	( memberchk(d(_,[]),Lanss) ->
	  Klist = succ-Links,
	  Slist = []
        ; Klist = [],
	  Slist = Links
        )
      ; % non-ground call
	extract_known_anss(Links,Anss,[],Slist,[],Klist)
      )
    ; % non-ground call
      extract_known_anss(Links,Anss,[],Slist,[],Klist)
    ).
      
extract_known_anss([],_,Slist,Slist,Klist,Klist).
extract_known_anss([Link|Links],Anss,Slist0,Slist,Klist0,Klist) :-
    Link = (_:Lit),
    extract_lit_val(Lit,Anss,true,Val),
    ( Val == undefined ->
      Slist1 = [Link|Slist0],
      Klist1 = Klist0
    ; Slist1 = Slist0,
      Klist1 = [Val-Link|Klist0]
    ),
    extract_known_anss(Links,Anss,Slist1,Slist,Klist1,Klist).

/* extract_lit_val(Lit,Anss,Comp,Val):
   extract the truth value of Lit according to Anss and Comp.
   In case of a non-ground calls, the corresponding portion
   of Anss has to be searched.
*/
extract_lit_val(Lit,Anss,Comp,Val) :-
    ( Lit = (\+ _) ->
      ( succeeded(Anss) ->
        Val = fail
      ; failed(Anss), Comp == true ->
        Val = succ
      ; Val = undefined
      )
    ; Lit = (_ - (\+GH)) ->
      ( find(Anss,GH,Lanss) ->
        ( (\+ \+ memberchk(d(GH,[]),Lanss)) ->
          Val = fail
        ; Lanss == [], Comp == true ->
	  Val = succ
        ; Val = undefined
        )
      ; ( Comp == true ->
	  Val = succ
        ; Val = undefined
        )
      )
    ; Lit = (_-GH) ->
      ( find(Anss,GH,Lanss) ->
        ( (\+ \+ memberchk(d(GH,[]),Lanss)) ->
          Val = succ
        ; Lanss == [], Comp == true ->
	  Val = fail
        ; Val = undefined
        )
      ; ( Comp == true ->
	  Val = fail
        ; Val = undefined
        )
      )
    ).

/* simplify(KnownLinks,Tab0,Tab,Abd):
   Given a list of KnownLinks, Tab0 and Abd,
   it tries to simplify answers according to
   KnownLinks. When a subgoal is found to be
   true or false according to answers, 
   consistency with assumed truth values in Abd
   is checked.
*/
simplify([],Tab,Tab,_Abd).
simplify([Val-Link|Klist],Tab0,Tab,Abd) :-
    simplify_one(Val,Link,Tab0,Tab1,Abd),
    simplify(Klist,Tab1,Tab,Abd).
simplify(Val-Links,Tab0,Tab,Abd) :-
    simplify_list(Links,Val,Tab0,Tab,Abd).

simplify_list([],_,Tab,Tab,_Abd).
simplify_list([Link|Links],Val,Tab0,Tab,Abd) :-
    Link = (_ : Lit),
    ( ( Lit = (\+_); Lit = (_ - (\+_)) ) ->
      ( Val = fail -> LVal = succ; LVal = fail )
    ; LVal = Val
    ),
    simplify_one(LVal,Link,Tab0,Tab1,Abd),
    simplify_list(Links,Val,Tab1,Tab,Abd).

simplify_one(Val,Link,Tab0,Tab,Abd) :-
    Link = ((Ngoal - GH) : Lit),
    updatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    Ent0 = e(Nodes,ANegs,Anss0,Delay,Comp,Dfn,Slist0),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    ( updatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      simplify_anss(Lanss0,Val,Lit,[],Lanss,C),
      ( C == true ->
	( find(Abd,GH,Aval) ->
	  ( Aval == true, Lanss == [] -> % deduced result inconsistent with assumption
	    fail
	  ; Aval == false, memberchk( d(_ , []), Lanss) ->
	    fail
	  ; true
          )
	; true
        ),
        extract_known(Ngoal,Anss,Slist0,Slist,Klist),
        simplify(Klist,Tab1,Tab,Abd)
      ; Tab = Tab0
      )
    ; Tab = Tab0
    ).

/* simplify_anss(List,Val,Lit,Lanss0,Lanss,C):
   Given a List of answers, Val of Lit, it 
   simplifies the List and construct a new list
   Lanss0/Lanss of answers. C is unified with true
   if some simplification is carried out.

   As soon as a true answer is detected, all
   other answers with the same head are deleted.
*/
simplify_anss([],_,_,Anss,Anss,_).
simplify_anss([Ans|Rest],Val,Lit,Anss0,Anss,C) :-
    ( simplified_ans(Ans,Val,Lit,NewAns,C) ->
      ( NewAns = d(_,[]) ->
        Anss = [NewAns]
      ; Anss1 = [NewAns|Anss0],
        simplify_anss(Rest,Val,Lit,Anss1,Anss,C)
      )
    ; C = true,
      simplify_anss(Rest,Val,Lit,Anss0,Anss,C)
    ).

simplified_ans(Ans,Val,Lit,NewAns,C) :-
    Ans = d(H,Ds),
    ( Ds == [] ->
      NewAns = Ans
    ; Ds = all(Dlist) ->
      ( Val == fail ->
        delete_lit(Dlist,Lit,NewDlist,[],C),
        ( NewDlist == [] ->
          fail
        ; NewAns = d(H,all(NewDlist))
        )
      ; % Val == succ ->
        ( memberchk(Lit,Dlist) ->
          NewAns = d(H,[]),
          C = true
        ; NewAns = Ans
        )
      )
    ; % Ds is a conjunction
      ( Val == fail ->
        ( memberchk(Lit,Ds) ->
          fail
        ; NewAns = Ans
        )
      ; % Val == succ ->
        delete_lit(Ds,Lit,NewDs,[],C),
        NewAns = d(H,NewDs)
      )
    ).

/* delete_lit(Delays,Lit,Ds0,Ds,C):
   deletes Lit from Delays. Delays is 
   a list of delayed literals and it
   is guaranteed to have no duplicates.
*/
delete_lit([],_,Ds,Ds,_).
delete_lit([D|Rest],Lit,Ds0,Ds,C) :-
    ( D == Lit ->
      Ds0 = Rest,
      C = true
    ; Ds0 = [D|Ds1],
      delete_lit(Rest,Lit,Ds1,Ds,C)
    ).

% return answers to negative nodes within universal disjunctions
return_aneg_nodes([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
return_aneg_nodes([(Anss,Ngoal)-ANegs|Alist],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_anegs(ANegs,Anss,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    return_aneg_nodes(Alist,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

map_anegs([],_Anss,_Ngoal,Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
map_anegs([Node|ANegs],Anss,Ngoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    return_to_disj(Anss,Node,Ngoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    map_anegs(ANegs,Anss,Ngoal,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* process a component of subgoals that may be involved in 
   negative loops.
*/
process_neg_scc(Ggoal,Tab0,Tab,S0,S,Dfn0,Dfn,Dep,TP0,TP) :-
    ( wfs_trace ->
      write('Stack: '), nl, display_stack(S0,Tab0),
      write('Possible negative loop: '), write(Ggoal), nl, 
      display_table(Tab0)
    ; true
    ),
    extract_subgoals(Ggoal,S0,Scc,[]),
    reset_nmin(Scc,Tab0,Tab1,Ds,[]),
    ( wfs_trace ->
      write('Delaying: '), display_dlist(Ds)
    ; true
    ),
    delay_and_cont(Ds,Tab1,Tab2,S0,S1,Dfn0,Dfn1,maxint-maxint,Dep1,TP0,TP1),
    recomp_scc(Scc,Tab2,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* extract_subgoals(Ggoal,S0,Scc0,Scc)
   extract subgoals that may be involved in negative loops,
   but leave the stack of subgoals intact.
*/
extract_subgoals(Ggoal,[Sent|S],[Sent|Scc0],Scc) :-
    ( Ggoal == Sent ->
      Scc0 = Scc
    ; extract_subgoals(Ggoal,S,Scc0,Scc)
    ).

/* reset_nmin(Scc,Tab0,Tab,Dnodes0,Dnodes)
   reset NegLink and collect all waiting nodes that need to be 
   delayed. Dnodes0/Dnodes is a difference list.
*/
reset_nmin([],Tab,Tab,Ds,Ds).
reset_nmin([Ggoal|Scc],Tab0,Tab,Ds0,Ds) :-
    get_and_reset_negs(Tab0,Ggoal,ANegs,Tab1),
    ( ANegs == [] ->
      Ds0 = Ds1
    ; Ds0 = [Ggoal-ANegs|Ds1]
    ),
    reset_nmin(Scc,Tab1,Tab,Ds1,Ds).

delay_and_cont([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
delay_and_cont([Ggoal-Negs|Dnodes],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    map_nodes(Negs,d(\+Ggoal,[\+Ggoal]),Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    delay_and_cont(Dnodes,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

recomp_scc([],Tab,Tab,S,S,Dfn,Dfn,Dep,Dep,TP,TP).
recomp_scc([Ggoal|Scc],Tab0,Tab,S0,S,Dfn0,Dfn,Dep0,Dep,TP0,TP) :-
    comp_tab_ent(Ggoal,Tab0,Tab1,S0,S1,Dfn0,Dfn1,Dep0,Dep1,TP0,TP1),
    recomp_scc(Scc,Tab1,Tab,S1,S,Dfn1,Dfn,Dep1,Dep,TP1,TP).

/* routines for incremental update of dependency information
*/

/* update_mins(Ggoal,Dep,Sign,Tab0,Tab,Gdfn,Gdep)
   update the PosLink and NegLink of Ggoal according to 
   Dep and Sign
*/
update_mins(Ggoal,Dep,Sign,Tab0,Tab,Gdfn,Gdep) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn:Gdep0,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn:Gdep,Slist),
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    compute_mins(Gdep0,Dep,Sign,Gdep).

/* update_lookup_mins(Ggoal,Node,Ngoal,Sign,Tab0,Tab,Dep0,Dep)
   There is a lookup edge (Node) from Ggoal to Ngoal 
   with Sign. It adds Node to the corresponding waiting list
   in Ngoal and then update the dependencies of Ggoal.
*/
update_lookup_mins(Ggoal,Node,Ngoal,Sign,Tab0,Tab,Dep0,Dep) :-
    updatevs(Tab0,Ngoal,Ent0,Ent,Tab1),
    ( Sign == pos ->
      pos_to_newent(Ent0,Ent,Node)
    ; Sign == aneg ->
      aneg_to_newent(Ent0,Ent,Node)
    ),
    Ent0 = e(_,_,_,_,_,_Ndfn:Ndep,_),
    compute_mins(Dep0,Ndep,Sign,Dep),
    update_mins(Ggoal,Ndep,Sign,Tab1,Tab,_,_).

/* update_solution_mins(Ggoal,Ngoal,Sign,Tab0,Tab,Ndep,Dep0,Dep)
   There is an edge with Sign from Ggoal to Ngoal, where Ngoal is 
   a new subgoal. Ndep is the final dependency information of 
   Ngoal. Dep0/Dep is for the most recent enclosing new call.
   This predicate is called after Ngoal is solved.
*/
update_solution_mins(Ggoal,Ngoal,Sign,Tab0,Tab,Ndep,Dep0,Dep) :-
    find(Tab0,Ngoal,Nent),
    ent_to_comp(Nent,Ncomp),
    ( Ncomp == true ->
      ( Ndep == maxint-maxint ->
        Tab = Tab0, Dep = Dep0
      ; update_mins(Ggoal,Ndep,pos,Tab0,Tab,_,_),
        compute_mins(Dep0,Ndep,pos,Dep)
      )
    ; update_mins(Ggoal,Ndep,Sign,Tab0,Tab,_,_),
      compute_mins(Dep0,Ndep,Sign,Dep)
    ).

compute_mins(Gpmin-Gnmin,Npmin-Nnmin,Sign,Newpmin-Newnmin) :-
    ( Sign == pos ->
      min(Gpmin,Npmin,Newpmin),
      min(Gnmin,Nnmin,Newnmin)
    ; % (Sign == neg; Sign == aneg) ->
      Newpmin=Gpmin,
      min(Gnmin,Npmin,Imin), 
      min(Imin,Nnmin,Newnmin)
    ).
    
min(X,Y,M) :- ( X @< Y -> M=X; M=Y ).

%%%%%%%%%%%%%%% Local table manipulation predicates %%%%%%%%%%

/* Table Entry Structure:
   For each Call, its table entry is identified with its number-vared
   version -- Ggoal. Its value is a term of the form

    e(Nodes,ANegs,Anss,Delay,Comp,Dfn:Dep,Slist)

   where
     Nodes:  positive suspension list
     ANegs:  negative suspension list (for universal disjunction clauss)
     Anss:   another table.
     Delay:  whether Anss contains any answer with delay
     Comp:   whether Call is completely evaluated or not
     Dfn:    depth-first number of Gcall
     Dep:    (PosLink-NegLink) --- dependency information
     Slist:  a list of nodes whose answers may be simplified
             if the truth value of Ggoal is known. Each element of Slist
         is of the form (Ngoal-GH):Literal.
   Stack Entry Structure:
     Ggoal
*/

/* routines for accessing individual fields of an entry
*/
ent_to_nodes(e(Nodes,_,_,_,_,_,_),Nodes).
ent_to_anegs(e(_,ANegs,_,_,_,_,_),ANegs).
ent_to_anss(e(_,_,Anss,_,_,_,_),Anss).
ent_to_delay(e(_,_,_,Delay,_,_,_),Delay).
ent_to_comp(e(_,_,_,_,Comp,_,_),Comp).
ent_to_dfn(e(_,_,_,_,_,Dfn,_),Dfn).
ent_to_slist(e(_,_,_,_,_,_,Slist),Slist).

get_and_reset_negs(Tab0,Ggoal,ANegs,Tab) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Gdfn: (Gpmin - _),Slist),
    Ent = e(Nodes,[],Anss,Delay,Comp,Gdfn:Gpmin-maxint,Slist),
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab).

/* adding a new table entry
*/
add_tab_ent(Ggoal,Ent,Tab0,Tab) :- 
    addkey(Tab0,Ggoal,Ent,Tab).

/* The following three routines are for creating
   new calls
*/
/* a new call with empty suspensions 
*/
new_init_call(Call,Ggoal,Ent,S0,S,Dfn0,Dfn) :-
    ground(Call,Ggoal),
    S = [Ggoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[],[],false,false,Dfn0:Dfn0-maxint,[]).

/* a new call with an initial negative suspension from 
   inside a universal disjunction
*/
new_aneg_call(Ngoal,Neg,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([],[Neg],[],false,false,Dfn0:Dfn0-maxint,[]).

/* a new call with an initial positive suspension
*/
new_pos_call(Ngoal,Node,Ent,S0,S,Dfn0,Dfn) :-
    S = [Ngoal|S0],
    Dfn is Dfn0+1,
    Ent = e([Node],[],[],false,false,Dfn0:Dfn0-maxint,[]).

/* routines for adding more information to a
   table entry.
*/
aneg_to_newent(Ent0,Ent,ANeg) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e(Nodes,[ANeg|ANegs],Anss,Delay,Comp,Dfn,Slist).

pos_to_newent(Ent0,Ent,Node) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e([Node|Nodes],ANegs,Anss,Delay,Comp,Dfn,Slist).

add_link_to_ent(Tab0,Ggoal,Link,Tab) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    link_to_newent(Ent0,Ent,Link).

link_to_newent(Ent0,Ent,Link) :-
    Ent0 = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,[Link|Slist]).

/* routines for manipulating answers */
ansstree_to_list([],L,L).
ansstree_to_list(l(_GH,Lanss),L0,L) :-
    attach(Lanss,L0,L).
ansstree_to_list(n2(T1,_M,T2),L0,L) :-
    ansstree_to_list(T1,L0,L1),
    ansstree_to_list(T2,L1,L).
ansstree_to_list(n3(T1,_M2,T2,_M3,T3),L0,L) :-
    ansstree_to_list(T1,L0,L1),
    ansstree_to_list(T2,L1,L2),
    ansstree_to_list(T3,L2,L).

attach([],L,L).
attach([d(H,B)|R],[X|L0],L) :-
    ( B == [] ->
      X = H
    ; X = (H <- B)
    ),
    attach(R,L0,L).

member_anss(Ans,Anss) :-
	member_anss_1(Anss,Ans).

member_anss_1(l(_,Lanss),Ans) :-
	member(Ans,Lanss).
member_anss_1(n2(T1,_,T2),Ans) :-
	( member_anss_1(T1,Ans)
        ; member_anss_1(T2,Ans)
        ).
member_anss_1(n3(T1,_,T2,_,T3),Ans) :-
	( member_anss_1(T1,Ans)
        ; member_anss_1(T2,Ans)
        ; member_anss_1(T3,Ans)
        ).

/* failed(Anss): Anss is empty */
failed([]).
failed(l(_,[])).

/* succeeded(Anss): Anss contains a single definite answer */
succeeded(l(_,Lanss)) :-
	memberchk(d(_,[]),Lanss).

/* add_ans(Tab0,Goal,Ans,Nodes,Mode,Tab):
   If Ans is not subsumed by any existing answer then
      Ans is added to Anss(Goal);
      If some existing answer also has head H then
         Mode = no_new_head
      else 
         Mode = new_head
   else
      fail.
*/
add_ans(Tab0,Ggoal,Ans,Nodes,Mode,Tab) :-
    updatevs(Tab0,Ggoal,Ent0,Ent,Tab),
    Ans = d(H,Ds),
    ( Ds == [] ->
      new_ans_ent(Ent0,Ent,Ans,Nodes,Mode)
    ; setof(X,member(X,Ds),NewDs),
      new_ans_ent(Ent0,Ent,d(H,NewDs),Nodes,Mode)
    ).

new_ans_ent(Ent0,Ent,Ans,Nodes,Mode) :-
    Ent0 = e(Nodes,ANegs,Anss0,Delay0,Comp,Dfn,Slist),
    Ent = e(Nodes,ANegs,Anss,Delay,Comp,Dfn,Slist),
    Ans = d(H,D),
    ground(H,GH),
    ( updatevs(Anss0,GH,Lanss0,Lanss,Anss) ->
      ( D == [] ->
        \+(memberchk(d(_,[]),Lanss0)),
        Lanss = [Ans]
      ; not_subsumed_ans(Ans,Lanss0),
        Lanss = [Ans|Lanss0]
      ),
      Mode = no_new_head
    ; addkey(Anss0,GH,[Ans],Anss),
      Mode = new_head
    ),
    ( D == [] -> 
      Delay = Delay0
    ; Delay = true
    ).

/* returned_ans(Ans,Ggoal,RAns):
   determines whether SLG resolution or SLG factoring should 
   be applied.
*/
returned_ans(d(H,Tv),Ggoal,d(H,NewTv)) :-
    ( Tv = [] ->
      NewTv = []
    ; ground(H,GH),
      NewTv = [Ggoal-GH]
    ).

% reduce a list of answers, by reducing delay list, and by subsumption
reduce_ans(Anss0,Anss,Tab) :-
    reduce_completed_ans(Anss0,Anss,Tab).

% simplify all the delay lists in a list of answers.
reduce_completed_ans([],[],_Tab).
reduce_completed_ans(l(GH,Lanss0),l(GH,Lanss),Tab) :-
    reduce_completed_anslist(Lanss0,[],Lanss,Tab).
reduce_completed_ans(n2(T1,M,T2),n2(NT1,M,NT2),Tab) :-
    reduce_completed_ans(T1,NT1,Tab),
    reduce_completed_ans(T2,NT2,Tab).
reduce_completed_ans(n3(T1,M2,T2,M3,T3),n3(NT1,M2,NT2,M3,NT3),Tab) :-
    reduce_completed_ans(T1,NT1,Tab),
    reduce_completed_ans(T2,NT2,Tab),
    reduce_completed_ans(T3,NT3,Tab).

reduce_completed_anslist([],Lanss,Lanss,_Tab).
reduce_completed_anslist([d(G,D0)|List],Lanss0,Lanss,Tab) :-
    ( D0 = all(Dlist1) ->
      ( filter_delays(Dlist1,[],Dlist,disj,V,Tab) ->
        ( V == true ->       % true answer
          Lanss = [d(G,[])]
        ; Dlist == [] ->     % false answer, ignore
          reduce_completed_anslist(List,Lanss0,Lanss,Tab)
        ; reduce_completed_anslist(List,[d(G,all(Dlist))|Lanss0],Lanss,Tab)
        )
      ; reduce_completed_anslist(List,Lanss0,Lanss,Tab)
      )
    ; ( filter_delays(D0,[],D,conj,_V,Tab) ->
	( D == [] ->
	  Lanss = [d(G,[])]
        ; reduce_completed_anslist(List,[d(G,D)|Lanss0],Lanss,Tab)
        )
      ; reduce_completed_anslist(List,Lanss0,Lanss,Tab)
      )
    ).

% simplify a delay list by the completed table: delete true negations,
%    fail if a false one.
filter_delays([],Fds,Fds,_DC,_V,_Tab).
filter_delays([Lit|Ds],Fds0,Fds,DC,V,Tab) :-
    lit_to_call(Lit,Gcall),
    find(Tab,Gcall,Gent),
    ent_to_comp(Gent,Gcomp),
    ent_to_anss(Gent,Ganss),
    extract_lit_val(Lit,Ganss,Gcomp,Val),
    ( Val == succ ->
      ( DC == conj ->
        filter_delays(Ds,Fds0,Fds,DC,V,Tab)
      ; DC == disj ->
        V = true
      )
    ; Val == fail ->
      ( DC == conj ->
        fail
      ; DC == disj ->
        filter_delays(Ds,Fds0,Fds,DC,V,Tab)
      )
    ; % Val == undefined
      filter_delays(Ds,[Lit|Fds0],Fds,DC,V,Tab)
    ).

lit_to_call(\+G,G).
lit_to_call(Gcall-_,Gcall).

not_subsumed_ans(Ans,Lanss0) :-
    \+
    ( numbervars(Ans,0,_),
      subsumed_ans1(Ans,Lanss0)
    ).

% succeed if answer is subsumed by any in list1 or 2.
subsumed_ans(Tv,List1,List2) :- 
    \+ 
    (numbervars(Tv,0,_),
     \+ subsumed_ans1(Tv,List1),
     \+ subsumed_ans1(Tv,List2)
    ).

% check if a delay is subsumed one of the element in the list
subsumed_ans1(d(T,V),List) :-
    member(d(T,V1),List),
    ( V1 == []
    ; V = all(LV), V1 = all(LV1) ->
      subset(LV,LV1)
    ; subset(V1,V)
    ).

/****************** auxiliary routines *******************/
% variantchk/2 finds a variant in a list of atoms.
variantchk(G,[G1|_]) :- variant(G,G1), !.
variantchk(G,[_|L]) :- variantchk(G,L).

variant(A, B) :-
    A == B
     ->    true
     ;     subsumes_chk(A, B),
           subsumes_chk(B, A),
           A = B.
/*
subsumes_chk(General, Specific) :-
        \+ (    numbervars(Specific, 0, _),
                \+ General = Specific
         ).
*/
ground(O,C) :- ground(O) -> C = O ; copy_term(O,C), numbervars(C,0,_).

subset([],_).
subset([E|L1],L2) :- memberchk(E,L2), subset(L1,L2).

reverse([],R,R).
reverse([Goal|Scc],R0,R) :- reverse(Scc,[Goal|R0],R).

/***************** routines for debugging *******************/

% Debugging help: pretty-prints strongly connected components and local table.
display_stack(Stack,Tab) :-
    reverse(Stack,[],Rstack),
    display_st(Rstack,Tab).
display_st([],_Tab).
display_st([Ggoal|Scc],Tab) :-
    find(Tab,Ggoal,Ent),
    ent_to_dfn(Ent,Dfn:Pmin-Nmin),
    tab(2), 
    write(Ggoal-Dfn),
    write(':  '),
    write('Pmin='),
    write(Pmin),
    write(';  '),
    write('Nmin='),
    write(Nmin),
    write(';  '),
    nl,
    display_st(Scc,Tab).

display_dlist([]) :- nl,nl.
display_dlist([Ngoal-_|Dlist]) :-
    write(\+ Ngoal), 
    write('; '), 
    display_dlist(Dlist).

display_table(Tab) :-
    write('Table: '), 
    nl,
    write_tab(Tab).

display_final(Tab) :-
    write(' Final Set of Answers: '), 
    nl,
    display_final1(Tab).
display_final1([]).
display_final1(l(_,e(_,_,Anss,_,_,_,_))) :-
    write_anss(Anss).
display_final1(n2(X,_,Y)) :- 
    display_final1(X),
    display_final1(Y).
display_final1(n3(X,_,Y,_,Z)) :- 
    display_final1(X),
    display_final1(Y),
    display_final1(Z).

write_tab([]).
write_tab(l(G,e(Nodes,ANegs,Anss,_,Comp,Dfn:_,_))) :-
    write(' Entry: '),
    write(G-Dfn),
    write(': '),
    ( Comp == true -> 
      write('Complete!')
    ; write('Incomplete!') 
    ), 
    nl,
    ( Anss == [] -> 
      true
    ; write('   Anss: '), 
      nl,
      write_anss(Anss)
    ),
    ( ( Comp == true; Nodes == []) -> 
      true 
    ; write('   Nodes: '),
      write(Nodes),
      nl
    ),
    ( ( Comp == true; ANegs == []) ->
      true
    ; write('   ANegs: '),
      write(ANegs),
      nl
    ).
write_tab(n2(X,_,Y)) :- 
    write_tab(X),
    write_tab(Y).
write_tab(n3(X,_,Y,_,Z)) :- 
    write_tab(X),
    write_tab(Y),
    write_tab(Z).

write_anss([]).
write_anss(l(_,Lanss)) :-
    write_anss_list(Lanss).
write_anss(n2(T1,_,T2)) :-
    write_anss(T1),
    write_anss(T2).
write_anss(n3(T1,_,T2,_,T3)) :-
    write_anss(T1),
    write_anss(T2),
    write_anss(T3).

write_anss_list([]).
write_anss_list([Ans|Anss]) :-
    write_ans(Ans),
    write_anss_list(Anss).

write_ans(d(H,Ds)) :-
    write('         '), 
    write(H),
    ( Ds == [] -> 
      true
    ; write(' :- '),
      ( Ds = all([D|Ds1]) ->
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_delay(Ds1,'; ')
      ; Ds = [D|Ds1],
        ( D = (_-GH) ->
          write(GH)
        ; write(D)
        ),
        write_delay(Ds1,', ')
      )
    ), 
    write('.'), 
    nl.
write_delay([],_).
write_delay([D|Ds1],Sep) :-
    write(Sep),
    ( D = (_Gcall-GH) -> 
      write(GH)
    ; write(D) 
    ),
    write_delay(Ds1,Sep).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* 
This is a set of routines that supports indexed tables. Tables
are sets of key-value_list pairs. With each key is associated a list
of values. It uses 2-3 trees for the index (modified by D.S. Warren
from Ivan Bratko: ``Prolog Programming for Artificial
Intelligence'', Addison Wesley, 1986). Operations are: 

Keys must be ground! (so numbervar them)

addkey(Tree,Key,V,Tree1) adds a new Key with value V, returning 
    new Tree1. Fails if the key is already there.

find(Tree,Key,V) finds the entry with Key and returns associated
    values in V.

updatevs(Tree,Key,OldV,NewV,Tree1) replaces value of entry with key
    Key and value OldV with NewV.
*/


addkey(Tree,X,V,Tree1) :-
	ins2(Tree,X,V,Trees),
	cmb0(Trees,Tree1).
addkey([],X,V,l(X,V)).


find(l(X,V),Xs,V) :- X == Xs.
find(n2(T1,M,T2),X,V) :-
	M @=< X
	 ->	find(T2,X,V)
	 ;	find(T1,X,V).
find(n3(T1,M2,T2,M3,T3),X,V) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	find(T3,X,V)
		 ;	find(T2,X,V)
		)
	 ;	find(T1,X,V).


% updatevs(Tab0,X,Ov,Nv,Tab) updates Tab0 to Tab, by replacing
% Ov of entry with key X by Nv.
/*
updatevs(Tab0,X,Ov,Nv,Tab) :-
	updatevs(Tab0,X,Ov,Nv),
	Tab = Tab0.

updatevs(Tab,X,Ov,Nv) :-
	( Tab = l(Xs,Ov), Xs == X ->
	  setarg(2,Tab,Nv)
        ; Tab = n2(T1,M,T2) ->
	  ( M @=< X ->
	    updatevs(T2,X,Ov,Nv)
	  ; updatevs(T1,X,Ov,Nv)
          )
        ; Tab = n3(T1,M2,T2,M3,T3) ->
	  ( M2 @=< X ->
	    ( M3 @=< X ->
	      updatevs(T3,X,Ov,Nv)
	    ; updatevs(T2,X,Ov,Nv)
	    )
	  ; updatevs(T1,X,Ov,Nv)
          )
        ).
*/

updatevs(l(X,Ov),Xs,Ov,Nv,l(X,Nv)) :- X == Xs.
updatevs(n2(T1,M,T2),X,Ov,Nv,n2(NT1,M,NT2)) :-
	M @=< X
	 ->	NT1=T1, updatevs(T2,X,Ov,Nv,NT2)
	 ;	NT2=T2, updatevs(T1,X,Ov,Nv,NT1).
updatevs(n3(T1,M2,T2,M3,T3),X,Ov,Nv,n3(NT1,M2,NT2,M3,NT3)) :-
	M2 @=< X
	 ->	(M3 @=< X
		 ->	NT2=T2, NT1=T1, updatevs(T3,X,Ov,Nv,NT3)
		 ;	NT1=T1, NT3=T3, updatevs(T2,X,Ov,Nv,NT2)
		)
	 ;	NT2=T2, NT3=T3, updatevs(T1,X,Ov,Nv,NT1).

ins2(n2(T1,M,T2),X,V,Tree) :- 
	M @=< X
	 ->	ins2(T2,X,V,Tree1),
		cmb2(Tree1,T1,M,Tree)
	 ;	ins2(T1,X,V,Tree1),
		cmb1(Tree1,M,T2,Tree).
ins2(n3(T1,M2,T2,M3,T3),X,V,Tree) :- 
	M2 @=< X
	 ->	(M3 @=< X
		 ->	ins2(T3,X,V,Tree1),
			cmb4(Tree1,T1,M2,T2,M3,Tree)
		 ;	ins2(T2,X,V,Tree1),
			cmb5(Tree1,T1,M2,M3,T3,Tree)
		)
	 ;	ins2(T1,X,V,Tree1),
		cmb3(Tree1,M2,T2,M3,T3,Tree).
ins2(l(A,V),X,Vn,Tree) :-
	A @=< X
	 ->	(X @=< A
		 ->	fail
		 ;	Tree = t(l(A,V),X,l(X,Vn))
		)
	 ;	Tree = t(l(X,Vn),A,l(A,V)).

cmb0(t(Tree),Tree).
cmb0(t(T1,M,T2),n2(T1,M,T2)).

cmb1(t(NT1),M,T2,t(n2(NT1,M,T2))).
cmb1(t(NT1a,Mb,NT1b),M,T2,t(n3(NT1a,Mb,NT1b,M,T2))).

cmb2(t(NT2),T1,M,t(n2(T1,M,NT2))).
cmb2(t(NT2a,Mb,NT2b),T1,M,t(n3(T1,M,NT2a,Mb,NT2b))).

cmb3(t(NT1),M2,T2,M3,T3,t(n3(NT1,M2,T2,M3,T3))).
cmb3(t(NT1a,Mb,NT1b),M2,T2,M3,T3,t(n2(NT1a,Mb,NT1b),M2,n2(T2,M3,T3))).

cmb4(t(NT3),T1,M2,T2,M3,t(n3(T1,M2,T2,M3,NT3))).
cmb4(t(NT3a,Mb,NT3b),T1,M2,T2,M3,t(n2(T1,M2,T2),M3,n2(NT3a,Mb,NT3b))).

cmb5(t(NT2),T1,M2,M3,T3,t(n3(T1,M2,NT2,M3,T3))).
cmb5(t(NT2a,Mb,NT2b),T1,M2,M3,T3,t(n2(T1,M2,NT2a),Mb,n2(NT2b,M3,T3))).

start_slg:- assertz((
	term_expansion(X,Y) :- !,
	        do_term_expansion(X,Y)
	    )).

