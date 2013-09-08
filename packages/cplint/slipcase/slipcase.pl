/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).

:-dynamic setting/2,last_id/1, rule/5.

:-[revise].

setting(epsilon_em,0.0001).
setting(epsilon_em_fraction,0.00001).
setting(eps,0.0001).
setting(eps_f,0.00001).

/* if the difference in log likelihood in two successive em iteration is smaller
than epsilon_em, then em stops */
setting(epsilon_sem,2).

/* number of random restarts of em */
setting(random_restarts_REFnumber,1).
setting(random_restarts_number,1).
setting(iterREF,-1).
setting(iter,-1).
setting(examples,atoms).
setting(group,1).
setting(d,1).  
setting(verbosity,1).
setting(logzero,log(0.000001)).
setting(initial_clauses_modeh,1).
setting(max_iter,10).
setting(max_var,5).
setting(max_rules,10).
setting(beamsize,20).


sl(File):-
  generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL),
  reconsult(FileL),
  load_models(FileKB,DB),  
  statistics(walltime,[_,_]),
  (file_exists(FileBG)->
    set(compiling,on),
    load(FileBG,_ThBG,RBG),
    set(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    assert_all(ThBG)
  ;
    true
  ),
  (file_exists(FileIn)->
    set(compiling,on),
    load(FileIn,_Th1,R1),
    set(compiling,off)
  ;
    deduct(DB,[],InitialTheory),   
    length(InitialTheory,_LI),  %-LI=number of rules of the theory
    remove_duplicates(InitialTheory,Th0),
    length(Th0,_LI1),
    set(compiling,on),
    process_clauses(Th0,[],_Th1,[],R1),  %+Th0: rules Head:-body with prob. in the head,-_Th1: rules in the same form as Th0 but with bdds one/and+getvarn+equality, R1: same theory of the form rule(NR,[head atoms with prob. (with null),[body atoms].
    set(compiling,off)
  ),
  write('Initial theory'),nl,
  write_rules(R1,user_output),
  learn_struct(DB,R1,R2,CLL2), 
  learn_params(DB,R2,R,CLL),  
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  format("~nRefinement CLL  ~f - CLL after EMBLEM ~f~n",[CLL2,CLL]),
  format("Total execution time ~f~n~n",[WTS]),
  write_rules(R,user_output),
  listing(setting/2),
  open(FileOut,write,Stream),
  format(Stream,'/* SLIPCASE Final CLL ~f~n',[CLL]),
  format(Stream,'Execution time ~f~n',[WTS]),
  tell(Stream),
  listing(setting/2),
  format(Stream,'*/~n~n',[]),
  told, 
  open(FileOut,append,Stream1),
  write_rules(R,Stream1),
  close(Stream1).


learn_struct(DB,R1,R,CLL1):-   %+R1:initial theory of the form [rule(NR,[h],[b]],...], -R:final theory of the same form, -CLL1
  generate_clauses(R1,R2,0,[],Th1), 
  assert_all(Th1),  
  assert_all(R2),!,
  findall(R-HN,(rule(R,HL,_BL),length(HL,HN)),L),  
  keysort(L,LS),
  get_heads(LS,LSH),  
  length(LSH,NR),   
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),  
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),!   % 1 bdd for group of facts (for example if group=1)
  ;
    derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),! % 1 bdd for model
  ),
  setting(random_restarts_number,N),
  format("~nInitial CLL ~f~n~n",[CLL0]),
  random_restarts(N,Nodes,CLL0,CLL,initial,Par,LE),   %output:CLL,Par
  format("CLL after EMBLEM = ~f~n",[CLL]),
  retract_all(Th1),
  retract_all(R2),!,
  end,  %frees all variables
  update_theory(R2,Par,R3), 
  write('updated Theory'),nl,
  write_rules(R3,user_output),   %definite rules without probabilities in the head are not written
  setting(max_iter,M),
  cycle_struct([(R3,CLL)],DB,R3,R,M,CLL,-inf,CLL1). 

em(File):-
  generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL),
  reconsult(FileL),
  load_models(FileKB,DB),
  (file_exists(FileBG)->
    set(compiling,on),
    load(FileBG,_ThBG,RBG),
    set(compiling,off),
    generate_clauses(RBG,_RBG1,0,[],ThBG), 
    assert_all(ThBG)
  ;
    true
  ),
  set(compiling,on),
  load(FileIn,_TH,R0),
  set(compiling,off),
  set(verbosity,3),
  statistics(cputime,[_,_]),      
  learn_params(DB,R0,R,CLL),
  statistics(cputime,[_,CT]),
  CTS is CT/1000,
  format("EM: Final CLL ~f~n",[CLL]),
  format("Execution time ~f~n~n",[CTS]),
  write_rules(R,user_output),
  listing(setting/2),
  open(FileOut,write,Stream),
  format(Stream,'/* EMBLEM Final CLL ~f~n',[CLL]),
  format(Stream,'Execution time ~f~n',[CTS]),
  tell(Stream),
  listing(setting/2),
  format(Stream,'*/~n~n',[]),
  told,
  open(FileOut,append,Stream1),
  write_rules(R,Stream1),
  close(Stream1).

learn_params(DB,R0,R,CLL):-  
  generate_clauses(R0,R1,0,[],Th0), 
  assert_all(Th0),
  assert_all(R1),!,
  findall(R-HN,(rule(R,HL,_BL),length(HL,HN)),L),
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),!   
  ; 
   derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),!      
  ),
  setting(random_restarts_number,N),
  random_restarts(N,Nodes,CLL0,CLL,initial,Par,LE),  %computes new parameters Par
  end,  
  retract_all(Th0),
  retract_all(R1),!,
  update_theory(R1,Par,R).  %replaces in R1 the probabilities Par and outputs R


update_theory(R,initial,R):-!.

update_theory([],_Par,[]).

update_theory([def_rule(H,B)|T0],Par,[def_rule(H,B)|T]):-!,
  update_theory(T0,Par,T).

update_theory([(H:-B)|T0],Par,[(H:-B)|T]):-!,
  update_theory(T0,Par,T).

update_theory([rule(N,_H,_B)|T0],Par,T):-
  member([N,[1.0|_T]],Par),!,  
  update_theory(T0,Par,T).

update_theory([rule(N,H,B)|T0],Par,[rule(N,H1,B)|T]):-
  member([N,P],Par),!, 
  reverse(P,P1),
  update_head_par(H,P1,H1),  
  update_theory(T0,Par,T).

update_head_par([],[],[]).

update_head_par([H:_P|T0],[HP|TP],[H:HP|T]):-
  update_head_par(T0,TP,T).
  

cycle_struct([],_DB,R,R,_M,S,_SP,S):-!.

cycle_struct(_B,_DB,R,R,_M,S,SP,S):-
  setting(eps,Eps),
  setting(eps_f,EpsF),
  ( 
    (S-SP)<Eps
  ;
    (X is -S*EpsF,
    Y is S-SP, 
    Y<X)
  ),
  !.

cycle_struct(_Beam,_DB,R,R,0,S,_SP,S):-!.


cycle_struct([(RH,_ScoreH)|T],DB,R0,R,M,Score0,SP0,Score):-
  format("Iteration ~d",[M]),nl,nl,
  theory_revisions(RH,LR),!,   %+R1=rule(NR,[head],[body]), -LR:list of lists, each correponding to a different revised theory
  length(LR,NR),%NR:number of different revised theories
  write('Number of revisions '),write(NR),nl,
  score_refinements(LR,T,T1,1,NR,DB,R0,Score0,SP0,R3,S3,SP),
  %-SP, -R3: the best score SP and refined theory R3 (in the form: rule(NR,[head],[body])), from the set of theories generated in revise.pl
  write('Best refinement:'),nl,
  write_rules(R3,user_output),
  M1 is M-1,%decreases the number of max_iter M
  format("~nBest score (CLL) ~f~n~n",[S3]),
  cycle_struct(T1,DB,R3,R,M1,S3,SP,Score).

score_refinements([],B,B,_N,_NR,_DB,R,S,SP,R,S,SP).

score_refinements([R1|T],B0,B,Nrev,NRef,DB,R0,S0,SP0,R,S,SP):-  %scans the list of revised theories; returns S,R, the best (highest) score and revised theory R,after the comparisons at the end
  format('Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write_rules(R1,user_output),   
  generate_clauses(R1,R2,0,[],Th1),
  assert_all(Th1),
  assert_all(R2),!,
  findall(RN-HN,(rule(RN,HL,_BL),length(HL,HN)),L),  
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ; 
    derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),!
  ),
  setting(random_restarts_REFnumber,N),
  random_restarts_ref(N,Nodes,CLL0,CLL,initial,Par,LE),  
  end,
  update_theory(R2,Par,R3),
  write('Updated refinement'),nl,
  write_rules(R3,user_output), 
  Score = CLL,  
  write('Score (CLL) '),write(Score),nl,nl,nl,
  retract_all(Th1),
  retract_all(R2),!,
/*compares the score and theory found so far with the latest refinement R1 and associated score*/
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  setting(beamsize,BS),
  insert_in_order(B0,(R3,Score),BS,B1),
  Nrev1 is Nrev+1,  
  score_refinements(T,B1,B,Nrev1,NRef,DB,R4,S4,SP1,R,S,SP).

insert_in_order([],C,BeamSize,[C]):-
  BeamSize>0,!.

insert_in_order(Beam,_New,0,Beam):-!.

insert_in_order([(Th1,Heuristic1)|RestBeamIn],(Th,Heuristic),BeamSize,BeamOut):-
  Heuristic>Heuristic1,!,
  % larger heuristic, insert here
  NewBeam=[(Th,Heuristic),(Th1,Heuristic1)|RestBeamIn],
  length(NewBeam,L),
  (L>BeamSize->
    nth(L,NewBeam,_Last,BeamOut)
  ;
    BeamOut=NewBeam
  ).
  
insert_in_order([(Th1,Heuristic1)|RestBeamIn],(Th,Heuristic),BeamSize,
[(Th1,Heuristic1)|RestBeamOut]):-
  BeamSize1 is BeamSize -1,
  insert_in_order(RestBeamIn,(Th,Heuristic),BeamSize1,
  RestBeamOut).



remove_int_atom_list([],[]).

remove_int_atom_list([A|T],[A1|T1]):-
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).



remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].


get_heads([],[]).

get_heads([_-H|T],[H|TN]):-
  get_heads(T,TN).

derive_bdd_nodes([],_E,Nodes,Nodes,CLL,CLL).

derive_bdd_nodes([H|T],E,Nodes0,Nodes,CLL0,CLL):-
  get_output_atoms(O),
  generate_goal(O,H,[],GL),
  (prob(H,P)->
    CardEx is P*E
  
  ;
    CardEx is 1.0
  ),
  init_bdd,
  one(One),
  get_node_list(GL,One,BDD,CardEx),
  ret_prob(BDD,HP),
  (HP=:=0.0->
    setting(logzero,LZ),
    CLL1 is CLL0+LZ*CardEx
  ;
    CLL1 is CLL0+log(HP)*CardEx
  ),
  end_bdd,
  append(Nodes0,[[BDD,CardEx]],Nodes1),
  derive_bdd_nodes(T,E,Nodes1,Nodes,CLL1,CLL).


get_node_list([],BDD,BDD,_CE).

get_node_list([H|T],BDD0,BDD,CE):-
  get_node(H,BDD1),
  and(BDD0,BDD1,BDD2),
  get_node_list(T,BDD2,BDD,CE).

  
derive_bdd_nodes_groupatoms([],_E,_G,Nodes,Nodes,CLL,CLL,LE,LE).

derive_bdd_nodes_groupatoms([H|T],E,G,Nodes0,Nodes,CLL0,CLL,LE0,LE):-  %[H|T] models
  get_output_atoms(O),
  generate_goal(O,H,[],GL),
  length(GL,NA),
  (prob(H,P)->
    CardEx is P*E/NA
  ;
    CardEx is 1.0/NA
  ),
  get_node_list_groupatoms(GL,BDDs,CardEx,G,CLL0,CLL1,LE0,LE1),
  append(Nodes0,BDDs,Nodes1),
  derive_bdd_nodes_groupatoms(T,E,G,Nodes1,Nodes,CLL1,CLL,LE1,LE).

get_node_list_groupatoms([],[],_CE,_Gmax,CLL,CLL,LE,LE).

get_node_list_groupatoms([H|T],[[BDD,CE1]|BDDT],CE,Gmax,CLL0,CLL,LE0,LE):-
  init_bdd,  
  one(One),
  get_bdd_group([H|T],T1,Gmax,G,One,BDD,CE,LE0,LE1),  %output=BDD,CLL
  CE1 is CE*(Gmax-G),
  ret_prob(BDD,HP),
  end_bdd,
  (HP =:=0.0->
    setting(logzero,LZ),
    CLL2 is CLL0+LZ*CE1
  ;
    CLL2 is CLL0+log(HP)*CE1
  ),
  get_node_list_groupatoms(T1,BDDT,CE,Gmax,CLL2,CLL,LE1,LE).


get_bdd_group([],[],G,G,BDD,BDD,_CE,LE,LE):-!.

get_bdd_group(T,T,0,0,BDD,BDD,_CE,LE,LE):- !.

get_bdd_group([H|T],T1,Gmax,G1,BDD0,BDD,CE,[H|LE0],LE):-
  get_node(H,BDD1),  %creates bdd for atomo H
  and(BDD0,BDD1,BDD2),
  G is Gmax-1,
  get_bdd_group(T,T1,G,G1,BDD2,BDD,CE,LE0,LE).
  
/* EM start */
random_restarts(0,_Nodes,CLL,CLL,Par,Par,_LE):-!.

random_restarts(N,Nodes,CLL0,CLL,Par0,Par,LE):-
  setting(verbosity,Ver),
  (Ver>2->
    setting(random_restarts_number,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n~n",[Num]),
    flush_output
  ;
    true
  ),
  randomize,      
  setting(epsilon_em,EA),
  setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  setting(iter,Iter),
  em(Nodes,EA,ER,L,Iter,CLLR,Par1,_ExP),  
  setting(verbosity,Ver),
  (Ver>2->
    format("Random_restart: CLL ~f~n",[CLLR])
  ;
    true
  ),
  N1 is N-1,
  (CLLR>CLL0->     
    random_restarts(N1,Nodes,CLLR,CLL,Par1,Par,LE)
  ;
    random_restarts(N1,Nodes,CLL0,CLL,Par0,Par,LE)
  ).


random_restarts_ref(0,_Nodes,CLL,CLL,Par,Par,_LE):-!.

random_restarts_ref(N,Nodes,CLL0,CLL,Par0,Par,LE):-
  setting(verbosity,Ver),
  (Ver>2->
    setting(random_restarts_REFnumber,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n~n",[Num]),
    flush_output
  ;
    true
  ),
  setting(epsilon_em,EA),
  setting(epsilon_em_fraction,ER),
  length(Nodes,L),
  setting(iterREF,Iter),
  em(Nodes,EA,ER,L,Iter,CLLR,Par1,_ExP),  
  setting(verbosity,Ver),
  (Ver>2->
    format("Random_restart: CLL ~f~n",[CLLR])
  ;
    true
  ),
  N1 is N-1,
  (CLLR>CLL0->  
    random_restarts_ref(N1,Nodes,CLLR,CLL,Par1,Par,LE)
  ;
    random_restarts_ref(N1,Nodes,CLL0,CLL,Par0,Par,LE)
  ).


randomize([],[]):-!.

randomize([rule(N,V,NH,HL,BL,LogF)|T],[rule(N,V,NH,HL1,BL,LogF)|T1]):-
  length(HL,L),
  Int is 1.0/L,
  randomize_head(Int,HL,0,HL1),
  randomize(T,T1).

randomize_head(_Int,['':_],P,['':PNull1]):-!,
  PNull is 1.0-P,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).
  
randomize_head(Int,[H:_|T],P,[H:PH1|NT]):-
  PMax is 1.0-P,
  random(0,PMax,PH1),
  P1 is P+PH1,  
  randomize_head(Int,T,P1,NT).



update_head([],[],_N,[]):-!.  

update_head([H:_P|T],[PU|TP],N,[H:P|T1]):-
  P is PU/N,
  update_head(T,TP,N,T1).


/* EM end */    
  
  
/* utilities */

generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL):-
  generate_file_name(File,".kb",FileKB),
  generate_file_name(File,".cpl",FileIn),
  generate_file_name(File,".rules",FileOut),
  generate_file_name(File,".bg",FileBG),
  generate_file_name(File,".l",FileL).
        
generate_file_name(File,Ext,FileExt):-
  name(File,FileString),
  append(FileString,Ext,FileStringExt),
  name(FileExt,FileStringExt).
   
load_models(File,ModulesList):-  %carica le interpretazioni, 1 alla volta
  open(File,read,Stream),
  read_models(Stream,ModulesList),
  close(Stream).


read_models(Stream,[Name1|Names]):-
  read(Stream,begin(model(Name))),!,
  (number(Name)->
     name(Name,NameStr),
     append("i",NameStr,Name1Str),
     name(Name1,Name1Str)
  ;
     Name1=Name
  ),
  read_all_atoms(Stream,Name1),
  read_models(Stream,Names).

read_models(_S,[]).


read_all_atoms(Stream,Name):-
  read(Stream,At),
  At \=end(model(_Name)),!,
  (At=neg(Atom)->    
    Atom=..[Pred|Args],
    Atom1=..[Pred,Name|Args],
    assertz(neg(Atom1))
  ;
    (At=prob(Pr)->
      assertz(prob(Name,Pr))
    ;
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      assertz(Atom1)
    )
  ),
  read_all_atoms(Stream,Name).    

read_all_atoms(_S,_N).


write_param(initial,S):-!,
  format("~nInitial parameters~n",[]),
  findall(rule(R,H,B),rule(R,H,B),LDis),
  findall(rule(d,[H:1.0],B),def_rule(H,B),LDef),
  append(LDis,LDef,L),
  write_model(L,S).

write_param(L,S):-
  reverse(L,L1),
  write_par(L1,S).


write_par([],S):-
  findall(rule(d,[H:1.0],B),def_rule(H,B),L),
  write_model(L,S).

write_par([[N,P]|T],S):-
  rule(N,HL0,BL),
  reverse(P,PR),
  new_par(PR,HL0,HL),
  copy_term((HL,BL),(HL1,BL1)),
  numbervars((HL1,BL1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
  write_par(T,S).


write_rules([],_S).

write_rules([rule(_N,HL,BL)|T],S):-
  copy_term((HL,BL),(HL1,BL1)),
  numbervars((HL1,BL1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
  write_rules(T,S).


new_par([],[],[]).

new_par([HP|TP],[Head:_|TO],[Head:HP|TN]):-
  new_par(TP,TO,TN).


write_model([],_Stream):-!.

write_model([rule(_N,HL,BL)|Rest],Stream):-
  copy_term((HL,BL),(HL1,BL1)),
  numbervars((HL1,BL1),0,_M),
  write_disj_clause(Stream,(HL1:-BL1)),
  write_model(Rest,Stream).


write_disj_clause(S,(H:-[])):-!,
  write_head(S,H),
  format(S,".~n~n",[]).
    
write_disj_clause(S,(H:-B)):-
  write_head(S,H),
  write(S,' :-'),
  nl(S),
  write_body(S,B).



write_head(S,[A:1.0|_Rest]):-!,
  format(S,"~p",[A]).
  
write_head(S,[A:P,'':_P]):-!, 
  format(S,"~p:~g",[A,P]).

write_head(S,[A:P]):-!,
  format(S,"~p:~g",[A,P]).

write_head(S,[A:P|Rest]):-
  format(S,"~p:~g ; ",[A,P]),
  write_head(S,Rest).


write_body(S,[A]):-!,
  format(S,"\t~p.~n~n",[A]).
    
write_body(S,[A|T]):-
  format(S,"\t~p,~n",[A]),
  write_body(S,T).


list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).
 

deduct([],Th,Th).

deduct([M|T],InTheory0,InTheory):-
  get_head_atoms(O),
  generate_head(O,M,[],HL),
  generate_body(HL,InTheory1),
  append(InTheory0,InTheory1,InTheory2),
  deduct(T,InTheory2,InTheory).

        
get_head_atoms(O):-
  findall(A,modeh(_,A),O).


generate_head([],_M,HL,HL):-!.

generate_head([A|T],M,H0,H1):-
  functor(A,F,N),    
  functor(F1,F,N),   
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  findall((A,Pred1),call(neg(Pred1)),L),
  setting(initial_clauses_modeh,IC),   %IC: represents how many samples are extracted from the list L of example
  sample(IC,L,L1),   %+IC,L, -L1
  append(H0,L1,H2),
  generate_head(T,M,H2,H1).


sample(0,_List,[]):-!.

sample(N,List,List):-
  length(List,L),
  L=<N,!.

sample(N,List,[El|List1]):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample(N1,Rest,List1).

generate_body([],[]):-!.

generate_body([(A,H)|T],[(Head:0.5:-Body)|CL0]):-
  findall((R,B),modeb(R,B),BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  setting(d,D),
  cycle_modeb(ArgsTypes,Args,[],[],BL,a,[],BLout0,D,M),
  remove_duplicates(BLout0,BLout),
  variabilize((H:-BLout),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  copy_term((H:-BLout),CLa),
  numbervars(CLa,0,_N1),
  copy_term(CLV,CLav),
  numbervars(CLav,0,_N1v),
  CLV=(Head1:-BodyList1),
  remove_int_atom(Head1,Head),
  remove_int_atom_list(BodyList1,BodyList),
  list2and(BodyList,Body),
  generate_body(T,CL0).


variabilize((H:-B),(H1:-B1)):-
  variabilize_list([H|B],[H1|B1],[],_AS,_M).


variabilize_list([],[],A,A,_M).

variabilize_list([H|T],[H1|T1],A0,A,M):-
  H=..[F,_M|Args],
  variabilize_args(Args,Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).


variabilize_args([],[],A,A).

variabilize_args([C|T],[V|TV],A0,A):-
  member(C/V,A0),!,
  variabilize_args(T,TV,A0,A).

variabilize_args([C|T],[V|TV],A0,A):-
  variabilize_args(T,TV,[C/V|A0],A).


cycle_modeb(ArgsTypes,Args,ArgsTypes,Args,_BL,L,L,L,_,_M):-!.

cycle_modeb(_ArgsTypes,_Args,_ArgsTypes1,_Args1,_BL,_L,L,L,0,_M):-!.

cycle_modeb(ArgsTypes,Args,_ArgsTypes0,_Args0,BL,_L0,L1,L,D,M):-
  find_atoms(BL,ArgsTypes,Args,ArgsTypes1,Args1,L1,L2,M),
  D1 is D-1,
  cycle_modeb(ArgsTypes1,Args1,ArgsTypes,Args,BL,L1,L2,L,D1,M).


find_atoms([],ArgsTypes,Args,ArgsTypes,Args,L,L,_M).

find_atoms([(R,H)|T],ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-
  H=..[F|ArgsT],
  findall(A,instantiate_query(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,[],At),
  remove_duplicates(At,At1),
  (R = '*' ->
    R1= +inf
  ;
    R1=R
  ),
  sample(R1,At1,At2),
  extract_output_args(At2,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  append(L0,At2,L2),
  find_atoms(T,ArgsTypes1,Args1,ArgsTypes,Args,L2,L1,M).


call_atoms([],A,A).

call_atoms([H|T],A0,A):-
  findall(H,H,L),
  append(A0,L,A1),
  call_atoms(T,A1,A).


extract_output_args([],_ArgsT,ArgsTypes,Args,ArgsTypes,Args).

extract_output_args([H|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  H=..[_F,_M|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).


add_const([],[],ArgsTypes,Args,ArgsTypes,Args).

add_const([_A|T],[+_T|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[-Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).


already_present([+T|_TT],[C|_TC],C,T):-!.

already_present([_|TT],[_|TC],C,T):-
  already_present(TT,TC,C,T).


instantiate_query(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A=..[F,M|ArgsB].


instantiate_input([],_AT,_A,[]).

instantiate_input([-_Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([+Type|T],AT,A,[H|TA]):-
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).


find_val([T|_TT],[A|_TA],T,A).

find_val([_T|TT],[_A|TA],T,A):-
  find_val(TT,TA,T,A).


get_output_atoms(O):-
  findall((A/Ar),output((A/Ar)),O).


generate_goal([],_H,G,G):-!.

generate_goal([P/A|T],H,G0,G1):-
  functor(Pred,P,A),
  Pred=..[P|Rest],
  Pred1=..[P,H|Rest],
  findall(Pred1,call(Pred1),L),
  findall(\+ Pred1,call(neg(Pred1)),LN),
  append(G0,L,G2),
  append(G2,LN,G3),
  generate_goal(T,H,G3,G1).
  


:-[inference_sl].
