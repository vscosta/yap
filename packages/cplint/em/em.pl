/*

CEM

Copyright (c) 2011, Fabrizio Riguzzi

*/
%:- set_prolog_flag(unknown,error).
%:- set_prolog_flag(profiling,on).
%:- set_prolog_flag(debug,on).
:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).
:- set_prolog_flag(unknown,fail).
%:-source.
%:-yap_flag(gc_trace,very_verbose).
:- use_module(inference,
[find_deriv_inf1/3]).
%:-consult(inference).
:-use_module(library(rbtrees)).
:-use_module(library(random)).
:-use_module(library(avl)).
:-use_module(library(lists)).

%:-use_module(library(lpadsld)).
:-load_foreign_files(['cplint'],[],init_my_predicates).

:-dynamic setting/2,rule/5.


setting(depth,3).
setting(single_var,false).

setting(sample_size,1000). 
/* Total number of examples in case in which the models in the kb file contain
a prob(P). fact. In that case, one model corresponds to sample_size*P examples
*/
setting(equivalent_sample_size,100).
/* equivalent samaple size for computing the BD score of the network refinements
It is indicated with NPrime in the formulas on Heckerman, Geiger & Chickering
paper */
setting(epsilon_em,0.1).
setting(epsilon_em_fraction,0.01).
/* if the difference in log likelihood in two successive em iteration is smaller
than epsilon_em, then em stops */
setting(epsilon_sem,2).
setting(random_restarts_number,1).
/* number of random restarts of em */
setting(verbosity,1).



em(File):-
  generate_file_names(File,FileKB,FileOut,FileL,FileLPAD),
  reconsult(FileL),
  load_models(FileKB,DB),
  load_initial_model(FileLPAD,Model0),!,
  set(verbosity,3),
  statistics(cputime,[_,_]),  
  gen_ex(DB,[],DBE),
  compute_parameters_EM(Model0,Model,SuffStats,CLL,DBE),
  statistics(cputime,[_,CT]),
  CTS is CT/1000,
  format("Final CLL ~f~n",[CLL]),
  format("Execution time ~f~n",[CTS]),
  write_stats(user_output,SuffStats),
  listing(setting/2),
  format("Model:~n",[]),
  write_model(Model,user_output),
  open(FileOut,write,Stream),
  format(Stream,"/* Final CLL ~f~n",[CLL]),
  format(Stream,"Execution time ~f~n",[CTS]),
  tell(Stream),
  listing(setting/2),
  write_stats(Stream,SuffStats),
  format(Stream,"*/~n",[]),
  write_model(Model,Stream),
  told.

gen_ex([],DBE,DBE).

gen_ex([H|T],DB0,DB1):-
        get_ouptut_atoms(O),
  generate_goal(O,H,[],GL),
  append(DB0,GL,DB2),
  gen_ex(T,DB2,DB1).


cycle_head([],[],_NR,_S,_NH,_PG,_CSetList,_N):-!.

cycle_head([SSH0|T],[SSH1|T1],NR,S,NH,PG,CSetList,N):-
  extract_relevant_C_sets(NR,S,NH,CSetList,CSL1),
  (CSL1=[]->
    SSH1 is SSH0
  ;      
    build_formula(CSL1,Formula,[],Var),
    var2numbers(Var,0,NewVar),
    compute_prob(NewVar,Formula,Prob,0),
    SSH1 is SSH0 +Prob/PG*N
  ),
  NH1 is NH+1,
  cycle_head(T,T1,NR,S,NH1,PG,CSetList,N).

cycle_head_neg([],[],_NR,_S,_NH,_NA,_PG,_CSetList,_N):-!.

cycle_head_neg([SSH0|T],[SSH1|T1],NR,S,NH,NA,PG,CSetList,N):-
        extract_relevant_C_sets_neg(NR,S,NH,NA,CSetList,CSL1),
        (CSL1=[]->
                SSH1 is SSH0%+0.001
        ;                       
                build_formula(CSL1,Formula,[],Var),
                var2numbers(Var,0,NewVar),
                compute_prob(NewVar,Formula,Prob,0),
                (Prob>1 ->write(cyc),write(Prob),write(NewVar),nl;true),
                SSH1 is SSH0 +(1-Prob)/PG*N
        ),
        NH1 is NH+1,
        cycle_head_neg(T,T1,NR,S,NH1,NA,PG,CSetList,N).

extract_relevant_C_sets_neg(NR,S,NH,NA,CS,CS1):-
        neg_choice(0,NA,NH,NR,S,C),
        append(CS,C,CS1).

neg_choice(N,N,_NH,_NR,_S,[]):-!.

neg_choice(NH,NA,NH,NR,S,L):-!,
        N1 is NH+1,
        neg_choice(N1,NA,NH,NR,S,L).

neg_choice(N,NA,NH,NR,S,[[(N,NR,S)]|L]):-
        N1 is N+1,
        neg_choice(N1,NA,NH,NR,S,L).

extract_relevant_C_sets(_NR,_S,_NH,[],[]):-!.

extract_relevant_C_sets(NR,S,NH,[H|T],CS):-
  member((NH1,NR,S),H),!,
  extract_relevant_C_sets1(NR,S,NH,NH1,H,T,CS).

extract_relevant_C_sets(NR,S,NH,[H|T],[H1|CS]):-
  append(H,[(NH,NR,S)],H1),  
  extract_relevant_C_sets(NR,S,NH,T,CS).

extract_relevant_C_sets1(NR,S,NH,NH1,_H,T,CS):-
  NH1\=NH,!,
  extract_relevant_C_sets(NR,S,NH,T,CS).

extract_relevant_C_sets1(NR,S,NH,_NH1,H,T,[H|CS]):-
  extract_relevant_C_sets(NR,S,NH,T,CS).
    

  
/* EM start */
compute_parameters_EM([],[],SuffStats,-1e200,_DB):-!,
  rb_new(SuffStats).

compute_parameters_EM(Model0,Model1,SuffStats1,CLL1,DB):-
  setting(verbosity,Ver),
  (Ver>0->
    format("EM computation ~nInitial model:~n",[]),
    write_model(Model0,user_output),
    flush_output
  ;
    true
  ),
  (Ver>2->
    format("Initial EM Iteration ~n",[]),
    flush_output
  ;
    true
  ),
  randomize(Model0,ModelR),
  em_iteration(ModelR,Model,SuffStats,CLL,DB),
  (Ver>2->
    format("CLL ~f~n",[CLL])
  ;
    true
  ),
  flush_output,
  setting(random_restarts_number,N),
  random_restarts(N,Model,SuffStats,CLL,Model1,SuffStats1,CLL1,DB),
  (Ver>0->
    format("Final CLL ~f~n",[CLL1]),
    flush_output
  ;
    true
  ).
  
random_restarts(1,Model,SS,CLL,Model,SS,CLL,_DB):-!.

random_restarts(N,Model0,SS0,CLL0,Model1,SS1,CLL1,DB):-
  setting(verbosity,Ver),
  (Ver>2->
    setting(random_restarts_number,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n",[Num]),
    flush_output
  ;
    true
  ),
  randomize(Model0,ModelR),
  em_iteration(ModelR,ModelR1,SSR,CLLR,DB),
  setting(verbosity,Ver),
  (Ver>2->
    format("CLL ~f~n",[CLLR])
  ;
    true
  ),
  N1 is N-1,
  (CLLR>CLL0->
    random_restarts(N1,ModelR1,SSR,CLLR,Model1,SS1,CLL1,DB)
  ;
    random_restarts(N1,Model0,SS0,CLL0,Model1,SS1,CLL1,DB)
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



em_iteration(Model0,ModelPar,SuffStats1,CLL1,DB):-
  compute_CLL_stats(Model0,DB,CLL0,SuffStats0),
/*  setting(verbosity,Ver),
  (Ver>2->
    format("EM Iteration numer ~d~nCLL ~f~n",[N,CLL0]),
    write_stats(user_output,SuffStats0)
  ;
    true
  ),*/
  cycle_EM(Model0,SuffStats0,CLL0,ModelPar,SuffStats1,CLL1,DB,1).
  
cycle_EM(Model0,SuffStats0,CLL0,ModelPar,SuffStats,CLL,DB,N):-
  m_step(Model0,SuffStats0,Model1),
  compute_CLL_stats(Model1,DB,CLL1,SuffStats1),
  setting(verbosity,Ver),
  (Ver>2->
    format("Iteration: ~d CLL ~f~n",[N,CLL1])
  ;
    true
  ),
  flush_output,
%  write_stats(user_output,SuffStats1),
%  statistics,
  setting(epsilon_em,Epsilon_EM),
  setting(epsilon_em_fraction,Epsilon_EM_Frac),
  ((CLL1-CLL0<Epsilon_EM;(CLL1-CLL0)< - CLL0*Epsilon_EM_Frac)->
    ModelPar=Model1,
    SuffStats=SuffStats1,
    CLL=CLL1,!
  ;
    N1 is N+1,!,
    cycle_EM(Model1,SuffStats1,CLL1,ModelPar,SuffStats,CLL,DB,N1)
  ).

write_stats(S,SS):-
  rb_visit(SS,Pairs),
  format(S,"Suff stats~n",[]),
  write_stats_list(S,Pairs).

write_stats_list(S,[]):-nl(S),nl(S),!.

write_stats_list(S,[R-d(D,N,I)|T]):-
  format(S,"~d,~p,~f,~d~n",[R,D,N,I]),
  write_stats_list(S,T).

m_step([],_SS,[]):-!.

m_step([rule(N,V,NH,HL,BL,LogF)|T],SS,[rule(N,V,NH,HL1,BL,LogF)|T1]):-
  (rb_lookup(N,d(Distr,_NBT,_NI),SS)->
    sum_list(Distr,NBT),
    update_head(HL,Distr,NBT,HL1)
  ;
    HL1=HL
  ),
  m_step(T,SS,T1).

update_head([],[],_N,[]).  

update_head([H:_P|T],[PU|TP],N,[H:P|T1]):-
  P is PU/N,
  update_head(T,TP,N,T1).


/* EM end */    
  
  
/* Start of computation of log likelihood and sufficient stats */
compute_CLL_stats(Model,DB,CLL,SuffStats1):-
  assert_model(Model),
  compute_CLL_stats_examples(DB,CLL,SuffStats1),
  retract_model.

assert_model([]):-!.

assert_model([rule(N,V,NH,HL,BL,_LogF)|T]):-
  assert_rules(HL,0,HL,BL,NH,N,V),
  assertz(rule_by_num(N,V,NH,HL,BL)),
  assert_model(T).

retract_model:-
  retractall(rule_by_num(_,_,_,_,_)),
  retractall(rule(_,_,_,_,_,_,_,_)).

assert_rules([],_Pos,_HL,_BL,_Nh,_N,_V1):-!.

assert_rules(['':_P],_Pos,_HL,_BL,_Nh,_N,_V1):-!.

assert_rules([H:P|T],Pos,HL,BL,NH,N,V1):-
        assertz(rule(H,P,Pos,N,V1,NH,HL,BL)),
        Pos1 is Pos+1,
        assert_rules(T,Pos1,HL,BL,NH,N,V1).

compute_CLL_stats_examples(DB,CLL,SuffStats1):-
  rb_new(SuffStats0),
  compute_CLL_stats_cplint(DB,0,CLL,SuffStats0,SuffStats1).

get_ouptut_atoms(O):-
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
  
compute_CLL_stats_cplint([],CLL,CLL,S,S):-!.

compute_CLL_stats_cplint([\+ H|T],CLL0,CLL1,Stats0,Stats1):-!,
        setting(verbosity,V),
        (V>3->
                write(user_error,(\+ H)),nl(user_error),flush_output
        ;
                true
        ),
        s([H],CL,CSetList,PG),!,
        (PG=:=1.0->
                CLL2=CLL0,
                Stats2=Stats0   
        ;
                (prob(H,P)->
                        setting(sample_size,NTot),
                        N is P*NTot
                ;
                        N=1
                ),
                PG1 is 1-PG,
                CLL2 is CLL0+log(PG1)*N,
                collect_stats_cplint_neg(CL,PG1,CSetList,N,Stats0,Stats2)
        ),
        compute_CLL_stats_cplint(T,CLL2,CLL1,Stats2,Stats1).

compute_CLL_stats_cplint([H|T],CLL0,CLL1,Stats0,Stats1):-
        setting(verbosity,V),
        (V>3->
                write(user_error,H),nl(user_error),flush_output
        ;
                true
        ),
  s([H],CL,CSetList,PG),!,
  (PG=0.0->
    CLL2=CLL0,
    Stats2=Stats0  
  ;
    (prob(H,P)->
      setting(sample_size,NTot),
      N is P*NTot
    ;
      N=1
    ),
    CLL2 is CLL0+log(PG)*N,
    collect_stats_cplint(CL,PG,CSetList,N,Stats0,Stats2)
  ),
  compute_CLL_stats_cplint(T,CLL2,CLL1,Stats2,Stats1).



s(GoalsList,GroundLpad,CSets,Prob):-
  solve(GoalsList,GroundLpad,CSets,Prob).

solve(GoalsList,GroundLpad,LDup,Prob):-
        setting(depth,D),
        findall(Deriv,inference:find_deriv_inf1(GoalsList,D,Deriv),LDup),
        (LDup=[]->
                Prob=0.0,
                GroundLpad=[]
        ;
                append(LDup,L0),
                remove_head(L0,L1),
                remove_duplicates(L1,L2),
                build_ground_lpad(L2,GroundLpad),
                build_formula(LDup,Formula,[],Var),
                var2numbers(Var,0,NewVar),
                compute_prob(NewVar,Formula,Prob,0),
                true
        ).

collect_stats_cplint([],_PG,_CSetList,_N,Stats,Stats):-!.  

collect_stats_cplint([(R,S,Head,_Body)|T],PG,CSetList,N,Stats0,Stats1):-
  (rb_lookup(R,d(Distr0,N1,NInst1),Stats0)->
    cycle_head(Distr0,Distr,R,S,0,PG,CSetList,N),
    N2 is N+N1,
    rb_update(Stats0,R,d(Distr,N2,NInst1),Stats2)
  ;
    length(Head,LH),
    list0(0,LH,Distr0),
    cycle_head(Distr0,Distr,R,S,0,PG,CSetList,N),
    rb_insert(Stats0,R,d(Distr,N,1),Stats2)
  ),
  collect_stats_cplint(T,PG,CSetList,N,Stats2,Stats1).

collect_stats_cplint_neg([],_PG,_CSetList,_N,Stats,Stats):-!.

collect_stats_cplint_neg([(R,S,Head,_Body)|T],PG,CSetList,N,Stats0,Stats1):-
        length(Head,NA),
        (rb_lookup(R,d(Distr0,N1,NInst1),Stats0)->
                cycle_head_neg(Distr0,Distr,R,S,0,NA,PG,CSetList,N),
                N2 is N+N1,
                rb_update(Stats0,R,d(Distr,N2,NInst1),Stats2)
        ;
                length(Head,LH),
                list0(0,LH,Distr0),
                cycle_head_neg(Distr0,Distr,R,S,0,NA,PG,CSetList,N),
                rb_insert(Stats0,R,d(Distr,N,1),Stats2)
        ),
        collect_stats_cplint_neg(T,PG,CSetList,N,Stats2,Stats1).

/* build_formula(LC,Formula,VarIn,VarOut) takes as input a set of C sets
LC and a list of Variables VarIn and returns the formula and a new list
of variables VarOut 
Formula is of the form [Term1,...,Termn]
Termi is of the form [Factor1,...,Factorm]
Factorj is of the form (Var,Value) where Var is the index of
the multivalued variable Var and Value is the index of the value
*/
build_formula([],[],Var,Var,C,C).

build_formula([D|TD],[F|TF],VarIn,VarOut,C0,C1):-
        length(D,NC),
        C2 is C0+NC,
        build_term(D,F,VarIn,Var1),
        build_formula(TD,TF,Var1,VarOut,C2,C1).

build_formula([],[],Var,Var).

build_formula([D|TD],[F|TF],VarIn,VarOut):-
        build_term(D,F,VarIn,Var1),
        build_formula(TD,TF,Var1,VarOut).

build_term([],[],Var,Var).

build_term([(_,pruned,_)|TC],TF,VarIn,VarOut):-!,
        build_term(TC,TF,VarIn,VarOut).

build_term([(N,R,S)|TC],[[NVar,N]|TF],VarIn,VarOut):-
        (nth0_eq(0,NVar,VarIn,(R,S))->
                Var1=VarIn
        ;
                append(VarIn,[(R,S)],Var1),
                length(VarIn,NVar)
        ),
        build_term(TC,TF,Var1,VarOut).

/* nth0_eq(PosIn,PosOut,List,El) takes as input a List,
an element El and an initial position PosIn and returns in PosOut
the position in the List that contains an element exactly equal to El
*/
nth0_eq(N,N,[H|_T],El):-
        H==El,!.

nth0_eq(NIn,NOut,[_H|T],El):-
        N1 is NIn+1,
        nth0_eq(N1,NOut,T,El).

/* var2numbers converts a list of couples (Rule,Substitution) into a list
of triples (N,NumberOfHeadsAtoms,ListOfProbabilities), where N is an integer 
starting from 0 */
var2numbers([],_N,[]).

var2numbers([(R,S)|T],N,[[N,ValNumber,Probs]|TNV]):-
        find_probs(R,S,Probs),
        length(Probs,ValNumber),
        N1 is N+1,
        var2numbers(T,N1,TNV).

find_probs(R,S,Probs):-
        rule_by_num(R,S,_N,Head,_Body),
        get_probs(Head,Probs).

get_probs(uniform(_A:1/Num,_P,_Number),ListP):-
        Prob is 1/Num,
        list_el(Num,Prob,ListP).

get_probs([],[]).

get_probs([_H:P|T],[P1|T1]):-
        P1 is P,
        get_probs(T,T1).

list_el(0,_P,[]):-!.

list_el(N,P,[P|T]):-
        N1 is N-1,
        list_el(N1,P,T).
    
sum(_NS,[],[],[]):-!.  

sum(NS,[H0|T0],[H1|T1],[H2|T2]):-
  H2 is H0+H1*NS,
  sum(NS,T0,T1,T2).  
  
times(_NS,[],[]):-!.  

times(NS,[H0|T0],[H1|T1]):-
  H1 is H0*NS,
  times(NS,T0,T1).  

/* End of computation of log likelihood and sufficient stats */

/* Utility predicates */
generate_file_names(File,FileKB,FileOut,FileL,FileLPAD):-
    generate_file_name(File,".kb",FileKB),
    generate_file_name(File,".rules",FileOut),
    generate_file_name(File,".cpl",FileLPAD),
    generate_file_name(File,".l",FileL).
        
generate_file_name(File,Ext,FileExt):-
    name(File,FileString),
    append(FileString,Ext,FileStringExt),
    name(FileExt,FileStringExt).

    
set(Parameter,Value):-
  retract(setting(Parameter,_)),
  assert(setting(Parameter,Value)).

load_initial_model(File,Model):-
  open(File,read,S),
  read_clauses(S,C),
  close(S),
  process_clauses(C,1,_N,[],Model).

process_clauses([(end_of_file,[])],N,N,Model,Model).

process_clauses([((H:-B),_V)|T],N,N2,Model0,Model1):-
        H=(db(A)),!,
  assert((A:-B)),
  process_clauses(T,N,N2,Model0,Model1).

process_clauses([((H:-B),V)|T],N,N2,Model0,[rule(N,V1,NH,HL,BL,0)|Model1]):-
  H=(_;_),!,
  list2or(HL1,H),
  process_head(HL1,HL,VI),
  list2and(BL0,B),
  add_int_atom(BL0,BL,VI),
  length(HL,LH),
  listN(0,LH,NH),
  N1 is N+1,
  (setting(single_var,true)->
    V1=[]
  ;
    V1=V
  ),
%  assertz(rule(N,V,NH,HL,BL)),
  process_clauses(T,N1,N2,Model0,Model1).

process_clauses([((H:-B),V)|T],N,N2,Model0,[rule(N,V1,NH,HL,BL,0)|Model1]):-
  H=(_:_),!,
  list2or(HL1,H),
  process_head(HL1,HL,VI),
  list2and(BL0,B),
  add_int_atom(BL0,BL,VI),
  length(HL,LH),
  listN(0,LH,NH),
  (setting(single_var,true)->
    V1=[]
  ;
    V1=V
  ),
  N1 is N+1,
%  assertz(rule(N,V1,NH,HL,BL)),
  process_clauses(T,N1,N2,Model0,Model1).
  
process_clauses([((H:-B),V)|T],N,N2,Model0,[rule(N,V1,NH,HL,BL,0)|Model1]):-!,
  process_head([H:1.0],HL,VI),
  list2and(BL0,B),
  add_int_atom(BL0,BL,VI),
  length(HL,LH),
  listN(0,LH,NH),
  (setting(single_var,true)->
    V1=[]
  ;
    V1=V
  ),
  N1 is N+1,
%  assertz(rule(N,V1,NH,HL,BL)),
  process_clauses(T,N1,N2,Model0,Model1).

process_clauses([(H,V)|T],N,N2,Model0,[rule(N,V1,NH,HL,[],0)|Model1]):-
  H=(_;_),!,
  list2or(HL1,H),
  process_head(HL1,HL,_VI),
  length(HL,LH),
  listN(0,LH,NH),
  (setting(single_var,true)->
    V1=[]
  ;
    V1=V
  ),
  N1 is N+1,
%  assertz(rule(N,V,NH,HL,[])),
  process_clauses(T,N1,N2,Model0,Model1).

process_clauses([(H,V)|T],N,N2,Model0,[rule(N,V1,NH,HL,[],0)|Model1]):-
  H=(_:_),!,
  list2or(HL1,H),
  process_head(HL1,HL,_VI),
  length(HL,LH),
  listN(0,LH,NH),
  (setting(single_var,true)->
    V1=[]
  ;
    V1=V
  ),
  N1 is N+1,
%  assertz(rule(N,V,NH,HL,[])),
  process_clauses(T,N1,N2,Model0,Model1).
  
process_clauses([(H,V)|T],N,N2,Model0,[rule(N,V1,NH,HL,[],0)|Model1]):-
  process_head([H:1.0],HL,_VI),
  length(HL,LH),
  listN(0,LH,NH),
  (setting(single_var,true)->
    V1=[]
  ;
    V1=V
  ),
  N1 is N+1,
%  assertz(rule(N,V,NH,HL,[])),
  process_clauses(T,N1,N2,Model0,Model1).


/* if the annotation in the head are not ground, the null atom is not added
and the eventual formulas are not evaluated */
  
process_head([H:P|T],NHL,VI):-!,
    process_head_prob([H:P|T],0.0,NHL,VI).

process_head(HL,NHL,VI):-
    process_head_random(HL,0.0,NHL,VI).

process_head_random([],P,['':PNull1],_VI):-
  PNull is 1.0-P,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).
  
process_head_random([H|T],P,[H1:PH1|NT],VI):-
  add_int_atom([H],[H1],VI),
  PMax is 1.0-P,
  random(0,PMax,PH1),
  P1 is P+PH1,  
  process_head_random(T,P1,NT,VI).

    
process_head_prob([H:PH],P,[H1:PH1,'':PNull1],VI):-
  add_int_atom([H],[H1],VI),
  PH1 is PH,
  PNull is 1.0-P-PH1,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).
  
process_head_prob([H:PH|T],P,[H1:PH1|NT],VI):-
  add_int_atom([H],[H1],VI),
  PH1 is PH,
  P1 is P+PH1,
  process_head_prob(T,P1,NT,VI).


add_int_atom([],[],_VI).

add_int_atom([\+ H|T],[\+ H|T1],VI):-
    inference:builtin(H),!,
  add_int_atom(T,T1,VI).

add_int_atom([\+ H|T],[\+ H1|T1],VI):-!,
  H=..[F|Args],
  H1=..[F,VI|Args],
  add_int_atom(T,T1,VI).

add_int_atom([H|T],[H|T1],VI):-
  inference:builtin(H),!,
  add_int_atom(T,T1,VI).

add_int_atom([H|T],[H1|T1],VI):-
  H=..[F|Args],
  H1=..[F,VI|Args],
  add_int_atom(T,T1,VI).

/* predicates for reading in the program clauses */
read_clauses(S,Clauses):-
    read_clauses_ground_body(S,Clauses).


read_clauses_ground_body(S,[(Cl,V)|Out]):-
  read_term(S,Cl,[variable_names(V)]),
  (Cl=end_of_file->
    Out=[]
  ;
    read_clauses_ground_body(S,Out)
  ).



  
listN(N,N,[]):-!.

listN(NIn,N,[NIn|T]):-
  N1 is NIn+1,
  listN(N1,N,T).

list0(N,N,[]):-!.

list0(NIn,N,[0|T]):-
  N1 is NIn+1,
  list0(N1,N,T).

/* end of predicates for parsing an input file containing a program */


load_models(File,ModulesList):-
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
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      assertz(Atom1)
    ),
    read_all_atoms(Stream,Name).    

read_all_atoms(_S,_N).


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
    

write_model([],_Stream):-!.

write_model([rule(_N,_V,_NH,HL,BL,_LogF)|Rest],Stream):-
  copy_term((HL,BL),(HL1,BL1)),
    numbervars((HL1,BL1),0,_M),
    write_disj_clause(Stream,(HL1:-BL1)),
    format(Stream,".~n~n",[]),
    write_model(Rest,Stream).


write_disj_clause(S,(H:-[])):-!,
  write_head(S,H).
    
write_disj_clause(S,(H:-B)):-
  write_head(S,H),
  write(S,' :-'),
  nl(S),
  write_body(S,B).
  
write_head(S,[A:1.0|_Rest]):-!,
  remove_int_atom(A,A1),
    format(S,"~p",[A1]).
  
write_head(S,[A:P,'':_P]):-!,
  remove_int_atom(A,A1),
    format(S,"~p:~f",[A1,P]).

write_head(S,[A:P|Rest]):-
  remove_int_atom(A,A1),
    format(S,"~p:~f ; ",[A1,P]),
    write_head(S,Rest).

write_body(S,[\+ A]):-!,
  remove_int_atom(A,A1),
    format(S,"\t\\+ ~p",[A1]).

write_body(S,[A]):-!,
  remove_int_atom(A,A1),
    format(S,"\t~p",[A1]).
    
write_body(S,[\+ A|T]):-!,
  remove_int_atom(A,A1),
    format(S,"\t\\+ ~p,~n",[A1]),
    write_body(S,T).

write_body(S,[A|T]):-
  remove_int_atom(A,A1),
    format(S,"\t~p,~n",[A1]),
    write_body(S,T).

    
remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].

build_ground_lpad([],[]):-!.

build_ground_lpad([(R,S)|T],[(R,S,Head,Body)|T1]):-
  user:rule_by_num(R,S,_,Head,Body),
  build_ground_lpad(T,T1).


remove_head([],[]).

remove_head([(_N,R,S)|T],[(R,S)|T1]):-
  remove_head(T,T1).


append_all([],L,L):-!.

append_all([LIntH|IntT],IntIn,IntOut):-
  append(IntIn,LIntH,Int1),
  append_all(IntT,Int1,IntOut).

