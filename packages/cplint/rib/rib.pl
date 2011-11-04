/*

RIB

Copyright (c) 2011, Fabrizio Riguzzi and Nicola di Mauro

*/
:-multifile setting/2,mode/2,modeh/2,modeb/2,neg/1.
%:- set_prolog_flag(profiling,on).
%:- set_prolog_flag(debug,on).
:- set_prolog_flag(discontiguous_warnings,on).
:- set_prolog_flag(single_var_warnings,on).
%:-yap_flag(gc_trace,very_verbose).
%:- source.
:- use_module(inference_ib,
  [build_network/5,
  get_prob/3,
  get_CL/3,
  remove_head/2,
  get_atoms/2,
  find_deriv_inf1/2,
  find_atoms_head/3]).

:-use_module(library(lists)).
:-use_module(library(rbtrees)).
:-use_module(library(random)).
:-set_prolog_flag(unknown,fail).

%:-use_module(library(lpadsld)).

:-dynamic setting/2.

/* Total number of examples in case in which the models in the kb file contain
a prob(P). fact. In that case, one model corresponds to sample_size*P examples
*/
setting(sample_size,1000). 
setting(setrand,rand(1230,45,123)).

setting(verbosity,1).
setting(minimal_step,0.005).
setting(maximal_step,0.1).
setting(depth_bound,3).
setting(logsize_fraction,0.9).
/* value assigned to log 0
*/
setting(delta,-10).

setting(epsilon_fraction,100).

/* maximum number of non-ground rules 
*/
setting(max_rules,6000).
/* maximum number of variables in an output rule
*/


/* Data structures:

array qy: Q(y)
array q: Q(ch|y), for each y q store a red black tree with ch(N,S) as key
array p: P(N), for each rule N store d(Probs,Atoms)
PAX: rb tree with atoms as key and list of parents (ch(N,S)) as values
CH: list of choice variables, each of the form ch(N,S) where N is the rule
number and S is a substitution that ground rule N
R: list of elemtns N-Inst where N is a rule number and Inst is a list of
instantiations

Storage of examples:
an extra argument is added to each ground atom to store the example id (integer)
atom a(b,c,d) that is true in interpretation 10 is asserted as
a(10,a,b,c,d)
*/


ib_par(File):-
  setting(setrand,K),
  setrand(K),
  generate_file_names(File,FileKB,FileOut,FileL,FileLPAD,FileBG),
  reconsult(FileL),
        (file_exists(FileBG)->
                reconsult(FileBG)
        ;
                true
        ),
  load_models(FileKB),
  retractall(hb(_,_,_)),
  ex(NS),
  set(sample_size,NS),
  load_initial_model(FileLPAD,Model),!,
  randomize(Model,Model0),
  set(verbosity,3),
  statistics(runtime,[_,_]),  
  assert_model(Model0),  
  write_model(Model0,user_output),
  compute_parameters_IB(Model0,Model1,I,LogSize),
  statistics(runtime,[_,CT]),
  CTS is CT/1000,
  format("Execution time ~f LogSize ~f Final I ~f~n",[CTS,LogSize,I]),
  listing(setting/2),
  format("Model:~n",[]),
  write_model(Model1,user_output),
  open(FileOut,write,Stream),
  format(Stream,"/* Execution time ~f LogSize ~f Final I ~f~n",[CTS,LogSize,I]),
  tell(Stream),
  listing(setting/2),
  format(Stream,"*/~n",[]),
  write_model(Model1,Stream),
  told.

/* checks whether two variable substitutions are the same,
independently of the variable names */
match_subs([],_S1):-!.

match_subs([_V1=S|TS],[_V2=S|TS1]):-
  match_subs(TS,TS1).
  
/* computes the parameters of a model by using the information bottleneck
*/
compute_parameters_IB(Model0,Model1,I2,LogSize):-
  build_network_IB(CH,PAX,T,TCh,R,LogSize),
  my_set_value(ch,CH),
  my_set_value(pax,PAX),
  my_set_value(t,T),  
  my_set_value(tch,TCh),
  (CH=[]->
    Model1=Model0
  ;
    length(CH,LCH),
    format("Logsize ~f ground clauses ~d~n",[LogSize,LCH]),
    ex(NEx),
    array(q,NEx),
    array(qt,NEx),
    build_q_table(CH),
    build_qt_table(T),
    static_array(qy,NEx,float),
    build_q_Y_table(0,NEx),
    build_p_table(Model0),
    rb_new(Q_ch0),
    compute_Q_ch(CH,Q_ch0,Q_ch1),
    rb_new(Q_t0),
    compute_Q_t(T,Q_t0,Q_t1),
    compute_I(NEx,CH,Q_ch1,T,Q_t1,I1),
    compute_Nr(R,NR),
    iterate_IB_par(0.0,I1,I2,1,Q_ch1,Q_t1,R,NR,LogSize,Model0),
    build_new_model(Model0,Model1),
    flush_output
  ).

/* updates the model with the distribution stored in the array p (P 
distribution after the optimization)
*/
build_new_model([],[]).

build_new_model([rule(N,V,NH,HL,BL,LogF)|T],[rule(N,V,NH,HL1,BL,LogF)|T1]):-
  my_get_value(ch,CH),
  member(ch(N,_S),CH),!,
  array_element(p,N,d(Distr,_Val)),
  new_dist(HL,Distr,HL1),
  build_new_model(T,T1).

build_new_model([H|T],[H|T1]):-
  build_new_model(T,T1).
/* given a head as a list of annotated atoms and a list of probabilities,
returns the head with the annotations taken from the list of probabilities
*/
new_dist([],[],[]):-!.

new_dist([H:_P|HL],[P1|TP],[H:P1|HL1]):-
  new_dist(HL,TP,HL1).

/* builds the initial Q(Y) table:
it assigns to each example either
1) the probability indicated for the example in the .kb file
2) or 1/n if n is the number of examples
*/
build_q_Y_table(N,N):-!.

build_q_Y_table(N,NEx):-
  (prob(N,P)->
    true
  ;
    P is 1/NEx
  ),
  update_array(qy,N,P),
  N1 is N+1,
  build_q_Y_table(N1,NEx).

/* main cycle of parameter learning with ib
*/  
iterate_IB_par(Gamma,I,I,N,_Q_ch,_Q_t,_R,_NR,LogSize,_Model):-
  setting(logsize_fraction,LF),

  (
  Gamma>0.9999
  ;I>LogSize*LF
  ),
  format("Iteration ~d~nGamma ~f~n I ~f~n",[N,Gamma,I]),
  !.  


iterate_IB_par(Gamma,I,I2,N,Q_ch,Q_t,R,NR,LogSize,Model):-
  format("Iteration ~d~nGamma ~f~n I ~f~n",[N,Gamma,I]),
  maximize_Q(Gamma,Q_ch,Q_t,R,LogSize,NR,Gamma1),
  compute_Nr(R,NR1),
  maximize_P(R,NR1),
  ex(NEx),
  rb_new(Q_ch0),
  my_get_value(ch,CH),
  compute_Q_ch(CH,Q_ch0,Q_ch1),
  rb_new(Q_t0),
  my_get_value(t,T),
  compute_Q_t(T,Q_t0,Q_t1),
  compute_I(NEx,CH,Q_ch1,T,Q_t1,I1),
  N1 is N+1,
  build_new_model(Model,Model1),
  retract_model,
  assert_model(Model1),
  flush_output,
  iterate_IB_par(Gamma1,I1,I2,N1,Q_ch1,Q_t1,R,NR1,LogSize,Model).


/* computes the P distribution that maximizes the Lagrangian
*/
maximize_P([],[]):-!.

maximize_P([_N-_Inst|TR],[0.0|TNR]):-!,
  maximize_P(TR,TNR).

maximize_P([N-_Inst|TR],[_NR|TNR]):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  maximize_P(TR,TNR).

maximize_P([N-Inst|TR],[NR|TNR]):-
  rule_by_num(N,_V,_NH,HL,_BL),
  length(HL,LH),
  list0(0,LH,Num0),  
  ex(NEx),
  compute_theta_num(0,NEx,Inst,N,Num0,Num),
  divide(NR,Num,Theta),
%  normalize(Theta,Theta1),
  array_element(p,N,d(_,Val)),
  update_array(p,N,d(Theta,Val)),
  maximize_P(TR,TNR).

/* computes the numerator of \theta_{hd_r|true}, i.e.
\sum_{s\in i(r)}\sum_{y}Q(y)Q(Ch_s=hd_r|y)bt(pa_{ch_s}[y])+\alpha(r,hd_r,true)=
\sum_{s,ch_s\in i(r)}\mathcal{N}(s,hd_r)+\alpha(r,hd_r,true)=
\mathcal{N}(r,hd_r)
*/
compute_theta_num(N,N,_Inst,_N,Num,Num):-!.

compute_theta_num(Y,NEx,Inst,N,Num0,Num1):-
  array_element(qy,Y,Q_Y),
  array_element(q,Y,Q_ch_Y_par),
  find_true_inst_unseen(Inst,N,Y,TInst),
  compute_theta_term(TInst,Y,N,Q_Y,Q_ch_Y_par,Num0,Num2),
  Y1 is Y+1,
  compute_theta_num(Y1,NEx,Inst,N,Num2,Num1).

/*   computes 
\sum_{y}Q(y)Q(Ch_s=hd_r|y)bt(pa_{ch_s}[y])+\alpha(r,hd_r,true)=
\mathcal{N}(s,hd_r)+\alpha(r,hd_r,true)
*/
compute_theta_term([],_Y,_N,_Q_Y,_Q_ch_Y_par,Term,Term):-!.

compute_theta_term([S|TS],Y,N,Q_Y,Q_ch_Y_par,Term0,Term1):-
  rb_lookup(ch(N,S),Q_ch_Y,Q_ch_Y_par),
  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  prob_body(BL,Y,1,PTB,_B),
  times(Q_Y,Q_ch_Y,TermS0),
  times(PTB,TermS0,TermS),  
  sum(Term0,TermS,Term2),
  compute_theta_term(TS,Y,N,Q_Y,Q_ch_Y_par,Term2,Term1).
  
/* computes the Q distribution that maximizes the Lagrangian 
*/
maximize_Q(Gamma,Q_ch,Q_t,R,LogSize,NR,Gamma1):-
  my_get_value(ch,CH),
  my_get_value(pax,PAX),
  my_get_value(t,T),
  setting(delta,Delta),
  ex(NEx),
  compute_D(0,NEx,R,NR,D),
  compute_Q_gradient(0,NEx,Gamma,Delta,Q_ch,Q_t,R,NR,CH,PAX,T,D,[],QGrad,[],IGrad,
    [],QTGrad,[],ITGrad),
  compute_step(QGrad,IGrad,QTGrad,ITGrad,LogSize,S),
  setting(minimal_step,MinS),
  setting(maximal_step,MaxS),
  ((S<MinS;\+ S< +inf)->
    S1=MinS
  ;
    (S>MaxS->
      S1=MaxS
    ;
      S1=S
    )
  ),
  format("Step: original ~f, adjusted ~f~n",[S,S1]),
  Gamma1 is Gamma +S1,
  update_Q(0,NEx,CH,QGrad,S1),
  update_QT(0,NEx,T,QTGrad,S1).

/* computes I_Q(CH;Y)=
\sum_y\sum_i\sum_{ch_i}Q(y)Q(ch_i|y)(\log Q(ch_i|y)-\log Q(ch_i))=
(\sum_y\sum_i\sum_{ch_i}Q(y)Q(ch_i|y)\log Q(ch_i|y))-E_Q[\log Q(ch_i)]
*/
compute_I(NEx,CH,Q_ch,T,Q_t,I):-
  compute_I(0,NEx,CH,Q_ch,T,Q_t,0,I0),
  compute_Exp_Q_ch(CH,Q_ch,0,I1),
  compute_Exp_Q_t(T,Q_t,0,I2),
%  format("I0 ~f I1 ~f I2 ~f~n",[I0,I1,I2]),
  I is I0+I1+I2.

compute_I(N,N,_CH,_Q_ch_par,_T,_Q_t,I,I):-!.

compute_I(Y,NEx,CH,Q_ch_par,T,Q_t_par,I0,I1):-
  array_element(qy,Y,Q_Y),
  array_element(q,Y,Q_ch_Y_par),
  array_element(qt,Y,Q_t_Y_par),
  compute_I_ch(CH,Q_ch_Y_par,Q_Y,0,ICH),
  compute_I_t(T,Q_t_Y_par,Q_Y,0,IT),
  I2 is I0+ICH+IT,
  %format("Y ~d ICH ~f IT ~f~n",[Y,ICH,IT]),
  Y1 is Y+1,
  compute_I(Y1,NEx,CH,Q_ch_par,T,Q_t_par,I2,I1).

  
/* computes -\sum_i E_Q[\log Q(Ch_i)]=-\sum_{ch_i}Q(ch_i)\log Q(ch_i)
*/
compute_Exp_Q_ch([],_Q_ch_par,I,I):-!.

compute_Exp_Q_ch([ch(N,_S)|T],Q_ch_par,I0,I1):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  compute_Exp_Q_ch(T,Q_ch_par,I0,I1).

compute_Exp_Q_ch([CH|T],Q_ch_par,I0,I1):-
  rb_lookup(CH,Q_ch,Q_ch_par),
  compute_Exp_Q_ch_val(Q_ch,I0,I2),
  compute_Exp_Q_ch(T,Q_ch_par,I2,I1).

compute_Exp_Q_t([],_Q_t_par,I,I):-!.

compute_Exp_Q_t([H|T],Q_t_par,I0,I1):-
  rb_lookup(H,Q_t,Q_t_par),
  I2 is I0-Q_t*log(Q_t)-(1-Q_t)*log(1-Q_t),
  compute_Exp_Q_t(T,Q_t_par,I2,I1).

/* computes -E_Q[\log Q(Ch_i)]=-\sum_{ch_i}Q(ch_i)\log Q(ch_i)
*/
compute_Exp_Q_ch_val([],I,I):-!.

compute_Exp_Q_ch_val([Q_ch|T],I0,I1):-
  I2 is I0-Q_ch*log(Q_ch),
  compute_Exp_Q_ch_val(T,I2,I1).

/* computes \sum_i\sum_{ch_i}Q(y)Q(ch_i|y)\log Q(ch_i|y)
*/
compute_I_ch([],_Q_ch_Y_par,_Q_Y,I,I):-!.

compute_I_ch([ch(N,_S)|T],Q_ch_Y_par,Q_Y,I0,I1):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  compute_I_ch(T,Q_ch_Y_par,Q_Y,I0,I1).

compute_I_ch([CH|T],Q_ch_Y_par,Q_Y,I0,I1):-
  rb_lookup(CH,Q_ch_Y,Q_ch_Y_par),
  compute_exp_I(Q_ch_Y,Q_Y,I0,I2),
  compute_I_ch(T,Q_ch_Y_par,Q_Y,I2,I1).

compute_I_t([],_Q_t_Y_par,_Q_Y,I,I):-!.

compute_I_t([H|T],Q_t_Y_par,Q_Y,I0,I1):-
  rb_lookup(H,Q_t_Y,Q_t_Y_par),
  ((Q_t_Y=<0;Q_t_Y>1)->format("I ~p ~f~n",[H,Q_t_Y]);true),
  I2 is I0+Q_Y*(Q_t_Y*log(Q_t_Y)+(1-Q_t_Y)*log(1-Q_t_Y)),
  compute_I_t(T,Q_t_Y_par,Q_Y,I2,I1).

/* computes \sum_{ch_i}Q(y)Q(ch_i|y)\log Q(ch_i|y)
*/
compute_exp_I([],_Q_Y,I,I):-!.

compute_exp_I([Q_ch_Y|T0],Q_Y,I0,I1):-
  I2 is I0+Q_ch_Y*Q_Y*log(Q_ch_Y),
  compute_exp_I(T0,Q_Y,I2,I1).

/* computes the new values for the distribution Q(Ch|Y) given the gradient
vector
*/
update_Q(NEx,NEx,_CH,[],_S):-!.

update_Q(Y,NEx,CH,[QG|TQG],S):-
  array_element(q,Y,Q_ch_Y_par),
  update_Q_Y(CH,Q_ch_Y_par,QG,S,Q_ch_Y_par1),
  update_array(q,Y,Q_ch_Y_par1),
  Y1 is Y+1,
  update_Q(Y1,NEx,CH,TQG,S).


/* computes the new values for the distribution Q(Ch|Y) given the gradient
vector and an example Y
*/
update_Q_Y([],Q_ch_Y_par,[],_S,Q_ch_Y_par):-!.

update_Q_Y([ch(N,_S)|TCH],Q_ch_Y_par,TQG,S,Q_ch_Y_par1):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  update_Q_Y(TCH,Q_ch_Y_par,TQG,S,Q_ch_Y_par1).

update_Q_Y([CH|TCH],Q_ch_Y_par,[HQG|TQG],S,Q_ch_Y_par1):-
  rb_lookup(CH,Dist,Q_ch_Y_par),
  times(S,HQG,Delta),
  %fomat("Delta ~p~n",[Delta]),
  sum_lower(Dist,Delta,Dist1),
  normalize(Dist1,Dist2),
  rb_update(Q_ch_Y_par,CH,Dist2,Q_ch_Y_par2),
  update_Q_Y(TCH,Q_ch_Y_par2,TQG,S,Q_ch_Y_par1).

update_QT(NEx,NEx,_T,[],_S):-!.

update_QT(Y,NEx,T,[QG|TQG],S):-
  array_element(qt,Y,Q_t_Y_par),
%  format("Y ~d~n",[Y]),
  update_QT_Y(T,Q_t_Y_par,QG,S,Q_t_Y_par1),
  update_array(qt,Y,Q_t_Y_par1),
  Y1 is Y+1,
  update_QT(Y1,NEx,T,TQG,S).

update_QT_Y([],Q_t_Y_par,[],_S,Q_t_Y_par):-!.

update_QT_Y([HT|TT],Q_t_Y_par,[HQG|TQG],S,Q_t_Y_par1):-
  rb_lookup(HT,Pr,Q_t_Y_par),
  times(S,HQG,Delta),
    sum_lower([1-Pr,Pr],Delta,Dist1),
  normalize(Dist1,[_,Pr1]),
%  format("t=~p Dist ~p ~f~n",[HT,Dist1,Pr1]),
  rb_update(Q_t_Y_par,HT,Pr1,Q_t_Y_par2),
  update_QT_Y(TT,Q_t_Y_par2,TQG,S,Q_t_Y_par1).


/* sum the values of Q(ch|y) with the values of the step vector \delta
*/
sum_lower([],[],[]):-!.  

sum_lower([H0|T0],[H1|T1],[H3|T2]):-
  (H1 < +inf->
    H2 is H0+H1,
    (H2<0.0001->
      H3=0.0001
    ;
      H3=H2
    )
  ;
    format("overflow",[]),
    H3=H0+100
  ),
  sum_lower(T0,T1,T2).  

/* normalizes a distribution
*/
normalize(Dist1,Dist2):-
  sum_list(Dist1,Sum),
  (Sum=:=0.0->format("0 Sum",[])
  ;
    true
  ),
  divide(Sum,Dist1,Dist2).

/* compute the step size 
*/
compute_step(QGrad,IGrad,QTGrad,ITGrad,LogSize,S):-
  %format("CH grad~n",[]),
  compute_step(QGrad,IGrad,0,0,Sum0),
  %format("T grad~n",[]),
  compute_step(QTGrad,ITGrad,0,Sum0,Sum),
  setting(epsilon_fraction,EF),
  %format("Sum ~f~n",[Sum]),
  S is LogSize/EF/Sum.

compute_step([],[],_Y,S,S).


compute_step([HQ|TQ],[HI|TI],Y,Sum,S):-
  append(HQ,DQ),
  append(HI,DI),
  vec_times(DQ,DI,D),
  %format("~p~n",[D]),
  sum_list(D,SumY),
  %format("Y ~d SumY ~f~n",[Y,SumY]),
  Sum1 is Sum + SumY,
  Y1 is Y+1,
  compute_step(TQ,TI,Y1,Sum1,S).

/* computes Q(Ch)
*/  
compute_Q_ch([],Q_ch,Q_ch):-!.
  
compute_Q_ch([ch(N,S)|T],Q_ch0,Q_ch1):-
  rule_by_num(N,_V,_NH,HL,_BL),!,
  length(HL,L),
  list0(0,L,P0),
  ex(NEx),
  compute_Q_ch_DB(0,NEx,ch(N,S),P0,P1),
  rb_insert(Q_ch0,ch(N,S),P1,Q_ch2),
  compute_Q_ch(T,Q_ch2,Q_ch1).

compute_Q_ch([ch(N,S)|T],Q_ch0,Q_ch1):-
  rb_insert(Q_ch0,ch(N,S),1.0,Q_ch2),
  compute_Q_ch(T,Q_ch2,Q_ch1).


/* computes \sum_y Q(ch|y)Q(y) for all values ch of a single choice variable
*/
compute_Q_ch_DB(N,N,_CH,P,P):-!.

compute_Q_ch_DB(Y,NEx,CH,P0,P1):-
  array_element(qy,Y,PY),
  array_element(q,Y,Q_ch_par),
  rb_lookup(CH,Par,Q_ch_par),
  update_dist(P0,Par,PY,P2),
  Y1 is Y+1,
  compute_Q_ch_DB(Y1,NEx,CH,P2,P1).

/* computes Q(ch|y)Q(y) for all values ch of a single choice variable and sums
the result to an accumulator
*/
update_dist([],[],_PY,[]):-!.

update_dist([HP0|P0],[HPar|Par],PY,[HP1|P1]):-
  HP1 is HP0+HPar*PY,
  update_dist(P0,Par,PY,P1).
  
/* computes Q(T)
*/  
compute_Q_t([],Q_t,Q_t):-!.
  
compute_Q_t([H|T],Q_t0,Q_t1):-
  ex(NEx),
  compute_Q_t_DB(0,NEx,H,0,P1),
  rb_insert(Q_t0,H,P1,Q_t2),
  compute_Q_t(T,Q_t2,Q_t1).

/* computes \sum_y Q(t|y)Q(y) for all values t of a single t variable
*/
compute_Q_t_DB(N,N,_T,P,P):-!.

compute_Q_t_DB(Y,NEx,T,P0,P1):-
  array_element(qy,Y,PY),
  array_element(qt,Y,Q_t_par),
  rb_lookup(T,Par,Q_t_par),
  P2 is P0+Par*PY,
  Y1 is Y+1,
  compute_Q_t_DB(Y1,NEx,T,P2,P1).

/* computes the components of the gradient of Q, i.e. the direction that 
maximizes the Lagrangian, i.e.
\frac{\partial G_{ch_i,y}(Q,\gamma)}{\partial Q(ch_{i}|y)}&=&
-\frac{1}{Q(ch_{i}|y)}+\\
&&Q(y)(1-Q(ch_{i}|y))(\frac{1-\gamma}{Q(ch_i)}+\\
&&\gamma E_{Q(CH|ch_{i},y_0)}[\mathcal{D}(y,ch_{t(i,y)},ch_i)]
for all Ch_i, all values ch_i and all y

Moreover, it computes the gradient of I
\frac{\partial I_Q(CH;Y)}{\partial Q(ch_{i0}|y_0)}&=&-\frac{\partial E_Q[\log Q(CH_i)}{\partial Q(ch_{i0}|y_0)}+\frac{\partial E_Q[\log Q(CH_i|y_0)}{\partial Q(ch_{i0}|y_0)}=\\
&&-Q(y_0)(\log Q(ch_{i0})+1)+Q(y_0)(\log Q(ch_{i0}|y_0)+1)=\\
&&Q(y_0)(-\log Q(ch_{i0})-1+\log Q(ch_{i0}|y_0)+1)=\\
&&Q(y_0)(\log Q(ch_{i0}|y_0)-\log Q(ch_{i0}))
for all Ch_i, all values ch_i and all y
*/
compute_Q_gradient(NEx,NEx,_Gamma,_Delta,_Q_ch,_Q_t,_R,_NR,_CH,_PAX,_T,[],QGrad,QGrad,IGrad,IGrad,QTGrad,QTGrad,ITGrad,ITGrad):-!.
/*
  QTGrad=[H|_],
  format("~p~n",[H]).
*/
compute_Q_gradient(Y,NEx,Gamma,Delta,Q_ch,Q_t,R,NR,CH,PAX,T,[D|DT],QGrad0,QGrad1,IGrad0,IGrad1,QTGrad0,QTGrad1,ITGrad0,ITGrad1):-
  array_element(q,Y,Q_ch_Y_par),
  array_element(qt,Y,Q_t_Y_par),
  array_element(qy,Y,Q_Y),
  cycle_CH(CH,Y,Gamma,Delta,Q_ch_Y_par,Q_Y,Q_ch,Q_t_Y_par,PAX,D,[],QGradY,[],IGradY),
  append(QGrad0,[QGradY],QGrad2),
  append(IGrad0,[IGradY],IGrad2),
  rb_new(U0),
  compute_U(R,NR,Y,Q_ch_Y_par,U0,U),
  cycle_T(T,Y,Gamma,Delta,U,Q_ch_Y_par,Q_Y,Q_t,Q_t_Y_par,PAX,D,[],QTGradY,[],ITGradY),
  append(QTGrad0,[QTGradY],QTGrad2),
  append(ITGrad0,[ITGradY],ITGrad2),
  Y1 is Y+1,
  compute_Q_gradient(Y1,NEx,Gamma,Delta,Q_ch,Q_t,R,NR,CH,PAX,T,DT,QGrad2,QGrad1,IGrad2,IGrad1,QTGrad2,QTGrad1,ITGrad2,ITGrad1).

compute_U([],[],_Y,_Q_ch_Y_par,U,U).

compute_U([HR-Inst|TR],[NR|TNR],Y,Q_ch_Y_par,U0,U):-
  compute_U_inst(Inst,HR,NR,Y,Q_ch_Y_par,[],UR),
  rb_insert(U0,HR,UR,U1),
  compute_U(TR,TNR,Y,Q_ch_Y_par,U1,U).
  
compute_U_inst([],_N,_NR,_Y,_Q_ch_Y_par,U,U).

compute_U_inst([S|TS],N,NR,Y,Q_ch_Y_par,U0,U):-
  def_rule_by_num(N,Sub,_HL,BL),!,
  match_subs(S,Sub),
  (body_true_unseen(BL,_BL,Y,1)->
    prob_body(BL,Y,1,PTB,_B),
    rb_lookup(ch(N,S),Q_ch_y,Q_ch_Y_par),  
    scan_ch(Q_ch_y,[1.0],NR,PTB,UCH),
    append(U0,[ch(N,S)-UCH],U1)
  ;
    U1 = U0
  ),
  compute_U_inst(TS,N,NR,Y,Q_ch_Y_par,U1,U).

compute_U_inst([S|TS],N,NR,Y,Q_ch_Y_par,U0,U):-
  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  (body_true_unseen(BL,_BL,Y,1)->
    prob_body(BL,Y,1,PTB,_B),
    rb_lookup(ch(N,S),Q_ch_y,Q_ch_Y_par),  
    array_element(p,N,d(Theta,_Val)),
    scan_ch(Q_ch_y,Theta,NR,PTB,UCH),
    append(U0,[ch(N,S)-UCH],U1)
  ;
    U1 = U0
  ),
  compute_U_inst(TS,N,NR,Y,Q_ch_Y_par,U1,U).


scan_ch([],[],_NR,_PTB,[]).
  
scan_ch([Q_ch_yH|Q_ch_yT],[ThetaH|ThetaT],NR,PTB,[UCHH|UCHT]):-
  UCHH is Q_ch_yH*PTB/NR*(1/ThetaH+1),
  scan_ch(Q_ch_yT,ThetaT,NR,PTB,UCHT).
  
  




/* Computes \mathcal{N}(r), the denominator of \theta_{ch_r|true} for all rules
r
i.e
\sum_{s\in i(r)}\sum_y Q(y)bt(pa_{ch_s}[y]+\alpha(r,true)
for all r
*/
compute_Nr([],[]):-!.

compute_Nr([N-_Inst|TR],[1.0|TNR]):-
  def_rule_by_num(N,_S,_H,_B),!,
  compute_Nr(TR,TNR).

compute_Nr([N-Inst|TR],[NR|TNR]):-
  ex(NEx),
  compute_Nr_R(0,NEx,N,Inst,0,NR),
  compute_Nr(TR,TNR).

/* Computes \mathcal{N}(r), for a single rule r
i.e
\sum_{s\in i(r)}\sum_y Q(y)bt(pa_{ch_s}[y]+\alpha(r,true)
*/
compute_Nr_R(NEx,NEx,_N,_Inst,NR,NR):-!.

compute_Nr_R(Y,NEx,N,Inst,NR0,NR1):-
  array_element(qy,Y,Q_Y),
  find_true_inst_unseen(Inst,N,Y,TInst),
  scan_inst(TInst,N,Y,0,PTB),
  NR2 is NR0+PTB*Q_Y,
  Y1 is Y + 1,
  compute_Nr_R(Y1,NEx,N,Inst,NR2,NR1).

scan_inst([],_N,_Y,PTB,PTB).

scan_inst([S|TS],N,Y,PTB0,PTB):-
  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  prob_body(BL,Y,1,PTSB,_B),
  PTB1 is PTB0+PTSB,
  scan_inst(TS,N,Y,PTB1,PTB).
  
prob_body([],_Y,PTSB,PTSB,[]).

prob_body([\+ H|T],Y,PTSB0,PTSB,B1):-!,
  functor(H,P,A0),
  A is A0-1,
  (unseen(P/A)->
    H=..[P,_|Args],
    H1=..[P|Args],
    array_element(qt,Y,Q_t_Y_par),
    rb_lookup(H1,Q_t_y,Q_t_Y_par),
    PTSB1 is PTSB0*(1-Q_t_y),
    B1=[\+ H1|B0]
  ;
    PTSB1=PTSB0,
    B1=B0
    
  ),
  prob_body(T,Y,PTSB1,PTSB,B0).

prob_body([H|T],Y,PTSB0,PTSB,B1):-
  functor(H,P,A0),
  A is A0-1,
  (unseen(P/A)->
    H=..[P,_|Args],
    H1=..[P|Args],
    array_element(qt,Y,Q_t_Y_par),
    rb_lookup(H1,Q_t_y,Q_t_Y_par),
    PTSB1 is PTSB0*Q_t_y,
    B1=[H1|B0]
  ;
    PTSB1=PTSB0,
    B1=B0
  ),
  prob_body(T,Y,PTSB1,PTSB,B0).


/* computes \mathcal{D}(y_0,ch_{t(i,y_0)},ch_{i0}
\frac{1}{\mathcal{N}(r(i))}\left (\sum_{k,k\in  t(i,y_0)}\frac{1}{\theta_{Hd_{r(i)}=ch_k|true}}\right )\left (\sum_{s\in t(i,y_0),ch_{i0}=ch_s}
1\right )=\\
\frac{1}{\mathcal{N}(r(i))}T(i,ch_{t(i,y_0)})N(i,ch_{t(i,y_0)},ch_{i0})
for all values of y
*/
compute_D(NEx,NEx,_Rules,_NR,[]):-!.

compute_D(Y,NEx,Rules,NR,[DY|TDY]):-
  compute_D_y(Rules,NR,Y,DY),
  Y1 is Y+1,
  compute_D(Y1,NEx,Rules,NR,TDY).

/* computes \mathcal{D}(y_0,ch_{t(i,y_0)},ch_{i0}
for a single value of y
*/
compute_D_y([],[],_Y,[]):-!.

compute_D_y([N-_Inst|TR],[1.0|TNR],Y,[N-0|DY]):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  compute_D_y(TR,TNR,Y,DY).

compute_D_y([N-Inst|TR],[NR|TNR],Y,[N-DYN|DY]):-
  rule_by_num(N,_V,_NH,HL,_BL),
  length(HL,L),
  find_true_inst_unseen(Inst,N,Y,TInst),
%  length(TInst,LI),
%  format("N inst ~d~n",[LI]),
  generate_combinations(TInst,Y,N,L,NR,[],[],DYN),!,
  compute_D_y(TR,TNR,Y,DY).

/* computes \mathcal{D}(y_0,ch_{t(i,y_0)},ch_{i0}
for a single combination of values for ch_{t(i,y_0)}
*/
compute_D_comb(L,L,_Y,_Values,_PD,[]):-!.

compute_D_comb(L0,L,Y,Values,PD,[DVec|TD]):-
  L1 is L0+1,
  findall(ch(N,S),member((ch(N,S)=L1),Values),List),
  scan_subs_list(List,Y,PD,DVec),
  compute_D_comb(L1,L,Y,Values,PD,TD).

scan_subs_list([],_Y,_PD,[]).

scan_subs_list([ch(N,S)|T],Y,PD,[(B,QT,D)|DVec0]):-
  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  prob_body(BL,Y,1,QT,B),
  D is PD*QT,
  scan_subs_list(T,Y,PD,DVec0).


/* computes T(i,ch_{t(i,y_0)}=
\sum_{k,k\in  t(i,y_0)}\frac{1}{\theta_{Hd_{r(i)}=ch_k|true}}
for a single combination of values for ch_{t(i,y_0)}
*/
compute_T([],_Theta,T,T):-!.

compute_T([_=Val|TVal],ThetaR,T0,T1):-
  nth(Val,ThetaR,Theta),
  T2 is T0+1/Theta,
  compute_T(TVal,ThetaR,T2,T1).
  
/* generates all combinations of values for ch_{t(i,y_0)}
and computes \mathcal{D}(y_0,ch_{t(i,y_0)},ch_{i0}
*/
generate_combinations([],Y,N,L,NR,Values,DYN,[f(Values,D)|DYN]):-!,
  array_element(p,N,d(ThetaR,_Val)),
  compute_T(Values,ThetaR,0,T),
  PD is 1/NR*T,
  compute_D_comb(0,L,Y,Values,PD,D).
  
generate_combinations([S|TS],Y,N,L,NR,Values,DYN0,DYN1):-
  cycle_values(0,L,Y,N,S,TS,NR,Values,DYN0,DYN1).

/* auxiliary predicate for generating all combinations of values for 
ch_{t(i,y_0)}
*/  
cycle_values(L,L,_Y,_N,_S,_TS,_NR,_Values,DYN,DYN):-!.
  
cycle_values(V,L,Y,N,S,TS,NR,Values,DYN0,DYN1):-
  V1 is V+1,
  generate_combinations(TS,Y,N,L,NR,[ch(N,S)=V1|Values],DYN0,DYN2),
  cycle_values(V1,L,Y,N,S,TS,NR,Values,DYN2,DYN1).
  
/* finds the rule instances that have the body true */
find_true_inst([],_N,_Ex,[]):-!.

find_true_inst([S|TS],N,Ex,[S|TInst]):-
  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  body_true(BL,Ex,1),!,
  find_true_inst(TS,N,Ex,TInst).

find_true_inst([_S|TS],N,Ex,TInst):-
  find_true_inst(TS,N,Ex,TInst).

find_true_inst_unseen([],_N,_Ex,[]):-!.

find_true_inst_unseen([S|TS],N,Ex,[S|TInst]):-
  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  body_true_unseen(BL,_BL,Ex,1),!,
  find_true_inst_unseen(TS,N,Ex,TInst).

find_true_inst_unseen([_S|TS],N,Ex,TInst):-
  find_true_inst_unseen(TS,N,Ex,TInst).

/* cycles over all the choice variables for computing the gradient
\frac{\partial G_{ch_i,y}(Q,\gamma)}{\partial Q(ch_{i}|y)}
*/
cycle_CH([],_Y,_Gamma,_Delta,_Q_ch_Y_par,_Q_Y,_Q_ch_par,_Q_t_Y_par,_PAX,_D,QGrad,QGrad,IGrad,IGrad):-!.

cycle_CH([ch(N,_S)|T],Y,Gamma,Delta,Q_ch_Y_par,Q_Y,Q_ch_par,Q_t_Y_par,PAX,D,QGrad0,QGrad1,IGrad0,IGrad1):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  cycle_CH(T,Y,Gamma,Delta,Q_ch_Y_par,Q_Y,Q_ch_par,Q_t_Y_par,PAX,D,QGrad0,QGrad1,IGrad0,IGrad1).


cycle_CH([ch(N,S)|T],Y,Gamma,Delta,Q_ch_Y_par,Q_Y,Q_ch_par,Q_t_Y_par,PAX,D,QGrad0,QGrad1,IGrad0,IGrad1):-
  rule_by_num(N,S,_NH,HL,BL),
  get_atoms_head(HL,Val),
  rb_lookup(ch(N,S),Q_ch_y,Q_ch_Y_par),
  rb_lookup(ch(N,S),Q_ch,Q_ch_par),
  compute_E_log_Q_ch(Q_ch_y,Q_ch,0,E_log_Q_ch), %ok
  body_true_unseen(BL,_B,Y,BodyTrue),!,
  prob_body(BL,Y,1,PTB,_B1),
  array_element(p,N,d(Theta,_Val)),
  compute_R(Val,ch(N,S),1,Y,PAX,Q_ch_Y_par,Q_t_Y_par,0,[],R), % ok
  compute_EP(Val,Q_ch_y,Theta,BodyTrue,PTB,Q_t_Y_par,Y,EP,R,Delta,0,EEP),
  member((N-DN),D),!,
  length(HL,NVal),
  list0(0,NVal,ED0),
  compute_ED(DN,S,Q_ch_Y_par,ED0,ED),
  cycle_CH_values(Q_ch_y,Q_ch,Q_Y,E_log_Q_ch,Gamma,EP,EEP,ED,D_ch_y,DI_ch_y),
  append(QGrad0,[D_ch_y],QGrad2),
  append(IGrad0,[DI_ch_y],IGrad2),
  cycle_CH(T,Y,Gamma,Delta,Q_ch_Y_par,Q_Y,Q_ch_par,Q_t_Y_par,PAX,D,QGrad2,QGrad1,IGrad2,IGrad1).

/* cycles over all the values of a choice variable for computing the gradient
\frac{\partial G_{ch_i,y}(Q,\gamma)}{\partial Q(ch_{i}|y)}
*/
cycle_CH_values([],[],_Q_y,_E_log_Q_ch,_Gamma,[],_EEP,[],[],[]):-!.

cycle_CH_values([Q_ch_y|TQ_ch_y],[Q_ch|TQ_ch],Q_y,E_log_Q_ch,Gamma,[EP|TEP],EEP,[ED|TED],
  [Dir|D],[D_I|I]):-
  D_Q is -1/Q_ch_y+Q_y*(1-Q_ch_y)*((1-Gamma)/Q_ch+Gamma*ED),
  D_G is -log(Q_ch)+EP-EEP+E_log_Q_ch,
  Dir is -D_G/D_Q,
  ((Q_ch_y<1e-20;Q_ch<1e-20;abs(D_Q)<1e-10;abs(Dir)> 1e10)->
    format("~f ~f ~f~n",[Q_ch_y,Q_ch,Dir])
  ;
    true
  ),
  D_I is Q_y*(log(Q_ch_y)-log(Q_ch)),
  cycle_CH_values(TQ_ch_y,TQ_ch,Q_y,E_log_Q_ch,Gamma,TEP,EEP,TED,D,I).

cycle_T([],_Y,_Gamma,_Delta,_U,_Q_ch_Y_par,_Q_Y,_Q_t_par,_Q_t_Y_par,_PAX,_D,QGrad,QGrad,IGrad,IGrad):-!.

cycle_T([TH|T],Y,Gamma,Delta,U,Q_ch_Y_par,Q_Y,Q_t_par,Q_t_Y_par,PAX,D,QGrad0,QGrad1,IGrad0,IGrad1):-
  rb_lookup(TH,Q_t_y,Q_t_Y_par),
  rb_lookup(TH,Q_t,Q_t_par),
  ((Q_t=<0;Q_t>1)->format("Q_t ~d ~p ~f~n",[Y,TH,Q_t]);true),
  ((Q_t_y=<0;Q_t_y>1)->format("Q_t_y ~d ~p ~f~n",[Y,TH,Q_t_y]);true),
  Exp_log_Q_T is (1-Q_t_y)*log(1-Q_t)+Q_t_y*log(Q_t),
  TH=..[F|Args],
  TH1=..[F,_|Args],
  find_rules_with_T_in_the_body(TH,RT),
  rb_lookup(TH,Pa_Th,PAX),
  compute_EPT(RT,TH1,Pa_Th,Q_ch_Y_par,Q_t_y,Y,Delta,0,0,EPt,EPf),
  Exp_Q_EP is (1-Q_t_y)*EPf+Q_t_y*EPt,
  compute_EF(RT,TH1,Y,U,Q_ch_Y_par,Q_t_y,Q_Y,0,0,EFt,EFf),
  D_Qf is -1/(1-Q_t_y)+(1-Q_t)*Q_t_y*((1-Gamma)/(1-Q_t)+Gamma*EFf),
  D_Gf is -log(1-Q_t)+EPf-Exp_Q_EP+Exp_log_Q_T,
  Dirf is -D_Gf/D_Qf,
  D_Qt is -1/Q_t_y+Q_t*(1-Q_t_y)*((1-Gamma)/Q_t+Gamma*EFt),
  D_Gt is -log(Q_t)+EPt-Exp_Q_EP+Exp_log_Q_T,
  Dirt is -D_Gt/D_Qt,
  DirIf is Q_Y*(log(1-Q_t_y)-log(1-Q_t)),
  DirIt is  Q_Y*(log(Q_t_y)-log(Q_t)),
  append(QGrad0,[[Dirf,Dirt]],QGrad2),
  append(IGrad0,[[DirIf,DirIt]],IGrad2),
  cycle_T(T,Y,Gamma,Delta,U,Q_ch_Y_par,Q_Y,Q_t_par,Q_t_Y_par,PAX,D,QGrad2,QGrad1,IGrad2,IGrad1).

find_rules_with_T_in_the_body(TH,RT):-
  my_get_value(tch,TCh),
  rb_lookup(TH,RT,TCh).

compute_EF([],_T,_Y,_U,_Q_ch_Y_par,Q_t_y,Q_y,EFt0,EFf0,EFt,EFf):-
  EFt is EFt0*Q_y/Q_t_y,
  EFf is EFf0*Q_y/(1-Q_t_y).
  
compute_EF([ch(N,S)|TR],T,Y,U,Q_ch_Y_par,Q_t_y,Q_y,EFt0,EFf0,EFt,EFf):-
  def_rule_by_num(N,S1,_HL,BL),!,
  match_subs(S,S1),
  prob_body(BL,Y,1,PTB,_BT),
  rb_lookup(ch(N,S),Q_ch_y,Q_ch_Y_par),
  find_U_N(U,N,UN),
  scan_UN(UN,Q_ch_y,0,0,SumUt,SumUf),
  (member(T,BL)->
    PTB1 is PTB/Q_t_y,
    EFt1 is PTB1*SumUt+EFt0,
    EFf1 = EFf0
  ;
    PTB1 is PTB/(1-Q_t_y),
    EFf1 is PTB1*SumUf+EFf0,
    EFt1 = EFt0
  ),
  compute_EF(TR,T,Y,U,Q_ch_Y_par,Q_t_y,Q_y,EFt1,EFf1,EFt,EFf).


compute_EF([ch(N,S)|TR],T,Y,U,Q_ch_Y_par,Q_t_y,Q_y,EFt0,EFf0,EFt,EFf):-
  rule_by_num(N,S1,_NH,_HL,BL),
  match_subs(S,S1),
  prob_body(BL,Y,1,PTB,_BT),
  rb_lookup(ch(N,S),Q_ch_y,Q_ch_Y_par),
  find_U_N(U,N,UN),
  scan_UN(UN,Q_ch_y,0,0,SumUt,SumUf),
  (member(T,BL)->
    PTB1 is PTB/Q_t_y,
    EFt1 is PTB1*SumUt+EFt0,
    EFf1 = EFf0
  ;
    PTB1 is PTB/(1-Q_t_y),
    EFf1 is PTB1*SumUf+EFf0,
    EFt1 = EFt0
  ),
  compute_EF(TR,T,Y,U,Q_ch_Y_par,Q_t_y,Q_y,EFt1,EFf1,EFt,EFf).

find_U_N(U,N,UN):-
  rb_lookup(N,UN,U).
  
scan_UN([],_Q_ch_y,SumUt,SumUf,SumUt,SumUf).

scan_UN([ch(N,S)-U|UT],Q_ch_y,SumUt0,SumUf0,SumUt,SumUf):-
  def_rule_by_num(N,S1,_HL,BL),!,
  match_subs(S,S1),
  (member(T,BL)->
    vec_times(U,Q_ch_y,L),
    sum_list(L,Sum),
    SumUt1 is SumUt0+Sum,
    SumUf1 =SumUf0
  ;
    (member(\+ T,BL)->
      vec_times(U,Q_ch_y,L),
      sum_list(L,Sum),
      SumUf1 is SumUt0+Sum,
      SumUt1 =SumUt0
    ;
      SumUt1 =SumUt0,
      SumUf1 =SumUf0
    )
  ),
  scan_UN(UT,Q_ch_y,SumUt1,SumUf1,SumUt,SumUf).

scan_UN([ch(N,S)-U|UT],Q_ch_y,SumUt0,SumUf0,SumUt,SumUf):-
  rule_by_num(N,S1,_NH,_HL,BL),
  match_subs(S,S1),
  (member(T,BL)->
    vec_times(U,Q_ch_y,L),
    sum_list(L,Sum),
    SumUt1 is SumUt0+Sum,
    SumUf1 =SumUf0
  ;
    (member(\+ T,BL)->
      vec_times(U,Q_ch_y,L),
      sum_list(L,Sum),
      SumUf1 is SumUt0+Sum,
      SumUt1 =SumUt0
    ;
      SumUt1 =SumUt0,
      SumUf1 =SumUf0
    )
  ),
  scan_UN(UT,Q_ch_y,SumUt1,SumUf1,SumUt,SumUf).

compute_EPT([],T,Pa_T,Q_ch_Y_par,_Q_t_Y,_Y,Delta,EPt0,EPf0,EPt,EPf):-
  compute_prod_Pa_T(Pa_T,T,Q_ch_Y_par,1,Prod),
  EPt is EPt0+Prod*Delta,
  EPf is EPf0+(1-Prod)*Delta.

compute_EPT([ch(N,S)|TCH],T,Pa_T,Q_ch_Y_par,Q_t_Y,Y,Delta,EPt0,EPf0,EPt,EPf):-
  def_rule_by_num(N,S1,_HL,BL),!,
  match_subs(S,S1),
%  ((member(T,BL);member(\+ T,BL))->
    body_true_unseen(BL,_B,Y,BodyTrue),!,
    (BodyTrue=:=1->
      prob_body(BL,Y,1,PTB,_BT),
      (member(T,BL)->
        PTB_dif_T is PTB/Q_t_Y,
        EPf1 is Delta,
        EPt1 is 1-PTB_dif_T
      ;
        PTB_dif_T is PTB/(1-Q_t_Y),
        EPf1 is 1-PTB_dif_T,
        EPt1 is Delta
      )
    ;
      (member(T,BL)->
        EPf1 = 0,
        EPt1 is Delta
      ;
        EPf1 is EPf1+Delta,
        EPt1 is 0
      )
    )
%  ;
%    EPf1=0,
%    EPt1=0
%  )
  ,
  EPf3 is EPf0+EPf1,
  EPt3 is EPt0+EPt1,
  compute_EPT(TCH,T,Pa_T,Q_ch_Y_par,Q_t_Y,Y,Delta,EPt3,EPf3,EPt,EPf).


compute_EPT([ch(N,S)|TCH],T,Pa_T,Q_ch_Y_par,Q_t_Y,Y,Delta,EPt0,EPf0,EPt,EPf):-
  rule_by_num(N,S1,_NH,HL,BL),
  match_subs(S,S1),
%  ((member(T,BL);member(\+ T,BL))->
    body_true_unseen(BL,_B,Y,BodyTrue),!,
    (BodyTrue=:=1->
      prob_body(BL,Y,1,PTB,_BT),
      rb_lookup(ch(N,S),Q_ch_y,Q_ch_Y_par),
      array_element(p,N,d(Theta,_Val)),
      cycle_EPT_CH_val(Theta,HL,Q_ch_y,0,Sum,Q_ch_null_y),
      (member(T,BL)->
        PTB_dif_T is PTB/Q_t_Y,
        EPf1 is Delta*(1-Q_ch_null_y),
        EPt1 is PTB_dif_T*Sum+Delta*(Q_ch_null_y*PTB_dif_T+(1-Q_ch_null_y)*(1-PTB_dif_T))
      ;
        PTB_dif_T is PTB/(1-Q_t_Y),
        EPf1 is PTB_dif_T*Sum+Delta*(Q_ch_null_y*PTB_dif_T+(1-Q_ch_null_y)*(1-PTB_dif_T)),
        EPt1 is Delta*(1-Q_ch_null_y)
      )
    ;
      (member(T,BL)->
        EPf1 = 0,
        EPt1 is Delta*(1-Q_ch_null_y)
      ;
        EPf1 is EPf1+Delta*(1-Q_ch_null_y),
        EPt1 is 0
      )
    )
%  ;
%    EPf1=0,
%    EPt1=0
%  )
  ,
  EPf3 is EPf0+EPf1,
  EPt3 is EPt0+EPt1,
  compute_EPT(TCH,T,Pa_T,Q_ch_Y_par,Q_t_Y,Y,Delta,EPt3,EPf3,EPt,EPf).

compute_prod_Pa_T([],_T,_Q_ch_Y_par,Prod,Prod).

compute_prod_Pa_T([ch(N,_S)|TCH],T,Q_ch_Y_par,Prod0,Prod):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  compute_prod_Pa_T(TCH,T,Q_ch_Y_par,Prod0,Prod).

compute_prod_Pa_T([ch(N,S)|TCH],T,Q_ch_Y_par,Prod0,Prod):-
  rb_lookup(ch(N,S),Prob,Q_ch_Y_par),
  rule_by_num(N,S1,_NH,HL,_BL),
  match_subs(S,S1),
  generate_nth_pos(1, Pos, HL, (T:_P)),
  nth(Pos,Prob,P),
  Prod1 is Prod0*(1-P),
  compute_prod_Pa_T(TCH,T,Q_ch_Y_par,Prod1,Prod).
      
cycle_EPT_CH_val([],[],[],S,S,0).

cycle_EPT_CH_val([_],['':_P],[Q_ch_null_y],S,S,Q_ch_null_y):-!.

cycle_EPT_CH_val([HTheta|TTheta],[_H:_P|T],[Q_ch_yH|Q_ch_yT],S0,S,Q_ch_null_y):-
  S1 is S0+Q_ch_yH*log(HTheta),
  cycle_EPT_CH_val(TTheta,T,Q_ch_yT,S1,S,Q_ch_null_y).
  

/* given an annotated head as a list, return the list of atoms
*/
get_atoms_head([],[]):-!.

get_atoms_head(['':_P],['']):-!.

get_atoms_head([H:_P|T],[H1|T1]):-
  H=..[P,_|Rest],
  H1=..[P|Rest],
  get_atoms_head(T,T1).

/* computes R(i,ch,y)&=&
\sum_{j\in p(i),x_j[y]=true,ch_i\neq x_j[y]}\prod_{ch_s\in pa_{x_j},s\neq i}Q(ch_s\neq x_j[y]|y)
for all i and all y
*/
compute_R([],_CH,_NH,_Y,_PAX,_Q_Y,_Q_t_y,RTot,R0,R):-!,
  subt(R0,R,RTot).

compute_R([''],_CH,_NH,_Y,_PAX,_Q_Y,_Q_t_y,RTot,R0,R):-!,
  append(R0,[0],R1),
  subt(R1,R,RTot).

compute_R([Val|T],CH,NH,Y,PAX,Q_Y,Q_t_y,RTot0,R0,R):-
/*  rule_by_num(N,Sub,_NH,_HL,BL),
  match_subs(S,Sub),
  prob_body(BL,Y,1,PTSB,_B),*/
  Val=..[F|Arg],
  X=..[F,Y|Arg],
  X1=..[F,_Y|Arg],
  functor(Val,P,A),
  (unseen(P/A)->
    rb_lookup(Val,PAVal,PAX),
    delete(PAVal,CH,PAVal1),
    compute_R_x(PAVal1,X1,Q_Y,1,Prod0),
    rb_lookup(Val,Pr_t,Q_t_y),
    Prod is Prod0*Pr_t,
    RTot2 is RTot0+Prod,
    append(R0,[Prod],R1)  
  ;
    (call(X)->
      rb_lookup(Val,PAVal,PAX),
      delete(PAVal,CH,PAVal1),
      compute_R_x(PAVal1,X1,Q_Y,1,Prod),
      RTot2 is RTot0+Prod,
      append(R0,[Prod],R1)
    ;
      RTot2=RTot0,
      append(R0,[0],R1)
    )
  ),
  NH1 is NH+1,
  compute_R(T,CH,NH1,Y,PAX,Q_Y,Q_t_y,RTot2,R1,R).

/* subt(L,L1,RT) subts from RT all the elements of list L obtaining L1 
*/
subt([],[],_RT):-!.

subt([H|T],[H1|T1],RT):-
  H1 is RT-H,
  subt(T,T1,RT).

/* computes R(i,ch,y)&=&
\sum_{ch_i\neq x_j[y]}\prod_{ch_s\in pa_{x_j},s\neq i}Q(ch_s\neq x_j[y]|y)
for a single value x_j
*/
compute_R_x([],_X,_Q_Y,Prod,Prod):-!.

compute_R_x([ch(N,S)|T],X,Q_Y,Prod0,Prod1):-
  rb_lookup(ch(N,S),Prob,Q_Y),
  rule_by_num(N,S,_NH,HL,_BL),
  generate_nth_pos(1, Pos, HL, (X:_P)),
%  nth(Pos,HL,(X:_P)),!,
  nth(Pos,Prob,P),
  Prod2 is Prod0*(1-P),
  compute_R_x(T,X,Q_Y,Prod2,Prod1).

/*  body_true(Body,Ex,BT) sets BT to 1 if Body is true in Ex and to 0
otherwise
Body is a list of literals, each with the example argument 
*/
body_true([],_Ex,1):-!.

body_true([H|T],Ex,BT):-
  (inference_ib:builtin(H)->
    body_true(T,[H],Ex,BT)
  ;
    H=..[_P,Ex|_A],
    (test([H|T])->
      BT=1
    ;
      BT=0
    )
  ).

body_true([H|T],In,Ex,BT):-
  (inference_ib:builtin(H)->
    append(In,[H],In1),
    body_true(T,In1,Ex,BT)
  ;
    H=..[_P,Ex|_A],
    append(In,[H|T],B),
    (test(B)->
      BT=1
    ;
      BT=0
    )
  ).

body_true_unseen([],[],_Ex,1):-!.

body_true_unseen([\+ H|T],B,Ex,BT):-!,
  (inference_ib:builtin(H)->
    body_true_unseen(T,[\+ H],B,Ex,BT)
  ;
    H=..[_P,Ex|_A],
    (test_unseen([\+ H|T],B)->
      BT=1
    ;
      BT=0
    )
  ).
  
body_true_unseen([H|T],B,Ex,BT):-
  (inference_ib:builtin(H)->
    body_true_unseen(T,[ H],B,Ex,BT)
  ;
    H=..[_P,Ex|_A],
    (test_unseen([H|T],B)->
      BT=1
    ;
      BT=0
    )
  ).

body_true_unseen([\+ H|T],In,B,Ex,BT):-!,
  (inference_ib:builtin(H)->
    append(In,[\+ H],In1),
    body_true_unseen(T,In1,B,Ex,BT)
  ;
    H=..[_P,Ex|_A],
    append(In,[\+ H|T],BB),
    (test_unseen(BB,B)->
      BT=1
    ;
      BT=0
    )
  ).
  
body_true_unseen([H|T],In,B,Ex,BT):-
  (inference_ib:builtin(H)->
    append(In,[H],In1),
    body_true_unseen(T,In1,B,Ex,BT)
  ;
    H=..[_P,Ex|_A],
    append(In,[H|T],BB),
    (test_unseen(BB,B)->
      BT=1
    ;
      BT=0
    )
  ).
  
/* test(L) succeeds if L is true in the database
*/  
test([]).

test([H|T]):-
  call(H),
  test(T).      

test_unseen([],[]).

test_unseen([\+ H|Tail],B):-!,
  functor(H,P,A0),
  A is A0-1,
  (unseen(P/A)->
  /*  my_get_value(t,T),
    H=..[P,_|Args],
    H1=..[P|Args],
    member(H1,T),*/
    B=[\+ H|B1]
  ;
    call(\+ H),
    B=B1
  ),
  test_unseen(Tail,B1).      


test_unseen([H|Tail],B):-
  functor(H,P,A0),
  A is A0-1,
  (unseen(P/A)->
    my_get_value(t,T),
    H=..[P,_|Args],
    H1=..[P|Args],
    member(H1,T),
    B=[H|B1]
  ;
    call(H),
    B=B1
  ),
  test_unseen(Tail,B1).      

/* computes E_{Q(CH|ch_{i},y_0)}[\mathcal{D}(y,ch_{t(i,y)},ch_i)]
*/
compute_ED([],_S,_Q_y_par,ED,ED):-!.

compute_ED([f(Values,D)|TD],S,Q_y_par,ED0,ED1):-
  delete_matching(Values,(ch(_N,S)=_Val),Values1),!,
  single_rules_ed_contrib(D,Term0),
  compute_term(Values1,Q_y_par,Term0,Term),
  sum(ED0,Term,ED2),
  compute_ED(TD,S,Q_y_par,ED2,ED1).

/*
delete_matching([],_El,[]):-!.

delete_matching([El|T],El,TR):-!,
  delete_matching(T,El,TR).

delete_matching([H|T],El,[H|TR]):-!,
  delete_matching(T,El,TR).
*/


/* computes Q(ch_j|y)\mathcal{D}(y,ch_{t(i,y)},ch_i) for all values ch_j of 
Ch_j
*/
compute_term([],_Q_ch_y_par,Term,Term):-!.

compute_term([ch(N,S)=Val|TVal],Q_ch_y_par,Term0,Term1):-
  rb_lookup(ch(N,S),Q_ch_y,Q_ch_y_par),
  nth(Val,Q_ch_y,Q_ch_y_val),
  times(Q_ch_y_val,Term0,Term2),
  compute_term(TVal,Q_ch_y_par,Term2,Term1).

single_rules_ed_contrib([],[]).

single_rules_ed_contrib([H|T],[SR|TSR]):-
  single_rules_ed_contrib_ch_val(H,0,SR),
  single_rules_ed_contrib(T,TSR).
  
single_rules_ed_contrib_ch_val([],SR,SR).

single_rules_ed_contrib_ch_val([(_B,QT,D)|T],SR0,SR):-
  SR1 is SR0+QT*D,
  single_rules_ed_contrib_ch_val(T,SR1,SR).

/* sum(L1,L2,L) sums lists L1 and L2 element by element
*/
sum([],[],[]):-!.  

sum([H0|T0],[H1|T1],[H2|T2]):-
  H2 is H0+H1,
  sum(T0,T1,T2).  
  
/* vec_times(L1,L2,L) multiplies lists L1 and L2 element by element
*/
vec_times([],[],[]):-!.  

vec_times([H0|T0],[H1|T1],[H2|T2]):-
  H2 is H0*H1,
  vec_times(T0,T1,T2).  

/* computes 
EP'(ch_i,y)
&=&\\
&&\log \theta_{Hd_{r(k)}=ch_i|pa_{ch_k}}[y]1\{body(pa_{ch_i})=true\}+\\
&&\delta(\{body(pa_{ch_i})=false,ch_i\neq null\}+\\
&&R(i,ch,y) +\\
&&1\{ch_i\neq null,val(ch_i)[y]=false\})
*/
compute_EP([],[],[],_BodyTrue,_PTB,_Q_t_Y_par,_Ex,[],[],_Delta,EEP,EEP):-!.

compute_EP([''],[Prob_ch_y],[Theta],BodyTrue,PTB,_Q_t_Y_par,_Ex,[EP],[R],Delta,EEP0,EEP1):-!,
  EP is log(Theta)*BodyTrue*PTB+Delta*R,
  EEP1 is EEP0+EP*Prob_ch_y.

compute_EP([Val|TVal],[Prob_ch_y|TP],[Theta|TTheta],BodyTrue,PTB,Q_t_Y_par,Ex,[EP|TEP],[R|TR],Delta,EEP0,EEP1):-
  Val=..[F|Args],
  X=..[F,Ex|Args],
  functor(Val,Pred,Arity),
  (unseen(Pred/Arity)->
    rb_lookup(Val,XPFalse,Q_t_Y_par),
    XFalse is 1- XPFalse
  ;
    (call(X)->
      XFalse=0
    ;
      XFalse=1
    )
  ),
  EP is log(Theta)*BodyTrue*PTB+ Delta*((1-BodyTrue)+BodyTrue*(1-PTB)+R+XFalse),
  EEP2 is EEP0+EP*Prob_ch_y,
  compute_EP(TVal,TP,TTheta,BodyTrue,PTB,Q_t_Y_par,Ex,TEP,TR,Delta,EEP2,EEP1).

/* computes E_{Q(ch_i|y)}[\log Q(ch_i)]&=&\sum_{ch_i}Q(ch_i|y)\log Q(ch_i)
*/  
compute_E_log_Q_ch([],[],E,E):-!.

compute_E_log_Q_ch([HProb_ch_y|T],[Prob_ch|T1],E0,E1):-
  E2 is E0+HProb_ch_y*log(Prob_ch),
  compute_E_log_Q_ch(T,T1,E2,E1).

/* builds the initial Q table, with one entry for each example and each choice
variable 
q is an array with the example number as index
*/
build_q_table(CH):-
    ex(NEx),
  add_Q_par(0,NEx,CH).

/* cycles over the examples for building the Q table
*/
add_Q_par(N,N,_CH):-!.  

add_Q_par(N,NEx,CH):-
  rb_new(Q_Y0),
  add_Q_Y_par(CH,Q_Y0,Q_Y1),
  update_array(q,N,Q_Y1),
  N1 is N+1,
  add_Q_par(N1,NEx,CH).

/* cycles over the choice variables and builds Q(ch|y) a rb_tree with ch(N,S) as key
and the distribution in the form of a list of probabilities as value
*/
add_Q_Y_par([],Q,Q).  

add_Q_Y_par([ch(N,S)|T],Q0,Q1):-
  def_rule_by_num(N,_V,_HL,_BL),!,
  rb_insert(Q0,ch(N,S),[1.0],Q2),
  add_Q_Y_par(T,Q2,Q1).

add_Q_Y_par([ch(N,S)|T],Q0,Q1):-
  rule_by_num(N,_V,_NH,HL,_BL),
  length(HL,NHL),
  Prob is 1/NHL,
  PertSize is Prob*0.1,
  gen_random_param(NHL,0,PertSize,Prob,Par),
  rb_insert(Q0,ch(N,S),Par,Q2),
  add_Q_Y_par(T,Q2,Q1).
  
/* generates an initial random distribution for Q(ch|y)
the distribution is obtained by randomly perturbing an uniform distribution
*/
gen_random_param(0,_Sum,_PertSize,_P,[]):-!.

gen_random_param(N,Sum,PertSize,P,[P1|Par]):-
  TotMass is 1-Sum,
  Mass is TotMass/N,
  LowerBound is -PertSize,
  random(LowerBound,PertSize,Pert),
  P2 is Mass+Pert,
  N1 is N-1,
  Sum2 is Sum+P2,
  (Sum2>1.0->
    P1 = Mass,
    Sum1 is Sum+Mass
  ; 
    P1 = P2,
    Sum1 = Sum2    
  ),
  gen_random_param(N1,Sum1,PertSize,P,Par).

/* builds the initial Q(t|y) table, with one entry for each example and each T
variable 
q is an array with the example number as index
*/
build_qt_table(T):-
    ex(NEx),
  add_Qt_par(0,NEx,T).

/* cycles over the examples for building the Q(t|y) table
*/
add_Qt_par(N,N,_T):-!.  

add_Qt_par(N,NEx,T):-
  rb_new(Q_Y0),
  add_Qt_Y_par(T,Q_Y0,Q_Y1),
  update_array(qt,N,Q_Y1),
  N1 is N+1,
  add_Qt_par(N1,NEx,T).

/* cycles over the T variables and builds Q(t|y) a rb_tree with t as key
and the distribution in the form of a probability
*/
add_Qt_Y_par([],Q,Q).  

add_Qt_Y_par([H|T],Q0,Q1):-
  random(Par),
  rb_insert(Q0,H,Par,Q2),
  add_Qt_Y_par(T,Q2,Q1).
  

/* builds the p table with one entry for each example and each choice
variable 
p is an array with the rule number as index
*/
build_p_table(Rules):-
  setting(max_rules,MR),
%  length(Rules,N),
  array(p,MR),
  add_P_par(Rules).

/* retrieves the initial values of the theta vectore and stores it in the
p array
*/  
add_P_par([]).  

add_P_par([def_rule(_N,_S,_H,_BL)|T]):-!,
  add_P_par(T).

add_P_par([rule(N,_V,_NH,HL,_BL,_LogF)|T]):-
  find_atoms_head(HL,Atoms,Probs),
  update_array(p,N,d(Probs,Atoms)),
  add_P_par(T).

/* builds the Bayesian network equivalent to the current model
*/  
build_network_IB(CH,PAX,T,TCh,R,LogSize):-
  ex(NEx),
  get_ouptut_atoms(O),
  %rb_new(C0),
  generate_ground1(0,NEx,O,[],L),
  build_ground_lpad(L,CL),
%  write(find_ground_atoms),nl,
%  flush_output,
  find_ground_atoms(CL,[],GAD0,[],GUAD0),
  generate_goal_DB(0,NEx,O,[],GL),
  get_atoms(GL,Atoms1),
  append(GAD0,Atoms1,GAD),
  remove_duplicates(GAD,X),
%  write('X'),write(X),nl,
  remove_duplicates(GUAD0,T),
%  write('T'),write(T),nl,
  rb_new(R0),
  choice_vars_IB(CL,CH,0,LogSize0,R0,R1),
  length(T,LST),
  LogSize is LogSize0+LST,
  rb_visit(R1,R),
%  write('R'),write(R),nl,
  rb_new(PAX0),
  get_X_parents(CL,PAX0,PAX1),
  add_parentless_X(X,PAX1,PAX),
  rb_new(TCh0),
  get_T_children(CL,TCh0,TCh1),
  add_childless_T(T,TCh1,TCh).
  
find_ground_atoms([],GA,GA,GUA,GUA):-!.

find_ground_atoms([d(_N,_S,H,Body)|T],GA0,GA,GUA0,GUA):-!,
  %write(fga),
        H=..[F,_|R],
  H1=..[F|R],
  functor(H1,P,A),
  (input_cw(P/A)->
    find_atoms_body(T,GA0,GA,GUA0,GUA)
  ;        
    (unseen(P/A)->
                  find_atoms_body(T,GA0,GA1,[H1|GUA0],GUA1)
          ;
                  find_atoms_body(T,[H1|GA0],GA1,GUA0,GUA1)
          )
  ),
  find_atoms_body(Body,GA1,GA2,GUA1,GUA2),
  find_ground_atoms(T,GA2,GA,GUA2,GUA).

find_ground_atoms([(_N,_S,Head,Body)|T],GA0,GA,GUA0,GUA):-
  %write(fga),
  find_atoms_head(Head,GA0,GA1,GUA0,GUA1),
  find_atoms_body(Body,GA1,GA2,GUA1,GUA2),
  find_ground_atoms(T,GA2,GA,GUA2,GUA).

find_atoms_body([],GA,GA,GUA,GUA):-!.

find_atoms_body([\+H|T],GA0,GA,GUA0,GUA):-
  inference_ib:builtin(H),!,
  find_atoms_body(T,GA0,GA,GUA0,GUA).

find_atoms_body([H|T],GA0,GA,GUA0,GUA):-
  inference_ib:builtin(H),!,
  find_atoms_body(T,GA0,GA,GUA0,GUA).


find_atoms_body([\+H|T],GA0,GA,GUA0,GUA):-!,
  H=..[F,_|R],
  H1=..[F|R],
  functor(H1,P,A),
  (unseen(P/A)->
    find_atoms_body(T,GA0,GA,[H1|GUA0],GUA)
  ;
    find_atoms_body(T,[H1|GA0],GA,GUA0,GUA)
  ).

find_atoms_body([H|T],GA0,GA,GUA0,GUA):-!,
  H=..[F,_|R],
  H1=..[F|R],
  functor(H1,P,A),
  (unseen(P/A)->
    find_atoms_body(T,GA0,GA,[H1|GUA0],GUA)
  ;
    find_atoms_body(T,[H1|GA0],GA,GUA0,GUA)
  ).

find_atoms_head([],GA,GA,GUA,GUA).

find_atoms_head(['':_P],GA,GA,GUA,GUA):-!.

find_atoms_head([H:_P|T],GA0,GA,GUA0,GUA):-
  H=..[F,_|R],
  H1=..[F|R],
  functor(H1,P,A),
  (unseen(P/A)->
    find_atoms_head(T,GA0,GA,[H1|GUA0],GUA)
  ;
    find_atoms_head(T,[H1|GA0],GA,GUA0,GUA)
  ).

get_T_children([],TCh,TCh):-!.

get_T_children([d(N,S,_Head,Body)|T],TCh0,TCh):-!,
  scan_body_for_T(Body,N,S,TCh0,TCh1),
  get_T_children(T,TCh1,TCh).

get_T_children([(N,S,_Head,Body)|T],TCh0,TCh):-
  scan_body_for_T(Body,N,S,TCh0,TCh1),
  get_T_children(T,TCh1,TCh).

/* collects the atoms in the head of the rule corresponding to ch(M,S)
*/
scan_body_for_T([],_N,_S,TCh,TCh):-!.

scan_body_for_T([\+ H|T],N,S,TCh0,TCh):-!,
  H=..[F,_|R],
  H1=..[F|R],
  functor(H1,P,A),
  (unseen(P/A)->
    (rb_lookup(H,Ch,TCh0)->
      rb_update(TCh0,H,[ch(N,S)|Ch],TCh1)
    ;
      rb_insert(TCh0,H,[ch(N,S)],TCh1)
    )
  ;
    TCh1=TCh0
  ),
  scan_body_for_T(T,N,S,TCh1,TCh).

scan_body_for_T([H|T],N,S,TCh0,TCh):-  
  H=..[F,_|R],
  H1=..[F|R],
  (rb_lookup(H1,Ch,TCh0)->
    rb_update(TCh0,H1,[ch(N,S)|Ch],TCh1)
  ;
    rb_insert(TCh0,H1,[ch(N,S)],TCh1)
  ),
  scan_body_for_T(T,N,S,TCh1,TCh).
  
get_X_parents([],_CL,PAX,PAX):-!.

get_X_parents([X|TX],CL,PAX0,PAX1):-
  X=..[F|Args],
  X1=..[F,_Y|Args],
  findall(ch(N,S),
  (
    member((N,S,Head,_Body),CL),member((X1:_P),Head)
    ;
    member(d(N,S,X1,_Body),CL)
  ),
    PAX),
  rb_insert(PAX0,X,PAX,PAX2),
  get_X_parents(TX,CL,PAX2,PAX1).

add_parentless_X([],PAX,PAX).

add_parentless_X([X|T],PAX0,PAX1):-
  (rb_lookup(X,_PA,PAX0)->
    PAX2=PAX0
  ;
    rb_insert(PAX0,X,[],PAX2)
  ),
  add_parentless_X(T,PAX2,PAX1).

add_childless_T([],TCh,TCh).

add_childless_T([T|TT],TCh0,TCh):-
  (rb_lookup(T,_Children,TCh0)->
    TCh1=TCh0
  ;
    rb_insert(TCh0,T,[],TCh1)
  ),
  add_childless_T(TT,TCh1,TCh).

/* for each ground atoms, it finds the choice variables that are its parents
for each grounding of a clause, it collects the atoms in the head
*/
get_X_parents([],PAX,PAX):-!.

get_X_parents([d(N,S,Head,_Body)|T],PAX0,PAX1):-
  scan_head([Head:1.0],N,S,PAX0,PAX2),
  get_X_parents(T,PAX2,PAX1).


get_X_parents([(N,S,Head,_Body)|T],PAX0,PAX1):-
  scan_head(Head,N,S,PAX0,PAX2),
  get_X_parents(T,PAX2,PAX1).

/* collects the atoms in the head of the rule corresponding to ch(M,S)
*/
scan_head([],_N,_S,PAX,PAX).

scan_head(['':_P],_N,_S,PAX,PAX):-!.

scan_head([H:_P|T],N,S,PAX0,PAX1):-  
  H=..[F,_Y|Args],
  H1=..[F|Args],
  (rb_lookup(H1,Par,PAX0)->
    rb_update(PAX0,H1,[ch(N,S)|Par],PAX2)
  ;
    rb_insert(PAX0,H1,[ch(N,S)],PAX2)
  ),
  scan_head(T,N,S,PAX2,PAX1).

/* generates the grounding of a model 
*/
generate_ground(L1,GA):-
  find_modes(Modes),
  find_ground_atoms_modes(Modes,[],GA),
  findall(rule(N,V,HL,BL),rule_by_num(N,V,_NH,HL,BL),LR),
  ground_rules(LR,[],L1).
/*
generate_ground(L1,GA):-
  find_modes(Modes),
%  get_types(Modes,[],Types),
  rb_new(Types0),
  get_constants_types(Modes,Types0,Types1),
  rb_visit(Types1,TypesL),
  asser_all_const(TypesL),
  find_ground_atoms_modes(Modes,[],GA),
  format("Atoms ~p~n",[GA]),
  findall(rule(N,V,HL,BL),rule_by_num(N,V,_NH,HL,BL),LR),
  ground_rules(LR,[],L1).
*/

generate_ground1(N,N,_O,L0,L):-!,
  remove_duplicates(L0,L).

generate_ground1(N0,N,O,L0,L1):-
  %format("Ex ~d~n",[N0]),
  %(N0=320->bp;true),
  generate_goal1(O,N0,[],GL),
  generate_ground1_goals(GL,[],Exp0),
  append(Exp0,Exp1),
  %length(Exp1,Len),write(Len),nl,
  remove_head(Exp1,Exp),
  append(Exp,L0,L2),
%  add_to_tree(Exp,L0,L2),
  %write(add_to_tree),
%  flush_output,
  N1 is N0+1,
  generate_ground1(N1,N,O,L2,L1).

generate_ground1_goals([],Exp,Exp).

generate_ground1_goals([H|T],Exp0,Exp):-
  setting(depth_bound,DB),
  findall(Deriv,inference_ib:find_deriv_inf1([H],DB,Deriv),Exp1),
  append(Exp0,Exp1,Exp2),
  generate_ground1_goals(T,Exp2,Exp).

add_to_tree([],L,L).

add_to_tree([(R,S)|T],L0,L):-
  %write(t),
  (rb_lookup((R,S),_,L0)->
    L1=L0
  ;
    rb_insert(L0,(R,S),true,L1)
  ),
  add_to_tree(T,L1,L).

generate_ground2(N,N,_O,L,L):-%write(fine),nl,
  !.

generate_ground2(N0,N,O,L0,L1):-
  generate_goal(O,N0,[],GL),
  %format("Ex ~d~n",[N0]),
  setting(depth_bound,DB),
  findall(Deriv,inference_ib:find_deriv_inf1(GL,DB,Deriv),Exp0),
  append(Exp0,Exp1),
  remove_head(Exp1,Exp),
  add_to_list(Exp,L0,L2),
  N1 is N0+1,
  generate_ground2(N1,N,O,L2,L1).

add_to_list([],L,L).

add_to_list([(R,S)|T],L0,L):-
  (member(((R,S)-_),L0)->
    L1=L0
  ;
    L1=[((R,S)-true)|L0]
  ),
  add_to_list(T,L1,L).



get_constants_types([],Types,Types).

get_constants_types([H|T],Types0,Types1):-
  H=..[F|Args],
  length(Args,N),
  length(Args1,N),  
  H1=..[F,Mod|Args1],
  get_const_atom(Args,Args1,Mod,Args1,H1,Types0,Types2),
  get_constants_types(T,Types2,Types1).

get_const_atom([],[],_Mod,_Args1,_H,Types,Types).
  
get_const_atom([+Type|TT],[V|TV],Mod,Args1,H,Types0,Types1):-
  delete(Args1,V,Vars),
  setof(V,(Vars,Mod)^H,L),
  insert_const(L,Type,Types0,Types2),
  get_const_atom(TT,TV,Mod,Args1,H,Types2,Types1).
  
get_const_atom([-Type|TT],[V|TV],Mod,Args1,H,Types0,Types1):-
  delete(Args1,V,Vars),
  setof(V,(Vars,Mod)^H,L),
  insert_const(L,Type,Types0,Types2),
  get_const_atom(TT,TV,Mod,Args1,H,Types2,Types1).

insert_const(L,Type,Types0,Types1):-
  (rb_lookup(Type,LA,Types0)->
    append(L,LA,LP),
    remove_duplicates(LP,LPP),
    rb_update(Types0,Type,LPP,Types1)
  ;
    rb_insert(Types0,Type,L,Types1)
  ).

asser_all_const([]).

asser_all_const([Type-Const|T]):-
  format("Type ~a const ~p~n",[Type,Const]),
  assert_const(Const,Type),
  asser_all_const(T).
    
assert_const([],_Type).
  
assert_const([H|T],Type):-
  At=..[Type,H],
  assertz(At),
  assert_const(T,Type).
  
  
/* generates the grounding of a set of rules 
*/
ground_rules([],L,L):-!.

ground_rules([rule(N,V,HL,BL)|T],L0,L1):-
  HL=[A:_P|_T],
  A=..[_F,Y|_Args],
  remove_module_head(HL,HL1),
  remove_module(BL,BL1),
  setof((N,V,HL1,BL1),Y^(ground_head(HL),ground_body(BL)),L),!,
  append(L0,L,L2),
  ground_rules(T,L2,L1).

ground_rules([_H|T],L0,L1):-
  ground_rules(T,L0,L1).

/* removes the module (example) argument from the atoms of a head
*/
remove_module_head(['':P],['':P]):-!.

remove_module_head([H:P|T],[H1:P|T1]):-
  H=..[F,_Y|Args],
  H1=..[F|Args],
  remove_module_head(T,T1).

/* removes the module (example) argument from the atoms of a body
*/
remove_module([],[]):-!.

remove_module([\+H|T],[\+H1|T1]):-!,
  H=..[P,_Y|Args],
  H1=..[P|Args],
  remove_module(T,T1).

remove_module([H|T],[H1|T1]):-
  H=..[P,_Y|Args],
  H1=..[P|Args],
  remove_module(T,T1).

/* adds the module (example) argument from the atoms of a body
the value of the argument is Y
*/
add_module([],_Y,[]):-!.

add_module([\+H|T],Y,[\+H1|T1]):-!,
  H=..[P|Args],
  H1=..[P,Y|Args],
  add_module(T,Y,T1).

add_module([H|T],Y,[H1|T1]):-
  H=..[P|Args],
  H1=..[P,Y|Args],
  add_module(T,Y,T1).

/* instantiates a head
*/
/* instantiates a head
*/
ground_head(['':_P]):-!.

ground_head([H:_P|T]):-
  \+ \+ H,!,
  call(H),
  ground_head(T).

ground_head([H:_P|T]):-
  call(neg(H)),
  ground_head(T).
  

/* instantiates a body
*/
ground_body([]):-!.

ground_body([\+H|T]):-!,
  \+call(H),
  ground_body(T).

ground_body([H|T]):-
  call(H),
  ground_body(T).
/*
ground_head(['':_P]):-!.

ground_head([H:_P|T]):-
  H=..[F|Args],
  length(Args,N),
  length(Args1,N),
  H1=..[F|Args1],
  modeh(_,H1),
  instantiate_args(Args1,Args),
  ground_head(T).

instantiate_args([],[]).

instantiate_args([+Type|T],[V|T1]):-!,
  A=..[Type,V],
  call(A),
  instantiate_args(T,T1).
  
instantiate_args([-Type|T],[V|T1]):-
  A=..[Type,V],
  call(A),
  instantiate_args(T,T1).
*/

/* instantiates a body
*/
/*
ground_body([]):-!.

ground_body([\+H|T]):-!,
  H=..[F|Args],
  length(Args,N),
  length(Args1,N),
  H1=..[F|Args1],
  modeb(_,H1),
  instantiate_args(Args1,Args),
  ground_body(T).

ground_body([H|T]):-
  H=..[F|Args],
  length(Args,N),
  length(Args1,N),
  H1=..[F|Args1],
  modeb(_,H1),
  instantiate_args(Args1,Args),
  ground_body(T).
*/
/* returns the set of modes specified in the language bias
*/
find_modes(Modes):-
  findall(Pred,modeh(_,Pred),L0),
  findall(Pred,modeb(_,Pred),L1),
  findall(Pred,mode(_,Pred),L2),
  append(L0,L1,L3),
  append(L3,L2,Modes).

get_types([],L,L).

get_types([H|T],L0,L1):-
  H=..[_F|Args],
  remove_io(Args,L0,L2),
  get_types(T,L2,L1).

remove_io([],L,L).

remove_io([+H|T],L0,L1):-
  (member(H,L0)->
    L2=L0
  ;
    append(L0,[H],L2)
  ),
  remove_io(T,L2,L1).

remove_io([-H|T],L0,L1):-
  (member(H,L0)->
    L2=L0
  ;
    append(L0,[H],L2)
  ),
  remove_io(T,L2,L1).

/* find all the ground atoms for each mode 
*/
find_ground_atoms_modes([],GA,GA):-!.
    
find_ground_atoms_modes([A|T],GA0,GA1):-
  A=..[P|Args],
  length(Args,L),
  length(Args1,L),
  A1=..[P,Y|Args1],
  A2=..[P|Args1],
  (setof(A2,Y^A1,LInst)->
  /*  (setof(A2,Y^neg(A1),LInstN)->
      append(LInst,LInstN,LI)
    ;
      LI=LInst
    )*/
    LI=LInst
  ;
    LI=[]
  ),
  remove_duplicates(LI,LI1),
  append(GA0,LI1,GA2),
  assert_all(LI1),
  find_ground_atoms_modes(T,GA2,GA1).

/*
find_ground_atoms_modes([],GA,GA):-!.
    
find_ground_atoms_modes([A|T],GA0,GA1):-
  A=..[P|Args],
  length(Args,L),
  length(Args1,L),
  A2=..[P|Args1],
  findall(A2,instantiate_args(Args,Args1),LInst),
  append(GA0,LInst,GA2),
%  assert_all(LInst),
  find_ground_atoms_modes(T,GA2,GA1).
*/
/* assert_all(L) asserts all the atoms of list L
*/
assert_all([]):-!.

assert_all([H|T]):-
  assert(H),
  assert_all(T).
  
/* choice_vars_IB(CL,CH,0,LogSize,R0,R1) given the grounding CL of an LPAD 
returns the list of choice variables, the log size and an rb tree of rules R1,
where each rule id is associated to the set of the substitution that instantiate
it
*/
choice_vars_IB([],[],LogSize,LogSize,R,R):-!.

choice_vars_IB([d(N,S,_H,_B)|T],CH,LogSize0,LogSize1,R0,R1):-
  (rb_lookup(N,L,R0)->
    (member(S,L)->
      CH=CH1,
      R2=R0      
    ;
      rb_update(R0,N,[S|L],R2),
      CH=[ch(N,S)|CH1]
    )
  ;
    rb_insert(R0,N,[S],R2),
    CH=[ch(N,S)|CH1]
  ),
  choice_vars_IB(T,CH1,LogSize0,LogSize1,R2,R1).


choice_vars_IB([(N,S,H,_B)|T],CH,LogSize0,LogSize1,R0,R1):-
  (rb_lookup(N,L,R0)->
    (member(S,L)->
      LogSize2 = LogSize0,
      CH=CH1,
      R2=R0      
    ;
      rb_update(R0,N,[S|L],R2),
      length(H,SizeCH),
      LogSize2 is LogSize0+log(SizeCH),
      CH=[ch(N,S)|CH1]
    )
  ;
    rb_insert(R0,N,[S],R2),
    length(H,SizeCH),
    LogSize2 is LogSize0+log(SizeCH),
    CH=[ch(N,S)|CH1]
  ),
  choice_vars_IB(T,CH1,LogSize2,LogSize1,R2,R1).


generate_goal_DB(N,N,_O,GL,GL):-!.

generate_goal_DB(N0,N,O,G0,G1):-
  generate_goal(O,N0,G0,G2),
  N1 is N0+1,
  generate_goal_DB(N1,N,O,G2,G1).

/* unused
random_restarts(1,Model,SS,CLL,Model,SS,CLL,_DB):-!.

random_restarts(N,Model0,SS0,CLL0,Model1,SS1,CLL1,DB):-
  setting(verbosity,Ver),
  (Ver>4->
    setting(random_restarts_number,NMax),
    Num is NMax-N+1,
    format("Restart number ~d~n",[Num])
  ;
    true
  ),
  randomize(Model0,ModelR),
  em_iteration(ModelR,ModelR1,SSR,CLLR,DB),
  (Ver>4->
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
*/
randomize([],[]):-!.

randomize([rule(N,V,NH,HL,BL,LogF)|T],[rule(N,V,NH,HL1,BL,LogF)|T1]):-!,
  length(HL,L),
  Int is 1.0/L,
  randomize_head(Int,HL,0,HL1),
  randomize(T,T1).

randomize([H|T],[H|T1]):-
  randomize(T,T1).


randomize_head(_Int,['':_],P,['':PNull1]):-!,
  PNull is 1.0-P,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).

randomize_head(_Int,[H:_],P,[H:PN]):-!,
  PN is 1.0-P.

randomize_head(Int,[H:_|T],P,[H:PH1|NT]):-
  PMax is 1.0-P,
  random(0,PMax,PH1),
  P1 is P+PH1,  
  randomize_head(Int,T,P1,NT).


/* asserts the model in the database
*/
assert_model([]):-!.

assert_model([def_rule(N,S,H,BL)|T]):-
  assertz(def_rule(H,BL,N,S)),
  assertz(def_rule_by_num(N,S,H,BL)),
  assert_model(T).

assert_model([rule(N,V,NH,HL,BL,_LogF)|T]):-
  assert_rules(HL,0,HL,BL,NH,N,V),
  assertz(rule_by_num(N,V,NH,HL,BL)),
  assert_model(T).

/* retracts the model from the database
*/
retract_model:-!,
  retractall(rule_by_num(_,_,_,_,_)),
  retractall(rule(_,_,_,_,_,_,_,_)).

/* returns the list of predicates specification for output atoms
*/
get_ouptut_atoms(O):-
  findall((A/Ar),output((A/Ar)),O).

/* generates the list of goals that must be asked for computing the CLL
and the grounding of an LPAD
*/
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
  
generate_goal1([],_H,G,G):-!.

generate_goal1([P/A|T],H,G0,G1):-
  functor(Pred,P,A),
  Pred=..[P|Rest],
  Pred1=..[P,H|Rest],
  findall(Pred1,call(Pred1),L),
  findall(Pred1,call(neg(Pred1)),LN),
  append(G0,L,G2),
  append(G2,LN,G3),
  generate_goal1(T,H,G3,G1).
  
  

sum(_NS,[],[],[]):-!.  

sum(NS,[H0|T0],[H1|T1],[H2|T2]):-
  H2 is H0+H1*NS,
  sum(NS,T0,T1,T2).  
  
times(_NS,[],[]):-!.  

times(NS,[H0|T0],[H1|T1]):-
  H1 is H0*NS,
  times(NS,T0,T1).  

divide(_NS,[],[]):-!.  

divide(NS,[H0|T0],[H1|T1]):-
  H1 is H0/NS,
  divide(NS,T0,T1).  

/* End of computation of log likelihood and sufficient stats */

/* Utility predicates */
set(Parameter,Value):-
  retract(setting(Parameter,_)),
  assert(setting(Parameter,Value)).

generate_file_names(File,FileKB,FileOut,FileL,FileLPAD,FileBG):-
  generate_file_name(File,".kb",FileKB),
  generate_file_name(File,".rules",FileOut),
  generate_file_name(File,".cpl",FileLPAD),
  generate_file_name(File,".l",FileL),
  generate_file_name(File,".bg",FileBG).        

generate_file_name(File,Ext,FileExt):-
  name(File,FileString),
  append(FileString,Ext,FileStringExt),
  name(FileExt,FileStringExt).
    
load_initial_model(File,Model):-
  open(File,read,S),
  read_clauses1(S,C),
  close(S),
  process_clauses(C,0,_N,[],Model).

process_clauses([(end_of_file,[])],N,N,Model,Model).

process_clauses([((H:-B),V)|T],N,N2,Model0,[rule(N,V,NH,HL,BL,0)|Model1]):-
  H=(_;_),!,
  list2or(HL1,H),
  process_head(HL1,HL,VI),
  list2and(BL0,B),
  add_int_atom(BL0,BL,VI),
  length(HL,LH),
  listN(0,LH,NH),
  N1 is N+1,
%  assertz(rule(N,V,NH,HL,BL)),
  process_clauses(T,N1,N2,Model0,Model1).

process_clauses([((H:-B),V)|T],N,N2,Model0,[rule(N,V,NH,HL,BL,0)|Model1]):-
  H=(_:_),!,
  list2or(HL1,H),
  process_head(HL1,HL,VI),
  list2and(BL0,B),
  add_int_atom(BL0,BL,VI),
  length(HL,LH),
  listN(0,LH,NH),
  N1 is N+1,
%  assertz(rule(N,V1,NH,HL,BL)),
  process_clauses(T,N1,N2,Model0,Model1).
  
process_clauses([((H:-B),V)|T],N,N2,Model0,[def_rule(N,V,H1,BL)|Model1]):-!,
  list2and(BL0,B),
  N1 is N+1,
  add_int_atom([H|BL0],[H1|BL],_VI),
%  assertz(rule(N,V1,NH,HL,BL)),
  process_clauses(T,N1,N2,Model0,Model1).

process_clauses([(H,V)|T],N,N2,Model0,[rule(N,V,NH,HL,[],0)|Model1]):-
  H=(_;_),!,
  list2or(HL1,H),
  process_head(HL1,HL,_VI),
  length(HL,LH),
  listN(0,LH,NH),
  N1 is N+1,
%  assertz(rule(N,V,NH,HL,[])),
  process_clauses(T,N1,N2,Model0,Model1).

process_clauses([(H,V)|T],N,N2,Model0,[rule(N,V,NH,HL,[],0)|Model1]):-
  H=(_:_),!,
  list2or(HL1,H),
  process_head(HL1,HL,_VI),
  length(HL,LH),
  listN(0,LH,NH),
  N1 is N+1,
%  assertz(rule(N,V,NH,HL,[])),
  process_clauses(T,N1,N2,Model0,Model1).
  
process_clauses([(H,V)|T],N,N2,Model0,[def_rule(N,V,H1,[])|Model1]):-
  add_int_atom([H],[H1],_VI),
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

    
process_head_prob([H:PH],P,[H1:PH1],VI):-
  add_int_atom([H],[H1],VI),
  PH1 is PH,
  PNull is 1.0-P-PH1,
  PNull<1e-10,!.

process_head_prob([H:PH],P,[H1:PH1,'':PNull],VI):-
  add_int_atom([H],[H1],VI),
  PH1 is PH,
  PNull is 1.0-P-PH1.
  
process_head_prob([H:PH|T],P,[H1:PH1|NT],VI):-
  add_int_atom([H],[H1],VI),
  PH1 is PH,
  P1 is P+PH1,
  process_head_prob(T,P1,NT,VI).


add_int_atom([],[],_VI).

add_int_atom([H|T],[H|T1],VI):-
  inference_ib:builtin(H),!,
  add_int_atom(T,T1,VI).

add_int_atom([\+ H|T],[\+ H1|T1],VI):-!,
  H=..[F|Args],
  H1=..[F,VI|Args],
  add_int_atom(T,T1,VI).

add_int_atom([H|T],[H1|T1],VI):-
  H=..[F|Args],
  H1=..[F,VI|Args],
  add_int_atom(T,T1,VI).

/* predicates for reading in the program clauses */
read_clauses1(S,Clauses):-
    read_clauses_ground_body1(S,Clauses).


read_clauses_ground_body1(S,[(Cl,V)|Out]):-
  read_term(S,Cl,[variable_names(V)]),
  (Cl=end_of_file->
    Out=[]
  ;
    read_clauses_ground_body1(S,Out)
  ).


assert_rules([],_Pos,_HL,_BL,_Nh,_N,_V1):-!.

assert_rules(['':_P],_Pos,_HL,_BL,_Nh,_N,_V1):-!.

assert_rules([H:P|T],Pos,HL,BL,NH,N,V1):-
  assertz(rule(H,P,Pos,N,V1,NH,HL,BL)),
  Pos1 is Pos+1,
  assert_rules(T,Pos1,HL,BL,NH,N,V1).

  
listN(N,N,[]):-!.

listN(NIn,N,[NIn|T]):-
  N1 is NIn+1,
  listN(N1,N,T).

list0(N,N,[]):-!.

list0(NIn,N,[0|T]):-
  N1 is NIn+1,
  list0(N1,N,T).

list1(N,N,[]):-!.

list1(NIn,N,[1|T]):-
  N1 is NIn+1,
  list1(N1,N,T).

/* end of predicates for parsing an input file containing a program */


load_models(File):-
    open(File,read,Stream),
    read_models(Stream,0,Ex),
    retractall(ex(_)),
    assert(ex(Ex)),
    close(Stream).
    
read_models(Stream,N,NEx):-
    read(Stream,begin(model(_Name))),!,
    %format("Model ~p Y ~d~n",[Name,N]),
    read_all_atoms(Stream,N),
    N1 is N+1,
    read_models(Stream,N1,NEx).

read_models(_S,N,N).

read_all_atoms(Stream,Name):-
      read(Stream,At),
      (At \=end(model(_Name))->
        (At=neg(Atom)->    
      Atom=..[Pred|Args],
          Atom1=..[Pred,Name|Args],
          assertz(neg(Atom1))
        ;
          At=..[Pred|Args],
          Atom1=..[Pred,Name|Args],
          assertz(Atom1)
    ),
        read_all_atoms(Stream,Name)
;
    true
  ).    



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
    
lgamma1(A,B):-
  (A>=1.0->
    lgamma(A,B)
  ;
    B=0.0
  ).
write_model([],_Stream):-!.

write_model([def_rule(_N,_S,H,[])|Rest],Stream):-!,
  copy_term(H,H1),
    numbervars(H1,0,_M),
    remove_int_atom(H1,H2),
    format(Stream,"~p.~n~n",[H2]),
    write_model(Rest,Stream).


write_model([def_rule(_N,_S,H,BL)|Rest],Stream):-!,
  copy_term((H,BL),(H1,BL1)),
    numbervars((H1,BL1),0,_M),
  remove_int_atom(H1,H2),
    format(Stream,"~p :- ~n",[H2]),
    write_body(Stream,BL1),
    format(Stream,".~n~n",[]),
    write_model(Rest,Stream).

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

write_head(S,[A:P]):-!,
  remove_int_atom(A,A1),
    format(S,"~p:~f",[A1,P]).

write_head(S,[A:P|Rest]):-
  remove_int_atom(A,A1),
    format(S,"~p:~f ; ",[A1,P]),
    write_head(S,Rest).

write_body(S,[\+ A]):-
  inference_ib:builtin(A),!,
    format(S,"\t\\+ ~p",[A]).

write_body(S,[\+ A]):-!,
  remove_int_atom(A,A1),
    format(S,"\t\\+ ~p",[A1]).
 
write_body(S,[A]):-
  inference_ib:builtin(A),!,
    format(S,"\t~p",[A]).

write_body(S,[A]):-!,
  remove_int_atom(A,A1),
    format(S,"\t~p",[A1]).
    
write_body(S,[\+ A|T]):-
  inference_ib:builtin(A),!,
    format(S,"\t\\+ ~p,~n",[A]),
    write_body(S,T).

write_body(S,[\+ A|T]):-!,
  remove_int_atom(A,A1),
    format(S,"\t\\+ ~p,~n",[A1]),
    write_body(S,T).

write_body(S,[A|T]):-
  inference_ib:builtin(A),!,
    format(S,"\t~p,~n",[A]),
    write_body(S,T).

write_body(S,[A|T]):-
  remove_int_atom(A,A1),
    format(S,"\t~p,~n",[A1]),
    write_body(S,T).

    
remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].

build_ground_lpad([],[]):-!.

build_ground_lpad([d(R,S)|T],[d(R,S,Head,Body)|T1]):-!,
  %write(g),
  user:def_rule_by_num(R,S,Head,Body),
  build_ground_lpad(T,T1).

build_ground_lpad([(R,S)|T],[(R,S,Head,Body)|T1]):-
  %write(g),
  user:rule_by_num(R,S,_,Head,Body),
  build_ground_lpad(T,T1).

/*
remove_head([],[]).

remove_head([(_N,R,S)|T],[(R,S)|T1]):-
  remove_head(T,T1).
*/

generate_nth_pos(I, I, [Head|_], Head):-!.
generate_nth_pos(I, IN, [_|List], El) :-
  I1 is I+1,
  generate_nth_pos(I1, IN, List, El).

my_get_value(K,V):-
  recorded(K,V,_R).
  
my_set_value(K,V):-
  eraseall(K),
  recorda(K,V,_R).

delete_matching([],_El,[]).

delete_matching([El|T],El,T1):-!,
        delete_matching(T,El,T1).

delete_matching([H|T],El,[H|T1]):-
        delete_matching(T,El,T1).

