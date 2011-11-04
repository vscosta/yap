/*
	LPAD and CP-Logic reasoning suite
	File mcintyre.pl
	Monte Carlo inference for LPADs
	Copyright (c) 2011, Fabrizio Riguzzi
*/


:-dynamic rule_n/1,setting/2.
:-use_module(library(random)).
:-use_module(library(lists)).
:- set_prolog_flag(unknown,fail).
:-source.

rule_n(1).

setting(epsilon_parsing, 0.0001).

setting(compiling,true).
/* values: true, failse */

/* k
 * 
 * numnber of samples
 *
 * Default value:	1000
 */
setting(k, 1000).
/* min_error
 * ---------
 * This parameter shows the threshold for the probability interval.
 *
 * Default value:	0.01
 */
setting(min_error, 0.01).




s(Goals, Samples, CPUTime, WallTime, Lower, Prob, Upper):-
	solve(Goals, Samples, CPUTime, WallTime, Lower, Prob, Upper).

solve(Goals, Samples, CPUTime, WallTime, Lower, Prob, Upper) :-
	% Retrieving functional parameters...
	setting(k, K),
	setting(min_error, MinError),
	% Resetting the clocks...
  	statistics(cputime,[_,_]),
        statistics(walltime,[_,_]),
	% Performing resolution...
	montecarlo_cycle(0, 0, Goals, K, MinError, Samples, Lower, Prob, Upper),	
	% Taking elapsed times...
	statistics(cputime,[_,CT1]),
	CPUTime is CT1/1000,
	statistics(walltime,[_,WT1]),
	WallTime is WT1/1000.




/* montecarlo(Count, Success, Goals, K, MinError, Samples, Lower, Prob, Upper)
 * ---------------------------------------------------------------------------
 * This tail recursive predicate solves the problem currently in memory with a
 * Monte Carlo approach. 
 * It requires the number of samples and successes so far (Count and Success),
 * the desired list of Goals to fulfil, the number K of samples to consider at
 * once and the threshold MinError for the binomial proportion confidence 
 * interval.
 * It returns the total number of Samples considered, the Lower and Upper ends 
 * of the the binomial proportion confidence interval and the extimated Prob.
 * 
 * INPUT
 *  - Count: number of samples considered so far.
 *  - Success: number of successfull samples considered so far.
 *  - Goals: list of goals to fulfil.
 *  - K: number of samples to consider at once.
 *  - MinError: threshold for the binomial proportion confidence interval.
 * 
 * OUTPUT
 *  - Samples: total number of samples considered.
 *  - Lower: lower end of the the binomial proportion confidence interval.
 *  - Prob: extimated probability.
 *  - Upper: upper end of the the binomial proportion confidence interval.
 *
 * NB: This method is based on the binomial proportion confidence interval and
 *     the Brown's rule of thumb to avoid the case the sample proportion is 
 *     exactly 0.0 or 1.0 and doesn't make use of BDDs.
 */
montecarlo_cycle(N0, S0, Goals, K, MinError, Samples, Lower, Prob, Upper):-!,
	montecarlo(K,N0, S0, Goals, N, S),
	P is S / N,
	D is N - S,
	Semi is 1.95996 * sqrt(P * (1 - P) / N),
	Int is 2 * Semi,
	/*   N * P > 5;   N * S / N > 5;   S > 5
	 *   N (1 - P) > 5;   N (1 - S / N) > 5;   N (N - S) / N > 5;   N - S > 5
	 */
	%format("Batch: samples ~d positive ~d interval ~f~n",[N,S,Int]),
	%flush_output,
	%((S > 5, D > 5,
	((Int < MinError; Int =:= 0) ->
		Samples is N,
		Lower is P - Semi,
		Prob is P,
		Upper is P + Semi
	;
		montecarlo_cycle(N, S, Goals, K, MinError, Samples, Lower, Prob, Upper)
	).
	
montecarlo(0,N,S , _Goals,N,S):-!.

montecarlo(K1,Count, Success, Goals,N1,S1):-
	abolish_all_tables,
	eraseall(exp),
   	(Goals->
    		Valid=1
  	;
    		Valid=0
  	),
	N is Count + 1,
	S is Success + Valid,
	K2 is K1-1,
	montecarlo(K2,N, S, Goals, N1,S1).



member_eq(Item, [Head|_Tail]) :-
        Item==Head, !.

member_eq(Item, [_Head|Tail]) :-
	member_eq(Item, Tail).


list2and([X], X) :-
        X\= (_, _) , !.

list2and([H|T], (H, Ta)) :- !,
	list2and(T, Ta).



list2or([X], X) :-
	X \= ( _ ; _ ) , !.

list2or([H|T], (H ; Ta)) :- !,
	list2or(T, Ta).

get_var_n(R,S,Probs,V):-
	(v(R,S,V)->
		true
	;
		length(Probs,L),
		add_var(L,Probs,V),
		assert(v(R,S,V))
	).
sample_head(_HeadList,R,VC,NH):-
	recorded(exp,(R,VC,NH),_),!.

sample_head(HeadList,R,VC,NH):-
	sample(HeadList,NH),
	recorda(exp,(R,VC,NH),_).

generate_rules_fact([],_HeadList,_VC,_R,_Probs,_N,[]).

generate_rules_fact([(Head:_P1),(null:_P2)],_HeadList,VC,R,Probs,N,[Clause]):-!,
  (Clause=(Head:-(sample_head(Probs,R,VC,NH),NH=N))).


generate_rules_fact([(Head:_P)|T],HeadList,VC,R,Probs,N,[Clause|Clauses]):-
  (Clause=(Head:-(sample_head(Probs,R,VC,NH),NH=N))),
	N1 is N+1,
	generate_rules_fact(T,HeadList,VC,R,Probs,N1,Clauses).

generate_clause(Head,Body,_HeadList,VC,R,Probs,_BDDAnd,N,_Builtin,Clause):-
	(ground(VC)->
		Clause=(Head:-(sample_head(Probs,R,VC,NH),NH=N,Body))
	;
		Clause=(Head:-(Body,sample_head(Probs,R,VC,NH),NH=N))
  	).

generate_rules([],_Body,_HeadList,_VC,_R,_Probs,_BDDAnd,_N,_Builtin,[]).

generate_rules([(Head:_P1),(null:_P2)],Body,HeadList,VC,R,Probs,BDDAnd,N,Builtin,[Clause]):-!,
	generate_clause(Head,Body,HeadList,VC,R,Probs,BDDAnd,N,Builtin,Clause).

generate_rules([(Head:_P)|T],Body,HeadList,VC,R,Probs,BDDAnd,N,Builtin,[Clause|Clauses]):-
	generate_clause(Head,Body,HeadList,VC,R,Probs,BDDAnd,N,Builtin,Clause),
	N1 is N+1,
	generate_rules(T,Body,HeadList,VC,R,Probs,BDDAnd,N1,Builtin,Clauses).



/* set(Par,Value) can be used to set the value of a parameter */
set(Parameter,Value):-
	retract(setting(Parameter,_)),
	assert(setting(Parameter,Value)).



extract_vars(Variable, Var0, Var1) :- 
	var(Variable), !, 
	(member_eq(Variable, Var0) ->
		Var1 = Var0;
		append(Var0, [Variable], Var1)).

extract_vars(Term, Var0, Var1) :- 
	Term=..[_F|Args], 
	extract_vars_list(Args, Var0, Var1).



extract_vars_list([], Var, Var).

extract_vars_list([Term|Tail], Var0, Var1) :- 
	extract_vars(Term, Var0, Var), 
	extract_vars_list(Tail, Var, Var1).


difference([],_,[]).

difference([H|T],L2,L3):-
	member_eq(H,L2),!,
	difference(T,L2,L3).
	
difference([H|T],L2,[H|L3]):-
	difference(T,L2,L3).
	

process_head(HeadList, GroundHeadList) :- 
	ground_prob(HeadList), !,
	process_head_ground(HeadList, 0, GroundHeadList).
	 
process_head(HeadList, HeadList).



/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null]) :-!,
	ProbLast is 1 - Prob - ProbHead,
	setting(epsilon_parsing, Eps), 
	EpsNeg is - Eps, 
	ProbLast > EpsNeg, 
	(ProbLast > Eps ->
		Null = [null:ProbLast];
		Null = []). 

process_head_ground([Head:ProbHead|Tail], Prob, [Head:ProbHead|Next]) :- 
	ProbNext is Prob + ProbHead, 
	process_head_ground(Tail, ProbNext, Next).

ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :- 
	ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
	ground_prob(Tail).

get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :- 
	P1 is P, 
	get_probs(T, T1).

builtin(_A is _B).
builtin(_A > _B).
builtin(_A < _B).
builtin(_A >= _B).
builtin(_A =< _B).
builtin(_A =:= _B).
builtin(_A =\= _B).
builtin(true).
builtin(false).
builtin(_A = _B).
builtin(_A==_B).
builtin(_A\=_B).
builtin(_A\==_B).
builtin(length(_L, _N)).
builtin(member(_El, _L)).
builtin(average(_L, _Av)).
builtin(max_list(_L, _Max)).
builtin(min_list(_L, _Max)).
builtin(nth0(_, _, _)).
builtin(nth(_, _, _)).
builtin(eraseall(_Id)).
builtin(recordzifnot(_Id, _Item, _)).

parse(FileIn,FileOut):-
	open(FileIn,read,SI),
	read_clauses(SI,C),
	close(SI),
	process_clauses(C,[],C1),
	open(FileOut,write,SO),
	write_clauses(C1,SO),
	close(SO).

process_clauses([end_of_file],C,C).

process_clauses([H|T],C0,C1):-
	(expand_term(H,H1)->
		true
	;
		H1=H
	),
	(H1=[_|_]->
		append(C0,H1,C2)
	;
		append(C0,[H1],C2)
	),
	process_clauses(T,C2,C1).

read_clauses(S,[Cl|Out]):-
        read_term(S,Cl,[]),
	(Cl=end_of_file->
		Out=[]
	;
		read_clauses(S,Out)
	).

write_clauses([],_).

write_clauses([H|T],S):-
	write(S,H),
	write(S,'.'),
	nl(S),
	write_clauses(T,S).

sample(HeadList, HeadId) :-
	random(Prob), 
	sample(HeadList, 0, 0, Prob, HeadId), !.

sample([HeadProb|Tail], Index, Prev, Prob, HeadId) :-
	Succ is Index + 1,
	Next is Prev + HeadProb,
	(Prob =< Next ->
		HeadId = Index;
		sample(Tail, Succ, Next, Prob, HeadId)).

get_next_rule_number(R):-
	retract(rule_n(R)),
	R1 is R+1,
	assert(rule_n(R1)).


user:term_expansion((Head :- Body), Clauses):-
% disjunctive clause with more than one head atom
	setting(compiling,true),
	Head = (_;_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	get_next_rule_number(R),
	get_probs(HeadList,Probs),
	extract_vars((Head:-Body),[],VC),
	generate_rules(HeadList,Body,HeadList,VC,R,Probs,_BDDAnd,0,_Builtin,Clauses).

user:term_expansion((Head :- Body), Clauses) :- 
% disjunctive clause with a single head atom
	setting(compiling,true),
	((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
	Head = (H:_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	get_next_rule_number(R),
	get_probs(HeadList,Probs),
	extract_vars((Head:-Body),[],VC),
	generate_clause(H,Body,HeadList,VC,R,Probs,_BDDAnd,0,_Builtin,Clauses).



user:term_expansion(Head,Clauses) :- 
% disjunctive fact with more than one head atom
	setting(compiling,true),
	Head = (_;_),!, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList), 
	extract_vars((Head),[],VC),
	get_next_rule_number(R),
	get_probs(HeadList,Probs),
	generate_rules_fact(HeadList,HeadList,VC,R,Probs,0,Clauses).
            
user:term_expansion(Head,Clause) :- 
% disjunctive fact with a single head atom
	setting(compiling,true),
	(Head \= ((term_expansion(_,_)) :- _ )),
	Head = (Head1:_), !, 
	list2or(HeadListOr, Head), 
	process_head(HeadListOr, HeadList),
	get_probs(HeadList,Probs),
	extract_vars((Head),[],VC),
	get_next_rule_number(R),
	(Clause=(Head1:-(sample_head(Probs,R,VC,NH),NH=0))).

