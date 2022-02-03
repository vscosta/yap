
%:- dynamic is_malignant/2.
	
:- source.

:- use_module(library(ordsets),[ord_subset/2, ord_union/3, ord_subtract/3]).

:- yap_flag(unknown,error).

:- dynamic cover/3, cl_counter/1.

main(Threshold) :-
	do2('0.yap','cls0.yap',Threshold,mr,[ff0,ff1],[nf0,nf1]),
	do2('1.yap','cls1.yap',Threshold,mr,[ff1,ff2],[nf1,nf2]),
	do2('2.yap','cls2.yap',Threshold,mr,[ff2,ff3],[nf2,nf3]),
	do2('3.yap','cls3.yap',Threshold,mr,[ff3,ff4],[nf3,nf4]),
	do2('4.yap','cls4.yap',Threshold,mr,[ff4,ff0],[nf4,nf0]).

	
main_all :-
	Threshold = 50,
	do2('cls0.yap','allcls0.yap',Threshold,all,[ff1,ff2],[nf1,nf2]),
	do2('cls1.yap','allcls1.yap',Threshold,all,[ff2,ff3],[nf2,nf3]),
	do2('cls2.yap','allcls2.yap',Threshold,all,[ff3,ff4],[nf3,nf4]),
	do2('cls3.yap','allcls3.yap',Threshold,all,[ff4,ff5],[nf4,nf5]),
	do2('cls4.yap','allcls4.yap',Threshold,all,[ff5,ff1],[nf5,nf1]).
	
 
do2(F,OF,Threshold,WhatClauses,Ps,Ns) :-
	once((
	retract(cl_counter(_)),
	assert(cl_counter(0)),
%	atom_concat(['/u/dutra/vitor/BreastCancer/new_i',F],IF),
%	atom_concat(['i1',F],IF),
	atom_concat(['i',F],IF),
	compile(IF),
%	atom_concat(['i2',F],I2F),
%	consult(I2F),
	count_examples(Ps,As,PCount),
	count_examples(Ns,_,NCount),
	eval_clauses(WhatClauses,Ps,Ns,PCount,NCount,LClauses),
	length(LClauses,Ls), 
	all_covered(LClauses,[],PCovs),length(PCovs,PCov),
	ord_subtract(As,PCovs,Uncovered),
	format('   ~d clauses covering ~d/~d (~w)~n', [Ls,PCov,PCount,Uncovered]),nl,
	add_up(Threshold,LClauses,NList),
	write(NList),nl,
	write_down(NList,OF)
	    )),
	fail.
do2(F,OF,Threshold,WhatClauses,Ps,Ns).

eval_clauses(WhatClauses,Ps,Ns,PCount,NCount,LClauses) :-
	findall(Perf,clause_performance(WhatClauses,Ps,Ns,PCount,NCount,Perf),LClauses).

clause_performance(WhatClauses,Ps,Ns,PCount,NCount,cl(Score,Id,LPs,LNs)) :-
	target(_,Head,Id,A),
	clause(Head,Bd),
%	Bd = (_,_,_), 
%	( WhatClauses = all -> true ;
%	    attributes(Bd,A,0,As), As < 2 ),
	example_support(A,Ps,LPs0,Ns,LNs,Id),
	sort(LPs0,LPs),
	length(LPs,N),
	N >= 10,
%	write(doing:N:Bd),nl,
	score(LPs,LNs,Bd,Score). %,
%	write(Id:MEstimate),nl.

/*
score(LPs,LNs,Bd,Score) :-
	length(LPs,Score0),
	length(LNs,Score1),
	bdlength(Bd,0,B),
	Score is Score1/150-Score0.
*/
score(LPs,LNs,Bd,Score) :-
	length(LPs,P),
	length(LNs,N),
	Score is -(P+5)/(P+N+5).
/**/

add_up(T,Clauses,ClList) :-
	length(Clauses,Len),
	(Len > T -> Th = T; Th = Len),
        sort(Clauses,SClauses),
%	fetch_from_seeds(SClauses, SClauses2, ClList, ClListI, Th, TF, [], []),
	choose(Th, SClauses, ClList, [], [], RemainingClauses, RemainingClauses).

choose(0,_,[],_,Exs, [], _) :- !, length(Exs,L), write(covered(L)),nl.
choose(T,[],Out,SoFar,Exs, [], L) :- !, length(Exs,Ls), write(covered(Ls)),nl,
%	recompute_scores(L,NClauses0,[]),
%	sort(NClauses0,NClauses),
	choose_rest(T, L, SoFar, Out).
choose(T,[cl(S,I,Ps,Ns)|Clauses], Rem, SoFar, CovExs, [cl(S,I,Ps,Ns)|R], R0) :- 
 	ord_subset(Ps, CovExs), !,
	choose(T,Clauses,Rem,SoFar, CovExs, R, R0).
choose(T,[cl(Score,Id,Ps,Ns)|Clauses],Rem, SoFar, CovExs, R, R0) :-
	length(Ps,PL),length(Ns,NL),
	NScore is -Score, format('try ~d: ~w (~d/~d)~n',[Id,NScore,PL,NL]),
 	covered_so_far(SoFar,Score,Ps,Ns,10), !,
	choose(T,Clauses,Rem,SoFar, CovExs, R, R0).
choose(T,[cl(Score,Id,Ps,Ns)|Clauses],[Id|Rem],SoFar, CovExs0, R, R0) :-
	NewT is T - 1,
	ord_union(Ps, CovExs0, NCovExs),
	length(NCovExs, TCov),
	length(Ps,PL),length(Ns,NL),
	format('======> got ~d (~d/~d) --> ~d~n',[Id,PL,NL,TCov]),
%	recompute_scores(Clauses,NClauses0,NCovExs),
%	sort(NClauses0,NClauses),
	choose(NewT,Clauses,Rem,[cl(Score,Id,Ps,Ns)|SoFar], NCovExs, R, R0).


recompute_scores([],[],NCovExs).
recompute_scores([cl(Score,Id,Ps,Ns)|Clauses],[cl(NScore,Id,Ps,Ns)|NClauses],NCovExs) :-
	ord_subtract(Ps,NCovExs,NPs),
	target(_,Head,Id,A),
	clause(Head,Bd),
	score(NPs,Ns,Bd,NScore),
	recompute_scores(Clauses,NClauses,NCovExs).

choose_rest(0,_,_,[]) :- !.
choose_rest(T,[],_, []) :- !.
choose_rest(T,[cl(PTot,_,Ps,Ns)|Clauses], SoFar, OUT) :- 
 	covered_so_far(SoFar,PTot,Ps,Ns,30), !,
	choose_rest(T, Clauses ,SoFar, OUT).
choose_rest(T,[cl(Score,Id,Ps,Ns)|Clauses],SoFar, [Id|Rem]) :-
	length(Ps,PL),length(Ns,NL),
	NScore is -Score, write(cl(NScore,Id,PL/NL)),nl,
	NewT is T - 1,
	choose_rest(NewT,Clauses,[cl(Score,Id,Ps,Ns)|SoFar],Rem).

example_support(A,Ps,LPs,Ns,LNs,Id) :-
	findall(A,check_support(A,Id,Ps),LPs),
	findall(A,check_support(A,Id,Ns),LNs).

covered_so_far([cl(S,Id,Ps,Ns)|_], S0, Ps0, Ns0, Ratio) :-
	ord_subtract(Ps,Ps0,Sub),
	length(Sub,L),
	length(Ps,P1),
	(
	  L < P1/Ratio, L < 5
	->
	  format('  +  ~d caught (~d/~d)~n',[Id,L,P1])
	;
	  format('  -  ~d (~d/~d)~n',[Id,L,P1]),fail
	).
covered_so_far([_|L], S, Ps, Ns, Ratio) :-
	covered_so_far(L, S, Ps, Ns, Ratio).

check_support(A,Id,[Module|_]) :-
	target(Goal,Head,Id,A),
	Module:Goal,
	once(Head).
check_support(A,Id,[_|Modules]) :-
	check_support(A,Id,Modules).

count_examples(Ps,As,PCount) :-
	findall(A,in_p(A,Ps),As),
	length(As,PCount).

in_p(A,[Module|_]) :-
	target(Goal,_,_,A),
	Module:Goal.
in_p(A,[_|Modules]) :-
	in_p(A,Modules).

cl_counter(0).
my_cl_counter(0).

write_down(List,F) :-
	open(F,write,S),
	write_clauses(List,S),
	close(S).

write_clauses([],_).
write_clauses([Id|List],S) :-
	target(Goal,Head,Id,A),
	clause(Head,Bd),
	retract(cl_counter(J)),
	J1 is J+1,
	assert(cl_counter(J1)),
	target(Goal,Head,Id,A),
	target(_,JHead,J,A),
	portray_clause(S,(JHead :- Bd)),
	write_clauses(List,S).

append([],L,L) :- !.
append([H|L1],L2,[H|L3]) :-
	append(L1,L2,L3).

	

multi_relational((G,Bd),A) :- !,
	( multi_relational(G,A)
	;
    	  multi_relational(Bd,A)
	).
multi_relational(G,A) :-
	multi_relational_goal(G,A).

multi_relational_goal(G,A) :- arg(1,G,A1), A1 \== A.
 

attributes((G,Bd),A,I0,If) :- !,
	attributes(G,A,I0,I1),
	attributes(Bd,A,I1,If).
attributes(G,A,I0,If) :-
	attribute(G,A,I0,If).

attribute(same_study(A1,A2),A2,I,I) :- A1 \== A2, !. % back-ref
attribute(G,_,I,I) :- link_goal(G), !.
attribute(G,A,I0,If) :- arg(1,G,A1), A1 == A, !,
	If is I0+1.
attribute(_,_,I,I).

bdlength((A,B),I0,If) :- !,
	bdlength(A,I0,I1),
	bdlength(B,I1,If).
bdlength(Bd,I0,If) :-
	If is I0+1.


fetch_from_seeds(_,_,[],[],0,0,_,_) :- !.
fetch_from_seeds([],[],ClList,ClList,T,T,_,_).
fetch_from_seeds([cl(PTot,_,Ps,Ns)|SClauses],SClauses2,ClList,ClListI,T,TF,SeedsCovered,SoFar) :-
	covered_so_far(SoFar,PTot,Ps,Ns), !,
	fetch_from_seeds(SClauses,SClauses2,ClList,ClListI,T,TF,SeedsCovered,SoFar) .
fetch_from_seeds([cl(PTot,Id,Ps,Ns)|SClauses],SClauses2,[Id|ClList],ClListI,T,TF,SeedsCovered,SoFar) :-
	\+ ord_subset(Ps,SeedsCovered), !,
	write((Id->PTot)),
	T1 is T-1,
	ord_union(Ps,SeedsCovered,NewSeedsCovered),
	fetch_from_seeds(SClauses,SClauses2,ClList,ClListI,T1,TF,NewSeedsCovered,[cl(PTot,Id,Ps,Ns)|SoFar]).
fetch_from_seeds([Cl|SClauses],[Cl|SClauses2],ClList,ClListI,T,TF,SeedsCovered,SoFar) :-
	fetch_from_seeds(SClauses,SClauses2,ClList,ClListI,T,TF,SeedsCovered,SoFar).


all_covered([],L,L).
all_covered([cl(_,_,Ps,_)|S],L0,Lf) :-
	ord_union(Ps,L0,L),
	all_covered(S,L,Lf).

:- [use_background].

