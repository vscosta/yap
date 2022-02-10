%:- set(clauselength,6).
%:- set(depth, 200).
:- set(i,3).
%:- set(noise,0).
%:- set(minacc,1.0).
:- set(minpos,2).
:- set(noise, 10).
%:- set(nodes,1000000).
%:- set(m,20).
%:- set(evalfn,mestimate).
%:- set(test_pos,'prot_test_set.f').
%:- set(test_neg,'prot_test_set.n').
%:- set(optimise_clauses,true).

%:- set(record,true).
%:- set(recordfile,'test_aline.out').
%:- set(samplesize,0).

:- modeh(1,metabolism(+gene)).

:- modeb(1,essential(+gene,#essential)).
:- modeb(1,class(+gene,#class)).
:- modeb(1,complex(+gene,#complex)).
:- modeb(1,phenotype(+gene,#phenotype)).
:- modeb(1,motif(+gene,#motif)).
:- modeb(1,chromosome(+gene,#chromosome)).
:- modeb(1,gte(+number,#number)).
:- modeb(1,lte(+number,#number)).
%:- modeb(1,gte(+number,-number)).
:- modeb(*,interaction(+gene,-gene,-intertype,-number)).
%:- modeb(*,no_interaction(+gene,+gene,-intertype,-number)).
:- modeb(1,intertype(+intertype,#intertype)).

:- determination(metabolism/1,essential/2).
:- determination(metabolism/1,intertype/2).
:- determination(metabolism/1,class/2).
:- determination(metabolism/1,complex/2).
:- determination(metabolism/1,phenotype/2).
:- determination(metabolism/1,motif/2).
:- determination(metabolism/1,chromosome/2).
:- determination(metabolism/1,interaction/4).
:- determination(metabolism/1,gte/2).
:- determination(metabolism/1,lte/2).


lte(N1,N2):-
	number(N1),
	number(N2),
	!,
	N1 =< N2.

lte(N1,N2):- 
	number(N1),
	N1 =< 0.3,
	N1 = N2.


gte(N1,N2):-
	number(N1),
	number(N2),
	!,
	N1 >= N2.

gte(N1,N2):- 
	number(N1),
	N1 >= 0.3,
	N1 = N2.

intertype(X,X).

:- consult(metabolism).

/*
:- consult(interactions).

interaction(G1-Exclude,G2-[G1|Exclude],Type,Corr):- 
	!,
	interaction1(G1,G2,Type,Corr),
	\+ aleph_member(G2,Exclude).

interaction(G1,G2-[G1],Type,Corr):- 
	interaction1(G1,G2,Type,Corr).


essential(G-_,E):- essential(G,E).

class(G-_,C):- class(G,C).

complex(G-_,C):- complex(G,C).

phenotype(G-_,P):- phenotype(G,P).

motif(G-_,M):- motif(G,M).

chromosome(G-_,C):- chromosome(G,C).
*/


