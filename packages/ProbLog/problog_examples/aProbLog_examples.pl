%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% four aProbLog example programs corresponding to the four inference settings, all using the ProbLog graph
% (comment out all but one of the four cases to run)
% $Id: aProbLog_examples.pl 6460 2011-07-27 14:09:46Z bernd $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module('../aproblog').
:- use_module(library(lists)).

%%%%
% shared background knowledge
%%%%
% definition of acyclic path using list of visited nodes
path(X,Y) :- path(X,Y,[X],_).

path(X,X,A,A).
path(X,Y,A,R) :-
	X\==Y,
	edge(X,Z),
	absent(Z,A),
	path(Z,Y,[Z|A],R).

% using directed edges in both directions
edge(X,Y) :- dir_edge(Y,X).
edge(X,Y) :- dir_edge(X,Y).

% checking whether node hasn't been visited before
absent(_,[]).
absent(X,[Y|Z]):-X \= Y, absent(X,Z).

%/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% case 1: disjoint & neutral sums, bottleneck semiring %%%
%    ?- aproblog_label(path(1,6),L).
% L = 7 
:- set_aproblog_flag(disjoint_sum,true).
:- set_aproblog_flag(neutral_sum,true).

semiring_zero(-inf).
semiring_one(inf).
label_negated(_W,N) :-
	semiring_one(N).
semiring_addition(A,B,C) :-
	C is max(A, B).
semiring_multiplication(A,B,C) :-
	C is min(A,B).

9::dir_edge(1,2).
8::dir_edge(2,3).
6::dir_edge(3,4).
7::dir_edge(1,6).
5::dir_edge(2,6).
4::dir_edge(6,5).
7::dir_edge(5,3).
2::dir_edge(5,4).

%%% end case 1 %%%
%*/

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% case 2: disjoint & non-neutral sums, most likely interpretation semiring %%%
%    ?- aproblog_label(path(1,6),L).
% L = 0.0508032-[[e16],not[e54],[e53],not[e65],[e34],[e23],[e26],[e12]]
:- set_aproblog_flag(disjoint_sum,true).
:- set_aproblog_flag(neutral_sum,false).

% fact labels of form prob-[variable] with a unique "variable" for each fact 
% "practical" approach
% - resolves ties by taking first interpretation
% - uses arbitrary list order instead of some canonical representation
semiring_zero(0-[]).
semiring_one(1-[]).
label_negated(W-[A],WW-[not(A)]) :-
	WW is 1-W.
semiring_addition(A-L,B-LL,C-LLL) :-
	C is max(A, B),
	(
	 C =:= A
	->
	 LLL = L
	;
	 LLL = LL
	).
semiring_multiplication(A-L,B-LL,C-LLL) :-
	C is A*B,
	append(L,LL,LLL).

0.9-[[e12]]::dir_edge(1,2).
0.8-[[e23]]::dir_edge(2,3).
0.6-[[e34]]::dir_edge(3,4).
0.7-[[e16]]::dir_edge(1,6).
0.5-[[e26]]::dir_edge(2,6).
0.4-[[e65]]::dir_edge(6,5).
0.7-[[e53]]::dir_edge(5,3).
0.2-[[e54]]::dir_edge(5,4).
%%% end case 2 %%%
*/

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% case 3: non-disjoint & neutral sums, gradient semiring %%%
%    ?- aproblog_label(path(1,6),L).
% L = (0.8667952,[0.185328,0.039744,0.00259200000000001,0.444016,0.2064096,0.079488,0.038016,0.00777600000000005]) 
:- set_aproblog_flag(disjoint_sum,false).
:- set_aproblog_flag(neutral_sum,true).

% vector version for parallel computation: first argument is probability, second argument is an ordered list of gradients w.r.t. the different facts (0/1)
% as length varies, zero/one are still non-vector, thus different cases needed in definitions below
semiring_zero((0,0)).
semiring_one((1,0)).
% inverting base cases and lists
label_negated((P,0),(P2,0)) :-
	!,
	P2 is 1-P.
label_negated((P,1),(P2,-1)) :-
	!,
	P2 is 1-P.
label_negated((P,[]),(P2,[])) :-
	!,
	P2 is 1-P.
label_negated((P,[0|R]),(P2,[0|R2])) :-
	!,
	label_negated((P,R),(P2,R2)).
label_negated((P,[1|R]),(P2,[-1|R2])) :-
	label_negated((P,R),(P2,R2)).

% addition is per element on lists
% both are lists
semiring_addition((A1,[FA|RA]),(B1,[FB|RB]),(C1,Res)) :-
	!,
	C1 is A1 + B1,
	sum_lists_per_el([FA|RA],[FB|RB],Res).
% one is a neutral element 
semiring_addition((A1,[FA|RA]),(B1,B2),(C1,Res)) :-
	!,
	C1 is A1 + B1,
	sum_lists_per_el_constant([FA|RA],B2,Res).
semiring_addition((A1,A2),(B1,[FB|RB]),(C1,Res)) :-
	!,
	C1 is A1 + B1,
	sum_lists_per_el_constant([FB|RB],A2,Res).
% both are neutral elements
semiring_addition((A1,A2),(B1,B2),(C1,C2)) :-
	C1 is A1 + B1,
	C2 is A2 + B2.

sum_lists_per_el([],[],[]).
sum_lists_per_el([F|R],[FF|RR],[FFF|RRR]) :-
	FFF is F+FF,
	sum_lists_per_el(R,RR,RRR).

sum_lists_per_el_constant([],_,[]).
sum_lists_per_el_constant([F|R],C,[FFF|RRR]) :-
	FFF is F+C,
	sum_lists_per_el_constant(R,C,RRR).

% similar for multiplication, but need to pass on probabilities as well for product rule
semiring_multiplication((A1,[FA|RA]),(B1,[FB|RB]),(C1,Res)) :-
	!,
	C1 is A1 * B1,
	mult_lists_per_el([FA|RA],[FB|RB],A1,B1,Res).
semiring_multiplication((A1,[FA|RA]),(B1,B2),(C1,Res)) :-
	!,
	C1 is A1 * B1,
	mult_lists_per_el_constant([FA|RA],B2,A1,B1,Res).
semiring_multiplication((A1,A2),(B1,[FB|RB]),(C1,Res)) :-
	!,
	C1 is A1 * B1,
	mult_lists_per_el_constant([FB|RB],A2,B1,A1,Res).
semiring_multiplication((A1,A2),(B1,B2),(C1,C2)) :-
	C1 is A1*B1,
	C2 is A1*B2 + A2*B1.

mult_lists_per_el([],[],_,_,[]).
mult_lists_per_el([F|R],[FF|RR],P,PP,[FFF|RRR]) :-
	FFF is PP*F+FF*P,
	mult_lists_per_el(R,RR,P,PP,RRR).

mult_lists_per_el_constant([],_,_,_,[]).
mult_lists_per_el_constant([F|R],C,P,PP,[FFF|RRR]) :-
	FFF is F*PP+P*C,
	mult_lists_per_el_constant(R,C,P,PP,RRR).

(0.9,[1,0,0,0,0,0,0,0])::dir_edge(1,2).
(0.8,[0,1,0,0,0,0,0,0])::dir_edge(2,3).
(0.6,[0,0,1,0,0,0,0,0])::dir_edge(3,4).
(0.7,[0,0,0,1,0,0,0,0])::dir_edge(1,6).
(0.5,[0,0,0,0,1,0,0,0])::dir_edge(2,6).
(0.4,[0,0,0,0,0,1,0,0])::dir_edge(6,5).
(0.7,[0,0,0,0,0,0,1,0])::dir_edge(5,3).
(0.2,[0,0,0,0,0,0,0,1])::dir_edge(5,4).

%%% end case 3 %%%
*/

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% case 4: non-disjoint & non-neutral sums, expectation semiring %%%
%    ?- aproblog_label(path(1,6),L).
% L = (0.8667952,4.357428)
:- set_aproblog_flag(disjoint_sum,false).
:- set_aproblog_flag(neutral_sum,false).

% fact labels are tuples of (probability, probability*value)
% NB: this assumes that negative facts have value (and thus expected value) 0 

semiring_zero((0,0)).
semiring_one((1,0)).
label_negated((P,_),(Q,0)) :- Q is 1-P.
semiring_addition((A1,A2),(B1,B2),(C1,C2)) :-
	C1 is A1+B1,
	C2 is A2+B2.
semiring_multiplication((A1,A2),(B1,B2),(C1,C2)) :-
	C1 is A1*B1,
	C2 is A1*B2+B1*A2.

% positive edges have value 1
(0.9,0.9*1)::dir_edge(1,2).
(0.8,0.8*1)::dir_edge(2,3).
(0.6,0.6*1)::dir_edge(3,4).
(0.7,0.7*1)::dir_edge(1,6).
(0.5,0.5*1)::dir_edge(2,6).
(0.4,0.4*1)::dir_edge(6,5).
(0.7,0.7*1)::dir_edge(5,3).
(0.2,0.2*1)::dir_edge(5,4).

%%% end case 4 %%%
*/
