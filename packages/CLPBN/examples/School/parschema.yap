
:- use_module(library(pfl)).

/* base file for school database. Supposed to be called from school_*.yap */

%
% bayes is a parfactor for a bayesian network,
% first argument is target of other arguments pop(K) <- abi(K)
% second argument is the name of a predicate to call for \phi (CPT)
% last argument is a list of goals defining the constraints over the elements
% of the 
%

%
% these states that skolem variables abi(K) are in a parametric factor with
% with \phi defined by abi_table(X) and whose domain and constraints
% is obtained from professor/1.
%
bayes abi(K)::[h,m,l] ; abi_table ; [professor(K)].

bayes pop(K)::[h,m,l], abi(K) ; pop_table ; [professor(K)].

bayes grade(C,S)::[a,b,c,d], int(S), diff(C) ; grade_table ; [registration(_,C,S)].

bayes sat(C,S,P)::[h,m,l], abi(P), grade(C,S) ; sat_table ; [reg_sat(C,S,P)].

bayes rat(C) :: [h,m,l], avg(Sats) ; avg ; [course_rating(C, Sats)].

bayes diff(C) :: [h,m,l] ; diff_table ; [course(C,_)].

bayes int(S) :: [h,m,l] ; int_table ; [student(S)].

bayes rank(S) :: [a,b,c,d], avg(Grades) ; avg ; [student_ranking(S,Grades)].


grade(Key, Grade) :-
	registration(Key, CKey, SKey),
	grade(CKey, SKey, Grade).

reg_sat(CKey, SKey, PKey) :-
	registration(_Key, CKey, SKey),
	course(CKey, PKey).

course_rating(CKey, Sats) :-
	course(CKey,  _),
	setof(sat(CKey,SKey,PKey),
	   reg_sat(CKey, SKey, PKey),
          Sats).

student_ranking(SKey, Grades) :-
	student(SKey),
	setof(grade(CKey,SKey), RKey^registration(RKey,CKey,SKey), Grades).

:- ensure_loaded(tables).

%
% evidence
%
abi(p0, h).

pop(p1, m).
pop(p2, h).

% Query
% ?- abi(p0, X).

