/* Learn distribution for school database. */

/* We do not consider aggregates yet.      */

:- [pos:train].

:- ['../../examples/School/school_32'].

:- use_module(library(clpbn/learning/em)).

%:- set_em_solver(ve).
%:- set_em_solver(hve).
%:- set_em_solver(bdd).
%:- set_em_solver(bp).
%:- set_em_solver(cbp).

timed_main :-
	statistics(runtime, _),
	findall(X, goal(X), L),
	em(L, 0.01, 10, _, Lik),
	statistics(runtime, [T,_]),
	format('Took ~d msec and Lik ~3f~n',[T,Lik]).

main :-
	findall(X,goal(X),L),
	em(L, 0.01, 10, CPTs, Lik),
	writeln(Lik:CPTs).

%
% Change to 0.0, 0.1, 0.2 to make things simpler/harder
%
missing(0.2).

goal(professor_ability(P,V)) :-
	pos:professor_ability(P,V1),
	missing(X),
	( random > X -> V = V1 ; true).

goal(professor_popularity(P,V)) :-
	pos:professor_popularity(P,V1),
	missing(X),
	( random > X -> V = V1 ; true).

goal(registration_grade(P,V)) :-
	pos:registration_grade(P,V1),
	missing(X),
	( random > X -> V = V1 ; true).

goal(student_intelligence(P,V)) :-
	pos:student_intelligence(P,V1),
	missing(X),
	( random > X -> V = V1 ; true).

goal(course_difficulty(P,V)) :-
	pos:course_difficulty(P,V1),
	missing(X),
	( random > X -> V = V1 ; true).

goal(registration_satisfaction(P,V)) :-
	pos:registration_satisfaction(P,V1),
	missing(X),
	( random > X -> V = V1 ; true).

