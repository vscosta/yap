% learn distribution for school database.

% we do not consider the aggregates yet.

:- [pos:train].

:- ['~/Yap/work/packages/CLPBN/clpbn/examples/School/school_32'].

:- ['~/Yap/work/packages/CLPBN/learning/em'].

%:- clpbn:set_clpbn_flag(em_solver,gibbs).
:- clpbn:set_clpbn_flag(em_solver,jt).
%:- clpbn:set_clpbn_flag(em_solver,vel).

timed_main :-
	statistics(runtime, _),
	findall(X,goal(X),L),
        em(L,0.01,10,_,Lik),
	statistics(runtime, [T,_]),
	format('Took ~d msec and Lik ~3f~n',[T,Lik]).

main :-
        findall(X,goal(X),L),
        em(L,0.01,10,CPTs,Lik),
        writeln(Lik:CPTs).

%
% change to 0.0, 0.1, 0.2 to make things simpler/harder
%
missing(0.50).

% miss 30% of the examples.
goal(professor_ability(P,V)) :-
        pos:professor_ability(P,V1),
	missing(X),
        ( random > X -> V = V1 ; true).
% miss 10% of the examples.
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
