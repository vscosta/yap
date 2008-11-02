% learn distribution for school database.

:- [pos:train].

:- ['~/Yap/work/CLPBN/clpbn/examples/School/school_32'].

:- ['~/Yap/work/CLPBN/learning/em'].

main :-
        findall(X,goal(X),L),
        em(L,0.01,10,CPTs,Lik),
        writeln(Lik:CPTs).

missing(0.3).

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
