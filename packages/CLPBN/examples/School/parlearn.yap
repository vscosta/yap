
:- [pos:sample32].

:- ['~/Yap/work/CLPBN/clpbn/examples/School/school_32'].

% These libraries provide same functionality.
:- [library('clpbn/learning/mle')].
%:- [library('clpbn/learning/bnt_parms')].

:- [library(matrix)].

main :-
	findall(X,goal(X),L),
	learn_parameters(L,CPTs),
	write_cpts(CPTs).

goal(professor_ability(P,V)) :-
	pos:professor_ability(P,V),
	p(pa, M), random < M.
goal(professor_popularity(P,V)) :-
	pos:professor_popularity(P,V),
	p(pp, M), random < M.
goal(registration_grade(P,V)) :-
	pos:registration_grade(P,V),
	p(rg, M), random < M.
goal(student_intelligence(P,V)) :-
	pos:student_intelligence(P,V),
	p(si, M), random < M.
goal(course_difficulty(P,V)) :-
	pos:course_difficulty(P,V),
	p(cd, M), random < M.
goal(registration_satisfaction(P,V)) :-
	pos:registration_satisfaction(P,V),
	p(rs, M), random < M.

% sampling parameter
p(_, 1.0).

write_cpts([]).
write_cpts([CPT|CPTs]) :-
	matrix_to_list(CPT,L),
	format('CPT=~w~n',[L]),
	write_cpts(CPTs).

