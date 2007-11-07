/*
	LPAD and CP-Logic interpreter test program
	
Copyright (c) 2007, Fabrizio Riguzzi

use
:-t.
to execute the test

*/
:-use_module(library(cplint)).


epsilon(0.000001).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.


t:-
	files(F),
	statistics(runtime,[_,_]),
	test_files(F),
	statistics(runtime,[_,T]),
	T1 is T /1000,
	format("Time ~f secs~n",[T1]).

test_files([]).

test_files([H|T]):-
	format("~a~n",[H]),
	library_directory(LD),
	atom_concat(LD,'/cplint/examples/',ExDir),
	atom_concat(ExDir,H,NH),
	p(NH),
	findall(A,test(A,H),L),
	test_all(H,L),
	test_files(T).

test_all(_F,[]).

test_all(F,[H|T]):-
	copy_term(H,NH),
	NH=(Query,close_to('P',Prob)),
	format("~a ~p.~n",[F,NH]),
	call(H),
	test_all(F,T).


files([paper_ref_not,paper_ref,female,esapprox,esrange,threesideddice,
mendel,student,school_simple,school,coin2,es]).

test((s([\+ cites_cited(c1,p1)],P),close_to(P,0.7)),paper_ref_not).
test((s([cites_citing(c1,p1)],P),close_to(P,0.14)),paper_ref_not).


test((s([cites_cited(c1,p1)],P),close_to(P,0.181333333)),paper_ref).
test((s([cites_cited(c1,p2)],P),close_to(P,0.181333333)),paper_ref).
test((s([cites_cited(c1,p4)],P),close_to(P,0.181333333)),paper_ref).
test((s([cites_cited(c1,p3)],P),close_to(P,0.228)),paper_ref).
test((s([cites_cited(c1,p5)],P),close_to(P,0.228)),paper_ref).


test((s([female(f)],P),close_to(P,0.6)),female).
test((s([male(f)],P),close_to(P,0.4)),female).

test((s([a],P),close_to(P,0.1719)),esapprox).

test((s([a(1)],P),close_to(P,0.2775)),esrange).
test((s([a(2)],P),close_to(P,0.36)),esrange).

test((s([on(0,1)],P),close_to(P,0.333333333333333)),threesideddice).
test((s([on(1,1)],P),close_to(P,0.222222222222222)),threesideddice).
test((s([on(2,1)],P),close_to(P,0.148148147703704)),threesideddice).

test((sc([on(2,1)],[on(0,1)],P),close_to(P,0.222222222222222)),threesideddice).
test((sc([on(2,1)],[on(1,1)],P),close_to(P,0.333333333333333)),threesideddice).


test((s([cg(s,1,p)],P),close_to(P,0.75)),mendel).
test((s([cg(s,1,w)],P),close_to(P,0.25)),mendel).
test((s([cg(s,2,p)],P),close_to(P,0.25)),mendel).
test((s([cg(s,2,w)],P),close_to(P,0.75)),mendel).
test((s([cg(f,2,w)],P),close_to(P,0.5)),mendel).
test((s([cg(s,2,w)],P),close_to(P,0.75)),mendel).

test((s([a],P),close_to(P,0.226)),es).

test((s([heads(coin1)],P),close_to(P,0.51)),coin2).
test((s([heads(coin2)],P),close_to(P,0.51)),coin2).

test((s([tails(coin1)],P),close_to(P,0.49)),coin2).
test((s([tails(coin2)],P),close_to(P,0.49)),coin2).



test((s([student_rank(jane_doe,h)],P),close_to(P,0.465)),student).
test((s([student_rank(jane_doe,l)],P),close_to(P,0.535)),student).

test((s([course_rat(phil101,h)],P),close_to(P,0.330656)),student).
test((s([course_rat(phil101,l)],P),close_to(P,0.669344)),student).


test((s([professor_ability(p0,h)],P),close_to(P,0.5)),school).
test((s([professor_ability(p0,m)],P),close_to(P,0.4)),school).
test((s([professor_ability(p0,l)],P),close_to(P,0.1)),school).


test((s([professor_popularity(p0,h)],P),close_to(P,0.531)),school).
test((s([professor_popularity(p0,l)],P),close_to(P,0.175)),school).
test((s([professor_popularity(p0,m)],P),close_to(P,0.294)),school).

test((sc([professor_ability(p0,h)],[professor_popularity(p0,h)],P),close_to(P,0.847457627118644)),school).
test((sc([professor_ability(p0,l)],[professor_popularity(p0,h)],P),close_to(P,0.00188323917137476)),school).
test((sc([professor_ability(p0,m)],[professor_popularity(p0,h)],P),close_to(P,0.150659133709981)),school).

test((sc([professor_popularity(p0,h)],[professor_ability(p0,h)],P),close_to(P,0.9)),school).
test((sc([professor_popularity(p0,l)],[professor_ability(p0,h)],P),close_to(P,0.01)),school).
test((sc([professor_popularity(p0,m)],[professor_ability(p0,h)],P),close_to(P,0.09)),school).

test(( s([registration_grade(r0,1)],P),close_to(P,0.06675)),school).
test(( s([registration_grade(r0,2)],P),close_to(P,0.16575)),school).
test(( s([registration_grade(r0,3)],P),close_to(P, 0.356)),school).
test(( s([registration_grade(r0,4)],P),close_to(P,0.4115)),school).

test((sc([registration_grade(r0,1)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.15)),school).
test((sc([registration_grade(r0,2)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.285)),school).
test((sc([registration_grade(r0,3)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.424)),school).
test((sc([registration_grade(r0,4)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.141)),school).

test((sc([registration_grade(r0,1)], [registration_course(r0,C), course_difficulty(C,h), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.05)),school).
test((sc([registration_grade(r0,2)], [registration_course(r0,C), course_difficulty(C,h), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.15)),school).
test((sc([registration_grade(r0,3)], [registration_course(r0,C), course_difficulty(C,h), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.6)),school).
test((sc([registration_grade(r0,4)], [registration_course(r0,C), course_difficulty(C,h), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.2)),school).

test((sc([registration_grade(r0,1)],[registration_course(r0,C), course_difficulty(C,l), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.01)),school).
test((sc([registration_grade(r0,2)],[registration_course(r0,C), course_difficulty(C,l), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.02)),school).
test((sc([registration_grade(r0,3)],[registration_course(r0,C), course_difficulty(C,l), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.12)),school).
test((sc([registration_grade(r0,4)],[registration_course(r0,C), course_difficulty(C,l), registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.85)),school).

test((s([registration_satisfaction(r0,1)],P),close_to(P,0.15197525)),school).
test((s([registration_satisfaction(r0,2)],P),close_to(P,0.1533102)),school).
test((s([registration_satisfaction(r0,3)],P),close_to(P,0.6947145)),school).

test((sc([registration_satisfaction(r0,1)],[ registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.0959225)),school).
test((sc([registration_satisfaction(r0,2)],[ registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.124515)),school).
test((sc([registration_satisfaction(r0,3)],[ registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.7795625)),school).

test((sc([registration_satisfaction(r0,1)],[registration_grade(r0,4)],P),close_to(P,0.04)),school).
test((sc([registration_satisfaction(r0,2)],[registration_grade(r0,4)],P),close_to(P,0.06)),school).
test((sc([registration_satisfaction(r0,3)],[registration_grade(r0,4)],P),close_to(P,0.9)),school).

test((sc([registration_satisfaction(r0,1)],[registration_grade(r0,1)],P),close_to(P,0.528)),school).
test((sc([registration_satisfaction(r0,2)],[registration_grade(r0,1)],P),close_to(P,0.167)),school).
test((sc([registration_satisfaction(r0,3)],[registration_grade(r0,1)],P),close_to(P,0.305)),school).

test((sc([ registration_grade(r0,1)],[registration_satisfaction(r0,3)],P),close_to(P,0.0293052037923492)),school).
test((sc([ registration_grade(r0,2)],[registration_satisfaction(r0,3)],P),close_to(P, 0.114760451955444)),school).
test((sc([ registration_grade(r0,3)],[registration_satisfaction(r0,3)],P),close_to(P,0.322837654892765)),school).
test((sc([ registration_grade(r0,4)],[registration_satisfaction(r0,3)],P),close_to(P,0.533096689359442)),school).

test((s([course_rating(c0,h)],P),close_to(P,0.5392099)),school).
test((s([course_rating(c0,l)],P),close_to(P, 0.2)),school).
test((s([course_rating(c0,m)],P),close_to(P,0.2607901)),school).

test((sc([course_difficulty(c0,h)],[course_rating(c0,h)],P),close_to(P,0.235185778302661)),school).
test((sc([course_difficulty(c0,l)],[course_rating(c0,h)],P),close_to(P,0.259096503977393)),school).
test((sc([course_difficulty(c0,m)],[course_rating(c0,h)],P),close_to(P,0.505717717719945)),school).

test((s([course_difficulty(c0,h)],P),close_to(P,0.25)),school).
test((s([course_difficulty(c0,l)],P),close_to(P,0.25)),school).
test((s([course_difficulty(c0,m)],P),close_to(P,0.5)),school).


test((s([student_ranking(s0,h)],P),close_to(P,0.6646250000000005)),school_simple).
test((s([student_ranking(s0,l)],P),close_to(P,0.33537499999999987)),school_simple).

:-t.

:-halt.
