/*
	LPAD and CP-Logic reasoning suite
	Copyright (c) 2007, Fabrizio Riguzzi

Test file for lpadsld.pl, case where the body is grounded

Use
:-t.
to execute the test

*/
:-use_module(library(lpadvel)).
%:-use_module(lpadvelor).


epsilon(0.000001).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.

t:-
	format("~nTesting lpadvel.yap~n~n",[]),
	t(max_card),!,
	t(top_sort),!,
	t(min_def),
	format("All orders successful~n",[]).

t:-
	format("Test unsuccessful.~n",[]).

t(Order):-
	files(F),
	statistics(runtime,[_,_]),
	format("~nOrder=~p~n",[Order]),
	set(order,Order), 
	test_files(F,ground_body(true)),
	statistics(runtime,[_,T]),
	T1 is T /1000,
	format("Test successful, time ~f secs.~n",[T1]).


test_files([],_GB).

test_files([H|T],GB):-
	library_directory(LD),
	atom_concat(LD,'/cplint/examples/',ExDir),
	atom_concat(ExDir,H,NH),
	p(NH),!,
	findall(A,test(A,H,GB),L),
	test_all(H,L),
	test_files(T,GB).

test_all(_F,[]).

test_all(F,[H|T]):-
	copy_term(H,NH),
	NH=(_Query,close_to('P',_Prob)),
	format("~a ~p.~n",[F,NH]),
	call(H),!,
	test_all(F,T).


files([exapprox,exrange,
threesideddice,
mendel,coin2,ex,trigger,throws,light]).

test((s([death],P),close_to(P,0.305555555555556)),trigger,_).

test((s([throws(mary),throws(john),break],P),close_to(P,0.46)),throws,_). 
test((s([throws(mary),throws(john),\+break],P),close_to(P,0.04)),throws,_).
test((s([\+ throws(mary),throws(john),break],P),close_to(P,0.3)),throws,_). 
test((s([\+ throws(mary),throws(john),\+ break],P),close_to(P,0.2)),throws,_).

test((s([push,replace],P),close_to(P,0.5)),light,_).
test((s([push,light],P),close_to(P,0.5)),light,_).
test((s([push,light,replace],P),close_to(P,0)),light,_).
test((s([light,replace],P),close_to(P,0)),light,_).
test((s([light],P),close_to(P,0.5)),light,_).
test((s([replace],P),close_to(P,0.5)),light,_).


test((s([\+ cites_cited(c1,p1)],P),close_to(P,0.7)),paper_ref_not,_).
test((s([cites_citing(c1,p1)],P),close_to(P,0.14)),paper_ref_not,_).


test((s([cites_cited(c1,p1)],P),close_to(P,0.181333333)),paper_ref,_).
test((s([cites_cited(c1,p2)],P),close_to(P,0.181333333)),paper_ref,_).
test((s([cites_cited(c1,p4)],P),close_to(P,0.181333333)),paper_ref,_).
test((s([cites_cited(c1,p3)],P),close_to(P,0.228)),paper_ref,_).
test((s([cites_cited(c1,p5)],P),close_to(P,0.228)),paper_ref,_).


test((s([female(f)],P),close_to(P,0.6)),female,_).
test((s([male(f)],P),close_to(P,0.4)),female,_).

test((s([a],P),close_to(P,0.1719)),exapprox,ground_body(true)).
test((s([a],P),close_to(P,0.099)),exapprox,ground_body(false)).

test((s([a(1)],P),close_to(P,0.2775)),exrange,_).
test((s([a(2)],P),close_to(P,0.36)),exrange,_).

test((s([on(0,1)],P),close_to(P,0.333333333333333)),threesideddice,_).
test((s([on(1,1)],P),close_to(P,0.222222222222222)),threesideddice,_).
test((s([on(2,1)],P),close_to(P,0.148148147703704)),threesideddice,_).

test((sc([on(2,1)],[on(0,1)],P),close_to(P,0.222222222222222)),threesideddice,_).
test((sc([on(2,1)],[on(1,1)],P),close_to(P,0.333333333333333)),threesideddice,_).


test((s([cg(s,1,p)],P),close_to(P,0.75)),mendel,_).
test((s([cg(s,1,w)],P),close_to(P,0.25)),mendel,_).
test((s([cg(s,2,p)],P),close_to(P,0.25)),mendel,_).
test((s([cg(s,2,w)],P),close_to(P,0.75)),mendel,_).
test((s([cg(f,2,w)],P),close_to(P,0.5)),mendel,_).
test((s([cg(s,2,w)],P),close_to(P,0.75)),mendel,_).

test((s([a],P),close_to(P,0.226)),ex,_).

test((s([heads(coin1)],P),close_to(P,0.51)),coin2,_).
test((s([heads(coin2)],P),close_to(P,0.51)),coin2,_).

test((s([tails(coin1)],P),close_to(P,0.49)),coin2,_).
test((s([tails(coin2)],P),close_to(P,0.49)),coin2,_).



test((s([student_rank(jane_doe,h)],P),close_to(P,0.465)),student,_).
test((s([student_rank(jane_doe,l)],P),close_to(P,0.535)),student,_).

test((s([course_rat(phil101,h)],P),close_to(P,0.330656)),student,_).
test((s([course_rat(phil101,l)],P),close_to(P,0.669344)),student,_).


test((s([professor_ability(p0,h)],P),close_to(P,0.5)),school,_).
test((s([professor_ability(p0,m)],P),close_to(P,0.4)),school,_).
test((s([professor_ability(p0,l)],P),close_to(P,0.1)),school,_).


test((s([professor_popularity(p0,h)],P),close_to(P,0.531)),school,_).
test((s([professor_popularity(p0,l)],P),close_to(P,0.175)),school,_).
test((s([professor_popularity(p0,m)],P),close_to(P,0.294)),school,_).

test((sc([professor_ability(p0,h)],[professor_popularity(p0,h)],P),close_to(P,0.847457627118644)),school,_).
test((sc([professor_ability(p0,l)],[professor_popularity(p0,h)],P),close_to(P,0.00188323917137476)),school,_).
test((sc([professor_ability(p0,m)],[professor_popularity(p0,h)],P),close_to(P,0.150659133709981)),school,_).

test((sc([professor_popularity(p0,h)],[professor_ability(p0,h)],P),close_to(P,0.9)),school,_).
test((sc([professor_popularity(p0,l)],[professor_ability(p0,h)],P),close_to(P,0.01)),school,_).
test((sc([professor_popularity(p0,m)],[professor_ability(p0,h)],P),close_to(P,0.09)),school,_).

test(( s([registration_grade(r0,1)],P),close_to(P,0.06675)),school,_).
test(( s([registration_grade(r0,2)],P),close_to(P,0.16575)),school,_).
test(( s([registration_grade(r0,3)],P),close_to(P, 0.356)),school,_).
test(( s([registration_grade(r0,4)],P),close_to(P,0.4115)),school,_).

test((sc([registration_grade(r0,1)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.15)),school,_).
test((sc([registration_grade(r0,2)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.285)),school,_).
test((sc([registration_grade(r0,3)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.424)),school,_).
test((sc([registration_grade(r0,4)],[registration_course(r0,C), course_difficulty(C,h)],P),close_to(P,0.141)),school,_).

test((sc([registration_grade(r0,1)], [registration_course(r0,C), course_difficulty(C,h), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.05)),school,_).
test((sc([registration_grade(r0,2)], [registration_course(r0,C), course_difficulty(C,h), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.15)),school,_).
test((sc([registration_grade(r0,3)], [registration_course(r0,C), course_difficulty(C,h), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.6)),school,_).
test((sc([registration_grade(r0,4)], [registration_course(r0,C), course_difficulty(C,h), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.2)),school,_).

test((sc([registration_grade(r0,1)],[registration_course(r0,C), course_difficulty(C,l), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.01)),school,_).
test((sc([registration_grade(r0,2)],[registration_course(r0,C), course_difficulty(C,l), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.02)),school,_).
test((sc([registration_grade(r0,3)],[registration_course(r0,C), course_difficulty(C,l), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.12)),school,_).
test((sc([registration_grade(r0,4)],[registration_course(r0,C), course_difficulty(C,l), 
	registration_student(r0,S), student_intelligence(S,h)],P),close_to(P,0.85)),school,_).

test((s([registration_satisfaction(r0,1)],P),close_to(P,0.15197525)),school,_).
test((s([registration_satisfaction(r0,2)],P),close_to(P,0.1533102)),school,_).
test((s([registration_satisfaction(r0,3)],P),close_to(P,0.6947145)),school,_).

test((sc([registration_satisfaction(r0,1)],[ registration_student(r0,S), 
	student_intelligence(S,h)],P),close_to(P,0.0959225)),school,_).
test((sc([registration_satisfaction(r0,2)],[ registration_student(r0,S), 
	student_intelligence(S,h)],P),close_to(P,0.124515)),school,_).
test((sc([registration_satisfaction(r0,3)],[ registration_student(r0,S), 
	student_intelligence(S,h)],P),close_to(P,0.7795625)),school,_).

test((sc([registration_satisfaction(r0,1)],[registration_grade(r0,4)],P),close_to(P,0.04)),school,_).
test((sc([registration_satisfaction(r0,2)],[registration_grade(r0,4)],P),close_to(P,0.06)),school,_).
test((sc([registration_satisfaction(r0,3)],[registration_grade(r0,4)],P),close_to(P,0.9)),school,_).

test((sc([registration_satisfaction(r0,1)],[registration_grade(r0,1)],P),close_to(P,0.528)),school,_).
test((sc([registration_satisfaction(r0,2)],[registration_grade(r0,1)],P),close_to(P,0.167)),school,_).
test((sc([registration_satisfaction(r0,3)],[registration_grade(r0,1)],P),close_to(P,0.305)),school,_).

test((sc([ registration_grade(r0,1)],[registration_satisfaction(r0,3)],P),close_to(P,0.0293052037923492)),school,_).
test((sc([ registration_grade(r0,2)],[registration_satisfaction(r0,3)],P),close_to(P, 0.114760451955444)),school,_).
test((sc([ registration_grade(r0,3)],[registration_satisfaction(r0,3)],P),close_to(P,0.322837654892765)),school,_).
test((sc([ registration_grade(r0,4)],[registration_satisfaction(r0,3)],P),close_to(P,0.533096689359442)),school,_).

test((s([course_rating(c0,h)],P),close_to(P,0.5392099)),school,_).
test((s([course_rating(c0,l)],P),close_to(P, 0.2)),school,_).
test((s([course_rating(c0,m)],P),close_to(P,0.2607901)),school,_).

test((sc([course_difficulty(c0,h)],[course_rating(c0,h)],P),close_to(P,0.235185778302661)),school,_).
test((sc([course_difficulty(c0,l)],[course_rating(c0,h)],P),close_to(P,0.259096503977393)),school,_).
test((sc([course_difficulty(c0,m)],[course_rating(c0,h)],P),close_to(P,0.505717717719945)),school,_).

test((s([course_difficulty(c0,h)],P),close_to(P,0.25)),school,_).
test((s([course_difficulty(c0,l)],P),close_to(P,0.25)),school,_).
test((s([course_difficulty(c0,m)],P),close_to(P,0.5)),school,_).


test((s([student_ranking(s0,h)],P),close_to(P,0.6646250000000005)),school_simple,_).
test((s([student_ranking(s0,l)],P),close_to(P,0.33537499999999987)),school_simple,_).

