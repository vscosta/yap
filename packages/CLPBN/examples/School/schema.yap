
/* base file for school database. Supposed to be called from school_*.yap */

professor_key(Key) :-
	professor(Key).

professor_ability(Key,Abi) :-
	abi_table(Key, AbiDist),
	{ Abi = ability(Key) with p([h,m,l], AbiDist)   }.

professor_popularity(Key, Pop) :-
	professor_ability(Key, Abi),
	pop_table(Key,PopTable),
	{ Pop = popularity(Key) with
		p([h,m,l], PopTable,[Abi]) }.

registration_key(Key) :-
	registration(Key, _, _).

registration_course(Key, CKey) :-
	registration(Key, CKey, _).

registration_student(Key, SKey) :-
	registration(Key, _, SKey).

registration_grade(Key, Grade) :-
	registration(Key, CKey, SKey),
	course_difficulty(CKey, Dif),
	student_intelligence(SKey, Int),
	grade_table(Int, Dif, Table),
	{ Grade = grade(Key) with Table }.

% registration_satisfaction(r0, h) :- {}.
registration_satisfaction(Key, Sat) :-
	registration_course(Key, CKey),
	course_professor(CKey, PKey),
	professor_ability(PKey, Abi),
	registration_grade(Key, Grade),
	satisfaction_table(Abi, Grade, Table),
	{ Sat = satisfaction(Key) with Table }.

course_key(Key) :-
	course(Key,_).
	
course_professor(Key, PKey) :-
	course(Key, PKey).
	
course_rating(CKey, Rat) :-
	setof(Sat, RKey^(registration_course(RKey,CKey), registration_satisfaction(RKey,Sat)), Sats),
	{ Rat =  rating(CKey) with avg([h,m,l],Sats) }.

course_difficulty(Key, Dif) :-
	dif_table(Key, Dist),
	{ Dif = difficulty(Key) with p([h,m,l], Dist) }.

student_key(Key) :-
	student(Key).

student_intelligence(Key, Int) :-
	int_table(Key, IDist, Domain),
	{ Int = intelligence(Key) with p(Domain, IDist) }.

student_ranking(Key, Rank) :-
	setof(Grade, CKey^(registration_student(CKey,Key),
			 registration_grade(CKey, Grade)), Grades),
	{ Rank = ranking(Key) with avg([a,b,c,d],Grades) }.

:- ensure_loaded(tables).



