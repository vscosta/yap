
:- object(joeEmployee,
	extends(joePerson)).


	:- public(worksFor/1).

	:- public(salary/1).
	:- dynamic(salary/1).
	
	:- public(giveRaise/1).


	worksFor('ToonTown').


	salary(1500).


	giveRaise(Raise) :-
		retract(salary(Old)),
		New is Old + Raise,
		asserta(salary(New)).


:- end_object.
