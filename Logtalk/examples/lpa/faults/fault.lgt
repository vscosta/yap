
:- object(fault,
	imports(proto_hierarchy)).


	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Expert system for automobile fault diagnosis.',
		source is 'Example adopted from the LPA Prolog++ documentation.']).


	:- public(findall/0).
	:- mode(findall, one).

	:- private(told_by_user_/2).
	:- dynamic(told_by_user_/2).
	:- mode(told_by_user_(?nonvar, ?nonvar), zero_or_more).

	:- public(find/1).
	:- mode(find(?nonvar), zero_or_more).

	:- private(exhibited/1).
	:- mode(exhibited(+nonvar), zero_or_one).

	:- public(contrary/2).
	:- mode(contrary(?nonvar, ?nonvar), zero_or_more).

	:- public(fault/2).
	:- mode(fault(?nonvar, ?nonvar), zero_or_more).

	:- public(effect/2).
	:- mode(effect(?nonvar, ?nonvar), zero_or_more).

	:- public(symptom/2).
	:- mode(symptom(?nonvar, ?nonvar), zero_or_more).


	findall :-
		retractall(told_by_user_(_, _)),
		write('Please answer all questions with yes or no.'), nl, nl,
		forall(
 			(::descendant(Where), Where::find(Description)),
			(nl, write('Location      : '), write(Where), nl,
			 write('Possible Fault: '), write(Description), nl)),
		nl, write('No (more) explanations found.').


	find(Description) :-
		::fault(Fault, Description),
		forall(::effect(Fault, Symptom), exhibited(Symptom)).


	exhibited(Symptom) :-
		told_by_user_(Symptom, Reply),
		!,
		Reply = yes.

	exhibited(Symptom) :-
		::symptom(Symptom, Description),
		write(Description), write('? '),
		read(Reply),
		asserta(told_by_user_(Symptom, Reply)),
		Reply = yes,
		forall(
			(::contrary(Symptom, Contrary);
			 ::contrary(Contrary, Symptom)),
			asserta(told_by_user_(Contrary, no))).


:- end_object.
