
:- object(compound,
	extends(term)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Compound data type.']).


	valid(Compound) :-
		compound(Compound).


:- end_object.
