
:- object(colours,
	implements(comparingp)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Implements comparison between visible colors.']).


	Colour1 < Colour2 :-
		order(Colour1, N1),
		order(Colour2, N2),
		{N1 < N2}.


	Colour1 =< Colour2 :-
		order(Colour1, N1),
		order(Colour2, N2),
		{N1 =< N2}.


	Colour1 > Colour2 :-
		order(Colour1, N1),
		order(Colour2, N2),
		{N1 > N2}.


	Colour1 >= Colour2 :-
		order(Colour1, N1),
		order(Colour2, N2),
		{N1 >= N2}.


	Colour1 =:= Colour2 :-
		{Colour1 == Colour2}.


	Colour1 =\= Colour2 :-
		{Colour1 \== Colour2}.


	order(red, 1).
	order(orange, 2).
	order(yellow, 3).
	order(green, 4).
	order(blue, 5).
	order(indigo, 6).
	order(violet, 7).


:- end_object.
