
:- object(space). 


	:- public(xyz/3).
	:- mode(xyz(?integer, ?integer, ?integer), zero_or_one).

	:- private(xyz_/3).
	:- mode(xyz_(?integer, ?integer, ?integer), zero_or_one).
	:- dynamic(xyz_/3).

	:- public(rotate/3).
	:- mode(rotate(+integer, +integer, +integer), zero_or_one).


	xyz(X, Y, Z) :-
		::xyz_(X, Y, Z).


	rotate(X, Y, Z) :-
		integer(X),
		integer(Y),
		integer(Z),
		::retractall(xyz_(_, _, _)),
		::assertz(xyz_(X, Y, Z)).


:- end_object.
