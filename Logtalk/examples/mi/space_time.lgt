
:- object(space_time,
	extends(space, time)). 


	:- public(xyzt/4).
	:- mode(xyzt(?integer, ?integer, ?integer, ?integer), zero_or_one).


	xyzt(X, Y, Z, T) :-
		::xyz(X, Y, Z),
		::t(T).


:- end_object.
