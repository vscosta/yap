
:- object(airport).


	:- public(fly/1).
	:- mode(fly(?), zero_or_more).

	:- public(airport/1).
	:- mode(airport(?), zero_or_more).


	airport(Airport) :-
		self(Airport).


:- end_object.
