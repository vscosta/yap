
:- object(time(_T)). 


	:- public(time/1).
	:- mode(time(?integer), zero_or_one).
	

	time(Time) :-
		parameter(1, Time).


:- end_object.
