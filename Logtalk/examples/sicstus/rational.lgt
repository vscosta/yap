
:- object(rational,
	implements(comparingp)).


	:- info([
		authors is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Implements comparison between rational numbers represented as Num/Den.']).


	N1/D1 < N2/D2 :-
		{N1*D2 < N2*D1}.


	N1/D1 =< N2/D2 :-
		{N1*D2 =< N2*D1}.


	N1/D1 > N2/D2 :-
		{N1*D2 > N2*D1}.


	N1/D1 >= N2/D2 :-
		{N1*D2 >= N2*D1}.


	N1/D1 =:= N2/D2 :-
		{N1*D2 =:= N2*D1}.


	N1/D1 =\= N2/D2 :-
		{N1*D2 =\= N2*D1}.


:- end_object.
