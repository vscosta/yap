
:- object(root,
	instantiates(root)).


	:- private(cv_/1).
	:- dynamic(cv_/1).
	:- mode(cv_(?integer), zero_or_one).

	:- public(cv/1).
	:- mode(cv(?integer), zero_or_one).

	:- public(set_cv/1).
	:- mode(set_cv(+integer), one).


	cv_(0).						% cv value is stored locally, in this class


	cv(Value) :-
		cv_(Value).				% retrive cv value, shared for all instances


	set_cv(Value) :-
		retractall(cv_(_)),		% retract old cv value from this class
		asserta(cv_(Value)).	% assert the new value in this class


:- end_object.
