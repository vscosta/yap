
:- object(root,				% avoid infinite metaclass regression by
	instantiates(root)).	% making the class its own metaclass

	:- private(cv_/1).
	:- dynamic(cv_/1).
	:- mode(cv_(?integer), zero_or_one).

	:- public(cv/1).
	:- mode(cv(?integer), zero_or_one).

	:- public(set_cv/1).
	:- mode(set_cv(+integer), one).

	cv_(0).						% cv_/1 value is stored locally, in this class

	cv(Value) :-
		cv_(Value).				% retrive cv_/1 value, shared for all instances

	set_cv(Value) :-
		retractall(cv_(_)),		% retract old cv_/1 value from this class
		asserta(cv_(Value)).	% assert the new value into this class

:- end_object.


:- object(instance1,
	instantiates(root)).

:- end_object.


:- object(instance2,
	instantiates(root)).

:- end_object.


:- object(instance3,
	instantiates(root)).

:- end_object.
