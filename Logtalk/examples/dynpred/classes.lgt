
:- object(metaclass,			% avoid infinite metaclass regression by
	instantiates(metaclass)).	% making the class its own metaclass

:- end_object.


:- object(class,
	instantiates(metaclass)).

	:- public(p1/1).

	p1(class).

:- end_object.


:- object(instance,
	instantiates(class)).

:- end_object.
