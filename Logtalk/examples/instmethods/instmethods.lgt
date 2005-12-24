
:- object(root,				% avoid infinite metaclass regression by
	instantiates(root)).	% making the class its own metaclass

	:- public(method/0).

	method :-
		this(This),
		write('This is the default definition for the method, stored in class '),
		writeq(This), write('.'), nl.

:- end_object.



:- object(instance1,		% this instance simply inherits the method/0 predicate
	instantiates(root)).

:- end_object.



:- object(instance2,		% this instance provides its own definition for the
	instantiates(root)).	% method/0 predicate

	method :-
		this(This),
		write('This is an overriding definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl.

:- end_object.



:- object(instance3,		% this instance specializes the inherited definition
	instantiates(root)).	% of the method/0 predicate

	method :-
		this(This),
		write('This is a specializing definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl,
		write('It makes a super call to execute the default definition:'), nl, nl,
		^^method.

:- end_object.
