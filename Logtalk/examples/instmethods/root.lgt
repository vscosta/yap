
:- object(root,
	instantiates(root)).


	:- public(method/0).


	method :-
		this(This),
		write('This is the default definition for the method, stored in class '),
		writeq(This), write('.'), nl.


:- end_object.
