
:- object(instance2,
	instantiates(root)).


	method :-
		this(This),
		write('This is an overriding definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl.


:- end_object.
