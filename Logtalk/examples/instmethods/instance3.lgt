
:- object(instance3,
	instantiates(root)).


	method :-
		this(This),
		write('This is a specializing definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl,
		write('It makes a super call to execute the default definition:'), nl, nl,
		^^method.


:- end_object.
