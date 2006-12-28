
/*	Logtalk meta-predicates accept not only goals but also closures
	as meta-arguments as illustrated in this example
*/


:- object(meta).

	% the meta_predicate/1 directive below changes the interpretation of meta-calls on apply/2
	% clauses; the integer argument ("1") implies that the first argument is a closure that will 
	% be used to construct a goal by appending exactly one additional argument

	:- public(apply/2).
	:- mode(apply(+callable, ?term), zero_or_more).
	:- meta_predicate(apply(1, *)).

	apply(Closure, Arg) :-		% the Logtalk compiler verifies that any closure which is a
		call(Closure, Arg).		% meta-argument is used within a call/N method that honors the
								% meta-predicate directive (in this case, apply(1, *) => call/2)

	:- public(test_this/0).		% simple predicate for testing calls to a local meta-predicate

	test_this :-
		apply(foo(X), Y),
		write((X, Y)), nl.

	foo(1, meta).

:- end_object.


:- object(desc,
	extends(meta)).	

	:- public(test_self/0).		% simple predicate for testing calls to a meta-predicate
								% defined in an ancestor object
	test_self :-
		::apply(foo(X), Y),
		write((X, Y)), nl.

	foo(2, desc).

:- end_object.


:- object(test).

	:- public(test_obj/0).		% simple predicate for testing calls to a meta-predicate
								% defined in another object
	test_obj :-
		meta::apply(foo(X), Y),
		write((X, Y)), nl.

	foo(3, test).

:- end_object.
