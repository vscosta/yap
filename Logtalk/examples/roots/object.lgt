
:- object(object,
	implements(objectp, event_handlersp),
	imports(initialization, class_hierarchy),
	instantiates(class)).


	:- info([
		version is 1.0,
		date is 2000/7/24,
		author is 'Paulo Moura',
		comment is 'Minimal predicates for all objects. Default root of the inheritance graph.']).


	:- uses(event_registry).


	strict_instance.


	default_free_option(del_monitors).


	process_free_option(del_monitors) :-
		self(Self),
		event_registry::del_monitors(Self, _, _, _),
		event_registry::del_monitors(_, _, Self, _),
		event_registry::del_monitors(_, _, _, Self).


	nil :-
		fail.


	print :-
		self(Self),
		writeq(Self), nl, nl,
		forall(
			::current_predicate(Predicate),
			(writeq(Predicate), nl)),
		nl.


	before(_, _, _).


	after(_, _, _).


:- end_object.
