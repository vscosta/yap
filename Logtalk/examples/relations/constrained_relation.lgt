
:- object(constrained_relation,
	instantiates(class),
	specializes(relation)).


	:- info([
		version is 3.2,
		date is 2005/1/29,
		author is 'Paulo Moura',
		comment is 'Enables the representation of relations with constraints on the state of participating objects.']).


	:- uses(list,
		[member/2, memberchk/2, subtract/3]).


	:- private(activ_points_/3).
	:- dynamic(activ_points_/3).
	:- mode(activ_points_(?atom, ?event, -list), zero_or_more).

	:- public(activ_point/3).
	:- mode(activ_point(?atom, ?event, ?callable), zero_or_more).

	:- public(activ_points/3).
	:- mode(activ_points(?atom, ?event, -list), zero_or_more).

	:- public(set_activ_points/3).
	:- mode(set_activ_points(+atom, +event, +list), one).


	:- protected(unique_messages/4).
	:- mode(unique_messages(+list, +atom, +event, -list), one).

	:- protected(propagate/5).
	:- mode(propagate(+atom, +callable, +object, +atom, +list), zero_or_one).


	before(Object, Message, Sender) :-
		self(Self),
		(Self \= Sender ->
			forall(
				(::activ_point(Role, before, Message),
				 ::plays_role_in_tuple(Object, Role, Tuple)),
				::propagate(before, Message, Object, Role, Tuple))
			;
			true),
		^^before(Object, Message, Sender).


	after(Object, Message, Sender) :-
		self(Self),
		(Self \= Sender ->
			forall(
				(::activ_point(Role, after, Message),
				 ::plays_role_in_tuple(Object, Role, Tuple)),
				::propagate(after, Message, Object, Role, Tuple))
			;
			true),
		^^after(Object, Message, Sender).


	set_monitors(Tuple) :-
		^^set_monitors(Tuple),
		::descriptor(Descriptor),
		set_monitors(Tuple, Descriptor).


	set_monitors([], []).

	set_monitors([Object| Objects], [Role| Roles]) :-
		once(::activ_points(Role, before, Messages1)),	% avoid spurious backtracking
		set_object_before_monitors(Messages1, Object),
		once(::activ_points(Role, after, Messages2)),	% avoid spurious backtracking
		set_object_after_monitors(Messages2, Object),
		set_monitors(Objects, Roles).


	set_object_before_monitors([], _).

	set_object_before_monitors([Message| Messages], Object) :-
		self(Self),
		before_event_registry::set_monitor(Object, Message, _, Self),
		set_object_before_monitors(Messages, Object).


	set_object_after_monitors([], _).

	set_object_after_monitors([Message| Messages], Object) :-
		self(Self),
		after_event_registry::set_monitor(Object, Message, _, Self),
		set_object_after_monitors(Messages, Object).


	del_monitors(Tuple) :-
		^^del_monitors(Tuple),
		::descriptor(Descriptor),
		del_monitors(Tuple, Descriptor). 


	del_monitors([], []).

	del_monitors([Object| Objects], [Role| Roles]) :-
		del_object_monitors(Object, Role),
		del_monitors(Objects, Roles).


	del_object_monitors(Object, Role) :-
		::plays_roles(Object, Roles) ->
			(member(Role, Roles) ->
				true
				;
				del_object_monitors(Object, Role, Roles))
			;
			del_object_monitors(Object, Role, []).


	del_object_monitors(Object, Role, Roles) :-
		::unique_messages(Roles, Role, before, Messages1),
		del_object_before_monitors(Messages1, Object),
		::unique_messages(Roles, Role, after, Messages2),
		del_object_after_monitors(Messages2, Object).


	del_object_before_monitors([], _).

	del_object_before_monitors([Message| Messages], Object) :-
		self(Self),
		before_event_registry::del_monitors(Object, Message, _, Self),
		del_object_before_monitors(Messages, Object).


	del_object_after_monitors([], _).

	del_object_after_monitors([Message| Messages], Object) :-
		self(Self),
		after_event_registry::del_monitors(Object, Message, _, Self),
		del_object_after_monitors(Messages, Object).


	propagate(Event, Message, Object, Role, Tuple) :-
		self(Self),
		sender(Sender),
		throw(error(desc_responsibility, Self::propagate(Event, Message, Object, Role, Tuple), Sender)).


	activ_point(Role, Event, Message) :-
		::activ_points_(Role, Event, Messages),
		member(Message, Messages).


	activ_points(Role, Event, List) :-
		::activ_points_(Role, Event, List).


	set_activ_points(Role, Event, List) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_activ_points(Role, Event, List), Sender)).

	set_activ_points(Role, Event, List) :-
		::descriptor(Descriptor),
		memberchk(Role, Descriptor),
		::retractall(activ_points_(Role, Event, _)),
		::assertz(activ_points_(Role, Event, List)).


	unique_messages(Roles, Role, Event, Messages) :-
		::activ_points_(Role, Event, Original),
		filter_messages(Roles, Original, Event, Messages).


	filter_messages([], Messages, _, Messages).

	filter_messages([Role| Roles], Original, Event, Messages) :-
		::activ_points_(Role, Event, Excluded),
		subtract(Original, Excluded, Rest),
		filter_messages(Roles, Rest, Event, Messages).


	set_descriptor(Descriptor) :-
		^^set_descriptor(Descriptor),
		set_default_activ_points(Descriptor).


	set_default_activ_points([]).

	set_default_activ_points([Role| Roles]) :-
		::set_activ_points(Role, before, []),
		::set_activ_points(Role, after, []),
		set_default_activ_points(Roles).


	print :-
		^^print,
		::descriptor(Descriptor),
		write('call activation points:'), nl,
		findall(Messages,
			(member(Role, Descriptor),
             ::activ_points(Role, before, Messages)),
           CallList),
		write('  '), writeq(CallList), nl,
		write('exit activation points:'), nl,
		findall(Messages,
			(member(Role, Descriptor),
			 ::activ_points(Role, after, Messages)),
           ExitList),
		write('  '), writeq(ExitList), nl.


:- end_object.
