
:- object(relation,
	implements(relationp),
	instantiates(class),
	specializes(object)).


	:- info([
		version is 1.2,
		date is 2004/8/15,
		author is 'Esteban Zimanyi, Paulo Moura',
		comment is 'Enables the representation of relations between independent objects.']).


	:- uses(before_event_registry).
	:- uses(after_event_registry).
	:- uses(list,
		[length/2, member/2, memberchk/2, nth1/3, same_length/2]).


	tuple(Tuple) :-
		::tuple_(Tuple).


	tuples(Tuples) :-
		findall(Tuple, ::tuple_(Tuple), Tuples).


	add_tuple(Tuple) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		\+ same_length(Tuple, Descriptor),
		self(Self),
		sender(Sender),
		throw(error(invalid_length, Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		::key(Key),
		make_tuple_template(Tuple, Descriptor, Key, Template),
		::tuple(Template),
		self(Self),
		sender(Sender),
		throw(error(breaks_key(Key), Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role),
		::cardinality(Role, _, Maximum),
		::plays_role_n_times(Object, Role, Number),
		Maximum = Number,
		self(Self),
		sender(Sender),
		throw(error(breaks_max_cardinality(Object, Role, Maximum), Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role),
		::domain(Role, Domain),
		(Domain::strict_instance ->
			\+ Domain::valid(Object)
			;
			\+ Object::ancestor(Domain)),
		self(Self),
		sender(Sender),
		throw(error(breaks_domain(Object, Role, Domain), Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::assertz(tuple_(Tuple)),
		::set_monitors(Tuple).


	make_tuple_template([], [], _, []).

	make_tuple_template([Object| Objects], [Role| Roles], Key, [Var| Rest]) :-
		(member(Role, Key) ->
			Var = Object
			;
			true),
	make_tuple_template(Objects, Roles, Key, Rest).


	remove_tuple(Tuple) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::remove_tuple(Tuple), Sender)).

	remove_tuple(Tuple) :-
		::descriptor(Descriptor),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role),
		::cardinality(Role, Minimum, _),
		::plays_role_n_times(Object, Role, Number),
		Minimum = Number,
		self(Self),
		sender(Sender),
		throw(error(breaks_min_cardinality(Object, Role, Minimum), Self::remove_tuple(Tuple), Sender)).

	remove_tuple(Tuple) :-
		::retract(tuple_(Tuple)),
		::del_monitors(Tuple).


	remove_all_tuples :-
		::retractall(tuple_(_)),
		::del_all_monitors.


	number_of_tuples(Number) :-
		findall(1, ::tuple_(_), List),
		length(List, Number).


	plays_roles(Object, Roles) :-
		::descriptor(Descriptor),
		setof(Role,
			Tuple^Position^ (::tuple(Tuple),
                           member(Object, Tuple),
                           nth1(Position, Tuple, Object),
                           once(nth1(Position, Descriptor, Role))),
         Roles).


	plays_role_in_tuple(Object, Role, Tuple) :-
		::descriptor(Descriptor),
		::tuple(Tuple),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role).


	plays_role_n_times(Object, Role, Number) :-
		::descriptor(Descriptor),
		nth1(Position, Descriptor, Role),
		setof(Tuple,
			(::tuple(Tuple),
			 nth1(Position, Tuple, Object)), 
			Tuples),
		length(Tuples, Number).


	domains(Domains) :-
		::descriptor(Descriptor),
		domains(Descriptor, Domains).


	domains([], []).

	domains([Role| Roles], [Domain| Domains]) :-
		::domain_(Role, Domain),
		domains(Roles, Domains).


	domain(Role, Domain) :-
		::domain_(Role, Domain).


	set_domains(Domains) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_domains(Domains), Sender)).

	set_domains(Domains) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_domains(Domains), Sender)).

	set_domains(Domains) :-
		::descriptor(Descriptor),
		set_domains(Domains, Descriptor).
		

	set_domains([], []).

	set_domains([Role| Roles], [Domain| Domains]) :-
		::retractall(domain_(Role, _)),
		::assertz(domain_(Role, Domain)),
		set_domains(Roles, Domains).



	descriptor(Descriptor) :-
		::descriptor_(Descriptor).


	degree(Degree) :-
		::descriptor_(Descriptor),
		length(Descriptor, Degree).


	set_descriptor(Descriptor) :-
		\+ ::tuple(_),
		::assertz(descriptor_(Descriptor)),
		::set_keys([Descriptor]),
		set_role_defaults(Descriptor).


	set_role_defaults([]).

	set_role_defaults([Role| Roles]) :-
		::set_domain(Role, object),
		::set_cardinality(Role, 0, n),
		::set_delete_option(Role, cascade),
		set_role_defaults(Roles).


	key(Key) :-
		::key_(Key).


	keys(Keys) :-
		findall(Key, ::key_(Key), Keys).


	set_keys(Keys) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_keys(Keys), Sender)).

	set_keys(Keys) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_keys(Keys), Sender)).

	set_keys(Keys) :-
		\+ valid_keys(Keys),
		self(Self),
		sender(Sender),
		throw(error(invalid_key, Self::set_keys(Keys), Sender)).

	set_keys(Keys) :-
		::retractall(key_(_)),
		set_keys2(Keys).
	
	set_keys2([]).

	set_keys2([Key| Keys]) :-
		::assertz(key_(Key)),
		set_keys2(Keys).


	valid_keys(Keys) :-
		::descriptor(Descriptor),
		valid_keys(Keys, Descriptor).


	valid_keys([], _).

	valid_keys([Key| Keys], Descriptor) :-
		forall(
			member(Role, Key),
			memberchk(Role, Descriptor)),
		valid_keys(Keys, Descriptor).



	delete_options(Options) :-
		::descriptor(Descriptor),
		delete_options(Descriptor, Options).


	delete_options([], []).

	delete_options([Role| Roles], [Option| Options]) :-
		::delete_option_(Role, Option),
		delete_options(Roles, Options).


	delete_option(Role, Option) :-
		::delete_option_(Role, Option).


	set_delete_options(Options) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		::descriptor(Descriptor),
		\+ same_length(Options, Descriptor),
		self(Self),
		sender(Sender),
		throw(error(invalid_length, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		\+ valid_delete_options(Options),
		self(Self),
		sender(Sender),
		throw(error(invalid_delete_option, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		::descriptor(Descriptor),
		set_delete_options(Descriptor, Options).


	set_delete_options([], []).

	set_delete_options([Role| Roles], [Option| Options]) :-
		::retractall(delete_option_(Role, _)),
		::assertz(delete_option_(Role, Option)),
		set_delete_options(Roles, Options).


	valid_delete_options([]).

	valid_delete_options([Option| Options]) :-
		once((Option = restrict; Option = cascade)),
		valid_delete_options(Options).



	cardinalities(Cardinalities) :-
		::descriptor(Descriptor),
		cardinalities(Descriptor, Cardinalities).


	cardinalities([], []).

	cardinalities([Role| Roles], [(Min, Max)| Cardinalities]) :-
		::cardinality_(Role, Min, Max),
		cardinalities(Roles, Cardinalities).


	cardinality(Role, Min, Max) :-
		::cardinality_(Role, Min, Max).


	set_cardinalities(Cardinalities) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_cardinalities(Cardinalities), Sender)).

	set_cardinalities(Cardinalities) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_cardinalities(Cardinalities), Sender)).

	set_cardinalities(Cardinalities) :-
		\+ valid_cardinalities(Cardinalities),
		self(Self),
		sender(Sender),
		throw(error(invalid_cardinality, Self::set_cardinalities(Cardinalities), Sender)).

	set_cardinalities(Cardinalities) :-
		::descriptor(Descriptor),
		set_cardinalities(Cardinalities, Descriptor).


	set_cardinalities([], []).

	set_cardinalities([(Min, Max)| Cardinalities], [Role| Roles]) :-
		::retractall(cardinality_(Role, _, _)),
		::assertz(cardinality_(Role, Min, Max)),
		set_cardinalities(Cardinalities, Roles).


	valid_cardinalities([]).

	valid_cardinalities([Cardinality| Cardinalities]) :-
		nonvar(Cardinality),
		Cardinality = (Min, Max),
		lower_cardinality(Min, Max),
		valid_cardinalities(Cardinalities).


	lower_cardinality(I, n) :-
		integer(I), !.

	lower_cardinality(I, J) :-
		integer(I),
		integer(J),
		I < J. 



	free(Options) :-
		::remove_all_tuples,
		^^free(Options).



	set_monitors([]).

	set_monitors([Object| Objects]) :-
		(instantiates_class(Object, Class) ->
			self(Self),
			before_event_registry::set_monitor(Class, delete(Object, _), _, Self)
			;
			true),
		set_monitors(Objects).


	del_monitors([]).

	del_monitors([Object| Objects]) :-
		((instantiates_class(Object, Class),
		  \+ (::tuple(Other), member(Object, Other))) ->
			self(Self),
			before_event_registry::del_monitors(Class, delete(Object, _), _, Self)
			;
			true),
		del_monitors(Objects).


	del_all_monitors :-
		self(Self),
		before_event_registry::del_monitors(_, _, _, Self),
		after_event_registry::del_monitors(_, _, _, Self).


	before(_, delete(Object, Options), _) :-
		!,
		((::delete_option(Role, restrict),
		  ::plays_role_in_tuple(Object, Role, Tuple)) ->
			self(Self),
			sender(Sender),
			throw(error(can_not_be_deleted(Tuple, Object, Role), Self::delete(Object, Options), Sender))
			;
			forall(
				::plays_role_in_tuple(Object, Role, Tuple),
				::remove_tuple(Tuple))).


	before(_, _, _).


	after(_, _, _).


	restore_monitors :-
		self(Self),
		before_event_registry::del_monitors(_, _, _, Self),
		after_event_registry::del_monitors(_, _, _, Self),
		forall(::tuple(Tuple), ::set_monitors(Tuple)).


	print :-
		::descriptor(Descriptor),
		write('descriptor:'), nl, write('  '), writeq(Descriptor), nl,
		::domains(Domains),
		write('domains:'), nl, write('  '), writeq(Domains), nl,
		::cardinalities(Cardinalities),
		write('cardinalities:'), nl, write('  '), writeq(Cardinalities), nl,
		::delete_options(Options),
		write('delete options:'), nl, write('  '), writeq(Options), nl,
		write('keys:'), nl,
		forall(::key(Key), (write('  '), writeq(Key), nl)),
		write('tuples:'), nl,
		forall(::tuple(Tuple), (write('  '), writeq(Tuple), nl)).


:- end_object.
