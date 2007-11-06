
:- object(ctx_call_tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Tests <</2 built-in control construct.']).

%	:- initialization(::run).
%	:- initialization(::run('bios_tests.txt', write)).
	:- initialization(::run('results.txt', write)).

	throws(ctx1, _ << goal, error(instantiation_error, _, _)).
	throws(ctx2, object << _, error(instantiation_error, _, _)).
	throws(ctx3, 3 << goal, error(type_error(object_identifier, 3), _, _)).
	throws(ctx4, object << 3, error(type_error(callable, 3), _, _)).
	throws(ctx5, ctx_call_tests << goal, error(existence_error(procedure, goal/0), _)).
	throws(ctx6, xpto << goal, error(existence_error(object, xpto), _, _)).

	succeeds(ctx7, user << true).

	fails(ctx8, user << fail).

:- end_object.



:- object(bios_tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Tests built-in objects.']).

%	:- initialization(::run).
%	:- initialization(::run('bios_tests.txt', write)).
	:- initialization(::run('results.txt', append)).

	succeeds(all, (setof(Obj, (current_object(Obj), object_property(Obj, built_in)), Objs), Objs == [debugger,logtalk,user])).

	succeeds(user0, current_object(user)).
	succeeds(user1, object_property(user, built_in)).
	succeeds(user2, object_property(user, static)).

	succeeds(debugger0, current_object(debugger)).
	succeeds(debugger1, object_property(debugger, built_in)).
	succeeds(debugger2, object_property(debugger, static)).

	succeeds(logtalk0, current_object(logtalk)).
	succeeds(logtalk1, object_property(logtalk, built_in)).
	succeeds(logtalk2, object_property(logtalk, static)).

	throws(co0, current_object(1), error(type_error(object_identifier, 1), _)).

:- end_object.



:- object(list_tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Tests for the library object "list".']).

%	:- initialization(::run).
%	:- initialization(::run('list_tests.txt', write)).
	:- initialization(::run('results.txt', append)).

	setup :-
		current_logtalk_flag(report, Value),
		set_logtalk_flag(report, off),
		logtalk_load(library(types_loader), [reload(skip)]),
		set_logtalk_flag(report, Value),
		^^setup.

	fails(member0, list << member(_, [])).

	succeeds(member1, list << member(1, [1,2,3])).
	succeeds(member2, (findall(X, list << member(X, [1,2,3]), L), L == [1,2,3])).

	succeeds(length, (list << length([1,2,3], Length), Length =:= 3)).

:- end_object.



:- object(dyn_tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/09/15,
		comment is 'Tests dynamic objects and dynamic predicates.']).

%	:- initialization(::run).
%	:- initialization(::run('dyn_tests.txt', write)).
	:- initialization(::run('results.txt', append)).

	setup :-
		create_object(dyn_test, [], [], []).

	succeeds(dyn, This << goal) :-
		this(This).

	goal :-
		\+ dyn_test::current_predicate(_),
		dyn_test::asserta(a(1)),
		dyn_test::current_predicate(a/1),
		dyn_test::predicate_property(a(_), public),
		dyn_test::predicate_property(a(_), dynamic),
		dyn_test::predicate_property(a(_), declared_in(dyn_test)),
		dyn_test::predicate_property(a(_), defined_in(dyn_test)),
		dyn_test::assertz(a(2)),
		dyn_test::retractall(a(_)),
		\+ dyn_test::a(_),
		dyn_test::predicate_property(a(_), defined_in(dyn_test)),	% closed-world assumption
		dyn_test::current_predicate(a/1),
		dyn_test::abolish(a/1),
		\+ dyn_test::predicate_property(a(_), declared_in(dyn_test)),
		\+ dyn_test::predicate_property(a(_), defined_in(dyn_test)),
		\+ dyn_test::current_predicate(_).

	cleanup :-
		abolish_object(dyn_test).

:- end_object.
