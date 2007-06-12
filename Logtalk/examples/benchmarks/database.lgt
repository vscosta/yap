
:- object(database).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Dynamic database benchmark utility predicates.']).

	:- public(this_dyndb/1).
	:- mode(this_dyndb(+nonvar), one).
	:- info(this_dyndb/1, [
		comment is 'Asserts and retracts a predicate in "this".',
		argnames is ['Term']]).

	:- public(self_dyndb/1).
	:- mode(self_dyndb(+nonvar), one).
	:- info(self_dyndb/1, [
		comment is 'Asserts and retracts a predicate using ::/1.',
		argnames is ['Term']]).

	:- public(obj_dyndb/1).
	:- mode(obj_dyndb(+nonvar), one).
	:- info(obj_dyndb/1, [
		comment is 'Asserts and retracts a predicate using ::/2.',
		argnames is ['Term']]).
	
	:- private([pred_this/1, pred_self/1, pred_obj/1]).
	:- dynamic([pred_this/1, pred_self/1, pred_obj/1]).

	% direct calls to assertz/1 and retract/1:
	this_dyndb(N) :-
		assertz(pred_this(N)),
		retract(pred_this(N)).

	% calls to assertz/1 and retract/1 using ::/1:
	self_dyndb(N) :-
		::assertz(pred_self(N)),
		::retract(pred_self(N)).

	% calls to assertz/1 and retract/1 using ::/2:
	obj_dyndb(N) :-
		this(This),
		This::assertz(pred_obj(N)),
		This::retract(pred_obj(N)).

:- end_object.
