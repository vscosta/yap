
:- category(subject).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 03/02/09,
		comment is 'Smalltalk dependent handling predicates.']).

	:- public(changed/0).
	:- mode(changed, one).
	:- info(changed/0,
		[comment is 'Receiver changed in some way. Notify all dependents.']).

	:- public(changed/1).
	:- mode(changed(?nonvar), one).
	:- info(changed/1,
		[comment is 'Receiver changed as specified in the argument. Notify all dependents.',
		 argnames is ['Change']]).

	:- public(dependents/1).
	:- mode(dependents(-list), one).
	:- info(dependents/1,
		[comment is 'Returns a list of all dependent objects.',
		 argnames is ['Dependents']]).

	:- private(dependent_/1).
	:- dynamic(dependent_/1).
	:- mode(dependent_(?object), zero_or_more).
	:- info(dependent_/1,
		[comment is 'Dependent object table.',
		 argnames is ['Dependent']]).

	:- public(addDependent/1).
	:- mode(addDependent(@object), one).
	:- info(addDependent/1,
		[comment is 'Adds a new dependent object.',
		 argnames is ['Dependent']]).

	:- public(removeDependent/1).
	:- mode(removeDependent(?object), zero_or_more).
	:- info(removeDependent/1,
		[comment is 'Removes a dependent object.',
		 argnames is ['Dependent']]).

	changed :-
		self(Self),
		forall(::dependent_(Dependent), Dependent::update(Self)).

	changed(Change) :-
		forall(::dependent_(Dependent), Dependent::update(Change)).

	dependents(Dependents) :-
		findall(Dependent, ::dependent_(Dependent), Dependents).

	addDependent(Dependent) :-
		::dependent_(Dependent) ->
			true
			;
			::asserta(dependent_(Dependent)).

	removeDependent(Dependent) :-
		::retract(dependent_(Dependent)).

:- end_category.
