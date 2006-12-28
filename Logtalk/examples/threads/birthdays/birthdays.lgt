
:- object(agent,
	implements(monitoring)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura and Peter Robinson',
		date is 2006/12/27,
		comment is 'Simple multi-threading example with agents and their birthdays.']).

	:- threaded.

	:- public(new/3).
	:- mode(new(+atom, +integer, +atom), one).
	:- info(new/3, [
		comment is 'Creates a new agent given its name, age, and gender.',
		argnames is ['Name', 'Age', 'Gender']]).

	:- public(age/1).
	:- dynamic(age/1).
	:- mode(age(?integer), zero_or_one).
	:- info(age/1, [
		comment is 'Agent age.']).

	:- public(ask_age/2).
	:- mode(ask_age(+atom, ?integer), zero_or_one).
	:- info(ask_age/2, [
		comment is 'Ask a friend his/her age.',
		argnames is ['Name', 'Age']]).

	:- public(gender/1).
	:- dynamic(gender/1).
	:- mode(gender(?integer), zero_or_one).
	:- info(gender/1, [
		comment is 'Agent gender.']).

	:- public(birthday/0).
	:- mode(birthday, one).
	:- info(birthday/0, [
		comment is 'Increments an agent age, an unfortunate side-effect of its birthday.']).

	:- public(happy_birthday/1).
	:- mode(happy_birthday(+object_identifier), one).
	:- info(happy_birthday/1, [
		comment is 'Happy birthday message from a friend.',
		argnames is ['From']]).

	:- public(cake_slice/1).
	:- mode(cake_slice(+object_identifier), one).
	:- info(cake_slice/1, [
		comment is 'Offer a slice of birthday cake to a friend.',
		argnames is ['From']]).

	:- public(new_friend/1).
	:- mode(new_friend(+object_identifier), one).
	:- info(new_friend/1, [
		comment is 'New friend, watch out for his/her birthday.',
		argnames is ['Name']]).

	% new agents are created as threaded objects:
	new(Name, Age, Gender) :-
		this(This),
		create_object(Name, [extends(This)], [threaded], [age(Age), gender(Gender)]).

	% ask a friend's age using an asynchronous message:
	ask_age(Friend, Age) :-
		threaded_call(Friend::age(Age)),
		threaded_exit(Friend::age(Age)).

	% getting older:
	birthday :-
		::retract(age(Old)),
		New is Old + 1,
		::assertz(age(New)).

	% when someone congratulate us for our birthday, acknowledge it,
	% and offer her/him a slice of the birthday cake:
	happy_birthday(From) :-
		self(Self),
		write('Happy birthday from '), write(From), write('!'), nl,
		write('Thanks! Here, have a slice of cake, '), write(From), write('.'), nl,
		threaded_ignore(From::cake_slice(Self)).

	% be nice, give thanks for the cake offer:
	cake_slice(From) :-
		write('Thanks for the cake '), write(From), write('!'), nl.

	% whatch out for our new friend anniversary:
	new_friend(Friend) :-
		self(Self),
		define_events(after, Friend, birthday, _, Self).

	% congratule a friend for his/her birthday:
	after(Friend, birthday, _) :-
		self(Self),
		threaded_ignore(Friend::happy_birthday(Self)).

:- end_object.
