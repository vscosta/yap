 
:- object(loop,
	implements(loopp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Loop control structures predicates.']).


	:- metapredicate(dowhile(::, ::)).

	:- metapredicate(forto(*, *, ::)).

	:- metapredicate(forto(*, *, *, ::)).

	:- metapredicate(fordownto(*, *, ::)).

	:- metapredicate(fordownto(*, *, *, ::)).

	:- metapredicate(whiledo(::, ::)).


	dowhile(Action, Condition) :-
		\+ \+ call(Action),
		whiledo(Condition, Action).


	whiledo(Condition, Action) :-
		call(Condition) ->
			\+ \+ call(Action),
			whiledo(Condition, Action)
			;
			true.


	forto(First, Last, Call) :-
		First =< Last ->
			\+ \+ call(Call),
			Next is First + 1,
			forto(Next, Last, Call)
			;
			true.


	forto(Count, First, Last, Call) :-
		First =< Last ->
			\+ \+ (Count = First, call(Call)),
			Next is First + 1,
			forto(Count, Next, Last, Call)
			;
			true.


	fordownto(First, Last, Call) :-
		First >= Last ->
			\+ \+ call(Call),
			Next is First - 1,
			fordownto(Next, Last, Call)
			;
			true.


	fordownto(Count, First, Last, Call) :-
		First >= Last ->
			\+ \+ (Count = First, call(Call)),
			Next is First - 1,
			fordownto(Count, Next, Last, Call)
			;
			true.


:- end_object.
