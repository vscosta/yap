 
:- object(loop,
	implements(loopp)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/9/17,
		comment is 'Loop control structures predicates.']).


	:- meta_predicate(dowhile(::, ::)).

	:- meta_predicate(forto(*, *, ::)).

	:- meta_predicate(forto(*, *, *, ::)).

	:- meta_predicate(fordownto(*, *, ::)).

	:- meta_predicate(fordownto(*, *, *, ::)).

	:- meta_predicate(whiledo(::, ::)).


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
