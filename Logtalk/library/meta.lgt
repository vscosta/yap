
:- object(meta,
	implements(metap)).


	:- info([
		version is 1,
		date is 2000/7/24,
		author is 'Paulo Moura',
		comment is 'Useful meta-predicates.']).


	:- private(apply/3).
	:- metapredicate(apply(*, *, ::)).

	:- mode(apply(+callable, +list, -callable), zero_or_more).

	:- info(apply/3, [
		comment is 'Applies a predicate to list of arguments.',
		argnames is ['Predicate', 'Arguments', 'Goal']]).


	apply(Pred, Args) :-
		apply(Pred, Args, _).


	apply(Pred, Args, Goal) :-
		(atom(Pred) ->
			Goal =.. [Pred| Args]
			;
			Pred =.. Old,
			append(Old, Args, New),
			Goal =.. New),
		call(Goal).


	append([], List, List).

	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).


	callable(Term) :-
		nonvar(Term),
		functor(Term, Functor, _),
		atom(Functor).


	filter(Pred, In, Out) :-
		filter2(In, Pred, Out).


	filter2([], _, []).

	filter2([Arg| Args], Pred, List) :-
		(apply(Pred, [Arg], _) ->
			List = [Arg| Args2]
			;
			List = Args2),
		filter2(Args, Pred, Args2).


	map(Pred, In, Out) :-
		map2(In, Pred, Out).


	map2([], _, []).

	map2([Old| Olds], Pred, [New| News]) :-
		apply(Pred, [Old, New], _),
		map2(Olds, Pred, News).



	succeeds(Pred, List) :-
		succeeds2(List, Pred).


	succeeds2([], _).

	succeeds2([Head| Tail], Pred) :-
		apply(Pred, [Head], _),
		succeeds2(Tail, Pred).


:- end_object.
