
:- object(bintree,
	implements(dictionaryp),
	extends(compound)).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Dictionary protocol implemented using binary trees.']).


	:- private(map/4).
	:- metapredicate(map(*, *, *, ::)).

	:- mode(map(+atom, +tree, -tree, -callable), zero_or_one).


	as_list(Tree, List) :-
		as_list(Tree, [], List).


	as_list(t, List, List).

	as_list(t(Key, Value, Left, Right), Acc, List) :-
		as_list(Right, Acc, Acc2),
		as_list(Left, [Key-Value| Acc2], List).


	empty(Tree) :-
		Tree == t.


	insert(Key, Value, t, t(Key, Value, t, t)) :-
		nonvar(Key).

	insert(Key, Value, t(Key1, Value1, Left1, Right1), t(Key1, Value2, Left2, Right2)) :-
		compare(Order, Key, Key1),
		insert(Order, Key, Value, Key1, Value1, Left1, Right1, Value2, Left2, Right2).


	insert(=, _, Value, _, _, Left, Right, Value, Left, Right).

	insert(<, Key, Value, _, Value1, Left1, Right, Value1, Left2, Right) :-
		insert(Key, Value, Left1, Left2).

	insert(>, Key, Value, _, Value1, Left, Right1, Value1, Left, Right2) :-
		insert(Key, Value, Right1, Right2).


	insert_all([], Tree, Tree).

	insert_all([Key-Value| Rest], Old, New) :-
		insert(Key, Value, Old, Aux),
		insert_all(Rest, Aux, New).


	lookup(Key, Value, Tree) :-
		var(Key) ->
			lookup_var(Key, Value, Tree)
			;
			lookup_nonvar(Key, Value, Tree).


	lookup_nonvar(Key, Value, t(Key1, Value1, Left1, Right1)) :-
		compare(Order, Key, Key1),
		lookup_nonvar(Order, Key, Value, Value1, Left1, Right1).


	lookup_nonvar(=, _, Value, Value, _, _).

	lookup_nonvar(<, Key, Value, _, Left, _) :-
		lookup_nonvar(Key, Value, Left).

	lookup_nonvar(<, Key, Value, _, _, Right) :-
		lookup_nonvar(Key, Value, Right).


	lookup_var(Key, Value, t(_, _, Left, _)) :-
		lookup_var(Key, Value, Left).

	lookup_var(Key, Value, t(Key, Value,_,_)).

	lookup_var(Key, Value, t(_, _, _, Right)) :-
		lookup_var(Key, Value, Right).


	keys(Tree, Keys) :-
		keys(Tree, [], Keys).


	keys(t, Keys, Keys).

	keys(t(Key, _, Left, Right), Acc, Keys) :-
		keys(Right, Acc, Acc2),
		keys(Left, [Key| Acc2], Keys).


	delete(t, _, _, t).

	delete(t(Key1, Value1, Left1, Right1), Key, Value, Out) :-
		compare(Order, Key, Key1),
		delete(Order, Key1, Value1, Left1, Right1, Key, Value, Out).


	delete(=, Key1, Value1, Left1, Right1, Key1, Value1, Out) :-
		join(Left1, Right1, Out).

	delete(<, Key1, Value1, Left1, Right1, Key, Value, t(Key1, Value1, Left2, Right1)) :-
		delete(Left1, Key, Value, Left2).

	delete(>, Key1, Value1, Left1, Right1, Key, Value, t(Key1, Value1, Left1, Right2)) :-
		delete(Right1, Key, Value, Right2).


	join(t, Right, Right) :-
		!.

	join(Left, t, Left) :-
		!.

	join(t(Key, Value, Left, Right), Tree, t(Key, Value, Left, Right2)) :-
		join(Right, Tree, Right2).


	map(Pred, Old, New) :-
		map(Pred, Old, New, _).


	map(Pred, t(Key1, Value1, Left1, Right1), t(Key2, Value2, Left2, Right2), Goal) :-
		Goal =.. [Pred, Key1-Value1, Key2-Value2],
		once(Goal),
		map(Pred, Left1, Left2, _),
		map(Pred, Right1, Right2, _).

	map(_, t, t, _).


	new(t).


	size(Dictionary, Size) :-
		size(Dictionary, 0, Size).


	size(t, Size, Size).

	size(t(_, _, Left, Right), Acc, Size) :-
		size(Left, Acc, Acc2),
		Acc3 is Acc2 + 1,
		size(Right, Acc3, Size).


:- end_object.
