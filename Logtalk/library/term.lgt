
:- object(term,
	implements(termp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Prolog term predicates.']).


	ground(Term) :-
		nonvar(Term),
		functor(Term, _, Arity),
		ground(Arity, Term).


	ground(0, _) :-
		!.

	ground(N, Term) :-
		N > 0,
		arg(N, Term, Arg),
		ground(Arg),
		N2 is N - 1,
		ground(N2, Term).


	occurs(Var, Term) :-
		var(Term) ->
			Var == Term
			;
			functor(Term, _, Arity),
			occurs(Arity, Var, Term).


	occurs(N, Var, Term) :-
		compound(Term),
		arg(N, Term, Arg),
		occurs(Var, Arg),
		!.

	occurs(N, Var, Term) :-
		N > 1,
		N2 is N - 1,
		occurs(N2, Var, Term).


	subsumes(General, Specific) :-
		vars(Specific, Vars),
		subsumes(General, Specific, Vars).


	subsumes(General, Specific, Vars) :-
		var(General),
		!,
		(var_member_chk(General, Vars) ->
			General == Specific
			;
			General = Specific).

	subsumes(General, Specific, Vars) :-
		nonvar(Specific),
		functor(General, Functor, Arity),
		functor(Specific, Functor, Arity),
		subsumes(Arity, General, Specific, Vars).


	subsumes(0, _, _, _) :- !.

	subsumes(N, General, Specific, Vars) :-
		arg(N, General,  GenArg),
		arg(N, Specific, SpeArg),
		subsumes(GenArg, SpeArg, Vars),
		M is N-1, !,
		subsumes(M, General, Specific, Vars).


	var_member_chk(Var, [Head| Tail]) :-
		Var == Head ->
			true
			;
			var_member_chk(Var, Tail).


	subterm(Term, Term).

	subterm(Sub, Term) :-
		nonvar(Term),
		functor(Term, _, N),
		subterm(N, Sub, Term).


	subterm(N, Sub, Term) :-
		compound(Term),
		arg(N, Term, Arg),
		subterm(Sub, Arg).

	subterm(N, Sub, Term) :-
		N > 1,
		M is N-1,
		subterm(M, Sub, Term).


	valid(_).


	vars(Term, Vars) :-
		vars(Term, [], Vars).


	vars(Term, Acc, Vars) :-
		var(Term) ->
			(var_member_chk(Term, Acc) ->
				Vars = Acc
				;
				Vars = [Term| Acc])
			;
			Term =.. [_| Args],
			var_list(Args, Acc, Vars).


	var_list([], Vars, Vars).

	var_list([Term| Terms], Acc, Vars) :-
		vars(Term, Acc, Acc2),
		var_list(Terms, Acc2, Vars).


:- end_object.
