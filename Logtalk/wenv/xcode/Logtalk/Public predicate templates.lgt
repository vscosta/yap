
	:- public(Functor/0).
	:- mode(Functor, Solutions).
	:- info(Functor/0, [
		comment is '']).

	:- public(Functor/1).
	:- mode(Functor(), Solutions).
	:- info(Functor/1, [
		comment is '',
		argnames is ['Arg']]).

	:- public(Functor/2).
	:- mode(Functor(, ), Solutions).
	:- info(Functor/2, [
		comment is '',
		argnames is ['Arg1', 'Arg2']]).

	:- public(Functor/3).
	:- mode(Functor(, , ), Solutions).
	:- info(Functor/3, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3']]).

	:- public(Functor/4).
	:- mode(Functor(, , , ), Solutions).
	:- info(Functor/4, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3', 'Arg4']]).

	:- public(Functor/5).
	:- mode(Functor(, , , , ), Solutions).
	:- info(Functor/5, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3', 'Arg4', 'Arg5']]).
