
	:- private(Functor/0).
	:- mode(Functor, Solutions).
	:- info(Functor/0, [
		comment is '']).

	:- private(Functor/1).
	:- mode(Functor(), Solutions).
	:- info(Functor/1, [
		comment is '',
		argnames is ['Arg']]).

	:- private(Functor/2).
	:- mode(Functor(, ), Solutions).
	:- info(Functor/2, [
		comment is '',
		argnames is ['Arg1', 'Arg2']]).

	:- private(Functor/3).
	:- mode(Functor(, , ), Solutions).
	:- info(Functor/3, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3']]).

	:- private(Functor/4).
	:- mode(Functor(, , , ), Solutions).
	:- info(Functor/4, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3', 'Arg4']]).

	:- private(Functor/5).
	:- mode(Functor(, , , , ), Solutions).
	:- info(Functor/5, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3', 'Arg4', 'Arg5']]).
