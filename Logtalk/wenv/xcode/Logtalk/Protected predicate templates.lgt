
	:- protected(Functor/0).
	:- mode(Functor, Solutions).
	:- info(Functor/0, [
		comment is '']).

	:- protected(Functor/1).
	:- mode(Functor(), Solutions).
	:- info(Functor/1, [
		comment is '',
		argnames is ['Arg']]).

	:- protected(Functor/2).
	:- mode(Functor(, ), Solutions).
	:- info(Functor/2, [
		comment is '',
		argnames is ['Arg1', 'Arg2']]).

	:- protected(Functor/3).
	:- mode(Functor(, , ), Solutions).
	:- info(Functor/3, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3']]).

	:- protected(Functor/4).
	:- mode(Functor(, , , ), Solutions).
	:- info(Functor/4, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3', 'Arg4']]).

	:- protected(Functor/5).
	:- mode(Functor(, , , , ), Solutions).
	:- info(Functor/5, [
		comment is '',
		argnames is ['Arg1', 'Arg2', 'Arg3', 'Arg4', 'Arg5']]).
