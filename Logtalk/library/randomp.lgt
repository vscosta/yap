
:- protocol(randomp).


	:- info([
		version is 1.0,
		authors is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Random number generator protocol.']).


	:- public(random/1).

	:- mode(random(-float), one).

	:- info(random/1, [
		comment is 'Returns a new random float value in the interval [0.0, 1.0[.',
		argnames is ['Random']]).


	:- public(random/3).

	:- mode(random(+integer, +integer, -integer), zero_or_one).
	:- mode(random(+float, +float, -float), zero_or_one).

	:- info(random/3, [
		comment is 'Returns a new random value in the interval [Lower, Upper[.',
		argnames is ['Lower', 'Upper', 'Random']]).


	:- public(randseq/4).

	:- mode(randseq(+integer, +integer, +integer, -list), zero_or_one).
	:- mode(randseq(+integer, +float, +float, -list), zero_or_one).

	:- info(randseq/4, [
		comment is 'Returns a list of Length random values in the interval [Lower, Upper[.',
		argnames is ['Length', 'Lower', 'Upper', 'List']]).


	:- public(randset/4).

	:- mode(randset(+integer, +integer, +integer, -list), zero_or_one).
	:- mode(randset(+integer, +float, +float, -list), zero_or_one).

	:- info(randset/4, [
		comment is 'Returns an ordered set of Length random values in the interval [Lower, Upper[.',
		argnames is ['Length', 'Lower', 'Upper', 'Set']]).


	:- public(reset_seed/0).

	:- mode(reset_seed, one).

	:- info(reset_seed/0, [
		comment is 'Resets the random seed to its default value.']).


	:- public(set_seed/1).

	:- mode(set_seed(+integer), zero_or_one).

	:- info(set_seed/1, [
		comment is 'Sets the random seed to the given value.',
		argnames is ['Seed']]).


:- end_protocol.
