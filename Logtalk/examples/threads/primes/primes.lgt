
:- object(primes).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2006/11/26,
		comment is 'Simple example for comparing single and multi-threading calculation of prime numbers.']).

	:- threaded.

	:- public(st_prime_numbers/3).
	:- mode(st_prime_numbers(+integer, +integer, -list), one).
	:- info(st_prime_numbers/3, [
		comment is 'Returns all prime numbers in the given interval using a single calculation thread.',
		argnames is ['Inf', 'Sup', 'Primes']]).

	:- public(mt_prime_numbers/3).
	:- mode(mt_prime_numbers(+integer, +integer, -list), one).
	:- info(mt_prime_numbers/3, [
		comment is 'Returns all prime numbers in the given interval using two calculation threads.',
		argnames is ['Inf', 'Sup', 'Primes']]).

	st_prime_numbers(N, M, Primes) :-
		M > N,
		prime_numbers(N, M, [], Primes).

	mt_prime_numbers(N, M, Primes) :-
		M > N,
		N1 is N + (M - N) // 2,
		N2 is N1 + 1,
		threaded_call(prime_numbers(N, N1, [], Acc)),
		threaded_call(prime_numbers(N2, M, Acc, Primes)),
		threaded_exit(prime_numbers(N, N1, [], Acc)),
		threaded_exit(prime_numbers(N2, M, Acc, Primes)).

	prime_numbers(N, M, Primes, Primes) :-
		N > M,
		!.
	prime_numbers(N, M, Acc, Primes) :-
		(	is_prime(N) ->
			Acc2 = [N| Acc]
		;	Acc2 = Acc),
	    N2 is N + 1,
		prime_numbers(N2, M, Acc2, Primes).

	is_prime(2) :- !.
	is_prime(Prime):-
		Prime > 2,
		Prime mod 2 =:= 1,
		Sqrt is sqrt(Prime),
		is_prime(3, Sqrt, Prime).

	is_prime(N, Sqrt, Prime):-
		(	N > Sqrt ->
			true
		;	Prime mod N > 0,
 			N2 is N + 2,
			is_prime(N2, Sqrt, Prime)
		).

:- end_object.
