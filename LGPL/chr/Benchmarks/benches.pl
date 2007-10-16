benches :-
	bench(B),
	atom_concat(B, '.chr', File),
	style_check(-singleton),
	abolish(main,0),
	abolish(main,1),
	load_files(File,[silent(true)]),
%	(main;main;main;main),
	main,
	fail.
benches.

bench(bool).
bench(fib).
bench(fibonacci).
bench(leq).
bench(primes).
bench(ta).
bench(wfs).
bench(zebra).

prolog:cputime(Time) :-
	statistics(runtime, [_,Time]).

:- benches.
