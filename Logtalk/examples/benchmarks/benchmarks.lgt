
:- object(benchmarks).

	:- info([
		version is 3.0,
		author is 'Paulo Moura',
		date is 2007/06/11,
		comment is 'Benchmark utility predicates and standard set of benchmarks.']).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs all benchmarks the default number of times.']).

	:- public(run/1).
	:- mode(run(+integer), one).
	:- info(run/1, [
		comment is 'Runs all benchmarks the specified number of times.',
		argnames is ['N']]).

	:- public(run/2).
	:- mode(run(+atom, +integer), one).
	:- info(run/2, [
		comment is 'Runs a specific benchmark the specified number of times.',
		argnames is ['Id', 'N']]).

	:- public(benchmark/2).
	:- mode(move(?atom, -callable), zero_or_more).
	:- info(move/2, [
		comment is 'Table of benchmark identifiers and benchmark goals.',
		argnames is ['Id', 'Goal']]).

	% run all benchmarks the default number of times:
	run :-
		run(1000000).

	% run all benchmark tests N times:
	run(N) :-
		benchmark(Id, Goal),
		run(Id, N, Looptime, Goaltime, Average, Speed),
		report(Id, Goal, N, Looptime, Goaltime, Average, Speed),
		fail.
	run(_).

	% run a specific benchmark test:
	run(Id, N) :-
		benchmark(Id, Goal),
		run(Id, N, Looptime, Goaltime, Average, Speed),
		report(Id, Goal, N, Looptime, Goaltime, Average, Speed).

	run(Id, N, Looptime, Goaltime, Average, Speed) :-
		{'$lgt_cpu_time'(Seconds1)},		% defined in the config files
		do_benchmark(empty_loop, N),
		{'$lgt_cpu_time'(Seconds2)},
		Looptime is Seconds2 - Seconds1,
		{'$lgt_cpu_time'(Seconds3)},
		do_benchmark(Id, N),
		{'$lgt_cpu_time'(Seconds4)},
		Goaltime is Seconds4 - Seconds3,
		Average is (Goaltime - Looptime)/N,
		Speed is 1.0/Average.

	report(Id, Goal, N, Looptime, Goaltime, Average, Speed) :-
		write(Id), write(': '),
		writeq(Goal), nl,
		write('Number of repetitions: '), write(N), nl,
		write('Loop time: '), write(Looptime), nl,
		write('Goal time: '), write(Goaltime), nl,
		write('Average time per call: '), write(Average), nl,
		write('Number of calls per second: '), write(Speed), nl,
		nl.

	% some benchmark tests for static code:
	benchmark(s1, my_length(List, _)) :-
		{generate_list(20, List)}.
	benchmark(s2, object::length(List, _)) :-
		{generate_list(20, List)}.

	% some benchmark tests for category predicate calls:
	benchmark(c1, leaf::ctg_direct).
	benchmark(c2, leaf::ctg_self).

	% some benchmark tests for dynamic code:
	benchmark(d1, (create_object(xpto, [], [], []), abolish_object(xpto))).
	benchmark(d2, plain_dyndb(_)).
	benchmark(d3, database::this_dyndb(_)).
	benchmark(d4, database::self_dyndb(_)).
	benchmark(d5, database::obj_dyndb(_)).

	% repeat a goal N times without using call/1 and using a failure-driven loop to 
	% avoid the interference of Prolog compiler memory management mechanism (such as 
	% garbage collection) on the results 
	do_benchmark(empty_loop, N) :-
		{my_repeat(N)},
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(s1, N) :-
		{generate_list(20, List)},
		{my_repeat(N)},
			{my_length(List, _)},
		fail.
	do_benchmark(s1, _).

	do_benchmark(s2, N) :-
		{generate_list(20, List)},
		{my_repeat(N)},
			object::length(List, _),
		fail.
	do_benchmark(s2, _).

	do_benchmark(c1, N) :-
		{my_repeat(N)},
			leaf::ctg_direct,
		fail.
	do_benchmark(c1, _).

	do_benchmark(c2, N) :-
		{my_repeat(N)},
			leaf::ctg_self,
		fail.
	do_benchmark(c2, _).

	do_benchmark(d1, N) :-
		{my_repeat(N)},
			create_object(xpto, [], [], []),
			abolish_object(xpto),
		fail.
	do_benchmark(d1, _).

	do_benchmark(d2, N) :-
		{my_repeat(N)},
			{plain_dyndb(N)},
		fail.
	do_benchmark(d2, _).

	do_benchmark(d3, N) :-
		{my_repeat(N)},
			database::this_dyndb(N),
		fail.
	do_benchmark(d3, _).

	do_benchmark(d4, N) :-
		{my_repeat(N)},
			database::self_dyndb(N),
		fail.
	do_benchmark(d4, _).

	do_benchmark(d5, N) :-
		{my_repeat(N)},
			database::obj_dyndb(N),
		fail.
	do_benchmark(d5, _).

:- end_object.
