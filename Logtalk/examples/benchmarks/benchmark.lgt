
% benchmark a goal using a default number of repetitions and printing some 
% useful statistics

benchmark(Goal) :-
	N = 100000,
	benchmark(Goal, N, Looptime, Goaltime, Average, Speed),
	report(Goal, N, Looptime, Goaltime, Average, Speed).


benchmark(Goal, N) :-
	benchmark(Goal, N, Looptime, Goaltime, Average, Speed),
	report(Goal, N, Looptime, Goaltime, Average, Speed).
	

benchmark(Goal, N, Looptime, Goaltime, Average, Speed) :-
	'$lgt_cpu_time'(Seconds1),		% defined in the config files
	do_benchmark(N, true),
	'$lgt_cpu_time'(Seconds2),
	Looptime is Seconds2 - Seconds1,
	'$lgt_cpu_time'(Seconds3),
	do_benchmark(N, Goal),
	'$lgt_cpu_time'(Seconds4),
	Goaltime is Seconds4 - Seconds3,
	Average is (Goaltime - Looptime)/N,
	Speed is 1.0/Average.


report(Id, Goal, N, Looptime, Goaltime, Average, Speed) :-
	write(Id), write(': '),
	report(Goal, N, Looptime, Goaltime, Average, Speed).


report(Goal, N, Looptime, Goaltime, Average, Speed) :-
	writeq(Goal), nl,
	write('Number of repetitions: '), write(N), nl,
	write('Loop time: '), write(Looptime), nl,
	write('Goal time: '), write(Goaltime), nl,
	write('Average time per call: '), write(Average), nl,
	write('Number of calls per second: '), write(Speed), nl,
	nl.


% repeat a goal N times using a failure-driven loop to avoid the interference 
% of Prolog compiler memory management mechanism (such as garbage collection) 
% on the results

do_benchmark(N, Goal) :-
	repeat(N),		% another option would be to use a between/3 built-in predicate
		call(Goal),
	fail.

do_benchmark(_, _).


% some Prolog compilers define the predicate repeat/1 as a built-in predicate;
% if that's the case of the Prolog compiler you are using, then comment out 
% the definition that follows

repeat(_).

repeat(N) :-
	N > 1,
	N2 is N - 1,
	repeat(N2).


% generate a list containing the first N non-negative integers

generate_list(N, List) :-
	N >= 0,
	generate_list(0, N, List).


generate_list(N, N, []) :-
	!.

generate_list(M, N, [M| Ms]) :-
	M < N,
	M2 is M + 1,
	generate_list(M2, N, Ms).


% utility predicate for running all benchmark tests

benchmarks :-
	N = 100000,
	benchmark_goal(Id, Goal),
	benchmark(Goal, N, Looptime, Goaltime, Average, Speed),
	report(Id, Goal, N, Looptime, Goaltime, Average, Speed),
	fail.

benchmarks.


% some benchmark tests for static code:

benchmark_goal('S1', my_length(List, _)) :-
	generate_list(30, List).

benchmark_goal('S2', object::length(List, _)) :-
	generate_list(30, List).

benchmark_goal('S3', '$lgt_send_to_object_nv'(object, length(List, _), user)) :-
	generate_list(30, List).

benchmark_goal('S4', '$lgt_send_to_object_ne_nv'(object, length(List, _), user)) :-
	generate_list(30, List).


% some benchmark tests for dynamic code:

benchmark_goal('D1', (create_object(xpto, [], [], []), abolish_object(xpto))).

benchmark_goal('D2', db_test_plain).

benchmark_goal('D3', '$lgt_send_to_object_ne_nv'(database, db_test_this, user)).

benchmark_goal('D4', '$lgt_send_to_object_ne_nv'(database, db_test_self, user)).

benchmark_goal('D5', '$lgt_send_to_object_ne_nv'(database, db_test_obj, user)).

