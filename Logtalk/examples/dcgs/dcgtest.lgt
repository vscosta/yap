
:- object(dcgtest).

	:- info([
		version is 1.0,
		date is 2004/9/27,
		author is 'Paulo Moura',
		comment is 'Test cases for the Logtalk DCG translator.']).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs the Logtalk DCG translator on the test cases.']).

	run :-
		case(N, Rule),
		write('Test '), write(N), write(':'), nl,
		write('  '), writeq(Rule), nl,
		(catch(expand_term(Rule, Clause), Error, write_error(Error)) ->
			write('  '), writeq(Clause)
			;
			write('  FAILED TRANSLATION')),
		nl, nl,
		fail.
	run.

	write_error(Error) :-
		write('  ERROR: '), writeq(Error), nl, fail.

	% basic tests
	case( 1, (p --> b)).
	case( 2, (p --> 3)).
	case( 3, (p(X) --> b(X))).

	% metacall test
	case( 9, (p --> _)).

	% terminal tests with list notation
	case(11, (p --> [])).
	case(12, (p --> [b])).
	case(13, (p --> [abc, xyz])).
	case(14, (p --> [abc | xyz])).
	case(15, (p --> [[], {}, 3, 3.2, a(b)])).
	case(16, (p --> [_])).

	% terminal tests with string notation
	case(17, (p --> "b")).
	case(18, (p --> "abc", "q")).
	case(19, (p --> "abc" ; "q")).

	% control construct tests
	case(21, (p --> b, c)).
	case(22, (p --> b ; c)).
	case(23, (p --> b -> c)).
	case(24, (p --> b -> c1, c2 ; d)).
	case(25, (p --> b -> c ; d1, d2)).
	case(26, (p --> b1, b2 -> c ; d)).
	case(27, (p --> q ; [])).
	case(28, (p --> [x] -> [] ; q)).
	case(29, (p --> [a] ; [b])).

	% negation tests
	case(31, (p --> \+ b, c)).
	case(32, (p --> b, \+ c, d)).

	% {}/1 tests
	case(41, (p --> {b})).
	case(42, (p --> {3})).
	case(43, (p --> {c,d})).
	case(44, (p --> '{}'((c,d)))).
	case(45, (p --> {a}, {b}, {c})).
	case(46, (p --> {q} -> [a] ; [b])).
	case(47, (p --> {q} -> [] ; b)).
	case(48, (p --> [foo], {write(x)}, [bar])).
	case(49, (p --> [foo], {write(hello)},{nl})).
	case(50, (p --> [foo], {write(hello), nl})).

	% cut tests
	case(51, (p --> !, [a])).
	case(52, (p --> b, !, c, d)).
	case(53, (p --> b, !, c ; d)).
	case(54, (p --> [a], !, {fail})).
	case(55, (p(a), [X] --> !, [X, a], q)).
	case(56, (p --> a, ! ; b)).

	% non-terminals corresponding to "graphic" characters or built-in operators 
	case(61, ('[' --> b, c)).
	case(62, ('=' --> b, c)).

	% more tests
	case(71, (p --> true, c)).
	case(72, (p --> fail, c)).
	case(73, (p(X) --> call(X), c)).

	% pushback tests
	case(81, (p, [t] --> b, c)).
	case(82, (p, [t] --> b, [t])).
	case(83, (p, [t] --> b, [s, t])).
	case(84, (p, [t] --> b, [s], [t])).
	case(85, (p(X, Y), [X, Y] --> [X, Y])).
	case(86, (p(a), [X] --> !, [X, a], q)).
	case(87, (p, [a,b] --> [foo], {write(hello), nl})).
	case(88, (p, [t], [t2] --> b, c)).
	case(89, (p, b --> c)).
	case(90, ([b], a(27) --> c)).

:- end_object.
