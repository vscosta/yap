
:- protocol(find_rootp).

	:- info([
		version is 1.0,
		date is 2006/4/21,
		author is 'Paulo Nunes',
		comment is 'Default protocol for root find algorithms.']).

	:- public(find_root/5).
	:- mode(find_root(+object_identifier, +float, +float, +float, -float), one).
	:- info(find_root/5, [
		comment is 'Find the root of a function in the interval [A, B] given a maximum aproximation error.',
		argnames is ['Function', 'A', 'B', 'Error', 'Zero']]).

	:- public(find_root/6).
	:- mode(find_root(+object_identifier, +float, +float, +float, -float, -object_identifier), one).
	:- info(find_root/6, [
		comment is 'Find the root of a function in the interval [A, B] given a maximum aproximation error. Return the method used.',
		argnames is ['Function', 'A', 'B', 'Error', 'Zero', 'Method']]).

:- end_protocol.


:- protocol(functionp).

	:- info([
		version is 1.0,
		date is 2006/4/21,
		author is 'Paulo Nunes',
		comment is 'Default protocol for real functions of a single real variable.']).

	:- public(eval/2).
	:- mode(eval(+float, -float), one).
	:- info(eval/2, [
		comment is 'Calculate the function value.',
		argnames is ['X', 'Fx']]).

	:- public(evald/2).
	:- mode(evald(+float, -float), one).
	:- info(evald/2, [
		comment is 'Calculate the value of the function derivative.',
		argnames is ['X', 'DFx']]).

:- end_protocol.


:- object(f1,
	implements(functionp)).

	% x^2 - 4
	% 2.0 

	eval(X, Y) :-
		Y is X * X - 4.
	evald(X, Y) :-
		Y is 2 * X.

:- end_object.


:- object(f2,
	implements(functionp)).

	% x^7 + 9x^5 - 13x - 17
	% 1.29999999999945448
 
	eval(X, Y) :-
		Y is X**7 + 9*X**5 - 13*X - 17. 

	evald(X, Y) :-
		Y is 7*X**6 + 45*X**4 - 13. 

:- end_object.


:- object(f3,
	implements(functionp)).

	% (x - sqrt(2))^7
	% 1.41421356237309537
 
	eval(X, Y) :-
		Y is (X - sqrt(2.0))**8.

	evald(X, Y) :-
		Y is 8*(X - sqrt(2.0))**7. 

:- end_object.


:- object(function_root,
	implements(find_rootp)).

	:- info([
		version is 1.0,
		date is 2006/4/21,
		author is 'Paulo Nunes',
		comment is 'Multi-threading interface to root finding algorithms.']).

	:- threaded.

	find_root(Function, A, B, Error, Zero) :-
		find_root(Function, A, B, Error, Zero, _).

	find_root(Function, A, B, Error, Zero, Algorithm) :-
		threaded_call(
			(	try_method(bisection, Function, A, B, Error, Zero)
			;	try_method(newton, Function, A, B, Error, Zero)
			;	try_method(muller, Function, A, B, Error, Zero)
			)),
		threaded_exit(try_method(Algorithm, Function, A, B, Error, Zero)).

	try_method(Algorithm, Function, A, B, Error, Zero) :-
		Algorithm::find_root(Function, A, B, Error, Zero).

:- end_object.


:- object(bisection,
	implements(find_rootp)).

	:- info([
		version is 1.0,
		date is 2006/4/21,
		author is 'Paulo Nunes',
		comment is 'Bisection algorithm.']).

	find_root(Function, A, B, Error, Zero) :-
		Function::eval(A, Fa),
		Function::eval(B, Fb),
		(	Fa > 0.0, Fb < 0.0 ->
			true
		;	Fa < 0.0, Fb > 0.0
		),
		X0 is (A + B) / 2,
		Function::eval(X0, F0),
		bisection(Function, A, B, X0, F0, Error, Zero).

	bisection(_, _, _, Xn1, 0.0, _, Xn1) :-
		!.

	bisection(_, Xn1, Xn, _, _, Error, Xn1) :-
		abs(Xn1 - Xn) < Error,
		!.

	bisection(Function, An, Bn, _, _, Error, Zero) :-
		Xn1 is (An + Bn) / 2,
		Function::eval(Xn1, Fn1),
		Function::eval(An, FAn),
		(	Fn1*FAn < 0.0 ->
			An1 is An,
			Bn1 is Xn1
		;	An1 is Xn1,
			Bn1 is Bn
		),
		bisection(Function, An1, Bn1, Xn1, Fn1, Error, Zero).

:- end_object.


:- object(newton,
	implements(find_rootp)).

	:- info([
		version is 1.0,
		date is 2006/4/21,
		author is 'Paulo Nunes',
		comment is 'Newton algorithm.']).

	find_root(Function, Xa, Xb, Deviation, Zero) :-
		X0 is (Xa + Xb) / 2, 
		newton(Function, X0, Deviation, Zero).

	newton(Function, X0, Deviation, Zero) :-
		Xn1 is X0,
		Function::eval(Xn1, Fn1),
		Function::evald(Xn1, DFn1),
		Ac is -(Fn1 / DFn1), 
		newton(Function, Xn1, Deviation, Fn1, Ac, Zero).

	% test deviation
	newton(_, Xn1, Deviation, _, Ac, Xn1) :-
		abs(Ac) < Deviation,  
		!.

	% test solution
	newton(_, Xn1, _, 0.0, _, Xn1) :-
		!.

	% calc
	newton(Function, Xn, Deviation, _, Ac, Zero) :-
		Xn1 is Xn + Ac,
		Function::eval(Xn1, Fn1),
		Function::evald(Xn1, DFn1),
		Ac1 is (-(Fn1 / DFn1)), 
		newton(Function, Xn1, Deviation, Fn1, Ac1, Zero).			

:- end_object.


:- object(muller,
	implements(find_rootp)).

	:- info([
		version is 1.0,
		date is 2006/4/21,
		author is 'Paulo Nunes',
		comment is 'Muller algorithm.']).

	find_root(Function, Xa, Xb, Deviation, Zero) :-
		Xc is (Xa + Xb) / 2, 
		muller(Function, Xa, Xc, Xb, Deviation, Zero).

	muller(Function, Xa, Xb, Xc, Deviation, Zero) :-
		Function::eval(Xa, Ya),
		Function::eval(Xb, Yb),
		Function::eval(Xc, Yc),
		H1 is (Xb - Xa),
		DDba is ((Yb - Ya) / H1),
		Ac is (Deviation + 1),
		muller(Function, Xa, Xb, Xc, Deviation, Ya, Yb, Yc, Ac, H1, DDba, Zero).

	% complex
	muller(_, _, _, complex, _,  _, _, _, _, _, _, complex) :- 
		!.

	% test deviation
	muller(_, _, _, Xc, Deviation, _, _, _, Ac, _, _, Xc) :-
		abs(Ac) < Deviation,  
		!.

	% test solution
	muller(_, _, _, Xc, _, _, _, 0.0, _, _, _, Xc) :-
		!.

	% calc
	muller(Function, Xa, Xb, Xc, Deviation, _, Yb, Yc, _, _, DDba, Zero) :-
		H2n is (Xc - Xb),
		DDcbn is ((Yc - Yb) / H2n),
		Cn is ((DDcbn - DDba) / (Xc - Xa)),
		Bn is (DDcbn + H2n * Cn),
		Rn is (Bn * Bn - 4.0 * Yc * Cn),
		% complex
		% write(Rn),
		(	Rn < 0.0 ->
			muller(Function, _, _, complex, Deviation, _, _, _, _, _, _, Zero),
			!, fail
		;	V is sqrt(Rn)
		),
		(	Bn > 0.0 ->
			Dn is (Bn + V) 
		;	Dn is (Bn - V)
		),
		Acn is (-(2 * Yc / Dn)),
		Xan is Xb,
		Xbn is Xc,
		Xcn is Xc + Acn,

		Yan is Yb,
		Ybn is Yc,
		Function::eval(Xcn, Ycn),

		H1n is H2n,
		DDban is DDcbn,
		muller(Function, Xan, Xbn, Xcn, Deviation, Yan, Ybn, Ycn, Acn, H1n, DDban, Zero).

:- end_object.
