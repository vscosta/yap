
:- object(calculator,
	implements(parsep)).


	parse(Expression, Value) :-
		phrase(expr(Value), Expression).


	expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
	expr(Z) --> term(X), "-", expr(Y), {Z is X - Y}.
	expr(X) --> term(X).

	term(Z) --> number(X), "*", term(Y), {Z is X * Y}.
	term(Z) --> number(X), "/", term(Y), {Z is X / Y}.
	term(Z) --> number(Z).

	number(C) --> "+", number(C).
	number(C) --> "-", number(X), {C is -X}.
	number(X) --> [C], {0'0 =< C, C =< 0'9, X is C - 0'0}.


:- end_object.
