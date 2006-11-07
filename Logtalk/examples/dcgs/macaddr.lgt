
:- object(macaddr).

	:- public(valid/1).

	valid(Address) :-
		phrase(mac, Address).

	mac --> digits, ":", digits, ":", digits, ":", digits, ":", digits, ":", digits.

	digits --> digit, digit.

	digit --> [C], {0'0 =< C, C =< 0'9}.
	digit --> [C], {0'a =< C, C =< 0'f}.
	digit --> [C], {0'A =< C, C =< 0'F}.

:- end_object.
