
:- object(joeSportsman,
	extends(joePerson)).


    :- public(sport/1).
	:- public(stamina/1).
	:- public(weight/1).


    sport(snowboard).

	stamina(30).

	weight(111).


:- end_object.
