
:- object(distributor,
	extends(sparking)).


	fault(f1001, 'Condensation in the distributor cap').
	fault(f1002, 'Faulty distributor arm').
	fault(f1003, 'Worn distributor brushes').

	symptom(s1001, 'The starter turns but the engine doesnt fire').
	symptom(s1002, 'The engine has difficulty starting').
	symptom(s1003, 'The engine cuts out shortly after starting').
	symptom(s1004, 'The engine cuts out at speed').

	effect(f1001, s1001).
	effect(f1002, s1001).
	effect(f1002, s1004).
	effect(f1003, s1002).
	effect(f1003, s1003).

	contrary(s1002, s1001).
	contrary(s1003, s1001).


:- end_object.
