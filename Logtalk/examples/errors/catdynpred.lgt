
:- category(catdynpred).

	% dynamic predicates may be declared inside categories but ...
	:- public(dynpred/1).
	:- dynamic(dynpred/1).

	% ... clauses for dynamic predicates are not allowed
	dynpred(1).
	dynpred(2).
	dynpred(3).

:- end_object.
