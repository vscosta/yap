
:- module(evidence, [add_to_evidence/1,
		     execute_pre_evidence/0
		    ]).

% declare some new evidence.

add_to_evidence(G2) :-
	recordzifnot('$evidence',G2,_),
	fail.
add_to_evidence(_).

% use it at query evaluation time.

execute_pre_evidence :-
	findall(G, recorded('$evidence', G, _), LGs),
	execute_all(LGs).

execute_all([]).
execute_all([M:G|Gs]) :-
	call(M:G),
	execute_all(Gs).



