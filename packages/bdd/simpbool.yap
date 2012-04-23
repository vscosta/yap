
%
% simplify a boolean expression of the form A*B+C*D...
%

:- module(simplify_boolean, 
	[simplify_exp/2]).

%simplify_exp(V,V) :- writeln(V), fail, !.
simplify_exp(V,V) :- var(V), !.
simplify_exp(S1+S2,NS) :- !,
	simplify_exp(S1, SS1),
	simplify_exp(S2, SS2),
	simplify_sum(SS1, SS2, NS).
simplify_exp(S1*S2,NS) :- !,
	simplify_exp(S1, SS1),
	simplify_exp(S2, SS2),
	simplify_prod(SS1, SS2, NS).
simplify_exp(not(S),NS) :- !,
	simplify_exp(S, SS),
	simplify_not(SS, NS).
simplify_exp(S,S).

simplify_sum(V1, V2, O) :- 
	( var(V1) ->
	    ( var(V2) ->
		( V1 == V2 -> O = V1 ; O = V1+V2 ) ; /* var(V1) , var(V2) */
		( V2 == 0 -> O = V1 ; V2 == 1 -> O = 1 ; O = V1+V2 ) /* var(V1) , nonvar(V2) */
	    ) ;
	    ( var(V2) ->
		( V1 == 0 -> O = V2 ; V1 == 1 -> O = 1 ; O = V1+V2 ) ; /* nonvar(V1) , var(V2) */
		( V2 == 0 -> O = V1 ; V2 == 1 -> O = 1 ; V1 == 0 -> O = V2 ; V1 == 1 -> O = 1; O = V1+V2 ) /* nonvar(V1) , nonvar(V2) */
	    )
	).

simplify_prod(V1, V2, O) :- 
	( var(V1) ->
	    ( var(V2) ->
		( V1 == V2 -> O = V1 ; O = V1*V2 ) ; /* var(V1) , var(V2) */
		( V2 == 0 -> O = 0 ; V2 == 1 -> O = V1 ; O = V1*V2 ) /* var(V1) , nonvar(V2) */
	    ) ;
	    ( var(V2) ->
		( V1 == 0 -> O = 0 ; V1 == 1 -> O = V2 ; O = V1*V2 ) ; /* nonvar(V1) , var(V2) */
		( V2 == 0 -> O = 0 ; V2 == 1 -> O = V1 ; V1 == 0 -> O = 0 ; V1 == 1 -> O = V2; V1 == V2 -> O = V1 ; O = V1*V2 ) /* nonvar(V1) , nonvar(V2) */
	    )
	).


simplify_not(V, not(V)) :- var(V), !.
simplify_not(0, 1) :- !.
simplify_not(1, 0) :- !.
simplify_not(SS, not(SS)).


