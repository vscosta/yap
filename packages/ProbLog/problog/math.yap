%========================================================================
%= Calculates the sigmoid function respectivly the inverse of it
%= warning: applying inv_sigm1oid to 0.0 or 1.0 will yield +/-inf
%=
%= +Float, -Float
%========================================================================

:- table smoothen/2.

smoothen(Pr, NPr) :-
    		(
	    Pr > 0.999
	    ->
		NPr = 0.999
			;
			Pr < 0.001
			       ->
				   NPr = 0.001 ;
					   NPr = Pr
				   ).

sigmoid(T,Sig) :-
    problog:problog_flag(sigmoid_slope,Slope),
    sigmoid(T,Slope,Sig).

sigmoid(-inf,_Slope,0.0) :- !.
sigmoid(+inf,_Slope,1.0) :- !.
sigmoid(Pr,Slope,Sig) :-
    Sig is 1/(1+exp(-Pr*Slope)).

inv_sigmoid(T,Sig) :-
    problog:problog_flag(sigmoid_slope,Slope),
    inv_sigmoid(T,Slope,Sig).

inv_sigmoid(1.0,_Slope,+inf) :- !.
inv_sigmoid(T,Slope,InvSig) :-
    InvSig is -log(1/T-1)/Slope.


