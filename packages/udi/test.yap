:- nogc.

%% {A,B} :-
%%         {A},{B}.
overlap(A,B) :-
        attributes:get_all_atts(A,C),
        attributes:put_att_term(A,overlap(C,B)).

:- udi(rect(+,-)).
rect([0,0,2,2],1).
rect([5,5,7,7],2).
rect([8, 5, 9, 6],3).
rect([7, 1, 9, 2],4).

%:- overlap(R,[6, 4, 10, 6]), rect(R,ID).