:- nogc.

overlap(A,B) :-
        attributes:get_all_atts(A,C),
        attributes:put_att_term(A,overlap(C,B)).

:- udi(rect(-,+)).

:- ['roads.yap'].

r(ID1,ID2) :-
        rect(ID1,R1),
        overlap(R2,R1),
        rect(ID2,R2).