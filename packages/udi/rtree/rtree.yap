:- load_foreign_files(['libudi_rtree'],[],udi_rtree_init).

:- op(700,xfx,'&&').

A '&&' B :-
        attributes:get_all_atts(A,C),
        attributes:put_att_term(A,overlap(C,B)).
