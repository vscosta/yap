:- op(600,xfy,::).
:- op(600,fy,::).
:- op(600,fx,^^).
symdiffp0_(symdiffp0__dcl).
symdiffp0__dcl(diff(_2249),p(p(p)),static,no).
symdiffp0__dcl(simplify(_2249),p(p(p)),static,no).
symdiffp0__dcl(_2247,_2248,_2249,_2250,symdiffp):-symdiffp0__dcl(_2247,_2248,_2249,_2250).
:- initialization((lgt_assert_relation_clauses([lgt_current_protocol_(symdiffp,symdiffp0_)]))).
