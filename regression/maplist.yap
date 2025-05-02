:- ['../library/maplist'].


eqall(L1,L2) :- maplist(=,L1,L2).

eqall([X,Y,Z], LF).
?- eqall([1,2,3],L).

zero(0).
one(1).


g(N,Ones)  :- length(Ones,N), maplist(one,Ones).

zeros(L) :- maplist(zero,L).

?-  zeros([_,0,_,0]).

?-  zeros([_,0,_,0]).

?-  zeros([_,_,_|_]).


