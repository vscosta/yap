%   File   : varnumbers.yap
%   Author : Vitor Santos Costa
%   Updated: 2006
%   Purpose: opposite to numbervars

:- module(varnumbers, [
		  numbervars/1,
		  max_var_number/3,
		  varnumbers/2
              ]).


numbervars(Term) :-
	numbervars(Term, 0, _).

max_var_number(V,Max,Max) :- var(V), !.
max_var_number('$VAR'(I),Max0,Max) :- !,
	Max is max(I,Max0).
max_var_number(S,Max0,Max) :-
	functor(S,_,Ar),
	max_var_numberl(0,Ar,S,Max0,Max).

max_var_numberl(I0,Ar,T,Max0,Max) :-
	( I0 =:= Ar ->
	    Max0 = Max
	;
	    I is I0+1,
	    arg(I,T,A),
	    max_var_number(A,Max0,MaxI),
	    max_var_numberl(I,Ar,T,MaxI,Max)
	).

varnumbers(GT, VT) :-
	max_var_number(GT,0,Max),
	Max1 is Max+1,
	functor(Vars,vars,Max1),
	varnumbers(GT, Vars, VT).

varnumbers(V,_,V) :- var(V), !.
varnumbers('$VAR'(I),Vs,V) :- !,
	I1 is I+1,
	arg(I1,Vs,V).
varnumbers(S,Vs,NS) :-
	functor(S,N,Ar),
	functor(NS,N,Ar),
	varnumbersl(0,Ar,Vs,S,NS).

varnumbersl(I0,Ar,Vs,S,NS) :-
	(I0 =:= Ar ->
	    true
	;
	    I is I0+1,
	    arg(I,S,A),
	    arg(I,NS,NA),
	    varnumbers(A,Vs,NA),
	    varnumbersl(I,Ar,Vs,S,NS)
	).



