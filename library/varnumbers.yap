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
	unnumbervars(GT, VT).
