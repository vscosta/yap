% Slim Abdennadher, Thom fruehwirth, LMU, July 1998
% Straightforward Fourier Solver for linear inequations
% may loop because of producing more and mor eredundant equations
% compare to gauss.pl and fougau.pl

:- use_module(library(chr)).
:- ['math-utilities.pl'].           % load auxiliary file
:- use_module( library(lists), [member/2, memberchk/2,select/3]).

handler gauss.

option(check_guard_bindings, on).  % for delete(X...) 
option(already_in_store, off).
option(already_in_heads, off).

operator(100,xfx,leq).

constraints (leq)/2.

redundant @ 
[X*Coeff1|P1] leq C1 \ P leq C2 <=> 
        delete(X*Coeff2,P,P2), 
        is_div(Coeff2,Coeff1,C),
	C < 0,
        mult_const(eq0(C1,P1),C,eq0(C1C,P1C)),  
        add_eq0(eq0(C2,P2),eq0(C1C,P1C),eq0(C3,P3)),
	P3=[], 0 >= C3
        |
        true.

propagate(X) @ 
[X*Coeff1|P1] leq C1, P leq C2 ==> 
        delete(X*Coeff2,P,P2), 
        is_div(Coeff2,Coeff1,C), 
        C > 0 
        | 
        mult_const(eq0(C1,P1),C,eq0(C1C,P1C)),  
        add_eq0(eq0(C2,P2),eq0(C1C,P1C),eq0(C3,P3)),
        P3 leq C3.

zero @ [] leq C1 <=> 0 =< C1.


constraints {}/1.    
% curly brackets as wrapper to avoid name clash with built-in =:= etc.

split @ { C, Cs } <=> { C }, { Cs }.

normalize @ {A >= B} <=> {B =< A}.   	
normalize @ {A =:= B} <=> {A >= B}, {B =< A}.   	
normalize @ {A =< B} <=> 
	normalize(A,B,Poly,Const), 
	Poly leq Const.   	


/*

3 * X + 2 * Y - 4 * (3 + Z) =:= 2 * (X - 3) + (Y + Z) * 7 ,      
        2 * (X + Y + Z) =:= 3 * (X - Y - Z) , 
        5 * (X + Y) - 7 * X - Z =:= (2 + 1 + X) * 6.
*/



