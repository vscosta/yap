% rational tree handler with diseqquality and OZ type constraint, intersection
% thom fruehwirth ECRC 1993,1995
%
% 
% 950519 added OZ type constraint, equalit ~ from tree.chr
% 980211 Thom Fruehwirth LMU for Sicstus CHR

:- use_module( library(chr)).

handler oztype.

option(debug_compile,on).
option(already_in_store, on). 
option(already_in_heads, off).
option(check_guard_bindings, off).

constraints (~)/2, (':<')/2, ('&&')/2.

operator(100,xfx,(~)).	   % equality
operator(100,xfx,(':<')).  % type constraint of Oz
operator(110,xfx,('&&')).  % type intersection, precedence choosen so that

% need global order on variables
:- use_module( library('chr/ordering'), [globalize/1,var_compare/3]).
% var is smaller than any non-var term
lt(X,Y):- (var(X),var(Y) -> globalize(X),globalize(Y),var_compare(<,X,Y) ; X@<Y).
le(X,Y):- (var(X) -> true ; X@=<Y).


% equality ~ -----------------------------------------------------------------
% can be optimised using list of variables that are equated instead of 
% seperate constraints, i.e. [X1,...Xn] ~ Term, to avoid dereferencing

ident @ T ~ T <=> true.
decompose @ T1 ~ T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),     
		T1=..[F|L1],T2=..[F|L2],
		equate(L1,L2).
orient @ T ~ X <=> lt(X,T) | X ~ T.   
simplify @ X ~ T1 \ X ~ T2 <=> le(T1,T2) | T1 ~ T2.

  same_functor(T1,T2):- functor(T1,F,N),functor(T2,F,N).

  equate([],[]).
  equate([X|L1],[Y|L2]):- X ~ Y, equate(L1,L2).


% type constraint :< ---------------------------------------------------------
% similar to equality ~ 
% plus standard axioms for order relation plus intersection &&
% types are not cyclic

type_identity  @ XT :< XT <=> true.
type_decompose @ T1 :< T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),
		T1=..[_|L1],T2=..[_|L2],
		contain(L1,L2).
type_simplify1 @ X ~ T1 \ X :< T2 <=> var(X) | T1 :< T2.
type_simplify2 @ X ~ T1 \ T2 :< X <=> var(X) | T2 :< T1.
type_transitiv @ T1 :< Y, Y :< T2 ==> var(Y) | T1 :< T2. 
type_intersect @ X :< T1, X :< T2 <=> nonvar(T1),nonvar(T2) | 
		same_functor(T1,T2),
		T1=..[F|L1],T2=..[F|L2],
		type_intersect(L1,L2,L3),
		T3=..[F|L3],
		X :< T3.

  contain([],[]).
  contain([X|L1],[Y|L2]):-
	X :< Y,
	contain(L1,L2).

  type_intersect([],[],[]).
  type_intersect([X|L1],[Y|L2],[Z|L3]):-
	Z~X&&Y,					% was Z :< X, Z :< Y before
	type_intersect(L1,L2,L3).

% X~Y&&Z parses as (X~Y)&&Z, so it does not match X~T
type_functional @ Z1~X&&Y \ Z2~X&&Y <=> Z1=Z2.
type_functional @ Z1~Y&&X \ Z2~X&&Y <=> Z1=Z2.
type_propagate  @ Z~X&&Y ==> Z :< X, Z :< Y.

/*
:- f(a,b):<f(X,X).			% succeeds -  X is a "top" ('a hole')
   a:<X,b:<X.
:- Y~f(U),Z~f(X),X:<Y,X:<Z.		% succeeds
   Y~f(U),Z~f(X),UX~X&&U,X:<f(UX),UX:<X,UX:<U,UX:<f(UX)
:- Y~f(U),U~a,Z~f(X),X:<Y,X:<Z.	        % fails
:- X:<Y,X~f(X),X:<f(Y).
   X~f(X), f(X):<Y			% simplifies nicely
:- X:<Y,Y~f(U),U~a,Z~f(X),X:<Z.	        % fails
:- X~Y,U:<X,Z:<a,U:<Z,Y:<b.		% fails
:- X:<Y,X:<Z,Y~a,Z~b.			% fails
:- X:<Y,X:<Z,Y~f(Y,U),Z~f(Z,V),U~a,V~b. % fails, loops without type_functional
:- X:<f(X,Y), X~f(X1,U), X1~f(X11,U1), U1~g(U), a:<U, b:<U. % succeeds
:- X~ f(X,Y), X~f(X1,U), X1~f(X11,U1), U1~g(U), a:<U, b:<U. % fails
*/


% end of handler oztype =======================================================
