% FINITE and INFINITE DOMAINS		
% 910527 ECRC thom fruehwirth
% 910913 modified 
% 920409 element/3 added
% 920616 more CHIP predicates added
% 930726 started porting to CHR release
% 931014 mult/3 added for CHIC user meeting
% 931201 ported to CHR release
% 931208 removed special case of integer domain
% 940304 element/3 constraint loop fixed
% 961017 Christian Holzbaur SICStus mods
% 980714 Thom Fruehwirth, some updates reagrding alread_in*

% just quick port from Eclipse CHR library version
% does not take advantage of Sicstus CHR library features!
		
% Simplifies domains together with inequalities and some more CHIP predicates:
% 	element/3, atmost/3, alldistinct/1, circuit/1 and mult/3
% It also includes paired (!) domains (see element constraint)

:- use_module( library(chr)).
:- use_module( library('chr/getval')).
:- use_module( library(lists), [member/2,last/2]).

:- use_module( library(ordsets),
	[
          list_to_ord_set/2,
	  ord_intersection/3
        ]).

handler domain.

option(already_in_store, on).   
option(already_in_heads, off).   % see pragma already_in_heads
option(check_guard_bindings, off).

% for domain constraints
operator(700,xfx,'::').
operator(600,xfx,'..').
operator(600,xfx,':').  % clash with module operator?

% for inequality constraints
operator(700,xfx,lt).
operator(700,xfx,le).
operator(700,xfx,gt).
operator(700,xfx,ge).
operator(700,xfx,ne).

% X::Dom - X must be element of the finite or infinite domain Dom

% Domains can be either numbers (including arithemtic expressions)
% or arbitrary ground terms (!), the domain is set with setval(domain,Kind),
% where Kind is either number or term. Default for Kind is term.

:- setval(domain,term). 	% set default


% INEQUALITIES ===============================================================
% inequalities over numbers (including arithmetic expressions) or terms

constraints lt/2,le/2,ne/2.

A gt B :- B lt A.				% constraints gt/2,ge/2
A ge B :- B le A.
% some basic simplifications
A lt A <=> fail.
A le A <=> true.
A ne A <=> fail.
A lt B,B lt A <=> fail.
A le B,B le A <=> A=B.
A ne B \ B ne A <=> true.
% for number domain, allow arithmetic expressions in the arguments
A  lt  B <=> domain(number),ground(A),\+ number(A) | A1 is A, A1 lt B.
B  lt  A <=> domain(number),ground(A),\+ number(A) | A1 is A, B lt A1.
A  le  B <=> domain(number),ground(A),\+ number(A) | A1 is A, A1 le B.
B  le  A <=> domain(number),ground(A),\+ number(A) | A1 is A, B le A1.
A  ne  B <=> domain(number),ground(A),\+ number(A) | A1 is A, A1 ne B.
B  ne  A <=> domain(number),ground(A),\+ number(A) | A1 is A, B ne A1.
% use built-ins to solve the predicates if arguments are known
A  lt  B <=> ground(A),ground(B) | (domain(number) -> A < B ; A @< B).
A  le  B <=> ground(A),ground(B) | (domain(number) -> A =< B ; A @=< B).
A  ne  B <=> ground(A),ground(B) | (domain(number) -> A =\= B ; A \== B).



% FINITE and INFINITE DOMAINS ================================================

constraints (::)/2.

% enforce groundness of domain expression
 X::Dom <=> nonground(Dom) | 
        raise_exception( instantiation_error(X::Dom,2)).

constraints labeling/0.

labeling, (X::[Y|L]) # Ph <=> 
	member(X,[Y|L]), labeling
    pragma passive(Ph).

% binary search by splitting domain in halves
labeling, (X::Min:Max) # Ph <=> domain(number),Min+0.5<Max |  % ensure termination
	(integer(Min),integer(Max) ->  % assume we have integer domain
	Mid is (Min+Max)//2, Next is Mid+1
	;
	Mid is (Min+Max)/2, Next=Mid   % splitted domains overlap at Mid for floats
	),
	(
	X::Min:Mid
	;
	X::Next:Max
	% ;
	% Min+1>Max,	% for floats only, to get X also bound
	% X=Min		% or X=Max etc.
	),
	labeling
    pragma passive(Ph).

	nonground(X) :- ground(X), !, fail.
        nonground(_).

	domain(Kind) :- getval(domain,Kind).

% CHIP list shorthand for domain variables
% list must be known (end in the empty list)

 [X|L]::Dom <=> makedom([X|L],Dom).

	makedom([],D) :- true.
	makedom([X|L],D) :- 
                nonvar(L),
		X::D,
		makedom(L,D).


% Consecutive integer domain ---------------------------------------------
% X::Min..Max - X is an integer between the numbers Min and Max (included)
% constraint is mapped to enumeration domain constraint
 X::Min..Max <=> 
        Min0 is Min, 
        (Min0=:=round(float(Min0)) -> Min1 is integer(Min0) ; Min1 is integer(Min0+1)),
	Max1 is integer(Max),
	interval(Min1,Max1,L), 
	X::L.

 	interval(M,N,[M|Ns]):- 
		M<N, 
		!, 
		M1 is M+1, 
		interval(M1,N,Ns).
	interval(N,N,[N]).


% Enumeration domain -----------------------------------------------------

% X::Dom - X must be a ground term in the ascending sorted ground list Dom
 X::[A|L] <=> list_to_ord_set([A|L],SL), SL\==[A|L] | X::SL.
% for number domain, allow arithmetic expressions in domain
 X::[A|L] <=> domain(number), member(X,[A|L]), \+ number(X) |
		eval_list([A|L],L1),list_to_ord_set(L1,L2), X::L2.

	eval_list([],[]).
	eval_list([X|L1],[Y|L2]):-
		Y is X,
		eval_list(L1,L2).

% special cases
 X::[] <=> fail.				
 X::[Y] <=> X=Y.
 X::[A|L] <=> ground(X) | (member(X,[A|L]) -> true).

% intersection of domains for the same variable
% without pragma already_in_heads, needs already_in_store
 X::[A1|L1] \ X::[A2|L2] <=> 
    ord_intersection([A1|L1],[A2|L2],L),
    L \== [A2|L2]
    | 
    X::L.

% interaction with inequalities
 X::[A|L] \ X ne Y <=> integer(Y), remove(Y,[A|L],L1) | X::L1.
 X::[A|L] \ Y ne X <=> integer(Y), remove(Y,[A|L],L1) | X::L1.

 X::[A|L], Y le X ==> ground(Y), remove_lower(Y,[A|L],L1) | X::L1.
 X::[A|L], X le Y ==> ground(Y), remove_higher(Y,[A|L],L1) | X::L1.
 X::[A|L], Y lt X ==> ground(Y), remove_lower(Y,[A|L],L1),remove(Y,L1,L2) | X::L2.
 X::[A|L], X lt Y ==> ground(Y), remove_higher(Y,[A|L],L1),remove(Y,L1,L2) | X::L2.

% interaction with interval domain
 X::[A|L], X::Min:Max ==> remove_lower(Min,[A|L],L1),remove_higher(Max,L1,L2) | X::L2.

% propagation of bounds
 X le Y, Y::[A|L]   ==> var(X) | last([A|L],Max), X le Max.
 X le Y, X::[Min|_] ==> var(Y) | Min le Y.
 X lt Y, Y::[A|L]   ==> var(X) | last([A|L],Max), X lt Max.
 X lt Y, X::[Min|_] ==> var(Y) | Min lt Y.

% Interval domain ---------------------------------------------------------
% X::Min:Max - X must be a ground term between Min and Max (included)
% for number domain, allow for arithmetic expressions ind omain
% for integer domains, X::Min..Max should be used
 X::Min:Max <=> domain(number), \+ (number(Min),number(Max)) |
		Min1 is Min, Max1 is Max, X::Min1:Max1.
% special cases
 X::Min:Min <=> X=Min.
 X::Min:Max <=> (domain(number) -> Min>Max ; Min@>Max) | fail.
 X::Min:Max <=> ground(X) | 
		(domain(number) -> Min=<X,X=<Max ; Min@=<X,X@=<Max).
% intersection of domains for the same variable
% without pragma already_in_heads, needs already_in_store
 X::Min1:Max1 \ X::Min2:Max2 <=> maximum(Min1,Min2,Min),
	                         minimum(Max1,Max2,Max),
                                 (Min \== Min2  ; Max \== Max2 ) |
		X::Min:Max.

	minimum(A,B,C):- (domain(number) -> A<B ; A@<B) -> A=C ; B=C.
	maximum(A,B,C):- (domain(number) -> A<B ; A@<B) -> B=C ; A=C.

% interaction with inequalities
 X::Min:Max \ X ne Y <=> ground(Y),
	(domain(number) -> (Y<Min;Y>Max) ; (Y@<Min;Y@>Max)) | true.
 X::Min:Max \ Y ne X <=> ground(Y),
	(domain(number) -> (Y<Min;Y>Max) ; (Y@<Min;Y@>Max)) | true.
 X::Min1:Max \ Min2 le X <=> ground(Min2) , maximum(Min1,Min2,Min) | X::Min:Max.
 X::Min:Max1 \ X le Max2 <=> ground(Max2) , minimum(Max1,Max2,Max) | X::Min:Max.
 X::Min1:Max \ Min2 lt X <=> ground(Min2) , maximum(Min1,Min2,Min) |
		X::Min:Max, X ne Min.
 X::Min:Max1 \ X lt Max2 <=> ground(Max2) , minimum(Max1,Max2,Max) |
		X::Min:Max, X ne Max.
% propagation of bounds
 X le Y, Y::Min:Max ==> var(X) | X le Max.
 X le Y, X::Min:Max ==> var(Y) | Min le Y.
 X lt Y, Y::Min:Max ==> var(X) | X lt Max.
 X lt Y, X::Min:Max ==> var(Y) | Min lt Y.



% MULT/3 EXAMPLE EXTENSION ==================================================
% mult(X,Y,C) - integer X multiplied by integer Y gives the integer constant C.

constraints mult/3.

mult(X,Y,C) <=> ground(X) | (X=:=0 -> C=:=0 ; 0=:=C mod X, Y is C//X).
mult(Y,X,C) <=> ground(X) | (X=:=0 -> C=:=0 ; 0=:=C mod X, Y is C//X).
mult(X,Y,C), X::MinX:MaxX ==> 
	%(Dom=MinX:MaxX -> true ; Dom=[MinX|L],last(L,MaxX)),
	MinY is (C-1)//MaxX+1,
        MaxY is C//MinX,
	Y::MinY:MaxY.
mult(Y,X,C), X::MinX:MaxX ==>
	%(Dom=MinX:MaxX -> true ; Dom=[MinX|L],last(L,MaxX)),
	MinY is (C-1)//MaxX+1,
        MaxY is C//MinX,
	Y::MinY:MaxY.

/*
:- mult(X,Y,156),[X,Y]::2:156,X le Y.

X = X_g307
Y = Y_g331
 
Constraints:
(1) mult(X_g307, Y_g331, 156)
(7) Y_g331 :: 2 : 78
(8) X_g307 :: 2 : 78
(10) X_g307 le Y_g331

yes.
:- mult(X,Y,156),[X,Y]::2:156,X le Y,labeling.

X = 12
Y = 13     More? (;) 

X = 6
Y = 26     More? (;) 

X = 4
Y = 39     More? (;) 

X = 2
Y = 78     More? (;) 

X = 3
Y = 52     More? (;) 

no (more) solution.
*/




% CHIP ELEMENT/3 ============================================================
% translated to "pair domains", a very powerful extension of usual domains
% this version does not work with arithmetic expressions!

element(I,VL,V):- length(VL,N),interval(1,N,IL),gen_pair(IL,VL,BL), I-V::BL.

	gen_pair([],[],[]).
	gen_pair([A|L1],[B|L2],[A-B|L3]):-
		gen_pair(L1,L2,L3).

% special cases
 I-I::L <=> setof(X,member(X-X,L),L1), I::L1.
 I-V::L <=> ground(I) | setof(X,member(I-X,L),L1), V::L1.
 I-V::L <=> ground(V) | setof(X,member(X-V,L),L1), I::L1.
% intersections
 X::[A|L1], X-Y::L2 <=> intersect(I::[A|L1],I-V::L2,I-V::L3),
			length(L2,N2),length(L3,N3),N2>N3 | X-Y::L3.
 Y::[A|L1], X-Y::L2 <=> intersect(V::[A|L1],I-V::L2,I-V::L3),
			length(L2,N2),length(L3,N3),N2>N3 | X-Y::L3.
 X-Y::L1, Y-X::L2 <=> intersect(I-V::L1,V-I::L2,I-V::L3) | X-Y::L3.
 X-Y::L1, X-Y::L2 <=> intersect(I-V::L1,I-V::L2,I-V::L3) | X-Y::L3 pragma already_in_heads.

    intersect(A::L1,B::L2,C::L3):- setof(C,A^B^(member(A,L1),member(B,L2)),L3).

% inequalties with two common variables
 Y lt X, X-Y::L <=> A=R-S,setof(A,(member(A,L),R@< S),L1) | X-Y::L1.
 X lt Y, X-Y::L <=> A=R-S,setof(A,(member(A,L),S@< R),L1) | X-Y::L1.
 Y le X, X-Y::L <=> A=R-S,setof(A,(member(A,L),R@=<S),L1) | X-Y::L1.
 X le Y, X-Y::L <=> A=R-S,setof(A,(member(A,L),S@=<R),L1) | X-Y::L1.
 Y ne X, X-Y::L <=> A=R-S,setof(A,(member(A,L),R\==S),L1) | X-Y::L1.
 X ne Y, X-Y::L <=> A=R-S,setof(A,(member(A,L),S\==R),L1) | X-Y::L1.
% propagation between paired domains (path-consistency)
% X-Y::L1, Y-Z::L2 ==> intersect(A-B::L1,B-C::L2,A-C::L), X-Z::L.
% X-Y::L1, Z-Y::L2 ==> intersect(A-B::L1,C-B::L2,A-C::L), X-Z::L.
% X-Y::L1, X-Z::L2 ==> intersect(I-V::L1,I-W::L2,V-W::L), Y-Z::L.
% propagation to usual unary domains
 X-Y::L ==> A=R-S,setof(R,A^member(A,L),L1), X::L1,
	          setof(S,A^member(A,L),L2), Y::L2.



% ATMOST/3 ===================================================================

atmost(N,List,V):-length(List,K),atmost(N,List,V,K).

constraints atmost/4.

atmost(N,List,V,K) <=> K=<N | true.
atmost(0,List,V,K) <=> (ground(V);ground(List)) | outof(V,List).
atmost(N,List,V,K) <=> K>N,ground(V),delete_ground(X,List,L1) |
		(X==V -> N1 is N-1 ; N1=N),K1 is K-1, atmost(N1,L1,V,K1).

	delete_ground(X,List,L1):- delete(X,List,L1),ground(X),!.

delete( X, [X|Xs], Xs).
delete( Y, [X|Xs], [X|Xt]) :-
	delete( Y, Xs, Xt).


% ALLDISTINCT/1 ===============================================================
% uses ne/2 constraint

constraints alldistinct/1.

alldistinct([]) <=> true.
alldistinct([X]) <=> true.
alldistinct([X,Y]) <=> X ne Y.
alldistinct([A|L]) <=> delete_ground(X,[A|L],L1) | outof(X,L1),alldistinct(L1).

alldistinct([]).
alldistinct([X|L]):-
	outof(X,L),
	alldistinct(L).

outof(X,[]).
outof(X,[Y|L]):-
	X ne Y,
	outof(X,L).

constraints alldistinct1/2.
		
alldistinct1(R,[]) <=> true.
alldistinct1(R,[X]), X::[A|L] <=> ground(R) | 
			remove_list(R,[A|L],T), X::T.
alldistinct1(R,[X]) <=> (ground(R);ground(X)) | outof(X,R).	
alldistinct1(R,[A|L]) <=> ground(R),delete_ground(X,[A|L],L1) | 
			(member(X,R) -> fail ; alldistinct1([X|R],L1)).



% CIRCUIT/1 =================================================================

% constraints circuit1/1, circuit/1.
% uses list domains and ne/2


% lazy version

circuit1(L):-length(L,N),N>1,circuit1(N,L).

circuit1(2,[2,1]).
circuit1(N,L):- N>2,
		interval(1,N,D),
		T=..[f|L],
		domains1(1,D,L),
		alldistinct1([],L),
		no_subtours(N,1,T,[]).	

domains1(N,D,[]).
domains1(N,D,[X|L]):- 
		remove(N,D,DX),
		X::DX,
		N1 is N+1,
		domains1(N1,D,L).

no_subtours(0,N,L,R):- !.
no_subtours(K,N,L,R):- 
	outof(N,R),
	(var(N) -> freeze(N,no_subtours1(K,N,L,R)) ; no_subtours1(K,N,L,R)).
% no_subtours(K,N,T,R) \ no_subtours(K1,N,T,_) <=> K<K1 | true.

	no_subtours1(K,N,L,R):- 
		K>0,K1 is K-1,arg(N,L,A),no_subtours(K1,A,L,[N|R]).


% eager version

circuit(L):- length(L,N),N>1,circuit(N,L).

circuit(2,[2,1]).
%circuit(3,[2,3,1]).
%circuit(3,[3,1,2]).
circuit(N,L):- 	N>2,
		interval(1,N,D),
		T=..[f|L],
		N1 is N-1,
		domains(1,D,L,T,N1),
		alldistinct(L).		

domains(N,D,[],T,K).
domains(N,D,[X|L],T,K):- 
		remove(N,D,DX),
		X::DX,
		N1 is N+1,
		no_subtours(K,N,T,[]),		% unfolded
		%no_subtours1(K,X,T,[N]),		
		domains(N1,D,L,T,K).




% remove*/3 auxiliary predicates =============================================

remove(A,B,C):- 
	delete(A,B,C) -> true ; B=C.

remove_list(_,[],T):- !, T=[].
remove_list([],S,T):- S=T.
remove_list([X|R],[Y|S],T):- remove(X,[Y|S],S1),remove_list(R,S1,T).

remove_lower(_,[],L1):- !, L1=[].
remove_lower(Min,[X|L],L1):-
	X@<Min,
	!,
	remove_lower(Min,L,L1).
remove_lower(Min,[X|L],[X|L1]):-
	remove_lower(Min,L,L1).

remove_higher(_,[],L1):- !, L1=[].
remove_higher(Max,[X|L],L1):-
	X@>Max,
	!,
	remove_higher(Max,L,L1).
remove_higher(Max,[X|L],[X|L1]):-
	remove_higher(Max,L,L1).



% end of handler domain.chr =================================================
% ===========================================================================


