% Prolog term manipulation as constraints
% 931127 ECRC, thom fruehwirth based on ideas from 9203 and 9104
% 980207, 980312 thom fruehwirth LMU adapted for Sicstus CHR

:- use_module(library(chr)).

handler term.

option(already_in_store, off). 
option(already_in_heads, off).
option(check_guard_bindings, off). 

operator(100,xfx,unif).

T unif [F|L] :- chr_unif(T,F,L).

constraints chr_functor/3, chr_arg/3, chr_unif/3.

chr_functor(T,F,N) <=> (nonvar(T);nonvar(F),nonvar(N)) | functor(T,F,N). 
chr_functor(T,T,N) ==> N=0.
chr_functor(T,F,0) ==> T=F.
chr_functor(T,F,N) ==> chr_nonvar(T).
chr_functor(T,F,N) \ chr_functor(T,F1,N1) <=> F1=F,N1=N.
chr_functor(T,F,N), chr_arg(M,T,A) ==> nonvar(N),nonvar(M) | N>=M,N>0.

chr_arg(0,T,A) <=> fail.
chr_arg(N,T,A) <=> nonvar(N),nonvar(T) | arg(N,T,A).
chr_arg(N,T,A) \ chr_arg(N,T,A1) <=> A1=A.

chr_unif(T,F,L) <=> (nonvar(T);nonvar(F),islist(L)) | T=..[F|L].
chr_unif(T,T,L) ==> L=[].
chr_unif(T,F,[]) ==> T=F.
chr_unif(T,F,L) ==> chr_nonvar(T),chr_nonvar(L).
chr_unif(T,F,L) \ chr_unif(T,F1,L1) <=> F1=F,L1=L.
chr_unif(T,F,L) \ chr_unif(T1,F,L) <=> T1=T.
chr_unif(T,F,L) \ chr_functor(T,F1,N) <=> (nonvar(N);islist(L)) | F1=F,length(L,N).
chr_unif(T,F,L) \ chr_arg(M,T,A) <=> nonvar(M) | nth_member(M,L,A).
 
 
	nth_member(1,[X|_],X).
	nth_member(N,[_|L],X):-
		gt(N,1), plus(M,1,N), nth_member(M,L,X).

	islist([]) ?- true.
	islist([X|L]) ?- 
		islist(L).


constraints chr_var/1, chr_nonvar/1.

chr_var(X) <=> nonvar(X) | fail.
chr_nonvar(X) <=> nonvar(X) | true.
chr_nonvar(X), chr_var(X) <=> fail.
chr_var(X) \ chr_var(X) <=> true.
chr_nonvar(X) \ chr_nonvar(X) <=> true.


constraints plus/3, gt/2.

plus(A,B,C) <=> nonvar(A),nonvar(B) | C is A+B.
plus(A,B,C) <=> nonvar(A),nonvar(C) | B is C-A.
plus(A,B,C) <=> nonvar(B),nonvar(C) | A is C-B.

gt(A,B) <=> nonvar(A),nonvar(B) | A>B.




% Examples =================================================================
% these standard predicates using term manipulation run now backwards as well
% but sometimes this causes nontermination

% constraints needed in the examples

constraints diff/2, diff_list/2.

diff(X,X) <=> fail.

diff_list(V,[]) <=> true.
diff_list(V,L) <=> member(X,L),V==X | fail.

member(X,[Y|L]):- X=Y ; member(X,L).


% two variants of unification by sterling/shapiro

unify1(X,Y):- chr_var(X),chr_var(Y), X=Y.
unify1(X,Y):- chr_var(X),chr_nonvar(Y), X=Y.
unify1(X,Y):- chr_nonvar(X),chr_var(Y), X=Y.
unify1(X,Y):- % chr_nonvar(X),chr_nonvar(Y), 
	chr_functor(X,F,N),chr_functor(Y,F,N),
	unify_args(N,X,Y).

unify_args(0,X,Y).
unify_args(N,X,Y):-
	gt(N,0), 
	plus(N1,1,N),
	chr_arg(N,X,A),chr_arg(N,Y,B),
	unify1(A,B),
	unify_args(N1,X,Y).

/*
| ?- unify1(a,b).

no

| ?- unify1(A,B).

B = A,
chr_var(A) ? ;

B = A,
chr_nonvar(A),
chr_functor(A,A,0) ? ;

chr_nonvar(A),
chr_nonvar(B),
chr_var(_A),
chr_functor(A,_B,1),
chr_functor(B,_B,1),
chr_arg(1,A,_A),
chr_arg(1,B,_A) ? 

| ?- unify1(f(a,B),f(B,C)).

B = a,
C = a ? ;
% nontermination
*/

unify2(X,Y):- chr_var(X),chr_var(Y), X=Y.
unify2(X,Y):- chr_var(X),chr_nonvar(Y), X=Y.
unify2(X,Y):- chr_nonvar(X),chr_var(Y), Y=X.
unify2(X,Y):- % chr_nonvar(X),chr_nonvar(Y), 
	X unif [F|As],Y unif [F|Bs],
	unify_list(As,Bs).

unify_list([],[]).
unify_list([A|As],[B|Bs]):-
	unify2(A,B),		
	unify_list(As,Bs).

/*
| ?- unify2(A,B).

B = A,
chr_var(A) ? ;

B = A,
chr_nonvar(A),
chr_unif(A,A,[]) ? ;

B = A,
chr_nonvar(A),
chr_var(_A),
chr_unif(A,_B,[_A]) ? ;

B = A,
chr_nonvar(A),
chr_var(_A),
chr_var(_B),
chr_unif(A,_C,[_A,_B]) ? 

| ?- unify2(f(a,B),f(B,C)).

B = a,
C = a ? ;

no
*/


% collecting the variables of a term into a list, groundness and more	

varlist(A,Vars):- varlist(A,[],Vars).

	varlist(V,L,[V|L]):- chr_var(V),diff_list(V,L).
	varlist(V,L,L):- chr_var(V),member(V,L).
	varlist(T,L1,L2):-
		%chr_nonvar(T),
		chr_functor(T,_,N),
		varlist(N,T,L1,L2).

	varlist(0,T,L,L).
	varlist(N,T,L1,L3) :-
		gt(N,0), 
		plus(K,1,N),
		chr_arg(N,T,TK),
		varlist(TK,L1,L2),
		varlist(K,T,L2,L3).

/*
| ?- varlist(f(a,B),L).

L = [B],
chr_var(B) ? ;

L = [],
chr_nonvar(B),
chr_functor(B,B,0) ? ;

L = [_A],
chr_nonvar(B),
chr_var(_A),
chr_functor(B,_B,1),
chr_arg(1,B,_A) ?

| ?- varlist(X,[A,B]).

chr_nonvar(X),
chr_var(B),
chr_var(A),
diff_list(A,[B]),
chr_functor(X,_A,2),
chr_arg(2,X,B),
chr_arg(1,X,A) ? ;
% nontermination
*/

common_var(A,K,V1):-
	varlist(A,AV), varlist(K,KV), member(V,AV), member(V,KV).

ground0(A):- varlist(A,[]).
/*
% termination problems
*/

ground1(T):-
     %chr_nonvar(T),
     chr_functor(T, _, N),
     ground1(N, T).

ground1(0, _).
ground1(N, T):-
     gt(N,0),
     plus(N1,1,N),
     chr_arg(N, T, A), 
     ground1(A), 
     ground1(N1, T).

/*
| ?- ground1(h(A,b,C)).

chr_nonvar(C),
chr_nonvar(A),
chr_functor(C,C,0),
chr_functor(A,A,0) ? ;

chr_nonvar(C),
chr_nonvar(A),
chr_nonvar(_A),
chr_functor(C,C,0),
chr_functor(A,_B,1),
chr_arg(1,A,_A),
chr_functor(_A,_A,0) ? 
*/

ground2(T) :- 
	%chr_nonvar(T), 
	T unif [_|Args], 
	ground2l(Args).

ground2l([]).
ground2l([H|L]) :- ground2(H), ground2l(L).

/*
| ?- ground2(A).

chr_nonvar(A),
chr_unif(A,A,[]) ? ;

chr_nonvar(A),
chr_nonvar(_A),
chr_unif(A,_B,[_A]),
chr_unif(_A,_A,[]) ? 
*/

number_vars(Term,N0,N1) :- 
	var(Term),  	% chr_var(Term) would fail later
	plus(N0,1,N1), 
	name(N0,Digits), 
	name('V',[C]), 
	name(Term,[C|Digits]).
number_vars(Term,N0,N1) :- 
	%chr_nonvar(Term),
	Term unif [_|Args], 
	number_list(Args,N0,N1).

number_list([],N0,N0).
number_list([H|T],N0,N2) :- number_vars(H,N0,N1), number_list(T,N1,N2).


undupvar(A,B,R,L):- undupvar(A,B,[],R,[],L).

	undupvar(V,V,R,[V|R],L,L):- chr_var(V),diff_list(V,R).
	undupvar(V,W,R,R,L,[W=V|L]):- chr_var(V),member(V,R).
	undupvar(T,S,R1,R3,L1,L3):-
		%chr_nonvar(T),chr_nonvar(S),
		chr_functor(T,F,N),chr_functor(S,F,N),
		undupvar(N,T,S,R1,R3,L1,L3).

	undupvar(0,T,S,R,R,L,L).
	undupvar(N,T,S,R1,R3,L1,L3):-
		gt(N,0), 
		plus(M,1,N),
		chr_arg(N,T,TK),
		chr_arg(N,S,TS),
		undupvar(TK,TS,R1,R2,L1,L2),
		undupvar(M,T,S,R2,R3,L2,L3).



% from comp.lang.prolog on a sequent calculus implementation

% substitute(P, X, Y, Q) substitutes instances of X in P with Y, producing Q.

substitute(P1, K1, K2, P2) :-
        P1 = K1,  P2 = K2
          ; 
	diff(P1,K1),
	%chr_nonvar(P1),chr_nonvar(P2),
	P1 unif [F|Args1],
        P2 unif [F|Args2],
        substitute_list(Args1, K1, K2, Args2).

substitute_list([], _, _, []).
substitute_list([H1|T1], K1, K2, [H2|T2]) :-
          substitute(H1, K1, K2, H2),
          substitute_list(T1, K1, K2, T2).



% from comp.lang.prolog on heaps and trees
% uses is/2

%pos(Head,t(Head,Rel,L,[],0)-[], Nc, N0-N2):-     /* leaf node         */
%     atomic(Head), 
%	!,
%     string_length1(Head,L), 
%     N2 is N0+L,
%     Rel is L//2,                                /* middle of the node */
%     Nc  is (N0+N2)//2.                          /* center over node   */
pos(X,t(Head,Rel,L,Centers,Adj)-A, Nc, N0-N2):-  /* non-leaf node      */
     %chr_nonvar(X),
     X unif [Head|Args],
     pos_list(Args,A,Centers,N0-N1),
     string_length1(Head,L), 
     posdiff(N1-N0,L,Error),
     Adj is (Error+((N1-N0) mod 2))//2,
     N2 is N1+Error,
     Rel is L//2,                                /* middle of the node */
     Nc  is (N0+N2)//2.

pos_list([],   [],      [],         N-N).
%pos_list([H],  [A],     [Center],   N-N1) :- !, 
%     pos(H,A,Center,N-N1).
pos_list([H|T],[A|Args],[C|Centers],N0-Nn):-
     pos( H,    A,       C,         N0-N1),
     plus(N1,2,N2),  %N2 is N1+2, 
     pos_list(T,Args,Centers,N2-Nn).

string_length1(X,L):- atomic(X), name(X,S), length(S,L).

posdiff(Expr,L,0):- Adj is L-Expr, Adj =< 0.
posdiff(Expr,L,Adj):- Adj is L-Expr, Adj > 0.



% lsu(A,B,G): the least specific unifier of A and B is G
% joachims schimpfs code modified by thom
	
lsu(A, B, G) :-
        map(A, B, G, [], Map),
        sort(0, =<, Map, SortedMap),
        unify_duplicates(SortedMap).

map(A, B, G, Map, NewMap) :-
        %chr_nonvar(A),chr_nonvar(B),
        chr_functor(A, Name, Arity),
        chr_functor(B, Name, Arity),
        chr_functor(G, Name, Arity),
        map_arg(A, B, G, Map, NewMap, Arity-0).
map(A, B, G, Map, [subst(A, B, G)| Map]):-
	chr_var(A)
	;
	chr_var(B)
	;
        %chr_nonvar(A),chr_nonvar(B),
	chr_functor(A, Name1, Arity1),
        chr_functor(B, Name2, Arity2),
	(diff(Name1,Name2);diff(Arity1,Arity2)).

map_arg(A, B, G, Map, NewMap, Ar-N) :-
	Ar=N,
        Map = NewMap.
map_arg(A, B, G, Map0, NewMap, Ar-N) :-
	gt(Ar,N),
        plus(N,1,N1),
        chr_arg(N1, A, An),
        chr_arg(N1, B, Bn),
        chr_arg(N1, G, Gn),
        map(An, Bn, Gn, Map0, Map1),
        map_arg(A, B, G, Map1, NewMap, Ar-N1).

unify_duplicates([subst(A1, B1, G1)|T]) :-
        T = [subst(A2, B2, G2)|_],
        ( A1 = A2, B1 = B2, G1 = G2 ; diff(A1,A2) ; diff(B1,B2)),
        unify_duplicates(T).
unify_duplicates([T]).
unify_duplicates([]).




% end of handler term
