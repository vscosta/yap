/**
domain(Queen,[1..8]).

X=8 v x in [1..7]
X=8 V (X=7 or X in [1..6])

X=8 V X=7 v X=6 ..


*/

:- use_module(library(matrix)).
:- use_module(library(satsolver)).
:- use_module(library(cnf)).
:- use_module(library(lists)).
:- use_module(library(maplist)).


main :-
    between(8,1000000,I), write(I), queens(I,L),
    foldl2(out(I),L,0,_,L,Out),
    writeln(Out).
main.

out(40,-H,I,I1,L,L) :-
    !,
    I1 is (I+1) mod N.
out(N,H,I,I1,[H-I],L,L) :-
    !,
    I1 is (I+1) mod N.

queens(N, ens) :-
    time(queens(N,LQueens,_)).


queens(N, LQueens, F) :-
    Queens <== matrix[N,N] of _,
    LQueens <== Queens.list(),
		       queens(N, _, Queens, F, []),
		       %length(F,NF),
		       ( sat(F)
		       ->
		       true  % ( verify(Queens,N) -> writeln(ok);writeln(bad))
		       ;
		       writeln(no_solution)
		       ).

count(N,M,K,T0) :-
    Queens <== matrix[N,N] of _,
    queens(N, _, Queens, F, []),
    satMulti(F,M,K,T0).

inc(I,I,I1) :- I1 is I+1.

add_clause(F0) :-
    maplist(eval,F0,F),
    solver_add_clause(F).

eval(X, MX) :- MX is X.

wb(I) :-U is I, write(U), write(' ').
		       

formula([],1).
formula([A|L],FA*FL) :-
    formulad(A,FA),
    formula(L,FL).

formulad([],0).
formulad([A|L],A+FL) :-
    formulad(L,FL).


    
queens(N,V,Queens) -->
    rows(0,N,V,Queens),
    columns(0,N,V,Queens),
    !,
    {N2 is 2*N},
    % 
    diags(0,N2,N,V,Queens),
    rdiags(-N,N,N,V,Queens).


columns(N,N,_,_) --> !.
columns(I,N,V,Queens) -->
    {Col <== Queens[_,I] }, 		      
    [Col],
    pairs(Col,V),
    {I1 is I+1},
    columns(I1,N,V,Queens).

rows(N,N,_,_) --> !.
rows(I,N,V,Queens) -->
    {Col <== Queens[I,_]}, 		      
    [Col],
    pairs(Col,V),
    {I1 is I+1},
    rows(I1,N,V,Queens).

diags(M,M,_,_,_) --> !.
diags(I,M,N,V,Queens) -->
{	fetch(0,N,I,Queens,Col,[])},
    pairs(Col,V),
    {I1 is I+1},
    diags(I1,M,N,V,Queens).
	

fetch(N,N,_,_Q) -->!.
fetch(I,N,M,Q) -->
    { J is M-I, J>=0, J<N,
      !,
      V <== Q[I,J], I1 is I+1 },
    [V],
    fetch(I1,N,M,Q).
fetch(I,N,M,Q) -->
    { I1 is I+1 },
fetch(I1,N,M,Q).


rdiags(M,M,_,_,_) --> !.
rdiags(I,M,N,V,Queens) -->
    {
	rfetch(0,N,I,Queens,Col,[])},
    pairs(Col,V),
    {I1 is I+1},
    rdiags(I1,M,N,V,Queens).
	

rfetch(N,N,_,_Q) -->!.
rfetch(I,N,M,Q) -->
    { J is I+M, J>=0, J<N,
      !,
      V <== Q[I,J], I1 is I+1 },
    [V],
    rfetch(I1,N,M,Q).
rfetch(I,N,M,Q) -->
    { I1 is I+1 },
    rfetch(I1,N,M,Q ).

pairs([],_) --> [].
pairs([H|L],V) -->
    mkpairs(L,H,V),
    pairs(L,V).

mkpairs([],_V,_) --> [].
mkpairs([H|L],V,F) -->
    [[-H,-V]],
    mkpairs(L,V,F).

print_queens(LQueens, N) :-
    rows(LQueens,N).

rows([],_) :- !.
rows(LQ,N) :-
    length(Hs,N),
    append(Hs, R, LQ),
    maplist(pq, Hs),
    nl,
    rows(R,N).

pq(0) :- write(' ').
pq(1) :- write('Q').
pq(-1) :- write('.').

verify(Mat,N) :-
    matrix_foldl(set_bit,Mat,[],Els),
    maplist(place, Els, Rows,Cols,Ds,RDs),
    sort(Rows,SRo), length(SRo,N),
    sort(Cols,SCo), length(SCo,N),
    sort(Ds,SDi), length(SDi,N),
    sort(RDs,SRDi), length(SRDi,N).

set_bit(M,Els,[X-Y|Els],[X,Y]) :-
    V <== M[X,Y],
	V>0,
	!.
set_bit(_,Els,Els,_).

place(X-Y,X,Y,XPY,XMY) :-
    XPY is X+Y,
    XMY is X-Y.

allqueens(N, NS, T) :-
    Queens <== matrix[N,N] of _,
    LQueens <== Queens.list(),
		       queens(N, _, Queens, F, []),
		       %% length(F,NF),
%% 		       length(LQueens,NQ),
%% 		       tell(o),
%% 		       format('p cnf ~d ~d~n',[NQ,NF]),
%% 		       (foldl(inc,LQueens,1,_),member(C,F),maplist(wb,C),writeln(0),fail;true),
%% 		       told,
%% %		       formula(F,RF),
		       satMulti(F,100000,NS,T).
