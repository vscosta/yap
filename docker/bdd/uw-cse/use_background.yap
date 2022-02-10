
target(advisedby(A,B),advisedby(Id,A,B),Id,(A,B)).

load_bg :-
	[misc],
	[unknown],
	[ai,graphics,language,systems,theory].

:- abolish(advisedby/2).

%	fold(I),
%	load_bg(I).

load_bg(0) :- [graphics,language,systems,theory].
load_bg(1) :- [ai,language,systems,theory].
load_bg(2) :- [ai,graphics,systems,theory].
load_bg(3) :- [ai,graphics,language,theory].
load_bg(4) :- [ai,graphics,language,systems].

:- load_bg.

nofpubs(A,N) :-
	setof(P,publication(P,A),L), !,
	length(L,N).
nofpubs(_,0).

commonpub(A,B) :-
	setof(B,P^(publication(P,A),publication(P,B)),L),
	member(B,L).

commonpubs(A,B,N) :-
	setof(P,(publication(P,A),publication(P,B)),L),
	length(L,N).

commonpubsratio(A,B,N) :-
	nofpubs(A,N0),
	commonpubs(A,B,N1),
	N is integer((N1*100)/N0).

allpublicationswith(X,Y) :- 
	nofpubs(X,N),
	commonpubs(X,Y,N).

geq(X,Y) :- var(Y), !, X = Y.
geq(X,Y) :- X >= Y.


diff(X,Y) :- X \= Y.

member(B,[B|_]).
member(B,[_|L]) :- member(B,L).

:- [-library(terms)].
