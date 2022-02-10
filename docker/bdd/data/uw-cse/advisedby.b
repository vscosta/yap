:- set(clauselength,10).
%:- set(depth, 301).
:- set(i,5).
:- set(noise,1000).
%:- set(minacc,0.1).
:- set(minacc,0.1).
:- set(explore,true).
:- set(nodes,500000).
%:- set(m,20).
%:- set(evalfn,auto_m).
:- set(evalfn,f).
:- set(test_pos,'test.f').
:- set(test_neg,'test.n').
%:- set(optimise_clauses,true).
:- set(verbosity,10).
%:- set(search,heuristic).
:- set(minpos,10).
%:- set(construct_bottom,false).

%:- set(record,true).
%:- set(recordfile,'test.out').
%:- set(samplesize,0).

/*
load_bg :-
	[misc],
	[unknown],
	[ai,graphics,language,systems,theory].
*/
:- [bg].
:- abolish(advisedby/2).

%	fold(I),
%	load_bg(I).

load_bg(0) :- [graphics,language,systems,theory].
load_bg(1) :- [ai,language,systems,theory].
load_bg(2) :- [ai,graphics,systems,theory].
load_bg(3) :- [ai,graphics,language,theory].
load_bg(4) :- [ai,graphics,language,systems].

:- load_bg.

:- determination(advisedby/2,taughtby/3).
:- determination(advisedby/2,courselevel/2).
:- determination(advisedby/2,position/2).
:- determination(advisedby/2,projectmember/2).
:- determination(advisedby/2,phase/2).
:- determination(advisedby/2,tempadvisedby/2).
:- determination(advisedby/2,yearsinprogram/2).
:- determination(advisedby/2,ta/3).
:- determination(advisedby/2,professor/1).
:- determination(advisedby/2,student/1).
%:- determination(advisedby/2,publication/2).
:- determination(advisedby/2,allpublicationswith/2).
:- determination(advisedby/2,nofpubs/2).
:- determination(advisedby/2,commonpub/2).
:- determination(advisedby/2,commonpubs/2).
:- determination(advisedby/2,commonpubsratio/2).

:- modeb(*,taughtby(+course,-person,-date)).
:- modeb(*,taughtby(-course,+person,-date)).
:- modeb(*,courselevel(+course,-level)).
:- modeb(*,courselevel(+course,#level)).
:- modeb(*,position(+person,-faculty)).
:- modeb(*,position(+person,#faculty)).
:- modeb(*,projectmember(+project,-person)).
:- modeb(*,projectmember(-project,+person)).
:- modeh(*,advisedby(+person,+person)).
:- modeb(*,phase(+person,-phase)).
:- modeb(*,phase(+person,#phase)).
%:- modeb(*,tempadvisedby(-person,+person)).
:- modeb(*,tempadvisedby(+person,+person)).
:- modeb(*,yearsinprogram(+person,-number)).
%:- modeb(*,yearsinprogram(+person,#number)).
:- modeb(*,ta(+course,-person,-date)).
:- modeb(*,ta(-course,+person,-date)).
%:- modeb(*,ta(-course,-person,+date)).
:- modeb(*,professor(+person)).
:- modeb(*,student(+person)).
/*
%:- modeb(*,publication(+ref,+person)).
%:- modeb(*,publication(-ref,+person)).
:- modeb(*,allpublicationswith(+person,+person)).
:- modeb(*,not_allpulicationswith(+person,+person)).
:- modeb(*,nofpubs(+person,-number)).
:- modeb(*,commonpub(+person,-person)).
:- modeb(*,commonpubs(+person,+person,-number)).
:- modeb(*,commonpubsratio(+person,+person,-number)).
:- modeb(*,commonpubsratio(+person,+person,#number)).
*/

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

:- determination(advisedby/2,geq/2).
:- modeb(1,geq(+number,#number)).
:- modeb(1,geq(+number,+number)).

geq(X,Y) :- var(Y), !, X = Y.
geq(X,Y) :- X >= Y.


:- determination(advisedby/2,diff/2).
:- modeb(1,diff(+number,+number)).

diff(X,Y) :- X \= Y.

member(B,[B|_]).
member(B,[_|L]) :- member(B,L).

interesting_threshold(1).
/*
:- vsc_set(vsc_interesting_threshold(3)).
%:- vsc_set(vsc_uninteresting_threshold(0.0)).
:- open(interesting_clauses,write,S),
	vsc_set(vsc_interesting_threshold_stream(S)).

:- vsc_set(vsc_fifo_cache(on)).

:- [-library(terms)].

prune((equals(A,B):-(B1,B2))) :-
	( \+ variable_in_term((B1,B2),A) ;
	  \+ variable_in_term((B1,B2),B)).
*/