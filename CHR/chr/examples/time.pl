% TEMPORAL REASONING
% thom fruehwirth ECRC 920721
% follows work by Itay Meiri AAAI 1991
% uses path concistency handler pc.chr
% 930908 updated and modified for new CHR version
% Christian Holzbaur mods for SICStus (e.g. delay -> block/when)

:- use_module( library(chr)).
:- use_module( library(lists), [member/2,memberchk/2]).

:- multifile user:goal_expansion/3.
%
user:goal_expansion( once(G), _, (G->true)).

:- ensure_loaded('time-pc').	% get compiled path consistency handler

%% domain specific predicates ------------------------------------------------


	inf(   3.40282e38).
	minf( -3.40282e38).
	sup(   1.0e-45).
        msup( -1.0e-45).

        path1(1,X,Y,[R],p-p,I):- check_pp(X,Y,R).
        path1(1,X,Y,[R],p-i,I):- check_pi(X,Y,R).
        path1(1,X,Y,[R],i-p,I):- check_ip(Y,X,R).
        path1(1,X,Y,[R],i-i,I):- check_ii(X,Y,R).

:-block empty(-,-,?).
%
empty(0,[],T).

:- block universal(-,?,?), universal(?,-,?), universal(?,?,-).
%
universal(N,L,T):-
	(is_quantl(L) -> 
	        inf(Inf), minf(Minf),
		L=[A-B],(A=<Minf),(Inf=<B)
		; 
	T=p-p -> 		% 930212 to account for finite domains
		sort(L,[eq, ge, le])
		;
		size(T,N)
	),
	!.

	size(i-i,13).
	size(p-p,3).
	size(p-i,5).
	size(i-p,5).
	size(s-s,5).

:- block equality(?,-), equality(-,?).
%
equality(L,i-i):- !, member(equals,L).
equality(L,s-s):- !, member(eq,L).
equality(L,p-p):-
	(is_quall(L) ->			% succeeds also if var-case: dirty!!
		member(E,L),(E=eq;number(E),E=:=0)	% 930212
		;
		member(A-B,L),
		(A=0,B=0 ; (A=<0),(0=<B))
	),
	!.


unique( L) :- when( ground(L), unique_g(L)).

unique_g([A-B]):- !,(A=:=B).
unique_g([A]).

% 930212 for finite domains
bind_value(X,Y,[R]):- (R=V-_;R=V)->(Y=:=X+V).

shift_interval(X,[],[]).
shift_interval(X,[A-C|L1],[B-D|L2]):- !,
	B is A-X, D is C-X,
	shift_interval(X,L1,L2).
shift_interval(X,[A|L1],[B|L2]):-
	B is A-X,
	shift_interval(X,L1,L2).


:- block intersection(-,?,?,?), intersection(?,-,?,?).
%
intersection(L1,L2,L3,T):- qtype(L1,Q1),qtype(L2,Q2),
		((Q1==quall,Q2==quall) ->
			intersection(L1,L2,L3)
			;
			qualquant(L1,Q1,LQ1),qualquant(L2,Q2,LQ2),
			interint(LQ1,LQ2,L3)
		),
		!. 

  intersection([], _, []).
  intersection([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        L3 = [Head|L3tail],
        intersection(L1tail, L2, L3tail).
  intersection([_|L1tail], L2, L3) :-
        intersection(L1tail, L2, L3).


  % interint([1-2,4-5,6-9],[2-3,3-11],L).
  interint([],L,[]).
  interint(L,[],[]):- L=[_|_].
  interint([A|L1],[B|L2],L3):-
	(
	 isless(A,B) -> interint(L1,[B|L2],L3);
	 isless(B,A) -> interint([A|L1],L2,L3);
	 overlaps1(A,B,C) -> L3=[C|L3N],interint([A|L1],L2,L3N);
	 overlaps2(A,B,C) -> L3=[C|L3N],interint(L1,[B|L2],L3N)
	).

    isless(A-B,C-D):- (B<C).

    overlaps1(A-B,C-D,E-F):- 
	(B>=D),(C=<B),(A=<D), 
	my_max(A,C,E),my_min(B,D,F).
%	E is max(float(A),float(C)), F is min(float(B),float(D)).
    overlaps2(A-B,C-D,E-F):- 
	(D>=B),(C=<B),(A=<D), 
	my_max(A,C,E),my_min(B,D,F).
%	E is max(float(A),float(C)), F is min(float(B),float(D)).

	my_max(X,Y,Z):- (X>=Y),!,X=Z.
	my_max(X,Y,Y).

	my_min(X,Y,Z):- (X=<Y),!,X=Z.
	my_min(X,Y,Y).

:- block transl(-,-,?,?), transl(-,?,-,?), transl(?,-,-,?).
%
transl(A,B,C,T):- 
	qtype(A,QA),qtype(B,QB),qtype(C,QC),	
	(
	(T=p-p-p,(QA==quantl;QB==quantl;QC==quantl) ) ->  % at least one quantl
		qualquant(A,QA,A1),qualquant(B,QB,B1),qualquant(C,QC,C1),
		transl(A1,B1,C1,T,quantl)
		;
		quantqual(A,QA,A1),quantqual(B,QB,B1),quantqual(C,QC,C1),
		transl(A1,B1,C1,T,quall)
	),
	!.	

  transl(L1,L2,L3,T,Q):- var(L3),!,
	setof(C,A^B^(member(A,L1),member(B,L2),trans(A,B,C,T,Q)),L3N),
	mergerel(L3N,L3,T,Q).
  transl(L1,L2,L3,T,Q):- var(L2),!,
	setof(B,A^C^(member(A,L1),member(C,L3),trans(A,B,C,T,Q)),L2N),
	mergerel(L2N,L2,T,Q).
  transl(L1,L2,L3,T,Q):- var(L1),!,
	setof(A,B^C^(member(B,L2),member(C,L3),trans(A,B,C,T,Q)),L1N),
	mergerel(L1N,L1,T,Q).

	mergerel(L1,L2,T,Q):- 
		(Q==quantl -> mergerel(L1,L2) ; L1=L2),
		!.
	  mergerel([],[]).
	  mergerel([A-B,C-D|L1],L2):-
	        sup(Sup),
		(B+Sup>=C),		% +sup added 921029
		!,
		my_min(A,C,Min),		% min, max added 920129
		my_max(B,D,Max),
		mergerel([Min-Max|L1],L2).
	  mergerel([X|L1],[X|L2]):-
		mergerel(L1,L2).

     trans(A,B,C,s-s-s,quall):- !, 
		strans(A,B,C).
     trans(A,B,C,p-p-p,quall):- !, 
		prans(A,B,C).
     trans(A,B,C,p-p-p,quantl):- !, 
		qtrans(A,B,C).
     trans(A,B,C,U-V-W,quall):- !,
		itrans(U-V-W,A,B,C). 


%% qualitative and quantitative constraints interaction


qtype(L,T) :- when( ground(L), qtype_g(L,T)).

qtype_g(L,quantl):- is_quantl(L).
qtype_g(L,quall):- is_quall(L).

 is_quantl([X|_]):- is_quant(X).
 is_quall([X|_]):- is_qual(X).

  :- block is_quant(-).
  %
  is_quant(A-B). % :- A1 is A,B1 is B,number(A1),number(B1).

  :- block is_qual(-).
  %
  is_qual(A):- atomic(A).	% single numbers are treated like atoms 930212

   :- block qualquant(-,?,-).	% necessary?
   qualquant(A,QA,A1):- 	% hacked for var-case (== versus = below!)
		(QA==quall -> qualquant(A,A0),mergerel(A0,A1) ; QA=quantl -> A=A1).	% mergrel added 921029

   :- block quantqual(-,?,-).	% necessary?
   quantqual(A,QA,A1):- 	% hacked for var-case (== versus = below!)
		(QA==quantl -> quantqual(A,A1) ; QA=quall -> A=A1).

%path(N,X,Y,L,p-p) +=> qualquant(L,LIN), sort(LIN,LI), path(N,X,Y,LI,p-p).

	qualquant([],[]).
	qualquant([A|L1],[B|L2]):-
		qualquant1(A,B),
		qualquant(L1,L2).

		qualquant1(le,A-B):- !, sup(A), inf(B).
		qualquant1(eq,0-0):- !.
		qualquant1(ge,A-B):- !, minf(A), msup(B).
		% 930212 to treat single numbers 
		qualquant1(N,A-A):- A is N.	% 'is' used to catch type error

%path(N,X,Y,LI,p-p) +=> N>2 | 		% quick hack condition for termination
%	quantqual(LI,L), length(L,N1), path(N1,X,Y,L,p-p). 

	quantqual(LI,L):-
		findall(X,quantqual1(LI,X),L).

		quantqual1(LI,eq):- 
			once((member(I-J,LI), (I=<0),(0=<J))).
		quantqual1(LI,le):-  
			once((member(I-J,LI), (0<J))).
		quantqual1(LI,ge):-  
			once((member(I-J,LI), (I<0))).

		% 930212 to treat single numbers 
		quantqual1(LI,N):-  
			once((member(N-M,LI), (N=:=M))).




% ALLENS INTERVALS ---------------------------------------------------------

:- ensure_loaded( allentable).	% get cons_tri/3 transitivity table for Allens intervals

%[after, before, contains, during, equals, finished_by, finishes, meets, met_by, overlapped_by, overlaps, started_by, starts].

%930212
check_ii(X,Y,R):- interval_point(X,R,Y).	
% taken from jonathan lever
interval_point([X,Y],before,[U,V]):- ((Y < U)).
interval_point([X,Y],after,[U,V]):- ((V < X)).
interval_point([X,Y],meets,[U,V]):- ((Y =:= U)).
interval_point([X,Y],met_by,[U,V]):- ((V =:= X)).
interval_point([X,Y],starts,[U,V]):- ((X =:= U, Y < V)).
interval_point([X,Y],started_by,[U,V]):- ((X =:= U, V < Y)).
interval_point([X,Y],finishes,[U,V]):- ((Y =:= V, U < X)).
interval_point([X,Y],finished_by,[U,V]):- ((Y =:= V, X < V)).
interval_point([X,Y],during,[U,V]):- ((U < X, Y < V)).
interval_point([X,Y],contains,[U,V]):- ((X < U, V < Y)).
interval_point([X,Y],overlaps,[U,V]):- ((X < U, U < Y, Y < V)).
interval_point([X,Y],overlapped_by,[U,V]):- ((U < X, X < V, V < Y)).
interval_point([X,Y],equals,[U,V]):- ((X =:= U,Y =:= V)).


itrans(U-V-W,A,B,C):-	
		encode(U-V,A,X),encode(V-W,B,Y),encode(U-W,C,Z), 
		cons_tri(X,Y,Z).

:- block encode(?,-,-).
%
encode(i-i,A,B):-!,encode(A,B).
encode(p-i,A,B):-!,pi_ii(A,Y),encode(Y,B).
encode(i-p,A,B):-!,ip_ii(A,Y),encode(Y,B).
encode(p-p,A,B):-!,pp_pi(A,X),pi_ii(X,Y),encode(Y,B).

:- block encode(-,-).
%
encode(before,1).
encode(after,2).
encode(during,3).
encode(contains,4).
encode(overlaps,5).
encode(overlapped_by,6).
encode(meets,7).
encode(met_by,8).
encode(starts,9).
encode(started_by,10).
encode(finishes,11).
encode(finished_by,12).
encode(equals,13).



% POINT ALGEBRA ---------------------------------------------------------------

%[le,eq,ge]

% 930212
check_pp(X,Y,A-B):- !, ((X+A<Y,Y<X+B)).
check_pp(X,Y,N):- number(N),!, (X+N=:=Y).
check_pp(X,Y,T):- \+ member(T,[le,eq,ge]),!, Y=T.
check_pp(X,Y,R):- ((number(X),number(Y))->check_ppn(X,Y,R);check_ppt(X,Y,R)).

check_ppn(X,Y,le):- (X<Y).
check_ppn(X,Y,eq):- (X=:=Y).
check_ppn(X,Y,ge):- (X>Y).

check_ppt(X,Y,le):- (X@<Y).
check_ppt(X,Y,eq):- (X=Y).
check_ppt(X,Y,ge):- (X@>Y).

prans(A,B,C):- (number(A);number(B);number(C)),!,qtrans(A-A,B-B,C-C).
prans(le,le,le).
prans(le,eq,le).
prans(le,ge,le).
prans(le,ge,eq).
prans(le,ge,ge).
prans(eq,le,le).
prans(eq,eq,eq).
prans(eq,ge,ge).
prans(ge,le,le).
prans(ge,le,eq).
prans(ge,le,ge).
prans(ge,eq,ge).
prans(ge,ge,ge).

	




% QUANTITATIVE  ---------------------------------------------------------

% [I1-I2,...In-1-In] ordered Ii=<Ii+1, comparison problem with reals (equality)

qtrans(A-B,C-D,E-F):- (	(var(A),var(B)) -> safe_is(A,E-D), safe_is(B,F-C) ;
			(var(C),var(D)) -> safe_is(C,E-B), safe_is(D,F-A) ;
			(var(E),var(F)) -> safe_is(E,A+C), safe_is(F,B+D)
			).
	safe_is(A,X-Y):-
	        inf(Inf),
		minf(Minf),
		sup(Sup),
		msup(Msup),
		(X=:=Minf,Y=:=Inf -> A is Minf
		;
		 X=:=Inf,Y=:=Minf -> A is Inf
		;
		 X=:=Msup,Y=:=Sup -> A is Msup
		;
		 X=:=Sup,Y=:=Msup -> A is Sup
		;
		 A is X-Y).
	safe_is(A,X+Y):-
	        inf(Inf),
		minf(Minf),
		sup(Sup),
		msup(Msup),
		(X=:=Inf,Y=:=Inf -> A is Inf
		;
		 X=:=Minf,Y=:=Minf -> A is Minf
		;
		 X=:=Sup,Y=:=Sup -> A is Sup
		;
		 X=:=Msup,Y=:=Msup -> A is Msup
		;
		 A is X+Y).





% POINT-INTERVAL ---------------------------------------------------------

% p-i [before,starts,during,finishes,after]
% i-p [after,started_by,contains,finished_by,before]

%930212
check_pi(X,[A,B],before):- ((X<A)).
check_pi(X,[A,B],starts):- ((X=:=A)).
check_pi(X,[A,B],during):- ((A<X,X<B)).
check_pi(X,[A,B],finishes):- ((X=:=B)).
check_pi(X,[A,B],after):- ((B<X)).

check_pi([A,_B],X,after):- ((X<A)).
check_pi([A,_B],X,started_by):- ((X=:=A)).
check_pi([A,B],X,contains):- ((A<X,X<B)).
check_pi([_A,B],X,finished_by):- ((X=:=B)).
check_pi([_A,B],X,before):- ((B<X)).

% trans see itrans for INTERVAL

% pi_ii
:- block pi_ii(-,-).
%
pi_ii(before, before).
pi_ii(before, meets).
pi_ii(before, finished_by).
pi_ii(before, contains).
pi_ii(before, overlaps).
pi_ii(starts, starts).
pi_ii(starts, equals).
pi_ii(starts, started_by).
pi_ii(during, during).
pi_ii(during, finishes).
pi_ii(during, overlaped_by).
pi_ii(finishes, met_by).
pi_ii(after, after).

% ip_ii (inversion of pi_ii)
:- block ip_ii(-,-).
%
ip_ii(before, before).
ip_ii(finished_by, meets).
ip_ii(contains, contains).
ip_ii(contains, overlaps).
ip_ii(contains, finished_by).
ip_ii(started_by, starts).
ip_ii(started_by, equals).
ip_ii(started_by, started_by).
ip_ii(after, during).
ip_ii(after, finishes).
ip_ii(after, overlaped_by).
ip_ii(after, met_by).
ip_ii(after, after).

% pp_pi
:- block pp_pi(-,-).
%
pp_pi(le, before).
pp_pi(eq, starts).
pp_pi(ge, during).
pp_pi(ge, finishes).
pp_pi(ge, after).

% pp_ii
:- block pp_ii(-,-).
%
pp_ii(A,B):- pp_pi(A,C),pi_ii(C,B).



% end of handler time.chr ----------------------------------------------------
