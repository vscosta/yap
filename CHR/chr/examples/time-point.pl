/* time point constraints */
% Thom Fruehwirth ECRC 1991-92, 931223, LMU 961028, 980312
% 961105 Christian Holzbaur, SICStus mods

:- use_module(library(chr)).

handler time_point.

operator(700,xfy,'=<+').
operator(700,xfy,'=<*').

constraints (=<+)/2, (=<*)/2.
% A=<*Y=<*B means: time-point Y is between A and B which are positive numbers
% C=<+Y-Z=<+D means: distance of timepoints Y and Z is between C and D whcih are positive numbers
% start(X) means: time-point X has value zero

start(X):- 0=<*X=<*0.

inconsistent @
A=<*X=<*B <=> A>B | fail.

intersect @
A=<*Y=<*B , C=<*Y=<*D <=> AC is max(A,C), BD is min(B,D), AC=<*Y=<*BD
                         pragma already_in_heads.

propagate_forward @
A=<*Y=<*B,C=<+Y-Z=<+D ==> AC is A+C, BD is B+D, AC=<*Z=<*BD.
propagate_backward @
A=<*Y=<*B,C=<+Z-Y=<+D ==> AC is A-D, BD is B-C, AC=<*Z=<*BD.


% EXAMPLES using RANDOM GENERATOR ------------------------------------------

% Park Miller rnd random generator
% careful: 0,2147483647 is a fixpoint

:- setval( rnd, 2183).

rand( X) :-
        getval( rnd, X),
        pm_randstep( X, Y),
        setval( rnd, Y).

pm_randstep( State0, State1) :-
        Hi is State0  // 44488,
        Lo is State0 mod 44488,
        Test is 48271*Lo - 3399*Hi,
        ( Test > 0 -> State1=Test ; State1 is Test+2147483647 ).

pm_test :- pm_test( 10000, 1).
pm_test( 0, S) :- !, S=399268537.
pm_test( N, S) :-
        N1 is N-1,
        pm_randstep( S, T),
        pm_test( N1, T).

% random example generator
% M is number of constraints randomly generated

test(M):- N is M+1,
	length(L,N),
        getval( rnd, Seed),
        print( redo_with(Seed)), nl,
	L=[X|_],
	start(X),
	gen_cnstr(L,1),
	all_cnstr(L,2).

gen_cnstr([],_).
gen_cnstr([_],_).
gen_cnstr([X,Y|L],N):- cnstr(N,A,B),
	        write(A=<+X-Y=<+B),nl,
                A=<+X-Y=<+B,
                gen_cnstr([Y|L],N).

cnstr(N,A,B):- rand(I), A is I mod (20*N), rand(J), B is J mod (20*N) + 10.

all_cnstr([],_).
all_cnstr([_],_).
all_cnstr(L,N):-
	L=[_,_|_], random_list(L,L1), gen_cnstr(L1,N), M is N+1,
	all_cnstr(L1,M).

random_list([],[]).
random_list([N|L],R):- rand(X), 0 is (X mod 3), !, random_list(L,R).
random_list([N|L],[N|R]):- random_list(L,R).

/*
% More Examples

:- start(X),3=<+X-Y=<+10,4=<+Y-Z=<+5.  

0=<*X=<*0,
3=<+X-Y=<+10,
3=<*Y=<*10,
4=<+Y-Z=<+5,
7=<*Z=<*15 ?

:- start(E),
10 =<+ E - F =<+ 20,
10 =<+ H - G =<+ 20,
30 =<+ F - G =<+ 40,
40 =<+ H - I =<+ 50,
60 =<+ E - I =<+ 70.

0=<*E=<*0,
10=<+E-F=<+20,
10=<*F=<*20,
10=<+H-G=<+20,
30=<+F-G=<+40,
40=<+H-I=<+50,
60=<+E-I=<+70,
60=<*I=<*70,
20=<*H=<*30,
40=<*G=<*50 ?

:- start(E),
10 =<+ E - F =<+ 20,
10 =<+ H - G =<+ 20,
30 =<+ F - G =<+ 40,
40 =<+ H - I =<+ 50,
60 =<+ E - I =<+ 120.

0=<*E=<*0,
10=<+E-F=<+20,
10=<*F=<*20,
10=<+H-G=<+20,
30=<+F-G=<+40,
40=<*G=<*60,
20=<*H=<*50,
40=<+H-I=<+50,
60=<*I=<*100,
60=<+E-I=<+120 ? 

:- start(E),
10 =<+ E - F =<+ 20,
10 =<+ H - G =<+ 20,
30 =<+ F - G =<+ 40,
40 =<+ H - I =<+ 50,
50 =<+ E - I =<+ 60.

0=<*E=<*0,
10=<+E-F=<+20,
10=<+H-G=<+20,
30=<+F-G=<+40,
40=<+H-I=<+50,
50=<+E-I=<+60,
60=<*I=<*60,
20=<*H=<*20,
40=<*G=<*40,
10=<*F=<*10 ? 

:- start(A),14.3=<+A-B=<+17.5, 14.3=<+A-B=<+16.3, 12.2=<+B-C=<+14.5.

14.3=<+A-B=<+17.5,
0.0=<*A=<*0.0,
14.3=<+A-B=<+16.3,
14.3=<*B=<*16.3,
12.2=<+B-C=<+14.5,
26.5=<*C=<*30.8 ? 

*/
