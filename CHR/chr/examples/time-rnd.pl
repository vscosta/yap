
% random test generator for time.chr
% thom fruehwirth LMU 961022
% call test(N) with small N

/*
Allens interval constraints
type i-i
[after, before, contains, during, equals, finished_by, finishes, meets, met_by, overlapped_by, overlaps, started_by, starts].

time point constraints
type p-p
[le,eq,ge] or any list of interval distances, e.g. [1-2,3-5, 6-11]

point-interval constraints
type p-i [before,starts,during,finishes,after]
type i-p [after,started_by,contains,finished_by,before]

set constraints
type s-s
not fully specified

*/

:- ( current_module( prolog) -> use_module( library('chr/getval')) ; true ).

:- setval( rnd, 2183).

rand( X) :- 
	getval( rnd, X),
	pm_randstep( X, Y),
	setval( rnd, Y).

%
% Park Miller rnd
% careful: 0,2147483647 is a fixpoint
%
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


cnstr1([after, before, contains, during, equals, finished_by, finishes, meets, met_by, overlapped_by, overlaps, started_by, starts], i-i).
cnstr1([le,eq,ge],p-p):- rand(I), 0 is I mod 2.  % fail sometimes to try more
cnstr1([before,starts,during,finishes,after],p-i).
cnstr1([after,started_by,contains,finished_by,before],i-p).

cnstr(L,T):- cnstr1(L1,T), rand(I), 0 is I mod 2, random_list(L1,L).
cnstr(L,p-p):- rand(I), J is (I mod 20)+10, length(L,J),
		pair_list(L1,L),
		rand(X), Y is (X mod 100)-50, L1=[Y|_], random_list(L1).

random_list([],[]).
random_list([N|L],R):- rand(X), 0 is (X mod 3), !, random_list(L,R).
random_list([N|L],[N|R]):- random_list(L,R).

random_list([]).
random_list([N]).
random_list([N,Y|L]):- rand(X), Y is N+(X mod 10), random_list([Y|L]).

pair_list([],[]).
pair_list([N,M|L],[N-M|R]):- pair_list(L,R).

gen_cnstr([]).
gen_cnstr([_]).
gen_cnstr([X-A,Y-B|L]):- T=A-B, cnstr(L1,T),
		% arc(X,Y,L1,T),
		write(arc(X,Y,L1,T)),write(','), nl,
		gen_cnstr([Y-B|L]).

all_cnstr([]).
all_cnstr([_]).
all_cnstr(L):- 	L=[_,_|_], random_list(L,L1), gen_cnstr(L1), all_cnstr(L1).

test(N):- length(L,N),
	%T=[p,p,p,p,p,i,p,p,i,i,i,i,p,i,p,i,p,i,p,p,p,p|T],
	getval( rnd, Seed),
	print( redo_with(Seed)), 
	nl,
	gen_cnstr(L),
	all_cnstr(L),
	nl,
	css( Css),
	nl, 
	pl( Css), 
	fail.

css( Css) :-
	current_module( prolog),		% SICStus
	!,
	findall_constraints( _, C),
	cs( C, Cs),
	sort( Cs, Css).
css( Css) :-
	current_module( eclipse),
	findall( C, chr_get_constraint(C), Cs),
	sort( Cs, Css).

cs( [],       []).
cs( [C#_|Cs], [C|Ts]) :-
	cs( Cs, Ts).

pl( []).
pl( [C|Cs]) :-
	print( C), write(','), nl,
	pl( Cs).


/*
arc(_231,_226,[34-43,52-55,62-68,72-79,84-85,93-93,100-102,103-108,114-120,124-132,135-144,149-158,165-173,174-180,187-196,204-204,209-217,223-225,229-230,237-243,249-250,256-260,263-270],p-p),
arc(_226,_6963,[le],p-p),
arc(_231,_226,[28-33,38-45,45-54,59-64,64-68,71-71,78-86,92-100,103-104,109-109,116-116,122-129,138-139,146-149,149-158,159-167,167-172],p-p),
arc(_231,_226,[-22- -14,-12- -4,-3-2,11-20,27-33,37-38,47-49,49-51,51-54,62-70,72-77,81-83,87-95,98-105,109-115,124-129,134-140,149-158,167-175,179-181,184-189,189-194,195-195],p-p),
arc(_231,_226,[le,ge],p-p),
arc(_231,_226,[28-35,42-50,59-60,63-69,78-81,81-82,85-92,95-97,98-100,102-107,107-114,114-114,115-123,127-130,138-143,148-151,151-155,163-170,172-179,183-184,186-190,192-198,206-206,206-213,217-217,218-223,226-227,230-232],p-p)

*/

