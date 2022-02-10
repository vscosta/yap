:- modeh(active(+drug)).
:- modeb(1,phenyl(+drug,-atomid, -center)).
:- modeb(1,bigalkyl(+drug,-atomid, -center)).
:- modeb(1,butyl(+drug,-atomid, -center)).
:- modeb(1,propyl(+drug,-atomid, -center)).
:- modeb(1,ethyl(+drug,-atomid, -center)).
:- modeb(1,methyl(+drug,-atomid, -center)).
:- modeb(1,hetar6ring(+drug,-atomid, -center)).
:- modeb(1,hetar5ring(+drug,-atomid, -center)).
:- modeb(1,hetnar6ring(+drug,-atomid, -center)).
:- modeb(1,hetnar5ring(+drug,-atomid, -center)).
:- modeb(1,cyclohexane(+drug,-atomid, -center)).
:- modeb(1,aldehyde(+drug,-atomid, -center)).
:- modeb(1,ether(+drug,-atomid, -center)).
:- modeb(1,thioether(+drug,-atomid, -center)).
:- modeb(1,ester(+drug,-atomid, -center)).
:- modeb(1,carboxylic_acid(+drug,-atomid, -center)).
:- modeb(1,nh2(+drug,-atomid, -center)).
:- modeb(1,amide(+drug,-atomid, -center)).
:- modeb(1,nh(+drug,-atomid, -center)).
:- modeb(1,keton(+drug,-atomid, -center)).
:- modeb(1,alcohol(+drug,-atomid, -center)).
:- modeb(1,ternitrogen(+drug,-atomid, -center)).
:- modeb(1,quanitrogen(+drug,-atomid, -center)).
:- modeb(1,nitro(+drug,-atomid, -center)).
:- modeb(1,alkene(+drug,-atomid, -center)).
:- modeb(1,alkyne(+drug,-atomid, -center)).
:- modeb(1,conjugated_alkene(+drug,-atomid, -center)).
:- modeb(1,cyanide(+drug,-atomid, -center)).
:- modeb(1,isopropyl(+drug,-atomid, -center)).
:- modeb(1,tertbutyl(+drug,-atomid, -center)).
:- modeb(1,isobutyl(+drug,-atomid, -center)).
:- modeb(1,ccl3(+drug,-atomid, -center)).
:- modeb(1,ccl2(+drug,-atomid, -center)).
:- modeb(1,chlorine_aromatic(+drug,-atomid, -center)).
:- modeb(1,chlorine_aliphatic(+drug,-atomid, -center)).
:- modeb(1,bromine_aromatic(+drug,-atomid, -center)).
:- modeb(1,bromine_aliphatic(+drug,-atomid, -center)).
:- modeb(1,fluorine_aromatic(+drug,-atomid, -center)).
:- modeb(1,fluorine_aliphatic(+drug,-atomid, -center)).
:- modeb(1,iodine_aromatic(+drug,-atomid, -center)).
:- modeb(1,iodine_aliphatic(+drug,-atomid, -center)).
:- modeb(1,iodine(+drug,-atomid, -center)).
:- modeb(1,fluorine(+drug,-atomid, -center)).
:- modeb(1,bromine(+drug,-atomid, -center)).
:- modeb(1,chlorine(+drug,-atomid, -center)).
:- modeb(1,p(+drug,-atomid, -center)).
:- modeb(1,sn(+drug,-atomid, -center)).
:- modeb(1,sulfur(+drug,-atomid, -center)).
:- modeb(1,nitrogenar(+drug,-atomid, -center)).
:- modeb(1,nitrogen2(+drug,-atomid, -center)).
:- modeb(1,nitrogen3(+drug,-atomid, -center)).
:- modeb(1,c2n(+drug,-atomid, -center)).
:- modeb(1,n2n(+drug,-atomid, -center)).
:- modeb(1,positive_charge(+drug,-atomid, -center)).
:- modeb(1,negative_charge(+drug,-atomid, -center)).
:- modeb(1,ic(+drug,-atomid, -center)).
:- modeb(1,hydrophobic_hydrogen(+drug,-atomid, -center)).
:- modeb(1,yxy(+drug,-atomid, -center)).
:- modeb(1,yxxy(+drug,-atomid, -center)).
:- modeb(1,polar_neg(+drug,-atomid, -center)).
:- modeb(1,polar_pos(+drug,-atomid, -center)).
:- modeb(1,hydrophobic(+drug,-atomid, -center)).
:- modeb(1,oxygen3(+drug,-atomid, -center)).
:- modeb(1,ewg(+drug,-atomid, -center)).
:- modeb(1,edg(+drug,-atomid, -center)).
:- modeb(1,distance(+center,+center,#float)).
:- modeb(1,distance(+center,+center,-float)).

:- modeb(1,lteq(+float,#float)).
:- modeb(1,gteq(+float,#float)).
:- modeb(1,gteq(+float,+float)).

% if we want the hypothesis to have atoms and bonds as well, uncomment these 2 mode body declarations

%:- modeb(1, atom(+drug,-atomid,#element)).
%:- modeb(9, bond(+drug,+atomid,#element,-atomid,#element,#bondtype)).

:- [fragments,atombond, examples_original_folds].

atom(M, Atom_ID, Elem):-
  atm(M, Atom_ID, _, Elem, _, _, _, _). % using the 4th argument as element is more specific than using the third
%  atm(M, Atom_ID, Elem, _, _, _, _, _).

bond(M, A1, E1, A2, E2, BT):-
  atom(M, A1, E1),
  atom(M, A2, E2),
  bond(M, A1, A2, BT).

:-set(maximum_singletons_in_clause, 2).
:-set(clause_length, 6).
:-set(cross_validation_folds, 5). % use 5 folds, as 
%:-set(cross_validation_folds, 1). % all training
%:-set(evalfn, laplace).
%:-set(evalfn, coverage).

%:-set(sample,0.1).% use only 10% (randomly chosen) examples for training and testing to speed things up
%:-set(nodes, 2000). % increase this number to have more hypothesis per example
%:-set(maximum_hypothesis_to_generate_theory, 1000000).  NOT SUPPORTED YET
:-set(example_inflation, 10). % increase inflation so that compression is a good measure
:-set(minpos, 20). % minimum positive score to consider a rule
%:-set(verbose,3).

%distance(+P1, +P2, -Dist)
distance(P1, P2, R):-
  var(R),!,
  dist(P1, P2, R).

%distance(+P1, +P2, #Dist)
% distance succeed if P1 and P2 are at distance Dist +/- 1 angstrom

distance(P1, P2, R):-
  ground(R),!,
  (var(P2)->format(" this should never happen, bad sign. distance/3 dsstox_fragments.pl!~n",[]);true),
  dist(P1, P2, D),
  abs(D-R)=<1.

dist(p(X1,Y1,Z1), p(X2,Y2,Z2), R):-
  R is sqrt((X1-X2)^2+(Y1-Y2)^2+(Z1-Z2)^2).

gteq(X,Y):-
	ground(X), ground(Y), !,
	X >= Y.
gteq(X,X):-
	number(X).

lteq(X,Y):-
	ground(X), ground(Y), !,
	X =< Y.
lteq(X,X):-
	number(X).

