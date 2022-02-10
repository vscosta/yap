:- modeh(active(+molecule)).
:- modeb(*, atom(+molecule,-atomid,#element)).
:- modeb(*, bond(+molecule,+atomid,#element,-atomid,#element,#bondtype)).
%:- modeb(*, bond(+molecule,-atomid,#element,-atomid,#element,#bondtype)).

atom(M, Atom_ID, Elem):-
  atm(M, Atom_ID, _, Elem, _, _, _, _). % using the 4th argument as element is more specific than using the third
%  atm(M, Atom_ID, Elem, _, _, _, _, _).

bond(M, A1, E1, A2, E2, BT):-
  atom(M, A1, E1),
  atom(M, A2, E2),
  bond(M, A1, A2, BT).

%:-set(maximum_singletons_in_clause, 1).
:-set(clause_length, 6).%change to 10 to have better results
%:-set(cross_validation_folds, 5).
%:-set(noise,0.1).
%:-set(i,20).
%:-set(evalfn, compression).
%:-set(evalfn, laplace).
:-set(evalfn, coverage).
%:-set(verbose,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interesting notice: In this dataset laplace and coverage evaluation functions have better results
% than compression. That is because there are relatively few examples (only 560) and since the interesting
% hypothesis are relatively large (about 6 literals) the cost of the pos coverage - neg coverage -
% num literals in hypothesis can easily become negative because pos score - neg score is only slightly positive
% (e.g. 8-2). To overcome that, we set example inflation to a high value (e.g. 10, i.e. multiplying each
% example weight by 10). This way the pos score - neg score for the same hypothesis would be 60. If such
% an hypothesis had 10 literals it would before be discarded but now accepted.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%:-set(example_inflation, 10). % increasing inflation so that compression is a good measure
%:-set(minpos, 20). % minimum positive score to consider a rule
%:-set(verbose,1).

%:-[atombond, examples_original_folds].
:-[atombond, examples_no_folds].

