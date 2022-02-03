:-[examples].

protein2Fold(Protein, Fold):-
  recorded(Protein, Fold, _), !.
protein2Fold(Protein, Fold):-
  recorded(previousFold, PFold, Ref),
  erase(Ref),
  Fold is PFold+1,
  recorda(previousFold, Fold, _),
  recorda(Protein, Fold, _).

foldExamples:-
  recorda(previousFold, 0, _),
  example(alpha(Protein, Position), Weight),
  protein2Fold(Protein, Fold),
  format("example(alpha(~w,~w),~w,~w).~N", [Protein, Position, Weight, Fold]),
  fail.
foldExamples:-
  recorded(previousFold, _, Ref1),
  erase(Ref1).
  %we are not removing proteins, we should do that

:- foldExamples.
