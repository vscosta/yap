:- use_module(library(python)).
:- use_module(library(lists)).

main :-
test(I),
   catch( dot(I), G, err(I,G) ),
  writeln('.'),
  fail.
  main.

test(I) :-
    findall(I, clause(det(I,_,_),_), IsF, Is0 ),
    Is0 = [],
    sort(IsF,Is),
    member(I, Is).

dot(I) :-
   det(I, Vs, Sol),
    Vs == Sol.

err(I,N) :-
  format(' test ~d failed with error: ~w',[I,N]).


det(a1,[X],[2]) :- X:=2.
det(a2,[],[]) :- x := range(1,10).
det(b2 [X],[9]) :- X := x.length().
det(c3,[X],[Y]) :- X:=cmath.sin(1), Y is sin(1).