
:- source.

:- [library(ytest)].
:- [library(hacks)].

:- [meta_tests].

meta_expand( Clause, Canon) :-
    source_module(M),
    indicator( M:Clause, I, _J),
    dynamic( I ),
    assert(Clause, R),
    clause(J, B, R),
    fully_strip_module(J, MH, H),
    ( MH == M
    ->
      HF = H
    ;
      HF = MH:H
    ),
    ( B == true
    ->
      Canon = HF
    ;
      Canon = ( HF :- B )
    ),
    erase(R).

indicator(MC, HM:N/A, HM:G0) :-
    fully_strip_module(MC, M, C),
    ( C = ( H:- _) -> true ; C = H),
    fully_strip_module(M:H, HM, HF),
    functor(HF, N, A),
    functor(G0, N, A).


:- initialization run_tests.
