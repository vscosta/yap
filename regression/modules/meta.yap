
% :- library(test).

%:- multifile ytest:test/4.

:- source.

:- [library(ytest)].
:- [library(ytest/preds)].

meta_expand(InputCl, C1) :-
    source_module(SM),
    '$yap_strip_module'(SM:InputCl, M, ICl),
    '$expand_a_clause'( M:ICl, SM, C1, _CO).

:- [meta_tests].

:- run_tests.
