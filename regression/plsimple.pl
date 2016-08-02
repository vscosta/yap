:- use_module(library(plunit)).


:- begin_tests(lists).
:- use_module(library(lists)).

test(reverse) :-
        reverse([a,b], [b,a]).

:- end_tests(lists).
