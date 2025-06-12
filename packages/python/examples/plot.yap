
:- use_module(library(python)).

:- python_import(matplotlib.pyplot as plt).

main :-
    plt.plot([1,2,3,4]),
    plt.ylabel("some numbers"),
    plt.show().

