
:- [library(python)].

main :-
:= import matplotlib.pyplot,
:= Plt = ematplotlib.pyplot,
Plt.plot([1,2,3,4]),
Plt.ylabel(`some numbers`),
Plt.show().

