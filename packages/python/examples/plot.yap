
:- [library(python)].

main :-
	    Plt = matplotlib.pyplot,
	:= import( Plt ),
	:= (
	    Plt.plot([1,2,3,4]),
	    Plt.ylabel(`some numbers`),
	    Plt.show()
	   ).
