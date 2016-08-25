
:- [library(python)].

main :-
	    Plt = matplotlib.pyplot,
	:= import( Plt ),
	:= (
	    Plt.figure(figsize=(10,2.5))
	    Plt.plot([1,2,3,4]),
	    Plt.ylabel(`some numbers`),
	    Plt.show()
	   ).

