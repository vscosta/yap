
:- ensure_loaded(library(clpbn)).

wet_grass(W) :-
	sprinkler(S),
	rain(R),
	{ W = wet with p([f,t],
			 ([1.0,0.1,0.1,0.01,
			   0.0,0.9,0.9,0.99]),
			 [S,R])
	}.


sprinkler(P) :-
	cloudy(C),
	{ P = sprinkler with p([f,t],
			       [0.5,0.9,
				0.5,0.1],
			       [C])
	}.

rain(R) :-
	cloudy(C),
	{ R = rain with p([f,t], [0.8,0.2,
				  0.2,0.8],
			  [C]) }.

cloudy(C) :-
	{ C = cloudy  with p([f,t],[0.5,0.5],[]) }.


