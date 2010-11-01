:- module(meld_core,
	  [simulate/1,
	   op(1200, fy, (type)),
	   op(1150,  fy, (logical_neighbor)),
	   op( 500, fy, (first)),
	   op( 500, fy, (max)),
	   op( 950, fy, (forall)),
	   op( 900, xfy, (then))
	  ]).

:- style_check(all).

:- reexport(meldi).

:- reexport(meldc).

simulate(G) :-
	 input_graph(G),
	 live.


	 
