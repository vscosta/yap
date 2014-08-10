

:- style_check(all).

:- yap_flag(unknown,error).

:- module(meld_core,
	  [simulate/1,
	   op(1200, fy, (type)),
	   op(1150, fy, (logical_neighbor)),
	   op(1150, fy, (const)),
	   op(1150, fy, (extensional)),
	   op(1150, fy, (persistent)),
	   op(1150, fy, (extern)),
	   op( 500, fy, (first)),
	   op( 500, fy, (max)),
	   op( 500, fy, (sum)),
	   op( 950, fy, (forall)),
	   op( 900, xfy, (then))
	  ]).

:- style_check(all).

%:- reexport(meld/meldi).

%:- reexport(meld/meldc).

simulate(G) :-
	 input_graph(G),
	 live.


