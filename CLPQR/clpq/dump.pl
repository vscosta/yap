%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   dump.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
dump( +Target, ?NewVars, ?CodedAnswer)

where Target and NewVars are lists of variables of equal length and
CodedAnswer is the term representation of the projection of constraints
onto the target variables where the target variables are replaced by   
the corresponding variables from NewVars.
*/

:- use_module( library(terms), [term_variables/2]).

:- use_module( library(assoc), 
	[
	    empty_assoc/1,
	    get_assoc/3,
	    put_assoc/4,
	    assoc_to_list/2
	]).

dump( Target, NewVars, Constraints) :-
	( 
	    ( proper_varlist( Target) -> 
	        true 
	    ; 
		raise_exception(instantiation_error(dump(Target,NewVars,Constraints),1))
	    ),
	    ordering( Target),
	    related_linear_vars( Target, All),
	    nonlin_crux( All, Nonlin),
	    project_attributes( Target, All),
	    related_linear_vars( Target, Again), % project drops/adds vars
	    all_attribute_goals( Again, Gs, Nonlin),
	    empty_assoc( D0),
	    mapping( Target, NewVars, D0,D1),	% late (AVL suffers from put_atts)
	    copy( Gs, Copy, D1,_),		% strip constraints
	    bb_put( copy, NewVars/Copy),
	    fail				% undo projection
	;
	    bb_delete( copy, NewVars/Constraints) % garbage collect
	).

proper_varlist( X) :- var( X), !, fail.
proper_varlist( []).
proper_varlist( [X|Xs]) :-
	var( X),
	proper_varlist( Xs).

related_linear_vars( Vs, All) :-
	empty_assoc( S0),
	related_linear_sys( Vs, S0,Sys),
	related_linear_vars( Sys, All, []).

related_linear_sys( [],     S0,L0) :- assoc_to_list( S0, L0).
related_linear_sys( [V|Vs], S0,S2) :-
	( get_atts( V, class(C)) ->
	    put_assoc( C, S0, C, S1)
	;
	    S1 = S0
	),
	related_linear_sys( Vs, S1,S2).

related_linear_vars( []) --> [].
related_linear_vars( [S-_|Ss]) -->
	{
	    class_allvars( S, Otl)
	},
	cpvars( Otl),
	related_linear_vars( Ss).

cpvars( Xs) --> {var(Xs)}, !.
cpvars( [X|Xs]) -->
	( {var(X)} -> [X] ; [] ),
	cpvars( Xs).

nonlin_crux( All, Gss) :-
	collect_nonlin( All, Gs, []),		% destructive
	this_linear_solver( Solver),
	nonlin_strip( Gs, Solver, Gss).

nonlin_strip( [],          _,      []).
nonlin_strip( [M:What|Gs], Solver, Res) :-
	( M == Solver ->
	    ( What = {G} ->
	        Res = [G|Gss]
	    ;
		Res = [What|Gss]
	    )
	;
	    Res = Gss
	),
	nonlin_strip( Gs, Solver, Gss).

all_attribute_goals( []) --> [].
all_attribute_goals( [V|Vs]) -->
	dump_linear( V, toplevel),
	dump_nonzero( V, toplevel),
	all_attribute_goals( Vs).

mapping( [],     [],     D0,D0).
mapping( [T|Ts], [N|Ns], D0,D2) :-
	put_assoc( T, D0, N, D1),
	mapping( Ts, Ns, D1,D2).

copy( Term, Copy, D0,D1) :- var( Term), 
	( get_assoc( Term, D0, New) ->
	    Copy = New,
	    D1 = D0
	;
	    put_assoc( Term, D0, Copy, D1)
	).
copy( Term, Copy, D0,D1) :- nonvar( Term),
	functor( Term, N, A),
	functor( Copy, N, A),
	copy( A, Term, Copy, D0,D1).

copy( 0, _, _, D0,D0) :- !.
copy( 1, T, C, D0,D1) :- !,
	arg( 1, T, At1),
	arg( 1, C, Ac1),
	copy( At1, Ac1, D0,D1).
copy( 2, T, C, D0,D2) :- !,
	arg( 1, T, At1),
	arg( 1, C, Ac1),
	copy( At1, Ac1, D0,D1),
	arg( 2, T, At2),
	arg( 2, C, Ac2),
	copy( At2, Ac2, D1,D2).
copy( N, T, C, D0,D2) :-
	arg( N, T, At),
	arg( N, C, Ac),
	copy( At, Ac, D0,D1),
	N1 is N-1,
	copy( N1, T, C, D1,D2).

end_of_file.
