 /*************************************************************************
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-2014	 *
 *									 *
 *************************************************************************/

/**
 * @short: Atom, and Atomic manipulation predicates in YAP
 *
*/									 *

:- module( '$atoms', [ atom_concat/2,
		       atomic_list_concat/2,
		       atomic_list_concat/3,
		       current_atom/1 ] ).

atom_concat(Xs,At) :-
	( var(At) ->
	   '$atom_concat'(Xs, At )
        ;
	 '$atom_concat_constraints'(Xs, 0, At, Unbound),
	 '$process_atom_holes'(Unbound)
        ).

% the constraints are of the form hole: HoleAtom, Begin, Atom, End
'$atom_concat_constraints'([At], 0, At, []) :- !.
'$atom_concat_constraints'([At0], mid(Next, At), At, [hole(At0, Next, At, end)]) :-  !.
% just slice first atom
'$atom_concat_constraints'([At0|Xs], 0, At, Unbound) :-
	atom(At0), !,
	sub_atom(At, 0, Sz, L, At0 ),
	sub_atom(At, _, L, 0, Atr ), %remainder
	'$atom_concat_constraints'(Xs, 0, Atr, Unbound).
% first hole: Follow says whether we have two holes in a row, At1 will be our atom
'$atom_concat_constraints'([At0|Xs], 0, At, [hole(At0, 0, At, Next)|Unbound]) :-
	 '$atom_concat_constraints'(Xs, mid(Next,At1), At, Unbound).
% end of a run
'$atom_concat_constraints'([At0|Xs], mid(end, At1), At, Unbound) :-
	atom(At0), !,
	sub_atom(At, Next, Sz, L, At0),
	sub_atom(At, 0, Next, Next, At1),
	sub_atom(At, _, L, 0, Atr), %remainder
	'$atom_concat_constraints'(Xs, 0, Atr, Unbound).
'$atom_concat_constraints'([At0|Xs], mid(Next,At1), At, Next, [hole(At0, Next, At, Follow)|Unbound]) :-
	 '$atom_concat_constraints'(Xs, mid(NextFollow, At1), At, Unbound).

'$process_atom_holes'([]).
'$process_atom_holes'([hole(At0, Next, At1, End)|Unbound]) :- End == end, !,
	sub_atom(At1, Next, _, 0, At0),
	 '$process_atom_holes'(Unbound).
'$process_atom_holes'([hole(At0, Next, At1, Follow)|Unbound]) :-
	sub_atom(At1, Next, Sz, _Left, At0),
	Follow is Next+Sz,
	 '$process_atom_holes'(Unbound).

	  
atomic_list_concat(L,At) :-
	atomic_concat(L, At).
	
atomic_list_concat(L, El, At) :-
	var(El), !,
	'$do_error'(instantiation_error,atom_list_concat(L,El,At)).
atomic_list_concat(L, El, At) :-
	nonvar(L), !,
	'$add_els'(L,El,LEl),
	atomic_concat(LEl, At).
atomic_list_concat(L, El, At) :-
	nonvar(At), !,
	atom_codes(At, S),
	atom_codes(El, [ElS]),
	'$split_elements'(S, ElS, SubS),
	'$atomify_list'(SubS, L).

'$add_els'([A,B|L],El,[A,El|NL]) :- !,
	'$add_els'([B|L],El,NL).
'$add_els'(L,_,L).
	
'$split_elements'(E.S, E, SubS) :- !,
	'$split_elements'(S, E, SubS).
'$split_elements'(E1.S, E, [E1|L].SubS) :- !,
	'$split_elements'(S, E, L, SubS).
'$split_elements'([], _, []).

'$split_elements'([], _, [], []).
'$split_elements'(E.S, E, [], SubS) :- !,
	'$split_elements'(S, E, SubS).
'$split_elements'(E1.S, E, E1.L, SubS) :-
	'$split_elements'(S, E, L, SubS).

'$atomify_list'([], []).
'$atomify_list'(S.SubS, A.L) :-
	atom_codes(A, S),
	'$atomify_list'(SubS, L).
	

%
% small compatibility hack

'$singletons_in_term'(T,VL) :-
	'$variables_in_term'(T,[],V10),
	'$sort'(V10, V1),
	'$non_singletons_in_term'(T,[],V20),
	'$sort'(V20, V2),	
	'$subtract_lists_of_variables'(V2,V1,VL).

'$subtract_lists_of_variables'([],VL,VL).
'$subtract_lists_of_variables'([_|_],[],[]) :- !.
'$subtract_lists_of_variables'([V1|VL1],[V2|VL2],VL) :-
	V1 == V2, !,
	'$subtract_lists_of_variables'(VL1,VL2,VL).
'$subtract_lists_of_variables'([V1|VL1],[V2|VL2],[V2|VL]) :-
	'$subtract_lists_of_variables'([V1|VL1],VL2,VL).

current_atom(A) :-				% check
	atom(A), !.
current_atom(A) :-				% generate
	'$current_atom'(A).
current_atom(A) :-				% generate
	'$current_wide_atom'(A).

