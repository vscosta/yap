%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   bb.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


bb_inf( Is, Term, Inf) :-
	bb_inf( Is, Term, Inf, _, 0.001).

bb_inf( Is, Term, Inf, Vertex, Eps) :-
	nf( Eps, ENf),
	nf_constant( ENf, EpsN),
	wait_linear( Term, Nf, bb_inf_internal(Is,Nf,EpsN,Inf,Vertex)).

% ---------------------------------------------------------------------

bb_inf_internal( Is, Lin, Eps, _, _) :-
	bb_intern( Is, IsNf),
	( bb_delete( incumbent, _) -> true ; true ),
	repair( Lin, LinR),			% bb_narrow ...
	deref( LinR, Lind),
	var_with_def_assign( Dep, Lind),
	determine_active_dec( Lind),
	bb_loop( Dep, IsNf, Eps),
	fail.
bb_inf_internal( _, _, _, Inf, Vertex) :-
	bb_delete( incumbent, InfVal-Vertex),	% GC
	{ Inf =:= InfVal }.

bb_loop( Opt, Is, Eps) :-
	bb_reoptimize( Opt, Inf),
	bb_better_bound( Inf),
	vertex_value( Is, Ivs),
	( bb_first_nonint( Is, Ivs, Eps, Viol, Floor, Ceiling) ->
	    bb_branch( Viol, Floor, Ceiling),
	    bb_loop( Opt, Is, Eps)
	;	
	    round_values( Ivs, RoundVertex),
	    % print( incumbent( Inf-RoundVertex)), nl,
	    bb_put( incumbent, Inf-RoundVertex)
	).

%
% added ineqs may have led to binding
%
bb_reoptimize( Obj, Inf) :- var( Obj), iterate_dec( Obj, Inf).
bb_reoptimize( Obj, Inf) :- nonvar( Obj), Inf = Obj.

bb_better_bound( Inf) :-
	bb_get( incumbent, Inc-_),
	!,
	arith_eval( Inf < Inc).
bb_better_bound( _).

bb_branch( V, U, _) :- { V =< U }.
bb_branch( V, _, L) :- { V >= L }.

vertex_value( [],     []).
vertex_value( [X|Xs], [V|Vs]) :-
	rhs_value( X, V),
	vertex_value( Xs, Vs).

rhs_value( Xn, Value) :- nonvar(Xn), Value=Xn.
rhs_value( Xn, Value) :- var(Xn),
	deref_var( Xn, Xd),
	decompose( Xd, _, R, I),
	arith_eval( R+I, Value).

%
% Need only one as we branch on the first anyway ...
%
bb_first_nonint( [I|Is], [Rhs|Rhss], Eps, Viol, F, C) :-
	( arith_eval( floor(Rhs), Floor),
	  arith_eval( ceiling(Rhs), Ceiling),
	  arith_eval(min(Rhs-Floor,Ceiling-Rhs) > Eps) ->
	    Viol = I,
	    F = Floor,
	    C = Ceiling
	;
	    bb_first_nonint( Is, Rhss, Eps, Viol, F, C)
	).

round_values( [],     []).
round_values( [X|Xs], [Y|Ys]) :-
	arith_eval( round(X), Y),
	round_values( Xs, Ys).

bb_intern( [],	   []).
bb_intern( [X|Xs], [Xi|Xis]) :-
	nf( X, Xnf),
	bb_intern( Xnf, Xi, X),
	bb_intern( Xs, Xis).

%
% allow more general expressions and conditions? integral(Exp) ???
%
bb_intern( [],             X, _) :- !, arith_eval( 0, X).
bb_intern( [v(I,[])],      X, _) :- !, X=I.
bb_intern( [v(One,[X^1])], X, _) :-
	arith_eval(One=:=1),
	!,
	get_atts( X, [type(T),strictness(S)]),
	bb_narrow( T, S, X).
bb_intern( _, _, Term) :-
	raise_exception( instantiation_error(bb_inf(Term,_,_),1)).

bb_narrow( t_l(L),    S, V) :- 
	S /\ 2'10 =\= 0, 
	!, 
	arith_eval( floor(1+L), B), 
	{ V >= B }.
bb_narrow( t_u(U),    S, V) :- 
	S /\ 2'01 =\= 0, 
	!, 
	arith_eval( ceiling(U-1), B), 
	{ V =< B }.
bb_narrow( t_lu(L,U), S, V) :- !,
	bb_narrow( t_l(L), S, V),
	bb_narrow( t_u(U), S, V).
bb_narrow( _, _, _).

