%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Constraint Handling Rules			      version 2.2 %
%								  %
%  (c) Copyright 1996-98					  %
%  LMU, Muenchen						  %
%								  %
%  File:   sbag_l.pl						  %
%  Author: Christian Holzbaur		christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% bags of suspensions
%

:- module( sbag,
	[
	    iter_init/2,
	    iter_last/1,
	    iter_next/3,
	    list_to_sbag/2,
	    sbag_empty/1,
	    sbag_member/2,
	    sbag_union/3,
	    sbag_add_element/3,
	    sbag_del_element/3
	]).

% -----------------------------------------------------------------
%
% *** MACROS ***
%
:- multifile
	user:goal_expansion/3.
:- dynamic
	user:goal_expansion/3.
%
user:goal_expansion( iter_init(A,A),	      _, true).
user:goal_expansion( iter_last([]),	      _, true).
user:goal_expansion( iter_next([A|B],A,B),    _, true).
user:goal_expansion( list_to_sbag(A,A),       _, true).
user:goal_expansion( sbag_empty(A),           _, A==[]).
user:goal_expansion( sbag_add_element(A,B,C), _, C=[B|A]).

% -----------------------------------------------------------------

iter_init( A, A).

iter_last( []).

iter_next( [A|B], A, B).

list_to_sbag( L, L).

sbag_empty( B) :- B == [].

%
% here for profiling
%
sbag_member( Element, [Head|Tail]) :-
	sbag_member( Element, Tail, Head).

% auxiliary to avoid choicepoint for last element
%
sbag_member( E, _,	     E).
sbag_member( E, [Head|Tail], _) :-
	sbag_member( E, Tail, Head).

sbag_union( A, B, C) :-
	sort( A, As),
	sort( B, Bs),
	prolog:merge( As, Bs, C).

sbag_add_element( Set1, Elem, Set2) :- Set2 = [Elem|Set1].

sbag_del_element( [],	  _,	[]).
sbag_del_element( [X|Xs], Elem, Set2) :-
	( X==Elem ->
	    Set2 = Xs
	;
	    Set2 = [X|Xss],
	    sbag_del_element( Xs, Elem, Xss)
	).

end_of_file.
