%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   arith_q.pl                                             %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module( arith_q, 
	[
	    arith_eps/1,
	    arith_normalize/2,
	    integerp/1,
	    integerp/2,
						% Q specifics
	    acosq/4,
	    addq/6,
	    asinq/4,
	    atanq/4,
	    ceilingq/4,
	    comq/5,
	    cosq/4,
	    divq/6,
	    divq_11/4,
	    'divq_-11'/4,
	    expq/4,
	    expq/6,
	    floorq/4,
	    getq/3,
	    logq/4,
	    maxq/6,
	    minq/6,
	    mulq/6,
	    putq/3,
	    rat_float/3,
	    roundq/4,
	    signumq/4,
	    sinq/4,
	    subq/6,
	    tanq/4,
	    truncateq/4
	]).

%
% Modules receiving Q expansion
%
arith_module( clpq).
arith_module( nfq).

:- multifile
	user:goal_expansion/3.

:- dynamic
	user:goal_expansion/3.

:- discontiguous
        user:goal_expansion/3.

%
user:goal_expansion( putq(D,N,Res), Module, (Res = rat(N,D))) :- arith_module( Module).

user:goal_expansion( arith_eval(Term,Res), Module, Expansion) :-
	arith_module( Module),
	compile_Qn( Term, Res, Code),
	l2conj( Code, Expansion).

user:goal_expansion(arith_eval(Rel), Module, Expansion) :-
	arith_module( Module),
	compile_Qn( Rel, boolean, Code),
	l2conj( Code, Expansion).

user:goal_expansion(case_signum(Term,Lt,Z,Gt), Module, Expansion) :-
	arith_module( Module),
	compile_case_signum_Qn( Term, Lt,Z,Gt, Code),
	l2conj( Code, Expansion).

:- use_module( arith).

arith_eps( 0).					% for Monash #zero expansion

arith_normalize( Const, Norm) :-
  getq( Const, N, D),
  putq( D, N, Norm).

integerp( rat(_,1)).

integerp( rat(I,1), I).

%---------------------------------------------------------------------------

user:goal_expansion(comq(Na,Da,Nb,_,S), arith_q, 0<Nb) :-
	Na==0, Da==1, S==(<).

user:goal_expansion(comq(Na,_,Nb,Db,S), arith_q, Na<0) :-
	Nb==0, Db==1, S==(<).

user:goal_expansion(divq(Na,Da,Nb,Db,Nc,Dc), arith_q, Exp) :-
	(   Na==1,Da==1    
	->  Exp = divq_11(Nb,Db,Nc,Dc)
	;   Na==(-1),Da==1
	->  Exp = 'divq_-11'(Nb,Db,Nc,Dc)
	).

/*
   1 addq(0, 1, 1, 1, _, _).
  10 comq(0, 1, _, _, <).
   1 comq(0, 1, _, _, _).
  16 comq(_, _, 0, 1, <).
   2 comq(_, _, 0, 1, _).
   1 comq(_, _, 1, 1000, <).
   6 comq(_, _, _, _, <).
   7 divq(-1, 1, _, _, _, _).
   1 divq(0, 1, 1, 1, _, _).
   1 divq(1, 1, 1000, 1, _, _).
   4 divq(1, 1, _, _, _, _).
   2 getq(_, -1, 1).
  13 getq(_, 0, 1).
   6 getq(_, 1, 1).
   1 mulq(0, 1, 1000, 1, _, _).
   1 putq(1, 0, _).
   1 putq(1, 1, _).
   1 putq(1000, 1, _).
*/

