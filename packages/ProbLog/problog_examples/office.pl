%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog program describing an office window
% example for using hybrid ProbLog
% $Id: office.pl 4970 2010-10-21 08:47:36Z bernd $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(problog)).

(W,gaussian(2,1)) :: width(W).
(L,gaussian(9,3)) :: length(L).
0.8 :: office_has_window.
0.001 :: corridor_has_window.

in_office :- width(W),length(L), in_interval(W,2,4), in_interval(L,2,4).
in_corridor :- width(W),length(L), below(W,2.5), above(L,3).

room_has_window:-
	in_office, office_has_window.
room_has_window:-
	in_corridor,corridor_has_window.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query ?- problog_exact(room_has_window, Prob, Status).
% Prob = 0.01517076,
% Status = ok ?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
