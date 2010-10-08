%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog program describing an office window
% example for using hybrid ProbLog
% $Id: office.pl 4876 2010-10-05 16:15:57Z theo $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module('../problog').

width(gaussian(2,1)).
length(gaussian(9,3)).
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
