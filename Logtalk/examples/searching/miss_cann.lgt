
:- object(miss_cann,
	instantiates(heuristic_state_space)).


	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2000/11/21,
		comment is 'Missionaries and cannibals heuristic state space search problem.']).


	:- uses(loop, [forto/3]).


	initial_state(start, ((3,3), left, (0,0))).


	goal_state(end, ((0,0), right, (3,3))).


	print_state(((Me,Ce), B, (Md,Cd))) :-
		forto(1, Me, write('M')),
		forto(1, Ce, write('C')),
		(B = left ->
			write('.<__>..........')
			;
			write('..........<__>.')),
		forto(1, Md, write('M')),
		forto(1, Cd, write('C')),
		nl.


	next_state(((Me,Ce),left,(Md,Cd)), ((Me2,Ce2),right,(Md2,Cd2)), 1) :-  %mm
		Me >= 2,
		once((Me - 2 =:= 0; Me - 2 >= Ce)),
		Cd =< 2,
		Me2 is Me - 2,
		Ce2 is Ce,
		Md2 is Md + 2,
		Cd2 is Cd.

	next_state(((Me,Ce),left,(Md,Cd)), ((Me2,Ce2),right,(Md2,Cd2)), 2) :-  %m
		Me >= 1,
		once((Me - 1 =:= 0; Me - 1 >= Ce)),
		Cd =< 1,
		Me2 is Me - 1,
		Ce2 is Ce,
		Md2 is Md + 1,
		Cd2 is Cd.

	next_state(((Me,Ce),left,(Md,Cd)), ((Me2,Ce2),right,(Md2,Cd2)), 1) :-  %cc
		Ce >= 2,
		once((Md >= Cd + 2;  Md =:= 0)),
		Me2 is Me,
		Ce2 is Ce - 2,
		Md2 is Md,
		Cd2 is Cd + 2.

	next_state(((Me,Ce),left,(Md,Cd)), ((Me2,Ce2),right,(Md2,Cd2)), 2) :-  %c
		Ce >= 1,
		once((Md >= Cd + 1; Md =:= 0)),
		Me2 is Me,
		Ce2 is Ce - 1,
		Md2 is Md,
		Cd2 is Cd + 1.

	next_state(((Me,Ce),left,(Md,Cd)), ((Me2,Ce2),right,(Md2,Cd2)), 1) :-  %mc
		Me >= 1,
		Ce >= 1,
		Md >= Cd,
		Me2 is Me - 1,
		Ce2 is Ce - 1,
		Md2 is Md + 1,
		Cd2 is Cd + 1.

	next_state(((Me,Ce),right,(Md,Cd)), ((Me2,Ce2),left,(Md2,Cd2)), Cost) :-
		next_state(((Md,Cd),left,(Me,Ce)), ((Md2,Cd2),right,(Me2,Ce2)), Cost).


	heuristic(((_, _), _, (Md, Cd)), Cost) :-
		Cost is 6 - (Md + Cd).


:- end_object.
