/**
 * @file   library/mapargs.yap
 * @author Lawrence Byrd + Richard A. O'Keefe, VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @author : E. Alphonse from code by Joachim Schimpf, Jan Wielemaker, Vitor Santos Costa
 * @date    4 August 1984 and Ken Johnson 11-8-87
 *
 * @brief  Macros to apply a predicate to all sub-terms of a term.
 *
 *
*/


:- module(mapargs,[ mapargs/2,                 % :Goal, +S
		    mapargs/3,                 % :Goal, +S, -S
		    mapargs/4,                 % :Goal, +S, -S1, -S2
		    mapargs/5,                 % :Goal, +S, -S1, -S2, -S3
		    mapargs/6,                 % :Goal, +S, -S1, -S2, -S3, -S4
		    sumargs/4,
		    foldargs/4, 		% :Pred, +S, ?V0, ?V
		    foldargs/5, 		% :Pred, +S, ?S1,  ?V0, ?V
		    foldargs/6, 		% :Pred, +S, ?S1, ?S2, ?V0, ?V
		    foldargs/7	 		% :Pred, +S, ?S1, ?S2, ?S3, ?V0, ?V
                  ]).

/**
* @defgroup mapargs Apply a predicate to all  arguments of a term
* @ingroup library
* @{
*/


:- use_module(library(maputils)).
:- use_module(library(lists), [append/3]).

:- meta_predicate
	mapargs(1,+),
	mapargs_args(1,+,+),
	mapargs(2,+,-),
	mapargs_args(1,+,-,+),
	mapargs(3,+,-,-),
	mapargs_args(2,+,-,-,+),
	mapargs(4,+,-,-,-),
	mapargs_args(3,+,-,-,-,+),
	mapargs(5,+,-,-,-,-),
	mapargs_args(2,+,-,-,-,-,+),
	sumargs(3,+,+,-),
	sumargs_args(3,+,+,-,+),
	foldargs(3, +, +, -),
	foldargs(4, +, ?, +, -),
	foldargs(5, +, ?, ?, +, -),
	foldargs(6, +, ?, ?, ?, +, -).


mapargs(Pred, TermIn) :-
    functor(TermIn, _F, N),
    mapargs_args(Pred, TermIn, 0, N).

mapargs_args(Pred, TermIn, I, N) :-
    ( I == N -> true ;
      I1 is I+1,
      arg(I1, TermIn, InArg),
      call(Pred, InArg),
      mapargs_args(Pred, TermIn, I1, N) ).

mapargs(Pred, TermIn, TermOut) :-
    functor(TermIn, F, N),
    functor(TermOut, F, N),
    mapargs_args(Pred, TermIn, TermOut, 0, N).

mapargs_args(Pred, TermIn, TermOut, I, N) :-
    ( I == N -> true ;
      I1 is I+1,
      arg(I1, TermIn, InArg),
      arg(I1, TermOut, OutArg),
      call(Pred, InArg, OutArg),
      mapargs_args(Pred, TermIn, TermOut, I1, N) ).

mapargs(Pred, TermIn, TermOut1, TermOut2) :-
    functor(TermIn, F, N),
    functor(TermOut1, F, N),
    functor(TermOut2, F, N),
    mapargs_args(Pred, TermIn, TermOut1, TermOut2, 0, N).

mapargs_args(Pred, TermIn, TermOut1, TermOut2, I, N) :-
    ( I == N -> true ;
      I1 is I+1,
      arg(I1, TermIn, InArg),
      arg(I1, TermOut1, OutArg1),
      arg(I1, TermOut2, OutArg2),
      call(Pred, InArg, OutArg1, OutArg2),
      mapargs_args(Pred, TermIn, TermOut1, TermOut2, I1, N) ).

mapargs(Pred, TermIn, TermOut1, TermOut2, TermOut3) :-
    functor(TermIn, F, N),
    functor(TermOut1, F, N),
    functor(TermOut2, F, N),
    mapargs_args(Pred, TermIn, TermOut1, TermOut2, TermOut3, 0, N).

mapargs_args(Pred, TermIn, TermOut1, TermOut2, TermOut3, I, N) :-
    ( I == N -> true ;
      I1 is I+1,
      arg(I1, TermIn, InArg),
      arg(I1, TermOut1, OutArg1),
      arg(I1, TermOut2, OutArg2),
      arg(I1, TermOut3, OutArg3),
      call(Pred, InArg, OutArg1, OutArg2, OutArg3),
      mapargs_args(Pred, TermIn, TermOut1, TermOut2, TermOut3, I1, N) ).

mapargs(Pred, TermIn, TermOut1, TermOut2, TermOut3, TermOut4) :-
    functor(TermIn, F, N),
    functor(TermOut1, F, N),
    functor(TermOut2, F, N),
    functor(TermOut3, F, N),
    functor(TermOut4, F, N),
    mapargs_args(Pred, TermIn, TermOut1, TermOut2, TermOut3, TermOut4, 0, N).

mapargs_args(Pred, TermIn, TermOut1, TermOut2, TermOut3, TermOut4, I, N) :-
    ( I == 0 -> true ;
      I1 is I+1,
      arg(I1, TermIn, InArg),
      arg(I1, TermOut1, OutArg1),
      arg(I1, TermOut2, OutArg2),
      arg(I1, TermOut3, OutArg3),
      arg(I1, TermOut4, OutArg4),
      call(Pred, InArg, OutArg1, OutArg2, OutArg3, OutArg4),
      mapargs_args(Pred, TermIn, TermOut1, TermOut2, TermOut3, TermOut4, I1, N) ).

sumargs(Pred, Term, A0, A1) :-
    functor(Term, _, N),
    sumargs(Pred, Term, A0, A1, N).

sumargs_args(_, _, A0, A1, 0) :-
    !,
    A0 = A1.
sumargs_args(Pred, Term, A1, A3, N) :-
    arg(N, Term, Arg),
    N1 is N - 1,
    call(Pred, Arg, A1, A2),
    sumargs_args(Pred, Term, A2, A3, N1).


foldargs(Goal, S, V0, V) :-
	functor(S, _, Ar),
	foldargs_(Goal, S, V0, V, 0, Ar).

foldargs_(Goal, S, V0, V, I, N) :-
	( I == N ->  V0 = V ;
	    I1 is I+1,
	    arg(I1, S, A),
	    call(Goal, A, V0, V1),
	    foldargs_(Goal, S, V1, V, I1, N) ).

foldargs(Goal, S, O1, V0, V) :-
	functor(S, N, Ar),
	functor(O1, N, Ar),
	foldargs_(Goal, S, O1, V0, V, 0, Ar).

foldargs_(Goal, S, O1,  V0, V, I, N) :-
	( I == N ->  V0 = V ;
	    I1 is I+1,
	    arg(I1, S, A),
	    arg(I1, O1, A1),
	    call(Goal, A, A1, V0, V1),
	    foldargs_(Goal, S, O1, V1, V, I1, N) ).

foldargs(Goal, S, O1, O2, V0, V) :-
	functor(S, N, Ar),
	functor(O1, N, Ar),
	functor(O2, N, Ar),
	foldargs_(Goal, S, O1, O2, V0, V, 0, Ar).

foldargs_(Goal, S, O1, O2, V0, V, I, N) :-
	( I == N -> V0 = V ;
	    I1 is I+1,
	    arg(I1, S, A),
	    arg(I1, O1, A1),
	    arg(I1, O2, A2),
	    call(Goal, A, A1, A2, V0, V1),
	    foldargs_(Goal, S, O1, O2, V1, V, I1, N) ).

foldargs(Goal, S, O1, O2, O3, V0, V) :-
	functor(S, N, Ar),
	functor(O1, N, Ar),
	functor(O2, N, Ar),
	functor(O3, N, Ar),
	foldargs_(Goal, S, O1, O2, O3, V0, V, 0, Ar).

foldargs_(Goal, S, O1, O2, O3, V0, V, I, N) :-
	( I == N ->  V0 = V ;
	    I1 is I+1,
	    arg(I1, S, A),
	    arg(I1, O1, A1),
	    arg(I1, O2, A2),
	    arg(I1, O3, A3),
	    call(Goal, A, A1, A2, A3, V0, V1),
	    foldargs_(Goal, S, O1, O2, O3, V1, V, I1, N) ).


goal_expansion(mapargs(Meta, In), (functor(In, _Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(mapargs, 1, Proto, GoalName),
	append(MetaVars, [In, 0, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, I, Ar], RecursionHead),
	append_args(Pred, [AIn], Apply),
	append_args(HeadPrefix, [In, I1, Ar], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == 0 -> true ; I1 is I+1, arg(I1, In, AIn), Apply, RecursiveCall )
		    ], Mod).

goal_expansion(mapargs(Meta, In, Out), (functor(In, Name, Ar), functor(Out, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(mapargs, 2, Proto, GoalName),
	append(MetaVars, [In, Out, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out, I], RecursionHead),
	append_args(Pred, [AIn, AOut], Apply),
	append_args(HeadPrefix, [In, Out, I1], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == 0 -> true ; arg(I, In, AIn), arg(I, Out, AOut), Apply, I1 is I-1, RecursiveCall )
		    ], Mod).

goal_expansion(mapargs(Meta, In, Out1, Out2), (functor(In, Name, Ar), functor(Out1, Name, Ar), functor(Out2, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(mapargs, 3, Proto, GoalName),
	append(MetaVars, [In, Out1, Out2, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out1, Out2, I], RecursionHead),
	append_args(Pred, [AIn, AOut1, AOut2], Apply),
	append_args(HeadPrefix, [In, Out1, Out2, I1], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == 0 -> true ; arg(I, In, AIn), arg(I, Out1, AOut1), arg(I, Out2, AOut2), Apply, I1 is I-1, RecursiveCall )
		    ], Mod).

goal_expansion(mapargs(Meta, In, Out1, Out2, Out3), (functor(In, Name, Ar), functor(Out1, Name, Ar), functor(Out3, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(mapargs, 4, Proto, GoalName),
	append(MetaVars, [In, Out1, Out2, Out3, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out1, Out2, Out3, I], RecursionHead),
	append_args(Pred, [AIn, AOut1, AOut2, AOut3], Apply),
	append_args(HeadPrefix, [In, Out1, Out2, Out3, I1], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == 0 -> true ; arg(I, In, AIn), arg(I, Out1, AOut1), arg(I, Out2, AOut2), arg(I, Out3, AOut3), Apply, I1 is I-1, RecursiveCall )
		    ], Mod).

goal_expansion(mapargs(Meta, In, Out1, Out2, Out3, Out4), (functor(In, Name, Ar), functor(Out1, Name, Ar), functor(Out3, Name, Ar), functor(Out4, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(mapargs, 4, Proto, GoalName),
	append(MetaVars, [In, Out1, Out2, Out3, Out4, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out1, Out2, Out3, Out4, I], RecursionHead),
	append_args(Pred, [AIn, AOut1, AOut2, AOut3, AOut4], Apply),
	append_args(HeadPrefix, [In, Out1, Out2, Out3, Out4, I1], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == 0 -> true ; arg(I, In, AIn), arg(I, Out1, AOut1), arg(I, Out2, AOut2), arg(I, Out3, AOut3), arg(I, Out4, AOut4), Apply, I1 is I-1, RecursiveCall )
		    ], Mod).

goal_expansion(sumargs(Meta, Term, AccIn, AccOut), Mod:Goal) :-
	goal_expansion_allowed,
	prolog_load_context(module, Mod),
	Goal = (
		 Term =.. [_|TermArgs],
		 sumlist(Meta, TermArgs, AccIn, AccOut)
	       ).

goal_expansion(foldargs(Meta, In, Acc0, AccF), (functor(In, _Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldargs, 1, Proto, GoalName),
	append(MetaVars, [In, Acc0, AccF, 0, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, VAcc0, VAccF, I, Ar], RecursionHead),
	append_args(Pred, [AIn, VAcc0, VAccI], Apply),
	append_args(HeadPrefix, [In, VAccI, VAccF, I1, Ar], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == Ar -> VAcc0 = VAccF ; I1 is I+1, arg(I1, In, AIn), Apply, RecursiveCall )
		    ], Mod).

goal_expansion(foldargs(Meta, In, Out1, Acc0, AccF), (functor(In, Name, Ar), functor(Out1, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldargs, 2, Proto, GoalName),
	append(MetaVars, [In, Out1, Acc0, AccF, 0, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out1, VAcc0, VAccF, I, Ar], RecursionHead),
	append_args(Pred, [AIn, AOut1, VAcc0, VAccI], Apply),
	append_args(HeadPrefix, [In, Out1, VAccI, VAccF, I1, Ar], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == Ar -> VAcc0 = VAccF ; I1 is I+1, arg(I1, In, AIn), arg(I1, Out1, AOut1), Apply, RecursiveCall )
		    ], Mod).

goal_expansion(foldargs(Meta, In, Out1, Out2, Acc0, AccF), (functor(In, Name, Ar), functor(Out1, Name, Ar), functor(Out2, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldargs, 3, Proto, GoalName),
	append(MetaVars, [In, Out1, Out2, Acc0, AccF, 0, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out1, Out2, VAcc0, VAccF, I, Ar], RecursionHead),
	append_args(Pred, [AIn, AOut1, AOut2, VAcc0, VAccI], Apply),
	append_args(HeadPrefix, [In, Out1, Out2, VAccI, VAccF, I1, Ar], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == Ar -> VAcc0 = VAccF ; I1 is I+1, arg(I1, In, AIn), arg(I1, Out1, AOut1), arg(I1, Out2, AOut2), Apply, RecursiveCall )
		    ], Mod).

goal_expansion(foldargs(Meta, In, Out1, Out2, Out3, Acc0, AccF), (functor(In, Name, Ar), functor(Out1, Name, Ar), functor(Out2, Name, Ar), functor(Out3, Name, Ar), Mod:Goal)) :-
	goal_expansion_allowed,
	callable(Meta),
	prolog_load_context(module, Mod),
	aux_preds(Meta, MetaVars, Pred, PredVars, Proto),
	!,
	% the new goal
	pred_name(foldargs, 4, Proto, GoalName),
	append(MetaVars, [In, Out1, Out2, Out3, Acc0, AccF, 0, Ar], GoalArgs),
	Goal =.. [GoalName|GoalArgs],
	% the new predicate declaration
	HeadPrefix =.. [GoalName|PredVars],
	% the new predicate declaration
	append_args(HeadPrefix, [In, Out1, Out2, Out3, VAcc0, VAccF, I, Ar], RecursionHead),
	append_args(Pred, [AIn, AOut1, AOut2, AOut3, VAcc0, VAccI], Apply),
	append_args(HeadPrefix, [In, Out1, Out2, Out3, VAccI, VAccF, I1, Ar], RecursiveCall),
	compile_aux([
		     (RecursionHead :- I == Ar -> VAcc0 = VAccF ; I1 is I+1, arg(I1, In, AIn), arg(I1, Out1, AOut1), arg(I1, Out2, AOut2), arg(I1, Out3, AOut3), Apply, RecursiveCall )
		    ], Mod).

%% @}
