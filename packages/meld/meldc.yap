%
% A compiler for Meld programs
% can understand aggregates.
%

:- module(meld_compiler,
	[
	 mcompile/1,
	 input_graph/1,
	 run/1
	]).

:- use_module(meldi,
	[
	 push/1,
	 first/2,
	 min/3,
	 max/3,
	 sum/3
	]).

:- use_module(meld).

:- use_module(library(meld)).

:- use_module(library(terms), [
        variable_in_term/2
    ]).

:- use_module(library(ordsets), [
				 ord_subset/2,
				 ord_union/3
				]).

:- dynamic meld_constants:const/2.

mcompile(Program) :-
	open(Program, read, P),
	init_mcompile(Program),
	repeat,
	read_term(P, Term, [variable_names(Vars), module(meld_compiler)]),
	(
	 Term == end_of_file
	->
	 !,
	 close(P)
	;
	 mcompile(Term, Program, Vars),
	 fail
	).

init_mcompile(Program) :-
	retractall(type(_, _, Program, _)).

mcompile(type(T), Program, Vars) :-
	ground_term(T, Vars),
	type_declaration(T, Program), !.
mcompile(const(T=V), _Program, Vars) :-
	ground_term(T, Vars),
	const_declaration(T, V), !.
mcompile((Head :- Body), _, _Vars) :-
	rule(Head, Body).

type_declaration(extensional(T), Program) :- !,
	functor(T, Na, Arity),
	functor(NT, Na, Arity),
	assert(meld_topdown:extensional(NT, Na, Arity)),
	type_declaration(T, Program). 
type_declaration(logical_neighbor(T), Program) :- !,
	type_declaration(T, Program). 
type_declaration(persistent(T), Program) :- !,
	type_declaration(T, Program). 
type_declaration(extern(T), Program) :- !,
	type_declaration(T, Program). 
type_declaration(T, _) :-
	functor(T, N, A),
	dynamic(meld_program:N/A),
	fail.
type_declaration(T, Program) :-
	T =.. [P|Args],
	check_aggregate(Args, 1, NewArgs, Aggregation, Arg),
	!,
	NT =.. [P|NewArgs],
	assert_type(NT, Program, aggregation(Aggregation, Arg)).
type_declaration(T, Program) :-
	assert_type(T, Program, horn).

assert_type(NT, Program, Agg) :-
	functor(NT, Na, Ar),
	functor(Spec, Na, Ar),
	assert(type(Spec, NT, Program, Agg)).

const_declaration(C,V) :- !,
	( atom(C) -> true ; throw(type_error(atom,C),const(C=V))),
	( number(V) -> true ; throw(type_error(number,V),const(C=V))),
	assert(meld_constants:const(C, V)). 

check_aggregate([first(Type)|Args], I, [Type|Args], first, I) :- !.
check_aggregate([max(Type)|Args], I, [Type|Args], max, I) :- !.
check_aggregate([min(Type)|Args], I, [Type|Args], min, I) :- !.
check_aggregate([sum(Type)|Args], I, [Type|Args], sum, I) :- !.
check_aggregate([Type|Args], I, [Type|NewArgs], Agg, Arg) :-
	atom(Type),
	I1 is I+1,
	check_aggregate(Args, I1, NewArgs, Agg, Arg).

ground_term(_, []).

%
% Rule compiler
%
rule(Head, Body) :-
        bodytolist(Body, L0, []),
        builtins(L0, L, R),
        builtins([Head], RLH, []),
	join(RLH, NHead, R, []),
	compile_goals(L, [], NHead).

builtins([]) --> [].
builtins(G.Gs) -->
	builtin(G),
        builtins(Gs).

builtin(Res = Op) --> !,
	process_constants(Op, Res).
builtin(Goal) -->
	process_constants(Goal, NGoal),
	[ NGoal ].

process_constants(G, G) -->
	{ var(G) }, !.
process_constants(C, V) -->
	{ meld_constants:const(C,V) }, !.
process_constants(G, G) -->
	{ atomic(G) }, !.
process_constants(to_float(Arg1), NArg1) --> !,
	process_constants(Arg1, NArg1).
process_constants(A, NA) --> 
	{ arithmetic(A, Op, Arg1, Arg2) }, !,
	process_constants(Arg1, NArg1),
	process_constants(Arg2, NArg2),
	{ arithmetic(NExp, Op, NArg1, NArg2) },
	[ NA is NExp ].
process_constants(A, NA) --> 
	{ arithmetic(A, Op, Arg1) }, !,
	process_constants(Arg1, NArg1),
	{ arithmetic(NExp, Op, NArg1) },
	[ NA is NExp ].
process_constants(G, NG) -->
	{ G =.. [A|Args] },
	process_args(Args, NArgs),
	{ NG =.. [A|NArgs] }.
	
process_args([], []) --> [].
process_args(A.Args, NA.NArgs) -->
	process_constants(A, NA),
	process_args(Args, NArgs).

join([H0], H0) --> !.
join([H|T], H0) -->
	[H],
	join(T, H0).

compile_goals([], _, _).
compile_goals([Goal|Goals], Gs, Head) :-
	compile_goal(Goal, Goals, Gs, Head),
	compile_goals(Goals, [Goal|Gs], Head).

compile_goal(BIP, _Goals, _Gs, _Head) :-
	meld_builtin(BIP, _, _), !.
compile_goal((forall G then Do), Goals, Gs, Head) :- !,
	% make sure quantified variables are not seen outside
	quantified_vars(G,Gs+Goals,NG),
	%
	% just collect the body into a number of goals
	%
	collect_body(Gs, [(forall G then Do)|Goals], BLF, BL1),
	% make a backup copy for deletion
	copy_term(h(Head,BLF,BL1,NG), h(Head,DelBLF,DelBL1,DelNG)),
	% add the operation, usually push
	extra_head(Head, BL1, []),
	% add the delete operation
	extra_delete(Head, DelBL1, []),
	% reorder builtins
	reorder_builtins(NG, BLF, BLF2),
	reorder_builtins(DelNG, DelBLF, DelBLF2),
	% create the body as a conjunction
	listtobody(BLF2, Body),
	listtobody(DelBLF2, DelBody),
	% done
	assert_static(meld_program:(run(NG) :- Body)),
	assert_static(meld_program:(run(delete(DelNG)) :- DelBody)).
compile_goal(Goal, Goals, Gs, Head) :-
	collect_body(Gs, Goals, BLF, BL1),
	copy_term(h(Head,BLF,BL1,Goal), h(Head,DelBLF,DelBL1,DelGoal)),
	extra_head(Head, BL1, []),
	extra_delete(Head, DelBL1, []),
	% reorder builtins
	reorder_builtins(Goal, BLF, BLF2),
	reorder_builtins(DelGoal, DelBLF, DelBLF2),
	listtobody(BLF2, Body),
	listtobody(DelBLF2, DelBody),
	assert_static(meld_program:(run(Goal) :- Body)),
	assert_static(meld_program:(run(deleted(DelGoal)) :- DelBody)).

% quantified variables should not leave the scope of the forall.
quantified_vars(G,Extern,NG) :-
	term_variables(G, TVs),
	copy_term(G+TVs,NG+NTVs),
	bind_external(TVs, NTVs, Extern).

bind_external([], [], _).
bind_external(V.TVs, NV.NTVs, Extern) :-
	variable_in_term(Extern, V), !,
	V = NV,
	bind_external(TVs, NTVs, Extern).
bind_external(_.TVs, _.NTVs, Extern) :-
	bind_external(TVs, NTVs, Extern).

% a very simple version
%
collect_body([], []) --> [].
collect_body([G|Gs], MGs) -->
	process_goal(G),
	collect_body(Gs, MGs).
collect_body([], [G|Gs]) -->
	process_goal(G),
	collect_body([], Gs).

process_goal((forall Goal then Conj)) --> !,
	[(Goal, \+ Conj -> fail ; true)].
process_goal( G ) -->
	[G].



extra_head(Head) -->
	{ type(Head, _, _, horn) },
	[push(Head)].
extra_head(Head) -->
	{ type(Head, _, _, aggregation(first, Arg)), 
	  freshen(Head, Arg, VHead) },
	[ meld_interpreter:first(VHead, Head)].
extra_head(Head) -->
	{ type(Head, _, _, aggregation(max, Arg)), 
          freshen(Head, Arg, VHead) },
	[ meld_interpreter:max(VHead, Arg, Head)].
extra_head(Head) -->
	{ type(Head, _, _, aggregation(min, Arg)), 
          freshen(Head, Arg, VHead) },
	[ meld_interpreter:min(VHead, Arg, Head)].
extra_head(Head) -->
	{ type(Head, _, _, aggregation(sum, Arg)), 
          freshen(Head, Arg, VHead) },
	[ meld_interpreter:sum(VHead, Arg, Head)].

extra_delete(Head) -->
	{ type(Head, _, _, horn) },
	[meld_interpreter:deleted(Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(first, Arg)), 
	  freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_first(VHead, Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(max, Arg)), 
	  freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_max(VHead, Arg, Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(min, Arg)), 
	  freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_min(VHead, Arg, Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(sum, Arg)), 
	  freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_sum(VHead, Arg, Head)].

freshen(Head, Arg, VHead) :-
	Head =.. [N|Args],
	freshen_arg(Arg, Args, VArgs),
	VHead =.. [N|VArgs].

freshen_arg(1, [_|Args], [_|Args]) :- !.
freshen_arg(N, A.Args, A.VArgs) :-
	N1 is N-1,
	freshen_arg(N1, Args, VArgs).

input_graph(Program) :-
	open(Program, read, P),
	repeat,
	read_term(P, Term, [variable_names(_Vars), module(meld_compiler)]),
	(
	 Term == end_of_file
	->
	 !,
	 close(P)
	;
	 add_graph_fact(Term),
	 fail
	).

add_graph_fact(Term) :-
	 push(Term).

bodytolist((G1,G2)) -->
	!,
	bodytolist(G1),
	bodytolist(G2).
bodytolist(G) -->
	[G].

listtobody([G], G) :- !.
listtobody([G|GL], (G,Gs)) :-
	listtobody(GL, Gs).

reorder_builtins(Head, BLF, BLF2) :-
	term_variables(Head, Vs0),
	sort(Vs0, Vs),
	reorder_term(BLF, Vs, [], BLF2).

% 4 arguments
% list of input goals
% queue of built-ins waiting for execution
% list of current variables
% output variables
%
reorder_term([], _, [], []).
reorder_term(G.Gs, Vs0, Queue, NGs) :-
        meld_builtin(G, Is, Os), !,
	term_variables(Is, InpVs0),
	sort(InpVs0, InpVs),
	continue_reorder_term(Gs, G, InpVs, Vs0, Queue, Os, NGs).
reorder_term(G.Gs, Vs0, Queue, G.NGs) :-
	term_variables(G, GVs0),
	sort(GVs0, GVs),
	ord_union(GVs, Vs0, Vs),
	wake_queue(Queue, NewQueue, Vs, Vs0, NewQueue, FVs, NGs, NGs0),
	reorder_term(Gs, FVs, NewQueue, NGs0).

continue_reorder_term(Gs, G, InpVs, Vs0, Queue, Os, G.NGs) :-
	ord_subset(InpVs, Vs0), !,
	term_variables(Os, OutVs0),
	sort(OutVs0, OutVs),
	ord_union(OutVs, Vs0, Vs),
	wake_queue(Queue, NewQueue, Vs, Vs0, NewQueue, FVs, NGs, NGs0),
	reorder_term(Gs, FVs, NewQueue, NGs0).
continue_reorder_term(Gs, G, InpVs, Vs0, Queue, Os, NGs) :-
	term_variables(Os, OutVs0),
	sort(OutVs0, OutVs),
	reorder_term(Gs, Vs0, q(InpVs, OutVs, G).Queue, NGs).
	
wake_queue([], _, Vs, _, [], Vs) --> [].
wake_queue(Q.Queue, _, Vs, Vs0, Q.Queue, Vs) --> { Vs == Vs0 }, !.
wake_queue(q(InpVs,OutVs,G).Queue, NewQueue, Vs, Vs0, Queue, FVs) -->
	{ ord_subset(InpVs, Vs) }, !,
	[G],
	{ ord_union(OutVs, Vs, NVs) },
	% restart from beginning
	wake_queue(NewQueue, NewNewQueue, NVs, Vs0, NewNewQueue, FVs).
wake_queue(Q.Queue, NewQueue, NVs, Vs0, Q.NQueue, FVs) -->
	wake_queue(Queue, NewQueue, NVs, Vs0, NQueue, FVs).


meld_builtin(O is I, I, O).
meld_builtin(I1 =< I2, I1-I2, []).
meld_builtin(I1 >= I2, I1-I2, []).
meld_builtin(I1 =:= I2, I1-I2, []).

arithmetic( A+B, (+), A, B).
arithmetic( A-B, (-), A, B).
arithmetic( A*B, (*), A, B).
arithmetic( A/B, (/), A, B).

arithmetic( sin(A), sin, A).
arithmetic( cos(A), cos, A).
arithmetic( tan(A), tan, A).
