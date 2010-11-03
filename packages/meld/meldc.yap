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
	 max/3
	]).

:- use_module(library(meld)).

:- use_module(meldtd,
	      [
	       meld_top_down_compile/2,
	       meld_top_down_aggregate/3
	       ]).

:- use_module(library(terms), [
        variable_in_term/2
    ]).

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
mcompile((Head :- Body), _, _Vars) :-
	rule(Head, Body).

type_declaration(extensional(T), Program) :- !,
	functor(T, Na, Arity),
	functor(NT, Na, Arity),
	assert(meld_topdown:extensional(NT, Na, Arity)),
	type_declaration(T, Program). 
type_declaration(logical_neighbor(T), Program) :- !,
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
	meld_top_down_aggregate(T, Aggregation, Arg),
	assert_type(NT, Program, aggregation(Aggregation, Arg)).
type_declaration(T, Program) :-
	meld_top_down_aggregate(T, horn, _),
	assert_type(T, Program, horn).

assert_type(NT, Program, Agg) :-
	functor(NT, Na, Ar),
	functor(Spec, Na, Ar),
	assert(type(Spec, NT, Program, Agg)).

check_aggregate([first(Type)|Args], I, [Type|Args], first, I) :- !.
check_aggregate([max(Type)|Args], I, [Type|Args], max, I) :- !.
check_aggregate([min(Type)|Args], I, [Type|Args], min, I) :- !.
check_aggregate([Type|Args], I, [Type|NewArgs], Agg, Arg) :-
	atom(Type),
	I1 is I+1,
	check_aggregate(Args, I1, NewArgs, Agg, Arg).

ground_term(_, []).

rule(Head, Body) :-
        bodytolist(Body, L, []),
	compile_goals(L, [], Head),
	meld_top_down_compile(Head, Body).

compile_goals([], _, _).
compile_goals([Goal|Goals], Gs, Head) :-
	compile_goal(Goal, Goals, Gs, Head),
	compile_goals(Goals, [Goal|Gs], Head).

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
	% create the body as a conjunction
	listtobody(BLF, Body),
	listtobody(DelBLF, DelBody),
	% done
	assert(meld_program:(run(NG) :- Body)),
	assert(meld_program:(run(delete(DelNG)) :- DelBody)).
compile_goal(Goal, Goals, Gs, Head) :-
	collect_body(Gs, Goals, BLF, BL1),
	copy_term(h(Head,BLF,BL1,Goal), h(Head,DelBLF,DelBL1,DelGoal)),
	extra_head(Head, BL1, []),
	extra_delete(Head, DelBL1, []),
	listtobody(BLF, Body),
	listtobody(DelBLF, DelBody),
	assert(meld_program:(run(Goal) :- Body)),
	assert(meld_program:(run(deleted(DelGoal)) :- DelBody)).

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
	{ type(Head, _, _, aggregation(first, Arg)), freshen(Head, Arg, VHead) },
	[ meld_interpreter:first(VHead, Head)].
extra_head(Head) -->
	{ type(Head, _, _, aggregation(max, Arg)), freshen(Head, Arg, VHead) },
	[ meld_interpreter:max(VHead, Arg, Head)].
extra_head(Head) -->
	{ type(Head, _, _, aggregation(min, Arg)), freshen(Head, Arg, VHead) },
	[ meld_interpreter:min(VHead, Arg, Head)].

extra_delete(Head) -->
	{ type(Head, _, _, horn) },
	[meld_interpreter:deleted(Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(first, Arg)), freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_first(VHead, Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(max, Arg)), freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_max(VHead, Arg, Head)].
extra_delete(Head) -->
	{ type(Head, _, _, aggregation(min, Arg)), freshen(Head, Arg, VHead) },
	[ meld_interpreter:delete_from_min(VHead, Arg, Head)].

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


