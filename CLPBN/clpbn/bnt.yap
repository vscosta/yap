
:- module(bnt, [dump_as_bnt/2,
		check_if_bnt_done/1]).

:- use_module(library(terms), [term_variables/2
		    ]).

:- use_module(library(ordsets), [ord_subtract/3,
				 ord_add_element/3
		    ]).

:- use_module(library(lists), [reverse/2
		    ]).

:- use_module(library(atts)).

:- use_module(library(heaps), [empty_heap/1,
			       add_to_heap/4,
			       heap_to_list/2
			      ]).

:- use_module(library(system), [exec/3
			      ]).

:- attribute topord/1, map/1.

check_if_bnt_done(Var) :-
	get_atts(Var, [map(_)]).

dump_as_bnt(GVars, [_|_]) :-
	exec('matlab -nojvm -nosplash',[pipe(CommandStream),pipe(Answer),pipe(Answer)],_),
	wait_for_matlab_prompt(Answer),
	get_value(clpbn_key, Key),
	send_command(CommandStream, Answer, 'cd /u/vitor/sw/BNT;~n', []),
%	send_command(CommandStream, Answer, 'cd /home/vitor/Yap/CLPBN/BNT;~n', []),
	send_command(CommandStream, Answer, 'add_BNT_to_path;~n', []),
	send_command(CommandStream, Answer, '~w = ~w;~n', ['$VAR'(Key), Key]),
	send_command(CommandStream, Answer, 'dag = zeros(~w,~w);~n', ['$VAR'(Key), '$VAR'(Key)]),
	Key1 is Key-1,
        dump_variable_indices(Key1, Vs, Heap, CommandStream, Answer),
	write_deps(Heap, CommandStream, Answer),
        find_observables(GVars, Observables),
	mknet(Vs, Key, Observables, CommandStream, Answer),
	dump_cpds(Vs, CommandStream, Answer),
	inf_engine(Vs, Key, CommandStream, Answer),
	output_answer(Observables, CommandStream, Answer),
	close(CommandStream),
	close(Answer).


dump_variable_indices(I, LF, HF, CommandStream, Answer) :-
	all_vars_with_deps(I, LDeps, LNoDeps),
	sort(LNoDeps, S0),
	empty_heap(H0),
	topsort(LDeps, S0, LNoDeps, LF, H0, HF),
	reverse(LF, Vs),
	dump_vlist(Vs, 0, CommandStream, Answer).

all_vars_with_deps(0, LDeps, LNoDeps) :- !,
	var_with_deps(0, LDeps, [], LNoDeps, []).
all_vars_with_deps(I, LDeps, LNoDeps) :-
	var_with_deps(I, LDeps, LDeps0, LNoDeps, LNoDeps0),
	I1 is I-1,
	all_vars_with_deps(I1, LDeps0, LNoDeps0).

var_with_deps(Indx, Deps, Deps0, NoDeps, NoDeps0) :-
	array_element(clpbn, Indx, V),
	clpbn:get_atts(V, [dist(_,_,VDeps)]), !,
	(VDeps = [] ->
	    NoDeps = [V|NoDeps0], Deps = Deps0 ;
	    sort(VDeps,SVDeps),
	    Deps = [[V|SVDeps]|Deps0], NoDeps = NoDeps0 ).
	
%
% this is a silly quadratic algorithm for topological sorting.
% it will have to do for now.
%
% to speedup things a bit I keep a sorted and unsorted version
% of the variables sorted so far.
%
topsort([], _, S, S, H, H) :- !.
topsort(LDeps, Sorted0, S0, SF, H, HF) :- !,
	delete_all(LDeps, Sorted0, S0, LI, SI, Sorted, H, HI),
	topsort(LI, Sorted, SI, SF, HI, HF).

delete_all([], SS, S, [], S, SS, H, H).
delete_all([[V|VDeps]|LDeps], SS, S, LF, SF, SSF, H0, HF) :-
	ord_subtract(VDeps, SS, VDepsI),
	ord_subtract(VDeps, VDepsI, Parents),
	add_parents_to_heap(Parents, V, H0, HI),
	( VDepsI = [] ->
	    LF = LI, ord_add_element(SS,V,SSI), SI = [V|S];
	    LF = [[V|VDepsI]|LI], SI = S, SSI = SS),
	delete_all(LDeps, SSI, SI, LI, SF, SSF, HI, HF).

add_parents_to_heap([], _, H, H).
add_parents_to_heap([P|Parents], V, H0, HF) :-
	add_to_heap(H0, P, V, HI), % if I put a HF here I get the debugger to loop
	add_parents_to_heap(Parents, V, HI, HF).

dump_vlist([], _, _, _).
dump_vlist([V|Vs], I, CommandStream, Answer) :-
	I1 is I+1,
	clpbn:get_atts(V,[key(_)]),
	send_command(CommandStream, Answer, '~w = ~w;~n', ['$VAR'(I), I1]),
	put_atts(V, [topord(I)]),
	I1 is I+1,
	dump_vlist(Vs, I1, CommandStream, Answer).

write_deps(H, CommandStream, Answer) :-
	heap_to_list(H,L),
	write_list_deps(L, CommandStream, Answer).

write_list_deps([], _, _).
write_list_deps([V-A|L], CommandStream, Answer) :-
	fetch_same_key(L, V, SK, LN),
	write_dep_relation([A|SK], V, CommandStream, Answer),
	write_list_deps(LN, CommandStream, Answer).

fetch_same_key([], _, [], []) :- !.
fetch_same_key([V1-A|L], V, [A|SK], LN) :- V1 == V, !,
	fetch_same_key(L, V, SK, LN).
fetch_same_key(L, _, [], L).

write_dep_relation([], _, _) :- !.
write_dep_relation([V], D, CommandStream, Answer) :- !,
	get_atts(V, [topord(IV)]),
	get_atts(D, [topord(ID)]),
	send_command(CommandStream, Answer, "dag(~w,~w) = 1;~n", ['$VAR'(ID),'$VAR'(IV)]).
write_dep_relation(Vs, D, CommandStream, Answer) :-
	get_atts(D, [topord(ID)]),
	my_format(CommandStream, "dag(~w,[",['$VAR'(ID)]),
	write_anc_list(Vs, start, CommandStream),
	send_command(CommandStream, Answer, "]) = 1;~n", []).
	
write_anc_list([], _, _).
write_anc_list([V|Vs], start, CommandStream) :- !,
	get_atts(V, [topord(I)]),
	my_format(CommandStream, "~w",['$VAR'(I)]),
	write_anc_list(Vs, cont, CommandStream).
write_anc_list([V|Vs], cont, CommandStream) :-
	get_atts(V, [topord(I)]),
	my_format(CommandStream, " ~w",['$VAR'(I)]),
	write_anc_list(Vs, cont, CommandStream).

mknet(Vs, Key, Observables, CommandStream, Answer) :-
	mknet_all_discrete(Vs, Key, Observables, CommandStream, Answer).


mknet_all_discrete(Vs, Key, Observables, CommandStream, Answer) :-
	send_command(CommandStream, Answer, "discrete_nodes = 1:~w;~n",['$VAR'(Key)]),
	my_format(CommandStream, "ns = [", []),
	reverse(Vs, RVs),
	send_var_sizes(RVs, CommandStream),
	send_command(CommandStream, Answer, "];~n", []),
	my_format(CommandStream, "onodes = [", []),
	dump_observables(Observables, start, CommandStream),
	send_command(CommandStream, Answer, "];~n", []),
	send_command(CommandStream, Answer, "bnet = mk_bnet(dag, ns, 'discrete', discrete_nodes, 'observed', onodes);~n", []).

send_var_sizes([V], CommandStream) :- !,
	clpbn:get_atts(V, [dist(_,Tab,_)]),
	length(Tab, Sz),
	my_format(CommandStream, "~w", [Sz]).
send_var_sizes([V|Vs], CommandStream) :-
	clpbn:get_atts(V, [dist(_,Tab,_)]),
	length(Tab, Sz),
	my_format(CommandStream, "~w ", [Sz]),
	send_var_sizes(Vs, CommandStream).


dump_observables([], _, _) :- !.
dump_observables([Observable|Observables], start, CommandStream) :- !,
	get_atts(Observable, [topord(I)]),	
	my_format(CommandStream, "~w",['$VAR'(I)]),
	dump_observables(Observables, mid, CommandStream).
dump_observables([Observable|Observables], mid, CommandStream) :-
	get_atts(Observable, [topord(I)]),	
	my_format(CommandStream, " ~w",['$VAR'(I)]),
	dump_observables(Observables, mid, CommandStream).
	
dump_cpds([], _, _).
dump_cpds([V|Vs], CommandStream, Answer) :-
	clpbn:get_atts(V, [dist(Domain,_,_)]),
	dump_cpds(Vs, CommandStream, Answer),
	dump_dist(Domain, V, CommandStream, Answer).

%
% this is a discrete distribution
%
dump_dist(Vs, average, Ss, V, CommandStream, Answer) :- !,
	vals_map(Vs, 1, Map),
	get_atts(V, [topord(I)]),
	my_format(CommandStream, "bnet.CPD{~w} = deterministic_CPD(bnet, ~w, inline('round(mean([",['$VAR'(I),'$VAR'(I)]),
	put_atts(V, [map(Map)]),
	length(Ss, Len),
	dump_indices(0,Len,CommandStream),
	send_command(CommandStream, Answer, "]))'));~n",[]).
dump_dist(Vs, sum, Ss, V, CommandStream, Answer) :- !,
	vals_map(Vs, 1, Map),
	get_atts(V, [topord(I)]),
	my_format(CommandStream, "bnet.CPD{~w} = deterministic_CPD(bnet, ~w, inline('sum([",['$VAR'(I),'$VAR'(I)]),
	put_atts(V, [map(Map)]),
	length(Ss, Len),
	dump_indices(0,Len,CommandStream),
	send_command(CommandStream, Answer, "])'));~n",[]).
dump_dist(Vs, normalised_average(N), Ss, V, CommandStream, Answer) :- !,
	vals_map(Vs, 1, Map),
	get_atts(V, [topord(I)]),
	my_format(CommandStream, "bnet.CPD{~w} = deterministic_CPD(bnet, ~w, inline('round((sum([",['$VAR'(I),'$VAR'(I)]),
	put_atts(V, [map(Map)]),
	length(Ss, Len),
	dump_indices(0,Len,CommandStream),
	N2 is N//2,
	send_command(CommandStream, Answer, "])+~d)/~d)'));~n",[N2,N]).
dump_dist(Vs,Ds,Ss0, V, CommandStream, Answer) :- !,
	vals_map(Vs, 1, Map),
	get_atts(V, [topord(I)]),
	my_format(CommandStream, "bnet.CPD{~w} = tabular_CPD(bnet, ~w, [ ",['$VAR'(I),'$VAR'(I)]),
	put_atts(V, [map(Map)]),
	reverse([V|Ss0], Ss),
	get_numbers_for_vars(Ss, Ns),
	calculate_new_numbers(Ds,Ns,0,KDs0),
	keysort(KDs0,KDs),
	dump_elements(KDs, CommandStream),
	send_command(CommandStream, Answer, "]);~n",[]).

vals_map([], _, []).
vals_map([V|Vs], I, [[V|I]|Map]) :-
	I1 is I+1,
	vals_map(Vs, I1, Map).

get_numbers_for_vars(Ss, Ns) :-
	numb_vars(Ss, 0, 1, VPs0),
	keysort(VPs0, VPs),
	compute_new_factors(VPs, 1, Int0),
	keysort(Int0, Int),
	select_factors(Int, [], Ns).
	

numb_vars([], _, _, []).
numb_vars([V|Vs], I, A0, [T-p(I,A0,L)|VPs]) :-
	get_atts(V, [map(Map),topord(T)]),
	length(Map,L),
	I1 is I+1,
	Ai is A0*L,
	numb_vars(Vs, I1, Ai, VPs).

compute_new_factors([], _, []).
compute_new_factors([_-p(I,Siz,L)|VPs], Div, [I-f(Siz,Div)|Os]) :-
	NDiv is Div*L,
	compute_new_factors(VPs, NDiv, Os).

select_factors([], L, L).
select_factors([_-Fac|Int], Ns0, Nsf) :-
	select_factors(Int, [Fac|Ns0], Nsf).

calculate_new_numbers([],_, _,[]).
calculate_new_numbers([P|Ps], Ls, I0, [Pos-P|KDs]) :-
	compute_new_position(Ls, I0, 0, Pos),
	I is I0+1,
	calculate_new_numbers(Ps, Ls, I, KDs).

compute_new_position([], _, P, P).
compute_new_position([f(Siz,Div)|Ls], I0, P0, Pf) :-
	A is I0 // Siz,
	I1 is I0 mod Siz,
	B is A*Div,
	Pi is P0+B,
	compute_new_position(Ls, I1, Pi, Pf).

dump_indices(Len,Len,_) :- !.
dump_indices(I0,Len,CommandStream) :-
	I is I0+1,
	my_format(CommandStream, "x(~d) ",[I]),	
	dump_indices(I,Len,CommandStream).

dump_elements([], _).
dump_elements([_-P|KDs], CommandStream) :-
	my_format(CommandStream, "~w~n",[P]),
	dump_elements(KDs, CommandStream).

dump_problist([], _).
dump_problist([P|KDs], CommandStream) :-
	my_format(CommandStream, "~w~n",[P]),
	dump_problist(KDs, CommandStream).

dump_dlist((D1;D2), Start, CommandStream) :- !,
	dump_dlist(D1, Start, CommandStream),
	dump_dlist(D2, mid, CommandStream).
dump_dlist((_ = V), Pos, CommandStream) :- !,
	dump_dlist(V, Pos, CommandStream).
dump_dlist((_ -> V), Pos, CommandStream) :- !,
	dump_dlist(V, Pos, CommandStream).
dump_dlist(V, start, CommandStream) :- !,
	my_format(CommandStream, "~w~n",[V]).
dump_dlist(V, mid, CommandStream) :-
	my_format(CommandStream, "~w~n",[V]).

find_map((D1;D2), I0, N, LF, L0) :- !,
	find_map(D1, I0, I, LF, LI),
	find_map(D2, I,  N, LI, L0).
find_map((M->_), I, I1, [[M|I]|L0], L0) :-
	I1 is I+1.
	
inf_engine(Vs, Key, CommandStream, Answer) :-
	send_command(CommandStream, Answer, "engine = jtree_inf_engine(bnet)~n", []),
%	send_command(CommandStream, Answer, "engine = var_elim_inf_engine(bnet)~n", []),
	send_command(CommandStream, Answer, "evidence = cell(1,~w)~n", ['$VAR'(Key)]),
	dump_evidence(Vs, CommandStream, Answer),
	send_command(CommandStream, Answer, "[engine, loglik] = enter_evidence(engine, evidence)~n",[]).

dump_evidence([], _, _).
dump_evidence([V|Vs], CommandStream, Answer) :-
	clpbn:get_atts(V, [evidence(Ev)]), !,
	get_atts(V, [topord(I),map(M)]), !,
	follow_map(M,Ev,NEv),
	send_command(CommandStream, Answer, "evidence{~w} = ~w~n", ['$VAR'(I),NEv]),
	dump_evidence(Vs, CommandStream, Answer).
dump_evidence([_|Vs], CommandStream, Answer) :-
	dump_evidence(Vs, CommandStream, Answer).

follow_map([[K|V]|_], K, V) :- !.
follow_map([_|Map], K, V) :- !,
	follow_map(Map, K, V).

find_observables([], []).
find_observables([Var|GVars], [Var|Observables]) :-
	clpbn:get_atts(Var, [dist(_,_,_)]), !,
	find_observables(GVars, Observables).
find_observables([_|GVars], Observables) :-
	find_observables(GVars, Observables).

output_answer(Observables, CommandStream, Answer) :-
	split_by_cliques(Observables, Cliques),
	output_cliques(Cliques, CommandStream, Answer).

split_by_cliques([], []).
split_by_cliques([V|Vs], Cliques) :-
	split_by_cliques(Vs, Cliques0),
	add_to_cliques(Cliques0, V, Cliques).

add_to_cliques([], V, [[V]]).
add_to_cliques([Cl|L], V, [[V|Cl]|L]) :-
	in_clique(Cl,V), !.
add_to_cliques([Cl|L], V, [Cl|LN]) :-
	add_to_cliques(L, V, LN).

in_clique([], _).
in_clique([V1|L], V) :-
	child(V, V1), !,
	in_clique(L, V).
in_clique([V1|L], V) :-
	child(V1, V),
	in_clique(L,V).

child(V,V1) :-
	clpbn:get_atts(V, [dist(_,_,LVs)]),
	varmember(LVs, V1).

varmember([H|_], V1) :- H == V1, !.
varmember([_|L], V1) :-
	varmember(L, V1).

output_cliques([], _, _).
output_cliques([Observables|Cliques], CommandStream, Answer) :-
	marginal(Observables, CommandStream, Answer),
	read_answer(Answer, -1, MargDis),
	parse_observables(Observables, MargDis),
	output_cliques(Cliques, CommandStream, Answer).

marginal(Margs, CommandStream, Answer) :-
	my_format(CommandStream, "marg = marginal_nodes(engine, ", []),
	write_margs(Margs, CommandStream),
	send_command(CommandStream, Answer, ")~n", []),
	my_format(CommandStream, "p = marg.T~n", []).


write_margs([], _) :- !.
write_margs([V], CommandStream) :- !,
	get_atts(V, [topord(IV)]),
	my_format(CommandStream, "~w", ['$VAR'(IV)]).
write_margs(Vs, CommandStream) :-
	my_format(CommandStream, "[", []),
	write_anc_list(Vs, start, CommandStream),
	my_format(CommandStream, "]", []).


	
read_answer(Answer, C0, [C1|L]) :-
	get0(Answer, C1),
	put(user_error, C1),
	( (( C0 = 10 ; C0 = 85) ,C1 = 62) ->
	    L = []
	;
	    read_answer(Answer, C1, L)
	).

wait_for_matlab_prompt(Answer) :-
	fetch_prompt(Answer, -1). 

fetch_prompt(Answer, C0) :-
	get0(Answer, C1),
	put(user_error, C1),
	( ((C0 = 62 ; C0 = 85) ,C1 = 62) ->
	    true
	;
	    fetch_prompt(Answer, C1)
	).

send_command(OStream, IStream, String, Args) :-
	my_format(OStream, String, Args),
	wait_for_matlab_prompt(IStream).

parse_observables([Obs], MargDis) :- !,
	get_atts(Obs, [map(Map)]),
	skip_to_eq(MargDis, L1),
	fetch_map(Map, L1, Out),
	clpbn:get_atts(Obs, [key(Key)]),
	Obs = {Key:Out}.
parse_observables(LObs, MargDis) :-
	joint_map(LObs, Map),
	skip_to_eq(MargDis, L1),
	fetch_maps(Map, L1, Out),
	bind_lobs(LObs, Key, Key, Out).

fetch_map([[Name|_]], L, (Name -> P)) :- !,
	get_next_float(L, P, _).
fetch_map([[Name|_]|Names], L0, (Name->P ; Rest)) :-
	get_next_float(L0, P, Lf),
	fetch_map(Names, Lf, Rest).

get_next_float(L0, P, Lf) :-
	skip_spaces(L0, Li),
	fetch_float(Li,Ls, Lf),
	number_codes(P, Ls).

skip_to_eq([61|L], L) :- !.
skip_to_eq([_|L], LF) :-
	skip_to_eq(L, LF).

skip_spaces([10|L], LF) :- !, skip_spaces(L, LF).
skip_spaces([32|L], LF) :- !, skip_spaces(L, LF).
skip_spaces(L, L).

fetch_float([10|L], [], L) :- !.
fetch_float([32|L], [], L) :- !.
fetch_float([C|Li], [C|Ls], Lf) :-
	fetch_float(Li, Ls, Lf).

joint_map(Vars,FMap) :-
	fetch_maps(Vars,Maps),
	join_maps(Maps, FMap).

fetch_maps([], []).
fetch_maps([V|Vs], [M|Ms]) :-
	get_atts(V, [map(M)]),
	fetch_maps(Vs, Ms).

join_maps([], [[]]).
join_maps([Map|Maps], Rf) :-
	join_maps(Maps, R1),
	add(Map, R1, Rf).

add([], _, []).
add([[Name|_]|R], R1, RsF) :-
	add_head(R1, Name, RsF, Rs0),
	add(R, R1, Rs0).

add_head([], _, Rs, Rs).
add_head([H|L], A, [[A|H]|Rs], Rs0) :-
	add_head(L, A, Rs, Rs0).


fetch_maps([Name1], L, (Name2 -> P)) :- !,
	generate_name(Name1, Name2),
	get_next_float(L, P, _).
fetch_maps([Name1|Names], L0, (Name2->P ; Rest)) :-
	generate_name(Name1, Name2),
	get_next_float(L0, P, Lf),
	fetch_maps(Names, Lf, Rest).

generate_name([Name], Name) :- !.
generate_name([Name|Names], (Name,New)) :-
	generate_name(Names, New).

bind_lobs([Obs], Key, FullKey, Out) :- !,
	clpbn:get_atts(Obs, [key(Key)]),
	Obs = {FullKey:Out}.
bind_lobs([Obs|Lobs], (Key,Rest), FullKey, Out) :-
	clpbn:get_atts(Obs, [key(Key)]),
	Obs = {FullKey:Out},
	bind_lobs(Lobs, Rest, FullKey, Out).


my_format(Stream, String, Args) :-
	format(user_error, String, Args),
	format(Stream, String, Args).

