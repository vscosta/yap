%%% -*- Mode: Prolog; -*-
:-source.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prefix-trees for managing a DNF
% remembers shortest prefix of a conjunction only (i.e. a*b+a*b*c results in a*b only, but b*a+a*b*c is not reduced)
% children are sorted, but branches aren't (to speed up search while keeping structure sharing from proof procedure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* derived from tptree.yap from ProbLog by Fabrizio Riguzzi in 2009 
for dealing with multivalued variables
instead of variables or their negation, the script can contain equations of the 
form
variable=value
*/

:- module(ptree_lpad,[init_ptree/1,
	delete_ptree/1,
	rename_ptree/2,
	member_ptree/2,
	enum_member_ptree/2,
	insert_ptree/2,
	delete_ptree/2,
	edges_ptree/2,
	count_ptree/2,
	prune_check_ptree/2,
	empty_ptree/1,
	merge_ptree/3,
	bdd_ptree/3,
	bdd_ptree_map/4
	]).
:-source.
:- use_module(library(tries),
	      [
	       trie_open/1,
	       trie_close/1,
	       trie_stats/4,
	       trie_check_entry/3,
	       trie_get_entry/2,
	       trie_put_entry/3,
	       trie_remove_entry/1,
	       trie_usage/4,
	       trie_dup/2,
	       trie_join/2,
	       trie_traverse/2
	      ]).

:- use_module(library(ordsets),
	      [
	       ord_subset/2
	      ]).

:- style_check(all).
%:- yap_flag(unknown,error).

%:- use_module(flags,[problog_flag/2]).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(system)).

% name lexicon external - internal
sym(1,tree1) :- !.
sym(2,tree2) :- !.
sym(3,tree3) :- !.
sym(N,AN) :- atomic_concat([tree,N],AN).

%%%%%%%%%%%%%%%%%%%%%%%%
% ptree basics
%%%%%%%%%%%%%%%%%%%%%%%%

init_ptree(ID) :-
	sym(ID,Sym),
	trie_open(Trie),
	nb_setval(Sym, Trie).

delete_ptree(ID) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie), !,
	trie_close(Trie),
	trie_open(NewTrie),
	nb_setval(Sym, NewTrie).
delete_ptree(_).

rename_ptree(OldID,NewID) :-
	sym(OldID,OldSym),
	sym(NewID,NewSym),
	nb_getval(OldSym, Trie),
	nb_set_shared_val(NewSym, Trie).

empty_ptree(ID) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_usage(Trie, 0, 0, 0).
	

%%%%%%%%%%%%%%%%%%%%%%%%
% member 
%%%%%%%%%%%%%%%%%%%%%%%%

% non-backtrackable (to check)
member_ptree(List,ID) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_check_entry(Trie, List, _).

% backtrackable (to list)
enum_member_ptree(ID,List) :-
	sym(ID,Sym),
	nb_getval(Sym, Tree),
	trie_path(Tree, List).

trie_path(Tree, List) :-
	trie_traverse(Tree,Ref),
	trie_get_entry(Ref, List).

%%%%%%%%%%%%%%%%%%%%%%%%
% insert conjunction
%%%%%%%%%%%%%%%%%%%%%%%%
insert_ptree(true,ID) :-
	sym(ID,Sym),
	!,
	nb_getval(Sym, Trie),
	trie_close(Trie),
	trie_open(NTrie),
	trie_put_entry(NTrie, true, _).
insert_ptree(List,ID) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_put_entry(Trie, List, _).

%%%%%%%%%%%%%%%%%%%%%%%%
% delete conjunction
%%%%%%%%%%%%%%%%%%%%%%%%
delete_ptree(List,ID) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_check_entry(Trie, List, Ref),
	trie_remove_entry(Ref).


%%%%%%%%
% return list -Edges of all edge labels in ptree
% doesn't use any heuristic to order those for the BDD
% (automatic reordering has to do the job)
%%%%%%%%%
edges_ptree(ID,[]) :-
	empty_ptree(ID),
	!.
edges_ptree(ID,[]) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_check_entry(Trie, true, _),
	!.
edges_ptree(ID,Edges) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	%(
	setof(X, trie_literal(Trie, X), Edges).%->
	/*	true
	;
		Edges=[]
	).*/

trie_literal(Trie, X) :-
	trie_traverse(Trie,Ref),
	trie_get_entry(Ref, List),
	member(X, List).

%%%%%%%%
% number of conjunctions in the tree
%%%%%%%%%

count_ptree(ID,N) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_usage(Trie, N, _, _).

%%%%%%%%
% check whether some branch of ptree is a subset of conjunction List
% useful for pruning the search for proofs (optional due to time overhead)
% currently not implemented, just fails
%%%%%%%

prune_check_ptree(_List,_TreeID) :-
	format(user,'FAIL: prune check currently not supported~n',[]),
	flush_output(user),
	fail.

%%%%%%%%%%%%%
% merge two ptrees
% - take care not to loose proper prefixes that are proofs!
%%%%%%%%%%%%%%%

merge_ptree(ID1,_,ID3) :-
	sym(ID1,Sym1),
	sym(ID3,Sym3),
	nb_getval(Sym1, T1),
	trie_check_entry(T1, true, _),
	!,
	trie_open(T3),	
	trie_put_entry(T3, true, _),
	nb_setval(Sym3, T3).
merge_ptree(_,ID2,ID3) :-
	sym(ID2,Sym2),
	sym(ID3,Sym3),
	nb_getval(Sym2, T2),
	trie_check_entry(T2, true, _),
	!,
	trie_open(T3),	
	trie_put_entry(T3, true, _),
	nb_setval(Sym3, T3).
merge_ptree(ID1,ID2,ID3) :-
	sym(ID1,Sym1),
	sym(ID2,Sym2),
	sym(ID3,Sym3),
	nb_getval(Sym1, T1),
	nb_getval(Sym2, T2),
	trie_dup(T1, T3),
	trie_join(T3,T2),
	nb_setval(Sym3, T3).


%%%%%%%%%%%%%%%%%%%%%%%%
% write BDD info for given ptree to file
% - initializes leaf BDDs (=variables) first
% - then compresses ptree to exploit subtree sharing 
% - bdd_pt/1 does the work on the structure itself
%%%%%%%%%%%%%%%%%%%%%%%%

bdd_ptree(ID,FileBDD,FileParam) :-
	bdd_ptree_script(ID,FileBDD,FileParam),
	eraseall(map),
	eraseall(vars).

% version returning variable mapping
bdd_ptree_map(ID,FileBDD,FileParam,FileMapping) :-
	bdd_ptree_script(ID,FileBDD,FileParam),
	findall(X,recorded(map,X,_),Map),
	add_probs(Map,Mapping),
	tell(FileMapping),
	write(Mapping),write('.'),
	told,
	eraseall(map),
	eraseall(vars).

add_probs([],[]).
add_probs([m(R,S,Num,Name)|Map],[m(R,S,Num,Name,Prob)|Mapping]) :-
	user:rule_by_num(R,S,_N,Head,_Body),
	user:get_probs(Head,Prob),
	add_probs(Map,Mapping).

% number of variables may be to high:
% counted on trie, but conversion to old tree representation 
% transforms A*B+A to A (prefix-test)
bdd_ptree_script(ID,FileBDD,FileParam) :-
	assert(v_num(0)),
	edges_ptree(ID,Edges),
	compute_nvars(Edges,0,NVars,0,NBVars),
	tell(FileParam),
	bdd_vars_script(Edges),

	flush_output,

	told,
	length(Edges,_VarCount),
	assert(c_num(1)),
	bdd_pt(ID,CT),!,
	c_num(NN),
	IntermediateSteps is NN-1,
	tell(FileBDD),
	format('@BDD1~n~w~n~w~n~w~n~w~n',[NVars,NBVars,0,IntermediateSteps]),
	output_compressed_script_only(CT),!,
	
	told,
	retractall(c_num(_)),
	retractall(v_num(_)),
	retractall(compression(_,_)).

compute_nvars([],NV,NV,NBV,NBV).

compute_nvars([(_V,R,S)|T],NV0,NV1,NBV0,NBV1):-
	(recorded(vars,v(R,S),_)->
		compute_nvars(T,NV0,NV1,NBV0,NBV1)
	;
		recorda(vars,v(R,S),_),
		NV2 is NV0+1,
		user:rule_by_num(R,S,_N,Head,_Body),
		length(Head,L),
		NBV2 is NBV0+integer(ceiling(log(L)/log(2))),
		compute_nvars(T,NV2,NV1,NBV2,NBV1)
	).

% write parameter file by iterating over all var/not(var) occuring in the tree
/*bdd_vars_script(Edges) :-
	bdd_vars_script(Edges,0).
*/
bdd_vars_script([]).
%%%% Bernd, changes for negated ground facts
/*
bdd_vars_script([(_V,R,S)|B],N) :-
        recorded(map,m(R,S,_Num,_NameA),_),!,
	bdd_vars_script(B,N).
*/
bdd_vars_script([(_V,R,S)|B]) :-
        (recorded(map,m(R,S,_Number,_NameA),_)->
		true
	;
		user:rule_by_num(R,S,_N,Head,_Body),
		user:get_probs(Head,P),
		get_var_name(R,S,_Number,NameA),
		length(Head,NV),
		format('@~w~n~d~n',[NameA,NV]),
		print_probs(P)
	),
	bdd_vars_script(B).

print_probs([H]):-!,
	format("~f~n",[H]).

print_probs([H|T]):-
	format("~f ",[H]),
	print_probs(T).


%%%%%%%%%%%%%%%%%%%%%%%%
% find top level symbol for script
%%%%%%%%%%%%%%%%%%%%%%%%

% special cases: variable-free formulae
bdd_pt(ID,false) :-
	empty_ptree(ID),
	!,
	once(retractall(c_num(_))),
	once(assert(c_num(2))).
bdd_pt(ID,true) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_check_entry(Trie, true, _),
	!,
	once(retractall(c_num(_))),
	once(assert(c_num(2))).

% general case: transform trie to nested tree structure for compression
bdd_pt(ID,CT) :-
	sym(ID,Sym),
	nb_getval(Sym, Trie),
	trie_to_tree(Trie, Tree),
	compress_pt(Tree,CT).

trie_to_tree(Trie, Tree) :-
	findall(Path,trie_path(Trie, Path), Paths),
	add_trees(Paths, [], Tree).

add_trees([], Tree, Tree).
add_trees([List|Paths], Tree0, Tree) :-
	ins_pt(List, Tree0, TreeI),
	add_trees(Paths, TreeI, Tree).

ins_pt([],_T,[]) :- !.    
ins_pt([A|B],[s(A1,AT)|OldT],NewT) :-
	compare(Comp, A1, A),
        (Comp == = ->
	 (AT == [] ->  
	     NewT=[s(A1,AT)|OldT]
	   ;
	   NewT = [s(A1,NewAT)|OldT],
	   ins_pt(B, AT, NewAT))
	;
	 Comp == > ->
	 NewT = [s(A1,AT)|Tree],
	 ins_pt([A|B], OldT, Tree)
	;
	 NewT = [s(A,BTree),s(A1,AT)|OldT],
	 ins_pt(B,[],BTree)
	).
ins_pt([A|B],[],[s(A,NewAT)]) :-
	ins_pt(B,[],NewAT).

%%%%%%%%%%%%
% BDD compression: alternates and- and or-levels to build BDD bottom-up
% each sub-BDD will be either a conjunction of a one-node BDD with some BDD or a disjunction of BDDs
% uses the internal database to temporarily store a map of components
%%%%%%%%%%%%

% T is completely compressed and contains single variable
% i.e. T of form x12 or ~x34
compress_pt(T,TT) :- 
	atom(T),
	test_var_name(T),
	!,
	get_next_name(TT),
	assertz(compression(TT,[T])).
% T is completely compressed and contains subtrees
% i.e. T of form 'L56'
compress_pt(T,T) :- 
	atom(T).
% T not yet compressed
% i.e. T is a tree-term (nested list & s/2 structure)	
% -> execute one layer of compression, then check again
compress_pt(T,CT) :- 
	\+ atom(T),
	and_or_compression(T,IT),
	compress_pt(IT,CT).

% transform tree-term T into tree-term CT where last two layers have been processed
% i.e. introduce names for subparts (-> Map) and replace (all occurrenes of) subparts by this names
and_or_compression(T,CT) :-
	and_comp(T,AT),
	or_comp(AT,CT).

% replace leaves that are single child by variable representing father-AND-child
and_comp(T,AT) :-
	all_leaves_pt(T,Leaves),
	compression_mapping(Leaves,Map),
	replace_pt(T,Map,AT).

% replace list of siblings by variable representing their disjunction
or_comp(T,AT) :-
	all_leaflists_pt(T,Leaves),
	compression_mapping(Leaves,Map),
	replace_pt(T,Map,AT).

all_leaves_pt(T,L) :-
	all(X,some_leaf_pt(T,X),L).

some_leaf_pt([s(A,[])|_],s(A,[])).
some_leaf_pt([s(A,L)|_],s(A,L)) :-
	 not_or_atom(L).
some_leaf_pt([s(_,L)|_],X) :-
	some_leaf_pt(L,X).
some_leaf_pt([_|L],X) :-
	some_leaf_pt(L,X).

all_leaflists_pt(L,[L]) :-
	atomlist(L),!.
all_leaflists_pt(T,L) :-
	all(X,some_leaflist_pt(T,X),L),!.
all_leaflists_pt(_,[]).

some_leaflist_pt([s(_,L)|_],L) :-
	atomlist(L).
some_leaflist_pt([s(_,L)|_],X) :-
	some_leaflist_pt(L,X).
some_leaflist_pt([_|L],X) :-
	some_leaflist_pt(L,X).

not_or_atom(T) :-
	(
	    T=not(T0)
	-> 
	    atom(T0);
	    atom(T)
	).

atomlist([]).
atomlist([A|B]) :-
	 not_or_atom(A),
	atomlist(B).

% for each subtree that will be compressed, add its name
% only introduce 'L'-based names when subtree composes elements, store these in compression/2 for printing the script
compression_mapping([],[]).
compression_mapping([First|B],[N-First|BB]) :-
	(
	    First = s((V,R,S),[])          % subtree is literal -> use variable's name x17 from map (add ~ for negative case)
	-> 
      		recorded(map,m(R,S,_Num,Tmp),_), %check
		atomic_concat([Tmp,'-',V],N)
	;
	    (First = s(A,L),not_or_atom(L)) % subtree is node with single completely reduced child -> use next 'L'-based name
	    -> (get_next_name(N),
	        assertz(compression(N,s(A,L))))
	;
	    (First = [L],not_or_atom(L)) % subtree is an OR with a single completely reduced element -> use element's name
	     -> N=L
	     /*,
	     	recorded(refc,m(L,RefC),Ref),
	     	erase(Ref),
	     	RefC1 is RefC+1,
	     	recorda(refc,m(L,RefC1),_)*/
	;
	    (atomlist(First), % subtree is an OR with only (>1) completely reduced elements -> use next 'L'-based name
	    get_next_name(N),
	    assertz(compression(N,First)))
	),
	compression_mapping(B,BB).

increase_counts([]).

increase_counts([H|T]):-
	recorded(refc,m(H,RC),Ref),
	erase(Ref),
	RC1 is RC+1,
	recorda(refc,m(H,RC1),_),
	increase_counts(T).

compute_or([A],Node0,Node1):-
	recorded(mapnodes,m(A,Node),_),
	or(Node0,Node,Node1).

compute_or([A,B|T],Node0,Node1):-
        recorded(mapnodes,m(A,Node),_),
	or(Node0,Node,Node2),
	compute_or([B|T],Node2,Node1).


% replace_pt(+T,+Map,-NT)
% given the tree-term T and the Map of Name-Subtree entries, replace each occurence of Subtree in T with Name -> result NT 
replace_pt(T,[],T).
replace_pt([],_,[]).
replace_pt(L,M,R) :-
	atomlist(L),
	member(R-L,M),
	!.
replace_pt([L|LL],[M|MM],R) :-
	replace_pt_list([L|LL],[M|MM],R).

replace_pt_list([T|Tree],[M|Map],[C|Compr]) :-
	replace_pt_single(T,[M|Map],C),
	replace_pt_list(Tree,[M|Map],Compr).
replace_pt_list([],_,[]).

replace_pt_single(s(A,T),[M|Map],Res) :-
	atomlist(T),
	member(Res-s(A,T),[M|Map]),
	!.
replace_pt_single(s(A,T),[M|Map],s(A,Res)) :-
	atomlist(T),
	member(Res-T,[M|Map]),
	!.
replace_pt_single(s(A,T),[M|Map],Res) :-
	member(Res-s(A,T),[M|Map]),
	!.
replace_pt_single(s(A,T),[M|Map],s(A,TT)) :-
	replace_pt_list(T,[M|Map],TT).
replace_pt_single(A,_,A) :-
	 not_or_atom(A).

output_compressed_script_only(false) :-
        !,
	format('L1 = FALSE~nL1~n',[]).
output_compressed_script_only(true) :-
	!,
	format('L1 = TRUE~nL1~n',[]).
output_compressed_script_only(T) :-
        once(retract(compression(Short,Long))),
	assertz(compression(Short,Long)),
	(T = Short ->
		format('~w = ',[Short]),
		format_compression_script_only(Long),
		format('~w~n',[Short])
	;
		format('~w = ',[Short]),
		format_compression_script_only(Long),
		output_compressed_script_only(T)
	).

format_compression_script_only(s((V,R,S),B0)) :-
	recorded(map,m(R,S,_Num,C),_),
	atomic_concat([C,'-',V],C1),
	format('~w * ~w~n',[C1,B0]).

format_compression_script_only([A]) :-
        format('~w~n',[A]).
format_compression_script_only([A,B|C]) :-
	format('~w + ',[A]),
	format_compression_script_only([B|C]).


%%%%%%%%%%%%
% output for script
% input argument is compressed tree, i.e. true/false or name assigned in last compression step
%%%%%%%%%%%%
output_compressed_script(false) :-
	!.
	%format('L1 = FALSE~nL1~n',[]).
output_compressed_script(true) :-
	!.
	%format('L1 = TRUE~nL1~n',[]).
% for each name-subtree pair, write corresponding line to script, e.g. L17 = x4 * L16
% stop after writing definition of root (last entry in compression/2), add it's name to mark end of script
output_compressed_script(T) :-
	once(retract(compression(Short,Long))),
	(T = Short -> 
	   % format('~w = ',[Short]), 
	    format_compression_script(Long,Short)
	   % format('~w~n',[Short]) 
	;
	   % format('~w = ',[Short]),
	    format_compression_script(Long,Short),
	    output_compressed_script(T)).

format_compression_script(s((V,R,S),B0),Short) :-!,
	% checkme
		recorded(map,m(R,S,_Num,C),_),
		atomic_concat([C,'-',V],C1),
		recorded(mapnodes,m(C1,Node1),_),
		recorded(mapnodes,m(B0,Node2),_),
	%	format('~w * ~w~n',[C1,B0]),
		and(Node1,Node2,Node),
		recorda(mapnodes,m(Short,Node),_),
		recorded(refc,m(C1,RefC1),Ref1),
		recorded(refc,m(B0,RefC2),Ref2),
		erase(Ref1),
		erase(Ref2),
		RefC11 is RefC1-1,
		RefC21 is RefC2-1,
		(RefC11 =:=0->
			deref(Node1)
		;
			recorda(refc,m(C1,RefC11),_)
		),
		(RefC21 =:=0->
			deref(Node2)
		;
			recorda(refc,m(B0,RefC21),_)
		).

format_compression_script([H1],Short):-!,
	% format('~w~n',[A]),
        recorded(mapnodes,m(H1,Node1),_),
	recorded(refc,m(H1,RefC1),Ref1),
	erase(Ref1),
	RefC11 is RefC1-1,
	(RefC11 =:=0->
		deref(Node1)
	;
		recorda(refc,m(H1,RefC11),_)
	),
	recorda(mapnodes,m(Short,Node1),_).

format_compression_script([H1,H2],Short):-!,
       % format('~w + ~w~n',[H1,H2]),
	recorded(mapnodes,m(H1,Node1),_),
	recorded(refc,m(H1,RefC1),Ref1),
	erase(Ref1),
	RefC11 is RefC1-1,
	recorded(mapnodes,m(H2,Node2),_),
	recorded(refc,m(H2,RefC2),Ref2),
	erase(Ref2),
	RefC21 is RefC2-1,
	or(Node1,Node2,Node),
	(RefC11 =:=0->
		deref(Node1)
	;
		recorda(refc,m(H1,RefC11),_)
	),
	(RefC21 =:=0->
		deref(Node2)
	;
		recorda(refc,m(H2,RefC21),_)
	),
	recorda(mapnodes,m(Short,Node),_).

format_compression_script([H1,H2,H3|T],Short):-
	%format('~w + ~w +',[H1,H2]),
	recorded(mapnodes,m(H1,Node1),_),
	recorded(refc,m(H1,RefC1),Ref1),
	erase(Ref1),
	RefC11 is RefC1-1,
	recorded(mapnodes,m(H2,Node2),_),
	recorded(refc,m(H2,RefC2),Ref2),
	erase(Ref2),
	RefC21 is RefC2-1,
	or(Node1,Node2,Node),
	(RefC11 =:=0->
		deref(Node1)
	;
		recorda(refc,m(H1,RefC11),_)
	),
	(RefC21 =:=0->
		deref(Node2)
	;
		recorda(refc,m(H2,RefC21),_)
	),
	format_compression_script1([H3|T],Node,Short).

format_compression_script1([A],Node1,Short) :-!,
%	format('~w~n',[A]),
	recorded(mapnodes,m(A,Node2),_),
	recorded(refc,m(A,RefC),Ref),
	erase(Ref),
	or(Node1,Node2,Node),
	deref(Node1),
	recorda(mapnodes,m(Short,Node),_),
	RefC1 is RefC-1,
	(RefC1=:=0->
		deref(Node2)
	;
		recorda(refc,m(A,RefC1),_)
	).

format_compression_script1([A,B|C],Node1,Short) :-
	format('~w + ',[A]),
	recorded(mapnodes,m(A,Node2),_),
	recorded(refc,m(A,RefC),Ref),
	erase(Ref),	
	or(Node1,Node2,Node),
	deref(Node1),
	RefC1 is RefC-1,
	(RefC1=:=0->
		deref(Node2)
	;
		recorda(refc,m(A,RefC1),_)
	),
	format_compression_script1([B|C],Node,Short).

%%%%%%%%%%%%%%%%%%%%%%%%
% auxiliaries for translation to BDD
%%%%%%%%%%%%%%%%%%%%%%%%

% prefix the current counter with "L" 
get_next_name(Name) :-
	retract(c_num(N)),
	NN is N+1,
	assert(c_num(NN)),
	atomic_concat('L',N,Name).

get_next_var_id(N,Name) :-
	retract(v_num(N)),
	NN is N+1,
	assert(v_num(NN)),
	atomic_concat('x',N,Name).

% create BDD-var as fact id prefixed by x
% learning.yap relies on this format!
% when changing, also adapt test_var_name/1 below
get_var_name(R,S,Number,NameA) :-
	get_next_var_id(Number,NameA),
	recorda(map,m(R,S,Number,NameA),_).

% test used by base case of compression mapping to detect single-variable tree
% has to match above naming scheme
test_var_name(T) :-
	atomic_concat(x,_,T).
