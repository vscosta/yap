%%% -*- mode: Prolog; -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2009-06-17 22:22:00 +0200 (Mi, 17 Jun 2009) $
%  $Revision: 1550 $
%
%  This file is part of ProbLog
%  http://dtai.cs.kuleuven.be/problog
%
%  Copyright 2009 Katholieke Universiteit Leuven
%
%  Authors: Luc De Raedt, Bernd Gutmann, Angelika Kimmig,
%           Vitor Santos Costa
%
%                                                                      
%  Main authors of this file:
%  Angelika Kimmig, Vitor Santos Costa, Bernd Gutmann
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Artistic License 2.0
% 
% Copyright (c) 2000-2006, The Perl Foundation.
% 
% Everyone is permitted to copy and distribute verbatim copies of this
% license document, but changing it is not allowed.  Preamble
% 
% This license establishes the terms under which a given free software
% Package may be copied, modified, distributed, and/or
% redistributed. The intent is that the Copyright Holder maintains some
% artistic control over the development of that Package while still
% keeping the Package available as open source and free software.
% 
% You are always permitted to make arrangements wholly outside of this
% license directly with the Copyright Holder of a given Package. If the
% terms of this license do not permit the full use that you propose to
% make of the Package, you should contact the Copyright Holder and seek
% a different licensing arrangement.  Definitions
% 
% "Copyright Holder" means the individual(s) or organization(s) named in
% the copyright notice for the entire Package.
% 
% "Contributor" means any party that has contributed code or other
% material to the Package, in accordance with the Copyright Holder's
% procedures.
% 
% "You" and "your" means any person who would like to copy, distribute,
% or modify the Package.
% 
% "Package" means the collection of files distributed by the Copyright
% Holder, and derivatives of that collection and/or of those files. A
% given Package may consist of either the Standard Version, or a
% Modified Version.
% 
% "Distribute" means providing a copy of the Package or making it
% accessible to anyone else, or in the case of a company or
% organization, to others outside of your company or organization.
% 
% "Distributor Fee" means any fee that you charge for Distributing this
% Package or providing support for this Package to another party. It
% does not mean licensing fees.
% 
% "Standard Version" refers to the Package if it has not been modified,
% or has been modified only in ways explicitly requested by the
% Copyright Holder.
% 
% "Modified Version" means the Package, if it has been changed, and such
% changes were not explicitly requested by the Copyright Holder.
% 
% "Original License" means this Artistic License as Distributed with the
% Standard Version of the Package, in its current version or as it may
% be modified by The Perl Foundation in the future.
% 
% "Source" form means the source code, documentation source, and
% configuration files for the Package.
% 
% "Compiled" form means the compiled bytecode, object code, binary, or
% any other form resulting from mechanical transformation or translation
% of the Source form.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Permission for Use and Modification Without Distribution
% 
% (1) You are permitted to use the Standard Version and create and use
% Modified Versions for any purpose without restriction, provided that
% you do not Distribute the Modified Version.
%
% Permissions for Redistribution of the Standard Version
% 
% (2) You may Distribute verbatim copies of the Source form of the
% Standard Version of this Package in any medium without restriction,
% either gratis or for a Distributor Fee, provided that you duplicate
% all of the original copyright notices and associated disclaimers. At
% your discretion, such verbatim copies may or may not include a
% Compiled form of the Package.
% 
% (3) You may apply any bug fixes, portability changes, and other
% modifications made available from the Copyright Holder. The resulting
% Package will still be considered the Standard Version, and as such
% will be subject to the Original License.
%
% Distribution of Modified Versions of the Package as Source
% 
% (4) You may Distribute your Modified Version as Source (either gratis
% or for a Distributor Fee, and with or without a Compiled form of the
% Modified Version) provided that you clearly document how it differs
% from the Standard Version, including, but not limited to, documenting
% any non-standard features, executables, or modules, and provided that
% you do at least ONE of the following:
% 
% (a) make the Modified Version available to the Copyright Holder of the
% Standard Version, under the Original License, so that the Copyright
% Holder may include your modifications in the Standard Version.  (b)
% ensure that installation of your Modified Version does not prevent the
% user installing or running the Standard Version. In addition, the
% modified Version must bear a name that is different from the name of
% the Standard Version.  (c) allow anyone who receives a copy of the
% Modified Version to make the Source form of the Modified Version
% available to others under (i) the Original License or (ii) a license
% that permits the licensee to freely copy, modify and redistribute the
% Modified Version using the same licensing terms that apply to the copy
% that the licensee received, and requires that the Source form of the
% Modified Version, and of any works derived from it, be made freely
% available in that license fees are prohibited but Distributor Fees are
% allowed.
%
% Distribution of Compiled Forms of the Standard Version or
% Modified Versions without the Source
% 
% (5) You may Distribute Compiled forms of the Standard Version without
% the Source, provided that you include complete instructions on how to
% get the Source of the Standard Version. Such instructions must be
% valid at the time of your distribution. If these instructions, at any
% time while you are carrying out such distribution, become invalid, you
% must provide new instructions on demand or cease further
% distribution. If you provide valid instructions or cease distribution
% within thirty days after you become aware that the instructions are
% invalid, then you do not forfeit any of your rights under this
% license.
% 
% (6) You may Distribute a Modified Version in Compiled form without the
% Source, provided that you comply with Section 4 with respect to the
% Source of the Modified Version.
%
% Aggregating or Linking the Package
% 
% (7) You may aggregate the Package (either the Standard Version or
% Modified Version) with other packages and Distribute the resulting
% aggregation provided that you do not charge a licensing fee for the
% Package. Distributor Fees are permitted, and licensing fees for other
% components in the aggregation are permitted. The terms of this license
% apply to the use and Distribution of the Standard or Modified Versions
% as included in the aggregation.
% 
% (8) You are permitted to link Modified and Standard Versions with
% other works, to embed the Package in a larger work of your own, or to
% build stand-alone binary or bytecode versions of applications that
% include the Package, and Distribute the result without restriction,
% provided the result does not expose a direct interface to the Package.
%
% Items That are Not Considered Part of a Modified Version
% 
% (9) Works (including, but not limited to, modules and scripts) that
% merely extend or make use of the Package, do not, by themselves, cause
% the Package to be a Modified Version. In addition, such works are not
% considered parts of the Package itself, and are not subject to the
% terms of this license.
%
% General Provisions
% 
% (10) Any use, modification, and distribution of the Standard or
% Modified Versions is governed by this Artistic License. By using,
% modifying or distributing the Package, you accept this license. Do not
% use, modify, or distribute the Package, if you do not accept this
% license.
% 
% (11) If your Modified Version has been derived from a Modified Version
% made by someone other than you, you are nevertheless required to
% ensure that your Modified Version complies with the requirements of
% this license.
% 
% (12) This license does not grant you the right to use any trademark,
% service mark, tradename, or logo of the Copyright Holder.
% 
% (13) This license includes the non-exclusive, worldwide,
% free-of-charge patent license to make, have made, use, offer to sell,
% sell, import and otherwise transfer the Package with respect to any
% patent claims licensable by the Copyright Holder that are necessarily
% infringed by the Package. If you institute patent litigation
% (including a cross-claim or counterclaim) against any party alleging
% that the Package constitutes direct or contributory patent
% infringement, then this Artistic License to you shall terminate on the
% date that such litigation is filed.
% 
% (14) Disclaimer of Warranty: THE PACKAGE IS PROVIDED BY THE COPYRIGHT
% HOLDER AND CONTRIBUTORS "AS IS' AND WITHOUT ANY EXPRESS OR IMPLIED
% WARRANTIES. THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
% PARTICULAR PURPOSE, OR NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT
% PERMITTED BY YOUR LOCAL LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT
% HOLDER OR CONTRIBUTOR WILL BE LIABLE FOR ANY DIRECT, INDIRECT,
% INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING IN ANY WAY OUT OF THE USE
% OF THE PACKAGE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* modified by Fabrizio Riguzzi in 2009 for dealing with multivalued variables
instead of variables or their negation, the script can contain equations of the 
form variable=value */




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
	setof(X, trie_literal(Trie, X), Edges). %->
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
