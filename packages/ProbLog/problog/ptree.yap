%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-11-28 14:41:26 +0100 (Mon, 28 Nov 2011) $
%  $Revision: 6764 $
%
%  This file is part of ProbLog
%  http://dtai.cs.kuleuven.be/problog
%
%  ProbLog was developed at Katholieke Universiteit Leuven
%
%  Copyright 2008, 2009, 2010
%  Katholieke Universiteit Leuven
%
%  Main authors of this file:
%  Angelika Kimmig, Vitor Santos Costa,Bernd Gutmann,
%  Theofrastos Mantadelis, Guy Van den Broeck
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% prefix-trees for managing a DNF
% remembers shortest prefix of a conjunction only (i.e. a*b+a*b*c results in a*b only, but b*a+a*b*c is not reduced)
% children are sorted, but branches aren't (to speed up search while keeping structure sharing from proof procedure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(ptree, [init_ptree/1,
                  delete_ptree/1,
                  member_ptree/2,
                  enum_member_ptree/2,
                  insert_ptree/2,
                  delete_ptree/2,
                  edges_ptree/2,
                  count_ptree/2,
                  prune_check_ptree/2,
                  empty_ptree/1,
                  merge_ptree/2,
                  merge_ptree/3,
                  bdd_ptree/3,
                  bdd_struct_ptree/3,
                  bdd_ptree_map/4,
                  bdd_struct_ptree_map/4,
                  traverse_ptree/2,            %theo
                  print_ptree/1,               %theo
                  statistics_ptree/0,          %theo
                  print_nested_ptree/1,        %theo
                  trie_to_bdd_trie/5,          %theo
                  trie_to_bdd_struct_trie/5,
                  nested_trie_to_bdd_trie/5,   %theo
                  nested_trie_to_bdd_struct_trie/5,
                  ptree_decomposition/3,
                  ptree_decomposition_struct/3,
                  nested_ptree_to_BDD_script/3, %theo
                  nested_ptree_to_BDD_struct_script/3,
                  ptree_db_trie_opt_performed/3,
                  bdd_vars_script/1
                 ]).

% load library modules
:- use_module(library(tries)).
:- use_module(library(lists), [append/3, member/2, memberchk/2, delete/3]).
:- use_module(library(system), [tmpnam/1]).
:- use_module(library(ordsets), [ord_intersection/3, ord_union/3]).



% load our own modules
:- use_module(flags).
:- use_module(utils).
:- use_module(nestedtries, [nested_trie_to_depth_breadth_trie/4]).

% switch on all tests to reduce bug searching time
:- style_check(all).
:- yap_flag(unknown,error).

% this is a test to determine whether YAP provides the needed trie library
:- initialization(
        (       predicate_property(trie_disable_hash, imported_from(_M)) ->
                trie_disable_hash
        ;       print_message(warning,'The predicate tries:trie_disable_hash/0 does not exist. Please update trie library.')
        )
).

%%%%%%%%%%%%%%%%%%%%%%%
% Define module flags
%%%%%%%%%%%%%%%%%%%%%%%

:- initialization((
	problog_define_flag(use_db_trie,     problog_flag_validate_boolean, 'use the builtin trie 2 trie transformation', false),
	problog_define_flag(db_trie_opt_lvl, problog_flag_validate_integer, 'optimization level for the trie 2 trie transformation', 0),
	problog_define_flag(compare_opt_lvl, problog_flag_validate_boolean, 'comparison mode for optimization level', false),
	problog_define_flag(db_min_prefix,   problog_flag_validate_integer, 'minimum size of prefix for dbtrie to optimize', 2),
	problog_define_flag(use_naive_trie,  problog_flag_validate_boolean, 'use the naive algorithm to generate bdd scripts', false),
	problog_define_flag(use_old_trie,    problog_flag_validate_boolean, 'use the old trie 2 trie transformation no nested', true),
	problog_define_flag(use_dec_trie,    problog_flag_validate_boolean, 'use the decomposition method', false),
  problog_define_flag(deref_terms,     problog_flag_validate_boolean, 'deref BDD terms after last use', false),
  problog_define_flag(export_map_file, problog_flag_validate_boolean, 'activates export of a variable map file', false, output)
)).


%%%%%%%%%%%%%%%%%%%%%%%%
% ptree basics
%%%%%%%%%%%%%%%%%%%%%%%%

init_ptree(Trie) :-
	trie_open(Trie).

delete_ptree(Trie) :-
	trie_close(Trie), !.
delete_ptree(_).

empty_ptree(Trie) :-
	trie_usage(Trie, 0, 0, 0).

traverse_ptree(Trie, List) :-
  trie_traverse(Trie, Ref),
  trie_get_entry(Ref, List).

traverse_ptree_mode(Mode) :-
  trie_traverse_mode(Mode).

%%%%%%%%%%%%%%%%%%%%%%%%
% member
%%%%%%%%%%%%%%%%%%%%%%%%

% non-backtrackable (to check)
member_ptree(List, Trie) :-
	trie_check_entry(Trie, List, _).

% backtrackable (to list)
enum_member_ptree(List, Trie) :-
	trie_path(Trie, List).

trie_path(Trie, List) :-
	trie_traverse(Trie, Ref),
	trie_get_entry(Ref, List).

%%%%%%%%%%%%%%%%%%%%%%%%
% insert conjunction
%%%%%%%%%%%%%%%%%%%%%%%%
insert_ptree(false, _Trie) :-!.
insert_ptree(true, Trie) :-
  !,
  trie_delete(Trie),
  trie_put_entry(Trie, [true], _).
insert_ptree(List, Trie) :-
  (trie_check_entry(Trie, [true], _) -> % prune if there is a prob=1 proof
    true
  ;
    trie_put_entry(Trie, List, _)
	).


%%%%%%%%%%%%%%%%%%%%%%%%
% delete conjunction
%%%%%%%%%%%%%%%%%%%%%%%%
delete_ptree(List, Trie) :-
	trie_check_entry(Trie, List, Ref),
	trie_remove_entry(Ref).


%%%%%%%%
% return list -Edges of all edge labels in ptree
% doesn't use any heuristic to order those for the BDD
% (automatic reordering has to do the job)
%%%%%%%%%
edges_ptree(Trie, []) :-
	empty_ptree(Trie),
	!.
edges_ptree(Trie, []) :-
	trie_check_entry(Trie, [true], _),
	!.
edges_ptree(Trie ,Edges) :-
	setof(X, trie_literal(Trie, X), Edges).

trie_literal(Trie, X) :-
	trie_traverse(Trie,Ref),
	trie_get_entry(Ref, List),
	member(X, List).

%%%%%%%%
% number of conjunctions in the tree
%%%%%%%%%

count_ptree(Trie, N) :-
	trie_usage(Trie, N, _, _).

%%%%%%%%
% check whether some branch of ptree is a subset of conjunction List
% useful for pruning the search for proofs (optional due to time overhead)
% currently not implemented, just fails
%%%%%%%

prune_check_ptree(_List, _Trie) :-
	format(user,'FAIL: prune check currently not supported~n',[]),
	flush_output(user),
	fail.

%%%%%%%%%%%%%
% merge two ptrees
% - take care not to loose proper prefixes that are proofs!
%%%%%%%%%%%%%%%
merge_ptree(T1, _) :-
	trie_check_entry(T1, [true], _), !.
merge_ptree(_, T2) :-
	trie_check_entry(T2, [true], _), !. % is this strange on the loop condition?
merge_ptree(T1, T2) :-
	trie_join(T1, T2).

merge_ptree(T1, _, T3) :-
	trie_check_entry(T1, [true], _),
	!,
	trie_open(T3),
	trie_put_entry(T3, [true], _).
merge_ptree(_, T2, T3) :-
	trie_check_entry(T2, [true], _),
	!,
	trie_open(T3),
	trie_put_entry(T3, [true], _).
merge_ptree(T1, T2, T3) :-
	trie_dup(T1, T3),
	trie_join(T3, T2).

%%%%%%%%%%%%%%%%%%%%%%%%
% Write structural BDD script for given trie to file
% does NOT write a parameter file but unifies a list of used variables
%
% Specialized versions are:
% - bdd_ptree -> bdd_struct_ptree
% - bdd_ptree_map -> bdd_struct_ptree_map
% - nested_ptree_to_BDD_script -> nested_ptree_to_BDD_struct_script
% - trie_to_bdd_trie -> trie_to_bdd_struct_trie
% - nested_trie_to_bdd_trie -> nested_trie_to_bdd_struct_trie
% - ptree_decomposition -> ptree_decomposition_struct
% - bdd_ptree_script -> bdd_struct_ptree_script
%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(c_num/1).

bdd_struct_ptree(Trie, FileBDD, Variables) :-
    bdd_struct_ptree_script(Trie, FileBDD, Variables),
    eraseall(map).

bdd_struct_ptree_map(Trie, FileBDD, Variables, Mapping) :-
    bdd_struct_ptree_script(Trie, FileBDD, Variables),
    findall(X, recorded(map, X, _), Map),
    add_probs(Map, Mapping),
    eraseall(map).

bdd_struct_ptree_script(Trie, FileBDD, Variables) :-
    edges_ptree(Trie, Variables),
    name_vars(Variables), % expected by output_compressed_script/1?
    length(Variables, VarCount),
    assertz(c_num(1)),
    bdd_pt(Trie, CT),
    c_num(NN),
    IntermediateSteps is NN - 1,
    tell(FileBDD),
    format('@BDD1~n~w~n~w~n~w~n', [VarCount, 0, IntermediateSteps]),
    output_compressed_script(CT),
    told,
    retractall(c_num(_)),
    retractall(compression(_, _)).

name_vars([]).
name_vars([A|B]) :-
    (
     A=not(ID)
    ->
     get_var_name(ID,_)
     ;
     get_var_name(A,_)
    ),
    name_vars(B).

nested_ptree_to_BDD_struct_script(Trie, BDDFileName, Variables):-
  tmpnam(TmpFile1),
  open(TmpFile1, 'write', BDDS),

  (
   generate_BDD_from_trie(Trie, Inter, BDDS)
  ->
   (
    next_intermediate_step(TMP),
    InterCNT is TMP - 1,
    format(BDDS,'~q~n',[Inter]),
    close(BDDS),
    (
     get_used_vars(Variables, VarCNT)
    ->
     true;
     VarCNT = 0
    ),
    prefix_bdd_file_with_header(BDDFileName,VarCNT,InterCNT,TmpFile1),
    cleanup_BDD_generation
   );(
    close(BDDS),
    delete_file_silently(TmpFile1),
    cleanup_BDD_generation,
    fail
   )
  ).

trie_to_bdd_struct_trie(A, B, OutputFile, OptimizationLevel, Variables) :-
  trie_to_depth_breadth_trie(A, B, LL, OptimizationLevel),
  (atomic_concat('L', InterStep, LL) -> % what does this mean?
    retractall(deref(_,_)),
    (problog_flag(deref_terms, true) ->
      asserta(deref(LL,no)),
      mark_for_deref(B),
      V = 3
    ;
      V = 1
    ),
    variables_in_dbtrie(B, Variables), %not the most efficient solution
    length(Variables, VarCNT),         %this 2 should be changed
    tell(OutputFile),
    write('@BDD'), write(V), nl,
    write(VarCNT), nl,
    write(0), nl,
    write(InterStep), nl,
    trie_write(B, LL),
    write(LL), nl,
    told
  ;
    (is_state(LL) ->
      Variables = []
    ;
      Variables = [LL]
    ),
    tell(OutputFile),
    write('@BDD1'), nl,
    write(1), nl,
    write(0), nl,
    write(1), nl,
    get_var_name(LL, NLL),
    write('L1 = '),write(NLL),nl,
    write('L1'), nl,
    told
  ).

nested_trie_to_bdd_struct_trie(A, B, OutputFile, OptimizationLevel, Variables):-
  %trie_nested_to_depth_breadth_trie(A, B, LL, OptimizationLevel, problog:problog_chktabled),
  nested_trie_to_depth_breadth_trie(A, B, LL, OptimizationLevel),
  (is_label(LL) ->
    retractall(deref(_,_)),
    (problog_flag(deref_terms, true) ->
      asserta(deref(LL,no)),
      mark_for_deref(B),
      V = 3
    ;
      V = 1
    ),
    variables_in_dbtrie(B, Variables), %not the most efficient solution
    length(Variables, VarCNT),         %this 2 should be changed
    tell(OutputFile),
    write('@BDD'), write(V), nl,
    write(VarCNT), nl,
    write(0), nl,
    (LL = not(NegL)->
      atomic_concat('L', NegStep, NegL),
      number_atom(NegStepN, NegStep),
      InterStep is NegStepN + 1,
      atomic_concat('L', InterStep, FL),
      write(InterStep), nl,
      trie_write(B, FL),
      write(FL), write(' = ~'), write(NegL), nl,
      write(FL), nl
    ;
      atomic_concat('L', InterStep, LL),
      write(InterStep), nl,
      trie_write(B, LL),
      write(LL), nl
    ),
    told
  ;
    (is_state(LL) ->
      Variables = []
    ;
      Variables = [LL]
    ),
    tell(OutputFile),
    write('@BDD1'), nl,
    write(1), nl,
    write(0), nl,
    write(1), nl,
    simplify(LL, FLL),
    (FLL = not(_) ->
      write('L1 = ~')
    ;
      write('L1 = ')
    ),
    get_var_name(FLL, NLL),
    write(NLL),nl,
    write('L1'), nl,
    told
  ).

ptree_decomposition_struct(Trie, BDDFileName, Variables) :-
  tmpnam(TmpFile1),
  nb_setval(next_inter_step, 1),
  variables_in_dbtrie(Trie, Variables),
  length(Variables, VarCnt),
  tell(TmpFile1),
  decompose_trie(Trie, Variables, L),
  (is_label(L)->
    atomic_concat('L', LCnt, L),
    write(L),nl
  ;
    LCnt = 1,
    write('L1 = '),
    (L == false ->
      write('FALSE')
    ;
      write(L)
    ), nl,
    write('L1'), nl
  ),
  told,
  prefix_bdd_file_with_header(BDDFileName,VarCnt,LCnt,TmpFile1).

%%%%%%%%%%%%%%%%%%%%%%%%
% write BDD info for given ptree to file
% - initializes leaf BDDs (=variables) first
% - then compresses ptree to exploit subtree sharing
% - bdd_pt/1 does the work on the structure itself
%%%%%%%%%%%%%%%%%%%%%%%%


bdd_ptree(Trie, FileBDD, FileParam) :-
	bdd_ptree_script(Trie, FileBDD, FileParam),
	eraseall(map).

% version returning variable mapping
bdd_ptree_map(Trie, FileBDD, FileParam, Mapping) :-
	bdd_ptree_script(Trie, FileBDD, FileParam),
	findall(X, recorded(map, X, _), Map),
	add_probs(Map, Mapping),
	eraseall(map).

add_probs([], []).
add_probs([m(A,Name)|Map], [m(A, Name, Prob)|Mapping]) :-
	% FIXME: Does this work with non-ground facts
	problog:get_fact_probability(A, Prob),
	add_probs(Map, Mapping).

% number of variables may be to high:
% counted on trie, but conversion to old tree representation
% transforms A*B+A to A (prefix-test)
bdd_ptree_script(Trie, FileBDD, FileParam) :-
	edges_ptree(Trie, Edges),
	tell(FileParam),
	bdd_vars_script(Edges),

	flush_output,

	told,
	length(Edges, VarCount),
	assertz(c_num(1)),
	bdd_pt(Trie, CT),
	c_num(NN),
	IntermediateSteps is NN - 1,
	tell(FileBDD),
	format('@BDD1~n~w~n~w~n~w~n', [VarCount, 0, IntermediateSteps]),
	output_compressed_script(CT),


	told,
	retractall(c_num(_)),
	retractall(compression(_, _)).

% write parameter file by iterating over all var/not(var) occuring in the tree

bdd_vars_script(Vars):-
  bdd_vars_script(Vars, Names),
  (problog_flag(export_map_file, true) ->
    problog_flag(map_file, MapFile),
    os:convert_filename_to_working_path(MapFile, MapFileName),
    flush_output,
    tell(MapFileName),
    problog:get_fact_list(Vars, Facts),
    writemap(Names, Facts),
    flush_output,
    told
  ;
    true
  ).
writemap([],[]).
writemap([Name|Names],[Fact|Facts]):-
  write(map(Name,Fact)),nl,
  writemap(Names, Facts).

bdd_vars_script([], []).
bdd_vars_script([false|T], Names):-
  bdd_vars_script(T, Names).
bdd_vars_script([true|T], Names):-
  bdd_vars_script(T, Names).
bdd_vars_script([not(A)|B], Names) :-
  !, bdd_vars_script([A|B], Names).
bdd_vars_script([A|B], [NameA|Names]) :-
  bdd_vars_script_intern(A, NameA),
  bdd_vars_script(B, Names).

bdd_vars_script_intern(A, NameA) :-
  (number(A) ->     % it's a ground fact
    get_var_name(A,NameA),
    (problog:decision_fact(A,_) ->   % it's a ground decision
      (problog:problog_control(check,internal_strategy) ->
        problog:get_fact_probability(A,P),
        format('@~w~n~12f~n~w~n',[NameA,P,1])
      ;
		dtproblog:initial_probability(P),
        format('@~w~n~12f~n~w~n',[NameA,P,1])
      )
    ; % it's a normal ProbLog fact
      problog:get_fact_probability(A,P),
      format('@~w~n~12f~n',[NameA,P])
    )
  ; % it's somethin else, call the specialist - it's a non-ground or continuous fact
    bdd_vars_script_intern2(A, NameA)
  ).

bdd_vars_script_intern2(A, NameA) :-
	get_var_name(A,NameA),
	atom_codes(A,A_Codes),

	once(append(Part1,[95|Part2],A_Codes)),	% 95 = '_'
	number_codes(ID,Part1),

	(	     % let's check whether Part2 contains an 'l' (l=low)
         member(108,Part2)
	->
         ( % it does, so it's a continuous fact
	   problog:get_continuous_fact_parameters(ID,gaussian(Mu,Sigma)),
	   format('@~w~n0~n0~n~12f;~12f~n',[NameA,Mu,Sigma])
	 );
         (
	  number_codes(Grounding_ID,Part2),
      (problog:decision_fact(ID,_) ->
          % it's a non-ground decision
          (problog:problog_control(check,internal_strategy) ->
            problog:grounding_is_known(Goal,Grounding_ID),
            problog:dynamic_probability_fact_extract(Goal,P),
            format('@~w~n~12f~n~w~n',[NameA,P,1])
          ;
			dtproblog:initial_probability(P),
            format('@~w~n~12f~n~w~n',[NameA,P,1])
          )
        ;
          (problog:dynamic_probability_fact(ID) ->
              problog:grounding_is_known(Goal,Grounding_ID),
              problog:dynamic_probability_fact_extract(Goal,P)
          ;
              problog:get_fact_probability(ID,P)
          ),
          format('@~w~n~12f~n',[NameA,P])
        )
	 )
	).

%%%%%%%%%%%%%%%%%%%%%%%%
% find top level symbol for script
%%%%%%%%%%%%%%%%%%%%%%%%

% special cases: variable-free formulae
bdd_pt(Trie, false) :-
	empty_ptree(Trie),
	!,
	retractall(c_num(_)),
	assertz(c_num(2)).
bdd_pt(Trie, true) :-
	trie_check_entry(Trie, [true], _),
	!,
	retractall(c_num(_)),
	assertz(c_num(2)).

% general case: transform trie to nested tree structure for compression
bdd_pt(Trie, CT) :-
	trie_to_tree(Trie, Tree),
	once(compress_pt(Tree, CT)).

trie_to_tree(Trie, Tree) :-
	findall(Path, trie_path(Trie, Path), Paths),
	add_trees(Paths, [], Tree).

add_trees([], Tree, Tree).
add_trees([List|Paths], Tree0, Tree) :-
	ins_pt(List, Tree0, TreeI),
	add_trees(Paths, TreeI, Tree).

% default: prune if adding prefix of known proof(s)
ins_pt([], _T, []) :- !.
% alternative: keep extensions of prefix
% ins_pt([],T,T) :- !.
ins_pt([A|B], [s(A1, AT)|OldT], NewT) :-
	compare(Comp, A1, A),
  (Comp == = ->
    (AT == [] ->
      NewT=[s(A1, AT)|OldT]
    ;
      NewT = [s(A1, NewAT)|OldT],
      ins_pt(B, AT, NewAT))
  ;
    Comp == > ->
    NewT = [s(A1, AT)|Tree],
    ins_pt([A|B], OldT, Tree)
	;
    NewT = [s(A, BTree), s(A1, AT)|OldT],
    ins_pt(B, [], BTree)
	).
ins_pt([A|B], [], [s(A, NewAT)]) :-
	ins_pt(B, [], NewAT).

%%%%%%%%%%%%
% BDD compression: alternates and- and or-levels to build BDD bottom-up
% each sub-BDD will be either a conjunction of a one-node BDD with some BDD or a disjunction of BDDs
% uses the internal database to temporarily store a map of components
%%%%%%%%%%%%

% T is completely compressed and contains single variable
% i.e. T of form x12 or ~x34
compress_pt(T, TT) :-
	atom(T),
	test_var_name(T),
	!,
	get_next_name(TT),
	assertz(compression(TT, [T])).
% T is completely compressed and contains subtrees
% i.e. T of form 'L56'
compress_pt(T, T) :-
	atom(T).
% T not yet compressed
% i.e. T is a tree-term (nested list & s/2 structure)
% -> execute one layer of compression, then check again
compress_pt(T, CT) :-
	\+ atom(T),
	and_or_compression(T, IT),
	compress_pt(IT, CT).

% transform tree-term T into tree-term CT where last two layers have been processed
% i.e. introduce names for subparts (-> Map) and replace (all occurrenes of) subparts by this names
and_or_compression(T, CT) :-
	and_comp(T, AT),
	or_comp(AT, CT).

% replace leaves that are single child by variable representing father-AND-child
and_comp(T, AT) :-
	all_leaves_pt(T, Leaves),
	compression_mapping(Leaves, Map),
	replace_pt(T, Map, AT).

% replace list of siblings by variable representing their disjunction
or_comp(T, AT) :-
	all_leaflists_pt(T, Leaves),
	compression_mapping(Leaves, Map),
	replace_pt(T, Map, AT).

all_leaves_pt(T, L) :-
	all(X, some_leaf_pt(T, X), L).

some_leaf_pt([s(A, [])|_], s(A,[])).
some_leaf_pt([s(A, L)|_], s(A, L)) :-
	 not_or_atom(L).
some_leaf_pt([s(_, L)|_], X) :-
	some_leaf_pt(L, X).
some_leaf_pt([_|L],X) :-
	some_leaf_pt(L,X).

all_leaflists_pt(L, [L]) :-
	atomlist(L), !.
all_leaflists_pt(T, L) :-
	all(X,some_leaflist_pt(T, X), L), !.
all_leaflists_pt(_, []).

some_leaflist_pt([s(_, L)|_], L) :-
	atomlist(L).
some_leaflist_pt([s(_, L)|_], X) :-
	some_leaflist_pt(L, X).
some_leaflist_pt([_|L], X) :-
	some_leaflist_pt(L, X).

not_or_atom(T) :-
	(
	    T = not(T0)
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
compression_mapping([], []).
compression_mapping([First|B], [N-First|BB]) :-
	(
	    First = s(A0, [])          % subtree is literal -> use variable's name x17 from map (add ~ for negative case)
	->
	(
	    A0 = not(A)
	->
	    (
      		recorded(map, m(A, Tmp), _), %check
		atomic_concat(['~', Tmp], N)
	    );
    	    recorded(map, m(A0, N), _) %check
	)
	;
	    (First = s(A, L), not_or_atom(L)) % subtree is node with single completely reduced child -> use next 'L'-based name
	    -> (get_next_name(N),
	        assertz(compression(N, s(A, L))))
	;
	    (First = [L], not_or_atom(L)) % subtree is an OR with a single completely reduced element -> use element's name
	     -> N = L
	;
	    (atomlist(First), % subtree is an OR with only (>1) completely reduced elements -> use next 'L'-based name
	    get_next_name(N),
	    assertz(compression(N, First)))
	),
	compression_mapping(B, BB).



% replace_pt(+T,+Map,-NT)
% given the tree-term T and the Map of Name-Subtree entries, replace each occurence of Subtree in T with Name -> result NT
replace_pt(T, [], T).
replace_pt([], _, []).
replace_pt(L, M, R) :-
	atomlist(L),
	member(R-L, M),
	!.
replace_pt([L|LL], [M|MM], R) :-
	replace_pt_list([L|LL], [M|MM], R).

replace_pt_list([T|Tree], [M|Map], [C|Compr]) :-
	replace_pt_single(T, [M|Map], C),
	replace_pt_list(Tree, [M|Map], Compr).
replace_pt_list([], _, []).

replace_pt_single(s(A, T), [M|Map], Res) :-
	atomlist(T),
	member(Res-s(A, T), [M|Map]),
	!.
replace_pt_single(s(A, T), [M|Map], s(A, Res)) :-
	atomlist(T),
	member(Res-T, [M|Map]),
	!.
replace_pt_single(s(A, T), [M|Map], Res) :-
	member(Res-s(A, T), [M|Map]),
	!.
replace_pt_single(s(A, T), [M|Map], s(A, TT)) :-
	!,
	replace_pt_list(T, [M|Map], TT).
replace_pt_single(A, _, A) :-
	 not_or_atom(A).

%%%%%%%%%%%%
% output for script
% input argument is compressed tree, i.e. true/false or name assigned in last compression step
%%%%%%%%%%%%
output_compressed_script(false) :-
	!,
	format('L1 = FALSE~nL1~n', []).
output_compressed_script(true) :-
	!,
	format('L1 = TRUE~nL1~n', []).
% for each name-subtree pair, write corresponding line to script, e.g. L17 = x4 * L16
% stop after writing definition of root (last entry in compression/2), add it's name to mark end of script
output_compressed_script(T) :-
	once(retract(compression(Short, Long))),
	(T = Short ->
	    format('~w = ', [Short]),
	    format_compression_script(Long),
	    format('~w~n', [Short])
	;
	    format('~w = ', [Short]),
	    format_compression_script(Long),
	    output_compressed_script(T)).

format_compression_script(s(A0, B0)) :-
	% checkme
	(
	    A0 = not(A)
	->
	    (
		recorded(map, m(A, C), _),
		format('~~~w * ~w~n', [C, B0])
	    ) ;
	    (
		recorded(map, m(A0, C), _),
		format('~w * ~w~n', [C, B0])
	    )
	).
format_compression_script([A]) :-
	format('~w~n', [A]).
format_compression_script([A, B|C]) :-
	format('~w + ', [A]),
	format_compression_script([B|C]).

%%%%%%%%%%%%%%%%%%%%%%%%
% auxiliaries for translation to BDD
%%%%%%%%%%%%%%%%%%%%%%%%

% prefix the current counter with "L"
get_next_name(Name) :-
	retract(c_num(N)),
	NN is N + 1,
	assertz(c_num(NN)),
	atomic_concat('L', N, Name).

% create BDD-var as fact id prefixed by x
% learning.yap relies on this format!
% when changing, also adapt test_var_name/1 below


simplify_list(List, SList):-
  findall(NEL, (member(El, List), simplify(El, NEL)), SList).

simplify(not(false), true):- !.
simplify(not(true), false):- !.
simplify(not(not(A)), B):-
  !, simplify(A, B).
simplify(A, A).



simplify(not(false), true):- !.
simplify(not(true), false):- !.
simplify(not(not(A)), B):-
  !, simplify(A, B).
simplify(A, A).

get_var_name(true, 'TRUE'):- !.
get_var_name(false, 'FALSE'):- !.
get_var_name(Variable, Name):-
  atomic(Variable), !,
  atomic_concat([x, Variable], Name),
  (recorded(map, m(Variable, Name), _) ->
    true
  ;
    recorda(map, m(Variable, Name), _)
  ).
get_var_name(not(A), NameA):-
  get_var_name(A, NameA).


/*
get_var_name(true, 'TRUE') :-!.
get_var_name(false, 'FALSE') :-!.
get_var_name(not(A), NameA):-
  !, get_var_name(A, NameA).
get_var_name(A, NameA) :-
	atomic_concat([x, A], NameA),
	(
	    recorded(map, m(A, NameA), _)
	->
	    true
	;
	    recorda(map, m(A, NameA), _)
	).*/

% test used by base case of compression mapping to detect single-variable tree
% has to match above naming scheme
test_var_name(T) :-
	atomic_concat(x, _, T).
test_var_name(T) :-
	atomic_concat('~x', _, T).


% Theo debuging additions

print_ptree(Trie):-
  trie_print(Trie).

statistics_ptree:-
  trie_stats(Memory,Tries,Entries,Nodes),
  write('--------------------------------'),nl,
  write('Memory: '),write(Memory),nl,
  write('Tries: '), write(Tries),nl,
  write('Entries: '), write(Entries),nl,
  write('Nodes: '), write(Nodes),nl,
  write('--------------------------------'),nl.


:- dynamic(nested_ptree_printed/1).

print_nested_ptree(Trie):-
  retractall(nested_ptree_printed(_)),
  print_nested_ptree(Trie, 0, '  '),
  retractall(nested_ptree_printed(_)).
print_nested_ptree(Trie, _, _):-
  nested_ptree_printed(Trie), !.
print_nested_ptree(Trie, Level, Space):-
  spacy_print(begin(t(Trie)), Level, Space),
  fail.
print_nested_ptree(Trie, Level, Space):-
  assertz(nested_ptree_printed(Trie)),
  trie_path(Trie, Path),
  NewLevel is Level + 1,
  spacy_print(Path, NewLevel, Space),
  (member(t(Hash), Path); member(not(t(Hash)), Path)),
  problog:problog_chktabled(Hash, SubTrie),
  NewLevel2 is NewLevel + 1,
  print_nested_ptree(SubTrie, NewLevel2, Space),
  fail.
print_nested_ptree(Trie, Level, Space):-
  spacy_print(end(t(Trie)), Level, Space).

spacy_print(Msg, 0, _):-
  write(Msg), nl, !.
spacy_print(Msg, Level, Space):-
  Level > 0,
  write(Space),
  NewLevel is Level - 1,
  spacy_print(Msg, NewLevel, Space).

% Theo Naive method works with Nested Trie to BDD Script

:- dynamic(get_used_vars/2).
:- dynamic(generated_trie/2).
:- dynamic(next_intermediate_step/1).


nested_ptree_to_BDD_script(Trie, BDDFileName, VarFileName):-
  tmpnam(TmpFile1),
  open(TmpFile1, 'write', BDDS),
  (generate_BDD_from_trie(Trie, Inter, BDDS) ->
    next_intermediate_step(TMP), InterCNT is TMP - 1,
    write(BDDS, Inter), nl(BDDS),
    close(BDDS),
    (
     get_used_vars(Vars, VarCNT)
    ->
     true;
     VarCNT = 0
    ),
    prefix_bdd_file_with_header(BDDFileName,VarCNT,InterCNT,TmpFile1),
    open(VarFileName, 'write', VarStream),
    bddvars_to_script(Vars, VarStream),
    close(VarStream),
    cleanup_BDD_generation
  ;
    close(BDDS),
    delete_file_silently(TmpFile1),
    cleanup_BDD_generation,
    fail
  ).

cleanup_BDD_generation:-
  retractall(get_used_vars(_, _)),
  retractall(generated_trie(_, _)),
  retractall(next_intermediate_step(_)).

generate_BDD_from_trie(Trie, TrieInter, Stream):-
  empty_ptree(Trie), !,
  get_next_intermediate_step(TrieInter),
  write(Stream, TrieInter), write(Stream, ' = FALSE'), nl(Stream), !.
generate_BDD_from_trie(Trie, TrieInter, _Stream):-
  clause(generated_trie(STrie, TrieInter), true),
  STrie = Trie, !.
generate_BDD_from_trie(Trie, TrieInter, Stream):-
  findall(LineInter, (
    trie_path(Trie, L),
    generate_line(L, LineTerms, LineInter, Stream),
    write_bdd_line(LineTerms, LineInter, '*', Stream)
  ), OrLineTerms),
  (OrLineTerms = [Inter|[]] ->
    TrieInter = Inter
  ;
    get_next_intermediate_step(TrieInter),
    write_bdd_line(OrLineTerms, TrieInter, '+', Stream)
  ),
  assertz(generated_trie(Trie, TrieInter)).

write_bdd_line([], _LineInter, _Operator, _Stream):-!.
write_bdd_line(LineTerms, LineInter, Operator, Stream):-
  write(Stream, LineInter), write(Stream, '='),
  write_bdd_lineterm(LineTerms, Operator, Stream).

write_bdd_lineterm([LineTerm|[]], _Operator, Stream):-
  write(Stream, LineTerm), nl(Stream), !.
write_bdd_lineterm([LineTerm|LineTerms], Operator, Stream):-
  write(Stream, LineTerm), write(Stream, Operator),
  write_bdd_lineterm(LineTerms, Operator, Stream).

generate_line([], [], Inter, _Stream):-
  !, get_next_intermediate_step(Inter).
generate_line([not(t(Hash))|L], [TrieInter|T] , Inter, Stream):-
  !, problog:problog_chktabled(Hash, Trie),
  generate_BDD_from_trie(Trie, TrieInterTmp, Stream),
  atomic_concat(['~', TrieInterTmp], TrieInter),
  generate_line(L, T, Inter, Stream).
generate_line([t(Hash)|L], [TrieInter|T] , Inter, Stream):-
  !, problog:problog_chktabled(Hash, Trie),
  generate_BDD_from_trie(Trie, TrieInter, Stream),
  generate_line(L, T, Inter, Stream).
generate_line([V|L], [BDDV|T], Inter, Stream):-
  make_bdd_var(V, BDDV),
  generate_line(L, T, Inter, Stream).

%
% Currently it is dublicate with bdd_vars_script predicate
% finally should be merged
%

bddvars_to_script([], _Stream):-!.
bddvars_to_script([H|T], Stream):-
  (number(H) ->
    CurVar = H
  ;
    atom_codes(H, H_Codes),
    % 95 = '_'
    append(Part1, [95|Part2], H_Codes),
    number_codes(CurVar, Part1),
    number_codes(Grounding_ID, Part2)
  ),
  (problog:dynamic_probability_fact(CurVar) ->
    problog:grounding_is_known(Goal, Grounding_ID),
    problog:dynamic_probability_fact_extract(Goal, P)
  ;
    problog:get_fact_probability(CurVar, P)
  ),
  get_var_name(H, VarName),
  format(Stream, '@~w~n~12f~n', [VarName, P]),
  bddvars_to_script(T, Stream).

get_next_intermediate_step('L1'):-
  \+ clause(next_intermediate_step(_), _), !,
  assertz(next_intermediate_step(2)).
get_next_intermediate_step(Inter):-
  next_intermediate_step(InterStep),
  retract(next_intermediate_step(InterStep)),
  NextInterStep is InterStep + 1,
  assertz(next_intermediate_step(NextInterStep)),
  atomic_concat(['L', InterStep], Inter).

make_bdd_var('true', 'TRUE'):-!.
make_bdd_var('false', 'FALSE'):-!.
/*make_bdd_var(neg(V), NotVName):-
  !, make_bdd_var(not(V), NotVName).*/
make_bdd_var(not(V), NotVName):-
  !,
  get_var_name(V, VName),
  atomic_concat(['~', VName], NotVName),
  add_to_vars(V).
make_bdd_var(V, VName):-
  get_var_name(V, VName),
  add_to_vars(V).


add_to_vars(V):-
	clause(get_used_vars(Vars, _Cnt), true),
	memberchk(V, Vars),!.
add_to_vars(V):-
	clause(get_used_vars(Vars, Cnt), true), !,
	retract(get_used_vars(Vars, Cnt)),
	NewCnt is Cnt + 1,
	assertz(get_used_vars([V|Vars], NewCnt)).
add_to_vars(V):-
	assertz(get_used_vars([V], 1)).


%%%%%%%%%%%%%%% depth breadth builtin support %%%%%%%%%%%%%%%%%
%%%
%%% Pending:
%%%          1) Replace term in trie, written in C level
%%%         *2) Support for false, true and 1 var
%%%          3) Decide if it is necessary to propagete loop from child
%%%          4) Possible memory leak with [true] (go(0))
%%%         *5) Handle correctly the trie_replace when not(false), not(true)
%%%          6) Compare sort with a good insert sort
%%%          7) Have a look to the write to file predicates
%%%


variables_in_dbtrie(Trie, []):-
  empty_ptree(Trie), !.
variables_in_dbtrie(Trie, []):-
  trie_check_entry(Trie, [true], _R), !.
variables_in_dbtrie(Trie, L):-
  all(V, variable_in_dbtrie(Trie,V), L).

variable_in_dbtrie(Trie, V):-
  trie_traverse(Trie, R),
  trie_get_entry(R, L),
  get_next_variable(NV, L),
  get_variable(NV, V).

get_next_variable(V, depth(L, _S)):-
  member(V, L),
  \+ is_label(V).
get_next_variable(V, breadth(L, _S)):-
  member(V, L),
  \+ is_label(V).
get_next_variable(V, L):-
  member(V, L),
  \+ is_label(V),
  \+ isnestedtrie(V).

get_variable(not(V), R):-
  !, get_variable(V, R).
get_variable(R, R).


  %trie_get_depth_breadth_reduction_opt_level_count(1, CNT1),
  %trie_get_depth_breadth_reduction_opt_level_count(2, CNT2),
  %trie_get_depth_breadth_reduction_opt_level_count(3, CNT3),
  %writeln([CNT1, CNT2, CNT3]),
  %trie_print(B),

trie_to_bdd_trie(A, B, OutputFile, OptimizationLevel, FileParam):-
  trie_to_depth_breadth_trie(A, B, LL, OptimizationLevel),
  (is_label(LL) ->
    atomic_concat('L', InterStep, LL),
    retractall(deref(_,_)),
    (problog_flag(deref_terms, true) ->
      asserta(deref(LL,no)),
      mark_for_deref(B),
      V = 3
    ;
      V = 1
    ),
    variables_in_dbtrie(B, Edges), %not the most efficient solution
    length(Edges, VarCNT),         %this 2 should be changed
    tell(FileParam),
    bdd_vars_script(Edges),
    told,
    tell(OutputFile),
    write('@BDD'), write(V), nl,
    write(VarCNT), nl,
    write(0), nl,
    write(InterStep), nl,
    trie_write(B, LL),
    write(LL), nl,
    told
  ;
    (is_state(LL) ->
      Edges = []
    ;
      Edges = [LL]
    ),
    tell(FileParam),
    bdd_vars_script(Edges),
    told,
    tell(OutputFile),
    write('@BDD1'), nl,
    write(1), nl,
    write(0), nl,
    write(1), nl,
    (LL = not(ID) ->
      get_var_name(ID, NLL),
      write('L1 = ~'), write(NLL),nl
    ;
      get_var_name(LL, NLL),
      write('L1 = '), write(NLL),nl
    ),
    write('L1'), nl,
    told
  ).

is_state(true).
is_state(false).

nested_trie_to_bdd_trie(A, B, OutputFile, OptimizationLevel, FileParam):-
%   trie_nested_to_depth_breadth_trie(A, B, LL, OptimizationLevel, problog:problog_chktabled),
  nested_trie_to_depth_breadth_trie(A, B, LL, OptimizationLevel),
  simplify(LL, FLL),
  (is_label(FLL) ->
    retractall(deref(_,_)),
    (problog_flag(deref_terms, true) ->
      asserta(deref(FLL,no)),
      mark_for_deref(B),
      V = 3
    ;
      V = 1
    ),
    variables_in_dbtrie(B, Edges), %not the most efficient solution
    length(Edges, VarCNT),         %this 2 should be changed
    tell(FileParam),
    bdd_vars_script(Edges),
    told,

    tell(OutputFile),
    write('@BDD'), write(V), nl,
    write(VarCNT), nl,
    write(0), nl,
    (FLL = not(NegL)->
      atomic_concat('L', NegStep, NegL),
      number_atom(NegStepN, NegStep),
      InterStep is NegStepN + 1,
      atomic_concat('L', InterStep, FL),
      write(InterStep), nl,
      trie_write(B, FL),
      write(FL), write(' = ~'), write(NegL), nl,
      write(FL), nl
    ;
      atomic_concat('L', InterStep, FLL),
      write(InterStep), nl,
      trie_write(B, FLL),
      write(FLL), nl
    ),
    told
  ;
    (is_state(FLL) ->
      Edges = []
    ;
      Edges = [FLL]
    ),
    tell(FileParam),
    simplify_list(Edges, SEdges),
    bdd_vars_script(SEdges),
    told,
    tell(OutputFile),
    write('@BDD1'), nl,
    write(1), nl,
    write(0), nl,
    write(1), nl,
    (FLL = not(_) ->
      write('L1 = ~')
    ;
      write('L1 = ')
    ),
    get_var_name(FLL, NLL),
    write(NLL),nl,
    write('L1'), nl,
    told
  ).


/*
  variables_in_dbtrie(B, Edges), %not the most efficient solution

  length(Edges, VarCNT),         %this 2 should be changed
  tell(FileParam),
  bdd_vars_script(Edges),
  told,
  (atomic_concat('L', InterStep, LL) ->
    tell(OutputFile),
    write('@BDD1'), nl,
    write(VarCNT), nl,
    write(0), nl,
    write(InterStep), nl,
    trie_write(B, LL),
    write(LL), nl,
    told
  ;
    tell(OutputFile),
    write('@BDD1'), nl,
    write(1), nl,
    write(0), nl,
    write(1), nl,
    fix(LL, NLL),
    write('L1 = '),write(NLL),nl,
    write('L1'), nl,
    told
  ).

fix(false, 'FALSE'):-!.
fix(true, 'TRUE'):-!.
fix(A, A).
*/

preprocess(Index, DepthBreadthTrie, OptimizationLevel, StartCount, FinalEndCount):-
  problog:problog_chktabled(Index, Trie), !,
  trie_dup(Trie, CopyTrie),
  initialise_ancestors(Ancestors),
  make_nested_trie_base_cases(CopyTrie, t(Index), DepthBreadthTrie, OptimizationLevel, StartCount, EndCount, Ancestors),
  trie_close(CopyTrie),
  Next is Index + 1,
  preprocess(Next, DepthBreadthTrie, OptimizationLevel, EndCount, FinalEndCount).
preprocess(_, _, _, FinalEndCount, FinalEndCount).

make_nested_trie_base_cases(Trie, t(ID), DepthBreadthTrie, OptimizationLevel, StartCount, FinalEndCount, Ancestors):-
  trie_to_depth_breadth_trie(Trie, DepthBreadthTrie, Label, OptimizationLevel, StartCount, EndCount),
  (Label \= t(_) ->
    FinalEndCount = EndCount,
    problog:problog_chktabled(ID, RTrie),!,
    get_set_trie_from_id(t(ID), Label, RTrie, Ancestors, _, Ancestors)
  ;
    trie_get_depth_breadth_reduction_entry(NestedEntry),
    trie_replace_entry(Trie, NestedEntry, Label, false),
    add_to_ancestors(Label, Ancestors, NewAncestors),
    make_nested_trie_base_cases(Trie, t(ID), DepthBreadthTrie, OptimizationLevel, EndCount, FinalEndCount, NewAncestors)
  ).

trie_nested_to_depth_breadth_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, Module:GetTriePredicate):-
  integer(OptimizationLevel),
  trie_open(DepthBreadthTrie),
  (problog_flag(trie_preprocess, true) ->
    preprocess(1, DepthBreadthTrie, OptimizationLevel, 0, StartCount)
  ;
    StartCount = 0
  ),
  initialise_ancestors(Ancestors),
  initialise_ancestors(Childs),
  trie_nested_to_db_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, StartCount, _, Module:GetTriePredicate, Ancestors, _, _, Childs),
  eraseall(problog_trie_table).

trie_nested_to_db_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, StartCount, FinalEndCount, Module:GetTriePredicate, AncestorList, ContainsLoop, Childs, ChildsAcc):-
  trie_dup(Trie, CopyTrie),
  nested_trie_to_db_trie(CopyTrie, DepthBreadthTrie, FinalLabel, OptimizationLevel, StartCount, FinalEndCount, Module:GetTriePredicate, AncestorList, ContainsLoop, Childs, ChildsAcc),
  trie_close(CopyTrie).

nested_trie_to_db_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, StartCount, FinalEndCount, Module:GetTriePredicate, Ancestors, ContainsLoop, Childs, ChildsAcc):-
  trie_to_depth_breadth_trie(Trie, DepthBreadthTrie, Label, OptimizationLevel, StartCount, EndCount),
  (Label \= t(_) ->
    (var(ContainsLoop) ->
      ContainsLoop = false
    ;
      true
    ),
    FinalLabel = Label,
    FinalEndCount = EndCount,
    Childs = ChildsAcc
  ;
    trie_get_depth_breadth_reduction_entry(NestedEntry),
    trie_get_entry(NestedEntry, Proof),
    (loopcheck(Proof, Ancestors) -> % to fix
      ContainsLoop = true,
      NewLabel = false,
      NewEndCount = EndCount
    ;
%       writeln(in(Label)),
      get_set_trie_from_id(Label, NewLabel, NestedTrie, Ancestors, Module:GetTriePredicate, ChildChilds),
%       writeln(out(NewLabel)),
      (nonvar(NewLabel) ->
        NewEndCount = EndCount
      ;
        add_to_ancestors(Label, Ancestors, CurAncestors),
        initialise_ancestors(ChildChildsAcc),
        trie_nested_to_db_trie(NestedTrie, DepthBreadthTrie, NewLabel, OptimizationLevel, EndCount, NewEndCount, Module:GetTriePredicate, CurAncestors, CheckLoop, ChildChilds, ChildChildsAcc),
        (CheckLoop ->
          StoreAncestors = CurAncestors
        ;
          initialise_ancestors(StoreAncestors)
        ),
        get_set_trie_from_id(Label, NewLabel, NestedTrie, StoreAncestors, Module:GetTriePredicate, ChildChilds)
      )
    ),
    trie_replace_entry(Trie, NestedEntry, Label, NewLabel),
    (problog_flag(refine_anclst, true) ->
      combine_ancestors(ChildsAcc, ChildChilds, AllChilds),
      add_to_ancestors(Label, AllChilds, FAllChilds)
    ;
      initialise_ancestors(FAllChilds)
    ),
    nested_trie_to_db_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel, NewEndCount, FinalEndCount, Module:GetTriePredicate, Ancestors, ContainsLoop, Childs, FAllChilds)
  ).

initialise_ancestors(Ancestors):-
  (problog_flag(anclst_represent, list) ->
    Ancestors = []
  ;
    Ancestors = 0
  ).

add_to_ancestors(t(ID), Ancestors, NewAncestors):-
  (problog_flag(anclst_represent, list) ->
    ord_union(Ancestors, [t(ID)], NewAncestors)
  ;
    NewAncestors is Ancestors \/ (1 << (ID - 1))
  ).

combine_ancestors(Ancestors, AddAncestors, Ancestors):-
  var(AddAncestors), !.
combine_ancestors(Ancestors, AddAncestors, AllAncestors):-
  (problog_flag(anclst_represent, list) ->
    ord_union(Ancestors, AddAncestors, AllAncestors)
  ;
    AllAncestors is Ancestors \/ AddAncestors
  ).


my_trie_print(T):-
  trie_traverse(T, R),
  trie_get_entry(R, E),
  format('~w~n', [E]),
  fail.
my_trie_print(_T).

loopcheck(Proof, AncestorList):-
  contains_nested_trie(Proof, ID),
%   memberchk(t(ID), AncestorList).
%   writeln(chk_id(ID, AncestorList)),
  chk_id(ID, AncestorList), !.
chk_id(ID, AncestorList):-
  (problog_flag(anclst_represent, list) ->
    memberchk(t(ID), AncestorList)
  ;
    (AncestorList /\ (1 << (ID - 1))) > 0
  ).
chk_id(ID, AncestorList):-
  get_negated_synonym_id(ID, NegID),
%   writeln(get_negated_synonym_id(ID, NegID)),
  (problog_flag(anclst_represent, list) ->
    memberchk(t(NegID), AncestorList)
  ;
    (AncestorList /\ (1 << (NegID - 1))) > 0
  ).
% % can also check for a proof with A, not(A)
%
% get_negated_synonym_id(ID, NegID):-
%   tabling:problog_tabling_get_negated_from_id(ID, Ref),
%   recorded(problog_table, store(_, NegID, _, _, _), Ref).

get_negated_synonym_id(ID, NegID):-
  tabling:has_synonyms,
  recorded(problog_table, store(Pred, ID, _, _, _), _),
  Pred =.. [Name0|Args],
  atomic_concat(problog_, Name1, Name0),
  atomic_concat(Name, '_original', Name1),
  (recorded(problog_table_synonyms, negated(Name, NotName1), _);
   recorded(problog_table_synonyms, negated(NotName1, Name), _)),
  atomic_concat([problog_, NotName1, '_original'], NotName),
  NegPred =.. [NotName|Args],
  recorded(problog_table, store(NegPred, NegID, _, _, _), _).


is_nested_trie(T):-
  nonvar(T),
  is_nested_trie(T, _).
is_nested_trie(NT, ID):-
  nonvar(NT),
  NT = not(T), !,
  is_nested_trie(T, ID).
is_nested_trie(t(ID), ID).

contains_nested_trie(L, ID):-
  member(T, L),
  is_nested_trie(T, ID).


subset([],_):-!.
subset(_,[]):-!,fail.
subset([H|T1], [H|T2]):-
  subset(T1, T2).
subset([H1|T1], [H2|T2]):-
  compare(>, H1, H2),
  subset([H1|T1],T2).

get_set_trie_from_id(t(ID), L, T, AncestorList, _GetTriePredicate, Childs):-
  nonvar(ID),
  atomic(L),
  nonvar(AncestorList),
  nonvar(T), !,
  (problog_flag(refine_anclst, true) ->
    (problog_flag(anclst_represent, list) ->
      ord_intersection(AncestorList, Childs, RefinedAncestorList)
    ;
      RefinedAncestorList is AncestorList /\ Childs
    )
  ;
    RefinedAncestorList = AncestorList
  ),
  recordz(problog_trie_table, get_step_from_id(ID, L, T, RefinedAncestorList, Childs), _).
get_set_trie_from_id(t(ID), L, T, SuperSetAncestorList, _GetTriePredicate, Childs):-
%   (clause(theo,_) ->writeln(get_set_trie_from_id(t(ID), L, T, SuperSetAncestorList, _GetTriePredicate, Childs));true),
  recorded(problog_trie_table, get_step_from_id(ID, L, T, AncestorList, StoredChilds), _),
  (problog_flag(refine_anclst, true) ->
    StoredChilds = Childs
  ;
    true
  ),
  (problog_flag(subset_check, true) ->
    (problog_flag(anclst_represent, list) ->
      subset(AncestorList, SuperSetAncestorList)
    ;
      AncestorList is AncestorList /\ SuperSetAncestorList
%       writeln(hi)
    )
  ;
    AncestorList = SuperSetAncestorList
  ), !.
get_set_trie_from_id(t(ID), _L, T, _SuperSetAncestorList, _GetTriePredicate, _):-
  recorded(problog_trie_table, get_step_from_id(ID, _, T, _AncestorList, _Childs), _), !.
get_set_trie_from_id(t(ID), _L, T, _AncestorList, Module:GetTriePredicate, _):-
  Goal =.. [GetTriePredicate, ID, T],
  call(Module:Goal).

trie_replace_entry(_Trie, Entry, _E, false):-
  !, trie_remove_entry(Entry).
trie_replace_entry(Trie, Entry, E, true):-
  !, trie_get_entry(Entry, Proof),
  delete(Proof, E, NewProof),
  (NewProof = [] ->
    trie_delete(Trie),
    trie_put_entry(Trie, [true], _)
  ;
    trie_remove_entry(Entry),
    trie_put_entry(Trie, NewProof, _)
  ).
/*trie_replace_entry(Trie, Entry, E, R):-
  trie_get_entry(Entry, List),
  replace_in_list(List, NewProof, E, R),
  trie_remove_entry(Entry),
  trie_put_entry(Trie, NewProof, _).*/
/*trie_replace_entry(Trie, _Entry, E, R):-
  trie_replace_term2(Trie, E, R).*/
trie_replace_entry(Trie, _Entry, t(ID), R):-
  trie_replace_nested_trie(Trie, ID, R).




trie_replace_term2(Trie, OldTerm, NewTerm):-
  trie_dup(Trie, A),
  %writeln(trie),
  %my_trie_print(A),
  trie_delete(Trie),
  trie_replace_term(A, Trie, OldTerm, NewTerm),
  trie_close(A).

trie_delete(Trie):-
  trie_traverse(Trie, R),
  trie_remove_entry(R),
  fail.
trie_delete(_Trie).

trie_replace_term(Trie, NewTrie, OldTerm, NewTerm):-
  trie_traverse(Trie, R),
  trie_get_entry(R, L),
  replace_in_list(L, NL, OldTerm, NewTerm),
  trie_put_entry(NewTrie, NL, _),
  fail.
trie_replace_term(_Trie, _NewTrie, _OldTerm, _NewTerm).

replace_in_list([],[],_,_):-!.
replace_in_list([H|T], [N|NT], H, N):-
  !, replace_in_list(T, NT, H, N).
replace_in_list([H|T], [NH|NT], R, N):-
  functor(H, _, 1), !,
  replace_in_functor(H, NH, R, N),
  replace_in_list(T, NT, R, N).
replace_in_list([H|T], [H|NT], R, N):-
  replace_in_list(T, NT, R, N).
replace_in_functor(F, NF, T, R):-
  F =.. L,
  replace_in_list(L, NL, T, R),
  NF =.. NL.



trie_write(T, MAXL):-
  atomic_concat('L', MAXLA, MAXL),
  atom_number(MAXLA, MAXLN),
  trie_traverse(T, R),
  trie_get_entry(R, L),
  %write(user_output, L),nl(user_output),
  (dnfbddformat(L, MAXLN) ->
    true
  ;
    write(user_error, warning(L, not_processed)), nl(user_error)
  ),
  fail.
trie_write(_, _).

dnfbddformat(depth(T, L), MAXL):-
  atomic_concat('L', LA, L),
  atom_number(LA, LN),
  MAXL >= LN,
  seperate(T, Li, V),
  %sort(Li, SL),
  %reverse(SL, RSL),
  append(Li, V, R),
  bddlineformat(R, L, ' * '),
  forall(deref(I, L), (
    atomic_concat('L', D, I),
    write('D'), write(D), nl
    )).
dnfbddformat(breadth(T, L), MAXL):-
  atomic_concat('L', LA, L),
  atom_number(LA, LN),
  MAXL >= LN,
  seperate(T, Li, V),
  %sort(Li, SL),
  %reverse(SL, RSL),
  append(V, Li, R),
  bddlineformat(R, L, ' + '),
  forall(deref(I, L), (
    atomic_concat('L', D, I),
    write('D'), write(D), nl
    )).


bddlineformat([not(H)|T], O):-
  write('~'), !,
  bddlineformat([H|T], O).
bddlineformat([H], _O):-
  (is_label(H) ->
    Var = H
  ;
    get_var_name(H, Var)
  ),
  write(Var), nl, !.
bddlineformat([H|T], O):-
  (is_label(H) ->
    Var = H
  ;
    get_var_name(H, Var)
  ),
  write(Var), write(O),
  bddlineformat(T, O).

/*
bddlineformat([not(H)], O):-
  !, write('~'),
  bddlineformat([H], O).
bddlineformat([H], _O):-!,
  (is_label(H) ->
    VarName = H
  ;
    get_var_name(H, VarName)
  ),
  write(VarName), nl.
bddlineformat([not(H)|T], O):-
  !, write('~'),
  bddlineformat([H|T], O).
bddlineformat([H|T], O):-
  (is_label(H) ->
    VarName = H
  ;
    get_var_name(H, VarName)
  ),
  write(VarName), write(O),
  bddlineformat(T, O).*/

bddlineformat(T, L, O):-
  (is_label(L) ->
    write(L), write(' = '),
    bddlineformat(T, O)
  ;
    write(user_output,bdd_script_error([L,T,O])),nl(user_output)
  ).

is_label(not(L)):-
  !, is_label(L).
is_label(Label):-
  atom(Label),
  atomic_concat('L', _, Label).

isnestedtrie(not(T)):-
  !, isnestedtrie(T).
isnestedtrie(t(_T)).

seperate([], [], []).
seperate([H|T], [H|Labels], Vars):-
  is_label(H), !,
  seperate(T, Labels, Vars).
seperate([H|T], Labels, [H|Vars]):-
  seperate(T, Labels, Vars).


ptree_decomposition(Trie, BDDFileName, VarFileName) :-
  tmpnam(TmpFile1),
  nb_setval(next_inter_step, 1),
  variables_in_dbtrie(Trie, T),

  length(T, VarCnt),
  tell(VarFileName),
  bdd_vars_script(T),
  told,
  tell(TmpFile1),
  decompose_trie(Trie, T, L),
  (is_label(L)->
    atomic_concat('L', LCnt, L),
    write(L),nl
  ;
    LCnt = 1,
    write('L1 = '),
    (L == false ->
      write('FALSE')
    ;
      write(L)
    ), nl,
    write('L1'), nl
  ),
  told,
  prefix_bdd_file_with_header(BDDFileName,VarCnt,LCnt,TmpFile1).

get_next_inter_step(I):-
  nb_getval(next_inter_step, I),
  NI is I + 1,
  nb_setval(next_inter_step, NI).

decompose_trie(Trie, _, false):-
  empty_ptree(Trie), !.

decompose_trie(Trie, _, 'TRUE'):-
  trie_check_entry(Trie, [true], _R),!.

decompose_trie(Trie, [H|[]], Var):-
  trie_usage(Trie, 1, _, _),
  get_var_name(H, VarA),
  trie_check_entry(Trie, [L], _R),
  (not(H) == L ->
    Var = not(VarA)
  ,
    Var = VarA
  ),
  !.

decompose_trie(Trie, [H|_T], L3):-
  trie_open(TrieWith),
  trie_open(TrieWithNeg),
  trie_open(TrieWithOut),
  trie_seperate(Trie, H, TrieWith, TrieWithNeg, TrieWithOut),
  /*trie_print(Trie),
  dwriteln('-----------'),
  trie_print(TrieWith),
  dwriteln('-----------'),
  trie_print(TrieWithNeg),
  dwriteln('-----------'),
  trie_print(TrieWithOut),
  dwriteln('-----------'),*/

  variables_in_dbtrie(TrieWith, T1),
  variables_in_dbtrie(TrieWithNeg, T2),
  variables_in_dbtrie(TrieWithOut, T3),

  %dwriteln([T1, not(T2), T3]),

  decompose_trie(TrieWith, T1, LWith),
  trie_close(TrieWith),

  decompose_trie(TrieWithNeg, T2, LWithNeg),
  trie_close(TrieWithNeg),

  decompose_trie(TrieWithOut, T3, LWithOut),
  trie_close(TrieWithOut),

  get_var_name(H, Var),
  %dwriteln([Var, ' * ', LWith, ' + ~', Var, ' * ', LWithNeg, ' + ', LWithOut]),
  (LWith == false ->
    L1 = false
  ;
    (Var == 'TRUE' ->
      L1 = LWith
    ;
      (LWith == 'TRUE' ->
        L1 = Var
      ;
        get_next_inter_step(I),
        atomic_concat(['L', I], L1),
        atomic_concat([L1, ' = ', Var, '*', LWith], W1),
        write(W1), nl
      )
    )
  ),
  (LWithNeg == false ->
    L2 = false
  ;
    (Var == 'TRUE' ->
      L2 = false
    ;
      (LWithNeg == 'TRUE' ->
        atomic_concat(['~', Var], L2)
      ;
        get_next_inter_step(I2),
        atomic_concat(['L', I2], L2),
        atomic_concat([L2, ' = ~', Var, '*', LWithNeg], W2),
        write(W2), nl
      )
    )
  ),
  (one_true(L1, L2, LWithOut) ->
    L3 = 'TRUE'
  ;
    (all_false(L1, L2, LWithOut)->
      L3 = false
    ;
      (one_non_false(L1, L2, LWithOut, L3) ->
        true
      ;
        get_next_inter_step(I3),
        atomic_concat(['L', I3], L3),
        write(L3), write(' = '),
        non_false([L1,L2,LWithOut], [First|Rest]),
        write(First),
        forall(member(NonFalse, Rest), (write('+'), write(NonFalse))),
        nl
      )
    )
  ).

dwriteln(A):-
  write(user_error, A),nl(user_error),flush_output.


non_false([], []):-!.
non_false([H|T], [H|NT]):-
  H \== false,
  non_false(T, NT).
non_false([H|T], NT):-
  H == false,
  non_false(T, NT).

one_true('TRUE', _, _):-!.
one_true(_, 'TRUE', _):-!.
one_true(_, _, 'TRUE'):-!.

all_false(false,false,false).
one_non_false(L, false, false, L):-
  L \== false, !.
one_non_false(false, L, false, L):-
  L \== false, !.
one_non_false(false, false, L, L):-
  L \== false, !.

trie_seperate(Trie, Var, TrieWith, TrieWithNeg, TrieWithOut):-
  trie_traverse(Trie, R),
  trie_get_entry(R, Proof),
  (memberchk(Var, Proof) ->
    remove_from_list(Var, Proof, NProof),
    (NProof == [] ->
      trie_put_entry(TrieWith, [true], _)
    ;
      trie_put_entry(TrieWith, NProof, _)
    )
  ;
    (memberchk(not(Var), Proof) ->
      remove_from_list(not(Var), Proof, NProof),
      (NProof == [] ->
        trie_put_entry(TrieWithNeg, [true], _)
      ;
        trie_put_entry(TrieWithNeg, NProof, _)
      )
    ;
      trie_put_entry(TrieWithOut, Proof, _)
    )
  ),
  fail.
trie_seperate(_Trie, _Var, _TrieWith, _TrieWithNeg, _TrieWithOut).

remove_from_list(_E, [], []):-!.
remove_from_list(E, [E|T], NT):-
  !, remove_from_list(E, T, NT).
remove_from_list(E, [A|T], [A|NT]):-
  remove_from_list(E, T, NT).

ptree_db_trie_opt_performed(LVL1, LVL2, LVL3):-
  trie_get_depth_breadth_reduction_opt_level_count(1, LVL1),
  trie_get_depth_breadth_reduction_opt_level_count(2, LVL2),
  trie_get_depth_breadth_reduction_opt_level_count(3, LVL3).

:- dynamic(deref/2).

mark_for_deref(DB_Trie):-
  traverse_ptree_mode(OLD),
  traverse_ptree_mode(backward),
  mark_deref(DB_Trie),
  traverse_ptree_mode(OLD).

mark_deref(DB_Trie):-
  traverse_ptree(DB_Trie, DB_Term),
  (DB_Term = depth(List, Inter); DB_Term = breadth(List, Inter)),
  member(L, List),
  ((is_label(L), \+ deref(L, _)) ->
    asserta(deref(L, Inter))
  ;
    true
  ),
  fail.
mark_deref(_).
