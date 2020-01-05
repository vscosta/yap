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
%  Theofrastos Mantadelis
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
% nested tries handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(nestedtries, [nested_trie_to_depth_breadth_trie/4]).

:- use_module(library(ordsets), [list_to_ord_set/2,
                                 ord_subset/2,
                                 ord_union/3,
                                 ord_intersection/3]).
:- use_module(library(lists), [append/3,
                               memberchk/2,
                               delete/3]).
:- use_module(library(tries), [trie_to_depth_breadth_trie/6,
                               trie_get_depth_breadth_reduction_entry/1,
                               trie_dup/2,
                               trie_close/1,
                               trie_open/1,
                               trie_replace_nested_trie/3,
                               trie_remove_entry/1,
                               trie_get_entry/2,
                               trie_put_entry/3,
                               trie_traverse/2,
                               trie_traverse_mode/1,
                               trie_usage/4]).

:- use_module(flags, [problog_define_flag/5, problog_flag/2]).

:- style_check(all).
:- yap_flag(unknown,error).

:- initialization((
			problog_define_flag(subset_check,    problog_flag_validate_boolean, 'perform subset check in nested tries', true, nested_tries),
  problog_define_flag(loop_refine_ancs, problog_flag_validate_boolean, 'refine ancestors if no loop exists', true, nested_tries),
  problog_define_flag(trie_preprocess, problog_flag_validate_boolean, 'perform a preprocess step to nested tries', false, nested_tries),
  problog_define_flag(refine_anclst,   problog_flag_validate_boolean, 'refine the ancestor list with their childs', false, nested_tries),
  problog_define_flag(anclst_represent,problog_flag_validate_in_list([list, integer]), 'represent the ancestor list', list, nested_tries)
)).

trie_replace_entry(_Trie, Entry, E, false):-
  trie_get_entry(Entry, Proof),
  memberchk(E, Proof), !,
  trie_remove_entry(Entry).
trie_replace_entry(Trie, Entry, E, true):-
  trie_get_entry(Entry, Proof),
  memberchk(E, Proof), !,
  delete(Proof, E, NewProof),
  (NewProof == [] ->
    trie_delete(Trie),
    trie_put_entry(Trie, [true], _)
  ;
    trie_remove_entry(Entry),
    trie_put_entry(Trie, NewProof, _)
  ).
trie_replace_entry(Trie, _Entry, t(ID), R):-
  trie_replace_nested_trie(Trie, ID, R).

trie_delete(Trie):-
  trie_traverse(Trie, R),
  trie_remove_entry(R),
  fail.
trie_delete(_Trie).

is_state(Variable):-
  Variable == true, !.
is_state(Variable):-
  Variable == false.
is_state(Variable):-
  nonvar(Variable),
  Variable = not(NestedVariable),
  is_state(NestedVariable).

is_trie(Trie, ID):-
  nonvar(Trie),
  Trie = t(ID), !.
is_trie(Trie, ID):-
  nonvar(Trie),
  Trie = not(NestedTrie),
  is_trie(NestedTrie, ID).

is_label(Label, ID):-
  atom(Label), !,
  atomic_concat('L', ID, Label).
is_label(Label, ID):-
  nonvar(Label),
  Label = not(NestedLabel),
  is_label(NestedLabel, ID).

simplify(not(false), true):- !.
simplify(not(true), false):- !.
simplify(not(not(A)), B):-
  !, simplify(A, B).
simplify(A, A).

% Ancestor related stuff

initialise_ancestors(0):-
  problog_flag(anclst_represent, integer).
initialise_ancestors([]):-
  problog_flag(anclst_represent, list).

add_to_ancestors(ID, Ancestors, NewAncestors):-
  integer(Ancestors), !,
  NewAncestors is (1 << (ID - 1)) \/ Ancestors.
add_to_ancestors(ID, Ancestors, NewAncestors):-
  is_list(Ancestors),
  list_to_ord_set([ID|Ancestors], NewAncestors).

ancestors_union(Ancestors1, Ancestors2, NewAncestors):-
  integer(Ancestors1), !,
  NewAncestors is Ancestors1 \/ Ancestors2.
ancestors_union(Ancestors1, Ancestors2, NewAncestors):-
  is_list(Ancestors1),
  ord_union(Ancestors1, Ancestors2, NewAncestors).

ancestor_subset_check(SubAncestors, Ancestors):-
  integer(SubAncestors), !,
  SubAncestors is Ancestors /\ SubAncestors.
ancestor_subset_check(SubAncestors, Ancestors):-
  is_list(SubAncestors),
  ord_subset(SubAncestors, Ancestors).

ancestor_loop_refine(Loop, Ancestors, 0):-
  var(Loop), integer(Ancestors), !.
ancestor_loop_refine(Loop, Ancestors, []):-
  var(Loop), is_list(Ancestors), !.
ancestor_loop_refine(true, Ancestors, Ancestors).

ancestor_child_refine(true, Ancestors, Childs, NewAncestors):-
  integer(Ancestors), !,
  NewAncestors is Ancestors /\ Childs.
ancestor_child_refine(true, Ancestors, Childs, NewAncestors):-
  is_list(Ancestors), !,
  ord_intersection(Ancestors, Childs, NewAncestors).
ancestor_child_refine(false, Ancestors, _, Ancestors).

% Cycle check related stuff
% missing synonym check

cycle_check(ID, Ancestors):-
  get_negated_synonym_id(ID, SynID),
  cycle_check_intern(SynID, Ancestors).

cycle_check_intern(ID, Ancestors):-
  integer(Ancestors), !,
  Bit is 1 << (ID - 1),
  Bit is Bit /\ Ancestors.
cycle_check_intern(ID, Ancestors):-
  is_list(Ancestors),
  memberchk(ID, Ancestors).

get_negated_synonym_id(ID, ID).
get_negated_synonym_id(ID, NegID):-
  tabling:has_synonyms,
  recorded(problog_table, store(Pred, ID, _, _, _), _),
  Pred =.. [Name0|Args],
  atomic_concat(problog_, Name1, Name0),
  atomic_concat(Name, '_original', Name1),
  get_negated_name(Name, NotName1),
  atomic_concat([problog_, NotName1, '_original'], NotName),
  NegPred =.. [NotName|Args],
  recorded(problog_table, store(NegPred, NegID, _, _, _), _).

get_negated_name(Name, NotName1):-
  recorded(problog_table_synonyms, negated(Name, NotName1), _), !.
get_negated_name(Name, NotName1):-
  recorded(problog_table_synonyms, negated(NotName1, Name), _).

trie_dup_reverse(Trie, DupTrie):-
  trie_open(DupTrie),
  trie_traverse_mode(backward),
  trie_dup_rev(Trie, DupTrie),
  trie_traverse_mode(forward).

trie_dup_rev(Trie, DupTrie):-
  \+ trie_usage(Trie, 0, 0, 0),
  trie_traverse(Trie, Entry),
  trie_get_entry(Entry, Term),
  trie_put_entry(DupTrie, Term, _),
  fail.
trie_dup_rev(_, _).


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
  (is_trie(Label, SID) ->
    trie_get_depth_breadth_reduction_entry(NestedEntry),
    trie_replace_entry(Trie, NestedEntry, Label, false),
    add_to_ancestors(SID, Ancestors, NewAncestors),
    make_nested_trie_base_cases(Trie, t(ID), DepthBreadthTrie, OptimizationLevel, EndCount, FinalEndCount, NewAncestors)
  ;
    FinalEndCount = EndCount,
    set_trie(ID, Label, Ancestors)
  ).

nested_trie_to_depth_breadth_trie(Trie, DepthBreadthTrie, FinalLabel, OptimizationLevel):-
  integer(OptimizationLevel),
  trie_open(DepthBreadthTrie),
  (problog_flag(trie_preprocess, true) ->
    preprocess(1, DepthBreadthTrie, OptimizationLevel, 0, StartCount)
  ;
    StartCount = 0
  ),
  initialise_ancestors(Ancestors),
  (problog_flag(loop_refine_ancs, true) ->
    trie_2_dbtrie_init(Trie, DepthBreadthTrie, OptimizationLevel, StartCount, _, Ancestors, FinalLabel, _, _Childs)
  ;
    trie_2_dbtrie_init(Trie, DepthBreadthTrie, OptimizationLevel, StartCount, _, Ancestors, FinalLabel, true, _Childs)
  ),
  eraseall(problog_trie_table).

trie_2_dbtrie_init(ID, DepthBreadthTrie, OptimizationLevel, StartCount, EndCount, Ancestors, Label, ContainLoop, FinalChilds):-
  initialise_ancestors(Childs),
  get_trie_pointer(ID, Trie),
  trie_dup_reverse(Trie, CopyTrie),
  trie_2_dbtrie_intern(CopyTrie, DepthBreadthTrie, OptimizationLevel, StartCount, EndCount, Ancestors, Label, ContainLoop, Childs, FinalChilds),
  trie_close(CopyTrie).

trie_2_dbtrie_intern(Trie, DepthBreadthTrie, OptimizationLevel, StartCount, FinalEndCount, Ancestors, TrieLabel, ContainLoop, Childs, FinalChilds):-
  trie_to_depth_breadth_trie(Trie, DepthBreadthTrie, Label, OptimizationLevel, StartCount, EndCount),
  (is_trie(Label, ID) ->
    problog_flag(refine_anclst, ChildRefineAncestors),
    trie_get_depth_breadth_reduction_entry(NestedEntry),
    (cycle_check(ID, Ancestors) ->
      ContainLoop = true,
      NewLabel = false,
      NewEndCount = EndCount,
      initialise_ancestors(GrandChilds)
    ; get_trie(ID, NewLabel, Ancestors) ->
      GrandChilds = Ancestors,
      NewEndCount = EndCount
    ;
      add_to_ancestors(ID, Ancestors, NewAncestors),
      trie_2_dbtrie_init(ID, DepthBreadthTrie, OptimizationLevel, EndCount, NewEndCount, NewAncestors, DerefLabel, NewContainLoop, GrandChilds),
      ancestor_loop_refine(NewContainLoop, Ancestors, RefinedAncestors1),
      ancestor_child_refine(ChildRefineAncestors, RefinedAncestors1, GrandChilds, RefinedAncestors),
      simplify(DerefLabel, NewLabel),
      set_trie(ID, NewLabel, RefinedAncestors),
      ContainLoop = NewContainLoop
    ),
    trie_replace_entry(Trie, NestedEntry, t(ID), NewLabel),
    (ChildRefineAncestors ->
      add_to_ancestors(ID, Childs, NewChilds1),
      ancestors_union(NewChilds1, GrandChilds, NewChilds)
    ;
      NewChilds = Childs
    ),
    trie_2_dbtrie_intern(Trie, DepthBreadthTrie, OptimizationLevel, NewEndCount, FinalEndCount, Ancestors, TrieLabel, ContainLoop, NewChilds, FinalChilds)
  ;
    FinalEndCount = EndCount,
    TrieLabel = Label,
    FinalChilds = Childs
  ).

% predicate to check/remember resolved tries

get_trie_pointer(ID, Trie):-
  problog:problog_chktabled(ID, Trie), !.
get_trie_pointer(Trie, Trie).

get_trie(Trie, Label, Ancestors):-
  problog_flag(subset_check, true), !,
  recorded(problog_trie_table, store(Trie, StoredAncestors, Label), _),
  ancestor_subset_check(StoredAncestors, Ancestors).
get_trie(Trie, Label, Ancestors):-
  recorded(problog_trie_table, store(Trie, StoredAncestors, Label), _),
  StoredAncestors == Ancestors.

set_trie(Trie, Label, Ancestors):-
  recordz(problog_trie_table, store(Trie, Ancestors, Label), _).
