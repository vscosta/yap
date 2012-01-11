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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Problog Tabling: use Yap tabling for monte carlo
%                  use probabilistic semantics tabling for exact
%                  rest methods not benefited from tabling yet
%
% Look at: CICLOPS 2009 Mantadelis Theofrastos & Gerda Janssens
%          SRL 2009 Angelika Kimming & Vitor
%
% Currently: Exact handles ground goals only and loops
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(tabling, [problog_table/1,
                    problog_tabled/1,
                    problog_neg/1,
                    init_tabling/0,
                    clear_tabling/0,
                    retain_tabling/0,
                    clear_retained_tables/0,
                    problog_chktabled/2,
                    problog_abolish_table/1,
                    problog_abolish_all_tables/0,
                    problog_tabling_negated_synonym/2,
                    problog_tabling_get_negated_from_pred/2,
                    problog_tabling_get_negated_from_id/2,
                    op(1150, fx, problog_table)]).

:- use_module(library(lists), [memberchk/2]).

:- use_module(extlists, _, [open_end_memberchk/2,
                            open_end_add/3,
                            open_end_add_unique/3,
                            open_end_close_end/2]).

:- use_module(flags, _, [problog_define_flag/4, problog_flag/2]).

:- use_module(ptree, _, [init_ptree/1,
                         delete_ptree/1,
                         merge_ptree/2,
                         empty_ptree/1]).

:- op( 1150, fx, problog_table ).

:- meta_predicate(problog_table(0)).
:- meta_predicate(problog_neg(0)).

:- dynamic(problog_tabled/1).
:- dynamic(has_synonyms/0).
:- dynamic(problog_tabling_retain/1).

:- initialization((
  problog_define_flag(max_depth, problog_flag_validate_integer, 'maximum proof depth', -1),
  problog_define_flag(retain_tables, problog_flag_validate_boolean, 'retain tables after query', false)
)).

init_tabling :-
  nb_setval(problog_current_depth, 0),
  nb_setval(problog_suspended_tries, []),
  nb_setval(problog_current_ptree, problog_completed_proofs),
  nb_setval(problog_nested_tries, false),
  nb_setval(problog_tabling_next_index, 1).

clear_tabling:-
  nb_setval(problog_suspended_tries, []),
  nb_setval(problog_current_ptree, 1),
  nb_setval(problog_nested_tries, false),
  nb_setval(problog_tabling_next_index, 1),
  forall(problog_chktabled(_, Trie),
    (problog_tabling_retain(Trie) ->
      true
    ;
      delete_ptree(Trie)
    )
  ),
  eraseall(problog_table), !.
clear_tabling.

retain_tabling:-
  forall(problog_chktabled(_, Trie), assertz(problog_tabling_retain(Trie))).

clear_retained_tables:-
  forall(problog_tabling_retain(Trie), delete_ptree(Trie)),
  retractall(problog_tabling_retain(_)).

problog_chktabled(Index, Trie):-
  recorded(problog_table, store(_, Index, Trie, _, _), _).

problog_table_next_index(Index):-
  nb_getval(problog_tabling_next_index, Index),
  NIndex is Index + 1,
  nb_setval(problog_tabling_next_index, NIndex).

problog_table(M:P) :- !,
  problog_table(P, M).
problog_table(P) :-
  prolog_load_context(module, M),
  problog_table(P, M).

problog_table(M:P, _) :-
  problog_table(P, M).
problog_table((P1, P2), M) :-
  problog_table(P1, M),
  problog_table(P2, M).
problog_table(Name/Arity, Module) :-
  length(Args,Arity),
  Head =.. [Name|Args],
  \+ predicate_property(Module:Head, dynamic), !,
  throw(error('problog_table: Problog tabling currently requires the predicate to be declared dynamic and compiles it to static.')).
problog_table(Name/Arity, Module) :-
  length(Args,Arity),
  Head =.. [Name|Args],
  atom_concat(['problog_', Name, '_original'], OriginalName),
  atom_concat(['problog_', Name, '_mctabled'], MCName),
  atom_concat(['problog_', Name, '_tabled'], ExactName),

  % Monte carlo tabling
  catch((table(Module:MCName/Arity),
         assertz(problog_tabled(Module:Name/Arity))), _,
        (format(user_error, 'Warning: Tabling was not enabled over compilation, montecarlo tabling is disabled!~nPredicate: ~q~n', [Module:MCName/Arity]))),

  findall(_,(
    OriginalPred =.. [OriginalName|Args],
    MCPred =.. [MCName|Args],
    retract(Module:(Head:-Body)),
    assert_static(Module:(OriginalPred:-Body)),
    assert_static(Module:(MCPred:-Body))
  ),_),
  OriginalPred =.. [OriginalName|Args],
  MCPred =.. [MCName|Args],
  ExactPred =.. [ExactName|Args],
  assertz(Module:(
                  Head:-
                    (problog:problog_control(check, exact) ->
                      ExactPred
                    ; problog:problog_control(check, mc) ->
                      MCPred
                    ;
                      OriginalPred
                    )
  )),
  % Exact method tabling
  assert_static((
    Module:ExactPred :-
      (user:problog_user_ground(Head) ->
        nb_setval(problog_nested_tries, true),
        get_negated_synonym_state(OriginalPred, State),
        (State = false ->
          true
        ;
          (recorded(problog_table, store(OriginalPred, Hash, HashTrie, SuspTrie, Finished), Ref)->
            (Finished = false ->
              b_getval(problog_suspended_tries, Susp),
              b_setval(problog_suspended_tries, [Hash|Susp])
            ;
              Finished
            ),
            b_getval(problog_current_proof, IDs),
            \+ open_end_memberchk(not(t(Hash)), IDs),
            open_end_add_unique(t(Hash), IDs, NIDs),
            b_setval(problog_current_proof, NIDs)
          ;
            b_getval(problog_current_proof, OIDs),
            b_getval(problog_current_ptree, CurrentControlTrie),
            b_getval(CurrentControlTrie, OCurTrie),
            init_ptree(HashTrie),
            init_ptree(SuspTrie),
            b_setval(problog_current_proof, []),
            b_setval(CurrentControlTrie, HashTrie),
            problog_table_next_index(Hash),
            recordz(problog_table, store(OriginalPred, Hash, HashTrie, SuspTrie, false), Ref),
            problog_flag(max_depth, MaxDepth),
            (MaxDepth > 0 ->
              nb_getval(problog_current_depth, CurDepth)
            ;
              CurDepth is MaxDepth - 1
            ),
            (CurDepth < MaxDepth ->
              NewDepth is CurDepth + 1,
              b_setval(problog_current_depth, NewDepth),
              findall(_, (
                Module:OriginalPred,
                b_getval(problog_suspended_tries, Susp),
                (memberchk(Hash, Susp) ->
                  b_setval(CurrentControlTrie, SuspTrie) % maybe necessary to remove hash from susp
                ;
                  true
                ),
                problog:add_solution
                        ), _) % eager goal proofing (easier to expand for non-ground version)
            ;
/*              (empty_ptree(HashTrie) ->
                erase(Ref),
                fail
              ;
                true
              )*/
              true
            ),
            erase(Ref),
            (empty_ptree(HashTrie) ->
              recordz(problog_table, store(OriginalPred, Hash, HashTrie, SuspTrie, fail), _NRef),
              delete_ptree(SuspTrie) %,
              %fail            % no justification exists
            ;
              recordz(problog_table, store(OriginalPred, Hash, HashTrie, SuspTrie, true), _NRef),
              merge_ptree(HashTrie, SuspTrie),
              delete_ptree(SuspTrie)
            ),
            b_setval(CurrentControlTrie, OCurTrie),
            \+ open_end_memberchk(not(t(Hash)), OIDs),
            open_end_add_unique(t(Hash), OIDs, NOIDs),
            b_setval(problog_current_proof, NOIDs)
          )
        )
      ;
%         writeln(non_ground),
        Module:OriginalPred
      )
  )).

problog_abolish_all_tables:-
  abolish_all_tables.

problog_abolish_table(M:P/A):-
  atom_concat(['problog_', P, '_mctabled'], MCName),
  abolish_table(M:MCName/A).

% supports exact, monte-carlo, requires expansion of tabling for rest methods
problog_neg(M:G):-
  problog:problog_control(check, exact),
  functor(G, Name, Arity),
  \+ problog_tabled(M:Name/Arity),
  \+ problog:problog_predicate(Name, Arity),
  \+ (Name == problog_neg, Arity == 1),
  throw(problog_neg_error('Error: goal must be dynamic and tabled', M:G)).
problog_neg(M:G):-
  % exact inference
  problog:problog_control(check, exact),
  b_getval(problog_current_proof, IDs),
  b_setval(problog_current_proof, []),
  M:G,
  b_getval(problog_current_proof, L),
  open_end_close_end(L, [Trie]),
  \+ open_end_memberchk(Trie, IDs),
  open_end_add_unique(not(Trie), IDs, NIDs),
  b_setval(problog_current_proof, NIDs).
problog_neg(M:G):-
  % monte carlo sampling
  problog:problog_control(check, mc),
  \+ M:G.

% This predicate assigns a synonym for negation that means: NotName = problog_neg(Name)
problog_tabling_negated_synonym(Name, NotName):-
  recorded(problog_table_synonyms, negated(Name, NotName), _), !.
problog_tabling_negated_synonym(Name, NotName):-
  retractall(has_synonyms),
  assertz(has_synonyms),
  recordz(problog_table_synonyms, negated(Name, NotName), _).

problog_tabling_get_negated_from_pred(Pred, Ref):-
  tabling:has_synonyms,
  Pred =.. [Name0|Args],
  atomic_concat(problog_, Name1, Name0),
  atomic_concat(Name, '_original', Name1),
  (recorded(problog_table_synonyms, negated(Name, NotName1), _);
   recorded(problog_table_synonyms, negated(NotName1, Name), _)),
  atomic_concat([problog_, NotName1, '_original'], NotName),
  NegPred =.. [NotName|Args],
  recorded(problog_table, store(NegPred, _, _, _, _), Ref), !.

problog_tabling_get_negated_from_id(ID, Ref):-
  tabling:has_synonyms,
  recorded(problog_table, store(Pred, ID, _, _, _), _),
  Pred =.. [Name0|Args],
  atomic_concat(problog_, Name1, Name0),
  atomic_concat(Name, '_original', Name1),
  (recorded(problog_table_synonyms, negated(Name, NotName1), _);
   recorded(problog_table_synonyms, negated(NotName1, Name), _)),
  atomic_concat([problog_, NotName1, '_original'], NotName),
  NegPred =.. [NotName|Args],
  recorded(problog_table, store(NegPred, _, _, _, _), Ref), !.


get_negated_synonym_state(Pred, Fin):-
  tabling:has_synonyms,
  Pred =.. [Name0|Args],
  atomic_concat(problog_, Name1, Name0),
  atomic_concat(Name, '_original', Name1),
  (recorded(problog_table_synonyms, negated(Name, NotName1), _);
   recorded(problog_table_synonyms, negated(NotName1, Name), _)),
  atomic_concat([problog_, NotName1, '_original'], NotName),
  NegPred =.. [NotName|Args],
  recorded(problog_table, store(NegPred, _, _, _, Fin), _), !.
get_negated_synonym_state(_, true).
/*

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


*/
