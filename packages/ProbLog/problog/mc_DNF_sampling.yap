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
%  Theofrastos Mantadelis, Dimitar Sht. Shterionov
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

:- module(mc_DNF_sampling, [problog_dnf_sampling/3]).

:- use_module(library(lists), [memberchk/2]).

:- use_module(variables).
:- use_module(sampling, _, [problog_random/1,
                            problog_convergence_check/6]).

:- use_module(flags, _, [problog_define_flag/5,
                         problog_flag/2]).

:- use_module(os, _, [convert_filename_to_working_path/2]).

:- use_module(hash_table).

:- initialization((
  problog_define_flag(search_method, problog_flag_validate_in_list([linear, binary]), 'search method for picking proof', binary, monte_carlo_sampling_dnf),
  problog_define_flag(represent_world, problog_flag_validate_in_list([list, record, array, hash_table]), 'structure that represents sampled world', array, monte_carlo_sampling_dnf),

  problog_var_define(dnf_sampling_time, times, time, messages('DNF Sampling', ':', ' ms')),
  problog_var_define(probability_lower, result, untyped, messages('Lower probability bound', ' = ', '')),
  problog_var_define(probability_upper, result, untyped, messages('Upper probability bound', ' = ', ''))
)).

% problog_independed(T, P):-
%   tries:trie_traverse_first(T, FirstRef), !,
%   problog_independed(FirstRef, P, 0.0, _, 0).
% problog_independed(_T, 0.0).

problog_independed(T, P, ProofCNT):-
  tries:trie_traverse_first(T, FirstRef), !,
  problog_independed(FirstRef, P, 0.0, ProofCNT, 0).
problog_independed(_T, 0.0, 0).

%%% this should be generalized to handle nested tries
problog_independed([], P, P, ProofCNT, ProofCNT).
problog_independed(ProofRef, P, A, ProofCNT, Index):-
  tries:trie_get_entry(ProofRef, Proof),
  calculate_prob_proof(Proof, Pproof),
  calculate_prob_proof(Proof, Pproof),
  NA is A + Pproof,
  NIndex is Index + 1,
  recordz(problog_mc_dnf, proof(Index, ProofRef, Pproof, NA), _),
  (tries:trie_traverse_next(ProofRef, NxtProofRef) ->
    NextProofRef = NxtProofRef
  ;
    NextProofRef = []
  ),
  problog_independed(NextProofRef, P, NA, ProofCNT, NIndex).


%%% this should be generalized to handle nested tries
calculate_prob_proof([true], 1.0):-!.
calculate_prob_proof(Proof, P):-
  calculate_curr_prob(Proof, 0.0, L),
  P is exp(L).


calculate_curr_prob([], Acc, Acc).
calculate_curr_prob([ID|Rest], AccCurrProb, CurrProb):-
  get_log_prob_not_check(ID, IDProb),
  AccCurrProb1 is AccCurrProb + IDProb,
  calculate_curr_prob(Rest, AccCurrProb1, CurrProb).

%%%% this should be generalized and go to problog_fact module
get_log_prob_not_check(not(ID), IDProb):-
  !, problog:get_fact_probability(ID, Prob1),
  Prob2 is 1 - Prob1, IDProb is log(Prob2).
get_log_prob_not_check(ID, IDProb):-
  problog:get_fact_log_probability(ID, IDProb).


problog_mc_DNF(Trie, Delta, P):-
  problog_flag(mc_batchsize, Samples),
  problog_independed(Trie, Pind, ProofCNT),
  (ProofCNT > 1 ->
    problog_mc_DNF(Trie, Pind, ProofCNT, Delta, Samples, 0, SamplesSoFar, Naccepted, 0, _Epsilon),
    P is Naccepted / SamplesSoFar * Pind
  ;
    P is Pind,
    problog_var_set(probability, P)
  ),
  eraseall(problog_mc_dnf).

problog_mc_DNF(_Trie, Pind, _ProofCNT, Delta, Samples, SamplesSoFar, SamplesSoFar, Naccepted, Naccepted, Epsilon):-
  SamplesSoFar > 0,
  SamplesSoFar mod Samples =:= 0,
  P is Naccepted / SamplesSoFar * Pind,
  problog_timer_pause(dnf_sampling_time, T),
  problog_timer_resume(dnf_sampling_time),
  problog_convergence_check(T, P, SamplesSoFar, Delta, Epsilon, Converge),
  (Converge = true; Converge = terminate), !,
  problog_var_set(samples, SamplesSoFar),
  problog_var_set(probability, P),
  Pl is P - Epsilon,
  Ph is P + Epsilon,
  problog_var_set(probability_lower, Pl),
  problog_var_set(probability_upper, Ph).
/*
problog_mc_DNF(_Trie, _Pind, _ProofCNT, _Delta, Samples, SamplesSoFar, _SamplesSoFar, _Naccepted, _Naccepted, _Epsilon):-
  SamplesSoFar mod Samples =:= 0,
  fail.*/

problog_mc_DNF(Trie, Pind, ProofCNT, Delta, Samples, SAcc, SamplesSoFar, Naccepted, NAcc, Epsilon):-
  NSAcc is SAcc + 1,
  problog_random(RND),
  Thr is RND * Pind,
  tries:trie_traverse_mode(backward),
  (problog_flag(search_method, binary) ->
    get_sample_proof_binary(CurRef, Thr, ProofCNT, L_true_pf, L_false_pf)
  ;
    get_sample_proof_linear(CurRef, Thr, L_true_pf, L_false_pf)
  ),
  (tries:trie_traverse_next(CurRef, NxtRef) ->
    NextRef = NxtRef
  ;
    NextRef = []
  ),
  (check_sample_proofs(NextRef, L_true_pf, L_false_pf) ->
    NNAcc is NAcc + 1
  ;
    NNAcc is NAcc
  ),
  (problog_flag(represent_world, record) ->
    eraseall(problog_sample_world)
  ;
    (problog_flag(represent_world, array) ->
      close_static_array(problog_sample_world)
    ;
      (problog_flag(represent_world, hash_table) ->
        hash_table_delete(L_true_pf),
        hash_table_delete(L_false_pf)
      ;
        true
      )
    )
  ),
  tries:trie_traverse_mode(forward),
  problog_mc_DNF(Trie, Pind, ProofCNT, Delta, Samples, NSAcc, SamplesSoFar, Naccepted, NNAcc, Epsilon).


get_sample_proof_linear(Ref, Thr, L_true_pf, L_false_pf):-
  recorded(problog_mc_dnf, proof(_Index, Ref, _Pproof, Ps), _),
  Thr < Ps,
  tries:trie_get_entry(Ref, Proof),
  (problog_flag(represent_world, hash_table) ->
    make_hash_tables(L_true_pf, L_false_pf),
    add_proof_to_hash_world(Proof, L_true_pf, L_false_pf)
  ;
    (problog_flag(represent_world, record) ->
      add_proof_to_rec_world(Proof)
    ;
      (problog_flag(represent_world, array) ->
        nb_getval(probclause_counter, ProbFactCNT),
        Size is ProbFactCNT + 1,
        static_array(problog_sample_world, Size, int),
        add_proof_to_array_world(Proof)
      ;
        add_proof_to_list_world(Proof, L_true_pf, L_false_pf)
      )
    )
  ).

get_sample_proof_binary(Ref, Thr, ProofCNT, L_true_pf, L_false_pf):-
  Last is ProofCNT - 1,
  binary_search(Thr, 0, Last, Ref), !,
  tries:trie_get_entry(Ref, Proof),
  (problog_flag(represent_world, hash_table) ->
    make_hash_tables(L_true_pf, L_false_pf),
    add_proof_to_hash_world(Proof, L_true_pf, L_false_pf)
  ;
    (problog_flag(represent_world, record) ->
      add_proof_to_rec_world(Proof)
    ;
      (problog_flag(represent_world, array) ->
        nb_getval(probclause_counter, ProbFactCNT),
        Size is ProbFactCNT + 1,
        static_array(problog_sample_world, Size, int),
        add_proof_to_array_world(Proof)
      ;
        add_proof_to_list_world(Proof, L_true_pf, L_false_pf)
      )
    )
  ).


binary_search(Thr, From, To, Ref):-
  1 is To - From, !,
  recorded(problog_mc_dnf, proof(From, RefF, _Pproof, PsF), _),
  (Thr > PsF ->
    recorded(problog_mc_dnf, proof(To, Ref, _PproofTo, _Ps), _)
  ;
    Ref = RefF
  ).

binary_search(_Thr, Index, Index, Ref):-
  !, recorded(problog_mc_dnf, proof(Index, Ref, _Pproof, _Ps), _).

binary_search(Thr, From, To, Res):-
  Look is From + integer((To - From + 1) / 2),
  recorded(problog_mc_dnf, proof(Look, _Ref, _Pproof, Ps), _), !,
  (Thr > Ps ->
    NewFrom is Look + 1,
    NewTo is To
  ;
    NewFrom is From,
    NewTo is Look
  ),
  binary_search(Thr, NewFrom, NewTo, Res).


%%%%%%%%% This code can be improved and generalized %%%%%%%%%
check_sample_proofs([], _, _).

check_sample_proofs(CurRef, L_true_pf, L_false_pf):-
  !, tries:trie_get_entry(CurRef, Proof),
  (problog_flag(represent_world, hash_table) ->
    check_proof_in_hash_world(Proof, L_true_pf, L_false_pf),
    NL_true_pf = L_true_pf,
    NL_false_pf = L_false_pf
  ;
    (problog_flag(represent_world, record) ->
      check_proof_in_rec_world(Proof),
      NL_true_pf = L_true_pf,
      NL_false_pf = L_false_pf
    ;
      (problog_flag(represent_world, array) ->
        check_proof_in_array_world(Proof),
        NL_true_pf = L_true_pf,
        NL_false_pf = L_false_pf
      ;
        check_proof_in_list_world(Proof, L_true_pf, NL_true_pf, L_false_pf, NL_false_pf)
      )
    )
  ),
  (tries:trie_traverse_next(CurRef, NxtRef) ->
    NextRef = NxtRef
  ;
    NextRef = []
  ),
  check_sample_proofs(NextRef, NL_true_pf, NL_false_pf).

add_proof_to_array_world([]).
add_proof_to_array_world([not(H)|T]):-
  !, update_array(problog_sample_world, H, -1), add_proof_to_array_world(T).
add_proof_to_array_world([H|T]):-
  update_array(problog_sample_world, H, 1), add_proof_to_array_world(T).


check_proof_in_array_world([not(F)|_Rest]):-
  array_element(problog_sample_world, F, 1), !.

check_proof_in_array_world([not(F)|Rest]):-
  array_element(problog_sample_world, F, -1), !,
  check_proof_in_array_world(Rest).

check_proof_in_array_world([not(F)|Rest]):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  (Dice =< NumProbF ->
    update_array(problog_sample_world, F, 1)
  ;
    update_array(problog_sample_world, F, -1),
    check_proof_in_array_world(Rest)
  ).

check_proof_in_array_world([F|_Rest]):-
  array_element(problog_sample_world, F, -1), !.

check_proof_in_array_world([F|Rest]):-
  array_element(problog_sample_world, F, 1), !,
  check_proof_in_array_world(Rest).

check_proof_in_array_world([F|Rest]):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  (Dice > NumProbF ->
    update_array(problog_sample_world, F, -1)
  ;
    update_array(problog_sample_world, F, 1),
    check_proof_in_array_world(Rest)
  ).




add_proof_to_rec_world([]).
add_proof_to_rec_world([not(H)|T]):-
  !, recordz(problog_sample_world, false_fact(H), _), add_proof_to_rec_world(T).
add_proof_to_rec_world([H|T]):-
  recordz(problog_sample_world, true_fact(H), _), add_proof_to_rec_world(T).

check_proof_in_rec_world([not(F)|_Rest]):-
  recorded(problog_sample_world, true_fact(F), _), !.

check_proof_in_rec_world([not(F)|Rest]):-
  recorded(problog_sample_world, false_fact(F), _), !,
  check_proof_in_rec_world(Rest).

check_proof_in_rec_world([not(F)|Rest]):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  (Dice =< NumProbF ->
    recordz(problog_sample_world, true_fact(F), _)
  ;
    recordz(problog_sample_world, false_fact(F), _),
    check_proof_in_rec_world(Rest)
  ).

check_proof_in_rec_world([F|_Rest]):-
  recorded(problog_sample_world, false_fact(F), _), !.

check_proof_in_rec_world([F|Rest]):-
  recorded(problog_sample_world, true_fact(F), _), !,
  check_proof_in_rec_world(Rest).

check_proof_in_rec_world([F|Rest]):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  (Dice > NumProbF ->
    recordz(problog_sample_world, false_fact(F), _)
  ;
    recordz(problog_sample_world, true_fact(F), _),
    check_proof_in_rec_world(Rest)
  ).



make_hash_tables(TrueHashTable, FalseHashTable):-
  nb_getval(probclause_counter, ProbFactCNT),
  hash_table_init(ProbFactCNT, TrueHashTable),
  hash_table_init(ProbFactCNT, FalseHashTable).


add_proof_to_hash_world([], _TrueHashTable, _FalseHashTable).
add_proof_to_hash_world([not(H)|T], TrueHashTable, FalseHashTable):-
  !, problog_key_to_tuple(H, Tuple),
  hash_table_lookup(FalseHashTable, Tuple, _),
  add_proof_to_hash_world(T, TrueHashTable, FalseHashTable).
add_proof_to_hash_world([H|T], TrueHashTable, FalseHashTable):-
  problog_key_to_tuple(H, Tuple),
  hash_table_lookup(TrueHashTable, Tuple, _),
  add_proof_to_hash_world(T, TrueHashTable, FalseHashTable).


check_proof_in_hash_world([not(F)|_Rest], TrueHashTable, _FalseHashTable):-
  problog_key_to_tuple(F, Tuple),
  hash_table_contains(TrueHashTable, Tuple, _), !.

check_proof_in_hash_world([not(F)|Rest], TrueHashTable, FalseHashTable):-
  problog_key_to_tuple(F, Tuple),
  hash_table_contains(FalseHashTable, Tuple, _), !,
  check_proof_in_hash_world(Rest, TrueHashTable, FalseHashTable).

check_proof_in_hash_world([not(F)|Rest], TrueHashTable, FalseHashTable):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  problog_key_to_tuple(F, Tuple),
  (Dice =< NumProbF ->
    hash_table_lookup(TrueHashTable, Tuple, _)
  ;
    hash_table_lookup(FalseHashTable, Tuple, _),
    check_proof_in_hash_world(Rest, TrueHashTable, FalseHashTable)
  ).

check_proof_in_hash_world([F|_Rest], _TrueHashTable, FalseHashTable):-
  problog_key_to_tuple(F, Tuple),
  hash_table_contains(FalseHashTable, Tuple, _), !.

check_proof_in_hash_world([F|Rest], TrueHashTable, FalseHashTable):-
  problog_key_to_tuple(F, Tuple),
  hash_table_contains(TrueHashTable, Tuple, _), !,
  check_proof_in_hash_world(Rest, TrueHashTable, FalseHashTable).

check_proof_in_hash_world([F|Rest], TrueHashTable, FalseHashTable):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  problog_key_to_tuple(F, Tuple),
  (Dice > NumProbF ->
    hash_table_lookup(FalseHashTable, Tuple, _)
  ;
    hash_table_lookup(TrueHashTable, Tuple, _),
    check_proof_in_hash_world(Rest, TrueHashTable, FalseHashTable)
  ).



add_proof_to_list_world([], [], []).
add_proof_to_list_world([not(H)|T], TrueList, [H|FalseList]):-
  add_proof_to_list_world(T, TrueList, FalseList).
add_proof_to_list_world([H|T], [H|TrueList], FalseList):-
  add_proof_to_list_world(T, TrueList, FalseList).

check_proof_in_list_world([not(F)|_Rest], TrueList, TrueList, FalseList, FalseList):-
  memberchk(F, TrueList), !.

check_proof_in_list_world([not(F)|Rest], TrueList, NewTrueList, FalseList, NewFalseList):-
  memberchk(F, FalseList), !,
  check_proof_in_list_world(Rest, TrueList, NewTrueList, FalseList, NewFalseList).

check_proof_in_list_world([not(F)|Rest], TrueList, NewTrueList, FalseList, NewFalseList):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  (Dice =< NumProbF ->
    NewTrueList = [F|TrueList],
    NewFalseList = FalseList
  ;
    check_proof_in_list_world(Rest, TrueList, NewTrueList, [F|FalseList], NewFalseList)
  ).

check_proof_in_list_world([F|_Rest], TrueList, TrueList, FalseList, FalseList):-
  memberchk(F, FalseList), !.

check_proof_in_list_world([F|Rest], TrueList, NewTrueList, FalseList, NewFalseList):-
  memberchk(F, TrueList), !,
  check_proof_in_list_world(Rest, TrueList, NewTrueList, FalseList, NewFalseList).

check_proof_in_list_world([F|Rest], TrueList, NewTrueList, FalseList, NewFalseList):-
  !, problog_random(RND), Dice is RND,
  problog:get_fact_probability(F, NumProbF),
  (Dice > NumProbF ->
    NewTrueList = TrueList,
    NewFalseList = [F|FalseList]
  ;
    check_proof_in_list_world(Rest, [F|TrueList], NewTrueList, FalseList, NewFalseList)
  ).



problog_collect_trie(Goal, Threshold) :-
  problog:init_problog_low(Threshold),
  problog:problog_control(off, up),
  problog:problog_control(on, exact),
  problog_var_timer_start(sld_time),
  problog:problog_call(Goal),
  problog:add_solution,
  fail.
problog_collect_trie(_, _) :-
  problog:problog_control(off, exact),
  problog_var_timer_stop(sld_time).

problog_dnf_sampling(Goal, Delta, P):-
  % this should be generalized with general log file
  problog_flag(mc_logfile, File1),
  convert_filename_to_working_path(File1, File),
  open(File, write, Log),
  format(Log,'# goal: ~q~n#delta: ~w~n',[Goal, Delta]),
  format(Log,'# samples  prob   low   high  time~2n',[]),
  close(Log),

  problog_collect_trie(Goal, 0.0),
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
  problog_var_timer_start(dnf_sampling_time),
  problog_mc_DNF(Trie_Completed_Proofs, Delta, P),
  problog_var_timer_stop(dnf_sampling_time),
  (problog_flag(verbose, true) ->
    print:problog_statistics
  ;
    true
  ),
  ptree:delete_ptree(Trie_Completed_Proofs),
  problog:clear_tabling.
