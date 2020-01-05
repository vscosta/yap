%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-12-08 16:20:16 +0100 (Thu, 08 Dec 2011) $
%  $Revision: 6775 $
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
%  Angelika Kimmig, Vitor Santos Costa, Bernd Gutmann,
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




/** @defgroup ProbLogPredicates  Computing Probabilities of Events
 *  @ingroup problog
 *
 * This chapter describes the predicates defined by ProbLog for evaluating the probability of queries.
 *
 *@{
 *
 * ProbLog inference
 *
 * assumes probabilistic facts as Prob::Fact and clauses in normal Prolog format
 *
 * provides following inference modes (16/12/2008):
 * - approximation with interval width Delta (IJCAI07): problog_delta(+Query,+Delta,-Low,-High,-Status)
 * - bounds based on single probability threshold: problog_threshold(+Query,+Threshold,-Low,-High,-Status)
 * - as above, but lower bound only: problog_low(+Query,+Threshold,-Low,-Status)
 * - lower bound based on K most likely proofs: problog_kbest(+Query,+K,-Low,-Status)
 * - explanation probability (ECML07): problog_max(+Query,-Prob,-FactsUsed)
 * - exact probability: problog_exact(+Query,-Prob,-Status)
 * - sampling: problog_montecarlo(+Query,+Delta,-Prob)
 *
 *
 * angelika.kimmig@cs.kuleuven.be
*/

/**
 * @pred problog_delta(+G , +Interval_width, -Bound_low, -Bound_up, -Status)
 *
This predicate returns the lower and upper bound of the probability of achieving the goal G by using an iterative
deepening approach with the given interval width.
*/
/**
 * @pred problog_threshold(+G , +Prob, -Bound_low, -Bound_up, -Status)
 *
This predicate returns the lower and upper bound of the probability of achieving the goal G obtained by cutting the sld tree at the given probability for each branch.
*/

/**
 * @pred problog_low(+G, +Prob, -Bound_low, -Status)
 *
This predicate returns the lower bound of the probability of achieving the goal G obtained by cutting the sld tree at the given probability for each branch.
*/

/**
@pred problog_help.


To access the help information in ProbLog type:
*/

%%@}

%%@{
/**
@defgroup ProbLogParameterLearning ProbLog Parameter Learning Predicates
@ingroup ProbLogI
@{
*/

/**
 * @pred example(+N, +Q, +Prob)
 *
This predicate specifies an example. Every example has as input a unique identifier (N), a query (Q) and a probability (Prob) associated with it.

Instead of queries, you can also give proofs as training example. They are encoded as the conjunction of the probabilistic facts used in the proof.
*/

/**
 * @pred test_example(+N, +Q, +Prob)
 *
This predicate specifies a test example. Every test example has as input a unique identifier (N), a query (Q) and a probability (Prob) associated with it.

Test examples are ignored during learning but are used afterwards to check the performance of the model. The ID namespace is shared between the test examples and the training examples and you may only reuse an ID if the queries are identical.
*/

/**
 * @pred do_learning(+N).
 *
Starts the learning algorithm with N iterations.

*/

/**
 * @pred do_learning(+N, +Epsilon).
 *
The output is created in the output subfolder of the current folder where YAP was started. There you will find the file log.dat which contains MSE on training and test set for every iteration, the timings, and some metrics on the gradient in CSV format. The files factprobs_N.pl contain the fact probabilities after the Nth iteration and the files predictions_N.pl contain the estimated probabilities for each training and test example - per default these file are generated every 5th iteration only.
1
Starts the learning algorithm. The learning will stop after N iterations or if the difference of the Mean Squared Error (MSE) between two iterations gets smaller than Epsilon - depending on what happens first.

@paragraph Learning Output
---------------

The output is created in the output subfolder of the current folder
where YAP was started. There you will find the file log.dat which
contains MSE on training and test set for every iteration, the timings,
and some metrics on the gradient in CSV format. The files
factprobs_N.pl contain the fact probabilities after the Nth iteration
and the files predictions_N.pl contain the estimated probabilities for
each training and test example - per default these file are generated
every 5th iteration only.

*/


:- module(problog, [problog_koptimal/3,
 		    problog_koptimal/4,
						  problog_delta/5,
                    problog_threshold/5,
                    problog_low/4,
                    problog_kbest/4,
                    problog_kbest_lbdd/4,
                    problog_kbest_save/6,
                    problog_max/3,
                    problog_kbest_explanations/3,
                    problog_exact/3,
                    problog_exact_lbdd/3,
		    problog_kbest_lbdd/4,
                    problog_all_explanations/2,
                    problog_all_explanations_unsorted/2,
                    problog_exact_save/5,
                    problog_montecarlo/3,
                    problog_dnf_sampling/3,
                    problog_answers/2,
                    problog_kbest_answers/3,
                    problog_table/1,
                    clear_retained_tables/0,
                    problog_neg/1,
                    get_fact_probability/2,
                    set_fact_probability/2,
                    get_continuous_fact_parameters/2,
                    set_continuous_fact_parameters/2,
                    get_fact/2,
                    tunable_fact/2,
                    tunable_continuous_fact/2,
                    continuous_fact/1,
                    non_ground_fact/1,
                    export_facts/1,
                    problog_help/0,
                    problog_version/0,
                    show_inference/0,
                    problog_dir/1,
                    set_problog_flag/2,
                    problog_flag/2,
                    problog_flags/0,
                    problog_flags/1,
                    reset_problog_flags/0,
                    problog_assert/1,
                    problog_assert/2,
                    problog_retractall/1,
                    problog_statistics/2,
                    problog_statistics/0,
                    grow_atom_table/1,
                    problog_exact_nested/3,
                    problog_tabling_negated_synonym/2,
                    problog_control/2,
                    build_trie/2,
                    build_trie/3,
                    problog_infer/2,
                    problog_infer/3,
                    problog_infer_forest/2,
                    write_bdd_struct_script/3,
                    problog_bdd_forest/1,
                    require/1,
                    unrequire/1,
                    bdd_files/2,
                    delete_bdd_forest_files/1,
                    recover_grounding_id/2,
                    grounding_is_known/2,
                    grounding_id/3,
                    decision_fact/2,
                    reset_non_ground_facts/0,
                    '::'/2,
                    probabilistic_fact/3,
		    continuous_fact/3,
                    init_problog/1,
                    problog_call/1,
                    problog_infer_forest_supported/0,
                    problog_bdd_forest_supported/0,
                    problog_real_kbest/4,
                    op( 550, yfx, :: ),
                    op( 550, fx, ?:: ),
                    op(1149, yfx, <-- ),
                    op( 1150, fx, problog_table ),
                    in_interval/3,
                    below/2,
                    above/2]).

:- style_check(all).
:- yap_flag(unknown,error).

% general yap modules
:- use_module(library(lists), [append/3,member/2,memberchk/2,reverse/2,select/3,nth1/3,nth1/4,nth0/4,sum_list/2]).
:- use_module(library(terms), [variable_in_term/2,variant/2] ).
:- use_module(library(random), [random/1]).
:- use_module(library(system), [tmpnam/1,shell/2,delete_file/1]).
:- use_module(library(ordsets), [list_to_ord_set/2, ord_insert/3, ord_union/3]).
%Joris
:- use_module(library(lineutils)).
%Joris


% problog related modules
:- use_module('problog/variables').
:- use_module('problog/extlists').
:- use_module('problog/gflags').
:- use_module('problog/flags').
:- use_module('problog/print').
:- use_module('problog/os').
:- use_module('problog/ptree', [init_ptree/1,
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
                                        bdd_vars_script/1]).
:- use_module('problog/tabling').
:- use_module('problog/sampling').
:- use_module('problog/intervals').
:- use_module('problog/mc_DNF_sampling').
:- use_module('problog/timer').
:- use_module('problog/utils').
:- use_module('problog/ad_converter').
:- catch(use_module('problog/variable_elimination'),_,true).

% op attaching probabilities to facts
:- op( 550, yfx, :: ).
:- op( 550, fx, ?:: ).

%%%%%%%%%%%%%%%%%%%%%%%%
% control predicates on various levels
%%%%%%%%%%%%%%%%%%%%%%%%

% global over all inference methods, internal use only
:- dynamic(problog_predicate/2).
:- dynamic(problog_continuous_predicate/3).
% global over all inference methods, exported
:- dynamic(tunable_fact/2).
:- dynamic(non_ground_fact/1).
:- dynamic(continuous_fact/1).
% global, manipulated via problog_control/2
:- dynamic(up/0).
:- dynamic(limit/0).
:- dynamic(mc/0).
:- dynamic(remember/0).
:- dynamic(exact/0).                         % Theo tabling
:- dynamic(find_decisions/0).
:- dynamic(internal_strategy/0).
% local to problog_delta
:- dynamic(low/2).
:- dynamic(up/2).
:- dynamic(stopDiff/1).
% local to problog_kbest
:- dynamic(current_kbest/3).
% local to problog_max
:- dynamic(max_probability/1).
:- dynamic(max_proof/1).
% local to problog_montecarlo
:- dynamic(mc_prob/1).
% local to problog_answers
:- dynamic(answer/1).
% to keep track of the groundings for non-ground facts
:- dynamic(grounding_is_known/2).
% for decisions
:- dynamic(decision_fact/2).
% for fact where the proabability is a variable
:- dynamic(dynamic_probability_fact/1).
:- dynamic(dynamic_probability_fact_extract/2).
% for storing continuous parts of proofs (Hybrid ProbLog)
:- dynamic([hybrid_proof/3, hybrid_proof/4]).
:- dynamic(hybrid_proof_disjoint/4).
% local to problog_koptimal
:- dynamic optimal_proof/2.
:- dynamic current_prob/1.
:- dynamic possible_proof/2.
:- dynamic impossible_proof/1.

:- table conditional_prob/4.

% ProbLog files declare prob. facts as P::G
% and this module provides the predicate X::Y to iterate over them
:- multifile('::'/2).

:- multifile(user:term_expansion/1).

% directory where simplecudd executable is located
% automatically set during loading -- assumes it is in /usr/local/bin or same place where YAP has
% been installed.)
:- getcwd(PD0),
	atom_concat(PD0, '../../bin', PD),
	set_problog_path(PD).

:- yap_flag(executable, Bin),
   file_directory_name(Bin, PD),
   set_problog_path(PD).


:- PD = '/usxor/local/bin',
	set_problog_path(PD).



%%%%%%%%%%%%
% iterative deepening on minimal proba(saved also in log-space for easy comparison with current values during search)
% - factor used to decrease threshold for next level, NewMin=Factor*OldMin (saved also in log-space)
%%%%%%%%%%%%

:- initialization((
	problog_define_flag(first_threshold, problog_flag_validate_indomain_0_1_open, 'starting threshold iterative deepening', 0.1, inference),
	problog_define_flag(last_threshold,  problog_flag_validate_indomain_0_1_open, 'stopping threshold iterative deepening', 1.0E-30, inference, flags:last_threshold_handler),
	problog_define_flag(id_stepsize,     problog_flag_validate_indomain_0_1_close, 'threshold shrinking factor iterative deepening', 0.5, inference, flags:id_stepsize_handler)
)).

%%%%%%%%%%%%
% prune check stops derivations if they use a superset of facts already known to form a proof
% (very) costly test, can be switched on/off here (This is obsolete as it is not included in implementation)
%%%%%%%%%%%%

:- initialization(
	problog_define_flag(prunecheck,      problog_flag_validate_switch, 'stop derivations including all facts of known proof', off, inference)
).

%%%%%%%%%%%%
% max number of calls to probabilistic facts per derivation (to ensure termination)
%%%%%%%%%%%%

:- initialization(
	problog_define_flag(maxsteps,        problog_flag_validate_posint, 'max. number of prob. steps per derivation', 1000, inference)
).

%%%%%%%%%%%%
% BDD timeout in seconds, used as option in BDD tool
% files to write BDD script and pars
% bdd_file overwrites bdd_par_file with matching extended name
% if different name wanted, respect order when setting
% save BDD information for the (last) lower bound BDD used during inference
% produces three files named save_script, save_params, save_map
% located in the directory given by problog_flag dir
%%%%%%%%%%%%

:- initialization((
%	problog_define_flag(bdd_path,        problog_flag_validate_directory, 'simplecudd directory', '.',bdd),
	problog_define_flag(bdd_time,        problog_flag_validate_posint, 'BDD computation timeout in seconds', 60, bdd),
	problog_define_flag(save_bdd,        problog_flag_validate_boolean, 'save BDD files for (last) lower bound', false, bdd),
	problog_define_flag(dynamic_reorder, problog_flag_validate_boolean, 'use dynamic re-ordering for BDD', true, bdd),
	problog_define_flag(bdd_static_order,    problog_flag_validate_boolean, 'use a static order', false, bdd)
)).


%%%%%%%%%%%%
% Storing the calculated BDD for later reuse in koptimal
% - nodedump bdd of the last constructed bdd
% - nodedump bdd file where the nodedump should be stored
%%%%%%%%%%%%
:- initialization((
	problog_define_flag(nodedump_bdd, problog_flag_validate_boolean, 'store the calculated BDD', false, bdd),
	problog_define_flag(nodedump_file, problog_flag_validate_file, 'file to store the nodedump of the BDD', nodedump_bdd, bdd)
)).

%%%%%%%%%%%%
% determine whether ProbLog outputs information (number of proofs, intermediate results, ...)
% default was true, as otherwise problog_delta won't output intermediate bounds
% default is false now, as dtproblog will flood the user with verbosity
%%%%%%%%%%%%

:- initialization(
	problog_define_flag(verbose,         problog_flag_validate_boolean, 'output intermediate information', false,output)
).

%%%%%%%%%%%%
% determine whether ProbLog outputs proofs when adding to trie
% default is false
%%%%%%%%%%%%

:- initialization(
	problog_define_flag(show_proofs,     problog_flag_validate_boolean, 'output proofs', false,output)
).

%%%%%%%%%%%%
% Trie dump parameter for saving a file with the trie structure in the directory by problog_flag dir
%%%%%%%%%%%%

:- initialization(
	problog_define_flag(triedump,        problog_flag_validate_boolean, 'generate file: trie_file containing the trie structure', false,output)
).

%%%%%%%%%%%%
% Default inference method
%%%%%%%%%%%%

:- initialization(problog_define_flag(inference,        problog_flag_validate_dummy, 'default inference method', exact, inference)).

%%%%%%%%%%%%
% Tunable Facts
%%%%%%%%%%%%

:- initialization(problog_define_flag(tunable_fact_start_value,problog_flag_validate_dummy,'How to initialize tunable probabilities',uniform(0.1,0.9),learning_general,flags:learning_prob_init_handler)).



problog_dir(PD):- problog_path(PD).

%%%%%%%%%%%%%%%%%%%%%%%%
% initialization of global parameters
%%%%%%%%%%%%%%%%%%%%%%%%

init_global_params :-
% vsc: removed this, it is major league weird...
%	grow_atom_table(1000000), % this will reserve us some memory, there are cases where you might need more

  %%%%%%%%%%%%
  % working directory: all the temporary and output files will be located there
  % it assumes a subdirectory of the current working dir
  % on initialization, the current dir is the one where the user's file is located
  % should be changed to use temporary folder structure of operating system
  %%%%%%%%%%%%
  tmpnam(TempFolder),
  atomic_concat([TempFolder, '_problog'], TempProblogFolder),
  problog_define_flag(dir, problog_flag_validate_directory, 'directory for files', TempProblogFolder, output),
  problog_define_flag(bdd_par_file,    problog_flag_validate_file, 'file for BDD variable parameters', example_bdd_probs, bdd, flags:working_file_handler),
  problog_define_flag(bdd_result,      problog_flag_validate_file, 'file to store result calculated from BDD', example_bdd_res, bdd, flags:working_file_handler),
  problog_define_flag(bdd_file,        problog_flag_validate_file, 'file for BDD script', example_bdd, bdd, flags:bdd_file_handler),
  problog_define_flag(static_order_file,    problog_flag_validate_file, 'file for BDD static order', example_bdd_order, bdd, flags:working_file_handler),
  problog_define_flag(map_file,        problog_flag_validate_file,    'the file to output the variable map', map_file, output, flags:working_file_handler),
%%%%%%%%%%%%
% montecarlo: recalculate current approximation after N samples
% montecarlo: write log to this file
%%%%%%%%%%%%
  problog_define_flag(mc_logfile,      problog_flag_validate_file, 'logfile for montecarlo', 'log.txt', mcmc, flags:working_file_handler),
  check_existance('simplecudd').

% parameter initialization to be called after returning to user's directory:
:- initialization(init_global_params).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% internal control flags
% if on
% - up: collect stopped derivations to build upper bound
% - limit: iterative deepening reached limit -> should go to next level
% - mc: using problog_montecarlo, i.e. proving with current sample instead of full program
% - remember: save BDD files containing script, params and mapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
problog_control(on,X) :-
	call(X),!.
problog_control(on,X) :-
	assertz(X).
problog_control(off,X) :-
	retractall(X).
problog_control(check,X) :-
	call(X).

reset_control :-
	problog_control(off,up),
	problog_control(off,mc),
	problog_control(off,limit),
%   problog_control(off,exact),
	problog_control(off,remember),
    nb_setval(problog_steps, 1).

:- initialization(reset_control).

grow_atom_table(N):-
	generate_atoms(N, 0),
	garbage_collect_atoms.
generate_atoms(N, N):-!.
generate_atoms(N, A):-
	NA is A + 1,
	atomic_concat([theo, A], _Atom),
	generate_atoms(N, NA).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nice user syntax Prob::Fact
% automatic translation to internal hardware access format
%
% probabilities =1 are dropped -> normal Prolog fact
%
% internal fact representation
% - prefixes predicate name with problog_
% - adds unique ID as first argument
% - adds logarithm of probability as last argument
% - keeps original arguments in between
%
% for each predicate appearing as probabilistic fact, wrapper clause is introduced:
% - head is most general instance of original fact
% - body is corresponding version of internal fact plus call to add_to_proof/2 to update current state during proving
% example: edge(A,B) :- problog_edge(ID,A,B,LogProb), add_to_proof(ID,LogProb).
%
% dynamic predicate problog_predicate(Name,Arity) keeps track of predicates that already have wrapper clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% converts annotated disjunctions
term_expansion_intern((Head<--Body), Module, C):-
	term_expansion_intern_ad((Head<--Body), Module,inference, C).
% converts ?:: prefix to ? :: infix, as handled by other clause
term_expansion_intern((Annotation::Fact), Module, ExpandedClause) :-
	Annotation == ( '?' ),
	term_expansion_intern(((?) :: Fact :- true), Module, ExpandedClause).
term_expansion_intern((Annotation::Head; Alternatives), Module, C):-
    is_alternatives( Alternatives ),
    !,
	term_expansion_intern_ad(((Annotation::Head; Alternatives)<--true), Module,inference, C).

% handles decision clauses
term_expansion_intern((Annotation :: Head :- Body), Module, problog:ExpandedClause) :-
	(
	 Annotation == ('?') ->
				% It's a decision with a body
	 (decision_fact(_,Head) ->
	  throw(error('New decision unifies with already defined decision!', (Head))) ; true
	 ),
	 copy_term((Head,Body),(HeadCopy,_BodyCopy)),
	 functor(Head, Functor, Arity),
	 atomic_concat([problog_,Functor],LongFunctor),
	 Head =.. [Functor|Args],
	 append(Args,[LProb],LongArgs),
	 probclause_id(ID),
	 ProbFactHead =.. [LongFunctor,ID|LongArgs],
	 assertz(decision_fact(ID,Head)),
	 ExpandedClause = (ProbFactHead :-
			  user:Body,
			   (problog_control(check,internal_strategy) ->
			    dtproblog:strategy_log(ID,Head,LProb)
			   ;
			    LProb = ('?')
			   )
			  ),
	 assertz(dynamic_probability_fact(ID)),
	 assertz((dynamic_probability_fact_extract(HeadCopy,P_New) :-
		dtproblog:strategy(ID,HeadCopy,P_New)
		)),
	 (ground(Head) ->
	  true
	 ;
	  assertz(non_ground_fact(ID))
	 ),
	 problog_predicate(Functor, Arity, LongFunctor, Module)
	;
				% If it has a body, it's not supported
	 (Body == true ->
				% format('Expanding annotated fact ~q :: ~q :- ~q in other clause.~n',[Annotation,Head,Body]),
	  fail
	 ;
	  throw(error('Please use an annoted disjunction P :: Head <-- Body instead of the annated clause.', (Annotation :: Head :- Body)))
	 )
	).




% handles continuous facts
term_expansion_intern(Head :: Goal,Module,problog:ProbFact) :-
	nonvar(Head),
	Head=(X,Distribution),
	!,
	(
	 Distribution=gaussian(Mu,Sigma)
	->
	 true;
	 ( throw(unknown_distribution)
	 )
	),

	 (
	  variable_in_term_exactly_once(Goal,X)
	 ->
	  true;
	  (
	   throw(variable)
	  )
	 ),

	% bind_the_variable
	X=Distribution,

	% find position in term
	Goal=..[Name|Args],
	once(nth1(Pos,Args,Distribution)),

	length(Args,Arity),
	atomic_concat([problogcontinuous_,Name],ProblogName),
	probclause_id(ID),

	% is it a tunable fact?
	(
	 (number(Mu),number(Sigma))
	->
	 NewArgs=Args;
	 (
	  Mu_Random is 0.1, % random*4-2,
	  Sigma_Random is 0.4, % random*2+0.5,
	  nth1(Pos,Args,_,KeepArgs),
	  nth1(Pos,NewArgs,gaussian(Mu_Random,Sigma_Random),KeepArgs),
	  assertz(tunable_fact(ID,gaussian(Mu,Sigma)))
	 )
	),
	ProbFact =.. [ProblogName,ID|NewArgs],

	(
	 ground(Goal)
	->
	 true;
	 assertz(non_ground_fact(ID))
	),
	assertz(continuous_fact(ID)),
	problog_continuous_predicate(Name, Arity, Pos,ProblogName,Module).





% handles probabilistic facts
term_expansion_intern(P :: Goal,Module,problog:ProbFact) :-
    copy_term((P,Goal),(P_Copy,Goal_Copy)),
	functor(Goal, Name, Arity),
	atomic_concat([problog_,Name],ProblogName),
	Goal =.. [Name|Args],
	append(Args,[LProb],L1),
	probclause_id(ID),
	ProbFact =.. [ProblogName,ID|L1],
	(
	 (nonvar(P), P = t(TrueProb))
	->
	 (
	  assertz(tunable_fact(ID,TrueProb)),
	  sample_initial_value_for_tunable_fact(Goal,LProb)
	 );
	 (
	  ground(P)
	 ->
	  EvalP is P, % allows one to use ground arithmetic expressions as probabilities
	  LProb is log(P),
	  assert_static(prob_for_id(ID,EvalP,LProb)); % Prob is fixed -- assert it for quick retrieval
	  (
				% Probability is a variable... check wether it appears in the term
	   (
	    variable_in_term(Goal,P)
	   ->
	    true;
	    (
	     format(user_error,'If you use probabilisitic facts with a variable as probabilility, the variable has to appear inside the fact.~n',[]),
	     format(user_error,'You used ~q in your program.~2n',[P::Goal]),
	     throw(non_ground_fact_error(P::Goal))
	    )
	   ),
	   LProb=log(P),
	   assertz(dynamic_probability_fact(ID)),
	   assertz(dynamic_probability_fact_extract(Goal_Copy,P_Copy))
	  )
	 )
	),
	(
	 ground(Goal)
	->
	 true;
	 assertz(non_ground_fact(ID))
	),
	problog_predicate(Name, Arity, ProblogName,Module).


sample_initial_value_for_tunable_fact(Goal,LogP) :-
	problog_flag(tunable_fact_start_value,Initializer),

	(
	 Initializer=uniform(Low,High)
	->
	 (
	  Spread is High-Low,
	  random(Rand),
	  P1 is Rand*Spread+Low,

	  % security check, to avoid log(0)
	  (
	   P1>0
	  ->
	   P=P1;
	   P=0.5
	  )
	 );
	 (
	  number(Initializer)
	 ->
	  P=Initializer
         ;
	  atom(Initializer)
         ->
          call(user:Initializer,Goal,P)
         ;
	  throw(unkown_probability_initializer(Initializer))
	 )
	),

	LogP is log(P).

is_alternatives( Var ) :-
	var( Var ),
	!,
	fail.
is_alternatives( _Prob::_Alt ).
is_alternatives( ( A1 ; As ) ) :-
	is_alternatives( A1 ),
	is_alternatives( As ).


%


% introduce wrapper clause if predicate seen first time
problog_continuous_predicate(Name, Arity,ContinuousArgumentPosition,_,_) :-
	problog_continuous_predicate(Name, Arity,OldContinuousArgumentPosition),
	!,
	(
	 ContinuousArgumentPosition=OldContinuousArgumentPosition
	->
	 true;
	 (
	  format(user_error,'Continuous predicates of the same name and arity must ',[]),
	  format(user_error,'have the continuous argument all at the same position.~n',[]),
	  format(user_error,'Your program contains the predicate ~q/~q. There are ',[]),
	  format(user_error,'atoms which have the continuous argument at position ',[]),
	  format(user_error,'~q and other have it at ~q.',[Name,Arity,OldContinuousArgumentPosition,ContinuousArgumentPosition]),
	  throw(continuous_argument(not_unique_position))
	 )
	).
problog_continuous_predicate(Name, Arity, ContinuousArgumentPosition, ProblogName,Module) :-

	LBefore is ContinuousArgumentPosition-1,
	LAfter is Arity-ContinuousArgumentPosition,

	length(ArgsBefore,LBefore),
	length(ArgsAfter,LAfter),
	append(ArgsBefore,[(ID,ID2,GaussianArg)|ArgsAfter],Args),
	append(ArgsBefore,[GaussianArg|ArgsAfter],ProbArgs),

	OriginalGoal =.. [Name|Args],


	ProbFact =.. [ProblogName,ID|ProbArgs],

	assertz( (Module:OriginalGoal :- ProbFact,
		                   % continuous facts always get a grounding ID, even when they are actually ground
		                   % this simplifies the BDD script generation
		                     non_ground_fact_grounding_id(ProbFact,Ground_ID),
		                     atomic_concat([ID,'_',Ground_ID],ID2),
		                     add_continuous_to_proof(ID,ID2)
		 )),

	assertz(problog_continuous_predicate(Name, Arity,ContinuousArgumentPosition)),
	ArityPlus1 is Arity+1,
	dynamic(problog:ProblogName/ArityPlus1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predicates for the user to manipulate continuous facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

in_interval(ID,Low,High) :-
	var(ID),
	throw(error(instantiation_error,in_interval(ID,Low,High))).
in_interval(ID,Low,High) :-
	var(Low),
	throw(error(instantiation_error,in_interval(ID,Low,High))).
in_interval(ID,Low,High) :-
	var(High),
	throw(error(instantiation_error,in_interval(ID,Low,High))).
in_interval(ID,Low,High) :-
	\+ number(Low),
	throw(error(type_error(number,Low),in_interval(ID,Low,High))).
in_interval(ID,Low,High) :-
	\+ number(High),
	throw(error(type_error(number,High),in_interval(ID,Low,High))).
in_interval(ID,Low,High) :-
	Low<High,
	interval_merge(ID,interval(Low,High)).


below(ID,X) :-
	var(ID),
	throw(error(instantiation_error,below(ID,X))).
below(ID,X) :-
	var(X),
	throw(error(instantiation_error,below(ID,X))).
below(ID,X) :-
	\+ number(X),
	throw(error(type_error(number,X),below(ID,X))).
below(ID,X) :-
	interval_merge(ID,below(X)).

above(ID,X) :-
	var(ID),
	throw(error(instantiation_error,above(ID,X))).
above(ID,X) :-
	var(X),
	throw(error(instantiation_error,above(ID,X))).
above(ID,X) :-
	\+ number(X),
	throw(error(type_error(number,X),above(ID,X))).
above(ID,X) :-
	interval_merge(ID,above(X)).


interval_merge((_ID,GroundID,_Type),Interval) :-
	atomic_concat([interval,'_',GroundID],Key),
	b_getval(Key,OldInterval),
	intervals_merge(OldInterval,Interval,NewInterval),
	NewInterval \= none,
	NewInterval \= interval(Bound,Bound),
	b_setval(Key,NewInterval).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% assert/retract for probabilistic facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_assert(P::Goal) :-
	problog_assert(user,P::Goal).
problog_assert(Module, P::Goal) :-
	term_expansion_intern(P::Goal,Module,problog:ProbFact),
	assertz(problog:ProbFact).

problog_retractall(Goal) :-
	Goal =.. [F|Args],
	append([_ID|Args],[_Prob],Args2),
	atomic_concat(['problog_',F],F2),
	ProbLogGoal=..[F2|Args2],
	retractall(problog:ProbLogGoal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% introduce wrapper clause if predicate seen first time
problog_predicate(Name, Arity, _,_) :-
	problog_predicate(Name, Arity), !.

problog_predicate(Name, Arity, ProblogName,Mod) :-
	functor(OriginalGoal, Name, Arity),
	OriginalGoal =.. [_|Args],
	append(Args,[Prob],L1),
	ProbFact =.. [ProblogName,ID|L1],
	assertz( (Mod:OriginalGoal :-
                ProbFact,
                grounding_id(ID,OriginalGoal,ID2),
				prove_problog_fact(ID,ID2,Prob)
		 )),

	assertz( (Mod:problog_not(OriginalGoal) :-
                ProbFact,
                grounding_id(ID,OriginalGoal,ID2),
                prove_problog_fact_negated(ID,ID2,Prob)
		 )),
	assertz(problog_predicate(Name, Arity)),
	ArityPlus2 is Arity+2,
	dynamic(problog:ProblogName/ArityPlus2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generating and storing the grounding IDs for
% non-ground probabilistic facts
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile(user:problog_user_ground/1).
user:problog_user_ground(Goal) :-
  ground(Goal).

non_ground_fact_grounding_id(Goal,ID) :-
  user:problog_user_ground(Goal), !,
	(grounding_is_known(Goal,ID) ->
	  true
	;
	  (
	  nb_getval(non_ground_fact_grounding_id_counter,ID),
	  ID2 is ID+1,
	  nb_setval(non_ground_fact_grounding_id_counter,ID2),
	  assertz(grounding_is_known(Goal,ID))
	  )
	).
non_ground_fact_grounding_id(Goal,_) :-
	format(user_error,'The current program uses non-ground facts.~n', []),
	format(user_error,'If you query those, you may only query fully-grounded versions of the fact.~n',[]),
	format(user_error,'Within the current proof, you queried for ~q which is not ground.~2n', [Goal]),
	throw(error(non_ground_fact(Goal))).

reset_non_ground_facts :-
	required(keep_ground_ids),
	!.
reset_non_ground_facts :-
	nb_setval(non_ground_fact_grounding_id_counter,0),
	retractall(grounding_is_known(_,_)).

:- initialization(reset_non_ground_facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Getting the ID for any kind of ground fact
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grounding_id(ID,Goal,ID2) :-
    (non_ground_fact(ID)->
        non_ground_fact_grounding_id(Goal,G_ID),
        atomic_concat([ID,'_',G_ID],ID2)
    ;
        ID2=ID
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% What to do when prolog tries to prove a problog fact
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prove_problog_fact(ClauseID,GroundID,Prob) :-
  (problog_control(check,find_decisions) ->
    signal_decision(ClauseID,GroundID)
  ;
    (Prob == ('?') ->
      add_to_proof(GroundID,0) % 0 is log(1)!
    ;
      % Checks needed for LeDTProbLog
      (Prob == always ->
        % Always true, do not add to trie
        true
      ;
        (Prob == never ->
          % Always false, do not add to trie
          fail
        ;
          % something in between, add to proof
          ProbEval is Prob,
          add_to_proof(GroundID,ProbEval)
        )
      )
    )
  ).

prove_problog_fact_negated(ClauseID,GroundID,Prob) :-
  (problog_control(check,find_decisions) ->
      signal_decision(ClauseID,GroundID)
  ;
      (Prob = ('?') ->
        add_to_proof_negated(GroundID,-inf) % 0 is log(1)!
      ;
        % Checks needed for LeDTProbLog
        (Prob = always ->
          % Always true, do not add to trie
          fail
        ;
          (Prob = never ->
            % Always false, do not add to trie
            true
          ;
            % something in between, add to proof
            ProbEval is Prob,
            add_to_proof_negated(GroundID,ProbEval)
          )
        )
      )
  ).

% generate next global identifier
:- initialization(nb_setval(probclause_counter,0)).

probclause_id(ID) :-
	nb_getval(probclause_counter,ID), !,
	C1 is ID+1,
	nb_setval(probclause_counter,C1), !.



% backtrack over all probabilistic facts
% must come before term_expansion
Prob::Goal :-
    probabilistic_fact(Prob,Goal,_ID).

(V,Distribution)::Goal :-
	continuous_fact((V,Distribution),Goal,_ID).

% backtrack over all probabilistic facts
probabilistic_fact(Prob,Goal,ID) :-
	ground(Goal),
	!,
	Goal =.. [F|Args],
	atomic_concat('problog_',F,F2),
	append([ID|Args],[LProb],Args2),
	Goal2 =..[F2|Args2],
	length(Args2,N),
	current_predicate(F2/N),
	Goal2,
	number(LProb),
	Prob is exp(LProb).
probabilistic_fact(Prob,Goal,ID) :-
	get_internal_fact(ID,ProblogTerm,_ProblogName,_ProblogArity),
	ProblogTerm =.. [F,_ID|Args],
	append(Args2,[LProb],Args),
	name(F,[_p,_r,_o,_b,_l,_o,_g,_|F2Chars]),
	name(F2,F2Chars),
	Goal =.. [F2|Args2],
	(
	 dynamic_probability_fact(ID)
	->
	 Prob=p;
	 Prob is exp(LProb)
	).

continuous_fact((V,Distribution),Goal,ID) :-
	get_internal_continuous_fact(ID,ProblogTerm,ProblogName,_ProblogArity,ContinuousPos),

	% strip away problog_continuous
	ProblogTerm=..[ProblogName,ID|Arguments],
	nth1(ContinuousPos,Arguments,Distribution,Rest),
	nth1(ContinuousPos,Arguments2,V,Rest),
	atomic_concat(problogcontinuous_,Name,ProblogName),

	% Build final term
	Goal=..[Name|Arguments2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% proof_id(-ID) generates a new ID for a proof
% reset_proof_id resets the ID counter to 0
%
% this ID is used by Hybrid ProbLog to identify proofs
% and later for disjoining them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proof_id(ID) :-
	nb_getval(problog_proof_id,ID),
	ID2 is ID+1,
	nb_setval(problog_proof_id,ID2).

reset_proof_id :-
	nb_setval(problog_proof_id,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% access/update the probability of ID's fact
% hardware-access version: naively scan all problog-predicates (except if prob is recorded in static database),
% cut choice points if ID is ground (they'll all fail as ID is unique),
% but not if it isn't (used to iterate over all facts when writing out probabilities for learning)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using a dummy for the static prob database is more efficient than checking for current_predicate
prob_for_id(dummy,dummy,dummy).

get_fact_probability(A, Prob) :-
  ground(A),
  \+ number(A),
  name(A, A_Codes),
  once(append(Part1, [95|Part2], A_Codes)), % 95 = '_'
  number_codes(ID, Part1), !,
  % let's check whether Part2 contains an 'l' (l=low)
  \+ memberchk(108,Part2),
  number_codes(Grounding_ID, Part2),
  (
   dynamic_probability_fact(ID)
  ->
   grounding_is_known(Goal, Grounding_ID),
   dynamic_probability_fact_extract(Goal, Prob)
  ;
   get_fact_probability(ID, Prob)
  ),
  !.
get_fact_probability(ID,Prob) :-
  ground(ID),
  prob_for_id(ID,Prob,_),
  !.
get_fact_probability(ID,Prob) :-
  (
  ground(ID) ->
    get_internal_fact(ID,ProblogTerm,_ProblogName,ProblogArity),!
  ;
    get_internal_fact(ID,ProblogTerm,_ProblogName,ProblogArity)
  ),
  arg(ProblogArity,ProblogTerm,Log),
  (Log = ('?') ->
      throw(error('Why do you want to know the probability of a decision?')) %fail
  ; ground(Log) ->
      Prob is exp(Log)
  ;
    Prob = p
  ).

get_fact_log_probability(ID,Prob) :-
	ground(ID),
	prob_for_id(ID,_,Prob),!.
get_fact_log_probability(ID,Prob) :-
  (
  ground(ID) ->
    get_internal_fact(ID,ProblogTerm,_ProblogName,ProblogArity),!
  ;
    get_internal_fact(ID,ProblogTerm,_ProblogName,ProblogArity)
  ),
  arg(ProblogArity,ProblogTerm,Prob),
  Prob \== ('?').
get_fact_log_probability(ID,Prob) :-
	get_fact_probability(ID,Prob1),
	Prob is log(Prob1).

set_fact_probability(ID,Prob) :-
	get_internal_fact(ID,ProblogTerm,ProblogName,ProblogArity),
	retract(ProblogTerm),
	ProblogTerm =.. [ProblogName|ProblogTermArgs],
	nth1(ProblogArity,ProblogTermArgs,_,KeepArgs),
	(isnan(Prob) -> NewLogProb = 0.0 ; NewLogProb is log(Prob)),
	nth1(ProblogArity,NewProblogTermArgs,NewLogProb,KeepArgs),
	NewProblogTerm =.. [ProblogName|NewProblogTermArgs],
	assertz(NewProblogTerm).

get_internal_fact(ID,ProblogTerm,ProblogName,ProblogArity) :-
	problog_predicate(Name,Arity),
	atomic_concat([problog_,Name],ProblogName),
	ProblogArity is Arity+2,
	functor(ProblogTerm,ProblogName,ProblogArity),
	arg(1,ProblogTerm,ID),
	call(ProblogTerm).

get_continuous_fact_parameters(ID,Parameters) :-
	(
	ground(ID) ->
		get_internal_continuous_fact(ID,ProblogTerm,_ProblogName,ProblogArity,ContinuousPos),!
	;
	get_internal_continuous_fact(ID,ProblogTerm,_ProblogName,ProblogArity,ContinuousPos)
	),
	InternalPos is ContinuousPos+1,
	arg(InternalPos,ProblogTerm,Parameters).

get_internal_continuous_fact(ID,ProblogTerm,ProblogName,ProblogArity,ContinuousPos) :-
	problog_continuous_predicate(Name,Arity,ContinuousPos),
	atomic_concat([problogcontinuous_,Name],ProblogName),
	ProblogArity is Arity+1,
	functor(ProblogTerm,ProblogName,ProblogArity),
	arg(1,ProblogTerm,ID),
	call(ProblogTerm).

set_continuous_fact_parameters(ID,Parameters) :-
	get_internal_continuous_fact(ID,ProblogTerm,ProblogName,_ProblogArity,ContinuousPos),
	retract(ProblogTerm),
	ProblogTerm =.. [ProblogName|ProblogTermArgs],
	nth0(ContinuousPos,ProblogTermArgs,_,KeepArgs),
	nth0(ContinuousPos,NewProblogTermArgs,Parameters,KeepArgs),
	NewProblogTerm =.. [ProblogName|NewProblogTermArgs],
	assertz(NewProblogTerm).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writing all probabilistic and continuous facts to Filename
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_facts(Filename) :-
	open(Filename,'write',Handle),

	%compiled ADs
	forall((current_predicate(user:ad_intern/3),user:ad_intern(Original,ID,Facts)),
	       print_ad_intern(Handle,Original,ID,Facts)
	       ),

	nl(Handle),

	% probabilistic facts
	% but comment out auxiliary facts stemmig from
	% compiled ADs
	forall(P::Goal,
	       (
		is_mvs_aux_fact(Goal)
	       ->
		format(Handle,'%  ~10f :: ~q.~n',[P,Goal]);
		format(Handle,'~10f :: ~q.~n',[P,Goal])
	       )
	      ),

	nl(Handle),

	% continuous facts (Hybrid ProbLog)
	forall(continuous_fact(ID),
	       (
		get_continuous_fact_parameters(ID,Param),
		format(Handle,'~q.  % ~q~n',[Param,ID])
	       )
	      ),

	close(Handle).


is_mvs_aux_fact(A) :-
	functor(A,B,_),
	atom_concat(mvs_fact_,_,B).

% code for printing the compiled ADs
print_ad_intern(Handle,(Head<--Body),_ID,Facts) :-
	print_ad_intern(Head,Facts,0.0,Handle),
	format(Handle,' <-- ~q.~n',[Body]).
print_ad_intern((A1;B1),[A2|B2],Mass,Handle) :-
	once(print_ad_intern_one(A1,A2,Mass,NewMass,Handle)),
	format(Handle,'; ',[]),
	print_ad_intern(B1,B2,NewMass,Handle).
print_ad_intern(_::Fact,[],Mass,Handle) :-
	P2 is 1.0 - Mass,
	format(Handle,'~10f :: ~q',[P2,Fact]).
print_ad_intern(P::A1,[A2],Mass,Handle) :-
	once(print_ad_intern_one(P::A1,A2,Mass,_NewMass,Handle)).
print_ad_intern_one(_::Fact,_::AuxFact,Mass,NewMass,Handle) :-
	% ask problog to get the fact_id
	once(probabilistic_fact(P,AuxFact,_FactID)),
	P2 is P * (1-Mass),
	NewMass is Mass+P2,
	format(Handle,'~10f :: ~q',[P2,Fact]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recover fact for given id
% list version not exported (yet?)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ID of ground fact
get_fact(ID,OutsideTerm) :-
	get_internal_fact(ID,ProblogTerm,ProblogName,ProblogArity),
	!,
	ProblogTerm =.. [_Functor,ID|Args],
	atomic_concat('problog_',OutsideFunctor,ProblogName),
	Last is ProblogArity-1,
	nth1(Last,Args,_LogProb,OutsideArgs),
	OutsideTerm =.. [OutsideFunctor|OutsideArgs].
% ID of instance of non-ground fact: get fact from grounding table
get_fact(ID,OutsideTerm) :-
	recover_grounding_id(ID,GID),
	grounding_is_known(OutsideTerm,GID).

recover_grounding_id(Atom,ID) :-
	name(Atom,List),
	reverse(List,Rev),
	recover_number(Rev,NumRev),
	reverse(NumRev,Num),
	name(ID,Num).
recover_number([95|_],[]) :- !.  % name('_',[95])
recover_number([A|B],[A|C]) :-
	recover_number(B,C).


get_fact_list([],[]).
get_fact_list([ID|IDs],[Fact|Facts]) :-
	(ID=not(X) -> Fact=not(Y); Fact=Y, ID=X),
	get_fact(X,Y),
	get_fact_list(IDs,Facts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ProbLog inference, core methods
%
% state of proving saved in two backtrackable global variables
% - problog_current_proof holds list of IDs of clauses used
% - problog_probability holds the sum of their log probabilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% called "inside" probabilistic facts to update current state of proving
% if number of steps exceeded, fail
% if fact used before, succeed and keep status as is
% if not prunable, calculate probability and
%    if threshold exceeded, add stopped derivation to upper bound and fail
%       else update state and succeed
%
% do not maintain gloabl variables in montecarlo mode
add_to_proof(ID, _LogProb) :-
	problog_control(check, mc),
	!,
	montecarlo_check(ID).
add_to_proof(ID, LogProb) :-
	b_getval(problog_steps,MaxSteps),
	MaxSteps>0,
	b_getval(problog_probability, CurrentLogProb),
	nb_getval(problog_threshold, CurrentThreshold),
	b_getval(problog_current_proof, IDs),

	% check whether negation of this fact is already used in proof
	\+ open_end_memberchk(not(ID),IDs),

	(  % check whether this fact is already used in proof
	   open_end_memberchk(ID, IDs)
	->
	   true;
	   (
	    open_end_add(ID, IDs, NIDs),
	    NewLogProb is CurrentLogProb+LogProb,
	    (
	     NewLogProb < CurrentThreshold
	    ->
	     (
	      upper_bound(NIDs),
	      fail
	     );
	     (
	      b_setval(problog_probability, NewLogProb),
	      b_setval(problog_current_proof, NIDs)
	     )
	    )
	   )
	),
	Steps is MaxSteps - 1,
	b_setval(problog_steps, Steps).

add_to_proof_negated(ID, _) :-
	problog_control(check, mc),
	!,
	% the sample has to fail if the fact is negated
	\+ montecarlo_check(ID).
add_to_proof_negated(ID, LogProb) :-
	b_getval(problog_steps, MaxSteps),
	MaxSteps>0,
	b_getval(problog_probability, CurrentLogProb),
	nb_getval(problog_threshold, CurrentThreshold),
	b_getval(problog_current_proof, IDs),

	% check whether unnegated fact is already used in proof
	\+ open_end_memberchk(ID, IDs),

	( % check wether negation of this fact is already used in proof
	 open_end_memberchk(not(ID), IDs)
	->
	 true;
	 (
	  open_end_add(not(ID), IDs, NIDs),
	  NewLogProb is CurrentLogProb + log(1-exp(LogProb)),
	  (
	   NewLogProb < CurrentThreshold
	  ->
	   (
	    upper_bound(NIDs),
	    fail
	   );
	   (
	    b_setval(problog_probability, NewLogProb),
	    b_setval(problog_current_proof, NIDs)
	   )
	  )
	 )
	),
	Steps is MaxSteps - 1,
	b_setval(problog_steps, Steps).

%Hybrid
add_continuous_to_proof(ID,GroundID) :-
	b_getval(problog_continuous_facts_used,Facts),
	(
	 memberchk((ID,GroundID),Facts)
	->
	 true;
	 (
	  b_setval(problog_continuous_facts_used,[(ID,GroundID)|Facts]),
	  atomic_concat([interval,'_',GroundID],Key),
	  b_setval(Key,all)
	 )
	).

% if in monte carlo mode ...
% (a) for ground facts (ID is number): check array to see if it can be used
montecarlo_check(ID) :-
	number(ID),
	!,
	array_element(mc_sample,ID,V),
	(
	 V == 1 -> true
	;
	 V == 2 -> fail
	;
	 new_sample(ID)
	).
% (b) for non-ground facts (ID is FactID_GroundingID): check database of groundings in current sample
montecarlo_check(ComposedID) :-
%   split_grounding_id(ComposedID,ID,GID),
  recorded(mc_true,problog_mc_id(ComposedID),_),
  !.
montecarlo_check(ComposedID) :-
%   split_grounding_id(ComposedID,ID,GID),
  recorded(mc_false,problog_mc_id(ComposedID),_),
  !,
  fail.
% (c) for unknown groundings of non-ground facts: generate a new sample (decompose the ID first)
montecarlo_check(ID) :-
	name(ID,IDN),
	recover_number(IDN,FactIDName),
	name(FactID,FactIDName),
	new_sample_nonground(ID,FactID).

% sampling from ground fact: set array value to 1 (in) or 2 (out)
new_sample(ID) :-
	get_fact_probability(ID,Prob),
	problog_random(R),
	R<Prob,
	!,
	update_array(mc_sample,ID,1).
new_sample(ID) :-
	update_array(mc_sample,ID,2),
	fail.

% sampling from ground instance of non-ground fact: set database value for this grounding to true or false
new_sample_nonground(ComposedID,ID) :-
  (dynamic_probability_fact(ID)	->
    get_fact(ID,Fact),
    split_grounding_id(ComposedID,ID,GID),
    grounding_is_known(Fact,GID),
    dynamic_probability_fact_extract(Fact,Prob)
  ;
    get_fact_probability(ID,Prob)
  ),
  problog_random(R),
  (R < Prob ->
    recorda(mc_true,problog_mc_id(ComposedID),_)
  ;
    recorda(mc_false,problog_mc_id(ComposedID),_),
    fail
  ).
% new_sample_nonground(ComposedID,_ID) :-
%   recorda(mc_false,problog_mc_id(ComposedID),_),
%         fail.

split_grounding_id(Composed,Fact,Grounding) :-
	name(Composed,C),
	split_g_id(C,F,G),
	name(Fact,F),
	name(Grounding,G).
split_g_id([95|Grounding],[],Grounding) :- !.
split_g_id([A|B],[A|FactID],GroundingID) :-
	split_g_id(B,FactID,GroundingID).



% if threshold reached, remember this by setting limit to on, then
% if up is on, store stopped derivation in second trie
%
% List always length>=1 -> don't need []=true-case for tries
upper_bound(List) :-
	problog_control(on, limit),
	problog_control(check, up),
	nb_getval(problog_stopped_proofs, Trie_Stopped_Proofs),
	open_end_close_end(List, R),
	% (prune_check(R, Trie_Stopped_Proofs) -> true; insert_ptree(R, Trie_Stopped_Proofs)).
	insert_ptree(R, Trie_Stopped_Proofs).

% this is called by all inference methods before the actual ProbLog goal
% to set up environment for proving
% it resets control flags, method specific values to be set afterwards!
init_problog(Threshold) :-
	reset_proof_id,
	reset_non_ground_facts,
	reset_control,
	LT is log(Threshold),
	b_setval(problog_probability, 0.0),
	nb_setval(problog_current_proof, []),
	nb_setval(problog_threshold, LT),
	problog_flag(maxsteps,MaxS),
	init_tabling,
	problog_var_clear_all,
	b_setval(problog_steps, MaxS),
	b_setval(problog_continuous_facts_used,[]),
	retractall(hybrid_proof(_,_,_)),
	retractall(hybrid_proof(_,_,_,_)),
	retractall(hybrid_proof_disjoint(_,_,_,_)),

	% reset all timers in case a query failed before
	timer_reset(variable_elimination_time),
	timer_reset(bdd_script_time),
	timer_reset(bdd_generation_time),
	timer_reset(script_gen_time_naive),
	timer_reset(bdd_gen_time_naive),
	timer_reset(script_gen_time_builtin),
	timer_reset(bdd_gen_time_builtin),
	timer_reset(script_gen_time_dec),
	timer_reset(bdd_gen_time_dec),
	timer_reset(sld_time),
	timer_reset(build_tree_low).

 :- initialization( ( init_problog(0.0),
                      reset_control ) ).

% idea: proofs that are refinements of known proof can be pruned as they don't add probability mass
% note that current ptree implementation doesn't provide the check as there's no efficient method known so far...
prune_check(Proof, Trie) :-
	problog_flag(prunecheck, on),
	prune_check_ptree(Proof, Trie).

% to call a ProbLog goal, patch all subgoals with the user's module context
% (as logical part is there, but probabilistic part in problog)
problog_call(Goal) :-
	yap_flag(typein_module, Module),
%%% if user provides init_db, call this before proving goal
	(current_predicate(_,Module:init_db) -> call(Module:init_db); true),
	put_module(Goal,Module,ModGoal),
	call(ModGoal).

put_module((Mod:Goal,Rest),Module,(Mod:Goal,Transformed)) :-
	!,
	put_module(Rest,Module,Transformed).
put_module((Goal,Rest),Module,(Module:Goal,Transformed)) :-
	!,
	put_module(Rest,Module,Transformed).
put_module((Mod:Goal),_Module,(Mod:Goal)) :-
	!.
put_module(Goal,Module,Module:Goal).

% end of core

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evaluating a DNF given as trie using BDD
% input: Trie the trie to be used
% output: probability and status (to catch potential failures/timeouts from outside)
%
% with internal BDD timeout (set using problog flag bdd_time)
%
% bdd_ptree/3 constructs files for problogbdd from the trie
%
% if calling ProblogBDD doesn't exit successfully, status will be timeout
%
% writes number of proofs in trie and BDD time to standard user output
%
% if remember is on, input files for problogbdd will be saved
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization((
	problog_var_define(sld_time, times, time, messages('SLD resolution', ':', ' ms')),
	problog_var_define(bdd_script_time, times, time, messages('Generating BDD script', ':', ' ms')),
	problog_var_define(bdd_generation_time, times, time, messages('Constructing BDD', ':', ' ms')),
	problog_var_define(trie_statistics, memory, untyped, messages('Trie usage', ':', '')),
	problog_var_define(probability, result, number, messages('Probabilty', ' = ', '')),
	problog_var_define(bdd_script_time(Method), times, time, messages('Generating BDD script '(Method), ':', ' ms')),
	problog_var_define(bdd_generation_time(Method), times, time, messages('Constructing BDD '(Method), ':', ' ms')),
	problog_var_define(probability(Method), result, number, messages('Probabilty '(Method), ' = ', '')),
	problog_var_define(trie_statistics(Method), memory, untyped, messages('Trie usage '(Method), ':', '')),
	problog_var_define(dbtrie_statistics(Method), memory, untyped, messages('Depth Breadth Trie usage '(Method), ':', '')),
	problog_var_define(db_trie_opts_performed(Method), memory, untyped, messages('Optimisations performed '(Method), ':', '')),
	problog_var_define(variable_elimination_time, times, time, messages('Variable Elimination', ':', ' ms')),
	problog_var_define(variable_elimination_stats, memory, untyped, messages('Variable Elimination', ':', ''))
)).

problog_statistics(Stat, Result):-
	problog_var_defined(Stat),
	problog_var_is_set(Stat),
	problog_var_get(Stat, Result).

generate_order_by_prob_fact_appearance(Order, FileName):-
	open(FileName, 'write', Stream),
	forall(member(PF, Order), (
				   ptree:get_var_name(PF, Name),
				   format(Stream, "@~w~n", [Name])
				  )),
	close(Stream).

get_order(Trie, Order):-
	findall(List, ptree:traverse_ptree(Trie, List), Proofs),
	flatten(Proofs, ProbFacts),
	remove_duplicates(ProbFacts, Order).


eval_dnf(OriTrie1, Prob, Status) :-
   % Check whether we use Hybrid ProbLog
   (
    hybrid_proof(_,_,_)
   ->
    ( % Yes! run the disjoining stuff
      	retractall(hybrid_proof_disjoint(_,_,_,_)),
	disjoin_hybrid_proofs,

	init_ptree(OriTrie),  % use this as tmp ptree
	%%%%%%%%%%%%%%%%%%%%%
	( % go over all stored proofs
	  enum_member_ptree(List,OriTrie1),
	  (
	   List=[_|_]
	  ->
	   Proof=List;
	   Proof=[List]
	  ),
	  (
	   select(continuous(ProofID),Proof,Rest)
	  ->
	   (
				% this proof is using continuous facts
	    all_hybrid_subproofs(ProofID,List2),
	    append(Rest,List2,NewProof),
	    insert_ptree(NewProof,OriTrie)
	    );
	   insert_ptree(Proof,OriTrie)
	  ),

	  fail;
	  true
	)
        %%%%%%%%%%%%%%%%%%%%%
    ) ;
     % Nope, just pass on the Trie
    OriTrie=OriTrie1
   ),


  ((problog_flag(variable_elimination, true), nb_getval(problog_nested_tries, false)) ->
    timer_start(variable_elimination_time),
    trie_check_for_and_cluster(OriTrie),
    timer_stop(variable_elimination_time,Variable_Elimination_Time),
    problog_var_set(variable_elimination_time, Variable_Elimination_Time),
    trie_replace_and_cluster(OriTrie, Trie),
    variable_elimination_stats(Clusters, OrigPF, CompPF),
    problog_var_set(variable_elimination_stats, compress(Clusters, OrigPF, CompPF)),
    clean_up
  ;
    Trie = OriTrie
  ),
  (problog_flag(bdd_static_order, true) ->
    get_order(Trie, Order),
    problog_flag(static_order_file, SOFName),
    convert_filename_to_working_path(SOFName, SOFileName),
    generate_order_by_prob_fact_appearance(Order, SOFileName)
  ;
    true
  ),
  ptree:trie_stats(Memory, Tries, Entries, Nodes),
  (nb_getval(problog_nested_tries, false) ->
    ptree:trie_usage(Trie, TEntries, TNodes, TVirtualNodes),
    problog_var_set(trie_statistics, tries(memory(Memory), tries(Tries), entries(TEntries), nodes(TNodes), virtualnodes(TVirtualNodes)))
  ;
    problog_var_set(trie_statistics, tries(memory(Memory), tries(Tries), entries(Entries), nodes(Nodes)))
  ),
  (problog_flag(triedump, true) ->
    convert_filename_to_working_path(trie_file, TrieFile),
    tell(TrieFile),
    print_nested_ptree(Trie),
    flush_output,
    told,
    tell(user_output)
  ;
    true
  ),
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
  ((Trie = Trie_Completed_Proofs, problog_flag(save_bdd, true)) ->
    problog_control(on, remember)
  ;
    problog_control(off, remember)
  ),
  problog_flag(bdd_file, BDDFileFlag),
  convert_filename_to_working_path(BDDFileFlag, BDDFile),
  problog_flag(bdd_par_file, BDDParFileFlag),
  convert_filename_to_working_path(BDDParFileFlag, BDDParFile),
  % old reduction method doesn't support nested tries
  ((problog_flag(use_old_trie, true), nb_getval(problog_nested_tries, false)) ->
    timer_start(bdd_script_time),
    (problog_control(check, remember) ->
      bdd_ptree_map(Trie, BDDFile, BDDParFile, Mapping),
      convert_filename_to_working_path(save_map, MapFile),
      tell(MapFile),
      format('mapping(~q).~n', [Mapping]),
      flush_output,
      told
    ;
      bdd_ptree(Trie, BDDFile, BDDParFile)
    ),
    timer_stop(bdd_script_time,BDD_Script_Time),
    problog_var_set(bdd_script_time, BDD_Script_Time),

    timer_start(bdd_generation_time),
    execute_bdd_tool(BDDFile, BDDParFile, Prob_old, Status_old),
    timer_stop(bdd_generation_time,BDD_Generation_Time),
    (Status_old == ok ->
      problog_var_set(bdd_generation_time, BDD_Generation_Time),
      problog_var_set(probability, Prob_old)
    ;
      problog_var_set(bdd_generation_time, fail),
      problog_var_set(probability, fail)
    )
  ;
    true
  ),
  % naive method with nested trie support but not loops
  ((problog_flag(use_naive_trie, true); (problog_flag(use_old_trie, true), nb_getval(problog_nested_tries, true))) ->
   timer_start(script_gen_time_naive),
    BDDFile = BDDFile_naive,
    nested_ptree_to_BDD_script(Trie, BDDFile_naive, BDDParFile),
    timer_stop(script_gen_time_naive,Script_Gen_Time_Naive),
    problog_var_set(bdd_script_time(naive), Script_Gen_Time_Naive),

     timer_start(bdd_gen_time_naive),
    execute_bdd_tool(BDDFile_naive, BDDParFile, Prob_naive, Status_naive),
     timer_stop(bdd_gen_time_naive,BDD_Gen_Time_Naive),
    (Status_naive == ok ->
      problog_var_set(bdd_generation_time(naive),BDD_Gen_Time_Naive),
      problog_var_set(probability(naive), Prob_naive)
    ;
      problog_var_set(bdd_generation_time(naive), fail),
      problog_var_set(probability(naive), fail)
    )
  ;
    true
  ),
  % reduction method with depth_breadth trie support
  problog_flag(db_trie_opt_lvl, ROptLevel),
  problog_flag(db_min_prefix, MinPrefix),

  (problog_flag(compare_opt_lvl, true) ->
    generate_ints(0, ROptLevel, Levels)
  ;
    Levels = [ROptLevel]
  ),
  forall(member(OptLevel, Levels), (
    (problog_flag(use_db_trie, true) ->
      tries:trie_db_opt_min_prefix(MinPrefix),
      timer_start(script_gen_time_builtin),
      BDDFile = BDDFile_builtin,
      (nb_getval(problog_nested_tries, false) ->
        trie_to_bdd_trie(Trie, DBTrie, BDDFile_builtin, OptLevel, BDDParFile)
      ;
        nested_trie_to_bdd_trie(Trie, DBTrie, BDDFile_builtin, OptLevel, BDDParFile)
      ),
      atomic_concat(['builtin_', OptLevel], Builtin),
      ptree:trie_stats(DBMemory, DBTries, DBEntries, DBNodes),
      FM is DBMemory - Memory,
      FT is DBTries - Tries,
      FE is DBEntries - Entries,
      FN is DBNodes - Nodes,
      problog_var_set(dbtrie_statistics(Builtin), tries(memory(FM), tries(FT), entries(FE), nodes(FN))),

      delete_ptree(DBTrie),
      timer_stop(script_gen_time_builtin,Script_Gen_Time_Builtin),

      problog_var_set(bdd_script_time(Builtin), Script_Gen_Time_Builtin),

      timer_start(bdd_gen_time_builtin),
      execute_bdd_tool(BDDFile_builtin, BDDParFile, Prob_builtin, Status_builtin),
      timer_stop(bdd_gen_time_builtin,BDD_Gen_Time_Builtin),
      ptree_db_trie_opt_performed(LVL1, LVL2, LV3),
      problog_var_set(db_trie_opts_performed(Builtin), opt_perform(LVL1, LVL2, LV3)),
      (Status_builtin == ok ->
        problog_var_set(bdd_generation_time(Builtin), BDD_Gen_Time_Builtin),
        problog_var_set(probability(Builtin), Prob_builtin)
      ;
        problog_var_set(bdd_generation_time(Builtin), fail),
        problog_var_set(probability(Builtin), fail)
      )
    ;
      true
    )
  )),

  % decomposition method
  (problog_flag(use_dec_trie, true) ->
    BDDFile = BDDFile_dec,
    timer_start(script_gen_time_dec),
    ptree_decomposition(Trie, BDDFile_dec, BDDParFile),
    timer_stop(script_gen_time_dec,Script_Gen_Time_Dec),
    problog_var_set(bdd_script_time(dec), Script_Gen_Time_Dec),

    timer_start(bdd_gen_time_dec),
    execute_bdd_tool(BDDFile_dec, BDDParFile, Prob_dec, Status_dec),
    timer_stop(bdd_gen_time_dec,BDD_Gen_Time_Dec),
    (Status_dec == ok ->
      problog_var_set(bdd_generation_time(dec), BDD_Gen_Time_Dec),
      problog_var_set(probability(dec), Prob_dec)
    ;
      problog_var_set(bdd_generation_time(dec), fail),
      problog_var_set(probability(dec), fail)
    )
  ;
    true
  ),

  (problog_control(check, remember) ->
    convert_filename_to_working_path('save_script', SaveBDDFile),
    copy_file(BDDFile, SaveBDDFile),
    convert_filename_to_working_path('save_params', SaveBDDParFile),
    copy_file(BDDParFile, SaveBDDParFile)
  ;
    true
  ),
  problog_control(off, remember),
  (var(Status_old)->
    (var(Status_naive)->
      (var(Status_dec) ->
        atomic_concat('builtin_', ROptLevel, ProbStat),
        problog_statistics(probability(ProbStat), ProbB),
        (ProbB = fail ->
          Status = timeout
        ;
          Prob = ProbB,
          Status = ok
        )
      ;
        Prob = Prob_dec,
        Status = Status_dec
      )
    ;
      Prob = Prob_naive,
      Status = Status_naive
    )
  ;
    Prob = Prob_old,
    Status = Status_old
  ),

  (Trie =\= OriTrie ->
    delete_ptree(Trie)
  ;
    true
  ).

generate_ints(End, End, [End]).
generate_ints(Start, End, [Start|Rest]):-
  Start < End,
  Current is Start + 1,
  generate_ints(Current, End, Rest).

execute_bdd_tool(BDDFile, BDDParFile, Prob, Status):-
  problog_flag(bdd_time, BDDTime),
  problog_flag(bdd_result, ResultFileFlag),
	(problog_flag(nodedump_bdd,true) ->
		problog_flag(nodedump_file,NodeDumpFile),
    convert_filename_to_working_path(NodeDumpFile, SONodeDumpFile),
		atomic_concat([' -sd ', SONodeDumpFile],ParamB)
	;
		ParamB = ''
	),
  (problog_flag(dynamic_reorder, true) ->
    ParamD = ParamB
  ;
    atomic_concat([ParamB, ' -dreorder'], ParamD)
  ),
  (problog_flag(bdd_static_order, true) ->
    problog_flag(static_order_file, FileName),
    convert_filename_to_working_path(FileName, SOFileName),
    atomic_concat([ParamD, ' -sord ', SOFileName], Param)
  ;
    Param = ParamD
  ),
  convert_filename_to_problog_path('simplecudd', ProblogBDD),
  convert_filename_to_working_path(ResultFileFlag, ResultFile),
  atomic_concat([ProblogBDD, Param,' -l ', BDDFile, ' -i ', BDDParFile, ' -m p -t ', BDDTime, ' > ', ResultFile], Command),
  shell(Command, Return),
  (Return =\= 0 ->
    Status = timeout
  ;
    see(ResultFile),
    read(probability(Prob)),
    seen,
    catch(delete_file(ResultFile),_, fail),
    Status = ok
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% different inference methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% approximate inference: bounds based on single probability threshold
% problog_threshold(+Goal,+Threshold,-LowerBound,-UpperBound,-Status)
%
% use backtracking over problog_call to get all solutions
%
% trie 1 collects proofs, trie 2 collects stopped derivations, trie 3 is used to unit them for the upper bound
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_threshold(Goal, Threshold, _, _, _) :-
	init_problog_threshold(Threshold),
	problog_control(on,up),
	problog_call(Goal),
	add_solution,
	fail.
problog_threshold(_, _, LP, UP, Status) :-
	compute_bounds(LP, UP, Status).

init_problog_threshold(Threshold) :-
	init_ptree(Trie_Completed_Proofs),
	nb_setval(problog_completed_proofs, Trie_Completed_Proofs),
	init_ptree(Trie_Stopped_Proofs),
	nb_setval(problog_stopped_proofs, Trie_Stopped_Proofs),
	init_problog(Threshold).

add_solution :-
	% get the probabilistic facts used in this proof
	b_getval(problog_current_proof, IDs),
	(IDs == [] -> R = []; open_end_close_end(IDs, R)),

	% get the continuous facts used in this proof
	% (Hybrid ProbLog
	b_getval(problog_continuous_facts_used,Cont_IDs),
	(
	 Cont_IDs == []
	->
	 Continuous=[];
	 (
	   proof_id(ProofID),
	   collect_all_intervals(Cont_IDs,ProofID,AllIntervals),
	   (
	    AllIntervals==[]
	   ->
	    Continuous=[];
	    (
	     Continuous=[continuous(ProofID)],
	     assertz(hybrid_proof(ProofID,Cont_IDs,AllIntervals))
	    )
	   )
	 )
	),

	% we have both, no add it to the trie
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	append(R,Continuous,Final),
	(
	 Final==[]
	->
	 insert_ptree(true, Trie_Completed_Proofs);
	 insert_ptree(Final, Trie_Completed_Proofs)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collect_all_intervals([],_,[]).
collect_all_intervals([(ID,GroundID)|T],ProofID,[Interval|T2]) :-
	atomic_concat([interval,'_',GroundID],Key),
	b_getval(Key,Interval),
	Interval \= all,  % we do not need to store continuous
	                  % variables with domain [-oo,oo] (they have probability 1)
	!,
	assertz(hybrid_proof(ProofID,ID,GroundID,Interval)),
	collect_all_intervals(T,ProofID,T2).
collect_all_intervals([_|T],ProofID,T2) :-
	collect_all_intervals(T,ProofID,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


all_hybrid_subproofs(ProofID,List) :-
	findall((ID,GroundID,Intervals),hybrid_proof_disjoint(ProofID,ID,GroundID,Intervals),All),
	generate_all_proof_combinations(All,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_all_proof_combinations([],[]).
generate_all_proof_combinations([(_ID,GroundID,Intervals)|T],Result) :-
	member((Interval,Tail),Intervals),
	intervals_encode(Interval,IntervalEncoded),
	atomic_concat([GroundID,IntervalEncoded],FullID),
	encode_tail(Tail,GroundID,TailEncoded),
	append([FullID|TailEncoded],T2,Result),
	generate_all_proof_combinations(T,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_tail([],_,[]).
encode_tail([A|T],ID,[not(FullID)|T2]) :-
	intervals_encode(A,AEncoded),
	atomic_concat([ID,AEncoded],FullID),
	encode_tail(T,ID,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disjoin_hybrid_proofs :-
	% collect all used continuous facts
	findall(GroundID,hybrid_proof(_,_,GroundID,_),IDs),
	sort(IDs,IDsSorted),

	disjoin_hybrid_proofs(IDsSorted).

disjoin_hybrid_proofs([]).
disjoin_hybrid_proofs([GroundID|T]) :-
	findall(Interval,hybrid_proof(_,_,GroundID,Interval),Intervals),
	intervals_partition(Intervals,Partition),

	% go over all proofs where this fact occurs
	forall(hybrid_proof(ProofID,ID,GroundID,Interval),
	       (
		intervals_disjoin(Interval,Partition,PInterval),
		assertz(hybrid_proof_disjoint(ProofID,ID,GroundID,PInterval))
	       )
	      ),

	disjoin_hybrid_proofs(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% End Hybrid

compute_bounds(LP, UP, Status) :-
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
  nb_getval(problog_stopped_proofs, Trie_Stopped_Proofs),
  eval_dnf(Trie_Completed_Proofs, LP, StatusLow),
  (StatusLow \== ok ->
    Status = StatusLow
  ;
  merge_ptree(Trie_Completed_Proofs, Trie_Stopped_Proofs, Trie_All_Proofs),
  nb_setval(problog_all_proofs, Trie_All_Proofs),
  eval_dnf(Trie_All_Proofs, UP, Status)),
  delete_ptree(Trie_Completed_Proofs),
  delete_ptree(Trie_Stopped_Proofs),
  delete_ptree(Trie_All_Proofs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% approximate inference: lower bound based on all proofs above probability threshold
% problog_low(+Goal,+Threshold,-LowerBound,-Status)
%
% same as problog_threshold/5, but lower bound only (no stopped derivations stored)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_low(Goal/Cond, Threshold, Status, P) :-
    !,
    problog_low((Cond,Goal), Threshold, P1, Status),
    problog_low( Cond, Threshold, P2, Status),
    P is P1/P2.
problog_low(Goal, Threshold, _, _) :-
	init_problog_low(Threshold),
	problog_control(off, up),
	timer_start(sld_time),
	problog_call(Goal),
	add_solution,
	fail.
problog_low(_, _, LP, Status) :-
	timer_stop(sld_time,SLD_Time),
	problog_var_set(sld_time, SLD_Time),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
%print_nested_ptree(Trie_Completed_Proofs),
	eval_dnf(Trie_Completed_Proofs, LP, Status),
	(problog_flag(verbose, true)->
	 problog_statistics
	;
	 true
	),
	delete_ptree(Trie_Completed_Proofs),
	(problog_flag(retain_tables, true) -> retain_tabling; true),
	clear_tabling.

init_problog_low(Threshold) :-
	init_ptree(Trie_Completed_Proofs),
	nb_setval(problog_completed_proofs, Trie_Completed_Proofs),
	init_problog(Threshold).

% generalizing problog_max to return all explanations, sorted by non-increasing probability
problog_all_explanations(Goal,Expl) :-
	problog_all_explanations_unsorted(Goal,Unsorted),
	keysort(Unsorted,Decreasing),
	reverse(Decreasing,Expl).

problog_all_explanations_unsorted(Goal, _) :-
	init_problog_low(0.0),
	problog_control(off, up),
	timer_start(sld_time),
	problog_call(Goal),
	add_solution,
	fail.
problog_all_explanations_unsorted(_,Expl) :-
	timer_stop(sld_time,SLD_Time),
	problog_var_set(sld_time, SLD_Time),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	explanations_from_trie(Trie_Completed_Proofs,Expl).

% catch basecases
explanations_from_trie(Trie,[]) :-
	empty_ptree(Trie),!.
explanations_from_trie(Trie,[1.0-[]]) :-
	traverse_ptree(Trie,[true]),!.
explanations_from_trie(Trie_Completed_Proofs,Expl) :-
	findall(Prob-Facts,
		(traverse_ptree(Trie_Completed_Proofs,L),
		 findall(P,(member(A,L),get_fact_log_probability(A,P)),Ps),
		 sum_list(Ps,LS),
		 Prob is exp(LS),
		 get_fact_list(L,Facts)
		),Expl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% approximate inference: bounds by iterative deepening up to interval width Delta
% problog_delta(+Goal,+Delta,-LowerBound,-UpperBound,-Status)
%
% wraps iterative deepening around problog_threshold, i.e.
% - starts with threshold given by first_threshold flag
% - if Up-Low >= Delta, multiply threshold by factor given in id_stepsize flag and iterate
% (does not use problog_threshold as trie 1 is kept over entire search)
%
% local dynamic predicates low/2, up/2, stopDiff/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_delta(Goal, Delta, Low, Up, Status) :-
	problog_flag(first_threshold,InitT),
	init_problog_delta(InitT,Delta),
	problog_control(on,up),
	problog_delta_id(Goal,Status),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	nb_getval(problog_stopped_proofs, Trie_Stopped_Proofs),
	delete_ptree(Trie_Completed_Proofs),
	delete_ptree(Trie_Stopped_Proofs),
	(retract(low(_,Low)) -> true; true),
	(retract(up(_,Up)) -> true; true).


init_problog_delta(Threshold,Delta) :-
	retractall(low(_,_)),
	retractall(up(_,_)),
	retractall(stopDiff(_)),
	init_ptree(Trie_Completed_Proofs),
	nb_setval(problog_completed_proofs, Trie_Completed_Proofs),
	init_ptree(Trie_Stopped_Proofs),
	nb_setval(problog_stopped_proofs, Trie_Stopped_Proofs),
	assertz(low(0,0.0)),
	assertz(up(0,1.0)),
	assertz(stopDiff(Delta)),
	init_problog(Threshold).

problog_delta_id(Goal, _) :-
	problog_call(Goal),
	add_solution,     % reused from problog_threshold
	fail.
problog_delta_id(Goal, Status) :-
	evaluateStep(Ans,StatusE),
	problog_flag(last_threshold_log,Stop),
	nb_getval(problog_threshold,Min),
	(StatusE \== ok ->
	    Status = StatusE
	;
	(
	    Ans = 1 ->
	    Status = ok
	;
	    Min =<  Stop ->
	    Status = stopreached
	;
	    problog_control(check,limit) ->
	    problog_control(off,limit),
	    problog_flag(id_stepsize_log,Step),
	    New is Min+Step,
	    nb_setval(problog_threshold,New),
	    problog_delta_id(Goal, Status)
	;
	true
	)).

% call the dnf evaluation where needed
evaluateStep(Ans,Status)  :- once(evalStep(Ans,Status)).

evalStep(Ans,Status) :-
	stopDiff(Delta),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	nb_getval(problog_stopped_proofs, Trie_Stopped_Proofs),
	count_ptree(Trie_Completed_Proofs, NProofs),
	count_ptree(Trie_Stopped_Proofs, NCands),
	format_if_verbose(user,'~w proofs, ~w stopped derivations~n',[NProofs,NCands]),
	eval_lower(NProofs,Low,StatusLow),
	(
	 StatusLow \== ok
	->
	 Status = StatusLow;
	 up(_, OUP),
	 IntDiff is OUP-Low,
	 ((IntDiff < Delta; IntDiff =:= 0) ->
      Up = OUP,
      StatusUp = ok
    ;
      eval_upper(NCands, Up, StatusUp),
      delete_ptree(Trie_Stopped_Proofs),
      init_ptree(New_Trie_Stopped_Proofs),
      nb_setval(problog_stopped_proofs, New_Trie_Stopped_Proofs)
    ),
    (StatusUp \== ok ->
      Status = StatusUp
    ;
      Diff is Up-Low,
      format_if_verbose(user,'difference:  ~6f~n',[Diff]),
      ((Diff < Delta; Diff =:= 0) -> Ans = 1; Ans = 0),
      Status = ok
    )
  ).

% no need to re-evaluate if no new proofs found on this level
eval_lower(N,P,ok) :-
	low(N,P).
% evaluate if there are proofs
eval_lower(N,P,Status) :-
	N > 0,
	low(OldN,_),
	N \= OldN,
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	eval_dnf(Trie_Completed_Proofs,P,Status),
	(Status == ok ->
	    retract(low(_,_)),
	    assertz(low(N,P)),
	    format_if_verbose(user,'lower bound: ~6f~n',[P])
	;
	true).

% if no stopped derivations, up=low
eval_upper(0,P,ok) :-
	retractall(up(_,_)),
	low(N,P),
	assertz(up(N,P)).
% else merge proofs and stopped derivations to get upper bound
% in case of timeout or other problems, skip and use bound from last level
eval_upper(N,UpP,ok) :-
  N > 0,
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
  nb_getval(problog_stopped_proofs, Trie_Stopped_Proofs),
  merge_ptree(Trie_Completed_Proofs,Trie_Stopped_Proofs,Trie_All_Proofs),
  nb_setval(problog_all_proofs, Trie_All_Proofs),
  eval_dnf(Trie_All_Proofs,UpP,StatusUp),
  delete_ptree(Trie_All_Proofs),
  (StatusUp == ok ->
    retract(up(_,_)),
    assertz(up(N,UpP))
  ;
     format_if_verbose(user,'~w - continue using old up~n',[StatusUp]),
    up(_,UpP)
  ).

/**
@pred problog_max(+G, -Prob, -FactsUsed)

This predicate returns the most likely explanation of proving the goal G
and the facts used in achieving this explanation.
explanation probability - returns list of facts used or constant 'unprovable' as third argument
problog_max(+Goal,-Prob,-Facts)

uses iterative deepening with same parameters as bounding algorithm
threshold gets adapted whenever better proof is found

uses local dynamic predicates max_probability/1 and max_proof/1
*/

problog_max(Goal, Prob, Facts) :-
	problog_flag(first_threshold,InitT),
	init_problog_max(InitT),
	problog_control(off,up),
	problog_max_id(Goal, Prob, FactIDs),% theo todo
	( FactIDs = [_|_] -> get_fact_list(FactIDs, Facts);
	    Facts = FactIDs).

init_problog_max(Threshold) :-
	retractall(max_probability(_)),
	retractall(max_proof(_)),
	assertz(max_probability(-999999)),
	assertz(max_proof(unprovable)),
	init_problog(Threshold).

update_max :-
  b_getval(problog_probability, CurrP),
  max_probability(MaxP),
  CurrP>MaxP,
  b_getval(problog_current_proof, IDs),
  open_end_close_end(IDs, R),
  retractall(max_proof(_)),
  assertz(max_proof(R)),
  nb_setval(problog_threshold, CurrP),
  retractall(max_probability(_)),
  assertz(max_probability(CurrP)).

problog_max_id(Goal, _Prob, _Clauses) :-
	problog_call(Goal),
	update_max,
	fail.
problog_max_id(Goal, Prob, Clauses) :-
  max_probability(MaxP),
  nb_getval(problog_threshold, LT),
  problog_flag(last_threshold_log, ToSmall),
  ((MaxP >= LT; \+ problog_control(check, limit); LT < ToSmall) ->
    ((max_proof(unprovable), problog_control(check,limit), LT < ToSmall) ->
      problog_flag(last_threshold, Stopping),
      Clauses = unprovable(Stopping)
    ;
      max_proof(Clauses)
    ),
    Prob is exp(MaxP)
  ;
    problog_flag(id_stepsize_log, Step),
    NewLT is LT + Step,
    nb_setval(problog_threshold, NewLT),
    problog_control(off, limit),
    problog_max_id(Goal, Prob, Clauses)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lower bound using k best proofs
% problog_kbest(+Goal,+K,-Prob,-Status)
%
% does iterative deepening search similar to problog_max, but for k(>=1) most likely proofs
% afterwards uses BDD evaluation to calculate probability (also for k=1 -> uniform treatment in learning)
%
% uses dynamic local predicate current_kbest/3 to collect proofs,
% only builds trie at the end (as probabilities of single proofs are important here)
%
% note: >k proofs will be used if the one at position k shares its probability with others,
% as all proofs with that probability will be included
%
% version with _save at the end  renames files for problogbdd to keep them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
problog_kbest_save(Goal, K, Prob, Status, BDDFile, ParamFile) :-
  problog_flag(dir, InternWorkingDir),
  problog_flag(bdd_file, InternBDDFlag),
  problog_flag(bdd_par_file, InternParFlag),
  split_path_file(BDDFile, WorkingDir, BDDFileName),
  split_path_file(ParamFile, _WorkingDir, ParamFileName),
  flag_store(dir, WorkingDir),
  flag_store(bdd_file, BDDFileName),
  flag_store(bdd_par_file, ParamFileName),
  problog_kbest(Goal, K, Prob, Status),
  flag_store(dir, InternWorkingDir),
  flag_store(bdd_file, InternBDDFlag),
  flag_store(bdd_par_file, InternParFlag).
% 	( Status=ok ->
% 	    problog_flag(bdd_file,InternBDDFlag),
% 	    problog_flag(bdd_par_file,InternParFlag),
% 	    convert_filename_to_working_path(InternBDDFlag, InternBDD),
% 	    convert_filename_to_working_path(InternParFlag, InternPar),
% 	    rename_file(InternBDD,BDDFile),
% 	    rename_file(InternPar,ParamFile)
% 	;
% 	true).


/**
 * @pred problog_kbest(+G, +K, -Prob, -Status)
 *
 * This predicate returns the sum of the probabilities of the best K proofs of achieving the goal G and the status of the query.
*/
problog_kbest(Goal, K, Prob, Status) :-
	problog_flag(first_threshold,InitT),
	init_problog_kbest(InitT),
	problog_control(off,up),
	problog_kbest_id(Goal, K),
	retract(current_kbest(_,ListFound,_NumFound)),
	build_prefixtree(ListFound),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	eval_dnf(Trie_Completed_Proofs,Prob,Status),
	delete_ptree(Trie_Completed_Proofs).

% generalizes problog_max to return the k best explanations
problog_kbest_explanations(Goal, K, Explanations) :-
	problog_flag(first_threshold,InitT),
	init_problog_kbest(InitT),
	problog_control(off,up),
	problog_kbest_id(Goal, K),
	retract(current_kbest(_,ListFound,_NumFound)),
	to_external_format_with_reverse(ListFound,Explanations).

problog_real_kbest(Goal, K, Prob, Status) :-
	problog_flag(first_threshold,InitT),
	init_problog_kbest(InitT),
	problog_control(off,up),
	problog_kbest_id(Goal, K),
	retract(current_kbest(_,RawListFound,NumFound)),
    % limiting the number of proofs is not only needed for fast SLD resolution but also for fast BDD building.
    % one can't assume that kbest is called for the former and not for the latter
	take_k_best(RawListFound,K,NumFound,ListFound),
	build_prefixtree(ListFound),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	eval_dnf(Trie_Completed_Proofs,Prob,Status),
	delete_ptree(Trie_Completed_Proofs).

init_problog_kbest(Threshold) :-
	retractall(current_kbest(_,_,_)),
	assertz(current_kbest(-999999,[],0)),  %(log-threshold,proofs,num_proofs)
	init_ptree(Trie_Completed_Proofs),
	nb_setval(problog_completed_proofs, Trie_Completed_Proofs),
	init_problog(Threshold).

problog_kbest_id(Goal, K) :-
	problog_call(Goal),
	update_kbest(K),
	fail.
problog_kbest_id(Goal, K) :-
	current_kbest(CurrentBorder,_,Found),
	nb_getval(problog_threshold, Min),
	problog_flag(last_threshold_log,ToSmall),
	((Found>=K ; \+ problog_control(check,limit) ; Min < CurrentBorder ; Min < ToSmall) ->
	   true
       ;
	problog_flag(id_stepsize_log,Step),
	NewLT is Min+Step,
	nb_setval(problog_threshold, NewLT),
	problog_control(off,limit),
	problog_kbest_id(Goal, K)).

update_kbest(K) :-
	b_getval(problog_probability,NewLogProb),
	current_kbest(LogThreshold,_,_),
	NewLogProb>=LogThreshold,
	b_getval(problog_current_proof,RevProof),
	open_end_close_end(RevProof,Proof),
	update_current_kbest(K,NewLogProb,Proof).

update_current_kbest(_,NewLogProb,Cl) :-
	current_kbest(_,List,_),
	memberchk(NewLogProb-Cl,List),
	!.
update_current_kbest(K,NewLogProb,Cl) :-
	retract(current_kbest(OldThres,List,Length)),
	sorted_insert(NewLogProb-Cl,List,NewList),
	NewLength is Length+1,
	(NewLength < K ->
	    assertz(current_kbest(OldThres,NewList,NewLength))
	;
	(NewLength>K ->
	    First is NewLength-K+1,
	    cutoff(NewList,NewLength,First,FinalList,FinalLength)
	   ; FinalList=NewList, FinalLength=NewLength),
	FinalList=[NewThres-_|_],
	nb_setval(problog_threshold,NewThres),
	assertz(current_kbest(NewThres,FinalList,FinalLength))).

sorted_insert(A,[],[A]).
sorted_insert(A-LA,[B1-LB1|B], [A-LA,B1-LB1|B] ) :-
	A =< B1.
sorted_insert(A-LA,[B1-LB1|B], [B1-LB1|C] ) :-
	A > B1,
	sorted_insert(A-LA,B,C).

% keeps all entries with lowest probability, even if implying a total of more than k
cutoff(List,Len,1,List,Len) :- !.
cutoff([P-L|List],Length,First,[P-L|List],Length) :-
	nth1(First,[P-L|List],PF-_),
	PF=:=P,
	!.
cutoff([_|List],Length,First,NewList,NewLength) :-
	NextFirst is First-1,
	NextLength is Length-1,
	cutoff(List,NextLength,NextFirst,NewList,NewLength).

build_prefixtree([]).
build_prefixtree([_-[]|_List]) :-
	!,
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	insert_ptree(true,Trie_Completed_Proofs).
build_prefixtree([LogP-L|List]) :-
	(
	 problog_flag(show_proofs,true)
	->
	 get_fact_list(L,ListOfFacts),
	 P is exp(LogP),
	 format(user,'~q ~q~n',[P,ListOfFacts])
	;
	 true
	),
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	insert_ptree(L,Trie_Completed_Proofs),
	build_prefixtree(List).

take_k_best(In,K,OutOf,Out) :-
	(
	 K>=OutOf
	->
	 In = Out;
	 In = [_|R],
	 OutOf2 is OutOf-1,
	 take_k_best(R,K,OutOf2,Out)
	).

to_external_format_with_reverse(Intern,Extern) :-
	to_external_format_with_reverse(Intern,[],Extern).
to_external_format_with_reverse([],Extern,Extern).
to_external_format_with_reverse([LogP-FactIDs|Intern],Acc,Extern) :-
	Prob is exp(LogP),
	( FactIDs = [_|_] -> get_fact_list(FactIDs, Facts);
	    Facts = FactIDs),
	to_external_format_with_reverse(Intern,[Prob-Facts|Acc],Extern).


/**
 * @pred problog_exact(+G, -Prob, -Status)
 *
This predicate returns the exact total probability of achieving the goal G and the status of the query.

 using all proofs = using all proofs with probability > 0
*/
problog_exact(Goal,Prob,Status) :-
	problog_control(on, exact),
	problog_low(Goal,0,Prob,Status),
	problog_control(off, exact).

problog_exact_save(Goal,Prob,Status,BDDFile,ParamFile) :-
  problog_flag(dir, InternWorkingDir),
  problog_flag(bdd_file, InternBDDFlag),
  problog_flag(bdd_par_file, InternParFlag),
  split_path_file(BDDFile, WorkingDir, BDDFileName),
  split_path_file(ParamFile, _WorkingDir, ParamFileName),
  flag_store(dir, WorkingDir),
  flag_store(bdd_file, BDDFileName),
  flag_store(bdd_par_file, ParamFileName),
  problog_control(on, exact),
	problog_low(Goal,0,Prob,Status),
	problog_control(off, exact),
  flag_store(dir, InternWorkingDir),
  flag_store(bdd_file, InternBDDFlag),
  flag_store(bdd_par_file, InternParFlag).
% 	(
% 	 Status==ok
% 	->
% 	 (
% 	  problog_flag(bdd_file,InternBDDFlag),
% 	  problog_flag(bdd_par_file,InternParFlag),
% 	  problog_flag(dir,DirFlag),
% 	  atomic_concat([DirFlag,InternBDDFlag],InternBDD),
% 	  atomic_concat([DirFlag,InternParFlag],InternPar),
% 	  rename_file(InternBDD,BDDFile),
% 	  rename_file(InternPar,ParamFile)
% 	 );
% 	 true
% 	).

problog_collect_trie(Goal):-
	problog_call(Goal),
	add_solution,
	fail.
problog_collect_trie(_Goal).

problog_save_state(State):-
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	nb_getval(problog_current_proof, IDs),
	recordz(problog_stack, store(Trie_Completed_Proofs, IDs), State),
	init_ptree(Sub_Trie_Completed_Proofs),
	nb_setval(problog_completed_proofs, Sub_Trie_Completed_Proofs),
	nb_setval(problog_current_proof, []).

problog_restore_state(State):-
	recorded(problog_stack, store(Trie_Completed_Proofs, IDs), State),
	erase(State),
	nb_setval(problog_completed_proofs, Trie_Completed_Proofs),
	nb_setval(problog_current_proof, IDs).

problog_exact_nested(Goal, Prob, Status):-
  problog_save_state(State),
  problog_collect_trie(Goal),
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
/*  writeln(Goal),
  print_nested_ptree(Trie_Completed_Proofs),*/
  eval_dnf(Trie_Completed_Proofs, Prob, Status),
  delete_ptree(Trie_Completed_Proofs),
  problog_restore_state(State).


/**
 * @pred problog_montecarlo(+G, +Interval_width, -Prob)
 *
 * This predicate approximates the probability of achieving the goal G by using a Monte Carlo approach, with 95% confidence in the given interval width.
 *
 * probability by sampling:
 * running another N samples until 95percentCI-width<Delta
 * lazy sampling using three-valued array indexed by internal fact IDs for ground facts,
 *   internal database keys mc_true and mc_false for groundings of non-ground facts (including dynamic probabilities)
 */
problog_montecarlo(Goal,Delta,Prob) :-
	retractall(mc_prob(_)),
	nb_getval(probclause_counter,ID), !,
	C is ID+1,
	static_array(mc_sample,C,char),
	problog_control(off,up),
	problog_flag(mc_batchsize,N),
	problog_flag(mc_logfile,File1),
	convert_filename_to_working_path(File1, File),
	montecarlo(Goal,Delta,N,File),
	retract(mc_prob(Prob)),
	close_static_array(mc_sample).

montecarlo(Goal,Delta,K,File) :-
	clean_sample,
	problog_control(on,mc),
	open(File,write,Log),
	format(Log,'# goal: ~q~n#delta: ~w~n',[Goal,Delta]),
	format(Log,'# num_programs  prob   low   high  diff  time~2n',[]),
	close(Log),
	timer_reset(monte_carlo),
	timer_start(monte_carlo),
	format_if_verbose(user,'search for ~q~n',[Goal]),
	montecarlo(Goal,Delta,K,0,File,0),
	timer_stop(monte_carlo,_Monte_Carlo_Time),
	problog_control(off,mc).

% calculate values after K samples
montecarlo(Goal,Delta,K,SamplesSoFar,File,PositiveSoFar) :-
	SamplesNew is SamplesSoFar+1,
	SamplesNew mod K =:= 0,
	!,
	copy_term(Goal,GoalC),
	(
	 mc_prove(GoalC)
	->
	 Next is PositiveSoFar+1;
	 Next=PositiveSoFar
	),
	Prob is Next/SamplesNew,
	timer_elapsed(monte_carlo,Time),

	problog_convergence_check(Time, Prob, SamplesNew, Delta, _Epsilon, Converged),
	(
	 (Converged == true; Converged == terminate)
	->
	 format_if_verbose(user,'Runtime ~w ms~2n',[Time]),
	 assertz(mc_prob(Prob))
	;
	 montecarlo(Goal,Delta,K,SamplesNew,File,Next)
	).
% continue until next K samples done
montecarlo(Goal,Delta,K,SamplesSoFar,File,PositiveSoFar) :-
	SamplesNew is SamplesSoFar+1,
	copy_term(Goal,GoalC),
	(mc_prove(GoalC) -> Next is PositiveSoFar+1; Next=PositiveSoFar),
	montecarlo(Goal,Delta,K,SamplesNew,File,Next).

mc_prove(A) :- !,
	(get_some_proof(A) ->
	 clean_sample
	;
	 clean_sample,fail
	).

clean_sample :-
	reset_static_array(mc_sample),
	eraseall(mc_true),
	eraseall(mc_false),
	reset_non_ground_facts,
%   problog_abolish_all_tables.
	problog_tabled(P),
	problog_abolish_table(P),
	fail.
clean_sample.

% find new proof -- need to reset control after init
get_some_proof(Goal) :-
	init_problog(0),
	problog_control(on,mc),
	problog_call(Goal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exact probability of all ground instances of Goal
% output goes to File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
problog_answers(Goal,File) :-
	set_problog_flag(verbose,false),
	retractall(answer(_)),
% this will not give the exact prob of Goal!
	problog_exact((Goal,ground(Goal),\+problog:answer(Goal),assertz(problog:answer(Goal))),_,_),
	open(File,write,_,[alias(answer)]),
	eval_answers,
	close(answer).

eval_answers :-
	retract(answer(G)),
	problog_exact(G,P,_),
	format(answer,'answer(~q,~w).~n',[G,P]),
	fail.
eval_answers.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% find k most likely different answers (using their explanation prob as score)
% largely copied+adapted from kbest, uses same dynamic predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
problog_kbest_answers(Goal,K,ResultList) :-
	problog_flag(first_threshold,InitT),
	init_problog_kbest(InitT),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
	delete_ptree( Trie_Completed_Proofs), % this is just because we reuse init from kbest and don't need the tree
	problog_control(off,up),
	problog_kbest_answers_id(Goal, K),
	retract(current_kbest(_,LogResultList,_NumFound)),
	transform_loglist_to_result(LogResultList,ResultList).

problog_kbest_answers_id(Goal, K) :-
	problog_call(Goal),
	copy_term(Goal,GoalCopy), % needed?
	update_kbest_answers(GoalCopy,K),
	fail.
problog_kbest_answers_id(Goal, K) :-
	current_kbest(CurrentBorder,_,Found),
	nb_getval(problog_threshold, Min),
	problog_flag(last_threshold_log,ToSmall),
	((Found>=K ; \+ problog_control(check,limit) ; Min < CurrentBorder ; Min < ToSmall) ->
	   true
       ;
	problog_flag(id_stepsize_log,Step),
	NewLT is Min+Step,
	nb_setval(problog_threshold, NewLT),
	problog_control(off,limit),
	problog_kbest_answers_id(Goal, K)).

update_kbest_answers(Goal,K) :-
	b_getval(problog_probability,NewLogProb),
	current_kbest(LogThreshold,_,_),
	NewLogProb>=LogThreshold,
	update_current_kbest_answers(K,NewLogProb,Goal).

update_current_kbest_answers(_,NewLogProb,Goal) :-
	current_kbest(_,List,_),
	update_prob_of_known_answer(List,Goal,NewLogProb,NewList),
	!,
	keysort(NewList,SortedList),%format(user_error,'updated variant of ~w~n',[Goal]),
	retract(current_kbest(K,_,Len)),
	assertz(current_kbest(K,SortedList,Len)).
update_current_kbest_answers(K,NewLogProb,Goal) :-
	retract(current_kbest(OldThres,List,Length)),
	sorted_insert(NewLogProb-Goal,List,NewList),%format(user_error,'inserted new element ~w~n',[Goal]),
	NewLength is Length+1,
	(NewLength < K ->
	    assertz(current_kbest(OldThres,NewList,NewLength))
	;
	(NewLength>K ->
	    First is NewLength-K+1,
	    cutoff(NewList,NewLength,First,FinalList,FinalLength)
	   ; FinalList=NewList, FinalLength=NewLength),
	FinalList=[NewThres-_|_],
	nb_setval(problog_threshold,NewThres),
	assertz(current_kbest(NewThres,FinalList,FinalLength))).

% this fails if there is no variant -> go to second case above
update_prob_of_known_answer([OldLogP-OldGoal|List],Goal,NewLogProb,[MaxLogP-OldGoal|List]) :-
	variant(OldGoal,Goal),
	!,
	MaxLogP is max(OldLogP,NewLogProb).
update_prob_of_known_answer([First|List],Goal,NewLogProb,[First|NewList]) :-
	update_prob_of_known_answer(List,Goal,NewLogProb,NewList).

transform_loglist_to_result(In,Out) :-
	transform_loglist_to_result(In,[],Out).
transform_loglist_to_result([],Acc,Acc).
transform_loglist_to_result([LogP-G|List],Acc,Result) :-
	P is exp(LogP),
	transform_loglist_to_result(List,[P-G|Acc],Result).

%%%%%%%%%%%%%%%%%%%%%%%%%
% koptimal
%%%%%%%%%%%%%%%%%%%%%%%%%

problog_koptimal(Goal,K,Prob) :-
  problog_flag(last_threshold, InitT),
	problog_koptimal(Goal,K,InitT,Prob).

problog_koptimal(Goal,K,Theta,Prob) :-
	init_problog_koptimal,
	problog_koptimal_it(Goal,K,Theta),
	nb_getval(problog_completed_proofs,Trie_Completed_Proofs),
	optimal_proof(_,Prob),
	set_problog_flag(save_bdd, false),
	set_problog_flag(nodedump_bdd, false),
	delete_ptree(Trie_Completed_Proofs),
	nb_getval(dtproblog_completed_proofs,DT_Trie_Completed_Proofs),
	delete_ptree(DT_Trie_Completed_Proofs),
	clear_tabling.

init_problog_koptimal :-
	%Set the reuse flag on true in order to retain the calculated bdd's
	set_problog_flag(save_bdd, true),
	set_problog_flag(nodedump_bdd, true),
	%Initialise the trie
	init_ptree(Trie_Completed_Proofs),
	nb_setval(problog_completed_proofs, Trie_Completed_Proofs),
	init_ptree(Trie_DT_Completed_Proofs),
	nb_setval(dtproblog_completed_proofs,Trie_DT_Completed_Proofs),
	problog_control(off,up),
	%Initialise the control parameters
	retractall(possible_proof(_,_)),
	retractall(impossible_proof(_)).

problog_koptimal_it(Goal,K,Theta) :-
	K > 0,
	init_problog_koptimal_it(Theta),
	%add optimal proof, this fails when no new proofs can be found
	(add_optimal_proof(Goal,Theta) -> Knew is K - 1; Knew = 0),!,
	problog_koptimal_it(Goal,Knew,Theta).
problog_koptimal_it(_,0,_).

init_problog_koptimal_it(Theta) :-
	%Clear the tables
	abolish_table(conditional_prob/4),
	%initialise problog
	init_problog(Theta),

	%retract control parameters for last iteration
	retractall(optimal_proof(_,_)),
	retractall(current_prob(_)),

	%calculate the bdd with the additional found proof
	nb_getval(problog_completed_proofs,Trie_Completed_Proofs),
	eval_dnf(Trie_Completed_Proofs,PCurr,_),

	%set the current probability
	assert(current_prob(PCurr)),
	assert(optimal_proof(unprovable,PCurr)),

	%use the allready found proofs to initialise the threshold
	findall(Proof-MaxAddedP,possible_proof(Proof,MaxAddedP),PossibleProofs),
	sort_possible_proofs(PossibleProofs,SortedPossibleProofs),
	initialise_optimal_proof(SortedPossibleProofs,Theta).

sort_possible_proofs(List,Sorted):-sort_possible_proofs(List,[],Sorted).
sort_possible_proofs([],Acc,Acc).
sort_possible_proofs([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	sort_possible_proofs(L1,Acc,Sorted1),sort_possible_proofs(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
pivoting(Pivot-PPivot,[Proof-P|T],[Proof-P|G],L):-P=<PPivot,pivoting(Pivot-PPivot,T,G,L).
pivoting(Pivot-PPivot,[Proof-P|T],G,[Proof-P|L]):-P>PPivot,pivoting(Pivot-PPivot,T,G,L).


initialise_optimal_proof([],_).
initialise_optimal_proof([Proof-MaxAdded|Rest],Theta) :-
	optimal_proof(_,Popt),
	current_prob(Pcurr),
	OptAdded is Popt - Pcurr,
	(MaxAdded > OptAdded ->
		calculate_added_prob(Proof, P,ok),

		%update the maximal added probability
		retractall(possible_proof(Proof,_)),
		AddedP is P - Pcurr,
		(AddedP > Theta ->
			%the proof can still add something
			assert(possible_proof(Proof,AddedP)),

			%Check whether to change the optimal proof
			(P > Popt ->
				retractall(optimal_proof(_,_)),
				assert(optimal_proof(Proof,P)),
				NewT is log(AddedP),
				nb_setval(problog_threshold,NewT)
			;
				true
			)
		;
			%the proof cannot add anything anymore
			assert(impossible_proof(Proof))
		),
		initialise_optimal_proof(Rest,Theta)
	;
		%The rest of the proofs have a maximal added probability smaller then the current found optimal added probability
		true
	).

add_optimal_proof(Goal,Theta) :-
	problog_call(Goal),
	update_koptimal(Theta).
add_optimal_proof(_,_) :-
	optimal_proof(Proof,_),
	((Proof = unprovable) ->
		%No possible proof is present
		fail
	;
		%We add the found to the trie
		remove_decision_facts(Proof, PrunedProof),
		nb_setval(problog_current_proof, PrunedProof-[]),
		(PrunedProof = [] -> true ; add_solution),
		nb_getval(dtproblog_completed_proofs,DT_Trie_Completed_Proofs),
		insert_ptree(Proof, DT_Trie_Completed_Proofs),
		retract(possible_proof(Proof,_)),
		assert(impossible_proof(Proof))
	).

update_koptimal(Theta) :-
	%We get the found proof	and the already found proofs
	b_getval(problog_current_proof, OpenProof),
	open_end_close_end(OpenProof, Proof),
	((possible_proof(Proof,_); impossible_proof(Proof))  ->
		%The proof is already treated in the initialization step
		fail
	;
		%The proof isn't yet treated
		calculate_added_prob(Proof,P,ok),
		optimal_proof(_,Popt),
		current_prob(PCurr),
		AddedP is P - PCurr,
		(AddedP > Theta ->
			assert(possible_proof(Proof,AddedP))
		;
			%The proof has an additional probability smaller than theta so gets blacklisted
			assert(impossible_proof(Proof)),
			fail
		),
		(P > Popt ->
			%We change the curret optimal proof with the found proof
			retractall(optimal_proof(_,_)),
			assert(optimal_proof(Proof,P)),
			NewT is log(AddedP),
			nb_setval(problog_threshold,NewT),
			fail
		;
			%The proof isn't better then the current optimal proof so we stop searching
			fail
		)
	).

remove_decision_facts([Fact|Proof], PrunedProof) :-
	remove_decision_facts(Proof,RecPruned),
	catch((get_fact_probability(Fact,_),PrunedProof = [Fact|RecPruned]),_,PrunedProof = RecPruned).
remove_decision_facts([],[]).

calculate_added_prob([],P,ok) :-
	current_prob(P).
calculate_added_prob(Proof,P,S) :-
	Proof \= [],
	remove_decision_facts(Proof,PrunedProof),
	remove_used_facts(PrunedProof,Used,New),
	bubblesort(Used,SortedUsed),
	calculate_added_prob(SortedUsed,New,[],PAdded,S),
	round_added_prob(PAdded,P).

calculate_added_prob([],[],_,1,ok).
calculate_added_prob([UsedFact|UsedProof],[],Conditions,P,S) :-
	calculate_added_prob(UsedProof,[],[UsedFact|Conditions],Prec,Srec),
	problog_flag(nodedump_file,NodeDumpFile),
  convert_filename_to_working_path(NodeDumpFile, SONodeDumpFile),
	convert_filename_to_working_path('save_params', ParFile),
	negate(UsedFact,NegatedFact),
	conditional_prob(SONodeDumpFile,ParFile,[NegatedFact|Conditions],Pcond,Scond),
	( Srec = ok ->
		( Scond = ok ->
			S = ok,
			get_fact_probability(UsedFact,Pfact),
			P is Pfact*Prec + (1 - Pfact)*Pcond
		;
			S = Scond
		)
	;
		S = Srec
	).
calculate_added_prob(UsedProof,[NewFact|NewFacts],[],P,S) :-
	calculate_added_prob(UsedProof,NewFacts,[],Prec,S),
	(	S = ok ->
		get_fact_probability(NewFact,Pfact),
		current_prob(Pcurr),
		P is Pfact*Prec + (1 - Pfact)*Pcurr
	;
		true
	).

bubblesort(List,Sorted):-
 swap(List,List1),!,
 bubblesort(List1,Sorted).
bubblesort(Sorted,Sorted).

swap([X,Y|Rest], [Y,X|Rest]):- bigger(X,Y).
swap([Z|Rest],[Z|Rest1]):- swap(Rest,Rest1).

bigger(not(X), X) :-
	!.
bigger(not(X), not(Y)) :-
	!,
	bigger(X,Y).
bigger(not(X),Y) :-
	!,
	bigger(X,Y).
bigger(X, not(Y)) :-
	!,
	bigger(X,Y).
bigger(X,Y) :-
	split_grounding_id(X,IDX,GIDX),
	split_grounding_id(Y,IDY,GIDY),!,
	(
		IDX > IDY
	;
		IDX == IDY,
		GIDX > GIDY
	).
bigger(X,Y) :-
	split_grounding_id(X,IDX,_),!,
	IDX > Y.
bigger(X,Y) :-
	split_grounding_id(Y,IDY,_),!,
	X > IDY.
bigger(X,Y) :-
	X > Y.

round_added_prob(P,RoundedP) :-
	P < 1,
	Pnew is P*10,
	round_added_prob(Pnew,RoundedPnew),
	RoundedP is RoundedPnew/10.
round_added_prob(P,RoundedP) :-
	P >= 1,
	RoundedP is round(P*1000000)/1000000.

negate(not(Fact),Fact).
negate(Fact,not(Fact)) :-
	Fact \= not(_).

remove_used_facts([],[],[]).
remove_used_facts([Fact|Rest],Used,New) :-
	remove_used_facts(Rest,RecUsed,RecNew),
	used_facts(Facts),
	(member(Fact,Facts) ->
		Used = [Fact|RecUsed],
		New = RecNew
	;

		Used = RecUsed,
		New = [Fact|RecNew]
	).


used_fact(Fact) :-
	used_facts(Facts),
	member(Fact,Facts).
used_facts(Facts) :-
	convert_filename_to_working_path('save_map', MapFile),
	see(MapFile),
	read(mapping(L)),
	findall(Var,member(m(Var,_,_),L),Facts),
	seen.

conditional_prob(_,_,[],P,ok) :-
	current_prob(P).
conditional_prob(NodeDump,ParFile,Conditions,P,S) :-
	problog_flag(save_bdd,Old_Save),
	problog_flag(nodedump_bdd,Old_File),
	set_problog_flag(save_bdd, false),
	set_problog_flag(nodedump_bdd, false),
	convert_filename_to_working_path('temp_par_file', ChangedParFile),
	change_par_file(ParFile,Conditions,ChangedParFile),
	execute_bdd_tool(NodeDump,ChangedParFile,P,S),
	%delete_file(ChangedParFile),
	set_problog_flag(save_bdd,Old_Save),
	set_problog_flag(nodedump_bdd,Old_File).

change_par_file(ParFile,[],ChangedParFile) :-
	%atomic_concat(['cp ', ParFile, ' ', ChangedParFile],Command),
	%statistics(walltime,[T1,_]),
	%shell(Command,_),
	copy_file(ParFile,ChangedParFile).
	%statistics(walltime,[T2,_]),
	%T is T2 - T1,
	%format("copy time: ~w\n",[T]).
change_par_file(ParFile,[ID|Rest],ChangedParFile) :-
	ID \= not(_),
	change_par_file(ParFile,Rest,ChangedParFile),
	open(ChangedParFile,'append',S),
	tell(S),
	format('@x~w\n1\n',[ID]),
	told.
change_par_file(ParFile,[not(ID)|Rest],ChangedParFile) :-
	change_par_file(ParFile,Rest,ChangedParFile),
	open(ChangedParFile,'append',S),
	tell(S),
	format('@x~w\n0\n',[ID]),
	told.

% Copies a file
copy_file(From,To) :-
	file_filter(From,To,copy_aux).
copy_aux(In,In).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERAL PURPOSE PREDICATES FOR DTPROBLOG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do inference of a single goal, using the default inference method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_infer(Goal,Prob) :-
	problog_flag(inference,Method),
	problog_infer(Method,Goal,Prob).

problog_infer(exact,Goal,Prob) :-
	problog_exact(Goal,Prob,ok).
problog_infer(atleast-K-best,Goal,Prob) :-
	problog_kbest(Goal,K,Prob,ok).
problog_infer(K-best,Goal,Prob) :-
	problog_real_kbest(Goal,K,Prob,ok).
problog_infer(montecarlo(Confidence),Goal,Prob) :-
	problog_montecarlo(Goal,Confidence,Prob).
problog_infer(delta(Width),Goal,Prob) :-
	problog_delta(Goal,Width,Bound_low,Bound_up,ok),
	Prob is 0.5*(Bound_low+Bound_up).
problog_infer(low(Threshold),Goal,Prob) :-
	problog_low(Goal,Threshold,Prob,ok).
problog_infer(threshold(Threshold),Goal,Prob) :-
	problog_threshold(Goal,Threshold,Bound_low,Bound_up,ok),
	Prob is 0.5*(Bound_low+Bound_up).
problog_infer(K-optimal,Goal,Prob) :-
	problog_koptimal(Goal,K,Prob).
problog_infer(K-T-optimal,Goal,Prob) :-
	problog_koptimal(Goal,K,T,Prob).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do inference of a set of queries, using the default inference method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_infer_forest([],[]) :- !.
problog_infer_forest(Goals,Probs) :-
    (problog_infer_forest_supported ->
      problog_bdd_forest(Goals),
      length(Goals,N),
      eval_bdd_forest(N,Probs,ok)
    ;
      throw(error('Flag settings not supported by problog_infer_forest/1.'))
    ).

problog_infer_forest_supported :- problog_bdd_forest_supported.

eval_bdd_forest(N,Probs,Status) :-
	bdd_files(BDDFile,BDDParFile),
	problog_flag(bdd_time,BDDTime),
    (problog_flag(dynamic_reorder, true) ->
      ParamD = ''
    ;
      ParamD = ' -dreorder'
    ),
    (problog_flag(bdd_static_order, true) ->
      problog_flag(static_order_file, FileName),
      convert_filename_to_working_path(FileName, SOFileName),
      atomic_concat([ParamD, ' -sord ', SOFileName], Param)
    ;
      Param = ParamD
    ),
    convert_filename_to_problog_path('simplecudd', ProblogBDD),
    problog_flag(bdd_result,ResultFileFlag),
    convert_filename_to_working_path(ResultFileFlag, ResultFile),
    atomic_concat([ProblogBDD, Param,' -l ', BDDFile, ' -i ', BDDParFile, ' -m p -t ', BDDTime, ' > ', ResultFile], Command),
	statistics(walltime,_),
	shell(Command,Return),
	(Return =\= 0 ->
	    Status = timeout
	;
	    statistics(walltime,[_,E3]),
	        format_if_verbose(user,'~w ms BDD processing~n',[E3]),
		see(ResultFile),
		read_probs(N,Probs),
		seen,
		Status = ok,
		% cleanup
		% TODO handle flag for keeping files
		(problog_flag(save_bdd,true) ->
			true
		;
		catch(delete_file(BDDFile),_, fail),
		catch(delete_file(BDDParFile),_, fail),
		catch(delete_file(ResultFile),_, fail),
		    delete_bdd_forest_files(N)
        )
	).

read_probs(N,Probs) :-
	(N = 0 ->
		Probs = []
	;
		Probs = [Prob|Rest],
		read(probability(Prob)),
		N2 is N-1,
		read_probs(N2,Rest)
	).

delete_bdd_forest_files(N) :-
	(N=0 ->
		true
	;
		bdd_forest_file(N,BDDFile),
		catch(delete_file(BDDFile),_, fail),
		N2 is N-1,
		delete_bdd_forest_files(N2)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Build a trie using the default inference method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_trie(Goal, Trie) :-
  (build_trie_supported ->
    problog_flag(inference,Method),
    once(build_trie(Method, Goal, Trie))
  ;
    throw(error('Flag settings not supported by build_trie/2.'))
  ).

build_trie_supported :- problog_flag(inference,exact).
build_trie_supported :- problog_flag(inference,low(_)).
build_trie_supported :- problog_flag(inference,atleast-_-best).
build_trie_supported :- problog_flag(inference,_-best).
build_trie_supported :- problog_flag(inference,_-optimal).
build_trie_supported :- problog_flag(inference,_-_-optimal).

build_trie(exact, Goal, Trie) :-
  problog_control(on, exact),
  build_trie(low(0), Goal, Trie),
  problog_control(off, exact).

build_trie(low(Threshold), Goal, _) :-
  number(Threshold),
  init_problog_low(Threshold),
  problog_control(off, up),
  timer_start(build_tree_low),
  problog_call(Goal),
  add_solution,
  fail.
build_trie(low(Threshold), _, Trie) :-
  number(Threshold),
  timer_stop(build_tree_low,Build_Tree_Low),
  problog_var_set(sld_time, Build_Tree_Low),
  nb_getval(problog_completed_proofs, Trie).
  % don't clear tabling; tables can be reused by other query

build_trie(atleast-K-best, Goal, Trie) :-
  number(K),
  problog_flag(first_threshold,InitT),
  init_problog_kbest(InitT),
  problog_control(off,up),
  problog_kbest_id(Goal, K),
  retract(current_kbest(_,ListFound,_NumFound)),
  build_prefixtree(ListFound),
  nb_getval(problog_completed_proofs, Trie),
  clear_tabling. % clear tabling because tables cannot be reused by other query


build_trie(K-best, Goal, Trie) :-
  number(K),
  problog_flag(first_threshold,InitT),
  init_problog_kbest(InitT),
  problog_control(off,up),
  problog_kbest_id(Goal, K),
  retract(current_kbest(_,RawListFound,NumFound)),
  % limiting the number of proofs is not only needed for fast SLD resolution but also for fast BDD building.
  % one can't assume that kbest is called for the former and not for the latter
  % thus, we take EXACTLY k proofs
  take_k_best(RawListFound,K,NumFound,ListFound),
  build_prefixtree(ListFound),
  nb_getval(problog_completed_proofs, Trie),
  clear_tabling. % clear tabling because tables cannot be reused by other query

build_trie(K-optimal, Goal, Trie) :-
	number(K),
	init_problog_koptimal,
  problog_flag(last_threshold, InitT),
	problog_koptimal_it(Goal,K,InitT),
	set_problog_flag(save_bdd, false),
	set_problog_flag(nodedump_bdd, false),
	nb_getval(problog_completed_proofs,Trie_Completed_Proofs),
	delete_ptree(Trie_Completed_Proofs),
	nb_getval(dtproblog_completed_proofs,Trie),
	clear_tabling.

build_trie(K-T-optimal, Goal, Trie) :-
	number(K),
	init_problog_koptimal,
	problog_koptimal_it(Goal,K,T),
	set_problog_flag(save_bdd, false),
	set_problog_flag(nodedump_bdd, false),
	nb_getval(problog_completed_proofs,Trie_Completed_Proofs),
	delete_ptree(Trie_Completed_Proofs),
	nb_getval(dtproblog_completed_proofs,Trie),
	clear_tabling.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write BDD structure script for a trie and list all variables used
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_bdd_struct_script(Trie,BDDFile,Variables) :-
	(
	 hybrid_proof(_,_,_)	% Check whether we use Hybrid ProbLog
	->
	 (
				% Yes! run the disjoining stuff
	  retractall(hybrid_proof_disjoint(_,_,_,_)),
	  disjoin_hybrid_proofs,

	  init_ptree(OriTrie),		       % use this as tmp ptree
	  forall(enum_member_ptree(List,OriTrie1), % go over all stored proofs
		 (
		  (
		   List=[_|_]
		  ->
		   Proof=List;
		   Proof=[List]
		  ),
		  (
		   select(continuous(ProofID),Proof,Rest)
		  ->
		   (
				% this proof is using continuous facts
		    all_hybrid_subproofs(ProofID,List2),
		    append(Rest,List2,NewProof),
		    insert_ptree(NewProof,OriTrie)
		   );
		   insert_ptree(Proof,OriTrie)
		  )
		 )
		)
	 );
				% Nope, just pass on the Trie
	 OriTrie=OriTrie1
	),

  ((problog_flag(variable_elimination, true), nb_getval(problog_nested_tries, false)) ->
    statistics(walltime, _),
    trie_check_for_and_cluster(OriTrie),
    statistics(walltime, [_, VariableEliminationTime]),
    trie_replace_and_cluster(OriTrie, Trie),
    problog_var_set(variable_elimination_time, VariableEliminationTime),
    variable_elimination_stats(Clusters, OrigPF, CompPF),
    problog_var_set(variable_elimination_stats, compress(Clusters, OrigPF, CompPF)),
    clean_up
  ;
    Trie = OriTrie
  ),
  (problog_flag(bdd_static_order, true) ->
    get_order(Trie, Order),
    problog_flag(static_order_file, SOFName),
    convert_filename_to_working_path(SOFName, SOFileName),
    generate_order_by_prob_fact_appearance(Order, SOFileName)
  ;
    true
  ),
  ptree:trie_stats(Memory, Tries, Entries, Nodes),
  (nb_getval(problog_nested_tries, false) ->
    ptree:trie_usage(Trie, TEntries, TNodes, TVirtualNodes),
    problog_var_set(trie_statistics, tries(memory(Memory), tries(Tries), entries(TEntries), nodes(TNodes), virtualnodes(TVirtualNodes)))
  ;
    problog_var_set(trie_statistics, tries(memory(Memory), tries(Tries), entries(Entries), nodes(Nodes)))
  ),
  (problog_flag(triedump, true) ->
    convert_filename_to_working_path(trie_file, TrieFile),
    tell(TrieFile),
    print_nested_ptree(Trie),
    flush_output,
    told,
    tell(user_output)
  ;
    true
  ),
  nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
  ((Trie = Trie_Completed_Proofs, problog_flag(save_bdd, true)) ->
    problog_control(on, remember)
  ;
    problog_control(off, remember)
  ),
  % old reduction method doesn't support nested tries
  ((problog_flag(use_old_trie, true), nb_getval(problog_nested_tries, false)) ->
    statistics(walltime, _),
    (problog_control(check, remember) ->
      bdd_struct_ptree_map(Trie, BDDFile, Variables, Mapping),
      convert_filename_to_working_path(save_map, MapFile),
      tell(MapFile),
      format('mapping(~q).~n', [Mapping]),
      flush_output,
      told
    ;
      bdd_struct_ptree(Trie, BDDFile, Variables)
    ),
    statistics(walltime, [_, ScriptGenerationTime]),
    problog_var_set(bdd_script_time, ScriptGenerationTime)
    % omitted call to execute_bdd_tool
  ;
    true
  ),
  % naive method with nested trie support but not loops
  ((problog_flag(use_naive_trie, true); (problog_flag(use_old_trie, true), nb_getval(problog_nested_tries, true))) ->
    statistics(walltime, _),
    atomic_concat([BDDFile, '_naive'], BDDFile_naive),
    nested_ptree_to_BDD_struct_script(Trie, BDDFile_naive, Variables),
    statistics(walltime, [_, ScriptGenerationTime_naive]),
    problog_var_set(bdd_script_time(naive), ScriptGenerationTime_naive)
    % omitted call to execute_bdd_tool
  ;
    true
  ),
  % reduction method with depth_breadth trie support
  problog_flag(db_trie_opt_lvl, ROptLevel),
  problog_flag(db_min_prefix, MinPrefix),

  (problog_flag(compare_opt_lvl, true) ->
    generate_ints(0, ROptLevel, Levels)
  ;
    Levels = [ROptLevel]
  ),
  % Removed forall here, because it hides 'Variables' from what comes afterwards
  memberchk(OptLevel, Levels),
  (
    (problog_flag(use_db_trie, true) ->
      tries:trie_db_opt_min_prefix(MinPrefix),
      statistics(walltime, _),
      (nb_getval(problog_nested_tries, false) ->
        trie_to_bdd_struct_trie(Trie, DBTrie, BDDFile, OptLevel, Variables)
      ;
        nested_trie_to_bdd_struct_trie(Trie, DBTrie, BDDFile, OptLevel, Variables)
      ),
      atomic_concat(['builtin_', OptLevel], Builtin),
      ptree:trie_stats(DBMemory, DBTries, DBEntries, DBNodes),
      FM is DBMemory - Memory,
      FT is DBTries - Tries,
      FE is DBEntries - Entries,
      FN is DBNodes - Nodes,
      problog_var_set(dbtrie_statistics(Builtin), tries(memory(FM), tries(FT), entries(FE), nodes(FN))),

      delete_ptree(DBTrie),
      statistics(walltime, [_, ScriptGenerationTime_builtin]),
      problog_var_set(bdd_script_time(Builtin), ScriptGenerationTime_builtin)
      % omitted call to execute_bdd_tool
    ;
      true
    )
  ),

  % decomposition method
  (problog_flag(use_dec_trie, true) ->
    atomic_concat([BDDFile, '_dec'], BDDFile_dec),
    timer_start(script_gen_time_dec),
    ptree_decomposition_struct(Trie, BDDFile_dec, Variables),
    timer_stop(script_gen_time_dec,Script_Gen_Time_Dec),
    problog_var_set(bdd_script_time(dec), Script_Gen_Time_Dec)
    % omitted call to execute_bdd_tool
  ;
    true
  ),

  (Trie =\= OriTrie ->
    delete_ptree(Trie)
  ;
    true
  ),
  (var(Variables) -> throw(error('novars')) ; true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Building a forest of BDDs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_bdd_forest(Goals) :-
	(problog_bdd_forest_supported ->
      require(keep_ground_ids),
      once(write_bdd_forest(Goals,[],Vars,1)),
      unrequire(keep_ground_ids),
      reset_non_ground_facts,
      bdd_par_file(BDDParFile),
% 	  format('Vars: ~w~n',[Vars]),
      tell(BDDParFile),
      bdd_vars_script(Vars),
      flush_output, % isnt this called by told/0?
      told,
%       false,
      length(Goals,L),
      length(Vars,NbVars),
      write_global_bdd_file(NbVars,L),
      (problog_flag(retain_tables, true) -> retain_tabling; true),
      clear_tabling
    ;
      throw(error('Flag settings not supported by problog_bdd_forest/1.'))
    ).

problog_bdd_forest_supported :- build_trie_supported.

% Iterate over all Goals, write BDD scripts and collect variables used.
write_bdd_forest([],AtomsTot,AtomsTot,_).
write_bdd_forest([Goal|Rest],AtomsAcc,AtomsTot,N) :-
  build_trie(Goal, Trie),
  write_nth_bdd_struct_script(N, Trie, Vars),
  (problog_flag(verbose, true)->
    problog_statistics
  ;
    true
  ),
  delete_ptree(Trie),
  N2 is N+1,
  % map 'not id' to id in Vars
  findall(ID,(member((not ID),Vars)) ,NegativeAtoms),
  findall(ID,(member(ID,Vars),ID \= (not _)),PositiveAtoms),
%   format('PositiveAtoms: ~w~n',[PositiveAtoms]),
%   format('NegativeAtoms: ~w~n',[NegativeAtoms]),
  append(PositiveAtoms,NegativeAtoms,Atoms),
  list_to_ord_set(Atoms,AtomsSet),
  ord_union(AtomsAcc,AtomsSet,AtomsAcc2),
  once(write_bdd_forest(Rest,AtomsAcc2,AtomsTot,N2)).

% Write files
write_nth_bdd_struct_script(N,Trie,Vars) :-
	bdd_forest_file(N,BDDFile),
	write_bdd_struct_script(Trie,BDDFile,Vars).

write_global_bdd_file(NbVars,L) :-
	bdd_file(BDDFile),
	open(BDDFile,'write',BDDFileStream),
	format(BDDFileStream,'@BDD2~n~w~n~w~n~w~n',[NbVars,0,L]),
	write_global_bdd_file_line(1,L,BDDFileStream),
	write_global_bdd_file_query(1,L,BDDFileStream),
	close(BDDFileStream).

write_global_bdd_file_line(I,Max,_Handle) :-
	I>Max,
	!.
write_global_bdd_file_line(I,Max,Handle) :-
	bdd_forest_file(I,BDDFile),
	format(Handle,'L~q = <~w>~n',[I,BDDFile]),
	I2 is I+1,
	write_global_bdd_file_line(I2,Max,Handle).

write_global_bdd_file_query(Max,Max,Handle) :-
	!,
	format(Handle,'L~q~n',[Max]).
write_global_bdd_file_query(I,Max,Handle) :-
	format(Handle,'L~q,',[I]),
	I2 is I+1,
	write_global_bdd_file_query(I2,Max,Handle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Filename specifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bdd_forest_file(N,BDDFile) :-
	problog_flag(bdd_file,BDDFileFlag),
	atomic_concat([BDDFileFlag,'_',N],BDDFileFlagWithN),
	convert_filename_to_working_path(BDDFileFlagWithN, BDDFile).

bdd_files(BDDFile,BDDParFile) :-
	bdd_file(BDDFile),
	bdd_par_file(BDDParFile).

bdd_file(BDDFile) :-
	problog_flag(bdd_file, BDDFileFlag),
	convert_filename_to_working_path(BDDFileFlag, BDDFile).

bdd_par_file(BDDParFile) :-
	problog_flag(bdd_par_file, BDDParFileFlag),
	convert_filename_to_working_path(BDDParFileFlag, BDDParFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Persistent Ground IDs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

require(Feature) :-
  atom(Feature),
  atomic_concat(['problog_required_',Feature],Feature_Required),
  atomic_concat([Feature_Required,'_',depth],Feature_Depth),
  (required(Feature) ->
      b_getval(Feature_Depth,Depth),
      Depth1 is Depth+1,
      b_setval(Feature_Depth,Depth1)
  ;
      b_setval(Feature_Required,required),
      b_setval(Feature_Depth,1)
      %,format("starting to require ~q~n",[Feature])
  ).

unrequire(Feature) :-
  atom(Feature),
  atomic_concat(['problog_required_',Feature],Feature_Required),
  atomic_concat([Feature_Required,'_',depth],Feature_Depth),
  b_getval(Feature_Depth,Depth),
  (Depth=1 ->
      nb_delete(Feature_Required),
      nb_delete(Feature_Depth)
      %,format("stopped keeping ground id's~n",[])
  ;
      Depth1 is Depth-1,
      b_setval(Feature_Depth,Depth1)
  ).

required(Feature) :-
	atom(Feature),
	atomic_concat(['problog_required_',Feature],Feature_Required),
	catch(b_getval(Feature_Required,Val),error(existence_error(variable,Feature_Required),_),fail),
	Val == required.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_if_verbose(H,T,L) :-
	problog_flag(verbose,true),
	!,
	format(H,T,L).
format_if_verbose(_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Should go to dtproblog.yap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
signal_decision(ClauseID,GroundID) :-
	(decision_fact(ClauseID,_) ->
		bb_get(decisions,S),
		ord_insert(S, GroundID, S2),
		bb_put(decisions,S2)
	;
		true
	).

				%
				% ProbLog in-memory inference
				%
:- include(problog_lbdd).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Term Expansion for user predicates
% Must come after clauses for '::'/2 and term_expansion_intern/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:term_expansion(Term,ExpandedTerm) :-
	Term \== end_of_file,
	prolog_load_context(module,Mod),
	problog:term_expansion_intern(Term,Mod,ExpandedTerm).

%% @}

