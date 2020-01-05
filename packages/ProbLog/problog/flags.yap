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
%  Theofrastos Mantadelis, Bernd Gutmann
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

%% @file problog/flags.yap

:-module(flags, [problog_define_flag/4,
                         problog_define_flag/5,
                         problog_define_flag/6,
                         problog_defined_flag/5,
                         problog_defined_flag_group/1,
                         set_problog_flag/2,
                         reset_problog_flags/0,
                         problog_flag/2]).

:- use_module(gflags).
:- use_module(os).
:- use_module(logger).
:- use_module(library(system), [file_exists/1, delete_file/1]).


/** @defgroup ProbLogMiscellaneous ProbLog Miscellaneous Predicates
@ingroup problog
@{


Both the learning and the inference module have various parameters, or flags, that can be adjusted by the user.
The following predicates are defined by ProbLog to access and set these flags.

*/

/**
 * @pred problog_flags
 *
This predicate lists all the flags name, value, domain and description.
*/


/**  @pred problog_flag(+Name, -Value)

This predicate gives the value of the flag with the specified name. The supported flags are:

+ use_db_trie

    Flag telling whether to use the builtin trie to trie transformation.
The possible values for this flag are true or false.

+ db_trie_opt_lvl

    Sets the optimization level for the trie to trie transformation
The possible values for this flag are any integer

+ compare_opt_lvl

    Flag telling whether to use comparison mode for the optimization level.
The possible values for this flag are true or false.

+ db_min_prefix

    Sets the minimum size of the prefix for dbtrie to optimize.
The possible values for this flag are any integer

+ use_naive_trie

    Flag telling whether to use the naive algorithm to generate bdd scripts.
The possible values for this flag are true or false.

+ use_old_trie

    Flag telling whether to use the old not nested trie to trie transformation.
The possible values for this flag are true or false.

+ use_dec_trie

    Flag telling whether to use the decomposition method.
The possible values for this flag are true or false.

+ subset_check

    Flag telling whether to perform subset check in nested tries.
The possible values for this flag are true or false.

+ deref_terms

    Flag telling whether to dereference BDD terms after their last use.
The possible values for this flag are true or false.

+ trie_preprocess

    Flag telling whether to perform a preprocess step to nested tries.
The possible values for this flag are true or false.

+ refine_anclst

    Flag telling whether to refine the ancestor list with their children.
The possible values for this flag are true or false.

+ anclst_represent

    Flag that sets the representation of the ancestor list.
The possible values for this flag are list or integer

+ max_depth

    Sets the maximum proof depth.
The possible values for this flag are any integer.

+ retain_tables

    Flag telling whether to retain tables after the query.
The possible values for this flag are true or false.

+ mc_batchsize

    Flag related to Monte Carlo Sampling that sets the number of samples before update.
The possible values for this flag are any integer greater than zero.

+ min_mc_samples

    Flag related to Monte Carlo Sampling that sets the minimum number of samples before convergence. The possible values for this flag are any integer greater than or equal to zero.

+ max_mc_samples

    Flag related to Monte Carlo Sampling that sets the maximum number of samples waiting to converge.
The possible values for this flag are any integer greater than or equal to zero.

+ randomizer

    Flag related to Monte Carlo Sampling telling whether the random numbers are repeatable or not.
The possible values for this flag are repeatable or nonrepeatable.

+ search_method

    Flag related to DNF Monte Carlo Sampling that sets the search method for picking the proof.
The possible values for this flag are linear or binary.

+ represent_world

    Flag related to Monte Carlo Sampling that sets the structure that represents sampled world.
The possible values for this flag are list, record, array or hash_table

+ first_threshold

    Flag related to inference that sets the starting threshold of iterative deepening.
The possible values for this flag are a number in the interval (0,1).

+ last_threshold

    Flag related to inference that sets the stopping threshold of iterative deepening.
The possible values for this flag are a number in the interval (0,1).

+ id_stepsize

    Flag related to inference that sets the threshold shrinking factor of iterative deepening.
The possible values for this flag are a number in the interval [0,1].

+ prunecheck

    Flag related to inference telling whether to stop derivations including all facts of known proofs.
The possible values for this flag are on or off.

+ maxsteps

    Flag related to inference that sets the max. number of prob. steps per derivation.
The possible values for this flag are any integer greater than zero.

+ mc_logfile

    Flag related to MCMC that sets the logfile for montecarlo.
The possible values for this flag are any valid filename.

+ bdd_time

    Flag related to BDD that sets the BDD computation timeout in seconds.
The possible values for this flag are any integer greater than zero.

+ bdd_par_file

    Flag related to BDD that sets the file for BDD variable parameters.
The possible values for this flag are any valid filename.

+ bdd_result

    Flag related to BDD that sets the file to store result calculated from BDD.
The possible values for this flag are any valid filename.

+ bdd_file

    Flag related to BDD that sets the file for the BDD script.
The possible values for this flag are any valid filename.

+ save_bdd

    Flag related to BDD telling whether to save BDD files for (last) lower bound.
The possible values for this flag are true or false.

+ dynamic_reorder

    Flag related to BDD telling whether to use dynamic re-ordering for BDD.
The possible values for this flag are true or false.

+ bdd_static_order

    Flag related to BDD telling whether to use static order.
The possible values for this flag are true or false.

+ static_order_file

    Flag related to BDD that sets the file for BDD static order.
The possible values for this flag are any valid filename.

+ verbose

    Flag telling whether to output intermediate information.
The possible values for this flag are true or false.

+ show_proofs

    Flag telling whether to output proofs.
The possible values for this flag are true or false.

+ triedump

    Flag telling whether to generate the file: trie_file containing the trie structure.
The possible values for this flag are true or false.

+ dir

    Flag telling the location of the output files directory.
The possible values for this flag are any valid directory name.

*/

/** @pred set_problog_flag(+Name, +Value)

the predicate sets the value of the given flag. The supported flags are the ones listed in above
*/

/** @pred learning_flags

the predicate sets the value of the given flag. The supported flags are the ones listed in above
*/

/** @pred learning_flag(+Name, -Value)

This predicate gives the value of the learning flag with the specified name. The supported flags are:

+ output_directory

    Flag setting the directory where to store results.
The possible values for this flag are any valid path name.

+ query_directory

    Flag setting the directory where to store BDD files.
The possible values for this flag are any valid path name.

+ verbosity_level

    Flag telling how much output shall be given.
The possible values for this flag are an integer between 0 and 5 (0=nothing, 5=all).

+ reuse_initialized_bdds

    Flag telling whether to reuse BDDs from previous runs.
The possible values for this flag are true or false.

+ rebuild_bdds

    Flag telling whether to rebuild BDDs every nth iteration.
The possible values for this flag are any integer greater or equal to zero (0=never).

+ check_duplicate_bdds

    Flag telling whether to store intermediate results in hash table.
The possible values for this flag are true or false.

+ init_method

    Flag setting the ProbLog predicate to search proofs.
The possible values for this flag are of the form: (+Query,-P,+BDDFile,+ProbFile,+Call). For example: A,B,C,D,problog_kbest_save(A,100,B,E,C,D)

+ probability_initializer

    Flag setting the ProbLog predicate to initialize probabilities.
The possible values for this flag are of the form: (+FactID,-P,+Call). For example: A,B,random_probability(A,B)

+ log_frequency

    Flag telling whether to log results every nth iteration.
The possible values for this flag are any integer greater than zero.

+ alpha

    Flag setting the weight of negative examples.
The possible values for this flag are number or "auto" (auto=n_p/n_n).

+ slope

    Flag setting the slope of the sigmoid function.
The possible values for this flag are any real number greater than zero.

+ learning_rate

    Flag setting the default Learning rate (if line_search=false)
The possible values for this flag are any number greater than zero or "examples``

+ line_search

    Flag telling whether to use line search to estimate the learning rate.
The possible values for this flag are true or false.

+ line_search_tau

    Flag setting the Tau value for line search.
The possible values for this flag are a number in the interval (0,1).

+ line_search_tolerance

    Flag setting the tolerance value for line search.
The possible values for this flag are any number greater than zero.

+ line_search_interval

    Flag setting the interval for line search.

*/


:- meta_predicate problog_flag_define(+, :, +, +),
		  problog_flag_define(+, +, :, +, +),
		  problog_flag_define(+, +, :, +, :, +).

problog_define_flag(Flag, Type, Description, DefaultValue):-
  flag_define(Flag, Type, DefaultValue, Description).

problog_define_flag(Flag, Type, Description, DefaultValue, FlagGroup):-
  flag_define(Flag, FlagGroup, Type, DefaultValue, Description).

problog_define_flag(Flag, Type, Description, DefaultValue, FlagGroup, Handler):-
  flag_define(Flag, FlagGroup, Type, DefaultValue, Handler, Description).

problog_defined_flag(Flag, Group, DefaultValue, Domain, Message):-
  flag_defined(Flag, Group, DefaultValue, Domain, Message).

problog_defined_flag_group(Group):-
  flag_group_defined(Group).

set_problog_flag(Flag, Value):-
  flag_set(Flag, Value).

problog_flag(Flag, Value):-
  flag_get(Flag, Value).

reset_problog_flags:- flags_reset.

:- initialization((
  flag_add_validation_syntactic_sugar(problog_flag_validate_dummy, flag_validate_dummy),
  flag_add_validation_syntactic_sugar(problog_flag_validate_atom, flag_validate_atom),
  flag_add_validation_syntactic_sugar(problog_flag_validate_atomic, flag_validate_atomic),
  flag_add_validation_syntactic_sugar(problog_flag_validate_number, flag_validate_number),
  flag_add_validation_syntactic_sugar(problog_flag_validate_integer, flag_validate_integer),
  flag_add_validation_syntactic_sugar(problog_flag_validate_directory, flag_validate_directory),
  flag_add_validation_syntactic_sugar(problog_flag_validate_file, flag_validate_file),
  flag_add_validation_syntactic_sugar(problog_flag_validate_in_list(L), flag_validate_in_list(L)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_in_interval(I, Type), flag_validate_in_interval(I, Type)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_in_interval_closed([L, U]), flag_validate_in_interval([L, U], number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_in_interval_open([L, U]), flag_validate_in_interval((L, U), number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_in_interval_left_open([L, U]), flag_validate_in_interval((L, [U]), number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_in_interval_right_open([L, U]), flag_validate_in_interval(([L], U), number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_integer_in_interval_closed([L, U]), flag_validate_in_interval([L, U], integer)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_integer_in_interval_open([L, U]), flag_validate_in_interval((L, U), integer)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_integer_in_interval_left_open([L, U]), flag_validate_in_interval((L, [U]), integer)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_integer_in_interval_right_open([L, U]), flag_validate_in_interval(([L], U), integer)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_float_in_interval_closed([L, U]), flag_validate_in_interval([L, U], float)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_float_in_interval_open([L, U]), flag_validate_in_interval((L, U), float)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_float_in_interval_left_open([L, U]), flag_validate_in_interval((L, [U]), float)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_float_in_interval_right_open([L, U]), flag_validate_in_interval(([L], U), float)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_posnumber, flag_validate_in_interval((0, [+inf]), number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_posint, flag_validate_in_interval((0, +inf), integer)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_nonegint, flag_validate_in_interval(([0], +inf), integer)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_boolean, flag_validate_in_list([true, false])),
  flag_add_validation_syntactic_sugar(problog_flag_validate_switch, flag_validate_in_list([on, off])),
  flag_add_validation_syntactic_sugar(problog_flag_validate_method, flag_validate_in_list([max, delta, exact, montecarlo, low, kbest])),
  flag_add_validation_syntactic_sugar(problog_flag_validate_aggregate, flag_validate_in_list([sum, prod, soft_prod])),
  flag_add_validation_syntactic_sugar(problog_flag_validate_indomain_0_1_open, flag_validate_in_interval((0, 1), number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_indomain_0_1_close, flag_validate_in_interval([0, 1], number)),
  flag_add_validation_syntactic_sugar(problog_flag_validate_0to5, flag_validate_in_interval([0, 5], integer))
)).

last_threshold_handler(message, '').
last_threshold_handler(validating, _Value).
last_threshold_handler(validated, _Value).
last_threshold_handler(stored, Value):-
  ValueLog is log(Value),
  flag_store(last_threshold_log, ValueLog).

id_stepsize_handler(message, '').
id_stepsize_handler(validating, _Value).
id_stepsize_handler(validated, _Value).
id_stepsize_handler(stored, Value):-
  ValueLog is log(Value),
  flag_store(id_stepsize_log, ValueLog).

bdd_file_handler(message, '').
bdd_file_handler(validating, _Value).
bdd_file_handler(validate, Value):-
  convert_filename_to_working_path(Value, Path),
  catch(file_exists(Path), _, fail), file_property(Path, type(regular)), !.
bdd_file_handler(validate, Value):-
  convert_filename_to_working_path(Value, Path),
  catch((\+ file_exists(Path), tell(Path)), _, fail),
  told, 
  catch(delete_file(Path),_, fail).
bdd_file_handler(validated, _Value).
bdd_file_handler(stored, Value):-
  atomic_concat(Value, '_probs', ParValue),
  flag_set(bdd_par_file, ParValue),
  atomic_concat(Value, '_res', ResValue),
  flag_set(bdd_result, ResValue).

working_file_handler(message, '').
working_file_handler(validating, _Value).
working_file_handler(validate, Value):-
  convert_filename_to_working_path(Value, Path),
  catch(file_exists(Path), _, fail), file_property(Path, type(regular)), !.
working_file_handler(validate, Value):-
  convert_filename_to_working_path(Value, Path),
  catch((\+ file_exists(Path), tell(Path)), _, fail),
  told, 
  catch(delete_file(Path),_, fail).
working_file_handler(validated, _Value).
working_file_handler(stored, _Value).

auto_handler(message, 'auto non-zero').
auto_handler(validating, Value) :-
	number(Value),
	Value =\= 0.
auto_handler(validate, Value):-
  Value == auto.
auto_handler(validated, _Value).
auto_handler(stored, _Value).


examples_handler(message, 'examples').
examples_handler(validating, _Value).
examples_handler(validate, Value):-
  Value == examples.
examples_handler(validated, _Value).
examples_handler(stored, _Value).


learning_init_handler(message, '(Q,P,BDDFile,ProbFile,Query)').
learning_init_handler(validating, (_,_,_,_,_)).
%learning_init_handler(validate, V_).
learning_init_handler(validated, _Value).
learning_init_handler(stored, _Value).

learning_libdd_init_handler(message, '(Q,BDD,Query)').
learning_libdd_init_handler(validating, (_,_,_)).
%learning_init_handler(validate, V_).
learning_libdd_init_handler(validated, _Value).
learning_libdd_init_handler(stored, _Value).

learning_prob_init_handler(message, '(0,1] or uniform(l,h) ').
learning_prob_init_handler(validating, uniform(Low,High)) :-
	number(Low),
	number(High),
	Low<High,
	Low>0,
	High =< 1.
learning_prob_init_handler(validating, N) :-
	number(N),
	N>0,
	N =< 1.
learning_prob_init_handler(validating, A) :-
       atom(A), !.
%learning_prob_init_handler(validate, V_).
learning_prob_init_handler(validated, _Value).
learning_prob_init_handler(stored, _Value).


linesearch_interval_handler(message,'nonempty interval(L,H)').
linesearch_interval_handler(validating,V):-
	V=(L,H),
	number(L),
	number(H),
	L<H.
%linesearch_interval_handler(validate,_).
linesearch_interval_handler(validated,_).
linesearch_interval_handler(stored,_).



learning_output_dir_handler(message, '').
learning_output_dir_handler(validating, _Value).
learning_output_dir_handler(validated, _Value).
learning_output_dir_handler(stored, Value):-
	concat_path_with_filename(Value,'out.dat',Filename),
	logger_set_filename(Filename).

/*
problog_flag_validate_learninginit
problog_flag_validate_interval


validation_type_values(problog_flag_validate_learninginit,'(QueryID,P, BDD,Probs,Call)').

validation_type_values(problog_flag_validate_learningprobinit,'(FactID,P,Call)').

validation_type_values(problog_flag_validate_interval,'any nonempty interval (a,b)').


problog_flag_validate_interval.
problog_flag_validate_interval( (V1,V2) ) :-
  number(V1),
  number(V2),
  V1<V2.

*/

%% @}
