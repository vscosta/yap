%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2010-08-30 18:09:17 +0200 (Mon, 30 Aug 2010) $
%  $Revision: 4728 $
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
% ProbLog inference
%
% assumes probabilistic facts as Prob::Fact and clauses in normal Prolog format
%
% provides following inference modes (16/12/2008):
% - approximation with interval width Delta (IJCAI07): problog_delta(+Query,+Delta,-Low,-High,-Status)
% - bounds based on single probability threshold: problog_threshold(+Query,+Threshold,-Low,-High,-Status)
% - as above, but lower bound only: problog_low(+Query,+Threshold,-Low,-Status)
% - lower bound based on K most likely proofs: problog_kbest(+Query,+K,-Low,-Status)
% - explanation probability (ECML07): problog_max(+Query,-Prob,-FactsUsed)
% - exact probability: problog_exact(+Query,-Prob,-Status)
% - sampling: problog_montecarlo(+Query,+Delta,-Prob)
%
%
% angelika.kimmig@cs.kuleuven.be
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(problog, [problog_delta/5,
                    problog_threshold/5,
                    problog_low/4,
                    problog_kbest/4,
                    problog_kbest_save/6,
                    problog_max/3,
                    problog_exact/3,
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
                    init_problog/1,
                    problog_call/1,
                    problog_infer_forest_supported/0,
                    problog_bdd_forest_supported/0,
                    problog_real_kbest/4,
                    op( 550, yfx, :: ),
                    op( 550, fx, ?:: ),
                    op( 1150, fx, problog_table ),
                    in_interval/3,
                    below/2,
                    above/2]).

:- style_check(all).

:- yap_flag(unknown,error).

:- set_prolog_flag(to_chars_mode,quintus).

% general yap modules
:- ensure_loaded(library(charsio)).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(random)).
:- ensure_loaded(library(system)).
:- ensure_loaded(library(rbtrees)).
:- ensure_loaded(library(ordsets)).

% problog related modules
:- ensure_loaded('problog/variables').
:- ensure_loaded('problog/extlists').
:- ensure_loaded('problog/flags').
:- ensure_loaded('problog/print').
:- ensure_loaded('problog/os').
:- ensure_loaded('problog/tptree').
:- ensure_loaded('problog/tabling').
:- ensure_loaded('problog/sampling').
:- ensure_loaded('problog/intervals').
:- ensure_loaded('problog/mc_DNF_sampling').
:- catch(ensure_loaded('problog/variable_elimination'),_,true).

% op attaching probabilities to facts
:- op( 550, yfx, :: ).
:- op( 550, fx, ?:: ).


%%%%%%%%%%%%%%%%%%%%%%%%
% control predicates on various levels
%%%%%%%%%%%%%%%%%%%%%%%%

% global over all inference methods, internal use only
:- dynamic problog_predicate/2.
:- dynamic problog_continuous_predicate/3.
% global over all inference methods, exported
:- dynamic tunable_fact/2.
:- dynamic non_ground_fact/1.
:- dynamic continuous_fact/1.
%:- dynamic problog_dir/1.
% global, manipulated via problog_control/2
:- dynamic up/0.
:- dynamic limit/0.
:- dynamic mc/0.
:- dynamic remember/0.
:- dynamic exact/0.                         % Theo tabling
:- dynamic find_decisions/0.
:- dynamic internal_strategy/0.
% local to problog_delta
:- dynamic low/2.
:- dynamic up/2.
:- dynamic stopDiff/1.
% local to problog_kbest
:- dynamic current_kbest/3.
% local to problog_max
:- dynamic max_probability/1.
:- dynamic max_proof/1.
% local to problog_montecarlo
:- dynamic mc_prob/1.
% local to problog_answers
:- dynamic answer/1.
% to keep track of the groundings for non-ground facts
:- dynamic grounding_is_known/2.

% for decisions
:- dynamic decision_fact/2.

% for fact where the proabability is a variable
:- dynamic dynamic_probability_fact/1.
:- dynamic dynamic_probability_fact_extract/2.

% for storing continuous parts of proofs (Hybrid ProbLog)
:- dynamic hybrid_proof/3, hybrid_proof/4.
:- dynamic hybrid_proof_disjoint/4.

% ProbLog files declare prob. facts as P::G
% and this module provides the predicate X::Y to iterate over them
:- multifile '::'/2.


% directory where problogbdd executable is located
% automatically set during loading -- assumes it is in same place as this file (problog.yap)
:- getcwd(PD), set_problog_path(PD).





%%%%%%%%%%%%
% iterative deepening on minimal probabilities (delta, max, kbest):
% - first threshold (not in log-space as only used to retrieve argument for init_threshold/1, which is also used with user-supplied argument)
% - last threshold to ensure termination in case infinite search space (saved also in log-space for easy comparison with current values during search)
% - factor used to decrease threshold for next level, NewMin=Factor*OldMin (saved also in log-space)
%%%%%%%%%%%%

:- problog_define_flag(first_threshold, problog_flag_validate_indomain_0_1_open, 'starting threshold iterative deepening', 0.1, inference).
:- problog_define_flag(last_threshold,  problog_flag_validate_indomain_0_1_open, 'stopping threshold iterative deepening', 1e-30, inference, flags:last_threshold_handler).
:- problog_define_flag(id_stepsize,     problog_flag_validate_indomain_0_1_close, 'threshold shrinking factor iterative deepening', 0.5, inference, flags:id_stepsize_handler).

%%%%%%%%%%%%
% prune check stops derivations if they use a superset of facts already known to form a proof
% (very) costly test, can be switched on/off here (This is obsolete as it is not included in implementation)
%%%%%%%%%%%%

:- problog_define_flag(prunecheck,      problog_flag_validate_switch, 'stop derivations including all facts of known proof', off, inference).

%%%%%%%%%%%%
% max number of calls to probabilistic facts per derivation (to ensure termination)
%%%%%%%%%%%%

:- problog_define_flag(maxsteps,        problog_flag_validate_posint, 'max. number of prob. steps per derivation', 1000, inference).

%%%%%%%%%%%%
% BDD timeout in seconds, used as option in BDD tool
% files to write BDD script and pars
% bdd_file overwrites bdd_par_file with matching extended name
% if different name wanted, respect order when setting
% save BDD information for the (last) lower bound BDD used during inference
% produces three files named save_script, save_params, save_map
% located in the directory given by problog_flag dir
%%%%%%%%%%%%

%:- problog_define_flag(bdd_path,        problog_flag_validate_directory, 'problogbdd directory', '.',bdd).
:- problog_define_flag(bdd_time,        problog_flag_validate_posint, 'BDD computation timeout in seconds', 60, bdd).
:- problog_define_flag(save_bdd,        problog_flag_validate_boolean, 'save BDD files for (last) lower bound', false, bdd).
:- problog_define_flag(dynamic_reorder, problog_flag_validate_boolean, 'use dynamic re-ordering for BDD', true, bdd).
:- problog_define_flag(bdd_static_order,    problog_flag_validate_boolean, 'use a static order', false, bdd).


%%%%%%%%%%%%
% determine whether ProbLog outputs information (number of proofs, intermediate results, ...)
% default was true, as otherwise problog_delta won't output intermediate bounds
% default is false now, as dtproblog will flood the user with verbosity
%%%%%%%%%%%%

:- problog_define_flag(verbose,         problog_flag_validate_boolean, 'output intermediate information', false,output).

%%%%%%%%%%%%
% determine whether ProbLog outputs proofs when adding to trie
% default is false
%%%%%%%%%%%%

:- problog_define_flag(show_proofs,     problog_flag_validate_boolean, 'output proofs', false,output).

%%%%%%%%%%%%
% Trie dump parameter for saving a file with the trie structure in the directory by problog_flag dir
%%%%%%%%%%%%

:- problog_define_flag(triedump,        problog_flag_validate_boolean, 'generate file: trie_file containing the trie structure', false,output).

%%%%%%%%%%%%
% Default inference method
%%%%%%%%%%%%

:- problog_define_flag(inference,        problog_flag_validate_dummy, 'default inference method', exact, inference).


problog_dir(PD):- problog_path(PD).

%%%%%%%%%%%%%%%%%%%%%%%%
% initialization of global parameters
%%%%%%%%%%%%%%%%%%%%%%%%

init_global_params :-
  %grow_atom_table(1000000),
  getcwd(Work),
  concat_path_with_filename(Work, output, WorkDir),
  %%%%%%%%%%%%
  % working directory: all the temporary and output files will be located there
  % it assumes a subdirectory of the current working dir
  % on initialization, the current dir is the one where the user's file is located
  % should be changed to use temporary folder structure of operating system
  %%%%%%%%%%%%
  tmpname(TempFolder),
  atomic_concat([TempFolder, '_problog'], TempProblogFolder),
  problog_define_flag(dir, problog_flag_validate_directory, 'directory for files', WorkDir, TempProblogFolder),
  check_existance('problogbdd').

check_existance(FileName):-
  convert_filename_to_problog_path(FileName, Path),
  catch(file_exists(Path), _, fail).
check_existance(FileName):-
  problog_path(PD),
  write(user_error, 'WARNING: Can not find file: '), write(user_error, FileName),
  write(user_error, ', please place file in problog path: '), write(user_error, PD), nl(user_error).

% parameter initialization to be called after returning to user's directory:
:- initialization(init_global_params).

:- problog_define_flag(bdd_par_file,    problog_flag_validate_file, 'file for BDD variable parameters', example_bdd_probs, bdd).
:- problog_define_flag(bdd_result,      problog_flag_validate_file, 'file to store result calculated from BDD', example_bdd_res, bdd).
:- problog_define_flag(bdd_file,        problog_flag_validate_file, 'file for BDD script', example_bdd, bdd, flags:bdd_file_handler).
:- problog_define_flag(static_order_file,    problog_flag_validate_file, 'file for BDD static order', example_bdd_order, bdd).

%%%%%%%%%%%%
% montecarlo: recalculate current approximation after N samples
% montecarlo: write log to this file
%%%%%%%%%%%%

:- problog_define_flag(mc_logfile,      problog_flag_validate_file, 'logfile for montecarlo', 'log.txt', mcmc).


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
	assert(X).
problog_control(off,X) :-
	retractall(X).
problog_control(check,X) :-
	call(X).

reset_control :-
	problog_control(off,up),
	problog_control(off,mc),
	problog_control(off,limit),
%   problog_control(off,exact),
	problog_control(off,remember).

:- reset_control.

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

% converts ?:: prefix to ? :: infix, as handled by other clause
term_expansion_intern((Annotation::Fact), Module, ExpandedClause) :-
	Annotation == '?',
	term_expansion_intern((? :: Fact :- true), Module, ExpandedClause).


% handles decision clauses
term_expansion_intern((Annotation :: Head :- Body), Module, problog:ExpandedClause) :-
	(
	 Annotation == '?' ->
    % It's a decision with a body
	 copy_term((Head,Body),(HeadCopy,_BodyCopy)),
	 functor(Head, Functor, Arity),
	 atomic_concat([problog_,Functor],LongFunctor),
	 Head =.. [Functor|Args],
	 append(Args,[LProb],LongArgs),
	 probclause_id(ID),
	 ProbFactHead =.. [LongFunctor,ID|LongArgs],
	 assert(decision_fact(ID,Head)),
	 ExpandedClause = (ProbFactHead :-
			  user:Body,
			   (problog_control(check,internal_strategy) ->
			    dtproblog:strategy_log(ID,Head,LProb)
			   ;
			    LProb = '?'
			   )
			  ),
	 assert(dynamic_probability_fact(ID)),
	 assert((dynamic_probability_fact_extract(HeadCopy,P_New) :-
		dtproblog:strategy(ID,HeadCopy,P_New)
		)),
	 (ground(Head) ->
	  true
	 ;
	  assert(non_ground_fact(ID))
	 ),
	 problog_predicate(Functor, Arity, LongFunctor, Module)
	;
				% If it has a body, it's not supported
	 (Body == true ->
				% format('Expanding annotated fact ~q :: ~q :- ~q in other clause.~n',[Annotation,Head,Body]),
	  fail
	 ;
	  throw(error('We do not support annotated clauses (yet)!', (Annotation :: Head :- Body)))
	 )
	).


/* this can slow down prolog time by several orders if there's lots of them
user:term_expansion(P::Goal,Goal) :-
	P \= t(_),
	P =:= 1,
	!.
*/

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
	  assert(tunable_fact(ID,TrueProb)),
	  LProb is log(random*0.9+0.05)	% set unknown probability randomly in [0.05, 0.95]
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
	   assert(dynamic_probability_fact(ID)),
	   assert(dynamic_probability_fact_extract(Goal_Copy,P_Copy))
	  )
	 )
	),
	(
	 ground(Goal)
	->
	 true;
	 assert(non_ground_fact(ID))
	),
	problog_predicate(Name, Arity, ProblogName,Module).






% Hybrid ProbLog stuff

is_valid_gaussian(X) :-
	compound(X),
	X=gaussian(Mu,Sigma),
	(
	 ((number(Mu),number(Sigma));(Mu=t(_),Sigma=t(_)))
	->
	 true;
	 throw(invalid_gaussian(X))
	).

:- multifile(user:term_expansion/1).

user:term_expansion(Goal, problog:ProbFact) :-
	compound(Goal),
	Goal=..[Name|Args],
	once( (nth(Pos,Args,GaussianArg),is_valid_gaussian(GaussianArg)) ),

	%Goal contains a Gaussian, there is some work to do

	( % check for a second Gaussian
	 (nth(Pos2,Args,GaussianArg2),Pos2\=Pos,is_valid_gaussian(GaussianArg2))
	->
	  (
	   format(user_error,'We only support continous atoms with at most one Gaussian inside.~n',[]),
	   format(user_error,'Your program contains the atom ~w with more than one.~n',[]),
	   throw(unsupported_multivariate_gaussian(Goal))
	  );
	  true
	),

	functor(Goal, Name, Arity),
	atomic_concat([problogcontinuous_,Name],ProblogName),
	probclause_id(ID),

	GaussianArg=gaussian(Mu_Arg,Sigma_Arg),

	% is it a tunable fact?
	(
	 (number(Mu_Arg),number(Sigma_Arg))
	->
	 NewArgs=Args;
	 (
	  Mu_Random is 0.1, % random*4-2,
	  Sigma_Random is 0.4, % random*2+0.5,
	  nth(Pos,Args,_,KeepArgs),
	  nth(Pos,NewArgs,gaussian(Mu_Random,Sigma_Random),KeepArgs),
	  assert(tunable_fact(ID,gaussian(Mu_Arg,Sigma_Arg)))
	 )
	),
	ProbFact =.. [ProblogName,ID|NewArgs],

	(
	 ground(Goal)
	->
	 true;
	 assert(non_ground_fact(ID))
	),
	assert(continuous_fact(ID)),
	problog_continuous_predicate(Name, Arity, Pos,ProblogName).


% introduce wrapper clause if predicate seen first time
problog_continuous_predicate(Name, Arity,ContinuousArgumentPosition,_) :-
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
problog_continuous_predicate(Name, Arity, ContinuousArgumentPosition, ProblogName) :-

	LBefore is ContinuousArgumentPosition-1,
	LAfter is Arity-ContinuousArgumentPosition,

	length(ArgsBefore,LBefore),
	length(ArgsAfter,LAfter),
	append(ArgsBefore,[(ID,ID2,GaussianArg)|ArgsAfter],Args),
	append(ArgsBefore,[GaussianArg|ArgsAfter],ProbArgs),

	OriginalGoal =.. [Name|Args],


	ProbFact =.. [ProblogName,ID|ProbArgs],
	prolog_load_context(module,Mod),

	assert( (Mod:OriginalGoal :- ProbFact,
		                   % continuous facts always get a grounding ID, even when they are actually ground
		                   % this simplifies the BDD script generation
		                     non_ground_fact_grounding_id(ProbFact,Ground_ID),
		                     atomic_concat([ID,'_',Ground_ID],ID2),
		                     add_continuous_to_proof(ID,ID2)
		 )),

	assert(problog_continuous_predicate(Name, Arity,ContinuousArgumentPosition)),
	ArityPlus1 is Arity+1,
	dynamic(problog:ProblogName/ArityPlus1).


in_interval(ID,Low,High) :-
	number(Low),
	number(High),
	Low<High,
	interval_merge(ID,interval(Low,High)).
below(ID,X) :-
	number(X),
	interval_merge(ID,below(X)).
above(ID,X) :-
	number(X),
	interval_merge(ID,above(X)).

interval_merge((_ID,GroundID,_Type),Interval) :-
	atomic_concat([interval,'_',GroundID],Key),
	b_getval(Key,OldInterval),
	intervals_merge(OldInterval,Interval,NewInterval),
	NewInterval \= none,
	NewInterval \= interval(Bound,Bound),
	b_setval(Key,NewInterval).




problog_assert(P::Goal) :-
	problog_assert(user,P::Goal).
problog_assert(Module, P::Goal) :-
	term_expansion_intern(P::Goal,Module,problog:ProbFact),
	assert(problog:ProbFact).

problog_retractall(Goal) :-
	Goal =.. [F|Args],
	append([_ID|Args],[_Prob],Args2),
	atomic_concat(['problog_',F],F2),
	ProbLogGoal=..[F2|Args2],
	retractall(problog:ProbLogGoal).


% introduce wrapper clause if predicate seen first time
problog_predicate(Name, Arity, _,_) :-
	problog_predicate(Name, Arity), !.

problog_predicate(Name, Arity, ProblogName,Mod) :-
	functor(OriginalGoal, Name, Arity),
	OriginalGoal =.. [_|Args],
	append(Args,[Prob],L1),
	ProbFact =.. [ProblogName,ID|L1],
	assert( (Mod:OriginalGoal :-
                ProbFact,
                grounding_id(ID,OriginalGoal,ID2),
				prove_problog_fact(ID,ID2,Prob)
		 )),

	assert( (Mod:problog_not(OriginalGoal) :-
                ProbFact,
                grounding_id(ID,OriginalGoal,ID2),
                prove_problog_fact_negated(ID,ID2,Prob)
		 )),
	assert(problog_predicate(Name, Arity)),
	ArityPlus2 is Arity+2,
	dynamic(problog:ProblogName/ArityPlus2).


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
    (Prob = '?' ->
      add_to_proof(GroundID,0) % 0 is log(1)!
    ;
      % Checks needed for LeDTProbLog
      (Prob = always ->
        % Always true, do not add to trie
        true
      ;
        (Prob = never ->
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
      (Prob = '?' ->
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
:- nb_setval(probclause_counter,0).
probclause_id(ID) :-
	nb_getval(probclause_counter,ID), !,
	C1 is ID+1,
	nb_setval(probclause_counter,C1), !.

non_ground_fact_grounding_id(Goal,ID) :-
	(
	    ground(Goal)
	->
	    true;
	    (
		format(user_error,'The current program uses non-ground facts.~n', []),
		format(user_error,'If you query those, you may only query fully-grounded versions of the fact.~n',[]),
		format(user_error,'Within the current proof, you queried for ~q which is not ground.~n~n', [Goal]),
		throw(error(non_ground_fact(Goal)))
	    )
	),
	(
	    grounding_is_known(Goal,ID)
	->
	    true;
	    (
		nb_getval(non_ground_fact_grounding_id_counter,ID),
		ID2 is ID+1,
		nb_setval(non_ground_fact_grounding_id_counter,ID2),
		assert(grounding_is_known(Goal,ID))
	    )
	).

reset_non_ground_facts :-
  (required(keep_ground_ids) ->
      true
  ;
      nb_setval(non_ground_fact_grounding_id_counter,0),
      retractall(grounding_is_known(_,_))
  ).

:- initialization(reset_non_ground_facts).

% backtrack over all probabilistic facts
% must come before term_expansion
P::Goal :-
    probabilistic_fact(P,Goal,_).

% backtrack over all probabilistic facts
probabilistic_fact(P2,Goal,ID) :-
	(
	 ground(Goal)
	->
	 (
	  Goal =.. [F|Args],
	  atomic_concat('problog_',F,F2),
	  append([ID|Args],[P],Args2),
	  Goal2 =..[F2|Args2],
	  length(Args2,N),
	  current_predicate(F2/N),
	  call(Goal2),
	  number(P),
	  P2 is exp(P)
	 );
	 (
	  get_internal_fact(ID,ProblogTerm,_ProblogName,_ProblogArity),
	  ProblogTerm =.. [F,_ID|Args],
	  append(Args2,[P],Args),
	  atom_codes(F,[_p,_r,_o,_b,_l,_o,_g,_|F2Chars]),
	  atom_codes(F2,F2Chars),
	  Goal =.. [F2|Args2],
	  (
	   dynamic_probability_fact(ID)
	  ->
	   P2=p;
	   P2 is exp(P)
	  )
	 )
	).


% generates unique IDs for proofs
proof_id(ID) :-
	nb_getval(problog_proof_id,ID),
	ID2 is ID+1,
	nb_setval(problog_proof_id,ID2).

reset_problog_proof_id :-
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
  not(number(A)),
  atom_codes(A, A_Codes),
  once(append(Part1, [95|Part2], A_Codes)), % 95 = '_'
  number_codes(ID, Part1), !,
  % let's check whether Part2 contains an 'l' (l=low)
  (member(108, Part2) ->
    fail
  ;
    number_codes(Grounding_ID, Part2),
    (dynamic_probability_fact(ID) ->
      grounding_is_known(Goal, Grounding_ID),
      dynamic_probability_fact_extract(Goal, Prob)
    ;
      get_fact_probability(ID, Prob)
    )
  ).
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
  (Log = '?' ->
      throw(error('Why do you want to know the probability of a decision?')) %fail
  ;
      Prob is exp(Log)
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
  Prob \== '?'.
get_fact_log_probability(ID,Prob) :-
	get_fact_probability(ID,Prob1),
	Prob is log(Prob1).

set_fact_probability(ID,Prob) :-
	get_internal_fact(ID,ProblogTerm,ProblogName,ProblogArity),
	retract(ProblogTerm),
	ProblogTerm =.. [ProblogName|ProblogTermArgs],
	nth(ProblogArity,ProblogTermArgs,_,KeepArgs),
	NewLogProb is log(Prob),
	nth(ProblogArity,NewProblogTermArgs,NewLogProb,KeepArgs),
	NewProblogTerm =.. [ProblogName|NewProblogTermArgs],
	assert(NewProblogTerm).

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
	assert(NewProblogTerm).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writing those facts with learnable parameters to File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_facts(Filename) :-
	open(Filename,'write',Handle),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all probabilistic facts
	  P::Goal,
	  format(Handle,'~w :: ~q.~n',[P,Goal]),

	  fail; % go to next prob. fact
	  true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all continuous facts
	  continuous_fact(ID),
	  get_continuous_fact_parameters(ID,Param),
	  format(Handle,'~q.  % ~q~n',[Param,ID]),

	  fail; % go to next cont. fact
	  true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	close(Handle).

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
	nth(Last,Args,_LogProb,OutsideArgs),
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
add_to_proof(ID, Prob) :-
  (problog_control(check, mc) ->
    montecarlo_check(ID)
  ;
    b_getval(problog_steps,MaxSteps),
    b_getval(problog_probability, CurrentP),
    nb_getval(problog_threshold, CurrentThreshold),
    b_getval(problog_current_proof, IDs),
    %%%% Bernd, changes for negated ground facts
    \+ open_end_memberchk(not(ID),IDs),
    %%%% Bernd, changes for negated ground facts
    (MaxSteps =< 0 ->
      fail
    ;
      (open_end_memberchk(ID, IDs) -> %Theo
        true
      ;
        open_end_add(ID, IDs, NIDs), %Theo
        % \+ prune_check(NIDs, Trie_Completed_Proofs),
        multiply_probabilities(CurrentP, Prob, NProb),
        (NProb < CurrentThreshold ->
          upper_bound(NIDs),
          fail
        ;
          b_setval(problog_probability, NProb),
          b_setval(problog_current_proof, NIDs)
        )
      ),
      Steps is MaxSteps - 1,
      b_setval(problog_steps, Steps)
    )
  ).

%%%% Bernd, changes for negated ground facts
add_to_proof_negated(ID, Prob) :-
  (problog_control(check, mc) ->
    % the sample has to fail if the fact is negated
    \+ montecarlo_check(ID)
  ;
    b_getval(problog_steps, MaxSteps),
    b_getval(problog_probability, CurrentP),
    nb_getval(problog_threshold, CurrentThreshold),
    b_getval(problog_current_proof, IDs),
    \+ open_end_memberchk(ID, IDs),
    (MaxSteps =< 0 ->
      fail
    ;
      (open_end_memberchk(not(ID), IDs) ->
        true
      ;
        open_end_add(not(ID), IDs, NIDs), %Theo
        % \+ prune_check(NIDs, Trie_Completed_Proofs),
        InverseProb is log(1 - exp(Prob)),
        multiply_probabilities(CurrentP, InverseProb, NProb),
        (NProb < CurrentThreshold ->
          upper_bound(NIDs),  %% checkme
          fail
        ;
          b_setval(problog_probability, NProb),
          b_setval(problog_current_proof, NIDs)
        )
      ),
      Steps is MaxSteps - 1,
      b_setval(problog_steps, Steps)
    )
  ).
%%%% Bernd, changes for negated ground facts

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

multiply_probabilities(CurrentLogP, LogProb, NLogProb) :-
  NLogProb is CurrentLogP + LogProb.

% this is called by all inference methods before the actual ProbLog goal
% to set up environment for proving
% it resets control flags, method specific values to be set afterwards!
init_problog(Threshold) :-
	reset_problog_proof_id,
	reset_non_ground_facts,
	reset_control,
	LT is log(Threshold),
	b_setval(problog_probability, 0.0),
	b_setval(problog_current_proof, []),
	nb_setval(problog_threshold, LT),
	problog_flag(maxsteps,MaxS),
	init_tabling,
	problog_var_clear_all,
	b_setval(problog_steps, MaxS),
	b_setval(problog_continuous_facts_used,[]),
	retractall(hybrid_proof(_,_,_)),
	retractall(hybrid_proof(_,_,_,_)),
	retractall(hybrid_proof_disjoint(_,_,_,_)).

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

:- problog_var_define(sld_time, times, time, messages('SLD resolution', ':', ' ms')).
:- problog_var_define(bdd_script_time, times, time, messages('Generating BDD script', ':', ' ms')).
:- problog_var_define(bdd_generation_time, times, time, messages('Constructing BDD', ':', ' ms')).
:- problog_var_define(trie_statistics, memory, untyped, messages('Trie usage', ':', '')).
:- problog_var_define(probability, result, number, messages('Probabilty', ' = ', '')).
:- problog_var_define(bdd_script_time(Method), times, time, messages('Generating BDD script '(Method), ':', ' ms')).
:- problog_var_define(bdd_generation_time(Method), times, time, messages('Constructing BDD '(Method), ':', ' ms')).
:- problog_var_define(probability(Method), result, number, messages('Probabilty '(Method), ' = ', '')).
:- problog_var_define(trie_statistics(Method), memory, untyped, messages('Trie usage '(Method), ':', '')).
:- problog_var_define(dbtrie_statistics(Method), memory, untyped, messages('Depth Breadth Trie usage '(Method), ':', '')).
:- problog_var_define(db_trie_opts_performed(Method), memory, untyped, messages('Optimisations performed '(Method), ':', '')).
:- problog_var_define(variable_elimination_time, times, time, messages('Variable Elimination', ':', ' ms')).
:- problog_var_define(variable_elimination_stats, memory, untyped, messages('Variable Elimination', ':', '')).

problog_statistics(Stat, Result):-
	problog_var_defined(Stat),
	problog_var_is_set(Stat),
	problog_var_get(Stat, Result).

generate_order_by_prob_fact_appearance(Order, FileName):-
  open(FileName, 'write', Stream),
  forall(member(PF, Order), (
    ptree:get_var_name(PF, Name),
    format(Stream, "@~w~n", [Name]))),
/*  findall(_, (recorded(variable_elimination, prob_fact(PF, _), _),
    ptree:get_var_name(PF, Name),
    format(Stream, "@~w~n", [Name])), _),*/
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
  problog_flag(bdd_file, BDDFileFlag),
  convert_filename_to_working_path(BDDFileFlag, BDDFile),
  problog_flag(bdd_par_file, BDDParFileFlag),
  convert_filename_to_working_path(BDDParFileFlag, BDDParFile),
  % old reduction method doesn't support nested tries
  ((problog_flag(use_old_trie, true), nb_getval(problog_nested_tries, false)) ->
    statistics(walltime, _),
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
    statistics(walltime, [_, ScriptGenerationTime]),
    problog_var_set(bdd_script_time, ScriptGenerationTime),

    statistics(walltime, _),
    execute_bdd_tool(BDDFile, BDDParFile, Prob_old, Status_old),
    statistics(walltime,[_, BDDGenerationTime]),
    (Status_old = ok ->
      problog_var_set(bdd_generation_time, BDDGenerationTime),
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
    statistics(walltime, _),
%     atomic_concat([BDDFile, '_naive'], BDDFile_naive),
    BDDFile = BDDFile_naive,
    nested_ptree_to_BDD_script(Trie, BDDFile_naive, BDDParFile),
    statistics(walltime, [_, ScriptGenerationTime_naive]),
    problog_var_set(bdd_script_time(naive), ScriptGenerationTime_naive),

    statistics(walltime, _),
    execute_bdd_tool(BDDFile_naive, BDDParFile, Prob_naive, Status_naive),
    statistics(walltime,[_, BDDGenerationTime_naive]),
    (Status_naive = ok ->
      problog_var_set(bdd_generation_time(naive), BDDGenerationTime_naive),
      problog_var_set(probability(naive), Prob_naive)
    ;
      problog_var_set(bdd_generation_time(naive), fail),
      problog_var_set(probability(naive), fail)
    )
  ;
    true
  ),
%   problog_statistics,
%   print_nested_ptree(Trie),
%   findall(_,(problog_chktabled(_ID, _T), writeln(problog_chktabled(_ID, _T))),_),
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
      statistics(walltime, _),
%       atomic_concat([BDDFile, '_builtin_', OptLevel], BDDFile_builtin),
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
      statistics(walltime, [_, ScriptGenerationTime_builtin]),
      problog_var_set(bdd_script_time(Builtin), ScriptGenerationTime_builtin),

      statistics(walltime, _),
      execute_bdd_tool(BDDFile_builtin, BDDParFile, Prob_builtin, Status_builtin),
      statistics(walltime,[_, BDDGenerationTime_builtin]),
      ptree_db_trie_opt_performed(LVL1, LVL2, LV3),
      problog_var_set(db_trie_opts_performed(Builtin), opt_perform(LVL1, LVL2, LV3)),
      (Status_builtin = ok ->
        problog_var_set(bdd_generation_time(Builtin), BDDGenerationTime_builtin),
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
    statistics(walltime, _),
%     atomic_concat([BDDFile, '_dec'], BDDFile_dec),
    BDDFile = BDDFile_dec,
    ptree_decomposition(Trie, BDDFile_dec, BDDParFile),
    statistics(walltime, [_, ScriptGenerationTime_dec]),
    problog_var_set(bdd_script_time(dec), ScriptGenerationTime_dec),

    statistics(walltime, _),
    execute_bdd_tool(BDDFile_dec, BDDParFile, Prob_dec, Status_dec),
    statistics(walltime,[_, BDDGenerationTime_dec]),
    (Status_dec = ok ->
      problog_var_set(bdd_generation_time(dec), BDDGenerationTime_dec),
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
    rename_file(BDDFile, SaveBDDFile),
    convert_filename_to_working_path('save_params', SaveBDDParFile),
    rename_file(BDDParFile, SaveBDDParFile)
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
  convert_filename_to_problog_path('problogbdd', ProblogBDD),
  convert_filename_to_working_path(ResultFileFlag, ResultFile),
  atomic_concat([ProblogBDD, Param,' -l ', BDDFile, ' -i ', BDDParFile, ' -m p -t ', BDDTime, ' > ', ResultFile], Command),
  shell(Command, Return),
  (Return =\= 0 ->
    Status = timeout
  ;
    see(ResultFile),
    read(probability(Prob)),
    seen,
    delete_file(ResultFile),
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
	     assert(hybrid_proof(ProofID,Cont_IDs,AllIntervals))
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
	assert(hybrid_proof(ProofID,ID,GroundID,Interval)),
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
	(
	 hybrid_proof(ProofID,ID,GroundID,Interval),
	 intervals_disjoin(Interval,Partition,PInterval),
	 assert(hybrid_proof_disjoint(ProofID,ID,GroundID,PInterval)),

	 fail; % go to next proof
	 true
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


problog_low(Goal, Threshold, _, _) :-
	init_problog_low(Threshold),
	problog_control(off, up),
	statistics(walltime, _),
	problog_call(Goal),
	add_solution,
	fail.
problog_low(_, _, LP, Status) :-
	statistics(walltime, [_,E]), %theo
	problog_var_set(sld_time, E),
	nb_getval(problog_completed_proofs, Trie_Completed_Proofs),
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
	assert(low(0,0.0)),
	assert(up(0,1.0)),
	assert(stopDiff(Delta)),
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
	(
	 problog_flag(verbose,true)
	->
	 format(user,'~w proofs, ~w stopped derivations~n',[NProofs,NCands]);
	 true
	),
	flush_output(user),
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
      (problog_flag(verbose,true) -> format(user,'difference:  ~6f~n',[Diff]);true),
      flush_output(user),
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
	(Status = ok ->
	    retract(low(_,_)),
	    assert(low(N,P)),
	    (problog_flag(verbose,true) -> format(user,'lower bound: ~6f~n',[P]);true),
	    flush_output(user)
	;
	true).

% if no stopped derivations, up=low
eval_upper(0,P,ok) :-
	retractall(up(_,_)),
	low(N,P),
	assert(up(N,P)).
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
  (StatusUp = ok ->
    retract(up(_,_)),
    assert(up(N,UpP))
  ;
    (problog_flag(verbose,true) -> format(user,'~w - continue using old up~n',[StatusUp]);true),
    flush_output(user),
    up(_,UpP)
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explanation probability - returns list of facts used or constant 'unprovable' as third argument
% problog_max(+Goal,-Prob,-Facts)
%
% uses iterative deepening with samw parameters as bounding algorithm
% threshold gets adapted whenever better proof is found
%
% uses local dynamic predicates max_probability/1 and max_proof/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	assert(max_probability(-999999)),
	assert(max_proof(unprovable)),
	init_problog(Threshold).

update_max :-
  b_getval(problog_probability, CurrP),
  max_probability(MaxP),
  (CurrP =< MaxP ->
    fail
  ;
    b_getval(problog_current_proof, IDs),
    open_end_close_end(IDs, R),
    retractall(max_proof(_)),
    assert(max_proof(R)),
    nb_setval(problog_threshold, CurrP),
    retractall(max_probability(_)),
    assert(max_probability(CurrP))
  ).

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
	problog_kbest(Goal, K, Prob, Status),
	( Status=ok ->
	    problog_flag(bdd_file,InternBDDFlag),
	    problog_flag(bdd_par_file,InternParFlag),
	    convert_filename_to_working_path(InternBDDFlag, InternBDD),
	    convert_filename_to_working_path(InternParFlag, InternPar),
	    rename_file(InternBDD,BDDFile),
	    rename_file(InternPar,ParamFile)
	;
	true).

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
	assert(current_kbest(-999999,[],0)),  %(log-threshold,proofs,num_proofs)
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
	(NewLogProb>=LogThreshold ->
	    b_getval(problog_current_proof,RevProof),
	    open_end_close_end(RevProof,Proof),
	    update_current_kbest(K,NewLogProb,Proof)
	;
	    fail).

update_current_kbest(_,NewLogProb,Cl) :-
	current_kbest(_,List,_),
	memberchk(NewLogProb-Cl,List),
	!.
update_current_kbest(K,NewLogProb,Cl) :-
	retract(current_kbest(OldThres,List,Length)),
	sorted_insert(NewLogProb-Cl,List,NewList),
	NewLength is Length+1,
	(NewLength < K ->
	    assert(current_kbest(OldThres,NewList,NewLength))
	;
	(NewLength>K ->
	    First is NewLength-K+1,
	    cutoff(NewList,NewLength,First,FinalList,FinalLength)
	   ; FinalList=NewList, FinalLength=NewLength),
	FinalList=[NewThres-_|_],
	nb_setval(problog_threshold,NewThres),
	assert(current_kbest(NewThres,FinalList,FinalLength))).

sorted_insert(A,[],[A]).
sorted_insert(A-LA,[B1-LB1|B], [A-LA,B1-LB1|B] ) :-
	A =< B1.
sorted_insert(A-LA,[B1-LB1|B], [B1-LB1|C] ) :-
	A > B1,
	sorted_insert(A-LA,B,C).

% keeps all entries with lowest probability, even if implying a total of more than k
cutoff(List,Len,1,List,Len) :- !.
cutoff([P-L|List],Length,First,[P-L|List],Length) :-
	nth(First,[P-L|List],PF-_),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exact probability
% problog_exact(+Goal,-Prob,-Status)
%
% using all proofs = using all proofs with probability > 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_exact(Goal,Prob,Status) :-
	problog_control(on, exact),
	problog_low(Goal,0,Prob,Status),
	problog_control(off, exact).

problog_exact_save(Goal,Prob,Status,BDDFile,ParamFile) :-
	problog_control(on, exact),
	problog_low(Goal,0,Prob,Status),
	problog_control(off, exact),
	(
	 Status==ok
	->
	 (
	  problog_flag(bdd_file,InternBDDFlag),
	  problog_flag(bdd_par_file,InternParFlag),
	  problog_flag(dir,DirFlag),
	  atomic_concat([DirFlag,InternBDDFlag],InternBDD),
	  atomic_concat([DirFlag,InternParFlag],InternPar),
	  rename_file(InternBDD,BDDFile),
	  rename_file(InternPar,ParamFile)
	 );
	 true
	).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% probability by sampling:
% running another N samples until 95percentCI-width<Delta
% lazy sampling using three-valued array indexed by internal fact IDs for ground facts,
%   internal database keys mc_true and mc_false for groundings of non-ground facts (including dynamic probabilities)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
%        reset_static_array(mc_sample),
	clean_sample,
	problog_control(on,mc),
	open(File,write,Log),
	format(Log,'# goal: ~q~n#delta: ~w~n',[Goal,Delta]),
	format(Log,'# num_programs  prob   low   high  diff  time~2n',[]),
	close(Log),
	statistics(walltime,[T1,_]),
	(problog_flag(verbose,true) -> format('search for ~q~n',[Goal]);true),
	montecarlo(Goal,Delta,K,0,File,0,T1),
	problog_control(off,mc).

% calculate values after K samples
montecarlo(Goal,Delta,K,SamplesSoFar,File,PositiveSoFar,InitialTime) :-
  SamplesNew is SamplesSoFar+1,
  SamplesNew mod K =:= 0,
  !,
  copy_term(Goal,GoalC),
  (mc_prove(GoalC) -> Next is PositiveSoFar+1; Next=PositiveSoFar),
  Prob is Next/SamplesNew,
  statistics(walltime,[T2,_]),
  Time is (T2-InitialTime),%/1000,

  problog_convergence_check(Time, Prob, SamplesNew, Delta, _Epsilon, Converged),
  ((Converged = true; Converged = terminate) ->
    (problog_flag(verbose,true) ->
      format('Runtime ~w ms~2n',[Time])
    ;
      true
    ),
    assert(mc_prob(Prob))
  ;
    montecarlo(Goal,Delta,K,SamplesNew,File,Next,InitialTime)
  ).


%   Epsilon is 2*sqrt(Prob*(1-Prob)/SamplesNew),
%   Low is Prob-Epsilon,
%   High is Prob+Epsilon,
%   Diff is 2*Epsilon,
%   (problog_flag(verbose,true) -> format('~n~w samples~nestimated probability ~w~n95 percent confidence interval [~w,~w]~n',[SamplesNew,Prob,Low,High]);true),
%   open(File,append,Log),
%   format(Log,'~w  ~8f  ~8f  ~8f  ~8f  ~3f~n',[SamplesNew,Prob,Low,High,Diff,Time]),
%   close(Log),


%   ((Diff<Delta; Diff =:= 0) ->
%     (problog_flag(verbose,true) ->
%       format('Runtime ~w sec~2n',[Time])
%     ;
%       true
%     ),
%     assert(mc_prob(Prob))
%   ;
%     montecarlo(Goal,Delta,K,SamplesNew,File,Next,InitialTime)
%   ).

% continue until next K samples done
montecarlo(Goal,Delta,K,SamplesSoFar,File,PositiveSoFar,InitialTime) :-
	SamplesNew is SamplesSoFar+1,
	copy_term(Goal,GoalC),
	(mc_prove(GoalC) -> Next is PositiveSoFar+1; Next=PositiveSoFar),
	montecarlo(Goal,Delta,K,SamplesNew,File,Next,InitialTime).

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
	problog_exact((Goal,ground(Goal),\+problog:answer(Goal),assert(problog:answer(Goal))),_,_),
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
	(NewLogProb>=LogThreshold ->
	    update_current_kbest_answers(K,NewLogProb,Goal)
	;
	    fail).

update_current_kbest_answers(_,NewLogProb,Goal) :-
	current_kbest(_,List,_),
	update_prob_of_known_answer(List,Goal,NewLogProb,NewList),
	!,
	keysort(NewList,SortedList),%format(user_error,'updated variant of ~w~n',[Goal]),
	retract(current_kbest(K,_,Len)),
	assert(current_kbest(K,SortedList,Len)).
update_current_kbest_answers(K,NewLogProb,Goal) :-
	retract(current_kbest(OldThres,List,Length)),
	sorted_insert(NewLogProb-Goal,List,NewList),%format(user_error,'inserted new element ~w~n',[Goal]),
	NewLength is Length+1,
	(NewLength < K ->
	    assert(current_kbest(OldThres,NewList,NewLength))
	;
	(NewLength>K ->
	    First is NewLength-K+1,
	    cutoff(NewList,NewLength,First,FinalList,FinalLength)
	   ; FinalList=NewList, FinalLength=NewLength),
	FinalList=[NewThres-_|_],
	nb_setval(problog_threshold,NewThres),
	assert(current_kbest(NewThres,FinalList,FinalLength))).

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
    convert_filename_to_problog_path('problogbdd', ProblogBDD),
    problog_flag(bdd_result,ResultFileFlag),
    convert_filename_to_working_path(ResultFileFlag, ResultFile),
    atomic_concat([ProblogBDD, Param,' -l ', BDDFile, ' -i ', BDDParFile, ' -m p -t ', BDDTime, ' > ', ResultFile], Command),
	statistics(walltime,_),
	shell(Command,Return),
	(Return =\= 0 ->
	    Status = timeout
	;
	    statistics(walltime,[_,E3]),
		(problog_flag(verbose,true) -> format(user,'~w ms BDD processing~n',[E3]);true),
		see(ResultFile),
		read_probs(N,Probs),
		seen,
		Status = ok,
		% cleanup
		% TODO handle flag for keeping files
		(problog_flag(save_bdd,true) ->
			true
		;
            delete_file(BDDFile),
		    delete_file(BDDParFile),
		    delete_file(ResultFile),
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
		delete_file(BDDFile,[]),
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

build_trie(exact, Goal, Trie) :-
  problog_control(on, exact),
  build_trie(low(0), Goal, Trie),
  problog_control(off, exact).

build_trie(low(Threshold), Goal, _) :-
  number(Threshold),
  init_problog_low(Threshold),
  problog_control(off, up),
  statistics(walltime, _),
  problog_call(Goal),
  add_solution,
  fail.
build_trie(low(Threshold), _, Trie) :-
  number(Threshold),
  statistics(walltime, [_,E]),
  problog_var_set(sld_time, E),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Write BDD structure script for a trie and list all variables used
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_bdd_struct_script(Trie,BDDFile,Variables) :-
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
  once(member(OptLevel, Levels)),
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
    statistics(walltime, _),
    atomic_concat([BDDFile, '_dec'], BDDFile_dec),
    ptree_decomposition_struct(Trie, BDDFile_dec, Variables),
    statistics(walltime, [_, ScriptGenerationTime_dec]),
    problog_var_set(bdd_script_time(dec), ScriptGenerationTime_dec)
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
      tell(BDDParFile),
      bdd_vars_script(Vars),
      flush_output, % isnt this called by told/0?
      told,
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
write_bdd_forest([],VarsTot,VarsTot,_).
write_bdd_forest([Goal|Rest],VarsAcc,VarsTot,N):-
  build_trie(Goal, Trie),
  write_nth_bdd_struct_script(N, Trie, Vars),
  (problog_flag(verbose, true)->
    problog_statistics
  ;
    true
  ),
  delete_ptree(Trie),
  N2 is N+1,
  list_to_ord_set(Vars,VarsSet),
  ord_union(VarsAcc,VarsSet,VarsAcc2),
  once(write_bdd_forest(Rest,VarsAcc2,VarsTot,N2)).

% Write files
write_nth_bdd_struct_script(N,Trie,Vars) :-
  bdd_forest_file(N,BDDFile),
  write_bdd_struct_script(Trie,BDDFile,Vars).

write_global_bdd_file(NbVars,L) :-
  bdd_file(BDDFile),
  open(BDDFile,'write',BDDFileStream),
  tell(BDDFileStream),
  format('@BDD2~n~w~n~w~n~w~n',[NbVars,0,L]),
  write_global_bdd_file_line(1,L),
  write_global_bdd_file_query(1,L),
  flush_output,
  told.

write_global_bdd_file_line(I,Max) :-
  (I>Max ->
      true
  ;
      bdd_forest_file(I,BDDFile),
      format("L~q = <~w>~n",[I,BDDFile]),
      I2 is I+1,
      write_global_bdd_file_line(I2,Max)
  ).

write_global_bdd_file_query(I,Max) :-
  (I=Max ->
      format("L~q~n",[I])
  ;
      format("L~q,",[I]),
      I2 is I+1,
      write_global_bdd_file_query(I2,Max)
  ).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Term Expansion for user predicates
% Must come after clauses for '::'/2 and term_expansion_intern/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:term_expansion(Term,ExpandedTerm) :-
	Term \== end_of_file,
	prolog_load_context(module,Mod),
	problog:term_expansion_intern(Term,Mod,ExpandedTerm).


