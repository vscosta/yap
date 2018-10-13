%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-04-21 14:18:59 +0200 (Thu, 21 Apr 2011) $
%  $Revision: 6364 $
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
%  Bernd Gutmann, Vitor Santos Costa
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


:- module(learning,[do_learning/1,
	            do_learning/2,
		    reset_learning/0,
		    sigmoid/3,
		    inv_sigmoid/3
		    ]).

% switch on all the checks to reduce bug searching time
:- style_check(all).
:- yap_flag(unknown,error).

% load modules from the YAP library
:- use_module(library(lists), [member/2,max_list/2, min_list/2, sum_list/2]).
:- use_module(library(system), [file_exists/1, shell/2]).
:- use_module(library(rbtrees)).
:- use_module(library(lbfgs)).

% load our own modules
:- reexport(problog).
:- use_module('problog/logger').
:- use_module('problog/flags').
:- use_module('problog/os').
:- use_module('problog/print_learning').
:- use_module('problog/utils_lbdd').
:- use_module('problog/utils').
:- use_module('problog/tabling').

% used to indicate the state of the system
:- dynamic(values_correct/0).
:- dynamic(learning_initialized/0).
:- dynamic(current_iteration/1).
:- dynamic(example_count/1).
%:- dynamic(query_probability_intern/2).
%:- dynamic(query_gradient_intern/4).
:- dynamic(last_mse/1).
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/2).


% used to identify queries which have identical proofs
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/3).

% used to identify queries which have identical proofs
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/3).

:- multifile(user:example/4).
:- multifile(user:problog_discard_example/1).
user:example(A,B,C,=) :-
	current_predicate(user:example/3),
	user:example(A,B,C),
	\+  user:problog_discard_example(B).

:- multifile(user:test_example/4).
user:test_example(A,B,C,=) :-
	current_predicate(user:test_example/3),
	user:test_example(A,B,C),
	\+  user:problog_discard_example(B).



%========================================================================
%= store the facts with the learned probabilities to a file
%========================================================================

save_model:-
	current_iteration(Iteration),
	create_factprobs_file_name(Iteration,Filename),
	export_facts(Filename).




%========================================================================
%= find out whether some example IDs are used more than once
%= if so, complain and stop
%=
%========================================================================


check_examples :-
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Check example IDs
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 (user:example(ID,_,_,_), \+ atomic(ID))
	->
	 (
	  format(user_error,'The example id of training example ~q ',[ID]),
	  format(user_error,'is not atomic (e.g foo42, 23, bar, ...).~n',[]),
	  throw(error(examples))
	 ); true
	),

	(
	 (user:test_example(ID,_,_,_), \+ atomic(ID))
	->
	 (
	  format(user_error,'The example id of test example ~q ',[ID]),
	  format(user_error,'is not atomic (e.g foo42, 23, bar, ...).~n',[]),
	  throw(error(examples))
	 ); true
	),

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Check example probabilities
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 (user:example(ID,_,P,_), (\+ number(P); P>1 ; P<0))
	->
	 (
	  format(user_error,'The training example ~q does not have a valid probability value (~q).~n',[ID,P]),
	  throw(error(examples))
	 ); true
	),

	(
	 (user:test_example(ID,_,P,_), (\+ number(P); P>1 ; P<0))
	->
	 (
	  format(user_error,'The test example ~q does not have a valid probability value (~q).~n',[ID,P]),
	  throw(error(examples))
	 ); true
	),


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Check that no example ID is repeated,
	% and if it is repeated make sure the query is the same
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 (
	  (
	   user:example(ID,QueryA,_,_),
	   user:example(ID,QueryB,_,_),
	   QueryA \= QueryB
	  ) ;

	  (
	   user:test_example(ID,QueryA,_,_),
	   user:test_example(ID,QueryB,_,_),
	   QueryA \= QueryB
	  );

	  (
	   user:example(ID,QueryA,_,_),
	   user:test_example(ID,QueryB,_,_),
	   QueryA \= QueryB
	  )
	 )
	->
	 (
	  format(user_error,'The example id ~q is used several times.~n',[ID]),
	  throw(error(examples))
	 ); true
	).
%========================================================================
%=
%========================================================================

reset_learning :-
	retractall(learning_initialized),
	retractall(values_correct),
	retractall(current_iteration(_)),
	retractall(example_count(_)),
%	retractall(query_probability_intern(_,_)),%
%	retractall(query_gradient_intern(_,_,_,_)),
	retractall(last_mse(_)),
	retractall(query_is_similar(_,_)),
	retractall(query_md5(_,_,_)),

	set_problog_flag(alpha,auto),
	set_problog_flag(learning_rate,examples),
	logger_reset_all_variables.



%========================================================================
%= initialize everything and perform Iterations times gradient descent
%= can be called several times
%= if it is called with an epsilon parameter, it stops when the change
%= in the MSE is smaller than epsilon
%========================================================================

do_learning(Iterations) :-
	do_learning(Iterations,-1).

do_learning(Iterations,Epsilon) :-
	current_predicate(user:example/4),
	!,
	integer(Iterations),
	number(Epsilon),
	Iterations>0,
	do_learning_intern(Iterations,Epsilon).
do_learning(_,_) :-
	format(user_error,'~n~Error: No training examples specified.~n~n',[]).


do_learning_intern(0,_) :-
	!.
do_learning_intern(Iterations,Epsilon) :-
	Iterations>0,
	init_learning,
	current_iteration(CurrentIteration),
	retractall(current_iteration(_)),
	NextIteration is CurrentIteration+1,
	assertz(current_iteration(NextIteration)),
	EndIteration is CurrentIteration+Iterations-1,

	format_learning(1,'~nIteration ~d of ~d~n',[CurrentIteration,EndIteration]),
	logger_set_variable(iteration,CurrentIteration),
	logger_start_timer(duration),
%	mse_testset,
	%	ground_truth_difference,
	gradient_descent,

	problog_flag(log_frequency,Log_Frequency),

	(
	 ( Log_Frequency>0, 0 =:= CurrentIteration mod Log_Frequency)
	->
	 once(save_model);
	 true
	),

%	update_values,

	(
	 last_mse(Last_MSE)
	->
	 (
	  retractall(last_mse(_)),
	  logger_get_variable(mse_trainingset,Current_MSE),
	  assertz(last_mse(Current_MSE)),
	  !,
	  MSE_Diff is abs(Last_MSE-Current_MSE)
	 );  (
	      logger_get_variable(mse_trainingset,Current_MSE),
	      assertz(last_mse(Current_MSE)),
	      MSE_Diff is Epsilon+1
	     )
	),

	(
	 (problog_flag(rebuild_bdds,BDDFreq),BDDFreq>0,0 =:= CurrentIteration mod BDDFreq)
	->
	 (
	  retractall(values_correct),
	  retractall(query_is_similar(_,_)),
	  retractall(query_md5(_,_,_)),
	  empty_bdd_directory,
	  init_queries
	 ); true
	),


	!,
	logger_stop_timer(duration),


	logger_write_data,



	RemainingIterations is Iterations-1,

	(
	 MSE_Diff>Epsilon
	->
	 do_learning_intern(RemainingIterations,Epsilon);
	 true
	).


%========================================================================
%= find proofs and build bdds for all training and test examples
%=
%=
%========================================================================
init_learning :-
	learning_initialized,
	!.
init_learning :-
	check_examples,

%	empty_output_directory,
	logger_write_header,

	format_learning(1,'Initializing everything~n',[]),



        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Check, if continuous facts are used.
	% if yes, switch to problog_exact
        % continuous facts are not supported yet.
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	set_default_gradient_method,
	(	problog_flag(continuous_facts, true )
	->
	 problog_flag(init_method,(_,_,_,_,OldCall)),
	 (
	  (
	   continuous_fact(_),
	   OldCall\=problog_exact_save(_,_,_,_,_)
	  )
	 ->
	  (
	   format_learning(2,'Theory uses continuous facts.~nWill use problog_exact/3 as initalization method.~2n',[]),
	   set_problog_flag(init_method,(Query,Probability,BDDFile,ProbFile,problog_exact_save(Query,Probability,_Status,BDDFile,ProbFile)))
	  );
	  true
	 )
	;
	  problog_tabled(_)
	 ->
	  (
	   format_learning(2,'Theory uses tabling.~nWill use problog_exact/3 as initalization method.~2n',[]),
	   set_problog_flag(init_method,(Query,Probability,BDDFile,ProbFile,problog_exact_save(Query,Probability,_Status,BDDFile,ProbFile)))
	  );
	  true
	 ),

	succeeds_n_times(user:test_example(_,_,_,_),TestExampleCount),
	format_learning(3,'~q test examples~n',[TestExampleCount]),

	succeeds_n_times(user:example(_,_,_,_),TrainingExampleCount),
	assertz(example_count(TrainingExampleCount)),
	format_learning(3,'~q training examples~n',[TrainingExampleCount]),



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% build BDD script for every example
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	once(init_queries),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% done
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	assertz(current_iteration(0)),
	assertz(learning_initialized),

	format_learning(1,'~n',[]).

 empty_bdd_directory :-
	current_key(_,I),
	integer(I),
	recorded(I,bdd(_,_,_),R),
	erase(R),
	fail.
empty_bdd_directory.


set_default_gradient_method :-
    problog_flag(continuous_facts, true),
    !,
    % problog_flag(init_method,OldMethod),
    format_learning(2,'Theory uses continuous facts.~nWill use problog_exact/3 as initalization method.~2n',[]),
    set_problog_flag(init_method,(Query,Probability,BDDFile,ProbFile,problog_exact_save(Query,Probability,_Status,BDDFile,ProbFile))).
set_default_gradient_method :-
    problog_tabled(_), problog_flag(fast_proofs,false),
    !,
    format_learning(2,'Theory uses tabling.~nWill use problog_exact/3 as initalization method.~2n',[]),
    set_problog_flag(init_method,(Query,Probability,BDDFile,ProbFile,problog_exact_save(Query,Probability,_Status,BDDFile,ProbFile))).
%set_default_gradient_method :-
%    problog_flag(init_method,(gene(X,Y),N,Bdd,graph2bdd(X,Y,N,Bdd))),
%    !.
set_default_gradient_method.

%========================================================================
%= This predicate goes over all training and test examples,
%= calls the inference method of ProbLog and stores the resulting
%= BDDs
%========================================================================


init_queries :-
	format_learning(2,'Build BDDs for examples~n',[]),
	forall(user:test_example(ID,Query,_Prob,_),init_one_query(ID,Query,test)),
	forall(user:example(ID,Query,_Prob,_),init_one_query(ID,Query,training)).

bdd_input_file(Filename) :-
	problog_flag(output_directory,Dir),
	concat_path_with_filename(Dir,'input.txt',Filename).

init_one_query(QueryID,Query,_Type) :-
%	format_learning(3,' ~q example ~q: ~q~n',[Type,QueryID,Query]),

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% if BDD file does not exist, call ProbLog
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 recorded(QueryID, _, _)
	->
	 format_learning(3,' Reuse existing BDD ~q~n~n',[QueryID])
	 ;
	 b_setval(problog_required_keep_ground_ids,false),
	 (QueryID mod 100 =:= 0 -> writeln(QueryID) ; true),
	  problog_flag(init_method,(Query,N,Bdd,graph2bdd(X,Y,N,Bdd))),
	  Query =.. [_,X,Y]
	  ->
	  Bdd = bdd(Dir, Tree, MapList),
	  (
	      graph2bdd(X,Y,N,Bdd)
	  ->
	  rb_new(H0),
	  maplist_to_hash(MapList, H0, Hash),
	  tree_to_grad(Tree, Hash, [], Grad)
	  % ;
	  % Bdd = bdd(-1,[],[]),
	  % Grad=[]
	  ),
		write('.'),
	  recordz(QueryID,bdd(Dir, Grad, MapList),_)
	 ;
	  problog_flag(init_method,(Query,NOf,Bdd,problog_kbest_as_bdd(Call,NOf,Bdd))) ->
	  b_setval(problog_required_keep_ground_ids,false),
	  rb_new(H0),
	  strip_module(Call,_,Goal),
	  !,
	  Bdd = bdd(Dir, Tree, MapList),
%	  trace,
	  problog:problog_kbest_as_bdd(Goal,NOf,Bdd),
	  maplist_to_hash(MapList, H0, Hash),
	  Tree \= [],
	  %put_code(0'.),
	  tree_to_grad(Tree, Hash, [], Grad),
	  recordz(QueryID,bdd(Dir, Grad, MapList),_)
	 ;
	  problog_flag(init_method,(Query,NOf,Bdd,Call)) ->
	  b_setval(problog_required_keep_ground_ids,false),
	  rb_new(H0),
	  Bdd = bdd(Dir, Tree, MapList),
%	  trace,
	  problog:Call,
	  maplist_to_hash(MapList, H0, Hash),
	  Tree \= [],
	  %put_code(0'.),
	  tree_to_grad(Tree, Hash, [], Grad),
	  recordz(QueryID,bdd(Dir, Grad, MapList),_)
	).




%========================================================================
%=
%=
%=
%========================================================================
query_probability(QueryID,Prob) :-
	Prob <== qp[QueryID].

%========================================================================
%=
%=
%=
%========================================================================



% FIXME
ground_truth_difference :-
	findall(Diff,(tunable_fact(FactID,GroundTruth),
		      \+continuous_fact(FactID),
		      \+ var(GroundTruth),
		      %% get_fact_probability(FactID,Prob),
		      Prob <== p[FactID],
		      Diff is abs(GroundTruth-Prob)),AllDiffs),
	(
	 AllDiffs=[]
	->
	 (
	  MinDiff=0.0,
	  MaxDiff=0.0,
	  DiffMean=0.0
	 ) ;
	 (
	  length(AllDiffs,Len),
	  sum_list(AllDiffs,AllDiffsSum),
	  min_list(AllDiffs,MinDiff),
	  max_list(AllDiffs,MaxDiff),
	  DiffMean is AllDiffsSum/Len
	 )
	),

	logger_set_variable(ground_truth_diff,DiffMean),
	logger_set_variable(ground_truth_mindiff,MinDiff),
	logger_set_variable(ground_truth_maxdiff,MaxDiff).

%========================================================================
%= Calculates the mse of training and test data
%=
%= -Float
%========================================================================

mse_trainingset_only_for_linesearch(MSE) :-
	update_values,

	example_count(Example_Count),

	bb_put(error_train_line_search,0.0),
	forall(user:example(QueryID,_Query,QueryProb,Type),
	       (
		once(update_query(QueryID,'.',probability)),
		query_probability(QueryID,CurrentProb),
		once(update_query_cleanup(QueryID)),
		(
		 (Type == '='; (Type == '<', CurrentProb>QueryProb); (Type=='>',CurrentProb<QueryProb))
		->
		 (
		  bb_get(error_train_line_search,Old_Error),
		  New_Error is Old_Error + (CurrentProb-QueryProb)**2,
		  bb_put(error_train_line_search,New_Error)
		 );true
		)
	       )
	      ),
	bb_delete(error_train_line_search,Error),
	MSE is Error/Example_Count,
	format_learning(3,' (~8f)~n',[MSE]),
	retractall(values_correct).

mse_testset :-
	current_iteration(Iteration),
	create_test_predictions_file_name(Iteration,File_Name),
	open(File_Name,'write',Handle),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),
	format(Handle,"% Iteration, train/test, QueryID, Query, GroundTruth, Prediction %~n",[]),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),

	format_learning(2,'MSE_Test ',[]),
	update_values,
	bb_put(llh_test_queries,0.0),
	findall(SquaredError,
		(user:test_example(QueryID,Query,TrueQueryProb,Type),
		 once(update_query(QueryID,'+',probability)),
		 query_probability(QueryID,CurrentProb),
		 format(Handle,'ex(~q,test,~q,~q,~10f,~10f).~n',[Iteration,QueryID,Query,TrueQueryProb,CurrentProb]),

		 once(update_query_cleanup(QueryID)),
		 (
		  (Type == '='; (Type == '<', CurrentProb>QueryProb); (Type=='>',CurrentProb<QueryProb))
		 ->
		  SquaredError is (CurrentProb-TrueQueryProb)**2;
		  SquaredError = 0.0
		 ),
		 bb_get(llh_test_queries,Old_LLH_Test_Queries),
		 New_LLH_Test_Queries is Old_LLH_Test_Queries+log(CurrentProb),
		 bb_put(llh_test_queries,New_LLH_Test_Queries)
		),
		AllSquaredErrors),

        close(Handle),
	bb_delete(llh_test_queries,LLH_Test_Queries),

	length(AllSquaredErrors,Length),

	(
	 Length>0
	->
	 (
	  sum_list(AllSquaredErrors,SumAllSquaredErrors),
	  min_list(AllSquaredErrors,MinError),
	  max_list(AllSquaredErrors,MaxError),
	  MSE is SumAllSquaredErrors/Length
	 );(
	    MSE=0.0,
	    MinError=0.0,
	    MaxError=0.0
	   )
	),

	logger_set_variable(mse_testset,MSE),
	logger_set_variable(mse_min_testset,MinError),
	logger_set_variable(mse_max_testset,MaxError),
	logger_set_variable(llh_test_queries,LLH_Test_Queries),
	format_learning(2,' (~8f)~n',[MSE]).


%========================================================================
%= Calculates the sigmoid function respectivly the inverse of it
%= warning: applying inv_sigmoid to 0.0 or 1.0 will yield +/-inf
%=
%= +Float, -Float
%========================================================================

sigmoid(T,Slope,Sig) :-
    IN <== T,
    OUT is 1/(1+exp(-IN*Slope)),
    Sig <== OUT.

inv_sigmoid(T,Slope,InvSig) :-
	InvSig <== -log(1/T-1)/Slope.


%========================================================================
%= Perform one iteration of gradient descent
%=
%= assumes that everything is initialized, if the current values
%= of query_probability/2 and query_gradient/4 are not up to date
%= they will be recalculated
%= finally, the values_correct/0 is retracted to signal that the
%= probabilities of the examples have to be recalculated
%========================================================================

save_old_probabilities :-
    old_prob <== p.


% vsc: avoid silly search
gradient_descent :-
    problog_flag(sigmoid_slope,Slope),
%	current_iteration(Iteration),
	% create_training_predictions_file_name(Iteration,File_Name),
	Handle = user_error,
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),
	format(Handle,"% Iteration, train/test, QueryID, Query, GroundTruth, Prediction %~n",[]),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",[]),
	findall(FactID,tunable_fact(FactID,GroundTruth),L), length(L,N),
%	leash(0),trace,
	lbfgs_initialize(N,X,0,Solver),
	forall(tunable_fact(FactID,GroundTruth),
	       (XZ is 0.0, X[FactID] <== XZ,sigmoid(XZ,Slope,Pr),set_fact_probability(FactID,Pr))),
	problog_flag(sigmoid_slope,Slope),
	%lbfgs_set_parameter(min_step, 0.0, Solver),
	lbfgs_run(Solver,BestF),
	format('~2nOptimization done~nWe found a minimum ~4f.~n',[BestF]),
	forall(tunable_fact(FactID,GroundTruth), set_tunable(FactID,Slope,X)),
	set_problog_flag(mse_trainset, BestF),
	lbfgs_finalize(Solver).

set_tunable(I,Slope,P) :-
    X <== P[I],
    sigmoid(X,Slope,Pr),
    set_fact_probability(I,Pr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% start calculate gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:evaluate(LLH_Training_Queries, X,Grad,N,_,_) :-
    %Handle = user_error,
    GradCount <== array[N] of ints,
    Probs  <== array[N] of floats,
    problog_flag(sigmoid_slope,Slope),
    N1 is N-1,
    
    forall(between(0,N1,I),
	   (Grad[I] <== 0.0, S <== X[I], sigmoid(S,Slope, P), Probs[I] <== P) 
	  ),
    findall(LL,
	    compute_grad(Grad, GradCount, Probs, Slope, LL),
	    LLs
	   ),
    sum_list(LLs,LLH_Training_Queries).
%wrap(X, Grad, GradCount).


compute_grad(Grad, GradCount, Probs, Slope, LL) :-
	user:example(QueryID,_Query,QueryProb,_),
	recorded(QueryID,BDD,_),
	BDD = bdd(_Dir, _GradTree, MapList),
	MapList = [_|_],
	bind_maplist(MapList, Slope, Probs),
%writeln( MapList ),
    qprobability(BDD,Slope,BDDProb),
    LL is (((BDDProb)-(QueryProb))**2),
%writeln( qprobability(BDD,Slope,BDDProb) ),
    forall(
	    member(I-_, MapList),
	gradientpair(I, BDD,Slope,BDDProb, QueryProb, Grad, Probs, GradCount)
    ).

gradientpair(I, BDD,Slope,BDDProb, QueryProb, Grad, Probs, 	GradCount) :-
    qgradient(I, BDD, Slope, FactID, GradValue),
    % writeln(FactID),
    G0 <== Grad[FactID],
    Prob <== Probs[FactID],
%writeln(    GN is G0-GradValue*(QueryProb-BDDProb)),
   GN is G0-GradValue*2*Prob*(1-Prob)*(QueryProb-BDDProb),
   %writeln(FactID:(G0->GN)),
   GC <== GradCount[FactID],
   GC1 is GC+1,
   GradCount[FactID] <== GC1,
    Grad[FactID] <== GN.

qprobability(bdd(Dir, Tree, _MapList), Slope, Prob) :-
/*	query_probability(21,6.775948e-01). */
	run_sp(Tree, Slope, 1.0, Prob0),
	(Dir == 1 -> Prob0 = Prob ;  Prob is 1.0-Prob0).


qgradient(I, bdd(Dir, Tree, _MapList), Slope, I, Grad) :-
	run_grad(Tree, I, Slope, 0.0, Grad0),
	( Dir = 1 -> Grad = Grad0 ; Grad is -Grad0).

wrap( X, Grad, GradCount) :-
    tunable_fact(FactID,GroundTruth),
	       Z<==X[FactID],
	       W<==Grad[FactID],
	       WC<==GradCount[FactID],
	       WC > 0,
	       format('ex(~d, ~q, ~4f, ~4f).~n',[FactID,GroundTruth,Z,W]),
%	       Grad[FactID] <== WN,
	       fail.
wrap( _X, _Grad, _GradCount).


%	writeln(grad(QueryID:I:Grad)),
%	assert(query_gradient_intern(QueryID,I,p,Grad)),
% fail.
%gradient(QueryID, g, Slope) :-
%	gradient(QueryID, l, Slope).

maplist_to_hash([], H0, H0).
maplist_to_hash([I-V|MapList], H0, Hash) :-
	rb_insert(H0, V, I, H1),
	maplist_to_hash(MapList, H1, Hash).

tree_to_grad([], _, Grad, Grad).
tree_to_grad([Node|Tree], H, Grad0, Grad) :-
	node_to_gradient_node(Node, H, GNode),
	tree_to_grad(Tree, H, [GNode|Grad0], Grad).

node_to_gradient_node(pp(P-G,X,L,R), H, gnodep(P,G,X,Id,PL,GL,PR,GR)) :-
	rb_lookup(X,Id,H),
	(L == 1 -> GL=0, PL=1 ; L == 0 -> GL = 0, PL=0 ; L = PL-GL),
	(R == 1 -> GR=0, PR=1 ; R == 0 -> GR = 0, PR=0 ; R = PR-GR).
node_to_gradient_node(pn(P-G,X,L,R), H, gnoden(P,G,X,Id,PL,GL,PR,GR)) :-
	rb_lookup(X,Id,H),
	(L == 1 -> GL=0, PL=1 ; L == 0 -> GL = 0, PL=0 ; L = PL-GL),
	(R == 1 -> GR=0, PR=1 ; R == 0 -> GR = 0, PR=0 ; R = PR-GR).

run_sp([], _, P0, P0).
run_sp(gnodep(P,_G, EP, _Id, PL, _GL, PR, _GR).Tree, Slope, _, PF) :-
	P is EP*PL+ (1.0-EP)*PR,
	run_sp(Tree, Slope, P, PF).
run_sp(gnoden(P,_G, EP, _Id, PL, _GL, PR, _GR).Tree, Slope, _, PF) :-
	P is EP*PL + (1.0-EP)*(1.0 - PR),
	run_sp(Tree, Slope, P, PF).

run_grad([], _I, _, G0, G0).
run_grad([gnodep(P,G, EP, Id, PL, GL, PR, GR)|Tree], I, Slope, _, GF) :-
	P is EP*PL+ (1.0-EP)*PR,
	G0 is EP*GL + (1.0-EP)*GR,
	% don' t forget the -X
	( I == Id -> G is PL-PR ; G = G0 ),
	run_grad(Tree, I, Slope, G, GF).
run_grad([gnoden(P,G, EP, Id, PL, GL, PR, GR)|Tree], I, Slope, _, GF) :-
	P is EP*PL + (1.0-EP)*(1.0 - PR),
	G0 is EP*GL  - (1.0 - EP) * GR,
	( I == Id -> G is PL-(1.0-PR) ; G = G0 ),
	run_grad(Tree, I, Slope, G, GF).



prob2log(_X,Slope,FactID,V) :-
    get_fact_probability(FactID, V0),
    inv_sigmoid(V0, Slope, V).

log2prob(X,Slope,FactID,V) :-
    V0 <== X[FactID],
    sigmoid(V0, Slope, V).

bind_maplist([], _Slope, _X).
bind_maplist([Node-Pr|MapList], Slope, X) :-
	Pr <== X[Node],
	bind_maplist(MapList, Slope, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% stop calculate gradient
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:progress(FX,X,_G,X_Norm,G_Norm,Step,_N,Iteration,Ls,0) :-
    problog_flag(sigmoid_slope,Slope),
    X0 <== X[0], sigmoid(X0,Slope,P0),
    X1 <== X[1], sigmoid(X1,Slope,P1),
    format('~d. Iteration : (x0,x1)=(~4f,~4f)  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f~n',[Iteration,P0                                               ,P1,FX,X_Norm,G_Norm,Step,Ls]).


%========================================================================
%= initialize the logger module and set the flags for learning
%= don't change anything here! use set_problog_flag/2 instead
%========================================================================

init_flags :-
	prolog_file_name(queries,Queries_Folder), % get absolute file name for './queries'
	prolog_file_name(output,Output_Folder), % get absolute file name for './output'
	problog_define_flag(bdd_directory, problog_flag_validate_directory, 'directory for BDD scripts', Queries_Folder,learning_general),
	problog_define_flag(output_directory, problog_flag_validate_directory, 'directory for logfiles etc', Output_Folder,learning_general,flags:learning_output_dir_handler),
	problog_define_flag(log_frequency, problog_flag_validate_posint, 'log results every nth iteration', 1, learning_general),
	problog_define_flag(rebuild_bdds, problog_flag_validate_nonegint, 'rebuild BDDs every nth iteration', 0, learning_general),
	problog_define_flag(reuse_initialized_bdds,problog_flag_validate_boolean, 'Reuse BDDs from previous runs',false, learning_general),
	problog_define_flag(check_duplicate_bdds,problog_flag_validate_boolean,'Store intermediate results in hash table',true,learning_general),
	problog_define_flag(init_method,problog_flag_validate_dummy,'ProbLog predicate to search proofs',(Query,Tree,problog:problog_kbest_as_bdd(Query,100,Tree)),learning_general,flags:learning_libdd_init_handler),
	problog_define_flag(alpha,problog_flag_validate_number,'weight of negative examples (auto=n_p/n_n)',auto,learning_general,flags:auto_handler),
	problog_define_flag(sigmoid_slope,problog_flag_validate_posnumber,'slope of sigmoid function',1.0,learning_general),
	% problog_define_flag(continuous_facts,problog_flag_validate_boolean,'support parameter learning of continuous distributions',1.0,learning_general),
	problog_define_flag(learning_rate,problog_flag_validate_posnumber,'Default learning rate (If line_search=false)',examples,learning_line_search,flags:examples_handler),
	problog_define_flag(line_search, problog_flag_validate_boolean,'estimate learning rate by line search',false,learning_line_search),
	problog_define_flag(line_search_never_stop, problog_flag_validate_boolean,'make tiny step if line search returns 0',true,learning_line_search),
	problog_define_flag(line_search_tau, problog_flag_validate_indomain_0_1_open,'tau value for line search',0.618033988749,learning_line_search),
	problog_define_flag(line_search_tolerance,problog_flag_validate_posnumber,'tolerance value for line search',0.05,learning_line_search),
	problog_define_flag(line_search_interval, problog_flag_validate_dummy,'interval for line search',(0,100),learning_line_search,flags:linesearch_interval_handler).

init_logger :-
    logger_define_variable(iteration, int),
	logger_define_variable(duration,time),
	logger_define_variable(mse_trainingset,float),
	logger_define_variable(mse_min_trainingset,float),
	logger_define_variable(mse_max_trainingset,float),
	logger_define_variable(mse_testset,float),
	logger_define_variable(mse_min_testset,float),
	logger_define_variable(mse_max_testset,float),
	logger_define_variable(gradient_mean,float),
	logger_define_variable(gradient_min,float),
	logger_define_variable(gradient_max,float),
	logger_define_variable(ground_truth_diff,float),
	logger_define_variable(ground_truth_mindiff,float),
	logger_define_variable(ground_truth_maxdiff,float),
	logger_define_variable(learning_rate,float),
	logger_define_variable(alpha,float),
	logger_define_variable(llh_training_queries,float),
	logger_define_variable(llh_test_queries,float).

:- initialization(init_flags).

:- initialization(init_logger).
