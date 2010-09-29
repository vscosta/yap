%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2010-09-29 18:43:14 +0200 (Wed, 29 Sep 2010) $
%  $Revision: 4854 $
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
%  Bernd Gutmann
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
	            do_learning/2
		    ]).

% switch on all the checks to reduce bug searching time
:- style_check(all).
:- yap_flag(unknown,error).

% load modules from the YAP library
:- use_module(library(lists), [max_list/2, min_list/2, sum_list/2]).
:- use_module(library(system), [delete_file/1, file_exists/1, shell/2]).

% load our own modules
:- use_module(problog).
:- use_module('problog/logger').
:- use_module('problog/flags').
:- use_module('problog/os').
:- use_module('problog/print_learning').
:- use_module('problog/utils_learning').

% used to indicate the state of the system
:- dynamic(values_correct/0).
:- dynamic(learning_initialized/0).
:- dynamic(current_iteration/1).
:- dynamic(example_count/1).
:- dynamic(query_probability_intern/2).
:- dynamic(query_gradient_intern/4).
:- dynamic(last_mse/1).

% used to identify queries which have identical proofs
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/3).

:- multifile(user:example/4).
user:example(A,B,C,=) :-
	current_predicate(user:example/3),
	user:example(A,B,C).

:- multifile(user:test_example/4).
user:test_example(A,B,C,=) :-
	current_predicate(user:test_example/3),
	user:test_example(A,B,C).


%========================================================================
%= store the facts with the learned probabilities to a file
%= if F is a variable, a filename based on the current iteration is used
%=
%========================================================================

save_model:-
	current_iteration(Iteration),
	atomic_concat(['factprobs_',Iteration,'.pl'],Filename),
	problog_flag(output_directory,Dir),
	concat_path_with_filename(Dir,Filename,Filename2),
	export_facts(Filename2).


%========================================================================
%= store the current succes probabilities for training and test examples
%= 
%========================================================================

save_predictions:-
	current_iteration(Iteration),
	atomic_concat(['predictions_',Iteration,'.pl'],Filename),
	problog_flag(output_directory,Dir),
	concat_path_with_filename(Dir,Filename,Filename2),

	open(Filename2,'append',Handle),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[]),
	format(Handle,"% Iteration, train/test, QueryID, Query, GroundTruth, Prediction %\n",[]),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[]),
	!,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start save prediction test examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all test examples
	   current_predicate(user:test_example/4),
	   user:test_example(Query_ID,Query,TrueQueryProb,_),
	   query_probability(Query_ID,LearnedQueryProb),

	   format(Handle,'ex(~q,test,~q,~q,~10f,~10f).\n',
	   [Iteration,Query_ID,Query,TrueQueryProb,LearnedQueryProb]),

	   fail; % go to next test example
	   true
        ),
	!,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop save prediction test examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start save prediction training examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all training examples
	    current_predicate(user:example/4),
	    user:example(Query_ID,Query,TrueQueryProb,_),
	    query_probability(Query_ID,LearnedQueryProb),

	    format(Handle,'ex(~q,train,~q,~q,~10f,~10f).\n',
		   [Iteration,Query_ID,Query,TrueQueryProb,LearnedQueryProb]),

	    fail; % go to next training example
	    true
	),
	!,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop save prediction training examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	format(Handle,'~3n',[]),
	close(Handle).



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
	 (current_predicate(user:example/4),user:example(ID,_,_,_), \+ atomic(ID))
	->
	 (
	  format(user_error,'The example id of training example ~q ',[ID]),
	  format(user_error,'is not atomic (e.g foo42, 23, bar, ...).~n',[]),
	  throw(error(examples))
	 ); true
	),

	(
	 (current_predicate(user:test_example/4),user:test_example(ID,_,_,_), \+ atomic(ID))
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
	 (current_predicate(user:example/4),user:example(ID,_,P,_), (\+ number(P); P>1 ; P<0))
	->
	 (
	  format(user_error,'The training example ~q does not have a valid probaility value (~q).~n',[ID,P]),
	  throw(error(examples))
	 ); true
	),

	(
	 (current_predicate(user:test_example/4),user:test_example(ID,_,P,_), (\+ number(P); P>1 ; P<0))
	->
	 (
	  format(user_error,'The test example ~q does not have a valid probaility value (~q).~n',[ID,P]),
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
	   current_predicate(user:example/4),
	   user:example(ID,QueryA,_,_),
	   user:example(ID,QueryB,_,_),
	   QueryA \= QueryB
	  ) ;
	  
	  (
	   current_predicate(user:test_example/4),
	   user:test_example(ID,QueryA,_,_),
	   user:test_example(ID,QueryB,_,_),
	   QueryA \= QueryB
	  );

	  (
	   current_predicate(user:example/4),
	   current_predicate(user:test_example/4),
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

	mse_testset,
	ground_truth_difference,  
	gradient_descent,

	problog_flag(log_frequency,Log_Frequency),
	
	(
	 ( Log_Frequency>0, 0 =:= CurrentIteration mod Log_Frequency)
	->
	 (
	  once(save_predictions),
	  once(save_model)
	 );
	 true
	),

	update_values,
	
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

	logger_write_header,

	format_learning(1,'Initializing everything~n',[]),
	empty_output_directory,

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Delete the BDDs from the previous run if they should
	% not be reused
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 (
	  problog_flag(reuse_initialized_bdds,true),
	  problog_flag(rebuild_bdds,0)
	 )
	->
	 true;
	 empty_bdd_directory
	),

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Check, if continuous facts are used.
	% if yes, switch to problog_exact 
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	problog_flag(init_method,(_,_,_,_,OldCall)),
	(
	 (
	  continuous_fact(_),
	  OldCall\=problog_exact_save(_,_,_,_,_)
	 )
	->
	 (
	  format('Theory uses continuous facts.~nWill use problog_exact/3 as initalization method.~2n',[]),
	  set_problog_flag(init_method,(Query,Probability,BDDFile,ProbFile,problog_exact_save(Query,Probability,_Status,BDDFile,ProbFile)))
	 );
	 true
	),
	

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start count test examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	bb_put(test_examples,0),
	( % go over all test examples
	  current_predicate(user:test_example/4),
	  user:test_example(_,_,_,_),
	  bb_get(test_examples, OldCounter),
	  NewCounter is OldCounter+1,
	  bb_put(test_examples,NewCounter),

	  fail; % go to next text example
	  true
	),
	bb_delete(test_examples,TestExampleCount),
	format_learning(3,'~q test examples~n',[TestExampleCount]),
	!,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop count test examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start count training examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	bb_put(training_examples,0),
	( % go over all training examples
	  current_predicate(user:example/4),
	  user:example(_,_,_,_),
	  bb_get(training_examples, OldCounter),
	  NewCounter is OldCounter+1,
	  bb_put(training_examples,NewCounter),

	  fail; %go to next training example
	  true
	),
	bb_delete(training_examples,TrainingExampleCount),
	assertz(example_count(TrainingExampleCount)),
	format_learning(3,'~q training examples~n',[TrainingExampleCount]),
	!,
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop count training examples
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% set learning rate and alpha
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 problog_flag(learning_rate,examples)
	->
	 set_problog_flag(learning_rate,TrainingExampleCount);
	 true
	),

	(
	 problog_flag(alpha,auto)
	->
	 auto_alpha;
	 true
	),

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



%========================================================================
%= This predicate goes over all training and test examples,
%= calls the inference method of ProbLog and stores the resulting
%= BDDs
%========================================================================


init_queries :-
	format_learning(2,'Build BDDs for examples~n',[]),
	(			% go over all test examples
	  current_predicate(user:test_example/4),
	  user:test_example(ID,Query,Prob,_),
	  format_learning(3,' test example ~q: ~q~n',[ID,Query]),
	  flush_output(user),
	  init_one_query(ID,Query,test),

	  fail;			% go to next test example
	  true
	),
	(			% go over all training examples
	  current_predicate(user:example/4),
	  user:example(ID,Query,Prob,_),
	  format_learning(3,' training example ~q: ~q~n',[ID,Query]),
	  flush_output(user),
	  init_one_query(ID,Query,training),
	  
	  fail;			%go to next training example
	  true
	).


bdd_input_file(Filename) :-
	problog_flag(output_directory,Dir),
	concat_path_with_filename(Dir,'input.txt',Filename).

init_one_query(QueryID,Query,Type) :-
	bdd_input_file(Probabilities_File),
	problog_flag(bdd_directory,Query_Directory),

	atomic_concat(['query_',QueryID],Filename1),
	concat_path_with_filename(Query_Directory,Filename1,Filename),

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% if BDD file does not exist, call ProbLog
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 file_exists(Filename)
	->
	 format_learning(3,' Reuse existing BDD ~q~n~n',[Filename]);
	 (
	  problog_flag(init_method,(Query,_Prob,Filename,Probabilities_File,Call)),
	  once(Call),
	  delete_file(Probabilities_File)
	 )
	),
    
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% check wether this BDD is similar to another BDD
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	 problog_flag(check_duplicate_bdds,true)
	->
	 (
	  calc_md5(Filename,Query_MD5),
	  ( 
	    query_md5(OtherQueryID,Query_MD5,Type)
	  ->
	    ( 
	      assertz(query_is_similar(QueryID,OtherQueryID)),
	      format_learning(3, '~q is similar to ~q~2n', [QueryID,OtherQueryID])
	    );
	    assertz(query_md5(QueryID,Query_MD5,Type))
	  )
	 );

	 true
	),!,
	garbage_collect.




%========================================================================
%= updates all values of query_probability/2 and query_gradient/4
%= should be called always before these predicates are accessed
%= if the old values are still valid, nothing happens
%========================================================================

update_values :-
	values_correct,
	!.
update_values :-
	\+ values_correct,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% delete old values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	retractall(query_probability_intern(_,_)),
	retractall(query_gradient_intern(_,_,_,_)),	

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	bdd_input_file(Probabilities_File),
	delete_file_silent(Probabilities_File),

	open(Probabilities_File,'write',Handle),

	(			% go over all probabilistic facts
	  get_fact_probability(ID,Prob),
	  inv_sigmoid(Prob,Value),
	  (
	   non_ground_fact(ID)
	  ->
	   format(Handle,'@x~q_*~n~10f~n',[ID,Value]);
	   format(Handle,'@x~q~n~10f~n',[ID,Value])
	  ),

	  fail;			% go to next probabilistic fact
	  true
	),

 	(			% go over all continuous facts
 	 get_continuous_fact_parameters(ID,gaussian(Mu,Sigma)),
 	 %SigmaL is log(Sigma),
				SigmaL=Sigma,
 	 format(Handle,'@x~q_*~n0~n0~n~10f;~10f~n',[ID,Mu,SigmaL]),
 
 	 fail;			% go to next continuous fact
 	 true
 	),

	close(Handle),
	!,
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	assertz(values_correct).



%========================================================================
%=
%=
%=
%========================================================================

update_query_cleanup(QueryID) :-
	(
	 (query_is_similar(QueryID,_) ; query_is_similar(_,QueryID))
	->
	    % either this query is similar to another or vice versa,
	    % therefore we don't delete anything
	 true;
	 retractall(query_gradient_intern(QueryID,_,_,_))
	).


update_query(QueryID,Symbol,What_To_Update) :-
	% fixme OS trouble
	problog_flag(output_directory,Output_Directory),
	problog_flag(bdd_directory,Query_Directory),
	bdd_input_file(Probabilities_File),
	(
	 query_is_similar(QueryID,_)
	->
				% we don't have to evaluate the BDD
	 format_learning(4,'#',[]);
	 (
	  problog_flag(sigmoid_slope,Slope),
	  problog_dir(PD),
	  ((What_To_Update=all;query_is_similar(_,QueryID)) -> Method='g' ; Method='l'),
	  atomic_concat([PD,
			 '/problogbdd',
			 ' -i "', Probabilities_File, '"',
			 ' -l "', Query_Directory,'/query_',QueryID, '"',
			 ' -m ', Method,
			 ' -id ', QueryID,
			 ' -sl ', Slope,
			 ' > "',
			 Output_Directory,
			 'values.pl"'],Command),
	  shell(Command,Error),
	 

	  (
	   Error = 2
	  ->
	   throw(error('SimpleCUDD has been interrupted.'));
	   true
	  ),
	  (
	   Error \= 0
	  ->
	   (
	   format(user_error,'SimpleCUDD stopped with error code ~q, command was ~q~n',[Error, shell(Command,Error)]),
	   throw(bdd_error(QueryID,Error)));
	   true
	  ),
	  atomic_concat([Output_Directory,'values.pl'],Values_Filename),
	  (
	   file_exists(Values_Filename)
	  ->
	   (
	    (
	     once(my_load(Values_Filename,QueryID))
	    ->
	     true;
	     (
	      format(user_error,'ERROR: Tried to read the file ~q but my_load/1 fails.~n~q.~2n',[Values_Filename,update_query(QueryID,Symbol,What_To_Update)]),
	      throw(error(my_load_fails))
	     )
	    );
	    (
	     format(user_error,'ERROR: Tried to read the file ~q but it does not exist.~n~q.~2n',[Values_Filename,update_query(QueryID,Symbol,What_To_Update)]),
	     throw(error(output_file_does_not_exist))
	    )
	   )
	  ),
	  
	  delete_file(Values_Filename),
	  format_learning(4,'~w',[Symbol])
	 )
	),
	flush_output(user).


%========================================================================
%= This predicate reads probability and gradient values from the file
%= the gradient ID is a mere check to uncover hidden bugs
%= +Filename +QueryID -QueryProbability
%========================================================================

my_load(File,QueryID) :-
	open(File,'read',Handle),
	read(Handle,Atom),
	once(my_load_intern(Atom,Handle,QueryID)),
	close(Handle).
my_load(File,QueryID) :-
	format(user_error,'Error at ~q.~2n',[my_load(File,QueryID)]),
	throw(error(my_load(File,QueryID))).

my_load_intern(end_of_file,_,_) :-
	!.
my_load_intern(query_probability(QueryID,Prob),Handle,QueryID) :-
	!,
	assertz(query_probability_intern(QueryID,Prob)),
	read(Handle,X),
	my_load_intern(X,Handle,QueryID).
my_load_intern(query_gradient(QueryID,XFactID,Type,Value),Handle,QueryID) :-
	!,
	atomic_concat(x,StringFactID,XFactID),
	atom_number(StringFactID,FactID),
	assertz(query_gradient_intern(QueryID,FactID,Type,Value)),
	read(Handle,X),
	my_load_intern(X,Handle,QueryID).
my_load_intern(X,Handle,QueryID) :-
	format(user_error,'Unknown atom ~q in results file.~n',[X]),
	read(Handle,X2),
	my_load_intern(X2,Handle,QueryID).




%========================================================================
%=
%=
%=
%========================================================================
query_probability(QueryID,Prob) :-
	(
	 query_probability_intern(QueryID,Prob)
	->
	 true;
	 (
	  query_is_similar(QueryID,OtherQueryID),
	  query_probability_intern(OtherQueryID,Prob)
	 )
	).
query_gradient(QueryID,Fact,Type,Value) :-
	(
	 query_gradient_intern(QueryID,Fact,Type,Value)
	->
	 true;
	 (
	  query_is_similar(QueryID,OtherQueryID),
	  query_gradient_intern(OtherQueryID,Fact,Type,Value)
	 )
	).

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
		      get_fact_probability(FactID,Prob),
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
	(
	 current_predicate(user:example/4)
	-> 
	 (
	  update_values,
	  findall(SquaredError,
		  (user:example(QueryID,_Query,QueryProb,Type),
		   once(update_query(QueryID,'.',probability)),
		   query_probability(QueryID,CurrentProb),
		   once(update_query_cleanup(QueryID)),
		   (
		    (Type == '='; (Type == '<', CurrentProb>QueryProb); (Type=='>',CurrentProb<QueryProb))
		   ->
		    SquaredError is (CurrentProb-QueryProb)**2;
		    SquaredError = 0.0
		   )
		  ),
		   
		  AllSquaredErrors),

	  length(AllSquaredErrors,Length),
	  sum_list(AllSquaredErrors,SumAllSquaredErrors),
	  MSE is SumAllSquaredErrors/Length,
	  format_learning(3,' (~8f)~n',[MSE])
	 ); true
	),
	retractall(values_correct).

mse_testset :-
	(
	 (current_predicate(user:test_example/4),user:test_example(_,_,_,_))
	->
	 (
	  format_learning(2,'MSE_Test ',[]),
	  update_values,
	  findall(SquaredError,
		  (user:test_example(QueryID,_Query,QueryProb,Type),
		   once(update_query(QueryID,'+',probability)),
		   query_probability(QueryID,CurrentProb),
		   once(update_query_cleanup(QueryID)),
		   (
		    (Type == '='; (Type == '<', CurrentProb>QueryProb); (Type=='>',CurrentProb<QueryProb))
		   ->
		    SquaredError is (CurrentProb-QueryProb)**2;
		    SquaredError = 0.0
		   )
		  ),
		  AllSquaredErrors),
	  
	  length(AllSquaredErrors,Length),
	  sum_list(AllSquaredErrors,SumAllSquaredErrors),
	  min_list(AllSquaredErrors,MinError),
	  max_list(AllSquaredErrors,MaxError),
	  MSE is SumAllSquaredErrors/Length,

	  logger_set_variable(mse_testset,MSE),
	  logger_set_variable(mse_min_testset,MinError),
	  logger_set_variable(mse_max_testset,MaxError),
	  format_learning(2,' (~8f)~n',[MSE])
	 ); true
	).

%========================================================================
%= Calculates the sigmoid function respectivly the inverse of it
%= warning: applying inv_sigmoid to 0.0 or 1.0 will yield +/-inf
%=
%= +Float, -Float
%========================================================================

sigmoid(T,Sig) :-
	problog_flag(sigmoid_slope,Slope),
	Sig is 1/(1+exp(-T*Slope)).

inv_sigmoid(T,InvSig) :-
	problog_flag(sigmoid_slope,Slope),
	InvSig is -log(1/T-1)/Slope.






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
	(			% go over all tunable facts
	  tunable_fact(FactID,_),

	    (
	     continuous_fact(FactID)
	    ->

	     (
	      get_continuous_fact_parameters(FactID,gaussian(OldMu,OldSigma)),
	      atomic_concat(['old_mu_',FactID],Key),
	      atomic_concat(['old_sigma_',FactID],Key2),
	      bb_put(Key,OldMu),
	      bb_put(Key2,OldSigma)
	     );
	     (
	      get_fact_probability(FactID,OldProbability),
	      atomic_concat(['old_prob_',FactID],Key),
	      bb_put(Key,OldProbability)
	     )
	    ),
				
	  fail;			% go to next tunable fact
	  true
	).



forget_old_probabilities :-	
	(			% go over all tunable facts
	  tunable_fact(FactID,_),
	  (
	   continuous_fact(FactID)
	  ->
	   (
	    atomic_concat(['old_mu_',FactID],Key),
	    atomic_concat(['old_sigma_',FactID],Key2),
	    atomic_concat(['grad_mu_',FactID],Key3),
	    atomic_concat(['grad_sigma_',FactID],Key4),
	    bb_delete(Key,_),
	    bb_delete(Key2,_),
	    bb_delete(Key3,_),
	    bb_delete(Key4,_)
	   );
	   (
	    atomic_concat(['old_prob_',FactID],Key),
	    atomic_concat(['grad_',FactID],Key2),
	    bb_delete(Key,_),
	    bb_delete(Key2,_)
	   )
	  ),

	  fail;			% go to next tunable fact
	  true
	).

add_gradient(Learning_Rate) :-
	(			% go over all tunable facts
	  tunable_fact(FactID,_),
	  (
	   continuous_fact(FactID)
	  ->
	   (
	    atomic_concat(['old_mu_',FactID],Key),
	    atomic_concat(['old_sigma_',FactID],Key2),
	    atomic_concat(['grad_mu_',FactID],Key3),
	    atomic_concat(['grad_sigma_',FactID],Key4),
	    
	    bb_get(Key,Old_Mu),
	    bb_get(Key2,Old_Sigma),
	    bb_get(Key3,Grad_Mu),
	    bb_get(Key4,Grad_Sigma),

	    Mu is Old_Mu  -Learning_Rate* Grad_Mu,
	    Sigma is exp(log(Old_Sigma)  -Learning_Rate* Grad_Sigma),

	    set_continuous_fact_parameters(FactID,gaussian(Mu,Sigma))
	   );
	   (
	    atomic_concat(['old_prob_',FactID],Key),
	    atomic_concat(['grad_',FactID],Key2),

	    bb_get(Key,OldProbability),
	    bb_get(Key2,GradValue),

	    inv_sigmoid(OldProbability,OldValue),
	    NewValue is OldValue -Learning_Rate*GradValue,
	    sigmoid(NewValue,NewProbability),

	  % Prevent "inf" by using values too close to 1.0
	    Prob_Secure is min(0.999999999,max(0.000000001,NewProbability)),
	    set_fact_probability(FactID,Prob_Secure)
	    )
	  ),
				
	  fail;			% go to next tunable fact
	  true
	),
	retractall(values_correct).


gradient_descent :-
	format_learning(2,'Gradient ',[]),
	
	save_old_probabilities,
	update_values,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start set gradient to zero
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(   % go over all tunable facts

	    tunable_fact(FactID,_),
	    (
	     continuous_fact(FactID)
	    ->

	     (
	      atomic_concat(['grad_mu_',FactID],Key),
	      atomic_concat(['grad_sigma_',FactID],Key2),
	      bb_put(Key,0.0),
	      bb_put(Key2,0.0)
	     );
	     (
	      atomic_concat(['grad_',FactID],Key),
	      bb_put(Key,0.0)
	     )
	    ),
	    
	    fail; % go to next tunable fact

	    true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop gradient to zero
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start calculate gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	bb_put(mse_train_sum, 0.0),
	bb_put(mse_train_min, 0.0),
	bb_put(mse_train_max, 0.0),
	
	problog_flag(alpha,Alpha),
	logger_set_variable(alpha,Alpha),
	example_count(Example_Count),

	( % go over all training examples
	  current_predicate(user:example/4),
	  user:example(QueryID,_Query,QueryProb,Type),
	  once(update_query(QueryID,'.',all)),
	  query_probability(QueryID,BDDProb),
	  (
	   QueryProb=:=0.0
	  ->
	   Y2=Alpha;
	   Y2=1.0
	  ),
	  (
	   (Type == '='; (Type == '<', BDDProb>QueryProb); (Type=='>',BDDProb<QueryProb))
	  ->
	   Y is Y2*2/Example_Count * (BDDProb-QueryProb);
	   Y=0.0
	  ),
	  
	  
	  % first do the calculations for the MSE on training set
	  (
	   (Type == '='; (Type == '<', BDDProb>QueryProb); (Type=='>',BDDProb<QueryProb))
	  ->
	   Squared_Error is (BDDProb-QueryProb)**2;
	   Squared_Error=0.0
	  ),
	 
	  bb_get(mse_train_sum,Old_MSE_Train_Sum),
	  bb_get(mse_train_min,Old_MSE_Train_Min),
	  bb_get(mse_train_max,Old_MSE_Train_Max),
	  New_MSE_Train_Sum is Old_MSE_Train_Sum+Squared_Error,
	  New_MSE_Train_Min is min(Old_MSE_Train_Min,Squared_Error),
	  New_MSE_Train_Max is max(Old_MSE_Train_Max,Squared_Error),
	  bb_put(mse_train_sum,New_MSE_Train_Sum),
	  bb_put(mse_train_min,New_MSE_Train_Min),
	  bb_put(mse_train_max,New_MSE_Train_Max),
	  


	  ( % go over all tunable facts
	    tunable_fact(FactID,_),
	    (
	     continuous_fact(FactID)
	    ->

	     (
	      atomic_concat(['grad_mu_',FactID],Key),
	      atomic_concat(['grad_sigma_',FactID],Key2),
	    
	      % if the following query fails,
	      % it means, the fact is not used in the proof
	      % of QueryID, and the gradient is 0.0 and will
	      % not contribute to NewValue either way
	      % DON'T FORGET THIS IF YOU CHANGE SOMETHING HERE!
	      query_gradient(QueryID,FactID,mu,GradValueMu),
	      query_gradient(QueryID,FactID,sigma,GradValueSigma),

	      bb_get(Key,OldValueMu),
	      bb_get(Key2,OldValueSigma),

	      NewValueMu is OldValueMu + Y*GradValueMu,
	      NewValueSigma is OldValueSigma + Y*GradValueSigma,

	      bb_put(Key,NewValueMu),
	      bb_put(Key2,NewValueSigma)
	     );
	     (
	      atomic_concat(['grad_',FactID],Key),
	    
	      % if the following query fails,
	      % it means, the fact is not used in the proof
	      % of QueryID, and the gradient is 0.0 and will
	      % not contribute to NewValue either way
	      % DON'T FORGET THIS IF YOU CHANGE SOMETHING HERE!
	      query_gradient(QueryID,FactID,p,GradValue),

	      bb_get(Key,OldValue),
	      NewValue is OldValue + Y*GradValue,
	      bb_put(Key,NewValue)
	     )
	    ),

	    fail;		% go to next fact
	    true
	  ),
	  
	  once(update_query_cleanup(QueryID)),
	  fail;			% go to next training example
	  true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop calculate gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start statistics on gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	findall(V, (
		    tunable_fact(FactID,_),
		    atomic_concat(['grad_',FactID],Key),
		    bb_get(Key,V)
		   ),Gradient_Values),

	(
	 Gradient_Values==[]
	->
	 (
	  logger_set_variable(gradient_mean,0.0),
	  logger_set_variable(gradient_min,0.0),
	  logger_set_variable(gradient_max,0.0)
	 );
	 (
	  sum_list(Gradient_Values,GradSum),
	  max_list(Gradient_Values,GradMax),
	  min_list(Gradient_Values,GradMin),
	  length(Gradient_Values,GradLength),
	  GradMean is GradSum/GradLength,

	  logger_set_variable(gradient_mean,GradMean),
	  logger_set_variable(gradient_min,GradMin),
	  logger_set_variable(gradient_max,GradMax)
	 )
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop statistics on gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	bb_delete(mse_train_sum,MSE_Train_Sum),
	bb_delete(mse_train_min,MSE_Train_Min),
	bb_delete(mse_train_max,MSE_Train_Max),
	MSE is MSE_Train_Sum/Example_Count,

	logger_set_variable(mse_trainingset,MSE),
	logger_set_variable(mse_min_trainingset,MSE_Train_Min),
	logger_set_variable(mse_max_trainingset,MSE_Train_Max),

	format_learning(2,'~n',[]),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start add gradient to current probabilities
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	    problog_flag(line_search,false)
	->
	    problog_flag(learning_rate,LearningRate);
	    lineSearch(LearningRate,_)
	),
	format_learning(3,'learning rate:~8f~n',[LearningRate]),
	add_gradient(LearningRate),
	logger_set_variable(learning_rate,LearningRate),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop add gradient to current probabilities
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,
	forget_old_probabilities.

%========================================================================
%=
%=
%========================================================================

line_search_evaluate_point(Learning_Rate,MSE) :-
	add_gradient(Learning_Rate),
	format_learning(2,'Line search (h=~8f) ',[Learning_Rate]),
	mse_trainingset_only_for_linesearch(MSE).


lineSearch(Final_X,Final_Value) :-

	% Get Parameters for line search
	problog_flag(line_search_tolerance,Tol),
	problog_flag(line_search_tau,Tau),
	problog_flag(line_search_interval,(A,B)),

	format_learning(3,'Line search in interval (~4f,~4f)~n',[A,B]),
	
	% init values
	Acc is Tol * (B-A),
	InitRight is A + Tau*(B-A),
	InitLeft is A + B - InitRight,

	line_search_evaluate_point(A,Value_A),
	line_search_evaluate_point(B,Value_B),
	line_search_evaluate_point(InitRight,Value_InitRight),
	line_search_evaluate_point(InitLeft,Value_InitLeft),

	bb_put(line_search_a,A),
	bb_put(line_search_b,B),
	bb_put(line_search_left,InitLeft),
	bb_put(line_search_right,InitRight),

	bb_put(line_search_value_a,Value_A),
	bb_put(line_search_value_b,Value_B),
	bb_put(line_search_value_left,Value_InitLeft),
	bb_put(line_search_value_right,Value_InitRight),

	bb_put(line_search_iteration,1),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% BEGIN BACK TRACKING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(
	    repeat, 

	    bb_get(line_search_iteration,Iteration),
	    bb_get(line_search_a,Ak),
	    bb_get(line_search_b,Bk),
	    bb_get(line_search_left,Left),
	    bb_get(line_search_right,Right),

	    bb_get(line_search_value_a,Fl),
	    bb_get(line_search_value_b,Fr),
	    bb_get(line_search_value_left,FLeft),
	    bb_get(line_search_value_right,FRight),

	 (
		% check for infinity, if there is, go to the left
		( FLeft >= FRight, \+ FLeft = (+inf), \+ FRight = (+inf) ) 
	    ->
	     (
		 AkNew=Left,
		 FlNew=FLeft,
		 LeftNew=Right,
		 FLeftNew=FRight,
		 RightNew is AkNew + Bk - LeftNew,
		 line_search_evaluate_point(RightNew,FRightNew),
		 BkNew=Bk,
		 FrNew=Fr
		);
		(
		 BkNew=Right,
		 FrNew=FRight,
		 RightNew=Left,
		 FRightNew=FLeft,
		 LeftNew is Ak + BkNew - RightNew,

		 line_search_evaluate_point(LeftNew,FLeftNew),
		 AkNew=Ak,
		 FlNew=Fl
		 )
	 ),

	 Next_Iteration is Iteration + 1,
	 
	 bb_put(line_search_iteration,Next_Iteration),

	 bb_put(line_search_a,AkNew),
	 bb_put(line_search_b,BkNew),
	 bb_put(line_search_left,LeftNew),
	 bb_put(line_search_right,RightNew),

	 bb_put(line_search_value_a,FlNew),
	 bb_put(line_search_value_b,FrNew),
	 bb_put(line_search_value_left,FLeftNew),
	 bb_put(line_search_value_right,FRightNew),

	% is the search interval smaller than the tolerance level?
	 BkNew-AkNew<Acc,

	% apperantly it is, so get me out of here and
	% cut away the choice point from repeat
	 !  
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% END BACK TRACKING
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        % clean up the blackboard mess
	bb_delete(line_search_iteration,_),
	bb_delete(line_search_a,_),
	bb_delete(line_search_b,_),
	bb_delete(line_search_left,_),
	bb_delete(line_search_right,_),
	bb_delete(line_search_value_a,_),
	bb_delete(line_search_value_b,_),
	bb_delete(line_search_value_left,_),
	bb_delete(line_search_value_right,_),

	% it doesn't harm to check also the value in the middle
	% of the current search interval
	Middle is (AkNew + BkNew) / 2.0,
	line_search_evaluate_point(Middle,Value_Middle),	

	% return the optimal value
	my_5_min(Value_Middle,FlNew,FrNew,FLeftNew,FRightNew,
		 Middle,AkNew,BkNew,LeftNew,RightNew,
		 Optimal_Value,Optimal_X),

	line_search_postcheck(Optimal_Value,Optimal_X,Final_Value,Final_X).

line_search_postcheck(V,X,V,X) :-
	X>0,
	!.
line_search_postcheck(V,X,V,X) :-
	problog_flag(line_search_never_stop,false),
	!.
line_search_postcheck(_,_, LLH, FinalPosition) :-
	problog_flag(line_search_tolerance,Tolerance),
	problog_flag(line_search_interval,(Left,Right)),


	Offset is (Right - Left) * Tolerance,

	bb_put(line_search_offset,Offset),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(

	 repeat,
	 
	 bb_get(line_search_offset,OldOffset),
	 NewOffset is OldOffset * Tolerance,
	 bb_put(line_search_offset,NewOffset),

	 Position is Left + NewOffset,
	 line_search_evaluate_point(Position,LLH),
	 bb_put(line_search_llh,LLH),

	 write(logAtom(lineSearchPostCheck(Position,LLH))),nl,


	 \+ LLH = (+inf),
	 !
	),  % cut away choice point from repeat
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	bb_delete(line_search_llh,LLH),
	bb_delete(line_search_offset,FinalOffset),
	FinalPosition is Left + FinalOffset.



my_5_min(V1,V2,V3,V4,V5,F1,F2,F3,F4,F5,VMin,FMin) :-
	(
	    V1<V2
	->
	 (VTemp1=V1,FTemp1=F1);
	 (VTemp1=V2,FTemp1=F2)
	),
	(
	 V3<V4
	->
	 (VTemp2=V3,FTemp2=F3);
	 (VTemp2=V4,FTemp2=F4)
	),
	(
	 VTemp1<VTemp2
	->
	 (VTemp3=VTemp1,FTemp3=FTemp1);
	 (VTemp3=VTemp2,FTemp3=FTemp2)
	),
	(
	 VTemp3<V5
	->
	 (VMin=VTemp3,FMin=FTemp3);
	 (VMin=V5,FMin=F5)
	).


%========================================================================
%= set the alpha parameter to the value
%=  # positive training examples / # negative training examples
%=
%= training example is positive if P(e)=1
%= training example is negative if P(e)=0
%=
%= if there are training example with 0<P<1, set alpha=1.0
%========================================================================


auto_alpha :-
	\+ current_predicate(user:example/4),
	!,
	set_problog_flag(alpha,1.0).
auto_alpha :-
	user:example(_,_,P,_),
	P<1,
	P>0,
	!,
	set_problog_flag(alpha,1.0).
auto_alpha :-
	findall(1,(user:example(_,_,P,=),P=:=1.0),Pos),
	findall(0,(user:example(_,_,P,=),P=:=0.0),Neg),
	length(Pos,NP),
	length(Neg,NN),
	Alpha is NP/NN,
	set_problog_flag(alpha,Alpha).



%========================================================================
%= initialize the logger module and set the flags for learning
%= don't change anything here! use set_problog_flag/2 instead
%========================================================================

init_flags :-
	prolog_file_name('queries',Queries_Folder), % get absolute file name for './queries'
	prolog_file_name('output',Output_Folder), % get absolute file name for './output'
	problog_define_flag(bdd_directory, problog_flag_validate_directory, 'directory for BDD scripts', Queries_Folder,learning_general),
	problog_define_flag(output_directory, problog_flag_validate_directory, 'directory for logfiles etc', Output_Folder,learning_general,flags:learning_output_dir_handler),
	problog_define_flag(log_frequency, problog_flag_validate_posint, 'log results every nth iteration', 1, learning_general),
	problog_define_flag(rebuild_bdds, problog_flag_validate_nonegint, 'rebuild BDDs every nth iteration', 0, learning_general),
	problog_define_flag(reuse_initialized_bdds,problog_flag_validate_boolean, 'Reuse BDDs from previous runs',false, learning_general),	
	problog_define_flag(check_duplicate_bdds,problog_flag_validate_boolean,'Store intermediate results in hash table',true,learning_general),
	problog_define_flag(init_method,problog_flag_validate_dummy,'ProbLog predicate to search proofs',(Query,Probability,BDDFile,ProbFile,problog_kbest_save(Query,100,Probability,_Status,BDDFile,ProbFile)),learning_general,flags:learning_init_handler),
	problog_define_flag(alpha,problog_flag_validate_number,'weight of negative examples (auto=n_p/n_n)',auto,learning_general,flags:auto_handler),
	problog_define_flag(sigmoid_slope,problog_flag_validate_posnumber,'slope of sigmoid function',1.0,learning_general),

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
	logger_define_variable(alpha,float).

:- initialization(init_flags).
:- initialization(init_logger).

