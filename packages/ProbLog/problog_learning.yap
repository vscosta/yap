%%% -*- Mode: Prolog; -*-

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
	            do_learning/2,
	            set_learning_flag/2,
		    save_model/1,
		    problog_help/0,
		    set_problog_flag/2,
		    problog_flag/2,
		    problog_flags/0
		    ]).

% switch on all the checks to reduce bug searching time
:- style_check(all).
:- yap_flag(unknown,error).

% load modules from the YAP library
:- use_module(library(lists)).
:- use_module(library(random),[random/1]).
:- use_module(library(system),[file_exists/1,
			       file_property/2,
			       delete_file/1,
			       make_directory/1,
			       working_directory/2,
			       shell/1,
	                       shell/2]).

% load our own modules
:- use_module(library('problog_learning/logger')).
:- use_module(library(problog)).

% used to indicate the state of the system
:- dynamic values_correct/0.
:- dynamic learning_initialized/0.
:- dynamic current_iteration/1.
:- dynamic example_count/1.
:- dynamic query_probability_intern/2.
:- dynamic query_gradient_intern/3.
:- dynamic last_mse/1.

% used to identify queries which have identical proofs
:- dynamic query_is_similar/2.
:- dynamic query_md5/2.

% used by set_learning_flag
:- dynamic init_method/5.
:- dynamic rebuild_bdds/1.
:- dynamic rebuild_bdds_it/1.
:- dynamic reuse_initialized_bdds/1.
:- dynamic learning_rate/1.
:- dynamic probability_initializer/3.
:- dynamic check_duplicate_bdds/1.
:- dynamic output_directory/1.
:- dynamic query_directory/1.
:- dynamic log_frequency/1.
:- dynamic alpha/1.
:- dynamic sigmoid_slope/1.
:- dynamic line_search/1.
:- dynamic line_search_tolerance/1.
:- dynamic line_search_tau/1.
:- dynamic line_search_never_stop/1.
:- dynamic line_search_interval/2.


%==========================================================================
%=  You can set some flags and parameters
%=
%=  init_method/5 specifies which ProbLog inference mechanism is used
%=  to answer queries
%=
%=
%=  if rebuild_bdds(true) is set, the bdds are rebuild after
%=  each N iterations for rebuild_bdds_it(N) 
%=
%=  if reuse_initialized_bdds(true) is set, the bdds which are on the
%=  harddrive from the previous run of LeProbLog are reused.
%=  do not use this, when you changed the init method in the meantime
%=
%==========================================================================
set_learning_flag(init_method,(Query,Probability,BDDFile,ProbFile,Call)) :-
	retractall(init_method(_,_,_,_,_)),
	assert(init_method(Query,Probability,BDDFile,ProbFile,Call)).

set_learning_flag(rebuild_bdds,Flag) :-
	(Flag=true;Flag=false),
	!,
	retractall(rebuild_bdds(_)),
	assert(rebuild_bdds(Flag)).

set_learning_flag(rebuild_bdds_it,Flag) :-
	integer(Flag),
	retractall(rebuild_bdds_it(_)),
	assert(rebuild_bdds_it(Flag)).


set_learning_flag(reuse_initialized_bdds,Flag) :-
	(Flag=true;Flag=false),
	!,
	retractall(reuse_initialized_bdds(_)),
	assert(reuse_initialized_bdds(Flag)).

set_learning_flag(learning_rate,V) :-
	(V=examples -> true;(number(V),V>=0)),
	!,
	retractall(learning_rate(_)),
	assert(learning_rate(V)).

set_learning_flag(probability_initializer,(FactID,Probability,Query)) :-
	var(FactID),
	var(Probability),
	callable(Query),
	retractall(probability_initializer(_,_,_)),
	assert(probability_initializer(FactID,Probability,Query)).

set_learning_flag(check_duplicate_bdds,Flag) :-
	(Flag=true;Flag=false),
	!,
	retractall(check_duplicate_bdds(_)),
	assert(check_duplicate_bdds(Flag)).

set_learning_flag(output_directory,Directory) :-
	(
	    file_exists(Directory)
	->
	    file_property(Directory,type(directory));
	    make_directory(Directory)
	),

	working_directory(PWD,PWD),
	atomic_concat([PWD,'/',Directory,'/'],Path),
	atomic_concat([Directory,'/log.dat'],Logfile),
	
	retractall(output_directory(_)),
	assert(output_directory(Path)),
	logger_set_filename(Logfile),
	set_problog_flag(dir,Directory).

set_learning_flag(query_directory,Directory) :-
	(
	    file_exists(Directory)
	->
	    file_property(Directory,type(directory));
	    make_directory(Directory)
	),
	
	working_directory(PWD,PWD),
	atomic_concat([PWD,'/',Directory,'/'],Path),
	retractall(query_directory(_)),
	assert(query_directory(Path)).

set_learning_flag(log_frequency,Frequency) :-
	integer(Frequency),
	Frequency>=0,
	retractall(log_frequency(_)),
	assert(log_frequency(Frequency)).

set_learning_flag(alpha,Alpha) :-
	number(Alpha),
	retractall(alpha(_)),
	assert(alpha(Alpha)).
set_learning_flag(sigmoid_slope,Slope) :-
	number(Slope),
	Slope>0,
	retractall(sigmoid_slope(_)),
	assert(sigmoid_slope(Slope)).


set_learning_flag(line_search,Flag) :-
	(Flag=true;Flag=false),
	!,
	retractall(line_search(_)),
	assert(line_search(Flag)).
set_learning_flag(line_search_tolerance,Number) :-
	number(Number),
	Number>0,
	retractall(line_search_tolerance(_)),
	assert(line_search_tolerance(Number)).
set_learning_flag(line_search_interval,(L,R)) :-
	number(L),
	number(R),
	L<R,
	retractall(line_search_interval(_,_)),
	assert(line_search_interval(L,R)).
set_learning_flag(line_search_tau,Number) :-
	number(Number),
	Number>0,
	retractall(line_search_tau(_)),
	assert(line_search_tau(Number)).
set_learning_flag(line_search_never_stop,Flag) :-
	(Flag=true;Flag=false),
	!,
	retractall(line_search_nerver_stop(_)),
	assert(line_search_never_stop(Flag)).


%========================================================================
%= store the facts with the learned probabilities to a file
%= if F is a variable, a filename based on the current iteration is used
%=
%========================================================================
save_model(F) :-
	(
	    var(F)
	->  
	    (
		current_iteration(Iteration),
		output_directory(Directory),
		atomic_concat([Directory,'factprobs_',Iteration,'.pl'],F)
	    );true
	),
	export_facts(F).

%========================================================================
%= store the probabilities for all training and test examples
%= if F is a variable, a filename based on the current iteration is used
%=
%========================================================================
save_predictions(F) :-
	update_values,

	current_iteration(Iteration),

	(
	    var(F)
	->  
	    (
		current_iteration(Iteration),
		output_directory(Directory),
		atomic_concat([Directory,'predictions_',Iteration,'.pl'],F)
	    );true
	),

	open(F,'append',Handle),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[]),
	format(Handle,"% Iteration, train/test, QueryID, Query, GroundTruth, Prediction %\n",[]),
	format(Handle,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[]),
	!,

	( % go over all training examples
	    current_predicate(user:example/3),
	    user:example(Query_ID,Query,TrueQueryProb),
	    query_probability(Query_ID,LearnedQueryProb),

	    format(Handle,'ex(~q,train,~q,~q,~10f,~10f).\n',
		   [Iteration,Query_ID,Query,TrueQueryProb,LearnedQueryProb]),

	    fail; % go to next training example
	    true
	),
	
	( % go over all test examples
	   current_predicate(user:test_example/3),
	   user:test_example(Query_ID,Query,TrueQueryProb),
	   query_probability(Query_ID,LearnedQueryProb),

	   format(Handle,'ex(~q,test,~q,~q,~10f,~10f).\n',
	   [Iteration,Query_ID,Query,TrueQueryProb,LearnedQueryProb]),

	   fail; % go to next test example
	   true
        ),
	format(Handle,'~3n',[]),
	close(Handle).



%========================================================================
%= find out whether some example IDs are used more than once
%= if so, complain and stop
%=
%========================================================================

check_examples :-
	(
	    (
		(current_predicate(user:example/3),user:example(ID,_,_), \+ atomic(ID)) ;
		(current_predicate(user:test_example/3),user:test_example(ID,_,_), \+ atomic(ID))
	    )
	->
	    (
		format(user_error,'The example id of example ~q is not atomic (e.g foo42, 23, bar, ...).~n',[ID]),
		throw(error(examples))
	    ); true
	),

	(
	    (
		(current_predicate(user:example/3),user:example(ID,_,P), (\+ number(P); P>1 ; P<0));
		(current_predicate(user:test_example/3),user:test_example(ID,_,P), (\+ number(P) ;  P>1 ; P<0))
	    )
	->
	    (
		format(user_error,'The example ~q does not have a valid probaility value (~q).~n',[ID,P]),
		throw(error(examples))
	    ); true
	),


	(
	    (
		(
		    current_predicate(user:example/3),
		    user:example(ID,QueryA,_),
		    user:example(ID,QueryB,_),
		    QueryA \= QueryB
		) ;
	
		(
		    current_predicate(user:test_example/3),
		    user:test_example(ID,QueryA,_),
		    user:test_example(ID,QueryB,_),
		    QueryA \= QueryB
		);

		(
		    current_predicate(user:example/3),
		    current_predicate(user:test_example/3),
		    user:example(ID,QueryA,_),
		    user:test_example(ID,QueryB,_),
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
	integer(Iterations),
	
	( 
	    current_predicate(user:example/3)
	->
	    true;
	    format(user_error,'~n~nWarning: No training examples specified !!!~n~n',[])
	),

	do_learning_intern(Iterations,-1).

do_learning(Iterations,Epsilon) :-
	integer(Iterations),
	float(Epsilon),

	Iterations>0,
	Epsilon>0.0,

	( 
	    current_predicate(user:example/3)
	->
	    true;
	    format(user_error,'~n~nWarning: No training examples specified !!!~n~n',[])
	),

	do_learning_intern(Iterations,Epsilon).

	





do_learning_intern(Iterations,Epsilon) :-
	(
	    Iterations=0
	->
	    true;
	    (
		Iterations>0,

		% nothing will happen, if we're already initialized
		init_learning,
		current_iteration(OldIteration),
		!,
		retractall(current_iteration(_)),
		!,
		CurrentIteration is OldIteration+1,
		assert(current_iteration(CurrentIteration)),
		EndIteration is OldIteration+Iterations,
		
		format('~n Iteration ~d of ~d~n',[CurrentIteration,EndIteration]),
		logger_set_variable(iteration,CurrentIteration),

		logger_start_timer(duration),
		gradient_descent,

		(
		    (rebuild_bdds(true),rebuild_bdds_it(BDDFreq),0 =:= CurrentIteration mod BDDFreq)
		->
		    (
			once(delete_all_queries),
			once(init_queries)
		    ); true
		),


		mse_trainingset,
		mse_testset,
		
		(
		    last_mse(Last_MSE)
		->
		    (
			retractall(last_mse(_)),
			logger_get_variable(mse_trainingset,Current_MSE),
			assert(last_mse(Current_MSE)),
			!,
			MSE_Diff is abs(Last_MSE-Current_MSE)
		    );  (
		        logger_get_variable(mse_trainingset,Current_MSE),
			assert(last_mse(Current_MSE)), 
			MSE_Diff is Epsilon+1
		    )
		),

		!,
		logger_stop_timer(duration),
		once(ground_truth_difference),

		logger_write_data,


		log_frequency(Log_Frequency),
		
		(
		    ( Log_Frequency=0; 0 =:= CurrentIteration mod Log_Frequency)
		->
		   (
		       save_predictions(_X),
		       save_model(_Y)
		   );
		   true
	        ),
		RemainingIterations is Iterations-1,

		(
		    MSE_Diff>Epsilon
		->
		    do_learning_intern(RemainingIterations,Epsilon);
		    true
		)
	    )
	).
	    


%========================================================================
%= find proofs and build bdds for all training and test examples
%=
%=
%========================================================================

init_learning :-
	(
	    learning_initialized
	->  
	    true;
	    (
		
		check_examples,


		format('Delete previous logs (if existing) from output directory~2n',[]),
		empty_output_directory,

		format('Initializing everything~n',[]),


                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		% Delete the BDDs from the previous run if they should
	        % not be reused
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	        (
		    (reuse_initialized_bdds(false);rebuild_bdds(true))
		->
		    delete_all_queries;
		    true
		),


		logger_write_header,
		logger_start_timer(duration),
		logger_set_variable(iteration,0),

                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		% start count examples
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	        bb_put(training_examples,0),
	        ( % go over all training examples
		    current_predicate(user:example/3),
		    user:example(_,_,_),
		    bb_get(training_examples, OldCounter),
		    NewCounter is OldCounter+1,
		    bb_put(training_examples,NewCounter),
		    fail;
		    true
		),

	        bb_put(test_examples,0),
	        ( % go over all test examples
		    current_predicate(user:test_example/3),
		    user:test_example(_,_,_),
		    bb_get(test_examples, OldCounter),
		    NewCounter is OldCounter+1,
		    bb_put(test_examples,NewCounter),
		    fail;
		    true
		),
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		% stop count examples
                %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	        !,

		bb_delete(training_examples,TrainingExampleCount),
		bb_delete(test_examples,TestExampleCount),

		assert(example_count(TrainingExampleCount)),

		(
		    learning_rate(examples)
		->
		    set_learning_flag(learning_rate,TrainingExampleCount);
		    true
		),
		learning_rate(Learning_Rate),
		format('~q training examples found.~n~q test examples found.~nlearning rate=~f~n~n',
		[TrainingExampleCount,TestExampleCount,Learning_Rate]),
		format('Generate BDDs for all queries in the training and test set~n',[]),
		initialize_fact_probabilities,
		init_queries,

		format('All Queries have been generated~n',[]),
		mse_trainingset,
		mse_testset,

		!,
		logger_stop_timer(duration),

		ground_truth_difference,

		logger_write_data,
		assert(current_iteration(0)),
		assert(learning_initialized),
		save_model(_),save_predictions(_)

	    )
	).

%========================================================================
%= 
%= 
%= 
%========================================================================



delete_all_queries :-
	query_directory(Directory),
	atomic_concat(['rm -f ',Directory,'query_*'],Command),
	(shell(Command) -> true; true),
	retractall(query_is_similar(_,_)),
	retractall(query_md5(_,_)).

empty_output_directory :-
	output_directory(Directory),
	atomic_concat(['rm -f ',Directory,'log.dat ',
                                Directory,'factprobs_*.pl ',
				Directory,'predictions_*.pl'],Command),
	(shell(Command) -> true; true).

%========================================================================
%= This predicate goes over all training and test examples,
%= calls the inference method of ProbLog and stores the resulting
%= BDDs
%========================================================================


init_queries :-

	( % go over all training examples
	    current_predicate(user:example/3),
	    user:example(ID,Query,Prob),
	    format('~n~n training example ~q: ~q~n',[ID,Query]),
	    flush_output(user),
	    init_one_query(ID,Query),
	    fail; %go to next training example

	    true
	),

	( % go over all test examples
	    current_predicate(user:test_example/3),
	    user:test_example(ID,Query,Prob),
	    format('~n~n test example ~q: ~q~n',[ID,Query]),
	    flush_output(user),
	    init_one_query(ID,Query),
	    fail; % go to next test example

	    true
	).

init_one_query(QueryID,Query) :-
	output_directory(Output_Directory),
	query_directory(Query_Directory),

	atomic_concat([Query_Directory,'query_',QueryID],Filename),
	atomic_concat([Output_Directory,'input.txt'],Filename2),
	atomic_concat([Output_Directory,'tmp_md5'],Filename3),

	    (
		file_exists(Filename)
	    ->
		format('Reuse existing BDD ~q~n~n',[Filename]);
		(
		    init_method(Query,_Prob,Filename,Filename2,InitCall),
		    once(call(InitCall)),
		    delete_file(Filename2)
	        )
	    ),

	    (
		check_duplicate_bdds(true)
	    ->
	        (
		    % calculate the md5sum of the bdd script file
		    atomic_concat(['cat ',Filename,' | md5sum | sed "s/ .*$/\')./"  | sed "s/^/md5(\'/"> ',Filename3],MD5Command),

		    (shell(MD5Command,0) -> true; throw(error("Something wrong with calculating the MD5 value"))),

		    open(Filename3, read, Handle),
		    read(Handle,md5(Query_MD5)),
		    close(Handle),

		    delete_file(Filename3),
	
        	    % Does another query exists which already has this MD5?
		    (
			query_md5(OtherQueryID,Query_MD5)
		    ->
			% yippie! we can save a lot of work
	                ( 
			    assert(query_is_similar(QueryID,OtherQueryID)),
			    format('~q is similar to ~q~2n', [QueryID,OtherQueryID])
			); assert(query_md5(QueryID,Query_MD5))
		    )
		);

		true
	    ).
		

%========================================================================
%= set all unknown fact probabilities to random values
%=
%=
%========================================================================

initialize_fact_probabilities :-
	( % go over all tunable facts
	    tunable_fact(FactID,_),

	    probability_initializer(FactID,Probability,Query),
	    once(call(Query)),
	    set_fact_probability(FactID,Probability),


	    fail; % go to next tunable fact
	    
	    true
	).

random_probability(_FactID,Probability) :-
	% use probs around 0.5 to not confuse k-best search
	random(Random),
	Probability is 0.5+(Random-0.5)/100.
	




%========================================================================
%= updates all values of query_probability/2 and query_gradient/3
%= should be called always before these predicates are accessed
%= if the old values are still valid, nothing happens
%========================================================================

update_values :-
	update_values(all).


update_values(_) :-
	values_correct,
	!.

update_values(What_To_Update) :-
	\+ values_correct,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% delete old values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	once(retractall(query_probability_intern(_,_))),
	once(retractall(query_gradient_intern(_,_,_))),


	output_directory(Directory),
	atomic_concat(Directory,'input.txt',Input_Filename),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	open(Input_Filename,'write',Handle),

	( % go over all probabilistic fact
	    get_fact_probability(ID,Prob),
	    inv_sigmoid(Prob,Value),
	    (
		non_ground_fact(ID)
	    ->
	        format(Handle,'@x~q_*~n~10f~n',[ID,Value]);
		format(Handle,'@x~q~n~10f~n',[ID,Value])
	    ),

	    fail; % go to next probabilistic fact
	    true
	),

	close(Handle),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,




	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start update values for all test examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ( What_To_Update = all
          ->
             ( % go over all training examples
		 current_predicate(user:test_example/3),
		 user:test_example(QueryID,_Query,_QueryProb),
		 once(call_bdd_tool(QueryID,'+',all)),
		 fail; % go to next training example
		 true
	     ); true
	 ),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop update values for all test examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start update values for all training examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all training examples
	    current_predicate(user:example/3),
	    user:example(QueryID,_Query,_QueryProb),
	    once(call_bdd_tool(QueryID,'.',What_To_Update)),
	    fail; % go to next training example
	    true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop update values for all training examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,


	nl,

	delete_file(Input_Filename),
	assert(values_correct).


%========================================================================
%=
%=
%=
%========================================================================


call_bdd_tool(QueryID,Symbol,What_To_Update) :-
	output_directory(Output_Directory),
	query_directory(Query_Directory),
	(
	    query_is_similar(QueryID,_)
	->
	    % we don't have to evaluate the BDD
	    write('#');
	    (
		sigmoid_slope(Slope),
		problog_dir(PD),
		(What_To_Update=all -> Method='g' ; Method='l'),
		atomic_concat([PD,
				'/ProblogBDD -i "',
		                  Output_Directory,
				  'input.txt',
				  '" -l "',
			          Query_Directory,
			          'query_',
				  QueryID,
				  '" -m ',Method,' -id ',
				  QueryID,
				  ' -sl ',Slope,
				  ' > "',
				  Output_Directory,
				  'values.pl"'],Command),
		shell(Command,Error),
	     
	 	atomic_concat([' cat "',
				  Output_Directory,
				  'values.pl" >> ~/all_results'],Command2),
	       shell(Command2),
		(
		    Error = 2
		->
		    throw(error('SimpleCUDD has been interrupted.'));
		    true
		),
		(
		    Error \= 0
		->
		    throw(bdd_error(QueryID,Error));
		    true
		),
		atomic_concat([Output_Directory,'values.pl'],Values_Filename),
	     (
	      file_exists(Values_Filename)
	     ->
	      (
	       (
		once(my_load(Values_Filename))
	       ->
		true;
		(
		 format(user_error,'ERROR: Tried to read the file ~q but my_load/1 fails.~n~q.~2n',[Values_Filename,call_bdd_tool(QueryID,Symbol,What_To_Update)]),
		 throw(error(my_load_fails))
		)	 
	      );
	      (
	       format(user_error,'ERROR: Tried to read the file ~q but it does not exist.~n~q.~2n',[Values_Filename,call_bdd_tool(QueryID,Symbol,What_To_Update)]),
	       throw(error(output_file_does_not_exist))
	      )
	     )
	     ),
		      
		delete_file(Values_Filename),
		write(Symbol)
	    )
	),
	flush_output(user).
	    

%========================================================================
%=
%=
%=
%========================================================================

my_load(File) :-
	see(File),
	read(X),
	my_load_intern(X),
	seen.
my_load_intern(end_of_file) :-
	!.
my_load_intern(query_probability(QueryID,Prob)) :-
	!,
	assert(query_probability_intern(QueryID,Prob)),
	read(X2),
	my_load_intern(X2).
my_load_intern(query_gradient(QueryID,XFactID,Value)) :-
	!,
	atomic_concat(x,StringFactID,XFactID),
	atom_number(StringFactID,FactID),
	assert(query_gradient_intern(QueryID,FactID,Value)),
	read(X2),
	my_load_intern(X2).
my_load_intern(X) :-
	format(user_error,'Unknown atom ~q in results file.~n',[X]),
	read(X2),
	my_load_intern(X2).


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
query_gradient(QueryID,Fact,Value) :-
	(
	    query_gradient_intern(QueryID,Fact,Value)
	->
	    true;
	    (
		query_is_similar(QueryID,OtherQueryID),
		query_gradient_intern(OtherQueryID,Fact,Value)
	    )
	).

%========================================================================
%=
%=
%=
%========================================================================


ground_truth_difference :-
	findall(Diff,(tunable_fact(FactID,GroundTruth),
		      \+ var(GroundTruth),
		      get_fact_probability(FactID,Prob),
		      Diff is abs(GroundTruth-Prob)),AllDiffs),

	% if no ground truth was specified for facts
	% set everything to zero
	(
	    AllDiffs=[]
	->
	    (
		MinDiff=0.0,
		MaxDiff=0.0,
		DiffMean=0.0
	    ) ; (
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
	    current_predicate(user:example/3)
	-> 
	    (
		update_values(probabilities),
		findall(SquaredError,
		        (user:example(QueryID,_Query,QueryProb),
			query_probability(QueryID,CurrentProb),
			SquaredError is (CurrentProb-QueryProb)**2),
			AllSquaredErrors),

		length(AllSquaredErrors,Length),
		sum_list(AllSquaredErrors,SumAllSquaredErrors),
		MSE is SumAllSquaredErrors/Length
	    ); true
	),
	retractall(values_correct).

% calculate the mse of the training data
mse_trainingset :-
	(
	    current_predicate(user:example/3)
	-> 
	    (
		update_values,
		findall(SquaredError,
		        (user:example(QueryID,_Query,QueryProb),
			query_probability(QueryID,CurrentProb),
			SquaredError is (CurrentProb-QueryProb)**2),
			AllSquaredErrors),

		length(AllSquaredErrors,Length),
		sum_list(AllSquaredErrors,SumAllSquaredErrors),
		min_list(AllSquaredErrors,MinError),
		max_list(AllSquaredErrors,MaxError),
		MSE is SumAllSquaredErrors/Length,

		logger_set_variable(mse_trainingset,MSE),
		logger_set_variable(mse_min_trainingset,MinError),
		logger_set_variable(mse_max_trainingset,MaxError)
	    ); true
	).



mse_testset :-
	(
	    current_predicate(user:test_example/3)
	->
	    (
		update_values,
		findall(SquaredError,
			 (user:test_example(QueryID,_Query,QueryProb),
			  query_probability(QueryID,CurrentProb),
			  SquaredError is (CurrentProb-QueryProb)**2),
			AllSquaredErrors),

		length(AllSquaredErrors,Length),
		sum_list(AllSquaredErrors,SumAllSquaredErrors),
		min_list(AllSquaredErrors,MinError),
		max_list(AllSquaredErrors,MaxError),
		MSE is SumAllSquaredErrors/Length,

		logger_set_variable(mse_testset,MSE),
		logger_set_variable(mse_min_testset,MinError),
		logger_set_variable(mse_max_testset,MaxError)
	     ); true
	 ).



%========================================================================
%= Calculates the sigmoid function respectivly the inverse of it
%= warning: applying inv_sigmoid to 0.0 or 1.0 will yield +/-inf
%=
%= +Float, -Float
%========================================================================

sigmoid(T,Sig) :-
	sigmoid_slope(Slope),
	Sig is 1/(1+exp(-T*Slope)).

inv_sigmoid(T,InvSig) :-
	sigmoid_slope(Slope),
	InvSig is -log(1/T-1)/Slope.


%========================================================================
%= this functions truncates probabilities too close to 1.0 or 0.0
%= the reason is, applying the inverse sigmoid function would yield +/- inf
%= for such extreme values
%=
%= +Float, -Float
%========================================================================

secure_probability(Prob,Prob_Secure) :-
	TMP is max(0.000000001,Prob),
	Prob_Secure is min(0.999999999,TMP).



%========================================================================
%= Perform one iteration of gradient descent
%=
%= assumes that everything is initialized, if the current values
%= of query_probability/2 and query_gradient/3 are not up to date
%= they will be recalculated
%= finally, the values_correct/0 is retracted to signal that the
%= probabilities of the examples have to be recalculated
%========================================================================

save_old_probabilities :-
	( % go over all tunable facts

	    tunable_fact(FactID,_),
	    get_fact_probability(FactID,OldProbability),
	    atomic_concat(['old_prob_',FactID],Key),
	    bb_put(Key,OldProbability),

	    fail; % go to next tunable fact
	    true
	).

forget_old_values :-	
	( % go over all tunable facts

	    tunable_fact(FactID,_),
	    atomic_concat(['old_prob_',FactID],Key),
	    atomic_concat(['grad_',FactID],Key2),
	    bb_delete(Key,_),
	    bb_delete(Key2,_),

	    fail; % go to next tunable fact
	    true
	).

add_gradient(Learning_Rate) :-
	( % go over all tunable facts

	    tunable_fact(FactID,_),
	    atomic_concat(['old_prob_',FactID],Key),
	    atomic_concat(['grad_',FactID],Key2),

	    bb_get(Key,OldProbability),
	    bb_get(Key2,GradValue),

	    inv_sigmoid(OldProbability,OldValue),
	    NewValue is OldValue -Learning_Rate*GradValue,
	    sigmoid(NewValue,NewProbability),

	    % Prevent "inf" by using values too close to 1.0
	    secure_probability(NewProbability,NewProbabilityS),
	    set_fact_probability(FactID,NewProbabilityS),

	    fail; % go to next tunable fact

	    true
	),
	retractall(values_correct).

simulate :-
	L = [0.6,1.0,2.0,3.0,10,50,100,200,300],

	findall((X,Y),(member(X,L),line_search_evaluate_point(X,Y)),List),
	write(List),nl.

gradient_descent :-

	save_old_probabilities,
	update_values,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start set gradient to zero
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(   % go over all tunable facts

	    tunable_fact(FactID,_),
	    atomic_concat(['grad_',FactID],Key),
	    bb_put(Key,0.0),
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
        alpha(Alpha),
	example_count(ExampleCount),
	(   % go over all training examples
	    current_predicate(user:example/3),
	    user:example(QueryID,_Query,QueryProb),
	    query_probability(QueryID,BDDProb),
	    (
		QueryProb=:=0.0
	    ->
	        Y2=Alpha;
		Y2=1.0
	    ),
	    Y is Y2*2/ExampleCount * (BDDProb-QueryProb),


	    (   % go over all tunable facts

		tunable_fact(FactID,_),
		atomic_concat(['grad_',FactID],Key),
		
		% if the following query fails,
		% it means, the fact is not used in the proof
		% of QueryID, and the gradient is 0.0 and will
		% not contribute to NewValue either way
		% DON'T FORGET THIS IF YOU CHANGE SOMETHING HERE!
		query_gradient(QueryID,FactID,GradValue),

		bb_get(Key,OldValue),
		NewValue is OldValue + Y*GradValue,
		bb_put(Key,NewValue),
		fail; % go to next fact

		true
	    ),
	    fail; % go to next training example

	    true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop calculate gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start statistics on gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	findall(V,(tunable_fact(FactID,_),atomic_concat(['grad_',FactID],Key),bb_get(Key,V)),GradientValues),

	sum_list(GradientValues,GradSum),
	max_list(GradientValues,GradMax),
	min_list(GradientValues,GradMin),
	length(GradientValues,GradLength),
	GradMean is GradSum/GradLength,

	logger_set_variable(gradient_mean,GradMean),
	logger_set_variable(gradient_min,GradMin),
	logger_set_variable(gradient_max,GradMax),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop statistics on gradient
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
      
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start add gradient to current probabilities
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        (
	    line_search(false)
	->
  	    learning_rate(LearningRate);
	    lineSearch(LearningRate,_)
	),
	format('learning rate = ~12f~n',[LearningRate]),
	add_gradient(LearningRate),
	logger_set_variable(learning_rate,LearningRate),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop add gradient to current probabilities
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,
	forget_old_values.

%========================================================================
%=
%=
%========================================================================

line_search_evaluate_point(Learning_Rate,MSE) :-
	add_gradient(Learning_Rate),
	mse_trainingset_only_for_linesearch(MSE).


lineSearch(Final_X,Final_Value) :-

	% Get Parameters for line search
	line_search_tolerance(Tol),	
	line_search_tau(Tau),
	line_search_interval(A,B),

	format(' Running line search in interval (~5f,~5f)~n',[A,B]),
	
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
		
	    write(lineSearch(Iteration,Ak,Fl,Bk,Fr,Left,FLeft,Right,FRight)),nl,
	
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
	     
	     ActAcc is BkNew -AkNew,

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
	     ActAcc < Acc,  

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
	line_search_never_stop(false),
	!.
line_search_postcheck(_,_, LLH, FinalPosition) :-
	line_search_tolerance(Tolerance),
	line_search_interval(Left,Right),


	Offset is (Right - Left) * Tolerance,

	bb_put(line_search_offset,Offset),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(


	    repeat,
	    
	    bb_get(line_search_offset,OldOffset),
	    NewOffset is OldOffset * Tolerance,
	    bb_put(line_search_offset,NewOffset),

	    Position is Left + NewOffset,
	    set_linesearch_weights_calc_llh(Position,LLH),
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
%= initialize the logger module and set the flags for learning
%=
%========================================================================


global_initialize :-
	set_learning_flag(output_directory,'./output'),
	set_learning_flag(query_directory,'./queries'),
	set_learning_flag(log_frequency,5),
	set_learning_flag(rebuild_bdds,false),
	set_learning_flag(rebuild_bdds_it,1),
	set_learning_flag(reuse_initialized_bdds,false),
	set_learning_flag(learning_rate,examples),
	set_learning_flag(check_duplicate_bdds,true),
	set_learning_flag(probability_initializer,(FactID,P,random_probability(FactID,P))),
	set_learning_flag(alpha,1.0),
	set_learning_flag(sigmoid_slope,1.0), % 1.0 gives standard sigmoid
	set_learning_flag(init_method,(Query,Probability,BDDFile,ProbFile,
	problog_kbest_save(Query,100,Probability,_Status,BDDFile,ProbFile))),
	set_learning_flag(line_search,false),
	set_learning_flag(line_search_never_stop,true),
	set_learning_flag(line_search_tau,0.618033988749895),
	set_learning_flag(line_search_tolerance,0.05),
	set_learning_flag(line_search_interval,(0,100)),

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
	logger_define_variable(learning_rate,float).

%========================================================================
%= 
%=
%========================================================================

:- initialization(global_initialize).
