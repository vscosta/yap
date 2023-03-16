
%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-12-05 14:07:19 +0100 (Mon, 05 Dec 2011) $
%  $Revision: 6766 $
%
%  This file is part of ProbLog
%  http://dtai.cs.kuleuven.be/problog
%
%  ProbLog was developed at Katholieke Universiteit Leuven
%
%  Copyright 2009
%  Angelika Kimmig, Vitor Santos Costa, Bernd Gutmann
%
%  Main author of this file:
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

:-source.
:- module(problog_lfi,[do_learning/1,
		       do_learning/2,
		       create_ground_tunable_fact/2,
		       reset_learning/0
		       ]).



% switch on all the checks to reduce bug searching time
:- style_check(all).
:- yap_flag(unknown,error).

% load modules from the YAP library
:- use_module(library(lists),[member/2,nth1/3,sum_list/2,min_list/2,max_list/2]).
:- use_module(library(system),[file_exists/1,exec/3,wait/2]).

% load our own modules
:- use_module('problog').
:- use_module('problog/logger').
:- use_module('problog/flags').
:- use_module('problog/os').
:- use_module('problog/completion').
:- use_module('problog/print_learning').
:- use_module('problog/utils_learning').
:- use_module('problog/utils').
:- use_module('problog/ad_converter').


% used to indicate the state of the system
:- dynamic(learning_initialized/0).
:- dynamic(current_iteration/1).
:- dynamic(query_all_scripts/2).
:- dynamic(last_llh/1).

:- discontiguous(user:myclause/1).
:- discontiguous(user:myclause/2).
:- discontiguous(user:known/3).
:- discontiguous(user:example/1).
:- discontiguous(user:test_example/1).

:- multifile(completion:bdd_cluster/2).
%:- multifile(completion:known_count/4).

user:term_expansion(myclause((Head<--Body)), C) :-
	prolog_load_context(module,Module),
	term_expansion_intern_ad((Head<--Body), Module,lfi_learning, C).

%========================================================================
%= Hack for Ingo, to allow tunable facts with body
%=
%= e.g. :- create_ground_tunable_fact( t(_) :: f(X), member(X,[a,b,c])).
%=  will create
%=   t(_) :: f(a).
%=   t(_) :: f(b).
%=   t(_) :: f(c).
%========================================================================

create_ground_tunable_fact(F,B) :-
	B,
	once(problog_assert(F)),
	fail.
create_ground_tunable_fact(_,_).


%========================================================================
%= store the facts with the learned probabilities to a file
%= if F is a variable, a filename based on the current iteration is used
%=
%========================================================================

save_model:-
	current_iteration(Iteration),
	create_factprobs_file_name(Iteration,Filename),
	open(Filename,'write',Handle),
	forall((current_predicate(user:ad_intern/3),user:ad_intern(Original,ID,Facts)),
	       print_ad_intern(Handle,Original,ID,Facts)
	       ),
	forall(probabilistic_fact(_,Goal,ID),
	       (
		array_element(factprob,ID,P),
		(
		is_mvs_aux_fact(Goal)
	       ->
		format(Handle,'%  ~10f :: ~q.   %ID=~q~n',[P,Goal,ID]);
		format(Handle   ,'~10f :: ~q.   %ID=~q~n',[P,Goal,ID])
	       )
	       )
	      ),
	close(Handle).

is_mvs_aux_fact(A) :-
	functor(A,B,_),
	atomic_concat(mvs_fact_,_,B).

print_ad_intern(Handle,(Head<--Body),_ID,Facts) :-
	format(Handle,'myclause( (',[]),
	print_ad_intern(Head,Facts,0.0,Handle),
	format(Handle,' <-- ~q) ).~n',[Body]).
print_ad_intern((A1;B1),[A2|B2],Mass,Handle) :-
	once(print_ad_intern_one(A1,A2,Mass,NewMass,Handle)),
	format(Handle,'; ',[]),
	print_ad_intern(B1,B2,NewMass,Handle).
print_ad_intern(_::Fact,[],Mass,Handle) :-
	P2 is 1.0 - Mass,
	format(Handle,'~f :: ~q',[P2,Fact]).
print_ad_intern_one(_::Fact,_::AuxFact,Mass,NewMass,Handle) :-
	% ask problog to get the fact_id
	once(probabilistic_fact(_,AuxFact,FactID)),
	% look in our table for the probability
	array_element(factprob,FactID,P),
	P2 is P * (1-Mass),
	NewMass is Mass+P2,
	format(Handle,'~f :: ~q',[P2,Fact]).
%========================================================================
%= initialize everything and perform Iterations times EM
%= can be called several times
%========================================================================

do_learning(Iterations) :-
	do_learning(Iterations,-1).

do_learning(Iterations,Epsilon) :-
	integer(Iterations),
	number(Epsilon),
	Iterations>0,
	init_learning,
	!,
	do_learning_intern(Iterations,Epsilon),
	!,
	copy_back_fact_probabilities.

do_learning_intern(0,_) :-
	!.
do_learning_intern(Iterations,Epsilon) :-
	Iterations>0,
	logger_start_timer(duration),

	current_iteration(CurrentIteration),
	!,
	retractall(current_iteration(_)),
	!,
	NextIteration is CurrentIteration+1,
	assertz(current_iteration(NextIteration)),
	EndIteration is CurrentIteration+Iterations-1,

	format_learning(1,'~nIteration ~d of ~d~n',[CurrentIteration,EndIteration]),
	logger_set_variable(iteration,CurrentIteration),
	write_probabilities_file,

	once(llh_testset),

	once(ground_truth_difference),
	once(em_one_iteration),

	problog_flag(log_frequency,Log_Frequency),
	(
	 ( Log_Frequency>0, 0 =:= CurrentIteration mod Log_Frequency)
	->
	 once(save_model);
	 true
	),
	!,

	(
	 last_llh(Last_LLH)
	->
	 (
	  retractall(last_llh(_)),
	  logger_get_variable(llh_training_set,Current_LLH),
	  assertz(last_llh(Current_LLH)),
	  !,
	  LLH_Diff is abs(Last_LLH-Current_LLH)
	 );  (
	      logger_get_variable(llh_training_set,Current_LLH),
	      assertz(last_llh(Current_LLH)),
	      LLH_Diff is Epsilon+1
	     )
	),

	logger_stop_timer(duration),
	logger_write_data,
	RemainingIterations is Iterations-1,
	!,
	garbage_collect,
	!,

	(
	 LLH_Diff>Epsilon
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
	convert_filename_to_problog_path('simplecudd_lfi', Path),
	(
	 file_exists(Path)
	->
	 true;
	 (
	  problog_path(PD),
	  format(user_error, 'WARNING: Can not find file: simplecudd_lfi. Please place file in problog path: ~q~n',[PD]),
	  fail
	 )
	),

	check_theory,


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Delete the stuff from the previous run
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	problog_flag(reuse_initialized_bdds,Re_Use_Flag),

	(
	 Re_Use_Flag==false
	->
	 empty_bdd_directory;
	 true
	),
	empty_output_directory,


	logger_write_header,

	format_learning(1,'Initializing everything~n',[]),

	(
	 current_predicate(user:test_example/1)
	->
	 (
	  succeeds_n_times(user:test_example(_),TestExampleCount),
	  format_learning(3,'~q test example(s)~n',[TestExampleCount])
	 );
	 true
	),

	succeeds_n_times(user:example(_),TrainingExampleCount),
	format_learning(3,'~q training example(s)~n',[TrainingExampleCount]),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Create arrays for probabilities and counting tables
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	once(initialize_fact_probabilities),
	problog:probclause_id(N),
	static_array(factprob_temp,N,float),
	static_array(factusage,N,int),
	static_array(known_count_true_training,N,int),
	static_array(known_count_false_training,N,int),
	static_array(known_count_true_test,N,int),
	static_array(known_count_false_test,N,int),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% build BDD script for every example
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	once(init_queries),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% done
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	assertz(current_iteration(0)),
	assertz(learning_initialized),
        once(save_model),

	format_learning(1,'~n',[]),

	garbage_collect,
	garbage_collect_atoms.




%========================================================================
%= This predicate checks some aspects of the data given by the user.
%= You know folks: Garbage in, garbage out.
%=
%========================================================================
check_theory :-
	 (
	  (user:myclause(Head,Body),P :: Head)
	 ->
	  (
	   format(user_error,'===============================================================~n',[]),
	   format(user_error,' The theory contains an atom that appears both as probabilistic~n',[]),
	   format(user_error,' fact and as head of an rule. This is not allowed.~2n',[]),
	   format(user_error,'    ~q~n',[P :: Head]),
	   format(user_error,'    ~q~2n',[myclause(Head,Body)]),
   	   format(user_error,'===============================================================~2n',[]),

	   throw(bad_theory(Head))
	  );
	  true
	 ),

	 (
	  (current_predicate(user:example/1),user:example(_))
	 ->
	  true;
	  (
	   format(user_error,'===============================================================~n',[]),
	   format(user_error,' No training examples specified.~n',[]),
   	   format(user_error,'===============================================================~2n',[]),
	   throw(bad_theory(no_training_examples))
	  )
	 ),

	 (
	  ( current_predicate(user:test_example/1),user:example(ID), user:test_example(ID) )
	 ->
	  (
	   format(user_error,'===============================================================~n',[]),
	   format(user_error,' The example ~q appears both as test and as training example.~n',[ID]),
	   format(user_error,' Example IDs from test and training examples must be disjoint.~2n',[]),
	   format(user_error,' Do NOT bypass this test, since the implementation yields wrong resuls~n',[]),
	   format(user_error,' when an example ID appears both as test and training example.',[]),
   	   format(user_error,'===============================================================~2n',[]),

	   throw(bad_theory(double_id(ID)))
	  );
	  true
	 ),

	 (
	  (current_predicate(user:known/3),user:example(ID2),user:known(ID2,_,_))
	 ->
	  true;
	  (
	   format(user_error,'===============================================================~n',[]),
	   format(user_error,' No evidence specified.~n',[]),
   	   format(user_error,'===============================================================~2n',[]),
	   throw(bad_theory(no_evidence))
	  )
	 ),


	 (
	  (user:known(ID,Foo,Evidence), (Evidence\=true,Evidence\=false))
	 ->
	  (
	   format(user_error,'===============================================================~n',[]),
	   format(user_error,' Bad evidence for training example ~q: ~q.~n',[ID,known(ID,Foo,Evidence)]),
   	   format(user_error,'===============================================================~2n',[]),
	   throw(bad_theory(bad_evidence(ID)))
	  );
	  true
	 ),

	 (
	  (user:known(ID,Foo,true), user:known(ID,Foo,false))
	 ->
	  (
	   format(user_error,'===============================================================~n',[]),
	   format(user_error,' Bad evidence for training example ~q: ~q and ~q~n',[ID,known(ID,Foo,true),known(ID,Foo,false)]),
   	   format(user_error,'===============================================================~2n',[]),
	   throw(bad_theory(bad_evidence(ID)))
	  );
	  true
	 ).



%========================================================================
%= copy fact probabilities to array for speeding up the update
%=
%=
%========================================================================

initialize_fact_probabilities :-
	problog:probclause_id(N),
	static_array(factprob,N,float),

	forall(get_fact_probability(FactID,P),
	       update_array(factprob,FactID,P)).

copy_back_fact_probabilities :-
	forall(tunable_fact(FactID,_),
	       (
		array_element(factprob,FactID,P),
		set_fact_probability(FactID,P)
	       )
	      ).



%========================================================================
%= This predicate goes over all training and test examples,
%= calls the inference method of ProbLog and stores the resulting
%= BDDs
%========================================================================


init_queries :-
	problog_flag(cluster_bdds,Cluster_BDDs),
	format_learning(2,'Build BDDs for examples~n',[]),
	forall(user:example(Training_ID),
	       (
		   format_learning(3,'training example ~q: ',[Training_ID]),
		init_one_query(Training_ID,training)
	       )
	      ),

	forall(
	       (
		current_predicate(user:test_example/1),
		user:test_example(Test_ID)
	       ),
	       (
		format_learning(3,'test example ~q: ',[Test_ID]),
		init_one_query(Test_ID,test)
	       )
	      ),

	(
	 Cluster_BDDs==true
	->
	 (
	  format_learning(2,'Calculate MD5s for training example BDD scripts~n',[]),
	  create_training_query_cluster_list(Training_Set_Cluster_List),
	  format_learning(2,'Calculate MD5s for test example BDD scripts~n',[]),
	  create_test_query_cluster_list(Test_Set_Cluster_List)
	 );
	 (
	  findall( a(QueryID,ClusterID,1), (
					   current_predicate(user:test_example/1),
					   user:test_example(QueryID),
					   bdd_cluster(QueryID,ClusterIDs),
					   member(ClusterID,ClusterIDs)
					  ), Test_Set_Cluster_List),

	  findall( a(QueryID,ClusterID,1), (
					   user:example(QueryID),
					   bdd_cluster(QueryID,ClusterIDs),
					   member(ClusterID,ClusterIDs)
					  ), Training_Set_Cluster_List)
	 )
	),

	assertz(training_set_cluster_list(Training_Set_Cluster_List)),
	assertz(test_set_cluster_list(Test_Set_Cluster_List)).

%========================================================================
%=
%========================================================================

init_one_query(QueryID,_Query_Type) :-
	create_known_values_file_name(QueryID,File_Name),
	file_exists(File_Name),
	!,
	format_learning(3,'Will reuse existing BDD script ~q for example ~q.~n',[File_Name,QueryID]),
	consult(File_Name).

	%FIXME

	% check whether we can read the BDD script for each cluster

init_one_query(QueryID,Query_Type) :-
	once(propagate_evidence(QueryID,Query_Type)),
	format_learning(3,'~n',[]),
	garbage_collect_atoms,
	garbage_collect.


create_test_query_cluster_list(L2) :-
	findall( a(QueryID,ClusterID), (
					 current_predicate(user:test_example/1),
					 user:test_example(QueryID),
					 bdd_cluster(QueryID,ClusterIDs),
					 member(ClusterID,ClusterIDs)
				      ), AllCluster),
	calc_all_md5(AllCluster,AllCluster2),
	findall(a(QueryID1,ClusterID1,Len),(bagof(a(QueryID,ClusterID),member(a(QueryID,ClusterID,_MD5),AllCluster2),L),nth1(1,L,a(QueryID1,ClusterID1)),length(L,Len)),L2),
	!,
	length(AllCluster,Len1),
	length(L2,Len2),
	(
	 Len1>0
	->
	 (
	  Reduction is Len2/Len1,
	  format_learning(3,' ~d cluster after splitting, ~d unique cluster ==> reduction factor of ~4f~n',[Len1,Len2,Reduction])
	 );
	 true
	).

calc_all_md5([],[]).
calc_all_md5([a(QueryID,ClusterID)|T],[a(QueryID,ClusterID,MD5)|T2]) :-
	create_bdd_file_name(QueryID,ClusterID,File_Name),
	calc_md5(File_Name,MD5),
	calc_all_md5(T,T2).

create_training_query_cluster_list(L2) :-
	findall( a(QueryID,ClusterID), (
					 user:example(QueryID),
					 bdd_cluster(QueryID,ClusterIDs),
					 member(ClusterID,ClusterIDs)
				      ), AllCluster),

	calc_all_md5(AllCluster,AllCluster2),
	findall(a(QueryID1,ClusterID1,Len),
		(
		 bagof(a(QueryID,ClusterID),member(a(QueryID,ClusterID,_MD5),AllCluster2),L),
		 nth1(1,L,a(QueryID1,ClusterID1)),
		 length(L,Len)
		),L2),
	length(AllCluster,Len1),
	length(L2,Len2),

	Reduction is Len2/Len1,

	format_learning(3,' ~d cluster after splitting, ~d unique cluster ==> reduction factor of ~4f~n',[Len1,Len2,Reduction]).


%========================================================================
%=
%========================================================================

reset_learning :-
	(
	 learning_initialized
	->
	 (
	  retractall(current_iteration(_)),
	  retractall(learning_initialized),

	  retractall(training_set_cluster_list(_)),
	  retractall(test_set_cluster_list(_)),
	  close_static_array(factprob),
	  close_static_array(factprob_temp),
	  close_static_array(factusage),

	  close_static_array(known_count_true_training),
	  close_static_array(known_count_false_training),
	  close_static_array(known_count_true_test),
	  close_static_array(known_count_false_test),

	  reset_completion,
	  empty_bdd_directory,
	  empty_output_directory,

	  logger_reset_all_variables
	 );
	 true
	).

%========================================================================
%= calculate the LLH on the test set and set the variable
%= in the logger module
%========================================================================

llh_testset :-
	current_predicate(user:test_example/1),
	!,
	current_iteration(Iteration),
	create_test_predictions_file_name(Iteration,F),

	open(F,'write',Handle),

	catch(
	sum_forall(LProb,
		   (
		    probabilistic_fact(_,_,FactID),
		    array_element(factprob,FactID,PFact),
		    array_element(known_count_true_test,FactID,KK_True),
		    array_element(known_count_false_test,FactID,KK_False),

		    (
			KK_True>0
		    ->
		        Part1 is KK_True*log(PFact);
			Part1 is 0.0
		    ),
		    (
			KK_False>0
		    ->
		        LProb is Part1+KK_False*log(1-PFact);
			LProb is Part1
		    )
		   ),
		   PropagatedLLH
		  ),_,PropagatedLLH is 0.0/0.0),
	format(Handle,'prob_known_atoms(~15e).~n',[PropagatedLLH]),

	test_set_cluster_list(AllCluster),
	% deal with test examples where BDD needs to be evaluated
	problog_flag(parallel_processes,Parallel_Processes),
	once(evaluate_bdds(AllCluster,Handle,Parallel_Processes,'d',':',PropagatedLLH,LLH)),
	logger_set_variable(llh_test_set,LLH),
	close(Handle).
llh_testset :-
	true.





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
		      array_element(factprob,FactID,Prob),
		      Diff is abs(GroundTruth-Prob)),AllDiffs),
	(
	 AllDiffs==[]
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
%=
%=
%========================================================================

write_probabilities_file :-
	current_iteration(Iteration),
	create_bdd_input_file_name(Iteration,Probabilities_File),
	open(Probabilities_File,'write',Handle),
	forall(get_fact_probability(ID,_),
	       (
		array_element(factprob,ID,Prob),

		(
		 non_ground_fact(ID)
		->
		 format(Handle,'@x~q_*~n~15e~n1~nx~q~N',[ID,Prob,ID]);
		 format(Handle,'@x~q~n~15e~n1~nx~q~N',[ID,Prob,ID])
		)
	       )
	      ),
	close(Handle).




%========================================================================
%=
%=
%=
%========================================================================

update_query(QueryID,ClusterID ,Method,Command,PID,Output_File_Name) :-
	current_iteration(Iteration),

	create_bdd_input_file_name(Iteration,Input_File_Name),
	create_bdd_output_file_name(QueryID,ClusterID,Iteration,Output_File_Name),
	create_bdd_file_name(QueryID,ClusterID,BDD_File_Name),

	convert_filename_to_problog_path('simplecudd_lfi',Absolute_Name),

	atomic_concat([Absolute_Name,
		       ' -i "', Input_File_Name, '"',
		       ' -l "', BDD_File_Name, '"',
		       ' -m ', Method,
		       ' -id ', QueryID],Command),
	open( Output_File_Name, write, Stream ),
	exec(Command,[std, Stream ,std],PID),
	close( Stream ).

update_query_wait(QueryID,_ClusterID,Count,Symbol,Command,PID,OutputFilename,BDD_Probability) :-
	wait(PID,Error),
	format_learning(4,'~w',[Symbol]),
	(
	 Error \= 0
	->
	   (
	    format(user_error,'SimpleCUDD stopped with error code ~q.~n', [Error]),
	    format(user_error,'The command was~n  ~q~n',[Command]),
	    throw(bdd_error(QueryID,Error))
	   );
	 true
	),

	once(my_load_allinone(OutputFilename,QueryID,Count,BDD_Probability)),

	problog_flag(retain_bdd_output,Retain_BDD_Output),

	(
	 Retain_BDD_Output==true
	->
	 true;
	 delete_file_silently(OutputFilename)
	).


%========================================================================
%=
%=
%=
%========================================================================


my_load_allinone(File,QueryID,Count,BDD_Probability) :-
	open(File,'read',Handle),
	read(Handle,Atom),
	once(my_load_intern_allinone(Atom,Handle,QueryID,Count,error,BDD_Probability)),
	!,
	close(Handle).

my_load_allinone(File,QueryID,_,_,_,_) :-
	format(user_error,'Error at ~q.~2n',[my_load(File,QueryID)]),
	throw(error(my_load(File,QueryID))).

my_load_intern_allinone(end_of_file,_,_,_,BDD_Probability,BDD_Probability) :-
	!.
my_load_intern_allinone(query_probability(QueryID,Prob),Handle,QueryID,Count,Old_BDD_Probability,BDD_Probability) :-
	!,
	(
	 Old_BDD_Probability==error
	->
	 true;
	 throw(error(bdd_output_contains_prob_twice(query_probability(QueryID,Prob))))
	),
	Prob2 is Prob*Count,   % this is will throw an exception if simplecudd delivers non-number garbage
	read(Handle,X),
	my_load_intern_allinone(X,Handle,QueryID,Count,Prob2,BDD_Probability).
my_load_intern_allinone(ec(QueryID,VarName,Value),Handle,QueryID,Count,Old_BDD_Probability,BDD_Probability) :-
	!,
	split_atom_name(VarName,FactID,_GroundID),
	MultValue is Value*Count,
	add_to_array_element(factprob_temp,FactID,MultValue,_NewEC),
	add_to_array_element(factusage,FactID,Count,_NewDiv),
	read(Handle,X),
	my_load_intern_allinone(X,Handle,QueryID,Count,Old_BDD_Probability,BDD_Probability).
my_load_intern_allinone(X,Handle,QueryID,Count,Old_BDD_Probability,BDD_Probability) :-
	format(user_error,'Unknown atom ~q in results file.~n',[X]),
	read(Handle,X2),
	my_load_intern_allinone(X2,Handle,QueryID,Count,Old_BDD_Probability,BDD_Probability).

%========================================================================
%= Perform one iteration of EM
%========================================================================

my_reset_static_array(Name) :-
  %%% DELETE ME AFTER VITOR FIXED HIS BUG
        static_array_properties(Name,Size,Type),
	LastPos is Size-1,
	(
	    Type==int
	->
	 forall(between(0,LastPos,Pos), update_array(Name,Pos,0))
	;
	    Type==float
	->
  	    forall(between(0,LastPos,Pos), update_array(Name,Pos,0.0))
	;
	    fail
	).

em_one_iteration :-
	write_probabilities_file,
	my_reset_static_array(factprob_temp),
	my_reset_static_array(factusage),

	current_iteration(Iteration),
	create_training_predictions_file_name(Iteration,Name),

	open(Name,'write',Handle),


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start calculate new values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% process known_count information
	bb_put(dummy,0.0),
	(
			% go over all tunable facts and get their current probability
		    tunable_fact(FactID,_),
		    array_element(factprob,FactID,P),
		    % get known counts

		    array_element(known_count_true_training,FactID,KK_True),
		    array_element(known_count_false_training,FactID,KK_False),
		    KK_Sum is KK_True+KK_False,

		    KK_Sum>0,

		    % add counts
		    add_to_array_element(factprob_temp,FactID,KK_True,_NewValue),
		    add_to_array_element(factusage,FactID,KK_Sum,_NewCount),

		    % for LLH training set

		    (
			KK_True>0
		    ->
		        Part1 is KK_True*log(P);
			Part1 is 0.0
		    ),
		    (
			KK_False>0
		    ->
		        LProb is Part1 + KK_False*log(1-P);
			LProb is Part1
		    ),

		    bb_get(dummy,Old),
	            New is Old+LProb,
		    bb_put(dummy,New),

	            fail;
	            true
	),
	bb_delete(dummy,LLH_From_True_BDDs),

	format(Handle,'propagatedprob(~15e).~n',[LLH_From_True_BDDs]),

	training_set_cluster_list(AllCluster),

	problog_flag(parallel_processes,Parallel_Processes),
	evaluate_bdds(AllCluster,Handle,Parallel_Processes,'e','.',LLH_From_True_BDDs,LLH),

	logger_set_variable(llh_training_set,LLH),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop calculate new values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	format_learning(2,'~n',[]),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start copy new values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	problog_flag(pc_numerator,Pseudo_Counts_Numerator),
	problog_flag(pc_denominator,Pseudo_Counts_Denominator),

	forall(
	       (
		tunable_fact(FactID,_),
		array_element(factusage,FactID,Used),
		Used>0		% only update relevant facts
	       ),
	       (
		array_element(factprob_temp,FactID,NewValue),
		NewP is (NewValue+ Pseudo_Counts_Numerator) / (Used+Pseudo_Counts_Denominator),
		update_array(factprob,FactID,NewP)
	       )
	      ),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop copy new values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	close(Handle).


%========================================================================
%= Call SimpleCUDD for each BDD Cluster script
%=   L      : a list containing 3-tuples a(QueryID,ClusterID,Count)
%=   H      : file handle for the log file
%=   P      : number of parallel SimpleCUDD processes
%=   T      : type of evaluation, either 'd' or 'e'
%=   S      : symbol to print after a process finished
%=   OldLLH : accumulator for LLH
%=   LLH    : resulting LLH
%=
%=  evaluate_bdds(+L,+H,+P,+T,+S,+OldLLH,-LLH)
%========================================================================

evaluate_bdds([],_,_,_,_,LLH,LLH).
evaluate_bdds([H|T],Handle,Parallel_Processes,Type,Symbol,OldLLH,LLH) :-
	once(slice_n([H|T],Parallel_Processes,ForNow,Later)),
	logger_start_timer(bdd_evaluation),
	once(evaluate_bdds_start(ForNow,Type,ForNow_Jobs)),
	once(evaluate_bdds_stop(ForNow_Jobs,Handle,Symbol,OldLLH,NewLLH)),
	logger_stop_timer(bdd_evaluation),
	evaluate_bdds(Later,Handle,Parallel_Processes,Type,Symbol,NewLLH,LLH).

evaluate_bdds_start([],_,[]).
evaluate_bdds_start([a(QueryID,ClusterID,Count)|T],Type,[job(QueryID,ClusterID,Count,Command,PID,OutputFilename)|T2]) :-
	once(update_query(QueryID,ClusterID,Type,Command,PID,OutputFilename)),
	evaluate_bdds_start(T,Type,T2).
evaluate_bdds_stop([],_,_,LLH,LLH).
evaluate_bdds_stop([job(ID,ClusterID,Count,Command,PID,OutputFilename)|T],Handle,Symbol,OldLLH,LLH) :-
	once(update_query_wait(ID,ClusterID,Count,Symbol,Command,PID,OutputFilename,BDD_Prob)),
	format(Handle,'bdd_prob(~w,~w,~15e). % Count=~w~n',[ID,ClusterID,BDD_Prob,Count]),
	catch(NewLLH is OldLLH + Count*log(BDD_Prob),_Exception,NewLLH is 0.0/0.0),
	evaluate_bdds_stop(T,Handle,Symbol,NewLLH,LLH).


%========================================================================
%=
%=
%========================================================================



%========================================================================
%= initialize the logger module and set the flags for learning
%= don't change anything here! use set_learning_flag/2 instead
%========================================================================

init_flags :-
	prolog_file_name('queries',Queries_Folder), % get absolute file name for './queries'
	prolog_file_name('output',Output_Folder), % get absolute file name for './output'
	problog_define_flag(bdd_directory, problog_flag_validate_directory, 'directory for BDD scripts', Queries_Folder,learning_general),
	problog_define_flag(output_directory, problog_flag_validate_directory, 'directory for logfiles etc', Output_Folder,learning_general,flags:learning_output_dir_handler),
	problog_define_flag(retain_bdd_output,problog_flag_validate_boolean,'Keep output files from BDD tool',false,learning_general),
	problog_define_flag(log_frequency, problog_flag_validate_posint, 'log results every nth iteration', 1, learning_general),
	problog_define_flag(reuse_initialized_bdds,problog_flag_validate_boolean, 'Reuse BDDs from previous runs',false, learning_general),
	problog_define_flag(pc_numerator,problog_flag_validate_in_interval_right_open([0.0,+inf]),'Add X to numerator (Pseudocounts)',0.0,learning_general),
	problog_define_flag(pc_denominator,problog_flag_validate_in_interval_right_open([0.0,+inf]),'Add X to denominator (Pseudocounts)',0.0,learning_general),
	problog_define_flag(parallel_processes,problog_flag_validate_posint,'Number of parallel BDD processes',8,learning_general),

	problog_define_flag(cluster_bdds,problog_flag_validate_boolean,'Cluster similar BDDs',true,learning_general).


init_logger :-
	logger_define_variable(iteration, int),
	logger_define_variable(duration,time),

	logger_define_variable(llh_training_set,float),
	logger_define_variable(llh_test_set,float),

	logger_define_variable(bdd_evaluation,time),

	logger_define_variable(ground_truth_diff,float),
	logger_define_variable(ground_truth_mindiff,float),
	logger_define_variable(ground_truth_maxdiff,float),

	logger_define_variable(train_bdd_script_generation,time),
	logger_define_variable(train_bdd_script_generation_grounding,time),
	logger_define_variable(train_bdd_script_generation_completion,time),
	logger_define_variable(train_bdd_script_generation_propagation,time),
	logger_define_variable(train_bdd_script_generation_splitting,time),
	logger_define_variable(train_bdd_script_generation_active_ground_atoms,int),
	logger_define_variable(train_bdd_script_generation_propagated_ground_atoms,int),

	logger_define_variable(test_bdd_script_generation,time),
	logger_define_variable(test_bdd_script_generation_grounding,time),
	logger_define_variable(test_bdd_script_generation_completion,time),
	logger_define_variable(test_bdd_script_generation_propagation,time),
	logger_define_variable(test_bdd_script_generation_splitting,time),
	logger_define_variable(test_bdd_script_generation_active_ground_atoms,int),
	logger_define_variable(test_bdd_script_generation_propagated_ground_atoms,int).

:- initialization(init_flags).
:- initialization(init_logger).

%:- spy em_one_iteration.


%:- initialization(do_learning(100) ).
