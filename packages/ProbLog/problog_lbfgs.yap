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
		    store_bdd/4,
		    reset_learning/0,
                   op( 550, yfx, :: ),
                    op( 550, fx, ?:: ),
                    op(1149, yfx, <-- ),
                    op( 1150, fx, problog_table )
		    ]).

:- reexport(library(matrix)).
:- reexport(library(terms)).
:- reexport(library(nb)).
:- reexport(problog).
:- reexport(problog/flags).
:- reexport(problog/logger).
:- reexport(problog/math).

% load our own modules

% switch on all the checks to reduce bug searching time

:- style_check(all).
%:- yap_flag(unknown,error).

% load modules from the YAP library
:- use_module(library(lists), [member/2,max_list/2, min_list/2, sum_list/2, reverse/2,sumlist/2]).
:- use_module(library(system), [file_exists/1, shell/2]).
:- use_module(library(rbtrees)).
:- use_module(library(lbfgs)).
:- use_module(problog/utils_learning).
:- use_module(problog/print_learning).
:- use_module(problog/utils).
:- use_module(problog/lbdd).


% used to indicate the state of the system
:- dynamic(values_correct/0).
:- dynamic(learning_initialized/0).
:- dynamic(current_iteration/1).
:- dynamic(current_epoch/2).
:- dynamic(example_count/1).
%:- dynamic(query_gradient_intern/4).
:- dynamic(last_mse/1).
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/2).

% used to identify queries which have identical proofs
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/3).

:- table user:example/4.
%:- multifile(user:example/4).
:- multifile(user:problog_discard_example/1).
user:example(A,B,Nr,=) :-
	current_predicate(user:example/3),
	user:example(A,B,Pr),
	smoothen(Pr,Nr),
	\+  user:problog_discard_example(B).



:- multifile(user:test_example/4).
user:test_example(A,B,Pr,=) :-
	current_predicate(user:test_example/3),
	user:test_example(A,B,Pr).

%========================================================================
%= store the facts with the learned probabilities to a file
%========================================================================

save_model:-
     current_iteration(Id),
     create_factprobs_file_name(Id,Filename),
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
	  format(user_error,'The trianing example ~q does not have a valid probability value (~q).~n',[ID,P]),
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
%	retractall(query_gradient_intern(_,_,_,_)),
	retractall(last_mse(_)),
	retractall(query_is_similar(_,_)),
	retractall(query_md5(_,_,_)),
	set_problog_flag(alpha,auto),
	set_problog_flag(learning_rate,examples),
	logger_reset_all_variables,
	logger_set_variable(lbfgs_training,+inf).




%========================================================================
%= initialize everything and perform Iterations times gradient descent
%= can be called several times
%= if it is called with an epsilon parameter, it stops when the change
%= in the MSE is smaller than epsilon
%========================================================================

do_learning(Iterations) :-
	do_learning(Iterations,0.000001).

do_learning(Iterations,Epsilon) :-
	current_predicate(user:example/4),
	!,
	integer(Iterations),
	number(Epsilon),
	Iterations>0,
	init_learning(X),
	do_learning_intern(Iterations,Epsilon,+inf,X).
do_learning(_,_) :-
	format(user_error,'~n~Error: No training examples specified.~n~n',[]).


do_learning_intern(EpochsMax,_,_,_) :-
    current_epoch(EpochsMax),
    !,
    logger_stop_timer(duration).
do_learning_intern(EpochsMax,Epsilon,Lik0,X) :-
%    db_usage,
%        db_static(128*1024),
    %	db_dynamic(128*1024),
	retract(current_epoch(Epochs)),
	NextEpochs is Epochs+1,
	format_learning(1,'~nstarted epoch ~w~n',[NextEpochs]),
	assert(current_epoch(NextEpochs)),
    logger_write_data,
         gradient_descent(Lik,X),
	writeln(1),
	mse_trainingset(X),
%	logger_get_variable(mse_trainingset,Last_MSE),
	logger_start_timer(duration),
	mse_testset(X),
	ground_truth_difference(X),
%leash(0),trace,
        logger_stop_timer(duration),
	init_queries,
	(
	    Lik >= Lik0-Epsilon;true
	->
    	do_learning_intern(EpochsMax, Epsilon, Lik,X)
	;
	true
	).
    	

%========================================================================
%= find proofs and build bdds for all training and test examples
%=
%=
%========================================================================
init_learning(__) :-
	learning_initialized,
	!.
init_learning(X) :-
\+ learning_initialized,
    check_examples,
	retractall(current_epoch(_)),

	assert(current_epoch(0)),
	retractall(current_iteration(_)),

	assert(current_iteration(0)),
	empty_output_directory,
	logger_write_header,
	format_learning(1,'Initializing everything~n',[]),

	succeeds_n_times(user:test_example(_,_,_,_),TestExampleCount),
	format_learning(3,'~q test examples~n',[TestExampleCount]),

	findall(Ex,user:example(Ex,_,_,_),Exs),
	max_list(Exs,TrainingExampleCount),
	assertz(example_count(TrainingExampleCount)),
	format_learning(3,'~q training examples~n',[TrainingExampleCount]),
	%current_probs <== array[TrainingExampleCount ] of floats,
	%current_lls <== array[TrainingExampleCount ] of floats,
	forall(tunable_fact(FactID,_),
	       set_fact_probability(FactID,0.5)
	      ),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% build BDD script for every example
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	init_queries,
	nl,
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% allocate LBFGS space
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	succeeds_n_times(tunable_fact(_FactID,_),N),
	lbfgs_allocate(N, X),
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% done
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%u%%%%%%%%%
	assertz(learning_initialized),
	format_learning(1,'~n',[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check, if continuous facts are used.
% if yes, switch to problog_exact
% continuous facts are not supported yet.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_default_gradient_method :-
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
	 ).



%========================================================================
%= This predicate goes over all training and test examples,
%= calls the inference method of ProbLog and stores the resulting
%= BDDs
%========================================================================


init_queries :-
    format_learning(2,'Build BDDs for examples~n',[]),
    forall(user:test_example(ID,Query,_Prob,_),init_one_query(ID,Query,test)),
	forall(user:example(ID,Query,_Prob,_),init_one_query(ID,Query,training)),
	fail.
init_queries.


init_one_query(QueryID,Query,_Type) :-
    %	format_learning(~q example ~q: ~q~n',[Type,QueryID,Query]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     b_setval(problog_required_keep_ground_ids,false),
    problog_flag(init_method,Call),
    %	  trace,
    (
	call(Call, Query,bdd(Dir,Tree0,MapList) )
    ->
    reverse(Tree0,Tree),
    store_bdd(QueryID, Dir, Tree, MapList)
;
true).

add_bdd(QueryID,Query, Bdd) :-

	Bdd = bdd(Dir, Tree0,MapList),
	user:graph2bdd(Query,1,Bdd),
	Tree0 \= [],
	!,
	 reverse(Tree0,Tree),
 	  %rb_new(H0),
	  %maplist_to_hash(MapList, H0, Hash),
	  %tree_to_grad(Tree, Hash, [], Grad),fev
	  % ;
	  % Bdd = bdd(-1,[],[]),
	  % Grad=[]
	 store_bdd(QueryID, Dir, Tree, MapList).
add_bdd(_QueryID,_Query, bdd(1,[],[])).

store_bdd(QueryID, _Dir, _Tree, _MapList) :-
    QueryID mod 100 =:= 0,
    format('~n~d: ',[QueryID]),
    fail.
store_bdd(QueryID, Dir, Tree, MapList) :-
    recordzifnot(QueryID,bdd(Dir, Tree, MapList),R),
    !,
    new_bdd(QueryID, bdd(Dir, Tree, MapList),R),
    put_char('.').
store_bdd(_QueryID, _Dir, _Tree, _MapList) :-
    !,
    put_char('#').

new_bdd(QueryID,_,R) :-
    recorded(QueryID,bdd(_,_,M),Ref),
    R\=Ref,
    erase(Ref),
    fail.
new_bdd(QueryID,bdd(_Dir, _Tree, MapList),_) :-
    user:example(QueryID,_,_),
    fail.
new_bdd(_QueryID,_,_).

%========================================================================
%=
%=
%=
%========================================================================
:- dynamic query_probability/2.

%========================================================================
%=
%=
%=
%========================================================================



% FIXME
ground_truth_difference(X) :-
    problog_flag(sigmoid_slope,Slope),
	findall(Diff,(tunable_fact(FactID,GroundTruth),
		      \+continuous_fact(FactID),
		      \+ var(GroundTruth),
		      %% get_fact_probability(FactID,Prob),
		      Value <== X[FactID],
		      sig2pr(Prob, Slope, Value),
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

update_query_cleanup(QueryID) :-
	(
	 (query_is_similar(QueryID,_) ; query_is_similar(_,QueryID))
	->
	% either this query is similar to another or vice versa,
	    % therefore we don't delete anything
	 true;
	 retractall(query_gradient_intern(QueryID,_,_,_))
	).


partial_m2(Iteration,Handle,LogCurrentProb,SquaredError,Slope,X,train) :-
    user:example(QueryID,Query,TrueQueryProb,_),
    query_probability(QueryID,Slope,X,CurrentProb),
    format(Handle,'ex(~q,training,~q,~q,~10f,~10f).~n',[Iteration,QueryID,Query,TrueQueryProb,CurrentProb]),
    once(update_query_cleanup(QueryID)),
     SquaredError is (CurrentProb-TrueQueryProb)**2,
   LogCurrentProb is log(max(0.0001,CurrentProb)).

partial_m2(Iteration,Handle,LogCurrentProb,SquaredError,Slope,X,test) :-
    user:test_example(QueryID,Query,TrueQueryProb,_),
    query_probability(QueryID,Slope,X,CurrentProb),
    format(Handle,'ex(~q,test,~q,~q,~10f,~10f).~n',[Iteration,QueryID,Query,TrueQueryProb,CurrentProb]),
    once(update_query_cleanup(QueryID)),
    SquaredError is (CurrentProb-TrueQueryProb)**2,
    LogCurrentProb is log(max(0.0001,CurrentProb)).



mse_trainingset(X) :-
    current_epoch(Epoch),
    current_iteration(Iteration),
    problog_flag(sigmoid_slope,Slope),
    atomic_concat([Epoch,'_',Iteration],Id),
        create_training_predictions_file_name(Id,File_Name),
        open(File_Name, write,Handle),
        format_learning(2,'MSE_Training ',[]),
        findall(t(LogCurrentProb,SquaredError),
                partial_m2(Iteration,Handle,LogCurrentProb,SquaredError,Slope,X,train),
                All),
        maplist(tuple, All, AllLogs, AllSquaredErrors),
        sum_list( AllLogs, LLH_Training_Queries),
        close(Handle),
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
        logger_set_variable_again(mse_trainingset,MSE),
        logger_set_variable_again(mse_min_trainingset,MinError),
        logger_set_variable_again(mse_max_trainingset,MaxError),
        logger_set_variable_again(llh_training_queries,LLH_Training_Queries),
%%%%%   format(' (~8f)~n',[MSE]).
        format_learning(2,' (~8f)~n',[MSE]).

tuple(t(X,SqE),X,SqE ).

mse_testset(X) :-
    problog_flag(sigmoid_slope,Slope),
    current_iteration(Iteration),
	create_test_predictions_file_name(Iteration,File_Name),
  	open(File_Name, write,Handle),
	format_learning(2,'MSE_Test ',[]),
	findall(t(LogCurrentProb,SquaredError),
		partial_m2(Iteration,Handle,LogCurrentProb,SquaredError,Slope,X,test),
		All),
	maplist(tuple, All, AllLogs, AllSquaredErrors),
	sum_list( AllLogs, LLH_Test_Queries),
        close(Handle),


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
	logger_set_variable_again(mse_testset,MSE),
	logger_set_variable_again(mse_min_testset,MinError),
	logger_set_variable_again(mse_max_testset,MaxError),
	logger_set_variable_again(llh_test_queries,LLH_Test_Queries),
	!,
	format_learning(2,' (~8f)~n',[MSE]).

%========================================================================
%= Perform one iteration of gradient descent
%=
%= assumes that everything is initialized, if the current values
%= of query_probability/2 and query_gradient/4 are not up to date
%= they will be recalculated
%= finally, the values_correct/0 is retracted to signal that the
%= probabilities of the examples have to be recalculated
%========================================================================



% vsc: avoid silly search
gradient_descent(BestF,X) :-
    ( current_predicate(user:iteration_prologue/0)
    -> ignore(user:iteration_prologue)
    ;
    true
    ),
    findall(FactID,tunable_fact(FactID,_GroundTruth),L),
    length(L,N),
    lbfgs_run(N,X,BestF),
    writeln(BestF),
    !.


set_fact(FactID, Slope, X ) :-
    P <== X[FactID],
    sig2pr(P, Slope, Pr),
    set_fact_probability(FactID, Pr).



%========================================================================
%= Updates all values of query_probability/2 and query_gradient/4
%= should be called always before these predicates are accessed
%= if the old values are still valid, nothing happens
%========================================================================

update_values(_,_) :-
	values_correct,
	!.
update_values(X,Slope) :-
    \+ values_correct,
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% delete old values
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	retractall(problog:query_probability_intern(_,_)),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	bdd_input_file(Probabilities_File),
	delete_file_silently(Probabilities_File),

	open(Probabilities_File,'write',Handle),

	problog_flag(sigmoid_slope,Slope),
	forall(get_fact_probability(ID,Prob),
	   (
		   (problog:dynamic_probability_fact(ID) ->
			get_fact(ID, Term),
			forall(grounding_is_known(Term, GID), (
				   V <== X[ID],
				   sig2pr(V, Slope, Value),
				   format(Handle, '@x~q_~q~n~10f~n', [ID,GID, Value])))
		   ; non_ground_fact(ID) ->
		     inv_sigmoid(Prob,Value),
		     format(Handle,'@x~q_*~n~10f~n',[ID,Value])
		   ;
				   V <== X[ID],
				   sig2pr(V, Slope, Value),
		   format(Handle,'@x~q~n~10f~n',[ID,Value])
		   )
	       )),

	forall(get_continuous_fact_parameters(ID,gaussian(Mu,Sigma)),
	       format(Handle,'@x~q_*~n0~n0~n~10f;~10f~n',[ID,Mu,Sigma])),

	close(Handle),
	!,
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	assertz(values_correct).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  calculate gradient
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:evaluate(LF, X,Grad,N,_Step,_) :-
    Grad <== 0.0,
    example_count(Exs),
    go( X,N,Exs,Grad, LLs),
    LF[0] <== LLs/Exs,
    %L <== sum(LLs),
    %SLL <== LLs[0],
    %Grad <== Grad,
    %
    %writeln(lls=LLs),
    %
    %show(Grad),
           !.


show(X) :-
    N <== length(X),
    show(X,N).

show(X,N) :-    
    N1 is N-1,
    between(0, N1, I),
    V <== X[I],
    format('~3f ', [V]),
    fail.
.show(_,_) :-
    nl.

go(X,N,Exs,Grad,LogLik) :-
    problog_flag(sigmoid_slope,Slope),
    findall(P-Error,current_error(Exs,X,Slope,P,Error), PErrors),
    maplist(spl, PErrors, Ps, SqErrors, Errors),
    LLL <== array[Exs] of SqErrors,
    PV <== array[Exs] of Ps,
    EV <== array[Exs] of Errors,
    LogLik <== sum(LLL),
    gradients(X,N,Slope,PV,EV,Grad).

spl(P-0, P, 0, 0) :- !.
spl(P-Error, P, SqE, Error) :-
    SqE is (Error*Error).

current_error(Exs,X,Slope,BDDProb,Error) :-
    between(1,Exs,QueryID),
    add_error(QueryID,X,Slope,BDDProb,Error).

add_error(QueryID,X,Slope,BDDProb,Error) :-
    BDD = bdd(_,_,MapList),
    recorded(QueryID,BDD,_),
    user:example(QueryID,_,Pr),
    MapList = [_|_],
    query_probability( BDD, X, Slope, BDDProb),
    !,
    Error is BDDProb-Pr.
%add_error(_QueryID,_X,_Slope,0.5,0.25).

query_probability(DBDD, X, Slope, Prob) :-
    DBDD = bdd(Dir, Tree,  MapList),
    maplist(bindpx(X,Slope), MapList),
     evalps(Tree, Prob0),
   % nonvar(Prob0),
    (Dir == 1 -> Prob0 = Prob ;  Prob is 1.0-Prob0).

%% this versions does not know what is in X.
%%
query_probability(QueryID, Prob) :-
    DBDD = bdd(Dir, Tree,  MapList),
    recorded(QueryID,DBDD,_),
    maplist(get_fact_probability_1arg, MapList),
    evalps(Tree, Prob0),
    % nonvar(Prob0),
    (Dir == 1 -> Prob0 = Prob ;  Prob is 1.0-Prob0).

get_fact_probability_1arg( I - Pr) :-
    get_fact_probability( I, Pr).

bindpx(X, Slope,I-Pr) :-
    I1 is I,
    SigPr <== X[I1],
    sig2pr(SigPr, Slope, Pr).

evalps( Tree, Prob0) :-
    foldl(evalp, Tree, _, Prob0).

gradients(X,N,Slope,PV,EV,Grads) :-
    BDD = bdd(_,_,M),
    user:example(QueryID,_,_),
    recorded(QueryID,BDD,_),
    Q1 is QueryID-1,
    Prob <== PV[Q1],
    Error <== EV[Q1],
    member(I-_,M),
    query_gradient(BDD,X,I,Slope, GradValue),
    Grad is GradValue*Prob*(1.0-Prob)*2*Error,
    G <== Grads[I],
Grads[I] <== G+Grad,
    fail.
    %format('~d: ~g ',[QueryID,Grad]).
gradients( _X, _,  _Slope,_PV,_EV, _).

query_gradient(bdd(Dir, Tree, MapList),X,I,Slope,Grad) :-
    maplist(bindpxx(X,Slope), MapList),
    % run_grad(Tree, I, Slope, 0.0, Grad0),
    evalgs(I, Tree, Grad0),
    ( Dir == 1 -> Grad = Grad0 ; Grad is -Grad0).

evalgs(I,Tree,Grad0) :-
    foldl( evalg(I), Tree, _, Grad0).

bindpxx(X,Slope,I-(I-Pr)) :-
    I1 is I-1,
    SigPr <== X[I1],
    sig2pr(SigPr, Slope, Pr).

sig2pr(SigPr,Slope, NPr) :-
    sigmoid(SigPr, Slope, Pr),
    NPr is min(0.99,max(0.01,Pr)).


evalp( pn(P, X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*(1.0-PR).
evalp( pp(P, X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*PR.
evalp( pn(P, _-X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*(1.0-PR).
evalp( pp(P, _-X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*PR.

evalg( I, pp(P-G, J-X, L, R), _, G ):-
    ( number(L) -> PL=L, GL = 0.0 ; L = PL-GL ),
    ( number(R) -> PR=R, GR = 0.0 ; R = PR-GR ),
    P is X*PL+ (1.0-X)*PR,
    (
	I == J
    ->
    G is X*GL+ (1.0-X)*GR+PL-PR
    ;
    G is X*GL+ (1.0-X)*GR
    ).
evalg( I, pn(P-G, J-X, L, R), _,G ):-
    ( number(L) -> PL=L, GL = 0.0 ; L = PL-GL ),
    ( number(R) -> PR=R, GR = 0.0 ; R = PR-GR ),
    P is X*PL+ (1.0-X)*(1.0-PR),
    (
	I == J
    ->
    G is X*GL-(1.0-X)*GR+PL-(1-PR)
    ;
    G is X*GL- (1.0-X)*GR
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% stop calculate gradient
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:progress(FX,_X,_G, _X_Norm,_G_Norm,_Step,_N,_CurrentIteration,_Ls) :-
    FX < 0, !,
    format('Bad FX=~4f~n',[FX]),
    lbfgs_progress_done(-1).
user:progress(FX,X,G,X_Norm,G_Norm,Step,_N, LBFGSIteration,Ls) :-
    problog_flag(sigmoid_slope,Slope),
     save_state(X, Slope, G),
    (retract(solver_iterations(SI,_)) -> true ; SI = 0),
    (retract(current_iteration(TI)) -> true ; TI = 0),
    TI1 is TI+1,
    assert(current_iteration(TI1)),
    save_model,
    X0 <== X[0], sig2pr(X0,Slope,P0),
    X1 <== X[1], sig2pr(X1,Slope,P1),
    format('~d ~d. Iteration : (x0,x1)=(~4f,~4f)  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f~n',[SI,LBFGSIteration,P0,P1,FX,X_Norm,G_Norm,Step,Ls]),
    lbfgs_progress_done(0).



save_state(X,Slope,_Grad) :-
    retractall(values_correct),
%    update_values(X, Slope),
    fail.
save_state(X,Slope,_Grad) :-
    tunable_fact(FactID,_GroundTruth),
    Log <== X[FactID],
    	sig2pr(Log, Slope, Pr),
    	set_fact_probability(FactID,Pr),
    	fail.
save_state(_X, _, _).

%========================================================================
%= initialize the logger module and set the flags for learning
%= don't change anything here! use set_problog_flag/2 instead
%========================================================================

init_flags :-
	prolog_file_name(queries,Queries_Folder), % get absolute file name for' ./queries'
	prolog_file_name(output,Output_Folder), % get absolute file name for './output'
	problog_define_flag(bdd_directory, problog_flag_validate_directory, 'directory for BDD scripts', Queries_Folder,learning_general),
	problog_define_flag(output_directory, problog_flag_validate_directory, 'directory for logfiles etc', Output_Folder,learning_general,flags:learning_output_dir_handler),
	problog_define_flag(log_frequency, problog_flag_validate_posint, 'log results every nth iteration', 1, learning_general),
	problog_define_flag(rebuild_bdds, problog_flag_validate_nonegint, 'rebuild BDDs every nth iteration', 0, learning_general),
	problog_define_flag(reuse_initialized_bdds,problog_flag_validate_boolean, 'Reuse BDDs from previous runs',false, learning_general),
	problog_define_flag(check_duplicate_bdds,problog_flag_validate_boolean,'Store intermediate results in hash table',true,learning_general),
	problog_define_flag(init_method,problog_flag_validate_dummy,'ProbLog predicate to search proofs',problog:problog_lbdd_exact_tree,learning_general,flags:learning_libdd_init_handler),
	problog_define_flag(alpha,problog_flag_validate_number,'weight of negative examples (auto=n_p/n_n)',auto,learning_general,flags:auto_handler),
	problog_define_flag(sigmoid_slope,problog_flag_validate_posnumber,'slope of sigmoid function',1.0,learning_general),
	problog_define_flag(continuous_facts,problog_flag_validate_boolean,'support parameter learning of continuous distributions',false,learning_general),
	problog_define_flag(m, problog_flag_validate_dummy,'The number of corrections to approximate the inverse hessian matrix.',(0,100),lbfgs,lbfgs:lbfgs_set_parameter(m)),
	problog_define_flag(epsilon,   problog_flag_validate_float, 'Epsilon for convergence test.',       0.0000100,lbfgs,lbfgs:lbfgs_set_parameter(epsilon)),
	problog_define_flag(past   ,   problog_flag_validate_float, 'Distance for delta-based convergence test.',    0   ,lbfgs,lbfgs:lbfgs_set_parameter(past)),
	problog_define_flag(delta   ,   problog_flag_validate_float, 'Delta for convergence test.',    0.001   ,lbfgs,lbfgs:lbfgs_set_parameter(delta)),
	problog_define_flag( lbfgs_max_iterations   ,   problog_flag_validate_posint, 'The maximum number of iterations',   0    ,lbfgs,lbfgs:lbfgs_set_parameter(max_iterations )),
	problog_define_flag( linesearch  ,   problog_flag_validate_posint, 'The line search algorithm.',    40   ,lbfgs,lbfgs:lbfgs_set_parameter(linesearch)),
	problog_define_flag(min_step   ,   problog_flag_validate_float, 'The minimum step of the line search routine.', 0      ,lbfgs,lbfgs:lbfgs_set_parameter(min_step)),
	problog_define_flag(  max_step  ,   problog_flag_validate_float, 'The maximum step of the line search.',   100000000000000000000    ,lbfgs,lbfgs:lbfgs_set_parameter( max_step)),
	problog_define_flag(ftol   ,   problog_flag_validate_float, 'A parameter to control the accuracy of the line search routine.', 0.0001      ,lbfgs,lbfgs:lbfgs_set_parameter(ftol)),
	problog_define_flag(gtol   ,   problog_flag_validate_float, 'A parameter to control the accuracy of the line search routine.', 0.9        ,lbfgs,lbfgs:lbfgs_set_parameter(gtol)),
    problog_define_flag(xtol   ,   problog_flag_validate_float, 'The machine precision for floating-point values.',     0.0000000000000001      ,lbfgs,lbfgs:lbfgs_set_parameter(xtol)),
    problog_define_flag(orthantwise_c   ,   problog_flag_validate_float, 'Coefficient for the L1 norm of variables.', 0      ,lbfgs,lbfgs:lbfgs_set_parameter(orthantwise_c)),
    problog_define_flag(orthantwise_start   ,   problog_flag_validate_posint, 'Start index for computing the L1 norm of the variables.',    0   ,lbfgs,lbfgs:lbfgs_set_parameter(orthantwise_c)),

    problog_define_flag(orthantwise_end   ,   problog_flag_validate_int, 'End index for computing the L1 norm of the variables.',   -1    ,lbfgs, lbfgs:lbfgs_set_parameter(orthantwise_c)).


init_logger :-
    logger_define_variable(iteration, int),
	logger_define_variable(duration,time),
	logger_define_variable(lbfgs_trainingset,float),
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
	logger_define_variable(m2_training_queries,float),
	logger_define_variable(llh_test_queries,float).

:- initialization(init_flags).

:- initialization(init_logger).
