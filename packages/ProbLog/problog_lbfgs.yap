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
		    test_vs/2,
		    tp/1,
		    fp/1,
		    tn/1,
		    fn/1,
                    op( 550, yfx, :: ),
                    op( 550, fx, ?:: ),
                    op(1149, yfx, <-- ),
                    op( 1150, fx, problog_table )
		   ]).

:- reexport(library(matrix)).
:- reexport(library(python)).
:- reexport(problog).
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
:- use_module(problog/os).
:- use_module(problog/lbdd).


% used to indicate the state of the system
:- dynamic(values_correct/0).
:- dynamic(learning_initialized/0).
:- dynamic(current_iteration/1).
:- dynamic(current_epoch/1).
:- dynamic(example_count/1).
:- dynamic(test_example_count/1).
%:- dynamic(query_gradient_intern/4).
:- dynamic(last_mse/1).
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/2).

% used to identify queries which have identical proofs
:- dynamic(query_is_similar/2).
:- dynamic(query_md5/3).

%:- table user:example/4.

:- dynamic user:example_/3.
:- multifile(user:problog_discard_example/1).
user:example(NA,B,Pr,=) :-
    user:example_(NA,B,Pr).

:- dynamic i/1.
i(0).

init_inc :-
    retractall(i(_)),
    assert(i(0)).

inc(I) :-
    retract(i(I)),
    I1  is I+1,
    assert(i(I1)).


:- multifile(user:test_example/3).
user:test_example(A,B,Pr,=) :-
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
    retractall(test_example_count(_)),
    %	retractall(query_gradient_intern(_,_,_,_)),
    retractall(last_mse(_)),
    retractall(query_is_similar(_,_)),
    retractall(query_md5(_,_,_)),
    set_problog_flag(alpha,auto),
    logger_reset_all_variables.
%    logger_set_variable(lbfgs_training,+inf).




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
    init_learning,
    do_learning_intern(Iterations,Epsilon,+inf).
do_learning(_,_) :-
    format(user_error,'~n~Error: No training examples specified.~n~n',[]).


do_learning_intern(EpochsMax,_,_) :-
    current_epoch(EpochsMax),
!.  
    %lhogger_stop_timer(duration).
do_learning_intern(_EpochsMax,_Epsilon,_Lik0) :-
    %    db_usage,
    %        db_static(128*1024),
    %	db_dynamic(128*1024),
    retract(current_epoch(Epochs)),
    NextEpochs is Epochs+1,
    format_learning(1,'~nstarted epoch ~w~n',[NextEpochs]),
    assert(current_epoch(NextEpochs)),
    %        logger_start_timer(duration),
    gradient_descent(_X,_Lik).

 %%%   ground_truth_difference(X,Slope),
    %leash(0),trace,
    %        logger_stop_timer(duration),
%    lbfgs_free(X),
/*init_queries,
    (
	Lik >= Lik0-Epsilon;true
			    ->
    				do_learning_intern(EpochsMax, Epsilon, Lik)
	;
	true
    ).
*/

%========================================================================
%= find proofs and build bdds for all training and test examples
%=0
%=
%========================================================================
init_learning :-
    learning_initialized,
    !.
init_learning :-
    \+ learning_initialized,
    problog_flag(output_directory, Dir),
    concat_path_with_filename(Dir,'run.csv',FileName),
    open(FileName,write,_O,[alias(run)]),
    format(run,'~8s|',['Iteration']),
    format(run,'~5s|',['Epoch']),
    format(run,'~5s|',['Evals']),
    format(run,'~4s|',['More']),
    format(run,'~10s|',['FX']),
    format(run,'~10s|',['_X_Norm']),
    format(run,'~10s|',['_G_Norm']),
    format(run,'~10s|',['_Step']),
    format(run,'~10s|',['LLH_Test']),
    format(run,'~10s|',['MinError']),
    format(run,'~10s|',['MaxError']),
    nl(run),
    check_examples,
    retractall(current_epoch(_)),
    assert(current_epoch(0)),
    retractall(current_iteration(_)),
    assert(current_iteration(0)),
    empty_output_directory,
    logger_write_header,
    format_learning(1,'Initializing everything~n',[]),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % build BDD script for every example
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    init_queries,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % allocate LBFGS space
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
     
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


init_bdds :-
    format_learning(2,'Build BDDs for examples~n',[]),
    forall(user:example(ID,Query,_Prob,_),init_one_query(ID,Query,training)),
    forall(user:test_example(ID,Query,_Prob,_),init_one_query(ID,Query,test)).

init_queries :-
    init_bdds,
    fail.
init_queries :-
    findall(Ex,user:test_example(Ex,_,_),TestExs),
    findall(Ex,user:test_example(_,_,Ex),TestPExs),
    (
	TestExs == []
    ->
    assertz(test_example_count(0)),
    format_learning(3,'NO test examples~n',[]),
    TestExampleCount = 16
    ;
    max_list(TestExs,TestExampleCount0),
    TestExampleCount is TestExampleCount0+1,
    assertz(test_example_count(	 TestExampleCount)),
    format_learning(3,'~q test examples~n',[TestExampleCount])
    ),
    lbfgs_allocate(TestExampleCount, Test_p0 ),
    lbfgs_allocate(TestExampleCount, Test_p ),
    lbfgs_allocate(TestExampleCount, Test_em),
    lbfgs_allocate(TestExampleCount, Test_ll),
    tcount <== matrix [2] of ints,
    maplist(set_p0(Test_p0),TestExs,TestPExs),
    nb_setval(test_data,t(Test_p0,Test_p, Test_em, Test_ll, tcount)),

    findall(Ex,user:example(Ex,_,_,_),Exs),

    max_list(Exs,TrainingExampleCount0),
    TrainingExampleCount is    TrainingExampleCount0+1,
    assertz(example_count(TrainingExampleCount)),
    lbfgs_allocate(TrainingExampleCount,Training_p0 ), 
    lbfgs_allocate(TrainingExampleCount,Training_p ),  
    lbfgs_allocate(TrainingExampleCount, Training_em ),
    lbfgs_allocate(TrainingExampleCount,Training_ll ),
    format_learning(3,'~d training examples~n',[TrainingExampleCount]),
    nb_setval(training_data,t(Training_p0,Training_p,  Training_em, Training_ll, TrainingExampleCount)),
    forall(tunable_fact(FactID,_),
	   set_fact_probability(FactID,0.5)
	  ).

init_one_query(QueryID,Query,_Type) :-
    %	format_learning(~q example ~q: ~q~n',[Type,QueryID,Query]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%xf
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

set_p0(X,I,P) :- X[I] <==P.

add_bdd(QueryID,Query, Bdd) :-
    Bdd = bdd(Dir, Tree0,MapList),
    user:graph2bdd(Query,1,Bdd),
    Tree \= [],
    !,
    reverse(Tree0,Tree),
    store_bdd(QueryID, Dir, Tree, MapList).
add_bdd(_QueryID,_Query, bdd(1,[],[])).

store_bdd(QueryID, _Dir, _Tree, _MapList) :-
    QueryID mod 100 =:= 0,
    format('~n~d: ',[QueryID]),
    fail.
store_bdd(QueryID, Dir, Tree, MapList) :-
    recordzifnot(QueryID,bdd(Dir, Tree, MapList),R),
    !,
    ignore((recorded(QueryID,_,Ref),
	    R\=Ref,
	    erase(Ref))),
    put_char('.').
store_bdd(_QueryID, _Dir, _Tree, _MapList) :-
    put_char('#').

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


	assertz(values_correct).


%========================================================================
%=
%=
%=
%========================================================================
report(FX,X,Slope, X_Norm,G_Norm,Step,_N,Evaluations, Stop) :-
    current_iteration(Iteration),
    current_epoch(Epoch),
    format(run,'~d|',[Iteration]),
    format(run,'~d|',[Epoch]),
    format(run,'~d|',[Evaluations]),
    format(run,'~d|',[Stop]),
    format(run,'~10g|',[FX]),
    format(run,'~10g|',[X_Norm]),
    format(run,'~10g|',[G_Norm]),
    format(run,'~10g|',[Step]),
    nb_getval(test_data,t(_PP0, PV, EV, LLL, _Count)),
    LLL<== 0,
    PV <== 0,
    EV <== 0.0,
    %Count <== zeros(1),
	     findall(P0-PP,(
			 user:test_example(QueryID,_,P0),
			 query_ex(QueryID,P0,X,Slope,LLL,PV,EV,PP)
			 ), L),
    LLH_Test <== LLL.sum(),
    MinError <== EV.min(),
    MaxError <== EV.max(),
    test_vs(L, Evaluations),
%	user:p_message('Test set performance'),
%	user:write_cmatrix([TP,FP,FN,TN]),
    format(run,'~10g|',[LLH_Test]),
    format(run,'~10g|',[MinError]),
    format(run,'~10g|',[MaxError]),
    nl(run).



%========================================================================
								    %= Calculates the mse of training and test da
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
    SquaredError is (CurrentProb-TrueQueryProb)**2,
    LogCurrentProb is log(max(0.0001,CurrentProb)).

partial_m2(Iteration,Handle,LogCurrentProb,SquaredError,Slope,X,test) :-
    user:test_example(QueryID,Query,TrueQueryProb,_),
    query_probability(QueryID,Slope,X,CurrentProb),
    format(Handle,'ex(~q,test,~q,~q,~10f,~10f).~n',[Iteration,QueryID,Query,TrueQueryProb,CurrentProb]),
    SquaredError is (CurrentProb-TrueQueryProb)**2,
    LogCurrentProb is log(max(0.0001,CurrentProb)).

test_vs(L,Evaluations) :-
%    writeln(user_error,T),
    maplist(zip,L,PP0L,PVL),
    current_predicate(user:induce/0),
    selectlist(tp,L,Tps), length(Tps,TP),
    selectlist(tn,L,Tns), length(Tns,TN),
    selectlist(fn,L,Fns), length(Fns,FN),
    selectlist(fp,L,Fps), length(Fps,FP),
    O is (TP+TN)/(TP+TN+FP+FN),
    format('[ LBFGS iter accuracy=~g with [TP,FP,FN,TN] = ~w ]~n',[O,[TP,FP,FN,TN]] ),
    aleph_utils:xsetting(alg,Alg),
    aleph_utils:xsetting(fold,Fold),
    aleph_utils:xsetting(induce,Duce),
    problog_flag(sigmoid_slope,Slope),
    x(X),
    LFacts <== X.list(),
    maplist(s2pr(Slope),LFacts,Facts),
    AUC := skm.roc_auc_score(PP0L,PVL),
    format(results,'lbfgs(~d,~a,~w,~d,auc=~g, acc=~g, [TP,FP,FN,TN] = ~w, parameters=~w, scores=~w).~n',[Evaluations,Alg,Duce,Fold,AUC,O,[TP,FP,FN,TN],Facts,L]).


s2pr(Slope,L,X) :-
    sig2pr(L, Slope,X).
		     

zip(B-C,B,C).

tp(A-B) :- A>0.5,B>0.5.
tn(A-B) :- A=<0.5,B=<0.5.
fn(A-B) :- A>0.5,B=<0.5.
fp(A-B) :- A=<0.5,B>0.5.



% vsc: avoid silly search
gradient_descent(X,BestF) :-
    ( current_predicate(user:iteration_prologue/0)
    -> ignore(user:iteration_prologue)
    ;
    true
    ),
    findall(FactID,tunable_fact(FactID,_GroundTruth),L),
    length(L,N),
    lbfgs_allocate(N,X),
    retractall(x(_)),
    assert(x(X)),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % stop add gradient to current probabilities
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    (lbfgs_run(N,X)),
    lbfgs_fx(BestF),
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
update_values(_X,_Slope) :-
    \+ values_correct,
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % delete old values
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    retractall(problog:query_probability_intern(_,_)),

    assertz(values_correct).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  calculate gradient
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:evaluate(LF, X,Grad,_N,_Step,_) :-
    problog_flag(sigmoid_slope,Slope),
    nb_getval(training_data,t(_,LLL,PV,EV,_TotalExCount)), 
    Grad <== 0.0,
    LLL  <== 0.0,
	       PV <== 0.0,
	     EV <== 0.0,
		      run_queries(X,Slope,LLL,PV,EV),
  LF <== LLL.sum(), 
    forall(user:example(QueryID,_,_P0,_),query_ex_gradient(QueryID,X,Slope,EV,Grad)).



run_queries(X,Slope,LLL,PV,EV)  :-
    forall(user:example(QueryID,_,P0,_),query_ex(QueryID,P0,X,Slope,LLL,PV,EV,_)).



query_probability(QueryID,Slope,X,CurrentProb) :-
    nb_getval(training_data,t(_,LLL,PV,EV,_TotalExCount)), 
    query_ex(QueryID,_TrueProb,X,Slope,LLL,PV,EV,CurrentProb).
    
query_ex(QueryID,TrueProb,X,Slope,LLL,PV,EV,Prob) :-
    recorded(QueryID,bdd(Dir,Tree,MapList),_),
    MapList \= [],
    !,
    maplist(bindpx(X,Slope), MapList),
    evalps(Tree, Prob0),
    % nonvar(Prob0),
    (Dir == 1 -> Prob0 = Prob ;  Prob is 1.0-Prob0),
    Q1 is QueryID,
    PV[Q1] <== Prob,
    Error is Prob-TrueProb,
    EV[Q1] <== (Error),
    LLL[Q1] <== Error*Error.
query_ex(_QueryID,_TrueProb,_X,_Slope,_LLL,_PV,_EV,0).

query_ex_gradient(QueryID,X,Slope,EV,Grads) :-
    recorded(QueryID,bdd(Dir,Tree,MapList),_),
     MapList \= [],
    !,
    Q1 is QueryID,
    Error <== EV[Q1],
     maplist(bindpxx(X,Slope), MapList),
     forall( member(I-(I-Prob), MapList),
            gradxy(I,bdd(Dir,Tree,MapList),Prob,Error,Grads) ).
query_ex_gradient(_QueryID,_,_,_,_Grads).

gradxy(I,bdd(Dir,Tree,_MapList),Prob,Error,Grads) :-
     evalgs(I, Tree, Grad0),
     !,
    ( Dir == 1 -> GradValue = Grad0 ; GradValue is -Grad0),
    Grad is (GradValue*Prob*(1.0-Prob)*2*Error),
    G <== Grads[I],
    Grads[I] <== G+Grad.

bindpx(X, Slope,I-Pr) :-
   SigPr <== X[I],
    sig2pr(SigPr, Slope, Pr).
 
bindpxx(X,Slope,I-(I-Pr)) :-
    SigPr <== X[I],
    sig2pr(SigPr, Slope, Pr).

sig2pr(SigPr,Slope, NPr) :-
    sigmoid(SigPr, Slope, Pr),
    NPr is min(0.99,max(0.01,Pr)).

evalps(Tree,P ) :-
    foldl( evalp, Tree, _,  P).

evalp( pn(P, X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*(1.0-PR).
evalp( pp(P, X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*PR.
evalp( pn(P, _-X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*(1.0-PR).
evalp( pp(P, _-X, PL, PR), _,P ):-
    P is X*PL+ (1.0-X)*PR.

evalgs(I,Tree,Grad0) :-
    foldl( evalg(I), Tree, _, Grad0).

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

mse_testset(X,Slope) :-
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
		     query_probability(QueryID,Slope,X,CurrentProb),
		 format(Handle,'ex(~q,test,~q,~q,~10f,~10f).~n',[Iteration,QueryID,Query,TrueQueryProb,CurrentProb]),
	%	 once(update_query_cleanup(QueryID)),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% stop calculate gradient
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user:progress(FX,_X,_G, _X_Norm,_G_Norm,_Step,_N,_Ev,_L) :-
    FX < 0, !,
    format('Bad FX=~4f~n',[FX]),
    lbfgs_progress_done(-1).
user:progress(FX,X,G,X_Norm,G_Norm,Step, N, Evals,Ls) :-
    problog_flag(sigmoid_slope,Slope),
    save_state(X, Slope, G),
    report(FX,X,Slope, X_Norm,G_Norm,Step,N,Evals,Ls),
    (retract(solver_iterations(SI,_)) -> true ; SI  = 0),
    (retract(current_iteration(TI)) -> true ; TI = 0),
    TI1 is TI+1,
    assert(current_iteration(TI1)),
    save_model,
    XLength <== X.length(),
    X0 <== X[0], sig2pr(X0,Slope,P0),
    (XLength == 1
    ->
	P1 is 0
    ;
    X1 <== X[1], sig2pr(X1,Slope,P1)
    ),
    format('~d ~d. Iteration : (x0,x1)=(~4f,~4f)  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~  Ls=~4f~n',[SI,TI,P0,P1,FX,X_Norm,G_Norm,Step,Ls]),
%    mse_testset(X,Slope),
    format_learning(2,'~n',[]),                                                lbfgs_progress_done(0).



save_state(X,Slope,_Grad) :-
    retractall(values_correct),
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
    ( aleph_utils:xsetting(fold,Fold) ->true ; Fold=''),
    prolog_file_name(queries,Queries_Folder), % get absolute file name for' ./queries'
    atomic_concat(output,Fold, Xoutput),
    prolog_file_name(Xoutput,Output_Folder), % get absolute file name for './output'
    problog_define_flag(bdd_directory, problog_flag_validate_directory, 'directory for BDD scripts', Queries_Folder,learning_general),
    problog_define_flag(output_directory, problog_flag_validate_directory, 'directory for logfiles etc', Output_Folder,learning_general,flags:learning_output_dir_handler),
    problog_define_flag(log_frequency, problog_flag_validate_posint, 'log results every nth iteration', 1, learning_general),
    problog_define_flag(rebuild_bdds, problog_flag_validate_nonegint, 'rebuild BDDs every nth iteration', 0, learning_general),
    problog_define_flag(reuse_initialized_bdds,problog_flag_validate_boolean, 'Reuse BDDs from previous runs',false, learning_general),
    problog_define_flag(check_duplicate_bdds,problog_flag_validate_boolean,'Store intermediate results in hash table',true,learning_general),
    problog_define_flag(init_method,problog_flag_validate_dummy,'ProbLog predicate to search proofs',problog:problog_lbdd_tree,learning_general,flags:learning_libdd_init_handler),
    problog_define_flag(alpha,problog_flag_validate_number,'weight of negative examples (auto=n_p/n_n)',auto,learning_general,flags:auto_handler),
    problog_define_flag(sigmoid_slope,problog_flag_validate_posnumber,'slope of sigmoid function',1.0,learning_general),
    problog_define_flag(continuous_facts,problog_flag_validate_boolean,'support parameter learning of continuous distributions',false,learning_general).
nooo :-
    problog_define_flag(m, problog_flag_validate_dummy,'The number of corrections to approximate the inverse hessian matrix.',(0,100),lbfgs,lbfgs:lbfgs_set_parameter(m)),
    problog_define_flag(epsilon,   problog_flag_validate_float, 'Epsilon for convergence test.',       0.0000100,lbfgs,lbfgs:lbfgs_set_parameter(epsilon)),
    problog_define_flag(past   ,   problog_flag_validate_float, 'Distance for delta-based convergence test.',    0   ,lbfgs,lbfgs:lbfgs_set_parameter(past)),
    problog_define_flag(delta   ,   problog_flag_validate_float, 'Delta for convergence test.',    0.001   ,lbfgs,lbfgs:bfgs_set_parameter(delta)),
    problog_define_flag( lbfgs_max_iterations   ,   problog_flag_validate_posint, 'The maximum number of iterations',   0    ,lbfgs,lbfgs:lbfgs_set_parameter(max_iterations )),
    problog_define_flag( linesearch  ,   problog_flag_validate_posint, 'The line search algorithm.',    40   ,lbfgs,lbfgs:lbfgs_set_parameter(linesearch)),
    problog_define_flag(min_step   ,   problog_flag_validate_float, 'The minimum step of the line search routine.', 0      ,lbfgs,lbfgs:lbfgs_set_parameter(min_step)),
    problog_define_flag(  max_step  ,   problog_flag_validate_float, 'The maximum step of the line search.',   100000000000000000000.0    ,lbfgs,lbfgs:lbfgs_set_parameter( max_step)),
    problog_define_flag(ftol   ,   problog_flag_validate_float, 'A parameter to control the accuracy of the line search routine.', 0.0001      ,lbfgs,lbfgs:lbfgs_set_parameter(ftol)),
    problog_define_flag(gtol   ,   problog_flag_validate_float, 'A parameter to control the accuracy of the line search routine.', 0.9        ,lbfgs,lbfgs:lbfgs_set_parameter(gtol)),
    problog_define_flag(xtol   ,   problog_flag_validate_float, 'The machine precision for floating-point values.',     0.0000000000000001      ,lbfgs,lbfgs:lbfgs_set_parameter(xtol)),
    problog_define_flag(orthantwise_c   ,   problog_flag_validate_float, 'Coefficient for the L1 norm of variables.', 0.0    ,lbfgs,lbfgs:lbfgs_set_parameter(orthantwise_c)),
    problog_define_flag(orthantwise_start   ,   problog_flag_validate_posint, 'Start index for computing the L1 norm of the variables.',    0   ,lbfgs,lbfgs:lbfgs_set_parameter(orthantwise_end)),

    problog_define_flag(orthantwise_end   ,   problog_flag_validate_int, 'End index for computing the L1 norm of the variables.',   -1    ,lbfgs, lbfgs:lbfgs_set_parameter(orthantwise_end)).


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
%    logger_define_variable(learning_rate,float),
    logger_define_variable(alpha,float),
    logger_define_variable(llh_trainingset,float),
    logger_define_variable(m2_trainingset,float),
    logger_define_variable(llh_test_queries,float).

:- initialization(init_flags).

:- initialization(init_logger).



