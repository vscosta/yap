%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2009-07-21 18:30:23 +0200 (Tue, 21 Jul 2009) $
%  $Revision: 1805 $
%
%  This file is part of ProbLog
%  http://dtai.cs.kuleuven.be/problog
%
%  ProbLog was developed at Katholieke Universiteit Leuven
%                                                            
%  Copyright 2009
%  Angelika Kimmig, Vitor Santos Costa, Bernd Gutmann
%                                                              
%  Main authors of this file:
%  Angelika Kimmig, Vitor Santos Costa,Bernd Gutmann
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
	problog_montecarlo/3,
	problog_answers/2,
	problog_table/1,	    
	get_fact_probability/2,
	set_fact_probability/2,
	get_fact/2,
	tunable_fact/2,
	non_ground_fact/1,
	export_facts/1,
	problog_help/0,
	show_inference/0,
	problog_dir/1,
	set_problog_flag/2,
	problog_flag/2,
	problog_flags/0]).

:- style_check(all).
:- yap_flag(unknown,error).

% problog related modules
:- use_module('problog/flags',[set_problog_flag/2,
	problog_flag/2,
	problog_flags/0]).

:- use_module('problog/print', [print_sep_line/0,
	print_inference/2]).

:- use_module('problog/tptree',[init_ptree/1,
	delete_ptree/1,
	insert_ptree/2,
	count_ptree/2,
	prune_check_ptree/2,
	merge_ptree/3,
	bdd_ptree_map/4,
	bdd_ptree/3]).

% general yap modules
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(random)).
:- ensure_loaded(library(system)).
:- ensure_loaded(library(rbtrees)).

% op attaching probabilities to facts
:- op( 550, yfx, :: ).
:- op( 1150, fx, problog_table ).

:- meta_predicate problog_table(:).

%%%%%%%%%%%%%%%%%%%%%%%%
% control predicates on various levels
%%%%%%%%%%%%%%%%%%%%%%%%

% global over all inference methods, internal use only
:- dynamic problog_predicate/2.
% global over all inference methods, exported
:- dynamic tunable_fact/2.
:- dynamic non_ground_fact/1.
:- dynamic problog_dir/1.
% global, manipulated via problog_control/2
:- dynamic up/0.
:- dynamic limit/0.
:- dynamic mc/0.
:- dynamic remember/0.
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

% for fact where the proabability is a variable
:- dynamic dynamic_probability_fact/1.
:- dynamic dynamic_probability_fact_extract/2.

% keep a tab on tabling
:- dynamic problog_tabled/1.

% directory where ProblogBDD executable is located
% automatically set during loading -- assumes it is in same place as this file (problog.yap)
%:- getcwd(PD),retractall(problog_dir(_)),assert(problog_dir(PD)).
:- yap_flag(shared_object_search_path,PD),
	retractall(problog_dir(_)),
	assert(problog_dir(PD)).


%%%%%%%%%%%%%%%%%%%%%%%%
% help
%%%%%%%%%%%%%%%%%%%%%%%%

problog_help :-
	format('~2nProbLog inference currently offers the following inference methods:~n',[]),
	show_inference,
	format('~2nThe following global parameters are available:~n',[]),
	problog_flags,
	print_sep_line,
	format('~n     use problog_help/0 to display this information~n',[]),
	format('~n     use problog_flags/0 to display current parameter values~2n',[]),
	print_sep_line,
	nl,
	flush_output.

show_inference :-
	format('~n',[]),
	print_sep_line,
	print_inference(call,description),
	print_sep_line,
	print_inference('problog_delta(+Query,+Delta,-Low,-High,-Status)','approximation with interval width Delta (IJCAI07)'), 
	print_inference('problog_threshold(+Query,+Threshold,-Low,-High,-Status)','bounds based on single probability threshold'), 
	print_inference('problog_low(+Query,+Threshold,-Low,-Status)','lower bound based on single probability threshold'), 
	print_inference('problog_kbest(+Query,+K,-Low,-Status)','lower bound based on K most likely proofs'), 
	print_inference('problog_max(+Query,-Prob,-FactsUsed)','explanation probability (ECML07)'),
	print_inference('problog_exact(+Query,-Prob,-Status)','exact probability'),
	print_inference('problog_montecarlo(+Query,+Delta,-Prob)','sampling with 95\%-confidence-interval-width Delta'),
	print_sep_line.
	
%%%%%%%%%%%%%%%%%%%%%%%%
% initialization of global parameters
%%%%%%%%%%%%%%%%%%%%%%%%

init_global_params :-
	set_problog_flag(bdd_time,60), 
	set_problog_flag(first_threshold,0.1), 
	L is 10**(-30), 
	set_problog_flag(last_threshold,L), 
	set_problog_flag(id_stepsize,0.5), 
	set_problog_flag(prunecheck,off), 
	set_problog_flag(maxsteps,1000),
	set_problog_flag(mc_batchsize,1000),
	set_problog_flag(mc_logfile,'log.txt'),
	set_problog_flag(bdd_file,example_bdd),
	set_problog_flag(dir,output),
	set_problog_flag(save_bdd,false),
	set_problog_flag(hacked_proofs,false),
	set_problog_flag(verbose,true).
%	problog_flags,
%	print_sep_line,
%	format('~n     use problog_help/0 for information~n',[]),
%	format('~n     use problog_flags/0 to display current parameter values~2n',[]),
%	print_sep_line,
%	nl,
%	flush_output.

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
	assert(X).
problog_control(off,X) :-
	retractall(X).
problog_control(check,X) :-
	call(X).

reset_control :-
	problog_control(off,up),
	problog_control(off,mc),
	problog_control(off,limit),
	problog_control(off,remember).
:- reset_control.

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

user:term_expansion(_P::( _Goal :- _Body ), _Error) :-
	throw(error('we do not support this (yet?)!')).

/* this can slow down prolog time by several orders if there's lots of them 
user:term_expansion(P::Goal,Goal) :-
	P \= t(_),
	P =:= 1,
	!.
*/
user:term_expansion(P::Goal, problog:ProbFact) :-
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
		LProb is log(0.5)
	    );
	    (
		ground(P)
	    ->
	        EvalP is P, % allows one to use ground arithmetic expressions as probabilities
	        assert_static(prob_for_id(ID,EvalP)), % Prob is fixed -- assert it for quick retrieval
	        LProb is log(P);
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
	problog_predicate(Name, Arity, ProblogName).
	    

% introduce wrapper clause if predicate seen first time
problog_predicate(Name, Arity, _) :-
	problog_predicate(Name, Arity), !.

problog_predicate(Name, Arity, ProblogName) :-
	functor(OriginalGoal, Name, Arity),
	OriginalGoal =.. [_|Args],
	append(Args,[Prob],L1),
	ProbFact =.. [ProblogName,ID|L1],
	prolog_load_context(module,Mod),
	make_add_to_proof(ID2,ProbEval,AddToProof),
	assert( (Mod:OriginalGoal :- ProbFact, 
	                             (
					 non_ground_fact(ID)
				     ->
				         (non_ground_fact_grounding_id(OriginalGoal,G_ID),
					   atomic_concat([ID,'_',G_ID],ID2));
					 ID2=ID
				     ),
				     % take the log of the probability (for non ground facts with variable as probability
				     ProbEval is Prob,
				     AddToProof
		 )),

	assert( (Mod:problog_not(OriginalGoal) :- ProbFact,
	                                          (
						      non_ground_fact(ID)
						  ->
						     ( non_ground_fact_grounding_id(OriginalGoal,G_ID),
						        atomic_concat([ID,'_',G_ID],ID2));
						      ID2=ID
						  ),
						% take the log of the probability (for non ground facts with variable as probability
						  ProbEval is Prob,
						  add_to_proof_negated(ID2,ProbEval)
		 )),
	    
	assert(problog_predicate(Name, Arity)),
	ArityPlus2 is Arity+2,
	dynamic(problog:ProblogName/ArityPlus2).	

make_add_to_proof(ID2,ProbEval,O) :-
	problog_flag(hacked_proofs,true), !,
	O = hacked_add_to_proof(ID2,ProbEval).
make_add_to_proof(ID2,ProbEval,add_to_proof(ID2,ProbEval)).




% generate next global identifier
probclause_id(ID) :-
	nb_getval(probclause_counter,ID), !,
	C1 is ID+1,
	nb_setval(probclause_counter,C1), !.
probclause_id(0) :-
	nb_setval(probclause_counter,1).

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
		once(assert(grounding_is_known(Goal,ID)))
	    )
	).

reset_non_ground_facts :-
	nb_setval(non_ground_fact_grounding_id_counter,0),
	retractall(grounding_is_known(_,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% access/update the probability of ID's fact
% hardware-access version: naively scan all problog-predicates (except if prob is recorded in static database),
% cut choice points if ID is ground (they'll all fail as ID is unique),
% but not if it isn't (used to iterate over all facts when writing out probabilities for learning)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using a dummy for the static prob database is more efficient than checking for current_predicate
prob_for_id(dummy,dummy).
get_fact_probability(ID,Prob) :-
	ground(ID),
	prob_for_id(ID,Prob),
	!.
get_fact_probability(ID,Prob) :-
	(
	ground(ID) -> 
		get_internal_fact(ID,ProblogTerm,_ProblogName,ProblogArity),!
	;
	get_internal_fact(ID,ProblogTerm,_ProblogName,ProblogArity)
	),
	arg(ProblogArity,ProblogTerm,Log),
	Prob is exp(Log).
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% writing those facts with learnable parameters to File
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export_facts(File) :-
	tell(File),
	export_facts,
	flush_output,
	told.
export_facts :-
	tunable_fact(ID,_),
	once(write_tunable_fact(ID)),
	fail.
export_facts.

write_tunable_fact(ID) :-
	get_internal_fact(ID,ProblogTerm,ProblogName,ProblogArity),
	ProblogTerm =.. [_Functor,ID|Args],
	atomic_concat('problog_',OutsideFunctor,ProblogName),
	Last is ProblogArity-1,
	nth(Last,Args,LogProb,OutsideArgs),
	OutsideTerm =.. [OutsideFunctor|OutsideArgs],
	Prob is exp(LogProb),
	format('~w :: ~q.~n',[Prob,OutsideTerm]).

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
add_to_proof(ID,Prob) :-
	(
	 problog_control(check,mc)
	->
	 montecarlo_check(ID)
	;
	 b_getval(problog_steps,MaxSteps),
	 b_getval(problog_probability, CurrentP),
	 nb_getval(problog_threshold, CurrentThreshold),
	 b_getval(problog_current_proof, IDs),

%%%% Bernd, changes for negated ground facts
	 \+ memberchk(not(ID),IDs),
%%%% Bernd, changes for negated ground facts
	 
	 (
	  MaxSteps =< 0
	 -> 
	  fail
	 ;
	  (
	   memberchk(ID, IDs)
	  ->
	   true
	  ;
	   \+ prune_check([ID|IDs],1),
	   multiply_probabilities(CurrentP, Prob, NProb),
	   (
	    NProb < CurrentThreshold
	   ->
	    upper_bound([ID|IDs]),
	    fail
	   ;
	    b_setval(problog_probability, NProb),
	    b_setval(problog_current_proof, [ID|IDs])
	   )
	  ),
	  Steps is MaxSteps-1,
	  b_setval(problog_steps,Steps)
	 )
	).

% simpliciation
hacked_add_to_proof(ID,Prob) :-
	b_getval(problog_probability, CurrentP),
	nb_getval(problog_threshold, CurrentThreshold),
	multiply_probabilities(CurrentP, Prob, NProb),
	b_getval(problog_current_proof, IDs),
	(
	    NProb < CurrentThreshold
	   ->
	    upper_bound([ID|IDs]),
	    fail
         ;
	 b_setval(problog_probability, NProb),
	 b_setval(problog_current_proof, [ID|IDs])
	).

%%%% Bernd, changes for negated ground facts
add_to_proof_negated(ID,Prob) :-
	(
	 problog_control(check,mc)
	->
	% the sample has to fail if the fact is negated
	 \+ montecarlo_check(ID)
	;
	 b_getval(problog_steps,MaxSteps),
	 b_getval(problog_probability, CurrentP),
	 nb_getval(problog_threshold, CurrentThreshold),
	 b_getval(problog_current_proof, IDs),
	 
	 \+ memberchk(ID,IDs),
	 (
	  MaxSteps =< 0
	 -> 
	  fail
	 ;
	  (
	   memberchk(not(ID), IDs)
	  ->
	   true
	  ;
%	    \+ prune_check([ID|IDs],1),
	   InverseProb is log(1 - exp(Prob)),
	   multiply_probabilities(CurrentP, InverseProb, NProb),
	   (
	    NProb < CurrentThreshold
	   ->
	    upper_bound([not(ID)|IDs]),	%% checkme
	    fail
	   ;
	    b_setval(problog_probability, NProb),
	    b_setval(problog_current_proof, [not(ID)|IDs])
	   )
	  ),
	  Steps is MaxSteps-1,
	  b_setval(problog_steps,Steps)
	 )
	).
%%%% Bernd, changes for negated ground facts


% if in monte carlo mode, check array to see if fact can be used
montecarlo_check(ID) :-
	array_element(mc_sample,ID,V),
	(
	 V == 1 -> true
	;
	 V == 2 -> fail
	;
	 new_sample(ID)
	).

new_sample(ID) :-
	get_fact_probability(ID,Prob),
	random(R),
	R<Prob,
	!,
        update_array(mc_sample,ID,1).
new_sample(ID) :-
        update_array(mc_sample,ID,2),
        fail.

% if threshold reached, remember this by setting limit to on, then
% if up is on, store stopped derivation in second trie
% 
% List always length>=1 -> don't need []=true-case for tries
upper_bound(List) :-
	problog_control(on,limit),
	problog_control(check,up),
	reverse(List,R),
	(prune_check(R,2) -> true; insert_ptree(R,2)).

multiply_probabilities(CurrentLogP, LogProb, NLogProb) :-
	NLogProb is CurrentLogP+LogProb.	

% this is called by all inference methods before the actual ProbLog goal
% to set up environment for proving
% it resets control flags, method specific values to be set afterwards!
init_problog(Threshold) :-
	reset_non_ground_facts,
	reset_control,
	LT is log(Threshold),
	b_setval(problog_probability, 0.0),
	b_setval(problog_current_proof, []),
	nb_setval(problog_threshold, LT),
	problog_flag(maxsteps,MaxS),
	b_setval(problog_steps, MaxS).

% idea: proofs that are refinements of known proof can be pruned as they don't add probability mass
% note that current ptree implementation doesn't provide the check as there's no efficient method known so far...
prune_check(Proof,TreeID) :-
	problog_flag(prunecheck,on),
	prune_check_ptree(Proof,TreeID).

% to call a ProbLog goal, patch all subgoals with the user's module context
% (as logical part is there, but probabilistic part in problog)
problog_call(Goal) :-
	yap_flag(typein_module,Module),
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
% input: ID of trie to be used
% output: probability and status (to catch potential failures/timeouts from outside)
% 
% with internal BDD timeout (set using problog flag bdd_time)
%
% bdd_ptree/3 constructs files for ProblogBDD from the trie
%
% if calling ProblogBDD doesn't exit successfully, status will be timeout
%
% writes number of proofs in trie and BDD time to standard user output
%
% if remember is on, input files for ProblogBDD will be saved
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eval_dnf(ID,Prob,Status) :- 
	((ID = 1, problog_flag(save_bdd,true)) -> problog_control(on,remember); problog_control(off,remember)),
	count_ptree(ID,NX),
	(
	    problog_flag(verbose,true)
	->
	(
	    NX=1
	->
	    format(user,'1 proof~n',[]);
            format(user,'~w proofs~n',[NX])
	);
	true
        ),
	problog_flag(dir,DirFlag),
	problog_flag(bdd_file,BDDFileFlag),
	atomic_concat([DirFlag,BDDFileFlag],BDDFile),
	problog_flag(bdd_par_file,BDDParFileFlag),
	atomic_concat([DirFlag,BDDParFileFlag],BDDParFile),
	(problog_control(check,remember) ->
	    bdd_ptree_map(ID,BDDFile,BDDParFile,Mapping),
	    atomic_concat([DirFlag,'save_map'],MapFile),
	    tell(MapFile),
	    format('mapping(~q).~n',[Mapping]),
	    flush_output,
	    told
	;
	bdd_ptree(ID,BDDFile,BDDParFile)
        ), 
	problog_flag(bdd_time,BDDTime),
	problog_flag(bdd_result,ResultFileFlag),
	atomic_concat([DirFlag,ResultFileFlag],ResultFile),
	problog_dir(PD),
	atomic_concat([PD,'/ProblogBDD -l ',BDDFile,' -i ',BDDParFile,' -m p -t ', BDDTime,' > ', ResultFile],Command),
	statistics(walltime,_),
	shell(Command,Return), 
	(
	    Return =\= 0
	->
	    Status = timeout
	;
	    (
		statistics(walltime,[_,E3]),
		(problog_flag(verbose,true) -> format(user,'~w ms BDD processing~n',[E3]);true),
		see(ResultFile),
		read(probability(Prob)),
		seen,
		delete_file(ResultFile),
		Status = ok
	    )
	),
	(problog_control(check,remember) ->
	    atomic_concat([DirFlag,'save_script'],SaveBDDFile),
	    rename_file(BDDFile,SaveBDDFile),
	    atomic_concat([DirFlag,'save_params'],SaveBDDParFile),
	    rename_file(BDDParFile,SaveBDDParFile)
	;
	true
        ),
	problog_control(off,remember).

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
	init_ptree(1),
	init_ptree(2),
	init_problog(Threshold).

add_solution :-
	b_getval(problog_current_proof, IDs),
	(IDs == [] -> R = true ; reverse(IDs,R)),
	insert_ptree(R,1).

compute_bounds(LP, UP, Status) :-
	eval_dnf(1,LP,StatusLow),
	(StatusLow \== ok ->
	    Status = StatusLow
	;
	merge_ptree(1,2,3),
	eval_dnf(3,UP,Status)),
	delete_ptree(1),
	delete_ptree(2),
	delete_ptree(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% approximate inference: lower bound based on all proofs above probability threshold
% problog_low(+Goal,+Threshold,-LowerBound,-Status)
% 
% same as problog_threshold/5, but lower bound only (no stopped derivations stored) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_low(Goal, Threshold, _, _) :-
	init_problog_low(Threshold),
	problog_control(off,up),
	problog_call(Goal),
	add_solution,
	fail.
problog_low(_, _, LP, Status) :-
	eval_dnf(1,LP,Status),
	delete_ptree(1).

init_problog_low(Threshold) :-
	init_ptree(1),
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
	delete_ptree(1),
	delete_ptree(2),
	(retract(low(_,Low)) -> true; true),
	(retract(up(_,Up)) -> true; true).


init_problog_delta(Threshold,Delta) :-
	retractall(low(_,_)),
	retractall(up(_,_)),
	retractall(stopDiff(_)),
	init_ptree(1),
	init_ptree(2),
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
	count_ptree(1,NProofs),
	count_ptree(2,NCands),
	(problog_flag(verbose,true) -> format(user,'~w proofs, ~w stopped derivations~n',[NProofs,NCands]);true),
	flush_output(user),
	eval_lower(NProofs,Low,StatusLow),
	(StatusLow \== ok ->
	    Status = StatusLow
	;
	    up(_,OUP),
	    IntDiff is OUP-Low,
	    ((IntDiff < Delta; IntDiff =:= 0) -> 
		Up=OUP, StatusUp = ok
	    ;
	        eval_upper(NCands,Up,StatusUp),
		delete_ptree(2),
		init_ptree(2),
		delete_ptree(3)
	    ),
	    (StatusUp \== ok ->
		Status = StatusUp
	    ;
	        Diff is Up-Low,
		(problog_flag(verbose,true) -> format(user,'difference:  ~6f~n',[Diff]);true),
		flush_output(user),
		((Diff < Delta; Diff =:= 0) -> Ans = 1; Ans = 0),
		Status = ok)).

% no need to re-evaluate if no new proofs found on this level
eval_lower(N,P,ok) :-
	low(N,P).
% evaluate if there are proofs
eval_lower(N,P,Status) :-
	N > 0,
	low(OldN,_),
	N \= OldN,
	eval_dnf(1,P,Status),
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
	merge_ptree(1,2,3),
	eval_dnf(3,UpP,StatusUp),
	(StatusUp = ok ->
	    retract(up(_,_)),
	    assert(up(N,UpP))
	;
	(problog_flag(verbose,true) -> format(user,'~w - continue using old up~n',[StatusUp]);true),
	flush_output(user),
	up(_,UpP)).

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
	problog_max_id(Goal, Prob, FactIDs),
	( FactIDs = [_|_] -> get_fact_list(FactIDs,Facts);
	    Facts = FactIDs).

init_problog_max(Threshold) :-
	retractall(max_probability(_)),
	retractall(max_proof(_)),
	assert(max_probability(-999999)),
	assert(max_proof(unprovable)),
	init_problog(Threshold).

update_max :-
	b_getval(problog_probability,CurrP),
	max_probability(MaxP),
	(CurrP =< MaxP ->
	    fail
	;
	b_getval(problog_current_proof, IDs),
	reverse(IDs,R),
	retractall(max_proof(_)),
	assert(max_proof(R)),
	nb_setval(problog_threshold, CurrP),
	retractall(max_probability(_)),
	assert(max_probability(CurrP))).

problog_max_id(Goal, _Prob, _Clauses) :-
	problog_call(Goal),
	update_max,
	fail.
problog_max_id(Goal, Prob, Clauses) :-
	max_probability(MaxP),
	nb_getval(problog_threshold, LT),
	problog_flag(last_threshold_log,ToSmall),
	((MaxP >= LT ; \+ problog_control(check,limit); LT < ToSmall) ->
	    ((max_proof(unprovable), problog_control(check,limit), LT < ToSmall) ->
		problog_flag(last_threshold,Stopping),
		Clauses = unprovable(Stopping) 
	    ; max_proof(Clauses)),
	   Prob is exp(MaxP)
       ;
	problog_flag(id_stepsize_log,Step),
	NewLT is LT+Step,
	nb_setval(problog_threshold, NewLT),
	problog_control(off,limit),
	problog_max_id(Goal, Prob, Clauses)).

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
% version with _save at the end  renames files for ProblogBDD to keep them
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
problog_kbest_save(Goal, K, Prob, Status, BDDFile, ParamFile) :-
	problog_kbest(Goal, K, Prob, Status),
	( Status=ok ->
	    problog_flag(bdd_file,InternBDDFlag),
	    problog_flag(bdd_par_file,InternParFlag),
	    problog_flag(dir,DirFlag),
	    atomic_concat([DirFlag,InternBDDFlag],InternBDD),
	    atomic_concat([DirFlag,InternParFlag],InternPar),
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
	eval_dnf(1,Prob,Status),
	delete_ptree(1).

init_problog_kbest(Threshold) :-
	retractall(current_kbest(_,_,_)),
	assert(current_kbest(-999999,[],0)),  %(log-threshold,proofs,num_proofs)  
	init_ptree(1),
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
	    reverse(RevProof,Proof),
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
	   (NewLength>K
            -> 
	    First is NewLength-K+1,
	    cutoff(NewList,NewLength,First,FinalList,FinalLength)
            ;
	    FinalList=NewList, FinalLength=NewLength
           ),
	   FinalList=[NewThres-_|_],
	   nb_setval(problog_threshold,NewThres),
	 assert(current_kbest(NewThres,FinalList,FinalLength))
       ).

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
	insert_ptree(true,1).
build_prefixtree([_-L|List]) :-
	insert_ptree(L,1),
	build_prefixtree(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exact probability 
% problog_exact(+Goal,-Prob,-Status)
%
% using all proofs = using all proofs with probability > 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_exact(Goal,Prob,Status) :-
	problog_low(Goal,0,Prob,Status).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% probability by sampling: 
% running another N samples until 95percentCI-width<Delta 
% lazy sampling using three-valued array indexed by internal fact IDs
%
% still collects actual proofs found in samples in ptree, though this is no longer used  
%   by method itself, only to write number to log-file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_montecarlo(_,_,_) :-
	non_ground_fact(_),
	!,
	format(user_error,'Current database contains non-ground facts.',[]),
	format(user_error,'Monte Carlo inference is not possible in this case. Try k-best instead.',[]),
	fail.
	

problog_montecarlo(Goal,Delta,Prob) :-
	retractall(mc_prob(_)),
        nb_getval(probclause_counter,ID), !,
        C is ID+1,
        static_array(mc_sample,C,char),
  	problog_control(off,up),
	problog_flag(mc_batchsize,N),
	problog_flag(mc_logfile,File1),
	problog_flag(dir,Dir),
	atomic_concat([Dir,File1],File),
	montecarlo(Goal,Delta,N,File),
	retract(mc_prob(Prob)).

montecarlo(Goal,Delta,K,File) :-
        reset_static_array(mc_sample),
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
	Epsilon is 2*sqrt(Prob*(1-Prob)/SamplesNew), 
	Low is Prob-Epsilon,
	High is Prob+Epsilon,
	Diff is 2*Epsilon,
	statistics(walltime,[T2,_]),
	Time is (T2-InitialTime)/1000,
	(problog_flag(verbose,true) -> format('~n~w samples~nestimated probability ~w~n95 percent confidence interval [~w,~w]~n',[SamplesNew,Prob,Low,High]);true),
	open(File,append,Log),
	format(Log,'~w  ~8f  ~8f  ~8f  ~8f  ~3f~n',[SamplesNew,Prob,Low,High,Diff,Time]),
	close(Log),
	((Diff<Delta; Diff =:= 0) -> 	(problog_flag(verbose,true) -> format('Runtime ~w sec~2n',[Time]);true),assert(mc_prob(Prob))
		    ;	
	                montecarlo(Goal,Delta,K,SamplesNew,File,Next,InitialTime)).

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
	problog_tabled(P),%show_table(P),table_statistics(P),get(_),
	abolish_table(P),
	fail.
clean_sample.

% find new proof -- need to reset control after init
get_some_proof(Goal) :-
	init_problog(0),
	problog_control(on,mc),
	problog_call(Goal).

problog_table(M:P) :- !,
	problog_table(P,M).
problog_table(P) :-
	prolog_load_context(module,M),
	problog_table(P,M).

problog_table(M:P,_) :-
	problog_table(P,M).
problog_table((P1,P2),M) :-
	problog_table(P1,M),
	problog_table(P2,M).
problog_table(P,M) :-
	table(M:P),
	assert(problog_tabled(M:P)).


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
