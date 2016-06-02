%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2010-08-24 15:14:21 +0200 (Di, 24. Aug 2010) $
%  $Revision: 4671 $
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
%  Guy Van den Broeck
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
% DECISION-THEORETIC PROBLOG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(dtproblog, [
    problog_delta/5,
    problog_threshold/5,
    problog_low/4,
    problog_kbest/4,
    problog_kbest_save/6,
    problog_max/3,
    problog_exact/3,
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
    continuous_fact/1,
    non_ground_fact/1,
    export_facts/1,
    problog_help/0,
    show_inference/0,
    problog_dir/1,
    set_problog_flag/2,
    problog_flag/2,
    problog_flags/0,
    reset_problog_flags/0,
    problog_assert/1,
    problog_assert/2,
    problog_retractall/1,
    problog_statistics/2,
    problog_statistics/0,
    grow_atom_table/1,
    problog_exact_nested/3,
    problog_tabling_negated_synonym/2,
    build_trie/2,
    build_trie/3,
    problog_infer/2,
    problog_infer/3,
    problog_infer_forest/2,
    write_bdd_struct_script/3,
    problog_bdd_forest/1,
    require/1,
    unrequire/1,
    '::'/2,
    probabilistic_fact/3,
    problog_real_kbest/4,
    in_interval/3,
    below/2,
    above/2,
    op( 550, yfx, :: ),
    op( 550, fx, ?:: ),
    op( 1150, fx, problog_table ),

    % DTProbLog
    set_strategy/1,
    unset_strategy/1,
    dtproblog_utility_facts/1,
    dtproblog_utility_attributes/1,
    dtproblog_ev/2,
    dtproblog_ev/3,
    dtproblog_ev/4,
    dtproblog_decisions/1,
    dtproblog_decision_ids/1,
    dtproblog_decision_ids/2,
    dtproblog_solve/2,
    dtproblog_solve_specialized/2,
    dtproblog_solve_general/2,
    dtproblog_solve_local/4,
    dtproblog_solve_naive/2,
    op( 550, yfx, => )
  ]).

:- style_check(all).
:- yap_flag(unknown,error).

% problog-related modules

:- use_module('problog').

:- use_module('problog/flags',[
    problog_define_flag/4,
    problog_define_flag/5,
    problog_define_flag/6,
    set_problog_flag/2,
    reset_problog_flags/0,
    problog_flag/2
  ]).

:- use_module('problog/os', [convert_filename_to_working_path/2,
                             convert_filename_to_problog_path/2]).

:- use_module('problog/ptree', [delete_ptree/1]).

:- use_module('problog/tabling', [clear_tabling/0]).


% general yap modules
:- use_module(library(system), [delete_file/2, shell/2]).

:- initialization((
	problog_define_flag(optimization, problog_flag_validate_atom, 'optimization algorithm [local/global]', global, dtproblog),
	problog_define_flag(forest_type, problog_flag_validate_atom, 'type of BDD forest [dependent/independent]', dependent, dtproblog)
)).

init_dtproblog :-
	problog_control(off,find_decisions),
	problog_control(off,internal_strategy).

:- initialization(init_dtproblog).

:- op( 550, yfx, :: ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility Attributes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% op for utility attributes
:- op( 550, yfx, => ).

% SETOF DOESNT WORK!!! BUT all/3 is A LOT slower because it doesn't sort the solutions?
dtproblog_utility_facts(Facts) :-
  all((Attr => Util),user:(Attr => Util),Facts).

dtproblog_utility_attributes(Attrs) :-
  dtproblog_utility_facts(Facts),
  facts_to_attributes(Facts,Attrs).

facts_to_attributes([],[]).
facts_to_attributes([A => _|FR],[A|AR]) :- facts_to_attributes(FR,AR).

conditioned_utility_facts([],_,[],[]).
conditioned_utility_facts([(Attr => Reward)|Facts],Condition,[(Condition,Attr)|Attrs],[Reward|Rewards]):-
    conditioned_utility_facts(Facts,Condition,Attrs,Rewards).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strategies (getting/setting/transforming)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Internal strategy representation
% for GROUND strategies (one can specify a ground strategy for a non-ground decision clause)
% e.g. 1 :: market(guy) for ? :: market(P).
set_ground_strategy(GID,LogProb) :- bb_put(GID,LogProb).
get_ground_strategy(GID,LogProb) :- bb_get(GID,LogProb),!.
get_ground_strategy(_,never).

% Internal strategy representation
% for NON-GROUND strategies
% e.g. 1 :: market(guy) for ? :: market(P)
:- dynamic(non_ground_strategy/2).

% Get Strategy
strategy(_,_,_) :-
    \+ problog_control(check,internal_strategy),
    throw(error('Trying to get a strategy that is not set.')).
strategy(ID,Decision,Prob) :-
	grounding_id(ID,Decision,GID),
	bb_get(GID,LogProb), % because we don't want the default, maybe there is a non-ground strategy
    !,
    logprob_prob(LogProb, Prob).
strategy(_,Decision,Prob) :-
	non_ground_strategy(Decision,LogProb),
    !,
    logprob_prob(LogProb, Prob).
strategy(_,_,never).

strategy_log(ID,Decision,LogProb) :-
  strategy(ID,Decision,Prob),
  logprob_prob(LogProb, Prob).

% convert from/to probabilities and their logarithms
logprob_prob(always,always) :- !.
logprob_prob(never,never) :- !.
logprob_prob(0,always) :- !.
logprob_prob(-inf,never) :- !.
logprob_prob(always,1) :- !.
logprob_prob(never,0) :- !.
logprob_prob(LogP,P) :-
  number(LogP),
  !,
  P is exp(LogP).
logprob_prob(LogP,P) :-
  number(P),
  !,
  LogP is log(p).

% Set Strategy
% expects a list of (p :: decision) terms
%   - (decision) is interpreted as (1 :: decision)
%   - decisions that are not set will evaluate to (0 :: decision)
set_strategy(_) :-
  problog_control(check,internal_strategy),
  throw(error('A strategy is already set, unset first.')).
set_strategy([]) :- problog_control(on,internal_strategy).
set_strategy([Term|R]) :-
  strategy_entry(Term,LogProb,Decision),
  (user:problog_user_ground(Decision)->
      decision_fact(ID,Decision),
      grounding_id(ID,Decision,ID2),
      %format("Setting ~q/~q to ~q~n",[Decision,ID2,Prob]),
      set_ground_strategy(ID2,LogProb)
  ;
      copy_term(Decision, Decision2),
      assertz(non_ground_strategy(Decision2,LogProb))
  ),
  set_strategy(R).

unset_strategy(_) :-
  \+ problog_control(check,internal_strategy),
  throw(error('Cannot unset a strategy when no strategy is set.')).
unset_strategy([]) :-
  retractall(non_ground_strategy(_,_)),
  problog_control(off,internal_strategy).
unset_strategy([Term|R]) :-
  strategy_entry(Term,LogProb,Decision),
  (user:problog_user_ground(Decision)->
      decision_fact(ID,Decision),
      grounding_id(ID,Decision,ID2),
      %format("Unsetting ~q/~q to ~q~n",[Decision,ID2,Prob]),
      bb_delete(ID2,LogProb)
  ;
      true
  ),
  unset_strategy(R).

strategy_entry('::'(Prob,Decision),LogProb,Decision) :-
  !,logprob_prob(LogProb, Prob).
strategy_entry(Decision,always,Decision).

% Get strategy for a list of decision IDs
% only use when grounding ids are known and strategy is stored internally!
strategy_as_term_list(IDs,List) :- strategy_as_term_list(IDs,[],List).
strategy_as_term_list([],In,In).
strategy_as_term_list([ID|R],In,Out) :-
	strategy_as_term(ID,In,In2),
	strategy_as_term_list(R,In2,Out).

% Get strategy for a decision ID
strategy_as_term(ID,In,Out) :-
	%findall(grounding_is_known(D,I),grounding_is_known(D,I),LGround),
	%findall(decision_fact(D,I),decision_fact(D,I),LBasic),
	%format("Known IDs: ~q~n",[LGround]),
	%format("Known IDs: ~q~n",[LBasic]),
	((recover_grounding_id(ID,GID),grounding_is_known(Decision,GID)) ->
		 %original fact was non-ground
		 true
	;
		% original fact was ground
		decision_fact(ID,Decision)
	),
	strategy(ID,Decision,Prob),
	strategy_as_term_entry(Decision,Prob,In,Out).

% Convert strategy for a decision to term representation
strategy_as_term_entry(_,0,In,In) :- !.
strategy_as_term_entry(Decision,1,In,[Decision|In]) :- !.
strategy_as_term_entry(_,never,In,In) :- !.
strategy_as_term_entry(Decision,always,In,[Decision|In]) :- !.
strategy_as_term_entry(Decision,P,In,[P'::'Decision|In]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utility inference
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Unconditional expected value for all utilities
dtproblog_ev(Strategy,Ev) :- dtproblog_ev(Strategy,true,Ev).

% Conditional expected value for a given strategy and all utility attributes
dtproblog_ev(Strategy,Condition,Ev) :-
    (dtproblog_utility_facts(UtilFacts) ->
         dtproblog_ev(Strategy,Condition,UtilFacts,Ev)
    ;
         format('There are no utility facts in the program.~n',[]),
         Ev = 0
    ).

% Conditional expected value for a given strategy and utility attributes
dtproblog_ev(Strategy,Condition,UtilFacts,Ev) :-
	require(keep_ground_ids),
	set_strategy(Strategy),
	ev_for_internal_strategy(Condition,UtilFacts,Ev),
	unset_strategy(Strategy),
	unrequire(keep_ground_ids),
	reset_non_ground_facts.

% Conditional expected value for internal strategy and given utility attributes
% assumes that problog_control(on,internal_strategy)
ev_for_internal_strategy(Condition,UtilFacts,Ev) :-
  require(keep_ground_ids),
  (problog_infer_forest_supported ->
    % specialized version for inference using forests
    conditioned_utility_facts(UtilFacts,Condition,Goals,Utilities),
    problog_infer_forest([Condition|Goals],[CondProb|GoalProbs]),
    !, % forest inference was supported, don't try general purpose
    summed_utils(Utilities,GoalProbs,0,EvUncond),
    Ev is EvUncond/CondProb
  ;
    % general-purpose version
    ev_loop(Condition, UtilFacts, 0, EvUnnormalized),
    problog_infer(Condition, Prob),
    %format("Dividing the utilities by the conditional probability ~q~n",[Prob]),
    (Prob > 0.000001 ->
      Ev is EvUnnormalized/Prob
    ;
      format('Impossible condition: ~q has probability ~q.~n', [Condition,Prob]),
      %throw(error(improbable_condition(Condition)))
      Ev = -inf
    )
  ),
  unrequire(keep_ground_ids),
  reset_non_ground_facts.

summed_utils([],[],Ev,Ev).
summed_utils([Util|Utils],[Prob|Probs],Acc,Ev) :-
	Acc2 is Acc + (Util * Prob),
	summed_utils(Utils,Probs,Acc2,Ev).

ev_loop(_, [],Acc,Acc).
ev_loop(Condition,[(Attr => Util)|R],Acc,Ev) :-
        problog_infer((Condition,Attr), Prob),
        Acc2 is Acc + (Prob * Util),
        %format('The probability of ~q is ~q, yielding a utility of ~q.~n', [(Condition,Attr),Prob,Prob * Util]),
        ev_loop(Condition,R,Acc2,Ev).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Finding all decisions used in proofs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Finding all decisions used in proofs
dtproblog_decisions(Decisions) :-
	require(keep_ground_ids),
	dtproblog_decision_ids(IDs),
	ids_as_decisions(IDs,Decisions),
	unrequire(keep_ground_ids),
	reset_non_ground_facts.


% Get decisions for a list of IDs
ids_as_decisions(IDs,List) :- ids_as_decisions(IDs,[],List).

ids_as_decisions([],In,In).
ids_as_decisions([ID|R],In,Out) :-
    id_as_decision(ID,In,In2),
    ids_as_decisions(R,In2,Out).

id_as_decision(ID,In,[Decision|In]) :-
    %findall(grounding_is_known(D,I),grounding_is_known(D,I),LGround),
    %findall(decision_fact(D,I),decision_fact(D,I),LBasic),
    %format("Known IDs: ~q~n",[LGround]),
    %format("Known IDs: ~q~n",[LBasic]),
    ((recover_grounding_id(ID,GID),grounding_is_known(Decision,GID)) ->
        % original fact was non-ground
        true
    ;
        % original fact was ground
        decision_fact(ID,Decision)
    ).

% Finding all decision IDs used in proofs
dtproblog_decision_ids(Decisions) :-
	(dtproblog_utility_attributes(UtilityAttrs) ->
    	 dtproblog_decision_ids(UtilityAttrs,Decisions)
    ;
         Decisions = []
    ).

dtproblog_decision_ids(UtilityAttrs,Decisions) :-
	require(keep_ground_ids),
	problog_control(on,find_decisions),
	reset_decisions,
	add_decisions_all(UtilityAttrs),
	unrequire(keep_ground_ids),
	reset_non_ground_facts,
	get_decisions(Decisions),
	problog_control(off,find_decisions),
	reset_decisions.

% TODO generalize so that it works with every inference method, not just exact.
add_decisions_all([]) :-
    clear_tabling.
add_decisions_all([Goal|R]) :-
	add_decisions(Goal),
	add_decisions_all(R).

% UGLY - needs to actually build tries for tabling to work.
% TODO change tabling.yap to do nothing when problog_control(on,find_decisions)
% then, simplify this predicate so that it doesn't build tries
% setting problog_control(on,mc) might work, but will maybe prune some decisions away?
add_decisions(Goal) :-
    problog_control(on, exact),
    build_trie(exact, Goal, Trie),
    delete_ptree(Trie),
    problog_control(off, exact).

reset_decisions :- bb_put(problog:decisions,[]).

get_decisions(D) :- bb_get(problog:decisions,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Strategy optimization
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dtproblog_solve(Strategy,EV) :-
	(dtproblog_solve_specialized_supported -> % try to go specialized
      dtproblog_solve_specialized(Strategy,EV)
	;
        format('Flag settings not supported by specialized solution algorithm.~nTrying general purpose version.~n',[]),
		(dtproblog_solve_general_supported -> % try to go general
          dtproblog_solve_general(Strategy,EV)
        ;
          throw(error('Flag settings not supported by dtproblog_solve/2.'))
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strategy optimization (specialized in BDD)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dtproblog_solve_specialized(Strategy,EV) :-
  (dtproblog_solve_specialized_supported ->
    require(keep_ground_ids),
    dtproblog_utility_facts(UtilFacts),
    conditioned_utility_facts(UtilFacts,true,Goals,Utilities),
    write_util_file(Utilities),
    problog_bdd_forest(Goals),
    length(Goals,N),
    bdd_optimization(N,EV,DecisionIDs,ok),
    (problog_flag(save_bdd,true) -> true ; delete_util_file ),
    strategy_as_term_list(DecisionIDs,Strategy),
    unset_strategy(Strategy), % was set by bdd_optimization/4
    require(keep_ground_ids),
    reset_non_ground_facts
  ;
    throw(error('Flag settings not supported by dtproblog_solve_specialized/2.'))
  ).

dtproblog_solve_specialized_supported :-
  problog_bdd_forest_supported,
  (
    problog_flag(forest_type, dependent)
  ;
    problog_flag(optimization, local)
  ).


% Write a utility file, with for every utility attribute BDD, its reward value on a new line
write_util_file(Utils) :-
	bdd_util_file(UtilFile),
	open(UtilFile,'write',UtilFileStream),
	tell(UtilFileStream),
	length(Utils,N),
	format("~w~n",[N]),
	write_util_file_line(Utils),
	flush_output,
	told.

write_util_file_line([]).
write_util_file_line([U|R]) :-
	format("~w~n",[U]),
	write_util_file_line(R).

bdd_util_file(UtilFile) :-
	problog_flag(bdd_file,BDDFileFlag),
    atomic_concat([BDDFileFlag,'_',utils],UtilFileName),
	convert_filename_to_working_path(UtilFileName, UtilFile).

delete_util_file :-
    bdd_util_file(UtilFile),
    delete_file(UtilFile,[]).

bdd_optimization(N,EV,Decisions,Status) :-
    bdd_files(BDDFile,BDDParFile),
    problog_flag(bdd_time,BDDTime),
    (problog_flag(dynamic_reorder, true) -> ParamD = '' ; ParamD = ' -dreorder'),
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
	bdd_util_file(UtilFile),
	(problog_flag(optimization,local) -> LocalPar = ' -lo';LocalPar = ''),
	(problog_flag(forest_type,independent) -> Forest = ' -if';Forest = ''),
	%(problog_flag(verbose,true) -> Debug = ' -d';Debug = ''), % messes up result parsing
    atomic_concat([ProblogBDD, Param, ' -l ',BDDFile,' -i ',BDDParFile,' -u ',UtilFile,' -m s',LocalPar,Forest,' -t ', BDDTime,' > ', ResultFile],Command),
	statistics(walltime,_),
% 	format(user,'$ ~w~n',[Command]),
	shell(Command,Return),
	(Return =\= 0 ->
	    Status = timeout
	;
	    statistics(walltime,[_,E3]),
		(problog_flag(verbose,true) -> format(user,'~w ms BDD processing~n',[E3]);true),
		see(ResultFile),
		read(expected_value(EV)),
		read_strategy(Decisions),
		seen,
		Status = ok,
		% cleanup
		(problog_flag(save_bdd,true) ->
			true
		;
			delete_file(BDDFile,[]),
			delete_file(BDDParFile,[]),
			delete_file(ResultFile,[]),
			delete_bdd_forest_files(N)
		)
	).

% set the strategy in the internal format and returns a list of all decisions
read_strategy(_) :-
  problog_control(check,internal_strategy),
  throw(error('A strategy is already set, unset first.')).
read_strategy(DecisionIDs) :-
  problog_control(on,internal_strategy),
  read_strategy_intern(DecisionIDs).
read_strategy_intern(DecisionIDs) :-
	read(T),
	(T = end_of_file ->
		DecisionIDs = []
	;
		T = strategy(ID,Prob),
        logprob_prob(LProb,Prob),
		set_ground_strategy(ID,LProb),
		DecisionIDs = [ID|Rest],
		read_strategy_intern(Rest)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Strategy optimization (general purpose)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dtproblog_solve_general(Strategy,EV) :-
  (dtproblog_solve_general_supported ->
    dtproblog_utility_facts(UtilFacts),
    dtproblog_solve_local(true,UtilFacts,Strategy,EV)
  ;
    throw(error('Flag settings not supported by dtproblog_solve_specialized/2.'))
  ).

dtproblog_solve_general_supported :- problog_flag(optimization, local).

dtproblog_solve_local(Condition,Utils,Strategy,EV) :-
  require(keep_ground_ids),
  facts_to_attributes(Utils,Attrs),
  dtproblog_decision_ids(Attrs,DecisionIDs),
  problog_control(on,internal_strategy), % consider strategy set, even though everything is default
  ev_for_internal_strategy(Condition,Utils,EvStart),
  optimization_iteration_loop(DecisionIDs,Condition,Utils,EvStart,EV),
  strategy_as_term_list(DecisionIDs,Strategy),
  unset_strategy(Strategy),
  unrequire(keep_ground_ids),
  reset_non_ground_facts.

optimization_iteration_loop(Decisions,Condition,Utils,EVIn,EVOut) :-
	optimization_decision_loop(Decisions,Condition,Utils,EVIn,EVTemp),
	strategy_as_term_list(Decisions,Strategy),
	(problog_flag(verbose,true) -> format("Found strategy ~q with EV=~q~n",[Strategy,EVTemp]);true),
	(EVIn == EVTemp ->
		EVOut = EVTemp
	;
		optimization_iteration_loop(Decisions,Condition,Utils,EVTemp,EVOut)
	).

optimization_decision_loop([],_,_,Ev,Ev).
optimization_decision_loop([ID|Rest],Condition,Utils,EvIn,EvOut) :-
	get_ground_strategy(ID,ProbBefore),
	flip(ProbBefore, ProbAfter),
	set_ground_strategy(ID,ProbAfter),
	ev_for_internal_strategy(Condition,Utils,EvTest),
	(EvTest>EvIn ->
		EvTemp = EvTest,
		(problog_flag(verbose,true) -> format("Changing strategy for #~q to ~q for EV of ~q~n",[ID,ProbAfter,EvTest]);true)
	;
		EvTemp = EvIn,
		set_ground_strategy(ID,ProbBefore),
		(problog_flag(verbose,true) -> format("Keeping strategy for #~q at ~q because EV is ~q~n",[ID,ProbBefore,EvTest]);true)
	),
	optimization_decision_loop(Rest,Condition,Utils,EvTemp,EvOut).

flip(always,never) :- !.
flip(never,always) :- !.
flip(P,always) :- P<1, !.
flip(_,never).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Naive Search
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dtproblog_solve_naive(Strategy,EV) :-
  require(keep_ground_ids),
  dtproblog_decisions(Decisions),
  all_subsets(Decisions,Strategies),
  max_strategy(Strategies,[],-inf,Strategy,EV),
  unrequire(keep_ground_ids),
  reset_non_ground_facts.

max_strategy([],Strategy,EV,Strategy,EV).
max_strategy([S1|Rest],S2,U2,S3,U3) :-
  dtproblog_ev(S1,true,U1),
  %format("EV of ~q is ~q~n",[S1,U1]),
  (U1>U2 ->
    max_strategy(Rest,S1,U1,S3,U3)
  ;
    max_strategy(Rest,S2,U2,S3,U3)
  ).

% List of all sublists
all_subsets([], [[]]).
all_subsets([X|Xs], Subsets) :-
    all_subsets(Xs, Subsets1),
    attach_first_element(Subsets1, X, Subsets, Subsets1).

attach_first_element([], _, S, S).
attach_first_element([Sub|Subs], X, [[X|Sub]|XSubs], S) :-
    attach_first_element(Subs, X, XSubs, S).


