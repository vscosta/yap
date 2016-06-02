%%% -*- Mode: Prolog; -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-07-27 17:38:26 +0200 (Wed, 27 Jul 2011) $
%  $Revision: 6461 $
%
%  This file is part of ProbLog
%  http://dtai.cs.kuleuven.be/problog
%
%  ProbLog was developed at Katholieke Universiteit Leuven
%
%  Copyright 2008, 2009, 2010
%  Katholieke Universiteit Leuven
%
%  Main author of this file:
%  Angelika Kimmig
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% aProbLog prototype
%
% for background, see
% Kimmig et al "An Algebraic Prolog for Reasoning about Possible Worlds" AAAI 2011
% http://dtai.cs.kuleuven.be/problog/publications.html
%
% includes ProbLog code fragments
% uses the online interface to problogbdd/simplecudd written by Theofrastos Mantadelis (problog/bdd.yap)
%
% NOTE:
% - flags neutral_sum and disjoint_sum decide which inference method is called from aproblog_label/2, they are ignored when calling these underlying methods directly
% - all four methods use the set of explanations found by SLD resolution as covering set of explanations 
% - compensation for non-neutral sums is restricted to the variables that occur in some proof of the query by default,
%   setting flag compensate_unused to true will activate compensation for all ground unseen variables (throws error in programs with non-ground facts)
% - for disjoint sum, no trie representation of the DNF is built, i.e. n proofs resulting in same explanation appear n times in sum (old versions _on_dnf not exported)
% - BDDs are constructed using dbtries with optimization level 0 (predicates for naive preprocessing can be activated in the source code (search comments on dnf_to_bdd_naive))
% - dynamic labels are not yet supported (i.e. no L::fact(L).)
%
% hacker's corner:
% - declaring sums to be neutral simulates labels defined in terms of the set of SLD-explanations (not considered in AAAI paper)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(aproblog,[op( 550, yfx, :: ),
		    aproblog_label/2,          % decide cases based on flags disjoint_sum and neutral_sum (default: both false -> case 4)
		    label_neutral_disjoint/2,  % case 1: sums are neutral and disjoint
		    label_disjoint_neutral/2,  % synonym for label_neutral_disjoint/2
		    label_disjoint/2,          % case 2: sums are disjoint but not neutral (also solves case 1, but with overhead)
		    label_neutral/2,           % case 3: sums are neutral but not disjoint (also solves case 1, but with overhead)
		    label/2,                   % case 4: sums are not neutral and not disjoint (also solves cases 1-3, but with overhead)
		    '::'/2,
  		    set_aproblog_flag/2,
		    aproblog_flag/2,
		    print_bdd/1,
		    print_dnf/1]).

:- style_check(all).
:- yap_flag(unknown,error).

:- op( 550, yfx, :: ).

:- multifile('::'/2).

:- ensure_loaded('problog/ptree').
:- ensure_loaded('problog/bdd').
:- ensure_loaded('problog/gflags').
:- ensure_loaded('problog/flags').
:- ensure_loaded('problog/os').
:- ensure_loaded(library(tries)).
:- ensure_loaded(library(terms)).
:- ensure_loaded(library(lists)).

:- dynamic(aproblog_predicate/2).
:- dynamic(non_ground_fact/1).
:- dynamic calcp/2.                     % used in lazy evaluation
:- dynamic aproblog_cached/4.          % cache in depth first search
:- dynamic aproblog_cache_vars/0.      % decides whether cache also contains variables which are then used for compensation 

% by default don't talk, take care of both potential problems, and do not compensate for unused facts 
:- initialization((
	problog_define_flag(verbose,     problog_flag_validate_boolean, 'display information', false, aproblog),
	problog_define_flag(disjoint_sum,     problog_flag_validate_boolean, 'sum is disjoint', false, aproblog),
	problog_define_flag(neutral_sum,     problog_flag_validate_boolean, 'sum is neutral', false, aproblog),
	problog_define_flag(compensate_unused,     problog_flag_validate_boolean, 'compensate non-neutral sum for unused facts', false, aproblog)	   
)).

% directory where simplecudd executable is located
% automatically set during loading -- assumes it is in same place as this file (problog.yap)
%:- PD = /usr/local/bin, set_problog_path(PD).
:- getcwd(PD), set_problog_path(PD).

aproblog_flag(F,V) :-
	problog_flag(F,V).
set_aproblog_flag(F,V) :-
	set_problog_flag(F,V).


% backtrack over all labeled facts
% must come before term_expansion
Label::Goal :-
    labeled_fact(Label,Goal,_ID).

% backtrack over all labeled facts
labeled_fact(Label,Goal,ID) :-
	ground(Goal),
	!,
	Goal =.. [F|Args],
	atomic_concat('aproblog_',F,F2),
	append([ID|Args],[Label],Args2),
	Goal2 =..[F2|Args2],
	length(Args2,N),
	current_predicate(F2/N),
	Goal2.
labeled_fact(Label,Goal,ID) :-
	get_internal_fact(ID,ProblogTerm,_ProblogName,_ProblogArity),
	ProblogTerm =.. [F,_ID|Args],
	append(Args2,[Label],Args),
	name(F,[_a,_p,_r,_o,_b,_l,_o,_g,_|F2Chars]),
	name(F2,F2Chars),
	Goal =.. [F2|Args2].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% term expansion / core is taken from problog_neg and adapted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

user:term_expansion(_P::( _Goal :- _Body ), _Error) :-
	throw(error('we do not support this (yet?)!')).

user:term_expansion(P::Goal, aproblog:ProbFact) :- 
	functor(Goal, Name, Arity),
        atomic_concat([aproblog_,Name],AproblogName),
	Goal =.. [Name|Args],
	append(Args,[P],L1),
	labelclause_id(ID),
	ProbFact =.. [AproblogName,ID|L1],
	(
	 ground(P)
	->
	 assert_static(id_label(ID,P)) % Label is fixed -- assert it for quick retrieval
	;
				% Label is a variable... we don't support that yet
	    throw(error('Variable labels are not (yet) supported! Your program contains':P::Goal))
	),
 	(
	    ground(Goal)
	->
	    true;
	    assert(non_ground_fact(ID))
	),
	aproblog_predicate(Name, Arity, AproblogName).
	    

% introduce wrapper clause if predicate seen first time
aproblog_predicate(Name, Arity, _) :-
	aproblog_predicate(Name, Arity), !.

aproblog_predicate(Name, Arity, AproblogName) :-
	functor(OriginalGoal, Name, Arity),
	OriginalGoal =.. [_|Args],
	append(Args,[_],L1),
	ProbFact =.. [AproblogName,ID|L1],
	prolog_load_context(module,Mod),
	
	assert( (Mod:OriginalGoal :- ProbFact, 
	                             (
					 non_ground_fact(ID)
				     ->
				         (non_ground_fact_grounding_id(OriginalGoal,G_ID),
					   atomic_concat([ID,'_',G_ID],ID2));
					 ID2=ID
				     ),
				     add_to_proof(ID2)
		 )),

	assert( (Mod:aproblog_not(OriginalGoal) :- ProbFact,
	                                          (
						      non_ground_fact(ID)
						  ->
						     ( non_ground_fact_grounding_id(OriginalGoal,G_ID),
						        atomic_concat([ID,'_',G_ID],ID2));
						      ID2=ID
						  ),
						  add_to_proof_negated(ID2)
		 )),
	    
	assert(aproblog_predicate(Name, Arity)),
	ArityPlus2 is Arity+2,
	dynamic(aproblog:AproblogName/ArityPlus2).	

% generate next global identifier
:- nb_setval(labelclause_counter,0).
labelclause_id(ID) :-
	nb_getval(labelclause_counter,ID), !,
	C1 is ID+1,
	nb_setval(labelclause_counter,C1), !.

% managing non-ground facts
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
	nb_setval(non_ground_fact_grounding_id_counter,0),
	retractall(grounding_is_known(_,_)).

% accessing internal information
get_fact_label(ID,Prob) :-
	(
	 id_label(ID,W)
	->
	 Prob = W
	;
	 get_fact_from_id(ID,F),
	 atom_number(F,N),
	 id_label(N,Prob)
	).

get_internal_fact(ID,AproblogTerm,AproblogName,AproblogArity) :-
	aproblog_predicate(Name,Arity),   
	atomic_concat([aproblog_,Name],AproblogName),
	AproblogArity is Arity+2,
	functor(AproblogTerm,AproblogName,AproblogArity),
	arg(1,AproblogTerm,ID),
	call(AproblogTerm). % have to keep choicepoint to allow for :: backtracking over all facts


get_fact(ID,OutsideTerm) :-
	get_internal_fact(ID,AproblogTerm,AproblogName,AproblogArity),
	AproblogTerm =.. [_Functor,ID|Args],
	atomic_concat('aproblog_',OutsideFunctor,AproblogName),
	Last is AproblogArity-1,
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
get_fact_list([neg(T)|IDs],[not(Goal)|Facts]) :-
	!,
	aproblog_context(Goal,_,T),
	get_fact_list(IDs,Facts).
get_fact_list([ID|IDs],[Fact|Facts]) :-
	(ID=not(X) -> Fact=not(Y); Fact=Y, ID=X),
	get_fact(X,Y),
	get_fact_list(IDs,Facts).


% called "inside" probabilistic facts to update current state of proving
% if number of steps exceeded, fail
% if fact used before, succeed and keep status as is
%       else update state and succeed
add_to_proof(ID) :-
	b_getval(aproblog_steps,MaxSteps),
	b_getval(aproblog_current_proof, IDs),

%%%% Bernd, changes for negated ground facts
        \+ memberchk(not(ID),IDs),
%%%% Bernd, changes for negated ground facts

	( MaxSteps =< 0 -> 
	    fail
	;
	  ( memberchk(ID, IDs) ->
	    true
	  ;
	    b_setval(aproblog_current_proof, [ID|IDs])
	  ),
	  Steps is MaxSteps-1,
	  b_setval(aproblog_steps,Steps)
	).

%%%% Bernd, changes for negated ground facts
add_to_proof_negated(ID) :-
	b_getval(aproblog_steps,MaxSteps),
	b_getval(aproblog_current_proof, IDs),

        \+ memberchk(ID,IDs),
	( MaxSteps =< 0 -> 
	    fail
	;
	  ( memberchk(not(ID), IDs) ->
	    true
	  ;
	    b_setval(aproblog_current_proof, [not(ID)|IDs])
	  ),
	  Steps is MaxSteps-1,
	  b_setval(aproblog_steps,Steps)
	).
%%%% Bernd, changes for negated ground facts



% this is called before the actual aProbLog goal
% to set up environment for proving
init_aproblog :-
	reset_non_ground_facts,
	b_setval(aproblog_current_proof, []),
	b_setval(aproblog_steps,999999).
init_aproblog_trie :-
	init_ptree(Trie_Completed_Proofs),
	nb_setval(aproblog_completed_proofs, Trie_Completed_Proofs).
	


% to call an aProbLog goal, patch all subgoals with the user's module context
% (as logical part is there, but labeled part in aproblog)
aproblog_call(Goal) :-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predicates related to DNF construction and evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%
% given a query, build the DNF in the trie named aproblog_completed_proofs
%%%%%%%%%%%%%
build_dnf(Goal) :-
	init_aproblog,
	init_aproblog_trie,
	nb_getval(aproblog_completed_proofs, Trie),
	aproblog_call(Goal),
	add_solution(Trie),
	fail.
build_dnf(_).

add_solution(N) :-
	b_getval(aproblog_current_proof, IDs),
	(IDs == [] -> R = true ; reverse(IDs,R)),
	insert_ptree(R,N).

delete_dnf :-
	nb_getval(aproblog_completed_proofs, Trie),
	delete_ptree(Trie).

print_dnf :-
	nb_getval(aproblog_completed_proofs, Trie),
	print_ptree(Trie).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculating the label of the DNF:
% iterates over all conjunctions, performing semiring multiplication in conj, semiring addition between conj
%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluate_dnf(_) :-
	semiring_zero(Zero),
	nb_setval(aproblog_label, Zero),
	nb_getval(aproblog_completed_proofs, Trie),
	traverse_ptree(Trie,Explanation),
	update_label(Explanation),
	fail.
evaluate_dnf(Label) :-
	nb_getval(aproblog_label, Label).

update_label(Explanation) :-
	semiring_one(One),
	multiply_label(Explanation,One,Label),
	nb_getval(aproblog_label, OldLabel),
	semiring_addition(OldLabel,Label,NewLabel),
	nb_setval(aproblog_label, NewLabel).

multiply_label([],Result,Result).
multiply_label([not(First)|Rest],Acc,Result) :-
	!,
	get_fact_label(First,W),
	label_negated(W,WBar),
	semiring_multiplication(Acc,WBar,Next),
	multiply_label(Rest,Next,Result).
multiply_label([First|Rest],Acc,Result) :-
	!,
	get_fact_label(First,W),
	semiring_multiplication(Acc,W,Next),
	multiply_label(Rest,Next,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculating the label of the DNF in case the sum is not neutral, 
% compensation ignores variables not appearing in DNF
%%%%%%%%%%%%%%%%%%%%%%%%%%%
evaluate_dnf_with_compensation(_) :-
	semiring_zero(Zero),
	nb_setval(aproblog_label, Zero),
	nb_setval(aproblog_variables, []),
	nb_getval(aproblog_completed_proofs, Trie),
	traverse_ptree(Trie,Explanation),
	update_label_with_compensation(Explanation),
	fail.
evaluate_dnf_with_compensation(Label) :-
	nb_getval(aproblog_label, Label).

update_label_with_compensation(Explanation) :-
	semiring_one(One),
	multiply_label(Explanation,One,LabelI),  % LabelI is the label of the i-th explanation...
	nb_getval(aproblog_variables, Var),
	get_variables(Explanation,VarI),
	compensate_label(Var,VarI,LabelI,CLabelI), % ... which is corrected for Var\VarI
	nb_getval(aproblog_label, OldLabel),
	compensate_label(VarI,Var,OldLabel, COldLabel),  % OldLabel gets corrected for VarI\Var 
	semiring_addition(COldLabel,CLabelI,NewLabel),  % now we sum corrected labels up
	nb_setval(aproblog_label, NewLabel),
	append(Var,VarI,List),
	sort(List,NewVar),
	nb_setval(aproblog_variables,NewVar).  % and update the list of seen variables

% variant that always compensates for the full set of DNF variables
% does some unnecessary append and sort at the end of each update
evaluate_dnf_with_compensation_naive(_) :-
	semiring_zero(Zero),
	nb_setval(aproblog_label, Zero),
	nb_getval(aproblog_completed_proofs, Trie),
	edges_ptree(Trie,Vars),
	nb_setval(aproblog_variables, Vars),
	traverse_ptree(Trie,Explanation),
	update_label_with_compensation(Explanation),
	fail.
evaluate_dnf_with_compensation_naive(Label) :-
	nb_getval(aproblog_label, Label).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% predicates related to BDD construction and evaluation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%
% dnf to bdd translation using naive preprocessing
% (full conjunctions as intermediate results, one big disjunction at end)
%%%%%%%%%%%%%%%%%%%
dnf_to_bdd_naive :-
	bdd_init(FDO, PID),
	dnf_to_bdd_naive(FDO),
	bdd_kill(FDO, PID, _S).

dnf_to_bdd_naive(FDO) :-
	nb_setval(aproblog_script_lines,[]),
	nb_getval(aproblog_completed_proofs, Trie),
	traverse_ptree(Trie,Explanation),
	add_to_bdd(Explanation, FDO),
	fail.
dnf_to_bdd_naive(FDO) :-
	nb_getval(aproblog_script_lines,Lines),
	(
	 Lines = []            % empty trie is false
	->
	 bdd_line([],'FALSE',_,L)
	;
	 bdd_OR([], Lines, L)
	),
	bdd_laststep(LID),
	bdd_commit(FDO, L),
	bdd_commit(FDO, LID).

% trie with single element 'true"
add_to_bdd([true],FDO) :-
	!,
	bdd_line([],'TRUE',_,L1),
	bdd_laststep(L1S),
	bdd_commit(FDO, L1),
	nb_getval(aproblog_script_lines,SoFar),
	nb_setval(aproblog_script_lines,[L1S|SoFar]).
add_to_bdd(AndList,FDO) :-
	ids_to_vars(AndList,List),%write(List),nl,
	bdd_AND([], List, L1),
	bdd_laststep(L1S),
	bdd_commit(FDO, L1),
	nb_getval(aproblog_script_lines,SoFar),
	nb_setval(aproblog_script_lines,[L1S|SoFar]).

%%%%%%%%%%%%%%%%%%
% dnf to bdd translation using dbtrie at optimization level 0
% adapted copy of ptree's trie_to_bdd_trie
%%%%%%%%%%%%%%%%%%%
dnf_to_bdd :-
	bdd_init(FDO, PID),
	dnf_to_bdd(FDO),
	bdd_kill(FDO, PID, _S).

% taken from ptree.yap's trie_to_bdd_trie and adapted to write to online interface
dnf_to_bdd(FDO) :-
	nb_getval(aproblog_completed_proofs, Trie),
	trie_to_depth_breadth_trie(Trie, B, LL, 0), % the last one is the optimization level, LL the last definition's name  
	(ptree:is_label(LL) ->
	 tell(FDO),
	 ptree:trie_write(B, LL),
	 write(LL), nl,
	 tell(user)
	;
	 (ptree:is_state(LL) ->
	  Edges = []
	 ;
	  Edges = [LL]
	 ),
	 tell(FDO),
	 (LL = not(ID) ->
	  ptree:get_var_name(ID, NLL),
	  write('L1 = ~'), write(NLL),nl
	 ;
	  ptree:get_var_name(LL, NLL),
	  write('L1 = '), write(NLL),nl
	 ),
	 write('L1'), nl,
	 tell(user)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% core of Theo's BDD traversal with lazy evaluation, adapted to semiring operators
% this does not use caching, so don't try with larger BDDs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lazy_eval(FDO,FDI,Value) :-
	repeat,
	bdd_current(FDO, FDI, N, I, NodeId),
	(calcp(R, L) ->
	 retract(calcp(R, L)),
	 L = [CP|T],
	 (bdd_leaf(N) ->
	  CP = N,
	  NL = T
	 ;
	  CP = s(m(N,PH),m(c(N),PL)), 
	  NL = [PH,PL|T]
	 ),%write(R),nl,
	 assert(calcp(R, NL))
	;
	 R = s(m(N,PH),m(c(N),PL)),
	 NL = [PH, PL],%write(R),nl,
	 assert(calcp(R, NL))
	),
	bdd_nextDFS(FDO),
	I = 0, bdd_leaf(N),
	bdd_current(FDO, FDI, N, I, NodeId),
	!,
	calcp(FR, FL),
%	write(FR),nl,
	evaluate_expression(FR,Value),
	retract(calcp(FR, FL)).

%%%%%%%%%%%%%%%
% lazy evaluation builds a nested term that needs to be evaluated:
%%%%%%%%%%%%%%%
% attempt to catch base cases
evaluate_expression(s(m('FALSE',_),m(c('FALSE'),_)), Z) :-
	!,
	semiring_zero(Z).
evaluate_expression(s(m('TRUE',_),m(c('TRUE'),_)), Z) :-
	!,
	semiring_one(Z).
evaluate_expression(V,Z) :-
	var(V),
	!,
	format(user_error,'~n  ERROR: unresolved variable in lazy evaluation, will be assumed zero...~n         likely it is a trivial BDD, in which case the result should still be ok, but...~2n',[]),
	semiring_zero(Z).
% normal evaluation
evaluate_expression(s(A,B),C) :-
	!,
	evaluate_expression(A,AE),
	evaluate_expression(B,BE),
	semiring_addition(AE,BE,C).
evaluate_expression(m(A,B),C) :-
	!,
	evaluate_expression(A,AE),
	evaluate_expression(B,BE),
	semiring_multiplication(AE,BE,C).
evaluate_expression(c(A),C) :-
	!,
	evaluate_expression(A,V),
	label_negated(V,C).
evaluate_expression('FALSE',Z) :-
	!,
	semiring_zero(Z).
evaluate_expression('TRUE',Z) :-
	!,
	semiring_one(Z).
evaluate_expression(A,C) :-
	get_var_label(A,C,_).


%%%%%%%%%%%%%
% depth first search in BDD with result caching (dymanic predicate aproblog_cached/4 with args NodeVar, NodeID, Label, SeenVars)
% the first argument of traverse_bdd_caching/3 is a stack remembering how to combine cached results
% - FDO and FDI are the output and input communication channels for the BDD
% - it initially contains a dummy element "root" such that the empty stack indicates the end of the procedure
% - other elements are of form n(Node,High,Low), each argument consisting of VariableID-BDDNodeID (the first two args of the cache)
% key idea:
% - always record the current BDD node in the stack as a child of the current element
% - if current BDD node is cached already
%      then pop it from BDD traversal (bdd_ignoreDFS), 
%      else add it to the stack as new current element and expand it in BDD traversal (bdd_nextDFS)
% - before looking at the next node, reduce the stack
%%%%%%%%%%%%%%
eval_bdd_cached(FDO, FDI, Result,Vars ) :-
	retractall(aproblog_cached(_,_,_,_)),
	bdd_current(FDO, FDI, N, _I, NodeId),
	traverse_bdd_caching([root],FDO, FDI), % normally n(VariableID-BDDNodeID, HighChild, LowChild), but dummy "root" first
	aproblog_cached(N,NodeId,Result,Vars).

traverse_bdd_caching([],_FDO, _FDI).
traverse_bdd_caching([HeadS|RestS],FDO, FDI) :-
	bdd_current(FDO, FDI, N, _I, NodeID),
	add_child(N-NodeID,HeadS,NewHead),
	(
	 aproblog_cached(N,NodeID,_,_)
	->
	 bdd_ignoreDFS(FDO),
	 NewStack = [NewHead|RestS]
	;
	 NewStack = [n(N-NodeID,_,_),NewHead|RestS],
	 bdd_nextDFS(FDO)
	),
	reduce_stack(NewStack,RedStack),%write(NewStack),nl,write(RedStack),nl,nl,
	traverse_bdd_caching(RedStack, FDO, FDI).

% recording the current node as the next unknown child 
add_child(_Kid,root,root).
add_child(Kid,n(Node,High,Low),n(Node,Kid,Low)) :-
	var(High),!.
add_child(Kid,n(Node,High,Low),n(Node,High,Kid)) :-
	var(Low).

%%%%%%%%%%%%%%
% reducing the stack and caching the result:
% - whenever the current stack element is either a leaf or ground, the entire subtree below has been evaluated
%   and we can calculate and cache the result
% - once the first other element is reached, we know this is the parent of the next visited node
% - "root" is the dummy at the end of the stack that makes it possible to use the empty stack as stopping criterion
%%%%%%%%%%%%%
reduce_stack([root],[]).
reduce_stack([n(N-ID,_,_)|Stack],Red) :-
	bdd_leaf(N),
	!,
	cache_leaf(N,ID),
	reduce_stack(Stack,Red).
reduce_stack([n(N-ID,H,L)|Stack],Reduced) :-
	(
	 ground(n(N-ID,H,L))
	->
	 cache_inner_node(N-ID,H,L),
	 reduce_stack(Stack,Reduced) 
	;
	 Reduced = [n(N-ID,H,L)|Stack]
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% evaluation and caching of labels associated to BDD nodes
% - this takes care of keeping variables for compensation if needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% leaves
cache_leaf(Var,Node) :- 
	(
	 aproblog_cache_vars
	->
	 cache_leaf_vars(Var,Node)
	;
	 cache_leaf_pure(Var,Node)
	).

cache_leaf_pure('TRUE',ID) :-
	semiring_one(W),
	assert(aproblog_cached('TRUE',ID,W,na)).
cache_leaf_pure('FALSE',ID) :-
	semiring_zero(W),
	assert(aproblog_cached('FALSE',ID,W,na)).

cache_leaf_vars('TRUE',ID) :-
	semiring_one(W),
	assert(aproblog_cached('TRUE',ID,W,[])).
cache_leaf_vars('FALSE',ID) :-
	semiring_zero(W),
	assert(aproblog_cached('FALSE',ID,W,[])).

% for inner nodes, multiply value of children with corresponding label and sum
cache_inner_node(N,H,L) :- 
	(
	 aproblog_cache_vars
	->
	 cache_inner_node_vars(N,H,L)
	;
	 cache_inner_node_pure(N,H,L)
	).

cache_inner_node_pure(N-ID,H-HID,L-LID) :-
	aproblog_cached(H,HID,HW,_),
	aproblog_cached(L,LID,LW,_),
	get_var_label(N,W,_),
	label_negated(W,C),
	semiring_multiplication(W,HW,HighW),
	semiring_multiplication(C,LW,LowW),
	semiring_addition(HighW,LowW,Label),
	assert(aproblog_cached(N,ID,Label,na)).

cache_inner_node_vars(N-ID,H-HID,L-LID) :-
	aproblog_cached(H,HID,HW,HV),
	aproblog_cached(L,LID,LW,LV),
	get_var_label(N,W,VarID),
	label_negated(W,C),
	compensate_label(LV,HV,HW,HighW),%format(user_error,'compensated ~w ~w ~w ~w~n',[LV,HV,HW,HighW]),
	semiring_multiplication(W,HighW,HWComp),%format(user_error,'multiplied ~w ~w ~w~n',[W,HighW,HWComp]),
	compensate_label(HV,LV,LW,LowW),%format(user_error,'compensated ~w ~w ~w ~w~n',[HV,LV,LW,LowW]),
	semiring_multiplication(C,LowW,LWComp),%format(user_error,'multiplied ~w ~w ~w~n',[C,LowW,LWComp]),
	semiring_addition(HWComp,LWComp,Label),%format(user_error,'added ~w ~w ~w~n',[HWComp,LWComp,Label]),
	append([VarID|HV],LV,AllV),
	sort(AllV,SortV),%format(user_error,'cache ~w ~w ~w ~w~n',[N,ID,Label,SortV]),
	assert(aproblog_cached(N,ID,Label,SortV)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% general auxiliaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% given list of possibly negated fact identifiers (= random variables), strip off negation
get_variables([],[]).
get_variables([not(V)|Vs],[V|Others]) :-
	!,
	get_variables(Vs,Others).
get_variables([V|Vs],[V|Others]) :-
	get_variables(Vs,Others).

% for variables in the first but not the second list, we multiply by the sum of their positive and negative label
compensate_label([],_,W,W).
compensate_label([A|Rest],Vars,Acc,Result) :-
	memberchk(A,Vars),
	!,
	compensate_label(Rest,Vars,Acc,Result).
compensate_label([A|Rest],Vars,Acc,Result) :-
	get_fact_label(A,W),
	label_negated(W,WW),
	semiring_addition(W,WW,CA),
	semiring_multiplication(CA,Acc,Next),
	compensate_label(Rest,Vars,Next,Result).

% transform a list of possibly negated fact identifiers into the corresponding list of (negated) BDD variables
ids_to_vars([],[]).
ids_to_vars([not(A)|B],[C|D]) :-
	!,
	atomic_concat(['~x',A],C),
	ids_to_vars(B,D).
ids_to_vars([A|B],[C|D]) :-
	atomic_concat(['x',A],C),
	ids_to_vars(B,D).

% given a BDD variable, get the associated label and ID
% for ground facts, return just the ID (without quotes - breaks compensation for unseen variables on BDD else!)
% for non-ground facts, return the ID including the grounding ID
get_var_label(XID,Label,VariableName) :-
 	atom_concat(x,IAtom,XID),
 	get_fact_from_id(IAtom,NumAtom),
 	atom_number(NumAtom,FactID),
 	get_fact_label(FactID,Label),
	(
	 IAtom == NumAtom
	->
	 VariableName = FactID
	;
	 VariableName = IAtom
	).

% for nonground facts, extract fact id
get_fact_from_id(IAtom,NumAtom) :-
	atom_concat(NumAtom,Part2,IAtom),
	atom_concat('_',_GID,Part2),!.
get_fact_from_id(I,I).

conditional_format(_String,_Args) :-
	aproblog_flag(verbose,false),!.
conditional_format(String,Args) :-
	format(String,Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user needs to provide these five predicates as part of the aproblog program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

semiring_zero(Z) :-
	user:semiring_zero(Z).
semiring_one(Z) :-
	user:semiring_one(Z).
semiring_addition(OldLabel,Label,NewLabel) :-
	user:semiring_addition(OldLabel,Label,NewLabel).
semiring_multiplication(OldLabel,Label,NewLabel) :-
	user:semiring_multiplication(OldLabel,Label,NewLabel).
label_negated(W,Wbar) :-
	user:label_negated(W,Wbar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% top level predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% choose automatically based on flags (default: both false)
aproblog_label(Query,Label) :-
	aproblog_flag(disjoint_sum,true),
	aproblog_flag(neutral_sum,true),
	label_neutral_disjoint(Query,Label).
aproblog_label(Query,Label) :-
	aproblog_flag(disjoint_sum,true),
	aproblog_flag(neutral_sum,false),
	label_disjoint(Query,Label).
aproblog_label(Query,Label) :-
	aproblog_flag(disjoint_sum,false),
	aproblog_flag(neutral_sum,true),
	label_neutral(Query,Label).
aproblog_label(Query,Label) :-
	aproblog_flag(disjoint_sum,false),
	aproblog_flag(neutral_sum,false),
	label(Query,Label).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query label: if sums are neutral and disjoint, calculate the label on the fly
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% for those not remembering the order :)
label_disjoint_neutral(Query,Label) :-
	label_neutral_disjoint(Query,Label).

label_neutral_disjoint(Query,Label) :-
	conditional_format('disjoint and neutral~n',[]),
	statistics(walltime,[S,_]),
	direct_eval(Query,Label),
	statistics(walltime,[D,_]),
	Time is D - S,
	conditional_format('time to calculate label: ~w~n',[Time]).

direct_eval(Goal,_) :-
	init_aproblog,
	semiring_zero(Zero),
	nb_setval(aproblog_label, Zero),
	aproblog_call(Goal),
	add_solution_to_eval,
	fail.
direct_eval(_,Label) :-
	b_getval(aproblog_label, Label).

add_solution_to_eval :-
	b_getval(aproblog_current_proof, IDs),
	update_label(IDs).

% old version: evaluate DNF as is
label_neutral_disjoint_on_dnf(Query,Label) :-
	conditional_format('disjoint and neutral~n',[]),
	statistics(walltime,[S,_]),
	build_dnf(Query),
	statistics(walltime,[D,_]),
	BT is D - S,
	conditional_format('time to build DNF: ~w~n',[BT]),
	evaluate_dnf(Label),
	statistics(walltime,[W,_]),
	WT is W - D,
	conditional_format('time to calculate label: ~w~n',[WT]),
	delete_dnf.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% query label: if sums are disjoint but not neutral, calculate the label on the fly with compensation; 
% compensation ignores labeled facts not used in any proof of the query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
label_disjoint(Query,Label) :-
	conditional_format('disjoint but not neutral~n',[]),
	statistics(walltime,[S,_]),
	direct_eval_with_compensation(Query,Label),
	statistics(walltime,[D,_]),
	T is D - S,
	conditional_format('time to calculate label: ~w~n',[T]).

direct_eval_with_compensation(Goal,_) :-
	init_aproblog,
	nb_setval(aproblog_variables, []),
	semiring_zero(Zero),
	nb_setval(aproblog_label, Zero),
	aproblog_call(Goal),
	add_solution_to_eval_with_compensation,
	fail.
direct_eval_with_compensation(_,Label) :-
	b_getval(aproblog_label, LabelOnUsed),
	(
	 aproblog_flag(compensate_unused, true)
	->
	 b_getval(aproblog_variables, UsedVars),
	 compensate_for_unseen_vars(LabelOnUsed, UsedVars, Label)
	;
	 Label = LabelOnUsed
	).

add_solution_to_eval_with_compensation :-
	b_getval(aproblog_current_proof, IDs),
	update_label_with_compensation(IDs).

compensate_for_unseen_vars(LabelOnUsed, UsedVars, Label) :-
	findall(ID,(labeled_fact(_,_,ID),\+non_ground_fact(ID)),AllVars),
	compensate_label(AllVars,UsedVars,LabelOnUsed,Label),
	(
	 non_ground_fact(SomeId)
	->
	 get_fact(SomeId,SomeIdFact),
	 SomeIdLabel::SomeIdFact,
	 format(user_error,'~2nERROR: cannot fully compensate in program with non-ground facts such as ~q::~q!~nResult with respect to used and ground facts is ~q~2n',[SomeIdLabel,SomeIdFact,Label]),
	 throw(error('tried compensation on non-ground facts'))
	;
	 true
	).

% old version: evaluate DNF with compensation
label_disjoint_on_dnf(Query,Label) :-
	conditional_format('disjoint but not neutral~n',[]),
	statistics(walltime,[S,_]),
	build_dnf(Query),
	statistics(walltime,[D,_]),
	BT is D - S,
	conditional_format('time to build DNF: ~w~n',[BT]),
	evaluate_dnf_with_compensation(Label),
	statistics(walltime,[W,_]),
	WT is W - D,
	conditional_format('time to calculate label: ~w~n',[WT]),
	delete_dnf.

% variant that always compensates for all DNF variables
% intended for debugging purposes, does some redundant list operations in reused code
label_disjoint_naive(Query,Label) :-
	statistics(walltime,[S,_]),
	build_dnf(Query),
	statistics(walltime,[D,_]),
	BT is D - S,
	conditional_format('time to build DNF: ~w~n',[BT]),
	evaluate_dnf_with_compensation_naive(Label),
	statistics(walltime,[W,_]),
	WT is W - D,
	conditional_format('time to calculate label: ~w~n',[WT]),
	delete_dnf.

%%%%%%%%%%%%%%%
% query label: if sums are neutral but not disjoint, evaluate the BDD; 
% using depth first search with caching
%%%%%%%%%%%%%%
label_neutral(Query,Result) :-
	conditional_format('not disjoint but neutral~n',[]),
	retractall(aproblog_cache_vars),   % do not cache variables for compensation
	label_internal(Query,Result).

% variant using lazy evaluation without caching
label_lazy(Query,Label) :-
	statistics(walltime,[S,_]),
	build_dnf(Query),
	statistics(walltime,[D,_]),
	BT is D - S,
	conditional_format('time to build DNF: ~w~n',[BT]),
	bdd_init(FDO,FDI, PID),
	dnf_to_bdd(FDO),  % change to dnf_to_bdd_naive to use naive preprocessing
	statistics(walltime,[B,_]),
	BBT is B - D,
	conditional_format('time to build BDD: ~w~n',[BBT]),
	lazy_eval(FDO,FDI,Label),
	statistics(walltime,[EB,_]),
	EBT is EB - B,
	conditional_format('time to lazily calculate label on BDD: ~w~n',[EBT]),
	bdd_kill(FDO,FDI, PID, _),
	delete_dnf.


%%%%%%%%%%%%%%%
% query label: if sums are neither neutral nor disjoint, evaluate the BDD with compensation; 
% using depth first search with caching
% ignores labeled facts not used in any proof of the query
%%%%%%%%%%%%%%
label(Query,Result) :-
	conditional_format('neither disjoint nor neutral~n',[]),
	retractall(aproblog_cache_vars),
	assert(aproblog_cache_vars),      % cache variables for compensation
	label_internal(Query,Result).

% shared skeleton of bdd-based methods label_neutral/2 and label/2, controlled by dynamic predicate aproblog_cache_vars/0
% 1. collect explanations in DNF
% 2. feed DNF to BDD tool
% 3. evaluate BDD with caching
label_internal(Query,Label) :-	
	statistics(walltime,[S,_]),
	build_dnf(Query),
	statistics(walltime,[D,_]),
	BT is D - S,
	conditional_format('time to build DNF: ~w~n',[BT]),
	bdd_init(FDO,FDI, PID),
	dnf_to_bdd(FDO),      % change to dnf_to_bdd_naive to use naive preprocessing
	statistics(walltime,[B,_]),
	BBT is B - D,
	conditional_format('time to build BDD: ~w~n',[BBT]),
	eval_bdd_cached(FDO, FDI, LabelOnUsed, UsedVars ),
	bdd_kill(FDO,FDI, PID, _), % clean up first, as compensate_for_unseen_vars throws error for non-ground facts
	retractall(aproblog_cached(_,_,_,_)),
	delete_dnf,
	(
	 (aproblog_flag(compensate_unused, true), aproblog_cache_vars) % only compensate if we're in the general case, not for neutral sums...
	-> 
	 compensate_for_unseen_vars(LabelOnUsed, UsedVars, Label)
	;
	 Label = LabelOnUsed
	),	
	statistics(walltime,[EB,_]),
	EBT is EB - B,
	conditional_format('time to calculate label on BDD: ~w~n',[EBT]).

%%%%%%%%%%%%%%%%%%%%%%
% structural output only
%%%%%%%%%%%%%%%%%%%%%%
% DNF
print_dnf(Query) :-
	build_dnf(Query),
	print_dnf,
	delete_dnf.
% BDD
print_bdd(Query) :-
	build_dnf(Query),
	dnf_to_bdd,  % change to dnf_to_bdd_naive to use naive preprocessing
	delete_dnf.

% random variables / facts used
used_vars(Query,Vars) :-
	build_dnf(Query),
	nb_getval(aproblog_completed_proofs, Trie),
	edges_ptree(Trie,Vars),
	delete_dnf.
used_facts(Query,Facts) :-
	used_vars(Query,Vars),
	get_fact_list(Vars,Facts).
	

%%%%%%%%%%%%%%
% testing predicates
%%%%%%%%%%%%
% call all labeling functions
test(Query) :-
	label_neutral_disjoint(Query,LND),
	format('~nResult: ~q~2n',[LND]),
	label_disjoint(Query,LD),
	format('~nResult: ~q~2n',[LD]),
	label_neutral(Query,LN),
	format('~nResult: ~q~2n',[LN]),
	label(Query,L),
	format('~nResult: ~q~2n',[L]).

% this works on internal predicates on DNF, which aren't used any more directly
test_inner(Query) :-
	statistics(walltime,[S,_]),
	build_dnf(Query),
	statistics(walltime,[DNF,_]),
	DNFTime is DNF - S,
	format('time to build DNF: ~w~n',[DNFTime]),
	evaluate_dnf(WX),
	statistics(walltime,[WXT,_]),
	DNFEvalTime is WXT - DNF,
	format('time to calculate label on DNF: ~w~2nResult: ~w~2n',[DNFEvalTime,WX]),
	statistics(walltime,[StartComp,_]),
	evaluate_dnf_with_compensation(DNFwithComp),
	statistics(walltime,[EndComp,_]),
	Diffwc is EndComp-StartComp,
	format('time to calculate label on DNF with compensation: ~w~2nResult: ~w~2n',[Diffwc,DNFwithComp]),
	bdd_init(FDO,FDI, PID),
	dnf_to_bdd(FDO),  % change to dnf_to_bdd_naive to use naive preprocessing
	statistics(walltime,[BDD,_]),
	BddBuild is BDD - EndComp,
	format('time to build BDD: ~w~n',[BddBuild]),
	retractall(aproblog_cache_vars),
	eval_bdd_cached(FDO, FDI, WS, _Vars ),
	statistics(walltime,[TWS,_]),
	BddTimeWS is TWS - BDD,
	format('time to calculate label on BDD: ~w~2nResult: ~w~2n',[BddTimeWS,WS]),
	bdd_reset(FDO),
	assert(aproblog_cache_vars),
	eval_bdd_cached(FDO, FDI, WS2, _ ),
	statistics(walltime,[TWS2,_]),
	BddTimeWS2 is TWS2 - TWS,
	format('time to calculate label on BDD with compensation: ~w~2nResult: ~w~2n',[BddTimeWS2,WS2]),
	bdd_kill(FDO,FDI, PID, _),
	delete_dnf.


%%%%%%%%%%%%%% trial area %%%%%%%%%%

