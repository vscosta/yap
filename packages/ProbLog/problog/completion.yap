%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-12-05 14:07:19 +0100 (Mon, 05 Dec 2011) $
%  $Revision: 6766 $
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


:- module(completion, [propagate_evidence/2,
		       bdd_cluster/2,
		       split_atom_name/3,
		       reset_completion/0]).

:- style_check(all).
:- yap_flag(unknown,error).

% load library modules
:- use_module(library(lists),[member/2,append/3,reverse/2]).
:- use_module(library(system), [tmpnam/1]).

% load our own modules
:- use_module('../problog').
:- use_module(grounder).
:- use_module(logger).
:- use_module(termhandling).
:- use_module(flags).
:- use_module(print_learning).
:- use_module(utils).
:- use_module(utils_learning).

:- dynamic seen_atom/4.
:- dynamic bdd_cluster/2.

:- initialization(problog_define_flag(propagate_known,problog_flag_validate_boolean,'Propagate known atoms',true,learning_bdd_generation)).
:- initialization(problog_define_flag(propagate_det,problog_flag_validate_boolean,'Propagate deterministic atoms',true,learning_bdd_generation)).
:- initialization(problog_define_flag(output_dot_files,problog_flag_validate_boolean,'Output .dot files for BDD scripts',true,learning_bdd_generation)).
:- initialization(problog_define_flag(split_bdds,problog_flag_validate_boolean,'Split BDD scripts when possible',true,learning_bdd_generation)).


%========================================================================
%=
%========================================================================

reset_completion :-
	retractall(seen_atom(_,_,_,_)),
	retractall(bdd_cluster(_,_)).

%========================================================================
%=
%========================================================================

propagate_evidence(_,_) :-
	\+ current_predicate(user:known/3),
	!,
	throw(error(system,'The predicate user:known/3 is not defined. If you really have empty interpretations declare the user:known/3 as dynamic and come back.')).


propagate_evidence(InterpretationID,Query_Type) :-
	atomic(InterpretationID),
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Clean up                                     %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	eraseall(rules),
	eraseall(unpropagated_rules),
	eraseall(known_atoms),
	grounder_reset,

	(
	 Query_Type==test
	->
	 (
	  Key_BDD_script_generation=test_bdd_script_generation,
	  Key_BDD_script_generation_grounding=test_bdd_script_generation_grounding,
	  Key_BDD_script_generation_completion=test_bdd_script_generation_completion,
	  Key_BDD_script_generation_propagation=test_bdd_script_generation_propagation,
	  Key_BDD_script_generation_splitting=test_bdd_script_generation_splitting,
	  Key_BDD_script_generation_active_ground_atoms=test_bdd_script_generation_active_ground_atoms,
	  Key_BDD_script_generation_propagated_ground_atoms=test_bdd_script_generation_propagated_ground_atoms
	 );
	 (
	  Key_BDD_script_generation=train_bdd_script_generation,
	  Key_BDD_script_generation_grounding=train_bdd_script_generation_grounding,
	  Key_BDD_script_generation_completion=train_bdd_script_generation_completion,
	  Key_BDD_script_generation_propagation=train_bdd_script_generation_propagation,
	  Key_BDD_script_generation_splitting=train_bdd_script_generation_splitting,
	  Key_BDD_script_generation_active_ground_atoms=train_bdd_script_generation_active_ground_atoms,
	  Key_BDD_script_generation_propagated_ground_atoms=train_bdd_script_generation_propagated_ground_atoms
	 )
	),
	  

	logger_start_timer(Key_BDD_script_generation),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Calc dep()                                   %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	logger_start_timer(Key_BDD_script_generation_grounding),
	format_learning(5,'d',[]),
        % iterate over all evidence atoms
	forall(user:known(InterpretationID,Atom,Value),
	       (
		catch( grounder_compute_reachable_atoms(Atom,InterpretationID,Success), _, fail),
		(
		    (Success==true; Value==false)
		->
		    true
		;
		    throw(unprovable_evidence(Atom))
		)
	       )
	      ),
	logger_stop_timer(Key_BDD_script_generation_grounding),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Calc completion                              %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	logger_start_timer(Key_BDD_script_generation_completion),
	format_learning(5,'c',[]),
	once(completion(InterpretationID)),
	logger_stop_timer(Key_BDD_script_generation_completion),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Bring out intermediate garbage               %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        grounder_reset,
	!,
	garbage_collect_atoms,


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Calc propagation                             %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	problog_flag(propagate_known,Propagate_Known),

	(
	 Propagate_Known==true
	->
	 (
	  logger_start_timer(Key_BDD_script_generation_propagation),
	  format_learning(5,'p',[]),
	  once(propagate),
	  logger_stop_timer(Key_BDD_script_generation_propagation)
	 );
	 true
	),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Split BDD Script                             %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	problog_flag(split_bdds,Split_BDDs),
	format_learning(5,'S',[]),
	(
	 Split_BDDs==false
	->
	 (
	  findall(R,(recorded(rules,_,R);recorded(unpropagated_rules,_,R)),All_R),
	  Cluster=[All_R]
	 );
	 (
	  logger_start_timer(Key_BDD_script_generation_splitting),
	  split_rules(Cluster),
	  logger_stop_timer(Key_BDD_script_generation_splitting)
	 )
	),



	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Print BDD script                             %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	format_learning(5,'s',[]),
	print_script_per_cluster(Cluster,InterpretationID,1,0,Seen_Atoms,[],ClusterIDs),
	store_known_atoms(InterpretationID,ClusterIDs,Query_Type),
	key_statistics(known_atoms,Known_Atoms,_),
	logger_add_to_variable(Key_BDD_script_generation_active_ground_atoms,Seen_Atoms),
	logger_add_to_variable(Key_BDD_script_generation_propagated_ground_atoms,Known_Atoms),


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Clean up                                     %%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	eraseall(rules),
	eraseall(unpropagated_rules),
	eraseall(known_atoms),
	grounder_reset,
	logger_stop_timer(Key_BDD_script_generation).


%========================================================================
%= 
%========================================================================

print_script_per_cluster([],_,_,Seen_Atoms,Seen_Atoms,Cluster_IDs,Cluster_IDs).
print_script_per_cluster([Refs|T],InterpretationID,Cluster_ID,Old_Seen_Atoms,Seen_Atoms,Old_Cluster_IDs,Cluster_IDs) :-
	create_bdd_file_name(InterpretationID,Cluster_ID,File_Name),
	once(print_simplecudd_script(Refs,File_Name,This_Seen_Atoms)),
	New_Seen_Atoms is Old_Seen_Atoms+This_Seen_Atoms,
	Next_Cluster_ID is Cluster_ID+1,
	print_script_per_cluster(T,InterpretationID,Next_Cluster_ID,New_Seen_Atoms,Seen_Atoms,[Cluster_ID|Old_Cluster_IDs],Cluster_IDs).


%========================================================================
%= 
%========================================================================

completion(InterpretationID) :-
	% iterate over all reachable atoms where the completion
	% can be computed. This will skip reachable probabilistic facts.
	forall((
	        grounder_reachable_atom(Head),
		grounder_completion_for_atom(Head,InterpretationID,Rule)
	       ),
	       (
		once(propagate_interpretation(Rule,InterpretationID,Rule2)),
		simplify(Rule2,Rule3,_),
		(
		 (Rule3\==false,record_constraint_cs_check(Rule3))
		->
		 true;
		 (
		  print_theory,
		  format(user_error,'=============================~n',[]),
		  format(user_error,'Inconsistency error at building completion for atom ~q (Example ~q)~n',[Head,InterpretationID]),
		  format(user_error,'  Completion was~n    ~q~2n',[Rule]),
		  format(user_error,'  After subsituting evidence~n    ~q~2n',[Rule2]),
		  format(user_error,'  After simplifying~n    ~q~2n',[Rule3]),
		  format(user_error,'=============================~2n',[]),
		  throw(theory_is_inconsistent)
		 )
		)
	       )
	      ),

%	print_theory,

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Store known Atoms %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        forall(user:known(InterpretationID,Atom,Value),
	       recorda(known_atoms,'$atom'(Atom) <=> Value,_)
	      ).

	

%========================================================================
%= find rule which makes sense to propagate
%========================================================================

propagate :-
	problog_flag(propagate_det,true),
	!,
	repeat,
	once(propagate_intern_known(Result1)),
%	print_theory,
	Result1==false,
	once(propagate_intern_deterministic(Result2)),
	Result2==false,
	!.
propagate :-
	repeat,
	once(propagate_intern_known(Result1)),
	Result1==false,
	!.

propagate_intern_known(true) :-
	recorded(unpropagated_rules,Atom <=> AtomValue,Key1),
	!,
	erase(Key1),
	recorda(known_atoms,Atom <=> AtomValue,_),
	forall(
	       (
		recorded(rules,Rule,Key2),
		once(propagate(Rule,Atom,AtomValue,NewRule,true)) % will succeed only when Atom appears in Rule
	       ),
	       (
		erase(Key2),
		once(simplify(NewRule,NewRuleSimplified,_)),
		(
		 (NewRuleSimplified\==false,record_constraint_cs_check(NewRuleSimplified))
		->
		 true;
		 (
		  print_theory,
		  format(user_error,'Propagating ~q=~q for ~q leads to an inconsistency.!!!~2n',[Atom,AtomValue,Rule]),
		  throw(inconsitent)
		 )
		)
	       )
	      ).
propagate_intern_known(false).

propagate_intern_deterministic(true) :-
	recorded(rules,Atom <=> AtomValue,Key1),
	!,
	erase(Key1),
	forall(
	       (
		recorded(rules,Rule,Key2),
		once(propagate(Rule,Atom,AtomValue,NewRule,true)) % will succeed only when Atom appears in Rule
	       ),
	       (
		erase(Key2),
		once(simplify(NewRule,NewRuleSimplified,_)),
		(
		 (NewRuleSimplified\=false,record_constraint_cs_check(NewRuleSimplified))
		->
		 true;
		 (
		  print_theory,
		  format(user_error,'Propagating ~q=~q for ~q leads to an inconsistency.!!!~2n',[Atom,AtomValue,Rule]),
		  throw(inconsitent)
		 )
		)
	       )
	      ).
propagate_intern_deterministic(false).


%========================================================================
%= 
%========================================================================

record_constraint_cs_check( (X <=> Y) ) :-
	recorda(rules,(X <=> Y),_).
record_constraint_cs_check((X,Y)) :-
	record_constraint_cs_check(X),
	record_constraint_cs_check(Y).
record_constraint_cs_check( (X;Y)) :-
	recorda(rules,(X;Y),_).
record_constraint_cs_check( \+ '$atom'(X) ) :-
	(
	 recorded(unpropagated_rules, ('$atom'(X)<=>OldValue),_)
	->
	 OldValue==false;
	 recorda(unpropagated_rules, ('$atom'(X) <=> false),_)
	).
record_constraint_cs_check('$atom'(X)) :-
	(
	 recorded(unpropagated_rules, ('$atom'(X)<=>OldValue),_)
	->
	 OldValue==true;
	 recorda(unpropagated_rules, ('$atom'(X) <=> true),_)
	).
record_constraint_cs_check(true).

%========================================================================
%= 
%========================================================================


split_atom_name(Name,ID,GroundID) :-
	atom(Name),
	atomic_concat(x,Temp,Name),
	atom_codes(Temp,TempC),

	(
	 append(Head,[95|Tail],TempC) % 95-_-
	->
	 (
	  number_chars(ID,Head),
	  number_chars(GroundID,Tail)
	 );
	 (
	  number_chars(ID,TempC),
	  GroundID=0
	 )
	),
	!.

store_known_atoms(ID,ClusterIDs,Query_Type) :-
	(
	 Query_Type==test
	->
	 (
	  KK_True_Array=known_count_true_test,
	  KK_False_Array=known_count_false_test
	 );
	 (
	  KK_True_Array=known_count_true_training,
	  KK_False_Array=known_count_false_training
	 )
	),
	 
	retractall(bdd_cluster(ID,_)),

	assertz(bdd_cluster(ID,ClusterIDs)),
	create_known_values_file_name(ID,File_Name),
	open(File_Name,'write',Handle),
	format(Handle,'completion:bdd_cluster(~w,~w).~n',[ID,ClusterIDs]),
	
	forall((
		recorded(known_atoms,'$atom'(Atom) <=> Value,_),
		remember(Atom,Name),
		split_atom_name(Name,FactID,GroundID)
	       ),
	       (
		(
		 Value==true
		->
		 add_to_array_element(KK_True_Array,FactID,1,_);
		 add_to_array_element(KK_False_Array,FactID,1,_)
		),
		know_atom_expected_count(Value,Count),
		format(Handle,'completion:known_count(~w,~w,~w,~w). % ~w~n',[ID,FactID,GroundID,Count,Atom])
	       )
	      ),

	close(Handle).

know_atom_expected_count(true,1).
know_atom_expected_count(false,0).


%========================================================================
%= 
%========================================================================

print_theory :-
	format_learning(5,'~n  Current Theory~n  == Unpropagated Rules ==~n',[]),
	forall(recorded(unpropagated_rules,Rule,Key),
	       format_learning(5,'   ~q.  (~q)~n',[Rule,Key])
	      ),
	
	format_learning(5,'  == Rules ==~n',[]),
	forall(recorded(rules,Rule,Key),
	       format_learning(5,'   ~q.  (~q)~n',[Rule,Key])),

	format_learning(5,'  == Known and Propagated Atoms ==~n',[]),
	forall(recorded(known_atoms,Head <=> Bodies,Key),
	       format_learning(5,'   ~q <=> ~q.  (~q)~n',[Head,Bodies,Key])
	      ),

	format_learning(5,'~3n',[]).


%========================================================================
%= split_rules(-Cluster)
%========================================================================
split_rules(Cluster) :-
	eraseall(cluster),

        % add all rules to the clusters
	forall(recorded(rules,Expression,Reference),
	       include_in_clusters(Expression,Reference)),

	% add all unpropagated rules to the clusters
	forall(recorded(unpropagated_rules,Expression,Reference),
	       include_in_clusters(Expression,Reference)),

	garbage_collect_atoms,

	% Merge clusters until
	% no more clusters can be merged
	(
	 repeat,
	 merge_cluster(Result),
	 Result==false,
	 !
	),
	
	findall(Keys,recorded(cluster,c(_Facts,Keys),_),Cluster),
	eraseall(cluster),

	garbage_collect_atoms.

%========================================================================
%= include_in_clusters(+Expression,+Reference)
%========================================================================

include_in_clusters(Expression,Reference) :-
	(
	 setof(F, Expression^term_element(Expression, F), Facts_Sorted)
	->
	 true;
	 Facts_Sorted = []
	),
	
	bb_put(facts,Facts_Sorted),
	bb_put(rule_keys,[Reference]),
	
	% iterate over all cluster that overlap with Current_Facts
	forall((
		recorded(cluster,c(CFacts,Cluster_Rule_Keys),CKey),
		bb_get(facts,Current_Facts),
		sorted_overlap_test(Current_Facts,CFacts)
	       ),
	       (
		erase(CKey),
		bb_get(rule_keys,Current_Rule_Keys),
		append(Current_Facts,CFacts,Merged_Facts),
		append(Current_Rule_Keys,Cluster_Rule_Keys,Merged_Rule_Keys),
		sort(Merged_Facts,Sorted_Facts),
		bb_put(facts,Sorted_Facts),
		bb_put(rule_keys,Merged_Rule_Keys)
	       )
	      ),

	%clean up and store the new (possibly merged) cluster
	bb_delete(facts,Final_Facts),
	bb_delete(rule_keys,Final_Rule_Keys),
	recorda(cluster,c(Final_Facts,Final_Rule_Keys),_).

%========================================================================
%= find two clusters that should be merged because they both
%= contain the same fact
%========================================================================

merge_cluster(true) :-
	recorded(cluster,c(CFacts1,Cluster_Rule_Keys1),CKey1),
	recorded(cluster,c(CFacts2,Cluster_Rule_Keys2),CKey2),
	CKey1 @< CKey2,
	sorted_overlap_test(CFacts1,CFacts2),
	!,
	erase(CKey1),
	erase(CKey2),
	
	append(CFacts1,CFacts2,Merged_Facts),
	sort(Merged_Facts,Sorted_Facts),

	append(Cluster_Rule_Keys1,Cluster_Rule_Keys2,Merged_Rule_Keys),	 
	recorda(cluster,c(Sorted_Facts,Merged_Rule_Keys),_).
merge_cluster(false).

%========================================================================
%= 
%========================================================================

print_simplecudd_script(Refs,BDDFilename,Seen_Atoms) :-
	retractall(seen_atom(_,_,_,_)),
	retractall(script_hash(_,_)),

	bb_put(counter,0),
	bb_put(det_counter,0),
	bb_put(grounding_counter,0),

	tmpnam(Temp_File_Name),
	open(Temp_File_Name,'write',Handle1),
	findall(X,(
		   member(R,Refs),
		   recorded(_,Expression,R),
		   print_expression(Expression,Handle1,X)
		  ),L),
	reverse(L,L_Rev),
	list_to_conjunction(L_Rev,Con),


	print_expression_and_final(Con,Handle1,'',Final),

	(
	 (atom_codes(Final,[76|_]))  % X='L....'
	->
	 LastID=Final;
	 (
	  next_counter(LastID),
	  format(Handle1,'~w=~w~n',[LastID,Final])
	 )
	),
	
	format(Handle1,'~w~n',[LastID]),
	close(Handle1),
	
	succeeds_n_times(seen_atom(_,_,_,_),Seen_Atoms),
	bb_get(counter,IntermediateSteps),

	prefix_bdd_file_with_header(BDDFilename,Seen_Atoms,IntermediateSteps,Temp_File_Name),

	problog_flag(output_dot_files,Output_Dot_Files),

	(
	 Output_Dot_Files==true
	->
	 (
	  atomic_concat([BDDFilename,'.dot'],Dot_File_Name),
	  open(Dot_File_Name,'write',Handle2),
	  format(Handle2,'digraph d{~n',[]),
	  
	  forall(seen_atom(Atom,ID,_FactID,_),
		 format(Handle2,'~q [label="~q\\n~q", style="filled", color="lightblue"];~n',[ID,Atom,ID])
		),

	  findall(X,(member(R,Refs),recorded(_,Expression,R),print_dot_expression(Expression,Handle2,X)),_L2),

	  % switch off printing final line until bugfix
	  %list_to_conjunction(L2,Con2),
%	  print_dot_expression(Con2,Handle2,_),
	  format(Handle2,'}~n',[]),
	  close(Handle2)
	 );
	 true
	),

	retractall(script_hash(_,_)),
	retractall(seen_atom(_,_,_,_)).


%========================================================================
%= 
%========================================================================

print_expression(Term,_Handle,N) :-
	script_hash(Term,N),
	!.

print_expression(X <=> Y, Handle,N3) :-
	print_expression(X,Handle,N1),
	print_expression(Y,Handle,N2),
	next_counter(N3),
	assert(script_hash(X <=> Y, N3)),
	format(Handle,'~w = ~w ~~# ~w~n',[N3,N1,N2]).
print_expression( (X,Y), Handle,Number) :-
	print_expression_and((X,Y),Handle,'',Number),
	assert(script_hash((X,Y), Number)).
print_expression( (X;Y), Handle,Number) :-
	print_expression_or((X;Y),Handle,'',Number),
	assert(script_hash((X;Y), Number)).
print_expression( \+ '$atom'(X), _Handle,ID) :-
	remember(X,Name),
	atomic_concat(['~',Name],ID).
print_expression( true, _Handle,'TRUE').
print_expression( false, _Handle,'FALSE').
print_expression('$atom'(X), _Handle,ID) :-
	remember(X,ID).

print_expression_or((X;Y), Handle,OldAcc,Number) :-
	!,
	print_expression(X,Handle,NX),
	atomic_concat([OldAcc,NX,' + '],NewAcc),
	print_expression_or(Y,Handle,NewAcc,Number).
print_expression_or(X, Handle,OldAcc,Number) :-
	print_expression(X,Handle,NX),
	next_counter(Number),
	format(Handle,'~w = ~w~w~n',[Number,OldAcc,NX]).


print_expression_and((X,Y), Handle,OldAcc,Number) :-
	!,
	print_expression(X,Handle,NX),
	atomic_concat([OldAcc,NX,' * '],NewAcc),
	print_expression_and(Y,Handle,NewAcc,Number).
print_expression_and(X, Handle,OldAcc,Number) :-
	print_expression(X,Handle,NX),
	next_counter(Number),
	format(Handle,'~w = ~w~w~n',[Number,OldAcc,NX]).


print_expression_and_final((X,Y), Handle,OldAcc,Number) :-
	!,
	atomic_concat([OldAcc,X,' * '],NewAcc),
	print_expression_and_final(Y,Handle,NewAcc,Number).
print_expression_and_final( true, _Handle,_ACC,'TRUE').
print_expression_and_final(X, Handle,OldAcc,Number) :-
	next_counter(Number),
	format(Handle,'~w = ~w~w~n',[Number,OldAcc,X]).


%========================================================================
%= 
%========================================================================

print_dot_expression_or((X;Y), Handle,Number) :-
	!,
	print_dot_expression(X,Handle,NX),
	print_dot_line(NX,Number,Handle),
	print_dot_expression_or(Y,Handle,Number).
print_dot_expression_or(X, Handle,Number) :-
	print_dot_expression(X,Handle,NX),
	print_dot_line(NX,Number,Handle).


print_dot_expression_and((X,Y), Handle,Number) :-
	!,
	print_dot_expression(X,Handle,NX),
	print_dot_line(NX,Number,Handle),
	print_dot_expression_and(Y,Handle,Number).
print_dot_expression_and(X, Handle,Number) :-
	print_dot_expression(X,Handle,NX),
	print_dot_line(NX,Number,Handle).




print_dot_expression(X <=> Y, Handle,N3) :-
	print_dot_expression(X,Handle,N1),
	print_dot_expression(Y,Handle,N2),
	next_counter(N3),
	format(Handle,'~w [label="<=>",shape="diamond", style="filled", color="lightsalmon"];~n',[N3]),
	print_dot_line(N1,N3,Handle),
	print_dot_line(N2,N3,Handle).
print_dot_expression( (X,Y), Handle,Number) :-
	next_counter(Number),
	format(Handle,'~w [label="^",shape="triangle", style="filled", color="lightgoldenrod"];~n',[Number]),
	print_dot_expression_and((X,Y),Handle,Number).
print_dot_expression( (X;Y), Handle,Number) :-
	next_counter(Number),
	format(Handle,'~w [label="v",shape="invtriangle", style="filled", color="greenyellow"];~n',[Number]),
	print_dot_expression_or((X;Y),Handle,Number).
print_dot_expression( \+ '$atom'(X), _Handle,ID) :-
	remember(X,Name),
	atomic_concat(['~',Name],ID).
print_dot_expression(true, _Handle,'TRUE').
print_dot_expression( false, _Handle,'FALSE').
print_dot_expression( '$atom'(X), _Handle,ID) :-
	remember(X,ID).


print_dot_line(N1,N2,Handle) :-
	(
	 atomic_concat('~',ID,N1)
	->
	 format(Handle,'~w -> ~w [style="dashed, bold"];~n',[ID,N2]);
	 format(Handle,'~w -> ~w;~n',[N1,N2])
	).

%========================================================================
%= 
%========================================================================


remember(X,Name) :-
	seen_atom(X,Name,_,_),
	!.
remember(X,X) :-
	atom(X),
	atom_codes(X,[76|_]),  % X='L....'
	!.
remember(X,Name) :-
	probabilistic_fact(P,X,ID),
	!,
	(
	 non_ground_fact(ID)
	->
	 (
	  next_grounding_id(Grounding_ID),
	  atomic_concat([x,ID,'_',Grounding_ID],Name)
	 );
	 atomic_concat([x,ID],Name)
	),
	assertz(seen_atom(X,Name,ID,P)).
remember(X,Name) :-
	next_det_counter(Det_ID),
	atomic_concat([y,Det_ID],Name),
	assertz(seen_atom(X,Name,det,1.0)).


next_grounding_id(N) :-
	bb_get(grounding_counter,N),
	N2 is N+1,
	bb_put(grounding_counter,N2).

next_det_counter(ID) :-
	bb_get(det_counter,N),
	N2 is N+1,
	atomic_concat(['y',N2],ID),
	bb_put(det_counter,N2).

next_counter(ID) :-
	bb_get(counter,N),
	N2 is N+1,
	atomic_concat(['L',N2],ID),
	bb_put(counter,N2).


