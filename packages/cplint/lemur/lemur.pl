/***************************************************************************************************
  MCLPADS
	http://www.di.uniba.it/~ndm/mclpads/

	Copyright (c) 2013 University of Bari "Aldo Moro"
  Author: Nicola Di Mauro                                                 

	**************************************************************************************************
	
	This code is part of the SLIPCOVER code https://sites.google.com/a/unife.it/ml/slipcover
	Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi
	Parts of this code are thaken from the SLIPCOVER source code

	***************************************************************************************************

  The MCLPADS Software is made available under the terms and conditions of the Artistic License 2.0.
	LICENSEE shall acknowledge University of Bari "Aldo Moro" as the provider of the Software. 

***************************************************************************************************/

:- include(slipcover_lemur).

/**************************************
	 __BEGIN__
	 New source code for MCLPADS
 **************************************/

%setting(mcts_max_depth,8).
%setting(mcts_c,0.7). /* see L. Kocsis, C. Szepesvri, and J. Willemson, "Improved Monte-Carlo Search", 2006 */
%setting(mcts_iter,100).
setting(mcts_beamsize,3).
setting(mcts_visits,+inf).
%setting(max_rules,6).

setting(max_var,4).

mcts(File,ParDepth,ParC,ParIter,ParRules,Covering):-
	assert(setting(mcts_max_depth,ParDepth)),
	assert(setting(mcts_c,ParC)),
	assert(setting(mcts_iter,ParIter)),
	assert(setting(mcts_covering,Covering)),
	( Covering = true ->
		assert(setting(max_rules,1)),
		assert(setting(mcts_maxrestarts,ParRules))
	;
		assert(setting(max_rules,ParRules))
	),
  setting(seed,Seed),
  setrand(Seed),
	format("\nMonte Carlo Tree Search for LPAD Structure Learning\n",[]),
	generate_file_names(File,FileKB,FileIn,FileBG,FileOut,FileL),

	name(File,FileDot),
  append(FileDot,".dot",FileDotExt),
  name(FileExt,FileDotExt),
	assert(filedot(FileExt)),
	
	reconsult(FileL),
	load_models(FileKB,DB),	
	statistics(walltime,[_,_]),	
	(file_exists(FileBG)->
	 set(compiling,on),
	 load(FileBG,_ThBG,RBG),
	 set(compiling,off),
	 generate_clauses(RBG,_RBG1,0,[],ThBG), 
	 assert_all(ThBG)
	;
	 true
	),
  (file_exists(FileIn)->
    set(compiling,on),
    load(FileIn,_Th1,R1),
    set(compiling,off)
  ;
	 get_head_atoms(LHM,_LH0),
	 generate_top_cl(LHM,R1)
  ),
	
%	write('Initial theory'),nl,
%	write_rules(R1,user_output),

  findall(BL , modeb(_,BL), BLS0),
	sort(BLS0,BSL),
	assert(mcts_modeb(BSL)),

	assert(mcts_restart(1)),
	learn_struct_mcts(DB,R1,R2,CLL2),
	retract(mcts_restart(_)),
	learn_params(DB,R2,R,CLL),  

	statistics(walltime,[_,WT]),
	WTS is WT/1000,
  format("~nRefinement CLL  ~f - CLL after EMBLEM ~f~n",[CLL2,CLL]),
  format("Total execution time ~f~n~n",[WTS]),
	write_rules(R,user_output),
	listing(setting/2),
	format("Model:~n",[]),
	open(FileOut,write,Stream),
	format(Stream,"/* MCTS Final CLL(da prolog) ~f~n",[CLL]),
	format(Stream,"Execution time ~f~n",[WTS]),
	tell(Stream),	
	listing(setting/2),
	format(Stream,"*/~n~n",[]),
	told, 
	open(FileOut,append,Stream1),
	write_rules(R,Stream1),
	close(Stream1).

learn_struct_mcts(DB,R1,R,CLL1):-  
	generate_clauses(R1,R2,0,[],Th1), 
	assert_all(Th1),  
	assert_all(R2),
	!,
	findall(R-HN,(rule(R,HL,_BL,_Lit),length(HL,HN)),L),  
	keysort(L,LS),
	get_heads(LS,LSH),  
	length(LSH,NR),   
	init(NR,LSH),
	retractall(v(_,_,_)),
	length(DB,NEx),  
	(setting(examples,atoms) ->
	 setting(group,G),	
	 derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),
	 ! 
	;
	 derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),
	 ! 
	),
	setting(random_restarts_number,N),
	format("~nInitial CLL ~f~n~n",[CLL0]),
	random_restarts(N,Nodes,CLL0,CLL,initial,Par,LE), 
  format("CLL after EMBLEM = ~f~n",[CLL]),
	retract_all(Th1),
	retract_all(R2),
	!,
	end, 
	update_theory(R2,Par,R3), 
	write('updated Theory'),nl,
	write_rules(R3,user_output),


	assert(mcts_best_score(CLL)),
	assert(mcts_best_theory(R3)),
	assert(mcts_theories(0)),

	assert(mcts_best_theories_iteration([])),	

	mcts(R3,CLL,DB),
%	assert(mcts_best_by_cll(-inf)),
%	assert(mcts_best_theory_by_cll([])),
%	assert(mcts_best_by_visits(-inf)),
%	select_the_best_bycll,
%	select_the_best_byvisits,	

	retract(mcts_best_theories_iteration(BestsIter)),
	format("\nBests found at : ~w",[BestsIter]),
	
	retract(mcts_theories(_)),
	retract(mcts_best_score(CLLNew)),
	retract(mcts_best_theory(RNew)),

	( setting(mcts_covering,true) ->
		
		setting(mcts_maxrestarts,MctsRestarts),
		mcts_restart(CurrentRestart),
	
		Improvement is CLLNew - CLL,
		( (CLLNew > CLL, Improvement > 0.1, CurrentRestart =< MctsRestarts) ->
	 
			format("\n---------------- Improvement ~w",[Improvement]),
			retractall(node(_, _, _, _, _, _, _)),
			retract(setting(max_rules,ParRules)),
			ParRules1 is ParRules + 1,
			assert(setting(max_rules,ParRules1)),
			retract(mcts_restart(Restart)),
			Restart1 is Restart + 1,
			assert(mcts_restart(Restart1)),	 
			learn_struct_mcts(DB,RNew,R,CLL1)
		;
			CLL1 = CLLNew,
			R = RNew
		)
	;
		CLL1 = CLLNew,
		R = RNew
	).
				 

/*
	retract(mcts_best_by_cll(CLL1)),
%	retract(mcts_best_theory_by_visits(_)),
	retract(mcts_best_theory_by_cll(R)).
	*/
select_the_best_bycll:-
	node(_, _, _, CLL, Theory, VISITED, BACKSCORE),
	( VISITED >= 0 ->
		mcts_best_by_cll(BS),
		Score is CLL,
		( Score =< 0, Score >= BS ->
			format("\n Best Theory ~w\n\t Backscore ~w\n\t Visits ~w\n\t CLL ~w",[Theory,BACKSCORE,VISITED,CLL]),
			retract(mcts_best_by_cll(_)),
			assert(mcts_best_by_cll(Score)),
			retract(mcts_best_theory_by_cll(_)),
			assert(mcts_best_theory_by_cll(Theory))
		;
			true
		)
	;
		true
	),
	fail.
select_the_best_bycll.

select_the_best_byvisits:-
	node(_, _, _, CLL, Theory, VISITED, BACKSCORE),
	( VISITED >= 0 ->
		mcts_best_by_visits(BS),
		Score is VISITED,
		( Score >= BS ->
			format("\n Best Theory ~w\n\t Backscore ~w\n\t Visits ~w\n\t CLL ~w",[Theory,BACKSCORE,VISITED,CLL]),
			retract(mcts_best_by_visits(_)),
			assert(mcts_best_by_visits(Score))
		;
			true
		)
	;
		true
	),
	fail.
select_the_best_byvisits.


mcts(InitialTheory,InitialScore,DB):-
	% node(ID, CHILDRENS, PARENT, CLL, Theory, VISITED, BACKSCORE)
	assert(node(1, [], 0, InitialScore , InitialTheory, 0 , 0)),
	assert(lastid(1)),
	setting(mcts_iter,I),
	assert(mcts_iteration(0)),
	cycle_mcts(I,DB),
	retract(mcts_iteration(_)),
	retract(lastid(Nodes)),
	print_graph,	
	format("\nTree size: ~w nodes.",[Nodes]).

print_graph:-
	filedot(FileDot),
	open(FileDot,write,S),
	format(S,"digraph UCT{\n",[]),
	format(S,"graph [splines=line];\n",[]),
	format(S,"edge [dir=\"none\"];\n",[]),
	format(S,"node [style=\"filled\",label=\"\",shape=point];\n",[]),
	
	print_graph([1],S),
	format(S,"}",[]),
	close(S).
print_graph([],S).
print_graph([ID|R],S):-
	node(ID, Childs, Parent , CLL, Theory, Visited, Backscore),
	print_edges(ID,Childs,S),
	print_graph(R,S),
	print_graph(Childs,S).
print_edges(ID,[],S).
print_edges(ID,[ID1|R],S):-
	node(ID1, Childs, Parent , CLL, Theory, Visited, Backscore),
	(Visited > 1 ->
	 format(S,"~w -> ~w;\n",[ID,ID1])
	 %format(S,"~w [label=\"~w,~w\"] ;\n",[ID1,ID1,Visited])
	;
	 true
	),
	print_edges(ID,R,S).

backup_transposition(1,Reward,_):-
	!,
	(retract(node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
	 true
	;
	 format(user_error,"\nNo node with ID ~w in backup",[NodeID]),	 
	 throw(no_node_id(NodeID))
	),
	Visited1 is Visited + 1,
	assert(node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)).	
backup_transposition(NodeID,Reward,ParentsTranspose):-
	(retract(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
	 true
	;
	 format(user_error,"\nNo node with ID ~w in backup",[NodeID]),	 
	 throw(no_node_id(NodeID))
	),
	(member(NodeID,ParentsTranspose) -> 
	 Backscore1 is Backscore,
	 Visited1 is Visited,
	 format("~w- ",[NodeID])	 
	;
	 (Visited == 1 -> Backscore1 = Reward ; Backscore1 is Backscore + Reward),	 
	 Visited1 is Visited + 1,
	 format("~w+ ",[NodeID])
	),
	assert(node(NodeID, Childs, Parent , PSLL, MLN, Visited1, Backscore1)),
	backup_transposition(Parent,Reward,ParentsTranspose).


check_transposition(NodeID,Theory,SigmoidValue,ParentsTranspose):-
	lastid(Nodes),
	check_transposition(Nodes,NodeID,Theory,SigmoidValue,ParentsTranspose).

check_transposition(1,NodeID,_,SigmoidValue,ParentsTranspose):-
	!.
check_transposition(Node,NodeID,Theory,SigmoidValue,ParentsTranspose):-
	Node \== NodeID,
	!,
	node(Node, Childs, Parent , CLL, TheoryN, Visited, Backscore),
	( same_theory(Theory,TheoryN) ->
		format("\n\tTransposition node ~w - node ~w ~w: ",[Node,NodeID,ParentsTranspose]),
		backup_transposition(Node,SigmoidValue,ParentsTranspose)
	;
		true
	),
	Node1 is Node - 1,
	check_transposition(Node1,NodeID,Theory,SigmoidValue,ParentsTranspose).
	
check_transposition(Node,NodeID,Theory,SigmoidValue,ParentsTranspose):-
	Node1 is Node - 1,
	check_transposition(Node1,NodeID,Theory,SigmoidValue,ParentsTranspose).


backup_amaf(1,Reward,_):-
	!,
	(retract(node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
	 true
	;
	 format(user_error,"\nNo node with ID ~w in backup",[NodeID]),	 
	 throw(no_node_id(NodeID))
	),
	Visited1 is Visited + 1,
	assert(node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)).	
backup_amaf(NodeID,Reward,ParentsTranspose):-
	(retract(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
	 true
	;
	 format(user_error,"\nNo node with ID ~w in backup",[NodeID]),	 
	 throw(no_node_id(NodeID))
	),
	(member(NodeID,ParentsTranspose) -> 
	 Backscore1 is Backscore,
	 Visited1 is Visited
%	 format("~w- ",[NodeID])	 
	;
	 SigmoidValue is 1 / (1 - PSLL),
	 ( PSLL = 1  ->
		 Backscore1 is Backscore + Reward
	 ;
		 ( Reward > SigmoidValue ->
			 Backscore1 is Backscore + Reward
		 ;
			 Backscore1 is Backscore + SigmoidValue
%			 Backscore1 is Backscore + Reward
		 )
	 ),

	 Visited1 is Visited + 1
%	 format("~w+ ",[NodeID])
	),
	assert(node(NodeID, Childs, Parent , PSLL, MLN, Visited1, Backscore1)).
%%%	backup_amaf(Parent,Reward,ParentsTranspose).


check_amaf(NodeID,Theory,SigmoidValue,ParentsTranspose):-
	lastid(Nodes),
	format("\nChecking amaf: node ~w, parents ~w: ",[NodeID,ParentsTranspose]),	
	check_amaf(Nodes,NodeID,Theory,SigmoidValue,ParentsTranspose).

check_amaf(1,NodeID,_,SigmoidValue,ParentsTranspose):-
	retract(node(1, Childs, Parent , PSLL, MLN, Visited, Backscore)),
	Visited1 is Visited + 1,
	assert(node(1, Childs, Parent , PSLL, MLN, Visited1, Backscore)),
	!.
check_amaf(Node,NodeID,Theory,SigmoidValue,ParentsTranspose):-
	Node \== NodeID,
	!,
	node(Node, Childs, Parent , CLL, TheoryN, Visited, Backscore),
	( subsume_theory(TheoryN,Theory) ->
%%%		format("\n\t ~w ~w: ",[TheoryN,Theory]),
		backup_amaf(Node,SigmoidValue,ParentsTranspose)
	;
		true
	),
	Node1 is Node - 1,
	check_amaf(Node1,NodeID,Theory,SigmoidValue,ParentsTranspose).
	
check_amaf(Node,NodeID,Theory,SigmoidValue,ParentsTranspose):-
	Node1 is Node - 1,
	check_amaf(Node1,NodeID,Theory,SigmoidValue,ParentsTranspose).


subsume_theory(Theory,TheoryN):-
	copy_term(Theory,Theory1),
	skolemize(TheoryN,TheoryN1),	
	subsume_theory1(Theory1,TheoryN1),
	!.

/*skolemize(Theory,Theory1):-
	copy_term(Theory,Theory1),
	term_variables(Theory1,Vars),
	skolemize1(Vars,1).

skolemize1([],_).
skolemize1([Var|R],K):-
	atomic_list_concat([s,K],Skolem),
	Var = Skolem,
	K1 is K + 1,
	skolemize1(R,K1).
*/

subsume_theory1([],_).
subsume_theory1([Rule|R],TheoryN):-
	subsume_theory2(Rule,TheoryN,NewTheoryN),
	subsume_theory1(R,NewTheoryN).

subsume_theory2(Rule,[Rule1|R],R):-
	Rule = rule(_,[H: _, _: _],Body,_),
	Rule1 = rule(_,[H1: _, _: _],Body1,_),
	H = H1,
	subsume_body(Body,Body1),
	!.
subsume_theory2(Rule,[Rule1|R],[Rule1|R1]):-
	subsume_theory2(Rule,R,R1).
	
	
subsume_body(Body,Body1):-
	length(Body,L),
	length(Body1,L1),
	L =< L1,
	subsume_body1(Body,Body1).
subsume_body1([],_).
subsume_body1([L|R],Body):-
	nth(_,Body,L,Rest),
	subsume_body1(R,Rest).	



same_theory(Theory0,TheoryN):-
	copy_term(Theory0,Theory),	
	length(Theory,L),
	length(TheoryN,L),
	same_theory1(Theory,TheoryN),
	!.

same_theory1([],[]).
same_theory1([Rule|R],TheoryN):-
	same_theory2(Rule,TheoryN,NewTheoryN),
	same_theory1(R,NewTheoryN).

same_theory2(Rule,[Rule1|R],R):-
	Rule = rule(_,[H: _, _: _],Body,_),
	Rule1 = rule(_,[H1: _, _: _],Body1,_),
	H = H1,
	same_body(Body,Body1),
	!.
same_theory2(Rule,[Rule1|R],[Rule1|R1]):-
	same_theory2(Rule,R,R1).
	
	
same_body(Body,Body1):-
	length(Body,L),
	length(Body1,L),
	same_body1(Body,Body1).
same_body1([],[]).
same_body1([L|R],Body):-
	nth(_,Body,L,Rest),
	same_body1(R,Rest).	

%
	


	

cycle_mcts(0,_):-
	!.
cycle_mcts(K,DB):-
	setting(mcts_iter,MaxI),
	Iteration is MaxI - K + 1,
	retract(mcts_iteration(_)),
	assert(mcts_iteration(Iteration)),
	format("\nIteration ~w",[Iteration]),
	tree_policy(1,NodeID,DB,1,Depth),
	( node(NodeID, Childs, Parent , CLL, Theory, Visited, Backscore) ->
	%% do update with the sigmoid of the Score
	%% SigmoidValue is ((1 / (1 + exp(-PSLL)))/0.5),
	%% format("\n~w: ~w ~w Sigmoid ~w",[K,MLN,PSLL,SigmoidValue]),	
		setting(mcts_max_depth, MaxDepth),
		random(1,MaxDepth,MaxDepth1),
		default_policy(Theory,-inf,Reward,_,BestDefaultTheory,DB,1,MaxDepth1),
	% do update with the sigmoid of the Score
%%%	SigmoidValue is ((1 / (1 + exp(-Reward)))/0.5),


		SigmoidValue is 1 / (1 -  Reward),

		( SigmoidValue > 0 ->
		
%	(Reward > CLL ->
%	 SigmoidValue = 1
%	;
%	 SigmoidValue = 0
%	),

%%%	format("\n~w: ~w \nReward ~w Sigmoid ~w",[K,Theory,Reward,SigmoidValue]),
			format("\n[Backup reward ~w]",[SigmoidValue]),
			backup(NodeID,SigmoidValue,Parents),
																%	check_transposition(NodeID,Theory,SigmoidValue,Parents),
			check_amaf(NodeID,BestDefaultTheory,SigmoidValue,Parents)
		;
			format("\n--> no default policy expansion",[])
		),
		K1 is K - 1,
%%%	read(_),
		cycle_mcts(K1,DB)
	;
		format("\n--> tree policy end",[])
	).

check_pruning(ID):-
	node(ID, Childs, Parent , CLL, Theory, VISITED, BACKSCORE),
	Childs \== [],
	length(Childs,NumChilds),
	setting(mcts_beamsize,BeamSize),
	NumChilds > BeamSize,
	!,
	setting(mcts_visits,NumVisits),
	check_pruning(Childs,ID,NumVisits,BeamSize,NewChilds),
	retract(node(ID, Childs, Parent , CLL, Theory, VISITED, BACKSCORE)),
	assert(node(ID, NewChilds, Parent , CLL, Theory, VISITED, BACKSCORE)).
check_pruning(_ID).	

check_pruning(Childs,ID,NumVisits,BeamSize,Childs2):-
	check_pruning1(Childs,NumVisits,ToPrune,Childs1),
	length(Childs1,L1),
	L1 > BeamSize,
	ToPrune == 1,
	!,
	choose_best_childs(Childs1,BeamSize,Childs2),
	format("\n#Pruning tree ~w ~w",[ID,Childs2]),flush_output,
	prune(Childs,Childs2).
check_pruning(Childs,_,_NumVisits,_BeamSize,Childs).


choose_best_childs(Childs,BeamSize,Childs1):-
	add_visisted(Childs,ChildsV),
	keysort(ChildsV,ChildsV1),
	remove_visisted(ChildsV1,ChildsV2),
	length(Childs1,BeamSize),
	append(Childs1,_,ChildsV2),!.


remove_visisted([],[]).
remove_visisted([V-ID|R],[ID|R1]):-
	remove_visisted(R,R1).

add_visisted([],[]).
add_visisted([ID|R],[V-ID|R1]):-
	node(ID, Childs, Parent , CLL, Theory, VISITED, BACKSCORE),
	V is -1 * VISITED,
	add_visisted(R,R1).

prune([],_Childs1).
prune([ID|R],Childs1):-
	member(ID,Childs1),
	!,
	prune(R,Childs1).
prune([ID|R],Childs1):-
	prune_sub_tree(ID),
	prune(R,Childs1).

prune_sub_tree(ID):-
	retract(node(ID, Childs, _Parent , _CLL, _Theory, _VISITED, _BACKSCORE)),
	prune_sub_tree1(Childs).

prune_sub_tree1([]).
prune_sub_tree1([ID|R]):-
	retract(node(ID, Childs, _Parent , _CLL, _Theory, _VISITED, _BACKSCORE)),
	prune_sub_tree1(Childs),
	prune_sub_tree1(R).


check_pruning1([],_NumVisits,1,[]).
check_pruning1([ID|R],NumVisits,ToPrune,[ID|R1]):-
	node(ID, _Childs, _Parent , CLL, _Theory, VISITED, _BACKSCORE),
	(CLL == 1 ->
	 ToPrune = 0,
	 R1 = [],
	 !
	;
	 VISITED >= NumVisits,
	 !,
	 check_pruning1(R,NumVisits,ToPrune,R1)
	).
check_pruning1([ID|R],NumVisits,ToPrune,R1):-
	check_pruning1(R,NumVisits,ToPrune,R1).
	


tree_policy(ID,NodeID,DB,Od,Nd):-
%	check_pruning(ID),


	(retract(node(ID, Childs, Parent , CLL, Theory, VISITED, BACKSCORE)) ->
	 true
	;
	 throw(no_node_id(ID))
	),
%%%	format("\n Tree policy ~w ~w ~w",[Theory,VISITED, BACKSCORE]),
	format("\n[Tree Policy ~w, ~w, ~w] ",[ID,VISITED,BACKSCORE]),		flush_output,
%%%	( VISITED = 0, ID \= 1 ->
	( CLL = 1, ID \= 1 ->
		score_theory(Theory,DB,CLL1,BestTheory,NewTheory),
		mcts_best_score(BestScore),

%			Ratio is  BestScore / CLL1,
%			( Ratio > 1.001 ->

		
		( setting(mcts_covering,true) ->
			length(NewTheory,NewTheoryL),	%lemurc
			length(Theory,TheoryL),
			( NewTheoryL = TheoryL ->
				LengthCondition = true
			;
				LengthCondition = false
			)
		;
			LengthCondition = true
		),


		( (CLL1 > BestScore, LengthCondition = true) ->
			format("\n[New best score: ~w ~w]",[CLL1, BestTheory]),flush_output,


			retract(mcts_best_score(_)),
			retract(mcts_best_theory(_)),
			assert(mcts_best_score(CLL1)),
			assert(mcts_best_theory(NewTheory)),

			retract(mcts_best_theories_iteration(BestsIter)),
			mcts_iteration(Iteration),
			append(BestsIter,[Iteration],BestsIter1),
			assert(mcts_best_theories_iteration(BestsIter1)),
			
			retract(mcts_theories(Mlns)),
			Mlns1 is Mlns + 1,
			assert(mcts_theories(Mlns1))
		;
			true
		)
	;
		CLL1 = CLL,
		NewTheory = Theory
	),
	
	Visited1 is VISITED + 1,

%	(CLL = 1 ->
%	 Visited2 = Visited1,
%	 (Visited2 == 2 -> Backscore1 = BACKSCORE ; Backscore1 = 0) % in this case the node has been visited by transposition
%	;
%	 Visited2 = Visited1,
%	 Backscore1 = BACKSCORE
%	),

	 Visited2 = Visited1,
	 Backscore1 = BACKSCORE,

	
	(Childs == [] ->
	 Nd = Od,
	 expand(ID, Theory, CLL1, DB, NodeID, Childs1),
	 assert(node(ID, Childs1, Parent , CLL1, NewTheory, Visited2, Backscore1))
	;
	 Od1 is Od + 1,
	 minmaxvalue(Childs,MinV,MaxV),
%	 mean_value_level(Childs,Mvl),
	 once(uct(Childs, VISITED, MinV, MaxV, BestChild)),
%	 once(uct(Childs, VISITED, BestChild)),	 
	 tree_policy(BestChild,NodeID,DB,Od1, Nd),
	 assert(node(ID, Childs, Parent , CLL1, NewTheory, Visited2, Backscore1))
	).



default_policy(Theory, Reward, Reward, BestDefaultTheory,BestDefaultTheory,DB, Depth, MaxDepth):-
	Depth > MaxDepth,
	!.
default_policy(Theory,PrevR,Reward,PrevBestDefaultTheory,BestDefaultTheory,DB,Depth,MaxDepth):-
%%%	format("\nDefault policy",[]),flush_output,
	format("\n[Default Policy ~w]",[Depth]),
	theory_revisions_r(Theory,Revisions),
	( Revisions \== [] ->
		length(Revisions,L),
		random(0,L,K),
		nth0(K, Revisions,Spec),
		Depth1 is Depth + 1,


		score_theory(Spec,DB,Score,BestTheory,NewTheory),

		( setting(mcts_covering,true) ->
			length(NewTheory,NewTheoryL),	%lemurc
			length(Spec,TheoryL),
			( NewTheoryL = TheoryL ->
				LengthCondition = true
			;
				LengthCondition = false
			)
		;
			LengthCondition = true
		),
		
	
		( (Score > PrevR, LengthCondition = true) ->
		 Reward1 = Score,
		 BestDefaultTheory1 = NewTheory
		;
		 Reward1 = PrevR,
		 BestDefaultTheory1 = PrevBestDefaultTheory
		),

		format(" cll-reward ~w",[Reward1]),
		
		mcts_best_score(BestScore),

	
		( (Score > BestScore, LengthCondition = true) ->
			format("\n[New best score: ~w ~w]",[Score, BestTheory]),flush_output,

		
			retract(mcts_best_score(_)),
			retract(mcts_best_theory(_)),
			assert(mcts_best_score(Score)),
			assert(mcts_best_theory(NewTheory)),

			retract(mcts_best_theories_iteration(BestsIter)),
			mcts_iteration(Iteration),
			append(BestsIter,[Iteration],BestsIter1),
			assert(mcts_best_theories_iteration(BestsIter1)),
		
			
			retract(mcts_theories(Mlns)),
			Mlns1 is Mlns + 1,
			assert(mcts_theories(Mlns1))
		;
			true
		),


		
		default_policy(Spec, Reward1,Reward, BestDefaultTheory1,BestDefaultTheory,DB, Depth1,MaxDepth)
	
	;
		Reward = PrevR,
		BestDefaultTheory = PrevBestDefaultTheory
/*

%%%		format("\n\t Default ~w",[Theory]),
		score_theory(Theory,DB,Score,BestTheory),

		(Score > PrevR ->
		 Reward = Score
		;
		 Reward = PrevR
		),
	
		mcts_best_score(BestScore),

	
		( Score > BestScore ->
			format("\n[New best score: ~w ~w]",[Score, BestTheory]),flush_output,

			retract(mcts_best_score(_)),
			retract(mcts_best_theory(_)),
			assert(mcts_best_score(Score)),
			assert(mcts_best_theory(BestTheory)),

			retract(mcts_best_theories_iteration(BestsIter)),
			mcts_iteration(Iteration),
			append(BestsIter,[Iteration],BestsIter1),
			assert(mcts_best_theories_iteration(BestsIter1)),
		
			
			retract(mcts_theories(Mlns)),
			Mlns1 is Mlns + 1,
			assert(mcts_theories(Mlns1))
		;
			true
		)


*/
		
	).


minmaxvalue(Childs,MinV,MaxV):-
	Childs = [F|R],
	node(F, _, _ , _, _, Visits, Reward),
	V is Reward / Visits,
	minmaxvalue(R,V,V,MinV,MaxV).

minmaxvalue([],Min,Max,Min,Max).
minmaxvalue([C|R],PrevMin,PrevMax,MinV,MaxV):-
	node(C, _, _ , _, _, Visits, Reward),
	V is Reward / Visits,
	( V > PrevMax ->
		Max1 is V
	;
		Max1 is PrevMax
	),
	( V < PrevMin ->
		Min1 is V
	;
		Min1 is PrevMin
	),
	minmaxvalue(R,Min1,Max1,MinV,MaxV).
mean_value_level(Cs,M):-
	mean_value_level1(Cs,Me),
	length(Me,L),
	sum_list(Me,S),
	M is S / L.
mean_value_level1([],[]).
mean_value_level1([C|R],M1):-
	node(C, _, _ , 1, _, _Visits, _Reward),
	!,
	mean_value_level1(R,M1).
mean_value_level1([C|R],[M|Rm]):-
	node(C, _, _ , _, _, Visits, Reward),
	!,
	mean_value_level1(R,Rm),
	M is (Reward / Visits).

/*
uct(Childs, ParentVisits, BestChild):-
%%%	format("\nUCT ",[]),
	Childs = [FirstChild|RestChilds],
	node(FirstChild, _, _ , _, Theory, Visits, Reward),
	( Visits == 0 ->
		BestChild = FirstChild
	;
		setting(mcts_c,C),
		UCT is Reward / Visits + 2 * C * sqrt(2 * log(ParentVisits) / Visits),
%%%		format("~w ",[UCT]),
		uct(RestChilds, UCT, ParentVisits, FirstChild, BestChild)
	).


uct([], _CurrentBestUCT, _ParentVisits, BestChild, BestChild).
uct([Child|RestChilds], CurrentBestUCT, ParentVisits, CurrentBestChild, BestChild) :-
	node(Child, _, _ , _, Theory, Visits, Reward),
	( Visits == 0 ->
		BestChild = Child
	;
		setting(mcts_c,C),		
		UCT is Reward / Visits + 2 * C * sqrt(2 * log(ParentVisits) / Visits),
%%%		format("~w ",[UCT]),flush_output,
		(UCT > CurrentBestUCT ->
		 uct(RestChilds, UCT, ParentVisits, Child, BestChild)
		;
		 uct(RestChilds, CurrentBestUCT, ParentVisits, CurrentBestChild, BestChild)
		)
	).
*/



uct(Childs, ParentVisits, Min, Max, BestChild):-
%%%	format("\nUCT ",[]),
	Childs = [FirstChild|RestChilds],
	node(FirstChild, _, _ , Score, Theory, Visits, Reward),
	( Visits == 0 ->
		BestChild = FirstChild
	;
		setting(mcts_c,C),
%		(Score == 1 ->
%		 R is Mvl
%		;
%		 R is Reward
%		),
		R is Reward,
		AA is ((R / Visits) - Min ) / (Max-Min),
		BB is 2 * C * sqrt(2 * log(ParentVisits) / Visits),
		UCT is ((R / Visits) - Min ) / (Max-Min) + 2 * C * sqrt(2 * log(ParentVisits) / Visits),
%%%		format("\nID ~w UCT ~w ~w/~w=~w ~w",[FirstChild,UCT,R,Visits,AA,BB]),
%%%		format("\n\t ~w ~w",[Score,Theory]),
%%%		format("~w ",[UCT]),
		uct(RestChilds, UCT, ParentVisits, FirstChild, Min,Max, BestChild)
	).


uct([], _CurrentBestUCT, _ParentVisits, BestChild, _, _,BestChild).
uct([Child|RestChilds], CurrentBestUCT, ParentVisits, CurrentBestChild, Min, Max,BestChild) :-
	node(Child, _, _ , Score, Theory, Visits, Reward),
	( Visits == 0 ->
		BestChild = Child
	;
		setting(mcts_c,C),		
%		(Score == 1 ->
%		 R is Mvl
%		;
%		 R is Reward
%		),
		R is Reward,
		AA is ((R / Visits) - Min ) / (Max-Min),
		BB is 2 * C * sqrt(2 * log(ParentVisits) / Visits),
		UCT is ((R / Visits) - Min ) / (Max-Min) + 2 * C * sqrt(2 * log(ParentVisits) / Visits),
%%%		format("\nID ~w UCT ~w ~w/~w=~w ~w",[Child,UCT,R,Visits,AA,BB]),
%%%		format("\n\t ~w ~w",[Score,Theory]),		
%%%		format("~w ",[UCT]),flush_output,
		(UCT > CurrentBestUCT ->
		 uct(RestChilds, UCT, ParentVisits, Child, Min, Max, BestChild)
		;
		 uct(RestChilds, CurrentBestUCT, ParentVisits, CurrentBestChild, Min, Max, BestChild)
		)
	).


expand(ID, Theory, ParentCLL, DB, NodeID, Childs):-
%%%	format("  expanding...",[]),flush_output,
  theory_revisions(Theory,Revisions),
	!,
	assert_childs(Revisions,ID,ParentCLL,Childs),
	(Childs \= [] ->
	 Childs = [NodeID|_],
	 retract(node(NodeID, Childs1, Parent , _, Theory1, Visited, Backscore)),
	 format("\n[Expand ~w]",[NodeID]),	 
	 Visited1 is Visited + 1,
	 score_theory(Theory1,DB,CLL,BestTheory,NewTheory),
	 format(" CLL: ~w]",[CLL]),	 
	 %%%format("\nTree policy: ~w ~w]",[Score, Theory1]),
	 mcts_best_score(BestScore),

%			Ratio is BestScore / CLL,
%			( Ratio > 1.001 ->


	 ( setting(mcts_covering,true) ->
		 length(NewTheory,NewTheoryL), %lemurc
		 length(Theory1,Theory1L),
		 ( NewTheoryL = Theory1L ->
			 LengthCondition = true
		 ;
			 LengthCondition = false
		 )
	 ;
		 LengthCondition = true
	 ),


	 ( (CLL > BestScore, LengthCondition = true) ->
		 format("\n[New best score: ~w ~w]",[CLL, BestTheory]),flush_output,		 
		 retract(mcts_best_score(_)),
		 retract(mcts_best_theory(_)),
		 assert(mcts_best_score(CLL)),
		 assert(mcts_best_theory(NewTheory)),

		 retract(mcts_best_theories_iteration(BestsIter)),
		 mcts_iteration(Iteration),
		 append(BestsIter,[Iteration],BestsIter1),
		 assert(mcts_best_theories_iteration(BestsIter1)),

		 
		 retract(mcts_theories(Mlns)),
		 Mlns1 is Mlns + 1,
		 assert(mcts_theories(Mlns1))
	 ;
		 true
	 ),
	 assert(node(NodeID, Childs1, Parent , CLL, NewTheory, Visited1, Backscore))
	;
	 NodeID = -1
	).
%%%	format("  END",[]),flush_output.
	
assert_childs([],_,_,[]).
assert_childs([Spec|Rest],P,PCLL,[ID1|Childs]):-
	% node(ID, CHILDRENS, PARENT, PSLL, MLN, VISITED, BACKSCORE)
	retract(lastid(ID)),
%%%	format(" ~w",[ID]),flush_output,
	ID1 is ID + 1,
	assert(lastid(ID1)),
%	SigmoidValue is ((1 / (1 + exp(-PCLL)))/0.5),
		SigmoidValue is 1 / (1 -  PCLL),	
	%format(" ~w",[ID1]),
%%%	score_theory(Spec,DB,CLL),
	assert(node(ID1, [], P, 1 , Spec, 1 , SigmoidValue)),
%%	assert(node(ID1, [], P, 1 , Spec, 0 , 0)),
	assert_childs(Rest,P,PCLL,Childs).


theory_length([],X,X).
theory_length([T|R],K,K1):-
	theory_length(R,K,K0),
	T = rule(_,_,B,_),
	length(B,L),
	( L > K0 ->
		K1 = L
	;
		K1 = K0
	).

score_theory(Theory0,DB,Score,Theory,R3):-

	( mcts_theories(0) ->
		Theory = Theory0
	;
		theory_length(Theory0,0,Le),
		( Le > 1 ->
%			mcts_best_theory(TheoryBest),
%			append(TheoryBest,Theory0,Theory)
			Theory = Theory0
		;
			Theory = Theory0
		)
	),


%%%	format("   Scoring....",[]),flush_output,
%%%  write_rules(Theory,user_output),   flush_output,
  generate_clauses(Theory,R2,0,[],Th1),
%%%	format("\n ~w\n ~w\n ~w",[Theory,R2,Th1]),
  assert_all(Th1),
  assert_all(R2),!,
  findall(RN-HN,(rule(RN,HL,_BL,_Lit),length(HL,HN)),L),
  keysort(L,LS),
  get_heads(LS,LSH),
  length(LSH,NR),
  init(NR,LSH),
  retractall(v(_,_,_)),
  length(DB,NEx),
  (setting(examples,atoms)->
    setting(group,G),  
    derive_bdd_nodes_groupatoms(DB,NEx,G,[],Nodes,0,CLL0,LE,[]),!
  ; 
    derive_bdd_nodes(DB,NEx,[],Nodes,0,CLL0),!
  ),
  setting(random_restarts_REFnumber,N),
  random_restarts(N,Nodes,-inf,CLL,initial,Par,LE),  
  end,

%%%	format("\n Score ~w ~w",[CLL0,CLL]),
  update_theory_par(R2,Par,R3),
	
  %%%write('Updated refinement'),nl,
%%  nl,nl,write_rules(R3,user_output), 
  Score = CLL,  
%%%  nl,write('Score (CLL) '),write(Score),nl,nl,nl,
  retract_all(Th1),
  retract_all(R2),
%%%	format(" End",[]),flush_output,
	!.

backup(1,Reward,[]):-
	!.
backup(NodeID,Reward,[Parent|R]):-
	(retract(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore)) ->
	 true
	;
	 format(user_error,"\nNo node with ID ~w in backup",[NodeID]),	 
	 throw(no_node_id(NodeID))
	),
	SigmoidValue is 1 / (1 -  PSLL),
	( Reward > SigmoidValue ->
		Backscore1 is Backscore + Reward,
		Reward1 is Reward
	;
		Backscore1 is Backscore + SigmoidValue,
		Reward1 is SigmoidValue		
%		Backscore1 is Backscore + Reward,
%		Reward1 is Reward
		),
%%%	format("\n backup ~w ~w",[NodeID,MLN]),
	assert(node(NodeID, Childs, Parent , PSLL, MLN, Visited, Backscore1)),
	backup(Parent,Reward1,R).


/**************************************
	 __END__
	 New source code for MCLPADS
 **************************************/

