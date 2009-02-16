%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parameter Learning for ProbLog
%
% 27.10.2008
% bernd.gutmann@cs.kuleuven.be
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
			       shell/1,
	                       shell/2]).

% load our own modules
:- use_module('learning/logger').
:- use_module(problog).

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
	retractall(rebuild_bdds(_)),
	assert(rebuild_bdds(Flag)).

set_learning_flag(rebuild_bdds_it,Flag) :-
	integer(Flag),
	retractall(rebuild_bdds_it(_)),
	assert(rebuild_bdds_it(Flag)).


set_learning_flag(reuse_initialized_bdds,Flag) :-
	(Flag=true;Flag=false),
	retractall(reuse_initialized_bdds(_)),
	assert(reuse_initialized_bdds(Flag)).

set_learning_flag(learning_rate,V) :-
	(V=examples -> true;(number(V),V>=0)),
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
	retractall(check_duplicate_bdds(_)),
	assert(check_duplicate_bdds(Flag)).

set_learning_flag(output_directory,Directory) :-
	(
	    file_exists(Directory)
	->
	    file_property(Directory,type(directory));
	    make_directory(Directory)
	),

	atomic_concat([Directory,'/'],Path),
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
	
	atomic_concat([Directory,'/'],Path),
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
		
		format(' Iteration ~d of ~d~n',[CurrentIteration,EndIteration]),
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
	values_correct,
	!.

update_values :-
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

	( % go over all tunable facts
	    get_fact_probability(ID,Prob),
	    inv_sigmoid(Prob,Value),
	    format(Handle,'@x~q~n~10f~n',[ID,Value]),

	    fail; % go to next tunable fact
	    true
	),

	close(Handle),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop write current probabilities to file
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start update values for all training examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	( % go over all training examples
	    current_predicate(user:example/3),
	    user:example(QueryID,_Query,_QueryProb),
	    once(call_bdd_tool(QueryID,'.')),
	    fail; % go to next training example
	    true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop update values for all training examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start update values for all test examples
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ( % go over all training examples
	    current_predicate(user:test_example/3),
	    user:test_example(QueryID,_Query,_QueryProb),
	    once(call_bdd_tool(QueryID,'+')),
	    fail; % go to next training example
	    true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop update values for all test examples
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


call_bdd_tool(QueryID,Symbol) :-
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
		atomic_concat([PD,
	                          '/ProblogBDD -i "',
		                  Output_Directory,
				  'input.txt',
				  '" -l "',
			          Query_Directory,
			          'query_',
				  QueryID,
				  '" -m g -id ',
				  QueryID,
				  ' -sl ',Slope,
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
		    throw(bdd_error(QueryID,Error));
		    true
		),
		atomic_concat([Output_Directory,'values.pl'],Values_Filename),
		once(my_load(Values_Filename)),
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
	TMP is max(0.00001,Prob),
	Prob_Secure is min(0.99999,TMP).



%========================================================================
%= Perform one iteration of gradient descent
%=
%= assumes that everything is initialized, if the current values
%= of query_probability/2 and query_gradient/3 are not up to date
%= they will be recalculated
%= finally, the values_correct/0 is retracted to signal that the
%= probabilities of the examples have to be recalculated
%========================================================================


gradient_descent :-
	update_values,

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% start set gradient to zero
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	(   % go over all tunable facts

	    tunable_fact(FactID,_),
	    bb_put(FactID,0.0),
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
		(
		    query_gradient(QueryID,FactID,GradValue)
		->
		     true;
		     GradValue=0.0
		),
		bb_get(FactID,OldValue),
		NewValue is OldValue + Y*GradValue,
		bb_put(FactID,NewValue),
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
	findall(V,(tunable_fact(FactID,_),bb_get(FactID,V)),GradientValues),

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

	learning_rate(LearningRate),

	( % go over all tunable facts

	    tunable_fact(FactID,_),
	    get_fact_probability(FactID,OldProbability),
	    bb_delete(FactID,GradValue),

	    inv_sigmoid(OldProbability,OldValue),
	    NewValue is OldValue -LearningRate*GradValue,
	    sigmoid(NewValue,NewProbability),

	    % Prevent "inf" by using values too close to 1.0
	    secure_probability(NewProbability,NewProbabilityS),
	    set_fact_probability(FactID,NewProbabilityS),

	    fail; % go to next tunable fact

	    true
	),
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% stop add gradient to current probabilities
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	!,

	% we're done, mark old values as incorrect
	retractall(values_correct).


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
	problog_kbest_save(Query,10,Probability,_Status,BDDFile,ProbFile))),

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
	logger_define_variable(ground_truth_maxdiff,float).

%========================================================================
%= 
%=
%========================================================================

:- initialization(global_initialize).
