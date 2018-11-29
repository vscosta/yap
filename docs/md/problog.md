The Problog-I Language and Learning System {#ProbLogI}
============


[TOC]

This document is intended as a user guide for the users of ProbLog-I.
ProbLog is a probabilistic Prolog, a probabilistic logic programming
language, which is integrated in YAP-Prolog. Most of the work in ProbLog is now based on(Prolog-II), but we still maintain ProbLog-I in order to experiment with close integration of probabilistic nd logical systems.

@section InstallingProbLog Installing ProbLog


You will need the CUDD binary decision daagram generator. CUDD is available in Fedora Linux, MacPorts and other Linux distributions. If it is not available in your system, please fetch it from:

- [git@github.com:vscosta/cudd.git]

To compile CUDD you will need to run:

- `./configure --enable-dynamic=true`
- `make`
- `make -j install`

@section RunningProbLog Running ProbLog

To run ProbLog, go your ProbLog folder (eg. $\sim$/problog), and start
YAP (eg. $\sim$/yap/yap). This will start YAP with ProbLog
functionality.

To use ProbLog, the ProbLog module has to be loaded at the top of your
Prolog programs. This is done with the following statement:

> *:- use_module('../path/to/problog').*

where '../path/to/problog' represents the path to the problog.yap module
(ie. without the extension) from the current folder from where YAP was
started.



Similarly, to use the ProbLog learning module, use:

> *:- use_module('../path/to/problog_learning').*

@section EncodingProbs Encoding Probabilistic Facts

A probabilistic fact is encoded in ProbLog by preceding a predicate with
a probability value. For example:

> *0.5::heads(_).*

encodes the fact that there's 50% chance of getting heads when tossing
an unbiassed coin.

@subsection EncodingPars  Encoding Parameter Learning Facts

Instead of probabilities every fact has a t( ) prefix. The t stands for
tunable and indicate that ProbLog should learn the probability. The
number between the parentheses indicates the ground truth probability.
It is ignored by the learning algorithm and if you do not know the
ground truth, you can write t(_). The ground truth is used after
learning to estimate the distance of the learned model parameters to the
ground truth model parameters. For example:

> t(0.5)::heads(1).

@section ProbPreds ProbLog Predicates


This chapter describes the predicates defined by ProbLog for evaluating
the probability of queries.

In the description of the arguments of functors the following notation
will be used:

-   a preceding plus sign will denote an argument as an \"input
    argument\" - it cannot be a free variable at the time of the call

-   a preceding minus sign will denote an \"output argument\"

-   an argument with no preceding symbol can be used in both ways


@pred problog_max(+G, -Prob, -FactsUsed)


This predicate returns the most likely explanation of proving the goal G
and the facts used in achieving this explanation.



@pred problog_exact(+G, -Prob, -Status)


This predicate returns the exact total probability of achieving the goal
G and the status of the query.


@pred problog_kbest(+G, +K, -Prob, -Status)



This predicate returns the sum of the probabilities of the best K proofs
of achieving the goal G and the status of the query.


@pred problog_montecarlo(+G, +Interval_width, -Prob)



This predicate approximates the probability of achieving the goal G by
using a Monte Carlo approach, with 95% confidence in the given interval
width.


@pred problog_delta(+G , +Interval_width, -Bound_low, -Bound_up, Status)



This predicate returns the lower and upper bound of the probability of
achieving the goal G by using an iterative deepening approach with the
given interval width.


@pred problog_threshold(+G , +Prob, -Bound_low, -Bound_up, -Status)



This predicate returns the lower and upper bound of the probability of
achieving the goal G obtained by cutting the sld tree at the given
probability for each branch.

@pred problog_low(+G, +Prob, -Bound_low, -Status)



This predicate returns the lower bound of the probability of achieving
the goal G obtained by cutting the sld tree at the given probability for
each branch.

@section ProbParLearnPreds ProbLog Parameter Learning Predicates


@pred example(+N, +Q, +Prob)



This predicate specifies an example. Every example has as input a unique
identifier (N), a query (Q) and a probability (Prob) associated with it.

Instead of queries, you can also give proofs as training example. They
are encoded as the conjunction of the probabilistic facts used in the
proof.

@pred test_example(+N, +Q, +Prob)



This predicate specifies a test example. Every test example has as input
a unique identifier (N), a query (Q) and a probability (Prob) associated
with it.

Test examples are ignored during learning but are used afterwards to
check the performance of the model. The ID namespace is shared between
the test examples and the training examples and you may only reuse an ID
if the queries are identical.

@pred do_learning(+N).



Starts the learning algorithm with N iterations.



@pred do_learning(+N, +Epsilon).



Starts the learning algorithm. The learning will stop after N iterations
or if the difference of the Mean Squared Error (MSE) between two
iterations gets smaller than Epsilon - depending on what happens first.

@paragraph Learning Output
---------------

The output is created in the output subfolder of the current folder
where YAP was started. There you will find the file log.dat which
contains MSE on training and test set for every iteration, the timings,
and some metrics on the gradient in CSV format. The files
factprobs_N.pl contain the fact probabilities after the Nth iteration
and the files predictions_N.pl contain the estimated probabilities for
each training and test example - per default these file are generated
every 5th iteration only.

@section ProbMisc Miscelaneous

Both the learning and the inference module have various parameters, or
flags, that can be adjusted by the user. The following predicates are
defined by ProbLog to access and set these flags.


@pred problog_flags



This predicate lists all the flags name, value, domain and description.


@pred problog_flag(+Name, -Value)



This predicate gives the value of the flag with the specified name. The
supported flags are:



- `use_db_trie`
    Flag telling whether to use the builtin trie to trie transformation. The
possible values for this flag are true or false.



- `db_trie_opt_lvl`
    Sets the optimization level for the trie to trie transformation The
possible values for this flag are any integer



- `compare_opt_lvl`
    Flag telling whether to use comparison mode for the optimization level.
The possible values for this flag are true or false.

 - `db_min_prefix`
    Sets the minimum size of the prefix for dbtrie to optimize. The possible
values for this flag are any integer


- `use_naive_trie`
        Flag telling whether to use the naive algorithm to generate bdd scripts.
The possible values for this flag are true or false.


- `use_old_trie`

    Flag telling whether to use the old not nested trie to trie
transformation. The possible values for this flag are true or false.


- `use_dec_trie`

    Flag telling whether to use the decomposition method. The possible
values for this flag are true or false.


- `subset_check`
    Flag telling whether to perform subset check in nested tries. The
possible values for this flag are true or false.


- `deref_terms`
    Flag telling whether to dereference BDD terms after their last use. The
possible values for this flag are true or false.


- `trie_preprocess`
    Flag telling whether to perform a preprocess step to nested tries. The
possible values for this flag are true or false.


- `refine_anclst`
    Flag telling whether to refine the ancestor list with their children.
The possible values for this flag are true or false.


- `anclst_represent`
    Flag that sets the representation of the ancestor list. The possible
values for this flag are list or integer


- `max_depth`
    Sets the maximum proof depth. The possible values for this flag are any
integer.


- `retain_tables`
    Flag telling whether to retain tables after the query. The possible
values for this flag are true or false.


- `mc_batchsize`
    Flag related to Monte Carlo Sampling that sets the number of samples
before update. The possible values for this flag are any integer greater
than zero.


- `min_mc_samples`
    Flag related to Monte Carlo Sampling that sets the minimum number of
samples before convergence. The possible values for this flag are any
integer greater than or equal to zero.


- `max_mc_samples`
    Flag related to Monte Carlo Sampling that sets the maximum number of
samples waiting to converge. The possible values for this flag are any
integer greater than or equal to zero.


- `randomizer`
    Flag related to Monte Carlo Sampling telling whether the random numbers
are repeatable or not. The possible values for this flag are repeatable
or nonrepeatable.


- `search_method`
    Flag related to DNF Monte Carlo Sampling that sets the search method for
picking the proof. The possible values for this flag are linear or
binary.


- `represent_world`
    Flag related to Monte Carlo Sampling that sets the structure that
represents sampled world. The possible values for this flag are list,
record, array or hash_table


- `first_threshold`
    Flag related to inference that sets the starting threshold of iterative
deepening. The possible values for this flag are a number in the
interval (0,1).


- `last_threshold`
    Flag related to inference that sets the stopping threshold of iterative
deepening. The possible values for this flag are a number in the
interval (0,1).


- `id_stepsize`
    Flag related to inference that sets the threshold shrinking factor of
iterative deepening. The possible values for this flag are a number in
the interval \[0,1\].


- `prunecheck`
    Flag related to inference telling whether to stop derivations including
all facts of known proofs. The possible values for this flag are on or
off.


- `maxsteps`
    Flag related to inference that sets the max. number of prob. steps per
derivation. The possible values for this flag are any integer greater
than zero.


- `mc_logfile`
    Flag related to MCMC that sets the logfile for montecarlo. The possible
values for this flag are any valid filename.


- `bdd_time`
    Flag related to BDD that sets the BDD computation timeout in seconds.
The possible values for this flag are any integer greater than zero.


- `bdd_par_file`
    Flag related to BDD that sets the file for BDD variable parameters. The
possible values for this flag are any valid filename.


- `bdd_result`
    Flag related to BDD that sets the file to store result calculated from
BDD. The possible values for this flag are any valid filename.


- `bdd_file`
    Flag related to BDD that sets the file for the BDD script. The possible
values for this flag are any valid filename.


- `save_bdd`
    Flag related to BDD telling whether to save BDD files for (last) lower
bound. The possible values for this flag are true or false.


- `dynamic_reorder`
    Flag related to BDD telling whether to use dynamic re-ordering for BDD.
The possible values for this flag are true or false.


- `bdd_static_order`
    Flag related to BDD telling whether to use static order. The possible
values for this flag are true or false.


- `static_order_file`
    Flag related to BDD that sets the file for BDD static order. The
possible values for this flag are any valid filename.


- `verbose`
    Flag telling whether to output intermediate information. The possible
values for this flag are true or false.


- `show_proofs`
    Flag telling whether to output proofs. The possible values for this flag
are true or false.


- `triedump`
    Flag telling whether to generate the file: trie_file containing the
trie structure. The possible values for this flag are true or false.


- `dir`
    Flag telling the location of the output files directory. The possible
values for this flag are any valid directory name.

set_problog_flag
------------------

@pred set_problog_flag(+Name, +Value)



This predicate sets the value of the given flag. The supported flags are
the ones listed in above.

learning_flags
---------------

@pred learning_flags



This predicate lists all the learning flags name, value, domain and
description.

learning_flag
--------------

@pred learning_flag(+Name, -Value)



This predicate gives the value of the learning flag with the specified
name. The supported flags are:


- `output_directory`
    Flag setting the directory where to store results. The possible values
for this flag are any valid path name.


- `query_directory`
    Flag setting the directory where to store BDD files. The possible values
for this flag are any valid path name.


- `verbosity_level`
    Flag telling how much output shall be given. The possible values for
this flag are an integer between 0 and 5 (0=nothing, 5=all).


- `reuse_initialized_bdds`
    Flag telling whether to reuse BDDs from previous runs. The possible
values for this flag are true or false.


- `rebuild_bdds`
    Flag telling whether to rebuild BDDs every nth iteration. The possible
values for this flag are any integer greater or equal to zero (0=never).


- `check_duplicate_bdds`
    Flag telling whether to store intermediate results in hash table. The
possible values for this flag are true or false.


- `init_method`
    Flag setting the ProbLog predicate to search proofs. The possible values
for this flag are of the form: (+Query,-P,+BDDFile,+ProbFile,+Call). For
example: A,B,C,D,problog_kbest_save(A,100,B,E,C,D)


- `probability_initializer`
    Flag setting the ProbLog predicate to initialize probabilities. The
possible values for this flag are of the form: (+FactID,-P,+Call). For
example: A,B,random_probability(A,B)


- `log_frequency`
    Flag telling whether to log results every nth iteration. The possible
values for this flag are any integer greater than zero.


- `alpha`
    Flag setting the weight of negative examples. The possible values for
this flag are number or \"auto\" (auto=n_p/n_n).


- `slope`
    Flag setting the slope of the sigmoid function. The possible values for
this flag are any real number greater than zero.


- `learning_rate`
    Flag setting the default Learning rate (if line_search=false) The
possible values for this flag are any number greater than zero or
\"examples"


- `line_search`
    Flag telling whether to use line search to estimate the learning rate.
The possible values for this flag are true or false.


- `line_search_tau`
    Flag setting the Tau value for line search. The possible values for this
flag are a number in the interval (0,1).


- `line_search_tolerance`
    Flag setting the tolerance value for line search. The possible values
for this flag are any number greater than zero.


- `line_search_interval`
    Flag setting the interval for line search. The possible values for this
flag are of the form a,b where a and b are numbers and define an
interval with $0<=a<b$


- `line_search_never_stop`
    Flag telling whether to make tiny step if line search returns 0. The
possible values for this flag are true or false.

set_learning_flag
-------------------

@pred set_learning_flag(+Name, +Value)



This predicate sets the value of the given learning flag. The supported
flags are the ones listed in above.

Further Help
============



To access the help information in ProbLog type:

@pred problog_help.
