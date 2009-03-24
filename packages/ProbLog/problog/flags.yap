%%% -*- Mode: Prolog; -*-

:- module(flags, [set_problog_flag/2,
	problog_flag/2,
	problog_flags/0]).

:- style_check(all).
:- yap_flag(unknown,error).

:- use_module(print, [print_param/4,
	print_sep_line/0]).

:- ensure_loaded(library(system)).

:- dynamic bdd_time/1, first_threshold/1, last_threshold/1, id_stepsize/1, prunecheck/1, maxsteps/1, mc_batchsize/1, mc_logfile/1, bdd_file/1, bdd_par_file/1, bdd_result/1, work_dir/1, save_bdd/1, problog_verbose/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% global parameters that can be set using set_problog_flag/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_flag(Flag,Option) :-
	get_problog_flag(Flag,Option).
get_problog_flag(bdd_time,X) :-
	bdd_time(X).
get_problog_flag(first_threshold,X) :-
	first_threshold(X).
get_problog_flag(last_threshold,X) :-
	last_threshold(L),
	X is exp(L).
get_problog_flag(last_threshold_log,X) :-
	last_threshold(X).
get_problog_flag(id_stepsize,X) :-
	id_stepsize(L),
	X is exp(L).
get_problog_flag(id_stepsize_log,X) :-
	id_stepsize(X).
get_problog_flag(prunecheck,X) :-
	prunecheck(X).
get_problog_flag(maxsteps,X) :-
	maxsteps(X).
get_problog_flag(mc_batchsize,X) :-
	mc_batchsize(X).
get_problog_flag(mc_logfile,X) :-
	mc_logfile(X).
get_problog_flag(bdd_file,X) :-
	bdd_file(X).
get_problog_flag(bdd_par_file,X) :-
	bdd_par_file(X).
get_problog_flag(bdd_result,X) :-
	bdd_result(X).
get_problog_flag(dir,X) :-
	work_dir(X).
get_problog_flag(save_bdd,X) :-
	save_bdd(X).
get_problog_flag(verbose,X) :-
	problog_verbose(X).


%%%%%%%%%%%%
% BDD timeout in seconds, used as option in BDD tool
%%%%%%%%%%%%

set_problog_flag(bdd_time,X) :-
	(\+ integer(X); X<0),
	!,
	format(user,'\% ERROR: value must be positive integer!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(bdd_time,X) :-
	retractall(bdd_time(_)),
	assert(bdd_time(X)).


%%%%%%%%%%%%
% iterative deepening on minimal probabilities (delta, max, kbest):
% - first threshold (not in log-space as only used to retrieve argument for init_threshold/1, which is also used with user-supplied argument)
% - last threshold to ensure termination in case infinite search space (saved in log-space for easy comparison with current values during search)
% - factor used to decrease threshold for next level, NewMin=Factor*OldMin (saved in log-space)
%%%%%%%%%%%%

set_problog_flag(first_threshold,X) :-
	(\+ number(X); X<0 ; X>1),
	!,
	format(user,'\% ERROR: value must be in [0,1]!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(first_threshold,X) :-
	retractall(first_threshold(_)),
	assert(first_threshold(X)).

set_problog_flag(last_threshold,X) :-
	(\+ number(X); X<0 ; X>1),
	!,
	format(user,'\% ERROR: value must be in [0,1]!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(last_threshold,X) :-
	retractall(last_threshold(_)),
	L is log(X),
	assert(last_threshold(L)).

set_problog_flag(id_stepsize,X) :-
	(\+ number(X); X=<0 ; X>=1),
	!,
	format(user,'\% ERROR: value must be in ]0,1[!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(id_stepsize,X) :-
	retractall(id_stepsize(_)),
	L is log(X),
	assert(id_stepsize(L)).


%%%%%%%%%%%%
% prune check stops derivations if they use a superset of facts already known to form a proof
% (very) costly test, can be switched on/off here
%%%%%%%%%%%%

set_problog_flag(prunecheck,on) :-
	!,
	format(user,'WARNING: prune check not implemented, will fail~n',[]),
	flush_output(user),
	retractall(prunecheck(_)),
	assert(prunecheck(on)).
set_problog_flag(prunecheck,off) :-
	!,
	retractall(prunecheck(_)),
	assert(prunecheck(off)).
set_problog_flag(prunecheck,_) :-
	format(user,'\% ERROR: value must be \'on\' or \'off\'!~n',[]),
	flush_output(user),
	fail.

%%%%%%%%%%%%
% max number of calls to probabilistic facts per derivation (to ensure termination)
%%%%%%%%%%%%

set_problog_flag(maxsteps,X) :-
	(\+ integer(X); X<0),
	!,
	format(user,'\% ERROR: value must be positive integer!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(maxsteps,X) :-
	retractall(maxsteps(_)),
	assert(maxsteps(X)).


%%%%%%%%%%%%
% montecarlo: recalculate current approximation after N samples
%%%%%%%%%%%%

set_problog_flag(mc_batchsize,X) :-
	(\+ integer(X); X<0),
	!,
	format(user,'\% ERROR: value must be positive integer!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(mc_batchsize,X) :-
	retractall(mc_batchsize(_)),
	assert(mc_batchsize(X)).

%%%%%%%%%%%%
% montecarlo: write log to this file
%%%%%%%%%%%%

set_problog_flag(mc_logfile,X) :-
	\+ atom(X),
	!,
	format(user,'\% ERROR: value must be atom!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(mc_logfile,X) :-
	retractall(mc_logfile(_)),
	assert(mc_logfile(X)).

%%%%%%%%%%%%
% files to write BDD script and pars
% bdd_file overwrites bdd_par_file with matching extended name
% if different name wanted, respect order when setting
%%%%%%%%%%%%

set_problog_flag(bdd_file,X) :-
	\+ atom(X),
	!,
	format(user,'\% ERROR: value must be atom!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(bdd_file,X) :-
	retractall(bdd_file(_)),
	atomic_concat(X,'_probs',Y),
	set_problog_flag(bdd_par_file,Y),
	atomic_concat(X,'_res',Z),
	set_problog_flag(bdd_result,Z),
	assert(bdd_file(X)).
set_problog_flag(bdd_par_file,X) :-
	\+ atom(X),
	!,
	format(user,'\% ERROR: value must be atom!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(bdd_par_file,X) :-
	retractall(bdd_par_file(_)),
	assert(bdd_par_file(X)).
set_problog_flag(bdd_result,X) :-
	\+ atom(X),
	!,
	format(user,'\% ERROR: value must be atom!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(bdd_result,X) :-
	retractall(bdd_result(_)),
	assert(bdd_result(X)).

%%%%%%%%%%%%
% working directory: all the temporary and output files will be located there
% it assumes a subdirectory of the current working dir
% on initialization, the current dir is the one where the user's file is located
%%%%%%%%%%%%
set_problog_flag(dir,X) :-
	\+ atom(X),
	!,
	format(user,'\% ERROR: value must be atom!~n',[]),
	flush_output(user),
	fail.
set_problog_flag(dir,X) :-
	retractall(work_dir(_)),
	working_directory(PWD,PWD),
	atomic_concat([PWD,'/',X,'/'],D),
	atomic_concat(['mkdir ',D],Mkdir),
	(file_exists(X) -> true; shell(Mkdir)),
	assert(work_dir(D)).

%%%%%%%%%%%%
% save BDD information for the (last) lower bound BDD used during inference 
% produces three files named save_script, save_params, save_map
% located in the directory given by problog_flag dir
%%%%%%%%%%%%

set_problog_flag(save_bdd,true) :-
	!,
	retractall(save_bdd(_)),
	assert(save_bdd(true)).
set_problog_flag(save_bdd,false) :-
	!,
	retractall(save_bdd(_)),
	assert(save_bdd(false)).
set_problog_flag(save_bdd,_) :-
	format(user,'\% ERROR: value must be \'true\' or \'false\'!~n',[]),
	flush_output(user),
	fail.

%%%%%%%%%%%%
% determine whether ProbLog outputs information (number of proofs, intermediate results, ...) 
% default is true, as otherwise problog_delta won't output intermediate bounds
%%%%%%%%%%%%

set_problog_flag(verbose,true) :-
	!,
	retractall(problog_verbose(_)),
	assert(problog_verbose(true)).
set_problog_flag(verbose,false) :-
	!,
	retractall(problog_verbose(_)),
	assert(problog_verbose(false)).
set_problog_flag(verbose,_) :-
	format(user,'\% ERROR: value must be \'true\' or \'false\'!~n',[]),
	flush_output(user),
	fail.


%%%%%%%%%%%%%%%%%%%%%%%%
% show values
%%%%%%%%%%%%%%%%%%%%%%%%

problog_flags :-
	format('~n',[]),
	print_sep_line,
	format('problog flags: use set_problog_flag(Flag,Option) to change, problog_flag(Flag,Option) to view~n',[]),
	print_sep_line,
	print_param(description,value,flag,option),
	print_sep_line,
	problog_flag(bdd_time,StopBDD),
	print_param('BDD computation timeout in seconds',StopBDD,'bdd_time','positive integer'),
	problog_flag(first_threshold,First),
	print_param('starting threshold iterative deepening',First,'first_threshold','0 =< Option =< 1'),
	problog_flag(last_threshold,Last),
	print_param('stopping threshold iterative deepening',Last,'last_threshold','0 =< Option =< 1'),
	problog_flag(id_stepsize,Decrease),
	print_param('threshold shrinking factor iterative deepening',Decrease,'id_stepsize','0 < Option < 1'),
	problog_flag(prunecheck,Check),
	print_param('stop derivations including all facts of known proof',Check,'prunecheck','on/off'),
	problog_flag(maxsteps,Steps),
	print_param('max. number of prob. steps per derivation',Steps,'maxsteps','positive integer'),
	problog_flag(mc_batchsize,MCBatch),
	print_param('number of samples before update in montecarlo',MCBatch,'mc_batchsize','positive integer'),
	problog_flag(mc_logfile,MCFile),
	print_param('logfile for montecarlo',MCFile,'mc_logfile','atom'),
	problog_flag(bdd_file,BDDFile),
	print_param('file for BDD script',BDDFile,'bdd_file','atom'),
	problog_flag(dir,WorkDir),
	print_param('directory for files',WorkDir,'dir','atom'),
	problog_flag(save_bdd,Save),
	print_param('save BDD files for (last) lower bound',Save,'save_bdd','true/false'),
	problog_flag(verbose,Verbose),
	print_param('output intermediate information',Verbose,'verbose','true/false'),
	print_sep_line,
	format('~n',[]),
	flush_output.

