%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2009-06-17 22:22:00 +0200 (Wed, 17 Jun 2009) $
%  $Revision: 1550 $
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
%  Angelika Kimmig, Vitor Santos Costa
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

:- module(flags, [set_problog_flag/2,
	problog_flag/2,
	problog_flags/0]).

:- style_check(all).
:- yap_flag(unknown,error).

:- use_module(print, [print_param/4,
	print_sep_line/0]).

:- ensure_loaded(library(system)).

:- dynamic bdd_time/1, first_threshold/1, last_threshold/1, id_stepsize/1, prunecheck/1, maxsteps/1, mc_batchsize/1, mc_logfile/1, bdd_file/1, bdd_par_file/1, bdd_result/1, work_dir/1, save_bdd/1, problog_verbose/1, hacked_proofs/1.

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
get_problog_flag(hacked_proofs,X) :-
	hacked_proofs(X).


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

set_problog_flag(hacked_proofs,true) :-
	retractall(hacked_proofs(_)),
	assert(hacked_proofs(true)).
set_problog_flag(hacked_proofs,false) :-
	retractall(hacked_proofs(_)),
	assert(hacked_proofs(true)).
set_problog_flag(hacked_proofs,V) :-
	format(user,'\% ERROR: value ~w should be \'true\' or \'false\'!~n',[V]),
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

