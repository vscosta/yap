%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2009-07-31 14:57:09 +0200 (Fri, 31 Jul 2009) $
%  $Revision: 1826 $
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

:- module(flags_learning, [set_learning_flag/2,
	learning_flag/2,
	learning_flags/0]).

:- style_check(all).
:- yap_flag(unknown,error).

:- use_module(logger).
:- use_module('../problog/flags').
:- use_module('../problog/print').

:- ensure_loaded(library(system)).

:- dynamic init_method/5.
:- dynamic rebuild_bdds/1.
:- dynamic reuse_initialized_bdds/1.
:- dynamic learning_rate/1.
:- dynamic probability_initializer/3.
:- dynamic check_duplicate_bdds/1.
:- dynamic output_directory/1.
:- dynamic query_directory/1.
:- dynamic log_frequency/1.
:- dynamic alpha/1.
:- dynamic sigmoid_slope/1.
:- dynamic line_search/1.
:- dynamic line_search_tolerance/1.
:- dynamic line_search_tau/1.
:- dynamic line_search_never_stop/1.
:- dynamic line_search_interval/2.
:- dynamic verbosity_level/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% global parameters that can be set using set_learning_flag/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

learning_flag(Flag,Option) :-
	get_learning_flag(Flag,Option).

get_learning_flag(init_method,(Query,Probability,BDDFile,ProbFile,Call)) :-
	init_method(Query,Probability,BDDFile,ProbFile,Call).

get_learning_flag(rebuild_bdds,Iteration) :-
	rebuild_bdds(Iteration).

get_learning_flag(reuse_initialized_bdds,Flag) :-
	reuse_initialized_bdds(Flag).

get_learning_flag(learning_rate,R) :-
	learning_rate(R).

get_learning_flag(probability_initializer,(FactID,Probability,Query)) :-
	probability_initializer(FactID,Probability,Query).

get_learning_flag(check_duplicate_bdds,Flag) :-
	check_duplicate_bdds(Flag).

get_learning_flag(output_directory,Directory) :-
	output_directory(Directory).

get_learning_flag(query_directory,Directory) :-
	query_directory(Directory).

get_learning_flag(log_frequency,Frequency) :-
	log_frequency(Frequency).

get_learning_flag(alpha,Alpha) :-
	alpha(Alpha).

get_learning_flag(sigmoid_slope,Slope) :-
	sigmoid_slope(Slope).

get_learning_flag(line_search,Flag) :-
	line_search(Flag).

get_learning_flag(line_search_tolerance,Tolerance) :-
	line_search_tolerance(Tolerance).

get_learning_flag(line_search_interval,(L,R)) :-
	line_search_interval(L,R).

get_learning_flag(line_search_tau,Tau) :-
	line_search_tau(Tau).

get_learning_flag(line_search_never_stop,Flag) :-
	line_search_never_stop(Flag).

get_learning_flag(verbosity_level,Number) :-
	verbosity_level(Number).




set_learning_flag(init_method,(Query,Probability,BDDFile,ProbFile,Call)) :-
	retractall(init_method(_,_,_,_,_)),
	assert(init_method(Query,Probability,BDDFile,ProbFile,Call)).


set_learning_flag(rebuild_bdds,Frequency) :-
	integer(Frequency),
	Frequency>=0,
	retractall(rebuild_bdds(_)),
	assert(rebuild_bdds(Frequency)).


set_learning_flag(reuse_initialized_bdds,Flag) :-
	(Flag==true;Flag==false),
	!,
	retractall(reuse_initialized_bdds(_)),
	assert(reuse_initialized_bdds(Flag)).

set_learning_flag(learning_rate,V) :-
	(V=examples -> true;(number(V),V>=0)),
	!,
	retractall(learning_rate(_)),
	assert(learning_rate(V)).

set_learning_flag(probability_initializer,(FactID,Probability,Query)) :-
	var(FactID),
	var(Probability),
	callable(Query),
	retractall(probability_initializer(_,_,_)),
	assert(probability_initializer(FactID,Probability,Query)).

set_learning_flag(check_duplicate_bdds,Flag) :-
	(Flag==true;Flag==false),
	!,
	retractall(check_duplicate_bdds(_)),
	assert(check_duplicate_bdds(Flag)).

set_learning_flag(output_directory,Directory) :-
	(
	    file_exists(Directory)
	->
	    file_property(Directory,type(directory));
	    make_directory(Directory)
	),

	absolute_file_name(Directory,Path),
	atomic_concat([Path,'/'],PathSlash),
	atomic_concat([Path,'/log.dat'],Log_File),
		
	retractall(output_directory(_)),
	assert(output_directory(PathSlash)),
	logger_set_filename(Log_File),
	set_problog_flag(dir,Directory).

set_learning_flag(query_directory,Directory) :-
	(
	    file_exists(Directory)
	->
	    file_property(Directory,type(directory));
	    make_directory(Directory)
	),
	absolute_file_name(Directory,Path),
	atomic_concat([Path,'/'],PathSlash),
	retractall(query_directory(_)),
	assert(query_directory(PathSlash)).

set_learning_flag(log_frequency,Frequency) :-
	integer(Frequency),
	Frequency>=0,
	retractall(log_frequency(_)),
	assert(log_frequency(Frequency)).

set_learning_flag(alpha,Alpha) :-
	(number(Alpha);Alpha==auto),
	!,
	retractall(alpha(_)),
	assert(alpha(Alpha)).
set_learning_flag(sigmoid_slope,Slope) :-
	number(Slope),
	Slope>0,
	retractall(sigmoid_slope(_)),
	assert(sigmoid_slope(Slope)).


set_learning_flag(line_search,Flag) :-
	(Flag==true;Flag==false),
	!,
	retractall(line_search(_)),
	assert(line_search(Flag)).
set_learning_flag(line_search_tolerance,Number) :-
	number(Number),
	Number>0,
	retractall(line_search_tolerance(_)),
	assert(line_search_tolerance(Number)).
set_learning_flag(line_search_interval,(L,R)) :-
	number(L),
	number(R),
	L<R,
	retractall(line_search_interval(_,_)),
	assert(line_search_interval(L,R)).
set_learning_flag(line_search_tau,Number) :-
	number(Number),
	Number>0,
	retractall(line_search_tau(_)),
	assert(line_search_tau(Number)).
set_learning_flag(line_search_never_stop,Flag) :-
	(Flag==true;Flag==false),
	!,
	retractall(line_search_nerver_stop(_)),
	assert(line_search_never_stop(Flag)).

set_learning_flag(verbosity_level,Level) :-
	integer(Level),
	retractall(verbosity_level(_)),
	assert(verbosity_level(Level)),
	(
	 Level<4
	->
	 set_problog_flag(verbose,false);
	 set_problog_flag(verbose,true)
	).
	


%%%%%%%%%%%%%%%%%%%%%%%%
% show values
%%%%%%%%%%%%%%%%%%%%%%%%

skolemize(T1,T2):-
	copy_term(T1,T2),
	numbervars(T2,0,_).

learning_flags :-
	format('~n',[]),
	print_sep_line,
	format('learning flags: use set_learning_flag(Flag,Option) to change, learning_flag(Flag,Option) to view~n',[]),
	print_sep_line,
	print_param(description,value,flag,option),
	print_sep_line,

	learning_flag(output_directory,Output_Directory),
	print_long_param('Where to store results',Output_Directory,'output_directory','path'),

	learning_flag(query_directory,Query_Directory),
	print_long_param('Where to store BDD files',Query_Directory,'query_directory','path'),

	learning_flag(verbosity_level,Verbosity_Level),
	print_param('How much output shall be given (0=nothing,5=all)',Verbosity_Level,'verbosity_level','0,1,..,5'),

	print_sep_line,

	learning_flag(reuse_initialized_bdds,Reuse_Initialized_Bdds),
	print_param('Reuse BDDs from previous runs',Reuse_Initialized_Bdds,'reuse_initialized_bdds','true/false'),

	learning_flag(rebuild_bdds,Rebuild_BDDs),
	print_param('Rebuild BDDs every nth iteration (0=never)',Rebuild_BDDs,'rebuild_bdds','Integer>=0'),
	learning_flag(check_duplicate_bdds,Check_Duplicate_BDDs),
	print_param('Store intermediate results in hash table',Check_Duplicate_BDDs,'check_duplicate_bdds','true/false'),

	learning_flag(init_method,Init_Method),
	skolemize(Init_Method,Init_Method_SK),
	print_long_param('ProbLog predicate to search proofs',Init_Method_SK,'init_method','(+Query,-P,+BDDFile,+ProbFile,+Call)'),

	learning_flag(probability_initializer,Prob_Initializer),
	skolemize(Prob_Initializer,Prob_Initializer_SK),
	print_long_param('Predicate to initialize probabilities',Prob_Initializer_SK,'probability_initializer','(+FactID,-P,+Call)'),

	print_sep_line,


	learning_flag(log_frequency,Log_Frequency),
	print_param('log results every nth iteration',Log_Frequency,'log_frequency','integer>0'),

	learning_flag(alpha,Alpha),
	print_param('weight of negative examples (auto=n_p/n_n)',Alpha,'alpha','number or "auto"'),

	learning_flag(sigmoid_slope,Slope),
	print_param('slope of sigmoid function',Slope,'slope','number>0'),

	print_sep_line,

	
	learning_flag(learning_rate,Learning_Rate),
	print_param('Default Learning rate (If line_search=false)',Learning_Rate,'learning_rate','0<Number or "examples"'),
	learning_flag(line_search,Line_Search),
	print_param('Use line search to estimate learning rate',Line_Search,'line_search','true/false'),
	learning_flag(line_search_tau,Line_Search_Tau),
	print_param('Tau value for line search',Line_Search_Tau,'line_search_tau','0<Number<1'),
	learning_flag(line_search_tolerance,Line_Search_Tolerance),
	print_param('Tolerance value for line search',Line_Search_Tolerance,'line_search_tolerance','0<Number'),
	learning_flag(line_search_interval,Line_Search_Interval),
	print_param('Interval for line search',Line_Search_Interval,'line_search_interval','(a,b) an interval with 0<=a<b'),
	learning_flag(line_search_never_stop,Line_Search_Never_Stop),
	print_param('Make tiny step if line search returns 0',Line_Search_Never_Stop,'line_search_never_stop','true/false'),
	
	print_sep_line,
	
	format('~n',[]),
	flush_output.

