%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
				%  $Date: 2011-11-28 14:41:26 +0100 (Mon, 28 Nov 2011) $
%  $Revision: 6764 $
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


:- module(logger,[logger_define_variable/2,
	          logger_set_filename/1,
		  logger_set_delimiter/1,
		  logger_set_variable/2,
		  logger_set_variable_again/2,
		  logger_get_variable/2,
		  logger_start_timer/1,
		  logger_stop_timer/1,
		  logger_write_data/0,
		  logger_write_header/0,
		  logger_variable_is_set/1,
		  logger_add_to_variable/2,
		  logger_reset_all_variables/0]).

:- use_module(library(system),[datime/1,mktime/2]).
:- use_module(library(lists),[append/3,member/2]).

:- yap_flag(unknown,error).
:- style_check(single_var).
:- initialization((
	bb_put(logger_filename,'out.dat'),
	bb_put(logger_delimiter,';'),
  bb_put(logger_variables,[])
                  )).

%========================================================================
%= Defines a new variable, possible types are: int, float and time
%=
%= +Name, +Type
%========================================================================

logger_define_variable(Name,Type) :-
	bb_get(logger_variables,Variables),
	member((Name,Type0),Variables),
	!,
	( Type == Type0
	->
		write('redefining logger variable '),write(Name),write(' of type '), write(Type0), nl
		;
	throw(error(variable_redefined(logger_define_variable(Name,Type))))
	).
logger_define_variable(Name,Type) :-
	ground(Type),
	atomic(Name),
	!,
	logger_define_variable_intern(Type,Name).
logger_define_variable(Name,Type) :-
	throw(error(illegal_variable_name(logger_define_variable(Name,Type)))).

logger_define_variable_intern(int,Name) :-
	!,
	bb_delete(logger_variables,OldVariables),
	append(OldVariables,[(Name,int)],NewVariables),
	bb_put(logger_variables,NewVariables),
	atom_concat(logger_data_,Name,Key),
	bb_put(Key,null).
logger_define_variable_intern(float,Name) :-
	!,
	bb_delete(logger_variables,OldVariables),
	append(OldVariables,[(Name,float)],NewVariables),
	bb_put(logger_variables,NewVariables),
	atom_concat(logger_data_,Name,Key),
	bb_put(Key,null).
logger_define_variable_intern(time,Name) :-
	!,
	bb_delete(logger_variables,OldVariables),
	append(OldVariables,[(Name,time)],NewVariables),
	bb_put(logger_variables,NewVariables),
	atom_concat(logger_data_,Name,Key),
	atom_concat(logger_start_time_,Name,Key2),
	bb_put(Key,null),
	bb_put(Key2,null).
logger_define_variable_intern(Type,Name) :-
	throw(error(unknown_variable_type(logger_define_variable(Name,Type)))).


%========================================================================
%= Set the filename, to which the output should be appended
%=
%= +Name
%========================================================================

logger_set_filename(Name) :-
	bb_put(logger_filename,Name).

%========================================================================
%= Set the delimiter for the fields
%=
%= +Delimiter
%========================================================================

logger_set_delimiter(Delimiter) :-
	bb_put(logger_delimiter,Delimiter).
%========================================================================
%= Set the value of the variable name. If the value is already set or
%= if the variable does not exists, an error will be displayed and the
%= Prolog will be halted.
%=
%= +Name, +Value
%========================================================================

logger_set_variable(Name,Value) :- logger_set_variable_again(Name,Value).
	/*
	atom_concat(logger_data_,Name,Key),
	(
	    bb_get(Key,null)
	->
	    (
		bb_put(Key,Value)
	    );(
	         bb_get(Key,_)
	      ->
	         (
				true
		     % write('logger_set_variable, Variable '),
		     % write(Name),
		     % write(' is already set'),
		     % nl %,
		     % fail
		 ) ; (
		     write('logger_set_variable, unknown variable '),
		     write(Name),
		     nl,
		     fail
		     )
		 )
	),!.
*/
%========================================================================
%= Set the value of the variable name. If the value is already set or
%= the old value is overwritten. If the variable does not exists, an
%= error will be displayed and the Prolog will be halted.
%=
%= +Name, +Value
%========================================================================

logger_set_variable_again(Name,Value) :-
	atom_concat(logger_data_,Name,Key),
	(
	    bb_get(Key,_)
	->
	    (
		bb_put(Key,Value)
	    );(
	         write('logger_set_variable, unknown variable '),
                 write(Name),
     		 nl,
		 fail
               )
	),!.


logger_variable_is_set(Name) :-
	atom_concat(logger_data_,Name,Key),
	bb_get(Key,X),
	X \= null.

logger_add_to_variable(Name,Value) :-
	(
	 logger_variable_is_set(Name)
	->
	 (
	  logger_get_variable(Name,OldValue),
	  NewValue is OldValue+Value,
	  logger_set_variable_again(Name,NewValue)
	 );
	 logger_set_variable(Name,Value)
	).

%========================================================================
%= Get the value of the variable name. If the value is not yet set or
%= if the variable does not exists, an error will be displayed and the
%= Prolog will be halted.
%=
%= +Name, +Value
%========================================================================

logger_get_variable(Name,Value) :-
	atom_concat(logger_data_,Name,Key),
	(
	    bb_get(Key,null)
	->
	    (
		write('logger_get_variable, Variable '),
		write(Name),
		write(' is not yet set'),
		nl,
		fail
	    );(
	         bb_get(Key,Value)
	         ;
		  (
		      write('logger_set_variable, unknown variable '),
		      write(Name),
		      nl,
		      fail
		   )
	)
        ),!.
%========================================================================
%=
%=
%= +Name
%========================================================================

logger_start_timer(Name) :-
	atom_concat(logger_start_time_,Name,Key),
	(
	    bb_get(Key,null)
	->
	    (
		statistics(walltime,[StartTime,_]),
		bb_put(Key,StartTime)
	    )
	;
	    (
	     bb_get(Key,_)
	    ->
	     format(user_error, 'logger_start_timer, timer ~a  is already started~n', [Name])
	    ;
	     format(user_error, 'logger_start_timer, timer ~a  is not defined~n', [Name])
	    )
	),
	!.


logger_stop_timer(Name) :-
	atom_concat(logger_start_time_,Name,Key),

	bb_delete(Key,StartTime),
	statistics(walltime,[StopTime,_]),

	bb_put(Key,null),

	Duration is StopTime-StartTime,

	(
	    logger_variable_is_set(Name)
        ->
	    (
		logger_get_variable(Name,OldDuration),
		NewDuration is Duration+OldDuration,
	        logger_set_variable_again(Name,NewDuration)
	    ); logger_set_variable(Name,Duration)
	),!.

%========================================================================
%= write a new line to the log file, which contains all the
%= values of the variables. afterwards, reset all variables to null.
%=
%========================================================================

logger_write_data :-
	bb_get(logger_filename,FName),
	bb_get(logger_variables,Variables),
	open(FName,'append',Handle),
	logger_write_data_intern(Variables,Handle),
	close(Handle),

	logger_reset_all_variables.

logger_write_data_intern([],_).
logger_write_data_intern([(Name,_Type)],Handle) :-
	variablevalue_with_nullcheck(Name,Value),
	write(Handle,Value),
	write(Handle,'\n').
logger_write_data_intern([(Name,_Type),Next|T],Handle) :-
	variablevalue_with_nullcheck(Name,Value),
	bb_get(logger_delimiter,D),
	write(Handle,Value),
	write(Handle,D),
	logger_write_data_intern([Next|T],Handle).

variablevalue_with_nullcheck(Name,Result) :-
	atom_concat(logger_data_,Name,Key),
	bb_get(Key,Value),
	(
	    Value=null
	->
	    Result = '' ;
	    Result=Value
	).
%========================================================================
%=
%=
%=
%========================================================================

logger_reset_all_variables :-
	bb_get(logger_variables,Variables),

	% reset variables
	findall(_,(member((Name,_),Variables),atom_concat(logger_data_,Name,Key),bb_put(Key,null)),_),
	findall(_,(member((Name,time),Variables),atom_concat(logger_start_time_,Name,Key2),bb_put(Key2,null)),_).


%========================================================================
%=
%=
%=
%========================================================================


logger_write_header :-
	bb_get(logger_filename,FName),
	bb_get(logger_variables,Variables),
	open(FName,'append',Handle),
	format(Handle,'#####################################################################~n',[]),
	format(Handle,'# ~w~6+~w~7+~w~n',['Pos','Type','Name']),
	format(Handle,'#####################################################################~n',[]),
	logger_write_header_intern(Variables,1,Handle),
	format(Handle,'#####################################################################~n',[]),
	close(Handle).

logger_write_header_intern([],_,_).
logger_write_header_intern([(Name,Type)|T],Position,Handle) :-
	format(Handle,'# ~q~6+~q~7+~q~n',[Position,Type,Name]),
	Position2 is Position+1,
	logger_write_header_intern(T,Position2,Handle).
