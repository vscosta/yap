%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-12-06 21:50:45 +0100 (Tue, 06 Dec 2011) $
%  $Revision: 6772 $
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

:- module(utils, [delete_file_silently/1,
		  delete_files_silently/1,
		  delete_file_pattern_silently/2,
		  variable_in_term_exactly_once/2,
		  slice_n/4,
		  sorted_overlap_test/2,
		  prefix_bdd_file_with_header/4,
		  split_list/3,
		  succeeds_n_times/2,
		  sum_forall/3]).

:- use_module(library(system), [delete_file/1]).

% load our own modules
:- use_module(os).

:- meta_predicate succeeds_n_times(0,?), sum_forall(?,:,?).


%========================================================================
%= deletes File, if it doesn't exists, it will succeed silently
%= delete_file_silently(+File)
%========================================================================

delete_file_silently(File) :-
	catch(delete_file(File),_, fail),
	!.
delete_file_silently(_).


%========================================================================
%= delete all the files in the list silently
%= delete_files_silently(+List)
%========================================================================

delete_files_silently([]).
delete_files_silently([H|T]) :-
	delete_file_silently(H),
	delete_files_silently(T).


%========================================================================
%= delete all the files matching a certain pattern silently
%=  i.e. delete_file_pattern_silently('~/a_path/b_path/','*.txt')
%=
%= delete_file_pattern_silently(+Path,+Pattern)
%========================================================================

delete_file_pattern_silently(Path,Pattern) :-
	concat_path_with_filename(Path,Pattern,AbsolutePattern),
	expand_file_name(AbsolutePattern,Files),
	delete_files_silently(Files).


%========================================================================
%= Split a list into the first n elements and the tail
%= slice_n(+List, +Integer, -Prefix, -Residuum)
%========================================================================

slice_n([],_,[],[]) :-
	!.
slice_n([H|T],N,[H|T2],T3) :-
	N>0,
	!,
	N2 is N-1,
	slice_n(T,N2,T2,T3).
slice_n(L,0,[],L).


%========================================================================
%= succeeds if the variable V appears exactly once in the term T
%========================================================================

variable_in_term_exactly_once(T,V) :-
	term_variables(T,Vars),
	var_memberchk_once(Vars,V).

var_memberchk_once([H|T],V) :-
	H==V,
	!,
	var_memberchk_none(T,V).
var_memberchk_once([_|T],V) :-
	var_memberchk_once(T,V).

var_memberchk_none([H|T],V) :-
	H\==V,
	var_memberchk_none(T,V).
var_memberchk_none([],_).


%========================================================================
%= sorted_overlap_test(+L1,+L2)
%= L1 and L2 are ground sorted lists
%= the predicate is true if there is an element X both appearing in L1 and L2
%========================================================================

sorted_overlap_test([H|_],[H|_]) :-
	!.
sorted_overlap_test([H1|T1],[H2|T2]) :-
	H1 @> H2,
	!,
	sorted_overlap_test([H1|T1],T2).
sorted_overlap_test([_|T1],[H2|T2]) :-
	sorted_overlap_test(T1,[H2|T2]).

%========================================================================
%= TmpFile is the file name that contains the "body" of a SimpleCUDD
%= script file. This predicate creates a new file BDD_File_Name that
%= starts with the right header and contains the body. The body file
%= is deleted afterwards.
%=
%= prefix_bdd_file_with_header(+BDD_File_Name,+VarCount,+IntermediateSteps,+TmpFile)
%========================================================================

prefix_bdd_file_with_header(BDD_File_Name,VarCount,IntermediateSteps,TmpFile) :-
	open(BDD_File_Name,write,H),
	% this is the header of the BDD script for problogbdd
	format(H, '@BDD1~n~w~n0~n~w~n',[VarCount,IntermediateSteps]),

	% append the content of the file TmpFile
	open(TmpFile,read,H2),

	(
	 repeat,
	 get_code(H2,C),
	 put_code(H,C),
	 at_end_of_stream(H2),
	 !
	),
	close(H2),
	
	close(H),
	delete_file_silently(TmpFile).


%========================================================================
%= Split the list L in the two lists L1 and L2 such that
%= append(L1,L2,L) holds.
%=
%= if length of L is even, then L1 and L2 will have the same length
%= if length of L is odd, then L1 will be one element longer than L2
%=
%= split_list(+L,-L1,-L2)
%========================================================================

split_list([],[],[]).
split_list([H|T],L1,L2) :-
	length([H|T],Len),
	Len1 is integer(Len/2+0.5),
	split_list_intern(Len1,[H|T],L1,L2).

split_list_intern(0,L,[],L).
split_list_intern(N,[H|T],[H|T1],L) :-
	N>0,
	N2 is N-1,
	split_list_intern(N2,T,T1,L).

%========================================================================
%= Counts how often Goal succeeds
%= (taken from the YAP manual)
%========================================================================

succeeds_n_times(Goal, Times) :- 
	Counter = counter(0), 
	( Goal, 
	  arg(1, Counter, N0), 
	  N is N0 + 1, 
	  nb_setarg(1, Counter, N), 
	  fail 
	; arg(1, Counter, Times) 
	).


%========================================================================
%=
%=
%========================================================================

sum_forall(X,Goal, Sum) :- 
	Temp = sum(0), 
	( Goal, 
	  arg(1, Temp, Sum0),
	  Sum is Sum0+X,
	  nb_setarg(1, Temp, Sum), 
	  fail 
	; arg(1, Temp, Sum) 
	).