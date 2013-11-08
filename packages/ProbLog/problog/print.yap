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
%  Angelika Kimmig, Vitor Santos Costa, Theofrastos Mantadelis
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% printing functions used for problog_help and problog_flags
% collected here to have formatting at one place
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(print, [print_param/4,
                  print_long_param/4,
                  print_sep_line/0,
                  print_sep_line_bold/0,
                  print_group_line/1,
                  print_group_line_bold/1,
                  print_inference/2,
                  problog_statistics/0,
                  show_inference/0,
                  problog_flags/0,
                  problog_flags/1,
                  problog_help/0,
                  problog_version/0]).

% load library modules
:- use_module(library(lists), [member/2]).
:- use_module(library(system), [directory_files/2]).

% load our own modules
:- use_module(flags).
:- use_module(variables).
:- use_module(os, [check_existance/1, convert_filename_to_problog_path/2, concat_path_with_filename2/3]).
:- use_module(version_control, [get_version/3]).


% size, line_char, line_char_bold
problog_pane_properties(125, 45, 61).
problog_pane_split_inference([65,60], [w,w]).
problog_pane_split_stat([40,3,1,1], ['t~w',w,q,w]).
problog_pane_split_param([55,30,20,20], [w,w,w,q]).

print_inference(Call,Description) :-
  problog_pane_split_inference(Columns, Style),
  print_column(Columns, Style, [Call,Description]).
%   format(user,'~w~65+~w~60+~n',[Call,Description]).

print_param(Keyword,Value,Function,Legal) :-
  problog_pane_split_param(Columns, Style),
  print_column(Columns, Style, [Keyword,Value,Function,Legal]).
% 	format(user,'~w~55+~w~29+~w~25+~q~25+~n',[Keyword,Value,Function,Legal]).
print_long_param(Keyword,Value,Function,Legal) :-
	format(user,'~w~55+~q~25+~w~20+~w~25+~n',[Keyword,Value,'','']),
	format(user,'~w~55+~w~25+~w~20+~w~25+~n',['','',Function,Legal]).

print_stat(StatName, Seperator, StatValue, StatUnit) :-
  problog_pane_split_stat(Columns, Style),
  print_column(Columns, Style, [StatName, Seperator, StatValue, StatUnit]).

print_sep_line :-
  problog_pane_properties(Size, LineChar, _LineCharBold),
  format(user,"~*c~n", [Size, LineChar]).
print_sep_line_bold :-
  problog_pane_properties(Size, _LineChar, LineCharBold),
  format(user,"~*c~n", [Size, LineCharBold]).

print_group_line(Group) :-
  atom_length(Group, L),
  problog_pane_properties(Size, LineChar, _LineCharBold),
  Rest is Size - 5 - L,
  format(user,"~*c ~w ~*c~n", [3, LineChar, Group, Rest, LineChar]).
print_group_line_bold(Group) :-
  atom_length(Group, L),
  problog_pane_properties(Size, _LineChar, LineCharBold),
  Rest is Size - 5 - L,
  format(user,"~*c ~w ~*c~n", [3, LineCharBold, Group, Rest, LineCharBold]).

print_column(Columns, Style, Messages):-
  make_column_format(Columns, Style, Format),
  format(user, Format, Messages).

make_column_format(Columns, Style, Format):-
  make_column_format(Columns, Style, PreFormat, ''),
  atomic_concat(PreFormat, '~n', Format).
make_column_format([], [], Format, Format).
make_column_format([HC|TC], [HS|TS], Format, Acc):-
  atomic_concat([Acc,'~', HS,'~',HC,'+'], NAcc),
  make_column_format(TC, TS, Format, NAcc).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the help part of problog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

problog_help :-
  format('~2nProbLog inference currently offers the following inference methods:~n',[]),
  show_inference,
  problog:problog_path(PD),
  format('~nProbLog directory: ~q~n',[PD]),
  format('~nThe following global parameters are available:~n',[]),
  problog_flags,
  print_sep_line,
  format('~n     use problog_help/0 to display this information~n',[]),
  format('~n     use problog_flags/0 to display current parameter values~n',[]),
  format('~n     use problog_flags/1 to display current parameter values of a group~2n',[]),
  print_sep_line,
  nl,
  flush_output.

problog_version :-
  MainProblogFiles = ['problog.yap', 'problog_learning.yap', 'dtproblog.yap'],
  nl,
  print_group_line_bold('Version Information'),
  print_version(MainProblogFiles, ''),
  print_sep_line,
  convert_filename_to_problog_path('problog', ProblogPath),
  directory_files(ProblogPath, ProblogFiles),
  sort(ProblogFiles, ProblogFilesS),
  print_version(ProblogFilesS, 'problog'),
  print_sep_line_bold.

print_version([], _Path).
print_version([H|T], Path):-
  atom_concat(_, '.yap', H), !,
  (Path == '' ->
    FileName = H
  ;
    concat_path_with_filename2(Path, H, FileName)
  ),
  check_existance(FileName),
  convert_filename_to_problog_path(FileName, FilePath),
  get_version(FilePath, Version, Revision),
  format('~w~35+ Last Modified at:~w~65+Revision:~w~n', [FileName, Version, Revision]),
  print_version(T, Path).
print_version([_H|T], Path):-
  print_version(T, Path).


show_inference :-
  format('~n',[]),
  print_sep_line,
  print_inference(call,description),
  print_sep_line,
  print_inference('problog_delta(+Query,+Delta,-Low,-High,-Status)','approximation with interval width Delta (IJCAI07)'), 
  print_inference('problog_threshold(+Query,+Threshold,-Low,-High,-Status)','bounds based on single probability threshold'), 
  print_inference('problog_low(+Query,+Threshold,-Low,-Status)','lower bound based on single probability threshold'), 
  print_inference('problog_kbest(+Query,+K,-Low,-Status)','lower bound based on K most likely proofs'), 
  print_inference('problog_max(+Query,-Prob,-FactsUsed)','explanation probability (ECML07)'),
  print_inference('problog_exact(+Query,-Prob,-Status)','exact probability'),
  print_inference('problog_montecarlo(+Query,+Delta,-Prob)','program sampling with 95%-confidence-interval-width Delta'),
  print_inference('problog_dnf_sampling(+Query,+Delta,-Prob)','DNF sampling with 95%-confidence-interval-width Delta'),
  print_sep_line.

%%%%%%%%%%%%%%% This is the flag part towards screen %%%%%%%%%%%%%%%%%%%%

% Currently does not print default values to gain space
problog_flags(Group):-
  problog_defined_flag_group(Group),
  print_group_line(Group),
  print_param(description, domain, flag, value),
  print_sep_line,
  (                         % iterate over all flags in this group
    problog_defined_flag(Flag, Group, _Default, LValues, Desc),
    problog_flag(Flag, Value),
    (is_list(LValues) ->
      atomic_concat(LValues, Values)
    ;
      Values = LValues
    ),
    print_param(Desc, Values, Flag, Value),
    fail
    ; % go to next flag
    true
  ),
  print_sep_line.

problog_flags:-
  format('~n',[]),
  print_sep_line_bold,
  format('problog flags: use set_problog_flag(Flag,Option) to change, problog_flag(Flag,Option) to view~n',[]),
  print_sep_line_bold,
  format('~n',[]),
  (     % iterate over all groups
    problog_flags(_),
    format('~n',[]),
    fail;      % go to next group
    true
  ),
  format('~n',[]).



%%%%%%%%%%%%%%% This is statistics part towards screen %%%%%%%%%%%%%%%%%%%

problog_statistics:-
  nb_setval(problog_statistics, false),
  problog_var_group(Group),
  findall(Stat/Result, (
    problog_var_defined(Stat, Group, _, _),
    problog_var_is_set(Stat),
    problog_var_get(Stat, Result)
  ), GroupStats),
  \+ GroupStats == [],
  nb_setval(problog_statistics, true),
  print_group_line(Group),
  forall(member(Stat/Result, GroupStats), (
    problog_var_defined(Stat, Group, _, messages(MsgBefore, Seperator, MsgAfter)),
    print_stat(MsgBefore, Seperator, Result, MsgAfter)
  )),
  fail.
problog_statistics:-
  (nb_getval(problog_statistics, true)->
    print_sep_line
  ;
    true
  ),
  nb_delete(problog_statistics).


% namee(A, Name):-
%   atomic(A), !,
%   name(A, Name).
% namee(L, Name):-
%   is_list(L), !,
%   namee(L, Name, []).
% namee(A, Name):-
%   A =.. L,
%   namee(L, Name, []).
% namee([], Name, Name).
% namee([H|T], Name, Acc):-
%   namee(H, N),
%   append(Acc, N, NAcc),
%   namee(T, Name, NAcc).

% print_sep_line :-
%   sep_line(125).
% sep_line(0) :- 
% 	!,
% 	format('~n',[]).
% sep_line(N) :-
% 	format('-',[]),
% 	NN is N-1,
% 	sep_line(NN).

% print_sep_line_bold :-
% 	sep_line_bold(125).
% sep_line_bold(0) :- 
% 	!,
% 	format('~n',[]).
% sep_line_bold(N) :-
% 	format('=',[]),
% 	NN is N-1,
% 	sep_line_bold(NN).
