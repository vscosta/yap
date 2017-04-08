%%% -*- Mode: Prolog; -*-

/**
 * @file   block_diagram.yap
 * @author  Theofrastos Mantadelis, Sugestions from Paulo Moura
 * @date   Tue Nov 17 14:12:02 2015
 * 
 * @brief Graph the program structure. 
 * 
 * @{
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Flags was developed at Katholieke Universiteit Leuven
%
%  Copyright 2010
%  Katholieke Universiteit Leuven
%
%  Contributions to this file:
%  Author: Theofrastos Mantadelis
%  Sugestions: Paulo Moura
%  Version: 1
%  Date: 19/11/2010
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

/** @defgroup block_diagram Block Diagram
@ingroup library
@{

This library provides a way of visualizing a prolog program using
modules with blocks.  To use it use:
`:-use_module(library(block_diagram))`.

 
*/

:- module(block_diagram, [make_diagram/2, make_diagram/5]).

/* ---------------------------------------------------------------------- *\
|* Missing stuff: a parameter that bounds the module connection depth     *|
|* and a parameter that diseables/limits the text over edges              *|
\* ---------------------------------------------------------------------- */

:- style_check(all).
:- yap_flag(unknown, error).


:- use_module(library(charsio), [term_to_atom/2]).
:- use_module(library(lists), [memberchk/2, member/2, append/3]).
:- use_module(library(system), [working_directory/2]).
:- dynamic([seen_module/1, parameter/1]).

parameter(texts((+inf))).
parameter(depth((+inf))).
parameter(default_ext('.yap')).

/** @pred make_diagram(+Inputfilename, +Ouputfilename) 



This will crawl the files following the use_module, ensure_loaded directives withing the inputfilename.
The result will be a file in dot format.
You can make a pdf at the shell by asking `dot -Tpdf filename > output.pdf`.

 
*/
make_diagram(InputFile, OutputFile):-
  tell(OutputFile),
  write('digraph G {\nrankdir=BT'), nl,
  extract_name_file(InputFile, Name, File),
  nb_setval(depth, 0),
  read_module_file(File, Name),
  write_explicit,
  write('}'), nl,
  told.

/** @pred make_diagram(+Inputfilename, +Ouputfilename, +Predicate, +Depth, +Extension)


The same as make_diagram/2 but you can define how many of the imported/exporeted predicates will be shown with predicate, and how deep the crawler is allowed to go with depth. The extension is used if the file use module directives do not include a file extension.

*/
make_diagram(InputFile, OutputFile, Texts, Depth, Ext):-
  integer(Texts),
  integer(Depth),
  retractall(parameter(_)),
  assertz(parameter(texts(Texts))),
  assertz(parameter(depth(Depth))),
  assertz(parameter(default_ext(Ext))),
  make_diagram(InputFile, OutputFile),
  retractall(parameter(_)),
  assertz(parameter(texts((+inf)))),
  assertz(parameter(depth((+inf)))),
  assertz(parameter(default_ext('.yap'))).

path_seperator('\\'):-
   yap_flag(windows, true), !.
path_seperator('/').

split_path_file(PathFile, Path, File):-
	path_seperator(PathSeperator),
	atom_concat(Path, File, PathFile),
	name(PathSeperator, [PathSeperatorName]),
	name(File, FileName),
	\+ memberchk(PathSeperatorName, FileName),
	!.
split_file_ext(FileExt, File, Ext):-
	atom_concat(File, Ext, FileExt),
	atom_concat('.', _, Ext),
	name('.', [DotName]),
	name(Ext, ExtName),
	findall(A, (member(A, ExtName), A = DotName), L),
	length(L, 1), !.


parse_module_directive(':-'(module(Name)), _):-
  seen_module(node(Name)), !.
parse_module_directive(':-'(module(Name, _Exported)), _):-
  seen_module(node(Name)), !.
parse_module_directive(':-'(module(Name, Exported)), Shape):-
  !, \+ seen_module(node(Name)),
  assertz(seen_module(node(Name))),
  list_to_message(Exported, ExportedMessage),
  atom_concat([Name, ' [shape=', Shape,',label="', Name, '\\n', ExportedMessage, '"]'], NodeDefinition),
  write(NodeDefinition), nl.
parse_module_directive(':-'(module(Name)), Shape):-
  \+ seen_module(node(Name)),
  assertz(seen_module(node(Name))),
  atom_concat([Name, ' [shape=', Shape,',label="', Name, '"]'], NodeDefinition),
  write(NodeDefinition), nl.

extract_name_file(PathFile, Name, FinalFile):-
  split_path_file(PathFile, Path, FileName), Path \== '', !,
  extract_name_file(FileName, Name, File),
  atom_concat(Path, File, FinalFile).
extract_name_file(File, Name, File):-
  split_file_ext(File, Name, _), !.
extract_name_file(Name, Name, File):-
  parameter(default_ext(Ext)),
  atom_concat(Name, Ext, File).

read_use_module_directive(':-'(ensure_loaded(library(Name))), Name, library(Name), []):- !.
read_use_module_directive(':-'(ensure_loaded(Path)), Name, FinalFile, []):-
  extract_name_file(Path, Name, FinalFile), !.
read_use_module_directive(':-'(use_module(library(Name))), Name, library(Name), []):- !.
read_use_module_directive(':-'(use_module(Path)), Name, FinalFile, []):-
  extract_name_file(Path, Name, FinalFile), !.
read_use_module_directive(':-'(use_module(library(Name), Import)), Name, library(Name), Import):- !.
read_use_module_directive(':-'(use_module(Path, Import)), Name, FinalFile, Import):-
  extract_name_file(Path, Name, FinalFile), !.
read_use_module_directive(':-'(use_module(Name, Path, Import)), Name, FinalFile, Import):-
  nonvar(Path),
  extract_name_file(Path, _, FinalFile), !.
read_use_module_directive(':-'(use_module(Name, Path, Import)), Name, FinalFile, Import):-
  var(Path),
  extract_name_file(Name, _, FinalFile), !.

parse_use_module_directive(Module, Directive):-
  read_use_module_directive(Directive, Name, File, Imported),
  parse_use_module_directive(Module, Name, File, Imported).
parse_use_module_directive(Module, Name, _File, _Imported):-
  seen_module(edge(Module, Name)), !.
parse_use_module_directive(Module, Name, File, Imported):-
  \+ seen_module(edge(Module, Name)),
  assertz(seen_module(edge(Module, Name))),
  read_module_file(File, Name),
  list_to_message(Imported, ImportedMessage),
  atom_concat([Module, ' -> ', Name, ' [label="', ImportedMessage, '"]'], NodeConnection),
  write(NodeConnection), nl.

list_to_message(List, Message):-
  length(List, Len),
  parameter(texts(TextCnt)),
  (Len > TextCnt + 1 ->
    append(FirstCnt, _, List),
    length(FirstCnt, TextCnt),
    append(FirstCnt, ['...'], First)
  ;
    First = List
  ),
  list_to_message(First, '', Message).

list_to_message([], Message, Message).
list_to_message([H|T], '', FinalMessage):-
  term_to_atom(H, HAtom), !,
  list_to_message(T, HAtom, FinalMessage).
list_to_message([H|T], AccMessage, FinalMessage):-
  term_to_atom(H, HAtom),
  atom_concat([AccMessage, '\\n', HAtom], NewMessage),
  list_to_message(T, NewMessage, FinalMessage).

read_module_file(library(Module), Module):-
  !, parse_module_directive(':-'(module(Module, [])), component).
read_module_file(File, Module):-
  parameter(depth(MaxDepth)),
  nb_getval(depth, Depth),
  MaxDepth > Depth,
  split_path_file(File, Path, FileName),
  catch((working_directory(CurDir,Path), open(FileName, read, S)), _, (parse_module_directive(':-'(module(Module, [])), box3d), fail)),
  NDepth is Depth + 1,
  nb_setval(depth, NDepth),
  repeat,
  catch(read(S, Next),_,fail),
  process(Module, Next),
  nb_setval(depth, Depth),
  close(S), working_directory(_,CurDir), !.
read_module_file(_, _).

/** @pred process(+ _StreamInp_, + _Goal_) 



For every line  _LineIn_ in stream  _StreamInp_, call
`call(Goal,LineIn)`.

 
*/
process(_, end_of_file):-!.
process(_, Term):-
  parse_module_directive(Term, box), !, fail.
process(Module, Term):-
  parse_use_module_directive(Module, Term), !, fail.
process(Module, Term):-
  find_explicit_qualification(Module, Term), fail.

find_explicit_qualification(OwnerModule, ':-'(Module:Goal)):-
  !, explicit_qualification(OwnerModule, Module, Goal).
find_explicit_qualification(OwnerModule, ':-'(_Head, Body)):-
  find_explicit_qualification(OwnerModule, Body).
find_explicit_qualification(OwnerModule, (Module:Goal, RestBody)):-
  !, explicit_qualification(OwnerModule, Module, Goal),
  find_explicit_qualification(OwnerModule, RestBody).
find_explicit_qualification(OwnerModule, (_Goal, RestBody)):-
  !, find_explicit_qualification(OwnerModule, RestBody).
find_explicit_qualification(OwnerModule, Module:Goal):-
  !, explicit_qualification(OwnerModule, Module, Goal).
find_explicit_qualification(_OwnerModule, _Goal).

explicit_qualification(InModule, ToModule, Goal):-
  nonvar(Goal), nonvar(ToModule), !,
  functor(Goal, FunctorName, Arity),
  \+ seen_module(explicit(InModule, ToModule, FunctorName/Arity)),
  assertz(seen_module(explicit(InModule, ToModule, FunctorName/Arity))).

explicit_qualification(InModule, ToModule, Goal):-
  var(Goal), nonvar(ToModule), !,
  \+ seen_module(explicit(InModule, ToModule, 'DYNAMIC')),
  assertz(seen_module(explicit(InModule, ToModule, 'DYNAMIC'))).

explicit_qualification(InModule, ToModule, Goal):-
  nonvar(Goal), var(ToModule), !,
  functor(Goal, FunctorName, Arity),
  \+ seen_module(explicit(InModule, 'DYNAMIC', FunctorName/Arity)),
  assertz(seen_module(explicit(InModule, 'DYNAMIC', FunctorName/Arity))).

explicit_qualification(InModule, ToModule, Goal):-
  var(Goal), var(ToModule),
  \+ seen_module(explicit(InModule, 'DYNAMIC', 'DYNAMIC')),
  assertz(seen_module(explicit(InModule, 'DYNAMIC', 'DYNAMIC'))).

write_explicit:-
  seen_module(explicit(InModule, ToModule, _Goal)),
  \+ seen_module(generate_explicit(InModule, ToModule)),
  assertz(seen_module(generate_explicit(InModule, ToModule))),
  all(Goal, seen_module(explicit(InModule, ToModule, Goal)), Goals),
  list_to_message(Goals, Explicit),
  atom_concat([InModule, ' -> ', ToModule, ' [label="', Explicit, '",style=dashed]'], NodeConnection),
  write(NodeConnection), nl, fail.
write_explicit.

/* 
  functor(Goal, FunctorName, Arity),
  term_to_atom(FunctorName/Arity, Imported),
  atom_concat([InModule, ' -> ', ToModule, ' [label="', Imported, '",style=dashed]'], NodeConnection),
  write(NodeConnection), nl.

  atom_concat([InModule, ' -> ', ToModule, ' [label="DYNAMIC",style=dashed]'], NodeConnection),
  write(NodeConnection), nl.

  functor(Goal, FunctorName, Arity),
  term_to_atom(FunctorName/Arity, Imported),
  atom_concat([InModule, ' -> DYNAMIC [label="', Imported, '",style=dashed]'], NodeConnection),
  write(NodeConnection), nl.

  atom_concat([InModule, ' -> DYNAMIC [label="DYNAMIC",style=dashed]'], NodeConnection),
  write(NodeConnection), nl.
 */

%% @} @}
