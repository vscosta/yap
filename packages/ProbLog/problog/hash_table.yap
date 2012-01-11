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
%  Theofrastos Mantadelis
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


%
% Module hash_table.
%
% Maps tuples of form (x1,x2,...,xn) to incremental IDs.
% Ensures lookup time for tuple = O(n) with n being the size of tuple lookup
% time is independed from the amount of entries in the table when +Size big.
% If +Size not big enought then lookup time linear per +Size entries.
% To return from ID to tuple again constant time but based on +TuppleSize.
%
% Exports:
%
% hash_table_init(+Size, -HashTable)
% hash_table_init(+Size, +TupleSize, -HashTable)
%
%   Initializes a hash table with internal array size of +Size, and #tuples
%   of +TupleSize.
%   +Size ~> # of problog different prob facts.
%   +TupleSize ~> estimated tuples to be used.
%
% hash_table_set_domain_size(+HashTable, +Tuple, +TupleSize)
%
%   Useful to define that a specific +Tuple mask will have specific internal
%   size. It also pre initialized an array for the ID.
%   Examples: if problog fact ID1 is a non-ground and we now that it has
%   1000 possible groundings then: hash_table_set_domain_size(HT, ID1, 1000).
%   problog fact ID2 is an mvs with 5 possible values and each is non
%   ground with 100 possible values then: hash_table_set_domain_size(HT, ID2, 5),
%   hash_table_set_domain_size(HT, (ID2,'*'), 100).
%   When emmitted the internal array will assume the defined size from father.
%
% hash_table_delete(+HashTable)
%
%   Clears the +HashTable
%
% hash_table_display(+HashTable, +ColSize, +PaneSize)
%
%   Debug only purposes currently.
%
% hash_table_lookup(+HashTable, +Tuple, -ID)
% hash_table_lookup(+HashTable, -Tuple, +ID)
%
%   This is the lookup function. Either takes a +Tuple and retuns an -ID
%   or vice versa. Handles collisions, automatically grows array by a chain
%   of arrays.
%
% hash_table_contains(+HashTable, +Tuple, -ID)
%
%   Similar with lookup but only checks if +Tuple has been seen and returns -ID.
%
% hash_table_get_entries(+HashTable, -Count)
%
%   Returns the # of tuples seen up to now.
%
% problog_key_to_tuple(+Key, -Tuple)
%
%   Takes a +Key of format "ID1_ID2" and makes a tuple (ID1, ID2)
%
%
% IMPORTANT: Currently it under performs for some reason...
%
:- module(hash_table, [hash_table_init/2,
                       hash_table_init/3,
                       hash_table_set_domain_size/3,
                       hash_table_delete/1,
                       hash_table_reset/1,
                       hash_table_lookup/3,
                       hash_table_contains/3,
                       hash_table_get_entries/2,
                       hash_table_display/3,
                       problog_key_to_tuple/2]).

:- use_module(library(lists), [member/2]).
%
% General use predicates
%
int(N):-
  int(0, N).
int(N, N).
int(P, R):-
  N is P + 1,
  int(N, R).

get_digits(Num, Digits):-
  get_digits(Num, Digits, 1).
get_digits(Num, Digits, Digits):-
  Num < 10, !.
get_digits(Num, Digits, Acc):-
  NNum is Num / 10,
  NAcc is Acc + 1,
  get_digits(NNum, Digits, NAcc).

%
% Simple Counters
%
:- initialization(bb_put(array_count, 1)).

get_next_array(ID, Name):-
  bb_get(array_count, ID),
  NewID is ID + 1,
  bb_put(array_count, NewID),
  number_atom(ID, Name).

get_next_identifier(Identifier, Next):-
  bb_get(Identifier, Next),
  NewNext is Next + 1,
  bb_put(Identifier, NewNext).

%
% Syntactic Sugar
%
get_array_name(ID, Array):-   % if you change this, you need to change also get_next_array
%   char_code(Array, ID).
  number_atom(ID, Array).
%   atomic_concat(array, ID, Array).

get_array_identifier(ID, Identifier):-
  atomic_concat(array_identifier, ID, Identifier).

%
% hash_table_init(+Size, -HashTable)
% initializes a HashTable with Size positions, collitions are handled
% by expanding a new array that is kept at the last array element
%
hash_table_init(Size, HashTable):-
  hash_table_init(Size, Size, HashTable).
hash_table_init(Size, RevSize, HashTable):-
  ArraySize is Size + 1,
  get_next_array(ID, Array),
  get_array_identifier(ID, Identifier),
  static_array(Array, ArraySize, int),
  bb_put(Identifier, 1),
  RevArraySize is RevSize + 1,
  get_next_array(_RevID, RevArray),
  static_array(RevArray, RevArraySize, term),
  recordz(hash_table, hash(Array, Size, Identifier, RevArray, RevSize), HashTable).

hash_table_expand_array(Array, Size, NewArray):-
  ArraySize is Size + 1,
  get_next_array(ID, NewArray),
  static_array(NewArray, ArraySize, int),
  update_array(Array, Size, ID).

hash_table_sub_array_init(Array, Index, NewArray, Size):-
  ArraySize is Size + 1,
  get_next_array(ID, NewArray),
  static_array(NewArray, ArraySize, int),
  recordz(hash_table_arrays, array(ID, NewArray, Size, Array), _),
  update_array(Array, Index, ID).


%
% hash_table_set_domain_size(HashTable, Index, DomainSize)
%
hash_table_set_domain_size(HashTable, Index, DomainSize):-
  ground(Index),
  ground(HashTable),
  recorded(hash_table, hash(Array, Size, _Identifier, _RevArray, _RevSize), HashTable),
  hash_table_set_domain_size(Array, Size, Index, DomainSize).

hash_table_set_domain_size(Array, Size, Index, DomainSize):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, A),
  (A is 0 ->
    hash_table_sub_array_init(Array, Index, _NewArray, DomainSize)
  ;
    throw(hash_table_exception(set_domain_size_fail(duplicate_definition(Index, DomainSize))))
  ).

hash_table_set_domain_size(Array, Size, Index, DomainSize):-
  integer(Index), !,
  NewIndex is Index - Size,
  array_element(Array, Size, SubArrayID),
  (SubArrayID is 0->
    hash_table_expand_array(Array, Size, NewArray)
  ;
    get_array_name(SubArrayID, NewArray)
  ),
  hash_table_set_domain_size(NewArray, Size, NewIndex, DomainSize).

hash_table_set_domain_size(Array, Size, Index, DomainSize):-
  Index == '*', !,
  int(N),
  hash_table_set_domain_size(Array, Size, N, DomainSize),
  N is Size - 1, !.

hash_table_set_domain_size(Array, Size, (Index, Rest), DomainSize):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, SubArrayID),
  (SubArrayID is 0->
    throw(hash_table_exception(set_domain_size_fail(sub_array_missing((Index, Rest), DomainSize))))
  ;
    recorded(hash_table_arrays, array(SubArrayID, SubArray, SubArraySize, Array), _)
  ),
  hash_table_set_domain_size(SubArray, SubArraySize, Rest, DomainSize).

hash_table_set_domain_size(Array, Size, (Index, Rest), DomainSize):-
  integer(Index),
  NewIndex is Index - Size,
  array_element(Array, Size, SubArrayID),
  (SubArrayID is 0->
    hash_table_expand_array(Array, Size, NewArray)
  ;
    get_array_name(SubArrayID, NewArray)
  ),
  hash_table_set_domain_size(NewArray, Size, (NewIndex, Rest), DomainSize).



%
% hash_table_delete(+HashTable)
% deletes the arrays, records, and blackboard variables related with the hashtable
%
hash_table_delete(HashTable):-
  ground(HashTable),
  recorded(hash_table, hash(Array, Size, Identifier, RevArray, RevSize), HashTable),
  bb_delete(Identifier, _),
  erase(HashTable),
  hash_table_delete_array(Array, Size),
  hash_table_delete_rev_array(RevArray, RevSize).

hash_table_delete_array(Array, Size):-
  hash_table_delete_chain(Array, Size),
  hash_table_delete_subarrays(Array),
  close_static_array(Array).

hash_table_delete_chain(Array, Size):-
  array_element(Array, Size, ChainArrayID),
  (ChainArrayID is 0 ->
    true
  ;
    get_array_name(ChainArrayID, ChainArray),
    hash_table_delete_array(ChainArray, Size)
  ).

hash_table_delete_subarrays(Array):- % I can improve the performance of this by making a second record with Array infront
  forall(recorded(hash_table_arrays, array(_SubArrayID, SubArray, Size, Array) , Ref),
    (erase(Ref), hash_table_delete_array(SubArray, Size))).

hash_table_delete_rev_array(Array, Size):-
  (array_element(Array, Size, ChainArray) ->
    hash_table_delete_rev_array(ChainArray, Size)
  ;
    true
  ),
  close_static_array(Array).


%
% hash_table_reset(+HashTable)
%
% resets the table values, retains the structure
%

hash_table_reset(HashTable):-
  ground(HashTable),
  recorded(hash_table, hash(Array, Size, Identifier, RevArray, RevSize), HashTable),
  hash_table_get_entries(HashTable, Count),
  int(Index),
  hash_table_lookup(HashTable, Tuple, Index),
  hash_table_reset_element(Array, Size, Tuple),
  Index is Count - 1, !,
  hash_table_reset_rev_array(RevArray, RevSize),
  bb_put(Identifier, 1).

hash_table_reset_element(Array, Size, Index):-
  integer(Index),
  Index < Size, !,
  update_array(Array, Index, 0).

hash_table_reset_element(Array, Size, Index):-
  integer(Index),
  NewIndex is Index - Size,
  array_element(Array, Size, SubArrayID),
  get_array_name(SubArrayID, SubArray),
  hash_table_reset_element(SubArray, Size, NewIndex).

hash_table_reset_element(Array, Size, (Index, Rest)):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, SubArrayID),
  recorded(hash_table_arrays, array(SubArrayID, SubArray, SubArraySize, Array), _),
  hash_table_reset_element(SubArray, SubArraySize, Rest).

hash_table_reset_element(Array, Size, (Index, Rest)):-
  integer(Index),
  NewIndex is Index - Size,
  array_element(Array, Size, SubArrayID),
  get_array_name(SubArrayID, SubArray),
  hash_table_reset_element(SubArray, Size, (NewIndex, Rest)).
  
hash_table_reset_rev_array(RevArray, RevSize):-
  array_element(RevArray, RevSize, ChainArray), !,
  reset_static_array(RevArray),
  update_array(RevArray, RevSize, ChainArray),
  hash_table_reset_rev_array(ChainArray, RevSize).
  
hash_table_reset_rev_array(RevArray, _RevSize):-
  reset_static_array(RevArray).

%
% hash_table_lookup(+HashTable, +Tuple, -ID)
% hash_table_lookup(+HashTable, -Tuple, +ID)
% lookup Tuple in HashTable and insert, return ID
% lookup ID in HashTable and return Tuple
%
% Known bug: If HashTable contains a Tuple of form (ID, _) looking up ID succeeds
%            and returns the SubArrayID.
%            This is safe under the assumption that ID is unique to start from.
%

hash_table_lookup(HashTable, Tuple, ID):-
  ground(Tuple),
  ground(HashTable), !,
  recorded(hash_table, hash(Array, Size, Identifier, RevArray, RevSize), HashTable),
  hash_table_lookup(Array, Size, Identifier, Tuple, ID),
  hash_table_update_rev_array(RevArray, RevSize, ID, Tuple).

hash_table_lookup(HashTable, Tuple, ID):-
  integer(ID),
  ground(HashTable),
  recorded(hash_table, hash(_Array, _Size, _Identifier, RevArray, RevSize), HashTable),
  hash_table_element_rev_array(RevArray, RevSize, ID, Tuple).

hash_table_lookup(Array, Size, Identifier, Index, RID):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, StoredID),
  (StoredID is 0 ->
    get_next_identifier(Identifier, ID),
    update_array(Array, Index, ID)
  ;
    ID = StoredID
  ),
  RID is ID - 1.

hash_table_lookup(Array, Size, Identifier, Index, ID):-
  integer(Index), !,
  NewIndex is Index - Size,
  array_element(Array, Size, ArrayID),
  (ArrayID is 0 ->
    hash_table_expand_array(Array, Size, SubArray)
  ;
    get_array_name(ArrayID, SubArray)
  ),
  hash_table_lookup(SubArray, Size, Identifier, NewIndex, ID).

hash_table_lookup(Array, Size, Identifier, (Index,Tuple), ID):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, ArrayID),
  (ArrayID is 0 ->
    NewSize = Size,
    hash_table_sub_array_init(Array, Index, SubArray, Size)
  ;
    recorded(hash_table_arrays, array(ArrayID, SubArray, NewSize, Array), _)
  ),
  hash_table_lookup(SubArray, NewSize, Identifier, Tuple, ID).

hash_table_lookup(Array, Size, Identifier, (Index,Tuple), ID):-
  integer(Index),
  NewIndex is Index - Size,
  array_element(Array, Size, ArrayID),
  (ArrayID is 0 ->
    hash_table_expand_array(Array, Size, SubArray)
  ;
    get_array_name(ArrayID, SubArray)
  ),
  hash_table_lookup(SubArray, Size, Identifier, (NewIndex, Tuple), ID).

hash_table_update_rev_array(Array, Size, Index, Tuple):-
  integer(Index),
  Index < Size, !,
  update_array(Array, Index, Tuple).

hash_table_update_rev_array(Array, Size, Index, Tuple):-
  integer(Index),
  NewIndex is Index - Size,
  (array_element(Array, Size, SubArray) ->
    true
  ;
    SubArraySize is Size + 1,
    get_next_array(_SubArrayID, SubArray),
    static_array(SubArray, SubArraySize, term),
    update_array(Array, Size, SubArray)
  ),
  hash_table_update_rev_array(SubArray, Size, NewIndex, Tuple).

hash_table_element_rev_array(Array, Size, Index, Tuple):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, Tuple).

hash_table_element_rev_array(Array, Size, Index, Tuple):-
  integer(Index),
  NewIndex is Index - Size,
  array_element(Array, Size, SubArray),
  hash_table_element_rev_array(SubArray, Size, NewIndex, Tuple).


%
% hash_table_contains(+HashTable, +Tuple, -ID)
% search the hash_table to see if it contains a Tuple and return the ID
%
% Known bug: If HashTable contains a Tuple of form (ID, _) asking if HashTable
%            contains ID it succeeds.
%            This is safe under the assumption that ID is unique to start from.
%
hash_table_contains(HashTable, Tuple, ID):-
  ground(Tuple),
  ground(HashTable), !,
  recorded(hash_table, hash(Array, Size, _Identifier, _RevArray, _RevSize), HashTable),
  hash_table_contains(Array, Size, Tuple, ID).

hash_table_contains(Array, Size, Index, RID):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, ID),
  ID > 0,
  RID is ID - 1.

hash_table_contains(Array, Size, Index, ID):-
  integer(Index), !,
  NewIndex is Index - Size,
  array_element(Array, Size, SubArrayID),
  SubArrayID > 0,
  get_array_name(SubArrayID, SubArray),
  hash_table_contains(SubArray, Size, NewIndex, ID).

hash_table_contains(Array, Size, (Index,Tuple), ID):-
  integer(Index),
  Index < Size, !,
  array_element(Array, Index, SubArrayID),
  SubArrayID > 0,
  recorded(hash_table_arrays, array(SubArrayID, SubArray, NewSize, Array), _),
  hash_table_contains(SubArray, NewSize, Tuple, ID).

hash_table_contains(Array, Size, (Index,Tuple), ID):-
  integer(Index),
  NewIndex is Index - Size,
  array_element(Array, Size, SubArrayID),
  SubArrayID > 0,
  get_array_name(SubArrayID, SubArray),
  hash_table_contains(SubArray, Size, (NewIndex, Tuple), ID).

%
% hash_table_get_entries(+HashTable, -Count)
%
% returns the number of entries inside the hash table
%
hash_table_get_entries(HashTable, Count):-
  ground(HashTable),
  recorded(hash_table, hash(_Array, _Size, Identifier, _RevArray, _RevSize), HashTable),
  bb_get(Identifier, Num),
  Count is Num - 1.

%
% hash_table_display(+HashTable, +ColSize, +PaneSize)
%
% Only for debugging reasons.
%
hash_table_display(HashTable, ColSize, PaneSize):-
  ground(HashTable), integer(ColSize), integer(PaneSize), ColSize =< PaneSize,
  recorded(hash_table, hash(Array, Size, Identifier, RevArray, RevSize), HashTable),
  hash_table_get_entries(HashTable, Count),
  format('Hash Table: ~q~n  Entries: ~d~n  Identifier: ~w~n', [HashTable, Count, Identifier]),
  hash_table_display_array(Array, Size),
  hash_table_display_rev_array(RevArray, RevSize, Identifier, ColSize, PaneSize).

hash_table_display_array(Array, Size):-
  hash_table_get_chains(Array, Size, Chains),
  findall(SubArray, recorded(hash_table_arrays, array(_, SubArray, _, Array),_), SubArrays),
  format('Array: ~q~n  Size: ~d~n  Chains: ~q~n  Sub Arrays: ~q~n',[Array, Size, Chains, SubArrays]),
  forall(member(SubArray, SubArrays), (
    recorded(hash_table_arrays, array(_, SubArray, SubSize, Array),_),
    hash_table_display_array(SubArray, SubSize)
  )).

hash_table_display_rev_array(RevArray, RevSize, Identifier, ColSize, PaneSize):-
  hash_table_get_chains(RevArray, RevSize, Chains),
  format('Array: ~q~n  Size: ~d~n  Chains: ~q~n', [RevArray, RevSize, Chains]),
  bb_get(Identifier, Num),
  get_digits(Num, Digits),
  hash_table_get_elements(RevArray, RevSize, 0, Tupples),
  hash_table_display_elements(0, Tupples, Digits, ColSize, PaneSize).

hash_table_get_elements(RevArray, RevSize, Current, [Tupple|Tupples]):-
  Current < RevSize,
  array_element(RevArray, Current, Tupple), !,
  Next is Current + 1,
  hash_table_get_elements(RevArray, RevSize, Next, Tupples).
hash_table_get_elements(_RevArray, RevSize, Current, []):-
  Current < RevSize, !.
hash_table_get_elements(RevArray, RevSize, RevSize, Tupples):-
  array_element(RevArray, RevSize, NextArray), !,
  hash_table_get_elements(NextArray, RevSize, 0, Tupples).
hash_table_get_elements(_RevArray, RevSize, RevSize, []).

hash_table_get_chains(Array, Size, Chains):-
  ((array_element(Array, Size, ChainID), ChainID \== 0) ->
    (integer(ChainID) ->
      get_array_name(ChainID, ChainName)
    ;
      ChainName = ChainID
    ),
    hash_table_get_chains(ChainName, Size, RestChains),
    Chains = [ChainName|RestChains]
  ;
    Chains = []
  ).

hash_table_display_elements(_Index, [], _Digits, _ColSize, _PaneSize):- format('~n',[]), !.
hash_table_display_elements(Index, [Element|T], Digits, ColSize, PaneSize):-
  NL is Index mod integer(PaneSize / ColSize),
  RealColSize is ColSize - Digits - 3,
  ((NL > 0; Index =:= 0) -> true; format('~n',[])),
  format('~t~d~*+ = ~q~*+', [Index, Digits, Element, RealColSize]),
  NewIndex is Index + 1,
  hash_table_display_elements(NewIndex, T, Digits, ColSize, PaneSize).


%
% problog_key_to_tuple(+Key, -Tuple)
%
% This should be removed in new implementation
%
problog_key_to_tuple(Key, Key):-
  integer(Key), !.
problog_key_to_tuple(Key, (PID, SID)):-
  atomic(Key),
  atom_chars(Key, ID_Chars),
  break_list_at(ID_Chars, 95, Part1, Part2),
%   once(append(Part1, [95|Part2], ID_Chars)), % 95 = '_'
  number_chars(PID, Part1),
  number_chars(SID, Part2).

break_list_at([H|T], H, [], T):-!.
break_list_at([H|T], At, [H|Part1], Part2):-
  break_list_at(T, At, Part1, Part2).
