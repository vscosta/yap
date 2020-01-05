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
% infringed by the Pack<age. If you institute patent litigation
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

% To define flags do: defined_flag(Flag, Type, Description, DefaultValue).
% Available predifined types:
%         flag_validate_dummy
%         flag_validate_atom
%         flag_validate_atomic
%         flag_validate_number
%         flag_validate_integer
%         flag_validate_directory
%         flag_validate_file
%         flag_validate_in_list(L)
%         flag_validate_in_interval(I, Type)
%         flag_validate_in_interval_closed(I)
%         flag_validate_in_interval_open(I)
%         flag_validate_in_interval_left_open(I)
%         flag_validate_in_interval_right_open(I)
%         flag_validate_integer_in_interval_closed(I)
%         flag_validate_integer_in_interval_open(I)
%         flag_validate_integer_in_interval_left_open(I)
%         flag_validate_integer_in_interval_right_open(I)
%         flag_validate_float_in_interval_closed(I)
%         flag_validate_float_in_interval_open(I)
%         flag_validate_float_in_interval_left_open(I)
%         flag_validate_float_in_interval_right_open(I)
%         flag_validate_posnumber
%         flag_validate_posint
%         flag_validate_nonegint
%         flag_validate_boolean
%         flag_validate_switch
%

:-module(gflags, [flag_define/4,
                 flag_define/5,
                 flag_define/6,
                 flag_add_validation_syntactic_sugar/2,
                 flag_group_defined/1, % messaging purposes
                 flag_defined/5,       % messaging purposes
                 flag_set/2,
                 flag_store/2,         % sets flag with no validation, useful for handler
                 flag_get/2,
                 flags_reset/0]).

:- use_module(library(lists), [append/3, memberchk/2, reverse/2]).
:- use_module(library(system), [delete_file/1, file_exists/1, file_property/2, make_directory/1]). % for file operations

flag_define(Flag, Type, DefaultValue, Message):-
  flag_define(Flag, general, Type, DefaultValue, flags:true, Message).

flag_define(Flag, Group, Type, DefaultValue, Message):-
  flag_define(Flag, Group, Type, DefaultValue, flags:true, Message).

flag_define(Flag, Group, Type, DefaultValue, Handler, Message):-
  recorded(flags, defined_flag(Flag, _Group, _Type, _DefaultValue, _Handler, _Message), _Ref),
  throw(duplicate_flag_definition(flag_define(Flag, Group, Type, DefaultValue, Handler, Message))).

flag_define(Flag, Group, Type, DefaultValue, Handler, Message):-
    (catch(Type, _, fail)->
    fail
  ;
    \+ (flag_validation_syntactic_sugar(Type, SyntacticSugar), catch(SyntacticSugar, _, fail)),
    throw(unknown_flag_type(flag_define(Flag, Group, Type, DefaultValue, Handler, Message)))
  ).

flag_define(Flag, Group, Type, DefaultValue, Handler, Message):-
  \+ Handler = _M:_Atom,
  throw(non_module_aware_flag_handler(flag_define(Flag, Group, Type, DefaultValue, Handler, Message))).
flag_define(Flag, Group, Type, DefaultValue, M:Handler, Message):-
  \+ (callable(Handler), atom(Handler), atom(M)),
  throw(non_callable_atom_flag_handler(flag_define(Flag, Group, Type, DefaultValue, M:Handler, Message))).

flag_define(Flag, Group, Type, DefaultValue, Handler, Message):-
  \+ flag_validate(Flag, DefaultValue, Type, Handler),
  throw(erroneous_flag_default_value(flag_define(Flag, Group, Type, DefaultValue, Handler, Message))).

flag_define(_Flag, Group, Type, DefaultValue, Handler, Message):-
  \+ Group == general,
  (var(Group) ->
    throw(erroneous_flag_group(flag_define(_Flag, Group, Type, DefaultValue, Handler, Message)))
  ;
    recordzifnot(flags, flag_group(Group), _Ref),
    fail
  ).

flag_define(Flag, Group, Type, DefaultValue, Handler, Message):-
  recordz(flags, defined_flag(Flag, Group, Type, DefaultValue, Handler, Message), Ref),
  (catch(flag_set(Flag, DefaultValue), Exception, (erase(Ref), throw(Exception))) ->
    true
  ;
    erase(Ref),
    throw(handler_error(flag_define(Flag, Group, Type, DefaultValue, Handler, Message)))
  ).

flag_group_defined(general).
flag_group_defined(Group):-
  recorded(flags, flag_group(Group), _Ref).

flag_defined(Flag, Group, DefaultValue, Domain, Message):-
  recorded(flags, defined_flag(Flag, Group, Type, DefaultValue, Handler, Message), _Ref),
  flag_get_domain_message(Type, Handler, Domain).

flag_get_domain_message(MT:Type, M:Handler, Message):-
  MT:validation_type_values(Type, Domain),
  (Handler == true ->
    Message = Domain
  ;
    Goal =.. [Handler, message, CustomDomain],
    call(M:Goal),
    ((nonvar(CustomDomain), CustomDomain \= '') ->
      (Domain == '' ->
        Message = CustomDomain
      ;
        atomic_concat([CustomDomain, '/', Domain], Message)
      )
    ;
      Message = Domain
    )
  ).

flag_set(Flag, _Value):-
  var(Flag), throw(not_defined_flag_exception('free variable')).

flag_set(Flag, _Value):-
  \+ recorded(flags, defined_flag(Flag, _Group, _Type, _DefaultValue, _Handler, _Message), _Ref),
  throw(not_defined_flag_exception(Flag)).

flag_set(Flag, Value):-
  recorded(flags, defined_flag(Flag, _Group, Type, _DefaultValue, M:Handler, _Message), _Ref),
  (Handler == true ->
    GoalValidated = true,
    GoalStored = true
  ;
    GoalValidated =.. [Handler, validated, Value],
    GoalStored =.. [Handler, stored, Value]
  ),
  flag_validate(Flag, Value, Type, M:Handler),
  call(M:GoalValidated),
  flag_store(Flag, Value),
  call(M:GoalStored).

flag_get(Flag, Value):-
  recorded(flag_values, flag(Flag, Value), _Ref).

flag_store(Flag, Value):-
  (recorded(flag_values, flag(Flag, _), Ref) ->
    erase(Ref)
  ;
    true
  ),
  recordz(flag_values, flag(Flag, Value), _Ref).

flags_reset:-
  recorded(flags, defined_flag(Flag, _Group, _Type, DefaultValue, _Handler, _Message), _Ref),
  flag_set(Flag, DefaultValue),
  fail.
flags_reset.


flag_validate(_Flag, Value, _Type, M:Handler):-
  Handler \= true,
  GoalValidate =.. [Handler, validate, Value],
  call(M:GoalValidate), !.

flag_validate(_Flag, Value, Type, _M:Handler):-
  Handler \= true,
  GoalValidating =.. [Handler, validating, Value],
  Type =.. LType,
  append(LType, [Value], LGoal),
  G =.. LGoal,
  catch((GoalValidating, G), _, fail), !.
flag_validate(_Flag, Value, Type, _M:Handler):-
  Handler == true,
  Type =.. LType,
  append(LType, [Value], LGoal),
  G =.. LGoal,
  catch(G, _, fail), !.

flag_validate(_Flag, Value, SyntacticSugar, M:Handler):-
  Handler \= true,
  GoalValidating =.. [Handler, validating, Value],
  flag_validation_syntactic_sugar(SyntacticSugar, Type),
  Type =.. LType,
  append(LType, [Value], LGoal),
  G =.. LGoal,
  catch((M:GoalValidating, gflags:G), _, fail), !.
flag_validate(_Flag, Value, SyntacticSugar, _M:Handler):-
  Handler == true,
  flag_validation_syntactic_sugar(SyntacticSugar, Type),
  Type =.. LType,
  append(LType, [Value], LGoal),
  G =.. LGoal,
  catch(G, _, fail), !.
flag_validate(Flag, Value, Type, Handler):-
  (var(Value) ->
    Value = 'free variable'
  ;
    true
  ),
  flag_get_domain_message(Type, Handler, Domain),
  throw(out_of_domain_exception(Flag, Value, Domain)).

%
% The validation predicates
%

flag_validate_dummy.
flag_validate_dummy(Value):-
  nonvar(Value).

flag_validate_atom.
flag_validate_atom(Value):-
  atom(Value).

flag_validate_atomic.
flag_validate_atomic(Value):-
  atomic(Value).

flag_validate_number.
flag_validate_number(Value):-
  number(Value).

flag_validate_integer.
flag_validate_integer(Value):-
  integer(Value).

flag_validate_directory.
flag_validate_directory(Value):-
    atomic(Value),
  catch(file_exists(Value), _, fail),
  file_property(Value, type(directory)), !.
flag_validate_directory(Value):-
  atomic(Value),
  % fixme : why not inform the user???
  catch((\+ file_exists(Value), make_directory(Value)), _, fail).

flag_validate_file.
flag_validate_file(Value):-
  catch(file_exists(Value), _, fail), file_property(Value, type(regular)), !.
flag_validate_file(Value):-
  atomic(Value),
  catch((\+ file_exists(Value), tell(Value)), _, fail),
  told,
  catch(delete_file(Value),_, fail).



flag_validate_in_list(Domain):-
  is_list(Domain), ground(Domain), \+ Domain = [].
flag_validate_in_list(Domain, Value):-
  ground(Value),
  memberchk(Value, Domain).


flag_validate_in_interval([L, U], Type):-
  nonvar(Type),
  (Type = number ->
    number(L), number(U), L =< U
  ;
    (Type = float ->
      float(L), float(U), L =< U
    ;
      Type = integer, integer(L), integer(U), L =< U
    )
  ).
flag_validate_in_interval((L, U), Type):-
  nonvar(Type),
  (Type = number ->
    number(L), number(U), L < U
  ;
    (Type = float ->
      float(L), float(U), L < U
    ;
      Type = integer, (integer(L);L is -inf), (integer(U);U is +inf), L < U
    )
  ).
flag_validate_in_interval(([L], U), Type):-
  nonvar(Type),
  (Type = number ->
    number(L), number(U), L < U
  ;
    (Type = float ->
      float(L), float(U), L < U
    ;
      Type = integer, integer(L), (integer(U);U is +inf), L < U
    )
  ).
flag_validate_in_interval((L, [U]), Type):-
  nonvar(Type),
  (Type = number ->
    number(L), number(U), L < U
  ;
    (Type = float ->
      float(L), float(U), L < U
    ;
      Type = integer, (integer(L);L is -inf), integer(U), L < U
    )
  ).
flag_validate_in_interval(([L], [U]), Type):-
  nonvar(Type),
  (Type = number ->
    number(L), number(U), L =< U
  ;
    (Type = float ->
      float(L), float(U), L =< U
    ;
      Type = integer, integer(L), integer(U), L =< U
    )
  ).

flag_validate_in_interval([L, U], Type, Value):-
  check_same_type(Type, Value, L, U),
  Value >= L,
  Value =< U, !.
flag_validate_in_interval((L, U), Type, Value):-
  check_same_type(Type, Value, L, U),
  Value > L,
  Value < U, !.
flag_validate_in_interval(([L], U), Type, Value):-
  check_same_type(Type, Value, L, U),
  Value >= L,
  Value < U, !.
flag_validate_in_interval((L, [U]), Type, Value):-
  check_same_type(Type, Value, L, U),
  Value > L,
  Value =< U, !.
flag_validate_in_interval(([L], [U]), Type, Value):-
  check_same_type(Type, Value, L, U),
  Value >= L,
  Value =< U.

check_same_type(integer, Value, L, U):-
  integer(L), integer(U), integer(Value), !.
check_same_type(integer, Value, L, +inf):-
  integer(L), integer(Value), !.
check_same_type(integer, Value, -inf, U):-
  integer(U), integer(Value), !.
check_same_type(integer, Value, -inf, +inf):-
  integer(Value), !.
check_same_type(float, Value, L, U):-
  float(L), float(U), float(Value).
check_same_type(number, Value, L, U):-
  number(L), number(U), number(Value).

% This is only for messaging purposes. Each validation type should have one.

make_list_msg([H], H).
validatemake_list_msg([H|T], Msg/H):-
  make_list_msg(T, Msg).

validation_type_values(flag_validate_dummy, '').
validation_type_values(flag_validate_atom, 'any atom').
validation_type_values(flag_validate_atomic, 'any atomic').
validation_type_values(flag_validate_number, 'any number').
validation_type_values(flag_validate_integer, 'any integer').
validation_type_values(flag_validate_directory, 'any valid directory').
validation_type_values(flag_validate_file, 'any valid file').

validation_type_values(flag_validate_in_list(L), Msg):-
  reverse(L, R),
  make_list_msg(R, Msg).

validation_type_values(flag_validate_in_interval((L, U), Type), Domain):-
  number(L), number(U),
  atomic_concat([Type,' in (', L, ',', U, ')'], Domain).
validation_type_values(flag_validate_in_interval(([L], U), Type), Domain):-
  number(L), number(U),
  atomic_concat([Type,' in [', L, ',', U, ')'], Domain).
validation_type_values(flag_validate_in_interval((L, [U]), Type), Domain):-
  number(L), number(U),
  atomic_concat([Type,' in (', L, ',', U, ']'], Domain).
validation_type_values(flag_validate_in_interval(([L], [U]), Type), Domain):-
  number(L), number(U),
  atomic_concat([Type,' in [', L, ',', U, ']'], Domain).
validation_type_values(flag_validate_in_interval([L, U], Type), Domain):-
  number(L), number(U),
  atomic_concat([Type,' in [', L, ',', U, ']'], Domain).

validation_type_values(ValidationType, Domain):-
  flag_validation_syntactic_sugar(ValidationType, SyntacticSugar),
  validation_type_values(SyntacticSugar, Domain).

%
% Syntactic sugar validation types
%

flag_validation_syntactic_sugar(SyntacticSugar, Type):-
  recorded(flags, validation_syntactic_sugar(SyntacticSugar, Type), _Ref).

flag_add_validation_syntactic_sugar(SyntacticSugar, Type):-
    recordzifnot(flags, validation_syntactic_sugar(SyntacticSugar,Type), _Ref).


% End of validation predicates

