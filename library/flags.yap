%%% -*- Mode: Prolog; -*-

/**
 * @file   library/flags.yap
 * @author Theofrastos Mantadelis, Bernd Gutmann, Paulo Moura
 * @date   Tue Nov 17 15:18:02 2015
 * 
 * @brief  Flag Manipulation in Prolog
 * 
 * 
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
%  Sugestions: Bernd Gutmann, Paulo Moura
%  $Date: 2011-02-15 13:33:01 +0100 (Tue, 15 Feb 2011) $
%  $Revision: 15 $
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


:- module(flags, [flag_define/2,
                  flag_define/5,
                  flag_define/7,
                  flag_set/2,
                  flag_set/3,
                  flag_unsafe_set/2,
                  flag_get/2,
                  flags_reset/0,
                  flags_reset/1,
                  flags_save/1,
                  flags_load/1,
                  flag_groups/1,
                  flag_group_chk/1,
                  flag_help/0,
                  flags_print/0,
                  defined_flag/7]).

/**
* @defgroup flags Flag Manipulation in Prolog
* @ingroup library
*
* Routines to manipulate flags: they allow defining, set,
* resetting.
* @{
*/


:- use_module(library(lists), [append/3, memberchk/2, member/2]).

:- style_check(all).
:- yap_flag(unknown, error).

:- dynamic(['$defined_flag$'/7, '$store_flag_value$'/2]).
:- meta_predicate(flag_define(+, +, +, ?, ?, ?, :)).
:- meta_predicate(flag_define(+, :)).
:- meta_predicate(validate(+, :, ?, +)).
:- multifile(flags_type_definition/3).

flag_define(FlagName, InputOptions):-
  strip_module(InputOptions, Module, UserOptions),
  Defaults = [flag_group(general), flag_type(nonvar), default_value(true), description(FlagName), access(read_write), handler(true)],
  append(UserOptions, Defaults, Options),
  memberchk(flag_group(FlagGroup), Options),
  memberchk(flag_type(FlagType), Options),
  memberchk(default_value(DefaultValue), Options),
  memberchk(description(Description), Options),
  memberchk(access(Access), Options),
  memberchk(handler(Handler), Options),
  flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler).

flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description):-
  flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, read_write, true).

flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, MHandler):-
  strip_module(MHandler, Module, Handler),
  nonvar(FlagName),
  nonvar(FlagGroup),
  nonvar(FlagType),
  nonvar(Access),
  nonvar(Handler), !,
  (\+ atom(FlagName) ->
    throw(error(type_error(atom, FlagName), message('Flag name needs to be an atom.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler))))
  ; \+ atom(FlagGroup) ->
    throw(error(type_error(atom, FlagGroup), message('Flag group needs to be an atom.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler))))
  ; \+ flag_type(FlagType) ->
    throw(error(domain_error(flag_type, FlagType), message('Unknown flag type.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Module:Handler))))
  ; \+ validate_type(FlagType) ->
    throw(error(evaluation_error(type_validation), message('Validation of flag type failed, check custom domain.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler))))
  ; '$defined_flag$'(FlagName, _FlagGroup, _FlagType, _DefaultValue, _Description, _Access, _Handler) ->
    throw(error(permission_error(create, flag, FlagName), message('Re-defining a flag is not allowed.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler))))
  ; \+ memberchk(Access, [read_write, read_only, hidden, hidden_read_only]),
    throw(error(domain_error(access, Access), message('Wrong access attribute, available are: read_write, read_only, hidden, hidden_read_only.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler))))
  ; \+ callable(Handler) -> % the Handler comes from: strip_module(MHandler, Module, Handler)
    throw(error(type_error(callable, Handler), message('Flag handler needs to be callable.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler))))
  ;
    validate(FlagType, Module:Handler, DefaultValue, FlagName),
    assertz('$defined_flag$'(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Module:Handler)),
    assertz('$store_flag_value$'(FlagName, DefaultValue)),
    (Handler == true ->
      true
    ;
      call(Module:Handler, stored, DefaultValue)
    )
  ).
flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Handler):-
  throw(error(instantiation_error, message('Flag name, group, type, access and handler need to be instantiated.', flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Handler)))).

flag_groups(FlagGroups):-
  all(FlagGroup, ('$defined_flag$'(_FlagName, FlagGroup, _FlagType, _DefaultValue, _Description, Access, _Handler), Access \== hidden, Access \== hidden_read_only), FlagGroups).

flag_group_chk(FlagGroup):-
  nonvar(FlagGroup),
  '$defined_flag$'(_FlagName, FlagGroup, _FlagType, _DefaultValue, _Description, _Access, _Handler), !.

flag_type(Type):-
  flags_type_definition(Type, _, _).

%% @pred flags_type_definition(TypeName, TypeHandler, TypeValidator).
flags_type_definition(nonvar, nonvar, true).
flags_type_definition(atom, atom, true).
flags_type_definition(atomic, atomic, true).
flags_type_definition(integer, integer, true).
flags_type_definition(float, float, true).
flags_type_definition(number, number, true).
flags_type_definition(ground, ground, true).
flags_type_definition(compound, compound, true).
flags_type_definition(is_list, is_list, true).
flags_type_definition(callable, callable, true).
flags_type_definition(in_interval(Type, Interval), in_interval(Type, Interval), in_interval(Type, Interval)).
flags_type_definition(integer_in_interval(Interval), in_interval(integer, Interval), in_interval(integer, Interval)).
flags_type_definition(positive_integer, in_interval(integer, (0, (+inf))), true).
flags_type_definition(non_negative_integer, in_interval(integer, ([0], (+inf))), true).
flags_type_definition(negative_integer, in_interval(integer, ((-inf), 0)), true).
flags_type_definition(float_in_interval(Interval), in_interval(float, Interval), in_interval(float, Interval)).
flags_type_definition(positive_float, in_interval(float, (0.0, (+inf))), true).
flags_type_definition(non_negative_float, in_interval(float, ([0.0], (+inf))), true).
flags_type_definition(negative_float, in_interval(float, ((-inf), 0.0)), true).
flags_type_definition(number_in_interval(Interval), in_interval(number, Interval), in_interval(number, Interval)).
flags_type_definition(positive_number, in_interval(number, (0.0, (+inf))), true).
flags_type_definition(non_negative_number, in_interval(number, ([0.0], (+inf))), true).
flags_type_definition(negative_number, in_interval(number, ((-inf), 0.0)), true).
flags_type_definition(in_domain(Domain), in_domain(Domain), in_domain(Domain)).
flags_type_definition(boolean, in_domain([true, false]), true).
flags_type_definition(switch, in_domain([on, off]), true).

in_domain(Domain):-
  ground(Domain),
  is_list(Domain).
in_domain(Domain, Value):-
  ground(Value),
  memberchk(Value, Domain).

in_interval(Type, Interval):-
  is_list(Interval), !,
  Interval \== [],
  in_interval_conj(Type, Interval).
in_interval(Type, Interval):-
  in_interval_single(Type, Interval).

in_interval_conj(_Type, []).
in_interval_conj(Type, [Interval|Rest]):-
  in_interval_single(Type, Interval),
  in_interval_conj(Type, Rest).

in_interval_single(Type, ([Min], [Max])):-
  !, call(Type, Min),
  call(Type, Max),
  Min =< Max.

in_interval_single(Type, ([Min], Max)):-
  !, call(Type, Min),
  type_or_inf(Type, Max),
  Min < Max.

in_interval_single(Type, (Min, [Max])):-
  !, type_or_inf(Type, Min),
  call(Type, Max),
  Min < Max.

in_interval_single(Type, (Min, Max)):-
  type_or_inf(Type, Min),
  type_or_inf(Type, Max),
  Min < Max,
  Max - Min > 0.0.

type_or_inf(Type, Value):-
  nonvar(Type), nonvar(Value),
  Value == (+inf), !.
type_or_inf(Type, Value):-
  nonvar(Type), nonvar(Value),
  Value == (-inf), !.
type_or_inf(Type, Value):- call(Type, Value).

in_interval(Type, [Interval|_Rest], Value):-
  in_interval(Type, Interval, Value), !.
in_interval(Type, [_Interval|Rest], Value):-
  in_interval(Type, Rest, Value).

in_interval(Type, ([Min], [Max]), Value):-
  !, call(Type, Value),
  Value >= Min,
  Value =< Max.

in_interval(Type, ([Min], Max), Value):-
  !, call(Type, Value),
  Value >= Min,
  Value < Max.

in_interval(Type, (Min, [Max]), Value):-
  !, call(Type, Value),
  Value > Min,
  Value =< Max.

in_interval(Type, (Min, Max), Value):-
  call(Type, Value),
  Value > Min,
  Value < Max.

validate_type(Type):-
  flags_type_definition(Type, _, TypeValidater),
  call(TypeValidater).

validate(FlagType, Handler, Value, FlagName):-
  strip_module(Handler, _Module, true),
  !, flags_type_definition(FlagType, FlagValidator, _),
  (call(FlagValidator, Value) ->
    true
  ;
    throw(error(validation_error(FlagType, Value), message('Validation of value fails.', validate(FlagType, Value, FlagName))))
  ).
validate(FlagType, Handler, Value, FlagName):-
  flags_type_definition(FlagType, FlagValidator, _),
  ((call(Handler, validating, Value), (call(FlagValidator, Value); call(Handler, validate, Value))) ->
    call(Handler, validated, Value)
  ;
    throw(error(validation_error(FlagType, Value), message('Validation of value fails.', validate(FlagType, Handler, Value, FlagName))))
  ).

flag_set(FlagName, FlagValue):-
  flag_set(FlagName, _OldValue, FlagValue).
flag_set(FlagName, OldValue, FlagValue):-
  atom(FlagName),
  '$defined_flag$'(FlagName, _FlagGroup, FlagType, _DefaultValue, _Description, Access, Module:Handler), !,
  (Access \== read_only, Access \== hidden_read_only ->
    validate(FlagType, Module:Handler, FlagValue, FlagName),
    retract('$store_flag_value$'(FlagName, OldValue)),
    assertz('$store_flag_value$'(FlagName, FlagValue)),
    (Handler == true ->
      true
    ;
      call(Module:Handler, stored, FlagValue)
    )
  ;
    throw(error(permission_error(set, flag, FlagName), message('Setting the flag value is not allowed.',flag_set(FlagName, OldValue, FlagValue))))
  ).
flag_set(FlagName, OldValue, FlagValue):-
  throw(error(existence_error(flag, FlagName), message('The flag is not defined.', flag_set(FlagName, OldValue, FlagValue)))).

flag_unsafe_set(FlagName, FlagValue):-
  retract('$store_flag_value$'(FlagName, _)),
  assertz('$store_flag_value$'(FlagName, FlagValue)).

flag_get(FlagName, FlagValue):-
  \+ '$store_flag_value$'(FlagName, _),
  throw(error(existence_error(flag, FlagName), message('The flag is not defined.', flag_get(FlagName, FlagValue)))).
flag_get(FlagName, FlagValue):-
  '$store_flag_value$'(FlagName, FlagValue).

flags_reset:-
  retractall('$store_flag_value$'(_, _)),
  '$defined_flag$'(FlagName, _FlagGroup, _FlagType, DefaultValue, _Description, _Access, Module:Handler),
  assertz('$store_flag_value$'(FlagName, DefaultValue)),
  (Handler == true ->
    true
  ;
    call(Module:Handler, stored, DefaultValue)
  ),
  fail.
flags_reset.

flags_reset(FlagGroup):-
  '$defined_flag$'(FlagName, FlagGroup, _FlagType, DefaultValue, _Description, _Access, Module:Handler),
  retractall('$store_flag_value$'(FlagName, _)),
  assertz('$store_flag_value$'(FlagName, DefaultValue)),
  (Handler == true ->
    true
  ;
    call(Module:Handler, stored, DefaultValue)
  ),
  fail.
flags_reset(_).

flags_save(FileName):-
  tell(FileName),
  catch(('$store_flag_value$'(FlagName, Value),
         write_canonical('$store_flag_value$'(FlagName, Value)),
         write('.'), nl),
         Exception, clean_and_throw(told, Exception)),
  fail.
flags_save(_FileName):-
  told.

flags_load(FileName):-
  see(FileName),
  catch((read('$store_flag_value$'(FlagName, Value)),
         flag_set(FlagName, Value)),
         Exception, clean_and_throw(seen, Exception)),
  fail.
flags_load(_FileName):-
  seen.

clean_and_throw(Action, Exception):-
  Action,
  throw(Exception).

flag_help:-
  format('This is a short tutorial for the flags library.~nExported predicates:~n'),
  format('     flag_define/5    : defines a new flag without a handler~n'),
  format('       flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description)~n'),
  format('     flag_define/6    : defines a new flag with a handler~n'),
  format('       flag_define(FlagName, FlagGroup, FlagType, DefaultValue, Description, Handler)~n'),
  format('            FlagName     : the name of the flag~n'),
  format('            FlagGroup    : the name of the flag group~n'),
  format('            FlagType     : the type of the flag available types are:~n'),
  flag_help_types,
  format('            DefaultValue : the default value for the flag~n'),
  format('            Description      : a flag description~n'),
  format('            Handler      : a handler~n'),
  flags:flag_help_handler,
  format('     flag_groups/1    : returns all the flag groups in a list~n'),
  format('     flag_group_chk/1 : checks if a group exists~n'),
  format('     flag_set/2       : sets the value of a flag~n'),
  format('     flag_get/2       : gets the value of a flag~n'),
  format('     flag_store/2     : sets the value of a flag ignoring all tests and handlers~n'),
  format('     flag_reset/0     : resets all flags to their default value~n'),
  format('     flag_reset/1     : resets all flags of a group to their default value~n'),
  format('     flag_help/0      : this screen~n'),
  format('     flags_print/0    : shows the current flags/values~n').
flag_help_types:-
    flag_type(FlagType),
    format('                   ~w~n', [FlagType]),
    fail.
flag_help_types.

flag_help_handler:-
  format('                   Handler important notes:~n'),
  format('                     Conjuction:    external_handler(validating, Value):-...~n'),
  format('                     Disjunction:   external_handler(validate, Value):-...~n'),
  format('                     After:         external_handler(validated, Value):-...~n'),
  format('                     After set:     external_handler(stored, Value):-...~n'),
  format('                     this is implemented as (validating,(original;validated))~n'),
  format('                     validating|original|validate|result~n'),
  format('                        true   | true   | true   | true~n'),
  format('                        true   | true   | fail   | true~n'),
  format('                        true   | fail   | true   | true~n'),
  format('                        true   | fail   | fail   | fail~n'),
  format('                        fail   | true   | true   | fail~n'),
  format('                        fail   | true   | fail   | fail~n'),
  format('                        fail   | fail   | true   | fail~n'),
  format('                        fail   | fail   | fail   | fail~n'),
  format('                     Default behaviour is validating->true, validate->fail~n'),
  format('                     To completly replace original set validate->true~n'),
  format('                     To add new values to original set validating->true~n'),
  format('                     To remove values from original set validate->fail~n'),
  format('              Example definition with a handler:~n'),
  format('                flag_define(myflag, mygroup, in_interval(integer, [(-5, 5),([15],[25])]), 0, description, my_handler).~n'),
  format('                my_handler(validate, Value):-Value is 10.~n'),
  format('                my_handler(validating, Value).~n'),
  format('                my_handler(validated, Value).~n'),
  format('                my_handler(stored, Value).~n'),
  format('                This has defined a flag that accepts integers (-5,5)v[15,25].~n'),
  format('                The handler adds the value 10 in those.~n').

flags_print:-
  flag_groups(Groups),
  forall(member(Group, Groups), flags_print(Group)).
flags_print(Group):-
  format('    ~w:~n~w~38+ ~w~19+ ~w~10+ ~w~10+~n', [Group, 'Description', 'Domain', 'Flag', 'Value']),
  fail.
flags_print(FlagGroup):-
  '$defined_flag$'(FlagName, FlagGroup, FlagType, _DefaultValue, Description, Access, _Handler),
  Access \== hidden, Access \== hidden_read_only,
  flag_get(FlagName, Value),
  format('~w~38+ ~w~19+ ~w~10+ ~q~10+~n', [Description, FlagType, FlagName, Value]), 
  fail.
flags_print(_).

defined_flag(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Handler):-
  '$defined_flag$'(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Handler),
  Access \== hidden, Access \== hidden_read_only.
defined_flag(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Handler):-
  nonvar(FlagName), nonvar(FlagGroup),
  '$defined_flag$'(FlagName, FlagGroup, FlagType, DefaultValue, Description, Access, Handler).

%% @}
