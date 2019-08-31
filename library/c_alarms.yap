%%% -*- Mode: Prolog; -*-
/**
 * @file   c_alarms.yap
 * @author Theofrastos Mantadelis
 * @date   Tue Nov 17 14:50:03 2015
 * 
 * @brief  Concurrent alarms
 * 
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Concurrent alarms was developed at Katholieke Universiteit Leuven
%
%  Copyright 2010
%  Katholieke Universiteit Leuven
%
%  Contributions to this file:
%  Author: Theofrastos Mantadelis
%  $Date: 2011-02-04 16:04:49 +0100 (Fri, 04 Feb 2011) $
%  $Revision: 11 $
%  Contributions: The timer implementation is inspired by Bernd Gutmann's timers
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

:- module(c_alarms, [set_alarm/3,
                     unset_alarm/1,
                     time_out_call_once/3,
                     timer_start/1,
                     timer_restart/1,
                     timer_stop/2,
                     timer_elapsed/2,
                     timer_pause/2]).

/** @defgroup c_alarms Concurrent Alarms
@ingroup library
@{

This library provides a concurrent signals.  To use it use:
`:-use_module(library(c_alarms))`.
*/


:- use_module(library(lists), [member/2, memberchk/2, delete/3]).
:- use_module(library(ordsets), [ord_add_element/3]).
:- use_module(library(apply_macros), [maplist/3]).

:- dynamic('$timer'/3).

:- meta_predicate(set_alarm(+, 0, -)).
:- meta_predicate(time_out_call_once(+, 0, -)).
:- meta_predicate(prove_once(0)).

:- initialization(local_init).

local_init:-
  bb_put(alarms, []),
  bb_put(identity, 0).

get_next_identity(ID):-
  bb_get(identity, ID),
  NID is ID + 1,
  bb_put(identity, NID).

set_alarm(Seconds, Execute, ID):-
  bb_get(alarms, []),
  get_next_identity(ID), !,
  bb_put(alarms, [alarm(Seconds, ID, Execute)]),
  alarm(Seconds, alarm_handler, _).

%% @pred set_alarm(+Seconds, +Execute, -ID)
%
%   calls Executes after a time interval of Seconds
%   ID is returned to be able to unset the alarm (the call will not be executed)
%   set_alarm/3 supports multiple & nested settings of alarms.
%   Known Bug: There is the case that an alarm might trigger +-1 second of the set time.
%
set_alarm(Seconds, Execute, ID):-
  get_next_identity(ID), !,
  bb_get(alarms, [alarm(CurrentSeconds, CurrentID, CurrentExecute)|Alarms]),
  alarm(0, true, Remaining),
  Elapsed is CurrentSeconds - Remaining - 1,
  maplist(subtract(Elapsed), [alarm(CurrentSeconds, CurrentID, CurrentExecute)|Alarms], RemainingAlarms),
  ord_add_element(RemainingAlarms, alarm(Seconds, ID, Execute), [alarm(NewSeconds, NewID, NewToExecute)|NewAlarms]),
  bb_put(alarms, [alarm(NewSeconds, NewID, NewToExecute)|NewAlarms]),
  alarm(NewSeconds, alarm_handler, _).
set_alarm(Seconds, Execute, ID):-
  throw(error(permission_error(create, alarm, set_alarm(Seconds, Execute, ID)), 'Non permitted alarm identifier.')).

subtract(Elapsed, alarm(Seconds, ID, Execute), alarm(NewSeconds, ID, Execute)):-
  NewSeconds is Seconds - Elapsed.

%% @pred unset_alarm(+ID)
%
%   It will unschedule the alarm.
%   It will not affect other concurrent alarms.
%
unset_alarm(ID):-
  \+ ground(ID),
  throw(error(instantiation_error, 'Alarm ID needs to be instantiated.')).
unset_alarm(ID):-
  bb_get(alarms, Alarms),
  \+ memberchk(alarm(_Seconds, ID, _Execute), Alarms),
  throw(error(existence_error(alarm, unset_alarm(ID)), 'Alarm does not exist.')).
unset_alarm(ID):-
  alarm(0, true, Remaining),
  bb_get(alarms, Alarms),
  [alarm(Seconds, _, _)|_] = Alarms,
  Elapsed is Seconds - Remaining - 1,
  delete_alarm(Alarms, ID, NewAlarms),
  bb_put(alarms, NewAlarms),
  (NewAlarms = [alarm(NewSeconds, _, _)|_] ->
    RemainingSeconds is NewSeconds - Elapsed,
    alarm(RemainingSeconds, alarm_handler, _)
  ;
    true
  ).

delete_alarm(Alarms, ID, NewAlarms):-
  memberchk(alarm(Seconds, ID, Execute), Alarms),
  delete(Alarms, alarm(Seconds, ID, Execute), NewAlarms).

alarm_handler:-
  bb_get(alarms, [alarm(_, _, CurrentExecute)|[]]),
  bb_put(alarms, []),
  call(CurrentExecute).
alarm_handler:-
  bb_get(alarms, [alarm(Elapsed, CurrentID, CurrentExecute)|Alarms]),
  maplist(subtract(Elapsed), Alarms, NewAlarms),
  find_zeros(NewAlarms, ZeroAlarms),
  findall(alarm(S, ID, E), (member(alarm(S, ID, E), NewAlarms), S > 0), NonZeroAlarms),
  bb_put(alarms, NonZeroAlarms),
  (NonZeroAlarms = [alarm(NewSeconds, _, _)|_] ->
    alarm(NewSeconds, alarm_handler, _)
  ;
    true
  ),
  execute([alarm(0, CurrentID, CurrentExecute)|ZeroAlarms]).

find_zeros([], []).
find_zeros([alarm(0, ID, E)|T], [alarm(0, ID, E)|R]):-
  find_zeros(T, R).
find_zeros([alarm(S, _, _)|T], R):-
  S > 0,
  find_zeros(T, R).

execute([]).
execute([alarm(_, _, Execute)|R]):-
  call(Execute),
  execute(R).

%% time_out_call(+Seconds, +Goal, -Return)
%
%   It will will execute the closure Goal and returns its success or failure at Return.
%   If the goal times out in Seconds then Return = timeout.
time_out_call_once(Seconds, Goal, Return):-
  bb_get(identity, ID),
  set_alarm(Seconds, throw(timeout(ID)), ID),
  catch((
    prove_once(Goal, Return),
    unset_alarm(ID))
  , Exception, (
    (Exception == timeout(ID) ->
      Return = timeout
    ;
      unset_alarm(ID),
      throw(Exception)
    ))).

prove_once(Goal, success):-
  once(Goal), !.
prove_once(_Goal, failure).

timer_start(Name):-
  \+ ground(Name),
  throw(error(instantiation_error, 'Timer name needs to be instantiated.')).
timer_start(Name):-
  '$timer'(Name, _, _),
  throw(error(permission_error(create, timer, timer_start(Name)), 'Timer already exists.')).
timer_start(Name):-
  statistics(walltime, [StartTime, _]),
  assertz('$timer'(Name, running, StartTime)).

timer_restart(Name):-
  \+ ground(Name),
  throw(error(instantiation_error, 'Timer name needs to be instantiated.')).
timer_restart(Name):-
  \+ '$timer'(Name, _, _), !,
  statistics(walltime, [StartTime, _]),
  assertz('$timer'(Name, running, StartTime)).
timer_restart(Name):-
  retract('$timer'(Name, running, _)), !,
  statistics(walltime, [StartTime, _]),
  assertz('$timer'(Name, running, StartTime)).
timer_restart(Name):-
  retract('$timer'(Name, paused, Duration)),
  statistics(walltime, [StartTime, _]),
  Elapsed is StartTime - Duration,
  assertz('$timer'(Name, running, Elapsed)).

timer_stop(Name, Elapsed):-
  \+ '$timer'(Name, _, _),
  throw(error(existence_error(timer, timer_stop(Name, Elapsed)), 'Timer does not exist.')).
timer_stop(Name, Elapsed):-
  retract('$timer'(Name, running, StartTime)), !,
  statistics(walltime, [EndTime, _]),
  Elapsed is EndTime - StartTime.
timer_stop(Name, Elapsed):-
  retract('$timer'(Name, paused, Elapsed)).

timer_elapsed(Name, Elapsed):-
  \+ '$timer'(Name, _, _),
  throw(error(existence_error(timer, timer_elapsed(Name, Elapsed)), 'Timer does not exist.')).
timer_elapsed(Name, Elapsed):-
  '$timer'(Name, running, StartTime), !,
  statistics(walltime, [EndTime, _]),
  Elapsed is EndTime - StartTime.
timer_elapsed(Name, Elapsed):-
  '$timer'(Name, paused, Elapsed).

timer_pause(Name, Elapsed):-
  \+ '$timer'(Name, _, _),
  throw(error(existence_error(timer, timer_pause(Name, Elapsed)), 'Timer does not exist.')).
timer_pause(Name, Elapsed):-
  '$timer'(Name, paused, _),
  throw(error(permission_error(timer, timer_pause(Name, Elapsed)), 'Timer already paused.')).
timer_pause(Name, Elapsed):-
  retract('$timer'(Name, _, StartTime)),
  statistics(walltime, [EndTime, _]),
  Elapsed is EndTime - StartTime,
  assertz('$timer'(Name, paused, Elapsed)).

/**
@}
*/
