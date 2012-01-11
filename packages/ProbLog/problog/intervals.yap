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

:- module(intervals, [intervals_merge/3,
		      intervals_disjoin/3,
		      intervals_disjoin/4,
		      intervals_partition/2,
		      intervals_encode/2]).

:- style_check(all).
:- yap_flag(unknown,error).

:- use_module(library(lists), [member/2, reverse/2, select/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  intervals_merge(+Interval1,+Interval2,-ResultingInterval)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
intervals_merge(all,X,X).
intervals_merge(none,_,none).
intervals_merge(above(X),Other,Result) :-
	number(X),
	intervals_merge_above(Other,X,Result).
intervals_merge(below(X),Other,Result) :-
	number(X),
	intervals_merge_below(Other,X,Result).
intervals_merge(interval(X1,X2),Other,Result) :-
	number(X1),
	number(X2),
	intervals_merge_interval(Other,X1,X2,Result).

intervals_merge_above(all,X,above(X)).
intervals_merge_above(none,_,none).
intervals_merge_above(above(Y),X,above(Z)) :-
	number(Y),
	Z is max(X,Y).
intervals_merge_above(below(Y),X,Result) :-
	number(Y),
	(
	 X=<Y
	->
	 Result=interval(X,Y);
	 Result=none
	).
intervals_merge_above(interval(Y1,Y2),X,Result):-
	number(Y1),
	number(Y2),
	(
	 X=<Y1
	->
	 Result=interval(Y1,Y2);
	 (
	  X=<Y2
	 ->
	  Result=interval(X,Y2);
	  Result=none
	 )
	).

intervals_merge_below(all,X,below(X)).
intervals_merge_below(none,_,none).
intervals_merge_below(above(Y),X,Result) :-
	number(Y),
	(
	 Y=<X
	->
	 Result=interval(Y,X);
	 Result=none
	).
intervals_merge_below(below(Y),X,below(Z)) :-
	number(Y),
	Z is min(X,Y).
intervals_merge_below(interval(Y1,Y2),X,Result) :-
	number(Y1),
	number(Y2),
	(
	 X>=Y2
	->
	 Result=interval(Y1,Y2);
	 (
	  X>=Y1
	 ->
	  Result=interval(Y1,X);
	  Result=none
	 )
	).



intervals_merge_interval(all,X1,X2,interval(X1,X2)).
intervals_merge_interval(none,_,_,none).
intervals_merge_interval(above(X),Y1,Y2,Result) :-
	number(X),
	intervals_merge_above(interval(Y1,Y2),X,Result).
intervals_merge_interval(below(X),Y1,Y2,Result) :-
	number(X),
	intervals_merge_below(interval(Y1,Y2),X,Result).
intervals_merge_interval(interval(X1,X2),Y1,Y2,Result) :-
	number(X1),
	number(X2),
	(
	    X1<Y1
	->
	    intervals_merge_interval_intern(X1,X2,Y1,Y2,Result);
	    intervals_merge_interval_intern(Y1,Y2,X1,X2,Result)
	).
intervals_merge_interval_intern(_X1,X2,Y1,Y2,Result) :-
	(
	 Y1=<X2
	->
	 (
	  Y2=<X2
	 ->
	  Result=interval(Y1,Y2);
	  Result=interval(Y1,X2)
	 );
	 Result=none
	).
	 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_all([],List,List).
select_all([H|T],List,Remainder) :-
	once(select(H,List,TMP)),
	select_all(T,TMP,Remainder).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intervals_disjoin(X,P,In,Out) :-
	disjoin_intern(X,P,In),
	select_all(In,P,Out).
intervals_disjoin(X,P,In) :-
	disjoin_intern(X,P,In).
disjoin_intern(below(X),P,In) :-
	findall((interval(A,B),Tail),(member((interval(A,B),Tail),P),B=<X),Tmp),
	(
	 (member((below(Y),Tail),P),Y=<X)
	->
	 In=[(below(Y),Tail)|Tmp];
	 In=Tmp
	).
disjoin_intern(above(X),P,In) :-
	findall((interval(A,B),Tail),(member((interval(A,B),Tail),P),A>=X),Tmp),
	(
	 (member((above(Y),Tail),P),Y>=X)
	->
	 In=[(above(Y),Tail)|Tmp];
	 In=Tmp
	).
disjoin_intern(interval(X,Y),P,In) :-
	findall((interval(A,B),Tail),(member((interval(A,B),Tail),P),A>=X,B=<Y),In).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partitions a list of intervals into disjoined intervals
% together with their prefix
%
%
% ?- intervals_partition([below(10),above(5)],X).
%    X = [(below(5.0),[]),
%         (interval(5.0,10.0),[below(5.0)]),
%         (above(10.0),[interval(5.0,10.0),below(5.0)])]   
%
%
% intervals_partition(+List,-List)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intervals_partition([],[]).
intervals_partition([X|T],[(below(A), [])|T2]) :-
	once(extract_points([X|T],[],[A|PT])),
	to_interval(PT,A,[below(A)],T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% extracts from list of intervals all relevant constants
%
%    ?- intervals:extract_points([below(10),above(5)],[],L).
%        L = [5.0,10.0] ?
%
% extract_points(+List, +List, -List)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_points([],X,Y) :-
	sort(X,Y).
extract_points([below(A)|T],X,Y) :-
	A2 is float(A),
	extract_points(T,[A2|X],Y).
extract_points([above(A)|T],X,Y) :-
	A2 is float(A),
	extract_points(T,[A2|X],Y).
extract_points([interval(A,B)|T],X,Y) :-
	A2 is float(A),
	B2 is float(B),
	extract_points(T,[A2,B2|X],Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transforms a sorted list of constants into a list of
% intervals together with their prefixes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_interval([],A,Tail,[(above(A),Tail)]).
to_interval([B|T],A,Tail,[(interval(A,B),Tail)|T2]) :-
	to_interval(T,B,[interval(A,B)|Tail],T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% encodes an interval as an atom
%
% ?- intervals_encode(below(42),X).
%    X = lm1000h42
% ?- intervals_encode(above(23),X).
%    X = l23h1000
% ?- intervals_encode(interval(-2.3,4.2),X).
%    X = lm2d3h4d2 ? 
%
% intervals_encode(+Interval,-Atom)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intervals_encode(below(X),Atom) :-
	(
	 X < -1000
	->
	 X2 is 2*X;
	 X2 is -1000
	),
	intervals_encode(interval(X2,X),Atom).
intervals_encode(above(X),Atom) :-
	(
	 X > 1000
	->
	 X2 is 2*X;
	 X2 is 1000
	),
	intervals_encode(interval(X,X2),Atom).
intervals_encode(interval(Low,High),Atom) :-
	once(my_number_atom(Low,LowA)),
	once(my_number_atom(High,HighA)),
	atomic_concat([l,LowA,h,HighA],Atom).

my_number_atom(Number,Atom) :-
	% make float
	NumberF is Number+0.0,
	number_codes(NumberF,XC),
	reverse(XC,A),
	remove_prefix_zeros(A,B),
	remove_prefix_dot(B,C),
	fix_special_cases(C,D),
	reverse(D,DC),
	replace_special_characters(DC,DC_Final),

	atom_codes(Atom,DC_Final).

remove_prefix_zeros([],[]).
remove_prefix_zeros([X|T],Result) :-
	(
	 X==48  % 48 = '0'
	->
	 remove_prefix_zeros(T,Result);
	 Result=[X|T]
	).

remove_prefix_dot([],[]).
remove_prefix_dot([X|T],Result) :-
	(
	 X==46  % 46 = '.'
	->
	 Result=T;
	 Result=[X|T]
	).

fix_special_cases([],[48]).
fix_special_cases([H|T],Result) :-
	(
	 [H|T] == [48,45]     % ='0-'
	->
	 Result=[48];
	 Result=[H|T]
	).

replace_special_characters([],[]).
replace_special_characters([H|T],[H2|T2]) :-
	(
	 H==45   % '-'
	->
	 H2=109;		% 'm'
	 (
	  H==46
	 ->
	  H2=100;		% 'd'
	  H2=H
	 )
	),
	replace_special_characters(T,T2).
