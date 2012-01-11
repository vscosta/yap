%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-11-28 14:41:26 +0100 (Mon, 28 Nov 2011) $
%  $Revision: 6764 $
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


:- module(termhandling, [term_element/2,
			 propagate/5,
			 propagate_interpretation/3,
			 simplify/3,
			 list_to_disjunction/2,
			 list_to_conjunction/2,
			 op( 551, yfx, <=> ),
			 not/2]).

%   propagate( +OldTerm, +AtomToReplace, +ReplacementTermForAtom, -NewTerm, -ReplaceHasHappend).
%   simplify(+OldTerm,-NewTerm,-SimplificationHasHappend).
%   list_to_disjunction(+list,-Disjunction).
%   list_to_conjunction(+list,+Conjunction).

:- op( 551, yfx, <=> ).

:- style_check(all).
:- yap_flag(unknown,error).

%========================================================================
%=
%=
%= term_element(+GroundTerm, ?Atom)
%========================================================================

term_element((X,Y),Z) :-
	(term_element(X,Z);term_element(Y,Z)).
term_element((X;Y),Z) :-
	(term_element(X,Z);term_element(Y,Z)).
term_element((X <=> Y),Z) :-
	(term_element(X,Z);term_element(Y,Z)).
term_element(\+ X,Z) :-
	term_element(X,Z).
term_element('$atom'(X),'$atom'(X)) :-
	X \== true,
	X \== false.


% or(+Boolean,+Boolean,-Boolean)

or(true,_,true).
or(false,X,X).


% not(+Boolean,-Boolean)
not(true,false).
not(false,true).

%========================================================================
%=
%=
%=  propagate_interpretation(+GroundTerm, +ID, -GroundTerm)
%========================================================================

propagate_interpretation((X,Y),ID,(X2,Y2)) :-
	propagate_interpretation(X,ID,X2),
	propagate_interpretation(Y,ID,Y2).
propagate_interpretation((X;Y),ID,(X2;Y2)) :-
	propagate_interpretation(X,ID,X2),
	propagate_interpretation(Y,ID,Y2).
propagate_interpretation((X <=> Y),ID,(X2 <=> Y2)) :-
	propagate_interpretation(X,ID,X2),
	propagate_interpretation(Y,ID,Y2).
propagate_interpretation((\+ X), ID,\+ X2) :-
	propagate_interpretation(X,ID,X2).
propagate_interpretation('$atom'(X),ID,Value) :-
	(
	 user:known(ID,X,Value)
	->
	 true;
	 Value='$atom'(X)
	).
propagate_interpretation(true,_,true).
propagate_interpretation(false,_,false).

%========================================================================
%=
%=
%= 
%========================================================================

propagate((X,Y),A,AValue,(X2,Y2),Result) :-
	propagate(X,A,AValue,X2,Result1),
	propagate(Y,A,AValue,Y2,Result2),
	or(Result1,Result2,Result).
propagate((X;Y),A,AValue,(X2;Y2),Result) :-
	propagate(X,A,AValue,X2,Result1),
	propagate(Y,A,AValue,Y2,Result2),
	or(Result1,Result2,Result).
propagate((X <=> Y),A,AValue,(X2 <=> Y2),Result) :-
	propagate(X,A,AValue,X2,Result1),
	propagate(Y,A,AValue,Y2,Result2),
	or(Result1,Result2,Result).
propagate((\+ X), A, AValue,\+ X2,Result) :-
	propagate(X,A,AValue,X2,Result).
propagate('$atom'(X),'$atom'(A),AValue,ResultTerm,Propagated) :-
	(
	    X==A
	->
	    ResultTerm=AValue,
	    Propagated=true
	;
	    ResultTerm='$atom'(X),
	    Propagated=false
	).
propagate(true,_,_,true,false).
propagate(false,_,_,false,false).

%========================================================================
%=
%=
%= 
%========================================================================


negate_atom(\+ '$atom'(X),   '$atom'(X)).
negate_atom(   '$atom'(X),\+ '$atom'(X)).

occurs_check_and((X,_), A) :-
	occurs_check_and(X,A),
	!.
occurs_check_and((_,Y), A) :-
	occurs_check_and(Y,A).
occurs_check_and( '$atom'(X), '$atom'(X)).


occurs_check_or((X;_), A) :-
	occurs_check_or(X,A),
	!.
occurs_check_or((_;Y), A) :-
	occurs_check_and(Y,A).
occurs_check_or( '$atom'(X), '$atom'(X) ).
	

%========================================================================
%=
%= Tries to simplify Term. If succeeded Status=true and SimplifiedTerm
%= is bound to the simplified term. If not succeeded Status=false and
%= SimplifiedTerm=Term.
%=
%= Only works for ground terms! If non-ground terms are used, the
%= the result is undefined!!!
%=
%= simplify(+Term, -SimplifiedTerm, -Status)
%=
%========================================================================


simplify(Term,Term3,true) :-
	simplify_intern(Term,Term2,Result),
	Result==true,
	!,
	simplify(Term2,Term3,_).
simplify(Term,Term,false).
%-----------

simplify_intern( (X<=>Y),  NewTerm,   Result) :-
	simplify_intern_implication(X,Y,NewTerm,Result).
simplify_intern((\+ X), NewTerm,Result) :-
	simplify_intern_negation(X,NewTerm,Result).
simplify_intern( (X;Y),  NewTerm,   Result) :-
	simplify_intern_or(X,Y,NewTerm,Result).
simplify_intern( (X,Y),  NewTerm,   Result) :-
	simplify_intern_and(X,Y,NewTerm,Result).
simplify_intern('$atom'(X),'$atom'(X),false).
simplify_intern(true,true,false).
simplify_intern(false,false,false).


%-----------
simplify_intern_or( true,_,true,true) :-
	!.
simplify_intern_or( false,X,X,true) :-
	!.
simplify_intern_or(_,true,true,true) :-
	!.
simplify_intern_or(X,false,X,true) :-
	!.
% quite expensive
% simplify_intern_or(X,Y,true,true) :-
% 	negate_atom(X,X2),
% 	occurs_check_or(Y,X2),
% 	!.
simplify_intern_or(X,Y,(X2;Y2),Result) :-
	!,
	simplify_intern(X,X2,Result1),
	simplify_intern(Y,Y2,Result2),
	or(Result1,Result2,Result).
%-----------
simplify_intern_and( true,X,X,true) :-
	!.
simplify_intern_and( false,_,false,true) :-
	!.
simplify_intern_and(X,true,X,true) :-
	!.
simplify_intern_and(_,false,false,true) :-
	!.
% quite expensive
% simplify_intern_and(X,Y,false,true) :-
% 	negate_atom(X,X2),
% 	occurs_check_and(Y,X2),
% 	!.
simplify_intern_and(X,Y,(X2,Y2),Result) :-
	!,
	simplify_intern(X,X2,Result1),
	simplify_intern(Y,Y2,Result2),
	or(Result1,Result2,Result).

%-----------
simplify_intern_implication(true,Y,Y,true) :-
	!.
simplify_intern_implication(false,Y,(\+ Y),true) :-
	!.
simplify_intern_implication(X,true,X,true) :-
	!.
simplify_intern_implication(X,false,(\+ X),true) :-
	!.
simplify_intern_implication(X,Y,(X <=> Y2), Result) :-
	!,
	simplify_intern(Y,Y2,Result).

%-----------

simplify_intern_negation(true,false,true).
simplify_intern_negation(false,true,true).
simplify_intern_negation((\+ X),X,true).
simplify_intern_negation((A,B),Term,true) :-
	simplify_intern_or( (\+ A), (\+ B), Term, _).
simplify_intern_negation((A;B),Term,true) :-
	simplify_intern_and( (\+ A), (\+ B), Term, _).
simplify_intern_negation('$atom'(X),(\+ '$atom'(X)),false).

%========================================================================
%=
%=
%= list_to_disjunction(+List,-Disjunction)
%========================================================================
list_to_disjunction([A,B|T],(A;T2)) :-
	!,
	list_to_disjunction([B|T],T2).
list_to_disjunction([A],A).
list_to_disjunction([],false).

%========================================================================
%=
%=
%= list_to_conjunction(+List,(A,T2)-Conjunction) :-
%========================================================================

list_to_conjunction([A,B|T],(A,T2)) :-
	!,
	list_to_conjunction([B|T],T2).
list_to_conjunction([A],A).
list_to_conjunction([],true).
