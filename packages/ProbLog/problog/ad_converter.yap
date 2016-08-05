%%% -*- Mode: Prolog; -*-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  $Date: 2011-11-28 16:17:25 +0100 (Mon, 28 Nov 2011) $
%  $Revision: 6765 $
%
%  This file is part of ProbLog
%  http://dtai.cs.kuleuven.be/problog
%
%  ProbLog was developed at Katholieke Universiteit Leuven
%
%  Copyright 2009
%  Angelika Kimmig, Vitor Santos Costa, Bernd Gutmann
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

:- module(ad_converter,[term_expansion_intern_ad/4,
			op(1149, yfx, <-- ),
			op( 550, yfx, :: )
		       ]).

% general yap modules
:- use_module(library(lists),[member/2,append/3]).
:- use_module(flags).

:- style_check(all).
:- yap_flag(unknown,error).

:- discontiguous user:(<--)/2, problog:(<--)/2.
:- discontiguous user:myclause/1, problog:myclause/1. % notation of ADs in LFI-ProbLog

:- op( 550, yfx, :: ).

% for annotated disjunctions
:- op(1149, yfx, <-- ).

:- initialization(problog_define_flag(show_ad_compilation,problog_flag_validate_boolean,'show compiled code for ADs',false,annotated_disjunctions)).
:- initialization(problog_define_flag(ad_cpl_semantics,problog_flag_validate_boolean,'use CP-logics semantics for ADs',true,annotated_disjunctions)).
:- initialization(problog_define_flag(ad_sumto1_learning,problog_flag_validate_boolean,'make p_i sum to 1 for learning',true,annotated_disjunctions)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% term_expansion_intern_ad( +AD, +Module, +Mode, -ListOfAtoms)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- bb_put(ad_converter_unique_id,1).

term_expansion_intern_ad((Head<--Body), Module, Mode, [user:ad_intern((Head<--Body),ID,Aux_Facts)|Result]) :-
	% the internal ID for the annotated disjunction
	bb_get(ad_converter_unique_id,ID),
	ID2 is ID+1,
	bb_put(ad_converter_unique_id,ID2),

	% if CPL semantics is on we need to add all body variables to the
	% auxilliary probabilistic facts to ensure that each grounding
	% of an AD "triggers" a new CP event
	(
	 problog_flag(ad_cpl_semantics,true) ->
	 term_variables(Body,Body_Vars)
	;
	 Body_Vars=[]
	),

	% construct the auxilliary facts we need to represent the AD
	(
	 % if it's a tunable AD create tunable auxilliary facts
	 proper_tunable_ad_head(Head) ->
	 create_tunable_ad_aux_facts(Head,Body_Vars,ID,1,Aux_Facts)
	;
	 % if it's a regular AD create auxilliary facts
	 proper_ad_head(Head,0.0) ->
	 create_ad_aux_facts(Head,Body_Vars,ID,1,0.0,Aux_Facts)
	;
	 % neither nor, let's complain
	 throw(error(invalid_annotated_disjunction,(Head<--Body)))
	),

	% call term_expansion for the aux facts, this has the same effect
	% as if the user had defined the facts in the original file
	findall(problog:Atom,(
			      member(F,Aux_Facts),
			      once(problog:term_expansion_intern(F,Module,Atom))
			     ),Result_Atoms),

	% construct the auxilliary clauses

	create_aux_bodies(Head,Body_Vars,Body,ID,1,Aux_Facts,Mode,Aux_Clauses),

	(
	 Mode==lfi_learning
        ->
	 findall(Module:myclause(H,B),member((H:-B),Aux_Clauses),Result,Result_Atoms)
	;
	 findall(Module:B,member(B,Aux_Clauses),Result,Result_Atoms)
	),
	(
	 problog_flag(show_ad_compilation,true)
	->
	 (
	  format('Compiling the annotated disjunction~n  ~q~ninto the following code~n',[(Head<--Body)]),
	  format('================================================~n',[]),
	  forall(member(F,Aux_Facts),format('   ~q.~n',[F])),
	  format('    - - - - - - - - - - - - - - - - - - - - - - ~n',[]),
	  forall(member(B,Aux_Clauses),format('   ~q.~n',[B])),
	  format('================================================~2n',[])
	 )
        ;
	 true
	).
term_expansion_intern_ad( (Head<--Body),_,_) :-
	format(chars(Error), 'Error at compiling the annotated disjunction ~q<--~m.',[Head,Body]),
	print_message(error,Error),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% proper_ad_head(+Head, +Acc)
%
% this predicate succeeds if Head is valid disjunction
% of probabilistic facts as used in the head of an AD
% in particular, it checks that all probabilities are
% valid and the sum does not exceed 1.0
%
% if will throw an exception if any of the probabilties P
%  P::A
% can not be evaluated using is/2
%
%   ?- proper_ad_head( 0.1::a, 0.1).
% yes
%   ?- proper_ad_head( (0.1::a,0.8::b), 0.1).
% no
%   ?- proper_ad_head( (0.1::a;0.8::b), 0.1).
% yes
%   ?- proper_ad_head( (0.1::a;0.8::b;0.2::c), 0.1).
% no
%   ?- proper_ad_head( (0.1::a;0.4::true), 0.1).
% no
%   ?- ad_converter:proper_ad_head( (1/2::a;0.4::foo(X)), 0.1).
% true
%   ?- ad_converter:proper_ad_head( (goo::a;0.4::foo(X)), 0.1).
%     ERROR at  clause 2 of ad_converter:proper_ad_head/2 !!
%     TYPE ERROR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proper_ad_head( P :: A, Acc) :-
	P>=0.0,
	P+Acc=<1.0,
	\+ var(A),
	\+ system_predicate(_,A),
	once((atom(A);compound(A))).

proper_ad_head((P :: A;T),Acc) :-
	\+ var(A),
	\+ system_predicate(_,A),
	once((atom(A);compound(A))),
	P>=0.0,
	Acc2 is P+Acc,
	proper_ad_head(T,Acc2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% proper_tunable_ad_head(+Head)
%
% this predicate succeeds if Head is valid disjunction of
% tunable probabilistic facts as used in the head of an AD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proper_tunable_ad_head( t(_)::A ) :-
	\+ var(A),
	\+ system_predicate(_,A),
	once((atom(A);compound(A))).

proper_tunable_ad_head( ( t(_)::A ;T) ) :-
	\+ var(A),
	\+ system_predicate(_,A),
	once((atom(A);compound(A))),
	proper_tunable_ad_head(T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_mws_atom(+Atom,+Body_Vars,+ID,+Pos,-A2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_mws_atom(A,Body_Vars,ID,Pos,A2) :-
	A =.. [_F|Args],
	append(Args,Body_Vars,Args2),
	atomic_concat([mvs_fact_,ID,'_',Pos],F2),
	A2 =.. [F2|Args2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_ad_aux_facts(+Head,+Vars,+ID,+POS,+Acc,-Facts)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_ad_aux_facts(P::_, _, _, _, Acc, []) :-
	% if the probabilities in the head of the AD
	% sum up to 1.0 drop the last aux fact
	abs(Acc+P-1.0) < 0.0000001,
	!.
create_ad_aux_facts(P::Atom, Body_Vars, ID, Pos, Acc, [P1::ProbFact]) :-
	create_mws_atom(Atom,Body_Vars,ID,Pos,ProbFact),
	(
	 (P=:=0; Acc=:=0)->
	 P1 is P
	;
	 P1 is  min(P/(1-Acc),1.0)
	).
create_ad_aux_facts((P::Atom;T), Body_Vars, ID, Pos, Acc, [P1::ProbFact|T2]) :-
	create_mws_atom(Atom,Body_Vars,ID,Pos,ProbFact),
	(
	 (P=:=0; Acc=:=0)->
	 P1 is P
	;
	 P1 is  min(P/(1-Acc),1.0)
	),
	Acc2 is Acc+P,
	Pos2 is Pos+1,
	create_ad_aux_facts(T,Body_Vars,ID,Pos2,Acc2,T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_tunable_ad_aux_facts(t(_)::_,_,_,Pos,[]) :-
	Pos>1,
	problog_flag(ad_sumto1_learning,true),
	!.
create_tunable_ad_aux_facts(t(_)::Atom,Body_Vars,ID,Pos,[t(_)::ProbFact]) :-
	create_mws_atom(Atom,Body_Vars,ID,Pos,ProbFact).
create_tunable_ad_aux_facts((t(_)::Atom;T),Body_Vars,ID,Pos,[t(_)::ProbFact|T2]) :-
	create_mws_atom(Atom,Body_Vars,ID,Pos,ProbFact),
	Pos2 is Pos+1,
	create_tunable_ad_aux_facts(T,Body_Vars,ID,Pos2,T2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_aux_bodies(_::Atom, Body_Vars, Body, ID, Pos, Aux_Facts , _, [(Atom:-Body2)]) :-
        create_mws_atom(Atom,Body_Vars,ID,Pos,ProbFact),
	(
	 member(_::ProbFact,Aux_Facts)->
	 tuple_append(Body,ProbFact,Body2)
	;
	 Body2=Body
	).

create_aux_bodies((_::Atom; T), Body_Vars, Body, ID, Pos, Aux_Facts , Mode, [(Atom:-Body2)|T2]) :-
        create_mws_atom(Atom,Body_Vars,ID,Pos,ProbFact),
	tuple_append(Body,ProbFact,Body2),
	(
	 Mode==lfi_learning ->
	 tuple_append(Body,\+ProbFact,Body3)
	;
	 tuple_append(Body,problog_not(ProbFact),Body3)
	),

	Pos2 is Pos+1,
	create_aux_bodies(T,Body_Vars,Body3,ID,Pos2,Aux_Facts,Mode,T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tuple_append(true,X,X) :-
	!.
tuple_append(X,true,X) :-
	!.
tuple_append((A,B),X,(A,B2)) :-
	X \= true,
	!,
	tuple_append(B,X,B2).
tuple_append(X,Y,(X,Y)) :-
	X \= true,
	Y \= true,
	X \= (_,_).
