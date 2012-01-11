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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Discrete probability distributions for ProbLog
%
% this file contains predicates to emulate discrete distributions in ProbLog 
% 
% uniform(I,N,ID)
%    emulates a uniform discrete distribution
%      P(I) = 1/N for I in {1,2,...,N}
%    If I is a variable, the predicate backtracks over all
%    possible values for I
%    ID has to be ground, it is an identifier which - if in the same proof -
%    reused, will always return the same value
%
% binomial(K,N,P,ID)
%    emulates a binomial distribution 
%      P(K) = (N over K) x P^K x (1-P)^(N-K) for K in {0,1,...,N}
%    If K is a variable, the predicate backtracks over all
%    possible values for K
%    ID has to be ground, it is an identifier which - if in the same proof -
%    reused, will always return the same value
%
% poisson(K,Lambda,ID)
%    emulates a Poisson distribution
%      P(K) = Lamda^K / K! x exp(-Lambda) for K in {0,1,2, ....}
%    If K is a variable, the predicate backtracks over all
%    possible values for K
%    ID has to be ground, it is an identifier which - if in the same proof -
%    reused, will always return the same value
%
%
% Author  : Bernd Gutmann, bernd.gutmann@cs.kuleuven.be
% Version : January 14, 2009
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(discrete, [uniform/3,binomial/4,poisson/3]).
:- use_module('../problog').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A distribution over 1,2, ..., N
% where P(I) := 1/N
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Prob::p_uniform(_I,_N,_ID,Prob).
uniform(I,N,ID) :-
	integer(N),
	N>0,
	( var(I) ; integer(I), I>0, I=<N),
	uniform(1,I,true,N,ID).
uniform(I,I,Old,N,ID) :-
	I=<N,
	FactProb is 1/(N-I+1),
	call(Old),
	p_uniform(I,N,ID,FactProb).
uniform(I,I2,Old,N,ID) :-
	I<N,
	FactProb is 1/(N-I+1),
	NextI is I+1,
	uniform(NextI,I2,(problog_not(p_uniform(I,N,ID,FactProb)),Old),N,ID).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Binomial Distribution
% K in { 0,1,2,3, ... }
% Lambda >= 0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Prob::p_binomial(_K,_N,_P,_ID,Prob).
binomial(K,N,P,ID) :-
	number(P),
	P >= 0,
	P =< 1,
	integer(N),
	N>=0,
	( var(K) ; integer(K),K>=0,K=<N),
	binomial(0,K,N,P,true,0.0,ID).
binomial(K,KResult,N,P,Old,ProbAcc,ID) :-
	% KResult is a number, make sure, not to go over it
	% safes some time
	(
	    number(KResult)
	->
	    K=<KResult;
	    true
	),

	binomial_coefficient(N,K,BinomCoeff),
	
	Prob is BinomCoeff * (P ** K) * ((1-P) ** (N-K)),
	FactProb is Prob / (1-ProbAcc),

	% this check stops the derivation, if the floating-point-based
	% rounding errors get too big
	FactProb > 0.0,
	FactProb =< 1.0,

	(
	    (
		call(Old),
		p_binomial(K,N,P,ID,FactProb),
		KResult=K
	    ); (
	       K<N,
	       NextK is K+1,
	       NextProbAcc is ProbAcc+Prob,
	       binomial(NextK,KResult,N,P,(problog_not(p_binomial(K,N,P,ID,FactProb)),Old),NextProbAcc,ID)
	   )
       ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Poisson Distribution 
% K in { 0,1,2,3, ... } or var(K)
% Lambda >= 0
% ID has to be ground
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
P :: p_poisson(_K,_Lambda,_ID,P).

poisson(K,Lambda,ID) :-
	(  var(K); integer(K),K>=0 ),
	number(Lambda),
	Lambda>=0,
	ground(ID),
	poisson(0,K,true,Lambda,0.0,ID).

poisson(K,K2,Old,Lambda,ProbAcc,ID) :-
	% KResult is a number, make sure, not to go over it
	% safes some time
	(
	    integer(K2)
	->
	    K=<K2;
	    true
	),


	power_over_factorial(K,Lambda,Part1),
	
	% Prob is P(K) for a Poisson distribution with Lambda
	Prob is Part1 * exp(-Lambda), 

	% now we have to determine the fact probability
	% conditioned on the aggregated probabilities so far
	FactProb is Prob/(1-ProbAcc),

	% this check stops the derivation, if the floating-point-based
	% rounding errors get too big
	FactProb > 0.0,
	FactProb =< 1.0,

	(
	    (
		call(Old),
		p_poisson(K,Lambda,ID,FactProb),
		K2=K
	    ); (
	       NextK is K+1,
	       NextProbAcc is ProbAcc+Prob,
	       poisson(NextK,K2,(problog_not(p_poisson(K,Lambda,ID,FactProb)),Old),Lambda,NextProbAcc,ID)
	   )
       ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





% calculates (Lambda ** N) / N!

power_over_factorial(N,Lambda,Result) :-
	integer(N),
	N>=0,
	power_over_factorial(N,Lambda,1.0,Result).
power_over_factorial(N,Lambda,Old,Result) :-
	(
	    N>0
	->
            (
		N2 is N-1,
		New is Old * Lambda/N,
		power_over_factorial(N2,Lambda,New,Result)
	    ); Result=Old
	).



% calculates (N \over K) = N!/(K! * (N-K)!)

binomial_coefficient(N,K,Result) :-
	integer(K),
	K >= 0,
	binomial_coefficient(K,N,1,Result).
binomial_coefficient(I,N,Product,Result) :-
	(
	    I=0
	->
	    Result=Product;
	    (
		I2 is I-1,
		Product2 is Product * (N+1-I)/I,
		binomial_coefficient(I2,N,Product2,Result)
	    )
	).