%============================================================================ 
%% @file CNF.pl
%% @brief Convertor of Boolean formulae to CNF
%%

%============================================================================ 
% Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
% 
% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the
% "Software"), to deal in the Software without restriction, including
% without limitation the rights to use, copy, modify, merge, publish,
% distribute, sublicense, and/or sell copies of the Software, and to
% permit persons to whom the Software is furnished to do so, subject to
% the following conditions:
% 
% The above copyright notice and this permission notice shall be included
% in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

/**
  @defgroup CnfConverter
  @ingroup SAT
  @ brief convert a generic propositional formula to CNF (conjunction of disjunctions of literals).
  @{

Propositional formulae are of the form:
-  `V`:  a proposition may be represented as a variable or as a Prolog atom
- `0, 1`: false and true
-  `A*B` -> the conjunction of two formulae
-  `A+B` -> the disjunction of  two formulae
-  `-A` -> the negation of a formula
-  `A xor B`: the exclusive-or of two formulae
-  `A == B`: the logical equivalence of two formulae
-  `A -> B`: the logical implication of two formulae

The output of the converter is a list of lists. The external list
represents a conjunction. The internal lists represent a disjunction,
or clause. Clauses disjoin propositions, or positive literals, and
negated propositions, or negative literals.

The converter adds an extra variable F to represent the truth value of the formula.

*/

:- module(cnf,[cnf/2,cnf_dl/2]).

/**
 * @pred cnf(F,Cnf)
 *
 * Convert formula `F` to a list `CNF`.
 */
cnf(F,Cnf) :- cnf_dl(F,Cnf-[]).

/**
 * @pred cnf(F,Cnf)
 *
 * Convert formula `F` to a difference list `CNF`.
 */
cnf_dl(F,[[B]|Cnf1]-Cnf2) :- iff(F,+,B,Cnf2,Cnf1).


iff(V,_,B,Acc,Cnf) :- var(V), !, V=B, Cnf=Acc.
iff(1,_,B,Acc,Cnf) :- !, B=1, Cnf=Acc.
iff(0,_,B,Acc,Cnf) :- !, B=0, Cnf=Acc.

iff(-X,+,B,Acc,Cnf) :- !, iff(X,-,BX,Acc,Cnf), neglit(BX,B).
iff(-X,-,B,Acc,Cnf) :- !, iff(X,+,BX,Acc,Cnf), neglit(BX,B).
iff(-X,*,B,Acc,Cnf) :- !, iff(X,*,BX,Acc,Cnf), neglit(BX,B).

iff((X+Y),Polarity,B,Acc,Cnf) :- !,
	iff(X,Polarity,BX,Acc,AccX),
	iff(Y,Polarity,BY,AccX,AccXY),
	(
	    Polarity == + -> Cnf = [[-B,BX,BY]|AccXY]
	;
	    Polarity == - -> Cnf = [[B,-BX],[B,-BY]|AccXY]
	;
	    Cnf = [[-B,BX,BY], [B,-BX], [B,-BY] | AccXY]
	).

iff((X*Y),Polarity,B,Acc,Cnf) :- !,
	iff(X,Polarity,BX,Acc,AccX),
	iff(Y,Polarity,BY,AccX,AccXY),
	(
	    Polarity == + -> Cnf = [[-B,BX],[-B,BY]|AccXY]
	;   
	    Polarity == - -> Cnf = [[B,-BX,-BY]|AccXY]
	;
	    Cnf = [[B,-BX,-BY], [-B,BX], [-B,BY] | AccXY]
	).

iff((X==Y),Polarity,B,Acc,Cnf) :- !,
	iff(X,*,BX,Acc,AccX),
	iff(Y,*,BY,AccX,AccXY),
	(
	    Polarity == + -> Cnf = [[-BX,BY,-B],[BX,-BY,-B] | AccXY]
	;   
	    Polarity == - -> Cnf = [[-BX,-BY,B],[BX,BY,B] | AccXY]
	;
	    Cnf =  [[-BX,BY,-B],[BX,-BY,-B],[-BX,-BY,B],[BX,BY,B] | AccXY]
	).

iff((X xor Y),Polarity,B,Acc,Cnf) :- !,
	iff(X,*,BX,Acc,AccX),
	iff(Y,*,BY,AccX,AccXY),
	(
	    Polarity == + -> Cnf = [[-BX,-BY,-B],[BX,BY,-B] | AccXY]
	;
	    Polarity == - -> Cnf = [[-BX,BY,B],[BX,-BY,B] | AccXY]
	;
	    Cnf = [[-BX,BY,B],[BX,-BY,B],[-BX,-BY,-B],[BX,BY,-B] | AccXY]
	).

iff((X->Y;Z),Polarity,B,Acc,Cnf) :- !,
	iff(X,*,BX,Acc,AccX),
	iff(Y,Polarity,BY,AccX,AccXY),
	iff(Z,Polarity,BZ,AccXY,AccXYZ),
	(   
	    Polarity == + -> Cnf = [[-BX,BY,-B],[BX,BZ,-B],[BY,BZ,-B]|AccXYZ]
	;   
	    Polarity == - -> Cnf = [[-BX,-BY,B],[BX,-BZ,B],[-BY,-BZ,B]|AccXYZ]
	;
	    Cnf = [[-BX,BY,-B], [BX,BZ,-B], [BY,BZ,-B],
	           [-BX,-BY,B], [BX,-BZ,B], [-BY,-BZ,B] | AccXYZ]
	).

neglit(V,-V) :- var(V), !.
neglit(-V,V).
neglit(0,1).
neglit(1,0).

%%}

