
%%============================================================================ 
%% CNF.pl
%% Convertor of Boolean formulae to CNF
%% Copyright (c) 2006, Michael Codish, Vitaly Lagoon, and Peter J. Stuckey
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the
%% "Software"), to deal in the Software without restriction, including
%% without limitation the rights to use, copy, modify, merge, publish,
%% distribute, sublicense, and/or sell copies of the Software, and to
%% permit persons to whom the Software is furnished to do so, subject to
%% the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included
%% in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
%% LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
%% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
%% WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

:- module(cnf,[cnf/2,cnf_dl/2]).

cnf(F,Cnf) :- cnf_dl(F,Cnf-[]).

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

