/*
	LPAD and CP-Logic reasoning suite
	Copyright (c) 2007, Fabrizio Riguzzi

Test file for lpad.pl

Use
:-t.
to execute the test

*/

:-use_module(library(lpad)).

epsilon(0.000001).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.


t:-
	format("~nTesting lpad.yap~n~n",[]),
	files(F),
	statistics(runtime,[_,_]),
	set(ground_body,false),
	format("~nNon ground body~n~n",[]),
	test_files(F,ground_body(false)),
	set(ground_body,true),
	format("~nGround body~n~n",[]),
	test_files(F,ground_body(true)),
	statistics(runtime,[_,T]),
	T1 is T /1000,
	format("Test successful, time ~f secs.~n",[T1]).

t:-
	format("Test unsuccessful.~n",[]).
	
test_files([],_GB).

test_files([H|T],GB):-
	format("~n~a.cpl~n",[H]),
	library_directory(LD),
	atom_concat(LD,'/cplint/examples/',ExDir),
	atom_concat(ExDir,H,NH),
	p(NH),!,
	findall(A,test(A,H,GB),L),
	test_all(H,L),
	test_files(T,GB).

test_all(_F,[]).

test_all(F,[H|T]):-
	copy_term(H,NH),
	NH=(_Query,close_to('P',_Prob)),
	format("~a ~p.~n",[F,NH]),
	call(H),!,
	test_all(F,T).


files([
exapprox,exrange,threesideddice,mendels,
coin2,ex,throws,trigger,win,hiv,light,
invalid,exist,exist1]).

test((s([a],P),close_to(P,0.18)),exist,ground_body(false)). 
test((s([a],P),close_to(P,0.19)),exist,ground_body(true)). 

test((s([a],P),close_to(P,0.276)),exist1,ground_body(false)). 
test((s([a],P),close_to(P,0.3115)),exist1,ground_body(true)). 

test((s([p],P),close_to(P,0.5)),invalid,_). 
test((s([q],P),close_to(P,0.5)),invalid,_). 
test((s([p,q],P),close_to(P,0)),invalid,_). 


test((s([throws(mary),throws(john),break],P),close_to(P,0.46)),throws,_). 
test((s([throws(mary),throws(john),\+break],P),close_to(P,0.04)),throws,_).
test((s([\+ throws(mary),throws(john),break],P),close_to(P,0.3)),throws,_). 
test((s([\+ throws(mary),throws(john),\+ break],P),close_to(P,0.2)),throws,_).

test((s([death],P),close_to(P,0.305555555555556)),trigger,_).

test((s([win(white)],P),close_to(P,0.5)),win,_).
test((s([win(black)],P),close_to(P,0.5)),win,_).
test((s([win(black),win(white)],P),close_to(P,0)),win,_).

test((s([hiv(a)],P),close_to(P,0.154)),hiv,_).
test((s([hiv(b)],P),close_to(P,0.154)),hiv,_).
test((s([hiv(b),hiv(a)],P),close_to(P,0.118)),hiv,_).
test((s([\+ hiv(b),\+ hiv(a)],P),close_to(P,0.81)),hiv,_).
test((s([ hiv(b),\+ hiv(a)],P),close_to(P,0.036)),hiv,_).
test((s([\+ hiv(b),hiv(a)],P),close_to(P,0.036)),hiv,_).

test((s([push,replace],P),close_to(P,0.5)),light,_).
test((s([push,light],P),close_to(P,0.5)),light,_).
test((s([push,light,replace],P),close_to(P,0)),light,_).
test((s([light,replace],P),close_to(P,0)),light,_).
test((s([light],P),close_to(P,0.5)),light,_).
test((s([replace],P),close_to(P,0.5)),light,_).


test((s([a],P),close_to(P,0.1719)),exapprox,ground_body(true)).
test((s([a],P),close_to(P,0.099)),exapprox,ground_body(false)).


test((s([a(1)],P),close_to(P,0.2775)),exrange,_).
test((s([a(2)],P),close_to(P,0.36)),exrange,_).

test((s([on(0,1)],P),close_to(P,0.333333333333333)),threesideddice,_).
test((s([on(1,1)],P),close_to(P,0.222222222222222)),threesideddice,_).
test((s([on(2,1)],P),close_to(P,0.148148147703704)),threesideddice,_).

test((sc([on(2,1)],[on(0,1)],P),close_to(P,0.222222222222222)),threesideddice,_).
test((sc([on(2,1)],[on(1,1)],P),close_to(P,0.333333333333333)),threesideddice,_).


test((s([cg(s,1,p)],P),close_to(P,0.75)),mendel,_).
test((s([cg(s,1,w)],P),close_to(P,0.25)),mendel,_).
test((s([cg(s,2,p)],P),close_to(P,0.25)),mendel,_).
test((s([cg(s,2,w)],P),close_to(P,0.75)),mendel,_).
test((s([cg(f,2,w)],P),close_to(P,0.5)),mendel,_).
test((s([cg(s,2,w)],P),close_to(P,0.75)),mendel,_).


test((s([heads(coin1)],P),close_to(P,0.51)),coin2,_).
test((s([heads(coin2)],P),close_to(P,0.51)),coin2,_).

test((s([tails(coin1)],P),close_to(P,0.49)),coin2,_).
test((s([tails(coin2)],P),close_to(P,0.49)),coin2,_).

test((s([a],P),close_to(P,0.226)),ex,_).

