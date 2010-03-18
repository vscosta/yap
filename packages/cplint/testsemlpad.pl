/*
	LPAD and CP-Logic reasoning suite
	Copyright (c) 2007, Fabrizio Riguzzi

	list of tests for semlpad.pl

*/
:-use_module(library(semlpad)).

epsilon(0.000001).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow<V,
	V<THigh.


t:-
	format("~nTesting semlpad.yap~n~n",[]),
	files_modes(FM),
	files_variables(FV),
	statistics(runtime,[_,_]),
	format("~nGrounding using modes~n~n",[]),
	set(ground_body,false),
	format("~nNon ground body~n~n",[]),
	test_files(FM,ground_body(false)),
	set(ground_body,true),
	format("~nGround body~n~n",[]),
	test_files(FM,ground_body(true)),
	format("~nGrounding using variables~n~n",[]),
	set(grounding,variables),
	set(ground_body,false),
	format("~nNon ground body~n~n",[]),
	test_files(FV,ground_body(false)),
	set(ground_body,true),
	format("~nGround body~n~n",[]),
	test_files(FV,ground_body(true)),
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

files_modes([
exapprox, 
exrange,  
%threesideddice, 
%mendels, %ok only with grounding=variables
coin2, 
ex,exist,exist1 
]).

files_variables([
exapprox, 
exrange,  
%threesideddice, removed because of a strange bug in yap, in debug it does not fail
mendels, 
coin2, 
ex
]).


test((s([a],P),close_to(P,0.18)),exist,ground_body(false)). 
test((s([a],P),close_to(P,0.19)),exist,ground_body(true)). 

test((s([a],P),close_to(P,0.276)),exist1,ground_body(false)). 
test((s([a],P),close_to(P,0.3115)),exist1,ground_body(true)). 

test((s([a],P),close_to(P,0.1719)),exapprox,ground_body(true)).
test((s([a],P),close_to(P,0.099)),exapprox,ground_body(false)).

test((s([a(1)],P),close_to(P,0.2775)),exrange,_).
test((s([a(2)],P),close_to(P,0.36)),exrange,_).

test((s([on(0,1)],P),close_to(P,0.333333333333333)),threesideddice,_).
test((s([on(1,1)],P),close_to(P,0.222222222222222)),threesideddice,_).
test((s([on(2,1)],P),close_to(P,0.148148147703704)),threesideddice,_).

test((sc([on(2,1)],[on(0,1)],P),close_to(P,0.222222222222222)),threesideddice,_).
test((sc([on(2,1)],[on(1,1)],P),close_to(P,0.333333333333333)),threesideddice,_).


test((s([cg(s,1,p)],P),close_to(P,0.5)),mendels,ground_body(false)).
test((s([cg(s,1,w)],P),close_to(P,0.5)),mendels,ground_body(false)).
test((s([cg(s,2,p)],P),close_to(P,1.0)),mendels,ground_body(false)).
test((s([cg(s,2,w)],P),close_to(P,0)),mendels,ground_body(false)).


test((s([heads(coin1)],P),close_to(P,0.51)),coin2,_).
test((s([heads(coin2)],P),close_to(P,0.51)),coin2,_).

test((s([tails(coin1)],P),close_to(P,0.49)),coin2,_).
test((s([tails(coin2)],P),close_to(P,0.49)),coin2,_).

test((s([a],P),close_to(P,0.226)),ex,_).

