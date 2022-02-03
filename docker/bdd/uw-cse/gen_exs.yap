
do_fold(TestF,TestN,Mod,OMods) :-
	do_pos(Mod,TestF),
	do_neg(Mod,TestN).

do_poss(OMods,Out) :-
	open(Out, write, S),
	poss(OMods,S),
	close(S).

do_pos(OMods,Out) :-
	open(Out, write, S),
	pos(OMods,S),
	close(S).

poss([],S).
poss([Mod|OMods],S) :-
	pos(Mod,S),
	poss(OMods,S).


pos(Mod,St) :-
	Mod:advisedby(P,S),
	format(St,'advisedby(~a,~a).~n',[P,S]),
	fail.
pos(_,_).


do_neg(Mod,Out) :-
	open(Out, write, S),
	run(Mod,S),
	close(S).

do_negs(Mods,Out) :-
	open(Out, write, S),
	runs(Mods,S),
	close(S).

runs([],_).
runs([Mod|Mods],S) :-
	run(Mod,S),
	runs(Mods,S).


run(Mod,St) :-
	Mod:professor(P),
	Mod:student(S),
	\+ Mod:advisedby(S,P),
	format(St,'advisedby(~a,~a).~n',[S,P]),
	fail.
run(_,_).


:- ensure_loaded(ai:ai).
:- ensure_loaded(graphics:graphics).
:- ensure_loaded(language:language).
:- ensure_loaded(misc:misc).
:- ensure_loaded(systems:systems).
:- ensure_loaded(theory:theory).
:- ensure_loaded(unknown:unknown).

main :-
	do_fold('fs/f0.f','fs/f0.n',ai,[graphics,language,misc,systems,theory,unknown]),
	do_fold('fs/f1.f','fs/f1.n',graphics,[ai,language,misc,systems,theory,unknown]),
	do_fold('fs/f2.f','fs/f2.n',language,[ai,graphics,misc,systems,theory,unknown]),
	do_fold('fs/f3.f','fs/f3.n',systems,[ai,graphics,language,misc,theory,unknown]),
	do_fold('fs/f4.f','fs/f4.n',theory,[ai,graphics,language,misc,systems,unknown]).

:- main.







