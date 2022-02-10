
:- source.

:- use_module(library(lists),[member/2]).

:- yap_flag(unknown,error).

/*
:- consult([f0:'is_malignant_fs/f0.f',f0:'is_malignant_fs/f0.n',
	    f1:'is_malignant_fs/f1.f',f1:'is_malignant_fs/f1.n',
	    f2:'is_malignant_fs/f2.f',f2:'is_malignant_fs/f2.n',
	    f3:'is_malignant_fs/f3.f',f3:'is_malignant_fs/f3.n',
	    f4:'is_malignant_fs/f4.f',f4:'is_malignant_fs/f4.n',
	    f5:'is_malignant_fs/f5.f',f5:'is_malignant_fs/f5.n',
	    f6:'is_malignant_fs/f6.f',f6:'is_malignant_fs/f6.n',
	    f7:'is_malignant_fs/f7.f',f7:'is_malignant_fs/f7.n',
	    f8:'is_malignant_fs/f8.f',f8:'is_malignant_fs/f8.n',
	    f9:'is_malignant_fs/f9.f',f9:'is_malignant_fs/f9.n']).
*/

main :-
	do_all(0,5).

do_all(I,I) :- !.
do_all(I0,Max) :-
	I1 is (I0+1) mod 5,
	I2 is (I0+2) mod 5,
	I3 is (I0+3) mod 5,
	I4 is (I0+4) mod 5,
	atomic_concat([cls,I0,'.yap'],F),
	atomic_concat(['bmaps/F',I0,'_bmap.train'],TR1),
	atomic_concat(['bmaps/F',I0,'_keys.train'],TR2),
	atomic_concat(['bmaps/F',I0,'_bmap.tune'],TU1),
	atomic_concat(['bmaps/F',I0,'_keys.tune'],TU2),
	atomic_concat(['bmaps/F',I0,'_bmap.test'],TE1),
	atomic_concat(['bmaps/F',I0,'_keys.test'],TE2),
	atomic_concat(['f',I0],F0),
	atomic_concat(['f',I1],F1),
	atomic_concat(['f',I2],F2),
	atomic_concat(['f',I3],F3),
	atomic_concat(['f',I4],F4),
	do(F,TR1,TR2,TU1,TU2,'/dev/null','/dev/null',TE1,TE2,[F0,F1],[F2,F3],[],[F4]),
	I is I0+1,
	do_all(I,Max).


main2 :-
	member(File,[0,1,2,3,4]),
	gen_folder(File),
	fail.
main2.

do(F,TR1,TR2,TU11,TU12,TU21,TU22,TE1,TE2,FTR,FTU1,FTU2,FTE) :-
%	atom_concat('i2',F,TF),
	F = TF,
	ensure_loaded(TF),
	open(TR1, write, STR1),
	open(TR2, write, STR2),
	open(TU11, write, STU11),
	open(TU12, write, STU12),
	open(TU21, write, STU21),
	open(TU22, write, STU22),
	open(TE1, write, STE1),
	open(TE2, write, STE2),
	run_examples(STR1,STR2,STU11,STU12,STU21,STU22,STE1,STE2,FTR,FTU1,FTU2,FTE),
	close(STR1),
	close(STR2),
	close(STU11),
	close(STU12),
	close(STU21),
	close(STU22),
	close(STE1),
	close(STE2).

run_examples(STR1,STR2,STU11,STU12,STU21,STU22,STE1,STE2,FTR,FTU1,FTU2,FTE) :-
	target(GEx,G,_,Ex),
	nofclauses(G,NCls),
	(pos:GEx ; neg:GEx),
	select_stream(Ex,STR1,STR2,STU11,STU12,STU21,STU22,STE1,STE2,FTR,FTU1,FTU2,FTE,S1,S2),
	send_example(Ex,S1,S2,NCls),
	fail.
run_examples(_,_,_,_,_,_,_,_,_,_,_,_).

select_stream(Ex,STR1,STR2,_,_,_,_,_,_,FTR,_,_,_,STR1,STR2) :-
	example_in(Ex,FTR), !.
select_stream(Ex,_,_,STU11,STU12,_,_,_,_,_,FTU1,_,_,STU11,STU12) :-
	example_in(Ex,FTU1), !.
select_stream(Ex,_,_,_,_,STU21,STU22,_,_,_,_,FTU2,_,STU21,STU22) :-
	example_in(Ex,FTU2), !.
select_stream(_,_,_,_,_,_,_,STE1,STE2,_,_,_,_,STE1,STE2).

example_in(Ex,[F|_]) :-
	target(GEx,_,_,Ex),
	call(F:GEx).
example_in(Ex,[_|Fs]) :-
	example_in(Ex,Fs).


gen_folder(File) :-
	atomic_concat([cls,File,'.yap'],FileName),
	compile(FileName),
	target(_,G,_,_),
	nofclauses(G,NCls),	
	member(Folder,[f0,f1,f2,f3,f4,f5,f6,f7,f8,f9]),
	gen_scores(File,Folder,NCls).

gen_scores(File, Folder,NCls) :-
	atomic_concat(['bmaps/F',File,'_',Folder,'_bmap'],BMap),
	atomic_concat(['bmaps/F',File,'_',Folder,'_key'],Key),
	open(BMap,write,BS),
	open(Key,write,KS),
	run_examples(Folder,BS, KS, NCls),
	close(BS),
	close(KS).

run_examples(ModuleName, BMap, Key, NCls) :-
	target(GEx,_,_,Ex),
	ModuleName:GEx,
	send_example(Ex,BMap,Key,NCls),
	fail.
run_examples(_, _, _, _).

nofclauses(H,NCls) :-
	findall(H,clause(H,_),L),
	length(L,NCls).

send_example(Ex,S1,S2,NCls) :-
	target(GEx,_,_,Ex),
	(pos:GEx -> Class = 1 ; Class = 0),
	format(S2,'~w ~d~n',[Ex,Class]),
	do_atts(Ex,NCls,S1),
	format(S1,'~d~n',[Class]).

do_atts(Ex,NCls,S1) :-
	target(_,Goal,I,Ex),
	for(0,NCls,I),
	(call(Goal) -> Att = 1 ; Att = 0),
	format(S1,'~d,',[Att]),
	fail.
do_atts(_,_,_).

for(Min,Max,Min) :-
	Min < Max.
for(Min,Max,I) :-
	Min < Max,
	Min1 is Min+1,
	for(Min1,Max,I).

:- ensure_loaded(use_background).

