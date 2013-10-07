%
% generative learning in MLNs:
%
% maximise SUM log Pw(Xi=xi|mb(Xi))
%
% or sum N(X) - p(0|mb)n0 - P(1|mb)n1
%
% note that Pw(X|MB) = P(0|MB) || P(1|MB)
%
%

:- module(learn_mlns_generative,
	[learn_mln_generative/0,
	 portray_mln/0]).

:- use_module(library(lists)).

:- use_module(library(tries)).

:- use_module(library(maplist)).

:- use_module(library(nb)).

:- use_module(library(matrix)).

:- reexport(library(mlns)).

:- reexport(library(pfl)).

:- use_module(library(lbfgs)).

:-  yap_flag(tabling_mode,local).

:- dynamic diff/4, lit/1, i/2.

prior_means(_, 0.0).
prior_dev(_, 1.0).

learn_mln_generative :-
	compile,
	optimize.	

set_weights :-
	retract( mln:mln_w(Id, _) ),
	optimizer_get_x( Id, W),
	%writeln(weight:W),
	assert( mln:mln_w(Id, W) ),
	fail.
set_weights.

adjust_lprior(Lik0, Lik) :-
	Lik0 = Lik, !.
adjust_lprior(Lik0, Lik) :-
	findall(I-WI, mln_w(I,WI), WIs),
	foldl(add_lprior, WIs, Lik0, Lik).
	
add_lprior(Id-WI, Lik0, Lik) :-
	prior_means(Id, PM),
	prior_dev(Id, PV),
	Lik is Lik0 + ((WI-PM)*(WI-PM))/(2*PV*PV).


likelihood(Lik) :-
	S = s(0.0),
%	nb_create_accumulator(0.0, Acc),
	(
	    recorded(i, [Ref|N], _),
	    peval(Ref, LogP),
	    %writeln(N*P),
	    S = s(V),
	    V1 is V+N*LogP,
	    nb_setarg(1, S, V1),
%	    nb_add_to_accumulator( Acc, LogP),
	    fail
        ;
%	    nb_accumulator_value(Acc, Lik)
	    S = s(Lik0),
%writeln(lik:Lik0),
	    adjust_lprior(Lik0, Lik1),
	    Lik is -Lik1
        ).

derive :-
	nb_getval(i2, Mat),
	nb_getval(d2, MatD),
	matrix_set_all(MatD, 0.0),
	recorded(i, [Ref|NI], _),
	trie_get_entry(Ref, e(_, Ds, Ps)),
	member(n(Id,Occs,DN0,DN1), Ds),
	matrix_get(Mat, [Id], N),
	matrix_get(MatD, [Id], V),
	peval(Ps, P0, P1),
	X is Occs*(N-P0*(N+DN0)-P1*(N+DN1)),
%writeln(X is NI*(-P0*(DN0)-P1*(DN1))),
	V1 is V-NI*X,
	matrix_set(MatD, [Id], V1),
	fail.
derive :-
	nb_getval(d2, MatD),
	mln(Ms),
	N1 is Ms-1,
	between(0, N1, Id),
	matrix_get(MatD, [Id], Sum),
	%writeln(d:Id:Sum),
	adjust_prior(Sum, Id, NSum),
	optimizer_set_g(Id, NSum  ),
	fail.
derive.

adjust_prior(Lik0, _, Lik) :-
	Lik0 = Lik, !.
adjust_prior(Sum, Id, NSum) :-
	mln_w(Id, Wi),
	prior_means(Id, PM),
	prior_dev(Id, PV),
	NSum is Sum+(Wi-PM)/(PV*PV).

:- dynamic old_fx/1.

old_fx(+inf).

% This is the call back function which is invoked to report the progress
% if the last argument is set to anything else than 0, the optimizer will
% stop right now
user:progress(FX,X_Norm,G_Norm,Step,_N,Iteration,Ls, Out) :-
	( Iteration mod 100 =:= 0 -> atomic_concat([tmp_,Iteration,'.pfl'], File), open( File, write, S), portray_mln(S), close(S) ; true ),
	retract(old_fx(FX0)),
	( Delta is FX-FX0, abs(Delta/FX) < 0.00001 -> Out = 1 ; Out = 0),
	optimizer_get_x(0,X0),
	assert(old_fx(FX)),
	format('/* ~d: w[0]=~10f  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f */~n',[Iteration,X0,FX,X_Norm,G_Norm,Step,Ls]).



 % This is the call back function which evaluates F and the gradient of F
user:evaluate(FX,_N,_Step) :-
	 set_weights,
	 likelihood(FX),
	 derive.

init_vars(Ev, Pr) :-
	 mln(N),
	 N1 is N-1,
	 format('/* We start the search for ~d weights at weight[_]=0 */~2n',[N]),
	 optimizer_initialize(N, Ev, Pr),
	 between(0, N1, I),
	 optimizer_set_x(I,0.0),
	 fail.
init_vars(_, _).

output_stat(BestF, Status) :-
	 ( portray_mln,
	   fail
	 ;
	   Lik is -BestF,
	   format('/* Final likelihood=~f */~n/* LBFGS Status=~w */~n',[Lik,Status])
	 ).

optimize :-
	 init_vars(evaluate, progress),
	 optimizer_run(BestF,Status),
	 output_stat(BestF, Status),
	 optimizer_finalize,
	 format('~2nOptimization done~n',[]).	

compile :-
	 init_compiler,
	 compile_literals,
	 fail.
/*
compile :-
	recorded(i, [Ref|N], _),
	trie_get_entry(Ref, E),
	writeln(N:E),
	fail.
*/
compile.

init_compiler :-
	mln(HowMany),
	D is HowMany+1,
	matrix_new(ints, [D], M),
	matrix_new(floats, [D], MD),
	nb_setval(i2,M),
	nb_setval(d2,MD),
	collect_literals,
	init_trie,
	retractall(p_l(_,_,_,_)),
	retractall(lmln:p(_,_,_,_)),
	fail.
init_compiler.

init_trie :-
	catch(nb_getval( mln_trie, Trie ), _, fail),
	trie_close( Trie ),
	eraseall( i ),
	fail.
init_trie :-
	trie_open( Trie ),
	nb_setval( mln_trie , Trie ).

collect_literals :-
	mln(ParFactor, _Type, _Els, _G, _DConstraints),
	factor(markov, ParFactor, Ks, _, _Phi, _Constraints),
	maplist(add_lit, Ks),
	fail.
collect_literals.

add_lit(K) :-
	functor(K, N, A),
	functor(K0, N, A),
	( lit(K0) -> true ; assert(lit(K0)) ).

compile_literals :-
	lit(K),
	functor(K, N, A),
	statistics(runtime,_),
	format(user_error, '/** grounding ~a/~d.~45+**/~n',[N,A]),
       ( evidence(K, 1), % only look at literals with evidence...
%	( ground_lit(K),
	%writeln(k:K),
	  compile_pw(K)
        ;
	  statistics(runtime,[_,T]),
	  format(user_error, '/**          took ~d msec.~45+**/~n',[T]),
          fail
        ).
	   

ground_lit(K) :- 
	functor(K, _, Ar),
	ground_lit(0, Ar, K).

ground_lit(Ar, Ar, _K).
ground_lit(I0, Ar, K) :-
	I is I0+1,
	(mln):mln_domain(I, K, G, _A),
	user:G,
	ground_lit(I, Ar, K).

compile_pw(VId) :-
	(evidence(VId, 1) -> P = 1 ; P = 0),
	compile(VId, P).

compile(VId, Val) :-
	findall(p(FId,W,P0,P1,I0,I1), find_prob(VId, Val, FId, W, P0, P1, I0, I1), Fs),
	(
	  Fs == [] -> fail
        ;
	   Fs = [p(FId,W,1,1,I0,I1)]
           ->
	   fail
       ;   
	  sort(Fs, FsS),
	  merge_lits(FsS, FsN, Ws),
	  nb_getval( mln_trie, Trie ),
	  store( Trie, e(Val, Ws, FsN) )
       ).

store( T , E ) :-
	trie_check_entry(T, E, R), !,
	recorded(i, [R|I], Ref),
	erase(Ref),
	I1 is I+1,
	recorda(i, [R|I1], _).
store( T , E ) :-
	trie_put_entry(T, E, R), !,
	recorda(i, [R|1], _).

merge_lits([], [], []).
merge_lits([N*p(F,W,A1,A2,I1,I2), p(F,W,A3,A4,I3,I4)|FsS], FsM, Is) :-
	A1 == A3,
	A2 == A4,
	I1 == I3,
	I2 == I4, !,
	N1 is N+1,
	merge_lits([N1*p(F,W,A3,A4,I3,I4)|FsS], FsM, Is).
merge_lits([p(F,W,A1,A2,I1,I2), p(F,W,A3,A4,I3,I4)|FsS], FsM, Is) :-
	A1 == A3,
	A2 == A4,
	I1 == I3,
	I2 == I4, !,
	merge_lits([2*p(F,W,A3,A4,I3,I4)|FsS], FsM, Is).
merge_lits([p(F,W,A1,A2,I1,I2) | FsS], [p(F,1,W,A1,A2)|FsM], [n(F,1,I1,I2)|Is]) :-
	merge_lits(FsS, FsM, Is).
merge_lits([N*p(F,W,A1,A2,I1,I2) | FsS], [p(F,N,W,A1,A2)|FsM], [n(F,N,I1,I2)|Is]) :-
	merge_lits(FsS, FsM, Is).

find_prob(VId, E, ParFactor, W, P0, P1, I0, I1) :-
	mln(ParFactor, _, _Type, _, Constraints),
%	maplist(call,Constraints),
	deletei(Constraints, VId, ConstraintsF, Pol),
	maplist(expand_domain(VId-Pol), ConstraintsF),
	% all other literals are false
        ( Pol == (+) -> 
	      P0 = 0, P1 = W,
	      (E == 1 -> /* we are making this true */ 
	         inc(ParFactor),
	         I0 = -1, I1 = 0
	      ;
                 /* it is false */
	         I0 = 0, I1 = 1
	      )
	;
	      P0 = W, P1 = 0,
	      (E == 1 -> /* we are making this false */ 
	         I0 = 1, I1 = 0
	      ;
                 /* it is true */
	         inc(ParFactor),
	         I0 = 0, I1 = -1
	      )
        ).

expand_domain(VIdPol, true - Lits) :- !,
	maplist( false_literal(VIdPol), Lits).

expand_domain(VIdPol, Dom-Lits) :-
	call(user:Dom),
	maplist( true_literal(VIdPol), Lits).

% we  need to check if we have 
% L ; L or L ; -L
% in this case skip or it is always true, so fail.
false_literal(L-(-), L).
false_literal(VId-_, L) :-
	evidence(L, 1),
	L \= VId.

% L is ground
true_literal(L-(+), L) :- !.
true_literal(VId-_, L) :-
	L \= VId,
	\+ evidence(L, 1).

deletei([true-Lits|More], K, [true-NLits|More], -) :-
	force_delete(Lits, K, NLits).
deletei([true-Lits|More], K, [true-Lits|NMore], -) :- !,
	force_delete(More, K, NMore).
deletei(More, K, NMore, +) :-
	deletei(More, K, NMore).

deletei([Dom-Lits|More], K, [Dom-NLits|More]) :-
	force_delete(Lits, K, NLits).
deletei([DomLits|More], K, [DomLits|NMore]) :-
	deletei(More, K, NMore).

force_delete([Elem|List], Elem, List).
force_delete([Head|List], Elem, [Head|Residue]) :-
	force_delete(List, Elem, Residue).

inc(Id) :-
	nb_getval(i2, M),
	matrix_inc(M, [Id]).

peval(Ref, P) :-
	trie_get_entry(Ref, e(Side, _, Ps)),
	foldl2(p_eval, Ps, 0.0, P0, 0.0, P1),
	logsum(P0, P1, P01),
	( Side == 0 -> P = P0-P01 ; P = P1-P01 ).

peval(Ps, P0, P1) :-
%writeln(p:Ds:Ps),
	foldl2(p_eval, Ps, 0.0, AP0, 0.0, AP1),
	logsum(AP0, AP1, AP01),
	P0 is exp( AP0 - AP01 ),
	P1 is 1-P0.

p_eval(p(WId, N, W, P0, P1), AP0, A0, AP1, A1) :-
	mln_w(WId, W),
	A0 is AP0+N*P0,
	A1 is AP1+N*P1.
