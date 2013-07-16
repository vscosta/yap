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

:- use_module(library(lists)).

:- use_module(library(maplist)).

:- use_module(library(nb)).

:- use_module(library(mlns)).

:- use_module(library(pfl)).

:- use_module(library(lbfgs)).

:- dynamic diff/4, i/2.

prior_means(_, 0.0).
prior_dev(_, 100.0).

learn_mln_generative :-
	compile,
	optimize.	

set_weights :-
	retract( mln:mln_w(Id, _) ),
	optimizer_get_x( Id, W),
%	writeln(weight:W),
	assert( mln:mln_w(Id, W) ),
	fail.
set_weights.

add_lprior(Id-WI, Lik0, Lik) :-
	prior_means(Id, PM),
	prior_dev(Id, PV),
	Lik is Lik0 + ((WI-PM)*(WI-PM))/(2*PV*PV).

adjust_lprior(Lik0, Lik) :-
	Lik0 = Lik, !.
adjust_lprior(Lik0, Lik) :-
	findall(I-WI, mln_w(I,WI), WIs),
	foldl(add_lprior, WIs, Lik0, Lik).
	

likelihood(Lik) :-
	S = s(0.0),
%	nb_create_accumulator(0.0, Acc),
	(
	    lmln:p(_Lit,P,_,_),
	    LogP is log(P),
%	    writeln(_Lit:P),
	    S = s(V),
	    V1 is V+LogP,
	    nb_setarg(1, S, V1),
%	    nb_add_to_accumulator( Acc, LogP),
	    fail
        ;
%	    nb_accumulator_value(Acc, Lik)
	    S = s(Lik0),
writeln(lik:Lik0),
	    adjust_lprior(Lik0, Lik1),
	    Lik is -Lik1
        ).

adjust_prior(Lik0, _, Lik) :-
	Lik0 = Lik, !.
adjust_prior(Sum, Id, NSum) :-
	mln_w(Id, Wi),
	prior_means(Id, PM),
	prior_dev(Id, PV),
	NSum is Sum+(Wi-PM)/(PV*PV).

derive :-
	mln(Id, _, Els, _),
	i(Id, N),
%writeln(Id:N),
%	nb_create_accumulator(0.0, Acc),
	S = s(0.0),
%	nb_accumulator_value(Acc, Sum0),writeln(sum0:Sum0),
	(
	    nth(_L, Els, VId),
	    p_l(Id, VId,  P0, P1),
	    diff( VId, Id, DN0, DN1),
	    X is (N-P0*(N+DN0)-P1*(N+DN1)),
%writeln(X is (N-P0*(N+DN0)-P1*(N+DN1))),
%	    nb_add_to_accumulator(Acc, X),
	    S = s(V),
	    V1 is V-X,
	    nb_setarg(1, S, V1),
	    fail
        ;
%	    nb_accumulator_value(Acc, Sum),
	    S = s(Sum),
	    writeln(d:Id:Sum),
	    adjust_prior(Sum, Id, NSum),
	    optimizer_set_g(Id, NSum  ),
	    fail
        ).
derive.

% This is the call back function which is invoked to report the progress
% if the last argument is set to anywhting else than 0, the optimizer will
% stop right now
progress(FX,X_Norm,G_Norm,Step,_N,Iteration,Ls,0) :-
	optimizer_get_x(0,X0),
	format('~d. Iteration : w=~4f  f(X)=~4f  |X|=~4f  |X\'|=~4f  Step=~4f  Ls=~4f~n',[Iteration,X0,FX,X_Norm,G_Norm,Step,Ls]).


% This is the call back function which evaluates F and the gradient of F
evaluate(FX,_N,_Step) :-
	set_weights,
	likelihood(FX),
	derive.

init_vars(Ev, Pr) :-
	mln(N),
	N1 is N-1,
	format('We start the search at weight=0~2n',[]),
	optimizer_initialize(N, Ev, Pr),
	between(0, N1, I),
	optimizer_set_x(I,0.0),
	fail.
init_vars(_, _).

output_stat(BestF, Status) :-
	mln(N),
	N1 is N-1,
	( between(0,N1,I),
	  optimizer_get_x(I,BestX0),
	  format('w[~d] = ~f~n', [I, BestX0]),
	  fail
        ;
	  Lik is -BestF,
	  format('Final likelihood=~f~2nLBFGS Status=~w~n',[Lik,Status])
        ).

optimize :-
	init_vars(evaluate, progress),
	optimizer_run(BestF,Status),
	output_stat(BestF, Status),
	optimizer_finalize,
	format('~2nOptimization done~n',[]).	

compile :-
	init_compiler,
	mln(ParFactor, _Type, _Els, _G),
	writeln(ParFactor),
	factor(markov, ParFactor, Ks, _, _Phi, Constraints),
	maplist(call, Constraints),
	nth(_L, Ks, VId),
	compile_pw(VId, P0, P1, G),
	assert((p_l(ParFactor, VId, P0, P1) :- lmln:G)),
	fail.
compile.

init_compiler :-
	retractall(i(_,_)),
	retractall(i(_,_,_,_)),
	retractall(p_l(_,_,_,_)),
	retractall(lmln:p(_,_,_,_)),
	fail.
init_compiler.

compile_pw(VId, P0, P1, p(VId, _, P0, P1)) :-
	clause(lmln:p(VId, _, P0, P1), _), !.
compile_pw(VId, Head0, Head1, G) :-
	G = p(VId, Head, Head0, Head1),
	compile(VId,Head,Head0,Head1,Bd),
	assert( ( lmln:G :- Bd) ).

compile(VId, P, P0, P1, Bd) :-
	findall(p(FId,W,P0,P1), find_prob(VId, FId, W, P0, P1), Fs),
	(evidence(VId, 1) -> P = P1 ; P = P0),
	(
	  Fs == [] -> fail
        ;
	   Fs = [p(F,W,1,1)]
           ->
	   P0 = 0.5, P1 = 0.5, Bd = true
       ;   Fs = [p(F,W,P0A,P1A)]
           ->
	   Bd = (mln_w(F, W), P0 is P0A/(P0A+P1A), P1 is 1-P0)
       ;
	  Fs = [p(FA,WA,P0A,P1A),p(FB,WB,P0B,P1B)]
           ->
	   Bd = (mln_w(FA, WA), mln_w(FB, WB), P0 is P0A*P0B/(P0A*P0B+P1A*P1B), P1 is 1-P0)
       ;
	 Bd = ( sumps(Fs, V0, 1.0, V1, 1.0), P0 is V1/(V0+V1), P1 is 1-P0)
       ).

find_prob(VId, ParFactor, W, P0, P1) :-
	defined_in_factor(VId, ParFactor, L),
	factor(markov, ParFactor, Ks, _, _Phi, Constraints),
	nth0(L, Ks, VId),
	maplist(call,Constraints),
	mln(ParFactor, _Type, LP, _Head),
	foldl(true_literal, LP, Ks, 0, NTs),
	NTs < 2, % >= 2 ignore, always true...
	maplist(polarity(VId, Pol), LP),
        (NTs == 0 ->
	/* We have no true literal */
          ( Pol == (+) -> 
%	      inc_n(ParFactor, L1, 1),
	      (evidence(VId, 1) -> /* we are */ 
		  assert(diff(VId,ParFactor,-1, 0))
	      ;
	          assert(diff(VId, ParFactor, 0, 1))
	      ),   
	      P0 = 1, P1 = exp(W)
	  ;
%	      inc_n(ParFactor, L1, 0),
	      (evidence(VId, 1) -> /* we are */ 
		  assert(diff(VId,ParFactor, 1, 0))
	      ;
	          assert(diff(VId, ParFactor, 0, -1))
	      ),   
	      P0 = exp(W), P1 = 1
	  )
	;
	  /* L == 0: increment true literals once */
	  (L == 0 -> inc(ParFactor) ; true ),
	  /* We have a single true literal */
          ( %are we that literal ?
	    Pol == (+) -> 
	      (evidence(VId, 1) -> /* we are */ 
		  % inc_n(ParFactor, L1, 1),
		  assert(diff(VId,ParFactor, -1, 0)),
		  P0 = 1, P1 = exp(W) ;
		  /* we are not */
		  assert(diff(VId,ParFactor, 0, 0)),
		  % inc_n(ParFactor, L1, 0),
		  % inc_n(ParFactor, L1, 1),
	          P0 = 1, P1 = 1
	      )
	      ;
	      % NEGATIVE polarity
	      (evidence(VId, 1) -> /* we are not */ 
		  assert(diff(VId,ParFactor, 0, 0)),
		  % inc_n(ParFactor, L1, 0),
		  % inc_n(ParFactor, L1, 1),
	          P0 = 1, P1 = 1 ;
		  /* we are */
		  assert(diff(VId,ParFactor, 0, -1)),
		  % inc_n(ParFactor, L1, 0),
		  P0 = exp(W), P1 = 1
	      )
	  )
        ).

polarity(L, -, -L) :- !.
polarity(L, +, L) :- !.
polarity(_, _, _).

true_literal(-L, L, N, N1) :- !,
	( evidence(L, 1) -> N1 = N ; N1 is N+1 ).
true_literal(L, L, N, N1) :-
	( evidence(L, 1) -> N1 is N+1 ; N1 = N ).

inc(Id) :-
	retract(i(Id, N)), !,
	N1 is N+1,
	assert(i(Id, N1)).
inc(Id) :-
	assert(i(Id, 1)).

% V  is f (0)
check(W, V, -V, exp(W), _R, Matters, Matters0) :- !, Matters = Matters0.
check(_W, V,  V, R, R, Matters, Matters0) :- !, Matters = Matters0.
check(W, _V, -V, Rf, R0, Matters, Matters0) :- !,
	(evidence(V, 1) -> Rf = R0, Matters=Matters0; Rf = exp(W), Matters = not).
check(W, _V, V, Rf, R0, Matters, Matters0) :-
	(evidence(V, 1) -> Rf = exp(W), Matters = not ; Rf = R0, Matters=Matters0).

complement(W, exp(W), 1).
complement(W, 1, exp(W)).

