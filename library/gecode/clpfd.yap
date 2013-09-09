:- module(clpfd, [
                  op(760, yfx, #<==>),
                  op(750, xfy, #==>),
                  op(750, yfx, #<==),
                  op(740, yfx, #\/),
                  op(730, yfx, #\),
                  op(720, yfx, #/\),
                  op(710,  fy, #\),
                  op(700, xfx, #>),
                  op(700, xfx, #<),
                  op(700, xfx, #>=),
                  op(700, xfx, #=<),
                  op(700, xfx, #=),
                  op(700, xfx, #\=),
                  op(700, xfx, in),
                  op(700, xfx, ins),
                  op(450, xfx, ..), % should bind more tightly than \/
                  (#>)/2,
                  (#<)/2,
                  (#>=)/2,
                  (#=<)/2,
                  (#=)/2,
                  (#\=)/2,
                  (#<==>)/2,
                  (#==>)/2,
                  (#<==)/2,
                  (#\)/1,
                  (#\/)/2,
                  (#/\)/2,
                  in/2 ,
                  ins/2,
                  all_different/1,
                  all_distinct/1,
                  all_distinct/2, /*
                  sum/3,
                  scalar_product/4,
                  tuples_in/2, */
                  labeling/2 /*,
                  label/1,
                  indomain/1,
                  lex_chain/1,
                  serialized/2,
                  global_cardinality/2,
                  global_cardinality/3,
                  circuit/1,
                  element/3,
                  automaton/3,
                  automaton/8,
                  transpose/2,
                  zcompare/3,
                  chain/2,
                  fd_var/1,
                  fd_inf/2,
                  fd_sup/2,
                  fd_size/2,
                  fd_dom/2 */
                 ]).

:- use_module(library(gecode)).
:- use_module(library(maplist)).

constraint( (_ #> _) ).
constraint( (_ #< _) ).
constraint( (_ #>= _) ).
constraint( (_ #=< _) ).
constraint( (_ #= _) ).
constraint( (_ #\= _) ).
constraint( (_ #\ _) ).
constraint( (_ #<==> _) ).
constraint( (_ #==> _) ).
constraint( (_ #<== _) ).
constraint( (_ #\/ _) ).
constraint( (_ #/\ _) ).
constraint( in(_, _) ). %2,
constraint( ins(_, _) ). %2,
constraint( all_different(_) ). %1,
constraint( all_distinct(_) ). %1,
constraint( all_distinct(_,_) ). %1,
constraint( sum(_, _, _) ). %3,
constraint( scalar_product(_, _, _, _) ). %4,
constraint( tuples_in(_, _) ). %2,
constraint( labeling(_, _) ). %2,
constraint( label(_) ). %1,
constraint( indomain(_) ). %1,
constraint( lex_chain(_) ). %1,
constraint( serialized(_, _) ). %2,
constraint( global_cardinality(_, _) ). %2,
constraint( global_cardinality(_, _, _) ). %3,
constraint( circuit(_) ). %1,
constraint( element(_, _, _) ). %3,
constraint( automaton(_, _, _) ). %3,
constraint( automaton(_, _, _, _, _, _, _, _) ). %8,
constraint( transpose(_, _) ). %2,
constraint( zcompare(_, _, _) ). %3,
constraint( chain(_, _) ). %2,
constraint( fd_var(_) ). %1,
constraint( fd_inf(_, _) ). %2,
constraint( fd_sup(_, _) ). %2,
constraint( fd_size(_, _) ). %2,
constraint( fd_dom(_, _) ). %2



process_constraints((B0,B1), (NB0, NB1), Env) :-
	process_constraints(B0, NB0, Env),
	process_constraints(B1, NB1, Env).
process_constraints(B, B, env(_Space)) :-
	constraint(B), !.
process_constraints(B, B, _Env).
%	process_constraint(B, NB, Space).

( A #= B) :-
	get_home(Env),
	post( (A #= B), Env, _).
( A #\= B) :-
	get_home(Env),
	post( (A #\= B), Env, _).
( A #< B) :-
	get_home(Env),
	post( (A #< B), Env, _).
( A #> B) :-
	get_home(Env),
	post( (A #< B), Env, _).
( A #=< B) :-
	get_home(Env),
	post( (A #=< B), Env, _).
( A #>= B) :-
	get_home(Env),
	post( (A #> B), Env, _).
( A #<==> Bool) :-
	get_home(Space-Map),
	Bool := boolvar(Space),
	Space += reify(Bool, 'RM_EQV', R),
	post(A, Space-Map, R).
( A #==> Bool) :-
	get_home(Space-Map),
	Bool := boolvar(Space),
	Space += reify(Bool, 'RM_IMP', R),
	post(A, Space-Map, R).
( A #<== Bool) :-
	get_home(Space-Map),
	Bool := boolvar(Space),
	Space += reify(Bool, 'RM_PMI', R),
	post(A, Space-Map, R).
'#\\'(A) :-
	get_home(Space-Map),
	B := boolvar(Space),
	Space += reify(B, 'RM_EQV', R),
	Space += rel(B, 'BOT_EQV', 0),
	post(A, Space-Map, R).
( A1 #\/ A2 ) :-
	get_home(Space-Map),
	B1 := boolvar(Space),
	B2 := boolvar(Space),
	Space += reify(B1, 'RM_EQV', R1),
	Space += reify(B2, 'RM_EQV', R2),
	post(A1, Space-Map, R1),
	post(A2, Space-Map, R2),
	Space += rel(B1, B2, 'BOT_OR', 1).
( A1 #/\ A2 ) :-
	get_home(Space-Map),
	B1 := boolvar(Space),
	B2 := boolvar(Space),
	Space += reify(B1, 'RM_EQV', R1),
	Space += reify(B2, 'RM_EQV', R2),
	post(A1, Space-Map, R1),
	post(A2, Space-Map, R2),
	Space += rel(B1, B2, 'BOT_AND', 1).
( X in A..B) :-
	get_home(Space-Map),
	m(X, NX, A, B, Map),
	NX := intvar(Space, A, B).
( Xs ins A..B) :-
	get_home(Space-Map),
	maplist(lm(A, B, Map), Xs, NXs),
	length(Xs, N),
	NXs := intvars(Space, N, A, B).
all_different( Xs ) :-
	get_home(Env),
	post( all_different( Xs ), Env, _ ).
all_distinct( Xs ) :-
	get_home(Env),
	post( all_distinct( Xs ), Env, _ ).
all_distinct( Cs, Xs ) :-
	get_home(Env),
	post( all_distinct( Cs, Xs ), Env, _ ).
labeling(_Opts, Xs) :-
	get_home(Space-Map),
	maplist(ll(Map), Xs, NXs),
	Space += branch(NXs, 'INT_VAR_SIZE_MIN', 'INT_VAL_MIN').


post( (A #= B), Space-Map, Reify):-
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	(var(Reify) ->
	    Space += linear(CAs, As, 'IRT_EQ', B0);
	    Space += linear(CAs, As, 'IRT_EQ', B0, Reify)
	).
post( (A #\= B), Space-Map, Reify):-
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	(var(Reify) ->
	    Space += linear(CAs, As, 'IRT_NQ', B0);
	    Space += linear(CAs, As, 'IRT_NQ', B0, Reify)
	).
post( (A #>B), Space-Map, Reify):-
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	(var(Reify) ->
	    Space += linear(CAs, As, 'IRT_NQ', B0);
	    Space += linear(CAs, As, 'IRT_NQ', B0, Reify)
	).
post( (A #>=B), Space-Map, Reify):-
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	(var(Reify) ->
	    Space += linear(CAs, As, 'IRT_GQ', B0);
	    Space += linear(CAs, As, 'IRT_GQ', B0, Reify)
	).
post( (A #<B), Space-Map, Reify):-
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	(var(Reify) ->
	    Space += linear(CAs, As, 'IRT_LE', B0);
	    Space += linear(CAs, As, 'IRT_LE', B0, Reify)
	).
post( (A #=<B), Space-Map, Reify):-
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	(var(Reify) ->
	    Space += linear(CAs, As, 'IRT_LQ', B0);
	    Space += linear(CAs, As, 'IRT_LQ', B0, Reify)
	).
post( all_different( Xs ), Space-Map, Reify) :-
	maplist(ll(Map), Xs, NXs),
	(var(Reify) ->
	    Space += distinct(NXs)
	;
	    throw(error(domain(not_reifiable),all_different( Xs )))
	).
post( all_distinct( Xs ), Space-Map, Reify) :-
	maplist(ll(Map), Xs, NXs),
	(var(Reify) ->
	    Space += distinct(NXs)
	;
	    throw(error(domain(not_reifiable),all_distinct( Xs )))
	).
post( all_distinct( Cs , Xs ), Space-Map, Reify) :-
	maplist(ll(Map), Xs, NXs),
	(var(Reify) ->
	    Space += distinct(Cs,NXs)
	;
	    throw(error(domain(not_reifiable),all_distinct( Cs , Xs )))
	).


linear(V, C, [A|As], As, [C|CAs], CAs, I, I, _-Map) :- 
	var(V), !,
	l(V, A, Map).
linear(A+B, C, As, Bs, CAs, CBs, I, IF, Env) :-
	linear(A, C, As, A1s, CAs, CA1s, I, I1, Env),
	linear(B, C, A1s, Bs, CA1s, CBs, I1, IF, Env).
linear(A-B, C, As, Bs, CAs, CBs, I, IF, Env) :-
	NC is -C,
	linear(A, C, As, A1s, CAs, CA1s, I, I1, Env),
	linear(B, NC, A1s, Bs, CA1s, CBs, I1, IF, Env).
linear(A, C, As, As, CAs, CAs, I, IF, _) :-
	integer(A), !,
	IF is I-C*A.
linear(A, C, As, As, CAs, CAs, I, IF, _) :-
	ground(A),
	catch( (B is eval(A)), _, fail ), !,
	IF is I-C*B.
linear(C1*B, C, As, Bs, CAs, CBs, I, IF, Env) :-
	integer(C1), !,
	NC is C*C1,
	linear(B, NC, As, Bs, CAs, CBs, I, IF, Env).
linear(B*C1, C, As, Bs, CAs, CBs, I, IF, Env) :-
	integer(C1), !,
	NC is C*C1,
	linear(B, NC, As, Bs, CAs, CBs, I, IF, Env).
linear(AC, C, [V|Bs], Bs, [C|CBs], CBs, I, I, Env) :-
	arith(AC),
	equality(AC, V, Env).

equality(abs(V), NV, Env) :-
	( var(V) -> VA = V ; equality(V, VA, Env) ),
	new_abs(VA, NV, Env).
equality(V1+V2, NV, Env) :-
	( var(V1) -> V1A = V1 ; equality(V1, V1A, Env) ),
	( var(V2) -> V2A = V2 ; equality(V2, V2A, Env) ),
	new_plus(V1A, V2A, NV, ENV).
equality(V1-V2, NV, Env) :-
	( var(V1) -> V1A = V1 ; equality(V1, V1A, Env) ),
	( var(V2) -> V2A = V2 ; equality(V2, V2A, Env) ),
	new_minus(V1A, V2A, NV, ENV).

new_abs( V, NV, Space-Map) :-
	l(V, X, Min0, Max0, Map),
	( Min0 < 0 ->
	    ( Max0 < 0 -> Min is -Max0, Max is -Min0 ;
		Min = 0 , Max is max( -Min, Max ) )
	    ;
	    Min = Min0, Max = Max0
	),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += abs(X, NX).

new_minus( V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	Min is Min1-Max2,
	Max is Max1-Min2,
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += linear([1,-1], [X1,X2], NX).

new_plus( V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	Min is Min1+Min2,
	Max is Max1+Max2,
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += linear([1,1], [X1,X2], NX).

	

user:term_expansion( ( H :- B), (H :- (clpfd:init_gecode(Space, Me), NB, clpfd:close_gecode(Space, Vs, Me)) ) ) :-
	process_constraints(B, NB, Env),
	term_variables(H, Vs),
	nonvar( Env ), !,
	Env = env( Space ).

init_gecode(Space, old) :-
	nb_current(gecode_space, Space), nonvar(Space), !.
init_gecode(Space-Map, new) :-
	Space := space,
	b_setval(gecode_space, Space-Map).

close_gecode(_Space, _Vs, old) :- !.
close_gecode(Space-Map, Vs0, new) :- 
	term_variables(Vs0, Vs),
	selectlist(intvar(Map), Vs, CVs),
	maplist(ll(Map), CVs, IVs),
	SolSpace := search(Space),
	CVs := val(SolSpace,IVs).

intvar(Map, V) :-
	l(V, _IV, Map).

get_home(Home) :-
	b_getval(gecode_space, Home).

m(NV, OV, NA, NB, Vs) :-
	var(Vs), !,
	Vs = [v(NV,OV,NA,NB)|_].
m(NV, OV, NA, NB, [_|Vs]) :-
	m(NV, OV, NA, NB, Vs).

lm(A, B, Map, X, Y) :-
	m(X, Y, A, B, Map).

l(NV, OV, Vs) :-
	var(Vs), !,
	fail.
l(NV, OV, [v(V, OV, _A, _B)|_Vs]) :-
	V == NV, !.
l(NV, OV, [_|Vs]) :-
	l(NV, OV, Vs).

ll(Map, X, Y) :-
	l(X, Y, Map).

l(NV, OV, _, _, Vs) :-
	var(Vs), !,
	fail.
l(NV, OV, A, B, [v(V, OV, A, B)|_Vs]) :-
	V == NV, !.
l(NV, O, A, BV, [_|Vs]) :-
	l(NV, OV, A, B, Vs).

