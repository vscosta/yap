:- module(ge_clpfd, [
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
/*                  (#\)/1,
                  (#<==>)/2,
                  (#==>)/2,
                  (#<==)/2,
                  (#\/)/2,
                  (#/\)/2, */
                  in/2 ,
                  ins/2,
                  all_different/1,
                  all_distinct/1, /*
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
	get_home(Space-Map),
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	Space += linear(CAs, As, 'IRT_EQ', B0).
( A #\= B) :-
	get_home(Space-Map),
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	Space += linear(CAs, As, 'IRT_NQ', B0).
( A #< B) :-
	get_home(Space-Map),
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	Space += linear(CAs, As, 'IRT_LE', B0).
( A #> B) :-
	get_home(Space-Map),
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	Space += linear(CAs, As, 'IRT_GT', B0).
( A #=< B) :-
	get_home(Space-Map),
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	Space += linear(CAs, As, 'IRT_LQ', B0).
( A #>= B) :-
	get_home(Space-Map),
	linear(A, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linear(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	Space += linear(CAs, As, 'IRT_GQ', B0).
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
	get_home(Space-Map),
	maplist(ll(Map), Xs, NXs),
	Space += distinct(NXs).
all_distinct( Xs ) :-
	get_home(Space-Map),
	maplist(ll(Map), Xs, NXs),
	Space += distinct(NXs).
labeling(_Opts, Xs) :-
	get_home(Space-Map),
	maplist(ll(Map), Xs, NXs),
	Space += branch(NXs, 'INT_VAR_SIZE_MIN', 'INT_VAL_MIN').

linear(V, C, [A|As], As, [C|CAs], CAs, I, I, _-Map) :- 
	var(V), !,
	l(V, A, Map).
linear(A+B, C, As, Bs, CAs, CBs, I, IF, Space) :-
	linear(A, C, As, A1s, CAs, CA1s, I, I1, Space),
	linear(B, C, A1s, Bs, CA1s, CBs, I1, IF, Space).
linear(A-B, C, As, Bs, CAs, CBs, I, IF, Space) :-
	NC is -C,
	linear(A, C, As, A1s, CAs, CA1s, I, I1, Space),
	linear(B, NC, A1s, Bs, CA1s, CBs, I1, IF, Space).
linear(A, C, As, As, CAs, CAs, I, IF, _) :-
	integer(A), !,
	IF is I-C*A.
linear(A, C, As, As, CAs, CAs, I, IF, _) :-
	ground(A),
	catch( (B is eval(A)), _, fail ), !,
	IF is I-C*B.
linear(C1*B, C, As, Bs, CAs, CBs, I, IF, Space) :-
	integer(C1), !,
	NC is C*C1,
	linear(B, NC, As, Bs, CAs, CBs, I, IF, Space).
linear(B*C1, C, As, Bs, CAs, CBs, I, IF, Space) :-
	integer(C1), !,
	NC is C*C1,
	linear(B, NC, As, Bs, CAs, CBs, I, IF, Space).

user:term_expansion( ( H :- B), (H :- (ge_clpfd:init_gecode(Space, Me), NB, ge_clpfd:close_gecode(Space, Vs, Me)) ) ) :-
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

