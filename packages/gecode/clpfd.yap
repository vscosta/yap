/**
@defgroup Gecode_and_ClPbBFDbC Programming Finite Domain Constraints in YAP/Gecode
@ingroup Gecode
@{

The gecode/clp(fd) interface is designed to use the GECODE functionality
in a more CLP like style. It requires

~~~~~{.prolog}
:- use_module(library(gecode/clpfd)).
~~~~~
Several example programs are available with the distribution.

Integer variables are declared as:

+ _V_ in  _A_.. _B_
declares an integer variable  _V_ with range  _A_ to  _B_.
+ _Vs_ ins  _A_.. _B_
declares a set of integer variabless  _Vs_ with range  _A_ to  _B_.
+ boolvar( _V_)
declares a  boolean variable.
+ boolvars( _Vs_)
declares a set of  boolean variable.


Constraints supported are:

*/


:- module(gecode_clpfd, [
		  op(100, yf, []),
                  op(760, yfx, #<==>),
                  op(750, xfy, #==>),
                  op(750, yfx, #<==),
                  op(740, yfx, #\/),
                  op(730, yfx, #\),
                  op(720, yfx, #/\),
                  op(710,  fy, #\),
                  op(705, xfx, where),
                  op(700, xfx, #>),
                  op(700, xfx, #<),
                  op(700, xfx, #>=),
                  op(700, xfx, #=<),
                  op(700, xfx, #=),
                  op(700, xfx, #\=),
                  op(700,  xf, #>),
                  op(700,  xf, #<),
                  op(700,  xf, #>=),
                  op(700,  xf, #=<),
                  op(700,  xf, #=),
                  op(700,  xf, #\=),
		  op(500, yfx, '<=>'),
		  op(500, yfx, '=>'),
                  op(450, xfx, ..), % should bind more tightly than \/
                  (#>)/2,
                  (#<)/2,
                  (#>=)/2,
                  (#=<)/2,
                  (#=)/2,
                  (#\=)/2,
                  (#>)/1,
                  (#<)/1,
                  (#>=)/1,
                  (#=<)/1,
                  (#=)/1,
                  (#\=)/1,
                  (#<==>)/2,
                  (#==>)/2,
                  (#<==)/2,
                  (#\)/1,
                  (#\/)/2,
                  (#/\)/2,
                  in/2 ,
                  ins/2,
		  boolvar/1,
		  boolvars/1,
                  all_different/1,
                  all_distinct/1,
                  all_distinct/2,
		  maximize/1,
		  minimize/1,
                  sum/3,
                  lex_chain/1,
		  minimum/2,
		  min/2,
		  maximum/2,
		  max/2,
                  scalar_product/4,
		  element/2,
		  extensional_constraint/2,
		  in_relation/2,
		  clause/4,
		  dfa/4,
		  in_dfa/2,
		  in_dfa/4, /*
                  tuples_in/2, */
                  labeling/2 /*,
                  label/1,
                  indomain/1,
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

/** @pred _X_ #<  _B_  is det
reified implication

As an example. consider finding out the people who wanted to sit
next to a friend and that are are actually sitting together:

~~~~~{.prolog}
preference_satisfied(X-Y, B) :-
    abs(X - Y) #= 1 #<==> B.
~~~~~
Note that not all constraints may be reifiable.


*/
/** @pred _X_ #<  _Y_ is semidet
smaller or equal

Arguments to this constraint may be an arithmetic expression with <tt>+</tt>,
<tt>-</tt>, <tt>\\*</tt>, integer division <tt>/</tt>, <tt>min</tt>, <tt>max</tt>, <tt>sum</tt>,
<tt>count</tt>, and
<tt>abs</tt>. Boolean variables support conjunction (/\), disjunction (\/),
implication (=>), equivalence (<=>), and xor. The <tt>sum</tt> constraint allows  a two argument version using the
`where` conditional, in Zinc style.

The send more money equation may be written as:

~~~~~{.prolog}
          1000*S + 100*E + 10*N + D +
          1000*M + 100*O + 10*R + E #=
10000*M + 1000*O + 100*N + 10*E + Y,
~~~~~

This example uses `where` to select from
column  _I_ the elements that have value under  _M_:

~~~~~{.prolog}
OutFlow[I] #= sum(J in 1..N where D[J,I]<M, X[J,I])
~~~~~

The <tt>count</tt> constraint counts the number of elements that match a
certain constant or variable (integer sets are not available).


*/
/** @pred _X_ #<==>  _B_  is det
reified equivalence

*/
/** @pred _X_ #=  _Y_ is semidet
equality

*/
/** @pred _X_ #=<  _Y_ is semidet
smaller

*/
/** @pred _X_ #==>  _B_  is det
Reified implication

*/
/** @pred _X_ #>  _Y_ is semidet
larger

*/
/** @pred _X_ #>=  _Y_ is semidet
larger or equal

*/
/** @pred _X_ #\=  _Y_ is semidet
disequality

*/
/** @pred labeling( _Opts_,  _Xs_)
performs labeling, several variable and value selection options are
available. The defaults are `min` and `min_step`.

Variable selection options are as follows:

+ leftmost
choose the first variable
+ min
choose one of the variables with smallest minimum value
+ max
choose one of the variables with greatest maximum value
+ ff
choose one of the most constrained variables, that is, with the smallest
domain.


Given that we selected a variable, the values chosen for branching may
be:

+ min_step
smallest value
+ max_step
largest value
+ bisect
median
+ enum
all value starting from the minimum.



*/
/** @pred scalar_product(+ _Cs_, + _Vs_, + _Rel_, ? _V_    )

The product of constant  _Cs_ by  _Vs_ must be in relation
 _Rel_ with  _V_ .


*/

:- use_module(library(gecode)).
:- use_module(library(maplist)).

:- reexport(library(matrix), [(<==)/2, op(800, xfx, '<=='),
	    op(700, xfx, in),
	    op(700, xfx, ins),
            op(450, xfx, ..), % should bind more tightly than \/
	    op(710, xfx, of),
	    foreach/2, foreach/4, of/2]).

% build array of constraints
%
matrix:array_extension(_.._ , gecode_clpfd:build).

build( I..J, _, Size, L) :-
	length( L, Size ),
	L ins I..J.

matrix:rhs_opaque(X) :- constraint(X).

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
constraint( min(_, _) ). %2,
constraint( minimum(_, _) ). %2,
constraint( max(_, _) ). %2,
constraint( maximum(_, _) ). %2,
constraint( in_relation(_, _) ). %2,
constraint( in_dfa(_, _) ). %2,
constraint( in_dfa(_, _, _, _) ). %2,
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
constraint( element(_, _) ). %2,
constraint( fd_var(_) ). %1,
constraint( fd_inf(_, _) ). %2,
constraint( fd_sup(_, _) ). %2,
constraint( fd_size(_, _) ). %2,
constraint( fd_dom(_, _) ). %2
constraint( clause(_, _, _, _) ). %2


process_constraints((B0,B1), (NB0, NB1), Env) :-
	process_constraints(B0, NB0, Env),
	process_constraints(B1, NB1, Env).
process_constraints(B, B, env(_Space)) :-
	constraint(B), !.
process_constraints(B, B, _Env).
%	process_constraint(B, NB, Space).

( A #= B) :-
	get_home(Env),
	check(A, NA),
	check(B, NB),
	post( rel(NA,  (#=),  NB), Env, _).
( A #\= B) :-
	get_home(Env),
	check(A, NA),
	check(B, NB),
	post( rel(NA,  (#\=),  NB), Env, _).
( A #< B) :-
	get_home(Env),
	check(A, NA),
	check(B, NB),
	post( rel(NA,  (#<),  NB), Env, _).
( A #> B) :-
	get_home(Env),
	check(A, NA),
	check(B, NB),
	post( rel(NA,  (#>),  NB), Env, _).
( A #=< B) :-
	get_home(Env),
	check(A, NA),
	check(B, NB),
	post( rel(NA,  (#=<),  NB), Env, _).
( A #>= B) :-
	get_home(Env),
	check(A, NA),
	check(B, NB),
	post( rel(NA,  (#>=),  NB), Env, _).
( A #= ) :-
	get_home(Env),
	check(A, NA),
	post( rel(NA,  (#=)), Env, _).
/** @pred _X_ #= is det
all elements of  _X_  must take the same value

*/
( A #\= ) :-
	get_home(Env),
	check(A, NA),
	post( rel(NA,  (#\=)), Env, _).
/** @pred _X_ #<  is det
elements of  _X_  must be decreasing or equal


*/
( A #< ) :-
	get_home(Env),
	check(A, NA),
	post( rel(NA,  (#<)), Env, _).
/** @pred _X_ #>  is det
elements of  _X_  must be increasing

*/
( A #> ) :-
	get_home(Env),
	check(A, NA),
	post( rel(NA,  (#>)), Env, _).
/** @pred _X_ #=<  is det
elements of  _X_  must be decreasing

*/
( A #=< ) :-
	get_home(Env),
	check(A, NA),
	post( rel(NA,  (#=<) ), Env, _).
/** @pred _X_ #>=  is det
elements of  _X_  must be increasinga or equal

*/
( A #>= ) :-
	get_home(Env),
	check(A, NA),
	post( rel(NA,  (#>=)), Env, _).
sum( L, Op, V) :-
	get_home( Env ),
	check(L, NL),
	check(V, NV),
	post( rel(sum(NL), Op, NV), Env, _).
( ( A #<==> VBool )) :-
	get_home(Space-Map),
	check(A, NA),
	check(VBool, NVBool),
	Bool := boolvar(Space),
	m( NVBool, Bool, 0, 1, Map),
	Space += reify(Bool, 'RM_EQV', R),
	post(NA, Space-Map, R).
( A #==> VBool) :-
	get_home(Space-Map),
	check(A, NA),
	check(VBool, NVBool),
	Bool := boolvar(Space),
	m( NVBool, Bool, 0, 1, Map),
	Space += reify(Bool, 'RM_IMP', R),
	post(NA, Space-Map, R).
( A #<== VBool) :-
	get_home(Space-Map),
	check(A, NA),
	check(VBool, NVBool),
	Bool := boolvar(Space),
	m( NVBool, Bool, 0, 1, Map),
	Space += reify(Bool, 'RM_PMI', R),
	post(NA, Space-Map, R).
'#\\'(A) :-
	get_home(Space-Map),
	check(A, NA),
	B := boolvar(Space),
	Space += reify(B, 'RM_EQV', R),
	Space += rel(B, 'BOT_EQV', 0),
	post(NA, Space-Map, R).
( A1 #\/ A2 ) :-
	get_home(Space-Map),
	check(A1, NA1),
	check(A2, NA2),
	B1 := boolvar(Space),
	B2 := boolvar(Space),
	Space += reify(B1, 'RM_EQV', R1),
	Space += reify(B2, 'RM_EQV', R2),
	post(NA1, Space-Map, R1),
	post(NA2, Space-Map, R2),
	Space += rel(B1, B2, 'BOT_OR', 1).
( A1 #/\ A2 ) :-
	get_home(Space-Map),
	check(A1, NA1),
	check(A2, NA2),
	B1 := boolvar(Space),
	B2 := boolvar(Space),
	Space += reify(B1, 'RM_EQV', R1),
	Space += reify(B2, 'RM_EQV', R2),
	post(NA1, Space-Map, R1),
	post(NA2, Space-Map, R2),
	Space += rel(B1, B2, 'BOT_AND', 1).
( X in A..B) :-
	get_home(Space-Map),
	check(A, NA),
	check(B, NB),
	m(X, NX, NA, NB, Map),
	NX := intvar(Space, NA, NB).
( Xs ins A..B) :-
	get_home(Space-Map),
 	check(A, NA),
	check(B, NB),
	maplist(lm(NA, NB, Map), Xs, NXs),
	length(Xs, N),
	NXs := intvars(Space, N, NA, NB).
boolvar( X ) :-
	get_home(Space-Map),
	m(X, NX, 0, 1, Map),
	NX := boolvar( Space ).
boolvars( Xs ) :-
	get_home(Space-Map),
	maplist(lm(0, 1, Map), Xs, NXs),
	length(Xs, N),
	NXs := boolvars( Space, N ).

/** @pred all_different( _Vs_    )
  Verifies whether all elements of a list are different.
*/
all_different( Xs ) :-
	get_home(Env),
	check(Xs, NXs),
	post( all_different( NXs ), Env, _ ).

all_distinct( Xs ) :-
	get_home(Env),
	check(Xs, NXs),
	post( all_distinct( NXs ), Env, _ ).
all_distinct( Cs, Xs ) :-
	get_home(Env),
	check(Xs, NXs),
	post( all_distinct( Cs, NXs ), Env, _ ).
scalar_product( Cs, Vs, Rels, X ) :-
	get_home(Env),
	check(Vs, NVs),
	post( scalar_product( Cs, NVs, Rels, X ), Env, _ ).
lex_chain( Cs ) :-
	get_home(Env),
	check(Cs, NCs),
	post( rel( NCs, '#=<' ), Env, _ ).
minimum( V, Xs ) :-
	get_home(Env),
	check(Xs, NXs),
	check(V, NV),
	post( rel( min(NXs), (#=), NV ), Env, _ ).
min( Xs, V ) :-
	get_home(Env),
	check(Xs, NXs),
	check(V, NV),
	post( rel( min(NXs), (#=), NV ), Env, _ ).
maximum( V, Xs ) :-
	get_home(Env),
	check(Xs, NXs),
	check(V, NV),
	post( rel( max(NXs), (#=), NV ), Env, _ ).
max( Xs, V ) :-
	get_home(Env),
	check(Xs, NXs),
	check(V, NV),
	post( rel( max(NXs), (#=), NV ), Env, _ ).
element( V, Xs ) :-
	get_home(Env),
	check(Xs, NXs),
	check(V, NV),
	post( element( NV, NXs ), Env, _ ).
in_relation( Xs, Rel ) :-
	get_home(Env),
	check(Xs, NXs),
	post(in_tupleset(NXs, Rel), Env, _ ).
in_dfa( Xs, Rel ) :-
	get_home(Env),
	check(Xs, NXs),
	post(in_dfa(NXs, Rel), Env, _ ).
in_dfa( Xs, S0, Ts, Fs ) :-
	get_home(Env),
	check(Xs, NXs),
	post(in_dfa(NXs, S0, Ts, Fs), Env, _ ).
clause( and, Ps, Ns, V ) :-
	get_home(Env),
	check(Ps, NPs),
	check(Ns, NNs),
	check(V, NV),
	post(clause( 'BOT_AND', NPs, NNs, NV), Env, _ ).
clause( or, Ps, Ns, V ) :-
	get_home(Env),
	check(Ps, NPs),
	check(Ns, NNs),
	check(V, NV),
	post(clause( 'BOT_OR', NPs, NNs, NV), Env, _ ).

labeling(Opts, Xs) :-
	get_home(Space-Map),
	foldl2( processs_lab_opt, Opts, 'INT_VAR_SIZE_MIN', BranchVar, 'INT_VAL_MIN', BranchVal),
	term_variables(Xs, Vs),
	check( Vs, X1s ),
	( X1s == [] -> true ;
	  maplist(ll(Map), X1s, NXs),
	  Space += branch(NXs, BranchVar, BranchVal) ).

processs_lab_opt(leftmost, _, 'INT_VAR_NONE', BranchVal, BranchVal).
processs_lab_opt(min, _, 'INT_VAR_SIZE_MIN', BranchVal, BranchVal).
processs_lab_opt(max, _, 'INT_VAR_SIZE_MAX', BranchVal, BranchVal).
processs_lab_opt(ff, _, 'INT_VAR_DEGREE_MIN', BranchVal, BranchVal).
processs_lab_opt(min_step, BranchVar, BranchVar, _, 'INT_VAL_MIN').
processs_lab_opt(max_step, BranchVar, BranchVar, _, 'INT_VAL_MIN').
processs_lab_opt(bisect, BranchVar, BranchVar, _, 'INT_VAL_MED').
processs_lab_opt(enum, BranchVar, BranchVar, _, 'INT_VALUES_MIN').


maximize(V) :-
	get_home(Space-Map),
	l(V, I, Map),
	Space += maximize(I).

minimize(V) :-
	get_home(Space-Map),
	l(V, I, Map),
	Space += minimize(I).

extensional_constraint( Tuples, TupleSet) :-
	TupleSet := tupleset( Tuples ).

dfa( S0, Transitions, Finals, DFA) :-
	DFA := dfa( S0, Transitions, Finals ).


check(V, NV) :-
	( var(V) -> V = NV ;
	  number(V) -> V = NV ;
	  is_list(V) -> maplist(check, V, NV) ;
	  V = sum(_,_) -> V = NV ;
	  V = '[]'(Indx, Mat) -> NV <== '[]'(Indx, Mat) ;
	  V = '$matrix'(_, _, _, _, C) -> C =.. [_|L], maplist(check, L, NV)  ;
	  V = A+B -> check(A,NA), check(B, NB), NV = NB+NA ;
	  V = A-B -> check(A,NA), check(B, NB), NV = NB-NA ;
	  arith(V, _) -> V =.. [C|L], maplist(check, L, NL), NV =.. [C|NL] ;
	  constraint(V) -> V =.. [C|L], maplist(check, L, NL), NV =.. [C|NL] ).

post( ( A #= B), Env, Reify) :-
	post( rel( A, (#=), B), Env, Reify).
post( ( A #\= B), Env, Reify) :-
	post( rel( A, (#\=), B), Env, Reify).
post( ( A #> B), Env, Reify) :-
	post( rel( A, (#>), B), Env, Reify).
post( ( A #< B), Env, Reify) :-
	post( rel( A, (#<), B), Env, Reify).
post( ( A #>= B), Env, Reify) :-
	post( rel( A, (#>=), B), Env, Reify).
post( ( A #=< B), Env, Reify) :-
	post( rel( A, (#=<), B), Env, Reify).
% [X,Y,Z] #<
post( rel( A, Op), Space-Map, Reify):-
	( var( A ) -> l(A, IA, Map) ; checklist( var, A ) -> maplist(ll(Map), A, IA ) ),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->	Space += rel(IA, GOP) ;
	    Space += rel(IA, GOP, Reify) ).

% X #< Y
% X #< 2
post( rel( A, Op, B), Space-Map, Reify):-
	var(A),
	( var(B) -> l(B, IB, Map) ; integer(B) -> IB = B ), !,
	l(A, IA, Map),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->	Space += rel(IA, GOP, IB) ;
	    Space += rel(IA, GOP, IB, Reify) ).

% 2 #\= B -> reverse
post( rel( A, Op, B), Space-Map, Reify) :-
	( var(A) ; integer(A) ), !,
	reverse_arith_op( Op, ROp ),
	post( rel( B, ROp, A), Space-Map, Reify).

% A is never unbound

% [A,B,C,D] #< 3
post( rel( A, Op, B ), Space-Map, Reify):-
	checklist( var, A ),  !,
	maplist(ll(Map), A, IL ),
	( var(B) -> l(B, IB, Map) ; integer(B) -> IB = B ; equality(B, NB, Space-Map), l(NB, IB, Map) ), !,
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->	Space += rel(IL, GOP) ;
	    Space += rel(IL, GOP, IB) ).

% sum([A,B,C]) #= X
post( rel( sum(L), Op, Out), Space-Map, Reify):- !,
	checklist( var, L ), !,
	maplist(ll(Map), L, IL ),
	( var(Out) -> l(Out, IOut, Map) ; integer(Out) -> IOut = Out ; equality(Out, NOut, Space-Map), l(NOut, IOut, Map) ),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->
	    Space += linear(IL, GOP, IOut);
	 Space += linear(IL, GOP, IOut, Reify)
	).

% count([A,B,C], 3) #= X
post( rel( count(X, Y), Op, Out), Space-Map, Reify):- !,
	( checklist( var, X ) ->  maplist(ll(Map), X, IX ) ),
	( var(Y) -> l(Y, IY, Map) ; integer(Y) -> IY = Y ; equality(Y, NY, Space-Map), l(NY, IY, Map)   ),
	( var(Out) -> l(Out, IOut, Map) ; integer(Out) -> IOut = Out ; equality(Out, NOut, Space-Map), l(NOut, IOut, Map) ), !,
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->
	    Space += count(IX, IY, GOP, IOut);
	 Space += count(IX, IY, GOP, IOut, Reify)
	).


% sum([I in 0..N-1, M[I]]) #= X
post( rel( sum(Foreach, Cond), Op, Out), Space-Map, Reify):- !,
	( var(Out) -> l(Out, IOut, Map) ; integer(Out) -> IOut = Out ; equality(Out, NOut, Space-Map), l(NOut, IOut, Map) ),
	cond2list( Foreach, Cond, Cs, L),
	maplist(ll(Map), [Out|L], [IOut|IL] ),
	gecode_arith_op( Op, GOP ),
	(L = [] -> true ;
	 var(Reify) ->
	    Space += linear(Cs, IL, GOP, IOut);
	 Space += linear(Cs, IL, GOP, IOut, Reify)
	).

post( rel(A1+A2, Op, B), Space-Map, Reify):-
	( nonvar(B) ; B = _ + _ ; B = _-_), !,
	linearize(A1+A2, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linearize(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->
	    ( checklist(is_one, CAs) ->
		Space += linear(As, GOP, B0);
		Space += linear(CAs, As, GOP, B0)
	    )
	    ;
	    ( checklist(is_one, CAs) ->
		Space += linear(As, GOP, B0, Reify);
		Space += linear(CAs, As, GOP, B0, Reify)
	    )
	).

post( rel(A1-A2, Op, B), Space-Map, Reify):-
	( nonvar(B) ; B = _ + _ ; B = _-_), !,
	linearize(A1-A2, 1, As, Bs, CAs, CBs, 0, A0, Space-Map),
	linearize(B, -1, Bs, [], CBs, [], A0, B0, Space-Map),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->
	    ( checklist(is_one, CAs) ->
		Space += linear(As, GOP, B0);
		Space += linear(CAs, As, GOP, B0)
	    )
	    ;
	    ( checklist(is_one, CAs) ->
		Space += linear(As, GOP, B0, Reify);
		Space += linear(CAs, As, GOP, B0, Reify)
	    )
	).

post( rel(A, Op, B), Space-Map, Reify):-
	arith(A, Name),
	A =.. [_Op,A1],
	is_list(A1), !,
	( _Op = min -> true ; _Op = max  ),
	maplist(equality_l( Space-Map),  A1, NA1),
	maplist(in_c_l( Space-Map), NA1, VA1),
	equality(B, B1,  Space-Map),
	out_c(Name, VA1, B1,  Op, Space-Map, Reify).

post( rel(A, Op, B), Space-Map, Reify):-
	arith(A, Name),
	A =.. [_Op,A1], !,
	equality(A1, NA1,  Space-Map),
	in_c(NA1, VA1,  Space-Map), !,
	equality(B, B1,  Space-Map),
	out_c(Name, VA1, B1,  Op, Space-Map, Reify).

post( rel(A, Op, B), Space-Map, Reify):-
	arith(A, Name),
	A =.. [_Op,A1,A2], !,
	equality(A1, NA1,  Space-Map),
	in_c(NA1, VA1,  Space-Map),
	equality(A2, NA2,  Space-Map),
	in_c(NA2, VA2,  Space-Map),
	equality(B, B1,  Space-Map),
	out_c(Name, VA1, VA2, B1,  Op, Space-Map, Reify).

post( scalar_product(Cs, L, Op, Out), Space-Map, Reify):-
	var(Out), !,
	maplist(ll(Map), [Out|L], [IOut|IL] ),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->
	    Space += linear(Cs, IL, GOP, IOut);
	    Space += linear(Cs, IL, GOP, IOut, Reify)
	).
post( scalar_product(Cs, L, Op, Out), Space-Map, Reify):-
	integer(Out), !,
	maplist(ll(Map), L, IL ),
	gecode_arith_op( Op, GOP ),
	(var(Reify) ->
	    Space += linear(Cs, IL, GOP, Out);
	    Space += linear(Cs, IL, GOP, Out, Reify)
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
post(in_tupleset(Xs, Tuples), Space-Map, Reify) :-
	is_list( Tuples ), !,
	TS := tupleset( Tuples ),
	maplist(ll(Map), Xs, IXs),
	(var(Reify) ->
	    Space += extensional(IXs, TS)
	;
	    throw(error(domain(not_reifiable),in_relation(Xs, Tuples)))
	).
post(in_tupleset(Xs, TS), Space-Map, Reify) :-
	maplist(ll(Map), Xs, IXs),
	(var(Reify) ->
	    Space += extensional(IXs, TS)
	;
	    throw(error(domain(not_reifiable),in_relation(Xs, TS)))
	).
post(in_dfa(Xs, S0, Trs, Fs), Space-Map, Reify) :-
	TS := dfa( S0, Trs, Fs ),
	maplist(ll(Map), Xs, IXs),
	(var(Reify) ->
	    Space += extensional(IXs, TS)
	;
	    throw(error(domain(not_reifiable),in_dfa(Xs, S0, Trs, Fs)))
	).
post(in_dfa(Xs, TS), Space-Map, Reify) :-
	maplist(ll(Map), Xs, IXs),
	(var(Reify) ->
	    Space += extensional(IXs, TS)
	;
	    throw(error(domain(not_reifiable),in_dfa(Xs, TS)))
	).

post(element(V, Xs), Space-Map, Reify) :-
	l(V, IV, Map),
	maplist(ll(Map), Xs, IXs),
	(var(Reify) ->
	    Space += element(IV, IXs)
	;
	    Space += element(IV, IXs, Reify)
	).

post(clause( Type, Ps, Ns, V), Space-Map, Reify) :-
	(integer(V) -> V = IV ; l(V, IV, Map) ),
	maplist(ll(Map), Ps, IPs),
	maplist(ll(Map), Ns, INs),
	(var(Reify) ->
	 Space += clause(Type, IPs, INs, IV)
	;
	 Space += clause(Type, IPs, INs, IV, Reify)
	).

gecode_arith_op( (#=)  , 'IRT_EQ' ).
gecode_arith_op( (#\=) , 'IRT_NQ' ).
gecode_arith_op( (#>)  , 'IRT_GR' ).
gecode_arith_op( (#>=) , 'IRT_GQ' ).
gecode_arith_op( (#<)  , 'IRT_LE' ).
gecode_arith_op( (#=<) , 'IRT_LQ' ).

reverse_arith_op( (#=)  ,  (#=) ).
reverse_arith_op( (#\=) ,  (#\=) ).
reverse_arith_op( (#>)  ,  (#<) ).
reverse_arith_op( (#>=) ,  (#=<) ).
reverse_arith_op( (#<)  ,  (#>) ).
reverse_arith_op( (#=<) ,  (#>=) ).

linearize(V, C, [A|As], As, [C|CAs], CAs, I, I, _-Map) :-
	var(V), !,
	l(V, A, Map).
linearize(A+B, C, As, Bs, CAs, CBs, I, IF, Env) :-
	linearize(A, C, As, A1s, CAs, CA1s, I, I1, Env),
	linearize(B, C, A1s, Bs, CA1s, CBs, I1, IF, Env).
linearize(A-B, C, As, Bs, CAs, CBs, I, IF, Env) :-
	NC is -C,
	linearize(A, C, As, A1s, CAs, CA1s, I, I1, Env),
	linearize(B, NC, A1s, Bs, CA1s, CBs, I1, IF, Env).
linearize(A, C, As, As, CAs, CAs, I, IF, _) :-
	integer(A), !,
	IF is I-C*A.
linearize(A, C, As, As, CAs, CAs, I, IF, _) :-
	ground(A),
	catch( (B is eval(A)), _, fail ), !,
	IF is I-C*B.
linearize(C1*B, C, As, Bs, CAs, CBs, I, IF, Env) :-
	integer(C1), !,
	NC is C*C1,
	linearize(B, NC, As, Bs, CAs, CBs, I, IF, Env).
linearize(B*C1, C, As, Bs, CAs, CBs, I, IF, Env) :-
	integer(C1), !,
	NC is C*C1,
	linearize(B, NC, As, Bs, CAs, CBs, I, IF, Env).
linearize(AC, C, [A|Bs], Bs, [C|CBs], CBs, I, I, Env) :-
	arith(AC, _),
	equality(AC, V, Env),
	Env = _-Map,
	l(V, A, Map).

arith('/\\'(_,_), (/\)).
arith('\\/'(_,_), (\/)).
arith('=>'(_,_), (=>)).
arith('<=>'(_,_), (<=>)).
arith(xor(_,_), xor).
arith(abs(_), abs).
arith(min(_), min).
arith(max(_), max).
arith(min(_,_), min).
arith(max(_,_), max).
arith((_ * _), times).
arith((_ / _), div).
arith(sum(_), sum).
arith(sum(_,_), sum).
arith(count(_,_), count).

% replace abs(min(A,B)-max(A,B)) by
%    min(A,B,A1), max(A,B,A2), linear([1,-1],[A1,B1],=,A3), abs(A3,AN)
equality(V, V, _Env) :-
	var( V ), !.
equality(V, V, _Env) :-
	integer( V ), !.
equality(abs(V), NV, Env) :-
	equality(V, VA, Env),
	new_arith(abs, VA, NV, Env).
equality(min(V), NV, Env) :-
	maplist( equality_l(Env), V, VA ),
	new_arith(min, VA, NV, Env).
equality(max(V), NV, Env) :-
	maplist( equality_l(Env), V, VA ),
	new_arith(max, VA, NV, Env).
equality(V1+V2, NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( plus, V1A, V2A, NV, Env).
equality(V1-V2, NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( minus, V1A, V2A, NV, Env).
equality(V1*V2, NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( times, V1A, V2A, NV, Env).
equality(V1/V2, NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( div, V1A, V2A, NV, Env).
equality(V1 mod V2, NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (mod), V1A, V2A, NV, Env).
equality(max( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (max), V1A, V2A, NV, Env).
equality(min( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (min), V1A, V2A, NV, Env).
equality(sum( V ), NV, Env) :-
	maplist( equality_l(Env), V, VA ),
	new_arith(sum, VA, NV, Env).
equality(sum( C, G ), NV, Env) :-
	new_arith(sum, C, G, NV, Env).
equality('/\\'( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (/\), V1A, V2A, NV, Env).
equality('\\/'( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (\/), V1A, V2A, NV, Env).
equality('<=>'( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (<=>), V1A, V2A, NV, Env).
equality('=>'( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (=>), V1A, V2A, NV, Env).
equality('xor'( V1 , V2), NV, Env) :-
	equality(V1, V1A, Env),
	equality(V2, V2A, Env),
	new_arith( (xor), V1A, V2A, NV, Env).

equality_l(Env, V0, V) :-
	equality(V0, V, Env).

% abs(X) #= 3
out_c(Name, A1, B,  Op, Space-Map, Reify) :-
	integer(B), !,
	new_arith( Name, A1, NB, Space-Map),
	gecode_arith_op( Op, BOP ),
	l(NB, IB, Map),
	( var(Reify) ->
	    Space += rel(IB, BOP, B)
	;
	    Space += rel(IB, BOP, B, Reify)
	).
% abs(X) #= Cin[..]
out_c(Name, A1, B,  (#=), Space-Map, Reify) :-
	var(Reify),
	l(B, IB, Map), !,
	l(A1, IA1, Map),
	G =.. [Name, IA1, IB],
	Space += G.
% abs(X) #= NEW
out_c(Name, A1, B,  (#=), Space-Map, Reify) :-
	var(Reify), !,
	new_arith( Name, A1, B, Space-Map).
% abs(X) #> NEW
out_c(Name, A1, B,  Op, Space-Map, Reify) :-
	l(B, IB0, Map), !,
	new_arith( Name, A1, NB, Space-Map),
	l(NB, IB, Map),
	gecode_arith_op( Op, BOP ),
	(
	    nonvar(Reify) ->
	    Space += rel(IB, BOP, IB0)
	;
	    Space += rel(IB, BOP, IB0, Reify)
	).

% X*Y #= 3
out_c(Name, A1, A2, B, Op, Space-Map, Reify) :-
	integer(B), !,
	new_arith( Name, A1, A2, NB, Space-Map),
	l(NB, IB, Map),
	gecode_arith_op( Op, BOP ),
	( var(Reify) ->
	    Space += rel(IB, BOP, B)
	;
	    Space += rel(IB, BOP, B, Reify)
	).
% X*Y #= Cin[..]
out_c(Name, A1, A2, B,  (#=), Space-Map, Reify) :-
	var(Reify),
	l(B, IB, Map), !,
	l(A1, IA1, Map),
	l(A2, IA2, Map),
	G =.. [Name, IA1, IA2, IB],
	Space += G.
% abs(X) #= NEW, cannot be reified
out_c(Name, A1, A2, B,  (#=), Space-Map, Reify) :-
	var(Reify), !,
	new_arith( Name, A1, A2, B, Space-Map).
% min(X,Y) #= Cin[..] <=>
out_c(Name, A1, A2, B, Op, Space-Map, Reify) :-
	l(B, IB0, Map), !,
	new_arith( Name, A1, A2, NB, Space-Map),
	l(NB, IB, Map),
	gecode_arith_op( Op, BOP ),
	( var(Reify) ->
	    Space += rel(IB, BOP, IB0)
	;
	    Space += rel(IB, BOP, IB0, Reify)
	).

new_arith( abs, V, NV, Space-Map) :-
	l(V, X, Min0, Max0, Map),
	( Min0 < 0 ->
	    ( Max0 < 0 -> Min is -Max0, Max is -Min0 ;
		Min = 0 , Max is max( -Min0, Max0 ) )
	    ;
	    Min = Min0, Max = Max0
	),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += abs(X, NX).

new_arith( min, V, NV, Space-Map) :-
	V = [V1|RV],
	l(V1, _X1, Min0, Max0, Map),
	foldl2( min_l(Map), RV, Max0, Max, Min0, Min),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	maplist(ll(Map), V, X),
	Space += min(X, NX).

new_arith( max, V, NV, Space-Map) :-
	V = [V1|RV],
	l(V1, _X1, Min0, Max0, Map),
	foldl2( max_l(Map), RV, Max0, Max, Min0, Min),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	maplist(ll(Map), V, X),
	Space += min(X, NX).

new_arith( sum, V, NV, Space-Map) :-
	foldl2( sum_l(Map), V, 0, Max, 0, Min),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	maplist(ll(Map), V, X),
	Space += linear(X, 'IRT_EQ', NX).

new_arith( minus, V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	Min is Min1-Max2,
	Max is Max1-Min2,
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += linear([1,-1], [X1,X2], 'IRT_EQ', NX).

new_arith( plus, V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	Min is Min1+Min2,
	Max is Max1+Max2,
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += linear([1,1], [X1,X2], 'IRT_EQ', NX).

new_arith( min, V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	Min is min(Min1,Min2),
	Max is min(Max1,Max2),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += min(X1, X2, NX).

new_arith( max, V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	Min is max(Min1,Min2),
	Max is max(Max1,Max2),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += max(X1, X2, NX).

new_arith( times, V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	min_times(Min1,Min2,Max1,Max2,Min),
	max_times(Min1,Min2,Max1,Max2,Max),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += times(X1, X2, NX).

new_arith( (div), V1, V2, NV, Space-Map) :-
	l(V1, X1, Min1, Max1, Map),
	l(V2, X2, Min2, Max2, Map),
	min_div(Min1,Min2,Max1,Max2,Min),
	max_div(Min1,Min2,Max1,Max2,Max),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += div(X1, X2, NX).

new_arith( (mod), V1, V2, NV, Space-Map) :-
	l(V1, X1, _Min1, Max1, Map),
	l(V2, X2, _Min2, Max2, Map),
	Min is 0,
	Max is min(abs(Max1), Max2-1),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	Space += mod(X1, X2, NX).

new_arith( sum, Foreach, Cond, NV, Space-Map) :-
	cond2list( Foreach, Cond, Cs, V),
	foldl2( sum_l(Map), V, 0, Max, 0, Min),
	NX := intvar(Space, Min, Max),
	m(NV, NX, Min, Max, Map),
	maplist(ll(Map), V, X),
	Space += linear(Cs, X, 'IRT_EQ', NX).

new_arith( (/\), V1, V2, NV, Space-Map) :-
	l(V1, X1, Map),
	l(V2, X2, Map),
	NX := boolvar(Space),
	m(NV, NX, 0, 1, Map),
	Space += rel(X1, 'BOT_AND', X2, NX).

new_arith( (\/), V1, V2, NV, Space-Map) :-
	l(V1, X1, Map),
	l(V2, X2, Map),
	NX := boolvar(Space),
	m(NV, NX, 0, 1, Map),
	Space += rel(X1, 'BOT_OR', X2, NX).

new_arith( (=>), V1, V2, NV, Space-Map) :-
	l(V1, X1, Map),
	l(V2, X2, Map),
	NX := boolvar(Space),
	m(NV, NX, 0, 1, Map),
	Space += rel(X1, 'BOT_IMP', X2, NX).


new_arith( (<=>), V1, V2, NV, Space-Map) :-
	l(V1, X1, Map),
	l(V2, X2, Map),
	NX := boolvar(Space),
	m(NV, NX, 0, 1, Map),
	Space += rel(X1, 'BOT_EQV', X2, NX).

new_arith( xor, V1, V2, NV, Space-Map) :-
	l(V1, X1, Map),
	l(V2, X2, Map),
	NX := boolvar(Space),
	m(NV, NX, 0, 1, Map),
	Space += rel(X1, 'BOT_XOR', X2, NX).



min_times(Min1,Min2,Max1,Max2,Min) :-
	Min is min(Min1*Min2, min(Min1*Max2, min(Max1*Min2, Max1*Max2))).

max_times(Min1,Min2,Max1,Max2,Max) :-
	Max is max(Min1*Min2, max(Min1*Max2, max(Max1*Min2, Max1*Max2))).

min_div(Min1,Min20,Max1,Max20,Min) :-
	( Min20 == 0 -> Min2 = 1 ; Min2 = Min20),
	( Max20 == 0 -> Max2 = -1; Max2 = Max20),
	Min is min(Min1 div Min2, min(Min1 div Max2, min(Max1 div Min2, Max1 div Max2))).

max_div(Min1,Min20,Max1,Max20,Max) :-
	( Min20 == 0 -> Min2 = 1 ; Min2 = Min20),
	( Max20 == 0 -> Max2 = -1; Max2 = Max20),
	Max is max(Min1 div Min2, max(Min1 div Max2, max(Max1 div Min2, Max1 div Max2))).

min_l(Map, V, Min0, Min, Max0, Max) :-
	l(V, _, Min1, Max1, Map),
	Min is min(Min0, Min1),
	Max is min(Max0, Max1).

max_l(Map, V, Min0, Min, Max0, Max) :-
	l(V, _, Min1, Max1, Map),
	Min is max(Min0, Min1),
	Max is max(Max0, Max1).

sum_l(Map, V, Min0, Min, Max0, Max) :-
	l(V, _, Min1, Max1, Map),
	Min is Min0 + Min1,
	Max is Max0 + Max1.


in_c(A, A,  _y) :-
	var(A), !.
in_c(C, A, Space-Map) :-
	integer(C),
	Min is C-1,
	NX := intvar(Space, Min, C),
	m(A, NX, Min, C, Map),
	Space += rel(NX, 'IRT_EQ', C).

in_c_l(Env, V, IV) :-
	in_c(V, IV, Env).

user:term_expansion( ( H :- B), (H :- (gecode_clpfd:init_gecode(Space, Me), NB, gecode_clpfd:close_gecode(Space, Vs, Me)) ) ) :-
	process_constraints(B, NB, Env),
	term_variables(H, Vs),
	nonvar( Env ), !,
	Env = env( Space ).

init_gecode(Space, old) :-
	nb_current(gecode_space, Space), nonvar(Space), !.
init_gecode(Space-Map, new) :-
	Space := space,
	b_setval(gecode_done, false),
	b_setval(gecode_space, Space-Map).

close_gecode(_Space, _Vs, old) :- !.
close_gecode(Space-Map, Vs0, new) :-
	term_variables(Vs0, Vs),
	selectlist(intvar(Map), Vs, CVs),
	maplist(ll(Map), CVs, IVs),
	SolSpace := search(Space),
	b_setval(gecode_done, true),
	CVs := val(SolSpace,IVs).

intvar(Map, V) :-
	l(V, _IV, Map).

get_home(Home) :-
	b_getval(gecode_space, Home).

cond2list((List where Goal), El, Cs, Vs) :- !,
	foreach( List, add_el(Goal, El), ([])-([]), Cs-Vs ).
cond2list(List, El, Cs, Vs) :- !,
	foreach( List, add_el(true, El), ([])-([]), Cs-Vs ).

add_el(G0, El, Cs-Vs, [C|Cs]-[V|Vs]) :-
	call(G0), !,
	E <== El,
	( var(E) -> C = 1, E = V ; E = C*V, integer(C), var(V) -> true ; E = V*C, integer(C), var(V) ).
add_el(_G0, _El, Cs-Vs, Cs-Vs).

%       An attributed variable with attribute value Domain has been
%       assigned the value Y

attr_unify_hook(_, _) :-
	b_getval(gecode_done, true), !.
attr_unify_hook(v(IV1,_,_), Y) :-
        (   get_attr(Y, gecode_clpfd, v(IV2,_,_))
        ->
	    nb_getval(gecode_space, Space-_),
	    ( IV1 == IV2 -> true ;
	    Space += rel(IV1, 'IRT_EQ', IV2) )
        ;   var(Y)
        ->  true
        ;   integer(Y) ->
	    nb_getval(gecode_space, Space-_),
	    Space += rel(IV1, 'IRT_EQ', Y)
        ).

%       Translate attributes from this module to residual goals

attribute_goals(X) -->
        { get_attr(X, gecode_clpfd, v(_,A,B)) },
        [X in A..B].

m(X, Y, A, B, _Map) :-
	put_attr(X, gecode_clpfd, v(Y, A, B)).
/*
m(NV, OV, NA, NB, Vs) :-
	var(Vs), !,
	Vs = [v(NV,OV,NA,NB)|_].
m(NV, OV, NA, NB, [_|Vs]) :-
	m(NV, OV, NA, NB, Vs).
*/

lm(A, B, Map, X, Y) :-
	m(X, Y, A, B, Map).

l(V, IV, _) :-
	get_attr(V, gecode_clpfd, v(IV, _, _)).
/*
l(_NV, _OV, Vs) :-
	var(Vs), !,
	fail.
l(NV, OV, [v(V, OV, _A, _B)|_Vs]) :-
	V == NV, !.
l(NV, OV, [_|Vs]) :-
	l(NV, OV, Vs).
*/

ll(Map, X, Y) :-
	l(X, Y, Map).

l(V, IV, A, B, _) :-
	get_attr(V, gecode_clpfd, v(IV, A, B)).

/*
l(_NV, _OV, _, _, Vs) :-
	var(Vs), !,
	fail.
l(NV, OV, A, B, [v(V, OV, A, B)|_Vs]) :-
	V == NV, !.
l(NV, OV, A, B, [_|Vs]) :-
	l(NV, OV, A, B, Vs).
*/

is_one(1).

/**
@}
*/
