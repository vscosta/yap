/**
 * @file   parameters.yap
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP.lan>
 * @date   Tue Nov 17 23:34:04 2015
 * 
 * @brief  Experimental test generation code. 
 * 
 * 
*/

:- module( parameters,
	   [such_that/2,
	    op(1060, xfx, such_that),
            op(1050, xfx, extra_arguments),
            op(1050, xfx, defaults),
            op(700, xfx, in),
            op(700, xfx, ?=),	%initialization
	    op(750, xfy, #==>),
	    op(500, yfx, '<=>'),
            op(500, yfx, '=>'),
	    op(800, yfx, '==>') ]
         ).




/**
  * @defgroup parameters Automating test generation
  * @ingroup library
  * @{

  This library aims at facilitating test generation in a logic
  program, namely when interfacing to foreign code. It introduces the
  following specs:

  - Named Arguments: you can refer to an argument through a name. This
  allows having optional arguments. As an example, you can say:

  maxlength=MaxLength

  and the original program is extended to support an optional
  parameter Length, eg:

  vector( Type, Length, V ) :- alloc( Type, Length, MaxLength, V )

  will become

  vector( Type, Length, V, MaxLength ) :- alloc( Type, Length, MaxLength, V )

  - Tests

  You can write type tests and ISO-like tests offline.

  - Initializers

  It allows default code for variables. In the previous code, the
  initializer MaxLength = 1024 would result in two clauses:

  vector( Type, Length, V, MaxLength ) :-
          alloc( Type, Length, MaxLength, V ).
  vector( Type, Length, V ) :-
          alloc( Type, Length, 1024, V ).

  See myddas.yap for a few examples.

  @author Vitor Santos Costa
*/



:- use_module( library(clauses),
           [list2conj/2,
            conj2list/2]).

:- use_module( library(maplist) ).

:- use_module( library(rbtrees) ).

:- use_module( library(lists) ).

:- use_module( library(bdd) ).

:- dynamic extension/4, init/2, frame/2, exclusive/0.

user:term_expansion(Term,Clauses) :-
	Term = ( Spec :- Body),
	prolog_load_context(module,Mod),
	extension( Mod:Spec,  Tests, GoalVars, Names),
	hacks:context_variables(UnsortedCurrentNames),
 	sort( UnsortedCurrentNames, CurrentNames ),
	merge_variables( Names, CurrentNames ),
	findall( (Mod:ExtHead :- ExtBody),
		 expand( Spec, Names, GoalVars, Body, Tests, (ExtHead :- ExtBody)),
		 Clauses ).

find_name( [Name=V0|_] , V, Name) :- V=V0, !.
find_name( [_|UnsortedCurrentNames] , V, Name) :-
	find_name( UnsortedCurrentNames , V, Name).

expand( Skel, Names, GoalVars, Body, Tests, Out) :-
        Skel =.. [N|As],		%
	%pick(Vs, As, Os),
	append(As, GoalVars, Os),
	Head =.. [N|Os],
	maplist(original_name(GoalVars), Names, Ts),
	LinkGoal =.. [access|Ts],
	trace,
	formula( Tests, Fs, Dic),
	bdd_new(Fs , BDD),
	bdd_print( BDD, '/Users/vsc/bdd.dot', Names),
	bdd_tree(BDD, Tree),
	ptree(Tree, Names, Dic),
%	portray_clause((Head:-GExtBody)),
	unnumbervars((Head:- LinkGoal,Body), Out).

swap_f(Key-V, Key=V).

ptree( bdd(Root,L,_Vs) , Names, File, Dic) :-
				%	term_variables(L, LVs),
%	Vs =.. [_|LVs],
	maplist( bindv,Names),
	rb_visit(Dic, Pairs),
	maplist( bindv,Pairs),
	absolute_file_name( File, [], AbsFile ),
	open(AbsFile, write, S) ,
	format(S,'digraph "DD" {
              size = "7.5,10"
              center = true;~n', []),
	format(S,' "~w" [label = "~w"];~n', [1, Root]),
	maplist( print_node(S), L),
	format(S, '}~n', []),
	close(S),
	fail.
ptree(_, _, _).

bindv( X = '$VAR'(X) ) :- !.
bindv( X - '$VAR'(X) ) :- !.
bindv(_).

print_node(S,pp( Val, Name, Left, Right )) :-
	%writeln(Name),
	simplify(Name, N),
	format(S,' "~w" [label = "~w"];~n', [Val, N]),
	format(S,' "~w" -> "~w" [arrowType="none" color="red"] ;~n', [Val, Left]),
	format(S,' "~w" -> "~w"  [style = dashed arrowType="none"];~n', [Val, Right]).
print_node(S,pn( Val, Name, Left, Right )) :-
	simplify(Name, N),
	%writeln(Name),
        format(S,' "~w" [label = "~w"];~n', [Val, N]),
        format(S,' "~w" -> "~w" [arrowType="none" color="red"];~n', [Val,Left]),
	( Right == 1 ->
	  format(S,' "~w" -> "0"  [dir=none style = dotted];~n', [Val])
	;
	  format(S,' "~w" -> "~w"  [style = dotted type="odot"];~n', [Val, Right])
	).

simplify(V,V) :- var(V),!.
simplify('$VAR'(X),Y) :- !, simplify(X,Y).
simplify(c^(X),Y) :- !, simplify(X,Y).
simplify(G, X:M) :- G=.. [X,N], !, simplify(N,M).
simplify(X, X).


/*
pick([LastV,LastV1|More], As, OVs) :-
	nonvar(LastV),
  LastV = (ID:_Name=V),
	nonvar(LastV1),
        LastV1 = (ID: _Name1=_V1), !,
	(
	 OVs = [V|NVs],
         skip_pick(  More, ID, Rest ),
	 pick_all(Rest, As, NVs)
	;
	 pick( LastV1, As, OVs)
	).
pick([Pair|More], As, OVs) :-
	nonvar(Pair),
	( Pair = (_:N = V) -> false ; Pair = (N = V) ),
	( OVs = [V|NVs],
	  pick_all( More, As, NVs );
	  pick( More, As, OVs )
	).
pick([], As, As).

pick_all([LastV,LastV1|More], As, OVs) :-
        nonvar(LastV),
        LastV = (ID:_Name=V),
        nonvar(LastV1),
        LastV1 = (ID: _Name1=_V1), !,
        (
         OVs = [V|NVs],
         skip_pick(  More, ID, Rest ),
         pick_all(Rest, As, NVs)
        ;
         pick_all( LastV1, As, OVs )
        ).
pick_all([Pair|More], As, [V|NVs]) :-
        nonvar(Pair),
        ( Pair = (_:_ = V) -> false ; Pair = (_ = V) ),
	pick_all(More, As, NVs).
pick_all([], As, As).

skip_pick([El|More], Id, Left ) :-
	nonvar(El),
	El = ( Id:_=_ ),
        !,
  skip_pick(More, Id, Left ).
skip_pick(More, _Id, More ).
*/

pick(Els,_As,Names) :-
	maplist(fetch_var,Els, Names).

fetch_var(V, V) :- var(V), !.
fetch_var(_:_=V, V) :- var(V), !.
fetch_var(_=V, V) :- var(V), !.

original_name(_,V,V) :- var(V), !.
original_name(HVs,_ID:Name=V,V) :- vmemberck(V, HVs), !, V='$VAR'(Name).
original_name(HVs,Name=V,V) :- vmemberchk(V, HVs), !,V='$VAR'(Name).
original_name(_HVs,_,_V).


vmemberchk(V,[V0|_]) :- V == V0, !.
vmemberchk(V,[_V0|Vs]) :-
	vmemberchk(V, Vs).


/* Several cases: the test is
   - undecided, we don't know the result yet
   ... -> b($VAR)
   -> satisfeito, pode ser simplificado
   atomic(2) ==> G ==> G
   atomic(2) ==> true
   -> falso, pode ser removido
   F ==> global fail
   F -> ..  ==> true
*/
new_test(Test, B, OUT ) :-
	pre_evaluate( Test, O ),
	!,
	( O == true -> OUT = B ;
	  O == false -> warning( inconsistent_test( Test ) ) ;
          O \= false -> OUT = ( O , B ) ).
%% false just fail.
new_test(Test, B, (Test, B) ) :-
 	warning( unimplemented_test( Test ) ).

%pre_evaluate( G, _) :- writeln(G), fail.
pre_evaluate( V?=C, true ) :- var(V), !, V = C.
pre_evaluate( _V?=_C, true ).
pre_evaluate( V=C, true ) :- nonvar(V), V \= '$VAR'(_), !, V = C.
pre_evaluate( V=_C, false ) :- var(V), !.
pre_evaluate( V=C, V=C ).
pre_evaluate( var(V), true ) :- var(V), !.
pre_evaluate( var(T), false ) :- T \= '$VAR'(_), !.
pre_evaluate( var(T), var(T) ) :- !.
pre_evaluate( A*B, O ) :-
	pre_evaluate( A, O1),
	pre_evaluate( B, O2),
	(O1 == false -> O = false ;
	 O1 == true -> O = O2 ;
	 O2 == false -> O = false ;
	 O2 == true -> O = O1 ;
	 O = O1*O2 ).
pre_evaluate( A+B, O ) :-
        pre_evaluate( A, O1),
        pre_evaluate( B, O2),
        (O1 == true -> O = true ;
         O1 == false -> O = O2 ;
         O2 == true -> O = true ;
         O2 == false -> O = O1 ;
         O = O1+O2 ).
pre_evaluate( G, O ) :-
	type_domain_goal( G, Parameter ), !,
	( var( Parameter ) -> O = false ;
	  Parameter = '$VAR'(_) -> O = G ;
	  call( G ) -> O = true ; O = false).
pre_evaluate( ( G1 ==> G2), O ) :-
	pre_evaluate(G1, O1 ),
        ( O1 = false -> O = true ;
	  pre_evaluate( G2, O2 ),
	  ( O1 == true -> O = O2 ;
	    O2 == false ->
	    ( O1 = false -> O = true
	    ; O = ( O1 -> error(O2) ; true ) ) ;
            O2 == true -> O = true ;
	    O = (  O1 -> ( O2 ; false ) ; true ) ) ).
pre_evaluate( c^G, G ).
pre_evaluate( ( G1 #==> G2), O ) :-
        pre_evaluate(G1, O1 ),
        ( O1 = false -> O = true ;
	  O1 = true -> O = G2 ;
	  O = (  O1 ->  G2 ; true   ) ).

type_domain_goal( nonvar(Parameter), Parameter).
type_domain_goal( atom(Parameter), Parameter).
type_domain_goal( atomic(Parameter), Parameter).
type_domain_goal( number(Parameter), Parameter).
type_domain_goal( float(Parameter), Parameter).
type_domain_goal( nonvar(Parameter), Parameter).
type_domain_goal( compound(Parameter), Parameter).
type_domain_goal( is_list(Parameter), Parameter).
type_domain_goal( integer(Parameter), Parameter).
type_domain_goal( internet_host(Parameter), Parameter).
type_domain_goal( positive_or_zero_integer(Parameter), Parameter).
type_domain_goal( file(Parameter), Parameter).
type_domain_goal( Parameter = A, Parameter) :- atomic(A), !.
type_domain_goal( A = Parameter, Parameter) :- atomic(A).
type_domain_goal( Parameter in _ , Parameter).

new_init( _, _ = G, B, B ) :- var(G), !.
new_init( _, _ = G, B, B ) :- G \= '$VAR'(_), !.
new_init( Vs, _ = Val, (Var = Val, B), B ) :-
	member( Var = Val, Vs ), !.


merge_variables( [], _CurrentNames ).
merge_variables( _Names, [] ).
merge_variables( [S0=V0|Names], [S1=V1|CurrentNames] ) :-
	compare(Diff, S0, S1),
	(Diff == (=) -> V0 = V1, merge_variables( Names, CurrentNames ) ;
	 Diff == (>) -> merge_variables( [S0=V0|Names], CurrentNames ) ;
         /*Diff == (<) ->*/ merge_variables(Names, [S1=V1|CurrentNames] ) ).




(ModName extra_arguments CVs such_that CCs ) :-
	term_variables(CVs, Vs),
	conj2list(CCs, Cs),
	hacks:context_variables(Names),
	strip_module( ModName , Mod, NameArity ),
	assert( extension( Mod:NameArity, Cs, Vs, Names) ).

cons(G, cons(G)).

cons(G) :-
	ground(G), !.
cons(G) :-
	call(G), !.
cons(_).

satisfy(( A ==> C)) :-
	satisfy(A) -> satisfy(C).
satisfy(domain(X,Vs)) :-
      memberchk(X, Vs).
satisfy(atom(X)) :-
	atom(X).
satisfy(integer(X)) :-
        integer(X).
satisfy(atom(X)) :-
        atom(X).
satisfy(internet_host(X)) :-
       atom(X) -> true
      ;
      X = ipv4(I1,I2,I3,I4),
       integer(I1),
       integer(I2),
       integer(I3),
       integer(I4).
satisfy(positive_or_zero_integer(X)) :-
       integer(X),
       X >= 0.
satisfy(file(X)) :-
	 atom(X).
satisfy(c^G) :-
        call(G).
satisfy((X in D)) :-
        (
         D = [_|_] -> memberchk(X, D)
	;
	 D = X
        ).


ensure(( A ==> C)) :-
	satisfy(A) -> ensure(C).
ensure(domain(X,Vs)) :-
	(
	 var(X) -> error(instantiation_error)
	;
	 satisfy( member(X, Vs) ) -> true ; error(domain_error(Vs,X))
	).
ensure(atom(X)) :-
	(
	 var(X) -> error(instantiation_error)
	;
	 atom(X) -> true
	;
	 error(type_error(atom,X))
	).
ensure(integer(X)) :-
	(
	 var(X) -> error(instantiation_error)
	;
	 integer(X) -> true
	;
	 error(type_error(integer,X))
	).
ensure(internet_host(X)) :-
	(
	 var(X) -> error(instantiation_error)
	;
	 atom(X) -> true
	;
	 X = ipv4(I1,I2,I3,I4),
	 integer(I1),
	 integer(I2),
	 integer(I3),
	 integer(I4)
	->
	 true
	;
	 error(type_error(atom,X))
	).
ensure(positive_or_zero_integer(X)) :-
	(
	 var(X) -> error(instantiation_error)
	;
	 \+ integer(X) -> error(type_error(integer,X))
	;
	 X < 0 -> throw(domain_error(not_less_than_zero,X))
	;
	true
	).
ensure(file(X)) :-
	(
	 var(X) -> error(instantiation_error)
	;
	 atom(X) -> true
	;
	 error(type_error(atom,X))
	).
ensure((X in D)) :-
        (
         var(X) -> error(instantiation_error)
        ;
         D = [_|_] -> member(X, D)
	;
	 D == X -> true
        ;
         error(domain_error(D,X))
        ).


formula( Axioms, FormulaE, Dic) :-
	rb_new( Dic0 ),
	partition( is_frame, Axioms, _, Goals),
	foldl2( eq, Goals, Formula, Dic0, Dic, [], Extras),
	append(Formula, Extras, FormulaL),
	list2prod( FormulaL, FormulaE).

is_frame( A =:= B ) :- assert( frame(A, B)).
is_frame( level(N, [H|L]) ) :- !, maplist( assertn(level, N), [H|L] ).
is_frame( level(N, L ) ) :- assert( level( N, L) ).

assertn(level, N, L) :- assert( level( N, L) ).

list2prod( [], true).
list2prod( [F], F).
list2prod( [F1,F2|Fs], F1*NF) :-
	list2prod( [F2|Fs], NF).

%eq(G,_,_,_,_,_) :- writeln(a:G), fail.
eq(1, 1, Dic,  Dic, I, I) :-  !.
eq(X, VX, Dic0,  Dic, I0, I) :- var(X), !,
	add(X, VX, Dic0, Dic, I0, I).
eq(X == Exp, (-TA + TY)*(-TY + TA), Dic0, Dic, I0, I) :- !,
	eq(X, TA, Dic0, Dic1, I0, I1),
	eq(Exp, TY, Dic1, Dic, I1, I).

eq((X ==> Y), (-TX + TY), Dic0, Dic, I0, I) :- !,
	eq( X, TX, Dic0, Dic1, I0, I1),
	eq( Y, TY, Dic1, Dic, I1, I).

eq((X :- Y), (TX + -TY), Dic0, Dic, I0, I) :- !,
eq( X, TX, Dic0, Dic1, I0, I1),
eq( Y, TY, Dic1, Dic, I1, I).

eq((X + Y), (TX + TY), Dic0, Dic, I0, I) :- !,
	eq( X, TX, Dic0, Dic1, I0, I1),
	eq( Y, TY, Dic1, Dic, I1, I).

eq((X * Y), (TX * TY), Dic0, Dic, I0, I) :- !,
	eq( X, TX, Dic0, Dic1, I0, I1),
	eq( Y, TY, Dic1, Dic, I1, I).

eq(-X,  -TX, Dic0, Dic, I0, I) :- !,
	eq( X, TX, Dic0, Dic, I0, I).

eq((X xor Y), (TX xor TY), Dic0, Dic, I0, I) :- !,
	eq( X, TX, Dic0, Dic1, I0, I1),
	eq( Y, TY, Dic1, Dic, I1, I).

eq(X in D, VX = (TAX + (-TAX * (EDX+ (-EDX * Ds ))))  , Dic0, Dic, I0, I) :- !,
	eq( t_atom(X), TAX, Dic0, Dic1, I0, I1),
	add( err(dom(X,D)), EDX, Dic1, Dic2, I1, I2),
	add(X, VX, Dic2, Dic3, I2, I3),
	t_domain( D, X, VX, Ds, Dic3, Dic, I3, I).

eq(one_of(D), Ds, Dic0, Dic, I0, I) :-
	!,
	t_domain0( D, Ds, Dic0, Dic, I0, I).

eq(G, NG, Dic0, Dic, I0, I) :-
        add( G, NG, Dic0, Dic, I0, I).

add_xors(L, V, I0, I) :-
	foldl(add_xor(V), L, I0, I).

add_xor(V, V0, I, I) :- V == V0, !.
add_xor(V, V0, I, [(V-V0)|I]).

xor(  VX, DV0s, DV , Disj0, Disj0+Conj) :- !,
        foldl( add_all2(VX, DV), DV0s, 1,Conj).

add_all2(VX, G, GD, C, C*(VX=G)
	) :- G == GD, ! .
add_all2(VX, _, G, C, C*(-(VX=G))).

list2prod(X, P, X *P).
list2sum(X, P, X +P).

t_domain0( [D], DX, Dic0, Dic, I0, I) :- !,
	eq(D , DX , Dic0, Dic, I0, I).
t_domain0( [D1|D2s], (DX1+ (-DX1*D2Xs)), Dic0, Dic, I0, I) :-
	eq(D1, DX1, Dic0, Dic1, I0, I1),
        t_domain0(D2s, D2Xs, Dic1, Dic, I1, I).

t_domain( [D], X, _VX, VDX, Dic0, Dic, I0, I) :- !,
	add( X=D, VDX, Dic0, Dic, I0, I).
t_domain( [D1|D2s], X, VX, VDX + (-VDX*D2S), Dic0, Dic, I0, I) :-
	add( X=D1, VDX, Dic0, Dic1, I0, I1),
	t_domain(D2s, X, VX, D2S, Dic1, Dic, I1, I ).

t_domain0( [D], VDX, Dic0, Dic, I0, I) :-
	!,
	add( D, VDX, Dic0, Dic, I0, I).
t_domain0( [D1|D2s], VDX + (-VDX*D2S), Dic0, Dic, I0, I) :-
        add( D1, VDX, Dic0, Dic1, I0, I1),
        t_domain0(D2s, D2S, Dic1, Dic, I1, I ).

add(AG, V, Dic, Dic, I, I) :-
	rb_lookup( AG, V, Dic),  !.
add( AG, V, Dic0, Dic, I0, IF) :-
	frame(AG, Body), !,
	rb_insert( Dic0, AG, V, Dic1),
	eq(AG==Body, O, Dic1, Dic, [O|I0], IF).
add( AG, V, Dic0, Dic, I, I) :-
	rb_insert( Dic0, AG, V, Dic).

simp_key(G , G) :- var(G), !.
simp_key(_^_:error(_^G) , G) :- !.
simp_key(_^_:G , G) :- !.
simp_key('$VAR'(S):A, SAG) :-
	atom(S),
	atom(A), !,
	SAG =.. [A, S].
simp_key(V:error(E), error(V,E)) :- !.
simp_key(AG, AG).


all_diff(L, Cs) :-
	all_pairs(L, [], Ps),
	foldl( pair2cs, Ps, true, Cs).

all_pairs([X,Y|L], E0, E) :-
	all_pairs([X|L], [[X|Y]|E0], E1),
	all_pairs([Y|L], E1, E).
all_pairs([_X], E, E).

pair2cs([X|Y],P,P*(X-> -Y) * (Y -> -X)).

lor(A, B, A+B).

atom(AA, VD, CS, (VD->AA)*CS).
