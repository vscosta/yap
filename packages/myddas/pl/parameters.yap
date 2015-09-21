
user:term_expansion(Term,ExpandedTerm) :-
	Term = ( Head :- Body),
	prolog_load_context(module,Mod),
	extension( Mod:NameArity, Max, Names, L2, Call )
	hacks:context_variables(CurrentNames),
	sort( UnsortedCurrentNames, CurrentNames ),
	merge_variables( Names, CurrentNames ),
	ExpandedTerm = (M:Head :- Mod:NG).

merge_variables( [], _CurrentNames ).
merge_variables( _Names, [] ).
merge_variables( [S0=V0|Names], [S1=V1|CurrentNames] ) :-
	compare(Diff, S0, S1),
	(Diff == (=) -> V0 = V1, merge_variables( Names, CurrentNames ) ;
	 Diff == (>) -> merge_variables( [S0=V0|Names], CurrentNames ) ;
         /*Diff == (<) ->*/ merge_variables(Names, [S1=V1|CurrentNames] ) ).




(ModName extra_arguments CVs such_that CCs defaults CDs ) :-
	conj2l(CVs, Vs),
	conj2l(CCs, Cs),
	conj2l(CDs, Ds),
	hacks:context_variables(UnsortedNames),
        sort( UnsortedNames, Names ),	
	strip_module( ModName , Mod, NameArity ),
	functor( NameArity, Name, Arity),
	foldl2( max, Vs, Max, Arity, _, []),
	foldl( cons, Ds, Goals, []),
	foldl(ensure, Cs, L1, L0),
	functor( Head, Name, Arity ),
	foldl( argument, Vs, Head, L2, []),
	assert_static( extension( Mod:NameArity, Max, Names, L2) ).

max( Concept:_ = _, Max, Max, Seen, Seen) :-
	member(Concept, SeeenSoFar),
        !.
max( Concept:_ = _, Max1, Max, [Concept|Seen], Seen) :-
        !,
	Max1 is Max+1.
max( _ = _, Max1, Max, Seen, Seen) :-
        Max1 is Max+1.

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
satisfy(integer(X)) :- integer(X).
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
satisfy((X in D)) :-
        (
         D = [_|_] -> memberchk(X, D)
	;
	 D == X
        ).


ensure(( A ==> C)) :-
	satisfy(A) -> ensure(C).
ensure(domain(X,Vs)) :-
	(
	 var(X) -> throw(error(instantiation_error,_G))
	;
	 satisfy( member(X, Vs) ) -> true ; throw(error(domain_error(Vs,X),_G))
	).
ensure(atom(X)) :-
	(
	 var(X) -> throw(error(instantiation_error,_G))
	;
	 atom(X) -> true
	;
	 throw(error(type_error(atom,X),_G))
	).
ensure(integer(X)) :-
	(
	 var(X) -> throw(error(instantiation_error,_G))
	;
	 integer(X) -> true
	;
	 throw(error(type_error(integer,X),_G))
	).
ensure(internet_host(X)) :-
	(
	 var(X) -> throw(error(instantiation_error,_G))
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
	 throw(error(type_error(atom,X),_G))
	).
ensure(positive_or_zero_integer(X)) :-
	(
	 var(X) -> throw(error(instantiation_error,_G))
	;
	 \+ integer(X) -> throw(error(type_error(integer,X),_G))
	;
	 X < 0 -> throw(domain_error(not_less_than_zero,X),_G))
	;
	true
	).
ensure(file(X)) :-
	(
	 var(X) -> throw(error(instantiation_error,_G))
	;
	 atom(X) -> true
	;
	 throw(error(type_error(atom,X),_G))
	).
ensure((X in D)) :-
        (
         var(X) -> throw(error(instantiation_error,_G))
        ;
         D = [_|_] -> member(X, D)
	;
	 D == X -> true
        ;
         throw(error(domain_error(D,X),_G))
        ).
