
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(rbtrees)).
:- ensure_loaded(library(tries)).
:- ensure_loaded(library(trie_sp)).
:- ensure_loaded(library(bdd)).
:- ensure_loaded(library(bhash)).
:- ensure_loaded(library(nb)).

%:- [inter].

:- dynamic best/4.

%:- ['../AlephUW/vsc_aleph_extensions'].
%vsc_check_mem(on).

:- ensure_loaded(library(dbusage)).


graph2bdd(Query,1,bdd(D,T,Vs)) :-
    Query =.. [_,X,Y],
    !,
    retractall(best(_,_,_,_)),
    graph(X,Y, TrieList, Vs),
    bdd_new(TrieList, C),
    bdd_tree(C, BDD),
    BDD = bdd(D,T,_Vs0).

:- set_problog_flag(init_method,(Q,N,Bdd,user:graph2bdd(Q,N,Bdd))).



%:- leash(0), spy graph2bdd.

cvt_to_id([E0,E1], VId*true, [Id-VId]) :-
	      problog:problog_dir_edge(Id,E0,E1,_Pr),
	      !.
cvt_to_id([E0,E1],VId*true, [Id-VId]) :-
    problog:problog_dir_edge(Id,E1,E0,_Pr),
    !.
cvt_to_id([E0,E1|Es], VId*Ids, [Id-VId|VIds]) :-
   problog:problog_dir_edge(Id,E0,E1,_Pr),
    !,
    cvt_to_id([E1|Es],Ids, VIds).
cvt_to_id([E0,E1|Es],  VId*Ids, [Id-VId|VIds]) :-
   problog:problog_dir_edge(Id,E1,E0,_Pr),
    !,
    cvt_to_id([E1|Es], Ids, VIds).

export_answer(Final, FinalIDs, Vs) :-
    cvt_to_id(Final,FinalIDs, Vs).
    %writeln(FinalIDs),


graph(X,Y,Trie_Completed_Proofs,Vs) :-
    best(X,Y,_Pr,Final),
    %writeln(_Pr),
    !,
    export_answer([Y|Final], Trie_Completed_Proofs,Vs).
graph(X,Y,Trie_Completed_Proofs, Vs) :-
    nb_heap(100000,Q),
    path(X,Y,X,[X],Final, 0, _Pr, Q),
    !,
    export_answer(Final, Trie_Completed_Proofs, Vs).
graph(_X,_Y,Trie_Completed_Proofs,Vs) :-
    export_answer([], Trie_Completed_Proofs,Vs).

path(X,X,_,P,P,Pr,Pr,_Q).
path(X,Y,X0,P,_,Pr0,_Pr,Q) :-
    X \= Y,
    edge(X,Z,PrD),
    absent(Z,P),
    Pr is Pr0+PrD,
    check_best(X0, Z, Pr, P),
    NPr is -Pr,
    nb_heap_add(Q,NPr,[Z|P]),
    %	nb_heap_size(Q,S), S mod 10000 =:= 0, gc_heap(Q), writeln(S),
    fail.
path(_,Y,X0,_,F,_,FPr,Q) :-
    nb_heap_del(Q,NPr,P),
    P=[Z|_],
    %	b_getval(problog_threshold, LT),
    Prf is -NPr,
%	Prf >= LT,
    path(Z,Y,X0,P,F,Prf,FPr,Q).

check_best(X, Z, _Pr, _P) :-
    best(X, Z, _Pr1, _P0),
    !,
%	Pr1 >= Pr, !,
	fail.
check_best(X, Z, Pr, P) :-
    retract(best(X, Z,_, _)),
    !,
    assert(best(X, Z,Pr,P)).
check_best(X, Z, Pr, P) :-
    assert(best(X, Z,Pr,P)).

d([H|L],H,L).
d([H|L], X, [H|Nl]) :-
    d(L,X,Nl).

% using directed edges in both directions
edge(X,Y,Pr) :- problog:problog_dir_edge(_,Y,X,Pr).
edge(X,Y,Pr) :- problog:problog_dir_edge(_,X,Y,Pr).

% checking whether node hasn't been visited before
absent(_,[]).
absent(X,[Y|Z]):- X \= Y, absent(X,Z).

% get rid of garbage elements
gc_heap(Q) :-
	heap_all(Q, [], L),
	sort(L, S),
	rebuild(S, Q),
	nb_heap_size(Q,Sz), writeln(done:Sz).


heap_all(Q, L, L) :-
	nb_heap_empty(Q), !.
heap_all(Q, Els, L) :-
	nb_heap_del(Q, Key, Val),
	Val = p(_,Z,_),
	heap_all(Q, f(Z,Key,Val).Els, L).

rebuild([], _).
rebuild([f(Z,Pr0,_), f(Z,NPr,V)|Zs], Q) :- Pr0 < NPr, !,
	rebuild([f(Z,NPr,V)|Zs], Q).
rebuild([f(_,NPr,V)|Els], Q) :-
	nb_heap_add(Q, NPr, V),
	rebuild(Els, Q).

