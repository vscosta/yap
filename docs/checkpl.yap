
:- use_module(library(lists)).
:- use_module(library(lineutils)).

:- initialization(main).

main :-
  %system('find . \( -name '*.pl' -o -name '*.yap'  -o -name '*.c' -o -name '*.h' -o -name '*.cpp' -o -name '*.hh' \) -type f -print | xargs grep  '@defgroup\|@ingroup\|@addtogroup\|@{|@}').

 file_filter_with_start_end( docs, tmp, add2graph, initgraph,  checkgraph).

initgraph(_,_).

:- dynamic node/3, edge/3, e_b_n/3.

checkgraph(_,_) :-
  e_b_n(F,KN,_C),
  node(KF,KN,_),
  add_edge(KF, F, KN),
  fail.
checkgraph(_,_) :-
  listing(node),
  listing(edge).


 add2graph(Line, _Out) :-
    split( Line, "% \t/*:", [File, Job, Name|_]),
    append(   _, L, Line),
    append(Name, R, L),
    dispatch( Name, File, Job, R).

dispatch( Name, File, "defgroup", Comment):-
    atom_codes(KN, Name),
    atom_codes(KF, File),
    atom_codes(C, Comment),
          add_node(KF, KN,C).
dispatch( Name, File, "addtogroup", Comment):-
  atom_codes(KN, Name),
  atom_codes(KF, File),
  atom_codes(C, Comment),
  add_node_edge(KF, KN,C).
dispatch( Name, File, "ingroup", Comment):-
    atom_codes(KN, Name),
    atom_codes(KF, File),
    atom_codes(C, Comment),
          add_edge(KF, KN,C).
add_node(F, K,_C) :-
  node(_,K,_),
  throw( repeat(F:K) ).
add_node(F, K,C) :-
  assert(node(F,K,C)).

  add_node_edge(F, K,_C) :-
    node(F1,K,_),
    !,
    assert(edge(F1,F,K)).
  add_node_edge(F, K,C) :-
    assert(node(F,K,C)).

    add_edge(F, K,_C) :-
      node(F1,K,_),
      !,
      assert(edge(F1,F,K)).
    add_edge(F, K,C) :-
      assert(e_b_n(F,K,C)).
