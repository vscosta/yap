
:- use_module(library(lists)).
:- use_module(library(lineutils)).
:- use_module(library(system)).

:- initialization(main).

:- yap_flag(write_strings,on).

main :-
  S = popen('./get_comments'),
 file_filter_with_start_end( S, user_output, add2graph, initgraph,  checkgraph).

initgraph(_,_) :-
    retractall(node(_,_,_,_)),
    retractall(edge(_,_,_)),
    retractall(e_b_n(_,_,_)).

:- dynamic node/4, edge/3, e_b_n/3.

checkgraph(_,_) :-
  e_b_n(F,KN,_C),
  node(_,KF,KN,_),
  add_edge(KF, F, KN),
  fail.
checkgraph(_,_) :-
  listing(node),
  listing(edge).


add2graph(Line, _Out) :-
	writeln(Line),
    split( Line, ":@% \t*", [File, Job, Name|_]),
    append(Name, R, L),
    append(   _, L, Line),
    !,
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
  node(d,F0,K,_),
  !,
  throw( repeat(K:F0/F) ).
add_node(F, K,_C) :-
  retract(node(d,F1,K,_)),
  !,
    assert(edge(F,F1,K)),
  assert(node(d,F,K,C)).
add_node(F, K,C) :-
  assert(node(d,F,K,C)).

  add_node_edge(F, K,_C) :-
    node(_,F1,K,_),
    !,
    assert(edge(F1,F,K)).
  add_node_edge(F, K,C) :-
    assert(node(a,F,K,C)).

    add_edge(F, K,_C) :-
      node(_,F1,K,_),
      !,
      assert(edge(F1,F,K)).
    add_edge(F, K,C) :-
      assert(e_b_n(F,K,C)).
