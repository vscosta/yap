
:- module(hmm,
		[init_hmm/0,
		 hmm_state/1,
		 emission/1
		]).

:- ensure_loaded(library(clpbn)).

:- use_module(library(atts)).

:- use_module(library(lists),
		[nth/3]).

:- use_module(library(bhash),
		[b_hash_new/2,
		 b_hash_lookup/3,
		 b_hash_insert/3
		]).

:- ensure_loaded(library(tries)).

:- meta_predicate hmm_state(:).

:- dynamic hmm_tabled/1.

:- attribute emission/1.

:- ensure_loaded(library('clpbn/viterbi')).

init_hmm :-
%	retractall(hmm_tabled(_)).
%	eraseall(hmm_tabled).
%	nb_hash_new(hmm_table, 1000000).
	trie_open(Trie), nb_setval(trie,Trie).

hmm_state(Mod:A) :- !, hmm_state(A,Mod).
hmm_state(A) :- prolog_flag(typein_module,Mod), hmm_state(A,Mod).

hmm_state(Mod:N/A,_) :- !,
	hmm_state(N/A,Mod).
hmm_state((A,B),Mod) :- !,
	hmm_state(A,Mod),
	hmm_state(B,Mod).
hmm_state(N/A,Mod) :-
	atom_codes(N,[TC|_]),
	atom_codes(T,[TC]),
	build_args(A,LArgs,KArgs,First,Last),
	Key =.. [T|KArgs],
	Head =.. [N|LArgs],
	asserta_static( (Mod:Head :-
	  (First > 2 ->
	    Last = Key, !
	  ;
	    nb_getval(trie, Trie), trie_check_entry(Trie, Key, _) ->
	      % leave work for solver!
	      Last = Key, !
	    ;
	      % first time we saw this entry
	      nb_getval(trie, Trie), trie_put_entry(Trie, Key, _),
	      fail
	  )
	)).

build_args(4,[A,B,C,D],[A,B,C],A,D).
build_args(3,  [A,B,C],  [A,B],A,C).
build_args(2,    [A,B],    [A],A,B).

emission(V) :-
	put_atts(V,[emission(Prob)]).

cvt_vals(aminoacids,[a,  c,  d,  e,  f,  g,  h,  i,  k,  l,  m,  n,  p,  q,  r,  s,  t,  v,  w,  y]).
cvt_vals(bool,[t,f]).
cvt_vals(dna,[a,c,g,t]).
cvt_vals(rna,[a,c,g,u]).
cvt_vals([A|B],[A|B]).

% first, try standard representation
find_probs(Logs,Nth,Log) :-
	arg(Nth,Logs,Log).

