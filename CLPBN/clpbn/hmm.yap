

:- module(hmm, [init_hmm/0,
		  hmm_state/1,
		  emission/4]).

:- ensure_loaded(library(clpbn)).

:- use_module(library(lists),
	      [nth/3]).

:- meta_predicate hmm_state(:).

:- dynamic hmm_tabled/1.

:- attribute emission/1.

:- ensure_loaded(library('clpbn/viterbi')).

init_hmm :-
	retractall(hmm_tabled(_)).

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
	build_args(A,LArgs,KArgs,Last),
	Key =.. [T|KArgs],
	Head =.. [N|LArgs],
	asserta_static( Mod:(Head :-
		(
		  hmm:hmm_tabled(Key)
		->
		  % leave work for solver!
		  %
%format('    ~w~n',[Key]),
		  Last = Key, !
%		  clpbn:put_atts(Last,[key(Key)]), !
		;
		  % first time we saw this entry
%format('+~w~n',[Key]),
%write(Key),nl,
%(Key = d(30,46) -> start_low_level_trace ; stop_low_level_trace),
		  assert(hmm:hmm_tabled(Key)), fail
		)
		)
	      ).
	
build_args(3,[A,B,C],[A,B],C).
build_args(2,[A,B],[A],B).

find_var(Key,Last) :-
	array_element(hmm_tree,1,Tree),
	lookup(Key, Tree, Last).


emission(Vals,CPT,Ev,V) :-
	cvt_vals(Vals,LVals),
	once(nth(Nth, LVals, Ev)),
	find_probs(CPT,Nth,Prob),
	put_atts(V,[emission(Prob)]).

cvt_vals(aminoacids,[a,  c,  d,  e,  f,  g,  h,  i,  k,  l,  m,  n,  p,  q,  r,  s,  t,  v,  w,  y]).
cvt_vals(bool,[t,f]).
cvt_vals(bases,[a,c,g,t]).
cvt_vals([A|B],[A|B]).

% first, try standard representation
find_probs(Logs,Nth,Log) :-
	arg(Nth,Logs,Log).



