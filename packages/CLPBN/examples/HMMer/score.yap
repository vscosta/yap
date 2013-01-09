#!/usr/bin/yap -L --
#.

:- initialization(main).

:- ensure_loaded(library('clpbn/viterbi')).

:- use_module(fasta,
		[fa2atoms/3]).

:- use_module(library(lists),
		[nth/3,
		 append/3
		]).

:- [plan7].

:- ['globin.yap'].
%:- ['hmmer_b.1.18.1.yap'].

:- dynamic len/1, emission/2.

main :-
	unix(argv([Sequence])),
	fa2atoms(Sequence, L, [a]),
	run_model([a|L],_Trace).

artemia :-
	fa2atoms('Artemia.fa', L, [a]),
	run_model([a|L],_Trace).

% given a string, construct a model
run_model(String,Trace) :-
	viterbi(s(0,_),t(_,_),String,Trace),
	out(Trace,String).

write_keys([]).
write_keys([V|Vars0]) :-
	clpbn:get_atts(V,[key(K)]), !,
	write(K), nl,
	write_keys(Vars0).
write_keys([_|Vars0]) :-
	write(no_key),nl,
	write_keys(Vars0).


convert2emissions([],_).
convert2emissions([E|String],I0) :-
	I is I0+1,
	assert(emission(I,E)),
	convert2emissions(String, I).


out([],_).
out([State|Trace],String) :-
	out_state(State,String,RString),
	out(Trace,RString).

out_state(s(_),String,String).
out_state(n(_),[_|String],String).
out_state(j(_),[_|String],String).
out_state(c(_),[_|String],String).
out_state(t(_),String,String).
out_state(b(Pos),String,String) :-
	format('START MATCH at ~d~n', [Pos]).
out_state(d(_,State),String,String) :-
	find_consensus(State,Consensus),
	format('     -~a~n', [Consensus]).
out_state(i(_,_),[Code|String],String) :-
	to_upper(Code,UCode),
	format('    ~a+~n', [UCode]).
out_state(m(_,State),[Code|String],String) :-
	to_upper(Code,UCode),
	find_consensus(State,Consensus),
	find_match_type(State,Code,Consensus,MatchType),
	format('    ~a~a~a~n', [UCode,MatchType,Consensus]).
out_state(e(Pos),String,String) :-
	format('END MATCH at ~d~n', [Pos]).

to_upper(A,U) :-
	atom_codes(A,[C]),
	( C =< "A", C>="Z" ->
	  U = A
	;
	  NC is C + ("A"-"a"),
	  atom_codes(U,[NC])
	).
find_consensus(State,Consensus) :-
	consensus(State,I),
	hmm_domain(Type),
	domain_vals(Type,Vals),
	arg(I, Vals, Consensus).

find_match_type(_,Code,Code,'=') :- !.
find_match_type(State,Code,_,'^') :-
	me_cpt(State,Probs,_,_),
	hmm_domain(Type),
	domain_vals(Type,Vals),
	find_prob_for_code(Vals,Code,Probs,Prob),
	Prob >= 0, !.
find_match_type(_,_,_,' ').

find_prob_for_code(Vals,Code,Probs,Prob) :-
	Vals =.. [_|L],
	once(nth(I,L,Code)),
	arg(I,Probs,Prob).

domain_vals(aminoacids,domain(a,  c,  d,  e,  f,  g,  h,  i,  k,  l,  m,  n,  p,  q,  r,  s,  t,  v,  w,  y)).
domain_vals(bool,domain(t,f)).
domain_vals(bases,domain(a,c,g,t)).
domain_vals([A|B],Domain) :-
	Domain =.. [domain,A|B].


start(S) :-
	s_state(0,S).

end(Items,E) :-
	t_state(Items,E).

