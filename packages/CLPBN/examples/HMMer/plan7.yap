


:- ensure_loaded(library(clpbn)).

:- ensure_loaded(library('clpbn/hmm')).

:- hmm_state((m/3, i/3, d/3, t/2, b/2, n/2, j/2, e/2, s/2, c/2)).

/*

We represent a plan7 HMMer as a recursive program. There are two parameters:
 i represents position on a string
 j slice in the HMMer: probability distributions are different for each slice.

An HMM has 10 states (M, I, D are the core states):
	S -> begin
	N -> before match
	B -> begin a match
	M -> match state
	I -> insertion
	D -> deletion
	E -> end of match
	C -> continuation after matches done
	T -> end of sequence
	J -> go back to match start. 

S, B, E, and T do not emit.

Each state will be represented as a binary random variable.

Also, you'll see terms of the form

	{ S = m(I) with p([t,f], trans([MMCPT,IMCPT,DMCPT]), [M0,I0,D0]) }.

the sum function is as examplified:

	P(S=t) = P(MMCPT|M0)P(M0=t)+P(IMCPT|M0)P(I0=t)+P(IDCPT|M0)P(D0=t)
	P(S=f) = 1-P(S=t)

With sum a single element may be true so if
	k1\=k2, P(A_k1=t,A_k2=t) = 0.

*/

% now this is our nice  DBN: notice that CPTs depend on slide,
% so this is really an "irregular"  DBN.
% first, the emission probabilities (they are easier ;-).

% we look at the core first: m, and i emissions

% next, go to inner states (state transitions).

% the first m-state

m(I,J,M) :-
	slices(J), !,
	I1 is I+1,
	e(I1,E),
	{ M = m(I,J) with p(bool, trans([0]),[E]) },
	emitting(M).
% standard m-state
m(I,J,M) :-
	I1 is I+1,
	J1 is J+1,
	i(I1,J,NI),
	m(I1,J1,NM),
	d(I1,J1,ND),
	e(I1,NE),
	m_i_cpt(J,MICPT),
	m_m_cpt(J,MMCPT),
	m_d_cpt(J,MDCPT),
	m_e_cpt(J,MECPT),
	{ M = m(I,J) with p(bool, trans([MICPT,MMCPT,MDCPT,MECPT]),[NI,NM,ND,NE]) },
	emitting(M).

i(I,J,S) :-
	I1 is I+1,
	J1 is J+1,
	m(I1,J1,M),
	i(I1,J,IS),
	i_i_cpt(J,IICPT),
	i_m_cpt(J,IMCPT),
	{ S = i(I,J) with p(bool, trans([IMCPT,IICPT]), [M,IS]) },
	emitting(S).

d(I,J,D) :-
	slices(J), !,
	e(I,E),
	{ D = d(I,J) with p(bool, trans([0]), [E]) }.
d(I,J,S) :-
	J1 is J+1,
	m(I,J1,M),
	d(I,J1,ND),
	m_d_cpt(J,MDCPT),
	d_d_cpt(J,DDCPT),
	{ S = d(I,J) with p(bool, trans([MDCPT,DDCPT]), [M,ND]) }.

e_evidence([],_).
e_evidence([Emission|Es],Emission) :-
	e_evidence(Es,Emission).

%
% N, C, and J states can also emit.
%
% and they have transitions.

% initial state
s(0,S) :-
	n(0,N),
	{ S = s(0) with p(bool, trans([0]),[N]) }.

n(I,S) :-
	I1 is I+1,
	b(I1, B0),
	n(I1, N0),
	n_n_cpt(NNCPT),
	n_b_cpt(NBCPT),
	{ S = n(I) with p(bool, trans([NBCPT,NNCPT]), [B0,N0]) },
	emitting(S).

b(I,S) :-
	slices(Ss),
	b_m_transitions(0,Ss,I,Ms,MCPTs),
	d(I,1, D),
	b_d_cpt(BMCPT),
	{ S = b(I) with p(bool, trans([BMCPT|MCPTs]), [D|Ms]) }.

b_m_transitions(Ss,Ss,_,[],[]) :- !.
b_m_transitions(J0,Ss,I,[M|Ms],[CPT|MCPTs]) :-
	J is J0+1,
	m(I,J,M),
	b_m_cpt(J,CPT),
	b_m_transitions(J,Ss,I,Ms,MCPTs).

j(I,S) :-
	I1 is I+1,
	b(I1, NB),
	j(I1, NJ),
	j_b_cpt(JBCPT),
	j_j_cpt(JJCPT),
	{ S = j(I) with p(bool, trans([JBCPT,JJCPT]), [NB,NJ]) },
	emitting(S).

e(I,S) :-
	c(I, NC),
	j(I, NJ),
	e_c_cpt(ECCPT),
	e_j_cpt(EJCPT),
	{ S = e(I) with p(bool, trans([ECCPT,EJCPT]), [NC,NJ]) }.

c(I,S) :-
	I1 is I+1,
	t(I1, T),
	c(I1, NC),
	c_t_cpt(CTCPT),
	c_c_cpt(CCCPT),
	{ S = c(I) with p(bool, trans([CCCPT,CTCPT]),[NC,T]) },
	emitting(S).

	
t(I,S) :-
	% I < IMax
	{ S = t(I) with p(bool, trans([]), []) }.


% the item I at slice J is a random variable P.
emitting(M) :-
	emission(M).

emission_cpt(Key, CPT) :-
	Key=..[A,_,Slice], !,
	emission_cpt(A, Slice, CPT).
emission_cpt(_, CPT) :-
	nule_cpt(CPT).

emission_cpt(m,J,CPT) :- !, me_cpt(J,CPT).
emission_cpt(i,J,CPT) :- !, ie_cpt(J,CPT).
emission_cpt(_,_,CPT) :- nule_cpt(CPT).


ie_cpt(I,Logs) :- ie_cpt(I,Logs,_,_).
me_cpt(I,Logs) :- me_cpt(I,Logs,_,_).
nule_cpt(Logs) :- nule_cpt(Logs,_,_).
b_m_cpt(I,Log) :- b_m_cpt(I,Log,_,_).
b_d_cpt(Log) :- b_d_cpt(Log,_,_).
c_c_cpt(Log) :- c_c_cpt(Log,_,_).
c_t_cpt(Log) :- c_t_cpt(Log,_,_).
d_d_cpt(I,Log) :- d_d_cpt(I,Log,_,_).
d_m_cpt(I,Log) :- d_m_cpt(I,Log,_,_).
e_c_cpt(Log) :- e_c_cpt(Log,_,_).
e_j_cpt(Log) :- e_j_cpt(Log,_,_).
i_i_cpt(I,Log) :- i_i_cpt(I,Log,_,_).
i_m_cpt(I,Log) :- i_m_cpt(I,Log,_,_).
j_b_cpt(Log) :- j_b_cpt(Log,_,_).
j_j_cpt(Log) :- j_j_cpt(Log,_,_).
m_d_cpt(I,Log) :- m_d_cpt(I,Log,_,_).
m_e_cpt(I,Log) :- m_e_cpt(I,Log,_,_).
m_i_cpt(I,Log) :- m_i_cpt(I,Log,_,_).
m_m_cpt(I,Log) :- m_m_cpt(I,Log,_,_).
n_b_cpt(Log) :- n_b_cpt(Log,_,_).
n_n_cpt(Log) :- n_n_cpt(Log,_,_).

%hmm_domain([a,  c,  d,  e,  f,  g,  h,  i,  k,  l,  m,  n,  p,  q,  r,  s,  t,  v,  w,  y]).

hmm_domain(aminoacids).

