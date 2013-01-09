%
% Preliminary support for FASTA format.
% Just convert sequence into string of atoms.
%
% support for a single sequence.
%

:- module(fasta,
		[fa2atoms/2,
		 fa2atoms/3
		]).

fa2atoms(F, L) :-
	fa2atoms(F, L, []).

fa2atoms(F,L,L0) :-
	open(F,read,S),
	get0(S,C0),
	skip_header(C0,S),
	get0(S,C),
	read_chars(C, S, L, L0),
	close(S).

read_chars(-1,_) --> [].
read_chars(10,S) --> !,
	{ get0(S,MC) },
	read_chars(MC,S).
read_chars(C,S) -->
	[AC],
	{
	  cvt_c(C,AC),
	  get0(S,MC)
	},
	read_chars(MC, S).

cvt_c(C,A) :-
	C >= 0'A, C =< 0'Z, !,
	AC is C-(0'A-0'a),
	atom_codes(A,[AC]).
cvt_c(C,A) :-
	C >= 0'a, C =< 0'z,
	atom_codes(A,[C]).

skip_header(10,_) :- !.
skip_header(_,S) :-
	get0(S,C),
	skip_header(C,S).


