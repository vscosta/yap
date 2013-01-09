%
% Convert HMMer model to CLP(BN) program
%
main :-
	open('hmmer_b.1.18.1.hmm', read, S),
	open('hmmer_b.1.18.1.yap', write, W),
%	open('globin.hmm', read, S),
	catch(parse_model(S,_),done(Info),stop(S,W,Info)),
	close(S),
	close(W).

stop(S,W,Info) :-
	ground(Info),
	gen_program(W, Info).
stop(_,_,_) :-
	format(user_error,"Bad HMM~n", []).

parse_model(S,Info) :-
	get_line(S, Line, Info),
%	format('~s~n',[Line]),
	match_field(Info, S, Line, []),
	parse_model(S, Info).

get_line(S, Out, Info) :-
	get0(S, C),
	(
	  C == 10 -> Out = [] ;
	  C == -1 -> throw(done(Info)) ;
	  Out = [C|Line], get_line(S, Line, Info)
	).


match_field(hmmer(2,_,_,_,_,_,_,_), _) --> "HMMER", !,
	scanner_skip.			% mandatory field, should be ground 
match_field(hmmer(_,Name,_,_,_,_,_,_),_) --> "NAME", !, 	% mandatory field 
	scanner_skip_blanks,
	get_name(String), {atom_codes(Name,String) }.
match_field(_,_) --> "ACC", !. % accession id, used to track DB accesses
match_field(hmmer(_,_,NOfStates,_,_,_,_,_),_) --> "LENG", !, % number of states
	scanner_skip_blanks,
	get_number(NOfStates, 0).
match_field(hmmer(_,_,_,Alph,_,_,_,_),_) --> "ALPH", !, % aminos or bases
	scanner_skip_blanks,
	check_alphabet(Alph).
match_field(_,_) --> "RF", !, scanner_skip.
match_field(_,_) --> "CS", !, scanner_skip.
match_field(hmmer(_,_,_,_,_,_,_,MAP),_) --> "MAP", !,
	scanner_skip_blanks,
	to_lower(Codes),
	{ map_code(Codes,MAP) }.
match_field(_,_) --> "COM", !, scanner_skip.
match_field(_,_) --> "CKSUM", !, scanner_skip.
match_field(_,_) --> "GA", !, scanner_skip.
match_field(_,_) --> "TC", !, scanner_skip.
match_field(_,_) --> "NC", !, scanner_skip.
match_field(_,_) --> "NSEQ", !, scanner_skip.
match_field(_,_) --> "DATE", !, scanner_skip.
match_field(hmmer(_,_,_,_,special(NB,NN,EC,EJ,CT,CC,JB,JJ,_,_),_,_,_),_) --> "XT", !,
	scan_transition(NB),
	scan_transition(NN),
	scan_transition(EC),
	scan_transition(EJ),
	scan_transition(CT),
	scan_transition(CC),
	scan_transition(JB),
	scan_transition(JJ).
match_field(hmmer(_,_,_,_,special(_,_,_,_,_,_,_,_,GG,GF),_,_,_),_) --> "NULT", !,
	scan_transition(GG),
	scan_transition(GF).
match_field(hmmer(_,_,_,Alph,_,NULE,_,_),_) --> "NULE", !,
	{ nof_symbols(Alph,N) },
	scan_transitions(N,Transitions),
	{ NULE =.. [null|Transitions] }.
match_field(_,_) --> "EVD", !,
	scanner_skip.			% optional, but should do later.x
match_field(Info,S) --> "HMM", !, 
	scanner_skip,
	{
	  get_line(S,_,Info),
	  Info = hmmer(_,_,NOfStates,Alph,_,_,model(BD,NBD,Transitions),MAP),
	  nof_symbols(Alph,N),
	  scan_model(S,NOfStates,N,BD,NBD,Transitions,MAP,Info),
	  throw(done(Info))
	}.

scan_model(S,NOfStates,N,BD,NBD,Transitions,MAP,Info) :-
	get_line(S,Line, Info),
	scan_bd(BD, NBD, Line, []),
	scan_states(NOfStates, N, S, MAP, Transitions, Info).

scan_states(0, _, _, MAP, [], _) :- !, close_map(MAP).
scan_states(NOfStates, N, Stream, MAP, [t(E,I,S)|Transitions], Info) :-
	NOfStates1 is NOfStates-1,
	scan_state(Stream, E, I, MAP, S, N, NMAP, Info),
	scan_states(NOfStates1, N, Stream, NMAP, Transitions, Info).

scan_state(Stream, E,I,MAP,s(MM,MI,MD,IM,II,DM,DD,BM,ME), N, NMAP, Info) :-
	get_line(Stream, ELine, Info),
	get_line(Stream, ILine, Info),
	get_line(Stream, SLine, Info),
%	format('~s~n~s~n~s~n',[ELine,ILine,SLine]),
	scanner_skip_field(ELine,Eline1),
	scan_transitions(N, E, Eline1, EMap),
	scan_map(MAP,NMAP,EMap, []),
	scanner_skip_field(ILine,ILine1),
	scan_transitions(N, I, ILine1, []),
	scanner_skip_field(SLine,SLine1),
	scan_transitions(MM, MI, MD, IM, II, DM, DD, BM, ME, SLine1, []).

scan_transitions(MM, MI, MD, IM, II, DM, DD, BM, ME) -->
	scan_transition(MM),
	scan_transition(MI),
	scan_transition(MD),
	scan_transition(IM),
	scan_transition(II),
	scan_transition(DM),
	scan_transition(DD),
	scan_transition(BM),
	scan_transition(ME).


scan_transitions(0, []) --> !.
scan_transitions(N, [T|Ts]) -->
	{ N1 is N-1 },
	scan_transition(T),
	scan_transitions(N1, Ts).

scan_transition(T) -->
	scanner_skip_blanks,
	get_transition(T).

get_transition('*') --> "*", !, scanner_skip_blanks.
get_transition(N) --> [C],
	{ C >= 0'0, C =< 0'9, !, C0 is C-0'0 },
	get_number(N, C0 ).
get_transition(N) --> "-",
	get_number(C0, 0 ),
	{ N is -C0}.

get_number(Nf, N0) --> [C], !,
	( { C >= 0'0, C =< 0'9 } -> { Ni is N0*10+(C-0'0) }, get_number(Nf,Ni) ;
	    { Nf = N0 }
	).
get_number(N, N) --> [].

scanner_skip_blanks --> " ", !, scanner_skip_blanks.
scanner_skip_blanks --> "	", !, scanner_skip_blanks.
scanner_skip_blanks --> [].


scanner_skip_field --> scan_transition(_).

scan_bd(BD, NBD) -->
	scan_transition(BD),
	scanner_skip_field,
	scan_transition(NBD).

check_alphabet(Alph) -->
	to_lower(Lower),
	{ get_alph(Lower, Alph) }.

to_lower([CF|Lower]) --> [C], !,
	{ ( C >= 0'A, C =< 0'Z -> CF is C+(0'a-0'A) ; CF = C) },
	to_lower(Lower).
to_lower([]) --> [].

get_alph("amino", amino).
get_alph("nucleic", nucleic).

map_code("yes", yes(_)).
map_code("no", no).

nof_symbols(amino,20).
nof_symbols(nucleic,4).

scanner_skip(_,_).

get_name(L,L,[]).

scan_map(yes([Id|Next]),yes(Next)) -->
	scan_transition(Id).
scan_map(no,no) --> [].

close_map(yes([])) :- !.
close_map(no).


gen_program(W, hmmer(VersionId,Name,NOfStates,Alphabet,SpecialTransitions,NULE,Model,MAP)) :-
	format(W, '~n% HMMer Version ~d (Plan 7) using ~a~n',[VersionId,Alphabet]),
	format(W, '~n% Name: ~a~n',[Name]),
	format(W, '~n% Size: ~d~n',[NOfStates]),
	format(W, 'slices(~d).~n',[NOfStates]),
	gen_specials(W, SpecialTransitions),
	gen_nule(W, NULE,Alphabet,NULEProbs),
	gen_model(W, Model,NULEProbs),
	gen_map(W, MAP).


%
% special nodes in graph.
%
gen_specials(W, special(NB,NN,EC,EJ,CT,CC,JB,JJ,GG,GF)) :-
	% reaching state N
	normalize(NN,1.0,NNCPT),
	format(W, '~n%Reaching state N.~n',[]),
	format(W, 'n_n_cpt(~w,1.0,~w).~n',[NN,NNCPT]),
	% reaching state B
	normalize(JB,1.0,JBCPT),
	format(W, '~n%Reaching state B.~n',[]),
	format(W, 'j_b_cpt(~w,1.0,~w).~n',[JB,JBCPT]),
	normalize(NB,1.0,NBCPT),
	format(W, 'n_b_cpt(~w,1.0,~w).~n',[NB,NBCPT]),
	% reaching state J
	format(W, '~n%Reaching state J.~n',[]),
	normalize(EJ,1.0,EJCPT),
	format(W, 'e_j_cpt(~w,1.0,~w).~n',[EJ,EJCPT]),
	normalize(JJ,1.0,JJCPT),
	format(W, 'j_j_cpt(~w,1.0,~w).~n',[JJ,JJCPT]),
	% reaching state C
	format(W, '~n%Reaching state C.~n',[]),
	normalize(EC,1.0,ECCPT),
	format(W, 'e_c_cpt(~w,1.0,~w).~n',[EC,ECCPT]),
	normalize(CC,1.0,CCCPT),
	format(W, 'c_c_cpt(~w,1.0,~w).~n',[CC,CCCPT]),
	% reaching state T
	format(W, '~n%Reaching state T.~n',[]),
	normalize(CT,1.0,CTCPT),
	format(W, 'c_t_cpt(~w,1.0,~w).~n',[CT,CTCPT]),
	% null model
	format(W, '~n%Reaching state G (Null Model).~n',[]),
	normalize(GG,1.0,GGCPT),
	format(W, 'g_g_cpt(~w,1.0,~w).~n',[GG,GGCPT]),
	format(W, '~n%Reaching state F (Null Model).~n',[]),
	normalize(GF,1.0,GFCPT),
	format(W, 'g_f_cpt(~w,1.0,~w).~n',[GF,GFCPT]).


normalize(*,_,0.0) :- !.
normalize(Score,NULL,Prob) :-
	Prob is NULL * exp(2.0, (Score/1000)).

normalizel([],_,[]).
normalizel([Score|Scores],NULL,[Prob|Probs]) :-
	normalize(Score, NULL, Prob),
	normalizel(Scores,NULL,Probs).

normalizell([],[],[]).
normalizell([Score|Scores],[Norm|Norms],[Prob|Probs]) :-
	normalize(Score, Norm, Prob),
	normalizell(Scores,Norms,Probs).

% null emission CPT
gen_nule(W, NULE,Alph,PsCPT) :-
	NULE =.. [_|Ps],
	nof_symbols(Alph,Size),
	Norm is 1/Size,
	normalizel(Ps,Norm,PsCPT),
	E0s =.. [e|Ps],
	Es =.. [e|PsCPT],
	format(W, '~n%Null state emission CPT.~n',[]),
	format(W, 'nule_cpt(~n     ~w,~n     ~w,~n     ~w).~n',[E0s,Norm,Es]).

gen_model(W, model(BD,NBD,States),PsCPT) :-
	normalize_bd(BD,NBD,BDCPT),
	format(W, '~n%Reaching first D.~n',[]),
	format(W, 'b_d_cpt(~w,~w,~w).~n',[BD,NBD,BDCPT]),
	gen_states(W, States,1,PsCPT).

gen_states(_, [],_,_).
gen_states(W, [State|States],StateNo,PsCPT) :-
	gen_state(W, State,StateNo,PsCPT),
	NStateNo is StateNo+1,
	gen_states(W, States,NStateNo,PsCPT).

gen_state(W, t(E,I,s(MM,MI,MD,IM,II,DM,DD,BM,ME)),StateNo,PsCPT) :-
	format(W, '~n%State ~d.~n',[StateNo]),
	Norm =.. [e|PsCPT],
	normalizell(E,PsCPT,ECPT),
	E0s =.. [e|E],
	Es =.. [e|ECPT],
	find_consensus(W,StateNo,ECPT),
	sum(ECPT,0,TE),
	format(W, 'me_cpt(~d,~n    ~w,~n    ~w,~n    ~w). % ~w~n',[StateNo,E0s,Norm,Es,TE]),
	normalizell(I,PsCPT,ICPT),
	I0s =.. [e|ICPT],
	Is =.. [e|ICPT],
	sum(ICPT,0,TI),
	format(W, 'ie_cpt(~d,~n    ~w,~n    ~w,~n    ~w). %~w~n',[StateNo,I0s,Norm,Is,TI]),
	normalize(MM,1.0,MMCPT),
	format(W, 'm_m_cpt(~d,~w,~w,~w).~n',[StateNo,MM,1.0,MMCPT]),
	normalize(MI,1.0,MICPT),
	format(W, 'm_i_cpt(~d,~w,~w,~w).~n',[StateNo,MI,1.0,MICPT]),
	normalize(MD,1.0,MDCPT),
	format(W, 'm_d_cpt(~d,~w,~w,~w).~n',[StateNo,MD,1.0,MDCPT]),
	normalize(II,1.0,IICPT),
	format(W, 'i_i_cpt(~d,~w,~w,~w).~n',[StateNo,II,1.0,IICPT]),
	normalize(IM,1.0,IMCPT),
	format(W, 'i_m_cpt(~d,~w,~w,~w).~n',[StateNo,IM,1.0,IMCPT]),
	normalize(DM,1.0,DMCPT),
	format(W, 'd_m_cpt(~d,~w,~w,~w).~n',[StateNo,DM,1.0,DMCPT]),
	normalize(DD,1.0,DDCPT),
	format(W, 'd_d_cpt(~d,~w,~w,~w).~n',[StateNo,DD,1.0,DDCPT]),
	normalize(BM,1.0,BMCPT),
	format(W, 'b_m_cpt(~d,~w,~w,~w).~n',[StateNo,BM,1.0,BMCPT]),
	normalize(ME,1.0,MECPT),
	format(W, 'm_e_cpt(~d,~w,~w,~w).~n',[StateNo,ME,1.0,MECPT]).

gen_map(_,_).

normalize_bd(A,B,A).

sum([],S,S).
sum([P|Ps],S0,S) :-
	Si is S0+P,
	sum(Ps,Si,S).

find_consensus(W, StateNo,ECPT) :-
	max_index(ECPT,1,0,-1,_,MaxIndex),
	format(W, 'consensus(~d,~d).~n',[StateNo,MaxIndex]).

max_index([],_,Max,MaxIndex,Max,MaxIndex).
max_index([H|L],I0,Max0,MaxIndex0,Max,MaxIndex) :-
	H > Max0, !,
	I is I0+1,
	max_index(L,I,H,I0,Max,MaxIndex).
max_index([_|L],I0,Max0,MaxIndex0,Max,MaxIndex) :-
	I is I0+1,
	max_index(L,I,Max0,MaxIndex0,Max,MaxIndex).

