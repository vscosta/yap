/**
 * @file jupyter.yap
 *
 * @brief allow interaction between Jupyter and YAP.
 *
 * @long The code in here:
 * - establishes communication between Prolog and Python Streams
 * - inputs Prolog code and queries
 * - supports completion of Prolog programs.
 * -
 */

:- module(jupyter, [jupyter_query/3,
        ready/3,
		valid/3,
		errors/2]).

		:-	 use_module(library(yapi)).
		:-	 use_module(library(lists)).
		:-	 use_module(library(maplist)).
		:-	 use_module(library(python)).

		:- python_import(sys).

		:- dynamic user:portray_message/2.
		:- multifile user:portray_message/2.

jupyter_query(Self, Cell, Line ) :-
	setup_call_cleanup(
			   enter_cell(Self),
			   jupyter_cell(Self, Cell, Line),
			   exit_cell(Self)
	 	   ).

ready(_Self, Line ) :-
            blank( Line ),
            !.
ready(Self, Line ) :-
    errors( Self, Line ),
	\+ syntax_error(_,_).

errors( Self, Text ) :-
       	setup_call_cleanup(
       			   open_events( Self, Text, Stream),
       			   clauses(Self, Stream),
       			   close_events( Self )
       	 	   ).

clauses(Self, Stream) :-
    repeat,
    read_clause(Stream, Cl, [term_position(_Pos), syntax_errors(fail)] ),
	command( Self, Cl ),
    Cl == end_of_file,
    !.

command( _Self, ( :- op(Prio,Assoc,Name) ) ) :-
	addop(Prio,Assoc,Name).

command( _Self, ( :- module(Name, Exports) )) :-
	retract( active_module( M0 ) ),
	atom_concat( '__m0_', Name, M ),
	assert( active_module(M) ),
	assert( undo( active_module(M0) ) ),
	maplist( addop2(M), Exports).


addop(Prio,Assoc,Name) :-
	(
	current_op(OPrio, SimilarAssoc, Name),
	op(Prio, Assoc, Name),
	matched_op(Assoc, SimilarAssoc)
	->
		assertz( undo(op( OPrio, Assoc, Name ) ) )
		;
		assertz( undo(op( 0, Assoc, Name ) ) )
		).

addop2(M, op(Prio, Assoc, Name)) :-
	addop( Prio, Assoc, M:Name ).

matched_op(A, B) :-
	optype( A, T),
	optype( B, T).

optype(fx,pre).
optype(fy,pre).
optype(xfx,in).
optype(xfy,in).
optype(yfx,in).
optype(yfy,in).
optype(xf,pos).
optype(yf,pos).

:- dynamic user:portray_message/2.
:-  multifile user:portray_message/2.

:- dynamic syntax_error/2, undo/1.

open_events(Self, Text, Stream) :-
    Self.errors := [],
    open_mem_read_stream( Text, Stream ),
    assert((user:portray_message(_Severity, error(error(syntax_error(_),info(between(_,LN,_), _FileName, CharPos, _Details)))) :-
        assert( syntax_error(LN,CharPos) )
        )).

close_events( Self ) :-
	retract( undo(G) ),
	call(G),
	fail.
close_events( Self ) :-
    retract( syntax_error( L, N )),
    Self.errors := [t(L,N)] + Self.errors,
    fail.
close_events( _ ).

cell2pq( Cell, ``, ``) :-
	sub_string(Cell, 0, 2, _, `%%`),
	string_code(3, Cell, Code),
	code_type(Code, alpha),
	!.
cell2pq( Cell, P, Q, N) :-
	sub_string(Cell, 0, 1, _, `%`),
	string_codes(Cell, [Code|Codes]),
	code_type(Code, alpha),
	skip(10, Codes, Rest, LineF,Line1),
	skip_blanks(Rest, Body, Line1,Line0),
	reverse(Body, RBody),
	cell2pq2(RBody, Ps, Qs, N),
	extend(Ps, Qs, LineF, Line0, NPs, NQs),
	string_codes(P, NPs),
	string_codes(Q, NQs).
cell2pq( Cell, P, Q, N) :-
	string_codes(Cell, Codes),
	reverse(Codes, RCodes),
	cell2pq2(RCodes, NPs, NQs, N),
	string_codes(P, NPs),
	string_codes(Q, NQs).

%
% terminates with dot
%
cell2pq2(RCodes, NP, NQ, N) :-
	skip_allblanks( RCodes, [C|Rest], L1, L0),
	( C =:= "."
	->
	N = 1,
	RP = RCodes,
	RQ = ""
	;
	skip_to_blank_line( [C|Rest], RP, L0, []),
	RQ = L1,
	(
	C =:= "*"
	->
	N = -1
	;
	N=1
	)
	),
	reverse(RP,NP),
	reverse(RQ,NQ).

/**
 * @pred skip( Char, Input, Remainder, Begin, End)
 *
 * split the list according  to character _Char_:
 *
 * - _Remainder_ is what is after chars
 * - _Begin_-_End_ represents what is before char.
 *
 */
skip(_, "", "")  -->
	!,
	[].
skip(C, [C|Cs], Cs)  -->
	!,
	[C].
skip(C, [OC|Cs], Line)  -->
	[OC],
	skip(C,Cs, Line).

skip_to_blank_line("", "")  -->
	!.
skip_to_blank_line(Cs, Left)  -->
	blank_line(Cs, Left),
	!,
    [].
skip_to_blank_line(Cs, Line)  -->
	line(Cs, Line),
	!.

blank_line("", []) --> [].
blank_line([10|Cs], Cs) -->
	[10],
	!.
blank_line([C|Cs], Rest) -->
	{ code_type(C, white)},
	!,
	[C],
	blank_line(Cs, Rest).

line("", []) -->
	[].
line([10|Cs], Cs) -->
	[10],
	!.
line([C|Cs], Rest) -->
	[C],
	line(Cs,Rest).


jupyter_cell(_Self, Cell, _) :-
	% stop_low_level_trace,
	jupyter_consult(Cell),
	fail.
jupyter_cell( _Self, _, Line ) :-
	blank( Line ),
	!.
jupyter_cell( Self, _, Line ) :-
	% start_low_level_trace,
	python_query( Self, Line ).

jupyter_consult(Text) :-
	blank( Text ),
	!.
jupyter_consult(Cell) :-
	open_mem_read_stream( Cell, Stream),
	load_files(user:'jupyter cell',[stream(Stream)]).
	%should load_files  close?

blank(Text) :-
	atom_codes(Text, L),
	maplist( blankc, L).

blankc(' ').
blankc('\n').
blankc('\t').

enter_cell(_Self) :-
	%open('//python/sys.stdin', read, _Input, []),
	open('//python/sys.stdout', append, _Output, []),
	open('//python/sys.stdout', append, _Error, []),
	%set_prolog_flag(user_input, _Input),
	set_prolog_flag(user_output, _Output),
	set_prolog_flag(user_error, _Error).

exit_cell(_Self) :-
	%close( user_input),
	close( user_output),
	close( user_error).


completions(S, Self) :-
	open_mem_read_stream(S, St),
	scan_to_list(St, Tokens),
	close(St),
	reverse(Tokens, RTokens),
	strip_final_tokens(RTokens, MyTokens),
	setof( Completion, complete(MyTokens, Completion), Cs),
	Self.completions := Cs.


strip_final_tokens(['EOT'|Ts], Ts) :- !.
strip_final_tokens( Ts, Ts ).

complete([E,l,C,l,A|More],
	 isconsult(A),
	  %B = l,
	  library(C,Lib),
	  %D=l,
	  E=atom(Prefix),
	\+ arg( Rest ),
	check_library( Prefix, Lib, C).
complete([E,l,C,l,-,'['|More],
	 isconsult(A),
	  %B = l,
	  library(C,Lib),
	  %D=l,
	  E=atom(Prefix),
	\+ arg( Rest ),
	check_library( Prefix, Lib, C).
complete([C,l,A|More],
	 isconsult(A),
	  %B = l,
	  C=atom(Prefix),
	\+ arg( Rest ),
	file_or_library( Prefix, C).
complete([C,l,-,'['|More],
	 isconsult(A),
	  %B = l,
	  C=atom(Prefix),
	\+ arg( Rest ),
	file_or_library( Prefix, C).
complete( [atom(F)|Rest], C) :-
	\+ arg( Rest ),
	predicate( F, Pred, Arity ),
	cont( Arity, F, Pred, C).

isconsult( atom(use_module) ).
isconsult( atom(ensure_loaded) ).
isconsult( atom(compile) ).
isconsult( atom(consult) ).
isconsult( atom(reconsult) ).
isconsult( atom(load_files) ).
isconsult( '['   ).

arg([']'|_]).
arg([l|_]).

file_or_library(F,C) :-
	libsym(C0),
	atom_cooncat(F,C,C0).
file_or_library(F,C) :-
	check_file(F,C).

check_file(F0,C) :-
	atom_concat('\'',F,F0),
	!,
	absolute_file_name( F, FF, [access(none)]  ),
	atom_concat( FF, '*'	, Pat),
	absolute_file_name( Pat, C0, [glob(true)]  ),
	atom_concat(Pat,C00,C0),
	atom_conct(C00,'\'',C).
check_file(F0,C) :-
	atom_concat( F0, '*'	, Pat),
	absolute_file_name( Pat, C0, [glob(true)]  ),
	atom_concat(Pat,C,C0).

check_library( Lib, F, C) :-
	atom_concat( F, '*'	, Pat),
	LibF =.. [Lib(Pat)],
	absolute_file_name( LibF, Lib, [glob(true)]  ),
	file_directory_name( Lib, Name),
	( atom_concat(C, '.yap', Name) -> true ;
	 atom_concat(C, '.ypp', Name) -> true ;
	 atom_concat(C, '.prolog', Name) -> true
	).

predicate(N,P,A) :-
	system_predicate(P0/A),
	atom_concat(N,P,P0).
predicate(N,P,A) :-
	current_predicate(P0/A),
	atom_concat(N,P,P0).

cont(0, F, P, P0) :-
		atom_concat( F, P, P0 ).
cont( _, F, P, PB ):-
	atom_concat( [F, P, '('], PB ).
