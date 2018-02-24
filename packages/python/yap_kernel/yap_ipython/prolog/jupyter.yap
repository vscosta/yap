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

 %% :- module( jupyter,
 %%            [jupyter_query/3,
 %%            errors/2,
 %%            ready/2,
 %%            completion/2,

 %%            ]
 %%            ).


:-	 reexport(library(yapi)).
:-	 use_module(library(lists)).
:-	 use_module(library(maplist)).
:-	 use_module(library(python)).

:- python_import(sys).

jupyter_query(Self, Cell, Line ) :-
        setup_call_cleanup(
			   enter_cell(Self),
			  jupyter_cell(Self, Cell, Line),
			   exit_cell(Self)
	 	   ).

jupyter_cell(_Self, Cell, _) :-
	jupyter_consult(Cell),
	fail.
jupyter_cell( _Self, _, Line ) :-
	blank( Line ),
	!.
jupyter_cell( _Self, _, [] ) :- !.
jupyter_cell( Self, _, Line ) :-
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
	open('//python/input', read, Input, []),
	open('//python/sys.stdout', append, Output, []),
	open('//python/sys.stdout', append, Error, []),
	set_prolog_flag(user_input, Input),
	set_prolog_flag(user_output, Output),
	set_prolog_flag(user_error, Error).

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
	Self.matches := Cs.


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

command(_, end_of_file) :- !.

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

:- dynamic syntax_error/4, undo/1.

user:portray_message(_Severity, error(syntax_error(Cause),info(between(_,LN,_), _FileName, CharPos, Details))) :-
	nb_getval(jupyter_cell, on),
        assert( syntax_error(Cause,LN,CharPos,Details) ).
user:portray_message(_Severity, error(style_check(_),_) ) :-
	nb_getval(jupyter_cell, on).
      
open_events(Self, Text, Stream) :-
	Self.errors := [],
	nb_setval( jupyter, on),
    open_mem_read_stream( Text, Stream ).

:- initialization( nb_setval( jupyter, off ) ).
		     
close_events( _Self ) :-
	nb_setval( jupyter, off ),
	retract( undo(G) ),
	call(G),
	fail.
close_events( Self ) :-
	retract( syntax_error( C, L, N, A )),
    Self.errors := [t(C,L,N,A)] + Self.errors,
    fail.
close_events( _ ).

:- ( start_low_level_trace ).
