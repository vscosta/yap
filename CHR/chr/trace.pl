%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Constraint Handling Rules			      version 2.2 %
%								  %
%  (c) Copyright 1998						  %
%  LMU, Muenchen						  %
%								  %
%  File:   trace.pl						  %
%  Author: Christian Holzbaur		christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

   2 Mechanisms: trace+leash, debug+spy


   Debugger integration issue:

     We could use conditional spypoints of the Prolog debugger
     to get hooked, but then we depend a lot on it ...

     :- spypoint_condition( debug_event(E), P, chr:de(P,E)).

   Todo:

     -) module_wrap/3 for all terms (M as arg to debug_event)

     -) guard-fail = rule-delay reason?

*/

:- dynamic spy_rule/2.
:- dynamic spy_constraint/2.

:- initialization
	getval( debug, _) -> true ; setval( debug, off).
:- initialization
	retractall( spy_rule(_,_)).
:- initialization
	retractall( spy_constraint(_,_)).

chr_trace :-
	setval( debug, trace),
	what_is_on( informational).

chr_notrace :-
	setval( debug, off),
	what_is_on( informational).

chr_debug :-
	setval( debug, debug),
	what_is_on( informational).

chr_nodebug :-
	chr_notrace.

chr_spy( constraints(Cs)) :-
	parse_spy_constraints( Cs, L, []),
	member( N/A, L),
	assert( spy_constraint(N,A)),
	fail.
chr_spy( rules(Rs)) :-
	parse_spy_rules( Rs, L, []),
	member( Handler:Rule, L),
	assert( spy_rule(Rule,Handler)),
	fail.
chr_spy( _) :- getval( debug, trace), !.
chr_spy( _) :- chr_debug.

chr_nospy( rules(Rs)) :-
	parse_spy_rules( Rs, L1, []),
	member( Handler:Rule, L1),
	retract( spy_rule(Rule,Handler)),
	fail.
chr_nospy( constraints(Cs)) :-
	parse_spy_constraints( Cs, L, []),
	member( N/A, L),
	retract( spy_constraint(N,A)),
	fail.
chr_nospy( _).

parse_spy_constraints( C) --> {var(C)}, !, [ _ ].
parse_spy_constraints( (C,Cs)) -->
	parse_spy_constraints( C),
	parse_spy_constraints( Cs).
parse_spy_constraints( N) --> {atom(N)}, [ N/_ ].
parse_spy_constraints( N/A) --> {atom(N),integer(A),A>0}, [ N/A ].

parse_spy_rules( R) --> {var(R)}, !, [ _ ].
parse_spy_rules( (R,Rs)) --> !,
	parse_spy_rules( R),
	parse_spy_rules( Rs).
parse_spy_rules( H:R) --> !, [ H:R ].
parse_spy_rules( R) --> [ _:R ].		% any handler

chr_leash( Spec) :-
	nonvar( Spec),
	chr_leash( Spec, I),
	setval( leashing, I),
	what_is_leashed( informational).

chr_leash( none,    0) :- !.
chr_leash( off,     0) :- !.
chr_leash( all,    -1) :- !.
chr_leash( default, I) :- !, chr_leash( 0, I, [call,wake,apply,exit,fail], []).
chr_leash( L,	    I) :- chr_leash( 0, I, L, []), !.
chr_leash( X,	    I) :- chr_leash( 0, I, [X], []).

chr_leash( I, K) --> [call],   {J is I\/2'100000000}, chr_leash( J, K).
chr_leash( I, K) --> [wake],   {J is I\/2'010000000}, chr_leash( J, K).
chr_leash( I, K) --> [try],    {J is I\/2'001000000}, chr_leash( J, K).
chr_leash( I, K) --> [apply],  {J is I\/2'000100000}, chr_leash( J, K).
chr_leash( I, K) --> [exit],   {J is I\/2'000010000}, chr_leash( J, K).
chr_leash( I, K) --> [redo],   {J is I\/2'000001000}, chr_leash( J, K).
chr_leash( I, K) --> [fail],   {J is I\/2'000000100}, chr_leash( J, K).
%
chr_leash( I, K) --> [insert], {J is I\/2'000000010}, chr_leash( J, K).
chr_leash( I, K) --> [remove], {J is I\/2'000000001}, chr_leash( J, K).
chr_leash( I, I) --> [].

:- initialization
	chr_leash( default, I), setval( leashing, I).

debug_stop( call(S), L, Why) :-
	( L/\2'100000000 > 0 -> true
	; spypoint_susp( S, Why)
	).
debug_stop( wake(S), L, Why) :-
	( L/\2'010000000 > 0 -> true
	; spypoint_susp( S, Why)
	).
debug_stop( exit(S), L, Why) :-
	( L/\2'000010000 > 0 -> true
	; spypoint_susp( S, Why)
	).
debug_stop( redo(S), L, Why) :-
	( L/\2'000001000 > 0 -> true
	; spypoint_susp( S, Why)
	).
debug_stop( fail(S), L, Why) :-
	( L/\2'000000100 > 0 -> true
	; spypoint_susp( S, Why)
	).
%
debug_stop( insert(S), L, Why) :-
	( L/\2'000000010 > 0 -> true
	; spypoint_susp( S, Why)
	).
debug_stop( remove(S), L, Why) :-
	( L/\2'000000001 > 0 -> true
	; spypoint_susp( S, Why)
	).
%
debug_stop( try(H,R,_,Hs,_,_), L, Why)	 :-
	( L/\2'001000000 > 0 -> true
	; spy_rule(R,H) -> Why = r
	; spypoint_head( Hs, Why)
	).
debug_stop( apply(H,R,_,Hs,_,_), L, Why) :-
	( L/\2'000100000 > 0 -> true
	; spy_rule(R,H) -> Why = r
	; spypoint_head( Hs, Why)
	).

spypoint_susp( S, c) :-
	S =.. [suspension,_,_,_,_,_,N|Args],
	length( Args, A),
	spy_constraint( N, A).

spypoint_head( Hs, c) :-
	member( H, Hs),
	arg( 1, H, Term),
	functor( Term, N, A),
	spy_constraint( N, A).

debug_stop_reason( Why, _) :- nonvar( Why).
debug_stop_reason( Why, Event) :- var( Why),
	( debug_stop( Event, 0, Why) ->
	    true
	;
	    Why = ' '
	).

chr_debugging :-
	what_is_on( help),
	what_is_leashed( help),
	what_spypoints( help).

what_is_on( Type) :-
	getval( debug, Mode),
	print_message( Type, debug(Mode)).

what_is_leashed( Type) :-
	getval( leashing, Leash),
	findall( P, (chr_leash(0,K,[P],[]),K/\Leash>0), L),
	print_message( Type, leash(L)).

what_spypoints( Type) :-
	findall( rules(E), (spy_rule(R,H),(var(H)->E=R;E=H:R)), L0, L1),
	findall( constraints(E), (spy_constraint(N,A),(var(A)->E=N;E=N/A)), L1, []),
	sort( L0, Ls),
	print_message( Type, spypoints(Ls)).

% -----------------------------------------------------------------

debug_event( Event) :-
	getval( debug, State),
	( State == off ->
	    true
	;
	    debug_event( State, Event),
	    debug_stack( Event)
	).

debug_event( trace, Event) :-
	getval( leashing, L),
	( debug_stop( Event, L, SpyInd) ->
	    debug_stop_reason( SpyInd, Event),
	    debug_show( SpyInd, Event),
	    get_command( Cmd),
	    debug_do( Cmd, Event, trace)
	;
	    debug_stop_reason( SpyInd, Event),
	    debug_show( SpyInd, Event),
	    errnl
	).
debug_event( debug, Event) :-
	( debug_stop( Event, 0, SpyInd) ->
	    debug_show( SpyInd, Event),
	    get_command( Cmd),
	    debug_do( Cmd, Event, debug)
	;
	    true
	).
debug_event( skip(N,S), Event) :-
	stack_depth( M),
	( M =< N, member( Event, [exit(_),fail(_)]) ->
	    setval( debug, S),
	    debug_event( S, Event)
	;
	    true
	).
debug_event( off, _).

debug_stack( Event) :- Event = call(_), !, stack_push( Event).
debug_stack( Event) :- Event = wake(_), !, stack_push( Event).
%
debug_stack( Event) :- Event = apply(_,_,_,_,_,_), !,
	stack_pop,
	stack_push( Event).
%
debug_stack( exit(_)) :- !, stack_pop.
%
debug_stack( _).

stack_push( S) :-
	get_dbg_state( Ref),
	get_mutable( Stack, Ref),
	update_mutable( [S|Stack], Ref).

stack_pop :-
	get_dbg_state( Ref),
	get_mutable( [_|Stack], Ref),
	update_mutable( Stack, Ref).

stack_depth( Depth) :-
	get_dbg_state( Ref),
	get_mutable( Stack, Ref),
	length( Stack, Depth).

show_stack :-
	get_dbg_state( Ref),
	get_mutable( Stack, Ref),
	length( Stack, N),
	errwrite('Ancestors:'), errnl,
	show_stack( Stack, N), errnl.

show_stack( [],     _).
show_stack( [S|Ss], N) :-
	M is N-1,
	show_stack( Ss, M),
	Spy = ' ',
	( arg( 3, S, Hp) -> true ; Hp = '-' ),
	functor( S, Port, _),
	errformat( ' ~w ~|~t~d~4+ ~|~t~w~3+ ~|~p~t~7+', [Spy,N,Hp,Port]),
	debug_show_event( S),
	errnl.

debug_show( Spy, Event) :-
	functor( Event, Port, _),
	( arg( 3, Event, Hp) -> true ; Hp = '-' ),
	stack_depth( Depth),
	errformat( ' ~w ~|~t~d~4+ ~|~t~w~3+ ~|~p~t~7+', [Spy,Depth,Hp,Port]),
	debug_show_event( Event).

debug_show_event( call(S)) :-
	debug_susp_term( S, Term), errtab( 1), errprint( Term).
debug_show_event( wake(S)) :-
	debug_susp_term( S, Term), errtab( 1), errprint( Term).
debug_show_event( exit(S)) :-
	debug_susp_term( S, Term), errtab( 1), errprint( Term).
debug_show_event( redo(S)) :-
	debug_susp_term( S, Term), errtab( 1), errprint( Term).
debug_show_event( fail(S)) :-
	debug_susp_term( S, Term), errtab( 1), errprint( Term).
debug_show_event( remove(S)) :-
	debug_susp_term( S, Term), errtab( 1), errprint( Term).
debug_show_event( insert(C)) :-
	errtab( 1), errprint( C).
debug_show_event( try(Handler,Rule,_,Heads,_,_)) :-
	errformat( ' ~p:~p @ ', [Handler,Rule]),
	show_heads( Heads, 0, 0, _).
debug_show_event( apply(Handler,Rule,_,Heads,_,_)) :-
	errformat( ' ~p:~p @ ', [Handler,Rule]),
	show_heads( Heads, 0, 0, _).

debug_susp_term( S, Term#S) :-
	S =.. [suspension,_,_,_,_,_,F|Args],
	Term =.. [F|Args].

debug_do( 0'a, _, _) :- !, abort.
debug_do( 0'n, _, _) :- !, chr_notrace.
debug_do( 0'&, E, S) :- !, show_store( 0), debug_event( S, E).
debug_do( [0'&|_], E, S) :- !, show_store( 1), debug_event( S, E).
debug_do( 0'g, E, S) :- !, show_stack, debug_event( S, E).
debug_do( 0'., E, S) :-
	dbg_at_rule( E, _, _),
	!,
	show_rule( E),
	debug_event( S, E).
debug_do( 0'+, E, S) :- !,
	( dbg_at_rule( E, Handler, Rule) ->
	    chr_spy( rules( Handler:Rule))
	; dbg_at_constraint( E, N, A) ->
	    chr_spy( constraints( N/A))
	),
	debug_event( S, E).
debug_do( 0'-, E, S) :- !,
	( dbg_at_rule( E, Handler, Rule) ->
	    chr_nospy( rules( Handler:Rule))
	; dbg_at_constraint( E, N, A) ->
	    chr_nospy( constraints( N/A))
	),
	debug_event( S, E).
debug_do( 0'b, E, S) :- !,
	setval( debug, off),
	break,
	setval( debug, S),
	debug_event( S, E).
debug_do( 0'
	     , _, _) :- !, setval( debug, trace). % CR = creep
debug_do( 0'c, _, _) :- !, setval( debug, trace). % creep
debug_do( 0'l, _, _) :- !, setval( debug, debug). % leap
debug_do( 0's, E, S) :- chr_skip( E, S, _), !.	% skip
debug_do( [0's,N], E, S) :- chr_skip( E, S, N), !. % skip
debug_do( 0'<, E, S) :- !, set_pd(10), debug_event( S, E).
debug_do( [0'<,N], E, S) :- !, set_pd(N), debug_event( S, E).
debug_do( 0'=, E, S) :- !, chr_debugging, debug_event( S, E).
debug_do( 0'?, E, S) :- !, dbg_help, debug_event( S, E).
debug_do( 0'h, E, S) :- !, dbg_help, debug_event( S, E).
debug_do( _,   E, S) :-
	print_message( informational, wrong_option),
	debug_event( S, E).

chr_skip( E, S, K) :- E = exit(_), stack_depth( K), !, debug_event( S, E).
chr_skip( E, S, K) :- E = fail(_), stack_depth( K), !, debug_event( S, E).
chr_skip( _, S, K) :-
	stack_depth( Depth),
	( var(K) ->
	    N is Depth+1
	;
	    1 =< K, K =< Depth,
	    N = K
	),
	setval( debug, skip(N,S)).

dbg_at_rule( try(Handler,Rule,_,_,_,_),   Handler, Rule).
dbg_at_rule( apply(Handler,Rule,_,_,_,_), Handler, Rule).

dbg_at_constraint( E, N, A) :-
	dbg_at_constraint( E, S),
	S =.. [suspension,_,_,_,_,_,N|Args],
	length( Args, A).

dbg_at_constraint( call(S), S).
dbg_at_constraint( wake(S), S).
dbg_at_constraint( exit(S), S).
dbg_at_constraint( redo(S), S).
dbg_at_constraint( fail(S), S).
dbg_at_constraint( insert(S), S).
dbg_at_constraint( remove(S), S).

%
% numbervars binds variables ...
%
show_rule( Event) :-
	Event =.. [Which,Handler,Rule,_,Heads,Guard,Body],
	member( Which, [try,apply]),
	current_handler( Handler, _),
	!,
	show_rule( Rule, Heads, Guard, Body).
show_rule( _).

show_rule :-
	chrcmp:rule( _, _, Name, Heads, Guard, Body, _),
	numbervars( Heads/Name/Guard/Body, 0, _),
	show_rule( Name, Heads, Guard, Body),
	fail.
show_rule.

show_rule( Name, Heads, Guard, Body) :-
	errformat( '~n ~p @', [Name]),
	show_heads( Heads, 2, 2, Ident),
	( member( k(_,_), Heads) ->
	    errformat( ' <=>~n~n', [])
	;
	    errformat( ' ==>~n~n', [])
	),
	( Guard==true ->
	    show_body( Body, Ident)
	;
	    show_body( Guard, Ident), errnl,
	    errtab( Ident), errwrite( '|'), errnl,
	    show_body( Body, Ident)
	),
	errput( 0'.), errnl, errnl.

show_body( (A,B), Tab) :- !,
	show_body( A, Tab),
	errwrite( ','), errnl,
	show_body( B, Tab).
show_body( (A->B;C), Tab) :- !,
	errtab( Tab), errwrite( '('), errnl,
	NTab1 is Tab+2,
	NTab2 is Tab+5,
	show_body( A, NTab1),
	errwrite( '  ->'), errnl,
	show_body( B, NTab2), errnl,
	errtab( Tab), errwrite( ';'), errnl,
	show_body( C, NTab2), errnl,
	errtab( Tab), errwrite( ')').
show_body( (A->B), Tab) :- !,
	errtab( Tab), errwrite( '('), errnl,
	NTab1 is Tab+2,
	NTab2 is Tab+5,
	show_body( A, NTab1),
	errwrite( '  ->'), errnl,
	show_body( B, NTab2), errnl,
	errtab( Tab), errwrite( ')').
show_body( (A;B), Tab) :- !,
	errtab( Tab), errwrite( '('), errnl,
	NTab is Tab+5,
	show_body( A, NTab), errnl,
	errtab( Tab), errwrite( ';'), errnl,
	show_body( B, NTab), errnl,
	errtab( Tab), errwrite( ')').
show_body( A, Tab) :-
	errtab( Tab),
	errwriteq( A).

show_heads( [],     I, _, I).
show_heads( [H|Hs], I, D, If) :-
	arg( 1, H, C),
	arg( 2, H, T),
	( I>0 -> errnl ; true ),
	errtab( I), errprint( C#T),
	( Hs=[] ->
	    If = I
	; H=r(_,_), Hs=[k(_,_)|_] ->
	    errput(0' ), errput(0'\\), errput(0' ),
	    J is I+D,
	    show_heads( Hs, J, D, If)
	;
	    errput(0',), errput(0' ),
	    J is I+D,
	    show_heads( Hs, J, D, If)
	).

show_store( 0) :-
	errnl,
	global_term_ref_1( Global),
	find_constraint_internal( Global, Term, S, active, Module),
	module_wrap( Term, Module, Wrapped),
	errprint( Wrapped#S), errnl,
	fail.
show_store( 1) :-
	prolog_flag( debugger_print_options, Options),
	errnl,
	global_term_ref_1( Global),
	find_constraint_internal( Global, Term, S, State, Module),
	S =.. [suspension,Id,_,_Closure,Gref,Href|_],
	get_mutable( Generation, Gref),
	get_mutable( Hist, Href),
	assoc_to_list( Hist, History),
	module_wrap( Term, Module, Wrapped),
	errformat( '~|~t~p~5+ ~|~t~d~3+ ~|~p~t~10+ ~|~@~t~50+ ',
	  [Id,Generation,State,write_term(Wrapped,Options)]),
	show_history( History),
	errnl,
	fail.
show_store( _) :- errnl.

show_history( []).
show_history( [K-_|Hs]) :-
	errprint( K),
	( Hs==[] -> true ; errput(0',) ),
	show_history( Hs).

set_pd( N) :-
	prolog_flag( debugger_print_options, Old),
	( select( max_depth(_), Old, Rest) ->
	    true
	;
	    Rest = Old
	),
	( N < 0 -> D = 0 ; D = N ),
	prolog_flag( debugger_print_options, _, [max_depth(D)|Rest]).


dbg_help :-
	errnl,
	errwrite('CHR debugging options:'), errnl,
	errwrite('   <cr>   creep            c      creep'), errnl,
	errwrite('    l     leap			 '), errnl,
	errwrite('    s     skip             s <i>  skip i'), errnl,
	errwrite('    g     ancestors			'), errnl,
	errwrite('    &     constraints      & <i>  constraints (details)'), errnl,
	errwrite('    n     nodebug          =      debugging'), errnl,
	errwrite('    +     spy this			'), errnl,
	errwrite('    -     nospy this       .      show rule'), errnl,
	errwrite('    <     reset printdepth < <n>  set printdepth'), errnl,
	errwrite('    a     abort            b      break'), errnl,
	errwrite('    ?     help             h      help'), errnl,
	errnl.

errnl :- nl( user_error).

errput( X) :- put( user_error, X).

errtab( X) :- tab( user_error, X).

errwrite( X) :- write( user_error, X).

errwriteq( X) :- writeq( user_error, X).

errprint( X) :-
	prolog_flag( debugger_print_options, Options),
	write_term( user_error, X, Options).

errformat( F, A) :- format( user_error, F, A).

% ----------------------------------------------------------
%
% code from the Bips/trace.pl
%

get_command(Command) :-
	errwrite(' ? '),
	ttyflush,
	ttyget0(C1),
	get_command(C1, Command).

get_command(0'
	, 0'
	   ) :- !.
get_command(C1, Command) :-
	ttyget0(C2),
	get_args(C2, Args),
	(   Args = [] -> Command = C1
	;   Command = [C1|Args]
	).

get_args(0'
	 , []) :- !.
get_args(C1, [Arg|Args]) :-
	C1 >= 0'0, C1 =< 0'9, !,
	get_arg(C1, 0, Arg, C2),
	get_args(C2, Args).
get_args(0'-, [Arg|Args]) :- !,
	ttyget0(C2),
	get_arg(C2, 0, Arg1, C3),
	Arg is -Arg1,
	get_args(C3, Args).
get_args(_, Args) :-
	ttyget0(C2),
	get_args(C2, Args).

get_arg(C1, Arg0, Arg, C) :-
	C1 >= 0'0, C1 =< 0'9, !,
	Arg1 is Arg0*10 + C1 - 0'0,
	ttyget0(C2),
	get_arg(C2, Arg1, Arg, C).
get_arg(C1, Arg, Arg, C1).
