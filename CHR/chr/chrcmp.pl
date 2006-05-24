%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Constraint Handling Rules			      version 2.2 %
%								  %
%  (c) Copyright 1996-98					  %
%  LMU, Muenchen						  %
%								  %
%  File:   chrcmp.pl						  %
%  Author: Christian Holzbaur		christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
   NOTES
   -----

   +) Environment trimming?
      No because the merrits are small and revive needs vars trimmed away ...

   +) Full macro expansion (decouple compiler from runtime system)

   -) Group rules with identical outer match prefix?
      Problem with total rule order.

*/

:- module( chrcmp,
	[
	  options/0,
	  cc/1
	]).

%vsc
% debug
:- ['operator'].
:- use_module(getval).
:- ['matching'].
:- use_module(concat, [concat_name/2]).

:- use_module( library(terms),
	[
	  term_variables/2,
	  variant/2
	]).

:- use_module( library(lists),
	[
	  is_list/1,
	  member/2,
	  append/3,
	  reverse/2,
	  same_length/2
	]).

:- use_module( library(ordsets),
	[
	  list_to_ord_set/2,
	  ord_intersection/3,
	  ord_member/2,
	  ord_union/3
	]).

%
% name, [default | values]
%
compiler_option( check_guard_bindings,	[on,off]).
compiler_option( already_in_store,	[off,on]).
compiler_option( already_in_heads,	[off,on]).
compiler_option( debug_compile, 	[off,on]).
%
% internal
%
compiler_option( flatten,		[on,off]).
compiler_option( rule_ordering, 	[canonical,heuristic]).
compiler_option( simpagation_scheme,	[single,multi]).
compiler_option( revive_scheme, 	[new,old]).
compiler_option( dead_code_elimination, [on,off]).

options :-
	compiler_option( Name, _),
	getval( Name, Value),
	print( option(Name,Value)), put(0'.), nl,
	fail.
options.

:- dynamic constraint/2.
:- dynamic rule/7.
:- dynamic aih_functor/3.

%
% Default compiler options, etc.
%
init( _) :-
	compiler_option( Name, [Default|_]),
	setval( Name, Default),
	fail.
init( Name) :-
	setval( rulenum, 0),
	setval( rules, _),
	setval( handler, Name),
	retractall( rule(Name,_,_,_,_,_,_)),
	retractall( constraint(Name,_)),
	retractall( aih_functor(Name,_,_)).

:- initialization
	init(_).

:- multifile
	user:portray_message/2,
	user:term_expansion/2,
	user:goal_expansion/3.

:- dynamic
	user:portray_message/2,
	user:term_expansion/2,
	user:goal_expansion/3.

user:goal_expansion( dbg(E), chr, Exp) :-
	( getval( debug_compile, on) ->
	    Exp = debug_event(E)
	;
	    Exp = true
	).
user:goal_expansion( '__remove_some'(L), _, Exp) :-
	remove_some( L, Exp).

remove_some( [],       true).
remove_some( [H#T|Ts], Exp) :-
	functor( H, F, A),
	flat( 'F'(n(detach,F/A), Vars, T), Call),
	Exp = (
		  chr:dbg( remove(T)),
		  chr:remove_constraint_internal( T, Vars),
		  Call,
		  Exps
	      ),
	remove_some( Ts, Exps).

user:portray_message( informational, compiler(What)) :-
	info_message( What).
user:portray_message( error, compiler(What)) :-
	error_message( What, '{CHR Compiler ERROR: ', '}').

info_message( start(F,A)) :- !,
	format( user_error, '{CHR compiling constraint ~p/~p}~n', [F,A]).
info_message( dce(H,R)) :- !,
	format( user_error, '{CHR   eliminated code for head ~p in ~p}~n', [H,R]).
info_message( What) :-
	print_message( force(informational), What).


error_message( syntax(Term,N), P, S) :- !,
	prolog_flag( toplevel_print_options, Opt),
	format( user_error, '~asyntax rule ~p: ~@~a~n', [P,N,write_term(Term,Opt),S]).
%
error_message( wild_head(Rule), P, S) :- !,
	format( user_error, '~atoo many general heads in ~p~a~n', [P,Rule,S]).
%
error_message( pragma(Prag,Rule), P, S) :- !,
	format( user_error, '~abad pragma ~p in ~p~a~n', [P,Prag,Rule,S]).
%
error_message( undefined_constraint(F,A,Rule,Poss), P, S) :- !,
	format( user_error, '~afound head ~p in ~p, expected one of: ~p~a~n',
	  [P,F/A,Rule,Poss,S]).
%
error_message( bad_ids(R), Prefix, Suffix) :- !,
	prolog_flag( toplevel_print_options, Opt),
	format( user_error, '~ahead identifiers in ~@ are not unique variables~a~n',
	  [Prefix,write_term(R,Opt),Suffix]).
%
error_message( handler_undefined, Prefix, Suffix) :- !,
	format( user_error,'~ano handler defined~a~n', [Prefix,Suffix]).
%
error_message( failed, Prefix, Suffix) :- !,
	format( user_error,'~acompilation failed~a~n', [Prefix,Suffix]).
%
error_message( What, _, _) :-
	print_message( force(error), What).

% ------------------------------------------------------------------

record_constr( C, _) :- var( C), !,
	raise_exception( instantiation_error(constraints(C),1)).
record_constr( (C,Cs), H) :- !,
	record_constr( C, H),
	record_constr( Cs, H).
record_constr( C, H) :-
	C = F/A,
	atom( F),
	integer( A),
	A >= 0,
	!,
	( constraint( H, C) ->
	    true
	;
	    assert( constraint( H, C))
	).
record_constr( C, _) :-
	raise_exception( domain_error(constraints(C),1,'Functor/Arity',C)).

handler( Name) :-
	getval( handler, Name),
	nonvar( Name),
	!.
handler( _) :-
	raise_exception( compiler(handler_undefined)).

user:term_expansion( (handler Name), []) :-
	( var( Name) ->
	    raise_exception( instantiation_error(handler(Name),1))
	; atom( Name) ->
	    init( Name)
	;
	    raise_exception( type_error(handler(Name),1,atom,Name))
	).
user:term_expansion( option(N,V), []) :-
	( compiler_option( N, Pval) ->
	    ( member( V, Pval) ->
		setval(N,V)
	    ;
		raise_exception( domain_error(option(N,V),2,one_of(Pval),V))
	    )
	;
	    findall( O, compiler_option(O,_), Opts),
	    raise_exception( domain_error(option(N,V),1,one_of(Opts),N))
	).
user:term_expansion( (rules Rs), []) :-
	setval( rules, Rs).
user:term_expansion( (constraints C), []) :-
	handler( Name),
	record_constr( C, Name).
%
% Motivation for operator/3: compiler local operators
%
user:term_expansion( operator(A,B,C), (:-op(A,B,C))) :- op(A,B,C).
%
user:term_expansion( Term, []) :- Term = (_ @ _),      !, parse_rule( Term).
user:term_expansion( Term, []) :- Term = (_ pragma _), !, parse_rule( Term).
user:term_expansion( Term, []) :- Term = (_ <=> _),    !, parse_rule( Term).
user:term_expansion( Term, []) :- Term = (_ ==> _),    !, parse_rule( Term).
%
user:term_expansion( end_of_file, Exp) :-
	prolog_load_context( module, Module),
	Module \== chrcmp,			% leave us alone
	getval( handler, Name),
	nonvar( Name),
	!,
	setval( handler, _),			% spill once only
	%
	% the system is unhappy when the expansion
	% of end_of_file rises an exception ...
	%
	on_exception( Error, spill(Name,Module,Exp), report(Error)).

report( Error) :-
	print_message( error, Error),
	fail.

spill( Handler, Module, Exp3) :-
	findall( Key, constraint(Handler,Key), Keys),
	findall( C, (constraint(Handler,F/A),functor(C,F,A)), Cs),
	comp_support( Keys, Handler, Module, Exp0,Exp1),
	comp_constraints( Cs, Handler, Module, Exp1,[]),
	expansion( Exp0, Exp3, []),
%	show( Exp3),
	!.
spill( _, _, _) :-
	raise_exception( compiler(failed)).

show( Cls) :-
	member( C, Cls),
	portray_clause( C),
	fail.
show( _).

expansion( []) --> [end_of_file].
expansion( [C|Cs]) -->
	( {
	      flat( C, F),
	      expand_term( F, E)
	  } ->
	    copy( E)
	;
	    {raise_exception( compiler(expansion_failed(C)))}
	),
	expansion( Cs).

copy( X) --> {var(X)}, !, [ X ].
copy( []) --> !.
copy( [X|Xs]) --> !, [ X ], copy( Xs).
copy( X) --> [ X ].

comp_support( Keys, Handler, Module) -->
	{
	    keys2atts( Keys, Atts, AttSpec)
	},
	[
	    (:- chr:register_handler(Handler,Keys,Atts)),
	    (:- use_module(library(atts))),
	    (:- attribute(AttSpec)),

	    (verify_attributes( X, Y, Later) :-
	       get_atts( X, Sx),
	       Sx = [_|_],
	       !,
	       sort( Sx, Sxs),
	       ( var(Y) ->
		   Later = [ chr:run_suspensions(Sz) ],
		   get_atts( Y, Sy),
		   sort( Sy, Sys),
		   chr:merge_attributes( Sxs, Sys, Sz),
		   call( put_atts( Y, Sz))
	       ;
		   Later = [ chr:run_suspensions(Sx) ],
		   ( compound(Y) ->		% optimization
		       chr:term_variables( Y, NewVars),
		       attach_increment( NewVars, Sxs)
		   ;
		       true
		   )
	       )),
	    (verify_attributes( _, _, [])),

	    (attach_increment( [],     _)),
	    (attach_increment( [V|Vs], Attl) :-
	       chr:not_locked( V),
	       get_atts( V, All),		% maybe []
	       sort( All, Alls),
	       chr:merge_attributes( Alls, Attl, AttsNew),
	       call( put_atts( V, AttsNew)),
	       attach_increment( Vs, Attl)),

	    (get_suspensions( X, Susp) :- get_atts( X, Susp)),
	    (put_suspensions( X, Susp) :- call(put_atts( X, Susp))),

	    (attribute_goal( X, Goal) :-
	       chr:global_term_ref_1( G),
	       X == G,				% succeed once per module
	       get_atts( G, Gatts),
	       chr:attribute_goals( Gatts, Goal, Module))

	],
	gen_attach( Keys),
	gen_detach( Keys),
	gen_detach_case( Keys),
	gen_insert( Keys, Module).

keys2atts( [],	      [],	  nil).
keys2atts( [FA],      [Att],	  Name/1) :- !,
	key2att( FA, Att, Name).
keys2atts( [FA|Keys], [Att|Atts], (Name/1,Specs)) :-
	key2att( FA, Att, Name),
	keys2atts( Keys, Atts, Specs).

key2att( FA, Attribute, Name) :-
	concat_name( FA, Name),
	functor( Attribute, Name, 1).

gen_attach( []) --> [].
gen_attach( [F/A|Fs]) -->
	{
	    concat_name( F/A, AttName),
	    Att1 =.. [AttName,Val1],
	    Att2 =.. [AttName,Val2],
	    Att3 =.. [AttName,Val3]
	},
	[
	  'F'(n(attach,F/A),[],    _),
	 ('F'(n(attach,F/A),[X|Xs],S) :-
	      ( get_atts( X, Att1) ->
		  chr:sbag_add_element( Val1, S, Val2),
		  put_atts( X, Att2)
	      ;
		  chr:list_to_sbag( [S], Val3),
		  put_atts( X, Att3)
	      ),
	      'F'(n(attach,F/A),Xs,S))
	],
	gen_attach( Fs).

gen_detach( []) --> [].
gen_detach( [F/A|Fs]) -->
	{
	    concat_name( F/A, AttName),
	    Att1 =.. [AttName,Val1],
	    Att2 =.. [AttName,Val2]
	},
	[
	  'F'(n(detach,F/A),[],    _),
	 ('F'(n(detach,F/A),[X|Xs],S) :-
	      get_atts( X, Att1),
	      chr:sbag_del_element( Val1, S, Val2),
	      ( chr:sbag_empty( Val2) ->
		  put_atts( X, -Att2)
	      ;
		  put_atts( X, Att2)
	      ),
	      'F'(n(detach,F/A),Xs,S))
	],
	gen_detach( Fs).

gen_detach_case( []) --> [].
gen_detach_case( [F/A|Fs]) -->
	[
	    (detach(F/A,Susp,Vars) :- 'F'(n(detach,F/A),Vars,Susp))
	],
	gen_detach_case( Fs).

gen_insert( Keys, Module) -->
	[
	    (insert_constraint(C,T) :- var(C), !,
	       raise_exception( instantiation_error( insert_constraint(C,T),1)))
	],
	gen_insert_2( Keys, Module),
	[
	    (insert_constraint(C,T) :-
	       raise_exception( type_error(insert_constraint(C,T),1,'a constraint term',C)))
	],
	%
	[
	    (insert_constraint(C,T,Vs) :- var(C), !,
	       raise_exception( instantiation_error( insert_constraint(C,T,Vs),1)))
	],
	gen_insert_3( Keys, Module),
	[
	    (insert_constraint(C,T,Vs) :-
	       raise_exception( type_error(insert_constraint(C,T,Vs),1,'a constraint term',C)))
	].

gen_insert_2( [],	  _) --> [].
gen_insert_2( [F/A|Keys], Module) -->
	{
	    length( Args, A),
	    C =.. [F|Args],
	    flat( 'F'( n(F/A,1), a(Args), h(Self)), Closure)
	},
	[(
	   insert_constraint( C, Self) :- !,
	     chr:insert_constraint_internal( Vs, Self, Module:Closure, F, Args),
	     chr:dbg( insert(C#Self)),
	     'F'(n(attach,F/A), Vs, Self)
	)],
	gen_insert_2( Keys, Module).

gen_insert_3( [],	  _) --> [].
gen_insert_3( [F/A|Keys], Module) -->
	{
	    length( Args, A),
	    C =.. [F|Args],
	    flat( 'F'( n(F/A,1), a(Args), h(Self)), Closure)
	},
	[(
	   insert_constraint( C, Self, Term) :- !,
	     chr:insert_constraint_internal( Vs, Self, Term, Module:Closure, F, Args),
	     chr:dbg( insert(C#Self)),
	     'F'(n(attach,F/A), Vs, Self)
	)],
	gen_insert_3( Keys, Module).

% -------------------------------------------------------------------------

comp_constraints( [],	  _,	   _) --> [].
comp_constraints( [C|Cs], Handler, Module) -->
	comp_constraint( C, Handler, Module),
	comp_constraints( Cs, Handler, Module).

comp_constraint( C, Handler, Module) -->
	{
	  functor( C, F, A),
	  print_message( informational, compiler(start(F,A))),
	  getval( rules, Active),
	  findall( rule(H,Ps,G,B,n(Handler,Na,F/A,N,Pos),Hs,Prag),
		    (
			rule(Handler,N,Na,Hs,G,B,Prag),
			active_rule( Active, Na),
			choose( Hs, H, Ps, 1, Pos),
			arg( 1, H, C)
		    ), Rs),
	  sort_rules( Rs, Rsss),

	  C =.. [_|Args],
	  flat( 'F'( n(F/A,1), a(Args), h(Self)), Closure),
	  Alloc = Ad:Closure/Self/Args,
	  ( getval( debug_compile, on) ->
	      Ad = early,
	      EntryPoint =			% byrd box
		(
		    chr:allocate_constraint( Module:Closure, Self, F, Args),
		    (	chr:dbg( call(Self)), Closure
		    ;	chr:dbg( fail(Self)), !, fail
		    ),
		    (	chr:dbg( exit(Self))
		    ;	chr:dbg( redo(Self)), fail
		    )
		)
	  ;
	      EntryPoint = Closure
	  )
	},

	[(
	   C :-
	      EntryPoint			% user entry point
	)],

	{ comp_rules_first( Rsss, M, Alloc, Module, RL, RLT) },

	%
	% Code for rules generated, pragmas seen
	%

	already_in_store( Handler, C, F, A),
	already_in_heads( Handler, C, F, A),

	splice( RL, RLT),			% insert rule code

	{
	  (var(Ad) ->				% compile time
	     Allocate =
	       ( var(Self) ->			% runtime
		   chr:insert_constraint_internal( LinkVars, Self, Module:Closure, F, Args)
	       ;
		   chr:activate_constraint( LinkVars, Self, _)
	       )
	  ;
	     Allocate = chr:activate_constraint( LinkVars, Self, _)
	  )
	},
	[( 'F'( n(F/A,M), a(Args), h(Self)) :-
		Allocate,
		chr:dbg( insert(C#Self)),
		'F'(n(attach,F/A), LinkVars, Self)
	)].

comp_rules_first( Rs, M, Alloc, Module, L, Lt) :-
	( getval(dead_code_elimination,on),
	  dead_code_elimination( Rs, Rse) ->
	    phrase( comp_rules( Rse, 1,M, Alloc, Module), L, Lt)
	;
	    phrase( comp_rules( Rs,  1,M, Alloc, Module), L, Lt)
	).

%
% Assumes knowledge about DCG expansion
%
splice( RL, LT, RL, LT).

alloc(	  X:_,		       _,    _,    _,	_,	true) :- nonvar( X), !.
alloc( done:Closure/Self/Args, Self, Args, F/_, Module, Code) :-
	Code = ( var(Self) ->
		   chr:allocate_constraint( Module:Closure, Self, F, Args)
	       ;
		   true
	       ).

choose( [X|Xs], X, Xs,	   N0,N0).
choose( [X|Xs], Y, [X|Xt], N0,N2) :-
	N1 is N0+1,
	choose( Xs, Y, Xt, N1,N2).

active_rule( Active, _) :- var( Active), !.
active_rule( (A,B),  N) :- !,
	( active_rule( A, N) ->
	    true
	;
	    active_rule( B, N)
	).
active_rule( R, N) :-
	variant( R, N).

%
% Heuristic ordering compatible with the Eclipse version:
%
%    single headed < double headed
%    propagation last
%    kill < revive
%
% Within a single rule we put clauses for active k(H,_) first
%
sort_rules( Rs, Rss) :-
	getval( rule_ordering, O),
	augment( Rs, O, Rsa),
	keysort( Rsa, Rsas),			% stable sort
	strip( Rsas, Rss).

augment( [],	 _, []).
augment( [R|Rs], O, [K-R|Rsa]) :-
	weight( O, R, K),
	augment( Rs, O, Rsa).

strip( [],	 []).
strip( [_-R|Rs], [R|Rss]) :-
	strip( Rs, Rss).

weight( canonical, rule(H,_,_,_,n(_,_,_,N,_),_,_), w(N,Nh)) :-
	functor( H, Nh, _).			% k < r
weight( heuristic, rule(H,Ps,_,_,_,_,_), w(Lw,Pw,Nh)) :-
	length( Ps, Lw),
	functor( H, Nh, _),
	( member( k(_,_), Ps) -> Pw=1 ; Pw=2 ).

%
% k(_) rules after the current one that are variants
% up to and including the guard can be dropped (cut semantics)
%
% In the presence of already_in_heads we give up to
% get the continuation(s) right.
%
% constraints e/4.
%
% write @ e(A,B,C,D) ==> write(e(A,B,C,D)),nl.
% id1 @ e(A,B,C,D) <=> write(id1),nl, e(B,A,C,D).
% id2 @ e(A,B,C,D) <=> write(id2),nl, e(C,D,A,B).
%
%| ?- e(1,2,a,b).
%
% The check for a passive/1 pragma is to get some code
% generated after all for the following rule:
%
% (X leq Y)#Id	, Y leq X <=> X=Y pragma passive(Id).
%
dead_code_elimination( Rs, Rse) :-
	getval( already_in_heads, on),
	!,
	Rse = Rs.
dead_code_elimination( Rs, Rse) :-
	member( rule(_,_,_,_,_,_,Pragma), Rs),
	( member( already_in_heads, Pragma) -> true
	; member( already_in_head(_), Pragma) -> true
	; member( passive(_), Pragma) -> true
	),
	!,
	Rse = Rs.
dead_code_elimination( Rs, Rse) :-
	reverse( Rs, Rr),
	dc_loop( Rr, Rkr),
	reverse( Rkr, Rse).

dc_loop( [],	 []).
dc_loop( [R|Rs], Res) :-
	R = rule(Active,Ps,G,_,n(_,Rnam,_,_,Hn),_,_),
	( Active=k(_,_),
	  member( rule(Ap,Pp,Gp,_,_,_,_), Rs),
	  variant( Active/Ps/G, Ap/Pp/Gp) ->
	    print_message( informational, compiler(dce(Hn,Rnam))),
	    Res = Rest
	;
	    Res = [R|Rest]
	),
	dc_loop( Rs, Rest).

%
% Currently for all constraints. Could be specific.
%
already_in_store( Handler, C, F, A) -->
	{
	   getval( already_in_store, on),
	   !,
	   C =.. [_|Args],
	   same_length( Args, Actual),
	   same_length( Args, Actual2),
	   vars( C, V0),
	   key2att( F/A, Att, _)
	},
	[( 'F'( n(F/A,1), a(Actual), h(T1)) :-
	    chr:inline_matching( Args, Actual),
	    chr:via( V0, Via),
	    nd_init_iteration( Via, Handler, F/A, Att, T2),
	    chr:load_args( T2, active, Actual2),
	    chr:inline_matching( V0-Args, V0-Actual2),
	    !,
	    chr:dbg( apply(Handler,already_in_store,2,[r(C,T2),k(C,T1)],true,true))
	)].
already_in_store( _, _, _, _) --> [].

comp_rules( [], 			      N0,N0, _,     _) --> [].
comp_rules( [rule(H,Ps,G,B,Name,Hs,Prag)|Rs], N0,N2, Alloc, Module) -->
	( {
	   N1 = N0,
	   arg( 2, H, Tid),
	   member( X, Prag),
	   X == passive(Tid)
	  } ->
	       []
	;
	       {split( Ps, Kill, Revive)},
	       comp_rule( H, Kill, Revive, G,B, N0,N1, Name, Hs, Alloc, Module, Prag)
	),
	comp_rules( Rs, N1,N2, Alloc, Module).

split( [],     [], []).
split( [P|Ps], K,  R) :-
	( P=k(H,T) -> K=[H#T|Ks], R=Rs
	; P=r(H,T) -> R=[H#T|Rs], K=Ks
	),
	split( Ps, Ks, Rs).


%
% Current constraint of type k(_,_), i.e. to be removed (easy, not yet allocated)
%
%   H ?- Exists p1,p2,p3, G | kill(some pi) B
%
comp_rule( k(H,Tid), Kill, Revive, G, Body, N0,N1, n(Hi,Ni,F/A,_,Hx), Hs, _, Module, Pragma) -->
	{
	  H =.. [_|Args],
	  same_length( Args, Actual),
	  vars( H, V0),
	  ndmpc( Kill,	 Hi, MatchKill,   Pragma, V0,V1, [],Ks),
	  ndmpc( Revive, Hi, MatchRevive, Pragma, V1,V2, Ks,_),
	  aih_expose( active(H,Tid), Hi, N0,N1, Kill, Body, Pragma, Continuation, FinalBody),
	  check_guard( V2, G, GuardCode)
	},
	[( 'F'( n(F/A,N0), a(Actual), h(Tid)) :-
		   chr:inline_matching( Args, Actual),
		   MatchKill,
		   MatchRevive,
		   chr:dbg( try(Hi,Ni,Hx,Hs,G,Body)),
		   GuardCode,
		   !,
		   chr:dbg( apply(Hi,Ni,Hx,Hs,G,Body)),
		   '__remove_some'( Ks),
		   (var(Tid)->true;'__remove_some'( [H#Tid])),
		   FinalBody
	)],
	( {N0=:=N1} -> []
	;
	    {
	      Continuation = Module:Cgoal,
	      flat( 'F'( n(F/A,N1), a(Actual), h(Tid)), Cgoal)
	    },
	    [('F'( n(F/A,N0), a(Actual), h(Tid)) :- Continuation )]
	).

comp_rule( r(H,Self), [], [], G, Body, N0,N1, n(Hi,Ni,F/A,Ri,Hx), Hs, Alloc, Module, _Prag) --> !,
	{
	  N1 is N0+1,
	  H =.. [_|Args],
	  same_length( Args, Actual),
	  vars( H, V0),
	  revive( 'F'( n(F/A,N1), a(Actual), h(Self)),
		  Body, Proceed, H#Self, Self),
	  alloc( Alloc, Self, Actual, F/A, Module, Allocate),
	  check_guard( V0, G, GuardCode)
	},
	[
	  ( 'F'( n(F/A,N0), a(Actual), h(Self)) :-
		 chr:inline_matching( Args, Actual),
		 Allocate,
		 Tuple = t(Ri,Self),
		 chr:novel_production( Self, Tuple),
		 chr:dbg( try(Hi,Ni,Hx,Hs,G,Body)),
		 GuardCode,
		 !,
		 chr:dbg( apply(Hi,Ni,Hx,Hs,G,Body)),
		 chr:extend_history( Self, Tuple),
		 Proceed
	       ),
	  ( 'F'( n(F/A,N0), a(Actual), h(Self)) :-
		 Allocate,
		 'F'( n(F/A,N1), a(Actual), h(Self))
	       )
	].

comp_rule( r(H,Tid), [], [R|Rs], G, B, N0,N1, Name, Hs, Alloc, Module, Pragma) --> !,
	{
	  N1 is N0+1,
	  Name = n(_,_,F/A,_,_),
	  H =.. [_|Args],
	  same_length( Args, Actual),
	  vars( H, V0),
	  matching:code( Args, Actual, Code)
	},
	fwd_first( Code, Actual, R, N0, V0, Name, Alloc, Module, Pragma),
	fwd_rest( Rs, R, H#Tid, N0, 0, V0, G, B, Name, Hs,
		  propagation, [s(n(F/A,N1),a(Args))], Pragma).
%
comp_rule( r(H,Tid), [K|Ks], Rs, G, B, N0,N1, Name, Hs, Alloc, Module, Pragma) -->
	{
	  N1 is N0+1,
	  Name = n(_,_,F/A,_,_),
	  H =.. [_|Args],
	  same_length( Args, Actual),
	  vars( H, V0),
	  matching:code( Args, Actual, Code)
	},
	fwd_first( Code, Actual, K, N0, V0, Name, Alloc, Module, Pragma),
	( {getval( simpagation_scheme, single)} ->
	    %
	    % Single forward loop for an arbitrary partner to
	    % be killed,
	    % remaining partners are found nondet. inside the
	    % loop.
	    %
	    fwd_rest( [], K, H#Tid, N0, 0, V0, G, B, Name, Hs,
		      simpagation(Ks,Rs), [s(n(F/A,N1),a(Args))], Pragma)
	;
	    %
	    % One forward loop for every partner to be killed,
	    % remaining partners are found nondet. inside the
	    % loops.
	    %
	    fwd_rest( Ks, K, H#Tid, N0, 0, V0, G, B, Name, Hs,
		      simpagation([],Rs), [s(n(F/A,N1),a(Args))], Pragma)
	).

fwd_first( HeadMatch, Actual, Next#_, N, V0, n(Handler,_,F/A,_,_), Alloc, Module, Pragma) -->
	{
	  N1 is N+1,
	  functor( Next, Fn, An),
	  vars( Next, Vn),
	  compute_via( V0, Vn, Vias, Pragma),
	  alloc( Alloc, Self, Actual, F/A, Module, Allocate),
	  key2att( Fn/An, Att, _)
	},
	[
	   ('F'( n(F/A,N), a(Actual), h(Self)) :-
		 HeadMatch,
		 chr:via( Vias, Via),
		 init_iteration( Via, Handler, Fn/An, Att, Ds),
		 !,
		 Allocate,
		 'F'( n(F/A,N,0), state(Ds), h(Self), c([]), k([]), g(V0))),

	   ('F'( n(F/A,N), a(Actual), h(Self)) :-
		 Allocate,
		 'F'( n(F/A,N1), a(Actual), h(Self)))
	].

%
% The issure here is to let the body see the actual constraint
% when executing. The continuation inserts the constraint.
% Thus, if we run the body ahead of the continuation, we explicitly
% insert the constraint, run the body, remove the constraint again
% and run the continuation (which inserts the constraint again).
%
revive( Continuation, Body, Code, _, _) :-
	getval( revive_scheme, old),
	!,
	Code = ( Continuation, Body ).
revive( Continuation, Body, Code, Term#_, Self) :-
	( bening( Body) ->			% optimization
	    Code = (Body, Continuation)
	;
	    functor( Term, F, A),
	    Code = (
		     chr:activate_constraint( LinkVars, Self, Generation),
		     'F'(n(attach,F/A), LinkVars, Self),
		     Body,
		     chr:constraint_generation( Self, State, Gen),
		     ( State == active, Gen == Generation ->
			 chr:change_state( Self, inactive),
			 Continuation
		     ;
			 true
		     ))
	).

fwd_rest( [], Q#Tid, Active, N,M, V0, Guard, Body, n(Hi,Ni,F/A,Ri,Hx), Hs,
	  propagation, Stack, _Pragma) --> !,
	{
	  Myname = n(F/A,N,M),
	  Active = _#Self,
	  length( Cs, M),
	  length( Ks, M),
	  vars( Q, Vq),
	  ord_union( V0, Vq, V1),
	  nextsol( Stack, M, Self, Cs, Ks, NextSol),
	  tids( Ks, KsT),
	  revive( 'F'(Myname,state(Dss),h(Self),c(Cs),k(KsT),g(V0)),
		  Body, Proceed, Active, Self),
	  tuple( Hs, Ri, Tv, Tuple, Checks),
	  decompose( Q, _, _, Args, Actual),
	  alldiffs( Ks, Tid, Diffs),
	  check_guard( V1, Guard, GuardCode)
	},
	[
	   ( 'F'( Myname, state(St), h(Self), c(Cs), k(KsT), g(V0)) :-
		   chr:iter_last( St),
		   NextSol),
	   ( 'F'( Myname, state(St), h(Self), c(Cs), k(KsT), g(V0)) :-
		   chr:iter_next( St, Tid, Dss),
		   ( chr:load_args( Tid, active, Actual),
		     Diffs,
		     chr:inline_matching( V0-Args, V0-Actual),
		     chr:(Tv=Tuple),
		     Checks,
		     chr:dbg( try(Hi,Ni,Hx,Hs,Guard,Body)),
		     GuardCode ->
		       chr:dbg( apply(Hi,Ni,Hx,Hs,Guard,Body)),
		       chr:extend_history( Self, Tv),
		       Proceed
		   ;
		       'F'( Myname, state(Dss), h(Self), c(Cs), k(KsT), g(V0))
		   ))

	].
%
% Kill early to let the continuation (new scheme) see the effect.
%
fwd_rest( [], Q#Tid, Active, N,M, V0, Guard, Body, Name, Hs,
	  simpagation(Kss,Rs), Stack, Pragma) -->
	{
	  Name = n(Hi,Ni,F/A,_,Hx),
	  Myname = n(F/A,N,M),
	  Active = _#Self,
	  length( Cs, M),
	  length( Ks, M),
	  vars( Q, Vq),
	  ord_union( V0, Vq, V1),
	  nextsol( Stack, M, Self, Cs, Ks, NextSol),
	  M1 is M+1,
	  append( _, [First,_], [s(Myname,g(V0),k([Q#Tid|Ks]))|Stack]),
	  nextsol( [First], M1, Self, [Dss|Cs], [Q#Tid|Ks], RevCon),
	  %
	  ndmpc( Kss, Hi, MatchCode1, Pragma, V1,V2, [Q#Tid|Ks],K1),
	  ndmpc( Rs,  Hi, MatchCode2, Pragma, V2,V3, K1,        _),
	  append( [Q#Tid|Ks], Kss, Allkills),
	  decompose( Q, _, _, Args, Actual),
	  alldiffs( Ks, Tid, Diffs),
	  aih_expose( passive, Hi, 0,0, [Q#Tid|Kss], Body, Pragma, _, RevBody),
	  revive( RevCon, RevBody, Proceed, Active, Self),
	  check_guard( V3, Guard, GuardCode),
	  tids( Ks, KsT)
	},
	[
	   ( 'F'( Myname, state(St), h(Self), c(Cs), k(KsT), g(V0)) :-
		   chr:iter_last( St),
		   NextSol ),
	   ( 'F'( Myname, state(St), h(Self), c(Cs), k(KsT), g(V0)) :-
		   chr:iter_next( St, Tid, Dss),
		   ( chr:load_args( Tid, active, Actual),
		     Diffs,
		     chr:inline_matching( V0-Args, V0-Actual),
		     MatchCode1,
		     MatchCode2,
		     chr:dbg( try(Hi,Ni,Hx,Hs,Guard,Body)),
		     GuardCode ->
		       chr:dbg( apply(Hi,Ni,Hx,Hs,Guard,Body)),
		       '__remove_some'( Allkills),
		       Proceed
		   ;
		       'F'( Myname, state(Dss), h(Self), c(Cs), k(KsT), g(V0))
		   ))

	].
fwd_rest( [P#TidP|Ps], Q#Tid, Active, N,M, V0, G, B, Name, Hs,
	  RuleType, Stack, Pragma) -->
	{
	  Myname = n(F/A,N,M),
	  Name = n(Handler,_,F/A,_,_),
	  L is M+1,
	  length( Cs, M),
	  length( Ks, M),
	  vars( Q, Vq),
	  ord_union( V0, Vq, V1),
	  vars( P, Vp),
	  compute_via( V1, Vp, Vias, Pragma),
	  nextsol( Stack, M, H, Cs, Ks, NextSol),
	  decompose( Q, _,  _, Args, Actual),
	  decompose( P, Pf, Pa, _, _),
	  alldiffs( Ks, Tid, Diffs),
	  key2att( Pf/Pa, Att, _),
	  tids( Ks, KsT)
	},
	[
	   ( 'F'( Myname, state(St), h(H), c(Cs), k(KsT), g(V0)) :-
		   chr:iter_last( St),
		   NextSol ),
	   ( 'F'( Myname, state(St), h(H), c(Cs), k(KsT), g(V0)) :-
		   chr:iter_next( St, Tid, Dss),
		   ( chr:load_args( Tid, active, Actual),
		     Diffs,
		     chr:inline_matching( V0-Args, V0-Actual),
		     chr:via( Vias, Via),
		     init_iteration( Via, Handler, Pf/Pa, Att, Ds) ->
		       'F'( n(F/A,N,L), state(Ds),  h(H), c([Dss|Cs]), k([Tid|KsT]), g(V1))
		   ;
		       'F'( Myname, state(Dss), h(H), c(Cs), k(KsT), g(V0))
		   ))
	],
	fwd_rest( Ps, P#TidP, Active, N,L, V1, G, B, Name, Hs,
		  RuleType, [s(Myname,g(V0),k([P#Tid|Ks]))|Stack], Pragma).

nextsol( [s(Name,a(Args))|_],	 _, H, _,   _,	 'F'(Name,a(Args),h(H))).
nextsol( [s(Name,g(V),k(Km))|_], L, H, Css, Kss, 'F'(Name,state(C),h(H),c(Cs),k(Ks),g(V))) :-
	Name = n(_,_,M),
	N is L-M,
	skip( N, Css, C, Cs),
	tids( Km, [_|Ks]),
	( Km=Kss -> true ; true ).

skip( N, [X|Xs], X, Xs) :- N =< 1, !.
skip( N, [_|Xs], X, Xt) :-
	M is N-1,
	skip( M, Xs, X, Xt).

tuple( Heads, Ri, Tv, Tuple, Checks) :-
	tuple( Heads, Tv, Checks, Tids),
	Tuple =.. [t,Ri|Tids].

tuple( [],     _,  true,			    []).
tuple( [H|Hs], Tv, (chr:novel_production(C,Tv),Co), [C|Cs]) :-
	arg( 2, H, C),
	tuple( Hs, Tv, Co, Cs).

vars( Term, Set) :-
	term_variables( Term, Vs),
	list_to_ord_set( Vs, Set).

%
%
% Trick:
%
% Instead of match(    Pattern,    Datum) we say
%	     match( Gv-Pattern, Gv-Datum)
%
% where Gv are the global variables from
% matches further to the left of the current head.
%
ndmpc( [],	   _,	    true, _,      S0,S0, C0,C0).
ndmpc( [H#Tid|Ps], Handler, Mc,   Pragma, S0,S2, C0,C2) :-
	vars( H, Hv),
	compute_via( S0, Hv, Vias, Pragma),
	ord_union( S0, Hv, S1),
	decompose( H, F, A, Args, Actual),
	alldiffs( C0, Tid, Diffs),
	key2att( F/A, Att, _),
	Mc = (
	      chr:via( Vias, Via),
	      nd_init_iteration( Via, Handler, F/A, Att, Tid),
	      chr:load_args( Tid, active, Actual),
	      Diffs,
	      chr:inline_matching( S0-Args, S0-Actual),
	      Mcc
	     ),
	ndmpc( Ps, Handler, Mcc, Pragma, S1,S2, [H#Tid|C0],C2).

compute_via( Sofar, Local, Vias, Pragma) :-
	ord_intersection( Sofar, Local, Common),
	compute_via_( Pragma, Sofar, Local, ViaPragma),
	list_to_ord_set( ViaPragma, Vp),
	ord_union( Common, Vp, Vias).

compute_via_( [], _, _, []).
compute_via_( [P|Ps], Sofar, Local, Vias) :-
	( P = sharing(A,B) ->
	    ( ord_member( A, Sofar),
	      ord_member( B, Local) ->
	        Vias = [A|Rest]
	    ; ord_member( B, Sofar),
	      ord_member( A, Local) ->
	        Vias = [B|Rest]
	    ;
		Vias = Rest
	    )
	;
	    Vias = Rest
	),
	compute_via_( Ps, Sofar, Local, Rest).

%
% This could be more precise to consider only
% pairs of heads that unify, but \==/2 is cheap and our
% chains are short.
%
alldiffs( [],	    _, true).
alldiffs( [_#T|Ts], S, (S\==T,Diffs)) :-
	alldiffs( Ts, S, Diffs).

decompose( Term, F, A, Args, Actual) :-
	functor( Term, F, A),
	Term =.. [F|Args],
	same_length( Args, Actual).

tids( [],         []).
tids( [_#Tid|Ts], [Tid|Tids]) :-
	tids( Ts, Tids).

% ------------------------ already_in_heads support ---------------------

%
% A killed, exposed constraint can be passive in the exposing rule
% or active. When it is passive, the state of the constraint is
% changed to active. Otherwise the associated continuation is called.
%
already_in_heads( Handler, C, F, A) -->
	{
	    aih_functor( Handler, F, A),
	    !,
	    C =.. [_|Args]
	},
	[( 'F'( n(F/A,1), a(Args), h(T1)) :-
	    chr:is_exposed( C, T2, Continuation),
	    !,
	    chr:dbg( apply(Handler,already_in_heads,2,[r(C,T2),k(C,T1)],true,true)),
	    ( Continuation==true ->		% passive
		chr:dbg( insert(C#T2)),
		chr:activate_constraint( LinkVars, T2, keep),
		'F'(n(attach,F/A), LinkVars, T2)
	    ;
		call( Continuation)
	    )
	)].
already_in_heads( _, _, _, _) --> [].

%
% If aih applies, we may need a continuation (N1=N0+1),
% or not, but it is computed by the caller.
%
aih_expose( Type, Hi, N0,N1, Kill, Body, Pragma, Continuation, FinalBody) :-
	( getval( already_in_heads, on) ->
	    aih_expose( Type, Hi, N0,N1, Kill, Continuation, Handle, ExposeCall)
	; member( already_in_heads, Pragma) ->
	    aih_expose( Type, Hi, N0,N1, Kill, Continuation, Handle, ExposeCall)
	; Type=active(H,Tid),
	  aih_collect( Pragma, [H#Tid|Kill], Expose),
	  Expose=[_#I|Es] ->			% at least one
	    ( I==Tid ->
		aih_expose( Type, Hi, N0,N1, Es, Continuation, Handle, ExposeCall)
	    ;
		aih_expose( passive, Hi, N0,N1, Expose, Continuation, Handle, ExposeCall)
	    )
	; Type=passive,
	  aih_collect( Pragma, Kill, Expose),
	  Expose = [_|_] ->			% at least one
	    aih_expose( Type, Hi, N0,N1, Expose, Continuation, Handle, ExposeCall)
	),
	\+ bening( Body),			% optimization
	!,
	FinalBody = ( ExposeCall, Body, chr:de_expose( Handle) ).
aih_expose( _, _, N0,N0, _, Body, _, _, Body).

aih_expose( passive,	   Hi, N0,N0, K, _, Handle, chr:expose_passive(Handle,K)) :-
	aih_record( K, Hi).
aih_expose( active(H,Tid), Hi, N0,N1, K, C, Handle, chr:expose_active(Handle,H,Tid,K,C)) :-
	N1 is N0+1,
	aih_record( [H#Tid|K], Hi).

aih_collect( [],     _,     []).
aih_collect( [P|Ps], Kills, Expose) :-
	( P=already_in_head(Id),
	  member( K, Kills),
	  K = _#I,
	  I == Id ->
	    Expose = [K|Exps],
	    aih_collect( Ps, Kills, Exps)
	;
	    aih_collect( Ps, Kills, Expose)
	).

aih_record( Heads, Handler) :-
	member( Head#_, Heads),
	functor( Head, F, A),
	( aih_functor( Handler, F, A) ->
	    true
	;
	    assert( aih_functor(Handler,F,A))
	),
	fail.
aih_record( _, _).

% -------------------------- guard evaluation -------------------------

check_guard( Global, Guard, Code) :-
	split_guard( Guard, Ask, Tell),
	( Ask==true ->
	    Code = Tell
	;
	    Code = ( Wrap, Tell ),
	    wrap_guard( Ask, Global, Wrap)
	).

split_guard( (Ask & Tell), A,	T) ?- !, Ask=A, Tell=T.
split_guard( Guard,	 true,	Guard) :- getval( check_guard_bindings, off), !.
split_guard( Guard,	 Guard, true).

%
%  Conservative guard analysis to avoid lock/unlock/on_exception
%  for simple tests.
%
wrap_guard( Goal, Global, Expansion) :-
	( bening( Goal) ->			% no need to lock
	    ( simple_guard( Goal, Term) ->
		term_variables( Term, Vars),
		ensure_ground( Vars, Goal, Expansion)
	    ;
		Expansion = on_exception( instantiation_error(_,_),Goal,fail)
	    )
	;
	    vars( Goal, Va),
	    ord_intersection( Global, Va, Lock),
	    Expansion =
	      (
		  chr:lock_some( Lock),
		  on_exception( instantiation_error(_,_),Goal,fail),
		  chr:unlock_some( Lock)
	      )
	).

%
% Goal is guaranteed not to bind any variable
% Careful: ground(Body) may still
% call a constraint
%
bening( Goal) :- var( Goal), !, fail.
bening( _:Goal) :- bening( Goal).		% ignore module prefix
bening( true).
bening( fail).
bening( \+ G) :- bening( G).			% don't want G to trigger anything
bening( (A,B)) :-
	bening( A),
	bening( B).
bening( (A;B)) :-
	bening( A),
	bening( B).
bening( (A->B)) :-
	bening( A),
	bening( B).
%
bening( G) :- type_check( G).
bening( G) :- arith_compare( G).
bening( G) :- term_compare( G).

type_check( var(_)).
type_check( nonvar(_)).
type_check( integer(_)).
type_check( float(_)).
type_check( number(_)).
type_check( atom(_)).
type_check( atomic(_)).
type_check( simple(_)).
type_check( compound(_)).
type_check( callable(_)).
type_check( ground(_)).

arith_compare( _ =:= _).
arith_compare( _ =\= _).
arith_compare( _ < _).
arith_compare( _ > _).
arith_compare( _ =< _).
arith_compare( _ >= _).

term_compare( _ == _).
term_compare( _ \== _).
term_compare( _ @< _).
term_compare( _ @=< _).
term_compare( _ @> _).
term_compare( _ @>= _).

%
% avoid on_exception/3 if trivial
%
simple_guard( G,    _) :- var( G), !, fail.
simple_guard( Goal, []) :- ground( Goal), !.	% incl. true,fail,...
simple_guard( (A,B), Ta+Tb) :-
	simple_guard( A, Ta),
	simple_guard( B, Tb).
%
simple_guard( G, []) :- type_check( G).
simple_guard( G, []) :- term_compare( G).
simple_guard( G, G) :- arith_compare( G).

ensure_ground( [],     Guard, Guard).
ensure_ground( [V|Vs], Guard, (ground(V),Exp)) :-
	ensure_ground( Vs, Guard, Exp).

% --------------------- flatten --------------------

flat( (H:-B), (Hf:-Bf)) :- !,
	flat_g( H, Hf),
	flat_body( B, Bf).
flat( (H-->B), (Hf-->Bf)) :- !,
	flat_g( H, Hf),
	flat_body( B, Bf).
flat( Fact, FFact) :-
	flat_g( Fact, FFact).

%
% Here we will reorder and flatten the wrapped arguments.
% Considerations: 1st argument indexing, register motion, ...
%
% The magic functor in the templates is 'F'/N
%
flat_g( Goal, Flat) :-
	nonvar( Goal),
	Goal =.. ['F',Nm|Args],
	Nm =.. [n|NmL],
	concat_name( NmL, Name),
	!,
	( getval( flatten, on) ->
	    flat_args( Args, FlatArgs, [])
	;
	    FlatArgs = Args
	),
	Flat =.. [Name|FlatArgs].
flat_g( Goal, Goal).

flat_args( []) --> [].
flat_args( [A|As]) -->
	flat_arg( A),
	flat_args( As).

flat_arg( A) --> {var(A)}, !, [A].
flat_arg( state(S)) --> !, [S].
flat_arg( h(H)) --> !, [H].
flat_arg( a(L)) --> !, flat_list( L).
flat_arg( g(L)) --> !, flat_list( L).
flat_arg( c(L)) --> !, flat_list( L).
flat_arg( k(L)) --> !, flat_list( L).
flat_arg( A) --> [A].

flat_list( []) --> [].
flat_list( [X|Xs]) --> [X], flat_list( Xs).

flat_body( (true,B), Bf) ?- !, flat_body( B, Bf).
flat_body( (B,true), Bf) ?- !, flat_body( B, Bf).
flat_body( (A,B), Res) ?- !,
	flat_body( A, Af),
	flat_body( B, Bf),
	( Af==true -> Res=Bf
	; Bf==true -> Res=Af
	;	      Res=(Af,Bf)
	).
flat_body( (A->B), Res) ?- !,
	Res=(Af->Bf),
	flat_body( A, Af),
	flat_body( B, Bf).
flat_body( (A;B), Res) ?- !,
	Res=(Af;Bf),
	flat_body( A, Af),
	flat_body( B, Bf).
%
flat_body( B, Bf) :- flat_g( B, Bf).


% --------------------------- parsing ---------------------------

parse_rule( Term) :-
	handler( Handler),
	incval( rulenum, N),
	proper_rule( Term, Handler, N, Name, Heads, Guard, Body, Pragma),
	assert(      rule( Handler, N, Name, Heads, Guard, Body, Pragma)).

proper_rule( Term, Handler, N, Name, Heads, Guard, Body, Pragma) :-
	is_rule( Term, Name, Heads, Guard, Body, Pragma),
	!,
	proper_name( Name, Handler, N),
	proper_heads( Heads, Name, Handler),
	proper_pragma( Pragma, Name).
proper_rule( Term, _, N, _, _, _, _, _) :-
	raise_exception( compiler(syntax(Term,N))).

% --------------------------- syntax -----------------------------

%
% fail means syntax error
%
is_rule( (Name @ Rule), Name, Heads, Guard, Body, Pragma) :- !,
	nonvar( Rule),
	is_rule( Rule, Heads, Guard, Body, Pragma).
is_rule( Rule, _, Heads, Guard, Body, Pragma) :-
	is_rule( Rule, Heads, Guard, Body, Pragma).

is_rule( (Rule pragma Pragma), Heads, Guard, Body, Prag) :- !,
	nonvar( Rule),
	is_rule( Rule, Heads, Guard, Body),
	is_pragma( Pragma, Prag, []).
is_rule( Rule, Heads, Guard, Body, []) :-	% no pragma
	is_rule( Rule, Heads, Guard, Body).

is_rule( (Head <=> Rhs), Heads, Guard, Body) :-
	is_simpagation( Head, Heads, []),
	is_rhs( Rhs, Guard, Body).
is_rule( (Head ==> Rhs), Heads, Guard, Body) :-
	is_propagation( Head, Heads, []),
	is_rhs( Rhs, Guard, Body).

is_simpagation( Kill) --> {var(Kill)}, !, is_head( Kill, k).
is_simpagation( (Keep \ Kill)) --> !,
	is_head( Keep, r),
	is_head( Kill, k).
is_simpagation( Kill) -->
	is_head( Kill, k).

is_propagation( Head) --> {var(Head)}, !, is_head( Head, r).
is_propagation( (_ \ _)) --> !, {fail}.
is_propagation( Head) --> is_head( Head, r).

is_head( Head,	      Type) --> {var(Head)}, !, is_head( Head, Type, _).
is_head( (A,B),       Type) --> !, is_head( A, Type), is_head( B,Type).
is_head( (Head # Id), Type) --> !, is_head( Head, Type, Id).
is_head( Head,	      Type) --> is_head( Head, Type, _).

is_head( H, Type, Id) -->
	{
	    Term =.. [Type,H,Id]
	},
	[ Term ].

is_pragma( P) --> {var(P)}, !, [ P ].
is_pragma( (P,Ps)) --> !,
	is_pragma( P),
	is_pragma( Ps).
is_pragma( P) --> [ P ].

is_rhs( Body,		_,     _) :- var( Body), !, fail.
is_rhs( (Guard | Body), Guard, Body) :- !.
is_rhs( Body,		true,  Body).

% ---------------------- statical semantics ----------------------

proper_name( Name, _, N) :- var( Name), !, Name = rule(N).
proper_name( _,    _, _).

proper_heads( Heads, Rname, Handler) :-
	proper_heads_( Heads, Rname, Handler),
	proper_ids( Heads, Rname, Hts),
	sort( Hts, Htss),			% var < anything
	( Htss=[] -> true
	; Htss=[V], var(V) -> true
	; Htss=[T|_], nonvar(T) -> true
	;
	    raise_exception( compiler(wild_head(Rname)))
	).

proper_heads_( [],     _,     _).
proper_heads_( [H|Hs], Rname, Handler) :-
	proper_head( H, Rname, Handler),
	proper_heads_( Hs, Rname, Handler).

proper_head( Head, Rname, Handler) :-
	arg( 1, Head, Term),
	( var( Term) ->
	    true
	; functor( Term, F, A),
	  constraint( Handler, F/A) ->
	    true
	;
	    functor( Term, F, A),
	    findall( C, constraint(Handler,C), L),
	    raise_exception( compiler(undefined_constraint(F,A,Rname,L)))
	).

proper_ids( Heads, _, Hs) :-
	proper_ids_( Heads, Tids, Hs),
	list_to_ord_set( Tids, Ts),
	same_length( Tids, Ts),
	vars( Hs, Vhs),
	ord_intersection( Ts, Vhs, []),
	!.
proper_ids( _, Rname, _) :-
	raise_exception( compiler(bad_ids(Rname))).

proper_ids_( [],     [],     []).
proper_ids_( [X|Xs], [T|Ts], [H|Hs]) :-
	arg( 1, X, H),
	arg( 2, X, T),
	var( T),
	proper_ids_( Xs, Ts, Hs).

proper_pragma( [],     _).
proper_pragma( [P|Ps], Rname) :-
	( var( P) ->
	    raise_exception( compiler(pragma(P,Rname)))
	;
	    proper_pragma( Ps, Rname)
	).

% --------------------------- development ---------------------

cc( F/A) ?- !, integer(A), functor( C, F, A), cc( C).
cc( C) :-
	comp_constraint( C, _, user, Cls, []),
	member( Cl, Cls),		  % nl,nl,portray_clause(Cl),
	macro_exp( Cl, Cll),
	expansion( [Cll], [Cle|_], []),
	portray_clause( Cle),
	fail.

%
% No need to do this. Just for the looks.
%
macro_exp( (H0:-B), (H0:-B1)) :-
	prolog:get_module(H0, H, Module),
	nonvar(H),
	functor(H, F, _),
	atom(F),
	prolog:exp_vars(H, HV, Module, assert),
	prolog:wellformed_body(B, []/*undef layout*/, +, B1, _, HV, Module, Module, assert),
	!.
macro_exp( Clause, Clause).

end_of_file.


