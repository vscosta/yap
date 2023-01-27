/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		listing.pl						 *
* Last rev:								 *
* mods:									 *
* comments:	listing a prolog program				 *
*									 *
*************************************************************************/

/**
  * @file   listing.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:05:19 2017
  * 
  * @brief  list predicates in a module
  */

/** @defgroup Listing  list predicates in a module
  * @ingroup Builtins
  * 
  * @{
*/

:- system_module( '$_listing',
		  [],
		  [listing/0,
		   listing/1,
		   listing/2,
		   portray_clause/1,
		   portray_clause/2]
		).

:- use_system_module( '$_errors', [throw_error/2]).

:- use_system_module( '$_preds', ['$clause'/4,
        '$current_predicate'/4]).

/* listing : Listing clauses in the database

*/

/** @pred  listing


vxuLists in the current output stream all the clauses for which source code
is available (these include all clauses for dynamic predicates and
clauses for static predicates compiled when source mode was `on`).

- listing/0 lists in the current module

- listing/1 receives a generalization of the predicate indicator:

    + `listing(_)` will list the whole sources.

    + `listing(lists:_)` will list the module lists.

    + `listing(lists:append)` will list all `append` predicates in the module lists.

    + `listing(lists:append/_)` will do the same.

    +  listing(lists:append/3)` will list the popular `append/3` predicate in the module lists.

- listing/2 is similar to listing/1, but t he first argument is a stream reference.

The `listing` family of built-ins does not enumerate predicates whose
name starts with a `$` character.

*/
listing :-
    current_output(Stream),
    '$current_module'(Mod),
    \+ system_module(Mod),
    Mod \= prolog,
    Mod \= system,
    \+ '$hidden_atom'( Mod ),
    current_predicate( Name, Mod:Pred ),
    \+ '$undefined'(Pred, Mod), % skip predicates exported from prolog.
    functor(Pred,Name,Arity),
    '$listing'(Name,Arity,Mod,Stream),
    fail.
listing.

/** @pred  listing(+ _P_)

Lists predicate  _P_ if its source code is available.
 If _P_ is unbound list all predicates in the current source module.
 If _P_ is of the form _M_:_P'_ use _M_ as source module.


*/
listing(MV) :-
    current_output(Stream),
    listing(Stream, MV).

/** @pred  listing(Stream, + _P_)

Lists predicate  _P_ if its source code is available.


*/
listing(Stream, MV) :-
    strip_module( MV, M, I),
    '$mlisting'(Stream, I, M).
listing(_Stream, []) :- !.
listing(Stream, [MV|MVs]) :- !,
    listing(Stream,  MV),
    listing(Stream, MVs).

'$mlisting'(Stream, MV, M) :-
    ( var(MV) ->
	  MV = NA,
	  '$do_listing'(Stream, M, NA)
	 ;
      atom(MV) ->
	  MV/_ = NA,
	  '$do_listing'(Stream, M, NA)
	 ;
	 MV = N//Ar -> ( integer(Ar) -> Ar2 is Ar+2, NA is N/Ar2 ; '$do_listing'(Stream, NA/Ar2, M), Ar2 >= 2, Ar is Ar2-2 )
	 ;
      MV = N/Ar, ( atom(N) -> true ; var(N) ), ( integer(Ar) -> true ; var(Ar) )  ->
	  '$do_listing'(Stream, M, MV)
	 ;
      MV = M1:PP ->  '$mlisting'(Stream, PP, M1)
	 ;
     throw_error(type_error(predicate_indicator,MV),listing(Stream, MV) )
    ).

'$do_listing'(Stream, M, Name/Arity) :-
    ( current_predicate(Name, M:Pred),
      \+ '$is_opaque_predicate'(Pred,M),
      functor( Pred, Name, Arity),
      \+ '$undefined'(Pred, M),
      '$listing'(Name,Arity,M,Stream),
      fail
   ;
   true
   ).

%
% at this point we are ground and we know who we want to list.
%
'$listing'(Name, Arity, M, Stream) :-
	% skip by default predicates starting with $
        functor(Pred,Name,Arity),
        '$list_clauses'(Stream,M,Pred).
'$listing'(_,_,_,_).

'$funcspec'(Name/Arity,Name,Arity) :- !, atom(Name).
'$funcspec'(Name,Name,_) :- atom(Name), !.
'$funcspec'(Name,_,_) :-
	throw_error(domain_error(predicate_spec,Name),listing(Name)).

'$list_clauses'(Stream, M, Pred) :-
	'$predicate_flags'(Pred,M,Flags,Flags),
	(Flags /\ 0x48602000 =\= 0
	->
	  nl(Stream),
	  fail
	;
          !
        ).
'$list_clauses'(Stream, M, Pred) :-
    ( '$is_dynamic'(Pred, M) -> true ; '$is_log_updatable'(Pred, M) ),
    functor( Pred, N, Ar ),
    '$current_module'(Mod),
    (
	M == Mod
    ->
      format( Stream, ':- dynamic ~q/~d.~n', [N,Ar])
    ;
      format( Stream, ':- dynamic ~q:~q/~d.~n', [M,N,Ar])
     ),
     fail.
'$list_clauses'(Stream, M, Pred) :-
    '$is_thread_local'(Pred, M),
    functor( Pred, N, Ar ),
    '$current_module'(Mod),
    (
	M == Mod
    ->
      format( Stream, ':- thread_local ~q/~d.~n', [N,Ar])
    ;
      format( Stream, ':- thread_local ~q:~q/~d.~n', [M,N,Ar])
     ),
     fail.
'$list_clauses'(Stream, M, Pred) :-
    '$is_multifile'(Pred, M),
    functor( Pred, N, Ar ),
    '$current_module'(Mod),
    (
	M == Mod
    ->
      format( Stream, ':- multifile ~q/~d.~n', [N,Ar])
    ;
      format( Stream, ':- multifile ~q:~q/~d.~n', [M,N,Ar])
     ),
     fail.
'$list_clauses'(Stream, M, Pred) :-
   '$is_meta_predicate'(Pred, M),
    functor( Pred, Name, Arity ),
    functor( PredDef, Name, Arity ),
    (recorded('$m', meta_predicate(M,PredDef),_);recorded('$m', meta_predicate(prolog,PredDef),_)),
    '$current_module'(Mod),
    (
	M == Mod
    ->
      format( Stream, ':- ~q.~n', [PredDef])
    ;
      format( Stream, ':- ~q:~q.~n', [M,PredDef])
     ),
     fail.
'$list_clauses'(Stream, _M, _Pred) :-
        nl( Stream ),
        fail.
'$list_clauses'(Stream, M, Pred) :-
    '$predicate_type'(Pred,M,Type),
    (Type == source_procedure -> true ;
     Type == updatable_procedure -> true ;
     Type == exo_procedure -> true ;
     Type == mega_procedure -> true
     ),
	'$clause'(Type,Pred, M, Body, _),
	'$current_module'(Mod),
	( M \= Mod -> H = M:Pred ; H = Pred ),
	term_variable_occurrences((H:-Body),VarTFound),
	msort(VarTFound, VarTSorted),
	'$variable_names'(VarTSorted,_,0,Names,[]),
	'$portray_clause'(Stream,(H:-Body),Names),
        fail.

/** @pred  portray_clause(+ _S_,+ _C_)

Write clause  _C_ on stream  _S_ as if written by listing/0.
*/
portray_clause(Stream, Clause) :-
    term_variable_occurrences(Clause,VarTFound),
    msort(VarTFound, VarTSorted),
    '$variable_names'(VarTSorted,_,0,Names,[]),
 writeln(Names),   '$portray_clause'(Stream, Clause, Names),
    fail.
portray_clause(_, _).

'$variable_names'([],_,_) --> [].
'$variable_names'([V1,V2|L],V0,I0) -->
    {V1 \== V0, V1 == V2 },
    !,
{     '$name_generator'(I0,LName,[]),
     atom_codes(Name,LName),
     I is I0+1
    },
    [Name=V1],
    '$variable_names'(L,V1,I).
'$variable_names'([V1,V2|L],V0,I0) -->
    {V1 \== V0, V1 \== V2 },
    !,
    ['_'=V1],
    '$variable_names'([V2|L],V1,I0).
'$variable_names'([V1|L],_V0,I0) -->
    '$variable_names'(L,V1,I0).

'$name_generator'(I) -->
    {I > 26},
    !,
    {
	L is I // 26+"A",
	R is I mod 26
    },
    [L],
    '$name_generator'(R).
'$name_generator'(I) -->
    { 
	L is I+"A"
    },
    [L].

    /** @pred  portray_clause(+ _C_)

Write clause  _C_ as if written by listing/0.

*/
portray_clause(Clause) :-
    current_output(Stream),
    portray_clause(Stream, Clause).

'$portray_clause'(Stream, (Pred :- true),Names) :- !,
	format(Stream, '~W.~n', [Pred,[variable_names(Names),quoted(true)]]).
'$portray_clause'(Stream, (Pred:-Body), Names) :- !,
    format(Stream, '~W :-', [Pred,[variable_names(Names),quoted(true)]]),
    '$write_body'(Body, 3, ',', Stream, Names),
    format(Stream, '.~n', []).
'$portray_clause'(Stream, Pred,Names) :-
    format(Stream, '~W.~n', [Pred,[variable_names(Names),quoted(true)]]).

'$write_body'(X,I,T,Stream, Names) :-
    var(X),
    !,
    '$beforelit'(T,I,Stream),
    format(Stream, '~W.~n', [X,[variable_names(Names),quoted(true)]]).
'$write_body'((P,Q), I, T, Stream, Names) :-
        !,
        '$write_body'(P,I,T, Stream, Names),
        put(Stream, 0',),
        '$write_body'(Q,I,',',Stream, Names).
'$write_body'((P->Q;S),I,_, Stream,Names) :-
	!,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
	'$write_body'(P,I1,'(',Stream, Names),
	format(Stream, '~n~*c->',[I,0' ]),
	'$write_disj'((Q;S),I,I1,'->',Stream, Names),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P->Q|S),I,_,Stream, Names) :-
	!,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
	'$write_body'(P,I,'(',Stream,Names),
	format(Stream, '~n~*c->',[I,0' ]),
	'$write_disj'((Q|S),I,I1,'->',Stream, Names),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P->Q),I,_,Stream, Names) :-
	!,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
        '$write_body'(P,I1,'(',Stream, Names),
	format(Stream, '~n~*c->',[I,0' ]),
        '$write_body'(Q,I1,'->',Stream, Names),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P;Q),I,_,Stream, Names) :-
        !,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
	'$write_disj'((P;Q),I,I1,'->',Stream, Names),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P|Q),I,_,Stream, Names) :-
        !,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
	'$write_disj'((P|Q),I,I1,'->',Stream, Names),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'(X,I,T,Stream, Names) :-
        '$beforelit'(T,I,Stream),
        write_term(Stream,X,[variable_names(Names),quoted(true)]).


'$write_disj'((Q;S),I0,I,C,Stream, Names) :-
    !,
	'$write_body'(Q,I,C,Stream, Names),
	format(Stream, '~n~*c;',[I0,0' ]),
	'$write_disj'(S,I0,I,';',Stream, Names).
'$write_disj'((Q|S),I0,I,C,Stream, Names) :-
    !,
	'$write_body'(Q,I,C,Stream, Names),
	format(Stream, '~n~*c|',[I0,0' ]),
	'$write_disj'(S,I0,I,'|',Stream, Names).
'$write_disj'(S,_,I,C,Stream, Names) :-
	'$write_body'(S,I,C,Stream, Names).


'$beforelit'('(',_,Stream) :-
    !,
    format(Stream,' ',[]).
'$beforelit'(_,I,Stream) :- format(Stream,'~n~*c',[I,0' ]).


%% @}
