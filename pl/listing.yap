
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
  * @file   pl/listing.yap
  * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
  * @date   Thu Oct 19 12:05:19 2017
  *
  * @brief  list predicates in a module
  *
  */


/*:- system_module( '$_listing', [listing/0,
        listing/1,
        portray_clause/1,
        portray_clause/2], []).
*/

/**
  * @defgroup listingGroup  List predicates in a module
  *
  * @ingroup builtins
  *
  * @{
*/



/** @brief listing : Listing clauses in the database
 *
 */

/** @pred  listing


Lists in the current output stream all the clauses for which source code
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


*/
listing(MV) :-
    current_output(Stream),
    listing(Stream, MV).

listing(Stream, MV) :-
    '$yap_strip_module'( MV, M, I),
    listing_(Stream, I, M),
    !.

listing_(Stream, V, M) :-
    var(V),
    !,
    '$mlisting'(Stream,  V, M).
listing_(_Stream, [], _) :-
    !.
listing_(Stream, [MV|MVs], M) :-
    !,
    '$mlisting'(Stream,  MV, M),
    listing_(Stream, MVs, M).
listing_(Stream, MV, M) :-
    '$mlisting'(Stream,  MV, M).

'$mlisting'(Stream, MV, M) :-
    ( var(MV) ->
      MV = NA,
      '$do_listing'(Stream, M, NA)
    ;
    atom(MV) ->
    MV/_ = NA,
    '$do_listing'(Stream, M, NA)
    ;
    MV = N//Ar ->
    ( integer(Ar) -> Ar2 is Ar+2, NA is N/Ar2 ;
      '$do_listing'(Stream, NA/Ar2, M), Ar2 >= 2, Ar is Ar2-2 )
    ;
    MV = N/Ar,
    ( atom(N) -> true ; var(N) ),
    ( integer(Ar) -> true ; var(Ar) )  -> '$do_listing'(Stream, M, MV)
    ;
    MV = M1:PP ->  '$mlisting'(Stream, PP, M1)
    ;
    '$do_error'(type_error(predicate_indicator,MV),listing(Stream, MV) )
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
'$funcspec'(Name,Name,0) :- atom(Name), !.
'$funcspec'(Name,_,_) :-
    '$do_error'(domain_error(predicate_spec,Name),listing(Name)).

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
    '$is_metapredicate'(Pred, M),
    functor( Pred, Name, Arity ),
    prolog:'$meta_predicate'(Name,M,Arity,PredDef),
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
    '$predicate_flags'(Pred,M,Flags,Flags),
    % has to be dynamic, source, or log update.
    Flags /\ 0x08402000 =\= 0,
    clause(M:Pred, Body, _),
    '$current_module'(Mod),
    ( M \= Mod -> H = M:Pred ; H = Pred ),
    portray_clause(Stream,(H:-Body)),
    fail.

/** @pred  portray_clause(+ _S_,+ _C_)

Write clause  _C_ on stream  _S_ as if written by listing/0.
*/
portray_clause(Stream, Clause) :-
    copy_term_nat(Clause, CopiedClause),
    '$beautify_vs'(CopiedClause),
    '$portray_clause'(Stream, CopiedClause),
    fail.
portray_clause(_, _).

/** @pred  portray_clause(+ _C_)

Write clause  _C_ as if written by listing/0.

*/
portray_clause(Clause) :-
    current_output(Stream),
    portray_clause(Stream, Clause).

'$portray_clause'(Stream, (Pred :- true)) :- !,
    format(Stream, '~q.~n', [Pred]).
'$portray_clause'(Stream, (Pred:-Body)) :- !,
    format(Stream, '~q :-', [Pred]),
    '$write_body'(Body, 3, ',', Stream),
    format(Stream, '.~n', []).
'$portray_clause'(Stream, Pred) :-
    format(Stream, '~q.~n', [Pred]).

'$write_body'(X,I,T,Stream) :-
    var(X), !,
    '$beforelit'(T,I,Stream),
    writeq(Stream, '_').
'$write_body'((P,Q), I, T, Stream) :-
    !,
    '$write_body'(P,I,T, Stream),
    put(Stream, 0',),  %
    '$write_body'(Q,I,',',Stream).
'$write_body'((P->Q;S),I,_, Stream) :-
    !,
    format(Stream, '~n~*c(',[I,0' ]),
    I1 is I+2,
    '$write_body'(P,I1,'(',Stream),
    format(Stream, '~n~*c->',[I,0' ]),
    '$write_disj'((Q;S),I,I1,'->',Stream),
    format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P->Q|S),I,_,Stream) :-
    !,
    format(Stream, '~n~*c(',[I,0' ]),
    I1 is I+2,
    '$write_body'(P,I,'(',Stream),
    format(Stream, '~n~*c->',[I,0' ]),
    '$write_disj'((Q|S),I,I1,'->',Stream),
    format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P->Q),I,_,Stream) :-
    !,
    format(Stream, '~n~*c(',[I,0' ]),
    I1 is I+2,
    '$write_body'(P,I1,'(',Stream),
    format(Stream, '~n~*c->',[I,0' ]),
    '$write_body'(Q,I1,'->',Stream),
    format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P;Q),I,_,Stream) :-
    !,
    format(Stream, '~n~*c(',[I,0' ]),
    I1 is I+2,
    '$write_disj'((P;Q),I,I1,'->',Stream),
    format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P|Q),I,_,Stream) :-
    !,
    format(Stream, '~n~*c(',[I,0' ]),
    I1 is I+2,
    '$write_disj'((P|Q),I,I1,'->',Stream),
    format(Stream, '~n~*c)',[I,0' ]).
'$write_body'(X,I,T,Stream) :-
    '$beforelit'(T,I,Stream),
    writeq(Stream,X).




'$write_disj'((Q;S),I0,I,C,Stream) :- !,
    '$write_body'(Q,I,C,Stream),
    format(Stream, '~n~*c;',[I0,0' ]),
    '$write_disj'(S,I0,I,';',Stream).
'$write_disj'((Q|S),I0,I,C,Stream) :- !,
    '$write_body'(Q,I,C,Stream),
    format(Stream, '~n~*c|',[I0,0' ]),
    '$write_disj'(S,I0,I,'|',Stream).
'$write_disj'(S,_,I,C,Stream) :-
    '$write_body'(S,I,C,Stream).

'$beforelit'('(',_,Stream) :-
    !,
    format(Stream,' ',[]).
'$beforelit'(_,I,Stream) :- format(Stream,'~n~*c',[I,0' ]). %'


'$beautify_vs'(T) :-
    non_singletons_in_term(T,[],Fs),
    '$vv_transform'(Fs,1),
    term_variables(T, NFs),
    '$v_transform'(NFs).

'$v_transform'([]).
'$v_transform'(['$VAR'(-1)|L]) :-
    '$v_transform'(L).

'$vv_transform'([],_) :- !.
'$vv_transform'(['$VAR'(M)|L],M) :-
    N is M+1,
    '$vv_transform'(L,N).

%% @}
