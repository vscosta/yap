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

/* listing : Listing clauses in the database

*/


listing :-
	current_output(Stream),
	'$current_module'(Mod),
        '$current_predicate_no_modules'(Mod,_,Pred),
        '$list_clauses'(Stream,Mod,Pred).
listing.


listing(V) :-
	current_output(Stream),
	'$current_module'(M),
	'$listing'(V,M,Stream).

'$listing'(V,Mod,Stream) :- var(V), !,
	'$current_predicate_no_modules'(Mod,_,Pred),
        '$list_clauses'(Stream,Mod,Pred).
'$listing'(M:V,_,Stream) :- !,
	'$listing'(V,M,Stream).
'$listing'([],_,_) :- !.
'$listing'([X|Rest], M, Stream) :-
        !,
        '$listing'(X, M, Stream),
        '$listing'(Rest, M, Stream).
'$listing'(X, M, Stream) :-
        '$funcspec'(X,Name,Arity),
        '$current_predicate_no_modules'(M,Name,Pred),
        functor(Pred,Name,Arity),
        '$list_clauses'(Stream,M,Pred).
'$listing'(_,_,_).

'$funcspec'(Name/Arity,Name,Arity) :- !, atom(Name).
'$funcspec'(Name,Name,_) :- atom(Name), !.
'$funcspec'(Name,_,_) :-
	'$do_error'(domain_error(predicate_spec,Name),listing(Name)).

'$list_clauses'(Stream, M, Pred) :-
	'$flags'(Pred,M,Flags,Flags),
	% has to be dynamic, source, or log update.
	Flags /\ 0x08402000 =\= 0,
	'$clause'(Pred, M, Body, _),
	'$portray_clause'(Stream,(Pred:-Body)),
        fail.

portray_clause(Stream, Clause) :-
	copy_term_nat(Clause, CopiedClause),
	'$portray_clause'(Stream, CopiedClause),
	fail.
portray_clause(_, _).

portray_clause(Clause) :-
        current_output(Stream),
	copy_term_nat(Clause, CopiedClause),
	'$portray_clause'(Stream, CopiedClause),
	fail.
portray_clause(_).

'$portray_clause'(Stream, (Pred :- true)) :- !,
	'$beautify_vars'(Pred),
	writeq(Stream, Pred),
	format(Stream, '.~n', []).
'$portray_clause'(Stream, (Pred:-Body)) :- !,
	'$beautify_vars'((Pred:-Body)),
	writeq(Stream, Pred),
	format(Stream, ' :-', []),
	'$write_body'(Body, 3, ',', Stream),
	format(Stream, '.~n', []).
'$portray_clause'(Stream, Pred) :- !,
	'$beautify_vars'(Pred),
	writeq(Stream, Pred),
	format(Stream, '.~n', []).

'$write_body'(X,I,T,Stream) :- var(X), !,
	'$beforelit'(T,I,Stream),
	writeq(Stream, '_').
'$write_body'((P,Q), I, T, Stream) :-
        !,
        '$write_body'(P,I,T, Stream),
        put(Stream, 0',),
        '$write_body'(Q,I,',',Stream).
'$write_body'((P->Q;S),I,_, Stream) :-
	!,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
	'$write_body'(P,I1,'(',Stream),
	format(Stream, ' ->',[]),
	'$write_disj'((Q;S),I,I1,'->',Stream),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P->Q|S),I,_,Stream) :-
	!,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
	'$write_body'(P,I,'(',Stream),
	format(Stream, ' ->',[]),
	'$write_disj'((Q|S),I,I1,'->',Stream),
	format(Stream, '~n~*c)',[I,0' ]).
'$write_body'((P->Q),I,_,Stream) :-
	!,
	format(Stream, '~n~*c(',[I,0' ]),
	I1 is I+2,
        '$write_body'(P,I1,'(',Stream),
	format(Stream, ' ->',[]),
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
	

'$beforelit'('(',_,Stream) :- !, format(Stream,' ',[]).
'$beforelit'(_,I,Stream) :- format(Stream,'~n~*c',[I,0' ]).

'$beautify_vars'(T) :-
	'$list_get_vars'(T,[],L),
	msort(L,SL),
	'$list_transform'(SL,0).


'$list_get_vars'(V,L,[V|L] ) :- var(V), !.
'$list_get_vars'(Atomic, M, M) :-
	primitive(Atomic), !.
'$list_get_vars'([Arg|Args], M, N) :-  !,
	'$list_get_vars'(Arg, M, K),
	'$list_get_vars'(Args, K, N).
'$list_get_vars'(Term, M, N) :-
	Term =.. [_|Args],
	'$list_get_vars'(Args, M, N).

'$list_transform'([],_) :- !.
'$list_transform'([X,Y|L],M) :-
	X == Y,
	X = '$VAR'(M),
	!,
	N is M+1,
	'$list_transform'(L,N).
'$list_transform'('$VAR'(-1).L,M) :- !,
	'$list_transform'(L,M).
'$list_transform'(_.L,M) :-
	'$list_transform'(L,M).

