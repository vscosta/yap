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
        current_predicate(_,Pred),
        '$list_clauses'(Pred).
listing.


listing(V) :- var(V), !.       % ignore variables
listing(M:V) :- !,
	'$mod_switch'(M,listing(V)).
listing([]) :- !.
listing([X|Rest]) :-
        !,
        listing(X),
        listing(Rest).
listing(X) :-
        '$funcspec'(X,Name,Arity),
        current_predicate(Name,Pred),
        functor(Pred,Name,Arity),
        '$list_clauses'(Pred).
listing(_).

'$funcspec'(Name/Arity,Name,Arity) :- !, atom(Name).
'$funcspec'(Name,Name,_) :- atom(Name), !.
'$funcspec'(Name,_,_) :- write('! Invalid procedure specification : '),
			write(Name), nl.

'$list_clauses'(Pred) :-
	( '$recordedp'(Pred,_,_) -> nl ),
	fail.
'$list_clauses'(Pred) :-
        '$recordedp'(Pred,(Pred:-Body),_),
	'$beautify_vars'((Pred:-Body)),
        '$write_clause'(Pred,Body),
        fail.

portray_clause((Pred:-Body)) :- !,
	'$beautify_vars'((Pred:-Body)),
	'$write_clause'(Pred,Body).
portray_clause(Pred) :- 
	'$beautify_vars'((Pred:-true)),
	'$write_clause'(Pred,true).

'$write_clause'(Head,Body) :-
        writeq(Head),
        ( Body = true  ;
                tab(1), write((:-)),
                '$write_body'(Body,3,',')
        ),
        put("."), nl,
        !.

'$write_body'(X,I,T) :- var(X), !, '$beforelit'(T,I), writeq('_').
'$write_body'((P,Q), IO, T) :-
        !,
        '$write_body'(P,IO,T),
        put(","),
        '$aftercomma'(T,IO,I),
        '$write_body'(Q,I,',').
'$write_body'((P->Q;S),I,T) :-
	!,
	nl, tab(I-2), put("("),
	'$write_body'(P,I,'('),
	put("-"), put(">"),
	'$write_body'(Q,I,'->'),
	put(";"),
	'$write_body'(S,I,';'),
	tab(1), put(")").
'$write_body'((P->Q|S),I,T) :-
	!,
	nl, tab(I-2), put("("),
	'$write_body'(P,I,'('),
	put("-"), put(">"),
	'$write_body'(Q,I,'->'),
	put("|"),
	'$write_body'(S,I,'|'),
	tab(1), put(")").
'$write_body'((P->Q),I,T) :-
	!,
	nl, tab(I-2), put("("),
        '$write_body'(P,I,'('),
        put("-"), put(">"),
        '$write_body'(Q,I,'->'),
        tab(1), put(")").
'$write_body'((P;Q),I,T) :-
        !,
        nl, tab(I-2), put("("),
        '$write_body'(P,I,'('),
        put(";"),
        '$write_body'(Q,I,';'),
        tab(1), put(")").
'$write_body'((P;Q),I,T) :-
        !,
        nl, tab(I-2), put("("),
        '$write_body'(P,I,'('),
        put(";"),
        '$write_body'(Q,I,';'),
        tab(1), put(")").
'$write_body'((P|Q),I,T) :-
        !,
        nl, tab(I-2), put("("),
        '$write_body'(P,I,'('),
        put("|"),
        '$write_body'(Q,I,'|'),
        tab(1), put(")").
'$write_body'((P|Q),I,T) :-
        !,
        nl, tab(I-2), put("("),
        '$write_body'(P,I,'('),
        put("|"),
        '$write_body'(Q,I,'|'),
        tab(1), put(")").
'$write_body'(X,I,T) :-
        '$beforelit'(T,I),
        writeq(X).

'$aftercomma'(',',I,I) :- !.
'$aftercomma'(_,I0,I) :- I is I0+3.

'$beforelit'('(',_) :- !, tab(1).
'$beforelit'(_,I) :- nl, tab(I).

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
'$list_transform'([X,Y|L],M) :- X == Y, X = '$VAR'(M), !, N is M+1,
			'$list_transform'(L,N).
'$list_transform'('$VAR'(-1).L,M) :- !, '$list_transform'(L,M).
'$list_transform'(X.L,M) :- '$list_transform'(L,M).
