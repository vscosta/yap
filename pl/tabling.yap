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
* File:		tabling.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	support tabling predicates				 *
*									 *
*************************************************************************/

:- meta_predicate table(:), tabling_mode(:), abolish_table(:), show_table(:), show_table_stats(:).



/******************
*     table/1     *
******************/

table(P) :- '$current_module'(M), '$table'(P,M).

'$table'(P,M) :- var(P), !, '$do_error'(instantiation_error,table(M:P)).
'$table'(M:P,_) :- !, '$table'(P,M).
'$table'([],_) :- !.
'$table'([H|T],M) :- !, '$table'(H,M), '$table'(T,M).
'$table'((P1,P2),M) :- !, '$table'(P1,M), '$table'(P2,M).
'$table'(A/N,M) :- integer(N), atom(A), !, functor(T,A,N), '$declare_tabled'(T,M).
'$table'(P,M) :- '$do_error'(type_error(callable,P),table(M:P)).

'$declare_tabled'(T,M) :- '$undefined'(T,M), !, '$do_table'(T,M).
'$declare_tabled'(T,M) :- '$flags'(T,M,F,F), F /\ 0x1991F880 =:= 0, !, '$do_table'(T,M).
'$declare_tabled'(T,M) :- functor(T,A,N), '$do_error'(permission_error(modify,table,M:A/N),table(M:A/N)).



/*************************
*     tabling_mode/2     *
*************************/

tabling_mode(Pred,Options) :- 
   '$current_module'(Mod), 
   '$tabling_mode'(Mod,Pred,Options).

'$tabling_mode'(Mod,Pred,Options) :- 
   var(Pred), !, 
   '$do_error'(instantiation_error,tabling_mode(Mod:Pred,Options)).
'$tabling_mode'(_,Mod:Pred,Options) :- !, 
   '$tabling_mode'(Mod,Pred,Options).
'$tabling_mode'(_,[],_) :- !.
'$tabling_mode'(Mod,[HPred|TPred],Options) :- !,
   '$tabling_mode'(Mod,HPred,Options),
   '$tabling_mode'(Mod,TPred,Options).
'$tabling_mode'(Mod,PredName/PredArity,Options) :- 
   atom(PredName), 
   integer(PredArity), !, 
   functor(PredFunctor,PredName,PredArity),
   '$flags'(PredFunctor,Mod,Flags,Flags),
   (Flags /\ 0x000040 =\= 0, !, '$set_tabling_mode'(Mod,PredFunctor,Options)
   ;
   '$do_error'(domain_error(table,Mod:PredName/PredArity),tabling_mode(Mod:PredName/PredArity,Options))).
'$tabling_mode'(Mod,Pred,Options) :- 
   '$do_error'(type_error(callable,Pred),tabling_mode(Mod:Pred,Options)).

'$set_tabling_mode'(Mod,PredFunctor,Options) :-
   var(Options), !, 
   '$do_tabling_mode'(Mod,PredFunctor,Options).
'$set_tabling_mode'(Mod,PredFunctor,[]) :- !.
'$set_tabling_mode'(Mod,PredFunctor,[HOption|TOption]) :- !,
   '$set_tabling_mode'(Mod,PredFunctor,HOption),
   '$set_tabling_mode'(Mod,PredFunctor,TOption).
'$set_tabling_mode'(Mod,PredFunctor,Option) :- 
   (Option = batched ; Option = local ; Option = exec_answers ; Option = load_answers), !, 
   '$do_tabling_mode'(Mod,PredFunctor,Option).
'$set_tabling_mode'(Mod,PredFunctor,Options) :- 
   functor(PredFunctor,PredName,PredArity), 
   '$do_error'(domain_error(flag_value,tabling_mode+Options),tabling_mode(Mod:PredName/PredArity,Options)).



/**************************
*     abolish_table/1     *
**************************/

abolish_table(P) :- '$current_module'(M), '$abolish_table'(P,M).

'$abolish_table'(P,M) :- var(P), !, '$do_error'(instantiation_error,abolish_table(M:P)).
'$abolish_table'(M:P,_) :- !, '$abolish_table'(P,M).
'$abolish_table'([],_) :- !.
'$abolish_table'([H|T],M) :- !, '$abolish_table'(H,M), '$abolish_table'(T,M).
'$abolish_table'((P1,P2),M) :- !, '$abolish_table'(P1,M), '$abolish_table'(P2,M).
'$abolish_table'(A/N,M) :- integer(N), atom(A), !, functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$do_abolish_table'(T,M)
	;
	'$do_error'(domain_error(table,M:A/N),abolish_table(M:A/N))).
'$abolish_table'(P,M) :- '$do_error'(type_error(callable,P),abolish_table(M:P)).



/***********************
*     show_table/1     *
***********************/

show_table(P) :-	'$current_module'(M), '$show_table'(P,M).

'$show_table'(P,M) :- var(P), !, '$do_error'(instantiation_error,show_table(M:P)).
'$show_table'(M:P,_) :- !, '$show_table'(P,M).
'$show_table'([],_) :- !.
'$show_table'([H|T],M) :- !, '$show_table'(H,M), '$show_table'(T,M).
'$show_table'((P1,P2),M) :- !, '$show_table'(P1,M), '$show_table'(P2,M).
'$show_table'(A/N,M) :- integer(N), atom(A), !,	functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$do_show_table'(T,M)
	;
	'$do_error'(domain_error(table,M:A/N),show_table(M:A/N))).
'$show_table'(P,M) :- '$do_error'(type_error(callable,P),show_table(M:P)).



/*****************************
*     show_table_stats/1     *
*****************************/

show_table_stats(P) :- '$current_module'(M), '$show_table_stats'(P,M).

'$show_table_stats'(P,M) :- var(P), !, '$do_error'(instantiation_error,show_table_stats(M:P)).
'$show_table_stats'(M:P,_) :- !, '$show_table_stats'(P,M).
'$show_table_stats'([],_) :- !.
'$show_table_stats'([H|T],M) :- !, '$show_table_stats'(H,M), '$show_table_stats'(T,M).
'$show_table_stats'((P1,P2),M) :- !, '$show_table_stats'(P1,M), '$show_table_stats'(P2,M).
'$show_table_stats'(A/N,M) :- atom(A), integer(N), !, functor(T,A,N), '$flags'(T,M,F,F),
	(F /\ 0x000040 =\= 0, !, '$do_show_table_stats'(T,M)
	;
	'$do_error'(domain_error(table,M:A/N),show_table_stats(M:A/N))).
'$show_table_stats'(P,M) :- '$do_error'(type_error(callable,P),show_table_stats(M:P)).
