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
* File:		dbload.yap						 *
* Last rev:	8/2/88							 *
* mods:									 *
* comments:	Compact Loading of Facts in YAP				 *
*									 *
*****err********************************************************************/

%% @file dbload.yap

:- module('$db_load',
	  []).

:- use_system_module( '$_errors', ['$do_error'/2]).

:- use_system_module( attributes, [get_module_atts/2,
        put_module_atts/2]).

%%
% @defgroup YAPBigLoad Loading Large Tables
% @ingroup YAPConsulting
% @{
% @brief Fast and  Exo Loading
%

/*!
 * @pred load_mega_clause( +Stream ) is detail
 * Load a single predicare composed of facts with the same size.
 */
load_mega_clause( Stream ) :-
%    	line_spec( Stream, Line),
			repeat,
			( fact( Stream ), fail ;
			  stream_property(Stream, at_end_of_file( on )) ).

'$input_lines'(R, csv, Lines ) :-
    '$process_lines'(R, Lines, _Type ),
	close(R).

/*!
 * @pred load_db( +Files ) is det
 * Load files each one containing as single predicare composed of facts with the same size.
 */
prolog:load_db(Fs) :-
        '$current_module'(M0),
	retractall(dbloading(_Na,_Arity,_M,_T,_NaAr,_)),
	prolog_flag(agc_margin,Old,0),
	dbload(Fs,M0,load_db(Fs)),
	load_facts,
	prolog_flag(agc_margin,_,Old),
	clean_up.

dbload(Fs, _, G) :-
	var(Fs),
	'$do_error'(instantiation_error,G).
dbload([], _, _) :- !.
dbload([F|Fs], M0, G) :- !,
	dbload(F, M0, G),
	dbload(Fs, M0, G).
dbload(M:F, _M0, G) :- !,
	dbload(F, M, G).
dbload(F, M0, G) :-
	atom(F), !,
	do_dbload(F, M0, G).
dbload(F, _, G) :-
	'$do_error'(type_error(atom,F),G).

do_dbload(F0, M0, _G) :-
	absolute_file_name(F0, [expand(true),file_type(prolog),access(read)], F),
	assert(dbprocess(F, M0)),
	open(F, read, R),
	check_dbload_stream(R, M0),
	close(R).


check_dbload_stream(R, M0) :-
	repeat,
	catch(read(R,T), _, fail),
	( T = end_of_file -> !;
	    dbload_count(T, M0),
	    fail
	).

dbload_count(T0, M0) :-
	'$yap_strip_module'(M0:T0,M,T),
	functor(T,Na,Arity),
%	dbload_check_term(T),
	(
	    dbloading(Na,Arity,M,_,NaAr,_) ->
	    nb_getval(NaAr,I0),
	    I is I0+1,
	    nb_setval(NaAr,I)
	;
	    atomic_concat([Na,'__',Arity,'__',M],NaAr),
	    assert(dbloading(Na,Arity,M,T,NaAr,0)),
	    nb_setval(NaAr,1)
	).


load_facts :-
	!, % yap_flag(exo_compilation, on), !.
	load_exofacts.
load_facts :-
	retract(dbloading(Na,Arity,M,T,NaAr,_)),
	nb_getval(NaAr,Size),
	prolog:'$dbload_get_space'(T, M, Size, Handle),
	assertz(dbloading(Na,Arity,M,T,NaAr,Handle)),
	nb_setval(NaAr,0),
	fail.
load_facts :-
	dbprocess(F, M),
	open(F, read, R),
	dbload_add_facts(R, M),
	close(R),
	fail.
load_facts.

dbload_add_facts(R, M) :-
	repeat,
	catch(read(R,T), _, fail),
	( T = end_of_file -> !;
	    dbload_add_fact(T, M),
	    fail
	).

dbload_add_fact(T0, M0) :-
	'$yap_strip_module'(M0:T0,M,T),
	functor(T,Na,Arity),
	dbloading(Na,Arity,M,_,NaAr,Handle),
	nb_getval(NaAr,I0),
	I is I0+1,
	nb_setval(NaAr,I),
	prolog:'$dbassert'(T,Handle,I0).

load_exofacts :-
	retract(dbloading(Na,Arity,M,T,NaAr,_)),
	nb_getval(NaAr,Size),
	exo_db_get_space(T, M, Size, Handle),
	assertz(dbloading(Na,Arity,M,T,NaAr,Handle)),
	nb_setval(NaAr,0),
	fail.
load_exofacts :-
	dbprocess(F, M),
	open(F, read, R),
	exodb_add_facts(R, M),
	close(R),
	fail.
load_exofacts.

exodb_add_facts(R, M) :-
	repeat,
	catch(protected_exodb_add_fact(R, M), _, fail),
	!.

protected_exodb_add_fact(R, M) :-
	repeat,
	read(R,T),
	( T == end_of_file -> !;
	    exodb_add_fact(T, M),
	    fail
	).

exodb_add_fact(T0, M0) :-
	'$yap_strip_module'(T0,M0,T,M),
	functor(T,Na,Arity),
	dbloading(Na,Arity,M,_,NaAr,Handle),
	nb_getval(NaAr,I0),
	I is I0+1,
	nb_setval(NaAr,I),
	exoassert(T,Handle,I0).

clean_up :-
	retractall(dbloading(_,_,_,_,_,_)),
	retractall(dbprocess(_,_)),
	fail.

clean_up.

%% @}
