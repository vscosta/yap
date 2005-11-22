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
* File:		myddas.yap	                                         *
* Last rev:							         *
* mods:									 *
* comments:	Global predicates for the MyDDAS Interface		 *
*									 *
*************************************************************************/

:- module(myddas,[
		  db_verbose/1,
		  db_is_database_predicate/3,
		  db_module/1,
		  db_stats/2
		 ]).

:- use_module(myddas_util_predicates).
:- use_module(myddas_errors).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_verbose/1
%
%
db_verbose(X):-
	var(X),!,
	get_value(db_verbose,X).
db_verbose(1):-!,
	set_value(db_verbose,1).
db_verbose(_):-
	set_value(db_verbose,0).
%default value
:- db_verbose(0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_is_database_predicate/3
%
%
db_is_database_predicate(PredName,Arity,Module):-
	'$error_checks'(db_is_database_predicate(PredName,Arity,Module)),
	c_db_check_if_exists_pred(PredName,Arity,Module).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_module/1
%
%
db_module(X):-
	var(X),!,
	get_value(db_module,X).
db_module(ModuleName):-
	set_value(db_module,ModuleName).
%default value
:- db_module(user).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_stats/2
%
%
db_stats(Connection,List):-
	'$get_value'(Connection,Conn),
	NumberOfStats = 2,
	'$make_a_list'(NumberOfStats,List),
	c_db_my_stats(Conn,List).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%