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
* File:		myddas_top_level.yap	                                 *
* Last rev:							         *
* mods:									 *
* comments:	MYDDAS Top Level predicates		                 *
*									 *
*************************************************************************/

:- module(myddas_top_level,[
		  db_top_level/4
		 ]).

:- use_module(myddas_mysql,[
			    db_my_result_set/1
			    ]).

db_top_level(Connection,_,_,_):-
	%'$error_checks'(db_open(mysql,Connection,Host/Db,User,Password)),
	get_value(Connection,Con),
	Con \= [],!,
	c_db_connection_type(Con,mysql),
	db_my_result_set(Mode),
	c_db_top_level(Con,Mode).

db_top_level(Connection,Host/Db,User,Password):-
	%'$error_checks'(db_open(mysql,Connection,Host/Db,User,Password)),
	c_db_my_connect(Host,User,Password,Db,Con),
	set_value(Connection,Con),
	db_my_result_set(Mode),
	c_db_top_level(Con,Mode).
