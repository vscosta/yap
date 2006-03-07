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
* File:		myddas_mysql.yap					 *
* Last rev:								 *
* mods:									 *
* comments:	MySQL Predicates                 			 *
*									 *
*************************************************************************/

:- module(myddas_mysql,[
			db_my_open/5,
			db_my_close/1,
			db_my_import/3,
			db_my_view/3,
			db_my_insert/2,
			db_my_insert/3,
			db_my_result_set/1,
			db_datalog_describe/1,
			db_datalog_describe/2,
			db_describe/3,
			db_describe/2,
			db_datalog_show_tables/1,
			db_datalog_show_tables/0,
			db_show_tables/2,
			db_show_tables/1,
			db_show_database/2,
			db_show_databases/2,
			db_show_databases/1,
			db_change_database/2,
			db_my_sql_select/3,
			db_my_number_of_fields/3,
			db_my_get_attributes_types/3
		       ]).


:- use_module(myddas_errors,[
			     '$error_checks'/1
			     ]).

:- use_module(myddas_util_predicates,[
				      '$get_value'/2,
				      '$make_atom'/2,
				      '$write_or_not'/1
				      ]).


%--------------------------------------------------------
% Public Predicates
%--------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_open/5
%
%
db_my_open(_,_,_,_,_) :-
	write('WARNING!! Now we use db_open/5'),nl,
	write('USAGE: db_open(ConType,Connection,Host/Db,User,Password).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_close/1
%
%
db_my_close(_):-
	write('WARNING!! Now we use db_close/1'),nl,
	write('USAGE: db_close(Connection).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_import/3
%
%
db_my_import(_,_,_) :-
	write('WARNING!! Now we use db_import/3'),nl,
	write('USAGE: db_import(Connection,RelationName,PredName).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_view/3
%
%
db_my_view(_,_,_) :-
	write('WARNING!! Now we use db_view/3'),nl,
	write('USAGE: db_import(Connection,RelationName,DBGoal).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% db_my_insert/2
%
%
db_my_insert(_,_):-
	write('WARNING!! Now we use db_insert/2'),nl,
	write('USAGE: db_insert(Connection,PredName).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_insert/3
%
%
db_my_insert(_,_,_) :-
	write('WARNING!! Now we use db_insert/3'),nl,
	write('USAGE: db_insert(Connection,RelationName,PredName).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_result_set/1
%
%
db_my_result_set(X):-
	var(X),!,
	get_value(db_my_result_set,X).
db_my_result_set(use_result):-
	set_value(db_my_result_set,use_result).
db_my_result_set(store_result):- 
	set_value(db_my_result_set,store_result).
%default value
:- db_my_result_set(store_result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_describe/2
%
%
db_datalog_describe(Relation):-
	db_datalog_describe(myddas,Relation).
db_datalog_describe(Connection,Relation) :-
	'$error_checks'(db_my_describe(Relation,Connection)),
	'$get_value'(Connection,Conn),
	'$make_atom'(['DESCRIBE ',Relation],SQL),
	db_my_result_set(Mode),
	c_db_my_query(SQL,ResultSet,Conn,Mode),
	c_db_my_table_write(ResultSet).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_describe/3
% db_describe/2
% gives the results of the DESCRIBE statement
% by backtracking
db_describe(Relation,TableInfo) :-
	db_describe(myddas,Relation,TableInfo).
db_describe(Connection,Relation,tableinfo(A1,A2,A3,A4,A5,A6)) :-
	'$error_checks'(db_my_describe(Relation,Connection)),
	'$get_value'(Connection,Conn),
	'$make_atom'(['DESCRIBE ',Relation],SQL),
	db_my_result_set(Mode),
	'$write_or_not'(SQL),
	c_db_my_query(SQL,ResultSet,Conn,Mode),
	!,c_db_my_row(ResultSet,6,[A1,A2,A3,A4,A5,A6]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_datalog_show_tables/1
%
%
db_datalog_show_tables:-
	db_datalog_show_tables(myddas).
db_datalog_show_tables(Connection) :-
	'$error_checks'(db_my_show_tables(Connection)),
	'$get_value'(Connection,Conn),
	db_my_result_set(Mode),
	'$write_or_not'('SHOW TABLES'),
	c_db_my_query('SHOW TABLES',ResultSet,Conn,Mode),
	c_db_my_table_write(ResultSet).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_show_tables/2
% db_show_tables/1
% gives the results of the SHOW TABLES statement
% by backtracking
db_show_tables(Table) :-
	db_show_tables(myddas,Table).
db_show_tables(Connection,table(Table)) :-
	'$error_checks'(db_my_show_tables(Connection)),
	'$get_value'(Connection,Conn),
	db_my_result_set(Mode),
	'$write_or_not'('SHOW TABLES'),
	c_db_my_query('SHOW TABLES',ResultSet,Conn,Mode),
	!,c_db_my_row(ResultSet,1,[Table]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_show_database/2
% 
%
db_show_database(Connection,Database) :-
	'$error_checks'(db_show_database(Connection,Database)),
	'$get_value'(Connection,Con),
	c_db_my_get_database(Con,Database).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_show_databases/2
% 
%
db_show_databases(Connection,database(Databases)) :-
	%'$error_checks'(db_show_databases(Connection,Database)),
	'$get_value'(Connection,Conn),
	db_my_result_set(Mode),
	'$write_or_not'('SHOW DATABASES'),
	c_db_my_query('SHOW DATABASES',ResultSet,Conn,Mode),
	!,c_db_my_row(ResultSet,1,[Databases]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_show_databases/1
% TODO Error Checks
%
db_show_databases(Connection) :-
	%'$error_checks'(db_my_show_databases(Connection)),
	'$get_value'(Connection,Conn),
	db_my_result_set(Mode),
	'$write_or_not'('SHOW DATABASES'),
	c_db_my_query('SHOW DATABASES',ResultSet,Conn,Mode),
	c_db_my_table_write(ResultSet).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_change_database/2
% 
%
db_change_database(Connection,Database) :-
	'$error_checks'(db_change_database(Connection,Database)),
	'$get_value'(Connection,Con),
	'$make_atom'(['USE ',Database],SQL),
	'$write_or_not'(SQL),
	c_db_my_change_database(Con,Database).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_sql_select/3
% 
%
db_my_sql_select(_,_,_):-
	write('WARNING!! Now we use db_sql_select/3'),nl,
	write('USAGE: db_sql_select(Connection,SQL,ListaArgs).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_number_of_fields/3
%
%
db_my_number_of_fields(_,_,_) :-
	write('WARNING!! Now we use db_number_of_fields/3'),nl,
	write('USAGE: db_number_of_fields(Connection,RelationName,Arity).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_get_attributes_types/3
%
%
db_my_get_attributes_types(_,_,_) :-
	write('WARNING!! Now we use db_get_attributes_types/3'),nl,
	write('USAGE: db_get_attributes_types(Connection,RelationName,TypesList).'),nl,
	halt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
