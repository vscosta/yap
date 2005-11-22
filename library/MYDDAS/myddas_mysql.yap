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
* comments:	MySQL Server communication library 			 *
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
			db_my_describe/2,
			db_my_describe/3,
			db_my_show_tables/1,
			db_my_show_tables/2,
			db_my_sql_select/3,
			db_my_number_of_fields/3,
			db_my_get_attributes_types/3
			]).



:- use_module(myddas).
:- use_module(myddas_errors).
:- use_module(myddas_prolog2sql,[translate/3,queries_atom/2]).
:- use_module(myddas_util_predicates).

:- use_module(lists,[append/3]).

%--------------------------------------------------------
% Public Predicates
%--------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_open/5
%
%
db_my_open(Host,User,Password,Db,Conn) :-
	'$error_checks'(db_my_open(Host,User,Password,Db,Conn)),
	c_db_my_connect(Host,User,Password,Db,Connection),
	%'$db_my_error'(ERROR,c_db_my_connect(Host,User,Password,Db,Connection)),
	set_value(Conn,Connection).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_close/1
%
%
db_my_close(Conn):-
	'$error_checks'(db_my_close(Conn)),
        '$abolish_all'(Conn).
db_my_close(Conn) :-
	'$error_checks'(db_my_close(Conn)),
	'$get_value'(Conn,Connection),
	c_db_my_disconnect(Connection),
	set_value(Conn,[]). % "deletes" atom 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_import/3
%
%
db_my_import(RelationName,PredName,Connection) :-
	'$error_checks'(db_my_import(RelationName,PredName,Connection)),
	'$get_value'(Connection,Conn),
	'$assert_import_clause'(RelationName,PredName,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_view/3
%
%
db_my_view(PredName,DbGoal,Connection) :-
	'$error_checks'(db_my_view(PredName,DbGoal,Connection)),
	'$get_value'(Connection,Conn),
	'$assert_view_clause'(PredName,DbGoal,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% db_my_insert/2
%
%
db_my_insert(PredName,Connection):-
	'$get_value'(Connection,Conn),
	translate(PredName,PredName,Code),
	'$error_checks'(db_my_insert2(PredName,Conn,Code)),
	'$get_values_for_insert'(Code,ValuesList,RelName),
	'$make_atom'(['INSERT INTO ',RelName,' VALUES'|ValuesList],SQL),
	db_my_result_set(Mode),
	c_db_my_query(SQL,_,Conn,Mode).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_insert/3
%
%
db_my_insert(RelationName,PredName,Connection) :-
	'$get_value'(Connection,Conn),
	'$error_checks'(db_my_insert3(RelationName,PredName,Connection)),
	'$assert_relation_insert'(RelationName,PredName,Conn).
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
db_my_describe(Relation,Connection) :-
	'$error_checks'(db_my_describe(Relation,Connection)),
	'$get_value'(Connection,Conn),
	'$make_atom'(['DESCRIBE ',Relation],SQL),
	db_my_result_set(Mode),
	c_db_my_query(SQL,ResultSet,Conn,Mode),
	c_db_my_table_write(ResultSet).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_describe/3
% gives the results of the DESCRIBE statement
% by backtracking
db_my_describe(Relation,Connection,tableinfo(A1,A2,A3,A4,A5,A6)) :-
	'$error_checks'(db_my_describe(Relation,Connection)),
	'$get_value'(Connection,Conn),
	'$make_atom'(['DESCRIBE ',Relation],SQL),
	db_my_result_set(Mode),
	'$write_or_not'(SQL),
	c_db_my_query(SQL,ResultSet,Conn,Mode),
	!,c_db_my_row(ResultSet,6,[A1,A2,A3,A4,A5,A6]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_show_tables/1
%
%
db_my_show_tables(Connection) :-
	'$error_checks'(db_my_show_tables(Connection)),
	'$get_value'(Connection,Conn),
	db_my_result_set(Mode),
	'$write_or_not'('SHOW TABLES'),
	c_db_my_query('SHOW TABLES',ResultSet,Conn,Mode),
	c_db_my_table_write(ResultSet).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_show_tables/2
% gives the results of the SHOW TABLES statement
% by backtracking
db_my_show_tables(Connection,table(Table)) :-
	'$error_checks'(db_my_show_tables(Connection)),
	'$get_value'(Connection,Conn),
	db_my_result_set(Mode),
	'$write_or_not'('SHOW TABLES'),
	c_db_my_query('SHOW TABLES',ResultSet,Conn,Mode),
	!,c_db_my_row(ResultSet,1,[Table]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_sql_select/3
% 
%
db_my_sql_select(Connection,SQL,LA):-
	'$error_checks'(db_my_sql_select(Connection,SQL,LA)),
	'$get_value'(Connection,Conn),
	c_db_my_number_of_fields_in_query(SQL,Conn,Arity),
	'$make_a_list'(Arity,LA),
	db_my_result_set(Mode),
	'$write_or_not'(SQL),!,
	c_db_my_query(SQL,ResultSet,Conn,Mode),
	c_db_my_row(ResultSet,Arity,LA).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_number_of_fields/3
%
%
db_my_number_of_fields(RelationName,Connection,Arity) :-
	'$error_checks'(db_my_number_of_fields(RelationName,Connection,Arity)),
	'$get_value'(Connection,Conn),
	c_db_my_number_of_fields(RelationName,Conn,Arity).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_get_attributes_types/3
%
%
db_my_get_attributes_types(RelationName,Connection,TypesList) :-
	'$error_checks'(db_my_get_attributes_types(RelationName,Connection,TypesList)),
	'$get_value'(Connection,Conn),
	c_db_my_number_of_fields(RelationName,Conn,Arity),
	Size is 2*Arity,
	'$make_a_list'(Size,TypesList),
	c_db_my_get_attributes_types(RelationName,Conn,TypesList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





%--------------------------------------------------------
% Private Predicates
%--------------------------------------------------------
'$assert_view_clause'(ViewName,DbGoal,Connection) :-
	% here we can add some error control, like checking DBgoals include
	% only DB relations
       	% get arity of projection term
	functor(ViewName,PredName,Arity),
	functor(NewName,PredName,Arity),
	db_module(Module),
	not c_db_my_check_if_exists_pred(PredName,Arity,Module),

	% This copy_term is done to prevent the unification
	% with top-level variables   A='var('A')' error
	copy_term((ViewName,DbGoal),(CopyView,CopyGoal)),
	translate(CopyView,CopyGoal,Code),
	queries_atom(Code,SQL),
	% checks if the WHERE commend of SQL exists in the string
	'$where_exists'(SQL,Flag),

	'$make_list_of_args'(1,Arity,NewName,LA),
	% build view clause
	Assert =..[':-',NewName,
		          ','(myddas_mysql:'$build_query'(Flag,SQL,Code,LA,FinalSQL),
			  ','(myddas_mysql:db_my_result_set(Mode),
			  ','(myddas_mysql:'$write_or_not'(FinalSQL),
			  ','(myddas_mysql:c_db_my_query(FinalSQL,ResultSet,Connection,Mode),
			  ','(!,myddas_mysql:c_db_my_row(ResultSet,Arity,LA))))))],
	assert(Module:Assert),
	c_db_add_preds(PredName,Arity,Module,Connection).


'$assert_relation_insert'(RelationName,PredName,Connection) :-
	% get relation arity
        c_db_my_number_of_fields(RelationName,Connection,Arity),
	db_module(Module),
	not c_db_my_check_if_exists_pred(PredName,Arity,Module),

	R=..[relation,PredName,Arity,RelationName],
	% assert relation fact
	assert(myddas_prolog2sql:R),

	Size is 2*Arity,
        '$make_a_list'(Size,TypesList),
	% get attributes types in TypesList [field0,type0,field1,type1...]
	c_db_my_get_attributes_types(RelationName,Connection,TypesList),
	
	% build PredName functor
	functor(P,PredName,Arity),
	'$make_list_of_args'(1,Arity,P,LA),

	% build PredName clause
	Assert =..[':-',P,','(myddas_mysql:'$get_values_for_insert'(TypesList,LA,ValuesList),
			  ','(myddas_mysql:'$make_atom'(['INSERT INTO ',RelationName,' VALUES ('|ValuesList],SQL),
			  ','(myddas_mysql:db_my_result_set(Mode),
			  ','(myddas_mysql:'$write_or_not'(SQL),
			      myddas_mysql:c_db_my_query(SQL,_,Connection,Mode)))))],
	assert(Module:Assert),
	c_db_add_preds(PredName,Arity,Module,Connection).


'$assert_import_clause'(RelationName,PredName,Connection) :-
	% get relation arity
        c_db_my_number_of_fields(RelationName,Connection,Arity),
	db_module(Module),
	not c_db_my_check_if_exists_pred(PredName,Arity,Module),

	R=..[relation,PredName,Arity,RelationName],
	% assert relation fact
	assert(myddas_prolog2sql:R),

	Size is 2*Arity,
        '$make_a_list'(Size,TypesList),
	% get attributes types in TypesList [field0,type0,field1,type1...]
	c_db_my_get_attributes_types(RelationName,Connection,TypesList),
	% assert attributes facts 
        '$assert_attribute_information'(0,Arity,RelationName,TypesList),

	% build PredName functor
	functor(P,PredName,Arity),
	'$make_list_of_args'(1,Arity,P,LA),

	%Optimization
	'$copy_term_nv'(P,[],G,_),

	%generate the SQL query
	translate(G,G,Code),
	queries_atom(Code,SQL),

	% build PredName clause
	Assert =..[':-',P,','(myddas_mysql:'$build_query'(0,SQL,Code,LA,FinalSQL),
			  ','(myddas_mysql:db_my_result_set(Mode),
			  ','(myddas_mysql:'$write_or_not'(FinalSQL),
			  ','(myddas_mysql:c_db_my_query(FinalSQL,ResultSet,Connection,Mode),
			  ','(!,myddas_mysql:c_db_my_row(ResultSet,Arity,LA))))))],
	assert(Module:Assert),
	c_db_add_preds(PredName,Arity,Module,Connection).
