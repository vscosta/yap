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
* File:		myddas_odbc.yap						 *
* Last rev:								 *
* mods:									 *
* comments:	ODBC Driver communication library 			 *
*									 *
*************************************************************************/

:- module(myddas_odbc,[
		       db_odbc_open/4,
		       db_odbc_close/1,
		       db_odbc_import/3,
		       db_odbc_view/3,
		       db_odbc_insert/2,
		       db_odbc_insert/3,
		       db_odbc_sql_select/3,
		       db_odbc_number_of_fields/3,
		       db_odbc_get_attributes_types/3
		      ]).

:- use_module(myddas).
:- use_module(myddas_errors).
:- use_module(myddas_prolog2sql,[translate/3,queries_atom/2]).
:- use_module(myddas_util_predicates).

:- use_module(lists,[append/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_open/4
%
%
db_odbc_open(ODBCEntry,User,Password,Conn) :-
	'$error_checks'(db_odbc_open(ODBCEntry,User,Password,Conn)),
	c_db_odbc_connect(ODBCEntry,User,Password,Connection),
	set_value(Conn,Connection).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_close/1
%
%
db_odbc_close(Conn):-
	'$error_checks'(db_odbc_close(Conn)),
        '$abolish_all'(Conn).
db_odbc_close(Conn) :-
	'$error_checks'(db_odbc_close(Conn)),
	'$get_value'(Conn,Connection),
	c_db_odbc_disconnect(Connection),
	set_value(Conn,[]). % "deletes" atom 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_import/3
%
%
db_odbc_import(RelationName,PredName,Connection) :-
	'$error_checks'(db_odbc_import(RelationName,PredName,Connection)),
	'$get_value'(Connection,Conn),
	'$assert_import_clause'(RelationName,PredName,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_view/3
%
%	
db_odbc_view(PredName,DbGoal,Connection) :-
	'$error_checks'(db_odbc_view(PredName,DbGoal,Connection)),
	'$get_value'(Connection,Conn),
	'$assert_view_clause'(PredName,DbGoal,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_insert/2
%
%
db_odbc_insert(PredName,Connection):-
	'$get_value'(Connection,Conn),
	translate(PredName,PredName,Code),
	'$error_checks'(db_odbc_insert2(PredName,Conn,Code)),
	'$get_values_for_insert'(Code,ValuesList,RelName),
	'$make_atom'(['INSERT INTO ',RelName,' VALUES'|ValuesList],SQL),
	c_db_odbc_query(SQL,_,_,_,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_insert/3
%
%
db_odbc_insert(RelationName,PredName,Connection) :-
	'$error_checks'(db_odbc_insert3(RelationName,PredName,Connection)),
	'$get_value'(Connection,Conn),
	'$assert_relation_insert'(RelationName,PredName,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_sql_select/3
%
%
db_odbc_sql_select(Connection,SQL,LA):-
	'$error_checks'(db_odbc_sql_select(Connection,SQL,LA)),
	'$get_value'(Connection,Conn),
	c_db_odbc_number_of_fields_in_query(SQL,Conn,Arity),
	'$make_a_list'(Arity,LA),
	'$make_a_list'(Arity,BindList),
	'$write_or_not'(SQL),
	c_db_odbc_query(SQL,ResultSet,Arity,BindList,Conn),!,
	c_db_odbc_row(ResultSet,BindList,LA).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_number_of_fields/3
%
%
db_odbc_number_of_fields(RelationName,Connection,Arity) :-
	'$error_checks'(db_odbc_number_of_fields(RelationName,Connection,Arity)),
	'$get_value'(Connection,Conn),
	c_db_odbc_number_of_fields(RelationName,Conn,Arity).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_odbc_get_attributes_types/3
%
%
db_odbc_get_attributes_types(RelationName,Connection,TypesList) :-
	'$error_checks'(db_odbc_get_attributes_types(RelationName,Connection,TypesList)),
	'$get_value'(Connection,Conn),
	c_db_odbc_number_of_fields(RelationName,Conn,Arity),
	Size is 2*Arity,
	'$make_a_list'(Size,TypesList),
	c_db_odbc_get_attributes_types(RelationName,Conn,TypesList).
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
	not c_db_check_if_exists_pred(PredName,Arity,Module),

	% This copy_term is done to prevent the unification
	% with top-level variables   A='var('A')' error
	copy_term((ViewName,DbGoal),(CopyView,CopyGoal)),
	translate(ViewName,DbGoal,Code),
	queries_atom(Code,SQL),
	% checks if the WHERE commend of SQL exists in the string
	'$where_exists'(SQL,Flag),
	
	'$make_list_of_args'(1,Arity,NewName,LA),
	% build view clause
	Assert =..[':-',NewName,
		          ','(myddas_odbc:'$build_query'(Flag,SQL,Code,LA,FinalSQL),
			  ','(myddas_odbc:'$make_a_list'(Arity,BindList),
              		  ','(myddas_odbc:c_db_odbc_query(FinalSQL,ResultSet,Arity,BindList,Connection),
			  ','(myddas_odbc:'$write_or_not'(FinalSQL),
			  ','(!,myddas_odbc:c_db_odbc_row(ResultSet,BindList,LA))))))],
        assert(Module:Assert),
	c_db_add_preds(PredName,Arity,Module,Connection).



'$assert_relation_insert'(RelationName,PredName,Connection) :-
	% get relation arity
        c_db_odbc_number_of_fields(RelationName,Connection,Arity),
	db_module(Module),
	not c_db_check_if_exists_pred(PredName,Arity,Module),

	R=..[relation,PredName,Arity,RelationName],
	% assert relation fact
	assert(myddas_prolog2sql:R),

	Size is 2*Arity,
        '$make_a_list'(Size,TypesList),
	% get attributes types in TypesList [field0,type0,field1,type1...]
	c_db_odbc_get_attributes_types(RelationName,Connection,TypesList),

	% build PredName functor
	functor(P,PredName,Arity),
	'$make_list_of_args'(1,Arity,P,LA),
	
	% build PredName clause
	Assert =..[':-',P,','(myddas_odbc:'$get_values_for_insert'(TypesList,LA,ValuesList),
			  ','(myddas_odbc:'$make_atom'(['INSERT INTO ',RelationName,' VALUES ('|ValuesList],SQL),
			      myddas_odbc:c_db_odbc_query(SQL,_,_,_,Connection)))],
	assert(Module:Assert),
	c_db_add_preds(PredName,Arity,Module,Connection).




'$assert_import_clause'(RelationName,PredName,Connection) :-
	% get relation arity
        c_db_odbc_number_of_fields(RelationName,Connection,Arity),
	db_module(Module),
	not c_db_check_if_exists_pred(PredName,Arity,Module),

	R=..[relation,PredName,Arity,RelationName],
 	% assert relation fact
 	assert(myddas_prolog2sql:R),

	Size is 2*Arity,
	'$make_a_list'(Size,TypesList),
 	% get attributes types in TypesList [field0,type0,field1,type1...]
	c_db_odbc_get_attributes_types(RelationName,Connection,TypesList),
 	% assert attributes facts 
        '$assert_attribute_information'(0,Arity,RelationName,TypesList),

	% build PredName functor
 	functor(P,PredName,Arity),
	% build arg list for db_row/2
        '$make_list_of_args'(1,Arity,P,LA),

	%Optimization
 	'$copy_term_nv'(P,[],G,_),

	%generate the SQL query
 	translate(G,G,Code),
 	queries_atom(Code,SQL),

	% build PredName clause
	Assert =..[':-',P,','(myddas_odbc:'$build_query'(0,SQL,Code,LA,FinalSQL),
			  ','(myddas_odbc:'$make_a_list'(Arity,BindList),
			  ','(myddas_odbc:c_db_odbc_query(FinalSQL,ResultSet,Arity,BindList,Connection),
			  ','(myddas_odbc:'$write_or_not'(FinalSQL),
			  ','(!,myddas_odbc:c_db_odbc_row(ResultSet,BindList,LA))))))],
 	assert(Module:Assert),
 	c_db_add_preds(PredName,Arity,Module,Connection).


