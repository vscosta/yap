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
		  db_open/5,
		  db_close/1,

		  db_verbose/1,
		  db_module/1,
		  db_is_database_predicate/3,
		  db_abolish/2,
		  db_stats/2,
		  
		  db_sql_select/3,
		  db_prolog_select/4,
		  db_command/2,
		  db_insert/2,
		  db_create_table/3,
		  db_export_view/4,
		  db_update/2,

		  db_get_attributes_types/3,
		  db_number_of_fields/3,
		  
		  % myddas_assert_predicates.yap
		  db_import/3,
		  db_view/3,
		  db_insert/3,

		  % myddas_mysql.yap
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

:- use_module(myddas_assert_predicates,[
					db_import/3,
					db_view/3,
					db_insert/3
				       ]).

:- use_module(myddas_mysql,[
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

:- use_module(myddas_util_predicates,[
				      '$process_sql_goal'/4,
				      '$process_fields'/3,
				      '$get_values_for_insert'/3,
				      '$make_atom'/2,
				      '$write_or_not'/1,
				      '$abolish_all'/1,
				      '$make_a_list'/2,
				      '$make_list_of_args'/4,
				      '$get_table_name'/2,
				      '$get_values_for_update'/4,
				      '$extract_args'/4
				      ]).

:- use_module(myddas_errors,[
			     '$error_checks'/1
			     ]).

:- use_module(myddas_prolog2sql,[
				 translate/3
				]).

:- use_module(lists,[
		     append/3
		    ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_open/5
%
%
db_open(mysql,Connection,Host/Db,User,Password) :-!,
	'$error_checks'(db_open(mysql,Connection,Host/Db,User,Password)),
	c_db_my_connect(Host,User,Password,Db,Con),
	set_value(Connection,Con).
db_open(odbc,Connection,ODBCEntry,User,Password) :-!,
	'$error_checks'(db_open(odbc,Connection,ODBCEntry,User,Password)),
	c_db_odbc_connect(ODBCEntry,User,Password,Con),
	set_value(Connection,Con).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_close/1
%
%
db_close(Connection):-
	'$error_checks'(db_close(Connection)),
	get_value(Connection,Con),
        '$abolish_all'(Con).
db_close(Connection) :-
	'$error_checks'(db_close(Connection)),
	get_value(Connection,Con),
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    c_db_my_disconnect(Con)
	;
	    c_db_odbc_disconnect(Con)
	),
	set_value(Connection,[]). % "deletes" atom 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
% db_module/1
%
%
db_module(X):-
	var(X),!,
	get_value(db_module,X).
db_module(ModuleName):-
	set_value(db_module,ModuleName).
% default value
:- db_module(user).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_is_database_predicate(+,+,+)
%
%
db_is_database_predicate(PredName,Arity,Module):-
	'$error_checks'(db_is_database_predicate(PredName,Arity,Module)),
	c_db_check_if_exists_pred(PredName,Arity,Module).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_abolish(+,+)
%
%
db_abolish(Module:PredName,Arity):-!,
	'$error_checks'(db_abolish(Module:PredName,Arity)),
	%c_db_delete_predicate(
	abolish(Module:PredName,Arity).
db_abolish(PredName,Arity):-
	'$error_checks'(db_abolish(PredName,Arity)),
	
	%c_db_delete_predicate(
	abolish(PredName,Arity).
			      
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_stats(+,-)
%
%
:- set_value(db_myddas_stats_count,0).
db_stats(Connection,List):-
	'$error_checks'(db_stats(Connection,List)),
	NumberOfStats = 9,
	'$make_a_list'(NumberOfStats,List),
	( var(Connection) ->
	    c_db_stats(0,List)
	;
	    get_value(Connection,Conn),
	    c_db_stats(Conn,List)
	),
	write('MYDDAS: '),nl,write(List),nl,
	get_value(db_myddas_stats_count,Value),
	write('Stats Counts '),write(Value),nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_sql_select(+,+,-)
% 
%
db_sql_select(Connection,SQL,LA):-
	'$error_checks'(db_sql_select(Connection,SQL,LA)),
	get_value(Connection,Con),
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    c_db_my_number_of_fields_in_query(SQL,Con,Arity)
	;
	    c_db_odbc_number_of_fields(SQL,Con,Arity)
	),
	'$make_a_list'(Arity,LA),
	'$write_or_not'(SQL),
	( ConType == mysql ->
	    db_my_result_set(Mode),
	    c_db_my_query(SQL,ResultSet,Con,Mode),!,
	    c_db_my_row(ResultSet,Arity,LA)
	;
	    '$make_a_list'(Arity,BindList),
	    c_db_odbc_query(SQL,ResultSet,Arity,BindList,Con),!,
	    c_db_odbc_row(ResultSet,BindList,LA)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_prolog_select(+,-,+,+)
%
%
db_prolog_select(Connection,LA,ViewName,DbGoal):-
	
	functor(ViewName,PredName,Arity),
	%functor(NewName,PredName,Arity),
	translate(ViewName,DbGoal,Code),
	queries_atom(Code,SQL),
	
	% build arg list for db_my_row/2
        '$make_list_of_args'(1,Arity,ViewName,LA),

	get_value(Connection,Con),
	c_db_connection_type(Con,ConType),
	'$write_or_not'(SQL),
	( ConType == mysql ->
	    db_my_result_set(Mode),
	    c_db_my_query(SQL,ResultSet,Con,Mode),
	    !,c_db_my_row(ResultSet,Arity,LA)
	;
	    true
	).
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_command/2
% 
%
db_command(Connection,SQL):-
	'$error_checks'(db_command(Connection,SQL)),
	get_value(Connection,Con),
	'$write_or_not'(SQL),
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    db_my_result_set(Mode),
	    c_db_my_query(SQL,_,Con,Mode)
	;
	    true
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_insert/2
%
%
db_insert(Connection,PredName):-
	translate(PredName,PredName,Code),
	'$error_checks'(db_insert2(Connection,PredName,Code)),
	'$get_values_for_insert'(Code,ValuesList,RelName),
	'$make_atom'(['INSERT INTO ',RelName,' VALUES'|ValuesList],SQL),

	get_value(Connection,Con),
	c_db_connection_type(Con,ConType),
	'$write_or_not'(SQL),
	( ConType == mysql ->
	    db_my_result_set(Mode),
	    c_db_my_query(SQL,_,Con,Mode)
	;
	    c_db_odbc_query(SQL,_,_,_,Con)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% db_create_table/3
% FieldsList = [field(Name,Type,Null,Key,DefaultValue)] 
% Example [field(campo1,'char(12)',y,y,a),field(campo2,int,y,y,0)]
% TODO Test with ODBC & Type Checks
db_create_table(Connection,TableName,FieldsInf):-
	'$error_checks'(db_create_table(Connection,TableName,FieldsInf)),
	get_value(Connection,Con),

	'$process_fields'(FieldsInf,FieldString,KeysSQL),
	'$make_atom'(['CREATE TABLE `',TableName,'` ( ',FieldString,KeysSQL,' )'],FinalSQL),

	c_db_connection_type(Con,ConType),
	'$write_or_not'(FinalSQL),
	( ConType == mysql ->
	    db_my_result_set(Mode),
	    c_db_my_query(FinalSQL,_,Con,Mode)
	;
	    c_db_odbc_query(FinalSQL,_,_,_,Con)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% db_export_view/4
% TODO Test with ODBC
% 
db_export_view(Connection,TableViewName,SQLorDbGoal,FieldsInf):-
	'$error_checks'(db_export_view(Connection,TableViewName,SQLorDbGoal,FieldsInf)),
	get_value(Connection,Con),
	'$process_sql_goal'(TableViewName,SQLorDbGoal,TableName,SQL),

	% Case there's some information about the
	% attribute fields of the relation given
	% by the user
	( FieldsInf == [] ->
	    '$make_atom'(['CREATE TABLE ',TableName,' AS ',SQL],FinalSQL)
	;
	    '$process_fields'(FieldsInf,FieldString,KeysSQL),
	    '$make_atom'(['CREATE TABLE ',TableName,' (',FieldString,KeysSQL,') AS ',SQL],FinalSQL)
	),    

	c_db_connection_type(Con,ConType),
	'$write_or_not'(FinalSQL),
	( ConType == mysql ->
	    db_my_result_set(Mode),
	    c_db_my_query(FinalSQL,_,Con,Mode)
	;
	    c_db_odbc_query(FinalSQL,_,_,_,Con)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_update/2
% 
%
db_update(Connection,WherePred-SetPred):-
	%TODO: error_checks
	get_value(Connection,Conn),

	% Match and Values must be "unifiable" 
	functor(WherePred,PredName,Arity),
	functor(SetPred,PredName,Arity),

	functor(NewRelation,PredName,Arity),
	
	'$extract_args'(WherePred,1,Arity,WhereArgs),
	'$extract_args'(SetPred,1,Arity,SetArgs),

	copy_term(WhereArgs,WhereArgsTemp),
	'$make_list_of_args'(1,Arity,NewRelation,WhereArgsTemp),
	translate(NewRelation,NewRelation,Code),

	'$get_values_for_update'(Code,SetArgs,SetCondition,WhereCondition),
	
	'$get_table_name'(Code,TableName),
	append(SetCondition,WhereCondition,Conditions), 
	'$make_atom'(['UPDATE ',TableName,' '|Conditions],SQL),
	'$write_or_not'(SQL),
	c_db_my_query_no_result(SQL,Conn).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_get_attributes_types/3
%
%
db_get_attributes_types(Connection,RelationName,TypesList) :-
	'$error_checks'(db_get_attributes_types(Connection,RelationName,TypesList)),
	get_value(Connection,Con),
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    c_db_my_number_of_fields(RelationName,Con,Arity)
	;
	    c_db_odbc_number_of_fields(RelationName,Con,Arity)
	),
	Size is 2*Arity,
	'$make_a_list'(Size,TypesList),
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    c_db_my_get_attributes_types(RelationName,Con,TypesList)
	;
	    c_db_odbc_get_attributes_types(RelationName,Con,TypesList)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_number_of_fields/3
%
%
db_number_of_fields(Connection,RelationName,Arity) :-
	'$error_checks'(db_number_of_fields(Connection,RelationName,Arity)),
	get_value(Connection,Con),
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    c_db_my_number_of_fields(RelationName,Con,Arity)
	;
	    c_db_odbc_number_of_fields(RelationName,Con,Arity)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
