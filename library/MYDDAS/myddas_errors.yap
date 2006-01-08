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
* File:		myddas_errors.yap					 *
* Last rev:								 *
* mods:									 *
* comments:	MYDDAS errors checks and errors Messages                 *
*									 *
*************************************************************************/

:- module(myddas_errors,[
			 '$error_checks'/1
			 ]).

:- use_module(myddas_util_predicates,[
				      '$make_a_list'/2,
				      '$check_fields'/2
				      ]).

:- use_module(lists,[
		     is_list/1
		    ]).

'$error_checks'(db_stats(_,List)):-!,
	var(List).
'$error_checks'(db_export_view(Connection,TableViewName,SQLorDbGoal,FieldsInf)):-!,
        atom(Connection),
	( atom(TableViewName) -> atom(SQLorDbGoal) ; true ),
        ( atom(SQLorDbGoal) -> atom(TableViewName) ; true ),
	is_list(FieldsInf).
'$error_checks'(db_create_table(Connection,TableName,FieldsInf)):-!,
        atom(Connection),
	atom(TableName),
	FieldsInf = [_|_].
'$error_checks'(db_insert3(Connection,RelationName,PredName)):-!,
        atom(Connection),
	atom(RelationName),
	atom(PredName).
'$error_checks'(db_insert2(Connection,_,[query(Att,[rel(Relation,_)],_)])) :- !,
        atom(Connection), 
	get_value(Connection,Con),
	% Number of fields of the Relation, must be
        % equal to the number of attributes
	c_db_connection_type(Con,ConType),
	( ConType == mysql ->
	    c_db_my_number_of_fields(Relation,Con,Arity)
	;
	    c_db_odbc_number_of_fields(Relation,Con,Arity)
	),
	length(Att,Arity),
	% All fields must be Instanciated ( FALTA POR O NULL )
	'$make_a_list'(Arity,FieldsProperties),
	( ConType == mysql ->
	    c_db_my_get_fields_properties(Relation,Con,FieldsProperties)
	;
	    c_db_odbc_get_fields_properties(Relation,Con,FieldsProperties)
	),
	'$check_fields'(Att,FieldsProperties).
'$error_checks'(db_open(mysql,Connection,Host/Db,User,Password)) :- !,
	nonvar(Host), % ==  \+var(Host)
	nonvar(User),
	nonvar(Password),
	nonvar(Db),
	atom(Connection),
	get_value(Connection,[]).  % Nao pode ter nenhum valor atribuido
'$error_checks'(db_open(odbc,Connection,ODBCEntry,User,Password)) :- !,
	nonvar(ODBCEntry), % ==  \+var(ODBCEntry)
	nonvar(User),
	nonvar(Password),
	atom(Connection),
	get_value(Connection,[]).  % Nao pode ter nenhum valor atribuido
'$error_checks'(db_view(Connection,PredName,DbGoal)) :- !,
        atom(Connection),
        nonvar(DbGoal),
	nonvar(PredName).
'$error_checks'(db_import(Connection,RelationName,PredName)) :- !,
        atom(Connection),
        atom(RelationName),
	atom(PredName).
'$error_checks'(db_get_attributes_types(Connection,RelationName,_)) :- !,
        atom(Connection),
	nonvar(RelationName).
'$error_checks'(db_sql_select(Connection,SQL,LA)):- !,
        atom(Connection),
        nonvar(SQL),
	var(LA).
'$error_checks'(db_number_of_fields(Connection,RelationName,_)) :- !,
        atom(Connection),
	nonvar(RelationName).
'$error_checks'(db_close(Connection)) :- !,
        atom(Connection).
% must have only one relation
'$error_checks'(db_my_describe(Relation,_)) :- !,
	nonvar(Relation).
'$error_checks'(db_my_show_tables(_)):- !.
'$error_checks'(db_is_database_predicate(PredName,Arity,Module)):-!,
        nonvar(PredName),
	nonvar(Arity),
	nonvar(Module).
% Prevent the error of given an atom that has no value
'$error_checks'(get_value(Connection,Con)) :- !,
	% This also prevents the case of giving the number of the connection
	% as an argument
	atom(Connection),
	var(Con),
	get_value(Connection,Value),
	Value \== [].

% Prevent the error of given an atom that has no value
'$error_checks'(get_value(Conn,Connection)) :- !,
	% This also prevents the case of giving the number of the connection
	% as an argument
	atom(Conn),
	var(Connection),
	get_value(Conn,Value),
	Value \== [].
