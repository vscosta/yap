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
			 '$error_checks'/1,
			 '$db_my_error'/2
			 ]).

:- use_module(myddas_util_predicates).

% must have only one relation
'$error_checks'(db_my_insert3(_,_,_)):-!.
'$error_checks'(db_my_insert2(_,Conn,[query(Att,[rel(Relation,_)],_)])) :- !,
	% Number of fields of the Relation, must be
        % equal to the number of attributes
        c_db_my_number_of_fields(Relation,Conn,Arity),
	length(Att,Arity),
	% All fields must be Instanciated ( FALTA POR O NULL )
	'$make_a_list'(Arity,FieldsProperties),
	c_db_my_get_fields_properties(Relation,Conn,FieldsProperties),
	'$check_fields'(Att,FieldsProperties).
%'$error_checks'(Preddb_Call):-
	%'$do_error'(db_my_error(incompatible_db_predicate,PredCall)).
	
'$error_checks'(db_my_open(Host,User,Password,Db,Conn)) :- !,
	nonvar(Host), % ==  \+var(Host)
	nonvar(User),
	nonvar(Password),
	nonvar(Db),
	atom(Conn),
	get_value(Conn,[]).  % Nao pode ter nenhum valor atribuido
'$error_checks'(db_my_close(_)) :- !.
'$error_checks'(db_my_import(RelationName,PredName,_)) :- !,
	nonvar(RelationName),
	nonvar(PredName).
'$error_checks'(db_my_view(PredName,DbGoal,_)) :- !,
	nonvar(DbGoal),
	nonvar(PredName).
'$error_checks'(db_my_number_of_fields(RelationName,_,_)) :- !,
	nonvar(RelationName).
'$error_checks'(db_my_get_attributes_types(RelationName,_,_)) :- !,
	nonvar(RelationName).
'$error_checks'(db_my_describe(Relation,_)) :- !,
	nonvar(Relation).
'$error_checks'(db_my_show_tables(_)):- !.
'$error_checks'(db_my_sql_select(_,SQL,LA)):- !,
	nonvar(SQL),
	var(LA).
'$error_checks'(db_is_database_predicate(PredName,Arity,Module)):-!,
        nonvar(PredName),
	nonvar(Arity),
	nonvar(Module).
% Prevent the error of given an atom that has no value
'$error_checks'(get_value(Conn,Connection)) :- !,
	% This also prevents the case of giving the number of the connection
	% as an argument
	atom(Conn),
	var(Connection),
	get_value(Conn,Value),
	Value \== [].

% must have only one relation
%'$error_checks'(db_insert(_,_,_)):-!.
'$error_checks'(db_odbc_insert3(_,_,_)):-!.
'$error_checks'(db_odbc_insert2(PredName,Conn,[query(Att,[rel(Relation,_)],_)])) :- !,
	% Number of fields of the Relation, must be
        % equal to the number of attributes
        c_db_odbc_number_of_fields(Relation,Conn,Arity),
	length(Att,Arity),
	% All fields must be Instanciated ( FALTA POR O NULL )
	'$make_a_list'(Arity,FieldsProperties),
	c_db_odbc_get_fields_properties(Relation,Conn,FieldsProperties),
	'$check_fields'(Att,FieldsProperties).
%'$error_checks'(PredCall):-
	%'$do_error'(db_error(incompatible_db_predicate,PredCall)).
	
'$error_checks'(db_odbc_open(Host,User,Password,Conn)) :- !,
	nonvar(Host), % ==  \+var(Host)
	nonvar(User),
	nonvar(Password),
	atom(Conn),
	get_value(Conn,[]).  % Nao pode ter nenhum valor atribuido
'$error_checks'(db_odbc_close(_)) :- !.
'$error_checks'(db_odbc_import(RelationName,PredName,_)) :- !,
	nonvar(RelationName),
	nonvar(PredName).
'$error_checks'(db_odbc_view(PredName,DbGoal,Connection)) :- !,
	nonvar(DbGoal),
	nonvar(PredName).
'$error_checks'(db_odbc_number_of_fields(RelationName,Connection,Arity)) :- !,
	nonvar(RelationName).
'$error_checks'(db_odbc_get_attributes_types(RelationName,Connection,TypesList)) :- !,
	nonvar(RelationName).
'$error_checks'(db_odbc_sql_select(Connection,SQL,LA)):- !,
	nonvar(SQL),
	var(LA).
% Prevent the error of given an atom that has no value
'$error_checks'(get_value(Conn,Connection)) :- !,
	% This also prevents the case of giving the number of the connection
	% as an argument
	atom(Conn),
	var(Connection),
	get_value(Conn,Value),
	Value \== [].



'$db_my_error'(ERROR,_):-var(ERROR),!.
'$db_my_error'(2005,c_db_my_connect(Host,User,Password,Db,Connection)):-!,
        write(Host),nl.