:- module(myddas_test_predicates,[
				  % Tests or Debug Predicates
				  %db_my_delete/2,
				  db_assert_view/4,
				  db_my_insert_test/2,
				  db_my_update/3,
				  db_my_import_michel/3,
				  db_view_original/3, % DEBUG ONLY
				  db_ilpview/4
				  ]).


:- use_module(myddas).
:- use_module(myddas_mysql).
:- use_module(myddas_util_predicates).
:- use_module(myddas_prolog2sql,[translate/3,queries_atom/2]).
:- use_module(myddas_errors).
:- use_module(lists).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% db_assert_view/4
% TODO Test with ODBC
% TODO error checks
db_assert_view(ViewName,SQLorDbGoal,FieldsInf,Connection):-
	'$get_value'(Connection,Con),
	%'$error_checks'(),
	( var(ViewName) ->
	    c_db_get_new_table_name(Con,ViewName),
	    TableName = ViewName
	),
	'$process_sql_goal'(ViewName,SQLorDbGoal,TableName,SQL),

	% Case there's some information about the
	% attribute fields of the relation given
	% by the user
	'$generate_final_sql'(FieldsInf,TableName,SQL,FinalSQL),
	'$run_query'(Con,FinalSQL),

	% TODO: Optimize this
	db_my_import(TableName,TableName,Connection).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% db_my_insert/2
%
%
db_my_insert_test(PredName,Connection):-
	'$get_value'(Connection,Conn),
	translate(PredName,PredName,Code),
	'$error_checks'(db_my_insert2(PredName,Conn,Code)),
	'$get_values_for_insert'(Code,ValuesList,RelName),
	'$make_atom'(['INSERT INTO ',RelName,' VALUES'|ValuesList],SQL),
	c_db_my_query_no_result(SQL,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_update/3
% UpdaList = [X,1,Y,2,T,0]
%
db_my_update(UpdateList,Relation,Connection):-
	'$get_value'(Connection,Conn),
	%TODO: error_checks
	functor(Relation,PredName,Arity),
	functor(NewRelation,PredName,Arity),
	'$extract_args'(Relation,1,Arity,ArgsList1),
	copy_term(ArgsList1,ArgsList2),
	'$make_list_of_args'(1,Arity,NewRelation,ArgsList2),
	translate(NewRelation,NewRelation,Code),
	'$get_table_name'(Code,TableName),
	'$get_values_for_update'(Code,SetCondition,ArgsList1,UpdateList,WhereCondition),
	append(SetCondition,WhereCondition,Conditions), 
	'$make_atom'(['UPDATE ',TableName,' '|Conditions],SQL),
	'$write_or_not'(SQL),
	c_db_my_query_no_result(SQL,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%





	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_delete/2
%
%
% db_my_delete(PredName,Connection):-
%  	'$get_value'(Connection,Conn),
%  	translate(PredName,PredName,Code),
% 	%'$error_checks'(db_my_delete(PredName,Conn,Code)),
% 	queries_atom(Code,SQL),
% 	c_db_my_query('SELECT 46 , "ola" , "Adeus" FROM estrada A WHERE A.id_estrada = 46 , "ola" , "Adeus" FROM estrada A WHERE A.id_estrada = 46 AND A.nome = "ola" AND A.nome_alternativo = "Adeus"',_,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_my_import_michel/3
%
%
db_my_import_michel(RelationName,PredName,Connection) :-
	'$error_checks'(db_my_import(RelationName,PredName,Connection)),
	% get connection id based on given atom
	'$get_value'(Connection,Conn),
	% assert information needed for translate/3 and PredName clause
	'$assert_relation_info_and_pred_clause_michel_query'(RelationName,PredName,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% --- '$assert_relation_info_and_pred_clause'(RelationName,PredName,Connection)
%     Asserts information needed for translate/3 and the clause of the
%     PredName predicate
% ---
'$assert_relation_info_and_pred_clause_michel_query'(RelationName,PredName,Connection) :-
	% get relation arity
        % C Predicate
        c_db_my_number_of_fields(RelationName,Connection,Arity),
	db_module(Module),
	not c_db_my_check_if_exists_pred(PredName,Arity,Module),
	R=..[relation,PredName,Arity,RelationName],
	% assert relation fact
	'$assert_facts'(myddas_prolog2sql,R),
	%assert(myddas_prolog2sql:R),
	Size is 2*Arity,
	'$make_a_list'(Size,TypesList),
	% get attributes types in TypesList [field0,type0,field1,type1...]
	c_db_my_get_attributes_types(RelationName,Connection,TypesList),
	% assert attributes facts
	'$assert_attribute_information'(0,Arity,RelationName,TypesList),
	% build PredName functor
	functor(P,PredName,Arity),
	% build PredName clause
	Assert =..[':-',P,','(myddas_test_predicates:'$generate_optimized_SQL'(P,SQL,LA,ArityProj),
			  ','(myddas_test_predicates:db_my_result_set(Mode),
		          ','(myddas_test_predicates:'$write_or_not'(SQL),
		          ','(myddas_test_predicates:c_db_my_query(SQL,ResultSet,Connection,Mode),
			  ','(!,myddas_test_predicates:c_db_my_row_unify(ResultSet,ArityProj,LA))))))],
	% we are assuming that all the predicates will be inserted in
	% the user module
	assert(Module:Assert),
	% ALTERACAO
	% Adds PredName and Arity to this Connection List
	% C Predicate
	c_db_add_preds(PredName,Arity,Module,Connection).


% Beginning of new predicates for optimized translate

'$generate_optimized_SQL'(Pred,SQL,DbRowList,Arity) :-
        Pred =..[F|Args],
	functor(Pred,_,A),
	myddas_prolog2sql:relation(F,A,Relation),
	'$divide_args_in_proj_and_where'(1,Relation,Args,[],Proj,Where),
	'$generate_SQL'(Relation,Proj,Where,SQL),
	'$generate_dbrow_list'(Proj,DbRowList,Arity).
	

'$divide_args_in_proj_and_where'(_, _, [], _, [], []) :- !.

'$divide_args_in_proj_and_where'(I, Relation, [H|T], Dict, Proj, [v(Att,Att1)|Where]) :-
	var(H),
	'$member_var'(H,Dict,Att),!,
	myddas_prolog2sql:attribute(I,Relation,Att1,_),
	I1 is I+1,
	'$divide_args_in_proj_and_where'(I1, Relation, T, Dict, Proj, Where).

'$divide_args_in_proj_and_where'(I, Relation, [H|T], Dict, [(Att,H)|Proj], Where) :-
	var(H),!,
	myddas_prolog2sql:attribute(I,Relation,Att,_),
	I1 is I+1,
	'$divide_args_in_proj_and_where'( I1, Relation, T, [(H,Att)|Dict], Proj, Where).

'$divide_args_in_proj_and_where'(I, Relation, [H|T], Dict, Proj, [g(Att,H)|Where]) :-
	myddas_prolog2sql:attribute(I,Relation,Att,_),
	I1 is I+1,
	'$divide_args_in_proj_and_where'(I1,Relation,T,Dict,Proj,Where).
	

'$generate_SQL'(Relation,_,[],SQL) :-
	!,
	atom_concat('SELECT * FROM ',Relation, SQL).


'$generate_SQL'(Relation,Proj,Where,SQL) :-
        '$make_proj_atom'(Proj,Proj_Atom),
        atom_concat('SELECT ',Proj_Atom,R),
	atom_concat(R, ' FROM ',R1),
	atom_concat(R1, Relation, R2),
	atom_concat(R2, ' WHERE ', R3),
	'$make_where_atom'(Where,Where_Atom),
	atom_concat(R3,Where_Atom,SQL).


'$generate_dbrow_list'([],[_],0) :- !.         % important for empty projection terms.

'$generate_dbrow_list'([(_,V)],[V],1) :- !.

'$generate_dbrow_list'([(_,V)|T],[V|R],I1) :-
        '$generate_dbrow_list'(T,R,I), I1 is I+1.


'$make_proj_atom'([],'1') :- !.              % important for empty projection terms.

'$make_proj_atom'([(A,_)],A) :- !.

'$make_proj_atom'([(A,_)|T],Atom) :-
        '$make_proj_atom'(T,Atom1),
        atom_concat(A,',',Atom2),
	atom_concat(Atom2,Atom1,Atom).


'$make_where_atom'([v(Att,H)],Atom) :-
        !,
	atom_concat(Att,' = ',R),
	atom_concat(R,H,Atom).

'$make_where_atom'([v(Att,H)|T],Atom) :-
        '$make_where_atom'(T,Atom1),
        atom_concat(Att,' = ',R),
	atom_concat(R,H,Atom2),
	atom_concat(Atom2,  ' AND ', Atom3),
	atom_concat(Atom3,Atom1,Atom).


'$make_where_atom'([g(Att,H)],Atom) :-
        number(H),
	!,
	number_atom(H,H1),
	atom_concat(Att,' = \'',R),
	atom_concat(R,H1,R1),
	atom_concat(R1,'\'',Atom).

'$make_where_atom'([g(Att,H)],Atom) :-
        !,
	atom_concat(Att,' = \'',R),
	atom_concat(R,H,R1),
	atom_concat(R1,'\'',Atom).

'$make_where_atom'([g(Att,H)|T],Atom) :-
        number(H),
	!,
	number_atom(H,H1),
	'$make_where_atom'(T,Atom1),
	atom_concat(Att,' = \'',R),
	atom_concat(R,H1,R1),
	atom_concat(R1,'\'',Atom2),
	atom_concat(Atom2,  ' AND ', Atom3),
	atom_concat(Atom3,Atom1,Atom).

'$make_where_atom'([g(Att,H)|T],Atom) :-
        '$make_where_atom'(T,Atom1),
        atom_concat(Att,' = \'',R),
	atom_concat(R,H,R1),
	atom_concat(R1,'\'',Atom2),
	atom_concat(Atom2,  ' AND ', Atom3),
	atom_concat(Atom3,Atom1,Atom).


'$member_var'(H,[(V,Att)|_],Att) :-
        H == V,!.

'$member_var'(H,[_|T],Att) :-
        '$member_var'(H,T,Att).

% End of predicates for optimized translate



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% db_ilpview/4
%
%
db_ilpview(Connection,LA,ViewName,DbGoal):-
	
	functor(ViewName,PredName,Arity),
	functor(NewName,PredName,Arity),
	translate(ViewName,DbGoal,Code),
	queries_atom(Code,SQL),
	
	% build arg list for db_my_row/2
        '$make_list_of_args'(1,Arity,NewName,LA),

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
% db_view_original/3
%
%
db_view_original(PredName,DbGoal,Connection) :-
	'$error_checks'(db_my_view(PredName,DbGoal,Connection)),
	'$get_value'(Connection,Conn),
	'$assert_view_clause2'(PredName,DbGoal,Conn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$assert_view_clause2'(ViewName,DbGoal,Connection) :-
	% here we can add some error control, like checking DBgoals include
	% only DB relations
       	% get arity of projection term
	% PredName necessary for c_db_add_preds
	functor(ViewName,PredName,Arity),
	functor(NewName,PredName,Arity),
	db_module(Module),
	not c_db_my_check_if_exists_pred(PredName,Arity,Module),
	%'$copy_term_nv'(NewName,[],G,_),
	%translate(ViewName,DbGoal,Code),
	%queries_atom(Code,SQL),
	% build arg list for db_my_row/2
        '$make_list_of_args'(1,Arity,NewName,LA),
	% checks if the WHERE commend of SQL exists in the string
	%'$where_exists'(SQL,Flag),
	% build view clause
	Assert =..[':-',NewName,
		   ','(myddas_test_predicates:translate(ViewName,DbGoal,Code),
		   ','(myddas_test_predicates:queries_atom(Code,FinalSQL),
		   ','(myddas_test_predicates:db_my_result_set(Mode),
		   ','(myddas_test_predicates:c_db_my_query(FinalSQL,ResultSet,Connection,Mode),
		   ','(myddas_test_predicates:'$write_or_not'(FinalSQL),
		   ','(!,myddas_test_predicates:c_db_my_row(ResultSet,Arity,LA)))))))],
	assert(Module:Assert),
	% ALTERACAO
	% Adds PredName and Arity to this Connection List 
	% C Predicate
	c_db_add_preds(PredName,Arity,Module,Connection).





