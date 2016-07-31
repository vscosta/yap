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
* File:		myddas_mysql.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for comunicating with a mysql database system *
*									 *
*************************************************************************/

#if MYDDAS_MYSQL

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mysql.h>
#include "Yap.h"
#include "Yatom.h"
#include "cut_c.h"
#include "myddas_structs.h"
#include "myddas_util.h"
#ifdef MYDDAS_STATS
#include "myddas_statistics.h"
#endif
#include "myddas_wkb2prolog.h"

#define IS_SQL_INT(FIELD) FIELD == FIELD_TYPE_INT24    || \
	                  FIELD == FIELD_TYPE_LONG     || \
	                  FIELD == FIELD_TYPE_LONGLONG || \
	                  FIELD == FIELD_TYPE_SHORT    || \
	                  FIELD == FIELD_TYPE_TINY

#define IS_SQL_FLOAT(FIELD) FIELD == FIELD_TYPE_DECIMAL    || \
	                    FIELD == FIELD_TYPE_DOUBLE     || \
	                    FIELD == FIELD_TYPE_FLOAT

#define IS_SQL_GEOMETRY(FIELD) FIELD == FIELD_TYPE_GEOMETRY

static Int null_id = 0;

static Int c_db_my_connect( USES_REGS1 );
static Int c_db_my_disconnect( USES_REGS1 );
static Int c_db_my_number_of_fields( USES_REGS1 );
static Int c_db_my_get_attributes_types( USES_REGS1 );
static Int c_db_my_query( USES_REGS1 );
static Int c_db_my_table_write( USES_REGS1 );
static Int c_db_my_row( USES_REGS1 );
static Int c_db_my_row_cut( USES_REGS1 );
static Int c_db_my_get_fields_properties( USES_REGS1 );
static Int c_db_my_get_next_result_set( USES_REGS1 );
static Int c_db_my_get_database( USES_REGS1 );
static Int c_db_my_change_database( USES_REGS1 );

void Yap_InitMYDDAS_MySQLPreds(void)
{
  /* db_connect: Host x User x Passwd x Database x Connection x ERROR_CODE */
  Yap_InitCPred("c_db_my_connect", 7, c_db_my_connect,  0);

  /* db_number_of_fields: Relation x Connection x NumberOfFields */
  Yap_InitCPred("c_db_my_number_of_fields",3, c_db_my_number_of_fields, 0);

  /* db_get_attributes_types: Relation x TypesList */
  Yap_InitCPred("c_db_my_get_attributes_types", 3, c_db_my_get_attributes_types,  0);

  /* db_query: SQLQuery x ResultSet x Connection */
  Yap_InitCPred("c_db_my_query", 5, c_db_my_query, 0);

  /* db_disconnect: Connection */
  Yap_InitCPred("c_db_my_disconnect", 1,c_db_my_disconnect, 0);

  /* db_table_write: Result Set */
  Yap_InitCPred("c_db_my_table_write", 1, c_db_my_table_write,  0);

  /* db_get_fields_properties: PredName x Connnection x PropertiesList*/
  Yap_InitCPred("c_db_my_get_fields_properties",3,c_db_my_get_fields_properties,0);


  Yap_InitCPred("c_db_my_get_next_result_set",2,c_db_my_get_next_result_set,0);

  /* c_db_my_get_database: Connnection x DataBaseName */
  Yap_InitCPred("c_db_my_get_database",2,c_db_my_get_database,0);

  /* c_db_my_change_database: Connnection x DataBaseName */
  Yap_InitCPred("c_db_my_change_database",2,c_db_my_change_database,0);


}

void Yap_InitBackMYDDAS_MySQLPreds(void)
{
	/* db_row: ResultSet x Arity x ListOfArgs */
  Yap_InitCPredBackCut("c_db_my_row", 3, sizeof(Int),
		    c_db_my_row,
		    c_db_my_row,
		    c_db_my_row_cut, 0);

}

static Int
c_db_my_connect( USES_REGS1 ) {
  Term arg_host = Deref(ARG1);
  Term arg_user = Deref(ARG2);
  Term arg_passwd = Deref(ARG3);
  Term arg_database = Deref(ARG4);
  Term arg_port = Deref(ARG5);
  Term arg_socket = Deref(ARG6);
  Term arg_conn = Deref(ARG7);

  MYSQL *conn;

  MYDDAS_UTIL_CONNECTION new = NULL;

  const char *host = AtomName(AtomOfTerm(arg_host));
  const char *user = AtomName(AtomOfTerm(arg_user));
  const char *passwd = AtomName(AtomOfTerm(arg_passwd));
  const char *database = AtomName(AtomOfTerm(arg_database));
  Int port = IntegerOfTerm(arg_port);

  const char *socket;
  if (IsNonVarTerm(arg_socket))
    socket = AtomName(AtomOfTerm(arg_socket));
  else
    socket = NULL;

  conn = mysql_init(NULL);
  if (conn == NULL) {
#ifdef DEBUG
    printf("ERROR: ** c_db_my_connect ** error on init\n");
#endif
    return FALSE;
  }

  if (mysql_real_connect(conn, host, user, passwd, database, port, socket, CLIENT_MULTI_STATEMENTS) == NULL) {
#ifdef DEBUG
    printf("ERROR: ** c_db_my_connect ** error on connect\n");
#endif
    return FALSE;
  }

  if (!Yap_unify(arg_conn, MkIntegerTerm((Int)conn)))
    return FALSE;
  else
    {
      /* Criar um novo no na lista de ligacoes*/
      new = myddas_util_add_connection(conn,NULL,MYDDAS_MYSQL);

      if (new == NULL){
#ifdef DEBUG
	printf("ERROR: ** c_db_my_connect ** Error allocating memory\n");
#endif
	return FALSE;
      }
      return TRUE;
    }
}

/* db_query: SQLQuery x ResultSet x Connection */
static Int
c_db_my_query( USES_REGS1 ) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_conn = Deref(ARG3);
  Term arg_mode = Deref(ARG4);
  Term arg_arity = Deref(ARG5);

  const char *sql = AtomName(AtomOfTerm(arg_sql_query));
  const char *mode = AtomName(AtomOfTerm(arg_mode));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));

  MYSQL_RES *res_set;

  MyddasInt length=strlen(sql);

#ifdef MYDDAS_STATS
  MYDDAS_UTIL_CONNECTION node = myddas_util_search_connection(conn);
  MyddasULInt count = 0;

  /* Count the number of querys made to the server */
  MyddasULInt number_querys;
  MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE(node,number_querys);
  MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE(node,++number_querys);
  MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE_COUNT(node,count);
  MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE_COUNT(node,++count);

  /* Measure time spent by the MySQL Server
     processing the SQL Query */
  MYDDAS_STATS_TIME start,end,total_time,diff;
  start = myddas_stats_walltime();
#endif

  /* Send query to server and process it */
  if (mysql_real_query(conn, sql, length) != 0)
    {
#ifdef DEBUG
      printf("ERROR: **c_db_my_query** Error on query! %s\n",sql);
#endif
      return FALSE;
    }

#ifdef MYDDAS_STATS
  /* Measure time spent by the MySQL Server
     processing the SQL Query */
  end = myddas_stats_walltime();

  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
  myddas_stats_subtract_time(diff,end,start);
  diff = myddas_stats_time_copy_to_final(diff);

  MYDDAS_FREE(end,struct myddas_stats_time_struct);
  MYDDAS_FREE(start,struct myddas_stats_time_struct);

  MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER(node,total_time);
    /* Automacally updates the MYDDAS_STRUCTURE */
  myddas_stats_add_time(total_time,diff,total_time);
  MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER_COUNT(node,count);
  MYDDAS_STATS_CON_SET_TOTAL_TIME_DBSERVER_COUNT(node,++count);

  MYDDAS_STATS_TIME time = NULL;
  MYDDAS_STATS_CON_GET_LAST_TIME_DBSERVER(node,time);
  myddas_stats_move_time(diff,time);
  MYDDAS_STATS_CON_GET_LAST_TIME_DBSERVER_COUNT(node,count);
  MYDDAS_STATS_CON_SET_LAST_TIME_DBSERVER_COUNT(node,++count);
#endif

  /* guardar os tuplos do lado do cliente */
  if (strcmp(mode,"store_result")!=0) //True
    res_set = mysql_use_result(conn);
  else{

#ifdef MYDDAS_STATS
    /* Measure time spent by the MySQL Server
       transferring the result of the last query
       back to the client */
    start = myddas_stats_walltime();
#endif
    res_set = mysql_store_result(conn);
#ifdef MYDDAS_STATS
    /* Measure time spent by the MySQL Server
       transferring the result of the last query
       back to the client */
    end = myddas_stats_walltime();

    MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
    myddas_stats_subtract_time(diff,end,start);
    diff = myddas_stats_time_copy_to_final(diff);

    MYDDAS_FREE(end,struct myddas_stats_time_struct);
    MYDDAS_FREE(start,struct myddas_stats_time_struct);

    MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING(node,total_time);
    /* Automacally updates the MYDDAS_STRUCTURE */
    myddas_stats_add_time(total_time,diff,total_time);
    MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING_COUNT(node,count);
    MYDDAS_STATS_CON_SET_TOTAL_TIME_TRANSFERING_COUNT(node,++count);

    time = NULL;
    MYDDAS_STATS_CON_GET_LAST_TIME_TRANSFERING(node,time);
    MYDDAS_STATS_CON_GET_LAST_TIME_TRANSFERING_COUNT(node,count);
    MYDDAS_STATS_CON_SET_LAST_TIME_TRANSFERING_COUNT(node,++count);
    myddas_stats_move_time(diff,time);

    /* Measure the number of Rows returned from the server */
    if (res_set != NULL)
      {
	/* With an INSERT statement, mysql_(use or store)_result()
	   returns a NULL pointer*/

	/* This is only works if we use mysql_store_result */
	MyddasUInt numberRows = mysql_num_rows(res_set);
	MyddasUInt rows;

	MYDDAS_STATS_CON_GET_TOTAL_ROWS(node,rows);
	numberRows = numberRows + rows;
	MYDDAS_STATS_CON_SET_TOTAL_ROWS(node,numberRows);
	MYDDAS_STATS_CON_GET_TOTAL_ROWS_COUNT(node,count);
	MYDDAS_STATS_CON_SET_TOTAL_ROWS_COUNT(node,++count);

	/* Calculate the ammount of data sent by the server */
	MyddasUInt total,number_fields = mysql_num_fields(res_set);
	MYSQL_ROW row;
	MyddasULInt i;
	total=0;
	while ((row = mysql_fetch_row(res_set)) != NULL){
	  mysql_field_seek(res_set,0);

	  for(i=0;i<number_fields;i++){
	    if (row[i] != NULL)
	      total = total + strlen(row[i]);
	  }
	}
	MYDDAS_STATS_CON_SET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(node,total);
	MYDDAS_STATS_CON_GET_LAST_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(node,count);
	MYDDAS_STATS_CON_SET_LAST_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(node,++count);

	MyddasUInt bytes = 0;
	MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(node,bytes);
	total = total + bytes;
	MYDDAS_STATS_CON_SET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(node,total);
	MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(node,count);
	MYDDAS_STATS_CON_SET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER_COUNT(node,++count);
	mysql_data_seek(res_set,0);
      }
#endif

  }
  if (res_set == NULL)
    {
      //INSERT statements don't return any res_set
      if (mysql_field_count(conn) == 0)
	return TRUE;
#ifdef DEBUG
      printf("Empty Query!\n");
#endif
      return FALSE;
    }

  if (!Yap_unify(arg_arity, MkIntegerTerm(mysql_num_fields(res_set)))){
    return FALSE;
  }

  if (!Yap_unify(arg_result_set, MkIntegerTerm((Int) res_set)))
    {
      mysql_free_result(res_set);
      return FALSE;
    }
  else
    {
      return TRUE;
    }
}

static Int
c_db_my_number_of_fields( USES_REGS1 ) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);

  const char *relation = AtomName(AtomOfTerm(arg_relation));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));

  char sql[256];

  MYSQL_RES *res_set;

  sprintf(sql,"DESCRIBE `%s`",relation);

  /* executar a query SQL */
  if (mysql_query(conn, sql) != 0)
    {
#ifdef DEBUG
      printf("ERROR: **c_db_my_number_of_fields** Error on the query! %s\n",sql);
#endif
      return FALSE;
    }

  /* guardar os tuplos do lado do cliente */
  if ((res_set = mysql_store_result(conn)) == NULL)
    {
#ifdef DEBUG
      printf("ERROR: **c_db_my_number_of_fields** Error storing the query! %s\n",sql);
#endif

      return FALSE;
    }

  if (!Yap_unify(arg_fields, MkIntegerTerm(mysql_num_rows(res_set)))){
    mysql_free_result(res_set);
    return FALSE;
  }
  mysql_free_result(res_set);
  return TRUE;
}


/* db_get_attributes_types: RelName x Connection -> TypesList */
static Int
c_db_my_get_attributes_types( USES_REGS1 ) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_types_list = Deref(ARG3);

  const char *relation = AtomName(AtomOfTerm(arg_relation));
  MYSQL *conn = (MYSQL *) IntegerOfTerm(arg_conn);
  char sql[256];

  MYSQL_RES *res_set;
  MYSQL_ROW row;
  Term head, list;

  sprintf(sql,"DESCRIBE `%s`",relation);

  Int length = strlen(sql);

  /* executar a query SQL */
  if (mysql_real_query(conn, sql, length) != 0)
    {
#ifdef DEBUG
      printf("Erro na query! %s\n",sql);
#endif
      return FALSE;
    }
  /* guardar os tuplos do lado do cliente */
  if ((res_set = mysql_store_result(conn)) == NULL)
  {
#ifdef DEBUG
      printf("Query vazia!\n");
#endif
      return FALSE;
  }

  list = arg_types_list;

  while ((row = mysql_fetch_row(res_set)) != NULL)
  {
    head = HeadOfTerm(list);
    Yap_unify(head, MkAtomTerm(Yap_LookupAtom(row[0])));
    list = TailOfTerm(list);
    head = HeadOfTerm(list);
    list = TailOfTerm(list);

    if (strncmp(row[1], "smallint",8) == 0 || strncmp(row[1],"int",3) == 0 ||
	strncmp(row[1], "mediumint",9) == 0 || strncmp(row[1], "tinyint",7) == 0 ||
	strncmp(row[1], "bigint",6) == 0 || strcmp(row[1], "year") == 0)
      Yap_unify(head, MkAtomTerm(Yap_LookupAtom("integer")));
    else if (strcmp(row[1], "float") == 0 || strncmp(row[1], "double",6) == 0
	     || strcmp(row[1], "real") == 0)
      Yap_unify(head, MkAtomTerm(Yap_LookupAtom("real")));
    else Yap_unify(head, MkAtomTerm(Yap_LookupAtom("string")));
  }

  mysql_free_result(res_set);
  return TRUE;

}

/* db_disconnect */
static Int
c_db_my_disconnect( USES_REGS1 ) {
  Term arg_conn = Deref(ARG1);

  MYSQL *conn = (MYSQL *) IntegerOfTerm(arg_conn);

  if ((myddas_util_search_connection(conn)) != NULL)
    {
      myddas_util_delete_connection(conn);
      mysql_close(conn);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

/* db_table_write: Result Set */
static Int
c_db_my_table_write( USES_REGS1 ) {
  Term arg_res_set = Deref(ARG1);

  MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_res_set);

  myddas_util_table_write(res_set);
  mysql_free_result(res_set);

  return TRUE;
}

static Int
c_db_my_row_cut( USES_REGS1 ) {
  MYSQL_RES *mysql_res=NULL;

  mysql_res = (MYSQL_RES *) IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,1));
  mysql_free_result(mysql_res);
  return TRUE;
}

/* db_row: ResultSet x Arity_ListOfArgs x ListOfArgs -> */
static Int
c_db_my_row( USES_REGS1 ) {
#ifdef MYDDAS_STATS
/*   Measure time used by the  */
/*      c_db_my_row function */
  MYDDAS_STATS_TIME start,end,total_time,diff;
  MyddasULInt count = 0;
  start = myddas_stats_walltime();
#endif
  Term arg_result_set = Deref(ARG1);
  Term arg_arity = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);

  MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_result_set);
  EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((Int)res_set);
  MYSQL_ROW row;
  MYSQL_FIELD *field;


  Term head, list, null_atom[1];
  Int i, arity;

  arity = IntegerOfTerm(arg_arity);

  while(TRUE)
    {
      if ((row = mysql_fetch_row(res_set)) != NULL)
	{
	  mysql_field_seek(res_set,0);
	  list = arg_list_args;

	  for (i = 0; i < arity; i++)
	    {
	      /* Here we perform data type conversion. */
	      field = mysql_fetch_field(res_set);
	      head = HeadOfTerm(list);
	      list = TailOfTerm(list);

	      if (row[i] == NULL)
		{
		  null_atom[0] = MkIntegerTerm(null_id++);

		  if (!Yap_unify(head, Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("null"),1),1,null_atom)))
		    continue;
		}
	      else
		{
		  if (IS_SQL_INT(field->type))
		    {
		      if (!Yap_unify(head, MkIntegerTerm(atoi(row[i]))))
			continue;
		    }
		  else if (IS_SQL_FLOAT(field->type))
		    {
		      if (!Yap_unify(head, MkFloatTerm(atof(row[i]))))
			continue;
		    }
		  else if (IS_SQL_GEOMETRY(field->type))
		    {
		      if (!Yap_unify(head, wkb2prolog(row[i])))
			continue;
		    }
		  else
		    {
		      if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(row[i]))))
			continue;
		    }
		}
	    }
#ifdef MYDDAS_STATS
	  end = myddas_stats_walltime();

	  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
	  myddas_stats_subtract_time(diff,end,start);
	  diff = myddas_stats_time_copy_to_final(diff);

	  MYDDAS_FREE(end,struct myddas_stats_time_struct);
	  MYDDAS_FREE(start,struct myddas_stats_time_struct);

	  MYDDAS_STATS_GET_DB_ROW_FUNCTION(total_time);
	  myddas_stats_add_time(total_time,diff,total_time);
	  MYDDAS_STATS_GET_DB_ROW_FUNCTION_COUNT(count);
	  MYDDAS_STATS_SET_DB_ROW_FUNCTION_COUNT(++count);

	  MYDDAS_FREE(diff,struct myddas_stats_time_struct);
#endif /* MYDDAS_STATS */
	  return TRUE;
	}
      else
	{
	  mysql_free_result(res_set);
#ifdef MYDDAS_STATS
	  end = myddas_stats_walltime();

	  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
	  myddas_stats_subtract_time(diff,end,start);
	  diff = myddas_stats_time_copy_to_final(diff);

	  MYDDAS_FREE(end,struct myddas_stats_time_struct);
	  MYDDAS_FREE(start,struct myddas_stats_time_struct);

	  MYDDAS_STATS_GET_DB_ROW_FUNCTION(total_time);
	  myddas_stats_add_time(total_time,diff,total_time);
	  MYDDAS_STATS_GET_DB_ROW_FUNCTION_COUNT(count);
	  MYDDAS_STATS_SET_DB_ROW_FUNCTION_COUNT(++count);

	  MYDDAS_FREE(diff,struct myddas_stats_time_struct);
#endif /* MYDDAS_STATS */
	  cut_fail();  /* This macro already does a return FALSE */
	}

    }

}

static Int
c_db_my_get_fields_properties( USES_REGS1 ) {
  Term nome_relacao = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term fields_properties_list = Deref(ARG3);
  Term head, list;

  const char *relacao = AtomName(AtomOfTerm(nome_relacao));
  char sql[256];
  Int num_fields,i;
  MYSQL_FIELD *fields;
  MYSQL_RES *res_set;
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));


  /* LIMIT 0 -> We only need the meta information about the fields
     to know their properties, we don't need the results of the
     query*/
  sprintf (sql,"SELECT * FROM `%s` LIMIT 0",relacao);

  Int length=strlen(sql);

  /* executar a query SQL */
  if (mysql_real_query(conn, sql, length) != 0)
    {
#ifdef DEBUG
      printf("Erro na query! %s\n",sql);
#endif
      return FALSE;
    }

  Functor functor = Yap_MkFunctor(Yap_LookupAtom("property"),4);

  Term properties[4];


  /* guardar os tuplos do lado do cliente */
  /* nao precisamos do resultado, mas apenas no res_set */
  /* para obter a informa��o atrav�s do mysql_fetch_fields*/
  res_set = mysql_store_result(conn);

  num_fields = mysql_num_fields(res_set);
  fields = mysql_fetch_fields(res_set);

  list = fields_properties_list;



  for (i=0;i<num_fields;i++)
    {
      head = HeadOfTerm(list);

      properties[0] = MkAtomTerm(Yap_LookupAtom(fields[i].name));

      if (fields[i].flags & NOT_NULL_FLAG)
	properties[1] = MkIntegerTerm(1); //Can't be NULL
      else
	properties[1] = MkIntegerTerm(0);

      if (fields[i].flags & PRI_KEY_FLAG)
	properties[2] = MkIntegerTerm(1); //It''s a primary key
      else
	properties[2] = MkIntegerTerm(0);

      if (fields[i].flags & AUTO_INCREMENT_FLAG)
	properties[3] = MkIntegerTerm(1); //It's auto_incremented field
      else
	properties[3] = MkIntegerTerm(0);


      list = TailOfTerm(list);
      if (!Yap_unify(head, Yap_MkApplTerm(functor,4,properties))){
      	return FALSE;
      }
    }

  mysql_free_result(res_set);

  return TRUE;
}

/* c_db_my_get_next_result_set: Connection * NextResSet */
static Int
c_db_my_get_next_result_set( USES_REGS1 ) {
  Term arg_conn = Deref(ARG1);
  Term arg_next_res_set = Deref(ARG2);

  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));
  MYSQL_RES *res_set=NULL;

  if (mysql_next_result(conn) == 0){
    res_set = mysql_store_result(conn);
    Yap_unify(arg_next_res_set, MkIntegerTerm((Int) res_set));
  }
  return TRUE;
}

static Int
c_db_my_get_database( USES_REGS1 ) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);

  MYSQL *con = (MYSQL *) (IntegerOfTerm(arg_con));

  if (!Yap_unify(arg_database,MkAtomTerm(Yap_LookupAtom(con->db))))
    return FALSE;

  return TRUE;

}

static Int
c_db_my_change_database( USES_REGS1 ) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);

  MYSQL *con = (MYSQL *) (IntegerOfTerm(arg_con));
  const char *database = AtomName(AtomOfTerm(arg_database));

  if (mysql_select_db(con,database)!=0)
    return FALSE;

  return TRUE;
}

#else

void Yap_InitMYDDAS_MySQLPreds(void);
void Yap_InitBackMYDDAS_MySQLPreds(void);

void Yap_InitMYDDAS_MySQLPreds(void)
{
}

void Yap_InitBackMYDDAS_MySQLPreds(void)
{
}

#endif

void init_mysql( void )
{
    Yap_InitMYDDAS_MySQLPreds();
    Yap_InitBackMYDDAS_MySQLPreds();
}


#ifdef _WIN32

#include <windows.h>

int WINAPI win_mysql(HANDLE hinst, DWORD reason, LPVOID reserved);

int WINAPI win_mysql(HANDLE hinst, DWORD reason, LPVOID reserved) {
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    break;
  case DLL_PROCESS_DETACH:
    break;
  case DLL_THREAD_ATTACH:
    break;
  case DLL_THREAD_DETACH:
    break;
  }
  return 1;
}
#endif
