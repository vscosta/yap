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

#if defined MYDDAS_MYSQL && defined CUT_C

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mysql/mysql.h>
#include "Yap.h"
#include "Yatom.h"
#include "cut_c.h"
#include "myddas_util.h"
#ifdef MYDDAS_STATS
#include "myddas_structs.h"
#include "myddas_statistics.h"
#endif

#define IS_SQL_INT(FIELD) FIELD == FIELD_TYPE_INT24    || \
	                  FIELD == FIELD_TYPE_LONG     || \
	                  FIELD == FIELD_TYPE_LONGLONG || \
	                  FIELD == FIELD_TYPE_SHORT    || \
	                  FIELD == FIELD_TYPE_TINY

#define IS_SQL_FLOAT(FIELD) FIELD == FIELD_TYPE_DECIMAL    || \
	                    FIELD == FIELD_TYPE_DOUBLE     || \
	                    FIELD == FIELD_TYPE_FLOAT 


static int null_id = 0;

STATIC_PROTO(int c_db_my_connect,(void));
STATIC_PROTO(int c_db_my_disconnect,(void));
STATIC_PROTO(int c_db_my_number_of_fields,(void));
STATIC_PROTO(int c_db_my_get_attributes_types,(void));
STATIC_PROTO(int c_db_my_query,(void));
STATIC_PROTO(int c_db_my_table_write,(void));
STATIC_PROTO(int c_db_my_row,(void));
STATIC_PROTO(int c_db_my_row_cut,(void));
STATIC_PROTO(int c_db_my_get_fields_properties,(void));
STATIC_PROTO(int c_db_my_number_of_fields_in_query,(void));
STATIC_PROTO(int c_db_my_get_next_result_set,(void));
STATIC_PROTO(int c_db_my_get_database,(void));
STATIC_PROTO(int c_db_my_change_database,(void));

void Yap_InitMYDDAS_MySQLPreds(void)
{
  /* db_connect: Host x User x Passwd x Database x Connection x ERROR_CODE */
  Yap_InitCPred("c_db_my_connect", 5, c_db_my_connect,  SafePredFlag|SyncPredFlag|HiddenPredFlag);
  
  /* db_number_of_fields: Relation x Connection x NumberOfFields */
  Yap_InitCPred("c_db_my_number_of_fields",3, c_db_my_number_of_fields, 0);  
  
  /* db_number_of_fields_in_query: SQLQuery x Connection x NumberOfFields */
  Yap_InitCPred("c_db_my_number_of_fields_in_query",3, c_db_my_number_of_fields_in_query, 0);  
  
  /* db_get_attributes_types: Relation x TypesList */
  Yap_InitCPred("c_db_my_get_attributes_types", 3, c_db_my_get_attributes_types,  0);  
  
  /* db_query: SQLQuery x ResultSet x Connection */
  Yap_InitCPred("c_db_my_query", 4, c_db_my_query, 0);  
  
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
  Yap_InitCPredBackCut("c_db_my_row", 3, sizeof(int),
		    c_db_my_row,
		    c_db_my_row,
		    c_db_my_row_cut, 0);

}

static int
c_db_my_connect(void) {
  Term arg_host = Deref(ARG1); 
  Term arg_user = Deref(ARG2);
  Term arg_passwd = Deref(ARG3);
  Term arg_database = Deref(ARG4);
  Term arg_conn = Deref(ARG5);  

  MYSQL *conn;

  MYDDAS_UTIL_CONNECTION new = NULL;

  char *host = AtomName(AtomOfTerm(arg_host));
  char *user = AtomName(AtomOfTerm(arg_user));
  char *passwd = AtomName(AtomOfTerm(arg_passwd));
  char *database = AtomName(AtomOfTerm(arg_database));

    
  conn = mysql_init(NULL);
  if (conn == NULL) {
    printf("erro no init\n");
    return FALSE;
  }

  if (mysql_real_connect(conn, host, user, passwd, database,0, NULL, CLIENT_MULTI_STATEMENTS) == NULL) {
    printf("erro no connect\n");
    return FALSE;
  }

  if (!Yap_unify(arg_conn, MkIntegerTerm((int)conn)))
    return FALSE;
  else
    {
      /* Criar um novo no na lista de ligacoes*/
      //new = add_connection(&TOP,conn,NULL);
      new = myddas_util_add_connection(conn,NULL);
      if (new == NULL){
	printf("Erro ao alocar memoria para lista\n");
	return FALSE;
      }
      return TRUE;
    }
}

/* db_query: SQLQuery x ResultSet x Connection */
static int 
c_db_my_query(void) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_conn = Deref(ARG3);
  Term arg_mode = Deref(ARG4);
  
  char *sql = AtomName(AtomOfTerm(arg_sql_query));
  char *mode = AtomName(AtomOfTerm(arg_mode));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));
  
  MYSQL_RES *res_set;
  
  int length=strlen(sql);

#ifdef MYDDAS_STATS 
  MYDDAS_UTIL_CONNECTION node = myddas_util_search_connection(conn);

  /* Count the number of querys made to the server */
  unsigned long number_querys;
  MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE(node,number_querys);
  MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE(node,++number_querys);

  /* Measure time spent by the MySQL Server 
     processing the SQL Query */ 
  MYDDAS_STATS_TIME start,end,total_time,diff;
  start = myddas_stats_walltime();
#endif 
  
  /* executar a query SQL */
  if (mysql_real_query(conn, sql, length) != 0)
    {
#ifdef DEBUG
      printf("Erro na query! %s\n",sql);
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
  
  free(end);
  free(start);
  
  MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER(node,total_time);
  
  /* Automacally updates the MYDDAS_STRUCTURE */
  myddas_stats_add_time(total_time,diff,total_time);
  
  myddas_stats_move_time(diff,node->lastTimeofDBServer);
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
    
    free(end);
    free(start);
    
    MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING(node,total_time);
    
    /* Automacally updates the MYDDAS_STRUCTURE */
    myddas_stats_add_time(total_time,diff,total_time);
        
    myddas_stats_move_time(diff,node->lastFromDBServer);
    
    /* Measure the number of Rows returned from the server */
    if (res_set != NULL)
      {
	/* With an INSERT statement, mysql_(use or store)_result()
	   returns a NULL pointer*/
	
	/* This is only works if we use mysql_store_result */
	unsigned long numberRows = mysql_num_rows(res_set);
	unsigned long rows;
	
	MYDDAS_STATS_CON_GET_TOTAL_ROWS(node,rows);
	numberRows = numberRows + rows;
	MYDDAS_STATS_CON_SET_TOTAL_ROWS(node,numberRows);
      
	/* Calculate the ammount of data sent by the server */
	unsigned long int total,number_fields = mysql_num_fields(res_set);
	MYSQL_ROW row;
	unsigned int i;
	total=0;
	while ((row = mysql_fetch_row(res_set)) != NULL){
	  mysql_field_seek(res_set,0);
	  
	  for(i=0;i<number_fields;i++){
	    if (row[i] != NULL)
	      total = total + strlen(row[i]);
	  }
	}
	MYDDAS_STATS_CON_SET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(node,total);
	unsigned long bytes;
	
	MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(node,bytes);
	total = total + bytes;
	MYDDAS_STATS_CON_SET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(node,total);
	mysql_data_seek(res_set,0);
      }
#endif 

  }
  if (res_set == NULL)
    {
      //INSERT statements don't return any res_set
      if (mysql_field_count(conn) == 0)
	return TRUE;
      printf("Query vazia!\n");
      return FALSE;
    }
  
  if (!Yap_unify(arg_result_set, MkIntegerTerm((int) res_set)))
    {
      mysql_free_result(res_set);
      return FALSE;
    }
  else
    {
      return TRUE;
    }
}

static int 
c_db_my_number_of_fields(void) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);
  
  char *relation = AtomName(AtomOfTerm(arg_relation));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));

  char sql[256];

  MYSQL_RES *res_set;

  sprintf(sql,"DESCRIBE %s",relation);

  /* executar a query SQL */
  if (mysql_query(conn, sql) != 0)
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
      printf("Erro na query! %s\n",sql);
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
static int 
c_db_my_get_attributes_types(void) {
  Term arg_relation = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_types_list = Deref(ARG3);

  char *relation = AtomName(AtomOfTerm(arg_relation));
  MYSQL *conn = (MYSQL *) IntegerOfTerm(arg_conn);
  char sql[256];

  MYSQL_RES *res_set;
  MYSQL_ROW row;
  Term head, list;

  sprintf(sql,"DESCRIBE %s",relation);

  /* executar a query SQL */
  if (mysql_query(conn, sql) != 0)
    {
#ifdef DEBUG
      printf("Erro na query! %s\n",sql);
#endif
      return FALSE;
    }
  /* guardar os tuplos do lado do cliente */
  if ((res_set = mysql_store_result(conn)) == NULL)
  {
      printf("Query vazia!\n");
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
static int
c_db_my_disconnect(void) {
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
static int 
c_db_my_table_write(void) {
  Term arg_res_set = Deref(ARG1);

  MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_res_set);
  
  myddas_util_table_write(res_set);
  mysql_free_result(res_set);
  
  return TRUE;  
}

static int
c_db_my_row_cut(void) {
  MYSQL_RES *mysql_res=NULL;
  
  mysql_res = (MYSQL_RES *) IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,1));
  mysql_free_result(mysql_res);
  return TRUE;
}

/* db_row: ResultSet x Arity_ListOfArgs x ListOfArgs -> */
static int
c_db_my_row(void) {
#ifdef MYDDAS_STATS
  /* Measure time used by the 
     c_db_my_row function */
  MYDDAS_STATS_TIME start,end,total_time,diff;
  start = myddas_stats_walltime();
#endif 
  Term arg_result_set = Deref(ARG1);
  Term arg_arity = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);
    
  MYSQL_RES *res_set = (MYSQL_RES *) IntegerOfTerm(arg_result_set);
  EXTRA_CBACK_ARG(3,1)=(CELL) MkIntegerTerm((int)res_set);
  MYSQL_ROW row;
  MYSQL_FIELD *field;
  
  
  Term head, list, null_atom[1];
  int i, arity;
  
  arity = IntegerOfTerm(arg_arity);

  while(TRUE)
    {
      if ((row = mysql_fetch_row(res_set)) != NULL)
	{
	  mysql_field_seek(res_set,0); 
	  list = arg_list_args;
	  
	  for (i = 0; i < arity; i++)
	    {
	      /* Aqui serão feitas as conversões de tipos de dados */
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
	  
	  free(end);
	  free(start);
	  
	  MYDDAS_STATS_GET_DB_ROW_FUNCTION(total_time);
	  myddas_stats_add_time(total_time,diff,total_time);
	  
	  free(diff);
#endif /* MYDDAS_STATS */
	  return TRUE;
	}
      else 
	{
	  mysql_free_result(res_set);
	  cut_fail();
#ifdef MYDDAS_STATS
	  end = myddas_stats_walltime();
	  
	  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
	  myddas_stats_subtract_time(diff,end,start);
	  diff = myddas_stats_time_copy_to_final(diff);
	  
	  free(end);
	  free(start);
	  
	  MYDDAS_STATS_GET_DB_ROW_FUNCTION(total_time);
	  myddas_stats_add_time(total_time,diff,total_time);
	  
	  free(diff);
#endif /* MYDDAS_STATS */
	  return FALSE;
	}
    }
}


/* Mudar esta funcao de forma a nao fazer a consulta, pois 
 no predicate db_sql_selet vai fazer duas vezes a mesma consutla*/ 
static int 
c_db_my_number_of_fields_in_query(void) {
  Term arg_query = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term arg_fields = Deref(ARG3);

  char *query = AtomName(AtomOfTerm(arg_query));
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));

  MYSQL_RES *res_set;

  /* executar a query SQL */
  if (mysql_query(conn, query) != 0)
    {
#ifdef DEBUG
      printf("Erro na query! %s\n",query);
#endif
      return FALSE;
    }
  
  /* guardar os tuplos do lado do cliente */
  if ((res_set = mysql_store_result(conn)) == NULL)
    {
#ifdef DEBUG
      printf("Erro na query! %s\n",query);
#endif
      return FALSE;
    }
  
  if (!Yap_unify(arg_fields, MkIntegerTerm(mysql_num_fields(res_set)))){
    return FALSE;
  }
  mysql_free_result(res_set);
  return TRUE;  
}

static int 
c_db_my_get_fields_properties(void) {
  Term nome_relacao = Deref(ARG1);
  Term arg_conn = Deref(ARG2);
  Term fields_properties_list = Deref(ARG3);
  Term head, list;

  char *relacao = AtomName(AtomOfTerm(nome_relacao));
  char sql[256];
  int num_fields,i;
  MYSQL_FIELD *fields;
  MYSQL_RES *res_set;
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));
  
  
  /* 1=2 -> We only need the meta information about the fields
     to know their properties, we don't need the results of the 
     query*/
  sprintf (sql,"SELECT * FROM %s where 1=2",relacao);

  /* executar a query SQL */
  if (mysql_query(conn, sql) != 0)
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
  /* para obter a informação através do mysql_fetch_fields*/
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
static int 
c_db_my_get_next_result_set(void) {
  Term arg_conn = Deref(ARG1);
  Term arg_next_res_set = Deref(ARG2);
  
  MYSQL *conn = (MYSQL *) (IntegerOfTerm(arg_conn));
  MYSQL_RES *res_set=NULL;
  
  if (mysql_next_result(conn) == 0){
    res_set = mysql_store_result(conn);
    Yap_unify(arg_next_res_set, MkIntegerTerm((int) res_set));
  }
  return TRUE;
}

static int 
c_db_my_get_database(void) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);
  
  MYSQL *con = (MYSQL *) (IntegerOfTerm(arg_con));

  if (!Yap_unify(arg_database,MkAtomTerm(Yap_LookupAtom(con->db))))
    return FALSE;
  
  return TRUE;
  
}

static int 
c_db_my_change_database(void) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);

  MYSQL *con = (MYSQL *) (IntegerOfTerm(arg_con));
  char *database = AtomName(AtomOfTerm(arg_database));

  if (mysql_select_db(con,database)!=0)
    return FALSE;

  return TRUE;
}

#endif /*MYDDAS_MYSQL && CUT_C*/
