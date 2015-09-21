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
* File:		myddas_sqlite3.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for comunicating with a sqlite3 database system *
*									 *
*************************************************************************/

#if defined MYDDAS_sqlite3

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include "Yap.h"
#include "Yatom.h"
#include "YapText.h"
#include "cut_c.h"
#include "myddas.h"
#ifdef MYDDAS_STATS
#include "myddas_structs.h"
#include "myddas_statistics.h"
#endif
#include "myddas_wkb2prolog.h"

#define CALL_SQLITE(f)                                          \
{                                                           \
	int i;                                                  \
	i = sqlite3_ ## f;                                      \
	if (i != SQLITE_OK) {                                   \
		fprintf (stderr, "%s failed with status %d: %s\n",  \
		#f, i, sqlite3_errmsg (db));               \
		exit (1);                                           \
	}                                                       \
}                                                           \

#define CALL_SQLITE_EXPECT(f,x)                                 \
{                                                           \
	int i;                                                  \
	i = sqlite3_ ## f;                                      \
	if (i != SQLITE_ ## x) {                                \
		fprintf (stderr, "%s failed with status %d: %s\n",  \
		#f, i, sqlite3_errmsg (db));               \
		exit (1);                                           \
	}                                                       \
}                                                           \

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

static Int c_db_lite_connect( USES_REGS1 );
static Int c_db_lite_disconnect( USES_REGS1 );
static Int c_db_lite_number_of_fields( USES_REGS1 );
static Int c_db_lite_get_attributes_types( USES_REGS1 );
static Int c_db_lite_query( USES_REGS1 );
static Int c_db_lite_table_write( USES_REGS1 );
static Int c_db_lite_row( USES_REGS1 );
static Int c_db_lite_row_cut( USES_REGS1 );
static Int c_db_lite_get_fields_properties( USES_REGS1 );
static Int c_db_lite_get_next_result_set( USES_REGS1 );
static Int c_db_lite_get_database( USES_REGS1 );
static Int c_db_lite_change_database( USES_REGS1 );

void Yap_InitMYDDAS_SQLitePreds(void)
{
  /* db_dbect: Host x User x Passwd x Database x dbection x ERROR_CODE */
  Yap_InitCPred("c_db_lite_connect", 7, c_db_lite_connect,  0);

  /* db_number_of_fields: Relation x connection x NumberOfFields */
  Yap_InitCPred("c_db_lite_number_of_fields",3, c_db_lite_number_of_fields, 0);

  /* db_get_attributes_types: Relation x TypesList */
  Yap_InitCPred("c_db_lite_get_attributes_types", 3, c_db_lite_get_attributes_types,  0);

  /* db_query: SQLQuery x ResultSet x conection */
  Yap_InitCPred("c_db_lite_query", 5, c_db_lite_query, 0);

  /* db_disconnect: connection */
  Yap_InitCPred("c_db_lite_disconnect", 1,c_db_lite_disconnect, 0);

  /* db_table_write: Result Set */
  Yap_InitCPred("c_db_lite_table_write", 1, c_db_lite_table_write,  0);

  /* db_get_fields_properties: PredName x connection x PropertiesList*/
  Yap_InitCPred("c_db_lite_get_fields_properties",3,c_db_lite_get_fields_properties,0);


  Yap_InitCPred("c_db_lite_get_next_result_set",2,c_db_lite_get_next_result_set,0);

  /* c_db_lite_get_database: connection x DataBaseName */
  Yap_InitCPred("c_db_lite_get_database",2,c_db_lite_get_database,0);

  /* c_db_lite_change_database: connection x DataBaseName */
  Yap_InitCPred("c_db_lite_change_database",2,c_db_lite_change_database,0);


}

void Yap_InitBackMYDDAS_sqlite3Preds(void)
{
	/* db_row: ResultSet x Arity x ListOfArgs */
  Yap_InitCPredBackCut("c_db_lite_row", 3, sizeof(Int),
		    c_db_lite_row,
		    c_db_lite_row,
		    c_db_lite_row_cut, 0);

}

static Int
c_db_lite_connect( USES_REGS1 ) {

  Term arg_file = Deref(ARG1);
  Term arg_db = ARG2;

  MYDDAS_UTIL_CONNECTION new = NULL;
  sqlite3 *db;

  char *file = AtomName(AtomOfTerm(arg_file));

  rc = CALL_SQLITE(sqlite3_open(argv[1], &db) );

  if (!Yap_unify(arg_db, MkIntegerTerm((Int)db)))
	return FALSE;
  else
    {
      /* Criar um novo no na lista de ligacoes*/
      new = myddas_util_add_connection(db,NULL);

      if (new == NULL){
#ifdef DEBUG
	fprintf(stderror, "ERROR: ** c_db_my_connect ** Error allocating memory\n");
#endif
	return FALSE;
      }
      return TRUE;
    }

}

static int callback(void *data, int argc, char **argv, char **azColName){
  int i;
  fprintf(stderr, "%s: ", (const char*)data);
  t0 = tf;
  for(i=0; i<argc; i++){
    const char *s = argc[argc-(i+1)];
    if (s === NULL)
      s = NULL;
    tf = MkPairTerm(Yap_CharsToTDQ( s, CurrentModule PASS_REGS ),
		    tf);
  }
  printf("\n");
  return 0;
}

static MYDDAS_STATS_TIME
myddas_stat_init_query( sqlite3 *db )
{
#ifdef MYDDAS_STATS
  MYDDAS_UTIL_connecTION node = myddas_util_search_connection(db);
  MyddasULInt count = 0;

  /* Count the number of querys made to the server */
  MyddasULInt number_querys;
  MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE(node,number_querys);
  MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE(node,++number_querys);
  MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE_COUNT(node,count);
  MYDDAS_STATS_CON_SET_NUMBER_QUERIES_MADE_COUNT(node,++count);
  /* Measure time spent by the sqlite3 Server
     processing the SQL Query */
  return myddas_stats_walltime();
#endif
}


static MYDDAS_STATS_TIME
myddas_stat_end_query( MYDDAS_STATS_TIME start )
{
#ifdef MYDDAS_STATS
  MYDDAS_STATS_TIME diff;
  /* Measure time spent by the sqlite3 Server
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
  return diff;
}

static void
myddas_stat_transfer_query( MYDDAS_STATS_TIME diff )
{
#ifdef MYDDAS_STATS_DISABLED
  /* Measure time spent by the sqlite3 Server
     transferring the result of the last query
     back to the client */
  start = myddas_stats_walltime();
  /* Measure time spent by the sqlite3 Server
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
      /* With an INSERT statement, sqlite3_(use or store)_result()
         returns a NULL pointer*/

      /* This is only works if we use sqlite3_store_result */
      MyddasUInt numberRows = sqlite3_num_rows(res_set);
      MyddasUInt rows;

      MYDDAS_STATS_CON_GET_TOTAL_ROWS(node,rows);
      numberRows = numberRows + rows;
      MYDDAS_STATS_CON_SET_TOTAL_ROWS(node,numberRows);
      MYDDAS_STATS_CON_GET_TOTAL_ROWS_COUNT(node,count);
      MYDDAS_STATS_CON_SET_TOTAL_ROWS_COUNT(node,++count);

      /* Calculate the ammount of data sent by the server */
      MyddasUInt total,number_fields = sqlite3_num_fields(res_set);
      sqlite3_ROW row;
      MyddasULInt i;
      total=0;
      while ((row = sqlite3_fetch_row(res_set)) != NULL){
        sqlite3_field_seek(res_set,0);

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
    }
#endif
}

/* db_query: SQLQuery x ResultSet x connection */
static Int
c_db_lite_query( USES_REGS1 ) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_db = Deref(ARG3);
  Term arg_mode = Deref(ARG4);
  Term arg_arity = Deref(ARG5);

  char *sql = AtomName(AtomOfTerm(arg_sql_query));
  char *mode = AtomName(AtomOfTerm(arg_mode));
  sqlite3 *db = (sqlite3 *) (IntegerOfTerm(arg_db));

  MYDDAS_STATS_TIME start, diff;  
  MyddasInt length=strlen(sql);

  start = myddas_stat_init_query( db );

   /* Send query to server and process it */
  if (strcmp(mode,"store_result")!=0) {
    // Leave data for extraction
    SQL_LITE (sqlite3_exec(db, sql, length) );
    end = myddas_stat_end_query( start );

  } else{
    // construct an intermediate table, res_set
    char *res_set[];
    int nrows;
    
    SQL_LITE (sqlite3_get_table(db, sql, &res_set, &nrows &length, &msg) );

    end = myddas_stat_end_query( start );
    if (res_set == NULL)
      {
#ifdef DEBUG
        printf("Empty Query!\n");
#endif
        return TRUE;
      }
    //INSERT statements don't return any res_set
    if (nrows == 0) {
      return TRUE;
    }
    if (!Yap_unify(arg_arity, MkIntegerTerm(nrows))){
      SQL_LITE (sqlite3_free_table(res_set) );
      return FALSE;
    }
    struct result_set *rs = malloc(sizeof( struct result_set));
    rs->res_set = res_set;
    rs->nrows = nrows;
    rs->length = length;
    if (!Yap_unify(arg_result_set, MkIntegerTerm((Int) rs)))
      {
	SQL_LITE (sqlite3_free_table(res_set) );
	return FALSE;
      }
  }
  return TRUE;
}


static Int
c_db_lite_number_of_fields( USES_REGS1 ) {
  Term arg_relation = Deref(ARG1);
  Term arg_db = Deref(ARG2);
  Term arg_fields = ARG3;

  char *relation = AtomName(AtomOfTerm(arg_relation));
  sqlite3 *db = (sqlite3 *) (IntegerOfTerm(arg_db));
  sqlite3_stmt *pStmt;
  
  char sql[256];

  sprintf(sql,"SELECT * FROM TABLE `%s`",relation);

  /* executar a query SQL */
  SQL_LITE (sqlite3_prepare_v2(db, sql, -1, stmt, NULL) );
  
  int fields = sqlite3_column_count( stmt );

  SQL_LITE (sqlite3_finalize( stmt ) );
  
  return Yap_unify(arg_fields, MkIntegerTerm( fields ));
}


/* db_get_attributes_types: RelName x connection -> TypesList */
static Int
c_db_lite_get_attributes_types( USES_REGS1 ) {
  Term arg_relation = Deref(ARG1);
  Term arg_db = Deref(ARG2);
  Term arg_types_list = Deref(ARG3);

  char *relation = AtomName(AtomOfTerm(arg_relation));
  sqlite3 *db = (sqlite3 *) IntegerOfTerm(arg_db);
  char sql[256];

  sqlite3_stmt *stmt;
  Int rc = TRUE;
  
  char sql[256];

  sprintf(sql,"SELECT * FROM TABLE `%s`",relation);

  /* executar a query SQL */
  SQL_LITE (sqlite3_prepare_v2(db, sql, -1, stmt, NULL) );
  
  int fields = sqlite3_column_count( stmt );

  list = arg_types_list;

  for (row = 0; row < fields; row++)
  {
    const char *tm;        

    head = HeadOfTerm(list);
    rc &&= Yap_unify(head, MkAtomTerm(Yap_LookupAtom(sqlite3_column_name(stmt, i))) );
    list = TailOfTerm(list);
    head = HeadOfTerm(list);
    list = TailOfTerm(list);

    int type = sqlite3_column_type(stmt, i);
    switch(type) {
      case SQLITE_INTEGER:
      tm = "integer";
      break;
    case SQLITE_FLOAT:
      tm = "real";
      break;
    case SQLITE_TEXT:
      tm = "string";
      break;
    case SQLITE_BLOB:
      tm = "blob";
      break;
    case SQLITE_NULL:
      tm = "null";
      break;
    }
    if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(rc))) )
      rc = FALSE;
  }

  SQL_LITE (sqlite3_finalize( stmt ) );
  
  return rc;

}

/* db_disconnect */
static Int
c_db_lite_disconnect( USES_REGS1 ) {
  Term arg_db = Deref(ARG1);

  sqlite3 *db = (sqlite3 *) IntegerOfTerm(arg_db);

  if ((myddas_util_search_connection(db)) != NULL)
    {
      myddas_util_delete_connection(db);
      sqlite3_close(db);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

/* db_table_write: Result Set */
static Int
c_db_lite_table_write( USES_REGS1 ) {
  Term arg_res_set = Deref(ARG1);

  sqlite3_RES *res_set = (sqlite3_RES *) IntegerOfTerm(arg_res_set);

  myddas_util_table_write(res_set);
  sqlite3_free_result(res_set);

  return TRUE;
}

static Int
c_db_lite_row_cut( USES_REGS1 ) {
  sqlite3_RES *sqlite3_res=NULL;

  sqlite3_res = (sqlite3_RES *) IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,1));
  sqlite3_free_result(sqlite3_res);
  return TRUE;
}


static Int
c_db_lite_get_fields_properties( USES_REGS1 ) {
  Term nome_relacao = Deref(ARG1);
  Term arg_db = Deref(ARG2);
  Term fields_properties_list = Deref(ARG3);
  Term head, list;

  char *relacao = AtomName(AtomOfTerm(nome_relacao));
  char sql[256];
  Int num_fields,i;
  sqlite3_FIELD *fields;
  sqlite3 *db = (sqlite3 *) (IntegerOfTerm(arg_db));

  sqlite3_stmt *stmt;
  Int rc = TRUE;
  
  sprintf(sql,"SELECT * FROM TABLE `%s`",relation);

  /* executar a query SQL */
  SQL_LITE (sqlite3_prepare_v2(db, sql, -1, stmt, NULL) );

  Functor functor = Yap_MkFunctor(Yap_LookupAtom("property"),4);

  Term properties[4];

  list = fields_properties_list;

  num_fields = sqlite3_column_count( stmt );

  for (i=0;i<num_fields;i++)
    {
      int not_null, prim, auto_inc;
      
      head = HeadOfTerm(list);

      const char *col = sqlite3_column_name(stmt, i);
      properties[0] = MkAtomTerm(Yap_LookupAtom(col));

      SQL_LITE (sqlite3_table_column_metadata(db, NULL, col, NULL,  NULL,
					      &not_null, &prim, &auto_inc) );
      properties[1] = MkIntegerTerm(not_null); //Can't be NULL

      properties[2] = MkIntegerTerm(prim); //It''s a primary key
      
      properties[3] = MkIntegerTerm(auto_inc);
      
      list = TailOfTerm(list);
      if (!Yap_unify(head, Yap_MkApplTerm(functor,4,properties))){
      	return FALSE;
      }
    }

  sqlite3_free_result(res_set);

  return TRUE;
}

/* c_db_lite_get_next_result_set: connection * NextResSet */
static Int
c_db_lite_get_next_result_set( USES_REGS1 ) {
  Term arg_db = Deref(ARG1);
  Term arg_next_res_set = Deref(ARG2);

  sqlite3 *db = (sqlite3 *) (IntegerOfTerm(arg_db));
  sqlite3_RES *res_set=NULL;

  if (sqlite3_next_result(db) == 0){
    res_set = sqlite3_store_result(db);
    Yap_unify(arg_next_res_set, MkIntegerTerm((Int) res_set));
  }
  return TRUE;
}

static Int
c_db_lite_get_database( USES_REGS1 ) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);

  sqlite3 *con = (sqlite3 *) (IntegerOfTerm(arg_con));

  if (!Yap_unify(arg_database,MkAtomTerm(Yap_LookupAtom(con->db))))
    return FALSE;

  return TRUE;

}

static Int
c_db_lite_change_database( USES_REGS1 ) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);

  sqlite3 *con = (sqlite3 *) (IntegerOfTerm(arg_con));
  char *database = AtomName(AtomOfTerm(arg_database));

  if (sqlite3_select_db(con,database)!=0)
    return FALSE;

  return TRUE;
}

#endif /* MYDDAS_sqlite3 */
