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
* File:		myddas_postgres.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for comunicating with a pq database system *
*									 *
*************************************************************************/

#ifdef MYDDAS_POSTGRES

#include <libpq-fe.h>

#include "Yap.h"
#include "Yatom.h"
#include "YapText.h"
#include "cut_c.h"
#include "eval.h"
#include "myddas.h"
#ifdef MYDDAS_STATS
#include "myddas_structs.h"
#include "myddas_statistics.h"
#endif
#include "myddas_wkb2prolog.h"

#define CALL_POSTGRES(f)                                          \
{                                                           \
  res = PQ ## f;                                      \
  if (PQstatus(conn) != CONNECTION_OK) {}                                   \
  fprintf(stderr, "Error %s when calling %s, line %d\n",  \
  PQerrorMessage(conn), #f,__LINE__);               \
    exit (1);                                           \
  }                                                       \
  PQclear(res); \
}                                                           \

#define GET_POSTGRES(f,x)                                 \
{                                                           \
  int i;                                                  \
  i = sqlite3_ ## f;                                      \
  if (i != SQLITE_ ## x) {                                \
    fprintf (stderr, "%s failed with status %d: %s\n",  \
    #f, i, sqlite3_errmsg (db));               \
    exit (1);                                           \
  }                                                       \
}					\



static Int NULL_id = 0;

typedef struct result_set {
  PGconn *db;
  const PGresult *res;
  int i     ;
  int nrows;
  int ncols;
} resultSet;

void Yap_InitMYDDAS_PGPreds();
void Yap_InitBackMYDDAS_PGPreds();


static Int c_PG_connect( USES_REGS1 );
static Int c_PG_disconnect( USES_REGS1 );
static Int c_PG_number_of_fields( USES_REGS1 );
static Int c_PG_get_attributes_types( USES_REGS1 );
static Int c_PG_query( USES_REGS1 );
static Int c_PG_table_write( USES_REGS1 );
static Int c_PG_row( USES_REGS1 );
static Int c_PG_row_cut( USES_REGS1 );
static Int c_PG_get_fields_properties( USES_REGS1 );
static Int c_PG_get_next_result_set( USES_REGS1 );
static Int c_PG_get_database( USES_REGS1 );
static Int c_PG_change_database( USES_REGS1 );

void Yap_InitMYDDAS_PGPreds(void)
{
  /* db_dbect: Host x User x Passwd x Database x dbection x ERROR_CODE */
  Yap_InitCPred("c_PG_connect", 7, c_PG_connect,  0);

  /* db_number_of_fields: Relation x connection x NumberOfFields */
  Yap_InitCPred("c_PG_number_of_fields",3, c_PG_number_of_fields, 0);

  /* db_get_attributes_types: Relation x TypesList */
  Yap_InitCPred("c_PG_get_attributes_types", 3, c_PG_get_attributes_types,  0);

  /* db_query: SQLQuery x ResultSet x conection */
  Yap_InitCPred("c_PG_query", 5, c_PG_query, 0);

  /* db_disconnect: connection */
  Yap_InitCPred("c_PG_disconnect", 1,c_PG_disconnect, 0);

  /* db_table_write: Result Set */
  Yap_InitCPred("c_PG_table_write", 1, c_PG_table_write,  0);

  /* db_get_fields_properties: PredName x connection x PropertiesList*/
  Yap_InitCPred("c_PG_get_fields_properties",3,c_PG_get_fields_properties,0);

  Yap_InitCPred("c_PG_get_next_result_set",2,c_PG_get_next_result_set,0);

  /* c_PG_get_database: connection x DataBaseName */
  Yap_InitCPred("c_PG_get_database",2,c_PG_get_database,0);

  /* c_PG_change_database: connection x DataBaseName */
  Yap_InitCPred("c_PG_change_database",2,c_PG_change_database,0);


}

void Yap_InitBackMYDDAS_PGPreds(void)
{
	/* db_row: ResultSet x Arity x ListOfArgs */
  Yap_InitCPredBackCut("c_PG_row", 3, sizeof(Int),
		    c_PG_row,
		    c_PG_row,
		    c_PG_row_cut, 0);

}

static Int
c_PG_connect( USES_REGS1 ) {
  int i=0;
  Term arg_host = Deref(ARG1);
  Term arg_user = Deref(ARG2);
  Term arg_passwd = Deref(ARG3);
  Term arg_database = Deref(ARG4);
  Term arg_port = Deref(ARG5);
  Term arg_socket = Deref(ARG6);
  Term arg_conn = ARG7;
  Term tnone = MkAtomTerm(Yap_LookupAtom(""));
  Term tzero = MkIntTerm(0);

  char *keywords[8], *values[8], ports[16];


  if (IsApplTerm(arg_host)) {
    keywords[i] = "hostaddr";
    values[i++] = RepAtom(AtomOfTerm(ArgOfTerm(1, arg_host)))->StrOfAE;
  } else {
    keywords[i] = "host";
    values[i++] = RepAtom(AtomOfTerm(arg_host))->StrOfAE;
  }
  if (IsNonVarTerm(arg_user) && arg_user != tnone && IsAtomTerm(arg_user)) {
    keywords[i] = "user";
    values[i++] = RepAtom(AtomOfTerm(arg_user))->StrOfAE;
  }
  if (IsNonVarTerm(arg_user) && arg_passwd != tnone && IsAtomTerm(arg_passwd)) {
    keywords[i] = "password";
    values[i++] = RepAtom(AtomOfTerm(arg_passwd))->StrOfAE;
  }
  if (IsNonVarTerm(arg_user) && arg_database != tnone && IsAtomTerm(arg_database)) {
    keywords[i] = "dbase";
    values[i++] = RepAtom(AtomOfTerm(arg_database))->StrOfAE;
  }
  if (IsNonVarTerm(arg_user) && arg_port != tzero && IsIntTerm(arg_port)) {
    keywords[i] = "port";
    values[i++] = snprintf(ports, sizeof(ports)-1, "%d", IntOfTerm(arg_port));
  } else if (IsNonVarTerm(arg_socket) && arg_socket != tempty && IsIntTerm(arg_socket)) {
    keywords[i] = "port";
    values[i++] = RepAtom(AtomOfTerm(arg_database))->StrOfAE;
  }
  keywords[i] = NULL;
  values[i++] = NULL;

  /* Make a connection to the database */
  conn = PQconnectdbParams(keywords, values);

  /* Check to see that the backend c  onnection was successfully made */
  if (PQstatus(conn) != CONNECTION_OK)
  {
    fprintf(stderr, "Connection to database failed: %s",
            PQerrorMessage(conn));
    return FALSE;
  }

  if (!Yap_unify(arg_conn, MkAddressTerm(conn)))
    return FALSE;
  else
    {
      /* Criar um novo no na lista de ligacoes*/
      new = myddas_util_add_connection(conn,NULL,MYDDAS_API);

      if (new == NULL){
#ifdef DEBUG
  printf("ERROR: ** c_db_my_connect ** Error allocating memory\n");
#endif
  return FALSE;
      }
      return TRUE;
    }

}



static MYDDAS_STATS_TIME
myddas_stat_init_query( PG *db )
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
  /* Measure time spent by the PG Server
     processing the SQL Query */
  return myddas_stats_walltime();
#else
  return NULL;
#endif
}


static MYDDAS_STATS_TIME
myddas_stat_end_query( MYDDAS_STATS_TIME start )
{
  MYDDAS_STATS_TIME diff = NULL;
#ifdef MYDDAS_STATS
  /* Measure time spent by the PG Server
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

#ifdef MYDDAS_STATS
/* measure transfer time */
static void
myddas_stat_transfer_query( MYDDAS_STATS_TIME diff )
{
  /* Measure time spent by the PG Server
     transferring the result of the last query
     back to the client */
  start = myddas_stats_walltime();
  /* Measure time spent by the PG Server
     transferring the result of the last query
     back to the client */
  end = myddas_stats_walltime();

  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
  myddas_stats_subtract_time(diff,end,start);
  diff = MYDDAS_STATS_TIME_copy_to_final(diff);

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
      /* With an INSERT statement, PG_(use or store)_result()
         returns a NULL pointer*/

      /* This is only works if we use PG_store_result */
      MyddasUInt numberRows = PG_num_rows(res_set);
      MyddasUInt rows;
      myddas_stat_transfer_query( diff );

      MYDDAS_STATS_CON_GET_TOTAL_ROWS(node,rows);
      numberRows = numberRows + rows;
      MYDDAS_STATS_CON_SET_TOTAL_ROWS(node,numberRows);
      MYDDAS_STATS_CON_GET_TOTAL_ROWS_COUNT(node,count);
      MYDDAS_STATS_CON_SET_TOTAL_ROWS_COUNT(node,++count);

      /* Calculate the ammount of data sent by the server */
      MyddasUInt total,number_fields = PG_num_fields(res_set);
      PG_ROW row;
      MyddasULInt i;
      total=0;
      while ((row = PG_fetch_row(res_set)) != NULL){
        PG_field_seek(res_set,0);

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
}
#endif

/* db_query: SQLQuery x ResultSet x connection */
static Int
c_PQ_query( USES_REGS1 ) {
  Term arg_sql_query = Deref(ARG1);
  Term arg_result_set = Deref(ARG2);
  Term arg_db = Deref(ARG3);
  Term arg_mode = Deref(ARG4);
  Term arg_arity = ARG5;

  char *sql = AtomName(AtomOfTerm(arg_sql_query));
  char *mode = AtomName(AtomOfTerm(arg_mode));
  PGresult   *res;
  PGconn *db = AddressOfTerm(arg_db);
  const char *stmt;

  MYDDAS_STATS_TIME start, end;
  int length=strlen(sql);
  struct result_set *rs = malloc(sizeof( struct result_set));
  if (!rs)
    return FALSE;
  rs->db = db;
  rs->i = 0;

  start = myddas_stat_init_query( db );

  char *stmt = new_stmt();

   /* Send query to server and process it */
  if (strcmp(mode,"store_result")!=0) {
    // Leave data for extraction
    CALL_POSTGRES (prepare(db, stmt, sql, 0, NULL) );
    res = GET_POSTGRES (PQdescribePrepared(db, stmt) );
    rs->stmt = stmt;
    rs->res_set = NULL;
    rs->nrows = -1;
    rs->length = PQnfields(res);
    if (!Yap_unify(arg_arity, MkIntegerTerm(rs->length)))
      {
	free(rs);
	return FALSE;
      }
   if (!Yap_unify(arg_result_set, MkAddressTerm( rs)))
      {
        free(rs);
        return FALSE;
      }
    return TRUE;
  } else{
    // construct an intermediate table, res_set
    char **res_set;
    char *msg;
    int nrows;

    res = GET_POSTGRES (PGexec(db, sql) );

    end = myddas_stat_end_query( start );
    length = PGnfields(res);
    nrows = PGntuples(res);
      //INSERT statements don't return any res_set
    if (nrows == 0) {
      return TRUE;
    }
    if (!Yap_unify(arg_arity, MkIntegerTerm(nrows))){
      /*
       * Should PGclear PGresult whenever it is no longer needed to avoid memory
       * leaks
       */
      free(rs);
      PGclear(res);
      return FALSE;
    }
    rs->stmt = NULL;
    rs->res_set = res_set;
    rs->nrows = nrows;
    rs->length = length;
    if (!Yap_unify(arg_result_set, MkAddressTerm( rs)))
      {
        free(rs);
        PGclear(res);
        return FALSE;
      }
  }
  return TRUE;
}


static Int
c_PG_number_of_fields( USES_REGS1 ) {
  Term arg_relation = Deref(ARG1);
  Term arg_db = Deref(ARG2);
  Term arg_fields = ARG3;

  char *relation = AtomName(AtomOfTerm(arg_relation));
  PG *db = AddressOfTerm(arg_db);
  PG_stmt *stmt;

  char sql[256];

  snprintf(sql, 255, "SELECT * FROM TABLE `%s`",relation);
  // Leave data for extraction
  CALL_POSTGRES (prepare(db, stmt, sql, 0, NULL) );

    /* executar a query SQL */

    int nrows = GET_POSTGRES (PGntuples(stmt) );

    PGClear( stmt );

  return Yap_unify(arg_fields, MkIntegerTerm( nrows ));
}


/* db_get_attributes_types: RelName x connection -> TypesList */
static Int
c_PG_get_attributes_types( USES_REGS1 ) {
  Term arg_relation = Deref(ARG1);
  Term arg_db = Deref(ARG2);
  Term arg_types_list = Deref(ARG3);
  Term list, head;

  char *relation = AtomName(AtomOfTerm(arg_relation));
  PG *db = AddressOfTerm(arg_db);
  char sql[256];
  int row;

  PG_stmt *stmt;
  Int rc = TRUE;

  sprintf(sql,"SELECT * FROM TABLE `%s`",relation);
  // Leave data for extraction
  res = GET_POSTGRES (PGprepare(db, stmt, sql, 0, NULL) );

    /* executar a query SQL */


  int cols = PGnfields( res );

  list = arg_types_list;

  for (col = 0; col < cols; col++)
  {
    const char *tm;


    head = HeadOfTerm(list);
    rc = (
	  rc && Yap_unify(head, MkAtomTerm(Yap_LookupAtom(PGfname(res, col))) ) );
    list = TailOfTerm(list);
    head = HeadOfTerm(list);
    list = TailOfTerm(list);

    Oid type = PGftype(res, col);
    switch(type) {
      case BYTEOID:
      case CHAROID:
      case INT2OID:
      case INT4OID:
      case INT8OID:
	     tm = "integer";
      break;
      case FLOAT4OID:
      case FLOAT8OID:
      tm = "real";
      break;
    case NAMEOID:
      tm = "atom";
      break;
    case TEXTOID:
      tm = "string";
      break;
    default:
      tm = "unknown type";
      break;
    }
    if (!Yap_unify(head, MkAtomTerm(Yap_LookupAtom(tm))) )
      rc = FALSE;
  }

  PGClear( stmt );

  return rc;

}

/* db_disconnect */
static Int
c_PG_disconnect( USES_REGS1 ) {
  Term arg_db = Deref(ARG1);

  PG *db = AddressOfTerm(arg_db);

  if ((myddas_util_search_connection(db)) != NULL)
    {
      myddas_util_delete_connection(db);
      PGfinish(db);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

/* db_table_write: Result Set */
static Int
c_PG_table_write( USES_REGS1 ) {
  /*
    Term arg_res_set = Deref(ARG1);
    PG_RES *res_set = (PG_RES *) IntegerOfTerm(arg_res_set);

    mydas_util_table_write(res_set);
    PG_free_result(res_set);
  */
  return TRUE;
}

static Int
c_PG_get_fields_properties( USES_REGS1 ) {
  Term nome_relacao = Deref(ARG1);
  Term arg_db = Deref(ARG2);
  Term fields_properties_list = Deref(ARG3);
  Term head, list;

  char *relation = AtomName(AtomOfTerm(nome_relacao));
  char sql[256];
  Int num_fields,i;
  PGconn *db = (PG *) (IntegerOfTerm(arg_db));

  sprintf(sql,"\\d+ `%s`",relation);

  res = GET_POSTGRES (PGexec(db, sql) );

  end = myddas_stat_end_query( start );
  Functor functor = Yap_MkFunctor(Yap_LookupAtom("property"),4);

  Term properties[4];

  list = fields_properties_list;

  num_fields = PGntuples(res);

  if (( coln = PGfnumber(res,"Column")) < 0)
    return FALSE;
  if (( propname = PGfname(res,2)) == NULL)
      return FALSE;
  if (!strstr(propname, "Table") || !strstr(propname, "Modifiers") )
              return FALSE;

  for (i=0;i<num_fields;i++)
    {
      bool not_null, prim, auto_inc;

      head = HeadOfTerm(list);

      if (!PGgetisnull(res, i, coln)) {
        const char *col = PGgetvalue(res, i, coln);
        properties[0] = MkAtomTerm(Yap_LookupAtom(col));
      }
      if (!PGgetisnull(res, i, 2)) {
        const char *props = PGgetvalue(res, i, 2);
        properties[1] = MkIntegerTerm(strstr(props, "not null") != NULL); //Can't be NULL
        properties[2] = MkIntegerTerm(strstr(props, "prim") != NULL); //Can't be NULL
        properties[3] = MkIntegerTerm(strstr(props, "nextval") != NULL); //Can't be NULL
      }
      list = TailOfTerm(list);
      if (!Yap_unify(head, Yap_MkApplTerm(functor,4,properties))){
      	return FALSE;
      }
    }

  PGclear(res);

  return TRUE;
}

/* c_PG_get_next_result_set: connection * NextResSet */
static Int
c_PG_get_next_result_set( USES_REGS1 ) {
  Term arg_db = Deref(ARG1);
  Term arg_next_res_set = Deref(ARG2);

  PGconn *db = AddressOfTerm(arg_db);

  return FALSE;
}

static Int
c_PG_get_database( USES_REGS1 ) {
  Term arg_con = Deref(ARG1);
  Term arg_database = Deref(ARG2);

  if (!Yap_unify(arg_database,arg_con))
    return FALSE;

  return TRUE;

}

static Int
c_PG_change_database( USES_REGS1 ) {
  /* no-op for now */
  return TRUE;
}

static Int
c_PG_row_cut( USES_REGS1 ) {
  struct result_set *res_set=NULL;
  PG *db = res_set->db;

  res_set = AddressOfTerm(EXTRA_CBACK_CUT_ARG(Term,1));
  PQclear( res_set->res );
  PQfinish( db );
  free(res_set);
  return TRUE;
}

#define cvt( s ) cvt__( s PASS_REGS )

static Term
cvt__(const char *s USES_REGS) {
  return Yap_CharsToTDQ( s, CurrentModule PASS_REGS);
}

/* db_row: ResultSet x Arity_ListOfArgs x ListOfArgs -> */
static Int
c_PG_row( USES_REGS1 ) {
#ifdef MYDDAS_STATS
  /*   Measure time used by the  */
  /*      c_PG_row function */
  //MYDDAS_STATS_TIME start,end,total_time,diff;
  MyddasULInt count = 0;
  start = myddas_stats_walltime();
#endif
  Term arg_result_set = Deref(ARG1);
  Term arg_arity = Deref(ARG2);
  Term arg_list_args = Deref(ARG3);
  Int rc = TRUE;

  if (IsVarTerm( arg_result_set ) ) {
    if (!c_PG_query( PASS_REGS1 ) ) {
      cut_fail();
    }
    arg_result_set = Deref(ARG1);
    EXTRA_CBACK_ARG(3,1)= arg_result_set ;
    EXTRA_CBACK_ARG(3,2)= MkIntegerTerm(0) ;
  }
  struct result_set *res_set = AddressOfTerm(arg_result_set);

  Term head, list, NULL_atom[1];
  Int arity;
  list = arg_list_args;
  arity = IntegerOfTerm(arg_arity);
  PG *db = res_set->db;
  if (res_set->stmt == NULL ) {
    CACHE_REGS
    Int i= IntegerOfTerm(EXTRA_CBACK_CUT_ARG(Term,2)), j;
    Int rc = true;
    // data needs to be copied to Prolog
    // row by row
#ifdef MYDDAS_STATS
    MYDDAS_STATS_TIME diff;

    MYDDAS_STATS_INITIALIZE_TIME_STRUCT(diff,time_copy);
#endif
	for (j = 0; j < arity; j++)
	  {
	    /* Ts -> List */
	    const char *field = res_set->res_set[indx++];
	    head = HeadOfTerm(list);
	    list = TailOfTerm(list);
	    rc = (rc && Yap_unify(head, cvt( field  )) );
	  }
	if (rc) {
    EXTRA_CBACK_ARG(3,2)= MkIntegerTerm(i+1);
  	  return rc;
  }
#ifdef MYDDAS_STATS
     myddas_stat_transfer_query( diff );
#endif
    cut_fail();
  }
  // busy-waiting
  if (!PQsetnonblocking(res_set->db, 1)) {
    fprintf(stderr, "Connection to database failed: %s",
            PQerrorMessage(conn));
    return FALSE;

  }
  if (!PQsendQueryPrepared(res_set->db, res_set->stmt, 0, NULL, NULL, NULL, 0)) {
    fprintf(stderr, "Connection to database failed: %s",
            PQerrorMessage(conn));
    return FALSE;

  }
  int res;
  while((res = PQgetResult(res_set->db)) != NULL);
  if (i == res_set->nrows) {
    // no more data
    PQfinish( res_set->db ) );
    free(res_set);
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

  } else if (i < res_ser->nrows) {
    list = arg_list_args;
    Term tf;

    for (j = 0; j < arity; j++)
      {
        /* convert data types here */
        head = HeadOfTerm(list);
        list = TailOfTerm(list);

        if (!PGgetisnull(res, i, j)) {
          const char *col = PGgetvalue(res, i, j);
          properties[0] = MkAtomTerm(Yap_LookupAtom(col));
        } else {
  	  NULL_atom[0] = MkIntegerTerm(NULL_id++);
	  tf = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("NULL"),1),1,NULL_atom);
        }
      }
    if (!Yap_unify(head, tf))
      rc = FALSE;
#ifdef MYDDAS_STATS
    end = myddas_stats_walltime();

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
  } else
    {
      Yap_Error(SYSTEM_ERROR, TermNil, "PG: %s", PG_errmsg( db ) );
    }
	return rc;
}


#endif /* MYDDAS_PG */
