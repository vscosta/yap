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
* File:		myddas_shared.c						 *
* Last rev:	22/03/05						 *
* mods:									 *
* comments:	Predicates for maintaining MYDDAS                        *
*									 *
*************************************************************************/

#if defined CUT_C && (defined MYDDAS_MYSQL || defined MYDDAS_ODBC)

#include "Yap.h"
#include "Yatom.h"
#include "cut_c.h"
#include "myddas_util.h"
#include <stdlib.h>

STATIC_PROTO(int c_db_get_new_table_name,(void));
STATIC_PROTO(int c_db_connection_type,(void));
STATIC_PROTO(int c_db_add_preds,(void));
STATIC_PROTO(int c_db_preds_conn_start ,(void));
STATIC_PROTO(int c_db_preds_conn_continue ,(void));
STATIC_PROTO(int c_db_check_if_exists_pred,(void));
#ifdef MYDDAS_STATS
STATIC_PROTO(int c_db_stats,(void));
#endif 
#ifdef DEBUG
STATIC_PROTO(int c_db_check,(void));
#endif

/* c_db_get_new_table_name: -TableName */
static int 
c_db_get_new_table_name (void){
  Term arg_con = Deref(ARG1);
  Term arg_name = Deref(ARG2);
  
  int *con = (int *) IntegerOfTerm(arg_con);
  char *tableName = myddas_util_get_table_name(con);
  
  Yap_unify(arg_name, MkAtomTerm(Yap_LookupAtom(tableName)));
  
  free(tableName);
  
  return TRUE;
}



/* Gives the type of a given connection, 
   in other words, type will be mysql or odbc 
   
   NOTE: In order to use this predicate, the connection*/
/* c_db_connection_type: +Connection * ?Type */
static int 
c_db_connection_type (void){
  Term arg_con = Deref(ARG1);
  Term arg_type = Deref(ARG2);
  
  int *con = (int *) IntegerOfTerm(arg_con);
  short int type = myddas_util_connection_type(con);
  
  if (type == 1) /* MYSQL Connection */
    Yap_unify(arg_type, MkAtomTerm(Yap_LookupAtom("mysql")));
  else if (type ==2) /* ODBC Connection */
    Yap_unify(arg_type, MkAtomTerm(Yap_LookupAtom("odbc")));
  else /* Not a valid connection*/
    return FALSE;
  
  return TRUE;
}

/* db_add_preds: PredName * Arity * Module * Connection*/
static int 
c_db_add_preds (void){
  Term arg_nome = Deref(ARG1);
  Term arg_aridade = Deref(ARG2);
  Term arg_module = Deref(ARG3);
  Term arg_conn = Deref(ARG4);
  
  char *nome = AtomName(AtomOfTerm(arg_nome));
  char *module = AtomName(AtomOfTerm(arg_module));
  int aridade = IntegerOfTerm(arg_aridade);
  int *conn = (int *) IntegerOfTerm(arg_conn);

  if (myddas_util_add_predicate(nome,aridade,module,conn) == NULL)
    {
#ifdef DEBUG
      printf ("ERRO : Nao consegui adicionar predicado\n");
#endif 
      return FALSE;
    }
  
  return TRUE;
}

/* db_add_preds: PredName * Arity */
static int 
c_db_check_if_exists_pred (void){
  Term arg_nome = Deref(ARG1);
  Term arg_aridade = Deref(ARG2);
  Term arg_module = Deref(ARG3);
    
  char *nome = AtomName(AtomOfTerm(arg_nome));
  char *module = AtomName(AtomOfTerm(arg_module));
  int aridade = IntegerOfTerm(arg_aridade);
  
  if (myddas_util_search_predicate(nome,aridade,module) == NULL)
    return FALSE;
  else
    return TRUE;
}


/* db_preds_conn : Connection(+) * Pred_name(-) * Pred_arity */
static int
c_db_preds_conn_start (void){
  Term arg_conn = Deref(ARG1);
  Term module = Deref(ARG2);
  Term nome = Deref(ARG3);
  Term aridade = Deref(ARG4);
  
  int *conn = (int *) IntegerOfTerm(arg_conn);
  MYDDAS_UTIL_CONNECTION node = 
    myddas_util_search_connection(conn);
  
  /* Caso a ligacao já tenha sido apagada*/
  if (node == NULL)
    {
      cut_fail();
      return FALSE;
    }
  
  void *pointer = myddas_util_get_list_pred(node);
  EXTRA_CBACK_ARG(4,1)=(CELL) MkIntegerTerm((int)pointer);
  
  if (IsVarTerm(nome) && IsVarTerm(aridade) && IsVarTerm(module))
    return (c_db_preds_conn_continue());
      
  cut_fail();
  return FALSE;
}

/* db_preds_conn : Connection(+) * Pred_name(-) * Pred_arity*/
static int 
c_db_preds_conn_continue (void){
  Term module = Deref(ARG2);
  Term nome = Deref(ARG3);
  Term aridade = Deref(ARG4);

  void *pointer;
  pointer = (void *) IntegerOfTerm(EXTRA_CBACK_ARG(4,1));
    
  if (pointer != NULL)
    {
      Yap_unify(module, MkAtomTerm(Yap_LookupAtom(myddas_util_get_pred_module(pointer))));
      Yap_unify(nome, MkAtomTerm(Yap_LookupAtom(myddas_util_get_pred_name(pointer))));
      Yap_unify(aridade, MkIntegerTerm((int)myddas_util_get_pred_arity(pointer)));
      
      EXTRA_CBACK_ARG(4,1)=(CELL) MkIntegerTerm((int)myddas_util_get_pred_next(pointer));
      return TRUE;
    }
  else
    {
      cut_fail();
      return FALSE;
    }
}



#ifdef DEBUG
static int 
c_db_check(void){
  check_int();
  return TRUE;
}
#endif /*DEBUG*/

#ifdef MYDDAS_STATS
//Returns the stats of this module in a list
static int 
c_db_stats(void) {
  Term arg_conn = Deref(ARG1);
  Term arg_list = Deref(ARG2);
  
  int *conn = (int *) (IntegerOfTerm(arg_conn));
    
  // TODO
  if (get_myddas_top() == 0 ){ /* We want all the statistics */
    return FALSE;
  }

  MYDDAS_UTIL_CONNECTION 
    node = myddas_util_search_connection(conn);
  Term head, list;
  list = arg_list;
  
  //[Index 1] -> Total Number of Rows by connection
  //Total number of Rows returned by the server
  //WARNING: only works with store_result
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long totalRows = myddas_util_get_conn_total_rows(node);
  Yap_unify(head, MkIntegerTerm(totalRows));
#ifdef DEBUG
  printf ("Total Number of Rows returned from the Server: %lu\n",totalRows);
#endif

  //[Index 2] -> Total of Time Spent by the DB Server
  // processing all the  SQL Querys
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long totalTimeDBServer = myddas_util_get_conn_total_time_DBServer(node);
  Yap_unify(head, MkIntegerTerm(totalTimeDBServer));
#ifdef DEBUG
  printf ("Time Spent by the Server, on all the SQL Querys: %lu\n",totalTimeDBServer);
#endif  

  //[Index 3] -> Total of Time Spent by the DB Server
  // processing a the last SQL Query
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long lastTimeDBServer = myddas_util_get_conn_last_time_DBServer(node);
  Yap_unify(head, MkIntegerTerm(lastTimeDBServer));
#ifdef DEBUG
  printf ("Time Spent by the Server, on the last SQL Query: %lu\n",lastTimeDBServer);
#endif
  
  //[Index 4] -> Total of Time Spent by the DB Server
  // transfering all the results of the  SQL Querys
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long totalFromDBServer = myddas_util_get_conn_total_transfering_from_DBServer(node);
  Yap_unify(head, MkIntegerTerm(totalFromDBServer));
#ifdef DEBUG
  printf ("Time Spent by the Server, transfering all the results SQL Query: %lu\n",totalFromDBServer);
#endif
  
  //[Index 5] -> Total of Time Spent by the DB Server
  // transfering the result of the last SQL Query
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long lastFromDBServer = myddas_util_get_conn_last_transfering_from_DBServer(node);
  Yap_unify(head, MkIntegerTerm(lastFromDBServer));
#ifdef DEBUG
  printf ("Time Spent by the Server, transfering the result of the last SQL Query: %lu\n",lastFromDBServer);
#endif
  
  //[Index 6] -> Total of Time Spent by the 
  // db_row_function
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long db_row = myddas_util_get_total_db_row_function();
  Yap_unify(head, MkIntegerTerm(db_row));
#ifdef DEBUG
  printf ("Time Spent by the db_row_function: %lu\n",db_row);
#endif
  
  //[Index 7] -> Total of Bytes Transfered by the 
  // DB Server on all SQL Querys
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long totalBytes = myddas_util_get_conn_total_bytes_transfering_from_DBserver(node);
  Yap_unify(head, MkIntegerTerm(totalBytes));
#ifdef DEBUG
  printf ("Bytes Transfered by the DB Server from all querys: %lu\n",totalBytes);
#endif
  
  //[Index 8] -> Total of Bytes Transfered by the 
  // DB Server on the last SQL Query
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long lastBytes = myddas_util_get_conn_last_bytes_transfering_from_DBserver(node);
  Yap_unify(head, MkIntegerTerm(lastBytes));
#ifdef DEBUG
  printf ("Bytes Transfered by the DB Server on the last query: %lu\n",lastBytes);
#endif
  
  //[Index 9] -> Number of querys made to the DBserver
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  unsigned long number_querys = myddas_util_get_conn_number_querys_made(node);
  Yap_unify(head, MkIntegerTerm(number_querys));
#ifdef DEBUG
  printf ("Number of Querys made to the server: %lu\n",number_querys);
#endif
  
  return TRUE;
}

#endif /* MYDDAS_STATS */


/* Function to delete all the temporary tables */
/* from the mysql server */
void Yap_MyDDAS_delete_all_myddas_structs(void)
{
  //char *table_name;
  //char query[500];
  
  /* NAO ESQUECER DE FAZER ISTO TB PARA O DB_CLOSE*/

/*   for(;(table_name = myddas_util_delete_all_temp_table()) != NULL ;) { */
/*     printf ("%s\n",table_name); */
/*     sprintf (query,"DROP TABLE IF EXISTS %s",table_name); */
/*     printf ("%s\n",query); */
/*     free(table_name); */
/*     query[0]=0; */
/*   } */   
}

void Yap_InitMYDDAS_SharedPreds(void)
{
  
  Yap_InitCPred("c_db_get_new_table_name",2,c_db_get_new_table_name, 0);

  
  Yap_InitCPred("c_db_connection_type",2,c_db_connection_type, 0);

  /* CORRECT THIS: db_add_preds : PredName * Arity * Connection */
  Yap_InitCPred("c_db_add_preds",4,c_db_add_preds, 0);

  /* db_check_if_exists_pred : PredName * Arity * Connection */
  Yap_InitCPred("c_db_check_if_exists_pred",3,c_db_check_if_exists_pred, 0);
  
#ifdef MYDDAS_STATS
  /* db_stats: Connection * Stats*/
  Yap_InitCPred("c_db_stats",2, c_db_stats, 0);
#endif

#ifdef DEBUG
  Yap_InitCPred("c_db_check",0, c_db_check, 0);
#endif
}

void Yap_InitBackMYDDAS_SharedPreds(void)
{
  Yap_InitCPredBack("c_db_preds_conn", 4, sizeof(int),
		    c_db_preds_conn_start, 
		    c_db_preds_conn_continue,  0); 

}




#endif /*CUT_C && (MYDDAS_MYSQL || MYDDAS_ODBC)*/
