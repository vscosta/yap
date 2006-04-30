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
#include "myddas_structs.h"
#ifdef MYDDAS_STATS
#include "myddas_statistics.h"
#endif

//STATIC_PROTO(Int c_db_get_new_table_name,(void));
STATIC_PROTO(Int c_db_connection_type,(void));
STATIC_PROTO(Int c_db_add_preds,(void));
STATIC_PROTO(Int c_db_preds_conn_start ,(void));
STATIC_PROTO(Int c_db_preds_conn_continue ,(void));
STATIC_PROTO(Int c_db_connection_start ,(void));
STATIC_PROTO(Int c_db_connection_continue ,(void));
STATIC_PROTO(Int c_db_check_if_exists_pred,(void));
STATIC_PROTO(Int c_db_delete_predicate,(void));
STATIC_PROTO(Int c_db_multi_queries_number,(void));
#ifdef MYDDAS_STATS
STATIC_PROTO(Int c_db_stats,(void));
#endif 
#ifdef DEBUG
STATIC_PROTO(Int c_db_check,(void));
#endif

void Yap_InitMYDDAS_SharedPreds(void)
{
  /* c_db_connection_type: Connection x Type */
  Yap_InitCPred("c_db_connection_type",2,c_db_connection_type, 0);

  /* CORRECT THIS: db_add_preds : PredName * Arity * Connection */
  Yap_InitCPred("c_db_add_preds",4,c_db_add_preds, 0);

  /* c_db_check_if_exists_pred : PredName * Arity * Connection */
  Yap_InitCPred("c_db_check_if_exists_pred",3,c_db_check_if_exists_pred, 0);
  
  /* c_db_delete_pred : Module * PredName * Arity */
  Yap_InitCPred("c_db_delete_predicate",3,c_db_delete_predicate, 0);

  /* c_db_delete_pred : Module * PredName * Arity */
  Yap_InitCPred("c_db_multi_queries_number",2,c_db_multi_queries_number, 0);

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
  /* Gives all the predicates associated to a given connection */
  Yap_InitCPredBack("c_db_preds_conn", 4, sizeof(Int),
		    c_db_preds_conn_start, 
		    c_db_preds_conn_continue,  0);
  /* Gives all the connections stored on the MYDDAS Structure*/
  Yap_InitCPredBack("c_db_connection", 1, sizeof(Int),
		    c_db_connection_start, 
		    c_db_connection_continue,  0); 
  

}

/* Gives the type of a given connection, 
   in other words, type will be mysql or odbc 
   
   NOTE: In order to use this predicate, the connection*/
/* c_db_connection_type: +Connection * ?Type */
static Int 
c_db_connection_type (void){
  Term arg_con = Deref(ARG1);
  Term arg_type = Deref(ARG2);
  
  Int *con = (Int *) IntegerOfTerm(arg_con);
  Int type = myddas_util_connection_type(con);
  
  if (type == 1) /* MYSQL Connection */
    Yap_unify(arg_type, MkAtomTerm(Yap_LookupAtom("mysql")));
  else if (type ==2) /* ODBC Connection */
    Yap_unify(arg_type, MkAtomTerm(Yap_LookupAtom("odbc")));
  else /* Not a valid connection*/
    return FALSE;
  
  return TRUE;
}

/* db_add_preds: PredName * Arity * Module * Connection*/
static Int 
c_db_add_preds (void){
  Term arg_nome = Deref(ARG1);
  Term arg_aridade = Deref(ARG2);
  Term arg_module = Deref(ARG3);
  Term arg_conn = Deref(ARG4);
  
/*   PredEntry *pe; */
/*   pe = RepPredProp(PredPropByFunc(FunctorOfTerm(arg_pred),arg_module)); */
  

  char *nome = AtomName(AtomOfTerm(arg_nome));
  char *module = AtomName(AtomOfTerm(arg_module));
  Int aridade = IntegerOfTerm(arg_aridade);
  Int *conn = (Int *) IntegerOfTerm(arg_conn);

  if (myddas_util_add_predicate(nome,aridade,module,conn) == NULL)
    {
#ifdef DEBUG
      printf ("ERROR : Could not add Predicate: Line: %d File: %s\n",__LINE__,__FILE__);
#endif
      return FALSE;
    }
  
  return TRUE;
}


static Int 
c_db_check_if_exists_pred (void){
  Term arg_nome = Deref(ARG1);
  Term arg_aridade = Deref(ARG2);
  Term arg_module = Deref(ARG3);
    
 
  char *nome = AtomName(AtomOfTerm(arg_nome));
  char *module = AtomName(AtomOfTerm(arg_module));
  Int aridade = IntegerOfTerm(arg_aridade);
  
  if (myddas_util_search_predicate(nome,aridade,module) == NULL)
    return FALSE;
  else
    return TRUE;
}


static Int 
c_db_delete_predicate(void){
  Term arg_module = Deref(ARG1);
  Term arg_name = Deref(ARG2);
  Term arg_arity = Deref(ARG3);
  
  char *module = AtomName(AtomOfTerm(arg_module));
  char *name = AtomName(AtomOfTerm(arg_name));
  Int arity = IntegerOfTerm(arg_arity);

  MYDDAS_UTIL_PREDICATE predicate = 
    myddas_util_search_predicate(name,arity,module);
  if (predicate == NULL)
    return FALSE;

  myddas_util_delete_predicate(predicate);

  return TRUE;
}


static Int 
c_db_multi_queries_number(void){
  Term arg_conn = Deref(ARG1);
  Term arg_number = Deref(ARG2);

  Int *conn = (Int *) IntegerOfTerm(arg_conn);
  MYDDAS_UTIL_CONNECTION node = 
    myddas_util_search_connection(conn);
  
  if (node == NULL)
    return FALSE;

  if (IsVarTerm(arg_number)){
    Yap_unify(arg_number,MkIntegerTerm(((Int)myddas_util_get_total_multi_queries_number(node))+1));
  } 
  else {
    Int number = IntegerOfTerm(arg_number);
    number--;
    myddas_util_set_total_multi_queries_number(node,number);
  }
  
  return TRUE;
  
}

static Int
c_db_connection_start(void){

  MYDDAS_UTIL_CONNECTION node =
    Yap_REGS.MYDDAS_GLOBAL_POINTER->myddas_top_connections;

  EXTRA_CBACK_ARG(1,1)=(CELL) MkIntegerTerm((Int)node);
  
  return (c_db_connection_continue());
}

static Int
c_db_connection_continue(void){
  Term arg_conn = Deref(ARG1);
  
  MYDDAS_UTIL_CONNECTION node;
  node = (MYDDAS_UTIL_CONNECTION) IntegerOfTerm(EXTRA_CBACK_ARG(1,1));
  
  /* There is no connections */
  if (node == NULL)
    {
      cut_fail();
      return FALSE;
    }

  Yap_unify(arg_conn, MkIntegerTerm((Int)(node->connection)));
  EXTRA_CBACK_ARG(1,1)=(CELL) MkIntegerTerm((Int)(node->next));
  
  return TRUE;
  
}

/* db_preds_conn : Connection(+) * Pred_name(-) * Pred_arity */
static Int
c_db_preds_conn_start (void){
  Term arg_conn = Deref(ARG1);
   
  Int *conn = (Int *) IntegerOfTerm(arg_conn);
  MYDDAS_UTIL_CONNECTION node = 
    myddas_util_search_connection(conn);
  
  /* Caso a ligacao já tenha sido apagada*/
  if (node == NULL)
    {
      cut_fail();
      return FALSE;
    }
  
  void *pointer = myddas_util_get_list_pred(node);
  EXTRA_CBACK_ARG(4,1)=(CELL) MkIntegerTerm((Int)pointer);
  
  return (c_db_preds_conn_continue());
}

/* db_preds_conn : Connection(+) * Pred_name(-) * Pred_arity*/
static Int 
c_db_preds_conn_continue (void){
  Term module = Deref(ARG2);
  Term name = Deref(ARG3);
  Term arity = Deref(ARG4);

  void *pointer;
  pointer = (void *) IntegerOfTerm(EXTRA_CBACK_ARG(4,1));
    
  if (pointer != NULL)
    {
      EXTRA_CBACK_ARG(4,1)=(CELL) MkIntegerTerm((Int)myddas_util_get_pred_next(pointer));
      
      if (!Yap_unify(module, MkAtomTerm(Yap_LookupAtom(myddas_util_get_pred_module(pointer))))){
	return FALSE;
      }
      if (!Yap_unify(name,MkAtomTerm(Yap_LookupAtom(myddas_util_get_pred_name(pointer))))){
	return FALSE;
      }
      if (!Yap_unify(arity, MkIntegerTerm((Int)myddas_util_get_pred_arity(pointer)))){
	return FALSE;
      }
      return TRUE;
    }
  else
    {
      cut_fail();
      return FALSE;
    }
}



#ifdef DEBUG
static Int 
c_db_check(void){
  check_int();
  return TRUE;
}
#endif /*DEBUG*/

#ifdef MYDDAS_STATS
//Returns the stats of this module in a list
static Int 
c_db_stats(void) {
  Term arg_conn = Deref(ARG1);
  Term arg_list = Deref(ARG2);
  
  Int *conn = (Int *) (IntegerOfTerm(arg_conn));
    
  // TODO
  if (get_myddas_top() == 0 ){ /* We want all the statistics */
    return FALSE;
  }

  MYDDAS_UTIL_CONNECTION 
    node = myddas_util_search_connection(conn);
  Term head, list;
  list = arg_list;

  MYDDAS_STATS_TIME time;
  UInt number;

  
  //[Index 1] -> Total Number of Rows by connection
  //Total number of Rows returned by the server
  //WARNING: only works with store_result
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_TOTAL_ROWS(node,number);
  Yap_unify(head, MkIntegerTerm(number));
#ifdef DEBUG
  printf ("Total Number of Rows returned from the Server\n");
  printf ("%lu\n\n",(unsigned long)number);
#endif

  //[Index 2] -> Total of Time Spent by the DB Server
  // processing all the  SQL Querys
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_TOTAL_TIME_DBSERVER(node,time);
  Yap_unify(head, MkIntegerTerm((Int)time));
#ifdef DEBUG
  printf ("Reference to time Spent by the Server, on all the SQL Querys\n");
  MYDDAS_STATS_PRINT_TIME_STRUCT(time);
  printf ("\n\n");
#endif  

  //[Index 3] -> Total of Time Spent by the DB Server
  // processing a the last SQL Query
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_LAST_TIME_DBSERVER(node,time);
  Yap_unify(head, MkIntegerTerm((Int)time));
#ifdef DEBUG
  printf ("Reference to time Spent by the Server, on the last SQL Query\n");
  MYDDAS_STATS_PRINT_TIME_STRUCT(time);
  printf ("\n\n");
#endif
  
  //[Index 4] -> Total of Time Spent by the DB Server
  // transfering all the results of the  SQL Querys
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_TOTAL_TIME_TRANSFERING(node,time);
  Yap_unify(head, MkIntegerTerm((Int)time));
#ifdef DEBUG
  printf ("Refence to time Spent by the Server, transfering all the results SQL Query\n");
  MYDDAS_STATS_PRINT_TIME_STRUCT(time);
  printf ("\n\n");
#endif
  
  //[Index 5] -> Total of Time Spent by the DB Server
  // transfering the result of the last SQL Query
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_LAST_TIME_TRANSFERING(node,time);
  Yap_unify(head, MkIntegerTerm((Int)time));
#ifdef DEBUG
  printf ("Reference to time Spent by the Server, transfering the result of the last SQL Query\n");
  MYDDAS_STATS_PRINT_TIME_STRUCT(time);
  printf ("\n\n");
#endif
  
  //[Index 6] -> Total of Time Spent by the 
  // db_row_function
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_GET_DB_ROW_FUNCTION(time);
  Yap_unify(head, MkIntegerTerm((Int)time));
#ifdef DEBUG
  printf ("Reference to time Spent by the db_row_function\n");
  MYDDAS_STATS_PRINT_TIME_STRUCT(time);
  printf ("\n\n");
#endif
  
  //[Index 7] -> Total of Bytes Transfered by the 
  // DB Server on all SQL Querys
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_TOTAL_BYTES_TRANSFERING_FROM_DBSERVER(node,number);
  Yap_unify(head, MkIntegerTerm(number));
#ifdef DEBUG
  printf ("Bytes Transfered by the DB Server from all querys\n");
  printf ("%lu\n\n",(unsigned long)number);
#endif
  
  //[Index 8] -> Total of Bytes Transfered by the 
  // DB Server on the last SQL Query
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_LAST_BYTES_TRANSFERING_FROM_DBSERVER(node,number);
  Yap_unify(head, MkIntegerTerm(number));
#ifdef DEBUG
  printf ("Bytes Transfered by the DB Server on the last query\n");
  printf ("%lu\n\n",(unsigned long)number);
#endif
  
  //[Index 9] -> Number of querys made to the DBserver
  head = HeadOfTerm(list);
  list = TailOfTerm(list);
  MYDDAS_STATS_CON_GET_NUMBER_QUERIES_MADE(node,number);
  Yap_unify(head, MkIntegerTerm(number));
#ifdef DEBUG
  printf ("Number of Querys made to the server\n");
  printf ("%lu\n\n",(unsigned long)number);
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





#endif /*CUT_C && (MYDDAS_MYSQL || MYDDAS_ODBC)*/
