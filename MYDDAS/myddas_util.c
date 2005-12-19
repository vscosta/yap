#ifdef CUT_C
#if defined MYDDAS_ODBC || defined MYDDAS_MYSQL 

#include <string.h>
#include <stdlib.h>
#include "cut_c.h"
#include "myddas_util.h"
#include "myddas_structs.h"
#ifdef MYDDAS_ODBC
#include <sql.h>
#endif /*MYDDAS_ODBC*/
#include "Yap.h"

static MYDDAS_GLOBAL MYDDAS_GLOBAL_STRUCT = NULL;

/* Get's a new number for the temporary tables */
static
unsigned int myddas_util_get_table_number(MYDDAS_UTIL_CONNECTION);
/* Prints a error message */
static void
myddas_util_error_message(char *,int,char *);

/* Search for the connection node, before of the actual*/
static MYDDAS_UTIL_CONNECTION 
myddas_util_search_previous_connection(void *);
/* Initializes a new connection node for the MYDDAS list*/
static MYDDAS_UTIL_CONNECTION 
myddas_util_initialize_connection(void *,void *, MYDDAS_UTIL_CONNECTION);

/* Initializes a new predicate node for the MYDDAS list */
static MYDDAS_UTIL_PREDICATE 
myddas_util_initialize_predicate(char *, int,char *);
/* Search for the predicate in the given predicate list*/
static MYDDAS_UTIL_PREDICATE
myddas_util_find_predicate(char *, int , char *, MYDDAS_UTIL_PREDICATE);
/* Add's a predicate node to the given predicate list*/
static void 
myddas_util_add_predicate_node(MYDDAS_UTIL_PREDICATE, MYDDAS_UTIL_PREDICATE *);
/* Deletes a predicate list */
static void 
myddas_util_delete_predicate_list(MYDDAS_UTIL_PREDICATE);

#ifdef DEBUG
void check_int(){
  int i;
  MYDDAS_UTIL_PREDICATE pred = NULL;
  MYDDAS_UTIL_CONNECTION top = MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
  for (i=1 ; top!=NULL ; top=top->next)
    {
      printf ("***************\n");
      printf ("===== top =====\n");
      printf ("======= %p =====\n",top);
      printf ("CONN: = %p =====\n",top->connection);
      printf ("ENV : = %p =====\n",top->odbc_enviromment);
      printf ("PRED: = %p =====\n",top->predicates);
      printf ("======= %p =====\n",top->next);
      if (top->predicates != NULL)
	{
	  printf ("\t******\n");
	  printf ("\t===== PREDICADOS =====\n");
	  for (pred = top->predicates ; pred != NULL ; pred = pred->next)
	    {
	      printf ("\t--------------\n");
	      printf ("\t===== %p =====\n",pred);
	      printf ("\t===== %s =====\n",pred->pred_name);
	      printf ("\t===== %d =====\n",pred->pred_arity);
	      printf ("\t===== %s =====\n",pred->pred_module);
	      printf ("\t===== %p =====\n",pred->next);
	    }
	}
      
    }
  
  return;
}
#endif

#ifdef MYDDAS_STATS
int 
myddas_util_get_conn_total_rows(MYDDAS_UTIL_CONNECTION node){
  return node->totalNumberOfRows;
}
void 
myddas_util_set_conn_total_rows(MYDDAS_UTIL_CONNECTION node ,
		    int totalRows){
  node->totalNumberOfRows = totalRows;
}

unsigned long
myddas_util_get_conn_total_time_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->totalTimeofDBServer;
}
void 
myddas_util_set_conn_total_time_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long totaltime){
  node->totalTimeofDBServer = totaltime;
}

unsigned long
myddas_util_get_conn_last_time_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->lastTimeofDBServer;
}
void 
myddas_util_set_conn_last_time_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long lasttime){
  node->lastTimeofDBServer = lasttime;
}

unsigned long
myddas_util_get_conn_last_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->lastFromDBServer;
}
void 
myddas_util_set_conn_last_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long lasttime){
  node->lastFromDBServer = lasttime;
}

unsigned long
myddas_util_get_conn_total_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node){
  return node->totalFromDBServer;
}
void 
myddas_util_set_conn_total_transfering_from_DBServer(MYDDAS_UTIL_CONNECTION node ,
		    unsigned long totaltime){
  node->totalFromDBServer = totaltime;
}

unsigned long
myddas_util_get_total_db_row_function(void){
  return MYDDAS_GLOBAL_STRUCT->myddas_statistics->total_db_row;
}
void
myddas_util_set_total_db_row_function(unsigned long time){
  MYDDAS_GLOBAL_STRUCT->myddas_statistics->total_db_row = time;
}


unsigned long 
myddas_current_time(void) {
  /* to get time as Yap */
  
    Int now, interval;
    Yap_cputime_interval(&now, &interval);
    //return ((realtime)now);
    return (now);
  
  /*Fine grained time 
    tv_usec -> microseconds [0-999999]
  */
  /*Fine grained time 
    sec -> [0-999]
    tv_usec -> microseconds [0-99999] -> last digit is negleted
    -> max execution time: 16minutes
    milliseconds -> s/1000
    microseconds -> s/1000000
  */
  /* struct timeval tempo; */
/*   if (!gettimeofday(&tempo, NULL)) */
/*     //returns time in microseconds */
/*     return (tempo.tv_sec %1000)*1000000+tempo.tv_usec; */
/*   //return (tempo.tv_sec %1000)*1000+tempo.tv_usec; */
  return 0;
}


#endif /* MYDDAS_STATS */

/* Type: MYSQL->1 ODBC->2*/
short int
myddas_util_connection_type(void *con){
  
  MYDDAS_UTIL_CONNECTION con_node =
    myddas_util_search_connection(con);
  
  if (con_node == NULL)
    return 0;

  if (con_node->odbc_enviromment != NULL) /* ODBC */ 
    return 2;
  else
    return 1;
}

void *
myddas_util_get_pred_next(void *pointer){
  MYDDAS_UTIL_PREDICATE temp = (MYDDAS_UTIL_PREDICATE) pointer;
  return (void *) (temp->next);
}

int 
myddas_util_get_pred_arity(void *pointer){
  MYDDAS_UTIL_PREDICATE temp = (MYDDAS_UTIL_PREDICATE) pointer;
  return temp->pred_arity;
}

char *
myddas_util_get_pred_name(void *pointer){
  MYDDAS_UTIL_PREDICATE temp = (MYDDAS_UTIL_PREDICATE) pointer;
  return temp->pred_name;
}

char *
myddas_util_get_pred_module(void *pointer){
  MYDDAS_UTIL_PREDICATE temp = (MYDDAS_UTIL_PREDICATE) pointer;
  return temp->pred_module;
}

void *
myddas_util_get_list_pred(MYDDAS_UTIL_CONNECTION node){
  return (void *)(node->predicates);
}

MYDDAS_UTIL_PREDICATE
myddas_util_search_predicate(char *pred_name, int pred_arity, 
			     char *pred_module){
  MYDDAS_UTIL_PREDICATE pred=NULL;
  MYDDAS_UTIL_CONNECTION top = MYDDAS_GLOBAL_STRUCT->myddas_top_connections;

  for (;top!=NULL;top=top->next)
    {
      if ((pred=myddas_util_find_predicate(pred_name,pred_arity,pred_module,top->predicates)))
	return pred;
    }
  return NULL;
}

/* When using this function, we must guarante that this predicate
 it's unique */
MYDDAS_UTIL_CONNECTION 
myddas_util_add_predicate(char *pred_name, int pred_arity, 
			   char *pred_module, void *conn){
  
  MYDDAS_UTIL_CONNECTION node_conn = 
    myddas_util_search_connection(conn);
  
  MYDDAS_UTIL_PREDICATE new = 
    myddas_util_initialize_predicate(pred_name,pred_arity,pred_module);
  
  if (new == NULL)
    {
      myddas_util_error_message("Could not initialize predicate node",__LINE__,__FILE__);
      return NULL;
    }
  
  myddas_util_add_predicate_node(new,&(node_conn->predicates));
  
  return node_conn;
} 

void 
myddas_util_delete_connection(void *conn){
  
  MYDDAS_UTIL_CONNECTION before_to_delete = NULL;
  MYDDAS_UTIL_CONNECTION to_delete = 
    myddas_util_search_connection(conn);
    
  if (to_delete == NULL) 
    return;
  else
    {
      /*Removes the predicates list*/
      myddas_util_delete_predicate_list(to_delete->predicates);
      
      if (to_delete == MYDDAS_GLOBAL_STRUCT->myddas_top_connections)
	{
	  MYDDAS_GLOBAL_STRUCT->myddas_top_connections= to_delete->next;
	  free(to_delete);
	  return;
	}
      else 
	{
	  before_to_delete = 
	    myddas_util_search_previous_connection(conn);
	  before_to_delete->next=to_delete->next;
	  free(to_delete);
	  return;
	}
    }
}

MYDDAS_UTIL_CONNECTION 
myddas_util_search_connection(void *conn){
  MYDDAS_UTIL_CONNECTION list = MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
  
  if (conn == 0) { /* We want all the statistics */
    return list;
  }
    

  for (;list!=NULL;list=list->next)
    if (list->connection == conn)
      return list;
  return NULL;
}
 
MYDDAS_UTIL_CONNECTION 
myddas_util_add_connection(void *conn, void *enviromment){
  
  MYDDAS_UTIL_CONNECTION node=NULL;
  MYDDAS_UTIL_CONNECTION temp=NULL;

  if (MYDDAS_GLOBAL_STRUCT == NULL) 
    {
      MYDDAS_GLOBAL_STRUCT = (MYDDAS_GLOBAL) malloc (sizeof(struct myddas_global));
      MYDDAS_GLOBAL_STRUCT->myddas_top_connections = NULL;
#ifdef MYDDAS_STATS
      MYDDAS_GLOBAL_STRUCT->myddas_statistics = NULL;
#endif 
    }
  
  if ((node = myddas_util_search_connection(conn)) != NULL)
    {
      return node;
    }
  
  if (MYDDAS_GLOBAL_STRUCT->myddas_top_connections!=NULL)
    {
      //put the new connection node on the top of the list
      temp = myddas_util_initialize_connection(conn,enviromment,MYDDAS_GLOBAL_STRUCT->myddas_top_connections);
      if (temp == NULL)
	{
#ifdef DEBUG
	  myddas_util_error_message("Could not initialize connection node",__LINE__,__FILE__);
#endif 
	  return NULL;
	}
      MYDDAS_GLOBAL_STRUCT->myddas_top_connections = temp;
      return MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
    }
  else //The MYDDAS list is empty
    {
#ifdef MYDDAS_STATS
      /* Allocates the MYDDAS stats 
	 global structure */
      MYDDAS_GLOBAL_STRUCT->myddas_statistics = (MYDDAS_GLOBAL_STATS) malloc (sizeof(struct myddas_global_stats));
      MYDDAS_GLOBAL_STRUCT->myddas_statistics->total_db_row = 0;
#endif /* MYDDAS_STATS */
      temp = myddas_util_initialize_connection(conn,enviromment,NULL);
      if (temp == NULL)
	{
	  myddas_util_error_message("Could not initialize connection node",__LINE__,__FILE__);
	  return NULL;
	}
      MYDDAS_GLOBAL_STRUCT->myddas_top_connections = temp;
      return MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
    }
}

#ifdef MYDDAS_ODBC
/* This function searches the MYDDAS list for odbc connections 
 If there isn't any, it returns NULL. This is a nice way to know 
 if there is any odbc connections left on the list*/
SQLHENV
myddas_util_get_odbc_enviromment(SQLHDBC connection){
  MYDDAS_UTIL_CONNECTION top = MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
  
  for (;top != NULL;top=top->next)
    if (top->connection == ((void *)connection))
      return top->odbc_enviromment;
  
  return NULL;
}
#endif

/* Dont forget to free the memory on the other side*/
char * 
myddas_util_delete_all_temp_table(){
  
  if (MYDDAS_GLOBAL_STRUCT == NULL)
    return NULL;
  
  MYDDAS_UTIL_CONNECTION node = MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
   
  if (node->temporary_tables == NULL){ 
    return NULL; 
  }
  
  MYDDAS_TEMP_TABLES tables = node->temporary_tables;
    
  if (tables->table_numbers == NULL){
    free(tables->default_table_name);
    free(tables);
    return NULL;    /* No more tables*/
  }
  else {
    int table_number = tables->table_numbers->number;
    
    struct myddas_table_integers *temporary = tables->table_numbers;
    tables->table_numbers = tables->table_numbers->next;
    if (tables->table_numbers == NULL)
      tables->last_number = NULL;
    free(temporary);

    int i=0; 
 
    /* Number of Digits of table_number */
    for (i=1;;i++) 
      if ((table_number / (10*i)) < 10) 
	break; 
   
    int size = strlen(node->temporary_tables->default_table_name);
    char *table_name = (char *) malloc (sizeof(char) * (i + size + 1));
    
    sprintf(table_name,"%s%d",
	    node->temporary_tables->default_table_name,table_number);
    
    return table_name;
  }
}

/* Dont forget to free the memory on the other side*/
char *
myddas_util_get_table_name(void *con){
  
  int table_number;
  int i=0;
  
  MYDDAS_UTIL_CONNECTION node = myddas_util_search_connection(con);
  
  
  if (node->temporary_tables == NULL) {
    node->temporary_tables = 
      (MYDDAS_TEMP_TABLES) malloc (sizeof(struct myddas_temp_tables));
    
    node->temporary_tables->last_number = NULL;
    node->temporary_tables->table_numbers = NULL;
    
    /* Number of Digits of the connnection pointer */
    for (i=1;;i++) 
      if ((((int)con) / (10*i)) < 10) 
	break; 
  
    node->temporary_tables->default_table_name = 
      (char *) malloc (sizeof(char *) + i + 1 + 23);
    sprintf (node->temporary_tables->default_table_name,"$myddas_temporary_table%d",(int)con);
  }
  
  table_number = myddas_util_get_table_number(node);
  
  /* Number of Digits of table_number */
  for (i=1;;i++) 
    if ((table_number / (10*i)) < 10) 
      break; 
  
  int size = strlen(node->temporary_tables->default_table_name);
  char *table_name = (char *) malloc (sizeof(char) * (i + size + 1));
 
  sprintf(table_name,"%s%d",
		      node->temporary_tables->default_table_name,table_number);

  return table_name;
}

static
unsigned int myddas_util_get_table_number(MYDDAS_UTIL_CONNECTION node){
  
  MYDDAS_TEMP_TABLES table = node->temporary_tables;
  
  if (table->table_numbers == NULL) {
    table->last_number = (struct myddas_table_integers *) malloc (sizeof(struct myddas_table_integers));
    table->table_numbers = table->last_number;
    table->last_number->number = 0;
    table->last_number->next=NULL;
  } 
  else {
    unsigned int last_number = table->last_number->number;
    table->last_number->next = (struct myddas_table_integers *) malloc (sizeof(struct myddas_table_integers));
    table->last_number = table->last_number->next;
    table->last_number->number = ++last_number;
    table->last_number->next=NULL;
  }
  return table->last_number->number;
}

static
void myddas_util_error_message(char *message,int line,char *file){
#ifdef DEBUG
  printf ("ERROR: %s at line %d in file %s\n",message,line,file);
#else
  printf ("ERROR: %s\n",message);
#endif
}

static MYDDAS_UTIL_CONNECTION
myddas_util_search_previous_connection(void *conn){
  MYDDAS_UTIL_CONNECTION top = MYDDAS_GLOBAL_STRUCT->myddas_top_connections;
  for(;top->next!=NULL;top=top->next)
    if (top->next->connection == conn)
      return top;
  return NULL;
}

static MYDDAS_UTIL_CONNECTION 
myddas_util_initialize_connection(void *conn,void *enviromment,
				  MYDDAS_UTIL_CONNECTION next){
  
  MYDDAS_UTIL_CONNECTION new = malloc (sizeof(struct list_connection));
  if (new == NULL)
    {
      return NULL;
    }
  new->predicates=NULL;
  new->connection=conn;
  new->temporary_tables=NULL;
  new->odbc_enviromment=enviromment;
  new->next=next;
#ifdef MYDDAS_STATS
  new->totalNumberOfRows=0;
  new->totalTimeofDBServer=0;
  new->lastTimeofDBServer=0;
  new->totalFromDBServer=0;
  new->lastFromDBServer=0;
  new->total_db_row=0;
#endif
  return new;
}

static MYDDAS_UTIL_PREDICATE 
myddas_util_initialize_predicate(char *pred_name, int pred_arity,
				 char *pred_module){
  MYDDAS_UTIL_PREDICATE new = malloc (sizeof(struct list_preds));
  if (new == NULL) 
    {
      return NULL;
    }
  new->pred_name=pred_name;
  new->pred_arity=pred_arity;
  new->pred_module=pred_module;
  new->next=NULL;
  return new;
}

static MYDDAS_UTIL_PREDICATE
myddas_util_find_predicate(char *pred_name, int pred_arity, 
			   char *pred_module, MYDDAS_UTIL_PREDICATE list){

  for(;list != NULL ; list = list->next)
    if (pred_arity == list->pred_arity && 
	!strcmp(pred_name,list->pred_name) && 
	!strcmp(pred_module,list->pred_module))
      return list;
  
  return NULL;
}

static void 
myddas_util_add_predicate_node(MYDDAS_UTIL_PREDICATE new, 
			       MYDDAS_UTIL_PREDICATE *list){

  MYDDAS_UTIL_PREDICATE temp = *list;
  *list = new;
  new->next = temp;

}

static void 
myddas_util_delete_predicate_list(MYDDAS_UTIL_PREDICATE preds_list){
  MYDDAS_UTIL_PREDICATE to_delete = NULL;
  
  for (;preds_list != NULL;)
    {
      to_delete = preds_list;
      preds_list = preds_list->next;

      free(to_delete);
    }
  return;
}


#endif /*defined MYDDAS_ODBC || defined MYDDAS_MYSQL*/
#endif /*CUT_C*/

