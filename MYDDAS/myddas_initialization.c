//* Initializes a new connection node for the MYDDAS list*/
static MYDDAS_UTIL_CONNECTION 
myddas_util_initialize_connection(void *,void *, 
				  MYDDAS_UTIL_CONNECTION);

/* Initializes a new predicate node for the MYDDAS list */
static MYDDAS_UTIL_PREDICATE 
myddas_util_initialize_predicate(char *, int,char *,
				 MYDDAS_UTIL_PREDICATE);

MYDDAS_GLOBAL
myddas_util_initialize_myddas(void){
  MYDDAS_GLOBAL global = NULL;
  
  global = (MYDDAS_GLOBAL) malloc (sizeof(struct myddas_global));
  
  global->myddas_top_connections = NULL;
#ifdef MYDDAS_TOP_LEVEL
  global->myddas_top_level_connection = NULL;
#endif
#ifdef MYDDAS_STATS
  global->myddas_statistics = (MYDDAS_GLOBAL_STATS) malloc (sizeof(struct myddas_global_stats));
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(global->myddas_statistics->total_db_row,time_final);
#endif
  return global;
}



/* Inserts the new node on the front of the list */
static MYDDAS_UTIL_CONNECTION 
myddas_util_initialize_connection(void *conn,void *enviromment,
				  MYDDAS_UTIL_CONNECTION next){
  
  MYDDAS_UTIL_CONNECTION new = malloc (sizeof(struct myddas_list_connection));
  if (new == NULL)
    {
      return NULL;
    }
  new->predicates=NULL;
  new->connection=conn;
  new->odbc_enviromment=enviromment;

  /* It saves n queries, doing at once n+1 queries */
  new->total_number_queries=0; //Default
  new->actual_number_queries=0;
  new->queries = NULL;

  /* List integrity */
  new->next=next;
  new->previous=NULL;
  /* If there's already at least one node 
   on the list */
  if (next != NULL)
    next->previous=new;
  
#ifdef MYDDAS_STATS
  new->totalNumberOfRows=0;
  
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(new->totalTimeofDBServer,time_final);
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(new->lastTimeofDBServer,time_final);
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(new->totalFromDBServer,time_final);
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(new->lastFromDBServer,time_final);
  
  new->lastBytesTransfered=0;
  new->totalBytesTransfered=0;
  MYDDAS_STATS_INITIALIZE_TIME_STRUCT(new->total_db_row,time_final);
  new->total_querys_made=0;
#endif
  return new;
}

static MYDDAS_UTIL_PREDICATE 
myddas_util_initialize_predicate(char *pred_name, int pred_arity,
				 char *pred_module, MYDDAS_UTIL_PREDICATE next){
  MYDDAS_UTIL_PREDICATE new = malloc (sizeof(struct myddas_list_preds));
  if (new == NULL) 
    {
      return NULL;
    }
  new->pred_name=pred_name;
  new->pred_arity=pred_arity;
  new->pred_module=pred_module;
  
  /* List integrity */
  new->next=next;
  new->previous=NULL;
  /* If there's already at least one node 
     on the list */
  if (next != NULL)
    next->previous=new;
  
  return new;
}

