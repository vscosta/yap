/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

#define TOP_STACK          YENV

#define HEAP_ARITY_ENTRY   (0)
#define VARS_ARITY_ENTRY   (1 + heap_arity)
#define SUBS_ARITY_ENTRY   (1 + heap_arity + 1 + vars_arity)

/* macros 'HEAP_ENTRY', 'VARS_ENTRY' and 'SUBS_ENTRY' **
** assume that INDEX starts at 1 (and not at 0 !!!)   */
#define HEAP_ENTRY(INDEX)  (HEAP_ARITY_ENTRY + (INDEX))
#define VARS_ENTRY(INDEX)  (VARS_ARITY_ENTRY + 1 + vars_arity - (INDEX))
#define SUBS_ENTRY(INDEX)  (SUBS_ARITY_ENTRY + 1 + subs_arity - (INDEX))

#if 0
/************************************************************************
**                           clause_with_cut                           **
************************************************************************/

Op(clause_with_cut, e)
  { printf("clause_with_cut not supported by JIT!!\n"); exit(1); }
ENDOp();

#endif

/************************************************************************
**                          table_load_answer                          **
************************************************************************/

PBOp(table_load_answer, Otapl)
{ printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDPBOp();



/************************************************************************
 **                          table_try_answer                           **
************************************************************************/

  PBOp(table_try_answer, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                          table_try_single                           **
************************************************************************/

  PBOp(table_try_single, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                            table_try_me                             **
************************************************************************/

  PBOp(table_try_me, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                             table_try                               **
************************************************************************/

  PBOp(table_try, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 
  ENDPBOp();
 


/************************************************************************
 **                           table_retry_me                            **
************************************************************************/

  Op(table_retry_me, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDOp();



/************************************************************************
**                            table_retry                              **
************************************************************************/

  Op(table_retry, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
   ENDOp();



/************************************************************************
**                           table_trust_me                            **
************************************************************************/

  Op(table_trust_me, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDOp();



/************************************************************************
**                            table_trust                              **
************************************************************************/

  Op(table_trust, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
   ENDOp();



/************************************************************************
**                          table_new_answer                           **
************************************************************************/

  PBOp(table_new_answer, s)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                      table_answer_resolution                        **
************************************************************************/

  BOp(table_answer_resolution, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDBOp();



/************************************************************************
 **                          table_completion                           **
************************************************************************/

  BOp(table_completion, Otapl)
  ENDBOp();



/************************************************************************
**                 table_answer_resolution_completion                  **
************************************************************************/

#ifdef THREADS_CONSUMER_SHARING
  BOp(table_answer_resolution_completion, Otapl)
  { printf("table_answer_resolution_completion not supported by JIT!!\n"); exit(1); }
  ENDBOp();
#endif /* THREADS_CONSUMER_SHARING */
