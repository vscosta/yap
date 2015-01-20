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



/************************************************************************
**                           clause_with_cut                           **
************************************************************************/

Op(traced_clause_with_cut, e)
  { printf("Tabling not supported by JIT!!\n"); exit(1); }
ENDOp();



/************************************************************************
**                          table_load_answer                          **
************************************************************************/

PBOp(traced_table_load_answer, Otapl)
{ printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDPBOp();



/************************************************************************
 **                          table_try_answer                           **
************************************************************************/

  PBOp(traced_table_try_answer, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                          table_try_single                           **
************************************************************************/

  PBOp(traced_table_try_single, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                            table_try_me                             **
************************************************************************/

  PBOp(traced_table_try_me, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                             table_try                               **
************************************************************************/

  PBOp(traced_table_try, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 
  ENDPBOp();
 


/************************************************************************
 **                           table_retry_me                            **
************************************************************************/

  Op(traced_table_retry_me, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDOp();



/************************************************************************
**                            table_retry                              **
************************************************************************/

  Op(traced_table_retry, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
   ENDOp();



/************************************************************************
**                           table_trust_me                            **
************************************************************************/

  Op(traced_table_trust_me, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDOp();



/************************************************************************
**                            table_trust                              **
************************************************************************/

  Op(traced_table_trust, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
   ENDOp();



/************************************************************************
**                          table_new_answer                           **
************************************************************************/

  PBOp(traced_table_new_answer, s)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDPBOp();



/************************************************************************
 **                      table_answer_resolution                        **
************************************************************************/

  BOp(traced_table_answer_resolution, Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
 ENDBOp();



/************************************************************************
 **                          table_completion                           **
************************************************************************/

  BOp(traced_table_completion, Otapl)
  ENDBOp();



/************************************************************************
**                 table_answer_resolution_completion                  **
************************************************************************/

  BOp(traced_table_answer_resolution_completion, Otapl)
#ifdef THREADS_CONSUMER_SHARING
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
#endif /* THREADS_CONSUMER_SHARING */
  ENDBOp();
