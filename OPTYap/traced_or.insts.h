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

/* -------------------------------- **
**      Scheduler instructions      **
** -------------------------------- */

PBOp(getwork_first_time,e)
{ printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
      ENDPBOp();




      PBOp(getwork,Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDPBOp();



  /* The idea is to check whether we are the last worker in the node.
     If we are, we can go ahead, otherwise we should call the scheduler. */
PBOp(getwork_seq,Otapl)
{ printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDPBOp();



  PBOp(sync,Otapl)
  { printf("Or-parallelism not supported by JIT!!\n"); exit(1); }
  ENDPBOp();
