/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G% 					 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Regs.h							 *
* mods:									 *
* comments:	YAP abstract machine registers				 *
* version:      $Id: Regs.h,v 1.6 2001-11-15 00:01:43 vsc Exp $	 *
*************************************************************************/


/*********  abstract machine registers **********************************/

#define MaxTemps	512

#ifdef i386
#define PUSH_REGS 1
#ifdef THREADS
#undef  PUSH_X
#endif
#endif

#if defined(sparc) || defined(__sparc)
#undef  PUSH_REGS
#undef  PUSH_X
#endif

#ifdef __alpha
#undef  PUSH_REGS
#undef  PUSH_X
#endif

#ifdef _POWER
#undef  PUSH_REGS
#undef  PUSH_X
#endif

#ifdef hppa
#undef  PUSH_REGS
#undef  PUSH_X
#endif

#ifdef mips
#undef  PUSH_REGS
#undef  PUSH_X
#endif

EXTERN void restore_machine_regs(void);
EXTERN void save_machine_regs(void);
EXTERN void restore_H(void);
EXTERN void save_H(void);
EXTERN void restore_B(void);
EXTERN void save_B(void);


typedef struct
  {
    CELL    CreepFlag_;		/* 13                                         */
    CELL   *HB_;		/* 4 heap (global) stack top at latest c.p.   */
#if (defined(YAPOR) && defined(SBA)) || defined(TABLING)
    choiceptr BB_;		/* 4 local stack top at latest c.p.   */
#endif
    CELL  *H0_;			/* 2 base of heap (global) stack              */
    tr_fr_ptr TR_;		/* 24 top of trail                            */
    CELL   *H_;			/* 25 top of heap (global)  stack             */
    choiceptr B_;		/* 26 latest choice point                     */
#ifdef  DEPTH_LIMIT
    CELL   DEPTH_;		/* 27                                         */
#endif  /* DEPTH_LIMIT */
    yamop *CP_;			/* 28 continuation program counter            */
    yamop *P_;			/* 7 prolog machine program counter           */
    CELL  *YENV_;		/* 5 current environment (may differ from ENV)*/
    CELL  *S_;			/* 6 structure pointer                        */
    CELL  *ENV_;		/* 1 current environment                      */
    CELL  *ASP_;		/* 8 top of local       stack                 */
    CELL  *LCL0_;		/* 3 local stack base                         */
    CELL  *AuxSp_;		/* 9 Auxiliary stack pointer                  */
    ADDR   AuxTop_;		/* 10 Auxiliary stack top                     */
    ADDR   HeapPlus_;		/* 11 To avoid collisions with HeapTop        */
    tr_fr_ptr MyTR_;		/* 12                                         */
/* visualc*/
    choiceptr   TopB_;		/* 17 Top up to where we can cut to           */
    choiceptr   DelayedB_;	/* 17 Cut Still To be Done                    */
    CELL   FlipFlop_;		/* 18                                         */
#ifdef COROUTINING
    Term  DelayedVars_;         /* maximum number of attributed variables     */
#endif
#ifndef USE_OFFSETS
#ifndef EXT_BASE
    Term   TermDot_;		/* 19                                         */
    Term   TermNil_;		/* 20                                         */
#endif
#endif
    SMALLUNSGN  CurrentModulePtr_;
#if (defined(YAPOR) && defined(SBA)) || defined(TABLING)
    CELL *H_FZ_;
    choiceptr B_FZ_;
    tr_fr_ptr TR_FZ_;
#endif
#ifdef YAPOR
#ifdef SBA
    choiceptr BSEG_;
    struct or_frame *frame_head_, *frame_tail_;
    char *binding_array_;
    int  sba_offset_;
    int  sba_end_;
    int  sba_size_;
#endif /* SBA */
#endif /* YAPOR */
#if PUSH_REGS
    /* On a X86 machine, the best solution is to keep the
       X registers on a global variable, whose address is known between
       runs, and to push the remaining registers to the stack.

       On a register based machine, one can have a dedicated register,
       always pointing to the XREG global variable. This spends an
       extra register, but makes it easier to access X[1].
     */

#if PUSH_X
    Term XTERMS[MaxTemps];	/* 29                                    */

#define XREGS	  REGS.XTERMS

#endif
  }
REGSTORE;


extern REGSTORE *regp;

#if !PUSH_X

	/* keep X as a global variable */

Term XREGS[MaxTemps];		/* 29                                     */
#endif

#define REGS (*regp)

#else /* PUSH_REGS */

    Term X[MaxTemps];		/* 29                                     */

#define XREGS	  REGS.X

  }
REGSTORE;

extern REGSTORE REGS;
#endif /* PUSH_REGS */

#define MinTrailGap (sizeof(CELL)*1024)
#define MinHeapGap  (sizeof(CELL)*4096)
#define MinStackGap (sizeof(CELL)*8*1024)
extern int stack_overflows;


#define ENV  REGS.ENV_	/* current environment                    */
#define ASP  REGS.ASP_	/* top of local   stack                   */
#define H0   REGS.H0_	/* base of heap (global) stack            */
#define LCL0 REGS.LCL0_	/* local stack base                       */

#if defined(__GNUC__) && defined(sparc) && !defined(__NetBSD__)

#define P    REGS.P_		/* prolog machine program counter         */
#define YENV REGS.YENV_	/* current environment (may differ from   ENV)*/
#define S    REGS.S_		/* structure pointer                      */

register CELL *H asm ("g6");
register tr_fr_ptr TR asm ("g7");
#ifdef __svr4__
register choiceptr B asm ("g5");
#else
#define  B         REGS.B_	/* latest choice point            */
#endif
#define CP         REGS.CP_	/* continuation   program counter         */
#define HB         REGS.HB_	/* heap (global) stack top at time of latest c.p. */
#define CreepFlag  REGS.CreepFlag_

EXTERN inline void save_machine_regs(void) {
  REGS.H_   = H;
  REGS.TR_  = TR;
#ifdef __svr4__
  REGS.B_  = B;
#endif
}

EXTERN inline void restore_machine_regs(void) {
  H = REGS.H_;
  TR = REGS.TR_;
#ifdef __svr4__
  B = REGS.B_;
#endif
}

#define BACKUP_MACHINE_REGS()           \
  CELL     *BK_H = H;                   \
  choiceptr BK_B = B;                   \
  tr_fr_ptr BK_TR = TR;                 \
  restore_machine_regs()

#define RECOVER_MACHINE_REGS()          \
  save_machine_regs();                  \
  H = BK_H;                             \
  B = BK_B;                             \
  TR = BK_TR

EXTERN inline void save_H(void) {
  REGS.H_   = H;
}

EXTERN inline void restore_H(void) {
  H = REGS.H_;
}

#define BACKUP_H()  CELL *BK_H = H;  restore_H()

#define RECOVER_H()   save_H(); H = BK_H

EXTERN inline void save_B(void) {
#ifdef __svr4__
  REGS.B_   = B;
#endif
}

EXTERN inline void restore_B(void) {
#ifdef __svr4__
  B = REGS.B_;
#endif
}

#ifdef __svr4__
#define BACKUP_B()  choiceptr BK_B = B; restore_B()

#define RECOVER_B()   save_B(); B = BK_B
#else
#define BACKUP_B()  

#define RECOVER_B()   
#endif

#elif defined(__GNUC__) && defined(__alpha)

#define P               REGS.P_	/* prolog machine program counter */
#define YENV            REGS.YENV_	/* current environment (may differ from ENV) */
register CELL *H asm ("$9");
register CELL *HB asm ("$10");
register choiceptr B asm ("$11");
register yamop *CP asm ("$12");
register CELL *S asm ("$13");
register CELL CreepFlag asm ("$14");
register tr_fr_ptr TR asm ("$15");

/* Interface with foreign code, make sure the foreign code sees all the
   registers the way they used to be */
EXTERN inline void save_machine_regs(void) {
  REGS.H_   = H;
  REGS.HB_ = HB;
  REGS.B_   = B;
  REGS.CP_ = CP;
  REGS.CreepFlag_ = CreepFlag;
  REGS.TR_  = TR;
}

EXTERN inline void restore_machine_regs(void) {
  H = REGS.H_;
  HB = REGS.HB_;
  B = REGS.B_;
  CP = REGS.CP_;
  CreepFlag = REGS.CreepFlag_;
  TR = REGS.TR_;
}

#define BACKUP_MACHINE_REGS()           \
  CELL     *BK_H = H;                   \
  CELL     *BK_HB = HB;                 \
  choiceptr BK_B = B;                   \
  CELL      BK_CreepFlag = CreepFlag;   \
  yamop    *BK_CP = CP;                 \
  tr_fr_ptr BK_TR = TR;                 \
  restore_machine_regs()

#define RECOVER_MACHINE_REGS()          \
  save_machine_regs();                  \
  H = BK_H;                             \
  HB = BK_HB;                           \
  B = BK_B;                             \
  CreepFlag = BK_CreepFlag;             \
  CP = BK_CP;                           \
  TR = BK_TR

EXTERN inline void save_H(void) {
  REGS.H_   = H;
}

EXTERN inline void restore_H(void) {
  H = REGS.H_;
}

#define BACKUP_H()  CELL *BK_H = H; restore_H()

#define RECOVER_H()  save_H(); H = BK_H

EXTERN inline void save_B(void) {
  REGS.B_   = B;
}

EXTERN inline void restore_B(void) {
  B = REGS.B_;
}

#define BACKUP_B()  choiceptr BK_B = B; restore_B()

#define RECOVER_B()  save_B(); B = BK_B

EXTERN inline void save_TR(void) {
  REGS.TR_ = TR;
}

EXTERN inline void restore_TR(void) {
  TR = REGS.TR_;
}

#elif defined(__GNUC__) && defined(mips)

#define P               REGS.P_	/* prolog machine program counter */
#define YENV            REGS.YENV_	/* current environment (may differ from ENV) */
register CELL *H asm ("$16");
register CELL *HB asm ("$17");
register choiceptr B asm ("$18");
register yamop *CP asm ("$19");
register CELL *S asm ("$20");
register CELL CreepFlag asm ("$21");
register tr_fr_ptr TR asm ("$22");

EXTERN inline void save_machine_regs(void) {
  REGS.H_   = H;
  REGS.HB_ = HB;
  REGS.B_   = B;
  REGS.CP_  = CP;
  REGS.CreepFlag_ = CreepFlag;
  REGS.TR_  = TR;
}

EXTERN inline void restore_machine_regs(void) {
  H = REGS.H_;
  HB = REGS.HB_;
  B = REGS.B_;
  CP = REGS.CP_;
  CreepFlag = REGS.CreepFlag_;
  TR = REGS.TR_;
}

#define BACKUP_MACHINE_REGS()           \
  CELL     *BK_H = H;                   \
  CELL     *BK_HB = HB;                 \
  choiceptr BK_B = B;                   \
  CELL      BK_CreepFlag = CreepFlag;   \
  yamop    *BK_CP = CP;                 \
  tr_fr_ptr BK_TR = TR;                 \
  restore_machine_regs()

#define RECOVER_MACHINE_REGS()          \
  save_machine_regs();                  \
  H = BK_H;                             \
  HB = BK_HB;                           \
  B = BK_B;                             \
  CreepFlag = BK_CreepFlag;             \
  CP = BK_CP;                           \
  TR = BK_TR

EXTERN inline void save_H(void) {
  REGS.H_   = H;
}

EXTERN inline void restore_H(void) {
  H = REGS.H_;
}

#define BACKUP_H()  CELL *BK_H = H; restore_H()

#define RECOVER_H()  save_H(); H = BK_H

EXTERN inline void save_B(void) {
  REGS.B_ = B;
}

EXTERN inline void restore_B(void) {
  B = REGS.B_;
}

#define BACKUP_B()  choiceptr BK_B = B; restore_B()

#define RECOVER_B()  save_B(); B = BK_B

#elif defined(__GNUC__) && defined(hppa)

#define P               REGS.P_	/* prolog machine program counter */
#define YENV            REGS.YENV_	/* current environment (may differ from ENV) */
register CELL *H asm ("r12");
register CELL *HB asm ("r13");
register choiceptr B asm ("r14");
register yamop *CP asm ("r15");
register CELL *S asm ("r16");
register CELL CreepFlag asm ("r17");
register tr_fr_ptr TR asm ("r18");

EXTERN inline void save_machine_regs(void) {
  REGS.H_   = H;
  REGS.HB_ = HB;
  REGS.B_   = B;
  REGS.CP_  = CP;
  REGS.CreepFlag_ = CreepFlag;
  REGS.TR_  = TR;
}

EXTERN inline void restore_machine_regs(void) {
  H = REGS.H_;
  HB = REGS.HB_;
  B = REGS.B_;
  CP = REGS.CP_;
  CreepFlag = REGS.CreepFlag_;
  TR = REGS.TR_;
}

#define BACKUP_MACHINE_REGS()           \
  CELL     *BK_H = H;                   \
  CELL     *BK_HB = HB;                 \
  choiceptr BK_B = B;                   \
  CELL      BK_CreepFlag = CreepFlag;   \
  yamop    *BK_CP = CP;                 \
  tr_fr_ptr BK_TR = TR;                 \
  restore_machine_regs()

#define RECOVER_MACHINE_REGS()          \
  save_machine_regs();                  \
  H = BK_H;                             \
  HB = BK_HB;                           \
  B = BK_B;                             \
  CreepFlag = BK_CreepFlag;             \
  CP = BK_CP;                           \
  TR = BK_TR

EXTERN inline void save_H(void) {
  REGS.H_   = H;
}

EXTERN inline void restore_H(void) {
  H = REGS.H_;
}

#define BACKUP_H()  CELL *BK_H = H; restore_H()

#define RECOVER_H()  save_H(); H = BK_H

EXTERN inline void save_B(void) {
  REGS.B_ = B;
}

EXTERN inline void restore_B(void) {
  B = REGS.B_;
}

#define BACKUP_B()  choiceptr BK_B = B; restore_B()

#define RECOVER_B()  save_B(); B = BK_B

EXTERN inline void save_TR(void) {
  REGS.TR_ = TR;
}

EXTERN inline void restore_TR(void) {
  TR = REGS.TR_;
}

#elif defined(__GNUC__) && defined(_POWER)

/* 

   Because of a bug in GCC, we should always start from the first available
   general register. According to rs6000.h, this is r13 everywhere
   except for svr4 machines, where r13 is fixed.

   If we don't do so, GCC will generate move multiple instructions for
   entering complex functions. These instructions will save and
   restore the global registers :-(.

 */
#ifndef __svr4__
register CELL CreepFlag  asm ("r13");
#else
register CELL CreepFlag  asm ("r21");
#endif
register CELL *H asm ("r14");
register CELL *HB asm ("r15");
register choiceptr B asm ("r16");
register yamop *CP asm ("r17");
register CELL *S asm ("r18");
register CELL *YENV asm ("r19");
register tr_fr_ptr TR asm ("r20");
#define P    REGS.P_		/* prolog machine program counter */

EXTERN inline void save_machine_regs(void) {
  REGS.CreepFlag_ = CreepFlag;
  REGS.H_   = H;
  REGS.HB_ = HB;
  REGS.B_   = B;
  REGS.CP_ = CP;
  REGS.YENV_  = YENV;
  REGS.TR_  = TR;
}

EXTERN inline void restore_machine_regs(void) {
  CreepFlag = REGS.CreepFlag_;
  H = REGS.H_;
  HB = REGS.HB_;
  B = REGS.B_;
  CP = REGS.CP_;
  YENV = REGS.YENV_;
  TR = REGS.TR_;
}

#define BACKUP_MACHINE_REGS()           \
  CELL     *BK_H = H;                   \
  CELL     *BK_HB = HB;                 \
  choiceptr BK_B = B;                   \
  CELL      BK_CreepFlag = CreepFlag;   \
  yamop    *BK_CP = CP;                 \
  tr_fr_ptr BK_TR = TR;                 \
  restore_machine_regs()

#define RECOVER_MACHINE_REGS()          \
  save_machine_regs();                  \
  H = BK_H;                             \
  HB = BK_HB;                           \
  B = BK_B;                             \
  CreepFlag = BK_CreepFlag;             \
  CP = BK_CP;                           \
  TR = BK_TR

EXTERN inline void save_H(void) {
  REGS.H_   = H;
}

EXTERN inline void restore_H(void) {
  H = REGS.H_;
}

#define BACKUP_H()  CELL *BK_H = H; restore_H()

#define RECOVER_H()  save_H(); H = BK_H

EXTERN inline void save_B(void) {
  REGS.B_   = B;
}

EXTERN inline void restore_B(void) {
  B = REGS.B_;
}

#define BACKUP_B()  choiceptr BK_B = B; restore_B()

#define RECOVER_B()  save_B(); B = BK_B

#else

#define CP         REGS.CP_	/* continuation   program counter         */
#define P          REGS.P_	/* prolog machine program counter */
#define YENV       REGS.YENV_ /* current environment (may differ from ENV) */
#define S          REGS.S_	/* structure pointer                      */
#define	H          REGS.H_	/* top of heap (global)   stack           */
#define B          REGS.B_	/* latest choice point            */
#define TR         REGS.TR_	/* top of trail                           */
#define HB         REGS.HB_	/* heap (global) stack top at time of latest c.p. */
#define CreepFlag REGS.CreepFlag_

EXTERN inline void save_machine_regs(void) {
}

EXTERN inline void restore_machine_regs(void) {
}

#define BACKUP_MACHINE_REGS()

#define RECOVER_MACHINE_REGS()

EXTERN inline void save_H(void) {
}

EXTERN inline void restore_H(void) {
}

#define BACKUP_H()

#define RECOVER_H()

EXTERN inline void save_B(void) {
}

EXTERN inline void restore_B(void) {
}

#define BACKUP_B()

#define RECOVER_B()

#endif

#define	AuxSp     REGS.AuxSp_
#define	AuxTop    REGS.AuxTop_
#define	HeapPlus  REGS.HeapPlus_	/*To avoid any chock with HeapTop */
#define MyTR	  REGS.MyTR_
#define TopB      REGS.TopB_
#define DelayedB  REGS.DelayedB_
#define FlipFlop  REGS.FlipFlop_
#define DEPTH	  REGS.DEPTH_
#if (defined(YAPOR) && defined(SBA)) || defined(TABLING)
#define H_FZ          REGS.H_FZ_
#define B_FZ          REGS.B_FZ_
#define TR_FZ         REGS.TR_FZ_
#endif
#ifdef YAPOR
#ifdef SBA
#define BSEG	      REGS.BSEG_
#define binding_array REGS.binding_array_
#define sba_offset    REGS.sba_offset_
#define sba_end       REGS.sba_end_
#define sba_size      REGS.sba_size_
#define frame_head    REGS.frame_head_
#define frame_tail    REGS.frame_tail_
#endif /* SBA */
#endif /* YAPOR */
#ifdef COROUTINING
#define DelayedVars   REGS.DelayedVars_
#endif
#define CurrentModule REGS.CurrentModulePtr_

#define REG_SIZE	sizeof(REGS)/sizeof(CELL *)

#define ARG1	XREGS[1]
#define ARG2	XREGS[2]
#define ARG3    XREGS[3]
#define ARG4	XREGS[4]
#define ARG5	XREGS[5]
#define ARG6	XREGS[6]
#define ARG7	XREGS[7]
#define ARG8	XREGS[8]
#define ARG9	XREGS[9]
#define ARG10	XREGS[10]
#define ARG11	XREGS[11]
#define ARG12	XREGS[12]
#define ARG13	XREGS[13]
#define ARG14	XREGS[14]
#define ARG15	XREGS[15]
#define ARG16	XREGS[16]

#define cut_succeed()	return( ( B = B->cp_b, 1 ))
#define cut_fail()	return( ( B = B->cp_b, 0 ))


/* by default, define HBREG to be HB */

#define HBREG HB

#if (defined(YAPOR) && defined(SBA)) || defined(TABLING)
#define BB            REGS.BB_
#define BBREG         BB
#endif

/* use global stack for intermediate data-structures */
#define AllocTmpSpaceFromGlobal()           ((char *)H)

#if !THREADS
/* use actual addresses for regs */
#define PRECOMPUTE_REGADDRESS 1
#endif /* THREADS */


/* aggregate several abstract machine operations in a single */
#define AGGREGATE_OPS 1

/* make standard registers globally accessible so that they are there
   when we come from a longjmp */
#if PUSH_REGS
/* In this case we need to initialise the abstract registers */
REGSTORE standard_regs;
#endif /* PUSH_REGS */

static inline UInt
CalculateStackGap(void)
{
  UInt gmin = (LCL0-H0)>>1;
  UInt min_gap = MinStackGap;
  return(gmin < min_gap ? min_gap : gmin );
}
