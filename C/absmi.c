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
* File:		absmi.c							 *
* Last rev:								 *
* mods:									 *
* comments:	Portable abstract machine interpreter                    *
*									 *
*************************************************************************/

#define IN_ABSMI_C 1

#include "absmi.h"
#include "heapgc.h"

#if USE_THREADED_CODE

/* easy access to instruction opcodes */
void **ABSMI_OPCODES;

#endif

inline static Functor
AritFunctorOfTerm(Term t) {
  if (IsVarTerm(t)) {
    return(FunctorDBRef);
  }
  if (IsApplTerm(t)) {
    return(FunctorOfTerm(t));
  } else {
    if (IsIntTerm(t))
      return(FunctorLongInt);
    else
      return(FunctorDBRef);
  }
}

#define RINT(v)     return(MkIntegerTerm(v))
#define RFLOAT(v)   return(MkFloatTerm(v))
#define RBIG(v)     return(MkBigIntTerm(v))
#define RERROR()    return(TermNil)

#define ArithIEval(t,v)     Eval(Deref(t),v)

#define E_FUNC   Term
#define E_ARGS   
#define USE_E_ARGS   

#include "arith2.h"

#ifdef COROUTINING
/*
  Imagine we are interrupting the execution, say, because we have a spy
   point or because we have goals to wake up. This routine saves the current
   live temporary registers into a structure pointed to by register ARG1.
   The registers are then recovered by a nasty builtin
   called 
*/
static Term
push_live_regs(yamop *pco)
{
  CELL *lab = (CELL *)(pco->u.l.l);
  CELL max = lab[0];
  Int curr = lab[1];
  CELL *start = H;
  Int tot = 0;

  if (max) {
    Int i;

    lab += 2;
    H++;
    for (i=0; i <= max; i++) {
      if (i == 8*CellSize) {
	curr = lab[0];
	lab++;
      }
      if (curr & 1) {
	CELL d1;

	tot+=2;
	H[0] = MkIntTerm(i);
	d1 = XREGS[i];
	deref_head(d1, wake_up_unk);
      wake_up_nonvar:
	/* just copy it to the heap */
	H[1] = d1;
	H += 2;
	continue;

	{
	  CELL *pt0;
	  deref_body(d1, pt0, wake_up_unk, wake_up_nonvar);
	  /* bind it, in case it is a local variable */
	  if (pt0 <= H) {
	    /* variable is safe */
	    H[1] = (CELL)pt0;
	  } else {
	    d1 = Unsigned(H+1);
	    RESET_VARIABLE(H+1);
	    Bind_Local(pt0, d1);
	  }
	}
	H += 2;
      }
      curr >>= 1;
    }
    start[0] = (CELL)MkFunctor(AtomTrue, tot);
    return(AbsAppl(start));
  } else {
    return(TermNil);
  }
}
#endif

Int 
absmi(int inp)
{


#if BP_FREE
  /* some function might be using bp for an internal variable, it is the
     callee's responsability to save it */
  yamop* PCBACKUP = P1REG;
#endif

#ifdef LONG_LIVED_REGISTERS
  register CELL d0, d1;
  register CELL *pt0, *pt1;

#endif /* LONG_LIVED_REGISTERS */

#if SHADOW_P
  register yamop *PREG = P;
#endif /* SHADOW_P */

#if SHADOW_CP
  register yamop *CPREG = CP;
#endif /* SHADOW_CP */

#if SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

#if SHADOW_CrFl
  register CELL CFREG = CreepFlag;
#endif /* SHADOW_CP */

#if SHADOW_Y
  register CELL *Y = REGS.YENV_;
#endif /* SHADOW_Y */

#if SHADOW_S
  register CELL *SREG = REGS.S_;
#else
#define SREG S
#endif /* SHADOW_S */

  /* The indexing register so that we will not destroy ARG1 without
   * reason */
#define I_R (XREGS[0])

#if USE_THREADED_CODE
/************************************************************************/
/*     Abstract Machine Instruction Address Table                       */
/*  This must be declared inside the function. We use the asm directive */
/* to make it available outside this function                           */
/************************************************************************/
  static void *OpAddress[] =
  {
#define OPCODE(OP,TYPE) && OP
#include "YapOpcodes.h"
#undef  OPCODE
  };

#endif /* USE_THREADED_CODE */

#if SHADOW_REGS

  /* work with a local pointer to the registers */
  register REGSTORE *regp = &REGS;

#endif /* SHADOW_REGS */

#if PUSH_REGS

/* useful on a X86 with -fomit-frame-pointer optimisation */
  /* The idea is to push REGS onto the X86 stack frame */

  /* first allocate local space */
  REGSTORE absmi_regs;
  REGSTORE *old_regs = regp;

#endif /* PUSH_REGS */

#if USE_THREADED_CODE
  /* absmadr */
  if (inp > 0) {
    ABSMI_OPCODES = OpAddress;
#if BP_FREE
    P1REG = PCBACKUP;
#endif
    return(0);
  }
#endif /* USE_THREADED_CODE */

 reset_absmi:

#if PUSH_REGS
  old_regs = &REGS;

  /* done, let us now initialise this space */
  init_absmi_regs(&absmi_regs);

  /* the registers are all set up, let's swap */
  regp = &absmi_regs;
#undef REGS
#define REGS absmi_regs

#endif /* PUSH_REGS */

#if SHADOW_REGS

  /* use regp as a copy of REGS */
  regp = &REGS;

#ifdef REGS
#undef REGS
#endif
#define REGS (*regp)

#endif /* SHADOW_REGS */

  setregs();

#if !S_IN_MEM
  CACHE_A1();
#endif

  SP = SP0;

#if USE_THREADED_CODE
  JMPNext();			/* go execute instruction at P          */

#else
  /* when we start we are not in write mode */

  {
    op_numbers opcode = _Ystop;
#ifdef DEBUG_XX
    op_numbers old_op;
    unsigned long ops_done;
#endif

    goto nextop;

  nextop_write:

#ifdef DEBUG_XX
    old_op = opcode;
#endif
    opcode = PREG->u.o.opcw;
    goto op_switch;

  nextop:

#ifdef DEBUG_XX
    old_op = opcode;
#endif
    opcode = PREG->opc;

  op_switch:

#ifdef ANALYST
    opcount[opcode]++;
#ifdef DEBUG_XX
    ops_done++;
    /*    if (B->cp_b > 0x103fff90)
      fprintf(stderr,"(%ld) doing %s, done %s, B is %p, HB is %p, H is %p\n",
      ops_done,op_names[opcode],op_names[old_op],B,B->cp_h,H);*/
#endif
#endif /* ANALYST */

    switch (opcode) {
#endif /* USE_THREADED_CODE */

    noheapleft:
      CFREG = CalculateStackGap();
      saveregs();
#if PUSH_REGS
      restore_absmi_regs(old_regs);
#endif
      if (!growheap(FALSE)) {
	Abort("[ SYSTEM ERROR: YAP failed to reserve space in growheap ]\n");
	FAIL();
      }
      goto reset_absmi;

#if !OS_HANDLES_TR_OVERFLOW
    notrailleft:
      saveregs();
      /* if we are within indexing code, the system may have to
       * update a S */
#if SHADOW_S
      S = SREG;
#endif
      /* Y was pointing to where we were going to build the
       * next choice-point. The stack shifter will need to know this
       * to move the local stack */
      if (Y > (CELL *) B) {
	ASP = (CELL *) B;
      }
      else {
	ASP = Y+E_CB;
      }
#if PUSH_REGS
      restore_absmi_regs(old_regs);
#endif
      if(!growtrail (sizeof(CELL) * 16 * 1024L)) {
	Error(SYSTEM_ERROR,TermNil,"YAP failed to reserve %ld bytes in growtrail",sizeof(CELL) * 16 * 1024L);
	FAIL();
      }
      goto reset_absmi;

#endif /* OS_HANDLES_TR_OVERFLOW */

      BOp(Ystop, e);
      saveregs();
#if PUSH_REGS
      restore_absmi_regs(old_regs);
#endif
#if BP_FREE
      P1REG = PCBACKUP;
#endif
      return (1);
      ENDBOp();

      BOp(Nstop, e);
      saveregs();
#if PUSH_REGS
      restore_absmi_regs(old_regs);
#endif
#if BP_FREE
      P1REG = PCBACKUP;
#endif
      return (0);
      ENDBOp();

/*****************************************************************
*        Plain try - retry - trust instructions                  *
*****************************************************************/
      /* try_me    Label,NArgs */
      Op(try_me, ld);
      /* check if enough space between trail and codespace */
      check_trail();
      /* I use Y to go through the choicepoint. Usually Y is in a
       * register, but sometimes (X86) not. In this case, have a
       * new register to point at Y */
      CACHE_Y(Y);
      /* store arguments for procedure */
      store_at_least_one_arg(PREG->u.ld.s);
      /* store abstract machine registers */
      store_yaam_regs(PREG->u.ld.d, 0);
      /* On a try_me, set cut to point at previous choicepoint,
       * that is, to the B before the cut.
       */
      set_cut(S_Y, B);
      /* now, install the new Y */
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* retry_me    Label,NArgs */
      Op(retry_me, ld);
      CACHE_Y(B);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      restore_yaam_regs(PREG->u.ld.d);
      restore_at_least_one_arg(PREG->u.ld.s);
#if defined(SBA) && defined(FROZEN_REGS)
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* trust_me    UnusedLabel,NArgs */
      Op(trust_me, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	pop_at_least_one_arg(PREG->u.ld.s);
	/* After trust, cut should be pointing at the new top
	 * choicepoint */
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      PREG = NEXTOP(PREG, ld);
      SET_BB(B_Y);
      ENDCACHE_Y();
      GONext();
      ENDOp();

/*****************************************************************
*        Profiled try - retry - trust instructions               *
*****************************************************************/

      /* profiled_enter_me    Label,NArgs */
      Op(enter_profiling, l);
      LOCK(((PredEntry *)(PREG->u.l.l))->StatisticsForPred.lock);
      ((PredEntry *)(PREG->u.l.l))->StatisticsForPred.NOfEntries++;
      UNLOCK(((PredEntry *)(PREG->u.l.l))->StatisticsForPred.lock);
      PREG = NEXTOP(PREG, l);
      GONext();
      ENDOp();

      /* profiled_enter_me    Label,NArgs */
      Op(retry_profiled, l);
      LOCK(((PredEntry *)(PREG->u.l.l))->StatisticsForPred.lock);
      ((PredEntry *)(PREG->u.l.l))->StatisticsForPred.NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->u.l.l))->StatisticsForPred.lock);
      PREG = NEXTOP(PREG, l);
      GONext();
      ENDOp();

      /* profiled_retry_me    Label,NArgs */
      Op(profiled_retry_me, ld);
      CACHE_Y(B);
      /* After retry, cut should be pointing at the parent
       * choicepoint for the current B */
      LOCK(((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.lock);
      ((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.lock);
      restore_yaam_regs(PREG->u.ld.d);
      restore_args(PREG->u.ld.s);
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* profiled_trust_me    UnusedLabel,NArgs */
      Op(profiled_trust_me, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	restore_args(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	pop_args(PREG->u.ld.s);
	/* After trust, cut should be pointing at the new top
	 * choicepoint */
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      LOCK(((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.lock);
      ((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.lock);
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

/*****************************************************************
*        Specialised try - retry - trust instructions            *
*****************************************************************/
      /* try_me0    Label,NArgs */
      Op(try_me0, ld);
      check_trail();
      CACHE_Y(Y);
      store_yaam_regs(PREG->u.ld.d, 0);
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* retry_me0    Label,NArgs */
      Op(retry_me0, ld);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ld.d);
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* trust_me0    UnusedLabel,NArgs */
      Op(trust_me0, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	S_Y = (CELL *)(B_Y+1);
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* try_me1    Label,NArgs */
      Op(try_me1, ld);
      check_trail();
      CACHE_Y(Y);
      {
	register CELL x1 = CACHED_A1();

	store_yaam_regs(PREG->u.ld.d, 1);
	B_Y->cp_a1 = x1;
      }
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* retry_me1    Label,NArgs */
      Op(retry_me1, ld);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ld.d);
      ARG1 = B_Y->cp_a1;
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* trust_me1    UnusedLabel,NArgs */
      Op(trust_me1, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	ARG1 = B_Y->cp_a1;
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	ARG1 = B_Y->cp_a1;
	S_Y = &(B_Y->cp_a2);
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* try_me2    Label,NArgs */
      Op(try_me2, ld);
      check_trail();
      CACHE_Y(Y);
#ifdef HAVE_FEW_REGS
      store_yaam_regs(PREG->u.ld.d, 2);
      B_Y->cp_a1 = CACHED_A1();
      B_Y->cp_a2 = ARG2;
#else
      {
	register CELL x2 = ARG2;
	register CELL x1 = CACHED_A1();

	store_yaam_regs(PREG->u.ld.d, 2);
	B_Y->cp_a1 = x1;
	B_Y->cp_a2 = x2;
      }
#endif
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* retry_me2    Label,NArgs */
      Op(retry_me2, ld);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ld.d);
      ARG1 = B_Y->cp_a1;
      ARG2 = B_Y->cp_a2;
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* trust_me2    UnusedLabel,NArgs */
      Op(trust_me2, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	ARG1 = B_Y->cp_a1;
	ARG2 = B_Y->cp_a2;
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	ARG1 = B_Y->cp_a1;
	ARG2 = B_Y->cp_a2;
	S_Y = &(B_Y->cp_a3);
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* try_me3    Label,NArgs */
      Op(try_me3, ld);
      check_trail();
      CACHE_Y(Y);
#ifdef HAVE_FEW_REGS
      store_yaam_regs(PREG->u.ld.d, 3);
      B_Y->cp_a1 = CACHED_A1();
      B_Y->cp_a2 = ARG2;
      B_Y->cp_a3 = ARG3;
#else
      {
	register CELL x1 = CACHED_A1();
	register CELL x2 = ARG2;
	register CELL x3 = ARG3;

	store_yaam_regs(PREG->u.ld.d, 3);
	B_Y->cp_a1 = x1;
	B_Y->cp_a2 = x2;
	B_Y->cp_a3 = x3;
      }
#endif
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* retry_me3    Label,NArgs */
      Op(retry_me3, ld);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ld.d);
      ARG1 = B_Y->cp_a1;
      ARG2 = B_Y->cp_a2;
      ARG3 = B_Y->cp_a3;
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* trust_me3    UnusedLabel,NArgs */
      Op(trust_me3, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	ARG1 = B_Y->cp_a1;
	ARG2 = B_Y->cp_a2;
	ARG3 = B_Y->cp_a3;
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	ARG1 = B_Y->cp_a1;
	ARG2 = B_Y->cp_a2;
	ARG3 = B_Y->cp_a3;
	S_Y = &(B_Y->cp_a4);
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* try_me4    Label,NArgs */
      Op(try_me4, ld);
      check_trail();
      CACHE_Y(Y);
      store_yaam_regs(PREG->u.ld.d, 4);
#ifdef HAVE_FEW_REGS
      B_Y->cp_a1 = CACHED_A1();
      B_Y->cp_a2 = ARG2;
      B_Y->cp_a3 = ARG3;
      B_Y->cp_a4 = ARG4;
#else
      {
	register CELL x1 = CACHED_A1();
	register CELL x2 = ARG2;
	register CELL x3 = ARG3;
	register CELL x4 = ARG4;

	B_Y->cp_a1 = x1;
	B_Y->cp_a2 = x2;
	B_Y->cp_a3 = x3;
	B_Y->cp_a4 = x4;
      }
#endif
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* retry_me4    Label,NArgs */
      Op(retry_me4, ld);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ld.d);
      ARG1 = B_Y->cp_a1;
      ARG2 = B_Y->cp_a2;
      ARG3 = B_Y->cp_a3;
      ARG4 = B_Y->cp_a4;
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

      /* trust_me4    UnusedLabel,NArgs */
      Op(trust_me4, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	ARG1 = B_Y->cp_a1;
	ARG2 = B_Y->cp_a2;
	ARG3 = B_Y->cp_a3;
	ARG4 = B_Y->cp_a4;
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	ARG1 = B_Y->cp_a1;
	ARG2 = B_Y->cp_a2;
	ARG3 = B_Y->cp_a3;
	ARG4 = B_Y->cp_a4;
	S_Y = &(B_Y->cp_a5);
#ifdef FROZEN_REGS
	S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = NEXTOP(PREG, ld);
      GONext();
      ENDOp();

/*****************************************************************
*        enter a logical semantics dynamic predicate             *
*****************************************************************/

      /* enter logical pred               */
      BOp(try_logical_pred, l);
      /* mark the indexing code */
      {
	Clause *cl = (Clause *)PREG->u.l.l;
	PREG = NEXTOP(PREG, l);
	LOCK(cl->ClLock);
	/* indicate the indexing code is being used */
#if defined(YAPOR) || defined(THREADS)
	/* just store a reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
#else
	if (!(cl->ClFlags & InUseMask)) {
	  cl->ClFlags |= InUseMask;
	  TRAIL_CLREF(cl);
	  cl->u2.ClUse =  TR-(tr_fr_ptr)(TrailBase);
	}
#endif
	UNLOCK(cl->ClLock);
      }
      GONext();
      ENDBOp();

      /* trust a logical pred, that is, release the code     */
      BOp(trust_logical_pred, l);
      /* unmark the indexing code */
      /* mark the indexing code */
      {
	Clause *cl = (Clause *)PREG->u.l.l;
	PREG = NEXTOP(PREG, l);
	/* check if we are the ones using this code */
#if defined(YAPOR) || defined(THREADS)
	LOCK(cl->ClLock);
	DEC_CLREF_COUNT(cl);
	/* clear the entry from the trail */
	TR = --(B->cp_tr);
	/* actually get rid of the code */
	if (!CL_IN_USE(cl) && cl->ClFlags & ErasedMask) {
	  UNLOCK(cl->ClLock);
	  /* I am the last one using this clause, hence I don't need a lock
	     to dispose of it 
	  */
	  ErCl(cl);
	} else {
	  UNLOCK(cl->ClLock);
	}
#else
	if (cl->u2.ClUse == TR-(tr_fr_ptr)(TrailBase)) {
	  cl->u2.ClUse = 0;
	  cl->ClFlags &= ~InUseMask;
	  /* clear the entry from the trail */
	  TR = --(B->cp_tr);
	  /* next, recover space for the indexing code if it was erased */
	  if (cl->ClFlags & ErasedMask) {
	    ErCl(cl);
	  }
	}
#endif
      }
      GONext();
      ENDBOp();

      /* enter logical pred               */
      BOp(alloc_for_logical_pred, EC);
      /* say that an environment is using this clause */
      /* we have our own copy for the clause */
#if defined(YAPOR) || defined(THREADS)
      {
	Clause *cl = (Clause *)PREG->u.EC.ClBase;

	LOCK(cl->ClLock);
	/* always add an extra reference */
	INC_CLREF_COUNT(cl);
	TRAIL_CLREF(cl);
	UNLOCK(cl->ClLock);
      }
#else
      if (PREG->u.EC.ClENV == 0) {
	Clause *cl = (Clause *)PREG->u.EC.ClBase;

	/* marking a new clause */
	PREG->u.EC.ClTrail = TR-(tr_fr_ptr)TrailBase;
	PREG->u.EC.ClENV = LCL0-YENV;
	cl->ClFlags |= InUseMask;
	TRAIL_CLREF(cl);
      }
#endif
      PREG = NEXTOP(PREG, EC);
      GONext();
      ENDBOp();

      /* exit logical pred               */
      BOp(dealloc_for_logical_pred, l);
      {
	yamop *ecl = (yamop *)PREG->u.l.l;
	PREG = NEXTOP(PREG, l);
	/* check first if environment is protected */
	BEGP(pt0);
	pt0 = (CELL *) YENV;
#ifdef FROZEN_REGS
	{ 
	  choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef SBA
	  if (pt0 > (CELL *) top_b || pt0 < H) {
	    GONext();
	  }
#else
	  if (pt0 > (CELL *) top_b) {
	    GONext();
	  }
#endif
	}
#else
	if (pt0 > (CELL *)B) {
	  GONext();
	}
#endif /* FROZEN_REGS */
	ENDP(pt0);
#if defined(YAPOR) || defined(THREADS)
	{
	  Clause *cl = (Clause *)(ecl->u.EC.ClBase);
	  Term tc = AbsPair(((CELL *)&(cl->ClFlags)));

	  /* Question: how do we find the trail cell we want to reset? */
	  /* Quick hack: search for it */
	  tr_fr_ptr trp = (((choiceptr) YENV[E_CB])->cp_tr);
	  while (TrailTerm(trp) != tc) {
	    trp++;
	  }
	  /* the correct solution would be to store this in the environment */
	  RESET_VARIABLE(&TrailTerm(trp));
	  LOCK(cl->ClLock);
	  DEC_CLREF_COUNT(cl);
	  UNLOCK(cl->ClLock);
	}
#else
	if (ecl->u.EC.ClENV == LCL0-YENV) {
	  Clause *cl = (Clause *)(ecl->u.EC.ClBase);
	  /* if the environment is protected we can't do nothing */
	  /* unmark the clause */
	  cl->ClFlags &= ~InUseMask;
	  ecl->u.EC.ClENV = 0;
	  RESET_VARIABLE((CELL *)((tr_fr_ptr)TrailBase+ecl->u.EC.ClTrail));
	  if ((cl->ClFlags & ErasedMask) && (ecl->u.EC.ClRefs == 0)) {
	    ErCl(cl);
	  }
	}
#endif
      }
      GONext();
      ENDBOp();

/*****************************************************************
*        try and retry of dynamic predicates                     *
*****************************************************************/

      /* spy_or_trymark                   */
      BOp(spy_or_trymark, ld);
      if (FlipFlop ^= 1) {
	READ_LOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
	PREG = (yamop *)(&(((PredEntry *)(PREG->u.ld.p))->OpcodeOfPred));
	READ_UNLOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
	goto dospy;
      }
      ENDBOp();

      /* try_and_mark   Label,NArgs       */
      BOp(try_and_mark, ld);
      check_trail();
#if defined(YAPOR) || defined(THREADS)
#ifdef YAPOR
      /* The flags I check here should never change during execution */
      CUT_wait_leftmost();
#endif /* YAPOR */
      READ_LOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
      if (((PredEntry *)(PREG->u.ld.p))->CodeOfPred !=
	  (CODEADDR)PREG) {
	/* oops, someone changed the procedure under our feet,
	   fortunately this is no big deal because we haven't done
	   anything yet */
	READ_UNLOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
	PREG = (yamop *)(((PredEntry *)(PREG->u.ld.p))->CodeOfPred);
	JMPNext();
      }
#endif
      CACHE_Y(Y);
      PREG = (yamop *) (PREG->u.ld.d);
      /*
	I've got a read lock on the DB, so I don't need to care...
	 niaaahh.... niahhhh...
      */
      LOCK(DynamicLock(PREG));
      /* one can now mess around with the predicate */
      READ_UNLOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
      BEGD(d1);
      d1 = PREG->u.ld.s;
      store_args(d1);
      store_yaam_regs(PREG, 0);
      ENDD(d1);
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
#if defined(YAPOR) || defined(THREADS)
      INC_CLREF_COUNT(ClauseCodeToClause(PREG));
      UNLOCK(DynamicLock(PREG));
      TRAIL_CLREF(ClauseCodeToClause(PREG));
#else
      if (FlagOff(InUseMask, DynamicFlags(PREG))) {

	SetFlag(InUseMask, DynamicFlags(PREG));
	TRAIL_CLREF(ClauseCodeToClause(PREG));
      }
#endif
      PREG = NEXTOP(PREG,ld);
      JMPNext();

      ENDBOp();

      BOp(profiled_retry_and_mark, ld);
      LOCK(((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.lock);
      ((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.NOfRetries++;
      UNLOCK(((PredEntry *)(PREG->u.ld.p))->StatisticsForPred.lock);
      /* enter a retry dynamic */
      ENDBOp();

      /* retry_and_mark   Label,NArgs     */
      BOp(retry_and_mark, ld);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      /* need to make the DB stable until I get the new clause */
      READ_LOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
      CACHE_Y(B);
      PREG = (yamop *) (PREG->u.ld.d);
      LOCK(DynamicLock(PREG));
      READ_UNLOCK(((PredEntry *)(PREG->u.ld.p))->PRWLock);
      restore_yaam_regs(PREG);
      restore_args(PREG->u.ld.s);
#ifdef FROZEN_REGS
      S_Y = (CELL *)PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
#if defined(YAPOR) || defined(THREADS)
      INC_CLREF_COUNT(ClauseCodeToClause(PREG));
      TRAIL_CLREF(ClauseCodeToClause(PREG));
      UNLOCK(DynamicLock(PREG));
#else
      if (FlagOff(InUseMask, DynamicFlags(PREG))) {

	SetFlag(InUseMask, DynamicFlags(PREG));
	TRAIL_CLREF(ClauseCodeToClause(PREG));
      }
#endif
      PREG = NEXTOP(PREG,ld);
      JMPNext();

      ENDBOp();

/*****************************************************************
*        Failure                                                 *
*****************************************************************/

      /* trust_fail                       */
      BOp(trust_fail, e);
      B = B->cp_b;
      goto fail;
      ENDBOp();

#ifdef YAPOR
    shared_fail:
      B = LOCAL_top_cp;
      SET_BB(PROTECT_FROZEN_B(B));
      goto fail;
#endif	/* YAPOR */

      /* fail                             */
      PBOp(op_fail, e);
#ifndef NO_CHECKING
      /* the fail builtin may end a loop of C builtins,
	 and some of these built-ins may grow the heap. */
#ifdef YAPOR
      if (HeapTop > GlobalBase - MinHeapGap) 
	goto noheapleft_in_fail;
#else
      if (HeapTop > Addr(AuxSp) - MinHeapGap)
	goto noheapleft_in_fail;
#endif /* YAPOR */
#endif 

    fail:
      {
	register tr_fr_ptr pt0 = TR;
	PREG = B->cp_ap;
	CACHE_TR(B->cp_tr);
	RESTORE_TR();
	PREFETCH_OP(PREG);
      failloop:
	if (pt0 == S_TR) {
#ifdef FROZEN_REGS  /* TRAIL */
#ifdef SBA
	  if (pt0 < TR_FZ || pt0 > (tr_fr_ptr)TrailTop)
#else
	  if (pt0 < TR_FZ)
#endif /* SBA */
	    {
	      TR = TR_FZ;
	      TRAIL_LINK(pt0);
	    }
#endif /* FROZEN_REGS */
	  SP = SP0;
#ifdef LOW_LEVEL_TRACER
	  if (do_low_level_trace) {
	    op_numbers opnum = op_from_opcode(PREG->opc);
	  restart_cp:
	    switch (opnum) {
#ifdef TABLING
	    case _table_answer_resolution:
	      {
		PredEntry *pe = ENV_ToP(B->cp_cp);
		op_numbers caller_op = op_from_opcode(ENV_ToOp(B->cp_cp));
		/* first condition  checks if this was a meta-call */
		if ((caller_op != _call && caller_op != _fcall) || pe == NULL) {
		  low_level_trace(retry_table_consumer, NULL, NULL);
		} else {
		  low_level_trace(retry_table_consumer, pe, NULL);
		}
	      }
	    case _table_completion:
	      {
		PredEntry *pe = ENV_ToP(B->cp_cp);
		op_numbers caller_op = op_from_opcode(ENV_ToOp(B->cp_cp));
		/* first condition  checks if this was a meta-call */
		if ((caller_op != _call && caller_op != _fcall) || pe == NULL) {
		  low_level_trace(retry_table_producer, NULL, NULL);
		} else {
		  low_level_trace(retry_table_producer, pe, (CELL *)(((gen_cp_ptr)B)+1));
		}
	      }
	      break;
	    case _trie_retry_var:
	    case _trie_retry_val:
	    case _trie_retry_atom:
	    case _trie_retry_list:
	    case _trie_retry_struct:
	    case _trie_trust_var:
	    case _trie_trust_val:
	    case _trie_trust_atom:
	    case _trie_trust_list:
	    case _trie_trust_struct:
	      low_level_trace(retry_table_consumer, NULL, NULL);
	      break;
	    case _table_retry_me:
	    case _table_trust_me:
	      low_level_trace(retry_pred, (PredEntry *)(PREG->u.lds.p), (CELL *)(((gen_cp_ptr)B)+1));
	      break;
#endif
	    case _or_else:
	    case _or_last:
	      low_level_trace(retry_or, (PredEntry *)PREG, &(B->cp_a1));
	      break;
	    case _trust_logical_pred:
	      low_level_trace(retry_pred, (PredEntry *)(NEXTOP(PREG,l)->u.ld.p), &(B->cp_a1));
	      break;
	    case _switch_last:
	    case _switch_l_list:
	      low_level_trace(retry_pred, (PredEntry *)(PREG->u.slll.p), &(B->cp_a1));
	      break;
	    case _retry_c:
	    case _retry_userc:
	      low_level_trace(retry_pred, (PredEntry *)(PREG->u.lds.p), &(B->cp_a1));
	      break;
	    case _retry_profiled:
	      opnum = op_from_opcode(NEXTOP(B->cp_ap,l)->opc);
	      goto restart_cp;
	    default:
	      low_level_trace(retry_pred, (PredEntry *)(PREG->u.ld.p), &(B->cp_a1));
	    }
	  }
#endif	/* LOW_LEVEL_TRACER */
	  GONext();
	}
	BEGD(d1);
	d1 = TrailTerm(pt0-1);
	pt0--;
	if (IsVarTerm(d1)) {
#if defined(SBA) && defined(YAPOR)
	  /* clean up the trail when we backtrack */
	  if (Unsigned((Int)(d1)-(Int)(H_FZ)) >
	      Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	    RESET_VARIABLE(STACK_TO_SBA(d1));
	  } else
#endif
	    /* normal variable */
	    RESET_VARIABLE(d1);
	  goto failloop;
	}
	/* pointer to code space */
	/* or updatable variable */
#if defined(TERM_EXTENSIONS) || defined(FROZEN_REGS) || defined(MULTI_ASSIGNMENT_VARIABLES)
	if (IsPairTerm(d1))
#endif
	  {
	    register CELL flags;

	    d1 = (CELL) RepPair(d1);
#ifdef FROZEN_REGS  /* TRAIL */
            /* avoid frozen segments */
#ifdef SBA
	    if ((ADDR) d1 >= HeapTop)
#else
	    if ((ADDR) d1 >= TrailBase)
#endif
	      {
		pt0 = (tr_fr_ptr) d1;
		goto failloop;
	      }
#endif /* FROZEN_REGS */
	    flags = Flags(d1);
#if defined(YAPOR) || defined(THREADS)
	    if (!FlagOn(DBClMask, flags)) {
	      Clause *cl = ClauseFlagsToClause(d1);
	      int erase;
	      LOCK(cl->ClLock);
	      DEC_CLREF_COUNT(cl);
	      erase = (cl->ClFlags & ErasedMask) && (cl->ref_count == 0);
	      UNLOCK(cl->ClLock);
	      if (erase) {
		saveregs();
		/* at this point, 
		   we are the only ones accessing the clause,
		   hence we don't need to have a lock it */
		ErCl(cl);
		setregs();
	      }
	    } else {
	      DBRef dbr = DBStructFlagsToDBStruct(d1);
	      int erase;

	      LOCK(dbr->lock);
	      DEC_DBREF_COUNT(dbr);
	      erase = (dbr->Flags & ErasedMask) && (dbr->ref_count == 0);
	      UNLOCK(dbr->lock);
	      if (erase) {
		saveregs();
		ErDBE(dbr);
		setregs();
	      }
	    }
#else
	    ResetFlag(InUseMask, flags);
	    Flags(d1) = flags;
	    /* vsc???	    if (FlagOn(StaticMask, flags)) {
	       if (FlagOff(SpiedMask, flags)) {
	       PredCode(d1) = TruePredCode(d1);
	         }
	       }
	       else
	    */
	    if (FlagOn(ErasedMask, flags)) {
	      if (FlagOn(DBClMask, flags)) {
		saveregs();
		ErDBE(DBStructFlagsToDBStruct(d1));
		setregs();
	      } else {
		saveregs();
		ErCl(ClauseFlagsToClause(d1));
		setregs();
	      }
	    }
#endif
	    goto failloop;
	  }
#ifdef MULTI_ASSIGNMENT_VARIABLES
	else /* if (IsApplTerm(d1)) */ {
	  CELL *pt = RepAppl(d1);
	  /* AbsAppl means */
	  /* multi-assignment variable */
	  /* so the next cell is the old value */ 
	  pt0--;
#if FROZEN_STACKS
	  pt[0] = TrailVal(pt0);
#else
	  pt[0] = TrailTerm(pt0);
#endif
	  goto failloop;
	}
#endif
	ENDD(d1);
	ENDCACHE_TR();
      }
      ENDPBOp();

    noheapleft_in_fail:
      ASP = (CELL *)B;
      goto noheapleft;



/************************************************************************\
*	Cut & Commit Instructions					*
\************************************************************************/

      /* cut                              */
      Op(cut, e);
      PREG = NEXTOP(PREG, e);
      BEGD(d0);
      /* assume cut is always in stack */
      d0 = Y[E_CB];
      if (SHOULD_CUT_UP_TO(B,(choiceptr) d0)) {
	/* cut ! */
#ifdef YAPOR
	CUT_prune_to((choiceptr) d0);
#else
	B = (choiceptr) d0;
#endif	/* YAPOR */
	SET_BB(PROTECT_FROZEN_B(B));
	HBREG = PROTECT_FROZEN_H(B);
	TR = trim_trail(B, TR, HBREG);
      }
      ENDD(d0);
      GONext();
      ENDOp();

      /* cut_t                            */
      /* cut_t does the same as cut */
      Op(cut_t, e);
      PREG = NEXTOP(PREG, e);
      BEGD(d0);
      /* assume cut is always in stack */
      d0 = Y[E_CB];
      if (SHOULD_CUT_UP_TO(B,(choiceptr) d0)) {
	/* cut ! */
#ifdef YAPOR
	CUT_prune_to((choiceptr) d0);
#else
	B = (choiceptr) d0;
#endif	/* YAPOR */
	SET_BB(PROTECT_FROZEN_B(B));
	HBREG = PROTECT_FROZEN_H(B);
	TR = trim_trail(B, TR, HBREG);
      }
      ENDD(d0);
      GONext();
      ENDOp();

      /* cut_e                            */
      Op(cut_e, e);
      PREG = NEXTOP(PREG, e);
      BEGD(d0);
      /* we assume dealloc leaves in S the previous env             */
      d0 = SREG[E_CB];
      if (SHOULD_CUT_UP_TO(B,(choiceptr)d0)) {
	/* cut ! */
#ifdef YAPOR
	CUT_prune_to((choiceptr) d0);
#else
	B = (choiceptr) d0;
#endif	/* YAPOR */
	SET_BB(PROTECT_FROZEN_B(B));
	HBREG = PROTECT_FROZEN_H(B);
	TR = trim_trail(B, TR, HBREG);
      }
      ENDD(d0);
      GONext();
      ENDOp();

      /* save_b_x      Xi                 */
      Op(save_b_x, x);
      BEGD(d0);
      d0 = PREG->u.x.x;
#if defined(SBA) && defined(FROZEN_REGS)
      XREG(d0) = MkIntegerTerm((Int)B);
#else
      XREG(d0) = MkIntTerm(LCL0-(CELL *) (B));
#endif
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      GONext();
      ENDOp();

      /* save_b_y      Yi                 */
      Op(save_b_y, y);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.y.y,MkIntegerTerm((Int)B));
#else
      Y[PREG->u.y.y] = MkIntTerm(LCL0-(CELL *) (B));
#endif
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDOp();

      /* comit_b_x    Xi                 */
      Op(comit_b_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
#ifdef COROUTINING
      CACHE_Y_AS_ENV(Y);
      check_stack(NoStackComitX, H);
      ENDCACHE_Y_AS_ENV();
    do_comit_b_x:
#endif
      /* skip a void call and a label */
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, x),sla),l);
      {
	choiceptr pt0;
#if defined(SBA) && defined(FROZEN_REGS)
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
	pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
	if (SHOULD_CUT_UP_TO(B,pt0)) {
#ifdef YAPOR
	  CUT_prune_to(pt0);
#else
	  B = pt0;
#endif	/* YAPOR */
	  SET_BB(PROTECT_FROZEN_B(B));
	  HBREG = PROTECT_FROZEN_H(pt0);
	  TR = trim_trail(B, TR, HBREG);
	}
      }
      ENDD(d0);
      GONext();
      ENDOp();

      /* comit_b_y    Yi                 */
      Op(comit_b_y, y);
      BEGD(d0);
      d0 = Y[PREG->u.y.y];
#ifdef COROUTINING
      CACHE_Y_AS_ENV(Y);
      check_stack(NoStackComitY, H);
      ENDCACHE_Y_AS_ENV();
    do_comit_b_y:
#endif
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, y),sla),l);
      {
	choiceptr pt0;
#if defined(SBA) && defined(FROZEN_REGS)
	pt0 = (choiceptr)IntegerOfTerm(d0);
#else
	pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
	if (SHOULD_CUT_UP_TO(B,pt0)) {
#ifdef YAPOR
	  CUT_prune_to(pt0);
#else
	  B = pt0;
#endif	/* YAPOR */
	  SET_BB(PROTECT_FROZEN_B(B));
	  HBREG = PROTECT_FROZEN_H(pt0);
	  TR = trim_trail(B, TR, HBREG);
	}
      }
      ENDD(d0);
      GONext();
      ENDOp();

/*************************************************************************
* 	Call / Proceed instructions                                      *
*************************************************************************/

/* Macros for stack trimming                                            */

      /* execute     Label               */
      BOp(execute, l);
      BEGP(pt0);
      CACHE_Y_AS_ENV(Y);
      pt0 = (CELL *) (PREG->u.l.l);
      CACHE_A1();
      ALWAYS_LOOKAHEAD(PredOpCode(pt0));
      BEGD(d0);
      d0 = (CELL)B;
#ifndef NO_CHECKING
      check_stack(NoStackExecute, H);
#endif
      PREG = (yamop *) PredCode(pt0);
      E_Y[E_CB] = d0;
      ENDD(d0);
#ifdef DEPTH_LIMIT
      if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is prolog */
	if (Module(pt0)) {
	  if (DEPTH == MkIntTerm(0))
	    FAIL();
	  else DEPTH = RESET_DEPTH();
	}
      } else if (Module(pt0))
	DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,pred_entry(pt0),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      /* this is the equivalent to setting up the stack */
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDCACHE_Y_AS_ENV();
      ENDP(pt0);
      ENDBOp();

    NoStackExecute:
      SREG = (CELL *) (PREG->u.l.l);
#ifdef YAPOR
      /* abort_optyap("NoStackExecute in function absmi"); */
      if (HeapTop > GlobalBase - MinHeapGap)
#else	/* YAPOR */
      if (HeapTop > Addr(AuxSp) - MinHeapGap)
#endif
	{
	  ASP = Y+E_CB;
	  if (ASP > (CELL *)B)
	    ASP = (CELL *)B;
	  goto noheapleft;
	}
      if (CFREG != CalculateStackGap())
	goto creep;
      else
	goto NoStackExec;

      /* dexecute    Label               */
/* joint deallocate and execute */
      BOp(dexecute, l);
      CACHE_Y_AS_ENV(Y);
      BEGP(pt0);
      CACHE_A1();
      pt0 = (CELL *) (PREG->u.l.l);
#ifndef NO_CHECKING
      /* check stacks */
      check_stack(NoStackDExecute, H);
#endif
#ifdef DEPTH_LIMIT
      if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
	if (Module(pt0)) {
	  if (DEPTH == MkIntTerm(0))
	    FAIL();
	  else DEPTH = RESET_DEPTH();
	}
      } else if (Module(pt0))
	DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,pred_entry(pt0),XREGS+1);
#endif	/* LOW_LEVEL_TRACER */
      PREG = (yamop *) PredCode(pt0);
      ALWAYS_LOOKAHEAD(PredOpCode(pt0));
      /* do deallocate */
      CPREG = (yamop *) E_Y[E_CP];
      E_Y = ENV = (CELL *) E_Y[E_E];
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);

#ifdef SBA
	if (E_Y > (CELL *) top_b || E_Y < H) E_Y = (CELL *) top_b;
#else
	if (E_Y > (CELL *) top_b) E_Y = (CELL *) top_b;
#endif
	else E_Y = (CELL *)((CELL)E_Y + ENV_Size(CPREG));
      }
#else
      if (E_Y > (CELL *)B) {
	E_Y = (CELL *)B;
      }
      else {
	E_Y = (CELL *) ((CELL) E_Y + ENV_Size(CPREG));
      }
#endif /* FROZEN_REGS */
      WRITEBACK_Y_AS_ENV();
      /* setup GB */
      E_Y[E_CB] = (CELL) B;
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDP(pt0);
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(fcall, sla);
      CACHE_Y_AS_ENV(Y);
      E_Y[E_CP] = (CELL) CPREG;
      E_Y[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      E_Y[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

      BOp(call, sla);
      CACHE_Y_AS_ENV(Y);
      BEGP(pt0);
      pt0 = (CELL *) (PREG->u.sla.l);
      CACHE_A1();
#ifndef NO_CHECKING
      check_stack(NoStackCall, H);
#endif
      ENV = E_Y;
      /* Try to preserve the environment */
      E_Y = (CELL *) (((char *) Y) + PREG->u.sla.s);
      CPREG =
	(yamop *) NEXTOP(PREG, sla);
      ALWAYS_LOOKAHEAD(PredOpCode(pt0));
      PREG = (yamop *) PredCode(pt0);
#ifdef DEPTH_LIMIT
      if (DEPTH <= MkIntTerm(1)) {/* I assume Module==0 is primitives */
	if (Module(pt0)) {
	  if (DEPTH == MkIntTerm(0))
	    FAIL();
	  else DEPTH = RESET_DEPTH();
	}
      } else if (Module(pt0))
	DEPTH -= MkIntConstant(2);
#endif	/* DEPTH_LIMIT */
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,pred_entry(pt0),XREGS+1);
#endif	/* LOW_LEVEL_TRACER */
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (E_Y > (CELL *) top_b || E_Y < H) E_Y = (CELL *) top_b;
#else
	if (E_Y > (CELL *) top_b) E_Y = (CELL *) top_b;
#endif
      }
#else
      if (E_Y > (CELL *) B) {
	E_Y = (CELL *) B;
      }
#endif /* FROZEN_REGS */
      WRITEBACK_Y_AS_ENV();
      /* setup GB */
      E_Y[E_CB] = (CELL) B;
#ifdef YAPOR
      SCH_check_requests();
#endif	/* YAPOR */
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDP(pt0);
      ENDCACHE_Y_AS_ENV();
      ENDBOp();

    NoStackCall:
      /* on X86 machines S will not actually be holding the pointer to pred */
      SREG = (CELL *) (PREG->u.sla.l);
#ifdef YAPOR
      /* abort_optyap("NoStackCall in function absmi"); */
      if (HeapTop > GlobalBase - MinHeapGap)
#else
      if (HeapTop > Addr(AuxSp) - MinHeapGap)
#endif /* YAPOR */
	{
	  ASP = (CELL *) (((char *) Y) + PREG->u.sla.s);
	  if (ASP > (CELL *)B)
	    ASP = (CELL *)B;
	  goto noheapleft;
	}
#ifdef COROUTINING
      if (CFREG == Unsigned(LCL0)) {
	if (ReadTimedVar(WokenGoals) != TermNil)
	  goto creepc;
	else {
	  CFREG = CalculateStackGap();
	  JMPNext();
	}
      }
#endif
      if (CFREG != CalculateStackGap())
	goto creepc;
      ASP = (CELL *) (((char *) Y) + PREG->u.sla.s);
      if (ASP > (CELL *)B)
	ASP = (CELL *)B;
      saveregs();
      gc(PredArity(SREG), Y, NEXTOP(PREG, sla));
      setregs();

      JMPNext();


#ifdef COROUTINING

     /* This is easier: I know there is an environment so I cannot do allocate */
    NoStackComitY:
      /* find something to fool S */
      if (CFREG == Unsigned(LCL0) && ReadTimedVar(WokenGoals) != TermNil) {
	SREG = (CELL *)&(RepPredProp(GetPredProp(AtomRestoreRegs,2))->StateOfPred);
	PREG = NEXTOP(PREG,x);
	XREGS[0] = XREG(PREG->u.y.y);
	goto creep_either;
      }
      /* don't do debugging and friends here */
      goto do_comit_b_y;

      /* Problem: have I got an environment or not? */
    NoStackComitX:
      /* find something to fool S */
      if (CFREG == Unsigned(LCL0) && ReadTimedVar(WokenGoals) != TermNil) {
	SREG = (CELL *)&(RepPredProp(GetPredProp(AtomRestoreRegs,2))->StateOfPred);
	PREG = NEXTOP(PREG,x);
#if USE_THREADED_CODE
	if (PREG->opc == (OPCODE)OpAddress[_fcall])
#else
	  if (PREG->opc == _fcall)
#endif
	    {
	      /* fill it up */
	      CACHE_Y_AS_ENV(Y);
	      E_Y[E_CP] = (CELL) CPREG;
	      E_Y[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
	      E_Y[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
	      ENDCACHE_Y_AS_ENV();
	    }
	XREGS[0] = XREG(PREG->u.x.x);
	goto creep_either;
      }
      /* don't do debugging and friends here */
      goto do_comit_b_x;

      /* don't forget I cannot creep at ; */
    NoStackEither:
      /* find something to fool S */
      SREG = (CELL *)&(RepPredProp(GetPredProp(AtomRestoreRegs,1))->StateOfPred);
#ifdef YAPOR
      /* abort_optyap("NoStackCall in function absmi"); */
      if (HeapTop > GlobalBase - MinHeapGap)
#else
      if (HeapTop > Addr(AuxSp) - MinHeapGap)
#endif /* YAPOR */
	{
	  ASP = (CELL *) (((char *) Y) + PREG->u.sla.s);
	  if (ASP > (CELL *)B)
	    ASP = (CELL *)B;
	  goto noheapleft;
	}
      if (CFREG == Unsigned(LCL0)) {
	if (ReadTimedVar(WokenGoals) != TermNil)
	  goto creep_either;
	else {
	  CFREG = CalculateStackGap();
	  JMPNext();
	}
      }
      if (CFREG != CalculateStackGap()) {
	goto either_notest;
      }
      ASP = (CELL *) (((char *) Y) + PREG->u.sla.s);
      if (ASP > (CELL *)B)
	ASP = (CELL *)B;
      saveregs();
      gc(0, Y, NEXTOP(PREG, sla));
      setregs();
      JMPNext();

    creep_either:			/* do creep in either      */
      ENV = Y;
      CPREG = NEXTOP(PREG, sla);
      Y = (CELL *) (((char *) Y) + PREG->u.sla.s);
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) Y = (CELL *) top_b;
#else
	if (Y > (CELL *) top_b) Y = (CELL *) top_b;
#endif
	else Y = Y + ENV_Size(CPREG);
      }
#else
      if (Y > (CELL *) B)
	Y = (CELL *) B;
#endif /* FROZEN_REGS */
      /* setup GB */
      ARG1 = push_live_regs(CPREG);
      /* ARG0 has an extra argument for suspended cuts */
      ARG2 = XREGS[0];
      Y[E_CB] = (CELL) B;
      goto creep;
#endif

    creepc:			/* do creep in call                                     */
      ENV = Y;
      CPREG = NEXTOP(PREG, sla);
      Y = (CELL *) (((char *) Y) + PREG->u.sla.s);
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) Y = (CELL *) top_b;
#else
	if (Y > (CELL *) top_b) Y = (CELL *) top_b;
#endif
	else Y = Y + ENV_Size(CPREG);
      }
#else
      if (Y > (CELL *) B)
	Y = (CELL *) B;
      else
	/* I am not sure about this */
	Y = Y + ENV_Size(CPREG);
#endif /* FROZEN_REGS */
      /* setup GB */
      Y[E_CB] = (CELL) B;
      goto creep;

    NoStackDExecute:
      /* set SREG for next instructions */
      SREG = (CELL *) (PREG->u.l.l);
#ifdef YAPOR
      /* abort_optyap("noStackDExecute in function absmi"); */
      if (HeapTop > GlobalBase - MinHeapGap)
#else
      if (HeapTop > Addr(AuxSp) - MinHeapGap)
#endif /* YAPOR */
	{
	  ASP = Y+E_CB;
	  if (ASP > (CELL *)B)
	    ASP = (CELL *)B;
	  goto noheapleft;
	}
#ifdef COROUTINING
      if (CFREG == Unsigned(LCL0)) {
	if (ReadTimedVar(WokenGoals) != TermNil)
	  goto creepde;
	else {
	  CFREG = CalculateStackGap();
	  JMPNext();
	}
      }
#endif
      if (CFREG != CalculateStackGap())
	goto creepde;

    NoStackExec:

      /* try performing garbage collection */

      ASP = Y+E_CB;
      if (ASP > (CELL *)B)
	ASP = (CELL *)B;
      saveregs();
      gc(PredArity(SREG), ENV, CPREG);
      setregs();
      /* hopefully, gc will succeeded, and we will retry
       * the instruction */
      JMPNext();

    creepde:
      /* first, deallocate */
      CPREG = (yamop *) Y[E_CP];
      ENV = Y = (CELL *) Y[E_E];
#ifdef DEPTH_LIMIT
      Y[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) Y = (CELL *) top_b;
#else
	if (Y > (CELL *) top_b) Y = (CELL *) top_b;
#endif
	else Y = (CELL *) ((CELL)Y + ENV_Size(CPREG));
      }
#else
      if (Y > (CELL *) B) {
	Y = (CELL *) B;
      }
      else {
	Y = (CELL *) ((CELL) Y + ENV_Size(CPREG));
      }
#endif /* FROZEN_REGS */
      /* setup GB */
      Y[E_CB] = (CELL) B;

      /* and now CREEP */

    creep:
#ifdef COROUTINING
      /* If I was not in creep mode, I have to unfold all woken
       * goals first.
       * Notice there might be a problem with
       * backtracking. I do not trail operations on WokenGoals,
       * hence Woken Goals is not restored on backtracking. This
       * is not a problem if suspended goals are quickly pushed onto
       * the stack for execution. In other words, WakeUpCode
       * *cannot* fail before having woken up all suspended goals.
       */
      /* make sure we are here because of an awoken goal */
      if (CFREG == Unsigned(LCL0) && !(PrologMode & (InterruptMode|AbortMode))) {
	Term WGs;
	Term my_goal;

	WGs = ReadTimedVar(WokenGoals);
	my_goal = AbsAppl(H);
	if (WGs != TermNil) {
#if SHADOW_S
	  /* save S for ModuleName() */
	  S = SREG;
#endif
	  BEGD(d0);
	  d0 = PredArity(SREG);
	  if (d0 == 0) {
	    my_goal = MkAtomTerm((Atom) PredFunctor(SREG));
	  }
	  else {
	    my_goal = AbsAppl(H);
	    *H = (CELL) PredFunctor(SREG);
	    H++;
	    BEGP(pt1);
	    pt1 = XREGS + 1;
	    for (; d0 > 0; --d0) {
	      BEGD(d1);
	      BEGP(pt0);
	      pt0 = pt1;
	      d1 = *pt0;
	      deref_head(d1, wake_up_unk);
	    wake_up_nonvar:
	      pt1++;
	      /* just copy it to the heap */
	      *H++ = d1;
	      continue;

	      derefa_body(d1, pt0, wake_up_unk, wake_up_nonvar);
	      /* bind it, in case it is a local variable */
	      if (pt0 <= H) {
		/* variable is safe */
		*H++ = (CELL)pt0;
		pt1++;
	      } else {
		d1 = Unsigned(H);
		RESET_VARIABLE(H);
		pt1++;
		H += 1;
		Bind_Local(pt0, d1);
	      }
	      /* notice I am incrementing H */
	      ENDP(pt0);
	      ENDD(d1);
	    }
	    ENDP(pt1);
	  }
	  ENDD(d0);
	  H[0] = Module_Name((CODEADDR)pred_entry(SREG));
	  H[1] = my_goal;
	  ARG1 = AbsPair(H);
	  H += 2;
	  ARG2 = ListOfWokenGoals();
	  SREG = (CELL *) (Unsigned(WakeUpCode) - sizeof(SMALLUNSGN));

	  /* no more goals to wake up */
	  UpdateTimedVar(WokenGoals, TermNil);
	  CFREG = CalculateStackGap();
	}
	else {
	  CFREG = CalculateStackGap();
	  /* We haven't changed P yet so this means redo the
	   * same instruction */
	  JMPNext();
	}
      }
      else {
#endif
	if (PrologMode & (AbortMode|InterruptMode)) {
	  CFREG = CalculateStackGap();
	  /* same instruction */
	  if (PrologMode & InterruptMode) {
	    PrologMode &= ~InterruptMode;
	    ProcessSIGINT();
	  } 
	  if (PrologMode & AbortMode) {
	    PrologMode &= ~AbortMode;
	    Abort("");
	  }
	  JMPNext();
	}
#if SHADOW_S
	S = SREG;
#endif
	BEGD(d0);
	d0 = PredArity(SREG);
	if (d0 == 0) {
	  H[1] = MkAtomTerm((Atom) PredFunctor(SREG));
	}
	else {
	  H[d0 + 2] = AbsAppl(H);
	  *H = (CELL) PredFunctor(SREG);
	  H++;
	  BEGP(pt1);
	  pt1 = XREGS + 1;
	  for (; d0 > 0; --d0) {
	    BEGD(d1);
	    BEGP(pt0);
	    pt0 = pt1;
	    d1 = *pt0;
	    deref_head(d1, creep_unk);
	  creep_nonvar:
	    /* just copy it to the heap */
	    pt1++;
	    *H++ = d1;
	    continue;

	    derefa_body(d1, pt0, creep_unk, creep_nonvar);
	    if (pt0 <= H) {
	      /* variable is safe */
	      *H++ = (CELL)pt0;
	      pt1++;
	    } else {
	      /* bind it, in case it is a local variable */
	      d1 = Unsigned(H);
	      RESET_VARIABLE(H);
	      pt1++;
	      H += 1;
	      Bind_Local(pt0, d1);
	    }
	    ENDP(pt0);
	    ENDD(d1);
	  }
	  ENDP(pt1);
	}
	ENDD(d0);
	H[0] = Module_Name((CODEADDR)pred_entry(SREG));
	ARG1 = (Term) AbsPair(H);

	H += 2;
	CFREG = CalculateStackGap();
	SREG = (CELL *) (Unsigned(CreepCode) - sizeof(SMALLUNSGN));
#ifdef COROUTINING
      }
#endif
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,pred_entry(SREG),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      PREG = (yamop *) PredCode(SREG);
      CACHE_A1();
      JMPNext();

      BOp(procceed, e);
      PREG = (yamop *) CPREG;
      Y = ENV;
#ifdef DEPTH_LIMIT
      DEPTH = Y[E_DEPTH];
#endif
      JMPNext();
      ENDBOp();

      Op(allocate, e);
      CACHE_Y_AS_ENV(Y);
      PREG = NEXTOP(PREG, e);
      E_Y[E_CP] = (CELL) CPREG;
      E_Y[E_E] = (CELL) ENV;
#ifdef DEPTH_LIMIT
      E_Y[E_DEPTH] = DEPTH;
#endif	/* DEPTH_LIMIT */
      ENV = E_Y;
      ENDCACHE_Y_AS_ENV();
      GONext();
      ENDOp();

      Op(deallocate, e);
      PREG = NEXTOP(PREG, e);
      /* other instructions do depend on S being set by deallocate
	 :-( */
      SREG = Y;
      CPREG = (yamop *) Y[E_CP];
      ENV = Y = (CELL *) Y[E_E];
#ifdef DEPTH_LIMIT
      DEPTH = Y[E_DEPTH];
#endif	/* DEPTH_LIMIT */
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) Y = (CELL *) top_b;
#else
	if (Y > (CELL *) top_b) Y = (CELL *) top_b;
#endif
	else Y = (CELL *)((CELL) Y + ENV_Size(CPREG));
      }
#else
      if (Y > (CELL *) B)
	Y = (CELL *) B;
      else
	Y = (CELL *) ((CELL) Y + ENV_Size(CPREG));
#endif /* FROZEN_REGS */
      GONext();
      ENDOp();

/**********************************************
*        OPTYap instructions                  *
**********************************************/

#ifdef YAPOR
#include "or.insts.i"
#endif /* YAPOR */
#ifdef TABLING
#include "tab.insts.i"
#include "tab.tries.insts.i"
#endif /* TABLING */


/************************************************************************\
*    Get Instructions							*
\************************************************************************/

      Op(get_x_var, xx);
      BEGD(d0);
      d0 = XREG(PREG->u.xx.xr);
      XREG(PREG->u.xx.xl) = d0;
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      GONext();
      ENDOp();

      Op(get_y_var, yx);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.yx.y;
      d0 = XREG(PREG->u.yx.x);
      PREG = NEXTOP(PREG, yx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      /* The code for get_x_val is hard to follow because I use a
       * lot of jumps. The convention is that in the label
       * gval_X_Y X refers to the state of the first argument, and
       * Y to the state of the second argument */
      Op(get_x_val, xx);
      BEGD(d0);
      d0 = XREG(PREG->u.xx.xl);
      deref_head(d0, gvalx_unk);

      /* d0 will keep the first argument */
    gvalx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->u.xx.xr);
      deref_head(d1, gvalx_nonvar_unk);

    gvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt0);
      /* deref second argument */
      deref_body(d1, pt0, gvalx_nonvar_unk, gvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xx);
      BIND(pt0, d0, bind_gvalx_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_gvalx_nonvar_var:
#endif
      GONext();

      ENDP(pt0);
      ENDD(d1);

      BEGP(pt0);
      /* first argument may be unbound */
      deref_body(d0, pt0, gvalx_unk, gvalx_nonvar);
      /* first argument is unbound and in pt0 and in d0 */
      BEGD(d1);
      d1 = XREG(PREG->u.xx.xr);
      deref_head(d1, gvalx_var_unk);

    gvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xx);
      BIND(pt0, d1, bind_gvalx_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_gvalx_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, gvalx_var_unk, gvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyCells(pt0, pt1, uc1, uc2);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc1:
#endif
      GONext();
#ifdef COROUTINING
    uc2:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      /* The code for get_y_val mostly uses the code for get_x_val
       */

      Op(get_y_val, yx);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = Y + PREG->u.yx.y;
      d0 = *pt0;

      /* From now on, it's just a copy of the code for get_x_val */

      deref_head(d0, gvaly_unk);
    gvaly_nonvar:

      /* first argument is bound */
      d1 = XREG(PREG->u.yx.x);
      deref_head(d1, gvaly_nonvar_unk);
    gvaly_nonvar_nonvar:

      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, yx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, gvaly_nonvar_unk, gvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, yx);
      BIND(pt1, d0, bind_gvaly_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
    bind_gvaly_nonvar_var:
#endif
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      derefa_body(d0, pt0, gvaly_unk, gvaly_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->u.yx.x);
      deref_head(d1, gvaly_var_unk);

    gvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, yx);
      BIND(pt0, d1, bind_gvaly_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_gvaly_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, gvaly_var_unk, gvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, yx);
      UnifyCells(pt0, pt1, uc3, uc4);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc3:
#endif
      GONext();
#ifdef COROUTINING
    uc4:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(get_atom, xc);
      BEGD(d0);
      BEGD(d1);
      /* fetch arguments */
      d0 = XREG(PREG->u.xc.x);
      d1 = PREG->u.xc.c;

      BEGP(pt0);
      deref_head(d0, gatom_unk);
      /* argument is nonvar */
    gatom_nonvar:
      if (d0 == d1) {
	PREG = NEXTOP(PREG, xc);
	GONext();
      }
      else {
	FAIL();
      }

      deref_body(d0, pt0, gatom_unk, gatom_nonvar);
      /* argument is a variable */
      PREG = NEXTOP(PREG, xc);
      BIND(pt0, d1, bind_gatom);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_gatom:
#endif
      GONext();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      /* The next instructions can lead to either the READ stream
       * or the write stream */

      OpRW(get_list, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, glist_unk);

    glist_nonvar:
      /* did we find a list? */
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      START_PREFETCH(x);
      PREG = NEXTOP(PREG, x);
      /* enter read mode */
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_unk, glist_nonvar);
      /* glist var */
      /* enter write mode */
      CACHE_S();
      S_SREG = H;
      START_PREFETCH_W(x);
      PREG = NEXTOP(PREG, x);
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      BIND(pt0, d0, bind_glist);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist:
#endif
      /* don't put an ENDD just after a label */
      H = S_SREG + 2;
      ENDD(d0);
      WRITEBACK_S(S_SREG);
      GONextW();


      END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(get_struct, xf);
      BEGD(d0);
      d0 = XREG(PREG->u.xf.x);
      deref_head(d0, gstruct_unk);

    gstruct_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a compound term */
      START_PREFETCH(xf);
      CACHE_S();
      S_SREG = RepAppl(d0);
      /* check functor */
      d0 = (CELL) (PREG->u.xf.f);
      if (*S_SREG != d0) {
	FAIL();
      }
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      PREG = NEXTOP(PREG, xf);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gstruct_unk, gstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH_W(xf);
      BEGD(d1);
      d1 = AbsAppl(H);
      BIND(pt0, d1, bind_gstruct);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_gstruct:
#endif
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = H;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->u.xf.f);
      *pt0++ = d0;
      H = pt0 + PREG->u.xf.a;
      PREG = NEXTOP(PREG, xf);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      Op(get_float, xc);
      BEGD(d0);
      d0 = XREG(PREG->u.xc.x);
      deref_head(d0, gfloat_unk);

    gfloat_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting float */
      START_PREFETCH(xc);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorDouble) {
	FAIL();
      }
      BEGP(pt1);
      pt1 = RepAppl(PREG->u.xc.c);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      PREG = NEXTOP(PREG, xc);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gfloat_unk, gfloat_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = PREG->u.xc.c;
      PREG = NEXTOP(PREG, xc);
      BIND(pt0, d1, bind_gfloat);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_gfloat:
#endif
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_longint, xc);
      BEGD(d0);
      d0 = XREG(PREG->u.xc.x);
      deref_head(d0, glongint_unk);

    glongint_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting longint */
      START_PREFETCH(xc);
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      BEGP(pt1);
      pt1 = RepAppl(PREG->u.xc.c);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      PREG = NEXTOP(PREG, xc);
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glongint_unk, glongint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = PREG->u.xc.c;
      PREG = NEXTOP(PREG, xc);
      BIND(pt0, d1, bind_glongint);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_glongint:
#endif
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(get_bigint, xc);
      BEGD(d0);
      d0 = XREG(PREG->u.xc.x);
      deref_head(d0, gbigint_unk);

    gbigint_nonvar:
      if (!IsApplTerm(d0))
	FAIL();
      /* we have met a preexisting bigint */
      START_PREFETCH(xc);
#ifdef USE_GMP
      BEGP(pt0);
      pt0 = RepAppl(d0);
      /* check functor */
      if (*pt0 != (CELL)FunctorBigInt)
#else
      if (TRUE)
#endif
	{
	  FAIL();
	}
#ifdef USE_GMP
      if (mpz_cmp(BigIntOfTerm(d0),BigIntOfTerm(PREG->u.xc.c)))
	FAIL();
#endif
      PREG = NEXTOP(PREG, xc);      
#ifdef USE_GMP
      ENDP(pt0);
#endif
      /* enter read mode */
      GONext();
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, gbigint_unk, gbigint_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH(xc);
      BEGD(d1);
      d1 = PREG->u.xc.c;
      PREG = NEXTOP(PREG, xc);
      BIND(pt0, d1, bind_gbigint);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_gbigint:
#endif
      GONext();
      ENDD(d1);
      END_PREFETCH();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

/************************************************************************\
*    Optimised Get List Instructions					*
\************************************************************************/
      OpRW(glist_valx, xx);
      BEGD(d0);
      d0 = XREG(PREG->u.xx.xl);
      deref_head(d0, glist_valx_write);
    glist_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      START_PREFETCH(xx);
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_valx_unk);

      /* first argument is in d0 */
    glist_valx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->u.xx.xr);
      deref_head(d1, glist_valx_nonvar_unk);

    glist_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, glist_valx_nonvar_unk, glist_valx_nonvar_nonvar);
      /* head bound, argument unbound */
      PREG = NEXTOP(PREG, xx);
      BIND(pt1, d0, bind_glist_valx_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
    bind_glist_valx_nonvar_var:
#endif
      GONext();
      ENDP(pt1);


      ENDD(d1);

      /* head may be unbound */
      derefa_body(d0, pt0, glist_valx_unk, glist_valx_nonvar);
      /* head is unbound, pt0 has the value */
      d0 = XREG(PREG->u.xx.xr);
      deref_head(d0, glist_valx_var_unk);

    glist_valx_var_nonvar:
      /* head is unbound, second arg bound */
      PREG = NEXTOP(PREG, xx);
      BIND_GLOBAL(pt0, d0, bind_glist_valx_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_valx_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, glist_valx_var_unk, glist_valx_var_nonvar);
      /* head and second argument are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalRegCells(pt0, pt1, uc5, uc6);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc5:
#endif
      GONext();
#ifdef COROUTINING
    uc6:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_valx_write, glist_valx_read);
      CACHE_S();
      /* enter write mode */
      S_SREG = H;
      BEGD(d1);
      d1 = XREG(PREG->u.xx.xr);
      d0 = AbsPair(S_SREG);
      S_SREG[0] = d1;
      ENDD(d1);
      ALWAYS_START_PREFETCH_W(xx);
#ifdef COROUTINING
      PREG = NEXTOP(PREG, xx);
      H = S_SREG + 2;
      WRITEBACK_S(S_SREG+1);
#endif
      DBIND(pt0, d0, dbind);
#ifndef COROUTINING
      /* include XREG on it */
      PREG = NEXTOP(PREG, xx);
      H = S_SREG + 2;
      WRITEBACK_S(S_SREG+1);
#endif

#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    dbind:
#endif
      ALWAYS_GONextW();
      ALWAYS_END_PREFETCH_W();
      ENDCACHE_S();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpRW(glist_valy, xy);
      BEGD(d0);
      d0 = XREG(PREG->u.xy.x);
      deref_head(d0, glist_valy_write);
    glist_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      START_PREFETCH(xy);
      /* enter read mode */
      pt0 = RepPair(d0);
      SREG = pt0 + 1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_valy_unk);

    glist_valy_nonvar:
      /* first argument is bound */
      BEGD(d1);
      BEGP(pt1);
      pt1 = Y + PREG->u.xy.y;
      d1 = *pt1;
      PREG = NEXTOP(PREG, xy);
      deref_head(d1, glist_valy_nonvar_unk);

    glist_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      SREG = pt0 + 1;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_valy_nonvar_unk, glist_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      BIND(pt1, d0, bind_glist_valy_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
    bind_glist_valy_nonvar_var:
#endif
      GONext();


      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_valy_unk, glist_valy_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = Y+PREG->u.xy.y;
      d1 = *pt1;
      deref_head(d1, glist_valy_var_unk);
    glist_valy_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xy);
      BIND_GLOBAL(pt0, d1, bind_glist_valy_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_valy_var_nonvar:
#endif
      GONext();

      derefa_body(d1, pt1, glist_valy_var_unk, glist_valy_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xy);
      UnifyGlobalRegCells(pt0, pt1, uc7, uc8);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc7:
#endif
      GONext();
#ifdef COROUTINING
    uc8:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDD(d1);

      END_PREFETCH();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_valy_write, glist_valy_read);
      /* enter write mode */
      START_PREFETCH_W(xy);
      BEGP(pt1);
      pt1 = H;
      d0 = AbsPair(pt1);
      BIND(pt0, d0, bind_glist_valy_write);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_valy_write:
#endif
      BEGD(d0);
      /* include XREG on it */
      d0 = Y[PREG->u.xy.y];
      pt1[0] = d0;
      ENDD(d0);
      H = pt1 + 2;
      SREG = pt1 + 1;
      ENDP(pt1);
      PREG = NEXTOP(PREG, xy);
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      Op(gl_void_varx, xx);
      BEGD(d0);
      d0 = XREG(PREG->u.xx.xl);
      deref_head(d0, glist_void_varx_write);
    glist_void_varx_read:
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      ALWAYS_START_PREFETCH(xx);
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      XREG(PREG->u.xx.xr) = d0;
      PREG = NEXTOP(PREG, xx);
      ALWAYS_GONext();
      ENDP(pt0);
      ALWAYS_END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_varx_write, glist_void_varx_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = H;
      /* include XREG on it */
      XREG(PREG->u.xx.xr) =
	Unsigned(pt1 + 1);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      H = pt1 + 2;
      BEGD(d0);
      d0 = AbsPair(pt1);
      BIND(pt0, d0, bind_glist_varx_write);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_varx_write:
#endif
      PREG = NEXTOP(PREG, xx);
      ENDD(d0);
      ENDP(pt1);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(gl_void_vary, xy);
      BEGD(d0);
      d0 = XREG(PREG->u.xy.x);
      deref_head(d0, glist_void_vary_write);
    glist_void_vary_read:
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      BEGP(pt0);
      pt0 = RepPair(d0);
      d0 = pt0[1];
      ENDP(pt0);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.xy.y,d0);
#else
      Y[PREG->u.xy.y] = d0;
#endif
      PREG = NEXTOP(PREG, xy);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_vary_write, glist_void_vary_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = H;
      /* include XREG on it */
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.xy.y,Unsigned(pt1 + 1));
#else
      Y[PREG->u.xy.y] = Unsigned(pt1 + 1);
#endif
      PREG = NEXTOP(PREG, xy);
      RESET_VARIABLE(pt1);
      RESET_VARIABLE(pt1+1);
      d0 = AbsPair(pt1);
      H = pt1 + 2;
      BIND(pt0, d0, bind_glist_void_vary_write);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_void_vary_write:
#endif
      GONext();
      ENDP(pt1);
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valx, xx);
      BEGD(d0);
      d0 = XREG(PREG->u.xx.xl);
      deref_head(d0, glist_void_valx_write);
    glist_void_valx_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valx_unk);

    glist_void_valx_nonvar:
      /* first argument is bound */
      BEGD(d1);
      d1 = XREG(PREG->u.xx.xr);
      deref_head(d1, glist_void_valx_nonvar_unk);

    glist_void_valx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xx);
      UnifyBound(d0, d1);

      /* deref second argument */
      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_nonvar_unk, glist_void_valx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xx);
      BIND(pt1, d0, bind_glist_void_valx_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_glist_void_valx_nonvar_var:
#endif
      GONext();
      ENDP(pt1);
      ENDD(d1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valx_unk, glist_void_valx_nonvar);
      /* first argument is unbound */
      BEGD(d1);
      d1 = XREG(PREG->u.xx.xr);
      deref_head(d1, glist_void_valx_var_unk);

    glist_void_valx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xx);
      BIND_GLOBAL(pt0, d1, bind_glist_void_valx_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_glist_void_valx_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, glist_void_valx_var_unk, glist_void_valx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xx);
      UnifyGlobalRegCells(pt0, pt1, uc9, uc10);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc9:
#endif
      GONext();
#ifdef COROUTINING
    uc10:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valx_write, glist_void_valx_read);
      /* enter write mode */
      BEGP(pt1);
      pt1 = H;
      d0 = AbsPair(SREG);
      BIND(pt0, d0, bind_glist_void_valx_write);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_void_valx_write:
#endif
      BEGD(d0);
      /* include XREG on it */
      d0 = XREG(PREG->u.xx.xr);
      RESET_VARIABLE(pt1);
      pt1[1] = d0;
      H = pt1 + 2;
      ENDD(d0);
      ENDP(pt1);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(gl_void_valy, xy);
      BEGD(d0);
      d0 = XREG(PREG->u.xy.x);
      deref_head(d0, glist_void_valy_write);
    glist_void_valy_read:
      BEGP(pt0);
      /* did we find a list? */
      if (!IsPairTerm(d0))
	FAIL();
      /* enter read mode */
      pt0 = RepPair(d0)+1;
      /* start unification with first argument */
      d0 = *pt0;
      deref_head(d0, glist_void_valy_unk);

    glist_void_valy_nonvar:
      /* first argument is bound */
      BEGD(d1);
      BEGP(pt1);
      pt1 = Y+PREG->u.xy.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_nonvar_unk);

    glist_void_valy_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, xy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, glist_void_valy_nonvar_unk, glist_void_valy_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, xy);
      BIND(pt1, d0, bind_glist_void_valy_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_glist_void_valy_nonvar_var:
#endif
      GONext();

      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, glist_void_valy_unk, glist_void_valy_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = Y+PREG->u.xy.y;
      d1 = *pt1;
      deref_head(d1, glist_void_valy_var_unk);

    glist_void_valy_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, xy);
      BIND_GLOBAL(pt0, d1, bind_glist_void_valy_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_glist_void_valy_var_nonvar:
#endif
      GONext();

      deref_body(d1, pt1, glist_void_valy_var_unk, glist_void_valy_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, xy);
      UnifyGlobalRegCells(pt0, pt1, uc11, uc12);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc11:
#endif
      GONext();
#ifdef COROUTINING
    uc12:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d0, pt0, glist_void_valy_write, glist_void_valy_read);
      /* enter write mode */
      CACHE_S();
      S_SREG = H;
      d0 = AbsPair(S_SREG);
      BIND(pt0, d0, bind_glist_void_valy_write);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_glist_void_valy_write:
#endif
      /* include XREG on it */
      BEGD(d1);
      d1 = Y[PREG->u.xy.y];
      RESET_VARIABLE(S_SREG);
      S_SREG[1] = d1;
      ENDD(d1);
      PREG = NEXTOP(PREG, xy);
      H = S_SREG + 2;
      ENDCACHE_S();
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();



/************************************************************************\
* 	Unify instructions						*
\************************************************************************/

      Op(unify_x_var, ox);
      CACHE_S();
      READ_IN_S();
      BEGD(d0);
      d0 = *S_SREG;
#if SBA
      if (d0 == 0)
	d0 = (CELL)S_SREG;
#endif
      WRITEBACK_S(S_SREG+1);
      ALWAYS_START_PREFETCH(ox);
      XREG(PREG->u.ox.x) = d0;
      PREG = NEXTOP(PREG, ox);
      ALWAYS_GONext();
      ALWAYS_END_PREFETCH();
      ENDD(d0);
      ENDCACHE_S();
      ENDOp();

      OpW(unify_x_var_write, ox);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->u.ox.x);
      PREG = NEXTOP(PREG, ox);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL) S_SREG;
      WRITEBACK_S(S_SREG+1);
      ENDP(pt0);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      BOp(unify_l_x_var, ox);
      ALWAYS_START_PREFETCH(ox);
      BEGP(pt0);
      BEGD(d0);
      d0 = SREG[0];
      pt0 = &XREG(PREG->u.ox.x);
      PREG = NEXTOP(PREG, ox);
#if SBA
      if (d0 == 0)
	d0 = (CELL)SREG;
#endif
      *pt0 = d0;
      ALWAYS_GONext();
      ENDD(d0);
      ENDP(pt0);
      ALWAYS_END_PREFETCH();
      ENDBOp();

      BOp(unify_l_x_var_write, ox);
      ALWAYS_START_PREFETCH(ox);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->u.ox.x);
      PREG = NEXTOP(PREG, ox);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL)S_SREG;
      ENDP(pt0);
      ENDCACHE_S();
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();

      BOp(unify_x_var2, oxx);
      CACHE_S();
      ALWAYS_START_PREFETCH(oxx);
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->u.oxx.xr);
      BEGD(d0);
      d0 = S_SREG[0];
      BEGD(d1);
      d1 = S_SREG[1];
#if SBA
      if (d0 == 0)
	d0 = (CELL)S_SREG;
      if (d1 == 0)
	d1 = (CELL)(S_SREG+1);
#endif
      WRITEBACK_S(S_SREG+2);
      XREG(PREG->u.oxx.xl) = d0;
      PREG = NEXTOP(PREG, oxx);
      *pt0 = d1;
      ENDD(d0);
      ENDD(d1);
      ENDP(pt0);
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();
      ENDCACHE_S();

      OpW(unify_x_var2_write, oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->u.oxx.xr);
      RESET_VARIABLE(S_SREG);
      XREG(PREG->u.oxx.xl) = (CELL) S_SREG;
      S_SREG++;
      PREG = NEXTOP(PREG, oxx);
      RESET_VARIABLE(S_SREG);
      *pt0 = (CELL) S_SREG;
      ENDP(pt0);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      BOp(unify_l_x_var2, oxx);
      ALWAYS_START_PREFETCH(oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->u.oxx.xr);
      BEGD(d0);
      d0 = S_SREG[0];
      BEGD(d1);
      d1 = S_SREG[1];
#if SBA
      if (d0 == 0)
	XREG(PREG->u.oxx.xl) = (CELL)S_SREG;
      else
#endif
	XREG(PREG->u.oxx.xl) = d0;
      PREG = NEXTOP(PREG, oxx);
#if SBA
      if (d1 == 0)
	*pt0 = (CELL)(S_SREG+1);
      else
#endif
	*pt0 = d1;
      ENDD(d0);
      ENDD(d1);
      ENDP(pt0);
      ENDCACHE_S();
      ALWAYS_GONext();
      ENDBOp();
      ALWAYS_END_PREFETCH();

      Op(unify_l_x_var2_write, oxx);
      CACHE_S();
      READ_IN_S();
      BEGP(pt0);
      pt0 = &XREG(PREG->u.oxx.xr);
      XREG(PREG->u.oxx.xl) = (CELL) S_SREG;
      RESET_VARIABLE(S_SREG);
      S_SREG++;
      *pt0 = (CELL) S_SREG;
      PREG = NEXTOP(PREG, oxx);
      RESET_VARIABLE(S_SREG);
      ENDP(pt0);
      ENDCACHE_S();
      GONext();
      ENDOp();

      Op(unify_y_var, oy);
      BEGD(d0);
      d0 = *SREG++;
#if defined(SBA)
#ifdef FROZEN_REGS
      if (d0 == 0) {
	Bind_Local(Y+PREG->u.oy.y,(CELL)(SREG-1));
      } else {
	Bind_Local(Y+PREG->u.oy.y,d0);
      }
#else
      if (d0 == 0) {
	Y[PREG->u.oy.y] = (CELL)(SREG-1);
      } else
	Y[PREG->u.oy.y] = d0;
#endif
#else
      Y[PREG->u.oy.y] = d0;
#endif
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDD(d0);
      ENDOp();

      OpW(unify_y_var_write, oy);
      CACHE_S();
      READ_IN_S();
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.oy.y,(CELL) S_SREG);
#else
      Y[PREG->u.oy.y] = (CELL) S_SREG;
#endif
      PREG = NEXTOP(PREG, oy);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_l_y_var, oy);
      BEGD(d0);
      d0 = SREG[0];
#ifdef SBA
#ifdef FROZEN_REGS
      if (d0 == 0) {
	Bind_Local(Y+PREG->u.oy.y,(CELL)SREG);
      } else {
	Bind_Local(Y+PREG->u.oy.y,d0);
      }
#else
      if (d0 == 0) {
	Y[PREG->u.oy.y] = (CELL)SREG;
      } else {
	Y[PREG->u.oy.y] = d0;
      }
#endif
#else
      Y[PREG->u.oy.y] = d0;
#endif
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_var_write, oy);
      CACHE_S();
      READ_IN_S();
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.oy.y,(CELL) S_SREG);
#else
      Y[PREG->u.oy.y] = (CELL) S_SREG;
#endif
      PREG = NEXTOP(PREG, oy);
      RESET_VARIABLE(S_SREG);
      ENDCACHE_S();
      GONext();
      ENDOp();

      OpW(unify_x_val_write, ox);
      /* we are in write mode */
      *SREG++ = XREG(PREG->u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_x_val, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_unk);

    uvalx_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, uvalx_nonvar_unk);

    uvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      /* pt0 is in the structure and pt1 the register */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_nonvar_unk, uvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      BIND(pt1, d0, bind_uvalx_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_uvalx_nonvar_var:
#endif
      GONext();
     ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_unk, uvalx_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, uvalx_var_unk);

    uvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      BIND_GLOBAL(pt0, d1, bind_uvalx_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_uvalx_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, uvalx_var_unk, uvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyGlobalRegCells(pt0, pt1, uc13, uc14);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc13:
#endif
      GONext();
#ifdef COROUTINING
    uc14:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_val_write, ox);
      /* we are in write mode */
      SREG[0] = XREG(PREG->u.ox.x);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_x_val, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_unk);

    ulvalx_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, ulvalx_nonvar_unk);

    ulvalx_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, ulvalx_nonvar_unk, ulvalx_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      BIND(pt1, d0, bind_ulvalx_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_ulvalx_nonvar_var:
#endif
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_unk, ulvalx_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, ulvalx_var_unk);

    ulvalx_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      BIND_GLOBAL(pt0, d1, bind_ulvalx_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulvalx_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_var_unk, ulvalx_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalRegCells(pt0, pt1, uc15, uc16);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc15:
#endif
      GONext();
#ifdef COROUTINING
    uc16:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_y_val_write, oy);
      /* we are in write mode */
      BEGD(d0);
      d0 = Y[PREG->u.oy.y];
#if SBA
      if (d0 == 0) /* free variable */
	*SREG++ = (CELL)(Y+PREG->u.oy.y);
      else
#endif
	*SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_y_val, oy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_unk);

    uvaly_nonvar:
      /* first argument is bound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y; 
      d1 = *pt1;
      deref_head(d1, uvaly_nonvar_unk);

    uvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_nonvar_unk, uvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      BIND(pt1, d0, bind_uvaly_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_uvaly_nonvar_var:
#endif
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_unk, uvaly_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_var_unk);

    uvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      BIND_GLOBAL(pt0, d1, bind_uvaly_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_uvaly_var_nonvar:
#endif
      GONext();

      derefa_body(d1, pt1, uvaly_var_unk, uvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyGlobalRegCells(pt0, pt1, uc17, uc18);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc17:
#endif
      GONext();
#ifdef COROUTINING
    uc18:
      DO_TRAIL(pt1, (CELL)pt1);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_val_write, oy);
      /* we are in write mode */
      BEGD(d0);
      d0 = Y[PREG->u.oy.y];
#if SBA
      if (d0 == 0) /* new variable */
	SREG[0] = (CELL)(Y+PREG->u.oy.y);
      else
#endif
	SREG[0] = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

      /* We assume the value in X is pointing to an object in the
       * global stack */
      Op(unify_l_y_val, oy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_unk);

    ulvaly_nonvar:
      /* first argument is bound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_nonvar_unk);

    ulvaly_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_nonvar_unk, ulvaly_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      BIND(pt1, d0, bind_ulvaly_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_ulvaly_nonvar_var:
#endif
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_unk, ulvaly_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_var_unk);

    ulvaly_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      BIND_GLOBAL(pt0, d1, bind_ulvaly_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulvaly_var_nonvar:
#endif
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_var_unk, ulvaly_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalRegCells(pt0, pt1, uc19, uc20);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc19:
#endif
      GONext();
#ifdef COROUTINING
    uc20:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_x_loc_write, ox);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->u.ox.x);
      deref_head(d0, unify_x_loc_unk);
    unify_x_loc_nonvar:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, ox);
      GONextW();

      BEGP(pt0);
      deref_body(d0, pt0, unify_x_loc_unk, unify_x_loc_nonvar);
      /* move ahead in the instructions */
      PREG = NEXTOP(PREG, ox);
      /* d0 is a variable, check whether we need to globalise it */
      if (pt0 < H) {
	/* variable is global */
	*SREG++ = Unsigned(pt0);
	GONextW();
      }
      else {
	/* bind our variable to the structure */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	WRITEBACK_S(S_SREG+1);
	ENDCACHE_S();
	GONextW();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      /* In the next instructions, we do not know anything about
       * what is in X */
      Op(unify_x_loc, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvalx_loc_unk);

    uvalx_loc_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, uvalx_loc_nonvar_unk);

    uvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyBound(d0, d1);

      BEGP(pt1);
      /* deref second argument */
      deref_body(d1, pt1, uvalx_loc_nonvar_unk, uvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      BIND(pt1, d0, bind_uvalx_loc_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_uvalx_loc_nonvar_var:
#endif
      GONext();
      ENDP(pt1);


      /* first argument may be unbound */
      derefa_body(d0, pt0, uvalx_loc_unk, uvalx_loc_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, uvalx_loc_var_unk);

    uvalx_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      BIND_GLOBAL(pt0, d1, bind_uvalx_loc_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_uvalx_loc_var_nonvar:
#endif
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      BEGP(pt1);
      deref_body(d1, pt1, uvalx_loc_var_unk, uvalx_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      SREG++;
      UnifyGlobalRegCells(pt0, pt1, uc21, uc22);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc21:
#endif
      GONext();
#ifdef COROUTINING
    uc22:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_x_loc_write, ox);
      /* we are in write mode */
      BEGD(d0);
      d0 = XREG(PREG->u.ox.x);
      deref_head(d0, ulnify_x_loc_unk);
    ulnify_x_loc_nonvar:
      SREG[0] = d0;
      PREG = NEXTOP(PREG, ox);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, ulnify_x_loc_unk, ulnify_x_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, ox);
      if (pt0 < H) {
	/* variable is global */
	SREG[0] = Unsigned(pt0);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	Bind_Local(pt0, Unsigned(SREG));
	RESET_VARIABLE(SREG);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      /* In the next instructions, we do not know anything about
       * what is in X */
      Op(unify_l_x_loc, ox);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvalx_loc_unk);

    ulvalx_loc_nonvar:
      /* first argument is bound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, ulvalx_loc_nonvar_unk);

    ulvalx_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, ox);
      UnifyBound(d0, d1);

      /* deref second argument */
      deref_body(d1, pt0, ulvalx_loc_nonvar_unk, ulvalx_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, ox);
      BIND(pt0, d0, bind_ulvalx_loc_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulvalx_loc_nonvar_var:
#endif
      GONext();

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvalx_loc_unk, ulvalx_loc_nonvar);
      /* first argument is unbound */
      d1 = XREG(PREG->u.ox.x);
      deref_head(d1, ulvalx_loc_var_unk);

    ulvalx_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, ox);
      BIND_GLOBAL(pt0, d1, bind_ulvalx_loc_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulvalx_loc_var_nonvar:
#endif
      GONext();

      BEGP(pt1);
      deref_body(d1, pt1, ulvalx_loc_var_unk, ulvalx_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, ox);
      UnifyGlobalRegCells(pt0, pt1, uc23, uc24);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc23:
#endif
      GONext();
#ifdef COROUTINING
    uc24:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_y_loc_write, oy);
      /* we are in write mode */
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y+PREG->u.oy.y;
      d0 = *pt0;
      deref_head(d0, unify_y_loc_unk);
    unify_y_loc_nonvar:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, oy);
      GONextW();

      derefa_body(d0, pt0, unify_y_loc_unk, unify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, oy);
      if (pt0 < H) {
	/* variable is global */
	*SREG++ = Unsigned(pt0);
	GONextW();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	WRITEBACK_S(S_SREG+1);
	ENDCACHE_S();
	GONextW();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOpW();

      Op(unify_y_loc, oy);
      /* we are in read mode */
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, uvaly_loc_unk);

    uvaly_loc_nonvar:
      /* structure is bound */
      BEGP(pt1);
      pt1 =  Y+PREG->u.oy.y;
      d1 = *pt1;
      deref_head(d1, uvaly_loc_nonvar_unk);

    uvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, uvaly_loc_nonvar_unk, uvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      BIND(pt1, d0, bind_uvaly_loc_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_uvaly_loc_nonvar_var:
#endif
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, uvaly_loc_unk, uvaly_loc_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y; 
      d1 = *pt1;
      deref_head(d1, uvaly_loc_var_unk);

    uvaly_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      BIND_GLOBAL(pt0, d1, bind_uvaly_loc_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_uvaly_loc_var_nonvar:
#endif
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, uvaly_loc_var_unk, uvaly_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      SREG++;
      UnifyGlobalRegCells(pt0, pt1, uc25, uc26);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc25:
#endif
      GONext();
#ifdef COROUTINING
    uc26:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_loc_write, oy);
      /* we are in write mode */
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y+PREG->u.oy.y;
      d0 = *pt0;
      deref_head(d0, ulunify_y_loc_unk);
    ulunify_y_loc_nonvar:
      SREG[0] = d0;
      PREG = NEXTOP(PREG, oy);
      GONext();

      derefa_body(d0, pt0, ulunify_y_loc_unk, ulunify_y_loc_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      PREG = NEXTOP(PREG, oy);
      if (pt0 < H) {
	/* variable is global */
	SREG[0] = Unsigned(pt0);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	CACHE_S();
	READ_IN_S();
	Bind_Local(pt0, Unsigned(S_SREG));
	RESET_VARIABLE(S_SREG);
	ENDCACHE_S();
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_y_loc, oy);
      /* else we are in read mode */
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulvaly_loc_unk);

    ulvaly_loc_nonvar:
      /* structure is bound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_nonvar_unk);

    ulvaly_loc_nonvar_nonvar:
      /* both arguments are bound */
      /* we may have to bind structures */
      PREG = NEXTOP(PREG, oy);
      UnifyBound(d0, d1);

      /* deref second argument */
      derefa_body(d1, pt1, ulvaly_loc_nonvar_unk, ulvaly_loc_nonvar_nonvar);
      /* first argument bound, second unbound */
      PREG = NEXTOP(PREG, oy);
      BIND(pt1, d0, bind_ulvaly_loc_nonvar_var);
#ifdef COROUTINING
      DO_TRAIL(pt1, d0);
      if (pt1 < H0) WakeUp(pt1);
  bind_ulvaly_loc_nonvar_var:
#endif
      GONext();
      ENDP(pt1);

      /* first argument may be unbound */
      derefa_body(d0, pt0, ulvaly_loc_unk, ulvaly_loc_nonvar);
      /* first argument is unbound */
      BEGP(pt1);
      pt1 = Y+PREG->u.oy.y;
      d1 = *pt1;
      deref_head(d1, ulvaly_loc_var_unk);

    ulvaly_loc_var_nonvar:
      /* first unbound, second bound */
      PREG = NEXTOP(PREG, oy);
      BIND_GLOBAL(pt0, d1, bind_ulvaly_loc_var_nonvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulvaly_loc_var_nonvar:
#endif
      GONext();

      /* Here we are in trouble: we have a clash between pt1 and
       * SREG. We address this by storing SREG in d0 for the duration. */
      derefa_body(d1, pt1, ulvaly_loc_var_unk, ulvaly_loc_var_nonvar);
      /* both arguments are unbound */
      PREG = NEXTOP(PREG, oy);
      UnifyGlobalRegCells(pt0, pt1, uc27, uc28);
#ifdef COROUTINING
      DO_TRAIL(pt0, (CELL)pt1);
      if (pt0 < H0) WakeUp(pt0);
    uc27:
#endif
      GONext();
#ifdef COROUTINING
    uc28:
      DO_TRAIL(pt1, (CELL)pt0);
      if (pt1 < H0) WakeUp(pt1);
      GONext();
#endif
      ENDP(pt1);
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      OpW(unify_void_write, o);
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(S_SREG);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONextW();
      ENDOpW();

      Op(unify_void, o);
      SREG++;
      PREG = NEXTOP(PREG, o);
      GONext();
      ENDOp();

      Op(unify_l_void_write, o);
      PREG = NEXTOP(PREG, o);
      RESET_VARIABLE(SREG);
      GONext();
      ENDOp();

      Op(unify_l_void, o);
      PREG = NEXTOP(PREG, o);
      GONext();
      ENDOp();

      OpW(unify_n_voids_write, os);
      BEGD(d0);
      CACHE_S();
      d0 = PREG->u.os.s;
      READ_IN_S();
      PREG = NEXTOP(PREG, os);
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(S_SREG);
	S_SREG++;
      }
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_n_voids, os);
      SREG += PREG->u.os.s;
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      Op(unify_l_n_voids_write, os);
      BEGD(d0);
      d0 = PREG->u.os.s;
      PREG = NEXTOP(PREG, os);
      CACHE_S();
      READ_IN_S();
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(S_SREG);
	S_SREG++;
      }
      ENDCACHE_S();
      ENDD(d0);
      GONext();
      ENDOp();

      Op(unify_l_n_voids, os);
      PREG = NEXTOP(PREG, os);
      GONext();
      ENDOp();

      OpW(unify_atom_write, oc);
      * SREG++ = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONextW();
      ENDOpW();

      Op(unify_atom, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, uatom_unk);
    uatom_nonvar:
      if (d0 != PREG->u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      derefa_body(d0, pt0, uatom_unk, uatom_nonvar);
      d0 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(pt0, d0, bind_uatom);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
  bind_uatom:
#endif
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_atom_write, oc);
      SREG[0] = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      GONext();
      ENDOp();

      Op(unify_l_atom, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *SREG;
      deref_head(d0, ulatom_unk);
    ulatom_nonvar:
      if (d0 != PREG->u.oc.c) {
	FAIL();
      }
      PREG = NEXTOP(PREG, oc);
      GONext();

      derefa_body(d0, pt0, ulatom_unk, ulatom_nonvar);
      d0 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(pt0, d0, bind_ulatom);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulatom:
#endif
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      OpW(unify_n_atoms_write, osc);
      BEGD(d0);
      BEGD(d1);
      d0 = PREG->u.osc.s;
      d1 = PREG->u.osc.c;
      /* write N atoms */
      CACHE_S();
      READ_IN_S();
      PREG = NEXTOP(PREG, osc);
      for (; d0 > 0; d0--)
	*S_SREG++ = d1;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d1);
      ENDD(d0);
      GONextW();
      ENDOpW();

      Op(unify_n_atoms, osc);
      {
	register Int i = PREG->u.osc.s;		/* not enough registers */

	BEGD(d1);
	d1 = PREG->u.osc.c;
	for (; i > 0; i--) {
	  BEGD(d0);
	  BEGP(pt0);
	  pt0 = SREG++;
	  d0 = *pt0;
	  deref_head(d0, uatom_n_var);
	uatom_n_nonvar:
	  if (d0 != d1) {
	    FAIL();
	  }
	  continue;

	  derefa_body(d0, pt0, uatom_n_var, uatom_n_nonvar);
	  BIND_GLOBAL(pt0, d1, bind_unlatom);
#ifdef COROUTINING
	  DO_TRAIL(pt0, d1);
	  if (pt0 < H0) WakeUp(pt0);
	bind_unlatom:
	  continue;
#endif
	  ENDP(pt0);
	  ENDD(d0);
	}
	ENDD(d1);
      }
      PREG = NEXTOP(PREG, osc);
      GONext();
      ENDOp();

      Op(unify_float, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ufloat_unk);
    ufloat_nonvar:
      /* look inside term */
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->u.oc.c);
      PREG = NEXTOP(PREG, oc);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ufloat_unk, ufloat_nonvar);
      BEGD(d1);
      d1 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(pt0, d1, bind_ufloat);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ufloat:
#endif
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_float, oc);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulfloat_unk);
    ulfloat_nonvar:
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorDouble) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->u.oc.c);
      PREG = NEXTOP(PREG, oc);
      if (
	  pt1[1] != pt0[1]
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
	  || pt1[2] != pt0[2]
#endif
	  ) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ulfloat_unk, ulfloat_nonvar);
      BEGD(d1);
      d1 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(S_SREG, d1, bind_ulfloat);
#ifdef COROUTINING
      DO_TRAIL(S_SREG, d1);
      if (S_SREG < H0) WakeUp(S_SREG);
  bind_ulfloat:
#endif
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_longint, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ulongint_unk);
    ulongint_nonvar:
      /* look inside term */
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->u.oc.c);
      PREG = NEXTOP(PREG, oc);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, pt0, ulongint_unk, ulongint_nonvar);
      BEGD(d1);
      d1 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(pt0, d1, bind_ulongint);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulongint:
#endif
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_longint, oc);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ullongint_unk);
    ullongint_nonvar:
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorLongInt) {
	FAIL();
      }
      ENDD(d0);
      BEGP(pt1);
      pt1 = RepAppl(PREG->u.oc.c);
      PREG = NEXTOP(PREG, oc);
      if (pt1[1] != pt0[1]) FAIL();
      ENDP(pt1);
      ENDP(pt0);
      GONext();

      derefa_body(d0, S_SREG, ullongint_unk, ullongint_nonvar);
      BEGD(d1);
      d1 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(S_SREG, d1, bind_ullongint);
#ifdef COROUTINING
      DO_TRAIL(S_SREG, d1);
      if (S_SREG < H0) WakeUp(S_SREG);
  bind_ullongint:
#endif
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      Op(unify_bigint, oc);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG++;
      d0 = *pt0;
      deref_head(d0, ubigint_unk);
    ubigint_nonvar:
      /* look inside term */
#ifdef USE_GMP
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorBigInt)
#endif
      {
	FAIL();
      }
#ifdef USE_GMP
      ENDD(d0);
      if (mpz_cmp(BigIntOfTerm(d0),BigIntOfTerm(PREG->u.oc.c)))
	FAIL();
#endif
      PREG = NEXTOP(PREG, oc);
#ifdef USE_GMP
      ENDP(pt0);
#endif
      GONext();

      derefa_body(d0, pt0, ubigint_unk, ubigint_nonvar);
      BEGD(d1);
      d1 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(pt0, d1, bind_ubigint);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
  bind_ubigint:
#endif
      GONext();
      ENDD(d1);
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(unify_l_bigint, oc);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = *S_SREG;
      deref_head(d0, ulbigint_unk);
    ulbigint_nonvar:
#ifdef USE_GMP
      BEGP(pt0);
      pt0 = RepAppl(d0);
      BEGD(d0);
      d0 = *pt0;
      if (d0 != (CELL)FunctorBigInt)
#endif
      {
	FAIL();
      }
#ifdef USE_GMP
      ENDD(d0);
      if (mpz_cmp(BigIntOfTerm(d0),BigIntOfTerm(PREG->u.oc.c)))
	FAIL();
#endif
      PREG = NEXTOP(PREG, oc);
#ifdef USE_GMP
      ENDP(pt0);
#endif
      GONext();

      derefa_body(d0, S_SREG, ulbigint_unk, ulbigint_nonvar);
      BEGD(d1);
      d1 = PREG->u.oc.c;
      PREG = NEXTOP(PREG, oc);
      BIND_GLOBAL(S_SREG, d1, bind_ulbigint);
#ifdef COROUTINING
      DO_TRAIL(S_SREG, d1);
      if (S_SREG < H0) WakeUp(S_SREG);
  bind_ulbigint:
#endif
      GONext();
      ENDD(d1);
      ENDCACHE_S();
      ENDD(d0);
      ENDOp();

      OpW(unify_list_write, o);
      PREG = NEXTOP(PREG, o);
      BEGD(d0);
      d0 = AbsPair(H);
      CACHE_S();
      READ_IN_S();
      SP -= 2;
      SP[0] = WRITE_MODE;
      SP[1] = Unsigned(S_SREG + 1);
      S_SREG[0] = d0;
      S_SREG = H;
      H = S_SREG + 2;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONextW();
      ENDD(d0);
      ENDOpW();

      OpRW(unify_list, o);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulist_unk);
    ulist_nonvar:
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      START_PREFETCH(o);
      SREG = RepPair(d0);
      PREG = NEXTOP(PREG, o);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ulist_unk, ulist_nonvar);
      /* we enter write mode */
      START_PREFETCH_W(o);
      CACHE_S();
      READ_IN_S();
      S_SREG = H;
      PREG = NEXTOP(PREG, o);
      H = S_SREG + 2;
      d0 = AbsPair(S_SREG);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      BIND_GLOBAL(pt0, d0, bind_ulist_var);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
  bind_ulist_var:
#endif
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_list_write, o);
      /* we continue in write mode */
      BEGD(d0);
      d0 = AbsPair(H);
      PREG = NEXTOP(PREG, o);
      CACHE_S();
      READ_IN_S();
      S_SREG[0] = d0;
      S_SREG = H;
      H = S_SREG + 2;
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONextW();
      ENDD(d0);
      ENDOpW();

      OpRW(unify_l_list, o);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ullist_unk);
    ullist_nonvar:
      START_PREFETCH(o);
      if (!IsPairTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      PREG = NEXTOP(PREG, o);
      SREG = RepPair(d0);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ullist_unk, ullist_nonvar);
      /* we enter write mode */
      START_PREFETCH_W(o);
      PREG = NEXTOP(PREG, o);
      CACHE_S();
      READ_IN_S();
      S_SREG = H;
      H = S_SREG + 2;
      d0 = AbsPair(S_SREG);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      BIND_GLOBAL(pt0, d0, bind_ullist_var);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
  bind_ullist_var:
#endif
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);
      ENDD(d0);
      ENDOpRW();

      OpW(unify_struct_write, of);
      CACHE_S();
      READ_IN_S();
      *--SP = Unsigned(S_SREG + 1);
      *--SP = WRITE_MODE;
      /* we continue in write mode */
      BEGD(d0);
      d0 = AbsAppl(H);
      S_SREG[0] = d0;
      S_SREG = H;
      d0 = (CELL) (PREG->u.of.f);
      *S_SREG++ = d0;
      H = S_SREG + PREG->u.of.a;
      PREG = NEXTOP(PREG, of);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      OpRW(unify_struct, of);
      *--SP = Unsigned(SREG + 1);
      *--SP = READ_MODE;
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      START_PREFETCH(of);
      deref_head(d0, ustruct_unk);
    ustruct_nonvar:
      /* we are in read mode */
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      CACHE_S();
      READ_IN_S();
      /* we continue in read mode */
      S_SREG = RepAppl(d0);
      /* just check functor */
      d0 = (CELL) (PREG->u.of.f);
      if (*S_SREG != d0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, of);
      WRITEBACK_S(S_SREG+1);
      ENDCACHE_S();
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ustruct_unk, ustruct_nonvar);
      /* Enter Write mode */
      START_PREFETCH_W(of);
      /* set d1 to be the new structure we are going to create */
      BEGD(d1);
      d1 = AbsAppl(H);
      /* we know the variable must be in the heap */
      BIND_GLOBAL(pt0, d1, bind_ustruct);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_ustruct:
#endif
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = H;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->u.of.f);
      *pt0++ = d0;
      H = pt0 + PREG->u.of.a;
      PREG = NEXTOP(PREG, of);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();

      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

      OpW(unify_l_struc_write, of);
      BEGD(d0);
      d0 = AbsAppl(H);
      CACHE_S();
      READ_IN_S();
      S_SREG[0] = d0;
      S_SREG = H;
      d0 = (CELL) (PREG->u.of.f);
      *S_SREG++ = d0;
      H = S_SREG + PREG->u.of.a;
      PREG = NEXTOP(PREG, of);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      ENDD(d0);
      GONextW();
      ENDOpW();

      OpRW(unify_l_struc, of);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, ulstruct_unk);
    ulstruct_nonvar:
      /* we are in read mode */
      START_PREFETCH(of);
      if (!IsApplTerm(d0)) {
	FAIL();
      }
      /* we continue in read mode */
      SREG = RepAppl(d0);
      /* just check functor */
      d0 = (CELL) (PREG->u.of.f);
      if (*SREG++ != d0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, of);
      GONext();
      END_PREFETCH();

      derefa_body(d0, pt0, ulstruct_unk, ulstruct_nonvar);
      /* Enter Write mode */
      /* set d1 to be the new structure we are going to create */
      START_PREFETCH_W(of);
      BEGD(d1);
      d1 = AbsAppl(H);
      /* we know the variable must be in the heap */
      BIND_GLOBAL(pt0, d1, bind_ulstruct);
#ifdef COROUTINING
      DO_TRAIL(pt0, d1);
      if (pt0 < H0) WakeUp(pt0);
    bind_ulstruct:
#endif
      /* now, set pt0 to point to the heap where we are going to
       * build our term */
      pt0 = H;
      ENDD(d1);
      /* first, put the functor */
      d0 = (CELL) (PREG->u.of.f);
      *pt0++ = d0;
      H = pt0 + PREG->u.of.a;
      PREG = NEXTOP(PREG, of);
      /* set SREG */
      SREG = pt0;
      /* update H */
      GONextW();
      END_PREFETCH_W();
      ENDP(pt0);

      ENDD(d0);
      ENDOpRW();

/************************************************************************\
* Put Instructions							 *
\************************************************************************/

      Op(put_x_var, xx);
      BEGP(pt0);
      pt0 = H;
      XREG(PREG->u.xx.xl) = Unsigned(pt0);
      H = pt0 + 1;
      XREG(PREG->u.xx.xr) = Unsigned(pt0);
      PREG = NEXTOP(PREG, xx);
      RESET_VARIABLE(pt0);
      ENDP(pt0);
      GONext();
      ENDOp();

      Op(put_y_var, yx);
      BEGP(pt0);
      pt0 = Y + PREG->u.yx.y;
      XREG(PREG->u.yx.x) = (CELL) pt0;
      PREG = NEXTOP(PREG, yx);
#if defined(SBA) && defined(FROZEN_REGS)
      /* We must initialise a shared variable to point to the SBA */
      if (Unsigned((Int)(pt0)-(Int)(H_FZ)) >
	  Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	*pt0 =  (CELL)STACK_TO_SBA(pt0);
      } else
#endif
	RESET_VARIABLE(pt0);
      ENDP(pt0);
      GONext();
      ENDOp();

      Op(put_x_val, xx);
      BEGD(d0);
      d0 = XREG(PREG->u.xx.xl);
      XREG(PREG->u.xx.xr) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, xx);
      GONext();
      ENDOp();

      Op(put_y_val, yx);
      BEGD(d0);
      d0 = Y[PREG->u.yx.y];
#if SBA
      if (d0 == 0) /* new variable */
	XREG(PREG->u.yx.x) = (CELL)(Y+PREG->u.yx.y);
      else
#endif
	XREG(PREG->u.yx.x) = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, yx);
      GONext();
      ENDOp();

      Op(put_unsafe, yx);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y+PREG->u.yx.y;
      d0 = *pt0;
      deref_head(d0, punsafe_unk);
    punsafe_nonvar:
      XREG(PREG->u.yx.x) = d0;
      PREG = NEXTOP(PREG, yx);
      GONext();

      derefa_body(d0, pt0, punsafe_unk, punsafe_nonvar);
      /* d0 is a variable, check whether we need to globalise it */
      if (pt0 <= H || pt0 >= Y) {
	/* variable is safe */
	XREG(PREG->u.yx.x) = Unsigned(pt0);
	PREG = NEXTOP(PREG, yx);
	GONext();
      }
      else {
	/* create a new Heap variable and bind our variable to it */
	Bind_Local(pt0, Unsigned(H));
	XREG(PREG->u.yx.x) = (CELL) H;
	RESET_VARIABLE(H);
	H++;
	PREG = NEXTOP(PREG, yx);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(put_atom, xc);
      BEGD(d0);
      d0 = PREG->u.xc.c;
      XREG(PREG->u.xc.x) = d0;
      PREG = NEXTOP(PREG, xc);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(put_list, x);
      CACHE_S();
      READ_IN_S();
      S_SREG = H;
      H += 2;
      BEGD(d0);
      d0 = AbsPair(S_SREG);
      XREG(PREG->u.x.x) = d0;
      PREG = NEXTOP(PREG, x);
      ENDD(d0);
      WRITEBACK_S(S_SREG);
      ENDCACHE_S();
      GONext();
      ENDOp();

      Op(put_struct, xf);
      BEGD(d0);
      d0 = AbsAppl(H);
      XREG(PREG->u.xf.x) = d0;
      d0 = (CELL) (PREG->u.xf.f);
      *H++ = d0;
      SREG = H;
      H += PREG->u.xf.a;
      ENDD(d0);
      PREG = NEXTOP(PREG, xf);
      GONext();
      ENDOp();

/************************************************************************\
* 	Write Instructions						*
\************************************************************************/

      Op(write_x_var, x);
      XREG(PREG->u.x.x) = Unsigned(SREG);
      PREG = NEXTOP(PREG, x);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_void, e);
      PREG = NEXTOP(PREG, e);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_n_voids, s);
      BEGD(d0);
      d0 = PREG->u.s.s;
      PREG = NEXTOP(PREG, s);
      for (; d0 > 0; d0--) {
	RESET_VARIABLE(SREG);
	SREG++;
      }
      ENDD(d0);
      GONext();
      ENDOp();

      Op(write_y_var, y);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.y.y,Unsigned(SREG));
#else
      Y[PREG->u.y.y] = Unsigned(SREG);
#endif
      PREG = NEXTOP(PREG, y);
      RESET_VARIABLE(SREG);
      SREG++;
      GONext();
      ENDOp();

      Op(write_x_val, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, x);
      GONext();
      ENDOp();

      Op(write_x_loc, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, w_x_unk);
    w_x_bound:
      *SREG++ = d0;
      PREG = NEXTOP(PREG, x);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, w_x_unk, w_x_bound);
#if defined(SBA) && defined(FROZEN_REGS)
      if (pt0 > H && pt0<(CELL *)B_FZ) {
#else
      if (pt0 > H) {
#endif
	PREG = NEXTOP(PREG, x);
	/* local variable: let us bind it to the list */
#ifdef FROZEN_REGS  /* TRAIL */
	Bind_Local(pt0, Unsigned(SREG));
#else
	TRAIL_LOCAL(pt0, Unsigned(SREG));
	*pt0 = Unsigned(SREG);
#endif /* FROZEN_REGS */
	RESET_VARIABLE(SREG);
	SREG++;
	GONext();
      }
      else {
	PREG = NEXTOP(PREG, x);
	*SREG++ = Unsigned(pt0);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(write_y_val, y);
      BEGD(d0);
      d0 = Y[PREG->u.y.y];
#if SBA
      if (d0 == 0) /* new variable */
	*SREG++ = (CELL)(Y+PREG->u.y.y);
      else
#endif
	*SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDOp();

      Op(write_y_loc, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y+PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, w_y_unk);
    w_y_bound:
      PREG = NEXTOP(PREG, y);
      *SREG++ = d0;
      GONext();

      derefa_body(d0, pt0, w_y_unk, w_y_bound);
#if defined(SBA) && defined(FROZEN_REGS)
      if (pt0 > H && pt0<(CELL *)B_FZ) {
#else
      if (pt0 > H) {
#endif
	PREG = NEXTOP(PREG, y);
	/* local variable: let us bind it to the list */
#ifdef FROZEN_REGS
	Bind_Local(pt0, Unsigned(SREG));
#else
	*pt0 = Unsigned(SREG);
	TRAIL_LOCAL(pt0, Unsigned(SREG));
#endif /* FROZEN_REGS */
	RESET_VARIABLE(SREG);
	SREG++;
	GONext();
      }
      else {
	PREG = NEXTOP(PREG, y);
	*SREG++ = Unsigned(pt0);
	GONext();
      }
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(write_atom, c);
      BEGD(d0);
      d0 = PREG->u.c.c;
      *SREG++ = d0;
      ENDD(d0);
      PREG = NEXTOP(PREG, c);
      GONext();
      ENDOp();

      Op(write_n_atoms, sc);
      BEGD(d0);
      BEGD(d1);
      d0 = PREG->u.sc.s;
      d1 = PREG->u.sc.c;
      for (; d0 > 0; d0--)
	*SREG++ = d1;
      ENDD(d1);
      ENDD(d0);
      PREG = NEXTOP(PREG, sc);
      GONext();
      ENDOp();

      Op(write_list, e);
      BEGD(d0);
      d0 = AbsPair(H);
      *SREG++ = d0;
      /* I will not actually store the mode in the stack */
      SP[-1] = Unsigned(SREG);
      SP[-2] = 1;		/* Put instructions follow the main stream */
      SP -= 2;
      SREG = H;
      H += 2;
      ENDD(d0);
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

      Op(write_l_list, e);
      ALWAYS_START_PREFETCH(e);
      PREG = NEXTOP(PREG, e);
      BEGD(d0);
      CACHE_S();
      READ_IN_S();
      d0 = AbsPair(H);
      *S_SREG = d0;
      WRITEBACK_S(H);
      H += 2;
      ENDCACHE_S();
      ENDD(d0);
      ALWAYS_END_PREFETCH();
      GONext();
      ENDOp();

      Op(write_struct, f);
      BEGD(d0);
      d0 = AbsAppl(H);
      *SREG++ = d0;
      SP[-1] = Unsigned(SREG);
      SP[-2] = 1;		/* Put instructions follow the main stream */
      SP -= 2;
      d0 = (CELL) (PREG->u.f.f);
      *H++ = d0;
      ENDD(d0);
      BEGD(d0);
      d0 = PREG->u.f.a;
      PREG = NEXTOP(PREG, f);
      SREG = H;
      H += d0;
      ENDD(d0);
      GONext();
      ENDOp();

      Op(write_l_struc, f);
      BEGD(d0);
      d0 = AbsAppl(H);
      *SREG = d0;
      d0 = (CELL) (PREG->u.f.f);
      *H++ = d0;
      SREG = H;
      ENDD(d0);
      BEGD(d0);
      d0 = PREG->u.f.a;
      PREG = NEXTOP(PREG, f);
      H += d0;
      ENDD(d0);
      GONext();
      ENDOp();

/************************************************************************\
*   Save last unified struct or list					*
\************************************************************************/

/* vitor: I think I should kill these two instructions, by expanding the
 * othe instructions.
 */

      OpW(save_pair_x_write, ox);
      XREG(PREG->u.ox.x) = AbsPair(SREG);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      Op(save_pair_x, ox);
      XREG(PREG->u.ox.x) = AbsPair(SREG);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      OpW(save_pair_y_write, oy);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.oy.y,AbsPair(SREG));
#else
      Y[PREG->u.oy.y] = AbsPair(SREG);
#endif
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();

      Op(save_pair_y, oy);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.oy.y,AbsPair(SREG));
#else
      Y[PREG->u.oy.y] = AbsPair(SREG);
#endif
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

      OpW(save_appl_x_write, ox);
      XREG(PREG->u.ox.x) = AbsAppl(SREG - 1);
      PREG = NEXTOP(PREG, ox);
      GONextW();
      ENDOpW();

      Op(save_appl_x, ox);
      XREG(PREG->u.ox.x) = AbsAppl(SREG - 1);
      PREG = NEXTOP(PREG, ox);
      GONext();
      ENDOp();

      OpW(save_appl_y_write, oy);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.oy.y,AbsAppl(SREG-1));
#else
      Y[PREG->u.oy.y] = AbsAppl(SREG - 1);
#endif
      PREG = NEXTOP(PREG, oy);
      GONextW();
      ENDOpW();

      Op(save_appl_y, oy);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(Y+PREG->u.oy.y,AbsAppl(SREG-1));
#else
      Y[PREG->u.oy.y] = AbsAppl(SREG - 1);
#endif
      PREG = NEXTOP(PREG, oy);
      GONext();
      ENDOp();

/************************************************************************\
*   Instructions for implemeting 'or;'					 *
\************************************************************************/

      BOp(jump, l);
      PREG = (yamop *) (PREG->u.l.l);
      JMPNext();
      ENDBOp();

      /* This instruction is called when the previous goal
	 was interrupted when waking up goals
      */	 
      BOp(move_back, l);
      PREG = (yamop *)(((char *)PREG)-(Int)(NEXTOP((yamop *)NULL,sla)));
      JMPNext();
      ENDBOp();

      /* This instruction is called when the previous goal
	 was interrupted when waking up goals
      */	 
      BOp(skip, l);
      PREG = NEXTOP(PREG,l);
      JMPNext();
      ENDBOp();

      Op(either, sla);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	low_level_trace(try_or, (PredEntry *)PREG, NULL);
      }
#endif
#ifdef COROUTINING
      CACHE_Y_AS_ENV(Y);
      check_stack(NoStackEither, H);
      ENDCACHE_Y_AS_ENV();
      either_notest:
#endif
      BEGD(d0);
      /* Try to preserve the environment */
      d0 = PREG->u.sla.s;
      BEGCHO(pt1);
      pt1 = (choiceptr) ((char *) Y + (YREG) d0);
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (pt1 > top_b || pt1 < (choiceptr)H) pt1 = top_b;
#else
	if (pt1 > top_b) pt1 = top_b;
#endif
      }
#else
      if (pt1 > B) {
	pt1 = B;
      }
#endif /* FROZEN_REGS */
      pt1 = (choiceptr)(((CELL *) pt1)-1);
      *(CELL **) pt1 = Y;
      store_yaam_regs_for_either(PREG->u.sla.l, PREG);
      SREG = (CELL *) (B = pt1);
#ifdef YAPOR
      SCH_set_load(pt1);
#endif	/* YAPOR */
      SET_BB(pt1);
      ENDCHO(pt1);
      /* skip the current instruction plus the next one */
      PREG = NEXTOP(NEXTOP(PREG, sla),l);
      GONext();
      ENDD(d0);
      ENDOp();

      Op(or_else, sla);
      H = HBREG = PROTECT_FROZEN_H(B);
      ENV = B->cp_env;
      B->cp_cp = PREG;
#ifdef DEPTH_LIMIT
      DEPTH = B->cp_depth;
#endif	/* DEPTH_LIMIT */
      SET_BB(PROTECT_FROZEN_B(B));
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_new_alternative(PREG, (yamop *) (PREG->u.sla.l));
      } else
#endif	/* YAPOR */
      B->cp_ap = (yamop *) PREG->u.sla.l;
      PREG = NEXTOP(PREG, sla);
      Y = (CELL *) B->cp_a1;
      GONext();
      ENDOp();

#ifdef YAPOR
      Op(or_last, sla);
#else
      Op(or_last, e);
#endif	/* YAPOR */
      BEGCHO(pt0);
      pt0 = B;
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	H = HBREG = PROTECT_FROZEN_H(pt0);
	Y = (CELL *) pt0->cp_a1;
	ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
	DEPTH = pt0->cp_depth;
#endif	/* DEPTH_LIMIT */
        SCH_new_alternative(PREG, NULL);
      }
      else
#endif	/* YAPOR */
      {
	B = pt0->cp_b;
	H = PROTECT_FROZEN_H(pt0);
	Y = (CELL *) pt0->cp_a1;
	ENV = pt0->cp_env;
#ifdef DEPTH_LIMIT
	DEPTH = pt0->cp_depth;
#endif	/* DEPTH_LIMIT */
	HBREG = PROTECT_FROZEN_H(B);
      }
#ifdef YAPOR
      PREG = NEXTOP(PREG, sla);
#else
      PREG = NEXTOP(PREG, e);
#endif	/* YAPOR */
      SET_BB(PROTECT_FROZEN_B(B));
      GONext();
      ENDCHO(pt0);
      ENDOp();

/************************************************************************\
*	Pop operations							 *
\************************************************************************/

      OpRW(pop_n, s);
      /* write mode might have been called from read mode */
      BEGD(d0);
      d0 = PREG->u.os.s;
      SP = (CELL *) (((char *) SP) + d0);
      ENDD(d0);
      BEGD(d0);
      d0 = SP[0];
      if (d0) {
	START_PREFETCH(s);
	SREG = (CELL *) (SP[1]);
	SP += 2;
	PREG = NEXTOP(PREG, s);
	GONext();
	END_PREFETCH();
      }
      else {
	START_PREFETCH_W(s);
	SREG = (CELL *) (SP[1]);
	SP += 2;
	PREG = NEXTOP(PREG, s);
	GONextW();
	END_PREFETCH_W();
      }
      ENDD(d0);
      ENDOpRW();

      OpRW(pop, e);
      BEGD(d0);
      d0 = SP[0];
      SREG = (CELL *) (SP[1]);
      SP += 2;
      if (d0) {
	START_PREFETCH(e);
	PREG = NEXTOP(PREG, e);
	GONext();
	END_PREFETCH();
      }
      else {
	START_PREFETCH_W(e);
	PREG = NEXTOP(PREG, e);
	GONextW();
	END_PREFETCH_W();
      }
      ENDD(d0);
      ENDOpRW();

/************************************************************************\
*	Call C predicates instructions					 *
\************************************************************************/

      BOp(call_cpred, sla);
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) ASP = (CELL *)top_b;
#else
	if (Y > (CELL *) top_b) ASP = (CELL *)top_b;
#endif
	else ASP = (CELL *)(((char *)Y) +  PREG->u.sla.s);
      }
#else
      if (Y > (CELL *) B) {
	ASP = (CELL *) B;
      } else {
	ASP = (CELL *) (((char *) Y) + PREG->u.sla.s);
      }
#endif /* FROZEN_REGS */
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,(PredEntry *)(PREG->u.sla.p),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = (CELL) (PREG->u.sla.l);
      PREG = NEXTOP(PREG, sla);
      saveregs();
      SREG = (CELL *) (*((Int (*)(void)) d0)) ();
      ENDD(d0);


      setregs();
      if (!SREG) {
	FAIL();
      }
      CACHE_A1();
      JMPNext();
      ENDBOp();
      
      /* Like previous, the only difference is that we do not */
      /* trust the C-function we are calling and hence we must */
      /* guarantee that *all* machine registers are saved and */
      /* restored */
      BOp(call_usercpred, sla);
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) ASP = (CELL *) top_b;
#else
	if (Y > (CELL *) top_b) ASP = (CELL *) top_b;
#endif
	else ASP = (CELL *)(((char *)Y) +  PREG->u.sla.s);
      }
#else
      if (Y > (CELL *) B)
	ASP = (CELL *) B;
      else {
	ASP = (CELL *) (((char *) Y) + PREG->u.sla.s);
      }
#endif /* FROZEN_REGS */
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,(PredEntry *)(PREG->u.sla.p),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      {
	CODEADDR p = PREG->u.sla.p;
	PREG = NEXTOP(PREG, sla);
	saveregs();
	save_machine_regs();

	SREG = (CELL *) YapExecute((CPredicate)(((PredEntry *)p)->CodeOfPred));
      }

      restore_machine_regs();
      setregs();
      if (!SREG) {
	FAIL();
      }
      JMPNext();
      ENDBOp();

      BOp(call_c_wfail, sdl);
#ifdef FROZEN_REGS
      { 
	choiceptr top_b = PROTECT_FROZEN_B(B);
#ifdef SBA
	if (Y > (CELL *) top_b || Y < H) ASP = (CELL *) top_b;
#else
	if (Y > (CELL *) top_b) ASP = (CELL *) top_b;
#endif
	else {
	  BEGD(d0);
	  d0 = PREG->u.sdl.s;
	  ASP = ((CELL *)Y) + d0;
	  ENDD(d0);
	}
      }
#else
      if (Y > (CELL *) B)
	ASP = (CELL *) B;
      else {
	BEGD(d0);
	d0 = PREG->u.sdl.s;
	ASP = ((CELL *) Y) + d0;
	ENDD(d0);
      }
#endif /* FROZEN_REGS */
      BEGD(d0);
      d0 = (CELL) (PREG->u.sdl.d);
      saveregs();
      SREG = (CELL *) (*((Int (*)(void)) d0)) ();
      ENDD(d0);
      setregs();
      if (!SREG)
	PREG = (yamop *) (PREG->u.sdl.l);
      else
	PREG = NEXTOP(PREG, sdl);
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(try_c, lds);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(Y);
      S_Y = S_Y - PREG->u.lds.extra;
      store_args(PREG->u.lds.s);
      store_yaam_regs(NEXTOP(PREG, lds), 0);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();

    TRYCC:
      ASP = (CELL *)B;
      saveregs();

      BEGD(d0);
      d0 = (CELL)PREG->u.lds.d;
      SREG = (CELL *) (*((Int (*)(void)) (d0))) ();
      ENDD(d0);

      setregs();
      if (!SREG) {
	FAIL();
      }
      if ((CELL *) B == Y && ASP != (CELL *) B) {
	/* as Luis says, the predicate that did the try C might
	 * have left some data on the stack. We should preserve
	 * it, unless the builtin also did cut */
	Y = ASP;
	HBREG = PROTECT_FROZEN_H(B);
	SET_BB(B);
      }
      PREG = (yamop *) CPREG;
      Y = ENV;
      JMPNext();
      ENDBOp();

      BOp(retry_c, lds);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(B);
      CPREG = B_Y->cp_cp;
      ENV = B_Y->cp_env;
      H = PROTECT_FROZEN_H(B);
#ifdef DEPTH_LIMIT
      DEPTH =B->cp_depth;
#endif
      HBREG = H;
      restore_args(PREG->u.lds.s);
      ENDCACHE_Y();
      goto TRYCC;
      ENDBOp();

      BOp(try_userc, lds);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(Y);
      S_Y = S_Y - PREG->u.lds.extra;
      store_args(PREG->u.lds.s);
      store_yaam_regs(NEXTOP(PREG, lds), 0);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif
      SET_BB(B_Y);
      ENDCACHE_Y();

    TRYUSERCC:
      ASP = YENV;
      saveregs();
      save_machine_regs();
      {
	CPredicate p;

	p = (CPredicate)(PREG->u.lds.d);
	SREG = (CELL *) YapExecute(p);
      }
      restore_machine_regs();
      setregs();
      if (!SREG) {
	FAIL();
      }
      if ((CELL *) B == Y && ASP != (CELL *) B) {
	/* as Luis says, the predicate that did the try C might
	 * have left some data on the stack. We should preserve
	 * it, unless the builtin also did cut */
	Y = ASP;
	HBREG = PROTECT_FROZEN_H(B);
      }
      PREG = (yamop *) CPREG;
      Y = ENV;
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(retry_userc, lds);
#ifdef YAPOR
      CUT_wait_leftmost();
#endif /* YAPOR */
      CACHE_Y(B);
      CPREG = B_Y->cp_cp;
      ENV = B_Y->cp_env;
      H = PROTECT_FROZEN_H(B);
#ifdef DEPTH_LIMIT
      DEPTH =B->cp_depth;
#endif
      HBREG = H;
      restore_args(PREG->u.lds.s);
      ENDCACHE_Y();
      goto TRYUSERCC;
      ENDBOp();

/************************************************************************\
*	support instructions             				 *
\************************************************************************/

      BOp(index_pred, e);
      saveregs();
      WRITE_LOCK(PredFromDefCode(PREG)->PRWLock);
#if defined(YAPOR) || defined(THREADS)
      /*
	we do not lock access to the predicate,
	we must take extra care here
      */
      /*
	Ideally, this code should only be executed by
	dynamic procedures in YAPOR. Unfortunately, it also seems
	to be executed when 
	running a procedure from within the file that defines it.
      */
      if (PredFromDefCode(PREG)->OpcodeOfPred != INDEX_OPCODE) {
	/* someone was here before we were */
	PREG = (yamop *) PredFromDefCode(PREG)->CodeOfPred;
	WRITE_UNLOCK(PredFromDefCode(PREG)->PRWLock);
	JMPNext();
      }
#endif 
      IPred((CODEADDR)PredFromDefCode(PREG));
      /* IPred can generate errors, it thus must get rid of the lock itself */
      setregs();
      CACHED_A1() = ARG1;
      PREG = (yamop *) PredFromDefCode(PREG)->CodeOfPred;
      JMPNext();
      ENDBOp();

      BOp(undef_p, e);
      /* save S for module name */
      { 
	PredEntry *pe = PredFromDefCode(PREG);
	BEGD(d0);
	READ_LOCK(pe->PRWLock);
	/* avoid trouble with undefined dynamic procedures */
	if (pe->PredFlags & (DynamicPredFlag|LogUpdatePredFlag)) {
	  READ_UNLOCK(pe->PRWLock);
	  FAIL();
	}
	READ_UNLOCK(pe->PRWLock);
	d0 = pe->ArityOfPE;
	if (d0 == 0) {
	  H[1] = MkAtomTerm(((Atom) pe->FunctorOfPred));
	}
	else {
	  H[d0 + 2] = AbsAppl(H);
	  *H = (CELL) pe->FunctorOfPred;
	  H++;
	  BEGP(pt1);
	  pt1 = XREGS + 1;
	  for (; d0 > 0; --d0) {
	    BEGD(d1);
	    BEGP(pt0);
	    pt0 = pt1++;
	    d1 = *pt0;
	    deref_head(d1, undef_unk);
	  undef_nonvar:
	    /* just copy it to the heap */
	    *H++ = d1;
	    continue;

	    derefa_body(d1, pt0, undef_unk, undef_nonvar);
	    if (pt0 <= H) {
	      /* variable is safe */
	      *H++ = (CELL)pt0;
	    } else {
	      /* bind it, in case it is a local variable */
	      d1 = Unsigned(H);
	      RESET_VARIABLE(H);
	      H += 1;
	      Bind_Local(pt0, d1);
	    }
	    ENDP(pt0);
	    ENDD(d1);
	  }
	  ENDP(pt1);
	}
	ENDD(d0);
	H[0] = Module_Name((CODEADDR)pe);
	ARG1 = (Term) AbsPair(H);
	H += 2;
      }

      if (UndefCode == NULL) {
	Atom at;

	at = FullLookupAtom("$undefp");
	{
	  Prop p = GetPredProp (at, 1);
	  if (p == NIL) {
	    CFREG = CalculateStackGap();
	    FAIL();
	  } else {
	    PredEntry *undefpe;
	    undefpe = RepPredProp (p);
	    READ_LOCK(undefpe->PRWLock);
	    UndefCode = (CELL *) & (undefpe->CodeOfPred);
	    READ_UNLOCK(undefpe->PRWLock);
	  }
	}
      }
      PREG = (yamop *)pred_entry_from_code(UndefCode)->CodeOfPred;
      CFREG = CalculateStackGap();
      CACHE_A1();
      JMPNext();
      ENDBOp();

      BOp(spy_pred, e);
      {
	PredEntry *pe = PredFromDefCode(PREG);
	if (!(FlipFlop ^= 1)) {
	  READ_LOCK(pe->PRWLock);
	  PREG = (yamop *) pe->TrueCodeOfPred;
	  READ_UNLOCK(pe->PRWLock);
	  JMPNext();
	}
	ENDBOp();
      }

    dospy:
      {
	PredEntry *pe = PredFromDefCode(PREG);
	BEGD(d0);
	d0 = pe->ArityOfPE;
      /* save S for ModuleName */
	if (d0 == 0) {
	  H[1] = MkAtomTerm((Atom) pe->FunctorOfPred);
	} else {
	  *H = (CELL) pe->FunctorOfPred;
	  H[d0 + 2] = AbsAppl(H);
	  H++;
	  BEGP(pt1);
	  pt1 = XREGS + 1;
	  for (; d0 > 0; --d0) {
	    BEGD(d1);
	    BEGP(pt0);
	    pt0 = pt1++;
	    d1 = *pt0;
	    deref_head(d1, dospy_unk);
	  dospy_nonvar:
	    /* just copy it to the heap */
	    *H++ = d1;
	    continue;
	    
	    derefa_body(d1, pt0, dospy_unk, dospy_nonvar);
	    if (pt0 <= H) {
	      /* variable is safe */
	      *H++ = (CELL)pt0;
	    } else {
	      /* bind it, in case it is a local variable */
	      d1 = Unsigned(H);
	      RESET_VARIABLE(H);
	      H += 1;
	      Bind_Local(pt0, d1);
	    }
	    ENDP(pt0);
	    ENDD(d1);
	  }
	  ENDP(pt1);
	}
	ENDD(d0);
	H[0] = Module_Name((CODEADDR)pe);
      }
      ARG1 = (Term) AbsPair(H);
      H += 2;
      BEGP(pt0);
      pt0 = (CELL *) (Unsigned(SpyCode) - sizeof(SMALLUNSGN));
      PREG = (yamop *) PredCode(pt0);
      CACHE_A1();
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,(PredEntry *)(PREG->u.sla.p),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      ENDP(pt0);
      JMPNext();

/************************************************************************\
* 	Try / Retry / Trust for main indexing blocks			*
\************************************************************************/

      BOp(try_clause, ld);
      check_trail();
      CACHE_Y(Y);
      /* Point AP to the code that follows this instruction */
      store_at_least_one_arg(PREG->u.ld.s);
      store_yaam_regs(NEXTOP(PREG, ld), 0);
      PREG = (yamop *) (PREG->u.ld.d);
      set_cut(S_Y, B);
      B = B_Y;
#ifdef YAPOR
      SCH_set_load(B_Y);
#endif	/* YAPOR */
      SET_BB(B_Y);
      ENDCACHE_Y();
      JMPNext();
      ENDBOp();

      /* do a jump, but make sure the alternative pointer is set to
	 point to the next instruction */
      BOp(try_in, l);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_new_alternative(PREG, NEXTOP(PREG,l));
      } else
#endif /* YAPOR */
      B->cp_ap = NEXTOP(PREG,l);
      PREG = (yamop *) (PREG->u.l.l);
      JMPNext();
      ENDBOp();

      BOp(retry, ld);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, ld));
      restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = (yamop *) (PREG->u.ld.d);
      JMPNext();
      ENDBOp();

      BOp(trust_in, ldl);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ldl.bl);
      restore_at_least_one_arg(PREG->u.ldl.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = (yamop *) (PREG->u.ldl.d);
      JMPNext();
      ENDBOp();

      BOp(trust, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	pop_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      PREG = (yamop *) (PREG->u.ld.d);
      JMPNext();
      ENDBOp();

/************************************************************************\
* retry_first and trust_first go straight to the first arg.             *
\************************************************************************/

      /* relies  on an extra S */
      BOp(retry_first, ld);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, ld));
      restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      d0 = ARG1;
      /* der it first */
      PREG = (yamop *) (PREG->u.ld.d);
      deref_head(d0,retry_first_unk);
    retry_first_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	JMPNext();
#ifdef DEBUG
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0,"argument to retry_first is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
	/* pair */
	SREG = RepAppl(d0)+1;
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, retry_first_unk, retry_first_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, d0, "unbound argument to retry_first");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();


      /* just like retry_first, but set B->cp_ap to point to the
	 beginning of the next group */
      BOp(trust_first_in, ldl);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ldl.bl);
      restore_at_least_one_arg(PREG->u.ldl.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.lds.d);
      deref_head(d0,trust_first_in_unk);
    trust_first_in_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	JMPNext();
#ifdef DEBUG
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_first_in is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
	/* pair */
	SREG = RepAppl(d0)+1;
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, trust_first_in_unk, trust_first_in_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, d0,"unbound argument to trust_first_in");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      /* recover S and avoid doing a get_list or get_struct */
      BOp(trust_first, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif	/* YAPOR */
      {
	pop_yaam_regs();
	pop_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.ld.d);
      deref_head(d0,trust_first_unk);
    trust_first_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	JMPNext();
#ifdef DEBUG
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR,d0,"argument to trust_first is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
	/* pair */
	SREG = RepAppl(d0)+1;
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, trust_first_unk, trust_first_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR,(CELL)pt0,"argument to trust_first is a variable");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();


/************************************************************************\
* retry_tail and trust_tail go straight to the tail of a list           *
\************************************************************************/

      /* relies  on an extra S */
      BOp(retry_tail, ld);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, ld));
      restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      d0 = ARG1;
      /* deref it first */
      deref_head(d0,retry_tail_unk);
      PREG = (yamop *) (PREG->u.ld.d);
    retry_tail_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0)+1;
	JMPNext();
#ifdef DEBUG
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR,d0,"argument to retry_tail is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
#ifdef DEBUG
	/* this should never happen */
	PREG = Error(SYSTEM_ERROR,d0,"argument to retry_tail is a compound term");
	JMPNext();
#endif
      }

      BEGP(pt0);
      deref_body(d0, pt0, retry_tail_unk, retry_tail_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR,(CELL)pt0,"unbound argument to retry_tail");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();


      /* just like retry_tail, but set B->cp_ap to point to the
	 beginning of the next group */
      BOp(trust_tail_in, ldl);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ldl.bl);
      restore_at_least_one_arg(PREG->u.ldl.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.lds.d);
      deref_head(d0,trust_tail_in_unk);
    trust_tail_in_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0)+1;
	JMPNext();
#ifdef DEBUG
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_tail_in is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
#ifdef DEBUG
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_tail_in is a compound term");
	JMPNext();
#endif
      }

      BEGP(pt0);
      deref_body(d0, pt0, trust_tail_in_unk, trust_tail_in_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, (CELL)pt0, "unbound argument to trust_tail_in");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      /* recover S and avoid doing a get_list or get_struct */
      BOp(trust_tail, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif /* YAPOR */
      {
	pop_yaam_regs();
	pop_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.ld.d);
      deref_head(d0,trust_tail_unk);
    trust_tail_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0)+1;
	JMPNext();
#ifdef DEBUG
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_tail is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
#ifdef DEBUG
	/* this should ever happen */
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_tail is a constant");
	JMPNext();
#endif /* DEBUG */
      }

      BEGP(pt0);
      deref_body(d0, pt0, trust_tail_unk, trust_tail_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, (CELL)pt0, "unbound argument to trust_tail");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();


/************************************************************************\
* retry_head and trust_head assume the first argument is known          *
\************************************************************************/

      /* retry an instruction, and avoid a get and unify */
      BOp(retry_head, ld);
      CACHE_Y(B);
      restore_yaam_regs(NEXTOP(PREG, ld));
      restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      BEGP(pt0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.ld.d);
      deref_head(d0,retry_head_unk);
    retry_head_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	pt0 = RepPair(d0);
	/* get the head of the list or the first argument of the struct */
	d0 = *pt0;
      inner_retry_head:
	/* inform that we were reading a structure or list */
	SP[-1] = (CELL)(pt0+1);
	SP[-2] = READ_MODE;
	SP -= 2;
	deref_head(d0,retry_head_first_unk);
      retry_head_first_nvar:
	if (IsPairTerm(d0)) {
	  SREG = RepPair(d0);
	  JMPNext();
#ifdef DEBUG
	} else if (!IsApplTerm(d0)) {
	  /* this should not happen */
	  PREG = Error(SYSTEM_ERROR, d0, "constant argument to retry_head");
	  JMPNext();
#endif /* DEBUG */
	} else {
	  /* appl */
	  /* pair */
	  SREG = RepAppl(d0)+1;
	  JMPNext();
	}

	deref_body(d0, pt0, retry_head_first_unk, retry_head_first_nvar);
	/* this should never happen */
#ifdef DEBUG
	PREG = Error(SYSTEM_ERROR, (CELL)pt0, "unbound argument to retry_head");
	JMPNext();
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0, "constant argument to retry_head");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
	/* pair */
	pt0 = RepAppl(d0)+1;
	d0 = *pt0;
	goto inner_retry_head;
      }

      deref_body(d0, pt0, retry_head_unk, retry_head_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, (CELL)pt0, "unbound argument to retry_head");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      /* This is a retry head that closes a subblock */
      BOp(trust_head_in, ldl);
      CACHE_Y(B);
      restore_yaam_regs(PREG->u.ldl.bl);
      restore_at_least_one_arg(PREG->u.ldl.s);
#ifdef FROZEN_REGS
      B_Y = PROTECT_FROZEN_B(B_Y);
      set_cut(S_Y, B->cp_b);
#else
      set_cut(S_Y, B_Y->cp_b);
#endif /* FROZEN_REGS */
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      BEGP(pt0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.ldl.d);
      deref_head(d0,trust_head_in_unk);
    trust_head_in_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	pt0 = RepPair(d0);
	/* get the head of the list or the first argument of the struct */
	d0 = *pt0;
      inner_trust_head_in:
	/* inform that we were reading a structure or list */
	SP[-1] = (CELL)(pt0+1);
	SP[-2] = READ_MODE;
	SP -= 2;
	deref_head(d0,trust_head_in_first_unk);
      trust_head_in_first_nvar:
	if (IsPairTerm(d0)) {
	  SREG = RepPair(d0);
	  JMPNext();
#ifdef DEBUG
	} else if (!IsApplTerm(d0)) {
	  /* this should not happen */
	  PREG = Error(SYSTEM_ERROR, d0, "head of argument to trust_head_in is a constant");
	  JMPNext();
#endif /* DEBUG */
	} else {
	  /* appl */
	  /* pair */
	  SREG = RepAppl(d0)+1;
	  JMPNext();
	}

	deref_body(d0, pt0, trust_head_in_first_unk, trust_head_in_first_nvar);
	/* this should never happen */
#ifdef DEBUG
	PREG = Error(SYSTEM_ERROR, d0, "head of argument to trust_head_in is unbound");
	JMPNext();
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_head_in is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
	/* pair */
	pt0 = RepAppl(d0)+1;
	d0 = *pt0;
	goto inner_trust_head_in;
      }

      deref_body(d0, pt0, trust_head_in_unk, trust_head_in_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, d0, "unbound argument to trust_head_in");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();


      BOp(trust_head, ld);
      CACHE_Y(B);
#ifdef YAPOR
      if (SCH_top_shared_cp(B)) {
	SCH_last_alternative(PREG, B_Y);
	restore_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B->cp_b);
      }
      else
#endif /* YAPOR */
      {
	pop_yaam_regs();
	pop_at_least_one_arg(PREG->u.ld.s);
#ifdef FROZEN_REGS
	B_Y = PROTECT_FROZEN_B(B_Y);
#endif /* FROZEN_REGS */
	set_cut(S_Y, B);
      }
      SET_BB(B_Y);
      ENDCACHE_Y();
      /* recover the value of SREG */
      BEGD(d0);
      BEGP(pt0);
      d0 = ARG1;
      /* deref it first */
      PREG = (yamop *) (PREG->u.ld.d);
      deref_head(d0,trust_head_unk);
    trust_head_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	pt0 = RepPair(d0);
	/* get the head of the list or the first argument of the struct */
	d0 = *pt0;
      inner_trust_head:
	/* inform that we were reading a structure or list */
	SP[-1] = (CELL)(pt0+1);
	SP[-2] = READ_MODE;
	SP -= 2;
	deref_head(d0,trust_head_first_unk);
      trust_head_first_nvar:
	if (IsPairTerm(d0)) {
	  SREG = RepPair(d0);
	  JMPNext();
#ifdef DEBUG
	} else if (!IsApplTerm(d0)) {
	  /* this should not happen */
	  PREG = Error(SYSTEM_ERROR, d0, "head of argument to trust_head is a constant");
	  JMPNext();
#endif /* DEBUG */
	} else {
	  /* appl */
	  /* pair */
	  SREG = RepAppl(d0)+1;
	  JMPNext();
	}

	deref_body(d0, pt0, trust_head_first_unk, trust_head_first_nvar);
	/* this should never happen */
#ifdef DEBUG
	PREG = Error(SYSTEM_ERROR, d0, "unbound head of argument to trust_head");
	JMPNext();
      } else if (!IsApplTerm(d0)) {
	/* this should not happen */
	PREG = Error(SYSTEM_ERROR, d0, "argument to trust_head is a constant");
	JMPNext();
#endif /* DEBUG */
      } else {
	/* appl */
	/* pair */
	pt0 = RepAppl(d0)+1;
	d0 = *pt0;
	goto inner_trust_head;
      }

      deref_body(d0, pt0, trust_head_unk, trust_head_nvar);
      /* this should never happen */
#ifdef DEBUG
      PREG = Error(SYSTEM_ERROR, d0, "unbound argument to trust_head");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();


/************************************************************************\
* 	Indexing in ARG1						*
\************************************************************************/

      BOp(switch_on_type, llll);
      BEGD(d0);
      d0 = CACHED_A1();
      deref_head(d0, swt_unk);
      /* nonvar */
    swt_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	PREG = (yamop *) (PREG->u.llll.l1);
	JMPNext();
      }
      else if (!IsApplTerm(d0)) {
	/* constant */
	PREG = (yamop *) (PREG->u.llll.l2);
	I_R = d0;
	JMPNext();
      }
      else {
	/* appl */
	PREG = (yamop *) (PREG->u.llll.l3);
	SREG = RepAppl(d0);
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, swt_unk, swt_nvar);
      /* variable */
      PREG = (yamop *) (PREG->u.llll.l4);
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      BOp(switch_on_nonv, lll);
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, swnv_unk);
    swnv_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	PREG = (yamop *) (PREG->u.lll.l1);
	JMPNext();
      }
      else if (!IsApplTerm(d0)) {
	/* constant */
	PREG = (yamop *) (PREG->u.lll.l2);
	I_R = d0;
	JMPNext();
      }
      else {
	/* appl */
	PREG = (yamop *) (PREG->u.lll.l3);
	SREG = RepAppl(d0);
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, swnv_unk, swnv_nvar);
#ifdef DEBUG
      /* This should never happen */
      PREG = Error(SYSTEM_ERROR, d0, "unbound argument to switch_nonvar");
      JMPNext();
#endif
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      BOp(jump_if_var, l);
      BEGD(d0);
      d0 = CACHED_A1();
      deref_head(d0, jump_if_unk);
      /* non var */
    jump_if_nonvar:
      PREG = NEXTOP(PREG, l);
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, jump_if_unk, jump_if_nonvar);
      /* variable */
      PREG = (yamop *) (PREG->u.l.l);
      ENDP(pt0);
      JMPNext();
      ENDD(d0);
      ENDBOp();

      BOp(if_not_then, cll);
      BEGD(d0);
      d0 = CACHED_A1();
      deref_head(d0, if_n_unk);
    if_n_nvar:
      /* not variable */
      if (d0 == PREG->u.cll.c) {
	/* equal to test value */
	PREG = (yamop *) PREG->u.cll.l2;
	JMPNext();
      }
      else {
	/* different from test value */
	/* the case to optimise */
	PREG = (yamop *) PREG->u.cll.l1;
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, if_n_unk, if_n_nvar);
      ENDP(pt0);
      /* variable */
      PREG = (yamop *) PREG->u.cll.l2;
      JMPNext();
      ENDD(d0);
      ENDBOp();

      /* specialised case where the arguments may be:
       * a list;
       * the empty list;
       * some other atom;
       * a variable;
       * 
       */
      BOp(switch_list_nl, llll);
      BEGD(d0);
      d0 = CACHED_A1();
#if UNIQUE_TAG_FOR_PAIRS
      deref_list_head(d0, swlnl_unk);
    swlnl_list:
#else
      deref_head(d0, swlnl_unk);
      /* non variable */
    swlnl_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
#endif
	PREG = (yamop *) (PREG->u.llll.l1);
	SREG = RepPair(d0);
	JMPNext();
#if UNIQUE_TAG_FOR_PAIRS
      swlnl_nlist:
	if (d0 == TermNil) {
#else
      }
      else if (d0 == TermNil) {
#endif
	/* empty list */
	PREG = (yamop *) (PREG->u.llll.l2);
	JMPNext();
      }
      else {
	/* appl or constant */
	if (IsApplTerm(d0)) {
	  SREG = RepAppl(d0);
	  PREG = (yamop *) (PREG->u.llll.l3);
	  JMPNext();
	} else {
	  I_R = d0;
	  PREG = (yamop *) (PREG->u.llll.l3);
	  JMPNext();
	}
      }

      BEGP(pt0);
#if UNIQUE_TAG_FOR_PAIRS
    swlnl_unk:
      deref_list_body(d0, pt0, swlnl_list, swlnl_nlist);
#else
      deref_body(d0, pt0, swlnl_unk, swlnl_nvar);
#endif
      ENDP(pt0);
      /* variable */
      PREG = (yamop *) (PREG->u.llll.l4);
      JMPNext();
      ENDD(d0);
      ENDBOp();

      /* specialised case where the arguments may be:
       * a list;
       * the empty list;
       * some other atom;
       * a variable;
       * and we know where we are jumping to!
       * 
       */
      BOp(switch_list_nl_prefetch, ollll);
      ALWAYS_LOOKAHEAD(PREG->u.ollll.pop);
      BEGD(d0);
      d0 = CACHED_A1();
#if UNIQUE_TAG_FOR_PAIRS
      deref_list_head(d0, swlnl_unk_p);
      swlnl_list_p:
      {
#else
	deref_head(d0, swlnl_unk_p);
	/* non variable */
      swlnl_nvar_p:
	if (IsPairTerm(d0)) {
	  /* pair */
#endif
	  PREG = (yamop *) (PREG->u.ollll.l1);
	  SREG = RepPair(d0);
	  ALWAYS_GONext();
	}
#if UNIQUE_TAG_FOR_PAIRS
      swlnl_nlist_p:
#endif
	if (d0 == TermNil) {
	  /* empty list */
	  PREG = (yamop *) (PREG->u.ollll.l2);
	  JMPNext();
	}
	else {
	  /* appl or constant */
	  if (IsApplTerm(d0)) {
	    SREG = RepAppl(d0);
	    PREG = (yamop *) (PREG->u.ollll.l3);
	    JMPNext();
	  } else {
	    I_R = d0;
	    PREG = (yamop *) (PREG->u.ollll.l3);
	    JMPNext();
	  }
	}

	BEGP(pt0);
#if UNIQUE_TAG_FOR_PAIRS
      swlnl_unk_p:
	deref_list_body(d0, pt0, swlnl_list_p, swlnl_nlist_p);
#else
	deref_body(d0, pt0, swlnl_unk_p, swlnl_nvar_p);
#endif
	ENDP(pt0);
	/* variable */
	PREG = (yamop *) (PREG->u.ollll.l4);
	JMPNext();
	ENDD(d0);
      }
      ENDBOp();

      BOp(switch_nv_list, lll);
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, swnvl_unk);
    swnvl_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	PREG = (yamop *) (PREG->u.lll.l1);
	JMPNext();
      }
      else if (d0 == TermNil) {
	/* empty list */
	PREG = (yamop *) (PREG->u.lll.l2);
	JMPNext();
      }
      else {
	/* appl or other constant */
	if (IsApplTerm(d0)) {
	  PREG = (yamop *) (PREG->u.lll.l3);
	  SREG = RepAppl(d0);
	  JMPNext();
	} else {
	  PREG = (yamop *) (PREG->u.lll.l3);
	  I_R = d0;
	  JMPNext();
	}
      ALWAYS_END_PREFETCH();

      BEGP(pt0);
      deref_body(d0, pt0, swnvl_unk, swnvl_nvar);
#ifdef DEBUG
      /* This should never happen */
      PREG = Error(SYSTEM_ERROR, d0, "switch_nv_list has unbound argument");
      JMPNext();
#endif /* DEBUG */
      ENDP(pt0);

      ENDD(d0);
      ENDBOp();

/************************************************************************\
* 	Indexing on ARG1							*
\************************************************************************/

#define HASH_SHIFT 6

      BOp(switch_on_func, s);
      BEGD(d1);
      d1 = *SREG++;
      /* we use a very simple hash function to find elements in a
       * switch table */
      {
	register CELL
	/* first, calculate the mask */
	  Mask = (PREG->u.s.s - 1) << 1,	/* next, calculate the hash function */
	  hash = d1 >> (HASH_SHIFT - 1) & Mask;

	PREG = NEXTOP(PREG, s);
	/* PREG now points at the beginning of the hash table */
	BEGP(pt0);
	/* pt0 will always point at the item */
	pt0 = (CELL *) (PREG) + hash;
	BEGD(d0);
	d0 = pt0[0];
	/* a match happens either if we found the value, or if we
	 * found an empty slot */
	if (d0 == d1 || d0 == 0) {
	  PREG = (yamop *) (pt0[1]);
	  JMPNext();
	}
	else {
	  /* ooops, collision, look for other items   */
	  register CELL d = ((d1 | 1) << 1) & Mask;

	  while (1) {
	    hash = (hash + d) & Mask;
	    pt0 = (CELL *) (PREG) + hash;
	    d0 = pt0[0];
	    if (d0 == d1 || d0 == 0) {
	      PREG = (yamop *) pt0[1];
	      JMPNext();
	    }
	  }
	}
	ENDD(d0);
	ENDP(pt0);
      }
      ENDD(d1);
      ENDBOp();

      BOp(switch_on_cons, s);
      BEGD(d1);
      d1 = I_R;
      /* we use a very simple hash function to find elements in a
       * switch table */
      {
	register CELL
	/* first, calculate the mask */
	  Mask = (PREG->u.s.s - 1) << 1,	/* next, calculate the hash function */
	  hash = d1 >> (HASH_SHIFT - 1) & Mask;

	PREG = NEXTOP(PREG, s);
	/* PREG now points at the beginning of the hash table */
	BEGP(pt0);
	/* pt0 will always point at the item */
	pt0 = (CELL *) (PREG) + hash;
	BEGD(d0);
	d0 = pt0[0];
	/* a match happens either if we found the value, or if we
	 * found an empty slot */
	if (d0 == d1 || d0 == 0) {
	  PREG = (yamop *) (pt0[1]);
	  JMPNext();
	}
	else {
	  /* ooops, collision, look for other items   */
	  register CELL d = ((d1 | 1) << 1) & Mask;

	  while (1) {
	    hash = (hash + d) & Mask;
	    pt0 = (CELL *) (PREG) + hash;
	    d0 = pt0[0];
	    if (d0 == d1 || d0 == 0) {
	      PREG = (yamop *) pt0[1];
	      JMPNext();
	    }
	  }
	}
	ENDD(d0);
	ENDP(pt0);
      }
      ENDD(d1);
      ENDBOp();

      BOp(go_on_func, fll);
      BEGD(d0);
      d0 = *SREG++;
      if (d0 == (CELL) (PREG->u.fll.f)) {
	PREG = (yamop *) (PREG->u.fll.l1);
	JMPNext();
      }
      else {
	PREG = (yamop *) (PREG->u.fll.l2);
	JMPNext();
      }
      ENDD(d0);
      ENDBOp();

      BOp(go_on_cons, cll);
      BEGD(d0);
      d0 = I_R;
      if (d0 == PREG->u.cll.c) {
	PREG = (yamop *) (PREG->u.cll.l1);
	JMPNext();
      }
      else {
	PREG = (yamop *) (PREG->u.cll.l2);
	JMPNext();
      }
      ENDD(d0);
      ENDBOp();

      BOp(if_func, sl);
      BEGD(d1);
      d1 = *SREG++;
      BEGD(d0);
      BEGP(pt0);
      d0 = PREG->u.sl.s;
      pt0 = (CELL *) NEXTOP(PREG, sl);
      while (d0-- > 0) {
	if (pt0[0] == d1) {
	  PREG = (yamop *) (pt0[1]);
	  JMPNext();
	}
	else
	  pt0 += 2;
      }
      PREG = (yamop *) (PREG->u.sl.l);
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDD(d1);
      ENDBOp();

      BOp(if_cons, sl);
      BEGD(d1);
      d1 = I_R;
      BEGD(d0);
      BEGP(pt0);
      d0 = PREG->u.sl.s;
      pt0 = (CELL *) NEXTOP(PREG, sl);
      while (d0-- > 0) {
	if (pt0[0] == d1) {
	  PREG = (yamop *) (pt0[1]);
	  JMPNext();
	}
	else
	  pt0 += 2;
      }
      PREG = (yamop *) (PREG->u.sl.l);
      JMPNext();
      ENDP(pt0);
      ENDD(d0);
      ENDD(d1);
      ENDBOp();

/************************************************************************\
* 	Indexing on the Head of a list					*
\************************************************************************/

      BOp(switch_on_head, llll);
      BEGD(d0);
      BEGP(pt0);
      pt0 = SREG;
      d0 = *pt0;
      deref_head(d0, swh_unk);
      /* nonvar */
    swh_nvar:
      /* advance S if not a list */
      ++SREG;
      if (IsPairTerm(d0)) {
	/* pair */
	PREG = (yamop *) (PREG->u.llll.l1);
	/* push: we are entering within a list */
	SP[-1] = (CELL) SREG;
	SP[-2] = READ_MODE;
	SP -= 2;
	SREG = RepPair(d0);
	JMPNext();
      }
      else if (!IsApplTerm(d0)) {
	/* constant */
	PREG = (yamop *) (PREG->u.llll.l2);
	I_R = d0;
	JMPNext();
      }
      else {
	/* appl */
	/* jump */
	PREG = (yamop *) (PREG->u.llll.l3);
	/* push: we are entering the compound term */
	SP[-1] = (CELL) SREG;
	SP[-2] = READ_MODE;
	SP -= 2;
	SREG = RepAppl(d0);
	JMPNext();
      }

      derefa_body(d0, pt0, swh_unk, swh_nvar);
      ENDP(pt0);
      /* variable */
      PREG = (yamop *) (PREG->u.llll.l4);
      JMPNext();
      ENDD(d0);
      ENDBOp();

/************************************************************************\
* Switch for final block, we know the information is in a choicepoint   *
\************************************************************************/
      BOp(switch_last, slll);
      BEGD(d0);
      d0 = B->cp_a1;
      deref_head(d0, swl_unk);
    swl_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	PREG = (yamop *) (PREG->u.slll.l1);
	JMPNext();
      }
      else if (!IsApplTerm(d0)) {
	/* constant */
	PREG = (yamop *) (PREG->u.slll.l2);
	I_R = d0;
	JMPNext();
      }
      else {
	/* appl */
	PREG = (yamop *) (PREG->u.slll.l3);
	SREG = RepAppl(d0);
	JMPNext();
      }

      BEGP(pt0);
      deref_body(d0, pt0, swl_unk, swl_nvar);
#ifdef DEBUG
      /* This should never happen */
      PREG = Error(SYSTEM_ERROR, d0, "switch_last has unbound argument");
      JMPNext();
#endif
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

      BOp(switch_l_list, slll);
      BEGD(d0);
      d0 = B->cp_a1;
      deref_head(d0, swll_unk);
    swll_nvar:
      if (IsPairTerm(d0)) {
	/* pair */
	SREG = RepPair(d0);
	PREG = (yamop *) (PREG->u.slll.l1);
	JMPNext();
      }
      else if (d0 == TermNil) {
	/* empty list */
	PREG = (yamop *) (PREG->u.slll.l2);
	JMPNext();
      }
      else {
	/* anything else */
	if (IsApplTerm(d0)) {
	  PREG = (yamop *) (PREG->u.slll.l3);
	  SREG = RepAppl(d0);
	  JMPNext();
	} else {
	  PREG = (yamop *) (PREG->u.slll.l3);
	  I_R = d0;
	  JMPNext();
	}
      }

      BEGP(pt0);
      deref_body(d0, pt0, swll_unk, swll_nvar);
#ifdef DEBUG
      /* This should never happen */
      PREG = Error(SYSTEM_ERROR, d0, "switch_l_list has unbound argument");
      JMPNext();
#endif
      ENDP(pt0);
      ENDD(d0);
      ENDBOp();

/************************************************************************\
*	Basic Primitive Predicates					 *
\************************************************************************/

      Op(p_atom_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, atom_x_unk);
    atom_x_nvar:
      if (IsAtomTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, atom_x_unk, atom_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_atom_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, atom_y_unk);
    atom_y_nvar:
      if (IsAtomTerm(d0)) {
	PREG = NEXTOP(PREG, y);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, atom_y_unk, atom_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_atomic_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, atomic_x_unk);
    atomic_x_nvar:
      /* non variable */
      if (IsAtomicTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, atomic_x_unk, atomic_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_atomic_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, atomic_y_unk);
    atomic_y_nvar:
      /* non variable */
      if (IsAtomicTerm(d0)) {
	PREG = NEXTOP(PREG, y);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, atomic_y_unk, atomic_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_integer_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, integer_x_unk);
    integer_x_nvar:
      /* non variable */
      if (IsIntTerm(d0) || IsLargeIntTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, integer_x_unk, integer_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_integer_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, integer_y_unk);
    integer_y_nvar:
      /* non variable */
      if (IsIntTerm(d0) || IsLargeIntTerm(d0)) {
	PREG = NEXTOP(PREG, y);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, integer_y_unk, integer_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_nonvar_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, nonvar_x_unk);
    nonvar_x_nvar:
      PREG = NEXTOP(PREG, x);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, nonvar_x_unk, nonvar_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_nonvar_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, nonvar_y_unk);
    nonvar_y_nvar:
      PREG = NEXTOP(PREG, y);
      GONext();

      derefa_body(d0, pt0, nonvar_y_unk, nonvar_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_number_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, number_x_unk);
    number_x_nvar:
      /* non variable */
      if (IsNumTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, number_x_unk, number_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_number_y, x);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, number_y_unk);
    number_y_nvar:
      /* non variable */
      if (IsNumTerm(d0)) {
	PREG = NEXTOP(PREG, y);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, number_y_unk, number_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_var_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, var_x_unk);
    var_x_nvar:
      /* non variable */
      FAIL();

      BEGP(pt0);
      deref_body(d0, pt0, var_x_unk, var_x_nvar);
      PREG = NEXTOP(PREG, x);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_var_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, var_y_unk);
    var_y_nvar:
      /* non variable */
      FAIL();

      derefa_body(d0, pt0, var_y_unk, var_y_nvar);
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_db_ref_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, dbref_x_unk);
    dbref_x_nvar:
      /* non variable */
      if (IsDBRefTerm(d0)) {
	/* only allow references to the database, not general references
	 * to go through. */
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, dbref_x_unk, dbref_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_db_ref_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, dbref_y_unk);
    dbref_y_nvar:
      /* non variable */
      if (IsDBRefTerm(d0)) {
	/* only allow references to the database, not general references
	 * to go through. */
	PREG = NEXTOP(PREG, y);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, dbref_y_unk, dbref_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_primitive_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, primi_x_unk);
    primi_x_nvar:
      /* non variable */
      if (IsPrimitiveTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, primi_x_unk, primi_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_primitive_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, primi_y_unk);
    primi_y_nvar:
      /* non variable */
      if (IsPrimitiveTerm(d0)) {
	PREG = NEXTOP(PREG, y);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, primi_y_unk, primi_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_compound_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, compound_x_unk);
    compound_x_nvar:
      /* non variable */
      if (IsPairTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else if (IsApplTerm(d0)) {
	if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	  FAIL();
	}
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      BEGP(pt0);
      deref_body(d0, pt0, compound_x_unk, compound_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_compound_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, compound_y_unk);
    compound_y_nvar:
      /* non variable */
      if (IsPairTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else if (IsApplTerm(d0)) {
	if (IsExtensionFunctor(FunctorOfTerm(d0))) {
	  FAIL();
	}
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      else {
	FAIL();
      }

      derefa_body(d0, pt0, compound_y_unk, compound_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_float_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, float_x_unk);
    float_x_nvar:
      /* non variable */
      if (IsFloatTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      FAIL();

      BEGP(pt0);
      deref_body(d0, pt0, float_x_unk, float_x_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_float_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, float_y_unk);
    float_y_nvar:
      /* non variable */
      if (IsFloatTerm(d0)) {
	PREG = NEXTOP(PREG, x);
	GONext();
      }
      FAIL();

      derefa_body(d0, pt0, float_y_unk, float_y_nvar);
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_cut_by_x, x);
      BEGD(d0);
      d0 = XREG(PREG->u.x.x);
      deref_head(d0, cutby_x_unk);
    cutby_x_nvar:
#if defined(SBA) && defined(FROZEN_REGS)
      if (!IsIntegerTerm(d0)) {
#else
      if (!IsIntTerm(d0)) {
#endif
	FAIL();
      }
      BEGCHO(pt0);
#if defined(SBA) && defined(FROZEN_REGS)
      pt0 = (choiceptr)IntegerOfTerm(d0);
#else
      pt0 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
      if (TopB != NULL && YOUNGER_CP(TopB,pt0)) {  
	pt0 = TopB;
	if (DelayedB == NULL || YOUNGER_CP(pt0,DelayedB))
	  DelayedB = pt0;
      }
      /* find where to cut to */
#ifdef YAPOR
      if (SHOULD_CUT_UP_TO(B,pt0)) {
	/* Wow, we're gonna cut!!! */
	CUT_prune_to(pt0);
#else
      if (SHOULD_CUT_UP_TO(B,pt0)) {
	/* Wow, we're gonna cut!!! */
	B = pt0;
#endif /* YAPOR */
	HBREG = PROTECT_FROZEN_H(B);
	TR = trim_trail(B, TR, HBREG);
      }
      ENDCHO(pt0);
      PREG = NEXTOP(PREG, x);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, cutby_x_unk, cutby_x_nvar);
      ENDP(pt1);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_cut_by_y, y);
      BEGD(d0);
      BEGP(pt0);
      pt0 = Y + PREG->u.y.y;
      d0 = *pt0;
      deref_head(d0, cutby_y_unk);
    cutby_y_nvar:
#if defined(SBA) && defined(FROZEN_REGS)
      if (!IsIntegerTerm(d0)) {
#else
      if (!IsIntTerm(d0)) {
#endif
	FAIL();
      }
      /* find where to cut to */
      BEGCHO(pt1);
#if defined(SBA) && defined(FROZEN_REGS)
      pt1 = (choiceptr)IntegerOfTerm(d0);
#else
      pt1 = (choiceptr)(LCL0-IntOfTerm(d0));
#endif
      if (TopB != NULL && YOUNGER_CP(TopB,pt1)) {  
	pt1 = TopB;
	if (DelayedB == NULL || YOUNGER_CP(pt1,DelayedB))
	  DelayedB = pt1;
      }
      if (SHOULD_CUT_UP_TO(B,pt1)) {
	/* Wow, we're gonna cut!!! */
#ifdef YAPOR
	CUT_prune_to(pt1);
#else
	B = pt1;
#endif /* YAPOR */
	HBREG = PROTECT_FROZEN_H(B);
	TR = trim_trail(B, TR, HBREG);
      }
      PREG = NEXTOP(PREG, y);
      GONext();
      ENDCHO(pt1);

      derefa_body(d0, pt0, cutby_y_unk, cutby_y_nvar);
      /* never cut to a variable */
      /* Abort */
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_plus_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, plus_vv_unk);
    plus_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, plus_vv_nvar_unk);
    plus_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_plus(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_vv_unk, plus_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A+B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, plus_vv_nvar_unk, plus_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A+B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_plus_vc, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, plus_vc_unk);
    plus_vc_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
	}
	else {
	  saveregs();
	  d0 = p_plus(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_vc_unk, plus_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A+B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_plus_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, plus_y_vv_unk);
    plus_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, plus_y_vv_nvar_unk);
    plus_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) + IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_plus(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_y_vv_unk, plus_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A+B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, plus_y_vv_nvar_unk, plus_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A+B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_plus_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      /* first check pt1 */
      deref_head(d0, plus_y_vc_unk);
    plus_y_vc_nvar:
      {
	Int d1 = PREG->u.yxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) + d1);
	}
	else {
	  saveregs();
	  d0 = p_plus(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, plus_y_vc_unk, plus_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A+B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_minus_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, minus_vv_unk);
    minus_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, minus_vv_nvar_unk);
    minus_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_minus(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_vv_unk, minus_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A-B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, minus_vv_nvar_unk, minus_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A-B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_minus_cv, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, minus_cv_unk);
    minus_cv_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
	}
	else {
	  saveregs();
	  d0 = p_minus(MkIntegerTerm(d1),d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_cv_unk, minus_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A-B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_minus_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, minus_y_vv_unk);
    minus_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, minus_y_vv_nvar_unk);
    minus_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) - IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_minus(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_y_vv_unk, minus_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A-B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, minus_y_vv_nvar_unk, minus_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A-B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_minus_y_cv, ycx);
      BEGD(d0);
      d0 = XREG(PREG->u.ycx.xi);
      /* first check pt1 */
      deref_head(d0, minus_y_cv_unk);
    minus_y_cv_nvar:
      {
	Int d1 = PREG->u.ycx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(d1 - IntOfTerm(d0));
	}
	else {
	  saveregs();
	  d0 = p_minus(MkIntegerTerm(d1), d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.ycx.y;
      PREG = NEXTOP(PREG, ycx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, minus_y_cv_unk, minus_y_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A-B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_times_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, times_vv_unk);
    times_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, times_vv_nvar_unk);
    times_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_times(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_vv_unk, times_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A*B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, times_vv_nvar_unk, times_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A*B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_times_vc, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, times_vc_unk);
    times_vc_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = times_int(IntOfTerm(d0), d1);
	}
	else {
	  saveregs();
	  d0 = p_times(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_vc_unk, times_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A*B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_times_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, times_y_vv_unk);
    times_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, times_y_vv_nvar_unk);
    times_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = times_int(IntOfTerm(d0), IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_times(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_y_vv_unk, times_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A*B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, times_y_vv_nvar_unk, times_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A*B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_times_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      /* first check pt1 */
      deref_head(d0, times_y_vc_unk);
    times_y_vc_nvar:
      {
	Int d1 = PREG->u.yxc.c;
	if (IsIntTerm(d0)) {
	  d0 = times_int(IntOfTerm(d0), d1);
	}
	else {
	  saveregs();
	  d0 = p_times(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, times_y_vc_unk, times_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A*B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, div_vv_unk);
    div_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, div_vv_nvar_unk);
    div_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	Int div = IntOfTerm(d1);
	if (div == 0) {
	  Error(EVALUATION_ERROR_ZERO_DIVISOR,TermNil,"// /2");
	  FAIL();
	}
	d0 = MkIntTerm(IntOfTerm(d0) / div);
      }
      else {
	saveregs();
	d0 = p_div(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_vv_unk, div_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, div_vv_nvar_unk, div_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_div_vc, xcx);
      BEGD(d0);
      d0 = XREG(PREG->u.xcx.xi);
      /* first check pt1 */
      deref_head(d0, div_vc_unk);
    div_vc_nvar:
      {
	Int d1 = PREG->u.xcx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntTerm(IntOfTerm(d0) / d1);
	}
	else {
	  saveregs();
	  d0 = p_div(d0,MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xcx.x) = d0;
      PREG = NEXTOP(PREG, xcx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_vc_unk, div_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_cv, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, div_cv_unk);
    div_cv_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  Int div = IntOfTerm(d0);
	  if (div == 0){
	    Error(EVALUATION_ERROR_ZERO_DIVISOR,TermNil,"// /2");
	    FAIL();
	  }
	  d0 = MkIntegerTerm(d1 / div);
	}
	else {
	  saveregs();
	  d0 = p_div(MkIntegerTerm(d1),d0);
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_cv_unk, div_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, div_y_vv_unk);
    div_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, div_y_vv_nvar_unk);
    div_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	Int div = IntOfTerm(d1);
	if (div == 0) {
	  Error(EVALUATION_ERROR_ZERO_DIVISOR,TermNil,"// /2");
	  FAIL();
	}
	d0 = MkIntTerm(IntOfTerm(d0) / div);
      }
      else {
	saveregs();
	d0 = p_div(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_vv_unk, div_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, div_y_vv_nvar_unk, div_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.ycx.xi);
      /* first check pt1 */
      deref_head(d0, div_y_vc_unk);
    div_y_vc_nvar:
      {
	Int d1 = PREG->u.ycx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntTerm(IntOfTerm(d0)/d1);
	}
	else {
	  saveregs();
	  d0 = p_div(d0,MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_vc_unk, div_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_div_y_cv, ycx);
      BEGD(d0);
      d0 = XREG(PREG->u.ycx.xi);
      /* first check pt1 */
      deref_head(d0, div_y_cv_unk);
    div_y_cv_nvar:
      {
	Int d1 = PREG->u.ycx.c;
	if (IsIntTerm(d0)) {
	  Int div = IntOfTerm(d0);
	  if (div == 0) {
	    Error(EVALUATION_ERROR_ZERO_DIVISOR,TermNil,"// /2");
	    FAIL();
	  }
	  d0 = MkIntegerTerm(d1 / div);
	}
	else {
	  saveregs();
	  d0 = p_div(MkIntegerTerm(d1), d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.ycx.y;
      PREG = NEXTOP(PREG, ycx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, div_y_cv_unk, div_y_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A//B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();


      Op(p_and_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, and_vv_unk);
    and_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, and_vv_nvar_unk);
    and_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_and(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_vv_unk, and_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A/\\B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, and_vv_nvar_unk, and_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A/\\B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_and_vc, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, and_vc_unk);
    and_vc_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
	}
	else {
	  saveregs();
	  d0 = p_and(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_vc_unk, and_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A/\\B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_and_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, and_y_vv_unk);
    and_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, and_y_vv_nvar_unk);
    and_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) & IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_and(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_y_vv_unk, and_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A/\\B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, and_y_vv_nvar_unk, and_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A/\\B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_and_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      /* first check pt1 */
      deref_head(d0, and_y_vc_unk);
    and_y_vc_nvar:
      {
	Int d1 = PREG->u.yxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) & d1);
	}
	else {
	  saveregs();
	  d0 = p_and(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, and_y_vc_unk, and_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A/\\B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();


      Op(p_or_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, or_vv_unk);
    or_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, or_vv_nvar_unk);
    or_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_or(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_vv_unk, or_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A\\/B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, or_vv_nvar_unk, or_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A\\/B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_or_vc, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, or_vc_unk);
    or_vc_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
	}
	else {
	  saveregs();
	  d0 = p_or(d0, MkIntegerTerm(d1));
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_vc_unk, or_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A\\/B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_or_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, or_y_vv_unk);
    or_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, or_y_vv_nvar_unk);
    or_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) | IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_or(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_y_vv_unk, or_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A\\/B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, or_y_vv_nvar_unk, or_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A\\/B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_or_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      /* first check pt1 */
      deref_head(d0, or_y_vc_unk);
    or_y_vc_nvar:
      {
	Int d1 = PREG->u.yxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) | d1);
	}
	else {
	  saveregs();
	  d0 = p_or(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, or_y_vc_unk, or_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A\\/B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, sll_vv_unk);
    sll_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, sll_vv_nvar_unk);
    sll_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) << IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_sll(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_vv_unk, sll_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, sll_vv_nvar_unk, sll_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_sll_vc, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, sll_vc_unk);
    sll_vc_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) << d1);
	}
	else {
	  saveregs();
	  d0 = p_sll(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_vc_unk, sll_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_cv, xcx);
      BEGD(d0);
      d0 = XREG(PREG->u.xcx.xi);
      /* first check pt1 */
      deref_head(d0, sll_cv_unk);
    sll_cv_nvar:
      {
	Int d1 = PREG->u.xcx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(d1 << IntOfTerm(d0));
	}
	else {
	  saveregs();
	  d0 = p_sll(MkIntegerTerm(d1), d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xcx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_cv_unk, sll_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, sll_y_vv_unk);
    sll_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, sll_y_vv_nvar_unk);
    sll_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) << IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_sll(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_vv_unk, sll_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, sll_y_vv_nvar_unk, sll_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_sll_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      /* first check pt1 */
      deref_head(d0, sll_y_vc_unk);
    sll_y_vc_nvar:
      {
	Int d1 = PREG->u.yxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) << d1);
	}
	else {
	  saveregs();
	  d0 = p_sll(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_vc_unk, sll_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();


      Op(p_sll_y_cv, ycx);
      BEGD(d0);
      d0 = XREG(PREG->u.ycx.xi);
      /* first check pt1 */
      deref_head(d0, sll_y_cv_unk);
    sll_y_cv_nvar:
      {
	Int d1 = PREG->u.ycx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(d1 << IntOfTerm(d0));
	}
	else {
	  saveregs();
	  d0 = p_sll(MkIntegerTerm(d1), d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.ycx.y;
      PREG = NEXTOP(PREG, ycx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, sll_y_cv_unk, sll_y_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A<<B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_vv, xxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.xxx.x1);
      /* first check pt1 */
      deref_head(d0, slr_vv_unk);
    slr_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, slr_vv_nvar_unk);
    slr_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) >> IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_slr(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      XREG(PREG->u.xxx.x) = d0;
      PREG = NEXTOP(PREG, xxx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_vv_unk, slr_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, slr_vv_nvar_unk, slr_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_slr_vc, xxc);
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      /* first check pt1 */
      deref_head(d0, slr_vc_unk);
    slr_vc_nvar:
      {
	Int d1 = PREG->u.xxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) >> d1);
	}
	else {
	  saveregs();
	  d0 = p_slr(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xxc);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_vc_unk, slr_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_cv, xcx);
      BEGD(d0);
      d0 = XREG(PREG->u.xcx.xi);
      /* first check pt1 */
      deref_head(d0, slr_cv_unk);
    slr_cv_nvar:
      {
	Int d1 = PREG->u.xcx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(d1 >> IntOfTerm(d0));
	}
	else {
	  saveregs();
	  d0 = p_slr(MkIntegerTerm(d1), d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(PREG, xcx);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_cv_unk, slr_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_vv, yxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.yxx.x1);
      /* first check pt1 */
      deref_head(d0, slr_y_vv_unk);
    slr_y_vv_nvar:
      d1 = XREG(PREG->u.xxx.x2);
      /* next check A2 */
      deref_head(d1, slr_y_vv_nvar_unk);
    slr_y_vv_nvar_nvar:
      /* d0 and d1 are where I want them */
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	d0 = MkIntegerTerm(IntOfTerm(d0) >> IntOfTerm(d1));
      }
      else {
	saveregs();
	d0 = p_slr(d0, d1);
	setregs();
	if (PREG == (yamop *)FAILCODE)
	  FAIL();
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxx.y;
      PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_vv_unk, slr_y_vv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, slr_y_vv_nvar_unk, slr_y_vv_nvar_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_vc, yxc);
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      /* first check pt1 */
      deref_head(d0, slr_y_vc_unk);
    slr_y_vc_nvar:
      {
	Int d1 = PREG->u.yxc.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(IntOfTerm(d0) >> d1);
	}
	else {
	  saveregs();
	  d0 = p_slr(d0, MkIntegerTerm(d1));
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_vc_unk, slr_y_vc_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      Op(p_slr_y_cv, ycx);
      BEGD(d0);
      d0 = XREG(PREG->u.ycx.xi);
      /* first check pt1 */
      deref_head(d0, slr_y_cv_unk);
    slr_y_cv_nvar:
      {
	Int d1 = PREG->u.ycx.c;
	if (IsIntTerm(d0)) {
	  d0 = MkIntegerTerm(d1 >> IntOfTerm(d0));
	}
	else {
	  saveregs();
	  d0 = p_slr(MkIntegerTerm(d1), d0);
	  setregs();
	  if (PREG == (yamop *)FAILCODE)
	    FAIL();
	}
      }
      BEGP(pt0);
      pt0 = Y + PREG->u.ycx.y;
      PREG = NEXTOP(PREG, ycx);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt0,d0);
#else
      *pt0 = d0;
#endif
      ENDP(pt0);
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, slr_y_cv_unk, slr_y_cv_nvar);
      Error(INSTANTIATION_ERROR, TermNil, "X is A>>B");
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

      BOp(call_bfunc_xx, lxx);
      BEGD(d0);
      BEGD(d1);
      d0 = XREG(PREG->u.lxx.x1);
    call_bfunc_xx_nvar:
      d1 = XREG(PREG->u.lxx.x2);
    call_bfunc_xx2_nvar:
      deref_head(d0, call_bfunc_xx_unk);
      deref_head(d1, call_bfunc_xx2_unk);
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	int flags;

	Int v = IntOfTerm(d0) - IntOfTerm(d1);
	flags = PREG->u.lxx.flags;
	PREG = NEXTOP(PREG, lxx);
	if (v > 0) {
	  if (flags & GT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else if (v < 0) {
	  if (flags & LT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else /* if (v == 0) */ {
	  if (flags & EQ_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	}
      } 
    exec_bin_cmp_xx:
      BEGD(d2);
      d2 = (CELL)(PREG->u.lxx.l);
      PREG = NEXTOP(PREG, lxx);
      saveregs();
      d0 = (CELL) (*((Int (*)(CELL, CELL)) d2)) (d0,d1);

      ENDD(d2);
      setregs();
      if (!d0) {
	FAIL();
      }
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_xx_unk, call_bfunc_xx_nvar);
      goto exec_bin_cmp_xx;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_xx2_unk, call_bfunc_xx2_nvar);
      goto exec_bin_cmp_xx;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_yx, lxy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = Y + PREG->u.lxy.y;
      d1 = XREG(PREG->u.lxy.x);
      d0 = *pt0;
      ENDP(pt0);
      deref_head(d0, call_bfunc_yx_unk);
    call_bfunc_yx_nvar:
      deref_head(d1, call_bfunc_yx2_unk);
    call_bfunc_yx2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	int flags;

	Int v = IntOfTerm(d0) - IntOfTerm(d1);
	flags = PREG->u.lxy.flags;
	PREG = NEXTOP(PREG, lxy);
	if (v > 0) {
	  if (flags & GT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else if (v < 0) {
	  if (flags & LT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else /* if (v == 0) */ {
	  if (flags & EQ_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	}
      } 
    exec_bin_cmp_yx:
      BEGD(d2);
      d2 = (CELL)(PREG->u.lxy.l);
      PREG = NEXTOP(PREG, lxy);
      saveregs();
      d0 = (CELL) (*((Int (*)(CELL, CELL)) d2)) (d0,d1);
      ENDD(d2);
      setregs();
      if (!d0) {
	FAIL();
      }
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_yx_unk, call_bfunc_yx_nvar);
      goto exec_bin_cmp_yx;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_yx2_unk, call_bfunc_yx2_nvar);
      goto exec_bin_cmp_yx;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_xy, lxy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = Y + PREG->u.lxy.y;
      d0 = XREG(PREG->u.lxy.x);
      d1 = *pt0;
      ENDP(pt0);
      deref_head(d0, call_bfunc_xy_unk);
    call_bfunc_xy_nvar:
      deref_head(d1, call_bfunc_xy2_unk);
    call_bfunc_xy2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	int flags;

	Int v = IntOfTerm(d0) - IntOfTerm(d1);
	flags = PREG->u.lxy.flags;
	PREG = NEXTOP(PREG, lxy);
	if (v > 0) {
	  if (flags & GT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else if (v < 0) {
	  if (flags & LT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else /* if (v == 0) */ {
	  if (flags & EQ_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	}
      } 
    exec_bin_cmp_xy:
      BEGD(d2);
      d2 = (CELL)(PREG->u.lxy.l);
      PREG = NEXTOP(PREG, lxy);
      saveregs();
      d0 = (CELL) (*((Int (*)(CELL, CELL)) d2)) (d0,d1);
      ENDD(d2);
      setregs();
      if (!d0) {
	FAIL();
      }
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_xy_unk, call_bfunc_xy_nvar);
      goto exec_bin_cmp_xy;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_xy2_unk, call_bfunc_xy2_nvar);
      goto exec_bin_cmp_xy;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      BOp(call_bfunc_yy, lyy);
      BEGD(d0);
      BEGD(d1);
      BEGP(pt0);
      pt0 = Y + PREG->u.lyy.y1;
      BEGP(pt1);
      pt1 = Y + PREG->u.lyy.y2;
      d0 = *pt0;
      d1 = *pt1;
      ENDP(pt1);
      ENDP(pt0);
      deref_head(d0, call_bfunc_yy_unk);
    call_bfunc_yy_nvar:
      deref_head(d1, call_bfunc_yy2_unk);
    call_bfunc_yy2_nvar:
      if (IsIntTerm(d0) && IsIntTerm(d1)) {
	int flags;

	Int v = IntOfTerm(d0) - IntOfTerm(d1);
	flags = PREG->u.lyy.flags;
	PREG = NEXTOP(PREG, lyy);
	if (v > 0) {
	  if (flags & GT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else if (v < 0) {
	  if (flags & LT_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	} else /* if (v == 0) */ {
	  if (flags & EQ_OK_IN_CMP) {
	    JMPNext();
	  } else
	    FAIL();
	}
      } 
    exec_bin_cmp_yy:
      BEGD(d2);
      d2 = (CELL)(PREG->u.lyy.l);
      PREG = NEXTOP(PREG, lyy);
      saveregs();
      d0 = (CELL) (*((Int (*)(CELL, CELL)) d2)) (d0,d1);
      ENDD(d2);
      setregs();
      if (!d0) {
	FAIL();
      }
      JMPNext();

      BEGP(pt0);
      deref_body(d0, pt0, call_bfunc_yy_unk, call_bfunc_yy_nvar);
      goto exec_bin_cmp_yy;
      ENDP(pt0);

      BEGP(pt0);
      deref_body(d1, pt0, call_bfunc_yy2_unk, call_bfunc_yy2_nvar);
      goto exec_bin_cmp_yy;
      ENDP(pt0);

      ENDD(d1);
      ENDD(d0);
      ENDBOp();

      Op(p_equal, e);
      save_hb();
      if (IUnify(ARG1, ARG2) == FALSE) {
	FAIL();
      }
      PREG = NEXTOP(PREG, e);
      GONext();
      ENDOp();

      Op(p_dif, e);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("\\="),2)),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      deref_head(d0, dif_unk1);
    dif_nvar1:
      /* first argument is bound */
      d1 = ARG2;
      deref_head(d1, dif_nvar1_unk2);
    dif_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
	FAIL();
      }
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
	PREG = NEXTOP(PREG, e);
	GONext();
      }
      {
#ifdef COROUTINING
	/*
	 * We may wake up goals during our attempt to unify the
	 * two terms. If we are adding to the tail of a list of
	 * woken goals that should be ok, but otherwise we need
	 * to restore WokenGoals to its previous value.
	 */
	CELL OldWokenGoals = ReadTimedVar(WokenGoals);

#endif
	/* We will have to look inside compound terms */
	register tr_fr_ptr pt0;
	/* store the old value of TR for clearing bindings */
	pt0 = TR;
	BEGCHO(pt1);
	pt1 = B;
	/* make B and HB point to H to guarantee all bindings will
	 * be trailed
	 */
	HBREG = H;
	B = (choiceptr) H;
	SET_BB(B);
	save_hb();
	if (IUnify(d0, d1) == TRUE) {
	  /* restore B, no need to restore HB */
	  B = pt1;
	  FAIL();
	}
	/* restore B, and later HB */
	PREG = NEXTOP(PREG, e);
	B = pt1;
	SET_BB(PROTECT_FROZEN_B(pt1));
	ENDCHO(pt1);
	/* untrail all bindings made by IUnify */
	while (TR != pt0) {
	  BEGD(d1);
	  d1 = TrailTerm(--TR);
#if defined(SBA) && defined(YAPOR)
	  /* clean up the trail when we backtrack */
	  if (Unsigned((Int)(d1)-(Int)(H_FZ)) >
	      Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
	    RESET_VARIABLE(STACK_TO_SBA(d1));
	  } else
#endif
	    /* normal variable */
	    RESET_VARIABLE(d1);
	  ENDD(d1);
	}
	HBREG = B->cp_h;
#ifdef COROUTINING
	/* now restore Woken Goals to its old value */
	UpdateTimedVar(WokenGoals, OldWokenGoals);
#endif
      }
      GONext();

      BEGP(pt0);
      deref_body(d0, pt0, dif_unk1, dif_nvar1);
      ENDP(pt0);
      /* first argument is unbound */
      FAIL();

      BEGP(pt0);
      deref_body(d1, pt0, dif_nvar1_unk2, dif_nvar1_nvar2);
      ENDP(pt0);
      /* second argument is unbound */
      FAIL();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_eq, e);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("=="),2)),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      BEGD(d1);
      d0 = ARG1;
      deref_head(d0, p_eq_unk1);
    p_eq_nvar1:
      /* first argument is bound */
      d1 = ARG2;
      deref_head(d1, p_eq_nvar1_unk2);
    p_eq_nvar1_nvar2:
      /* both arguments are bound */
      if (d0 == d1) {
	PREG = NEXTOP(PREG, e);
	GONext();
      }
      if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) {
	  FAIL();
	}
	BEGD(d2);
	always_save_pc();
	d2 = iequ_complex(RepPair(d0)-1, RepPair(d0)+1,RepPair(d1)-1);
	if (d2 == FALSE) {
	  FAIL();
	}
	ENDD(d2);
	always_set_pc();
	PREG = NEXTOP(PREG, e);
	GONext();
      }
      if (IsApplTerm(d0)) {
	Functor f0 = FunctorOfTerm(d0);
	Functor f1;

	/* f1 must be a compound term, even if it is a suspension */
	if (!IsApplTerm(d1)) {
	  FAIL();
	}
	f1 = FunctorOfTerm(d1);

	PREG = NEXTOP(PREG, e);
	/* we now know f1 is true */
	/* deref if a compound term */
	if (IsExtensionFunctor(f0)) {
	  switch ((CELL)f0) {
	  case (CELL)FunctorDBRef:
	    if (d0 == d1) GONext();
	    FAIL();
	  case (CELL)FunctorLongInt:
	    if (f1 != FunctorLongInt) FAIL();
	    if (LongIntOfTerm(d0) == LongIntOfTerm(d1)) GONext();
	    FAIL();
#ifdef USE_GMP
	  case (CELL)FunctorBigInt:
	    if (f1 != FunctorBigInt) FAIL();
	    if (mpz_cmp(BigIntOfTerm(d0), BigIntOfTerm(d1)) == 0) GONext();
	    FAIL();
#endif
	  case (CELL)FunctorDouble:
	    if (f1 != FunctorDouble) FAIL();
	    if (FloatOfTerm(d0) == FloatOfTerm(d1)) GONext();
	    FAIL();
	  default:
	    FAIL();
	  }
	}
	if (f0 != f1) {
	  FAIL();
	}
	always_save_pc();
	BEGD(d2);
	d2 = iequ_complex(RepAppl(d0), RepAppl(d0)+ArityOfFunctor(f0), RepAppl(d1));
	if (d2 == FALSE) {
	  FAIL();
	}
	ENDD(d2);
	always_set_pc();
	GONext();
      }
      FAIL();

      BEGP(pt0);
      deref_body(d1, pt0, p_eq_nvar1_unk2, p_eq_nvar1_nvar2);
      ENDP(pt0);
      /* first argument is bound */
      /* second argument is unbound */
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, p_eq_unk1, p_eq_nvar1);
      BEGD(d1);
      d1 = ARG2;
      deref_head(d1, p_eq_var1_unk2);
    p_eq_var1_nvar2:
      /* I don't need to worry about co-routining because an
	 unbound variable may never be == to a constrained variable!! */
      FAIL();

      BEGP(pt1);
      deref_body(d1, pt1, p_eq_var1_unk2, p_eq_var1_nvar2);
      /* first argument is unbound */
      /* second argument is unbound */
      if (pt1 != pt0) {
	FAIL();
      }
      PREG = NEXTOP(PREG, e);
      GONext();      
      ENDP(pt1);
      ENDD(d1);
      ENDP(pt0);

      ENDD(d0);
      ENDOp();

      Op(p_arg_vv, xxx);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	H[0] = XREG(PREG->u.xxx.x1);
	H[1] = XREG(PREG->u.xxx.x2);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("arg"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->u.xxx.x1);
      deref_head(d0, arg_arg1_unk);
    arg_arg1_nvar:
      /* ARG1 is ok! */
      if (IsIntTerm(d0))
	d0 = IntOfTerm(d0);
      else if (IsLongIntTerm(d0)) {
	d0 = LongIntOfTerm(d0);
      } else {
	Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	FAIL();
      }

      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->u.xxx.x2);
      deref_head(d1, arg_arg2_unk);
    arg_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
	BEGP(pt0);
	pt0 = RepAppl(d1);
	d1 = *pt0;
	if (IsExtensionFunctor((Functor) d1)) {
	  FAIL();
	}
	if ((Int)d0 <= 0 ||
	    d0 > ArityOfFunctor((Functor) d1)) {
	  /* don't complain here for Prolog compatibility 
	  if ((Int)d0 <= 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");	    
	  }
	  */
	  FAIL();
	}
	XREG(PREG->u.xxx.x) = pt0[d0];
	PREG = NEXTOP(PREG, xxx);
	GONext();
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 != 1 && d0 != 2) {
	  if ((Int)d0 < 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	  }
	  FAIL();
	}
	XREG(PREG->u.xxx.x) = pt0[d0-1];
	PREG = NEXTOP(PREG, xxx);
	GONext();
	ENDP(pt0);
      }
      else {
	Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_arg2_unk, arg_arg2_nvar);
      Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, arg_arg1_unk, arg_arg1_nvar);
      Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3");;
      ENDP(pt0);
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_arg_cv, xxc);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	CELL *Ho = H;
	Term t = MkIntegerTerm(PREG->u.xxc.c); 
	H[0] =  t;
	H[1] = XREG(PREG->u.xxc.xi);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("arg"),3)),H);
	H = Ho;
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = PREG->u.xxc.c;
      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->u.xxc.xi);
      deref_head(d1, arg_arg2_vc_unk);
    arg_arg2_vc_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
	BEGP(pt0);
	pt0 = RepAppl(d1);
	d1 = *pt0;
	if (IsExtensionFunctor((Functor) d1)) {
	  FAIL();
	}
	if ((Int)d0 <= 0 ||
	    d0 > ArityOfFunctor((Functor) d1)) {
	  /* don't complain here for Prolog compatibility 
	  if ((Int)d0 <= 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");	    
	  }
	  */
	  FAIL();
	}
	XREG(PREG->u.xxc.x) = pt0[d0];
	PREG = NEXTOP(PREG, xxc);
	GONext();
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 != 1 && d0 != 2) {
	  if ((Int)d0 < 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	  }
	  FAIL();
	}
	XREG(PREG->u.xxc.x) = pt0[d0-1];
	PREG = NEXTOP(PREG, xxc);
	GONext();
	ENDP(pt0);
      }
      else {
	Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_arg2_vc_unk, arg_arg2_vc_nvar);
      Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      ENDD(d0);
      ENDOp();

      Op(p_arg_y_vv, yxx);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	H[0] = XREG(PREG->u.yxx.x1);
	H[1] = XREG(PREG->u.yxx.x2);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("arg"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->u.yxx.x1);
      deref_head(d0, arg_y_arg1_unk);
    arg_y_arg1_nvar:
      /* ARG1 is ok! */
      if (IsIntTerm(d0))
	d0 = IntOfTerm(d0);
      else if (IsLongIntTerm(d0)) {
	d0 = LongIntOfTerm(d0);
      } else {
	Error(TYPE_ERROR_INTEGER,d0,"arg 1 of arg/3");
	FAIL();
      }

      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->u.yxx.x2);
      deref_head(d1, arg_y_arg2_unk);
    arg_y_arg2_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
	BEGP(pt0);
	pt0 = RepAppl(d1);
	d1 = *pt0;
	if (IsExtensionFunctor((Functor) d1)) {
	  FAIL();
	}
	if ((Int)d0 <= 0 ||
	    d0 > ArityOfFunctor((Functor) d1)) {
	  /* don't complain here for Prolog compatibility 
	  if ((Int)d0 <= 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");	    
	  }
	  */
	  FAIL();
	}
	BEGP(pt1);
	pt1 = Y + PREG->u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,pt0[d0]);
#else
	*pt1 = pt0[d0];
#endif
	ENDP(pt1);
	GONext();
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 != 1 && d0 != 2) {
	  if ((Int)d0 < 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	  }
	  FAIL();
	}
	BEGP(pt1);
	pt1 = Y + PREG->u.yxx.y;
	PREG = NEXTOP(PREG, yxx);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,pt0[d0-1]);
#else
	*pt1 = pt0[d0-1];
#endif
	GONext();
	ENDP(pt1);
	ENDP(pt0);
      }
      else {
	Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_y_arg2_unk, arg_y_arg2_nvar);
      Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, arg_y_arg1_unk, arg_y_arg1_nvar);
      Error(INSTANTIATION_ERROR, d0, "arg 1 of arg/3");;
      ENDP(pt0);
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_arg_y_cv, xxc);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	CELL *Ho = H;
	Term t = MkIntegerTerm(PREG->u.yxc.c); 
	H[0] =  t;
	H[1] = XREG(PREG->u.yxc.xi);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("arg"),3)),H);
	H = Ho;
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = PREG->u.yxc.c;
      /* d0 now got the argument we want */
      BEGD(d1);
      d1 = XREG(PREG->u.yxc.xi);
      deref_head(d1, arg_y_arg2_vc_unk);
    arg_y_arg2_vc_nvar:
      /* d1 now got the structure we want to fetch the argument
       * from */
      if (IsApplTerm(d1)) {
	BEGP(pt0);
	pt0 = RepAppl(d1);
	d1 = *pt0;
	if (IsExtensionFunctor((Functor) d1)) {
	  FAIL();
	}
	if ((Int)d0 <= 0 ||
	    d0 > ArityOfFunctor((Functor) d1)) {
	  /* don't complain here for Prolog compatibility 
	  if ((Int)d0 <= 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");	    
	  }
	  */
	  FAIL();
	}
	BEGP(pt1);
	pt1 = Y + PREG->u.yxc.y;
	PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,pt0[d0]);
#else
	*pt1 = pt0[d0];
#endif
	ENDP(pt1);
	GONext();
	ENDP(pt0);
      }
      else if (IsPairTerm(d1)) {
	BEGP(pt0);
	pt0 = RepPair(d1);
	if (d0 != 1 && d0 != 2) {
	  if ((Int)d0 < 0) {
	    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,
		  MkIntegerTerm(d0),"arg 1 of arg/3");
	  }
	  FAIL();
	}
	BEGP(pt1);
	pt1 = Y + PREG->u.yxc.y;
	PREG = NEXTOP(PREG, yxc);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,pt0[d0-1]);
#else
	*pt1 = pt0[d0-1];
#endif
	ENDP(pt1);
	GONext();
	ENDP(pt0);
      }
      else {
	Error(TYPE_ERROR_COMPOUND, d1, "arg 2 of arg/3");
	FAIL();
      }

      BEGP(pt0);
      deref_body(d1, pt0, arg_y_arg2_vc_unk, arg_y_arg2_vc_nvar);
      Error(INSTANTIATION_ERROR, d1,"arg 2 of arg/3");;
      ENDP(pt0);
      FAIL();
      ENDD(d1);

      ENDD(d0);
      ENDOp();

      Op(p_func2s_vv, xxx);
      /* A1 is a variable */
    restart_func2s:
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	RESET_VARIABLE(H);
	H[1] = XREG(PREG->u.xxx.x1);
	H[2] = XREG(PREG->u.xxx.x2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->u.xxx.x1);
      deref_head(d0, func2s_unk);
    func2s_nvar:
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->u.xxx.x2);
      deref_head(d1, func2s_unk2);
    func2s_nvar2:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1))
	d1 = IntegerOfTerm(d1);
      else {
	Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	FAIL();
      }
      if (!IsAtomicTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->u.xxx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),sla),l);
	GONext();
      }
      else if ((Int)d1 > 0) {
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
	  Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
	pt1 = H;
	*pt1++ = d0;
	d0 = AbsAppl(H);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  gc(0, Y, NEXTOP(NEXTOP(PREG,xxx),sla));
	  setregs();
	  goto restart_func2s;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	H = pt1;
	/* done building the term */
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->u.xxx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),sla),l);
	GONext();
      }	else if ((Int)d1  == 0) {
	XREG(PREG->u.xxx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),sla),l);
	GONext();
      }	else {
	Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_unk2, func2s_nvar2);
      Error(INSTANTIATION_ERROR, d1, "functor/3");
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func2s_unk, func2s_nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_cv, xcx);
      /* A1 is a variable */
    restart_func2s_cv:
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	RESET_VARIABLE(H);
	H[1] = XREG(PREG->u.xcx.c);
	H[2] = XREG(PREG->u.xcx.xi);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      /* We have to build the structure */
      d0 = PREG->u.xcx.c;
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->u.xcx.xi);
      deref_head(d1, func2s_unk2_cv);
    func2s_nvar2_cv:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1))
	d1 = IntegerOfTerm(d1);
      else {
	Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->u.xcx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xcx),sla),l);
	GONext();
      } else if ((Int)d1 > 0) {
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
	  Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
	pt1 = H;
	*pt1++ = d0;
	d0 = AbsAppl(H);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  gc(0, Y, NEXTOP(NEXTOP(PREG,xcx),sla));
	  setregs();
	  goto restart_func2s_cv;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	H = pt1;
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->u.xcx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xcx),sla),l);
	GONext();
      }	else if (d1  == 0) {
	XREG(PREG->u.xxx.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxx),sla),l);
	GONext();
      }	else {
	Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_unk2_cv, func2s_nvar2_cv);
      Error(INSTANTIATION_ERROR, d1, "functor/3");
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_func2s_vc, xxc);
      /* A1 is a variable */
    restart_func2s_vc:
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	Term ti;
	CELL *hi = H;

	ti = MkIntegerTerm((Int)(PREG->u.xxc.c));
	RESET_VARIABLE(H);
	H[1] = XREG(PREG->u.xxc.xi);
	H[2] = ti;
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
	H = hi;
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->u.xxc.xi);
      deref_head(d0, func2s_unk_vc);
    func2s_nvar_vc:
      BEGD(d1);
      d1 = PREG->u.xxc.c;
      if (!IsAtomicTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	XREG(PREG->u.xxc.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),sla),l);
	GONext();
      }
      /* now let's build a compound term */
      if (d1 == 0) {
	XREG(PREG->u.xxc.x) = d0;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),sla),l);
	GONext();
      }
      if (!IsAtomTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      BEGP(pt1);
      if (!IsAtomTerm(d0)) {
	FAIL();
      }
      else
	d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
      pt1 = H;
      *pt1++ = d0;
      d0 = AbsAppl(H);
      if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	/* make sure we have something to show for our trouble */
	saveregs();
	gc(0, Y, NEXTOP(NEXTOP(PREG,xxc),sla));
	setregs();
	goto restart_func2s_vc;
      }
      while ((Int)d1--) {
	RESET_VARIABLE(pt1);
	pt1++;
      }
      /* done building the term */
      H = pt1;
      ENDP(pt1);
      ENDD(d1);
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      XREG(PREG->u.xxc.x) = d0;
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, xxc),sla),l);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, func2s_unk_vc, func2s_nvar_vc);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_vv, yxx);
      /* A1 is a variable */
    restart_func2s_y:
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	RESET_VARIABLE(H);
	H[1] = XREG(PREG->u.yxx.x1);
	H[2] = XREG(PREG->u.yxx.x2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->u.yxx.x1);
      deref_head(d0, func2s_y_unk);
    func2s_y_nvar:
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->u.yxx.x2);
      deref_head(d1, func2s_y_unk2);
    func2s_y_nvar2:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1))
	d1 = IntegerOfTerm(d1);
      else {
	Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	FAIL();
      }
      if (!IsAtomicTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
	BEGP(pt1);
	pt1 = Y + PREG->u.yxx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      } else if ((Int)d1 > 0) {
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
	  Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
	pt1 = H;
	*pt1++ = d0;
	d0 = AbsAppl(H);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  gc(0, Y, NEXTOP(NEXTOP(PREG,yxx),sla));
	  setregs();
	  goto restart_func2s_y;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	H = pt1;
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	BEGP(pt1);
	pt1 = Y + PREG->u.yxx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }	else if (d1  == 0) {
	BEGP(pt1);
	pt1 = Y + PREG->u.yxx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxx),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }	else {
	Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_y_unk2, func2s_y_nvar2);
      Error(INSTANTIATION_ERROR, d1, "functor/3");
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func2s_y_unk, func2s_y_nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_cv, ycx);
      /* A1 is a variable */
    restart_func2s_y_cv:
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	RESET_VARIABLE(H);
	H[1] = XREG(PREG->u.ycx.c);
	H[2] = XREG(PREG->u.ycx.xi);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = PREG->u.ycx.c;
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = XREG(PREG->u.ycx.xi);
      deref_head(d1, func2s_y_unk_cv);
    func2s_y_nvar_cv:
      /* Uuuff, the second and third argument are bound */
      if (IsIntegerTerm(d1)) {
	d1 = IntegerOfTerm(d1);
      } else {
	Error(TYPE_ERROR_INTEGER,d1,"functor/3");
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	BEGP(pt1);
	pt1 = Y + PREG->u.ycx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, ycx),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }
      else if ((Int)d1 > 0) {
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
	  Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  FAIL();
	}
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
	BEGP(pt1);
	pt1 = H;
	*pt1++ = d0;
	d0 = AbsAppl(H);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  gc(0, Y, NEXTOP(NEXTOP(PREG,ycx),sla));
	  setregs();
	  goto restart_func2s_y_cv;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	H = pt1;
	ENDP(pt1);
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	BEGP(pt1);
	pt1 = Y + PREG->u.ycx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, ycx),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }	else if (d1  == 0) {
	BEGP(pt1);
	pt1 = Y + PREG->u.ycx.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, ycx),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }	else {
	Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	FAIL();
      }

      BEGP(pt1);
      deref_body(d1, pt1, func2s_y_unk_cv, func2s_y_nvar_cv);
      Error(INSTANTIATION_ERROR, d1, "functor/3");
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);
      ENDD(d0);
      ENDOp();

      Op(p_func2s_y_vc, yxc);
      /* A1 is a variable */
    restart_func2s_y_vc:
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	Term ti;
	CELL *hi = H;

	ti = MkIntegerTerm((Int)(PREG->u.yxc.c));
	RESET_VARIABLE(H);
	H[1] = XREG(PREG->u.yxc.xi);
	H[2] = ti;
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
	H = hi;
      }
#endif	/* LOW_LEVEL_TRACE */
      /* We have to build the structure */
      BEGD(d0);
      d0 = XREG(PREG->u.yxc.xi);
      deref_head(d0, func2s_y_unk_vc);
    func2s_y_nvar_vc:
      BEGD(d1);
      d1 = PREG->u.yxc.c;
      if (!IsAtomicTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
	/* else if arity is 0 just pass d0 through */
	/* Ding, ding, we made it */
	BEGP(pt1);
	pt1 = Y + PREG->u.yxc.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }
      if (d1 == 0) {
	BEGP(pt1);
	pt1 = Y + PREG->u.yxc.y;
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
	Bind_Local(pt1,d0);
#else
	*pt1 = d0;
#endif
	ENDP(pt1);
	GONext();
      }
      if (!IsAtomTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      /* now let's build a compound term */
      if (!IsAtomTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }
      BEGP(pt1);
      if (!IsAtomTerm(d0)) {
	FAIL();
      }
      else 
	d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
      pt1 = H;
      *pt1++ = d0;
      d0 = AbsAppl(H);
      if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	/* make sure we have something to show for our trouble */
	saveregs();
	gc(0, Y, NEXTOP(NEXTOP(PREG,yxc),sla));
	setregs();
	goto restart_func2s_y_vc;
      }
      while ((Int)d1--) {
	RESET_VARIABLE(pt1);
	pt1++;
      }
      /* done building the term */
      H = pt1;
      ENDP(pt1);
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      BEGP(pt1);
      pt1 = Y + PREG->u.yxc.y;
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, yxc),sla),l);
#if defined(SBA) && defined(FROZEN_REGS)
      Bind_Local(pt1,d0);
#else
      *pt1 = d0;
#endif
      ENDP(pt1);
      ENDD(d1);
      GONext();

      BEGP(pt1);
      deref_body(d0, pt1, func2s_y_unk_vc, func2s_y_nvar_vc);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_xx, xxx);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	H[0] = XREG(PREG->u.xxx.x);
	RESET_VARIABLE(H+1);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->u.xxx.x);
      deref_head(d0, func2f_xx_unk);
    func2f_xx_nvar:
      if (IsApplTerm(d0)) {
	Functor d1 = FunctorOfTerm(d0);
	if (IsExtensionFunctor(d1)) {
	  XREG(PREG->u.xxx.x1) = d0;
	  XREG(PREG->u.xxx.x2) = MkIntTerm(0);
	  PREG = NEXTOP(PREG, xxx);
	  GONext();
	}
	XREG(PREG->u.xxx.x1) = MkAtomTerm(NameOfFunctor(d1));
	XREG(PREG->u.xxx.x2) = MkIntegerTerm(ArityOfFunctor(d1));
	PREG = NEXTOP(PREG, xxx);
	GONext();
      } else if (IsPairTerm(d0)) {
	XREG(PREG->u.xxx.x1) = TermDot;
	XREG(PREG->u.xxx.x2) = MkIntTerm(2);
	PREG = NEXTOP(PREG, xxx);
	GONext();
      } else {
	XREG(PREG->u.xxx.x1) = d0;
	XREG(PREG->u.xxx.x2) = MkIntTerm(0);
	PREG = NEXTOP(PREG, xxx);
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_xx_unk, func2f_xx_nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_xy, xyx);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	H[0] = XREG(PREG->u.xyx.x);
	RESET_VARIABLE(H+1);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->u.xyx.x);
      deref_head(d0, func2f_xy_unk);
    func2f_xy_nvar:
      if (IsApplTerm(d0)) {
	Functor d1 = FunctorOfTerm(d0);
	CELL *pt0 = Y+PREG->u.xyx.y2;
	if (IsExtensionFunctor(d1)) {
	  XREG(PREG->u.xyx.x1) = d0;
	  PREG = NEXTOP(PREG, xyx);
	  *pt0 = MkIntTerm(0);
	  GONext();
	}
	XREG(PREG->u.xyx.x1) = MkAtomTerm(NameOfFunctor(d1));
	PREG = NEXTOP(PREG, xyx);
	*pt0 = MkIntegerTerm(ArityOfFunctor(d1));
	GONext();
      } else if (IsPairTerm(d0)) {
	CELL *pt0 = Y+PREG->u.xyx.y2;
	XREG(PREG->u.xyx.x1) = TermDot;
	PREG = NEXTOP(PREG, xyx);
	*pt0 = MkIntTerm(2);
	GONext();
      } else {
	CELL *pt0 = Y+PREG->u.xyx.y2;
	XREG(PREG->u.xyx.x1) = d0;
	PREG = NEXTOP(PREG, xyx);
	*pt0 = MkIntTerm(0);
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_xy_unk, func2f_xy_nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_yx, yxx);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	H[0] = XREG(PREG->u.yxx.x2);
	RESET_VARIABLE(H+1);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->u.yxx.x2);
      deref_head(d0, func2f_yx_unk);
    func2f_yx_nvar:
      if (IsApplTerm(d0)) {
	Functor d1 = FunctorOfTerm(d0);
	CELL *pt0 = Y+PREG->u.yxx.y;
	if (IsExtensionFunctor(d1)) {
	  XREG(PREG->u.yxx.x1) = MkIntTerm(0);
	  PREG = NEXTOP(PREG, yxx);
	  *pt0 = d0;
	  GONext();
	}
	XREG(PREG->u.yxx.x1) = MkIntegerTerm(ArityOfFunctor(d1));
	PREG = NEXTOP(PREG, yxx);
	*pt0 = MkAtomTerm(NameOfFunctor(d1));
	GONext();
      } else if (IsPairTerm(d0)) {
	CELL *pt0 = Y+PREG->u.yxx.y;
	XREG(PREG->u.yxx.x1) = MkIntTerm(2);
	PREG = NEXTOP(PREG, yxx);
	*pt0 = TermDot;
	GONext();
      } else {
	CELL *pt0 = Y+PREG->u.yxx.y;
	XREG(PREG->u.yxx.x1) = MkIntTerm(0);
	PREG = NEXTOP(PREG, yxx);
	*pt0 = d0;
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_yx_unk, func2f_yx_nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_func2f_yy, yyx);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace) {
	H[0] = XREG(PREG->u.yyx.x);
	RESET_VARIABLE(H+1);
	RESET_VARIABLE(H+2);
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),H);
      }
#endif	/* LOW_LEVEL_TRACE */
      BEGD(d0);
      d0 = XREG(PREG->u.yyx.x);
      deref_head(d0, func2f_yy_unk);
    func2f_yy_nvar:
      if (IsApplTerm(d0)) {
	Functor d1 = FunctorOfTerm(d0);
	CELL *pt0 = Y+PREG->u.yyx.y1;
	CELL *pt1 = Y+PREG->u.yyx.y2;
	if (IsExtensionFunctor(d1)) {
	  PREG = NEXTOP(PREG, yyx);
	  *pt0 =  d0;
	  *pt1 = MkIntTerm(0);
	  GONext();
	}
	PREG = NEXTOP(PREG, yyx);
	*pt0 = MkAtomTerm(NameOfFunctor(d1));
	*pt1 = MkIntegerTerm(ArityOfFunctor(d1));
	GONext();
      } else if (IsPairTerm(d0)) {
	CELL *pt0 = Y+PREG->u.yyx.y1;
	CELL *pt1 = Y+PREG->u.yyx.y2;
	PREG = NEXTOP(PREG, yyx);
	*pt0 = TermDot;
	*pt1 = MkIntTerm(2);
	GONext();
      } else {
	CELL *pt0 = Y+PREG->u.yyx.y1;
	CELL *pt1 = Y+PREG->u.yyx.y2;
	PREG = NEXTOP(PREG, yyx);
	*pt0 = d0;
	*pt1 = MkIntTerm(0);
	GONext();
      }

      BEGP(pt1);
      deref_body(d0, pt1, func2f_yy_unk, func2f_yy_nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDD(d0);
      ENDOp();

      Op(p_functor, e);
#ifdef LOW_LEVEL_TRACER
      if (do_low_level_trace)
	low_level_trace(enter_pred,RepPredProp(GetPredProp(LookupAtom("functor"),3)),XREGS+1);
#endif	/* LOW_LEVEL_TRACE */
      restart_functor:
      BEGD(d0);
      d0 = ARG1;
      deref_head(d0, func_unk);
    func_nvar:
      /* A1 is bound */
      BEGD(d1);
      if (IsApplTerm(d0)) {
	d1 = *RepAppl(d0);
	if (IsExtensionFunctor((Functor) d1)) {
	  if (d1 <= (CELL)FunctorDouble && d1 >= (CELL)FunctorLongInt ) {
	    d1 = MkIntTerm(0);
	  } else
	    FAIL();
	} else {
	    d0 = MkAtomTerm(NameOfFunctor((Functor) d1));
	    d1 = MkIntTerm(ArityOfFunctor((Functor) d1));
	}
      }
      else if (IsPairTerm(d0)) {
	d0 = TermDot;
	d1 = MkIntTerm(2);
      }
      else {
	d1 = MkIntTerm(0);
      }
      /* d1 and d0 now have the two arguments */
      /* let's go and bind them */
      {
	register CELL arity = d1;

	d1 = ARG2;
	deref_head(d1, func_nvar_unk);
      func_nvar_nvar:
	/* A2 was bound */
	if (d0 != d1) {
	  FAIL();
	}
	/* I have to this here so that I don't have a jump to a closing bracket */
	d0 = arity;
	goto func_bind_x3;

	BEGP(pt0);
	deref_body(d1, pt0, func_nvar_unk, func_nvar_nvar);
	/* A2 is a variable, go and bind it */
	BIND(pt0, d0, bind_func_nvar_var);
#ifdef COROUTINING
	DO_TRAIL(pt0, d0);
	if (pt0 < H0) WakeUp(pt0);
      bind_func_nvar_var:
#endif
	/* I have to this here so that I don't have a jump to a closing bracket */
	d0 = arity;
	ENDP(pt0);
      func_bind_x3:
	/* now let's process A3 */
	d1 = ARG3;
	deref_head(d1, func_nvar3_unk);
      func_nvar3_nvar:
	/* A3 was bound */
	if (d0 != d1) {
	  FAIL();
	}
	/* Done */
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),sla),l);
	GONext();

	BEGP(pt0);
	deref_body(d1, pt0, func_nvar3_unk, func_nvar3_nvar);
	/* A3 is a variable, go and bind it */
	PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),sla),l);
	BIND(pt0, d0, bind_func_nvar3_var);
	/* Done */
#ifdef COROUTINING
	DO_TRAIL(pt0, d0);
	if (pt0 < H0) WakeUp(pt0);
      bind_func_nvar3_var:
#endif
	GONext();


	ENDP(pt0);

      }
      ENDD(d1);

      BEGP(pt0);
      deref_body(d0, pt0, func_unk, func_nvar);
      /* A1 is a variable */
      /* We have to build the structure */
      d0 = ARG2;
      deref_head(d0, func_var_2unk);
    func_var_2nvar:
      /* we do, let's get the third argument */
      BEGD(d1);
      d1 = ARG3;
      deref_head(d1, func_var_3unk);
    func_var_3nvar:
      /* Uuuff, the second and third argument are bound */
      if (IsIntTerm(d1))
	d1 = IntOfTerm(d1);
      else {
	Error(TYPE_ERROR_INTEGER,ARG3,"functor/3");
	FAIL();
      }
      if (!IsAtomicTerm(d0)) {
	Error(TYPE_ERROR_ATOM,d0,"functor/3");
	FAIL();
      }      /* We made it!!!!! we got in d0 the name, in d1 the arity and
       * in pt0 the variable to bind it to. */
      if (d0 == TermDot && d1 == 2) {
	RESET_VARIABLE(H);
	RESET_VARIABLE(H+1);
	d0 = AbsPair(H);
	H += 2;
      }
      else if ((Int)d1 > 0) {
	/* now let's build a compound term */
	if (!IsAtomTerm(d0)) {
	  Error(TYPE_ERROR_ATOM,d0,"functor/3");
	  FAIL();
	}
	BEGP(pt1);
	if (!IsAtomTerm(d0)) {
	  FAIL();
	}
	else
	  d0 = (CELL) MkFunctor(AtomOfTerm(d0), (Int) d1);
	pt1 = H;
	*pt1++ = d0;
	d0 = AbsAppl(H);
	if (pt1+d1 > ENV || pt1+d1 > (CELL *)B) {
	  /* make sure we have something to show for our trouble */
	  saveregs();
	  gc(3, Y, NEXTOP(NEXTOP(PREG,e),sla));
	  setregs();
	  goto restart_functor;
	}
	while ((Int)d1--) {
	  RESET_VARIABLE(pt1);
	  pt1++;
	}
	/* done building the term */
	H = pt1;
	ENDP(pt1);
      }	else if ((Int)d1  < 0) {
	Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,MkIntegerTerm(d1),"functor/3");
	FAIL();
      }
      /* else if arity is 0 just pass d0 through */
      /* Ding, ding, we made it */
      PREG = NEXTOP(NEXTOP(NEXTOP(PREG, e),sla),l);
      BIND(pt0, d0, bind_func_var_3nvar);
#ifdef COROUTINING
      DO_TRAIL(pt0, d0);
      if (pt0 < H0) WakeUp(pt0);
    bind_func_var_3nvar:
#endif
      GONext();


      BEGP(pt1);
      deref_body(d1, pt1, func_var_3unk, func_var_3nvar);
      Error(INSTANTIATION_ERROR, d1, "functor/3");
      ENDP(pt1);
      /* Oops, third argument was unbound */
      FAIL();
      ENDD(d1);

      BEGP(pt1);
      deref_body(d0, pt1, func_var_2unk, func_var_2nvar);
      Error(INSTANTIATION_ERROR, d0, "functor/3");
      ENDP(pt1);
      /* Oops, second argument was unbound too */
      FAIL();
      ENDP(pt0);
      ENDD(d0);
      ENDOp();

#if !USE_THREADED_CODE
    default:
      PREG = Error(SYSTEM_ERROR, MkIntegerTerm(opcode), "trying to execute invalid YAAM instruction %d", opcode);
      JMPNext();
    }
  }
#endif

#if USE_THREADED_CODE
#if PUSH_REGS
  restore_absmi_regs(old_regs);
#endif
#if BP_FREE
  P1REG = PCBACKUP;
#endif
  return (0);
#if PUSH_REGS

#undef REGS
#define REGS (*regp)

#else

#endif /* PUSH_REGS */

#if SHADOW_REGS
#undef REGS
#endif /* SHADOW_REGS */
#endif

}

