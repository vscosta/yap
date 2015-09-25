/*************************************************************************
*									 *
  *	 Yap Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		jit_traced.c						 *
* comments:	Portable abstract machine interpreter                    *
* Last:     $Date: 2008-08-13 01:16:26 $,$Author: vsc $		 *
*									 *
*************************************************************************/


/**

@file jit_traced.c

@defgroup JIT_Impl Just-In-Time Compiler Implementation
@ingroup 

We next discuss several issues on trying to make Prolog programs run
fast in YAP. We assume two different programming styles:

+ Execution of <em>deterministic</em> programs ofte
n
boils down to a recursive loop of the form:

~~~~~
loop(Env) :-
        do_something(Env,NewEnv),
        loop(NewEnv).
~~~~
 */
 
#if YAP_JIT

//#define __YAP_TRACED 1

#define IN_TRACED_ABSMI_C 1

// #ifndef _NATIVE

#define HAS_CACHE_REGS 1


#include "absmi.h"
#include "heapgc.h"

#include "cut_c.h"

Int traced_absmi(void);

#ifndef PUSH_X
/* keep X as a global variable */

Term Yap_XREGS[MaxTemps];	/* 29                                     */

#endif


// #include "print_preg.h"
//#include "sprint_op.hpp"
//#include "print_op.hpp"

static Term
interrupt_pexecute( PredEntry *pen USES_REGS ) {
  return 0;
}

#include "IsGround.h"
#include "yaam_macros.hpp"
#include "fprintblock.h"

#if YAP_DBG_PREDS
#include "debug_printers.h"
#endif

// ref to JIT compiler
JIT_Compiler *J;


extern NativeContext *NativeArea;
extern IntermediatecodeContext *IntermediatecodeArea;

extern CELL l;

CELL nnexec;

void shutdown_llvm(void);

short global;
yamop* HEADPREG;
CELL BLOCK;
CELL BLOCKADDRESS;
CELL FAILED;

#undef SHADOW_P
#undef SHADOW_CP
#undef SHADOW_HB
#undef SHADOW_Y
#undef SHADOW_S

#undef  PREG
#define PREG P

#undef  CPREG
#define CPREG CP

#undef  SREG
#define SREG S

#undef  YREG
#define YREG YENV

#undef setregs
#define setregs()

#undef saveregs
#define saveregs()

#include "arith2.h"

Int
traced_absmi(void)
{
  CACHE_REGS

    static void *OpAddress[] =
    {
#define OPCODE(OP,TYPE) && _##OP
#include "YapOpcodes.h"
#undef  OPCODE
};

  /* The indexing register so that we will not destroy ARG1 without
   * reason */
#define I_R (XREGS[0])

  static void *control_labels[] = { &&fail, &&NoStackCut, &&NoStackCommitY, &&NoStackCutE, &&NoStackCutT, &&NoStackEither, &&NoStackExecute, &&NoStackCall, &&NoStackDExecute, &&NoStackDeallocate, &&notrailleft, &&NoStackFail, &&NoStackCommitX };

#if YAP_STAT_PREDS
  struct timeval timstart, timend;
struct rusage rustart, ruend;
#endif
curtrace = NULL;
curpreg = NULL;
globalcurblock = NULL;
ineedredefinedest = 0;
NativeArea = (NativeContext*)malloc(sizeof(NativeContext));
NativeArea->area.p = NULL;
NativeArea->area.ok = NULL;
NativeArea->area.pc = NULL;
#if YAP_STAT_PREDS
NativeArea->area.nrecomp = NULL;
NativeArea->area.compilation_time = NULL;
NativeArea->area.native_size_bytes = NULL;
NativeArea->area.trace_size_bytes = NULL;
NativeArea->success = NULL;
->runs = NULL;
NativeArea->t_runs = NULL;
#endif
NativeArea->n = 0;
IntermediatecodeArea = (IntermediatecodeContext*)malloc(sizeof(IntermediatecodeContext));
IntermediatecodeArea->area.t = NULL;
IntermediatecodeArea->area.ok = NULL;
IntermediatecodeArea->area.isactive = NULL;
IntermediatecodeArea->area.lastblock = NULL;
#if YAP_STAT_PREDS
IntermediatecodeArea->area.profiling_time = NULL;
#endif
IntermediatecodeArea->n = 0;
nnexec = 0;
l = 0;
  
  

setregs();

CACHE_A1();

 reset_absmi:

  SP = SP0;

  /* when we start we are not in write mode */

  {
    op_numbers opcode = _Ystop;
    goto critical_lbl;

    nextop_write:

     opcode = Yap_op_from_opcode( PREG->y_u.o.opcw );
    goto op_switch;

    
    nextop:

    opcode = Yap_op_from_opcode( PREG->opc );

  op_switch:

#if !USE_THREADED_CODE
    switch (opcode) {
#endif
        
#if !OS_HANDLES_TR_OVERFLOW
    notrailleft:
      /* if we are within indexing code, the system may have to
       * update a S */
      {
        CELL cut_b;

#ifdef SHADOW_S
        S = SREG;
#endif
        /* YREG was pointing to where we were going to build the
         * next choice-point. The stack shifter will need to know this
         * to move the local stack */
        SET_ASP(YREG, E_CB*sizeof(CELL));
        cut_b = LCL0-(CELL *)(ASP[E_CB]);
        saveregs();
        if(!Yap_growtrail (0, false)) {
          Yap_NilError(RESOURCE_ERROR_TRAIL,"YAP failed to reserve %ld bytes in growtrail",sizeof(CELL) * K16);
          setregs();
          FAIL();
        }
        setregs();
#ifdef SHADOW_S
        SREG = S;
#endif
        if (SREG == ASP) {
          SREG[E_CB] = (CELL)(LCL0-cut_b);
        }
      }
      goto reset_absmi;
#endif
        
      // move instructions to separate file
      // so that they are easier to analyse.
#include "../C/traced_absmi_insts.h"
#if YAPOR
#include "../OPTYap/traced_or.insts.h"
#endif
#if TABLING
#include "../OPTYap/traced_tab.insts.h"
#include "../OPTYap/traced_tab.tries.insts.h"
#endif
    
        
#if _NATIVE
      default:
      saveregs();
      Yap_Error(SYSTEM_ERROR_INTERNAL, MkIntegerTerm(opcode), "trying to execute invalid YAAM instruction %d", opcode);
      setregs();
      FAIL();
    }
#endif
  }

  return (0);

}

#endif /* YAP_JIT */

