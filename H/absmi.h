/*************************************************************************
 *                   *
 *   YAP Prolog                *
 *                   *
 *  Yap Prolog was developed at NCCUP - Universidade do Porto  *
 *                   *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997  *
 *                   *
 **************************************************************************
 *                   *
 * File:    absmi.h              *
 * Last rev:                 *
 * mods:                   *
 * comments:  Portable abstract machine interpreter includes           *
 *                   *
 *************************************************************************/


#ifndef ABSMI_H

#define ABSMI_H 1

#if NATIVE_EXECUTION
/* just a stub */
#else
#define EXEC_NATIVE(X)
#define MAX_INVOCATION 1024
#define Yapc_Compile(P) 0
#endif

#ifdef __cplusplus
#define register
#endif


#if TABLING
#define FROZEN_STACKS 1
//#define MULTIPLE_STACKS 1
#endif

/***************************************************************
 * Macros for register manipulation                             *
 ***************************************************************/
/*
 * Machine and compiler dependent definitions
 */
#if 1 //def __GNUC__

#ifdef hppa
#define SHADOW_P 1
#define SHADOW_Y 1
#define SHADOW_REGS 1
#define USE_PREFETCH 1
#endif

#ifdef __alpha
#define SHADOW_P 1
#define SHADOW_Y 1
#define SHADOW_REGS 1
#define USE_PREFETCH 1
#elif defined(_POWER) || defined(__POWERPC__)
#define SHADOW_P 1
#define SHADOW_REGS 1
#define USE_PREFETCH 1
#elif defined(__x86_64__)
#define SHADOW_P 1
#ifdef BP_FREE
#undef BP_FREE
#endif
#undef SHADOW_S
//#define SHADOW_Y       1
#define S_IN_MEM 1
#define Y_IN_MEM 1
#define TR_IN_MEM 1
#define LIMITED_PREFETCH 1

#elif defined(__i386__)
#undef SHADOW_S

#define Y_IN_MEM 1
#define S_IN_MEM 1
#define TR_IN_MEM 1
#define HAVE_FEW_REGS 1
#define LIMITED_PREFETCH 1

#ifdef BP_FREE
/***************************************************************
 * Use bp as PREG for X86 machines                   *
 ***************************************************************/
#if defined(IN_ABSMI_C)
register struct yami *P1REG asm("bp"); /* can't use yamop before Yap.h */
#define PREG P1REG
#endif
#define NEEDS_TO_SET_PC 1
#endif /* BP_FREE */
#endif /* i386 */

#ifdef sparc
#define SHADOW_P 1
#ifdef BP_FREE
#undef BP_FREE
#endif
#define S_IN_MEM 1
#define Y_IN_MEM 1
#define TR_IN_MEM 1
#endif /* sparc_ */


#if defined(__arm__) || defined(__thumb__) || defined(mips) ||                 \
    defined(__mips64) || defined(__arch64__)

#define Y_IN_MEM 1
#define S_IN_MEM 1
#define TR_IN_MEM 1
#define HAVE_FEW_REGS 1
#endif

#else /* other compilers */

#define S_IN_MEM 1

/* This works for xlc under AIX 3.2.5 */
#ifdef _IBMR2
#define SHADOW_P 1
#define SHADOW_REGS 1
#define SHADOW_S 1
#endif

#if defined(__x86_64__)
#define Y_IN_MEM 1
#define TR_IN_MEM 1
#elif defined(i386)
#define Y_IN_MEM 1
#define TR_IN_MEM 1
#define HAVE_FEW_REGS 1
#endif

#ifdef _HPUX_SOURCE
#define SHADOW_P 1
#define SHADOW_Y 1
#define SHADOW_S 1
#define SHADOW_CP 1
#define SHADOW_HB 1
#define USE_PREFETCH 1
#endif

#endif /* __GNUC__ */

#include "Yap.h"
#include "clause.h"
#include "YapEval.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#if YAP_JIT
#include "amijit.h"
#endif
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#ifdef USE_SYSTEM_MALLOC
#include "YapHeap.h"
#endif
#ifdef TABLING
#include "tab.macros.h"
#endif /* TABLING */
#ifdef LOW_LEVEL_TRACER
#include "tracer.h"
#endif

#ifdef DEBUG
/**********************************************************************
 *                                                                    *
 *                 Debugging Auxiliary variables                      *
 *                                                                    *
 **********************************************************************/
#include <stdio.h>
#endif

#if PUSH_REGS

/***************************************************************
 * Trick to copy REGS into absmi local environment              *
 ***************************************************************/
INLINE_ONLY void init_absmi_regs(REGSTORE *absmi_regs);

/* regp is a global variable */

INLINE_ONLY void init_absmi_regs(REGSTORE *absmi_regs) {
  CACHE_REGS
  memmove(absmi_regs, Yap_regp, sizeof(REGSTORE));
}

INLINE_ONLY void restore_absmi_regs(REGSTORE *old_regs);

INLINE_ONLY void restore_absmi_regs(REGSTORE *old_regs) {
  CACHE_REGS
  memmove(old_regs, Yap_regp, sizeof(REGSTORE));
#ifdef THREADS
  pthread_setspecific(Yap_yaamregs_key, (void *)old_regs);
  LOCAL_ThreadHandle.current_yaam_regs = old_regs;
#else
  Yap_regp = old_regs;
#endif
}
#endif /* PUSH_REGS */

/*****************************************************************

   Machine Dependent stuff

******************************************************************/

#ifdef LONG_LIVED_REGISTERS

#define BEGP(TMP)

#define ENDP(TMP)

#define BEGD(TMP)

#define ENDD(TMP)

#else

#define BEGP(TMP)                                                              \
  {                                                                            \
    register CELL *TMP

#define ENDP(TMP) }

#define BEGD(TMP)                                                              \
  {                                                                            \
    register CELL TMP

#define ENDD(TMP) }

#endif /* LONG_LIVED_REGISTERS */

#define BEGCHO(TMP)                                                            \
  {                                                                            \
    register choiceptr TMP

#define ENDCHO(TMP) }

/***************************************************************
 * YREG is usually, but not always, a register. This affects       *
 * choicepoints                                                 *
 ***************************************************************/

#if Y_IN_MEM

#define CACHE_Y(A)                                                             \
  {                                                                            \
    register CELL *S_YREG = ((CELL *)(A))

#define ENDCACHE_Y()                                                           \
  YREG = S_YREG;                                                               \
  }

#define B_YREG ((choiceptr)(S_YREG))

#else

#define S_YREG (YREG)

#define B_YREG ((choiceptr)(YREG))

#define CACHE_Y(A)                                                             \
  {                                                                            \
    YREG = ((CELL *)(A))

#define ENDCACHE_Y() }

#endif

#if Y_IN_MEM

#define CACHE_Y_AS_ENV(A)                                                      \
  {                                                                            \
    register CELL *ENV_YREG = (A)

#define FETCH_Y_FROM_ENV(A) ENV_YREG = (A)

#define WRITEBACK_Y_AS_ENV() YREG = ENV_YREG

#define ENDCACHE_Y_AS_ENV() }

#define saveregs_and_ycache()                                                  \
  YREG = ENV_YREG;                                                             \
  saveregs()

#define setregs_and_ycache()                                                   \
  ENV_YREG = YREG;                                                             \
  setregs()

#else

#define ENV_YREG (YREG)

#define WRITEBACK_Y_AS_ENV()

#define CACHE_Y_AS_ENV(A)                                                      \
  {                                                                            \
    YREG = (A)

#define FETCH_Y_FROM_ENV(A) ((YENV) = (A))

#define ENDCACHE_Y_AS_ENV() }

#define saveregs_and_ycache() saveregs()

#define setregs_and_ycache() setregs()

#endif

/***************************************************************
 * TR is usually, but not always, a register. This affects      *
 * backtracking                                                 *
 ***************************************************************/

#ifdef _NATIVE

#define CACHE_TR(A) register tr_fr_ptr S_TR = (A)

#define RESTORE_TR() TR = S_TR

#define ENDCACHE_TR()

#else

#define CACHE_TR(A)                                                            \
  {                                                                            \
    register tr_fr_ptr S_TR = (A)

#define RESTORE_TR() TR = S_TR

#define ENDCACHE_TR() }

#endif

/***************************************************************
 * S is usually, but not always, a register (X86 machines).     *
 * This affects unification instructions                        *
 ***************************************************************/

#if !SHADOW_S
#define SREG S
#endif

#if S_IN_MEM

/* jump through hoops because idiotic gcc will go and read S from
   memory every time it uses S :-( */

#define CACHE_S()                                                              \
  {                                                                            \
    register CELL *S_SREG;

#define ENDCACHE_S() }

#ifndef _NATIVE

#define READ_IN_S() S_SREG = SREG

#define CACHE_A1() (SREG = (CELL *)ARG1)

#define CACHED_A1() ((CELL)SREG)

#else

#define READ_IN_S() S_SREG = *_SREG

#define CACHE_A1() ((*_SREG) = (CELL *)ARG1)

#define CACHED_A1() ((CELL)(*_SREG))

   #endif

#else

/* do nothing if you are on a decent machine */

#define CACHE_S() {

#define ENDCACHE_S() }

#define READ_IN_S()

#define CACHE_A1()

#define CACHED_A1() (ARG1)

#define S_SREG SREG

#endif

#ifndef _NATIVE

#define WRITEBACK_S(X) SREG = (X)

#else

#define WRITEBACK_S(X) *_SREG = (X)

#endif

/*****************************************************************

   End of Machine Dependent stuff

******************************************************************/

/*****************************************************************

   Prefetch is a technique to obtain the place to jump to before actually
   executing instructions. It can speed up some machines, by having the
   next opcode in place before it is actually required for jumping.

******************************************************************/

#if __YAP_TRACED

#define DO_PREFETCH(TYPE)

#define DO_PREFETCH_W(TYPE)

#elif USE_THREADED_CODE

#ifndef _NATIVE

#if YAP_JIT

#define DO_PREFETCH(TYPE)                                                      \
  if (ExpEnv.config_struc.current_displacement)                                \
    to_go = (void *)OpAddress[Yap_op_from_opcode(NEXTOP(PREG, TYPE)->opc) +    \
                              ExpEnv.config_struc.current_displacement];       \
  else                                                                         \
    to_go = (void *)(NEXTOP(PREG, TYPE)->opc);

#define DO_PREFETCH_W(TYPE)                                                    \
  if (ExpEnv.config_struc.current_displacement)                                \
    to_go =                                                                    \
        (void *)OpAddress[Yap_op_from_opcode(NEXTOP(PREG, TYPE)->y_u.o.opcw) + \
                          ExpEnv.config_struc.current_displacement];           \
  else                                                                         \
    to_go = (void *)(NEXTOP(PREG, TYPE)->y_u.o.opcw);

#else /* YAP_JIT */

#define DO_PREFETCH(TYPE) to_go = (void *)(NEXTOP(PREG, TYPE)->opc)

#define DO_PREFETCH_W(TYPE) to_go = (void *)(NEXTOP(PREG, TYPE)->y_u.o.opcw)

#endif /* YAP_JIT */

#else /* _NATIVE */

#define DO_PREFETCH(TYPE)

#define DO_PREFETCH_W(TYPE)

#endif /* _NATIVE */

#ifndef _NATIVE

#if LIMITED_PREFETCH || USE_PREFETCH

#define ALWAYS_START_PREFETCH(TYPE)                                            \
  {                                                                            \
    register void *to_go;                                                      \
    DO_PREFETCH(TYPE)

#if YAP_JIT
#define ALWAYS_LOOKAHEAD(WHAT)                                                 \
  {                                                                            \
    register void *to_go;                                                      \
    if (ExpEnv.config_struc.current_displacement)                              \
      to_go = (void *)OpAddress[Yap_op_from_opcode(WHAT) +                     \
                                ExpEnv.config_struc.current_displacement];     \
    else                                                                       \
      to_go = (void *)(WHAT);
#else /* YAP_JIT */
#define ALWAYS_LOOKAHEAD(WHAT)                                                 \
  {                                                                            \
    register void *to_go = (void *)(WHAT);
#endif /* YAP_JIT */

#define ALWAYS_START_PREFETCH_W(TYPE)                                          \
  {                                                                            \
    register void *to_go;                                                      \
    DO_PREFETCH_W(TYPE)

#else

#define ALWAYS_START_PREFETCH(TYPE) {

#define ALWAYS_START_PREFETCH_W(TYPE) {

#define ALWAYS_LOOKAHEAD(WHERE) {

#endif /* LIMITED_PREFETCH||USE_PREFETCH */

#else /* _NATIVE */

#if LIMITED_PREFETCH || USE_PREFETCH

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_LOOKAHEAD(WHAT)

#define ALWAYS_START_PREFETCH_W(TYPE)

#else

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_START_PREFETCH_W(TYPE)

#define ALWAYS_LOOKAHEAD(WHERE)

#endif /* LIMITED_PREFETCH||USE_PREFETCH */

#endif /* _NATIVE */

#ifndef _NATIVE

#ifdef USE_PREFETCH

#define START_PREFETCH(TYPE) ALWAYS_START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE) ALWAYS_START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()                                                        \
  {                                                                            \
    register void *to_go;

#define PREFETCH_OP(X)                                                         \
  if (ExpEnv.config_struc.current_displacement)                                \
    to_go = (void *)OpAddress[Yap_op_from_opcode((X)->opc) +                   \
                              ExpEnv.config_struc.current_displacement];       \
  else                                                                         \
    to_go = (void *)((X)->opc);

#else

#define START_PREFETCH(TYPE) {

#define START_PREFETCH_W(TYPE) {

#define INIT_PREFETCH() {

#define PREFETCH_OP(X)

#endif /* USE_PREFETCH */

#else /* _NATIVE */

#ifdef USE_PREFETCH

#define START_PREFETCH(TYPE) ALWAYS_START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE) ALWAYS_START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#else

#define START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#endif /* USE_PREFETCH */

#endif /* _NATIVE */

#else /* USE_THREADED_CODE */

#ifndef _NATIVE

#define ALWAYS_START_PREFETCH(TYPE) {

#define ALWAYS_START_PREFETCH_W(TYPE) {

#define ALWAYS_LOOKAHEAD(WHERE) {

#define START_PREFETCH(TYPE) {

#define START_PREFETCH_W(TYPE) {

#define INIT_PREFETCH() {

#define PREFETCH_OP(X)

#else

#define ALWAYS_START_PREFETCH(TYPE)

#define ALWAYS_START_PREFETCH_W(TYPE)

#define ALWAYS_LOOKAHEAD(WHERE)

#define START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE)

#define INIT_PREFETCH()

#define PREFETCH_OP(X)

#endif /* _NATIVE */

#endif /* USE_THREADED_CODE */

#ifndef _NATIVE

#define ALWAYS_END_PREFETCH() }

#define ALWAYS_END_PREFETCH_W() }

#define END_PREFETCH() }

#define END_PREFETCH_W() }

#else

#define ALWAYS_END_PREFETCH()

#define ALWAYS_END_PREFETCH_W()

#define END_PREFETCH()

#define END_PREFETCH_W()

#endif /* _NATIVE */

/*****************************************************************

  How to jump to the next abstract machine instruction

******************************************************************/

#if __YAP_TRACED

#define JMP(Lab)                                                               \
  {                                                                            \
    opcode = Yap_op_from_opcode(goto * Lab);                                   \
    goto op_switch;                                                            \
  }

#define JMPNext(Lab)                                                           \
  {                                                                            \
    opcode = Yap_op_from_opcode(PREG->opc) +                                   \
             ExpEnv.config_struc.current_displacement;                         \
    goto op_switch;                                                            \
  }

#define JMPNextW(Lab)                                                          \
  {                                                                            \
    opcode = Yap_op_from_opcode(PREG->opcw) +                                  \
             ExpEnv.config_struc.current_displacement;                         \
    goto op_switch;                                                            \
  }

#elif USE_THREADED_CODE

#ifndef _NATIVE

#define JMP(Lab) goto *Lab;

#if YAP_JIT

#define JMPNext()                                                              \
  if (ExpEnv.config_struc.current_displacement)                                \
    JMP((void *)OpAddress[Yap_op_from_opcode(PREG->opc) +                      \
                          ExpEnv.config_struc.current_displacement]);          \
  JMP((void *)(PREG->opc));

#define JMPNextW()                                                             \
  if (ExpEnv.config_struc.current_displacement)                                \
    JMP((void *)OpAddress[Yap_op_from_opcode(PREG->y_u.o.opcw) +               \
                          ExpEnv.config_struc.current_displacement]);          \
  JMP((void *)(PREG->y_u.o.opcw));

#else /* YAP_JIT */

#define JMPNext() JMP((void *)(PREG->opc));

#define JMPNextW() JMP((void *)(PREG->y_u.o.opcw));

#endif /* YAP_JIT */

#else /* _NATIVE */

#if YAP_STAT_PREDS

#if YAP_DBG_PREDS

#define SUCCESSBACK()                                                          \
  {                                                                            \
    if (Yap_op_from_opcode((*_PREG)->opc) == _jit_handler) {                   \
      if ((*_PREG)->y_u.jhc.jh->caa.naddress != -1 &&                          \
          NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress] &&            \
          NativeArea->area.ok[(*_PREG)->y_u.jhc.jh->caa.naddress]) {           \
        void *(*callee)(yamop **, yamop **, CELL **, void *[], void *[]);      \
        void *go;                                                              \
        callee = (void *(*)(yamop **, yamop **, CELL **, void *[], void *[]))  \
                     NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress];   \
        go = (*callee)(&(*_PREG), &(*_CPREG), &(*_SREG), external_labels,      \
                       OpAddress);                                             \
        return go;                                                             \
      }                                                                        \
    }                                                                          \
    if ((char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success != 0 && \
        (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success !=      \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success); \
    }                                                                          \
    HEADPREG->y_u.jhc.jh->jitman.torecomp = ExpEnv.config_struc.torecompile;   \
    NativeArea->success[HEADPREG->y_u.jhc.jh->caa.naddress] += 1;              \
    (ExpEnv.config_struc.torecompile)                                          \
        ? (ExpEnv.config_struc.current_displacement =                          \
               ExpEnv.config_struc.TOTAL_OF_OPCODES)                           \
        : (ExpEnv.config_struc.current_displacement = 0);                      \
    if (ExpEnv.config_struc.current_displacement)                              \
      return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc) +            \
                                ExpEnv.config_struc.current_displacement]);    \
    return ((void *)((*_PREG)->opc));                                          \
  }

#define BACK()                                                                 \
  {                                                                            \
    if ((char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail != 0 &&    \
        (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail !=         \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail);    \
    }                                                                          \
    return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc)]);             \
  }

#else /* YAP_DBG_PREDS */

#define SUCCESSBACK()                                                          \
  {                                                                            \
    if (Yap_op_from_opcode((*_PREG)->opc) == _jit_handler) {                   \
      if ((*_PREG)->y_u.jhc.jh->caa.naddress != -1 &&                          \
          NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress] &&            \
          NativeArea->area.ok[(*_PREG)->y_u.jhc.jh->caa.naddress]) {           \
        void *(*callee)(yamop **, yamop **, CELL **, void *[], void *[]);      \
        void *go;                                                              \
        callee = (void *(*)(yamop **, yamop **, CELL **, void *[], void *[]))  \
                     NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress];   \
        go = (*callee)(&(*_PREG), &(*_CPREG), &(*_SREG), external_labels,      \
                       OpAddress);                                             \
        return go;                                                             \
      }                                                                        \
    }                                                                          \
    HEADPREG->y_u.jhc.jh->jitman.torecomp = ExpEnv.config_struc.torecompile;   \
    NativeArea->success[HEADPREG->y_u.jhc.jh->caa.naddress] += 1;              \
    (ExpEnv.config_struc.torecompile)                                          \
        ? (ExpEnv.config_struc.current_displacement =                          \
               ExpEnv.config_struc.TOTAL_OF_OPCODES)                           \
        : (ExpEnv.config_struc.current_displacement = 0);                      \
    if (ExpEnv.config_struc.current_displacement)                              \
      return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc) +            \
                                ExpEnv.config_struc.current_displacement]);    \
    return ((void *)((*_PREG)->opc));                                          \
  }

#define BACK()                                                                 \
  { return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc)]); }

#endif /* YAP_DBG_PREDS */

#else /* YAP_STAT_PREDS */

#if YAP_DBG_PREDS

#define SUCCESSBACK()                                                          \
  {                                                                            \
    if (Yap_op_from_opcode((*_PREG)->opc) == _jit_handler) {                   \
      if ((*_PREG)->y_u.jhc.jh->caa.naddress != -1 &&                          \
          NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress] &&            \
          NativeArea->area.ok[(*_PREG)->y_u.jhc.jh->caa.naddress]) {           \
        void *(*callee)(yamop **, yamop **, CELL **, void *[], void *[]);      \
        void *go;                                                              \
        callee = (void *(*)(yamop **, yamop **, CELL **, void *[], void *[]))  \
                     NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress];   \
        go = (*callee)(&(*_PREG), &(*_CPREG), &(*_SREG), external_labels,      \
                       OpAddress);                                             \
        return go;                                                             \
      }                                                                        \
    }                                                                          \
    if ((char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success != 0 && \
        (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success !=      \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_success); \
    }                                                                          \
    HEADPREG->y_u.jhc.jh->jitman.torecomp = ExpEnv.config_struc.torecompile;   \
    (ExpEnv.config_struc.torecompile)                                          \
        ? (ExpEnv.config_struc.current_displacement =                          \
               ExpEnv.config_struc.TOTAL_OF_OPCODES)                           \
        : (ExpEnv.config_struc.current_displacement = 0);                      \
    if (ExpEnv.config_struc.current_displacement)                              \
      return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc) +            \
                                ExpEnv.config_struc.current_displacement]);    \
    return ((void *)((*_PREG)->opc));                                          \
  }

#define BACK()                                                                 \
  {                                                                            \
    if ((char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail != 0 &&    \
        (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail !=         \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.nativerun_exit_by_fail);    \
    }                                                                          \
    return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc)]);             \
  }

#else /* YAP_DBG_PREDS */

#define SUCCESSBACK()                                                          \
  {                                                                            \
    if (Yap_op_from_opcode((*_PREG)->opc) == _jit_handler) {                   \
      if ((*_PREG)->y_u.jhc.jh->caa.naddress != -1 &&                          \
          NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress] &&            \
          NativeArea->area.ok[(*_PREG)->y_u.jhc.jh->caa.naddress]) {           \
        void *(*callee)(yamop **, yamop **, CELL **, void *[], void *[]);      \
        void *go;                                                              \
        callee = (void *(*)(yamop **, yamop **, CELL **, void *[], void *[]))  \
                     NativeArea->area.p[(*_PREG)->y_u.jhc.jh->caa.naddress];   \
        go = (*callee)(&(*_PREG), &(*_CPREG), &(*_SREG), external_labels,      \
                       OpAddress);                                             \
        return go;                                                             \
      }                                                                        \
    }                                                                          \
    HEADPREG->y_u.jhc.jh->jitman.torecomp = ExpEnv.config_struc.torecompile;   \
    (ExpEnv.config_struc.torecompile)                                          \
        ? (ExpEnv.config_struc.current_displacement =                          \
               ExpEnv.config_struc.TOTAL_OF_OPCODES)                           \
        : (ExpEnv.config_struc.current_displacement = 0);                      \
    if (ExpEnv.config_struc.current_displacement)                              \
      return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc) +            \
                                ExpEnv.config_struc.current_displacement]);    \
    return ((void *)((*_PREG)->opc));                                          \
  }

#define BACK()                                                                 \
  { return ((void *)OpAddress[Yap_op_from_opcode((*_PREG)->opc)]); }

#endif /* YAP_DBG_PREDS */

#endif /* YAP_STAT_PREDS */

#define JMP(Lab)

#define JMPNext()

#define JMPNextW()

#endif /* _NATIVE */

#if USE_THREADED_CODE && (LIMITED_PREFETCH || USE_PREFETCH)

#define ALWAYS_GONext() JMP(to_go)

#define ALWAYS_GONextW() JMP(to_go)

#else

#define ALWAYS_GONext() JMPNext()

#define ALWAYS_GONextW() JMPNextW()

#endif

#ifdef USE_PREFETCH

#define GONext() ALWAYS_GONext()

#define GONextW() ALWAYS_GONextW()

#else

#define GONext() JMPNext()

#define GONextW() JMPNextW()

#endif /* USE_PREFETCH */

#if YAP_DBG_PREDS

#if YAP_JIT

#define Op(Label, Type)                                                        \
  _##Label : {                                                                 \
    (ExpEnv.config_struc.current_displacement)                                 \
        ? print_instruction(PREG, ON_PROFILED_INTERPRETER)                     \
        : print_instruction(PREG, ON_INTERPRETER);                             \
    START_PREFETCH(Type)

#define OpW(Label, Type)                                                       \
  _##Label : {                                                                 \
    (ExpEnv.config_struc.current_displacement)                                 \
        ? print_instruction(PREG, ON_PROFILED_INTERPRETER)                     \
        : print_instruction(PREG, ON_INTERPRETER);                             \
    START_PREFETCH_W(Type)

#define BOp(Label, Type)                                                       \
  _##Label : {                                                                 \
    (ExpEnv.config_struc.current_displacement)                                 \
        ? print_instruction(PREG, ON_PROFILED_INTERPRETER)                     \
        : print_instruction(PREG, ON_INTERPRETER);

#define PBOp(Label, Type)                                                      \
  _##Label : {                                                                 \
    (ExpEnv.config_struc.current_displacement)                                 \
        ? print_instruction(PREG, ON_PROFILED_INTERPRETER)                     \
        : print_instruction(PREG, ON_INTERPRETER);                             \
    INIT_PREFETCH()

#define OpRW(Label, Type)                                                      \
  _##Label : {                                                                 \
    (ExpEnv.config_struc.current_displacement)                                 \
        ? print_instruction(PREG, ON_PROFILED_INTERPRETER)                     \
        : print_instruction(PREG, ON_INTERPRETER);

#else /* YAP_JIT */

#define Op(Label, Type)                                                        \
  _##Label : {                                                                 \
    print_instruction(PREG, ON_INTERPRETER);                                   \
    START_PREFETCH(Type)

#define OpW(Label, Type)                                                       \
  _##Label : {                                                                 \
    print_instruction(PREG, ON_INTERPRETER);                                   \
    START_PREFETCH_W(Type)

#define BOp(Label, Type)                                                       \
  _##Label : {                                                                 \
    print_instruction(PREG, ON_INTERPRETER);

#define PBOp(Label, Type)                                                      \
  _##Label : {                                                                 \
    print_instruction(PREG, ON_INTERPRETER);                                   \
    INIT_PREFETCH()

#define OpRW(Label, Type)                                                      \
  _##Label : {                                                                 \
    print_instruction(PREG, ON_INTERPRETER);

#endif /* YAP_JIT */

#else /* YAP_DBG_PREDS */

#define Op(Label, Type)                                                        \
  _##Label : {                                                                 \
    START_PREFETCH(Type)

#define OpW(Label, Type)						\
  _##Label : {                                                                 \
    START_PREFETCH_W(Type)

#define BOp(Label, Type) _##Label : {

#define PBOp(Label, Type)                                                      \
  _##Label : {                                                                 \
    INIT_PREFETCH()

#define OpRW(Label, Type) _##Label : {

#endif /* YAP_DBG_PREDS */

#else /* do not use threaded code */

#define JMPNext() goto nextop

#define JMPNextW() goto nextop_write

#define GONext() JMPNext()

#define GONextW() JMPNextW()

#define ALWAYS_GONext() GONext()

#define ALWAYS_GONextW() GONextW()

#define Op(Label, Type)                                                        \
  case _##Label: {                                                             \
    START_PREFETCH(Type)

#define OpW(Label, Type)                                                       \
  case _##Label: {                                                             \
    START_PREFETCH_W(Type)

#define BOp(Label, Type) case _##Label: {

#define PBOp(Label, Type)                                                      \
  case _##Label: {                                                             \
    INIT_PREFETCH()

#define OpRW(Label, Type) case _##Label: {

#endif

#define ENDOp()                                                                \
  END_PREFETCH()                                                               \
  }

#define ENDOpW()                                                               \
  END_PREFETCH_W()                                                             \
  }

#define ENDOpRW() }

#define ENDBOp() }

#define ENDPBOp()                                                              \
  END_PREFETCH()                                                               \
  }

/**********************************************************************
 *                                                                    *
 *                           PC manipulation                          *
 *                                                                    *
 **********************************************************************/

/*
 * How to set up and move a PC in a nice and disciplined way
 *
 */

typedef CELL label;

/* move PC */

#define ADJ(P, x) (P) + ADJUST(sizeof(x))

/*
 * Lookup PredEntry Structure
 *
 */

#define pred_entry(X)                                                          \
  ((PredEntry *)(Unsigned(X) - (CELL)(&(((PredEntry *)NULL)->StateOfPred))))
#define pred_entry_from_code(X)                                                \
  ((PredEntry *)(Unsigned(X) - (CELL)(&(((PredEntry *)NULL)->CodeOfPred))))
#define PredFromDefCode(X)                                                     \
  ((PredEntry *)(Unsigned(X) - (CELL)(&(((PredEntry *)NULL)->OpcodeOfPred))))
#define PredFromExpandCode(X)                                                  \
  ((PredEntry *)(Unsigned(X) -                                                 \
                 (CELL)(&(((PredEntry *)NULL)->cs.p_code.ExpandCode))))
#define PredCode(X) pred_entry(X)->CodeOfPred
#define PredOpCode(X) pred_entry(X)->OpcodeOfPred
#define TruePredCode(X) pred_entry(X)->TrueCodeOfPred
#define PredFunctor(X) pred_entry(X)->FunctorOfPred
#define PredArity(X) pred_entry(X)->ArityOfPE

#define FlagOff(Mask, w) !(Mask & w)
#define FlagOn(Mask, w) (Mask & w)
#define ResetFlag(Mask, w) w &= ~Mask
#define SetFlag(Mask, w) w |= Mask

/**********************************************************************
 *                                                                    *
 *                         X register access                          *
 *                                                                    *
 **********************************************************************/

#if PRECOMPUTE_REGADDRESS

#define XREG(I) (*(CELL *)(I))

#else

#define XREG(I) XREGS[I]

#endif /* PRECOMPUTE_REGADDRESS */

/* The Unification Stack is the Auxiliary stack */

#define SP0 ((CELL *)AuxTop)
#define SP AuxSp

/**********************************************************************
 *                                                                    *
 *                         RWREG Manipulatio                          *
 *                                                                    *
 **********************************************************************/

#define READ_MODE 1
#define WRITE_MODE 0

/**********************************************************************
 *                                                                    *
 *Setting Temporary Copies of Often Used WAM registers for efficiency *
 *                                                                    *
 **********************************************************************/

#ifdef SHADOW_P
#define NEEDS_TO_SET_PC 1
#endif

/*
 * First, the PC
 */
#ifdef NEEDS_TO_SET_PC
#ifndef _NATIVE
#define set_pc() PREG = P
#define save_pc() P = PREG
#else
#define set_pc() (*_PREG) = P
#define save_pc() P = (*_PREG)
#endif
#else
#define set_pc()
#define save_pc()
#define PREG (P)
#endif

/*
 * Next, Y
 */
#ifdef SHADOW_Y
#define set_y() YREG = YENV
#define save_y() YENV = YREG
#else
#define set_y()
#define save_y()
#define YREG YENV
#endif

/*
 * Next, CP
 */
#ifdef SHADOW_CP
#ifndef _NATIVE
#define set_cp() CPREG = CP
#define save_cp() CP = CPREG
#else
#define set_cp() (*_CPREG) = CP
#define save_cp() CP = (*_CPREG)
#endif
#else
#define set_cp()
#define save_cp()
#define CPREG CP
#endif

/* Say which registers must be saved at register entry and register
 * exit */
#define setregs()                                                              \
  set_hb();                                                                    \
  set_cp();                                                                    \
  set_pc();                                                                    \
  set_y()

#define saveregs()                                                             \
  save_hb();                                                                   \
  save_cp();                                                                   \
  save_pc();                                                                   \
  save_y()

#if BP_FREE
/* if we are using BP as a local register, we must save it whenever we leave
 * absmi.c */
#define always_save_pc() save_pc()
#define always_set_pc() set_pc()
#else
#define always_save_pc()
#define always_set_pc()
#endif /* BP_FREE */

/************************************************************

Macros to check the limits of stacks

*************************************************************/

#if HAVE_SIGSEGV
/* for the moment I don't know how to handle trail overflows
   in a pure Windows environment
*/
#if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(THREADS) &&        \
    !defined(YAPOR) && !defined(USE_SYSTEM_MALLOC) && !USE_DL_MALLOC
#define OS_HANDLES_TR_OVERFLOW 1
#endif
#endif

#ifdef OS_HANDLES_TR_OVERFLOW

#define check_trail(x)

#define check_trail_in_indexing(x)

#else

#ifdef _NATIVE

#if YAP_DBG_PREDS

#define check_trail(x)                                                         \
  if (__builtin_expect((Unsigned(CurrentTrailTop) < Unsigned(x)), 0)) {        \
    if ((char *)ExpEnv.debug_struc.pprint_me.native_treat_trail != 0 &&        \
        (char *)ExpEnv.debug_struc.pprint_me.native_treat_trail !=             \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.native_treat_trail);        \
    }                                                                          \
    return external_labels[9];                                                 \
  }

#else /* YAP_DBG_PREDS */

#define check_trail(x)                                                         \
  if (__builtin_expect((Unsigned(CurrentTrailTop) < Unsigned(x)), 0)) {        \
    return external_labels[9];                                                 \
  }

#endif /* YAP_DBG_PREDS */

#define check_trail_in_indexing(x)                                             \
  if (__builtin_expect((Unsigned(CurrentTrailTop) < Unsigned(x)), 0))          \
  goto notrailleft_from_index

#else

#if YAP_DBG_PREDS

#define check_trail(x)                                                         \
  if (__builtin_expect((Unsigned(CurrentTrailTop) < Unsigned(x)), 0)) {        \
    if ((char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_trail != 0 &&   \
        (char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_trail !=        \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_trail);   \
    }                                                                          \
    goto notrailleft;                                                          \
  }

#else /* YAP_DBG_PREDS */

#define check_trail(x)                                                         \
  if (__builtin_expect((Unsigned(CurrentTrailTop) < Unsigned(x)), 0)) {        \
    goto notrailleft;                                                          \
  }

#endif /* YAP_DBG_PREDS */

#define check_trail_in_indexing(x)                                             \
  if (__builtin_expect((Unsigned(CurrentTrailTop) < Unsigned(x)), 0))          \
  goto notrailleft_from_index

#endif /* _NATIVE */

#endif /* OS_HANDLES_TR_OVERFLOW */

#if YAP_DBG_PREDS
#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack(Label, GLOB)                                               \
  if (__builtin_expect(                                                        \
          ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG, B_FZ)) -            \
                 Unsigned(YOUNGEST_H(H_FZ, GLOB))) < CreepFlag),               \
          0)) {                                                                \
    if ((char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap != 0 &&    \
        (char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap !=         \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap);    \
    }                                                                          \
    goto Label;                                                                \
  }
#else
#define check_stack(Label, GLOB)                                               \
  if (__builtin_expect(                                                        \
          ((Int)(Unsigned(ENV_YREG) - Unsigned(GLOB)) < CreepFlag), 0)) {      \
    if ((char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap != 0 &&    \
        (char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap !=         \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.interpreted_treat_heap);    \
    }                                                                          \
    goto Label;                                                                \
  }
#endif /* YAPOR_SBA && YAPOR */
#else  /* YAP_DBG_PREDS */
#if (defined(YAPOR_SBA) && defined(YAPOR)) || defined(TABLING)
#define check_stack(Label, GLOB)                                               \
  if (__builtin_expect(                                                        \
          ((Int)(Unsigned(YOUNGEST_CP((choiceptr)ENV_YREG, B_FZ)) -            \
                 Unsigned(YOUNGEST_H(H_FZ, GLOB))) < CreepFlag),               \
          0)) {                                                                \
    goto Label;                                                                \
  }
#else
#define check_stack(Label, GLOB)                                               \
  if (__builtin_expect(                                                        \
          ((Int)(Unsigned(ENV_YREG) - Unsigned(GLOB)) < CreepFlag), 0)) {      \
    goto Label;                                                                \
  }
#endif /* YAPOR_SBA && YAPOR */
#endif /* YAP_DBG_PREDS */

/***************************************************************
 * Macros for choice point manipulation                         *
 ***************************************************************/

/***************************************************************
 * Store variable number of arguments in a choice point         *
 ***************************************************************/
/***
    pt1 points at the new choice point,
    pt0 points at XREGS[i]
    d0 is a counter
    The macro just pushes the arguments one by one to the local stack.
***/
#define store_args(arity)                                                      \
  BEGP(pt0);                                                                   \
  pt0 = XREGS + (arity);                                                       \
  while (pt0 > XREGS) {                                                        \
    register CELL x = pt0[0];                                                  \
    S_YREG = S_YREG - 1;                                                       \
    --pt0;                                                                     \
    (S_YREG)[0] = x;                                                           \
  }                                                                            \
  ENDP(pt0)

#define store_at_least_one_arg(arity)                                          \
  BEGP(pt0);                                                                   \
  pt0 = XREGS + (arity);                                                       \
  do {                                                                         \
    CELL x = pt0[0];                                                           \
    S_YREG = (S_YREG)-1;                                                       \
    --pt0;                                                                     \
    (S_YREG)[0] = x;                                                           \
  } while (pt0 > XREGS);                                                       \
  ENDP(pt0)

#if LOW_LEVEL_TRACER && 0
#define COUNT_CPS() LOCAL_total_choicepoints++
#else
#define COUNT_CPS()
#endif

/***************************************************************
 * Do the bulk of work in creating a choice-point               *
 * AP: alternative pointer                                      *
 ***************************************************************/
/*
 * The macro just sets pt1 to point to the base of the choicepoint
 * and then fills in all the necessary fields
 */
#ifdef DEPTH_LIMIT
#define store_yaam_reg_cpdepth(CPTR) (CPTR)->cp_depth = DEPTH
#else
#define store_yaam_reg_cpdepth(CPTR)
#endif

#ifndef _NATIVE
#define store_yaam_regs(AP, I)                                                 \
  { /* Jump to CP_BASE */                                                      \
    COUNT_CPS();                                                               \
    S_YREG = (CELL *)((choiceptr)((S_YREG) - (I)) - 1);                        \
    /* Save Information */                                                     \
    HBREG = HR;                                                                \
    B_YREG->cp_tr = TR;                                                        \
    B_YREG->cp_h = HR;                                                         \
    B_YREG->cp_b = B;                                                          \
    store_yaam_reg_cpdepth(B_YREG);                                            \
    B_YREG->cp_cp = CPREG;                                                     \
    B_YREG->cp_ap = AP;                                                        \
    B_YREG->cp_env = ENV;                                                      \
  }
#else /* _NATIVE */
#define store_yaam_regs(AP, I)                                                 \
  { /* Jump to CP_BASE */                                                      \
    COUNT_CPS();                                                               \
    S_YREG = (CELL *)((choiceptr)((S_YREG) - (I)) - 1);                        \
    /* Save Information */                                                     \
    HBREG = HR;                                                                \
    B_YREG->cp_tr = TR;                                                        \
    B_YREG->cp_h = HR;                                                         \
    B_YREG->cp_b = B;                                                          \
    store_yaam_reg_cpdepth(B_YREG);                                            \
    B_YREG->cp_cp = (*_CPREG);                                                 \
    B_YREG->cp_ap = AP;                                                        \
    B_YREG->cp_env = ENV;                                                      \
  }
#endif

#define store_yaam_regs_for_either(AP, d0)                                     \
  COUNT_CPS();                                                                 \
  pt1--; /* Jump to CP_BASE */                                                 \
  /* Save Information */                                                       \
  HBREG = HR;                                                                  \
  pt1->cp_tr = TR;                                                             \
  pt1->cp_h = HR;                                                              \
  pt1->cp_b = B;                                                               \
  store_yaam_reg_cpdepth(pt1);                                                 \
  pt1->cp_cp = d0;                                                             \
  pt1->cp_ap = AP;                                                             \
  pt1->cp_env = ENV;

/***************************************************************
 * Place B as the new place to cut to                           *
 ***************************************************************/
#define set_cut(E, B) (E)[E_CB] = (CELL)(B)

/***************************************************************
 * Restore WAM registers from a choice point                    *
 ***************************************************************/

#ifdef DEPTH_LIMIT
#define restore_yaam_reg_cpdepth(CPTR) DEPTH = (CPTR)->cp_depth
#else
#define restore_yaam_reg_cpdepth(CPTR)
#endif

#ifdef YAPOR
#define YAPOR_update_alternative(CUR_ALT, NEW_ALT)                             \
  if (SCH_top_shared_cp(B)) {                                                  \
    SCH_new_alternative(CUR_ALT, NEW_ALT);                                     \
  } else
#else
#define YAPOR_update_alternative(CUR_ALT, NEW_ALT)
#endif /* YAPOR */

#if defined(FROZEN_STACKS) && !defined(BFZ_TRAIL_SCHEME)
#define SET_BB(V) BBREG = (V)
#else
#define SET_BB(V)
#endif /* FROZEN_STACKS && !BFZ_TRAIL_SCHEME */

#ifdef FROZEN_STACKS
#ifdef YAPOR_SBA
#define PROTECT_FROZEN_H(CPTR)                                                 \
  ((Unsigned((Int)((CPTR)->cp_h) - (Int)(H_FZ)) <                              \
    Unsigned((Int)(B_FZ) - (Int)(H_FZ)))                                       \
       ? (CPTR)->cp_h                                                          \
       : H_FZ)
#define PROTECT_FROZEN_B(CPTR)                                                 \
  ((Unsigned((Int)(CPTR) - (Int)(H_FZ)) < Unsigned((Int)(B_FZ) - (Int)(H_FZ))) \
       ? (CPTR)                                                                \
       : B_FZ)
/*
  #define PROTECT_FROZEN_H(CPTR) ((CPTR)->cp_h > H_FZ && (CPTR)->cp_h < (CELL
  *)B_FZ ? (CPTR)->cp_h : H_FZ )

  #define PROTECT_FROZEN_B(CPTR)  ((CPTR) < B_FZ && (CPTR) > (choiceptr)H_FZ ?
  (CPTR) : B_FZ )
*/
#else /* TABLING */
#define PROTECT_FROZEN_B(CPTR) (YOUNGER_CP(CPTR, B_FZ) ? CPTR : B_FZ)
#define PROTECT_FROZEN_H(CPTR) (((CPTR)->cp_h > H_FZ) ? (CPTR)->cp_h : H_FZ)
#endif /* YAPOR_SBA */
#else
#define PROTECT_FROZEN_B(CPTR) (CPTR)
#define PROTECT_FROZEN_H(CPTR) (CPTR)->cp_h
#endif /* FROZEN_STACKS */

#ifndef _NATIVE
#define restore_yaam_regs(AP)                                                  \
  {                                                                            \
    register CELL *x1 = B_YREG->cp_env;                                        \
    register yamop *x2;                                                        \
    HR = HBREG = PROTECT_FROZEN_H(B_YREG);                                     \
    restore_yaam_reg_cpdepth(B_YREG);                                          \
    CPREG = B_YREG->cp_cp;                                                     \
    /* AP may depend on H */                                                   \
    x2 = (yamop *)AP;                                                          \
    ENV = x1;                                                                  \
    YAPOR_update_alternative(PREG, x2) B_YREG->cp_ap = x2;                     \
  }
#else
#define restore_yaam_regs(AP)                                                  \
  {                                                                            \
    register CELL *x1 = B_YREG->cp_env;                                        \
    register yamop *x2;                                                        \
    H = HBREG = PROTECT_FROZEN_H(B_YREG);                                      \
    restore_yaam_reg_cpdepth(B_YREG);                                          \
    (*_CPREG) = B_YREG->cp_cp;                                                 \
    /* AP may depend on H */                                                   \
    x2 = (yamop *)AP;                                                          \
    ENV = x1;                                                                  \
    YAPOR_update_alternative((*_PREG), x2) B_YREG->cp_ap = x2;                 \
  }
#endif

/***************************************************************
 * Restore variable number of arguments from a choice point     *
 ***************************************************************/
#define restore_args(Nargs)                                                    \
  BEGD(d0);                                                                    \
  d0 = Nargs;                                                                  \
  BEGP(pt0);                                                                   \
  BEGP(pt1);                                                                   \
  pt1 = (CELL *)(B_YREG + 1) + d0;                                             \
  pt0 = XREGS + 1 + d0;                                                        \
  while (pt0 > XREGS + 1) {                                                    \
    register CELL x = pt1[-1];                                                 \
    --pt0;                                                                     \
    --pt1;                                                                     \
    *pt0 = x;                                                                  \
  }                                                                            \
  ENDP(pt1);                                                                   \
  ENDP(pt0);                                                                   \
  ENDD(d0)

#define restore_at_least_one_arg(Nargs)                                        \
  BEGD(d0);                                                                    \
  d0 = Nargs;                                                                  \
  BEGP(pt0);                                                                   \
  BEGP(pt1);                                                                   \
  pt1 = (CELL *)(B_YREG + 1) + d0;                                             \
  pt0 = XREGS + 1 + d0;                                                        \
  do {                                                                         \
    register CELL x = pt1[-1];                                                 \
    --pt0;                                                                     \
    --pt1;                                                                     \
    *pt0 = x;                                                                  \
  } while (pt0 > XREGS + 1);                                                   \
  ENDP(pt1);                                                                   \
  ENDP(pt0);                                                                   \
  ENDD(d0)

/***************************************************************
 * Execute trust to release YAAM registers and pop choice point *
 ***************************************************************/
#ifdef DEPTH_LIMIT
#define pop_yaam_reg_cpdepth(CPTR) DEPTH = (CPTR)->cp_depth
#else
#define pop_yaam_reg_cpdepth(CPTR)
#endif

#ifdef TABLING
#define TABLING_close_alt(CPTR) (CPTR)->cp_ap = NULL
#else
#define TABLING_close_alt(CPTR)
#endif /* TABLING */

#ifndef _NATIVE
#define pop_yaam_regs()                                                        \
  {                                                                            \
    HR = PROTECT_FROZEN_H(B_YREG);                                             \
    B = B_YREG->cp_b;                                                          \
    pop_yaam_reg_cpdepth(B_YREG);                                              \
    CPREG = B_YREG->cp_cp;                                                     \
    ENV = B_YREG->cp_env;                                                      \
    TABLING_close_alt(B_YREG);                                                 \
    HBREG = PROTECT_FROZEN_H(B);                                               \
  }
#else
#define pop_yaam_regs()                                                        \
  {                                                                            \
    HR = PROTECT_FROZEN_H(B_YREG);                                             \
    B = B_YREG->cp_b;                                                          \
    pop_yaam_reg_cpdepth(B_YREG);                                              \
    (*_CPREG) = B_YREG->cp_cp;                                                 \
    ENV = B_YREG->cp_env;                                                      \
    TABLING_close_alt(B_YREG);                                                 \
    HBREG = PROTECT_FROZEN_H(B);                                               \
  }
#endif

#define pop_args(NArgs)                                                        \
  BEGD(d0);                                                                    \
  d0 = (NArgs);                                                                \
  BEGP(pt0);                                                                   \
  BEGP(pt1);                                                                   \
  S_YREG = (CELL *)(B_YREG + 1);                                               \
  pt0 = XREGS + 1;                                                             \
  pt1 = S_YREG;                                                                \
  while (pt0 < XREGS + 1 + d0) {                                               \
    register CELL x = pt1[0];                                                  \
    pt1++;                                                                     \
    pt0++;                                                                     \
    pt0[-1] = x;                                                               \
  }                                                                            \
  S_YREG = pt1;                                                                \
  ENDP(pt1);                                                                   \
  ENDP(pt0);                                                                   \
  ENDD(d0);

#define pop_at_least_one_arg(NArgs)                                            \
  BEGD(d0);                                                                    \
  d0 = (NArgs);                                                                \
  BEGP(pt0);                                                                   \
  BEGP(pt1);                                                                   \
  pt1 = (CELL *)(B_YREG + 1);                                                  \
  pt0 = XREGS + 1;                                                             \
  do {                                                                         \
    register CELL x = pt1[0];                                                  \
    pt1++;                                                                     \
    pt0++;                                                                     \
    pt0[-1] = x;                                                               \
  } while (pt0 < XREGS + 1 + d0);                                              \
  S_YREG = pt1;                                                                \
  ENDP(pt1);                                                                   \
  ENDP(pt0);                                                                   \
  ENDD(d0);

/**********************************************************************
 *                                                                    *
 *                    failure and backtracking                        *
 *                                                                    *
 **********************************************************************/

/* Failure can be called from two routines.
 *
 * If from within the emulator, we should jump to the label fail.
 *
 * If from within the complex-term unification routine, we should jump
 * to the label "cufail".
 *
 */

#ifndef _NATIVE

#if YAP_DBG_PREDS

#define FAIL()                                                                 \
  {                                                                            \
    if ((char *)ExpEnv.debug_struc.pprint_me.interpreted_backtrack != 0 &&     \
        (char *)ExpEnv.debug_struc.pprint_me.interpreted_backtrack !=          \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s",                                                    \
              (char *)ExpEnv.debug_struc.pprint_me.interpreted_backtrack);     \
    }                                                                          \
    goto fail;                                                                 \
  }

#define TRACED_FAIL()                                                          \
  {                                                                            \
    if ((char *)ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack != \
            0 &&                                                               \
        (char *)ExpEnv.debug_struc.pprint_me.profiled_interpreted_backtrack != \
            (char *)0x1) {                                                     \
      fprintf(stderr, "%s:%d\n", __FILE__, __LINE__);                          \
      fprintf(stderr, "%s", (char *)ExpEnv.debug_struc.pprint_me               \
                                .profiled_interpreted_backtrack);              \
    }                                                                          \
    goto traced_fail;                                                          \
  }

#else /* YAP_DBG_PREDS */

#define FAIL()                                                                 \
  { goto fail; }

#define TRACED_FAIL()                                                          \
  { goto traced_fail; }

#endif /* YAP_DBG_PREDS */

#else

#define FAIL()                                                                 \
  { return external_labels[0]; }

#endif

/**********************************************************************
 *                                                                    *
 *                      unification routines                          *
 *                                                                    *
 **********************************************************************/

#define UnifyGlobalCells(a, b)                                                 \
  if ((b) > (a)) {                                                             \
    if (GlobalIsAttVar(b) && !GlobalIsAttVar(a)) {                             \
      Bind_Global((a), (CELL)(b));                                             \
    } else {                                                                   \
      Bind_Global((b), (CELL)(a));                                             \
    }                                                                          \
  } else if ((b) < (a)) {                                                      \
    if (GlobalIsAttVar(a) && !GlobalIsAttVar(b)) {                             \
      Bind_Global((b), (CELL)(a));                                             \
    } else {                                                                   \
      Bind_Global((a), (CELL)(b));                                             \
    }                                                                          \
  }

#define UnifyGlobalCellToCell(b, a)                                            \
  if ((a) < HR) { /* two globals */                                            \
    UnifyGlobalCells(a, b);                                                    \
  } else {                                                                     \
    Bind_Local((a), (CELL)(b));                                                \
  }

#define UnifyCells(a, b)                                                       \
  if ((a) < HR) { /* at least one global */                                    \
    if ((b) > HR) {                                                            \
      Bind_Local((b), (CELL)(a));                                              \
    } else {                                                                   \
      UnifyGlobalCells(a, b);                                                  \
    }                                                                          \
  } else {                                                                     \
    if ((b) > (a)) {                                                           \
      Bind_Local((a), (CELL)(b));                                              \
    } else if ((a) > (b)) {                                                    \
      if ((b) < HR) {                                                          \
        Bind_Local((a), (CELL)(b));                                            \
      } else {                                                                 \
        Bind_Local((b), (CELL)(a));                                            \
      }                                                                        \
    }                                                                          \
  }

/* unify two complex terms.
 *
 * I use two stacks: one keeps the visited terms, and the other keeps the
 * terms to visit.
 *
 * The terms-to-visit stack is used to implement traditional
 * recursion. The visited-terms-stack is used to link structures already
 * visited and allows unification of infinite terms
 *
 */

#ifdef RATIONAL_TREES

#define UNWIND_CUNIF()                                                         \
  while (visited < AuxSp) {                                                    \
    pt1 = (CELL *)visited[0];                                                  \
    *pt1 = visited[1];                                                         \
    visited += 2;                                                              \
  }

#else
#define UNWIND_CUNIF()
#endif

#define UnifyBound_TEST_ATTACHED(f, d0, pt0, d1)                               \
  if (IsExtensionFunctor(f)) {                                                 \
    if (unify_extension(f, d0, RepAppl(d0), d1)) {                             \
      GONext();                                                                \
    } else {                                                                   \
      FAIL();                                                                  \
    }                                                                          \
  }

#define UnifyBound(d0, d1)                                                     \
  if (d0 == d1) {                                                              \
    GONext();                                                                  \
  }                                                                            \
  if (IsPairTerm(d0)) {                                                        \
    register CELL *ipt0, *ipt1;                                                \
    if (!IsPairTerm(d1)) {                                                     \
      FAIL();                                                                  \
    }                                                                          \
    ipt0 = RepPair(d0);                                                        \
    ipt1 = RepPair(d1);                                                        \
    save_hb();                                                                 \
    always_save_pc();                                                          \
    if (IUnify_complex(ipt0 - 1, ipt0 + 1, ipt1 - 1)) {                        \
      always_set_pc();                                                         \
      GONext();                                                                \
    } else {                                                                   \
      FAIL();                                                                  \
    }                                                                          \
  } else if (IsApplTerm(d0)) {                                                 \
    register CELL *ipt0, *ipt1;                                                \
    register Functor f;                                                        \
    if (!IsApplTerm(d1)) {                                                     \
      FAIL();                                                                  \
    }                                                                          \
    ipt0 = RepAppl(d0);                                                        \
    ipt1 = RepAppl(d1);                                                        \
    f = (Functor)*ipt0;                                                        \
    if (f != (Functor)*ipt1) {                                                 \
      FAIL();                                                                  \
    }                                                                          \
    UnifyBound_TEST_ATTACHED(f, d0, ipt0, d1);                                 \
    d0 = ArityOfFunctor(f);                                                    \
    always_save_pc();                                                          \
    save_hb();                                                                 \
    if (IUnify_complex(ipt0, ipt0 + d0, ipt1)) {                               \
      always_set_pc();                                                         \
      GONext();                                                                \
    } else {                                                                   \
      FAIL();                                                                  \
    }                                                                          \
  } else {                                                                     \
    FAIL();                                                                    \
  }

#define traced_UnifyBound_TEST_ATTACHED(f, d0, pt0, d1)                        \
  if (IsExtensionFunctor(f)) {                                                 \
    if (unify_extension(f, d0, RepAppl(d0), d1)) {                             \
      GONext();                                                                \
    } else {                                                                   \
      TRACED_FAIL();                                                           \
    }                                                                          \
  }

#define traced_UnifyBound(d0, d1)                                              \
  if (d0 == d1) {                                                              \
    GONext();                                                                  \
  }                                                                            \
  if (IsPairTerm(d0)) {                                                        \
    register CELL *ipt0, *ipt1;                                                \
    if (!IsPairTerm(d1)) {                                                     \
      TRACED_FAIL();                                                           \
    }                                                                          \
    ipt0 = RepPair(d0);                                                        \
    ipt1 = RepPair(d1);                                                        \
    save_hb();                                                                 \
    always_save_pc();                                                          \
    if (IUnify_complex(ipt0 - 1, ipt0 + 1, ipt1 - 1)) {                        \
      always_set_pc();                                                         \
      GONext();                                                                \
    } else {                                                                   \
      TRACED_FAIL();                                                           \
    }                                                                          \
  } else if (IsApplTerm(d0)) {                                                 \
    register CELL *ipt0, *ipt1;                                                \
    register Functor f;                                                        \
    if (!IsApplTerm(d1)) {                                                     \
      TRACED_FAIL();                                                           \
    }                                                                          \
    ipt0 = RepAppl(d0);                                                        \
    ipt1 = RepAppl(d1);                                                        \
    f = (Functor)*ipt0;                                                        \
    if (f != (Functor)*ipt1) {                                                 \
      TRACED_FAIL();                                                           \
    }                                                                          \
    traced_UnifyBound_TEST_ATTACHED(f, d0, ipt0, d1);                          \
    d0 = ArityOfFunctor(f);                                                    \
    always_save_pc();                                                          \
    save_hb();                                                                 \
    if (IUnify_complex(ipt0, ipt0 + d0, ipt1)) {                               \
      always_set_pc();                                                         \
      GONext();                                                                \
    } else {                                                                   \
      TRACED_FAIL();                                                           \
    }                                                                          \
  } else {                                                                     \
    TRACED_FAIL();                                                             \
  }

/*
 * Next, HB
 */
#ifdef SHADOW_HB
#undef HBREG
#define set_hb() HBREG = HB
#define save_hb() HB = HBREG
#else
#define set_hb()
#define save_hb()
#endif

typedef struct unif_record {
  CELL *ptr;
  Term old;
} unif_record;

typedef struct v_record {
  CELL *start0;
  CELL *end0;
  CELL *start1;
  Term old;
} v_record;

#if defined(IN_ABSMI_C) || defined(IN_UNIFY_C) || defined(IN_TRACED_ABSMI_C)

static int IUnify_complex(CELL *pt0, CELL *pt0_end, CELL *pt1) {
  CACHE_REGS
#ifdef THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
  register REGSTORE *regp = &Yap_REGS;

#define Yap_REGS (*regp)
#endif /* defined(B) || defined(TR) || defined(HB) */
#endif

#ifdef SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

  struct unif_record *unif = (struct unif_record *)AuxBase;
  struct v_record *to_visit = (struct v_record *)AuxSp;
#define unif_base ((struct unif_record *)AuxBase)
#define to_visit_base ((struct v_record *)AuxSp)

loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0 = pt0 + 1;
    register CELL d0;

    ++pt1;
    pt0 = ptd0;
    d0 = *ptd0;
    deref_head(d0, unify_comp_unk);
  unify_comp_nvar : {
    register CELL *ptd1 = pt1;
    register CELL d1 = *ptd1;

    deref_head(d1, unify_comp_nvar_unk);
  unify_comp_nvar_nvar:
    if (d0 == d1)
      continue;
    if (IsPairTerm(d0)) {
      if (!IsPairTerm(d1)) {
        goto cufail;
      }
      /* now link the two structures so that no one else will */
      /* come here */
      /* store the terms to visit */
      if (RATIONAL_TREES || pt0 < pt0_end) {
        to_visit--;
#ifdef RATIONAL_TREES
        unif++;
#endif
        if ((void *)to_visit < (void *)unif) {
          CELL **urec = (CELL **)unif;
          to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit,
                                                        &urec, NULL);
          unif = (struct unif_record *)urec;
        }
        to_visit->start0 = pt0;
        to_visit->end0 = pt0_end;
        to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
        unif[-1].old = *pt0;
        unif[-1].ptr = pt0;
        *pt0 = d1;
#endif
      }
      pt0_end = (pt0 = RepPair(d0) - 1) + 2;
      pt1 = RepPair(d1) - 1;
      continue;
    }
    if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2, *ap3;

      if (!IsApplTerm(d1)) {
        goto cufail;
      }
      /* store the terms to visit */
      ap2 = RepAppl(d0);
      ap3 = RepAppl(d1);
      f = (Functor)(*ap2);
      /* compare functors */
      if (f != (Functor)*ap3)
        goto cufail;
      if (IsExtensionFunctor(f)) {
        if (unify_extension(f, d0, ap2, d1))
          continue;
        goto cufail;
      }
      /* now link the two structures so that no one else will */
      /* come here */
      /* store the terms to visit */
      if (RATIONAL_TREES || pt0 < pt0_end) {
        to_visit--;
#ifdef RATIONAL_TREES
        unif++;
#endif
        if ((void *)to_visit < (void *)unif) {
          CELL **urec = (CELL **)unif;
          to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit,
                                                        &urec, NULL);
          unif = (struct unif_record *)urec;
        }
        to_visit->start0 = pt0;
        to_visit->end0 = pt0_end;
        to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
        unif[-1].old = *pt0;
        unif[-1].ptr = pt0;
        *pt0 = d1;
#endif
      }
      d0 = ArityOfFunctor(f);
      pt0 = ap2;
      pt0_end = ap2 + d0;
      pt1 = ap3;
      continue;
    }
    goto cufail;

    derefa_body(d1, ptd1, unify_comp_nvar_unk, unify_comp_nvar_nvar);
    /* d1 and pt2 have the unbound value, whereas d0 is bound */
    Bind_Global(ptd1, d0);
    continue;
  }

    derefa_body(d0, ptd0, unify_comp_unk, unify_comp_nvar);
    /* first arg var */
    {
      register CELL d1;
      register CELL *ptd1;

      ptd1 = pt1;
      d1 = ptd1[0];
      /* pt2 is unbound */
      deref_head(d1, unify_comp_var_unk);
    unify_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      Bind_Global(ptd0, d1);
      continue;

      derefa_body(d1, ptd1, unify_comp_var_unk, unify_comp_var_nvar);
      /* ptd0 and ptd1 are unbound */
      UnifyGlobalCells(ptd0, ptd1);
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit < to_visit_base) {
    pt0 = to_visit->start0;
    pt0_end = to_visit->end0;
    pt1 = to_visit->start1;
    to_visit++;
    goto loop;
  }
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return TRUE;

cufail:
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return FALSE;
#ifdef THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
#undef Yap_REGS
#endif /* defined(B) || defined(TR) */
#endif
}

/*  don't pollute name space */
#undef to_visit_base
#undef unif_base

#endif

#if /* defined(IN_ABSMI_C) ||*/ defined(                                       \
    IN_INLINES_C) /*|| defined(IN_TRACED_ABSMI_C) */

static int iequ_complex(register CELL *pt0, register CELL *pt0_end,
                        register CELL *pt1) {
  CACHE_REGS
#ifdef THREADS
#undef Yap_REGS
  register REGSTORE *regp = Yap_regp;
#define Yap_REGS (*regp)
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
  register REGSTORE *regp = &Yap_REGS;

#define Yap_REGS (*regp)
#endif /* defined(B) || defined(TR) || defined(HB) */
#endif

#ifdef SHADOW_HB
  register CELL *HBREG = HB;
#endif /* SHADOW_HB */

  struct unif_record *unif = (struct unif_record *)AuxBase;
  struct v_record *to_visit = (struct v_record *)AuxSp;
#define unif_base ((struct unif_record *)AuxBase)
#define to_visit_base ((struct v_record *)AuxSp)

loop:
  while (pt0 < pt0_end) {
    register CELL *ptd0 = pt0 + 1;
    register CELL d0;

    ++pt1;
    pt0 = ptd0;
    d0 = *ptd0;
    deref_head(d0, iequ_comp_unk);
  iequ_comp_nvar : {
    register CELL *ptd1 = pt1;
    register CELL d1 = *ptd1;

    deref_head(d1, iequ_comp_nvar_unk);
  iequ_comp_nvar_nvar:
    if (d0 == d1)
      continue;
    if (IsPairTerm(d0)) {
      if (!IsPairTerm(d1)) {
        goto cufail;
      }
      /* now link the two structures so that no one else will */
      /* come here */
      /* store the terms to visit */
      if (RATIONAL_TREES || pt0 < pt0_end) {
        to_visit--;
#ifdef RATIONAL_TREES
        unif++;
#endif
        if ((void *)to_visit < (void *)unif) {
          CELL **urec = (CELL **)unif;
          to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit,
                                                        &urec, NULL);
          unif = (struct unif_record *)urec;
        }
        to_visit->start0 = pt0;
        to_visit->end0 = pt0_end;
        to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
        unif[-1].old = *pt0;
        unif[-1].ptr = pt0;
        *pt0 = d1;
#endif
      }
      pt0_end = (pt0 = RepPair(d0) - 1) + 2;
      pt1 = RepPair(d1) - 1;
      continue;
    }
    if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2, *ap3;

      if (!IsApplTerm(d1)) {
        goto cufail;
      }
      /* store the terms to visit */
      ap2 = RepAppl(d0);
      ap3 = RepAppl(d1);
      f = (Functor)(*ap2);
      /* compare functors */
      if (f != (Functor)*ap3)
        goto cufail;
      if (IsExtensionFunctor(f)) {
        if (unify_extension(f, d0, ap2, d1))
          continue;
        goto cufail;
      }
      /* now link the two structures so that no one else will */
      /* come here */
      /* store the terms to visit */
      if (RATIONAL_TREES || pt0 < pt0_end) {
        to_visit--;
#ifdef RATIONAL_TREES
        unif++;
#endif
        if ((void *)to_visit < (void *)unif) {
          CELL **urec = (CELL **)unif;
          to_visit = (struct v_record *)Yap_shift_visit((CELL **)to_visit,
                                                        &urec, NULL);
          unif = (struct unif_record *)urec;
        }
        to_visit->start0 = pt0;
        to_visit->end0 = pt0_end;
        to_visit->start1 = pt1;
#ifdef RATIONAL_TREES
        unif[-1].old = *pt0;
        unif[-1].ptr = pt0;
        *pt0 = d1;
#endif
      }
      d0 = ArityOfFunctor(f);
      pt0 = ap2;
      pt0_end = ap2 + d0;
      pt1 = ap3;
      continue;
    }
    goto cufail;

    derefa_body(d1, ptd1, iequ_comp_nvar_unk, iequ_comp_nvar_nvar);
    /* d1 and pt2 have the unbound value, whereas d0 is bound */
    goto cufail;
  }

    derefa_body(d0, ptd0, iequ_comp_unk, iequ_comp_nvar);
    /* first arg var */
    {
      register CELL d1;
      register CELL *ptd1;

      ptd1 = pt1;
      d1 = ptd1[0];
      /* pt2 is unbound */
      deref_head(d1, iequ_comp_var_unk);
    iequ_comp_var_nvar:
      /* pt2 is unbound and d1 is bound */
      goto cufail;

      derefa_body(d1, ptd1, iequ_comp_var_unk, iequ_comp_var_nvar);
      /* pt2 and pt3 are unbound */
      if (ptd0 == ptd1)
        continue;
      goto cufail;
    }
  }
  /* Do we still have compound terms to visit */
  if (to_visit < to_visit_base) {
    pt0 = to_visit->start0;
    pt0_end = to_visit->end0;
    pt1 = to_visit->start1;
    to_visit++;
    goto loop;
  }
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return TRUE;

cufail:
#ifdef RATIONAL_TREES
  /* restore bindigs */
  while (unif-- != unif_base) {
    CELL *pt0;

    pt0 = unif->ptr;
    *pt0 = unif->old;
  }
#endif
  return FALSE;
#ifdef THREADS
#undef Yap_REGS
#define Yap_REGS (*Yap_regp)
#elif defined(SHADOW_REGS)
#if defined(B) || defined(TR)
#undef Yap_REGS
#endif /* defined(B) || defined(TR) */
#endif
}

#endif

static inline wamreg Yap_regnotoreg(UInt regnbr) {
#if PRECOMPUTE_REGADDRESS
  return (wamreg)(XREGS + regnbr);
#else
#if MSHIFTOFFS
  return regnbr;
#else
  return CELLSIZE * regnbr;
#endif
#endif /* ALIGN_LONGS */
}

static inline UInt Yap_regtoregno(wamreg reg) {
#if PRECOMPUTE_REGADDRESS
  return ((CELL *)reg) - XREGS;
#else
#if MSHIFTOFFS
  return reg;
#else
  return reg / CELLSIZE;
#endif
#endif /* ALIGN_LONGS */
}

#ifdef DEPTH_LIMIT
#define check_depth(DEPTH, ap)                                                 \
  if ((DEPTH) <= MkIntTerm(1)) { /* I assume Module==0 is prolog */            \
    if ((ap)->ModuleOfPred) {                                                  \
      if ((DEPTH) == MkIntTerm(0)) {                                           \
        FAIL();                                                                \
      } else                                                                   \
        (DEPTH) = RESET_DEPTH();                                               \
    }                                                                          \
  } else if ((ap)->ModuleOfPred)                                               \
    (DEPTH) -= MkIntConstant(2);
#else
#define check_depth(DEPTH, ap)
#endif

#if defined(THREADS) || defined(YAPOR)
#define copy_jmp_address(X) (PREG_ADDR = &(X))
#define copy_jmp_addressa(X) (PREG_ADDR = (yamop **)(X))
#else
#define copy_jmp_address(X)
#define copy_jmp_addressa(X)
#endif

static inline void prune(choiceptr cp USES_REGS) {
#ifdef YAPOR
  CUT_prune_to(cp);
#endif /* YAPOR */
  if (SHOULD_CUT_UP_TO(B, cp)) {
    if (ASP > (CELL *)PROTECT_FROZEN_B(B))
      ASP = (CELL *)PROTECT_FROZEN_B(B);
    while (B->cp_b < cp) {
      if (POP_CHOICE_POINT(B->cp_b)) {
        POP_EXECUTE();
      }
      if (B->cp_b == NULL)
        return;
      B = B->cp_b;
    }
    if (POP_CHOICE_POINT(B->cp_b)) {
      POP_EXECUTE();
    }
/* cut ! */
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
    HB = PROTECT_FROZEN_H(B->cp_b);
#include "trim_trail.h"
    B = B->cp_b;
    SET_BB(PROTECT_FROZEN_B(B));
  }
}

#if YAPOR
#define INITIALIZE_PERMVAR(PTR, V) Bind_Local((PTR), (V))
#else
#define INITIALIZE_PERMVAR(PTR, V) *(PTR) = (V)
#endif

/* l1: bind a, l2 bind b, l3 no binding */
#define UnifyAndTrailCells(a, b)                                               \
  if ((a) > (b)) {                                                             \
    if ((a) < HR) {                                                            \
      *(a) = (CELL)(b);                                                        \
      DO_TRAIL((a), (CELL)(b));                                                \
    } else if ((b) <= HR) {                                                    \
      *(a) = (CELL)(b);                                                        \
      DO_TRAIL((a), (CELL)(b));                                                \
    } else {                                                                   \
      *(b) = (CELL)(a);                                                        \
      DO_TRAIL((b), (CELL)(a));                                                \
    }                                                                          \
  } else if ((a) < (b)) {                                                      \
    if ((b) <= HR) {                                                           \
      *(b) = (CELL)(a);                                                        \
      DO_TRAIL((b), (CELL)(a));                                                \
    } else if ((a) <= HR) {                                                    \
      *(b) = (CELL)(a);                                                        \
      DO_TRAIL((b), (CELL)(a));                                                \
    } else {                                                                   \
      *(a) = (CELL)(b);                                                        \
      DO_TRAIL((a), (CELL)(b));                                                \
    }                                                                          \
  }

#define CHECK_ALARM(CONT)

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#if HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if YAP_JIT

extern Environment ExpEnv;
extern char fin[1024];

#ifndef _NATIVE

#include <math.h>

#ifndef __cplusplus
#include "JIT_Compiler.hpp"

void *(*Yap_JitCall)(JIT_Compiler *jc, yamop *p);
void (*Yap_llvmShutdown)(void);
Int (*Yap_traced_absmi)(void);
extern JIT_Compiler *J;
#endif


extern NativeContext *NativeArea;
extern IntermediatecodeContext *IntermediatecodeArea;

extern CELL l;
extern short global;
extern CELL nnexec;
extern yamop *HEADPREG;
extern CELL BLOCK;
extern CELL BLOCKADDRESS;
extern CELL FAILED;

extern TraceContext **curtrace;
extern yamop *curpreg;
extern BlocksContext **globalcurblock;
extern COUNT ineedredefinedest;
extern yamop *headoftrace;

#endif /* _NATIVE */
#endif /* YAP_JIT */

#ifdef SHADOW_S
#define PROCESS_INT(F, C)                                                      \
  BEGD(d0);                                                                    \
  Yap_REGS.S_ = SREG;                                                          \
   Yap_REGS.ASP_ = f;                                                          \
  saveregs();                                                                  \
  d0 = F(PASS_REGS1);                                                          \
  setregs();                                                                   \
  SREG = Yap_REGS.S_;                                                          \
  if (!d0)                                                                     \
    FAIL();                                                                    \
  PP = NULL;                                                                   \
  if (d0 == 2)                                                                 \
    goto C;                                                                    \
  JMPNext();                                                                   \
  ENDD(d0);
#else
#define PROCESS_INT(F, C)                                                      \
  BEGD(d0);                                                                    \
  saveregs();                                                                  \
  d0 = F(PASS_REGS1);                                                          \
  setregs();                                                                   \
  PP = NULL;                                                                   \
  if (!d0)                                                                     \
    FAIL();                                                                    \
  if (d0 == 2)                                                                 \
    goto C;                                                                    \
  JMPNext();\
 ENDD(d0);
#endif

/// after interrupt dispatch
#define INT_HANDLER_GO_ON      -1   ///> should continue testing
#define INT_HANDLER_FAIL 0      ///> should execute FAIL
#define INT_HANDLER_RET_NEXT  1  ///> should dispatch, ie JMPNext()
#define INT_HANDLER_RET_JMP       2   ///> should goto C label
#define HAS_INT(D)       (D>=0) 

#ifdef SHADOW_S
#define PROCESS_INTERRUPT(F, C, SZ)					\
  BEGD(d0);                                                                    \
  Yap_REGS.S_ = SREG;                                                          \
  SET_ASP(YENV, SZ);							\
  saveregs();                                                                  \
  d0 = F(PASS_REGS1);                                                          \
  setregs();                                                                   \
  SREG = Yap_REGS.S_;                                                          \
  if (d0 ==INT_HANDLER_FAIL )                                                                     \
    FAIL();                                                                    \
  PP = NULL;                                                                   \
  if (d0 == INT_HANDLER_RET_JMP)                                                                 \
    goto C;                                                                    \
  JMPNext();                                                                   \
  ENDD(d0);
#else
#define PROCESS_INTERRUPT(F, C, SZ)\
BEGD(d0);								\
  saveregs();                                                                  \
  SET_ASP(YENV, SZ);							\
  d0 = F(PASS_REGS1);                                                          \
  setregs();                                                                   \
  PP = NULL;                                                                   \
  if (d0 == INT_HANDLER_FAIL)                                                                     \
    FAIL();                                                                    \
  if (d0 == INT_HANDLER_RET_JMP)                                                                 \
    goto C;                                                                    \
  JMPNext();\
 ENDD(d0);
#endif

#define Yap_AsmError(e, d)                                                     \
  {                                                                            \
    saveregs();                                                                \
    Yap_ThrowError(e, d, "while executing inlined built-in");                  \
    setregs();                                                                 \
  }

#endif // ABSMI_H
