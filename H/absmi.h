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
* comments:	Portable abstract machine interpreter includes           *
*									 *
*************************************************************************/

#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif /* SCCS */

/***************************************************************
* Macros for register manipulation                             *
***************************************************************/
/*
 * Machine and compiler dependent definitions
 */
#ifdef __GNUC__

#if defined(sparc) || defined(__sparc)
#define SHADOW_P       1
#define SHADOW_Y       1
#define SHADOW_S       1
#define SHADOW_REGS    1
#define SHADOW_CP      1
#define SHADOW_HB      1
/*#define SHADOW_CrFl    1 Breaks alarm/3 */
#define USE_PREFETCH   1
#endif

#ifdef hppa
#define SHADOW_P       1
#define SHADOW_Y       1
#define SHADOW_REGS    1
#define USE_PREFETCH   1
#endif

#ifdef __alpha
#define SHADOW_P       1
#define SHADOW_Y       1
#define SHADOW_REGS    1
#define USE_PREFETCH   1
#endif

#ifdef mips
#define SHADOW_P       1
#define SHADOW_Y       1
#define SHADOW_REGS    1
#define USE_PREFETCH   1
#endif

#ifdef _POWER
#define SHADOW_P       1
#define SHADOW_REGS    1
#define USE_PREFETCH   1
#endif

#ifdef i386
#define      Y_IN_MEM  1
#define      S_IN_MEM  1
#define     TR_IN_MEM  1
#define HAVE_FEW_REGS  1
#define LIMITED_PREFETCH   1
#ifdef BP_FREE
/***************************************************************
* Use bp as PREG for X86 machines		               *
***************************************************************/
#if IN_ABSMI_C
register void* P1REG asm ("bp"); /* can't use yamop before Yap.h */
#define PREG ((yamop *)P1REG)
#endif
#define NEEDS_TO_SET_PC 1
#endif /* BP_FREE */
#endif /* i386 */

#else /* other compilers */

#define S_IN_MEM       1

/* This works for xlc under AIX 3.2.5 */
#ifdef _IBMR2
#define SHADOW_P       1
#define SHADOW_REGS    1
#define SHADOW_S       1
#endif

#ifdef i386
#define Y_IN_MEM       1
#define S_IN_MEM       1
#define TR_IN_MEM      1
#define HAVE_FEW_REGS  1
#endif

#ifdef mips
#define SHADOW_P       1
#define SHADOW_Y       1
#define SHADOW_S       1
#define SHADOW_CP      1
#define SHADOW_HB      1
#define USE_PREFETCH   1
#endif

#ifdef _HPUX_SOURCE
#define SHADOW_P       1
#define SHADOW_Y       1
#define SHADOW_S       1
#define SHADOW_CP      1
#define SHADOW_HB      1
#define USE_PREFETCH   1
#endif

#endif /* __GNUC__ */

#include "Yap.h"
#include "clause.h"
#include "eval.h"
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
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

int  STD_PROTO(IUnify_complex, (CELL *, CELL *,CELL *));
int  STD_PROTO(iequ_complex, (CELL *, CELL *,CELL *));

#ifdef ANALYST

static char *op_names[_std_top + 1] =
{
#define OPCODE(OP,TYPE) #OP
#include "YapOpcodes.h"
#undef  OPCODE
};

#endif


#if PUSH_REGS

/***************************************************************
* Trick to copy REGS into absmi local environment              *
***************************************************************/

/* regp is a global variable */

inline EXTERN void
init_absmi_regs(REGSTORE * absmi_regs)
{
  memcpy(absmi_regs, regp, sizeof(REGSTORE));
}

inline EXTERN void
restore_absmi_regs(REGSTORE * old_regs)
{
  memcpy(old_regs, regp, sizeof(REGSTORE));
  regp = old_regs;
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

#define BEGP(TMP) { register CELL *TMP

#define ENDP(TMP) }

#define BEGD(TMP) { register CELL TMP

#define ENDD(TMP) }

#endif /* LONG_LIVED_REGISTERS */

#define BEGCHO(TMP) { register choiceptr TMP

#define ENDCHO(TMP) }

/***************************************************************
* Y is usually, but not always, a register. This affects       *
* choicepoints                                                 *
***************************************************************/

#if Y_IN_MEM

#define CACHE_Y(A) { register CELL *S_Y = ((CELL *)(A))

#define ENDCACHE_Y() Y = S_Y; }

#define B_Y   ((choiceptr)(S_Y))

#else

#define S_Y   (Y)

#define B_Y ((choiceptr)(Y))

#define CACHE_Y(A) { Y = ((CELL *)(A))

#define ENDCACHE_Y() }

#endif

#if Y_IN_MEM

#define CACHE_Y_AS_ENV(A) { register CELL *E_Y = (A)

#define WRITEBACK_Y_AS_ENV()   Y = E_Y

#define ENDCACHE_Y_AS_ENV() }

#else

#define E_Y (Y)

#define WRITEBACK_Y_AS_ENV()   

#define CACHE_Y_AS_ENV(A) { Y = (A)

#define ENDCACHE_Y_AS_ENV() }

#endif

#if S_IN_MEM

#define CACHE_A1()

#define CACHED_A1()	ARG1

#else

#define CACHE_A1()	(SREG = (CELL *)ARG1)

#define CACHED_A1()	((CELL)SREG)

#endif /* S_IN_MEM */

/***************************************************************
* TR is usually, but not always, a register. This affects      *
* backtracking                                                 *
***************************************************************/

#if TR_IN_MEM

#define CACHE_TR(A) { register tr_fr_ptr S_TR = (A)

#define RESTORE_TR()    TR = S_TR

#define ENDCACHE_TR() }

#else

#define S_TR  TR

#define CACHE_TR(A) { TR = (A)

#define RESTORE_TR()

#define ENDCACHE_TR() }

#endif

/***************************************************************
* S is usually, but not always, a register (X86 machines).     *
* This affects unification instructions                        *
***************************************************************/

#if S_IN_MEM

/* jump through hoops because idiotic gcc will go and read S from
   memory every time it uses S :-( */

#define CACHE_S() { register CELL * S_SREG;

#define ENDCACHE_S() }

#define READ_IN_S() S_SREG = SREG

#else

/* do nothing if you are on a decent machine */

#define CACHE_S() {

#define ENDCACHE_S() }

#define READ_IN_S()

#define S_SREG  SREG

#endif

#define WRITEBACK_S(X) SREG = (X)

/*****************************************************************

   End of Machine Dependent stuff 

******************************************************************/

/*****************************************************************

   Prefetch is a technique to obtain the place to jump to before actually
   executing instructions. It can speed up some machines, by having the
   next opcode in place before it is actually required for jumping.

******************************************************************/

#if USE_THREADED_CODE

#define DO_PREFETCH(TYPE) to_go = (void *)(NEXTOP(PREG,TYPE)->opc)

#define DO_PREFETCH_W(TYPE) to_go = (void *)(NEXTOP(PREG,TYPE)->u.o.opcw)

#if LIMITED_PREFETCH

#define ALWAYS_START_PREFETCH(TYPE) \
 { register void *to_go; DO_PREFETCH(TYPE)

#define ALWAYS_LOOKAHEAD(WHAT) \
 { register void *to_go = (void *)(WHAT)

#define ALWAYS_START_PREFETCH_W(TYPE) \
 { register void *to_go; DO_PREFETCH_W(TYPE)

#else

#define ALWAYS_START_PREFETCH(TYPE) {

#define ALWAYS_START_PREFETCH_W(TYPE) {

#define ALWAYS_LOOKAHEAD(WHERE) {

#endif

#if USE_PREFETCH

#define START_PREFETCH(TYPE) ALWAYS_START_PREFETCH(TYPE)

#define START_PREFETCH_W(TYPE) ALWAYS_START_PREFETCH_W(TYPE)

#define INIT_PREFETCH() \
     { register void *to_go;

#define PREFETCH_OP(X) \
     to_go = (void *)((X)->opc);

#else

#define START_PREFETCH(TYPE) {

#define START_PREFETCH_W(TYPE) {

#define INIT_PREFETCH() {

#define PREFETCH_OP(X)

#endif	/* USE_PREFETCH */

#else /* USE_THREADED_CODE */

#define ALWAYS_START_PREFETCH(TYPE) {

#define ALWAYS_START_PREFETCH_W(TYPE) {

#define ALWAYS_LOOKAHEAD(WHERE) {

#define START_PREFETCH(TYPE) {

#define START_PREFETCH_W(TYPE) {

#define INIT_PREFETCH() {

#define PREFETCH_OP(X)

#endif /* USE_THREADED_CODE */

#define ALWAYS_END_PREFETCH() }

#define ALWAYS_END_PREFETCH_W() }

#define END_PREFETCH() }

#define END_PREFETCH_W() }

/*****************************************************************

  How to jump to the next abstract machine instruction

******************************************************************/

#if USE_THREADED_CODE

#define JMP(Lab)  goto *Lab 

#define JMPNext()						\
	JMP((void *)(PREG->opc))

#define JMPNextW()						\
	JMP((void *)(PREG->u.o.opcw))

#if USE_THREADED_CODE && LIMITED_PREFETCH

#define ALWAYS_GONext() JMP(to_go)

#define ALWAYS_GONextW() JMP(to_go)

#else

#define ALWAYS_GONext() JMPNext()

#define ALWAYS_GONextW() JMPNextW()

#endif

#if USE_PREFETCH

#define GONext() ALWAYS_GONext() 

#define GONextW() ALWAYS_GONextW() 

#else

#define GONext() JMPNext()

#define GONextW() JMPNextW()

#endif /* USE_PREFETCH */

#define Op(Label,Type)	 Label: START_PREFETCH(Type)

#define OpW(Label,Type)	 Label: START_PREFETCH_W(Type)

#define BOp(Label,Type)	 Label:

#define PBOp(Label,Type) Label: INIT_PREFETCH()

#define OpRW(Label,Type) Label:

#else /* do not use threaded code */

#define JMPNext()	goto nextop

#define JMPNextW()	goto nextop_write

#define GONext()	JMPNext()

#define GONextW()	JMPNextW()

#define ALWAYS_GONext() GONext()

#define ALWAYS_GONextW() GONextW()

#define Op(Label,Type)	 case _##Label: START_PREFETCH(Type)

#define OpW(Label,Type)	 case _##Label: START_PREFETCH_W(Type)

#define BOp(Label,Type)	 case _##Label:

#define PBOp(Label,Type) case _##Label: INIT_PREFETCH()

#define OpRW(Label,Type) case _##Label:

#endif

#define ENDOp() END_PREFETCH()

#define ENDOpW() END_PREFETCH_W()

#define ENDOpRW()

#define ENDBOp()

#define ENDPBOp() END_PREFETCH()

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

#define ADJ(P,x)    (P)+ADJUST(sizeof(x))

/*
 * Lookup PredEntry Structure
 * 
 */

#define pred_entry(X)		((PredEntry *)(Unsigned(X)-(CELL)(&(((PredEntry *)NULL)->StateOfPred))))
#define pred_entry_from_code(X)		((PredEntry *)(Unsigned(X)-(CELL)(&(((PredEntry *)NULL)->CodeOfPred))))
#define PredFromDefCode(X)	((PredEntry *)(Unsigned(X)-(CELL)(&(((PredEntry *)NULL)->OpcodeOfPred))))
#define Flags(X)		pred_entry(X)->StateOfPred
#define PredCode(X)		pred_entry(X)->CodeOfPred
#define PredOpCode(X)		pred_entry(X)->OpcodeOfPred
#define TruePredCode(X)		pred_entry(X)->TrueCodeOfPred
#define PredFunctor(X)		pred_entry(X)->FunctorOfPred
#define PredArity(X)		pred_entry(X)->ArityOfPE
#define Module(X)		pred_entry(X)->ModuleOfPred

#define FlagOff(Mask,w) !(Mask&w)
#define FlagOn(Mask,w) (Mask&w)
#define ResetFlag(Mask,w) w &= ~Mask
#define SetFlag(Mask,w) w |= Mask

/**********************************************************************
 *                                                                    *
 *                         X register access                          *
 *                                                                    *
 **********************************************************************/

#if PRECOMPUTE_REGADDRESS

#define XREG(I)		(*(CELL *)(I))

#else

#define XREG(I)		XREGS[I]

#endif /* ALIGN_LONGS */

	/* The Unification Stack is the Auxiliary stack */

#define SP0 ((CELL *)AuxTop)
#define SP  AuxSp

/**********************************************************************
 *                                                                    *
 *                         RWREG Manipulatio                          *
 *                                                                    *
 **********************************************************************/

#define READ_MODE     1
#define WRITE_MODE    0

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
#define set_pc()	PREG = P
#define save_pc()	P = PREG
#else
#define set_pc()
#define save_pc()
#define PREG            (P)
#endif

/* 
 * Next, Y
 */
#if SHADOW_Y
#define set_y()		Y = YENV
#define save_y()	YENV = Y
#else
#define set_y()
#define save_y()
#define Y               YENV
#endif

/* 
 * Next, CP
 */
#if SHADOW_CP
#define set_cp()	CPREG = CP
#define save_cp()	CP = CPREG
#else
#define set_cp()
#define save_cp()
#define CPREG           CP
#endif

/* 
 * Next, CP
 */
	/* Breaks alarm/3 */
#if SHADOW_CrFl
#define set_cf()	CFREG = CreepFlag
#define save_cf()	CreepFlag = CFREG
#else
#define set_cf()
#define save_cf()
#define CFREG           CreepFlag
#endif

/* Say which registers must be saved at register entry and register
 * exit */
#define setregs()                     \
	set_cf();                     \
	set_hb();                     \
	set_cp();                     \
	set_pc();                     \
        set_y()

#define saveregs()                     \
	save_cf();                     \
	save_hb();                     \
	save_cp();                     \
	save_pc();                     \
        save_y()

#if BF_FREE
/* if we are using BP as a local register, we must save it whenever we leave absmi.c */
#define always_save_pc()          save_pc()
#define always_set_pc()           set_pc()
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
#if !_MSC_VER && !defined(__MINGW32__)
#define OS_HANDLES_TR_OVERFLOW 1
#endif
#endif

#if OS_HANDLES_TR_OVERFLOW

#define check_trail()

#else

#define check_trail() if (Unsigned(TrailTop) - Unsigned(TR) < MinTrailGap) \
			goto notrailleft

#endif

#if defined(SBA) && defined(YAPOR)
#define check_stack(Label, GLOB)                             \
 if ( (Int)(Unsigned(E_Y) - CFREG) < (Int)(GLOB) &&          \
       (choiceptr)E_Y < B_FZ && E_Y > H_FZ       &&          \
       (GLOB) > H_FZ && (GLOB) < (CELL *)B_FZ) goto Label
#else
#define check_stack(Label, GLOB)                             \
 if ( (Int)(Unsigned(E_Y) - CFREG) < (Int)(GLOB) ) goto Label
#endif /* SBA && YAPOR */

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
#define store_args(arity)                                         \
                 BEGP(pt0);                                       \
		 pt0 = XREGS+(arity);                             \
		 while ( pt0 > XREGS )                            \
                   { register CELL x = pt0[0];                    \
                     S_Y = S_Y-1;			          \
                     --pt0;                                       \
                     (S_Y)[0] = x;	                          \
		   }                                              \
                 ENDP(pt0)

#define store_at_least_one_arg(arity)                             \
                 BEGP(pt0);                                       \
		 pt0 = XREGS+(arity);                             \
                 do { register CELL x = pt0[0];                   \
                     S_Y = (S_Y)-1;			          \
                     --pt0;                                       \
                     (S_Y)[0] = x;	                          \
		   }                                              \
		 while ( pt0 > XREGS );                           \
                 ENDP(pt0)

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

#define store_yaam_regs(AP,I) \
                 { register yamop *x1 = (yamop *)(AP);           \
                   register CELL *x2 = ENV;			 \
                   /* Jump to CP_BASE */                         \
                   S_Y = (CELL *)((choiceptr)((S_Y)-(I))-1);     \
                   /* Save Information */                        \
		   HBREG = H;                                    \
                   B_Y->cp_tr = TR;				 \
                   B_Y->cp_h  = H;				 \
                   B_Y->cp_b  = B;				 \
                   store_yaam_reg_cpdepth(B_Y);                  \
                   B_Y->cp_cp = CPREG;				 \
                   B_Y->cp_ap = x1;				 \
                   B_Y->cp_env= x2;				 \
                 }

#define store_yaam_regs_for_either(AP,d0) \
                 pt1 --; /* Jump to CP_BASE */		         \
                 /* Save Information */                          \
		 HBREG = H;                                      \
                 pt1->cp_tr = TR;	                         \
                 pt1->cp_h = H;		                         \
		 pt1->cp_b = B;		                         \
                 store_yaam_reg_cpdepth(pt1);                    \
                 pt1->cp_cp = d0;                                \
                 pt1->cp_ap = (yamop *)AP;                       \
		 pt1->cp_env = ENV;

/***************************************************************
* Place B as the new place to cut to                           *
***************************************************************/
#define	  set_cut(E,B) (E)[E_CB] = (CELL)(B)

/***************************************************************
* Restore WAM registers from a choice point                    *
***************************************************************/

#ifdef DEPTH_LIMIT
#define restore_yaam_reg_cpdepth(CPTR) DEPTH = (CPTR)->cp_depth
#else
#define restore_yaam_reg_cpdepth(CPTR)
#endif

#ifdef YAPOR
#define YAPOR_update_alternative(CUR_ALT, NEW_ALT)  \
	  if (SCH_top_shared_cp(B)) {               \
	    SCH_new_alternative(CUR_ALT, NEW_ALT);  \
	  } else
#else
#define YAPOR_update_alternative(CUR_ALT, NEW_ALT)
#endif /* YAPOR */

#if defined(FROZEN_STACKS) && !defined(BFZ_TRAIL_SCHEME)
#define SET_BB(V)    BBREG = (V)
#else
#define SET_BB(V)
#endif


#ifdef FROZEN_STACKS
#ifdef SBA
#define PROTECT_FROZEN_H(CPTR)                                  \
       ((Unsigned((Int)((CPTR)->cp_h)-(Int)(H_FZ)) <            \
	 Unsigned((Int)(B_FZ)-(Int)(H_FZ))) ?                   \
	(CPTR)->cp_h : H_FZ)
#define PROTECT_FROZEN_B(CPTR)                                  \
       ((Unsigned((Int)(CPTR)-(Int)(H_FZ)) <                    \
	 Unsigned((Int)(B_FZ)-(Int)(H_FZ)))  ?                  \
	(CPTR) : B_FZ)
	 /*
#define PROTECT_FROZEN_H(CPTR) ((CPTR)->cp_h > H_FZ && (CPTR)->cp_h < (CELL *)B_FZ ? (CPTR)->cp_h : H_FZ )

#define PROTECT_FROZEN_B(CPTR)  ((CPTR) < B_FZ && (CPTR) > (choiceptr)H_FZ ? (CPTR) : B_FZ )
	 */
#else
#define PROTECT_FROZEN_B(CPTR)  (YOUNGER_CP(CPTR, B_FZ) ? CPTR         : B_FZ)
#define PROTECT_FROZEN_H(CPTR)  (((CPTR)->cp_h > H_FZ) ? (CPTR)->cp_h : H_FZ)
#endif /* SBA */
#else
#define PROTECT_FROZEN_H(CPTR)  (CPTR)->cp_h
#endif /* TABLING */

#define restore_yaam_regs(AP)                                    \
                 { register CELL *x1 = B_Y->cp_env;	         \
                   register yamop *x2;				 \
                   H = HBREG = PROTECT_FROZEN_H(B_Y);            \
		   restore_yaam_reg_cpdepth(B_Y);	         \
                   CPREG  = B_Y->cp_cp;		                 \
		   /* AP may depend on H */			 \
		   x2 = (yamop *)AP;		                 \
                   ENV    = x1;                                  \
                   YAPOR_update_alternative(PREG, x2)            \
                   B_Y->cp_ap = x2;                              \
                 }

/***************************************************************
* Restore variable number of arguments from a choice point     *
***************************************************************/
#define restore_args(Nargs)                                        \
                 BEGD(d0);                                         \
                 d0 = Nargs;                                       \
                 BEGP(pt0);                                        \
                 BEGP(pt1);                                        \
                 pt1 = (CELL *)(B_Y+1)+d0;                         \
                 pt0 = XREGS+1+d0;                                 \
	         while (pt0 > XREGS +1 )                           \
                   { register CELL x = pt1[-1];                    \
                     --pt0;                                        \
                     --pt1;                                        \
                     *pt0   = x;                                   \
		   }                                               \
                 ENDP(pt1);                                        \
                 ENDP(pt0);                                        \
                 ENDD(d0)

#define restore_at_least_one_arg(Nargs)                            \
                 BEGD(d0);                                         \
                 d0 = Nargs;                                       \
                 BEGP(pt0);                                        \
                 BEGP(pt1);                                        \
                 pt1 = (CELL *)(B_Y+1)+d0;                         \
                 pt0 = XREGS+1+d0;                                 \
                 do { register CELL x = pt1[-1];                   \
                     --pt0;                                        \
                     --pt1;                                        \
                     *pt0   = x;                                   \
		   }                                               \
	         while (pt0 > XREGS +1 );                          \
                 ENDP(pt1);                                        \
                 ENDP(pt0);                                        \
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

#define pop_yaam_regs()                                           \
                 { register CELL *ptr1;                           \
                   H = PROTECT_FROZEN_H(B_Y);                     \
		   B = B_Y->cp_b;	                          \
                   pop_yaam_reg_cpdepth(B_Y);	                  \
		   CPREG = B_Y->cp_cp;		                  \
		   ptr1 = B_Y->cp_env;				  \
                   TABLING_close_alt(B_Y);	                  \
                   HBREG = PROTECT_FROZEN_H(B);		          \
                   ENV = ptr1;                                    \
                 }

#define pop_args(NArgs)                                           \
                 BEGD(d0);                                        \
                 d0 = (NArgs);                                    \
                 BEGP(pt0);                                       \
                 BEGP(pt1);                                       \
                 S_Y = (CELL *)(B_Y+1);	                          \
                 pt0 = XREGS + 1 ;                                \
                 pt1 = S_Y ;                                      \
		 while (pt0 < XREGS+1+d0)                         \
                   { register CELL x = pt1[0];                    \
                     pt1++;                                       \
                     pt0++;                                       \
                     pt0[-1] = x;                                 \
		   }                                              \
                 S_Y = pt1;					  \
                 ENDP(pt1);                                       \
                 ENDP(pt0);                                       \
                 ENDD(d0);

#define pop_at_least_one_arg(NArgs)                               \
                 BEGD(d0);                                        \
                 d0 = (NArgs);                                    \
                 BEGP(pt0);                                       \
                 BEGP(pt1);                                       \
                 pt1 = (CELL *)(B_Y+1);	                          \
                 pt0 = XREGS + 1 ;                                \
                 do { register CELL x = pt1[0];                   \
                     pt1++;                                       \
                     pt0++;                                       \
                     pt0[-1] = x;                                 \
		   }                                              \
		 while (pt0 < XREGS+1+d0);                        \
                 S_Y = pt1;	                                  \
                 ENDP(pt1);                                       \
                 ENDP(pt0);                                       \
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

#define FAIL()	goto fail

/**********************************************************************
 *                                                                    *
 *                      unification routines                          *
 *                                                                    *
 **********************************************************************/

#ifdef COROUTINING
#define UnifyCells(a, b, l1, l2)                                  \
     if((a) > (b)) {                                              \
	if ((a)<=H) { BIND_GLOBAL((a),(CELL)(b),l1); }            \
	else if ((b)<= H) { Bind_Local((a),(CELL)(b)); goto l1;}  \
	else { Bind_Local((b),(CELL) (a));  goto l1;}             \
     } else if((a) < (b)){                                        \
	if((b) <= H) { BIND_GLOBAL2((b),(CELL) (a),l2,l1); }      \
	else if ((a) <= H) { Bind_Local((b),(CELL) (a));  goto l1;} \
	else { Bind_Local((a),(CELL) (b));  goto l1;}             \
     }

/* I know (a) <= H */
#define UnifyGlobalRegCells(a, b, l1, l2)                         \
     if((a) > (b)) {                                              \
	BIND_GLOBAL((a),(CELL)(b),l1);                            \
     } else if((a) < (b)){                                        \
	if((b) <= H) { BIND_GLOBAL2((b),(CELL) (a),l2,l1); }      \
	Bind_Local((b),(CELL) (a));                               \
     }

#else
#define UnifyCells(a, b, l1, l2)                                  \
     if((a) > (b)) {                                              \
	if ((a)<=H) { BIND_GLOBAL((a),(CELL)(b),l1); }            \
	else if ((b)<= H) { Bind_Local((a),(CELL)(b)); }          \
	else { Bind_Local((b),(CELL) (a)); }                      \
     } else if((a) < (b)){                                        \
	if((b) <= H) { BIND_GLOBAL2((b),(CELL) (a),l2,l1); }      \
	else if ((a) <= H) { Bind_Local((b),(CELL) (a)); }        \
	else { Bind_Local((a),(CELL) (b)); }                      \
     }

/* I know (a) <= H */
#define UnifyGlobalRegCells(a, b, l1, l2)                         \
     if((a) > (b)) {                                              \
	BIND_GLOBAL((a),(CELL)(b),l1);                            \
     } else if((a) < (b)){                                        \
	if((b) <= H) { BIND_GLOBAL2((b),(CELL) (a),l2,l1); }      \
	Bind_Local((b),(CELL) (a));                               \
     }

#endif

#define UnifyGlobalCells(a, b)                                    \
     if((a) > (b)) {                                              \
	BIND_GLOBALCELL((a),(CELL)(b));                           \
     } else if((a) < (b)){                                        \
	BIND_GLOBALCELL((b),(CELL) (a));                          \
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

#define UNWIND_CUNIF()                                        \
         while (visited < AuxSp) {                            \
            pt1 = (CELL *)visited[0];                         \
            *pt1 = visited[1];                                \
            visited += 2;                                     \
         }

#else
#define UNWIND_CUNIF()
#endif

#define UnifyBound_TEST_ATTACHED(f,d0,pt0,d1)                          \
 if (IsExtensionFunctor(f)) {                                          \
   if (unify_extension(f, d0, RepAppl(d0), d1))                        \
        { GONext(); }                                                  \
      else                                                             \
        { FAIL(); }                                                    \
    }


#define UnifyBound(d0,d1)                                              \
  if (d0 == d1) GONext();                                              \
  if (IsPairTerm(d0)) {                                                \
    register CELL *pt0, *pt1;                                          \
    if (!IsPairTerm(d1)) { FAIL(); }                                   \
    pt0 = RepPair(d0);                                                 \
    pt1 = RepPair(d1);                                                 \
    save_hb();							       \
    always_save_pc();						       \
    if (IUnify_complex(pt0-1,pt0+1,pt1-1)) {always_set_pc(); GONext();}\
    else { FAIL(); }                                                   \
  } else if (IsApplTerm(d0)) {                                         \
    register CELL *pt0, *pt1;                                          \
    register Functor f;                                                \
    if (!IsApplTerm(d1)) { FAIL(); }                                   \
    pt0 = RepAppl(d0);                                                 \
    pt1 = RepAppl(d1);                                                 \
    f = (Functor)*pt0;                                                 \
    if (f != (Functor)*pt1) { FAIL(); }                                \
    UnifyBound_TEST_ATTACHED(f,d0,pt0,d1);                             \
    d0 = ArityOfFunctor(f);                                            \
    always_save_pc();						       \
    save_hb();							       \
    if (IUnify_complex(pt0, pt0+d0, pt1)) {always_set_pc(); GONext();} \
    else { FAIL(); }                                                   \
  }                                                                    \
  else { FAIL(); }


/* 
 * Next, HB
 */
#if SHADOW_HB
#undef HBREG
#define set_hb()	HBREG = HB
#define save_hb()	HB = HBREG
#else
#define set_hb()
#define save_hb()
#endif


#if FROZEN_STACKS
static inline tr_fr_ptr
trim_trail(choiceptr b, tr_fr_ptr tr, CELL *hbreg)
{
  tr_fr_ptr pt1, pt0;
  pt1 = tr;
  pt0 = TR = B->cp_tr;
  BEGD(d0);
  d0 = Unsigned(hbreg);
  while (pt0 < pt1) {
    BEGD(d1);
    if (IsVarTerm(d1 = TrailTerm(pt0))) {
      if (d1 < d0 || d1 > Unsigned(B)) { 
	DO_TRAIL(d1, TrailVal(pt0));     
      }                                  
      pt0++;                                 
    } else {
      if (!IsPairTerm(d1)) {
	DO_TRAIL(d1, TrailVal(pt0));
      }               
      pt0++;                  
    }
    ENDD(d1);                              
  }  
  ENDD(d0);                                
  return(TR);
}

#elif defined(SBA) || defined(MULTI_ASSIGNMENT_VARIABLES)

#define trim_trail(B, TR, HBREG)  (TR)

#else
static inline tr_fr_ptr
trim_trail(choiceptr b, tr_fr_ptr tr, CELL *hbreg)
{
  tr_fr_ptr pt1, pt0;
  pt1 = TR;
  pt0 = TR = B->cp_tr;
  BEGD(d0);
  d0 = Unsigned(HBREG);
  while (pt0 < pt1) {
    BEGD(d1);
    if (IsVarTerm(d1 = TrailTerm(pt0))) {
      if (d1 < d0 || d1 > Unsigned(B)) { 
	DO_TRAIL(d1, TrailVal(pt0));     
      }                                  
      pt0++;                                 
      ENDD(d1);                              
      ENDD(d0);                                
    }
  }  
  return(TR);
}
#endif /* FROZEN_STACKS */


