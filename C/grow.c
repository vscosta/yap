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
* File:		grow.c							 *
* Last rev:	Thu Feb 23 1989		vv				 *
* mods:									 *
* comments:	Shifting the stacks       				 *
*									 *
*************************************************************************/

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "yapio.h"
#include "alloc.h"
#include "sshift.h"
#include "compile.h"
#if HAVE_STRING_H
#include <string.h>
#endif

static int heap_overflows = 0;
static Int total_heap_overflow_time = 0;

int stack_overflows = 0;
static Int total_stack_overflow_time = 0;

int delay_overflows = 0;
static Int total_delay_overflow_time = 0;

static int trail_overflows = 0;
static Int total_trail_overflow_time = 0;

STATIC_PROTO(Int p_growheap, (void));
STATIC_PROTO(Int p_growstack, (void));
STATIC_PROTO(Int p_inform_trail_overflows, (void));
STATIC_PROTO(Int p_inform_heap_overflows, (void));
STATIC_PROTO(Int p_inform_stack_overflows, (void));

/* #define undf7  */
/* #define undf5 */

STATIC_PROTO(void MoveGlobal, (void));
STATIC_PROTO(void MoveLocalAndTrail, (void));
STATIC_PROTO(void SetHeapRegs, (void));
STATIC_PROTO(void SetStackRegs, (void));
STATIC_PROTO(void AdjustTrail, (int));
STATIC_PROTO(void AdjustLocal, (void));
STATIC_PROTO(void AdjustGlobal, (void));
STATIC_PROTO(void AdjustGrowStack, (void));
STATIC_PROTO(int  local_growheap, (long,int));
STATIC_PROTO(void cpcellsd, (CELL *, CELL *, CELL));
STATIC_PROTO(CELL AdjustAppl, (CELL));
STATIC_PROTO(CELL AdjustPair, (CELL));

static void
cpcellsd(register CELL *Dest, register CELL *Org, CELL NOf)
{
#if HAVE_MEMMOVE
  memmove((void *)Dest, (void *)Org, NOf*sizeof(CELL));
#else
  register Int    n_of = NOf;
  for (; n_of >= 0; n_of--)
    *--Dest = *--Org;
#endif
}


/* The old stack pointers */
CELL    *OldASP, *OldLCL0;
tr_fr_ptr OldTR;
CELL    *OldGlobalBase, *OldH, *OldH0;
ADDR     OldTrailBase, OldTrailTop;
ADDR     OldHeapBase, OldHeapTop;

Int
  GDiff,
  HDiff,
  LDiff,
  TrDiff,
  XDiff,
  DelayDiff;

static void
SetHeapRegs(void)
{
#ifdef undf7
  YP_fprintf(YP_stderr,"HeapBase = %x\tHeapTop=%x\nGlobalBase=%x\tGlobalTop=%x\nLocalBase=%x\tLocatTop=%x\n", HeapBase, HeapTop, GlobalBase, H, LCL0, ASP);
#endif
  /* The old stack pointers */
  OldLCL0 = LCL0;
  OldASP = ASP;
  OldGlobalBase = (CELL *)GlobalBase;
  OldH = H;
  OldH0 = H0;
  OldTrailBase = TrailBase;
  OldTrailTop = TrailTop;
  OldTR = TR;
  OldHeapBase = HeapBase;
  OldHeapTop = HeapTop;
  /* Adjust stack addresses */
  TrailBase = TrailAddrAdjust(TrailBase);
  TrailTop = TrailAddrAdjust(TrailTop);
  GlobalBase = DelayAddrAdjust(GlobalBase);
  LocalBase = LocalAddrAdjust(LocalBase);
  AuxSp = PtoDelayAdjust(AuxSp);
  AuxTop = DelayAddrAdjust(AuxTop);
  /* The registers pointing to one of the stacks */
  ENV = PtoLocAdjust(ENV);
  ASP = PtoLocAdjust(ASP);
  H0 = PtoGloAdjust(H0);
  LCL0 = PtoLocAdjust(LCL0);
  H = PtoGloAdjust(H);
  HB = PtoGloAdjust(HB);
  B = ChoicePtrAdjust(B);
  if (TopB != NULL)
    TopB = ChoicePtrAdjust(TopB);
  if (DelayedB != NULL)
    DelayedB = ChoicePtrAdjust(DelayedB);
#ifdef TABLING
  B_FZ = ChoicePtrAdjust(B_FZ);
  BB = ChoicePtrAdjust(BB);
  H_FZ = PtoGloAdjust(H_FZ);
  TR_FZ = PtoTRAdjust(TR_FZ);
#endif
  TR = PtoTRAdjust(TR);
  YENV = PtoLocAdjust(YENV);
  if (IsOldGlobalPtr(S))
    S = PtoGloAdjust(S);
  if (MyTR)
    MyTR = PtoTRAdjust(MyTR);
#ifdef COROUTINING
  DelayedVars = AbsAppl(PtoGloAdjust(RepAppl(DelayedVars)));
  MutableList = AbsAppl(PtoGloAdjust(RepAppl(MutableList)));
  AttsMutableList = AbsAppl(PtoGloAdjust(RepAppl(AttsMutableList)));
  WokenGoals = AbsAppl(PtoGloAdjust(RepAppl(WokenGoals)));
#endif
  if (CurrentModulePtr)
    CurrentModulePtr = PtoGloAdjust(CurrentModulePtr);
}

static void
SetStackRegs(void)
{
  /* The old local stack pointers */
  OldLCL0 = LCL0;
  OldASP = ASP;
  OldH = H;
  OldH0 = H0;
  OldGlobalBase = (CELL *)GlobalBase;
  OldTrailTop = TrailTop;
  OldTrailBase = TrailBase;
  OldTR = TR;
  OldHeapBase = HeapBase;
  OldHeapTop = HeapTop;
  /* The local and aux stack addresses */
  TrailBase = TrailAddrAdjust(TrailBase);
  TrailTop = TrailAddrAdjust(TrailTop);
  LocalBase = LocalAddrAdjust(LocalBase);
  TR = PtoTRAdjust(TR);
  /* The registers pointing to the local stack */
  ENV = PtoLocAdjust(ENV);
  ASP = PtoLocAdjust(ASP);
  LCL0 = PtoLocAdjust(LCL0);
  B = ChoicePtrAdjust(B);
  if (TopB != NULL)
    TopB = ChoicePtrAdjust(TopB);
  if (DelayedB != NULL)
    DelayedB = ChoicePtrAdjust(DelayedB);
#ifdef TABLING
  B_FZ = ChoicePtrAdjust(B_FZ);
  BB = ChoicePtrAdjust(BB);
  TR_FZ = PtoTRAdjust(TR_FZ);
#endif
  YENV = PtoLocAdjust(YENV);
  if (MyTR)
    MyTR = PtoTRAdjust(MyTR);
  if (CurrentModulePtr)
    CurrentModulePtr = PtoGloAdjust(CurrentModulePtr);
}

static void
MoveLocalAndTrail(void)
{
	/* cpcellsd(To,From,NOfCells) - copy the cells downwards  */
#if HAVE_MEMMOVE
  cpcellsd(ASP, OldASP, (CELL *)OldTR - OldASP);
#else
  cpcellsd((CELL *)TR, (CELL *)OldTR, (CELL *)OldTR - OldASP);
#endif
}

static void
MoveGlobal(void)
{
	/*
	 * cpcellsd(To,From,NOfCells) - copy the cells downwards - in
	 * absmi.asm 
	 */
#if HAVE_MEMMOVE
  cpcellsd((CELL *)GlobalBase, (CELL *)OldGlobalBase, OldH - (CELL *)OldGlobalBase);  
#else
  cpcellsd(H, OldH, OldH - (CELL *)OldGlobalBase);
#endif
}

static void
MoveGlobalOnly(void)
{
	/*
	 * cpcellsd(To,From,NOfCells) - copy the cells downwards - in
	 * absmi.asm 
	 */
#if HAVE_MEMMOVE
  cpcellsd(H0, OldH0, OldH - OldH0);  
#else
  cpcellsd(H, OldH, OldH - OldH0);
#endif
}

static inline CELL
AdjustAppl(register CELL t0)
{
  register CELL   *t = RepAppl(t0);

  if (IsOldGlobalPtr(t))
    return (AbsAppl(PtoGloAdjust(t)));
  else if (IsOldDelayPtr(t))
    return (AbsAppl(PtoDelayAdjust(t)));
  else if (IsOldTrailPtr(t))
    return (AbsAppl(CellPtoTRAdjust(t)));
  else if (IsHeapP(t))
    return (AbsAppl(CellPtoHeapAdjust(t)));
#ifdef DEBUG
  else {
    /* strange cell */
    /*    YP_fprintf(YP_stderr,"[ garbage appl %lx found in stacks by stack shifter ]\n", t0);*/
  }
#endif
  return(t0);
}

static inline CELL
AdjustPair(register CELL t0)
{
  register CELL  *t = RepPair(t0);

  if (IsOldGlobalPtr(t))
    return (AbsPair(PtoGloAdjust(t)));
  if (IsOldDelayPtr(t))
    return (AbsPair(PtoDelayAdjust(t)));
  if (IsOldTrailPtr(t))
    return (AbsPair(CellPtoTRAdjust(t)));
  else if (IsHeapP(t))
    return (AbsPair(CellPtoHeapAdjust(t)));
#ifdef DEBUG
  /* YP_fprintf(YP_stderr,"[ garbage pair %lx found in stacks by stack shifter ]\n", t0);*/
#endif
  return(t0);
}

static void
AdjustTrail(int adjusting_heap)
{
  register tr_fr_ptr ptt;

  ptt = TR;
  /* moving the trail is simple */
  while (ptt != (tr_fr_ptr)TrailBase) {
    register CELL reg = TrailTerm(ptt-1);

    ptt--;
    if (IsVarTerm(reg)) {
      if (IsOldLocalInTR(reg))
	TrailTerm(ptt) = LocalAdjust(reg);
      else if (IsOldGlobal(reg))
	TrailTerm(ptt) = GlobalAdjust(reg);
      else if (IsOldDelay(reg))
	TrailTerm(ptt) = DelayAdjust(reg);
      else if (IsOldTrail(reg))
	TrailTerm(ptt) = TrailAdjust(reg);
      else if (IsOldCode(reg)) {
	CELL *ptr;
	TrailTerm(ptt) = reg = CodeAdjust(reg);
	ptr = (CELL *)reg;
	if (IsApplTerm(*ptr)) {
	  *ptr = AdjustAppl(*ptr);
	} else if (IsPairTerm(*ptr)) {
	  *ptr = AdjustAppl(*ptr);
	}
#ifdef DEBUG
	else 
	  YP_fprintf(YP_stderr,"[ garbage heap ptr %p to %lx found in trail at %p by stack shifter ]\n", ptr, (unsigned long int)*ptr, ptt);
#endif
      }
    } else if (IsPairTerm(reg)) {
     TrailTerm(ptt) = AdjustPair(reg);
#ifdef MULTI_ASSIGNMENT_VARIABLES /* does not work with new structures */
    /* check it whether we are protecting a
       multi-assignment */
    } else if (IsApplTerm(reg)) {
      TrailTerm(ptt) = AdjustAppl(reg);
#endif
    }
  }

}

static void
AdjustLocal(void)
{
  register CELL   reg, *pt;

  /* Adjusting the local */
  pt = LCL0;
  while (pt > ASP) {
    reg = *--pt;
    if (IsVarTerm(reg)) {
      if (IsOldLocal(reg))
	*pt = LocalAdjust(reg);
      else if (IsOldGlobal(reg))
	*pt = GlobalAdjust(reg);
      else if (IsOldDelay(reg))
	*pt = DelayAdjust(reg);
      else if (IsOldTrail(reg))
	*pt = TrailAdjust(reg);
      else if (IsOldCode(reg))
	*pt = CodeAdjust(reg);
    } else if (IsApplTerm(reg)) {
      *pt = AdjustAppl(reg);
    } else if (IsPairTerm(reg)) {
      *pt = AdjustPair(reg);
    }
  }

}

static void
AdjustGlobal(void)
{
  register CELL *pt;

  /*
   * to clean the global now that functors are just variables pointing to
   * the code 
   */
  pt = CellPtr(GlobalBase);
  while (pt < H) {
    register CELL reg;
    
    reg = *pt;
      if (IsVarTerm(reg)) {
	if (IsOldGlobal(reg))
	  *pt = GlobalAdjust(reg);
	if (IsOldDelay(reg))
	  *pt = DelayAdjust(reg);
	else if (IsOldLocal(reg))
	  *pt = LocalAdjust(reg);
	else if (IsOldCode(reg)) {
	  Functor f;
	  f = (Functor)(*pt = CodeAdjust(reg));
	  if (f <= FunctorDouble && f >= FunctorLongInt) {
	    /* skip bitmaps */
	    switch((CELL)f) {
	    case (CELL)FunctorDouble:
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
	      pt += 3;
#else
	      pt += 2;
#endif
	      break;
#if USE_GMP
	    case (CELL)FunctorBigInt:
	      {
		Int sz = 1+
		  sizeof(MP_INT)+
		  (((MP_INT *)(pt+1))->_mp_alloc*sizeof(mp_limb_t));
		pt += sz;
	      }
	      break;
#endif
	    case (CELL)FunctorLongInt:
	    default:
	      pt += 2;
	      break;
	    }
	  }
	}
#ifdef MULTI_ASSIGNMENT_VARIABLES
	else if (IsOldTrail(reg))
	  *pt = TrailAdjust(reg);
#endif
      } else if (IsApplTerm(reg))
	*pt = AdjustAppl(reg);
      else if (IsPairTerm(reg))
	*pt = AdjustPair(reg);
      else if (IsAtomTerm(reg))
	*pt = AtomTermAdjust(reg);
    pt++;
  }
}

/*
 * When growing the stack we need to adjust: the local stack cells pointing
 * to the local; the local stack cells and the X terms pointing to the global
 * (just once) the trail cells pointing both to the global and to the local
 */
void
AdjustStacksAndTrail(void)
{
  AdjustTrail(TRUE);
  AdjustLocal();
  AdjustGlobal();
}

/*
 * When growing the stack we need to adjust: the local cells pointing to the
 * local; the trail cells pointing to the local
 */
static void
AdjustGrowStack(void)
{
  AdjustTrail(FALSE);
  AdjustLocal();
}

void
AdjustRegs(int n)
{
  int            i;
  CELL	       reg;
	
  for (i = 1; i < n; ++i) {
    reg = (CELL) XREGS[i];
    if (IsVarTerm(reg)) {
      if (IsOldLocal(reg))
	reg = LocalAdjust(reg);
      else if (IsOldGlobal(reg))
	reg = GlobalAdjust(reg);
      else if (IsOldDelay(reg))
	reg = DelayAdjust(reg);
      else if (IsOldTrail(reg))
	reg = TrailAdjust(reg);
      else if (IsOldCode(reg))
	reg = CodeAdjust(reg);
    } else if (IsApplTerm(reg))
      reg = AdjustAppl(reg);
    else if (IsPairTerm(reg))
      reg = AdjustPair(reg);
    XREGS[i] = (Term) reg;
  }
}

/* Used by do_goal() when we're short of heap space */
static int
local_growheap(long size, int fix_code)
{
  Int start_growth_time, growth_time;
  int gc_verbose;

  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  if (!ExtendWorkSpace(size)) {
    return(FALSE);
  }
  start_growth_time = cputime();
  gc_verbose = is_gc_verbose();
  heap_overflows++;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[HO] Heap overflow %d\n", heap_overflows);
    YP_fprintf(YP_stderr, "[HO]   growing the heap %ld bytes\n", size);
  }
  ASP -= 256;
  TrDiff = LDiff = GDiff = DelayDiff = size;
  XDiff = HDiff = 0;
  YAPEnterCriticalSection();
  SetHeapRegs();
  MoveLocalAndTrail();
  if (fix_code) {
    CELL *SaveOldH = OldH;
    OldH = (CELL *)freep;
    MoveGlobal();
    OldH = SaveOldH;
  } else {
    MoveGlobal();
  }
  AdjustStacksAndTrail();
  AdjustRegs(MaxTemps);
  YAPLeaveCriticalSection();
  ASP += 256;
  growth_time = cputime()-start_growth_time;
  total_heap_overflow_time += growth_time;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[HO]   took %g sec\n", (double)growth_time/1000);
    YP_fprintf(YP_stderr, "[HO] Total of %g sec expanding heap \n", (double)total_heap_overflow_time/1000);
  }
  return(TRUE);
}

/* Used by do_goal() when we're short of heap space */
static int
local_growglobal(long size)
{
  Int start_growth_time, growth_time;
  int gc_verbose;

  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  if (!ExtendWorkSpace(size)) {
    return(FALSE);
  }
  start_growth_time = cputime();
  gc_verbose = is_gc_verbose();
  delay_overflows++;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[DO] Delay overflow %d\n", delay_overflows);
    YP_fprintf(YP_stderr, "[DO]   growing the stacks %ld bytes\n", size);
  }
  ASP -= 256;
  TrDiff = LDiff = GDiff = size;
  XDiff = HDiff = DelayDiff = 0;
  YAPEnterCriticalSection();
  SetHeapRegs();
  MoveLocalAndTrail();
  MoveGlobalOnly();
  AdjustStacksAndTrail();
  AdjustRegs(MaxTemps);
  YAPLeaveCriticalSection();
  ASP += 256;
  growth_time = cputime()-start_growth_time;
  total_delay_overflow_time += growth_time;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[DO]   took %g sec\n", (double)growth_time/1000);
    YP_fprintf(YP_stderr, "[DO] Total of %g sec expanding stacks \n", (double)total_delay_overflow_time/1000);
  }
  return(TRUE);
}


static void
fix_compiler_instructions(PInstr *cpc)
{
  while (cpc != NULL) {
    PInstr *ncpc = cpc->nextInst;

    switch(cpc->op) {
      /* check c_var for functions that point at variables */
    case get_var_op:
    case get_val_op:
    case unify_var_op:
    case unify_last_var_op:
    case unify_val_op:
    case unify_last_val_op:
    case put_var_op:
    case put_val_op:
    case write_var_op:
    case write_val_op:
    case f_var_op:
    case f_val_op:
    case fetch_args_for_bccall:
    case bccall_op:
    case save_pair_op:
    case save_appl_op:
    case save_b_op:
    case comit_b_op:
      cpc->rnd1 = GlobalAdjust(cpc->rnd1);
      break;
    default:
      /* hopefully nothing to do */
      break;
    }
    if (ncpc != NULL) {
      ncpc = (PInstr *)GlobalAddrAdjust((ADDR)(cpc->nextInst));
      cpc->nextInst = ncpc;
    }
    cpc = ncpc;
  }
}

#ifdef TABLING
static void
fix_tabling_info(void) 
{
  /* we must fix the dependency frames and the subgoal frames, as they are
     pointing back to the global stack. */
  struct dependency_frame *df;
  struct subgoal_frame *sg;

  df = LOCAL_top_dep_fr;
  while (df != NULL) {
    if (DepFr_backchain_cp(df))
      DepFr_backchain_cp(df) = ChoicePtrAdjust(DepFr_backchain_cp(df));
    DepFr_leader_cp(df) = ChoicePtrAdjust(DepFr_leader_cp(df));
    DepFr_cons_cp(df) = ConsumerChoicePtrAdjust(DepFr_cons_cp(df));
    df = DepFr_next(df);
  }
  sg = LOCAL_top_sg_fr;
  while (sg != NULL) {
    SgFr_gen_cp(sg) = GeneratorChoicePtrAdjust(SgFr_gen_cp(sg));
    sg = SgFr_next(sg);
  }
}
#endif /* TABLING */

int
growheap(int fix_code)
{
  unsigned long size = sizeof(CELL) * 16 * 1024L;
  int shift_factor = (heap_overflows > 8 ? 8 : heap_overflows);
  unsigned long sz =  size << shift_factor;

#ifdef FIXED_STACKS
  abort_optyap("noheapleft in function absmi");
#endif
  if (SizeOfOverflow > sz)
    sz = AdjustPageSize(SizeOfOverflow);
  while(sz >= sizeof(CELL) * 16 * 1024L && !local_growheap(sz, fix_code)) {
    size = size/2;
    sz =  size << shift_factor;
  }
  /* we must fix an instruction chain */
  if (fix_code) {
    PInstr *cpc = CodeStart;
    if (cpc != NULL) {
      CodeStart = cpc = (PInstr *)GlobalAddrAdjust((ADDR)cpc);
    }
    fix_compiler_instructions(cpc);
    cpc = BlobsStart;
    if (cpc != NULL) {
      BlobsStart = cpc = (PInstr *)GlobalAddrAdjust((ADDR)cpc);
    }
    fix_compiler_instructions(cpc);
  }
#ifdef TABLING
  fix_tabling_info();
#endif
  return(sz >= sizeof(CELL) * 16 * 1024L);
}

int
growglobal(void)
{
  unsigned long sz = sizeof(CELL) * 16 * 1024L;

#ifdef FIXED_STACKS
  abort_optyap("noheapleft in function absmi");
#endif
  if (!local_growglobal(sz))
    return(FALSE);
#ifdef TABLING
  fix_tabling_info();
#endif
  return(TRUE);
}


/* Used by do_goal() when we're short of stack space */
int
growstack(long size)
{
  Int start_growth_time, growth_time;
  int gc_verbose;

#ifdef FIXED_STACKS
  abort_optyap("nostackleft in function absmi");
#endif
  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  if (!ExtendWorkSpace(size)) {
    return(FALSE);
  }
  start_growth_time = cputime();
  gc_verbose = is_gc_verbose();
  stack_overflows++;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[SO] Stack overflow %d\n", stack_overflows);
    YP_fprintf(YP_stderr, "[SO] Heap: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)GlobalBase),GlobalBase,H);
    YP_fprintf(YP_stderr, "[SO] Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    YP_fprintf(YP_stderr, "[SO] Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)TrailBase),TrailBase,TR);
    YP_fprintf(YP_stderr, "[SO]    growing the stacks %ld bytes\n", size);
  }
  TrDiff = LDiff = size;
  XDiff = HDiff = GDiff = DelayDiff = 0;
  ASP -= 256;
  YAPEnterCriticalSection();
  SetStackRegs();
  MoveLocalAndTrail();
  AdjustGrowStack();
  AdjustRegs(MaxTemps);
#ifdef TABLING
  fix_tabling_info();
#endif
  YAPLeaveCriticalSection();
  CreepFlag = CalculateStackGap();
  ASP += 256;
  growth_time = cputime()-start_growth_time;
  total_stack_overflow_time += growth_time;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[SO]    took %g sec\n", (double)growth_time/1000);
    YP_fprintf(YP_stderr, "[SO] Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  return(TRUE);
}

static void
AdjustVarTable(VarEntry *ves)
{
  ves->VarAdr = TermNil;
  if (ves->VarRight != NULL) {
    ves->VarRight = (VarEntry *)TrailAddrAdjust((ADDR)(ves->VarRight));
    AdjustVarTable(ves->VarRight);
  }
  if (ves->VarLeft != NULL) {
    ves->VarLeft = (VarEntry *)TrailAddrAdjust((ADDR)(ves->VarLeft));
    AdjustVarTable(ves->VarLeft);
  }
}

/*
  If we have to shift while we are scanning we need to adjust all
  pointers created by the scanner (Tokens and Variables)
*/
static void
AdjustScannerStacks(TokEntry **tksp, VarEntry **vep)
{
  TokEntry *tks = *tksp;
  VarEntry *ves = *vep;

  if (tks != NULL) {
    tks = *tksp = (TokEntry *)TrailAddrAdjust((ADDR)tks);
  }
  while (tks != NULL) {
    TokEntry *tktmp;

    switch (tks->Tok) {
    case Var_tok:
    case String_tok:
      tks->TokInfo = TrailAdjust(tks->TokInfo);
      break;
    case Name_tok:
      tks->TokInfo = (Term)AtomAdjust((Atom)(tks->TokInfo));
      break;
    default:
      break;
    }
    tktmp = tks->TokNext;
    if (tktmp != NULL) {
      tktmp = (TokEntry *)TrailAddrAdjust((ADDR)tktmp);
      tks->TokNext = tktmp;
    }
    tks = tktmp;
  }
  if (ves != NULL) {
    ves = *vep = (VarEntry *)TrailAddrAdjust((ADDR)ves);
    AdjustVarTable(ves);
  }
  ves = AnonVarTable;
  if (ves != NULL) {
    ves = AnonVarTable = (VarEntry *)TrailAddrAdjust((ADDR)ves);
  }
  while (ves != NULL) {
    VarEntry *vetmp = ves->VarLeft;
    if (vetmp != NULL) {
      vetmp = (VarEntry *)TrailAddrAdjust((ADDR)vetmp);
      ves->VarLeft = vetmp;
    }
    ves->VarAdr = TermNil;
    ves = vetmp;
  }
}

/* Used by parser when we're short of stack space */
int
growstack_in_parser(tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  Int start_growth_time, growth_time;
  int gc_verbose;
  long size  = sizeof(CELL)*(LCL0-(CELL *)GlobalBase);

#ifdef FIXED_STACKS
  abort_optyap("nostackleft in parser");
#endif
  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  if (!ExtendWorkSpace(size)) {
    return(FALSE);
  }
  start_growth_time = cputime();
  gc_verbose = is_gc_verbose();
  stack_overflows++;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[SO] Stack overflow %d\n", stack_overflows);
    YP_fprintf(YP_stderr, "[SO] Heap: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)GlobalBase),(CELL *)GlobalBase,H);
    YP_fprintf(YP_stderr, "[SO] Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    YP_fprintf(YP_stderr, "[SO] Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)TrailBase),TrailBase,TR);
    YP_fprintf(YP_stderr, "[SO]    growing the stacks %ld bytes\n", size);
  }
  TrDiff = LDiff = size;
  XDiff = HDiff = GDiff = DelayDiff = 0;
  ASP -= 256;
  YAPEnterCriticalSection();
  SetStackRegs();
  MoveLocalAndTrail();
  AdjustScannerStacks(tksp, vep);
  {
    tr_fr_ptr nTR = TR;
    *old_trp = TR = PtoTRAdjust(*old_trp);
    AdjustGrowStack();
    TR = nTR;
  }
  AdjustRegs(MaxTemps);
  YAPLeaveCriticalSection();
  CreepFlag = CalculateStackGap();
  ASP += 256;
  growth_time = cputime()-start_growth_time;
  total_stack_overflow_time += growth_time;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[SO]    took %g sec\n", (double)growth_time/1000);
    YP_fprintf(YP_stderr, "[SO] Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  return(TRUE);  
}


/* Used by do_goal() when we're short of stack space */
int
growtrail(long size)
{
  Int start_growth_time = cputime(), growth_time;
  int gc_verbose = is_gc_verbose();

#ifdef FIXED_STACKS
  abort_optyap("notrailleft in function absmi");
#endif
  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  trail_overflows++;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[TO] Trail overflow %d\n", trail_overflows);
    YP_fprintf(YP_stderr, "[TO]    growing the trail %ld bytes\n", size);
  }
  if (!ExtendWorkSpace(size)) {
    return(FALSE);
  }
  YAPEnterCriticalSection();
  TrailTop += size;
  YAPLeaveCriticalSection();
  growth_time = cputime()-start_growth_time;
  total_trail_overflow_time += growth_time;
  if (gc_verbose) {
    YP_fprintf(YP_stderr, "[TO]   took %g sec\n", (double)growth_time/1000);
    YP_fprintf(YP_stderr, "[TO] Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  return(TRUE);
}


static Int
p_inform_trail_overflows(void)
{
  Term tn = MkIntTerm(trail_overflows);
  Term tt = MkIntegerTerm(total_trail_overflow_time);
 
  return(unify(tn, ARG1) && unify(tt, ARG2));
}

/* :- grow_heap(Size) */
static Int
p_growheap(void)
{
  Int             diff;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "grow_heap/1");
    return(FALSE);
  } else if (!IsIntTerm(t1)) {
    Error(TYPE_ERROR_INTEGER, t1, "grow_heap/1");
    return(FALSE);
  }
  diff = IntOfTerm(t1);
  if (diff < 0) {
    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t1, "grow_heap/1");
  }
  return(local_growheap(diff, FALSE));
}

static Int
p_inform_heap_overflows(void)
{
  Term tn = MkIntTerm(heap_overflows);
  Term tt = MkIntegerTerm(total_heap_overflow_time);
 
  return(unify(tn, ARG1) && unify(tt, ARG2));
}

/* :- grow_stack(Size) */
static Int
p_growstack(void)
{
  Int             diff;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR, t1, "grow_stack/1");
    return(FALSE);
  } else if (!IsIntTerm(t1)) {
    Error(TYPE_ERROR_INTEGER, t1, "grow_stack/1");
    return(FALSE);
  }
  diff = IntOfTerm(t1);
  if (diff < 0) {
    Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t1, "grow_stack/1");
  }
  return(growstack(diff));
}

static Int
p_inform_stack_overflows(void)
{
  Term tn = MkIntTerm(stack_overflows);
  Term tt = MkIntegerTerm(total_stack_overflow_time);
 
  return(unify(tn, ARG1) && unify(tt, ARG2));

}

Int total_stack_shift_time(void)
{
  return(total_heap_overflow_time+
	 total_stack_overflow_time+
	 total_trail_overflow_time);
}

void
InitGrowPreds(void)
{
	InitCPred("$grow_heap", 1, p_growheap, SafePredFlag);
	InitCPred("$grow_stack", 1, p_growstack, SafePredFlag);
	InitCPred("$inform_trail_overflows", 2, p_inform_trail_overflows, SafePredFlag);
	InitCPred("$inform_heap_overflows", 2, p_inform_heap_overflows, SafePredFlag);
	InitCPred("$inform_stack_overflows", 2, p_inform_stack_overflows, SafePredFlag);
	init_gc();
}
