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

#include "absmi.h"
#include "YapHeap.h"
#include "yapio.h"
#include "alloc.h"
#include "sshift.h"
#include "compile.h"
#include "attvar.h"
#ifdef CUT_C
#include "cut_c.h"
#endif /* CUT_C */
#if HAVE_STRING_H
#include <string.h>
#endif

#if !HAVE_STRNCAT
#define strncat(s0,s1,sz)   strcat(s0,s1)
#endif

#if !COROUTINING
#define DelayTop() H0
#endif

static int heap_overflows = 0;
static Int total_heap_overflow_time = 0;

int stack_overflows = 0;
static Int total_stack_overflow_time = 0;

int delay_overflows = 0;
static Int total_delay_overflow_time = 0;

static int trail_overflows = 0;
static Int total_trail_overflow_time = 0;

static int atom_table_overflows = 0;
static Int total_atom_table_overflow_time = 0;

STATIC_PROTO(Int p_growheap, (void));
STATIC_PROTO(Int p_growstack, (void));
STATIC_PROTO(Int p_inform_trail_overflows, (void));
STATIC_PROTO(Int p_inform_heap_overflows, (void));
STATIC_PROTO(Int p_inform_stack_overflows, (void));

/* #define undf7  */
/* #define undf5 */

STATIC_PROTO(int growstack, (long));
STATIC_PROTO(void MoveGlobal, (void));
STATIC_PROTO(void MoveLocalAndTrail, (void));
STATIC_PROTO(void SetHeapRegs, (void));
STATIC_PROTO(void AdjustTrail, (int));
STATIC_PROTO(void AdjustLocal, (void));
STATIC_PROTO(void AdjustGlobal, (long));
STATIC_PROTO(void AdjustGrowStack, (void));
STATIC_PROTO(int  static_growheap, (long,int,struct intermediates *,tr_fr_ptr *, TokEntry **, VarEntry **));
STATIC_PROTO(void cpcellsd, (CELL *, CELL *, CELL));
STATIC_PROTO(CELL AdjustAppl, (CELL));
STATIC_PROTO(CELL AdjustPair, (CELL));
STATIC_PROTO(void AdjustStacksAndTrail, (long));
STATIC_PROTO(void AdjustRegs, (int));

static void
LeaveGrowMode(prolog_exec_mode grow_mode)
{
  Yap_PrologMode &= ~grow_mode;
  if (Yap_PrologMode & AbortMode) {
    Yap_PrologMode &= ~AbortMode;
      Yap_Error(PURE_ABORT, TermNil, "");
      /* in case someone mangles the P register */
      save_machine_regs();
#if  _MSC_VER || defined(__MINGW32__)
      /* don't even think about trying this */
#else
#if PUSH_REGS
      restore_absmi_regs(&Yap_standard_regs);
#endif
      siglongjmp (Yap_RestartEnv, 1);
#endif
  }
}


static void
cpcellsd(register CELL *Dest, register CELL *Org, CELL NOf)
{
#if HAVE_MEMMOVE
  memmove((void *)Dest, (void *)Org, NOf*sizeof(CELL));
#else
  register Int    n_of = NOf;
  for (; n_of >= 0; n_of--)
    *Dest++ = *Org++;
#endif
}


static void
SetHeapRegs(void)
{
#ifdef undf7
  fprintf(Yap_stderr,"HeapBase = %x\tHeapTop=%x\nGlobalBase=%x\tGlobalTop=%x\nLocalBase=%x\tLocatTop=%x\n", Yap_HeapBase, HeapTop, Yap_GlobalBase, H, LCL0, ASP);
#endif
  /* The old stack pointers */
  OldLCL0 = LCL0;
  OldASP = ASP;
  OldGlobalBase = (CELL *)Yap_GlobalBase;
  OldH = H;
  OldH0 = H0;
  OldTrailBase = Yap_TrailBase;
  OldTrailTop = Yap_TrailTop;
  OldTR = TR;
  OldHeapBase = Yap_HeapBase;
  OldHeapTop = HeapTop;
  OldDelayTop = CurrentDelayTop;
  /* Adjust stack addresses */
  Yap_TrailBase = TrailAddrAdjust(Yap_TrailBase);
  Yap_TrailTop = TrailAddrAdjust(Yap_TrailTop);
  if (GDiff) {
    /* make sure we are not just expanding the delay stack */
    Yap_GlobalBase = BaseAddrAdjust(Yap_GlobalBase);
  }
  Yap_LocalBase = LocalAddrAdjust(Yap_LocalBase);
#if !USE_SYSTEM_MALLOC && !USE_DL_MALLOC
  AuxSp = PtoBaseAdjust(AuxSp);
  AuxTop = (ADDR)PtoBaseAdjust((CELL *)AuxTop);
#endif
#if !USE_SYSTEM_MALLOC
  if (HeapLim)
    HeapLim = BaseAddrAdjust(HeapLim);
#endif
  /* The registers pointing to one of the stacks */
  if (ENV)
    ENV = PtoLocAdjust(ENV);
  if (ASP)
    ASP = PtoLocAdjust(ASP);
  if (H0)
    H0 = PtoGloAdjust(H0);
  LOCK(SignalLock);
  if (LCL0)
    LCL0 = PtoLocAdjust(LCL0);
  UNLOCK(SignalLock);
  if (H)
    H = PtoGloAdjust(H);
  if (HB)
    HB = PtoGloAdjust(HB);
  if (B)
    B = ChoicePtrAdjust(B);
  if (CurrentDelayTop)
    CurrentDelayTop = PtoDelayAdjust(CurrentDelayTop);
#ifdef CUT_C
  if (Yap_REGS.CUT_C_TOP)
    Yap_REGS.CUT_C_TOP = (cut_c_str_ptr)ChoicePtrAdjust((choiceptr)Yap_REGS.CUT_C_TOP);
#endif
#ifdef TABLING
  if (B_FZ)
    B_FZ = ChoicePtrAdjust(B_FZ);
  if (BB)
    BB = ChoicePtrAdjust(BB);
  if (H_FZ)
    H_FZ = PtoGloAdjust(H_FZ);
  if (TR_FZ)
    TR_FZ = PtoTRAdjust(TR_FZ);
#endif /* TABLING */
  if (TR)
    TR = PtoTRAdjust(TR);
  if (YENV)
    YENV = PtoLocAdjust(YENV);
  if (IsOldGlobalPtr(S))
    S = PtoGloAdjust(S);
  else if (IsOldLocalPtr(S))
    S = PtoLocAdjust(S);	   
  if (GlobalArena)
    GlobalArena = AbsAppl(PtoGloAdjust(RepAppl(GlobalArena)));
  if (GlobalDelayArena)
    GlobalDelayArena = GlobalAdjust(GlobalDelayArena);
#ifdef COROUTINING
  if (DelayedVars)
    DelayedVars = AbsAppl(PtoGloAdjust(RepAppl(DelayedVars)));
  if (AttsMutableList)
    AttsMutableList = AbsAppl(PtoGloAdjust(RepAppl(AttsMutableList)));
  if (WokenGoals)
    WokenGoals = AbsAppl(PtoGloAdjust(RepAppl(WokenGoals)));
#endif
  GcGeneration = AbsAppl(PtoGloAdjust(RepAppl(GcGeneration)));
  GcPhase = AbsAppl(PtoGloAdjust(RepAppl(GcPhase)));
}

static void
MoveLocalAndTrail(void)
{
	/* cpcellsd(To,From,NOfCells) - copy the cells downwards  */
#if USE_SYSTEM_MALLOC
  cpcellsd(ASP, (CELL *)((char *)OldASP+BaseDiff), (CELL *)OldTR - OldASP);
#else
  cpcellsd(ASP, OldASP, (CELL *)OldTR - OldASP);
#endif
}

static void
MoveGlobal(void)
{
  /*
   * cpcellsd(To,From,NOfCells) - copy the cells downwards - in
   * absmi.asm 
   */
  cpcellsd((CELL *)Yap_GlobalBase, (CELL *)OldGlobalBase, OldH - (CELL *)OldGlobalBase);  
}

static void
MoveExpandedGlobal(void)
{
  /*
   * cpcellsd(To,From,NOfCells) - copy the cells downwards - in
   * absmi.asm 
   */
  cpcellsd((CELL *)(Yap_GlobalBase+(GDiff-BaseDiff)), (CELL *)Yap_GlobalBase, OldH - (CELL *)OldGlobalBase);  
}

static void
MoveGlobalWithHole(void)
{
  /*
   * cpcellsd(To,From,NOfCells) - copy the cells downwards - in
   * absmi.asm 
   */
#if USE_SYSTEM_MALLOC
  cpcellsd((CELL *)((char *)Yap_GlobalBase+(GDiff0-BaseDiff)), (CELL *)Yap_GlobalBase, OldH - (CELL *)OldGlobalBase);  
#else
  cpcellsd((CELL *)((char *)OldGlobalBase+GDiff0), (CELL *)OldGlobalBase, OldH - (CELL *)OldGlobalBase);  
#endif
}

static void
MoveHalfGlobal(CELL *OldPt)
{
	/*
	 * cpcellsd(To,From,NOfCells) - copy the cells downwards - in
	 * absmi.asm 
	 */
  UInt diff = OldH-OldPt;
  CELL *NewPt = (CELL *)((char*)OldPt+GDiff);
  CELL *IntPt = (CELL *)((char*)OldPt+GDiff0);
  cpcellsd(NewPt, IntPt, diff);
}

static inline CELL
AdjustAppl(register CELL t0)
{
  register CELL   *t = RepAppl(t0);

  if (IsOldGlobalPtr(t))
    return (AbsAppl(PtoGloAdjust(t)));
  else if (IsOldTrailPtr(t))
    return (AbsAppl(CellPtoTRAdjust(t)));
  else if (IsHeapP(t))
    return (AbsAppl(CellPtoHeapAdjust(t)));
#ifdef DEBUG
  else {
    /* strange cell */
    /*    fprintf(Yap_stderr,"% garbage appl %lx found in stacks by stack shifter\n", t0);*/
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
  if (IsOldTrailPtr(t))
    return (AbsPair(CellPtoTRAdjust(t)));
  else if (IsHeapP(t))
    return (AbsPair(CellPtoHeapAdjust(t)));
#ifdef DEBUG
  /* fprintf(Yap_stderr,"% garbage pair %lx found in stacks by stack shifter\n", t0);*/
#endif
  return(t0);
}

static void
AdjustTrail(int adjusting_heap)
{
  volatile tr_fr_ptr ptt;

  ptt = TR;
  /* moving the trail is simple */
  while (ptt != (tr_fr_ptr)Yap_TrailBase) {
    register CELL reg = TrailTerm(ptt-1);
#ifdef FROZEN_STACKS
    register CELL reg2 = TrailVal(ptt-1);
#endif

    ptt--;
    if (IsVarTerm(reg)) {
      if (IsOldLocalInTR(reg))
	TrailTerm(ptt) = LocalAdjust(reg);
      else if (IsOldGlobal(reg))
	TrailTerm(ptt) = GlobalAdjust(reg);
      else if (IsOldTrail(reg))
	TrailTerm(ptt) = TrailAdjust(reg);
    } else if (IsPairTerm(reg)) {
     TrailTerm(ptt) = AdjustPair(reg);
#ifdef MULTI_ASSIGNMENT_VARIABLES /* does not work with new structures */
    /* check it whether we are protecting a
       multi-assignment */
    } else if (IsApplTerm(reg)) {
      TrailTerm(ptt) = AdjustAppl(reg);
#endif
    }
#ifdef FROZEN_STACKS
    if (IsVarTerm(reg2)) {
      if (IsOldLocal(reg2))
	TrailVal(ptt) = LocalAdjust(reg2);
      else if (IsOldGlobal(reg2))
	TrailVal(ptt) = GlobalAdjust(reg2);
      else if (IsOldTrail(reg2))
	TrailVal(ptt) = TrailAdjust(reg2);
    } else if (IsApplTerm(reg2)) {
      TrailVal(ptt) = AdjustAppl(reg2);
    } else if (IsPairTerm(reg2)) {
      TrailVal(ptt) = AdjustPair(reg2);
    }
#endif
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

static Term
AdjustGlobTerm(Term reg)
{
  if (IsVarTerm(reg)) {
    if (IsOldGlobal(reg))
      return GlobalAdjust(reg);
    else if (IsOldLocal(reg))
      return LocalAdjust(reg);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    else if (IsOldTrail(reg))
      return TrailAdjust(reg);
#endif
  } else if (IsApplTerm(reg))
    return AdjustAppl(reg);
  else if (IsPairTerm(reg))
    return AdjustPair(reg);
  return AtomTermAdjust(reg);
}

static volatile CELL *cpt=NULL;

static void
AdjustGlobal(long sz)
{
  CELL *pt;
  ArrayEntry *al = DynamicArrays;
  StaticArrayEntry *sal = StaticArrays;
  GlobalEntry *gl = GlobalVariables;

  while (al) {
    al->ValueOfVE = AdjustGlobTerm(al->ValueOfVE);
    al = al->NextAE;
  }
  while (gl) {
    if (IsVarTerm(gl->global) ||
	!IsAtomOrIntTerm(gl->global)) {
      gl->global = AdjustGlobTerm(gl->global);
    }
    gl = gl->NextGE;
  }
  while (sal) {
    if (sal->ArrayType == array_of_nb_terms) {
      UInt arity = -sal->ArrayEArity, i;
      for (i=0; i < arity; i++) {
	/*	    sal->ValueOfVE.lterms[i].tlive = AdjustGlobTerm(sal->ValueOfVE.lterms[i].tlive); */
	Term tlive  = sal->ValueOfVE.lterms[i].tlive;
	if (!IsVarTerm(tlive) || !IsUnboundVar(&sal->ValueOfVE.lterms[i].tlive)) {
	    sal->ValueOfVE.lterms[i].tlive = AdjustGlobTerm(sal->ValueOfVE.lterms[i].tlive);
	}
      }
    }
    sal = sal->NextAE;
  }

  /*
   * to clean the global now that functors are just variables pointing to
   * the code 
   */
  pt = CurrentDelayTop;
  while (pt < (H-sz/CellSize)) {
    CELL reg;
    
    cpt = pt;
    reg = *pt;
    if (IsVarTerm(reg)) {
      if (IsOldGlobal(reg))
	*pt = GlobalAdjust(reg);
      else if (IsOldLocal(reg))
	*pt = LocalAdjust(reg);
      else if (IsOldCode(reg)) {
	Functor f;
	f = (Functor)reg;
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
	    Int sz = 2+
	      (sizeof(MP_INT)+
	       (((MP_INT *)(pt+2))->_mp_alloc*sizeof(mp_limb_t)))/CellSize;
	    pt += sz;
	  }
	  break;
#endif
	case (CELL)0L:
	  break;
	case (CELL)FunctorLongInt:
	  pt += 2;
	  break;
	default:
	  *pt = CodeAdjust(reg);
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
static void
AdjustStacksAndTrail(long sz)
{
  AdjustTrail(TRUE);
  AdjustLocal();
  AdjustGlobal(sz);
}

void
Yap_AdjustStacksAndTrail(void)
{
  AdjustStacksAndTrail(0);
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

static void
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

static void
AdjustVarTable(VarEntry *ves)
{
  ves->VarAdr = TermNil;
  if (ves->VarRight != NULL) {
    if (IsOldVarTableTrailPtr(ves->VarRight)) {
      ves->VarRight = (VarEntry *)TrailAddrAdjust((ADDR)(ves->VarRight));
    }
    AdjustVarTable(ves->VarRight);
  }
  if (ves->VarLeft != NULL) {
    if (IsOldVarTableTrailPtr(ves->VarLeft)) {
      ves->VarLeft = (VarEntry *)TrailAddrAdjust((ADDR)(ves->VarLeft));
    }
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
    if (IsOldTokenTrailPtr(tks)) {
      tks = *tksp = TokEntryAdjust(tks);
    }
  }
  while (tks != NULL) {
    TokEntry *tktmp;

    switch (tks->Tok) {
    case Var_tok:
    case String_tok:
      if (IsOldTrail(tks->TokInfo))
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
      if (IsOldTokenTrailPtr(tktmp)) {
	tktmp = TokEntryAdjust(tktmp);
	tks->TokNext = tktmp;
      }
    }
    tks = tktmp;
  }
  if (ves != NULL) {
    if (IsOldVarTableTrailPtr(ves))
      ves = *vep = (VarEntry *)TrailAddrAdjust((ADDR)ves);
    AdjustVarTable(ves);
  }
  ves = Yap_AnonVarTable;
  if (ves != NULL) {
    if (IsOldVarTableTrailPtr(ves))
      ves = Yap_AnonVarTable = VarEntryAdjust(ves);
  }
  while (ves != NULL) {
    VarEntry *vetmp = ves->VarLeft;
    if (vetmp != NULL) {
      if (IsOldVarTableTrailPtr(vetmp)) {
	vetmp = VarEntryAdjust(vetmp);
      }
      ves->VarLeft = vetmp;
    }
    ves->VarAdr = TermNil;
    ves = vetmp;
  }
}

void
Yap_AdjustRegs(int n)
{
  AdjustRegs(n);
}

/* Used by do_goal() when we're short of heap space */
static int
static_growheap(long size, int fix_code, struct intermediates *cip, tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  UInt start_growth_time, growth_time;
  int gc_verbose;
  UInt minimal_request = 0L;

  CurrentDelayTop = (CELL *)DelayTop();
  /* adjust to a multiple of 256) */
  if (size < YAP_ALLOC_SIZE)
    size = YAP_ALLOC_SIZE;
  size = AdjustPageSize(size);
  Yap_ErrorMessage = NULL;
  if (!Yap_ExtendWorkSpace(size)) {
    Int min_size = AdjustPageSize(((CELL)Yap_TrailTop-(CELL)Yap_GlobalBase)+MinHeapGap);

    Yap_ErrorMessage = NULL;
    if (size < min_size) size = min_size;
    minimal_request = size;
    size = Yap_ExtendWorkSpaceThroughHole(size);
    if (size < 0) {
      Yap_ErrorMessage = "Database crashed against Stacks";
      return FALSE;
    }
  }
  start_growth_time = Yap_cputime();
  gc_verbose = Yap_is_gc_verbose();
  heap_overflows++;
  if (gc_verbose) {
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% Database Overflow %d\n", heap_overflows);
    fprintf(Yap_stderr, "%%   growing the heap %ld bytes\n", size);
  }
  /* CreepFlag is set to force heap expansion */
  if (ActiveSignals == YAP_CDOVF_SIGNAL) {
    LOCK(SignalLock);
    CreepFlag = CalculateStackGap();
    UNLOCK(SignalLock);
  }
  ASP -= 256;
  YAPEnterCriticalSection();
  TrDiff = LDiff = GDiff = GDiff0 = DelayDiff = BaseDiff = size;
  XDiff = HDiff = 0;
  GSplit = NULL;
  SetHeapRegs();
  MoveLocalAndTrail();
  if (fix_code) {
    CELL *SaveOldH = OldH;
    OldH = (CELL *)cip->freep;
    MoveGlobal();
    OldH = SaveOldH;
  } else {
    MoveGlobal();
  }
  if (old_trp) {
    tr_fr_ptr nTR;

    AdjustScannerStacks(tksp, vep);
    nTR = TR;
    *old_trp = PtoTRAdjust(*old_trp);
    TR = *old_trp;
    AdjustStacksAndTrail(0);
    TR = nTR;
  } else {
    AdjustStacksAndTrail(0);
  }
  AdjustRegs(MaxTemps);
  YAPLeaveCriticalSection();
  ASP += 256;
  if (minimal_request) 
    Yap_AllocHole(minimal_request, size);
  growth_time = Yap_cputime()-start_growth_time;
  total_heap_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding Database\n", (double)total_heap_overflow_time/1000);
  }
  return(TRUE);
}

/* Used when we're short of heap, usually because of an overflow in
   the attributed stack, but also because we allocated a zone  */
static int
static_growglobal(long request, CELL **ptr, CELL *hsplit)
{
  UInt start_growth_time, growth_time;
  int gc_verbose;
  char *omax = (ADDR)DelayTop();
  ADDR old_GlobalBase = Yap_GlobalBase;
  UInt minimal_request = 0L;
  long size = request;
  char vb_msg1 = '\0', *vb_msg2;
  int do_grow = TRUE;
  int insert_in_delays = FALSE;
  /*
    request is the amount of memory we requested, in bytes;
    base_move is the shift in global stacks we had to do
    size is how much space we allocate: it's negative if we just expand
	the delay stack.
    do_grow is whether we expand stacks
  */

  CurrentDelayTop = (CELL *)omax;
  if (hsplit) {
    /* just a little bit of sanity checking */
    if (hsplit < H0 && hsplit > (CELL *)Yap_GlobalBase) {
      insert_in_delays = TRUE;
      /* expanding attributed variables */
      if (omax - size > Yap_GlobalBase+4096*sizeof(CELL)) {
	/* we can just ask for more room */
	size = 0;
	do_grow = FALSE;
      }
    } else if (hsplit < (CELL*)omax ||
	hsplit > H)
      return FALSE;
    else if (hsplit == (CELL *)omax)
      hsplit = NULL;
    if (size < 0 ||
	(Unsigned(H)+size < Unsigned(ASP)-CreepFlag &&
	 hsplit > H0)) {
      /* don't need to expand stacks */
      insert_in_delays = FALSE;
      do_grow = FALSE;
    }
  } else {
    if (Unsigned(H)+size < Unsigned(ASP)-CreepFlag) {
      /* we can just ask for more room */
      do_grow = FALSE;
    }    
  }
  if (do_grow) {
    if (size < YAP_ALLOC_SIZE)
      size = YAP_ALLOC_SIZE;
    size = AdjustPageSize(size);
  }
  /* adjust to a multiple of 256) */
  Yap_ErrorMessage = NULL;
  Yap_PrologMode |= GrowStackMode;
  start_growth_time = Yap_cputime();
  if (do_grow) {
    if (!Yap_AllowGlobalExpansion) {
      Yap_ErrorMessage = "Global Stack crashed against Local Stack";
      LeaveGrowMode(GrowStackMode);
      return 0;
    }
    if (!Yap_AllowGlobalExpansion || !Yap_ExtendWorkSpace(size)) {
      /* always fails when using malloc */
      Yap_ErrorMessage = NULL;
      size += AdjustPageSize(((CELL)Yap_TrailTop-(CELL)Yap_GlobalBase)+MinHeapGap);   
      minimal_request = size;
      size = Yap_ExtendWorkSpaceThroughHole(size);
      if (size < 0) {
	Yap_ErrorMessage = "Global Stack crashed against Local Stack";
	LeaveGrowMode(GrowStackMode);
	return 0;
      }
    }
  }
  gc_verbose = Yap_is_gc_verbose();
  delay_overflows++;
  if (gc_verbose) {
    if (hsplit) {
      if (hsplit > H0) {
	vb_msg1 = 'H';
	vb_msg2 = "Global Variable Space";
      } else {
	vb_msg1 = 'D';
	vb_msg2 = "Global Variable Delay Space";
      }
    } else {
      vb_msg1 = 'D';
      vb_msg2 = "Delay";
    }
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% %cO %s Overflow %d\n", vb_msg1, vb_msg2, delay_overflows);
    fprintf(Yap_stderr, "%% %cO   growing the stacks %ld bytes\n", vb_msg1, size);
  }
  ASP -= 256;
  YAPEnterCriticalSection();
  /* we always shift the local and the stack by the same amount */
  if (do_grow) {
    /* we got over a hole */
    if (minimal_request) {
      /* we went over a hole */
      BaseDiff = size+((CELL)Yap_TrailTop-(CELL)Yap_GlobalBase)-minimal_request;
      LDiff = TrDiff = size;
    } else {
      /* we may still have an overflow */
      BaseDiff = Yap_GlobalBase - old_GlobalBase;
      /* if we grow, we need to move the stacks */
      LDiff = TrDiff = BaseDiff+size;
    }
  } else {
    /* stay still */
    LDiff = TrDiff = 0;
    BaseDiff = 0;
  }
  /* now, remember we have delay -- global with a hole in delay or a 
     hole in global */
  if (!hsplit) {
    if (!do_grow) {
      DelayDiff = GDiff = GDiff0 = size;
      request = 0L;
    } else {
      /* expand delay stack */
      DelayDiff = GDiff = GDiff0 = LDiff;
    }
  } else if (insert_in_delays) {
    /* we want to expand a hole for the delay stack */
    DelayDiff = size-request;
    GDiff = GDiff0 = size;
  } else {
    /* we want to expand a hole for the delay stack */
    GDiff0 = DelayDiff = BaseDiff;
    GDiff = BaseDiff+request;
  }
  GSplit = hsplit;
  XDiff = HDiff = 0;
  Yap_GlobalBase = old_GlobalBase;
  SetHeapRegs();
  if (do_grow) {
    MoveLocalAndTrail();
    if (hsplit) {
      MoveGlobalWithHole();
    } else {
      MoveExpandedGlobal();
    }
  } else if (!hsplit) {
    MoveExpandedGlobal();
  }
  /* don't run through garbage */
  if (hsplit && (OldH != hsplit)) {
    AdjustStacksAndTrail(request);
  } else {
    AdjustStacksAndTrail(0);
  }
  AdjustRegs(MaxTemps);
  if (ptr) {
    *ptr = PtoLocAdjust(*ptr);
  }
  if (hsplit) {
    if (insert_in_delays) {
      /* we have things not quite where we want to have them */
      cpcellsd((CELL *)(omax+DelayDiff), (CELL *)(omax+GDiff0), (ADDR)hsplit-omax);
    } else {
      MoveHalfGlobal(hsplit);
    }
  }
  YAPLeaveCriticalSection();
  ASP += 256;
  if (minimal_request) {
    Yap_AllocHole(minimal_request, size);
  }
  growth_time = Yap_cputime()-start_growth_time;
  total_delay_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% %cO   took %g sec\n", vb_msg1, (double)growth_time/1000);
    fprintf(Yap_stderr, "%% %cO Total of %g sec expanding stacks \n", vb_msg1, (double)total_delay_overflow_time/1000);
  }
  LeaveGrowMode(GrowStackMode);
  if (hsplit) {
    return request;
  } else
    return GDiff-BaseDiff;
}

static void
fix_compiler_instructions(PInstr *pcpc)
{
  while (pcpc != NULL) {
    PInstr *ncpc = pcpc->nextInst;

    switch(pcpc->op) {
      /* check c_var for functions that point at variables */
    case get_var_op:
    case get_val_op:
    case unify_var_op:
    case unify_last_var_op:
    case unify_val_op:
    case unify_local_op:
    case unify_last_val_op:
    case unify_last_local_op:
    case put_var_op:
    case put_val_op:
    case put_unsafe_op:
    case write_unsafe_op:
    case write_var_op:
    case write_val_op:
    case write_local_op:
    case f_var_op:
    case f_val_op:
    case fetch_args_for_bccall:
    case bccall_op:
    case save_pair_op:
    case save_appl_op:
    case save_b_op:
    case commit_b_op:
    case fetch_args_vv_op:
    case fetch_args_cv_op:
    case fetch_args_vc_op:
      pcpc->rnd1 = GlobalAdjust(pcpc->rnd1);
      break;
    case get_float_op:
    case put_float_op:
    case get_longint_op:
    case put_longint_op:
    case unify_float_op:
    case unify_last_float_op:
    case write_float_op:
      /* floats might be in the global */
      pcpc->rnd1 = AdjustAppl(pcpc->rnd1);
      break;
      /* hopefully nothing to do */
    case nop_op:
    case get_atom_op:
    case put_atom_op:
    case get_num_op:
    case put_num_op:
    case align_float_op:
    case get_bigint_op:
    case put_bigint_op:
    case get_dbterm_op:
    case put_dbterm_op:
    case get_list_op:
    case put_list_op:
    case get_struct_op:
    case put_struct_op:
    case unify_atom_op:
    case unify_last_atom_op:
    case write_atom_op:
    case unify_num_op:
    case unify_last_num_op:
    case write_num_op:
    case unify_longint_op:
    case unify_last_longint_op:
    case write_longint_op:
    case unify_bigint_op:
    case unify_last_bigint_op:
    case unify_dbterm_op:
    case unify_last_dbterm_op:
    case write_bigint_op:
    case write_dbterm_op:
    case unify_list_op:
    case write_list_op:
    case unify_struct_op:
    case write_struct_op:
    case fail_op:
    case cut_op:
    case cutexit_op:
    case allocate_op:
    case deallocate_op:
    case tryme_op:
    case jump_op:
    case jumpi_op:
    case procceed_op:
    case call_op:
    case execute_op:
    case safe_call_op:
    case label_op:
    case name_op:
    case pop_op:
    case retryme_op:
    case trustme_op:
    case either_op:
    case orelse_op:
    case orlast_op:
    case push_or_op:
    case pushpop_or_op:
    case pop_or_op:
    case patch_b_op:
    case try_op:
    case retry_op:
    case trust_op:
    case try_in_op:
    case jump_v_op:
    case jump_nv_op:
    case cache_arg_op:
    case cache_sub_arg_op:
    case user_switch_op:
    case switch_on_type_op:
    case switch_c_op:
    case if_c_op:
    case switch_f_op:
    case if_f_op:
    case if_not_op:
    case index_dbref_op:
    case index_blob_op:
    case index_long_op:
    case if_nonvar_op:
    case unify_last_list_op:
    case write_last_list_op:
    case unify_last_struct_op:
    case write_last_struct_op:
    case mark_initialised_pvars_op:
    case mark_live_regs_op:
    case enter_profiling_op:
    case retry_profiled_op:
    case count_call_op:
    case count_retry_op:
    case restore_tmps_op:
    case restore_tmps_and_skip_op:
    case enter_lu_op:
    case empty_call_op:
    case blob_op:
    case fetch_args_vi_op:
    case fetch_args_iv_op:
    case label_ctl_op:
    case f_0_op:
    case native_op:
#ifdef TABLING
    case table_new_answer_op:
    case table_try_single_op:
#endif /* TABLING */
#ifdef YAPOR
    case sync_op:
#endif
#ifdef BEAM
    case run_op:
    case body_op:
    case endgoal_op:
    case try_me_op:
    case retry_me_op:
    case trust_me_op:
    case only_1_clause_op:
    case create_first_box_op:
    case create_box_op:
    case create_last_box_op:
    case remove_box_op:
    case remove_last_box_op:
    case prepare_tries:
    case std_base_op:
    case direct_safe_call_op:
    case commit_op:
    case skip_while_var_op:
    case wait_while_var_op:
    case force_wait_op:
    case write_op:
    case is_op:
    case equal_op:
    case exit_op:
#endif
      break;
    }
    if (ncpc != NULL) {
      ncpc = (PInstr *)GlobalAddrAdjust((ADDR)(pcpc->nextInst));
      pcpc->nextInst = ncpc;
    }
    pcpc = ncpc;
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
  while (df) {
    if (DepFr_backchain_cp(df))
      DepFr_backchain_cp(df) = ChoicePtrAdjust(DepFr_backchain_cp(df));
    DepFr_leader_cp(df) = ChoicePtrAdjust(DepFr_leader_cp(df));
    DepFr_cons_cp(df) = ConsumerChoicePtrAdjust(DepFr_cons_cp(df));
    df = DepFr_next(df);
  }
  sg = LOCAL_top_sg_fr;
  while (sg) {
    SgFr_gen_cp(sg) = GeneratorChoicePtrAdjust(SgFr_gen_cp(sg));
    sg = SgFr_next(sg);
  }
}
#endif /* TABLING */

static int
do_growheap(int fix_code, UInt in_size, struct intermediates *cip, tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  unsigned long size = sizeof(CELL) * 16 * 1024L;
  int shift_factor = (heap_overflows > 8 ? 8 : heap_overflows);
  unsigned long sz =  size << shift_factor;

  if (sz < in_size) {
    sz = in_size;
  }
#if YAPOR
  Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"cannot grow Heap: more than a worker/thread running");
  return FALSE;
#endif
  if (SizeOfOverflow > sz) {
    if (size < YAP_ALLOC_SIZE)
      size = YAP_ALLOC_SIZE;
    sz = AdjustPageSize(SizeOfOverflow);
  }
  while(sz >= sizeof(CELL) * 16 * 1024L && !static_growheap(sz, fix_code, cip, old_trp, tksp, vep)) {
    size = size/2;
    sz =  size << shift_factor;
    if (sz < in_size) {
      return FALSE;
    }
  }
  /* we must fix an instruction chain */
  if (fix_code) {
    PInstr *pcpc = cip->CodeStart;
    if (pcpc != NULL) {
      cip->CodeStart = pcpc = (PInstr *)GlobalAddrAdjust((ADDR)pcpc);
    }
    fix_compiler_instructions(pcpc);
    pcpc = cip->BlobsStart;
    if (pcpc != NULL) {
      cip->BlobsStart = pcpc = (PInstr *)GlobalAddrAdjust((ADDR)pcpc);
    }
    fix_compiler_instructions(pcpc);
    cip->freep = (char *)GlobalAddrAdjust((ADDR)cip->freep);
    cip->label_offset = (int *)GlobalAddrAdjust((ADDR)cip->label_offset);
  }
#ifdef TABLING
  fix_tabling_info();
#endif /* TABLING */
  if (sz >= sizeof(CELL) * 16 * 1024L) {
    LOCK(SignalLock);
    ActiveSignals &= ~YAP_CDOVF_SIGNAL;
    if (!ActiveSignals)
	CreepFlag = CalculateStackGap();
    UNLOCK(SignalLock);
    return TRUE;
  }
  /* failed */
  return FALSE;
}

static void
init_new_table(AtomHashEntry *ntb, UInt nsize)
{
  UInt i;

  for (i = 0; i < nsize; ++i) {
    INIT_RWLOCK(ntb[i].AERWLock);
    ntb[i].Entry = NIL;
  }
}

static void
cp_atom_table(AtomHashEntry *ntb, UInt nsize)
{
  UInt i;

  for (i = 0; i < AtomHashTableSize; i++) {
    Atom            catom;

    READ_LOCK(HashChain[i].AERWLock);
    catom = HashChain[i].Entry;
    while (catom != NIL) {
      AtomEntry *ap = RepAtom(catom);
      Atom natom;
      CELL hash;

      hash = HashFunction((unsigned char *)ap->StrOfAE) % nsize;
      natom = ap->NextOfAE;
      ap->NextOfAE = ntb[hash].Entry;
      ntb[hash].Entry = catom;
      catom = natom;
    }
    READ_UNLOCK(HashChain[i].AERWLock);
  }
}

static int
growatomtable(void)
{
  AtomHashEntry *ntb;
  UInt nsize = 4*AtomHashTableSize-1;
  UInt start_growth_time = Yap_cputime(), growth_time;
  int gc_verbose = Yap_is_gc_verbose();

  LOCK(SignalLock);
  if (ActiveSignals == YAP_CDOVF_SIGNAL) {
    CreepFlag = CalculateStackGap();
  }
  ActiveSignals &= ~YAP_CDOVF_SIGNAL;
  UNLOCK(SignalLock);
  while ((ntb = (AtomHashEntry *)Yap_AllocCodeSpace(nsize*sizeof(AtomHashEntry))) == NULL) {
    /* leave for next time */
#if !USE_SYSTEM_MALLOC
    if (!do_growheap(FALSE, nsize*sizeof(AtomHashEntry), NULL, NULL, NULL, NULL))
#endif
      return FALSE;
  }
  atom_table_overflows++;
  if (gc_verbose) {
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% Atom Table Overflow %d\n", atom_table_overflows);
    fprintf(Yap_stderr, "%%    growing the atom table to %ld entries\n", (long int)(nsize));
  }
  YAPEnterCriticalSection();
  init_new_table(ntb, nsize);
  cp_atom_table(ntb, nsize);
  Yap_FreeCodeSpace((char *)HashChain);
  HashChain = ntb;
  AtomHashTableSize = nsize;
  YAPLeaveCriticalSection();
  growth_time = Yap_cputime()-start_growth_time;
  total_atom_table_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding atom table \n", (double)total_atom_table_overflow_time/1000);
  }
#if USE_SYSTEM_MALLOC
  return TRUE;
#else
  if (HeapTop + sizeof(YAP_SEG_SIZE)  > HeapLim - MinHeapGap) {
    /* make sure there is no heap overflow */
    int res;
    YAPEnterCriticalSection();
    res = do_growheap(FALSE, 0, NULL, NULL, NULL, NULL);
    YAPLeaveCriticalSection();
    return res;
  } else {
    return TRUE;
  }
#endif
}


int
Yap_growheap(int fix_code, UInt in_size, void *cip)
{
  int res;

  if (NOfAtoms > 2*AtomHashTableSize) {
    UInt n = NOfAtoms;
    if (AGcThreshold)
      Yap_atom_gc();
    /* check if we have a significant improvement from agc */
    if (n > NOfAtoms+ NOfAtoms/10 ||
	NOfAtoms > 2*AtomHashTableSize) {
      res  = growatomtable();
    } else {
      LOCK(SignalLock);
      if (ActiveSignals == YAP_CDOVF_SIGNAL) {
	CreepFlag = CalculateStackGap();
      }
      ActiveSignals &= ~YAP_CDOVF_SIGNAL;
      UNLOCK(SignalLock);
      return TRUE;
    }
    LeaveGrowMode(GrowHeapMode);
    return res;
  }
  res=do_growheap(fix_code, in_size, (struct intermediates *)cip, NULL, NULL, NULL);
  LeaveGrowMode(GrowHeapMode);
  return res;
}

int
Yap_growheap_in_parser(tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  int res;

  res=do_growheap(FALSE, 0L, NULL, old_trp, tksp, vep);
  LeaveGrowMode(GrowHeapMode);
  return res;
}

int
Yap_growglobal(CELL **ptr)
{
  unsigned long sz = sizeof(CELL) * 16 * 1024L;

#if defined(YAPOR) || defined(THREADS)
  if (NOfThreads != 1) {
    Yap_Error(OUT_OF_STACK_ERROR,TermNil,"cannot grow Global: more than a worker/thread running");
    return(FALSE);
  }
#endif
  if ( static_growglobal(sz, ptr, NULL) == 0)
    return FALSE;
#ifdef TABLING
  fix_tabling_info();
#endif /* TABLING */
  return TRUE;
}


UInt
Yap_InsertInGlobal(CELL *where, UInt howmuch)
{
  if ((howmuch = static_growglobal(howmuch, NULL, where)) == 0)
    return 0;
#ifdef TABLING
  fix_tabling_info();
#endif /* TABLING */
  return howmuch;
}


int
Yap_growstack(long size)
{
  int res;

  Yap_PrologMode |= GrowStackMode;
  res=growstack(size);
  LeaveGrowMode(GrowStackMode);
  return res;
}

static int
execute_growstack(long size0, int from_trail, int in_parser, tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  UInt minimal_request = 0L;
  long size = size0;
  ADDR old_Yap_GlobalBase = Yap_GlobalBase;
  
  CurrentDelayTop = (CELL *)DelayTop();
  if (!Yap_AllowGlobalExpansion) {
    Yap_ErrorMessage = "Database crashed against stacks";
    return FALSE;
  }
  if (!Yap_ExtendWorkSpace(size)) {
    /* make sure stacks and trail are contiguous */

    Yap_ErrorMessage = NULL;    
    minimal_request = AdjustPageSize(((CELL)Yap_TrailTop-(CELL)Yap_GlobalBase)+4*MinHeapGap+size0);

    size = Yap_ExtendWorkSpaceThroughHole(minimal_request);
    if (size < 0) {
      Yap_ErrorMessage = "Database crashed against stacks";
      return FALSE;
    }
    YAPEnterCriticalSection();
    GDiff = DelayDiff = BaseDiff = size-size0;
  } else {
    YAPEnterCriticalSection();
    if (Yap_GlobalBase != old_Yap_GlobalBase) {
      GDiff = BaseDiff = DelayDiff = Yap_GlobalBase-old_Yap_GlobalBase;
      Yap_GlobalBase=old_Yap_GlobalBase;
    } else {
      GDiff = BaseDiff = DelayDiff = 0;
    }
  }
  XDiff = HDiff = 0;
  GDiff0=GDiff;
#if USE_SYSTEM_MALLOC
  if (from_trail) {
    TrDiff = LDiff = GDiff;
  } else {
    TrDiff = LDiff = size+GDiff;
  }
#else
  if (from_trail) {
    TrDiff = LDiff = size-size0;
  } else {
    TrDiff = LDiff = size;
  }
#endif
  ASP -= 256;
  SetHeapRegs();
  if (from_trail) {
    Yap_TrailTop += size0;
  }
  if (LDiff) {
    MoveLocalAndTrail();
  }
  if (GDiff) {
#if !USE_SYSTEM_MALLOC
    /* That is done by realloc */
    MoveGlobal();
#endif
    if (in_parser) {
      tr_fr_ptr nTR;

      AdjustScannerStacks(tksp, vep);
      nTR = TR;
      *old_trp = PtoTRAdjust(*old_trp);
      TR = *old_trp;
      AdjustStacksAndTrail(0);
      TR = nTR;
    } else {
      AdjustStacksAndTrail(0);
    }
    AdjustRegs(MaxTemps);
#ifdef TABLING
    fix_tabling_info();
#endif /* TABLING */
  } else if (LDiff) {
    if (in_parser) {
      tr_fr_ptr nTR;

      AdjustScannerStacks(tksp, vep);
      nTR = TR;
      *old_trp = PtoTRAdjust(*old_trp);
      TR = *old_trp;
      AdjustGrowStack();
      TR = nTR;
    } else {
      AdjustGrowStack();
    }
    AdjustRegs(MaxTemps);
#ifdef TABLING
    fix_tabling_info();
#endif /* TABLING */
  }
  YAPLeaveCriticalSection();
  ASP += 256;
  if (minimal_request) 
    Yap_AllocHole(minimal_request, size);
  return TRUE;
}

/* Used by do_goal() when we're short of stack space */
static int
growstack(long size)
{
  UInt start_growth_time, growth_time;
  int gc_verbose;

  /* adjust to a multiple of 256) */
  if (size < YAP_ALLOC_SIZE)
    size = YAP_ALLOC_SIZE;
  size = AdjustPageSize(size);
  Yap_ErrorMessage = NULL;
  start_growth_time = Yap_cputime();
  gc_verbose = Yap_is_gc_verbose();
  stack_overflows++;
  if (gc_verbose) {
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% Stack Overflow %d\n", stack_overflows);
    fprintf(Yap_stderr, "%%   Global: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)Yap_GlobalBase),Yap_GlobalBase,H);
    fprintf(Yap_stderr, "%%   Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%   Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
    fprintf(Yap_stderr, "%% Growing the stacks %ld bytes\n", size);
  }
  if (!execute_growstack(size, FALSE, FALSE, NULL, NULL, NULL))
    return FALSE;
  growth_time = Yap_cputime()-start_growth_time;
  total_stack_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  return TRUE;
}

/* Used by parser when we're short of stack space */
int
Yap_growstack_in_parser(tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  UInt size;
  UInt start_growth_time, growth_time;
  int gc_verbose;

  Yap_PrologMode |= GrowStackMode;
  /* adjust to a multiple of 256) */
  size = AdjustPageSize((ADDR)LCL0-Yap_GlobalBase);
  Yap_ErrorMessage = NULL;
  start_growth_time = Yap_cputime();
  gc_verbose = Yap_is_gc_verbose();
  stack_overflows++;
  if (gc_verbose) {
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% Stack Overflow %d\n", stack_overflows);
    fprintf(Yap_stderr, "%%   Global: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)Yap_GlobalBase),Yap_GlobalBase,H);
    fprintf(Yap_stderr, "%%   Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%   Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
    fprintf(Yap_stderr, "%% Growing the stacks %ld bytes\n", (unsigned long int)size);
  }
  if (!execute_growstack(size, FALSE, TRUE, old_trp, tksp, vep)) {
    LeaveGrowMode(GrowStackMode);
    return FALSE;
  }
  growth_time = Yap_cputime()-start_growth_time;
  total_stack_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  LeaveGrowMode(GrowStackMode);
  return TRUE;
}

static int do_growtrail(long size, int contiguous_only, int in_parser, tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  UInt start_growth_time = Yap_cputime(), growth_time;
  int gc_verbose = Yap_is_gc_verbose();

#if USE_SYSTEM_MALLOC
  if (contiguous_only)
    return FALSE;
#endif
  /* at least 64K for trail */
  if (!size)
    size = ((ADDR)TR-Yap_TrailBase);
  size *= 2;
  if (size < YAP_ALLOC_SIZE)
    size = YAP_ALLOC_SIZE;
  if (size > 2048*1024)
    size = 2048*1024;
  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  trail_overflows++;
  if (gc_verbose) {
#if  defined(YAPOR) || defined(THREADS)
    fprintf(Yap_stderr, "%% Worker Id %d:\n", worker_id);
#endif
    fprintf(Yap_stderr, "%% Trail Overflow %d\n", trail_overflows);
#if USE_SYSTEM_MALLOC
    fprintf(Yap_stderr, "%%  Heap: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)Yap_GlobalBase),(CELL *)Yap_GlobalBase,H);
    fprintf(Yap_stderr, "%%  Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%  Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
#endif
    fprintf(Yap_stderr, "%% growing the trail %ld bytes\n", size);
  }
  Yap_ErrorMessage = NULL;
  if (!Yap_AllowTrailExpansion) {
    Yap_ErrorMessage = "Trail Overflow";
    return FALSE;
  }
#if USE_SYSTEM_MALLOC
  execute_growstack(size, TRUE, in_parser, old_trp, tksp, vep);
#else
  if (!Yap_ExtendWorkSpace(size)) {
    Yap_ErrorMessage = NULL;
    if (contiguous_only) {
      /* I can't expand in this case */
      trail_overflows--;
      return FALSE;
    }
    execute_growstack(size, TRUE, in_parser, old_trp, tksp, vep);
  } else {
    YAPEnterCriticalSection();
    if (in_parser) {
      TrDiff = LDiff = GDiff = BaseDiff = DelayDiff = XDiff = HDiff = GDiff0 = 0;
      AdjustScannerStacks(tksp, vep);
    }
    Yap_TrailTop += size;
    YAPLeaveCriticalSection();
  }
#endif
  growth_time = Yap_cputime()-start_growth_time;
  total_trail_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%  took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding trail \n", (double)total_trail_overflow_time/1000);
  }
  LOCK(SignalLock);
  if (ActiveSignals == YAP_TROVF_SIGNAL) {
    CreepFlag = CalculateStackGap();
  }
  ActiveSignals &= ~YAP_TROVF_SIGNAL;
  UNLOCK(SignalLock);
  return TRUE;
}


/* Used by do_goal() when we're short of stack space */
int
Yap_growtrail(long size, int contiguous_only)
{ 
 return do_growtrail(size, contiguous_only, FALSE, NULL, NULL, NULL);
}

int
Yap_growtrail_in_parser(tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  return do_growtrail(0, FALSE, TRUE, old_trp, tksp, vep);
}

CELL **
Yap_shift_visit(CELL **to_visit, CELL ***to_visit_maxp)
{
  CELL **to_visit_max = *to_visit_maxp;
  /* relative position of top of stack */
  Int off = (ADDR)to_visit-AuxBase;
  /* how much space the top stack was using */
  Int sz = AuxTop - (ADDR)to_visit_max;
  /* how much space the bottom stack was using */
  Int szlow = (ADDR)to_visit_max-AuxBase;
  /* original size for AuxSpace */
  Int totalsz0 = AuxTop - AuxBase; /* totalsz0 == szlow+sz */
  /* new size for AuxSpace */
  Int totalsz;
  /* how much we grow */
  Int dsz; /* totalsz == szlow+dsz+sz */
  char *newb = Yap_ExpandPreAllocCodeSpace(0, NULL, FALSE);

  if (newb == NULL) {
    Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"cannot allocate temporary space for unification (%p)", to_visit);       
    return to_visit;
  }
  /* check new size */
  totalsz  = AuxTop-AuxBase;
  /* how much we grew */
  dsz = totalsz-totalsz0;
  if (dsz == 0) {
    Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"cannot allocate temporary space for unification (%p)", to_visit);       
    return to_visit;
  }
  /* copy whole block to end */
  cpcellsd((CELL *)(newb+(dsz+szlow)), (CELL *)(newb+szlow), sz/sizeof(CELL));
  /* base pointer is block start */
  *to_visit_maxp = (CELL **)(newb+szlow);
  /* current top is originall diff + diff size */
  return (CELL **)(newb+(off+dsz));
}

static Int
p_inform_trail_overflows(void)
{
  Term tn = MkIntTerm(trail_overflows);
  Term tt = MkIntegerTerm(total_trail_overflow_time);
 
  return(Yap_unify(tn, ARG1) && Yap_unify(tt, ARG2));
}

/* :- grow_heap(Size) */
static Int
p_growheap(void)
{
  Int             diff;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "grow_heap/1");
    return(FALSE);
  } else if (!IsIntTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "grow_heap/1");
    return(FALSE);
  }
  diff = IntOfTerm(t1);
  if (diff < 0) {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t1, "grow_heap/1");
  }
  return(static_growheap(diff, FALSE, NULL, NULL, NULL, NULL));
}

static Int
p_inform_heap_overflows(void)
{
  Term tn = MkIntTerm(heap_overflows);
  Term tt = MkIntegerTerm(total_heap_overflow_time);
 
  return(Yap_unify(tn, ARG1) && Yap_unify(tt, ARG2));
}

/* :- grow_stack(Size) */
static Int
p_growstack(void)
{
  Int             diff;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "grow_stack/1");
    return(FALSE);
  } else if (!IsIntTerm(t1)) {
    Yap_Error(TYPE_ERROR_INTEGER, t1, "grow_stack/1");
    return(FALSE);
  }
  diff = IntOfTerm(t1);
  if (diff < 0) {
    Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t1, "grow_stack/1");
  }
  return(growstack(diff));
}

static Int
p_inform_stack_overflows(void)
{				/*  */
  Term tn = MkIntTerm(stack_overflows);
  Term tt = MkIntegerTerm(total_stack_overflow_time);
 
  return(Yap_unify(tn, ARG1) && Yap_unify(tt, ARG2));

}

Int
Yap_total_stack_shift_time(void)
{
  return(total_heap_overflow_time+
	 total_stack_overflow_time+
	 total_trail_overflow_time);
}

void
Yap_InitGrowPreds(void)
{
  Yap_InitCPred("$grow_heap", 1, p_growheap, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$grow_stack", 1, p_growstack, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$inform_trail_overflows", 2, p_inform_trail_overflows, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$inform_heap_overflows", 2, p_inform_heap_overflows, SafePredFlag|HiddenPredFlag);
  Yap_InitCPred("$inform_stack_overflows", 2, p_inform_stack_overflows, SafePredFlag|HiddenPredFlag);
  Yap_init_gc();
  Yap_init_agc();
}
