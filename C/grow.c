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

#if !HAVE_STRNCAT
#define strncat(s0,s1,sz)   strcat(s0,s1)
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
STATIC_PROTO(void SetStackRegs, (void));
STATIC_PROTO(void AdjustTrail, (int));
STATIC_PROTO(void AdjustLocal, (void));
STATIC_PROTO(void AdjustGlobal, (void));
STATIC_PROTO(void AdjustGrowStack, (void));
STATIC_PROTO(int  static_growheap, (long,int,struct intermediates *));
STATIC_PROTO(void cpcellsd, (CELL *, CELL *, CELL));
STATIC_PROTO(CELL AdjustAppl, (CELL));
STATIC_PROTO(CELL AdjustPair, (CELL));
STATIC_PROTO(void AdjustStacksAndTrail, (void));
STATIC_PROTO(void AdjustRegs, (int));

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
  /* Adjust stack addresses */
  Yap_TrailBase = TrailAddrAdjust(Yap_TrailBase);
  Yap_TrailTop = TrailAddrAdjust(Yap_TrailTop);
  Yap_GlobalBase = DelayAddrAdjust(Yap_GlobalBase);
  Yap_LocalBase = LocalAddrAdjust(Yap_LocalBase);
#if !USE_SYSTEM_MALLOC && !USE_DL_MALLOC
  AuxSp = PtoDelayAdjust(AuxSp);
  AuxTop = (ADDR)PtoDelayAdjust((CELL *)AuxTop);
#endif
  if (HeapLim)
    HeapLim = DelayAddrAdjust(HeapLim);
  /* The registers pointing to one of the stacks */
  if (ENV)
    ENV = PtoLocAdjust(ENV);
  if (ASP)
    ASP = PtoLocAdjust(ASP);
  if (H0)
    H0 = PtoGloAdjust(H0);
  if (LCL0)
    LCL0 = PtoLocAdjust(LCL0);
  if (H)
    H = PtoGloAdjust(H);
  if (HB)
    HB = PtoGloAdjust(HB);
  if (B)
    B = ChoicePtrAdjust(B);
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
#ifdef COROUTINING
  if (DelayedVars)
    DelayedVars = AbsAppl(PtoGloAdjust(RepAppl(DelayedVars)));
  if (MutableList)
    MutableList = AbsAppl(PtoGloAdjust(RepAppl(MutableList)));
  if (AttsMutableList)
    AttsMutableList = AbsAppl(PtoGloAdjust(RepAppl(AttsMutableList)));
  if (WokenGoals)
    WokenGoals = AbsAppl(PtoGloAdjust(RepAppl(WokenGoals)));
#endif
}

static void
SetStackRegs(void)
{
  /* The old local stack pointers */
  OldLCL0 = LCL0;
  OldASP = ASP;
  OldH = H;
  OldH0 = H0;
  OldGlobalBase = (CELL *)Yap_GlobalBase;
  OldTrailTop = Yap_TrailTop;
  OldTrailBase = Yap_TrailBase;
  OldTR = TR;
  OldHeapBase = Yap_HeapBase;
  OldHeapTop = HeapTop;
  /* The local and aux stack addresses */
  Yap_TrailBase = TrailAddrAdjust(Yap_TrailBase);
  Yap_TrailTop = TrailAddrAdjust(Yap_TrailTop);
  Yap_LocalBase = LocalAddrAdjust(Yap_LocalBase);
  TR = PtoTRAdjust(TR);
  /* The registers pointing to the local stack */
  if (ENV)
    ENV = PtoLocAdjust(ENV);
  if (ASP)
    ASP = PtoLocAdjust(ASP);
  if (LCL0)
    LCL0 = PtoLocAdjust(LCL0);
  if (B)
    B = ChoicePtrAdjust(B);
#ifdef TABLING
  if (B_FZ)
    B_FZ = ChoicePtrAdjust(B_FZ);
  if (BB)
    BB = ChoicePtrAdjust(BB);
  if (TR_FZ)
    TR_FZ = PtoTRAdjust(TR_FZ);
#endif /* TABLING */
  if (YENV)
    YENV = PtoLocAdjust(YENV);
#ifdef COROUTINING
  if (DelayedVars)
    DelayedVars = AbsAppl(PtoGloAdjust(RepAppl(DelayedVars)));
  if (MutableList)
    MutableList = AbsAppl(PtoGloAdjust(RepAppl(MutableList)));
  if (AttsMutableList)
    AttsMutableList = AbsAppl(PtoGloAdjust(RepAppl(AttsMutableList)));
  if (WokenGoals)
    WokenGoals = AbsAppl(PtoGloAdjust(RepAppl(WokenGoals)));
#endif
}

static void
MoveLocalAndTrail(void)
{
	/* cpcellsd(To,From,NOfCells) - copy the cells downwards  */
#if USE_SYSTEM_MALLOC
#if HAVE_MEMMOVE
  cpcellsd(ASP, (CELL *)((char *)OldASP+GDiff), (CELL *)OldTR - OldASP);
#else
  cpcellsd((CELL *)TR, (CELL *)((char *)OldTR+Gdiff), (CELL *)OldTR - OldASP);
#endif
#else
#if HAVE_MEMMOVE
  cpcellsd(ASP, OldASP, (CELL *)OldTR - OldASP);
#else
  cpcellsd((CELL *)TR, (CELL *)OldTR, (CELL *)OldTR - OldASP);
#endif
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
  cpcellsd((CELL *)Yap_GlobalBase, (CELL *)OldGlobalBase, OldH - (CELL *)OldGlobalBase);  
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
  if (IsOldDelayPtr(t))
    return (AbsPair(PtoDelayAdjust(t)));
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
  register tr_fr_ptr ptt;

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
	  fprintf(Yap_stderr,"%% garbage heap ptr %p to %lx found in trail at %p by stack shifter\n", ptr, (unsigned long int)*ptr, ptt);
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
#ifdef FROZEN_STACKS
    if (IsVarTerm(reg2)) {
      if (IsOldLocal(reg2))
	TrailVal(ptt) = LocalAdjust(reg2);
      else if (IsOldGlobal(reg2))
	TrailVal(ptt) = GlobalAdjust(reg2);
      else if (IsOldDelay(reg2))
	TrailVal(ptt) = DelayAdjust(reg2);
      else if (IsOldTrail(reg2))
	TrailVal(ptt) = TrailAdjust(reg2);
      else if (IsOldCode(reg2))
	TrailVal(ptt) = CodeAdjust(reg2);
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
  pt = CellPtr(Yap_GlobalBase);
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
static void
AdjustStacksAndTrail(void)
{
  AdjustTrail(TRUE);
  AdjustLocal();
  AdjustGlobal();
}

void
Yap_AdjustStacksAndTrail(void)
{
  AdjustStacksAndTrail();
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

void
Yap_AdjustRegs(int n)
{
  AdjustRegs(n);
}

/* Used by do_goal() when we're short of heap space */
static int
static_growheap(long size, int fix_code, struct intermediates *cip)
{
  UInt start_growth_time, growth_time;
  int gc_verbose;
  UInt minimal_request = 0L;

  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  Yap_ErrorMessage = NULL;
  if (!Yap_ExtendWorkSpace(size)) {
    Int min_size = AdjustPageSize(((CELL)Yap_TrailTop-(CELL)Yap_GlobalBase)+MinHeapGap);

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
    fprintf(Yap_stderr, "%% Database overflow %d\n", heap_overflows);
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
  TrDiff = LDiff = GDiff = DelayDiff = size;
  XDiff = HDiff = 0;
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
  AdjustStacksAndTrail();
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

/* Used by do_goal() when we're short of heap space */
static int
static_growglobal(long size, CELL **ptr)
{
  UInt start_growth_time, growth_time;
  int gc_verbose;

  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  Yap_ErrorMessage = NULL;
  if (!Yap_ExtendWorkSpace(size)) {
    Yap_ErrorMessage = "Global Stack crashed against Local Stack";
    return(FALSE);
  }
  start_growth_time = Yap_cputime();
  gc_verbose = Yap_is_gc_verbose();
  delay_overflows++;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% DO Delay overflow %d\n", delay_overflows);
    fprintf(Yap_stderr, "%% DO   growing the stacks %ld bytes\n", size);
  }
  ASP -= 256;
  YAPEnterCriticalSection();
  TrDiff = LDiff = GDiff = size;
  XDiff = HDiff = DelayDiff = 0;
  SetHeapRegs();
  MoveLocalAndTrail();
  MoveGlobalOnly();
  AdjustStacksAndTrail();
  AdjustRegs(MaxTemps);
  if (ptr)
    *ptr = PtoLocAdjust(*ptr);
  YAPLeaveCriticalSection();
  ASP += 256;
  growth_time = Yap_cputime()-start_growth_time;
  total_delay_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% DO   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% DO Total of %g sec expanding stacks \n", (double)total_delay_overflow_time/1000);
  }
  return(TRUE);
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
    case fetch_args_cv_op:
    case fetch_args_vc_op:
    case fetch_args_vv_op:
      pcpc->rnd1 = GlobalAdjust(pcpc->rnd1);
      break;
      /* hopefully nothing to do */
    case nop_op:
    case get_atom_op:
    case put_atom_op:
    case get_num_op:
    case put_num_op:
    case get_float_op:
    case put_float_op:
    case align_float_op:
    case get_longint_op:
    case put_longint_op:
    case get_bigint_op:
    case put_bigint_op:
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
    case unify_float_op:
    case unify_last_float_op:
    case write_float_op:
    case unify_longint_op:
    case unify_last_longint_op:
    case write_longint_op:
    case unify_bigint_op:
    case unify_last_bigint_op:
    case write_bigint_op:
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
    case switch_on_type_op:
    case switch_c_op:
    case if_c_op:
    case switch_f_op:
    case if_f_op:
    case if_not_op:
    case index_dbref_op:
    case index_blob_op:
    case if_nonvar_op:
    case commit_opt_op:
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

static int
do_growheap(int fix_code, UInt in_size, struct intermediates *cip)
{
  unsigned long size = sizeof(CELL) * 16 * 1024L;
  int shift_factor = (heap_overflows > 8 ? 8 : heap_overflows);
  unsigned long sz =  size << shift_factor;

  if (sz < in_size) {
    sz = in_size;
  }
#if YAPOR
  Yap_Error(SYSTEM_ERROR,TermNil,"cannot grow Heap: more than a worker/thread running");
  return FALSE;
#endif
  if (SizeOfOverflow > sz)
    sz = AdjustPageSize(SizeOfOverflow);
  while(sz >= sizeof(CELL) * 16 * 1024L && !static_growheap(sz, fix_code, cip)) {
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

static int
growatomtable(void)
{
  AtomHashEntry *ntb;
  UInt nsize = 4*AtomHashTableSize-1, i;
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
    if (!do_growheap(FALSE, nsize*sizeof(AtomHashEntry), NULL))
#endif
      return FALSE;
  }
  atom_table_overflows++;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% Atom Table overflow %d\n", atom_table_overflows);
    fprintf(Yap_stderr, "%%    growing the atom table to %ld entries\n", (long int)(nsize));
  }
  YAPEnterCriticalSection();
  for (i = 0; i < nsize; ++i) {
    INIT_RWLOCK(ntb[i].AERWLock);
    ntb[i].Entry = NIL;
  }
  for (i = 0; i < AtomHashTableSize; i++) {
    Atom            catom;

    READ_LOCK(HashChain[i].AERWLock);
    catom = HashChain[i].Entry;
    while (catom != NIL) {
      AtomEntry *ap = RepAtom(catom);
      Atom natom;
      CELL hash;

      hash = HashFunction(ap->StrOfAE) % nsize;
      natom = ap->NextOfAE;
      ap->NextOfAE = ntb[hash].Entry;
      ntb[hash].Entry = catom;
      catom = natom;
    }
    READ_UNLOCK(HashChain[i].AERWLock);
  }
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
  if (HeapTop + sizeof(YAP_SEG_SIZE) < HeapLim) {
    /* make sure there is no heap overflow */
    int res;
    YAPEnterCriticalSection();
    res = do_growheap(FALSE, 0, NULL);
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

  Yap_PrologMode |= GrowHeapMode;
  if (NOfAtoms > 2*AtomHashTableSize) {
      res  = growatomtable();
      Yap_PrologMode &= ~GrowHeapMode;
      return res;
  }
  res=do_growheap(fix_code, in_size, (struct intermediates *)cip);
  Yap_PrologMode &= ~GrowHeapMode;
  return res;
}

int
Yap_growglobal(CELL **ptr)
{
  unsigned long sz = sizeof(CELL) * 16 * 1024L;

#if YAPOR
  if (NOfThreads != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,"cannot grow Global: more than a worker/thread running");
    return(FALSE);
  }
#endif
  if (!static_growglobal(sz, ptr))
    return(FALSE);
#ifdef TABLING
  fix_tabling_info();
#endif /* TABLING */
  return(TRUE);
}


static int
execute_growstack(long size0, int from_trail)
{
  UInt minimal_request = 0L;
  long size = size0;
  ADDR old_Yap_GlobalBase = Yap_GlobalBase;
  
  if (!Yap_ExtendWorkSpace(size)) {
    /* make sure stacks and trail are contiguous */

    minimal_request = AdjustPageSize(((CELL)Yap_TrailTop-(CELL)Yap_GlobalBase)+4*MinHeapGap+size0);

    size = Yap_ExtendWorkSpaceThroughHole(minimal_request);
    if (size < 0) {
      Yap_ErrorMessage = "Database crashed against stacks";
      return FALSE;
    }
    YAPEnterCriticalSection();
    GDiff = DelayDiff = size;
  } else {
    YAPEnterCriticalSection();
    if (Yap_GlobalBase != old_Yap_GlobalBase) {
      GDiff = DelayDiff = Yap_GlobalBase-old_Yap_GlobalBase;
      Yap_GlobalBase=old_Yap_GlobalBase;
    } else {
      GDiff = DelayDiff = 0;
    }
  }
  XDiff = HDiff = 0;
#if USE_SYSTEM_MALLOC
  if (from_trail) {
    TrDiff = LDiff = GDiff;
  } else {
    TrDiff = LDiff = size+GDiff;
  }
#else
  TrDiff = LDiff = size;
#endif
  ASP -= 256;
  if (GDiff) {
    SetHeapRegs();
  } else {
    SetStackRegs();
  }
  if (from_trail) {
    Yap_TrailTop += size;
  }
  if (LDiff) {
    MoveLocalAndTrail();
  }
  if (GDiff) {
#if !USE_SYSTEM_MALLOC
    /* That is done by realloc */
    MoveGlobal();
#endif
    AdjustStacksAndTrail();
    AdjustRegs(MaxTemps);
#ifdef TABLING
    fix_tabling_info();
#endif /* TABLING */
  } else if (LDiff) {
    AdjustGrowStack();
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
  size = AdjustPageSize(size);
  Yap_ErrorMessage = NULL;
  start_growth_time = Yap_cputime();
  gc_verbose = Yap_is_gc_verbose();
  stack_overflows++;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% Stack Overflow %d\n", stack_overflows);
    fprintf(Yap_stderr, "%%   Global: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)Yap_GlobalBase),Yap_GlobalBase,H);
    fprintf(Yap_stderr, "%%   Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%   Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
    fprintf(Yap_stderr, "%% Growing the stacks %ld bytes\n", size);
  }
  if (!execute_growstack(size, FALSE))
    return FALSE;
  growth_time = Yap_cputime()-start_growth_time;
  total_stack_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  return(TRUE);
}

int
Yap_growstack(long size)
{
  int res;

  Yap_PrologMode |= GrowStackMode;
  res=growstack(size);
  Yap_PrologMode &= ~GrowStackMode;
  return res;
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
    tks = *tksp = TokEntryAdjust(tks);
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
      tktmp = TokEntryAdjust(tktmp);
      tks->TokNext = tktmp;
    }
    tks = tktmp;
  }
  if (ves != NULL) {
    ves = *vep = (VarEntry *)TrailAddrAdjust((ADDR)ves);
    AdjustVarTable(ves);
  }
  ves = Yap_AnonVarTable;
  if (ves != NULL) {
    ves = Yap_AnonVarTable = VarEntryAdjust(ves);
  }
  while (ves != NULL) {
    VarEntry *vetmp = ves->VarLeft;
    if (vetmp != NULL) {
      vetmp = VarEntryAdjust(vetmp);
      ves->VarLeft = vetmp;
    }
    ves->VarAdr = TermNil;
    ves = vetmp;
  }
}

/* Used by parser when we're short of stack space */
int
Yap_growstack_in_parser(tr_fr_ptr *old_trp, TokEntry **tksp, VarEntry **vep)
{
  UInt start_growth_time, growth_time;
  int gc_verbose;
  long size  = sizeof(CELL)*(LCL0-(CELL *)Yap_GlobalBase);

#if YAPOR
  if (NOfThreads != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,"cannot grow Parser Stack: more than a worker/thread running");
    return(FALSE);
  }
#endif
  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  Yap_ErrorMessage = NULL;
  if (!Yap_ExtendWorkSpace(size)) {
    Yap_ErrorMessage = "Parser stack overflowed";
    return(FALSE);
  }
  start_growth_time = Yap_cputime();
  gc_verbose = Yap_is_gc_verbose();
  stack_overflows++;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% Stack overflow %d\n", stack_overflows);
    fprintf(Yap_stderr, "%%   Global: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)Yap_GlobalBase),(CELL *)Yap_GlobalBase,H);
    fprintf(Yap_stderr, "%%   Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%   Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
    fprintf(Yap_stderr, "%%  growing the stacks %ld bytes\n", size);
  }
  ASP -= 256;
  YAPEnterCriticalSection();
  TrDiff = LDiff = size;
  XDiff = HDiff = GDiff = DelayDiff = 0;
  SetStackRegs();
  MoveLocalAndTrail();
  AdjustScannerStacks(tksp, vep);
  {
    tr_fr_ptr nTR;
    nTR = TR;
    *old_trp = PtoTRAdjust(*old_trp);
    TR = *old_trp;
    AdjustGrowStack();
    TR = nTR;
  }
  AdjustRegs(MaxTemps);
  YAPLeaveCriticalSection();
  ASP += 256;
  growth_time = Yap_cputime()-start_growth_time;
  total_stack_overflow_time += growth_time;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%%   took %g sec\n", (double)growth_time/1000);
    fprintf(Yap_stderr, "%% Total of %g sec expanding stacks \n", (double)total_stack_overflow_time/1000);
  }
  return(TRUE);  
}

static int do_growtrail(long size)
{
  UInt start_growth_time = Yap_cputime(), growth_time;
  int gc_verbose = Yap_is_gc_verbose();

  /* at least 64K for trail */
  if (size < 64*1024)
    size = 64*1024;
  /* adjust to a multiple of 256) */
  size = AdjustPageSize(size);
  trail_overflows++;
  if (gc_verbose) {
    fprintf(Yap_stderr, "%% Trail overflow %d\n", trail_overflows);
#if USE_SYSTEM_MALLOC
    fprintf(Yap_stderr, "%%  Heap: %8ld cells (%p-%p)\n", (unsigned long int)(H-(CELL *)Yap_GlobalBase),(CELL *)Yap_GlobalBase,H);
    fprintf(Yap_stderr, "%%  Local:%8ld cells (%p-%p)\n", (unsigned long int)(LCL0-ASP),LCL0,ASP);
    fprintf(Yap_stderr, "%%  Trail:%8ld cells (%p-%p)\n",
	       (unsigned long int)(TR-(tr_fr_ptr)Yap_TrailBase),Yap_TrailBase,TR);
#endif
    fprintf(Yap_stderr, "%% growing the trail %ld bytes\n", size);
  }
  Yap_ErrorMessage = NULL;
#if USE_SYSTEM_MALLOC
  execute_growstack(size, TRUE);
#else
  if (!Yap_ExtendWorkSpace(size)) {
    execute_growstack(size, TRUE);
  }
  YAPEnterCriticalSection();
  Yap_TrailTop += size;
  YAPLeaveCriticalSection();
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
Yap_growtrail(long size)
{
  return do_growtrail(size);
}

CELL **
Yap_shift_visit(CELL **to_visit, CELL ***to_visit_maxp)
{
#if USE_SYSTEM_MALLOC || USE_DL_MALLOC
  CELL **to_visit_max = *to_visit_maxp;
  Int sz1 = (CELL)to_visit_max-(CELL)to_visit;
  Int sz0 = AuxTop - (ADDR)to_visit_maxp, sz, dsz;
  char *newb = Yap_ExpandPreAllocCodeSpace(0);

  /* check new size */
  sz = AuxTop-newb;
  /* how much we grew */
  dsz = sz-sz0;
  /* copy whole block to end */
  cpcellsd((CELL *)newb, (CELL *)(newb+dsz), sz0/sizeof(CELL));
  /* base pointer is block start */
  *to_visit_maxp = (CELL **)newb;
  /* current top is originall diff + diff size */
  return (CELL **)((char *)newb+(sz1+dsz));
#else
  CELL **old_top = (CELL **)Yap_TrailTop;
  if (do_growtrail(64 * 1024L)) {
    CELL **dest = (CELL **)((char *)to_visit+64 * 1024L);
    cpcellsd((CELL *)dest, (CELL *)to_visit, (CELL)((CELL *)old_top-(CELL *)to_visit));
    return dest;
  } else {
    Yap_Error(SYSTEM_ERROR,TermNil,"cannot grow temporary stack for unification (%p)", Yap_TrailTop);    
    return to_visit;
  }
#endif
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
  return(static_growheap(diff, FALSE, NULL));
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
{
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
  Yap_InitCPred("$grow_heap", 1, p_growheap, SafePredFlag);
  Yap_InitCPred("$grow_stack", 1, p_growstack, SafePredFlag);
  Yap_InitCPred("$inform_trail_overflows", 2, p_inform_trail_overflows, SafePredFlag);
  Yap_InitCPred("$inform_heap_overflows", 2, p_inform_heap_overflows, SafePredFlag);
  Yap_InitCPred("$inform_stack_overflows", 2, p_inform_stack_overflows, SafePredFlag);
  Yap_init_gc();
  Yap_init_agc();
}
