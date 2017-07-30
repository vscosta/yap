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
* File:		agc.c							 *
* Last rev:								 *
* mods:									 *
* comments:	reclaim unused atoms and functors			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)agc.c	1.3 3/15/90";
#endif

#include "absmi.h"
#include "Foreign.h"
#include "alloc.h"
#include "yapio.h"
#include "iopreds.h"
#include "attvar.h"

#ifdef DEBUG
/* #define DEBUG_RESTORE1 1 */
/* #define DEBUG_RESTORE2 1 */
/* #define DEBUG_RESTORE3 1 */
#define errout GLOBAL_stderr
#endif

static void  RestoreEntries(PropEntry *, int USES_REGS);
static void  CleanCode(PredEntry * USES_REGS);
static void  RestoreDBTerm(DBTerm *dbr, bool src, int attachments USES_REGS);

#define AtomMarkedBit 1

static inline void
MarkAtomEntry(AtomEntry *ae)
{
  CELL c = (CELL)(ae->NextOfAE);
  c |= AtomMarkedBit;
  ae->NextOfAE = (Atom)c;
}

static inline int
AtomResetMark(AtomEntry *ae)
{
  CELL c = (CELL)(ae->NextOfAE);
  if (c & AtomMarkedBit) {
    c &= ~AtomMarkedBit;
    ae->NextOfAE = (Atom)c;
    return TRUE;
  }
  return FALSE;
}

static inline Atom
CleanAtomMarkedBit(Atom a)
{
  CELL c = (CELL)a;
  c &= ~AtomMarkedBit;
  return (Atom)c;
}


static inline Functor
FuncAdjust(Functor f)
{
  if (!IsExtensionFunctor(f)) {  
    AtomEntry *ae = RepAtom(NameOfFunctor(f));
    MarkAtomEntry(ae);
  }
  return(f);
}


static inline Term
AtomTermAdjust(Term t)
{
  AtomEntry *ae = RepAtom(AtomOfTerm(t));
  MarkAtomEntry(ae);
  return(t);  
}

static inline Term
TermToGlobalOrAtomAdjust(Term t)
{
  if (t && IsAtomTerm(t))
    return AtomTermAdjust(t);
  return(t);
}

static inline Atom
AtomAdjust(Atom a)
{
  AtomEntry *ae;
  if (a == NIL) return(a);
  ae = RepAtom(a);
  MarkAtomEntry(ae);
  return(a);
}

#define IsOldCode(P) FALSE
#define IsOldCodeCellPtr(P) FALSE
#define IsOldDelay(P) FALSE
#define IsOldDelayPtr(P) FALSE
#define IsOldLocalInTR(P) FALSE
#define IsOldLocalInTRPtr(P) FALSE
#define IsOldGlobal(P) FALSE
#define IsOldGlobalPtr(P) FALSE
#define IsOldTrail(P) FALSE
#define IsOldTrailPtr(P) FALSE

#define CharP(X) ((char *)(X))

#define REINIT_LOCK(P) 
#define REINIT_RWLOCK(P) 
#define BlobTypeAdjust(P) (P)
#define NoAGCAtomAdjust(P) (P)
#define OrArgAdjust(P) 
#define TabEntryAdjust(P) 
#define IntegerAdjust(D)  (D)
#define AddrAdjust(P) (P)
#define MFileAdjust(P) (P)
#define CodeVarAdjust(P) (P)
#define ConstantAdjust(P) (P)
#define ArityAdjust(P) (P)
#define DoubleInCodeAdjust(P) 
#define IntegerInCodeAdjust(P) 
#define OpcodeAdjust(P) (P)
#define ModuleAdjust(P) (P)
#define ExternalFunctionAdjust(P) (P)
#define DBRecordAdjust(P) (P)
#define PredEntryAdjust(P) (P)
#define ModEntryPtrAdjust(P) (P)
#define AtomEntryAdjust(P) (P)
#define GlobalEntryAdjust(P) (P)
#define BlobTermInCodeAdjust(P) (P)
#define CellPtoHeapAdjust(P) (P)
#define PtoAtomHashEntryAdjust(P) (P)
#define CellPtoHeapCellAdjust(P) (P)
#define CellPtoTRAdjust(P) (P)
#define CodeAddrAdjust(P) (P)
#define ConsultObjAdjust(P) (P)
#define DelayAddrAdjust(P) (P)
#define DelayAdjust(P) (P)
#define GlobalAdjust(P) (P)
#define DBRefAdjust(P,REF) (P)
#define DBRefPAdjust(P) (P)
#define DBTermAdjust(P) (P)
#define LUIndexAdjust(P) (P)
#define SIndexAdjust(P) (P)
#define LocalAddrAdjust(P) (P)
#define GlobalAddrAdjust(P) (P)
#define OpListAdjust(P) (P)
#define PtoLUCAdjust(P) (P)
#define PtoStCAdjust(P) (P)
#define PtoArrayEAdjust(P) (P)
#define PtoArraySAdjust(P) (P)
#define PtoGlobalEAdjust(P) (P)
#define PtoDelayAdjust(P) (P)
#define PtoGloAdjust(P) (P)
#define PtoLocAdjust(P) (P)
#define PtoHeapCellAdjust(P) (P)
#define TermToGlobalAdjust(P) (P)
#define PtoOpAdjust(P) (P)
#define PtoLUClauseAdjust(P) (P)
#define PtoLUIndexAdjust(P) (P)
#define PtoDBTLAdjust(P) (P)
#define PtoPredAdjust(P) (P)
#define PtoPtoPredAdjust(P) (P)
#define OpRTableAdjust(P) (P)
#define OpEntryAdjust(P) (P)
#define PropAdjust(P) (P)
#define TrailAddrAdjust(P) (P)
#define XAdjust(P) (P)
#define YAdjust(P) (P)
#define HoldEntryAdjust(P) (P)
#define CodeCharPAdjust(P) (P)
#define CodeConstCharPAdjust(P) (P)
#define CodeVoidPAdjust(P) (P)
#define HaltHookAdjust(P) (P)

#define recompute_mask(dbr)

#define rehash(oldcode, NOfE, KindOfEntries)

#define RestoreSWIHash()

static void
AdjustTermFlag(flag_term *tarr, UInt i)
{
  CACHE_REGS
  if (IsVarTerm(tarr[i].at)) {
    RestoreDBTerm( tarr[i].DBT, false, 0 PASS_REGS );
  } else if (IsAtomTerm( tarr[i].at )  )
    tarr[i].at = AtomTermAdjust(tarr[i].at);
}

static void RestoreFlags( UInt NFlags )
{
  CACHE_REGS
  size_t i;
  flag_term *tarr = GLOBAL_Flags;

  if (worker_id == 0)
    for (i=0; i<GLOBAL_flagCount; i++) {
      AdjustTermFlag( tarr, i);
    }
  tarr = LOCAL_Flags;
  for (i=0; i<LOCAL_flagCount; i++) {
    AdjustTermFlag( tarr, i);
  }
}

#include "rheap.h"

static void
RestoreHashPreds( USES_REGS1 )
{
}


static void init_reg_copies(USES_REGS1)
{
  LOCAL_OldASP = ASP;
  LOCAL_OldLCL0 = LCL0;
  LOCAL_OldTR = TR;
  LOCAL_OldGlobalBase = (CELL *)LOCAL_GlobalBase;
  LOCAL_OldH = HR;
  LOCAL_OldH0 = H0;
  LOCAL_OldTrailBase = LOCAL_TrailBase;
  LOCAL_OldTrailTop = LOCAL_TrailTop;
  LOCAL_OldHeapBase = Yap_HeapBase;
  LOCAL_OldHeapTop = HeapTop;
}


static void
RestoreAtomList(Atom atm USES_REGS)
{
  AtomEntry      *at;

  at = RepAtom(atm);
  if (EndOfPAEntr(at))
    return;
  do {
    RestoreAtom(atm PASS_REGS);
    atm = CleanAtomMarkedBit(at->NextOfAE);
    at = RepAtom(atm);
  } while (!EndOfPAEntr(at));
}

static void
mark_trail(USES_REGS1)
{
  register tr_fr_ptr pt;

  pt = TR;
  /* moving the trail is simple */
  while (pt != (tr_fr_ptr)LOCAL_TrailBase) {
    CELL reg = TrailTerm(pt-1);

    if (!IsVarTerm(reg)) {
      if (IsAtomTerm(reg)) {
	MarkAtomEntry(RepAtom(AtomOfTerm(reg)));
      }
    }

    pt--;
  }
}

static void
mark_registers(USES_REGS1)
{
  CELL *pt;

  pt = XREGS;
  /* moving the trail is simple */
  while (pt != XREGS+MaxTemps) {
    CELL reg = *pt++;

    if (!IsVarTerm(reg)) {
      if (IsAtomTerm(reg)) {
	MarkAtomEntry(RepAtom(AtomOfTerm(reg)));
      }
    }
  }
}

static void
mark_local(USES_REGS1)
{
  CELL   *pt;

  /* Adjusting the local */
  pt = LCL0;
  /* moving the trail is simple */
  while (pt > ASP) {
    CELL reg = *--pt;

    if (!IsVarTerm(reg)) {
      if (IsAtomTerm(reg)
#ifdef TABLING
	  /* assume we cannot have atoms on first page,
	     so this must be an arity
	  */
	  && reg > Yap_page_size
#endif
	  ) {
	MarkAtomEntry(RepAtom(AtomOfTerm(reg)));
      }
    }
  }
}

static CELL *
mark_global_cell(CELL *pt)
{   
  CELL reg = *pt;

  if (IsVarTerm(reg)) {
    /* skip bitmaps */
    switch(reg) {
    case (CELL)FunctorDouble:
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
      return pt + 4;
#else
      return pt + 3;
#endif
    case (CELL)FunctorString:
      return pt + 3 + pt[1];
    case (CELL)FunctorBigInt:
      {
	Int sz = 3 +
	  (sizeof(MP_INT)+
	   (((MP_INT *)(pt+2))->_mp_alloc*sizeof(mp_limb_t)))/sizeof(CELL);
	YAP_Opaque_CallOnGCMark f;
	YAP_Opaque_CallOnGCRelocate f2;
	Term t = AbsAppl(pt);

	if ( (f = Yap_blob_gc_mark_handler(t)) ) {
	  CELL ar[256];
	  Int i,n = (f)(Yap_BlobTag(t), Yap_BlobInfo(t), ar, 256);
	  if (n < 0) {
	    Yap_Error(RESOURCE_ERROR_HEAP,TermNil,"not enough space for slot internal variables in agc");
	      }
	  for (i = 0; i< n; i++) {
	    CELL *pt = ar+i;
	    CELL reg = *pt;
	    if (!IsVarTerm(reg) && IsAtomTerm(reg)) {
	      *pt = AtomTermAdjust(reg);
	    }
	  }
	  if ( (f2 = Yap_blob_gc_relocate_handler(t)) < 0 ) {
	    int out = (f2)(Yap_BlobTag(t), Yap_BlobInfo(t), ar, n);
	    if (out < 0)
	      Yap_Error(RESOURCE_ERROR_HEAP,TermNil,"bad restore of slot internal variables in agc");
	  }
	}

	return pt + sz;
      }
    case (CELL)FunctorLongInt:
      return pt + 3;
      break;
    }
  } else if (IsAtomTerm(reg)) {
    MarkAtomEntry(RepAtom(AtomOfTerm(reg)));
    return pt+1;
  }
  return pt+1;
}

static void
mark_global(USES_REGS1)
{
  CELL *pt;

  /*
   * to clean the global now that functors are just variables pointing to
   * the code 
   */
  pt = H0;
  while (pt < HR) {
    pt = mark_global_cell(pt);
  }
}

static void
mark_stacks(USES_REGS1)
{
  mark_registers(PASS_REGS1);
  mark_trail(PASS_REGS1);
  mark_local(PASS_REGS1);
  mark_global(PASS_REGS1);
}

static void
clean_atom_list(AtomHashEntry *HashPtr)
{
  Atom atm = HashPtr->Entry;
  Atom *patm = &(HashPtr->Entry);
  while (atm != NIL) {
    AtomEntry *at =  RepAtom(atm);
    if (AtomResetMark(at) ||
	( at->PropsOfAE != NIL && !IsBlob(at) ) ||
	(GLOBAL_AGCHook != NULL && !GLOBAL_AGCHook(atm))) {
      patm = &(at->NextOfAE);
      atm = at->NextOfAE;
    } else {
      NOfAtoms--;
      if (IsBlob(atm)) {
	YAP_BlobPropEntry *b = RepBlobProp(at->PropsOfAE);
	if (b->NextOfPE != NIL) {
	  patm = &(at->NextOfAE);
	  atm = at->NextOfAE;
	  continue;
	}
	NOfAtoms++;
	NOfBlobs--;
	Yap_FreeCodeSpace((char *)b);
	GLOBAL_agc_collected += sizeof(YAP_BlobPropEntry);
	GLOBAL_agc_collected += sizeof(AtomEntry)+sizeof(size_t)+at->rep.blob->length;
      }  else {
#ifdef DEBUG_RESTORE3
	fprintf(stderr, "Purged %p:%s patm=%p %p\n", at, at->StrOfAE, patm, at->NextOfAE);
#endif
	GLOBAL_agc_collected += sizeof(AtomEntry)+strlen((const char *)at->StrOfAE);
      }
      *patm = atm = at->NextOfAE;
      Yap_FreeCodeSpace((char *)at);
    }
  }
}

/*
 * This is the really tough part, to restore the whole of the heap 
 */
static void 
clean_atoms(void)
{
  AtomHashEntry *HashPtr = HashChain;
  register int    i;

  AtomResetMark(AtomFoundVar);
  AtomResetMark(AtomFreeTerm);
  for (i = 0; i < AtomHashTableSize; ++i) {
    clean_atom_list(HashPtr);
    HashPtr++;
  }
  clean_atom_list(&INVISIBLECHAIN);
  {
    AtomHashEntry list;
    list.Entry =                                                                                                                                                                                                                                                                                                                                                                                                                                                            Blobs;
    clean_atom_list(&list);
  }
}

static void
atom_gc(USES_REGS1)
{
  bool		gc_verbose = Yap_is_gc_verbose();
  bool          gc_trace = false;
  

  UInt		time_start, agc_time;
#if  defined(YAPOR) || defined(THREADS)
  return;
#endif
  
  if (Yap_GetValue(AtomGcTrace) != TermNil)
    gc_trace = true;

  GLOBAL_agc_calls++;
  GLOBAL_agc_collected = 0;
  
  if (gc_trace) {
    fprintf(stderr, "%% agc:\n");
  } else if (gc_verbose) {
    fprintf(stderr, "%%   Start of atom garbage collection %d:\n", GLOBAL_agc_calls);
  }
  time_start = Yap_cputime();
  /* get the number of active registers */
  YAPEnterCriticalSection();
  init_reg_copies(PASS_REGS1);
  mark_stacks(PASS_REGS1);
  restore_codes();
  clean_atoms();
  NOfBlobsMax = NOfBlobs+(NOfBlobs/2+256< 1024 ? NOfBlobs/2+256 : 1024);
  YAPLeaveCriticalSection();
  agc_time = Yap_cputime()-time_start;
  GLOBAL_tot_agc_time += agc_time;
  GLOBAL_tot_agc_recovered += GLOBAL_agc_collected;
  if (gc_verbose) {
#ifdef _WIN32
    fprintf(stderr, "%%   Collected %I64d bytes.\n", GLOBAL_agc_collected);
#else
    fprintf(stderr, "%%   Collected %lld bytes.\n", GLOBAL_agc_collected);
#endif
    fprintf(stderr, "%%   GC %d took %g sec, total of %g sec doing GC so far.\n", GLOBAL_agc_calls, (double)agc_time/1000, (double)GLOBAL_tot_agc_time/1000);
  }
}

void
Yap_atom_gc(USES_REGS1)
{
  atom_gc(PASS_REGS1);
}

static Int
p_atom_gc(USES_REGS1)
{
#ifndef FIXED_STACKS
  atom_gc(PASS_REGS1);
#endif  /* FIXED_STACKS */
  return TRUE;
}

static Int
p_inform_agc(USES_REGS1)
{
  Term tn = MkIntegerTerm(GLOBAL_tot_agc_time);
  Term tt = MkIntegerTerm(GLOBAL_agc_calls);
  Term ts = MkIntegerTerm(GLOBAL_tot_agc_recovered);

  return
    Yap_unify(tn, ARG2) &&
    Yap_unify(tt, ARG1) &&
    Yap_unify(ts, ARG3);
}


void 
Yap_init_agc(void)
{
  Yap_InitCPred("$atom_gc", 0, p_atom_gc, 0);
  Yap_InitCPred("$inform_agc", 3, p_inform_agc, 0);
}
