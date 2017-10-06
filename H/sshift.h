/*************************************************************************
*									 *
*	 YAP Prolog   %W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		sshift.h						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	stack shifter functionality for YAP			 *
*									 *
*************************************************************************/

#ifndef SSHIFT_H
#define SSHIFT_H

#define REINIT_LOCK(P) INIT_LOCK(P) 
#define REINIT_RWLOCK(P) INIT_RWLOCK(P) 

#include <blobs.h>

#define CharP(ptr)	((char *) (ptr))


#define CodeAdjust(ptr) CodeAdjust__(ptr PASS_REGS)
#define PtoTRAdjust(ptr) PtoTRAdjust__(ptr PASS_REGS)
#define BaseAddrAdjust(ptr) BaseAddrAdjust__(ptr PASS_REGS)
#define CutCAdjust(ptr) CutCAdjust__(ptr PASS_REGS)
#define ChoicePtrAdjust(ptr) ChoicePtrAdjust__(ptr PASS_REGS)
#define FuncAdjust(ptr) FuncAdjust__(ptr PASS_REGS)
#define AtomTermAdjust(ptr) AtomTermAdjust__(ptr PASS_REGS)
#define TermToGlobalOrAtomAdjust(ptr) TermToGlobalOrAtomAdjust__(ptr PASS_REGS)
#define AtomAdjust(ptr) AtomAdjust__(ptr PASS_REGS)
#define IsOldCode(ptr) IsOldCode__(ptr PASS_REGS)
#define IsOldLocal(ptr) IsOldLocal__(ptr PASS_REGS)
#define IsOldLocalPtr(ptr) IsOldLocalPtr__(ptr PASS_REGS)
#define IsOldCodeCellPtr(ptr) IsOldCodeCellPtr__(ptr PASS_REGS)
#define IsOldDelay(ptr) IsOldDelay__(ptr PASS_REGS)
#define IsOldDelayPtr(ptr) IsOldDelayPtr__(ptr PASS_REGS)
#define IsOldLocalInTR(ptr) IsOldLocalInTR__(ptr PASS_REGS)
#define IsOldLocalInTRPtr(ptr) IsOldLocalInTRPtr__(ptr PASS_REGS)
#define IsOldGlobal(ptr) IsOldGlobal__(ptr PASS_REGS)
#define IsOldGlobalPtr(ptr) IsOldGlobalPtr__(ptr PASS_REGS)
#define IsOldTrail(ptr) IsOldTrail__(ptr PASS_REGS)
#define IsOldTrailPtr(ptr) IsOldTrailPtr__(ptr PASS_REGS)
#define NoAGCAtomAdjust(ptr) NoAGCAtomAdjust__(ptr PASS_REGS)
// #define OrArgAdjust(ptr) OrArgAdjust__(ptr PASS_REGS) 
// #define TabEntryAdjust(ptr) TabEntryAdjust__(ptr PASS_REGS)
// #define IntegerAdjust(D)  IntegerAdjust__(ptr PASS_REGS)
#define AddrAdjust(ptr) AddrAdjust__(ptr PASS_REGS)
#define BlockAdjust(ptr) BlockAdjust__(ptr PASS_REGS)
#define CodeVarAdjust(ptr) CodeVarAdjust__(ptr PASS_REGS)
#define ConstantAdjust(ptr) ConstantAdjust__(ptr PASS_REGS)
#define ArityAdjust(ptr) ArityAdjust__(ptr PASS_REGS)
// #define DoubleInCodeAdjust(ptr) DoubleInCodeAdjust__(ptr PASS_REGS)
// #define IntegerInCodeAdjust(ptr) IntegerInCodeAdjust__(ptr PASS_REGS)
#define OpcodeAdjust(ptr) OpcodeAdjust__(ptr PASS_REGS)
#define ModuleAdjust(ptr) ModuleAdjust__(ptr PASS_REGS)
// #define ExternalFunctionAdjust(ptr) ExternalFunctionAdjust__(ptr PASS_REGS)
#define DBRecordAdjust(ptr) DBRecordAdjust__(ptr PASS_REGS)
#define PredEntryAdjust(ptr) PredEntryAdjust__(ptr PASS_REGS)
#define ModEntryPtrAdjust(ptr) ModEntryPtrAdjust__(ptr PASS_REGS)
#define AtomEntryAdjust(ptr) AtomEntryAdjust__(ptr PASS_REGS)
#define GlobalEntryAdjust(ptr) GlobalEntryAdjust__(ptr PASS_REGS)
#define BlobTermInCodeAdjust(ptr) BlobTermInCodeAdjust__(ptr PASS_REGS)
#define CellPtoHeapAdjust(ptr) CellPtoHeapAdjust__(ptr PASS_REGS)
#define PtoAtomHashEntryAdjust(ptr) PtoAtomHashEntryAdjust__(ptr PASS_REGS)
#define CellPtoHeapCellAdjust(ptr) CellPtoHeapCellAdjust__(ptr PASS_REGS)
#define CellPtoTRAdjust(ptr) CellPtoTRAdjust__(ptr PASS_REGS)
#define CodeAddrAdjust(ptr) CodeAddrAdjust__(ptr PASS_REGS)
#define ConsultObjAdjust(ptr) ConsultObjAdjust__(ptr PASS_REGS)
#define DelayAddrAdjust(ptr) DelayAddrAdjust__(ptr PASS_REGS)
#define DelayAdjust(ptr) DelayAdjust__(ptr PASS_REGS)
#define GlobalAdjust(ptr) GlobalAdjust__(ptr PASS_REGS)
#define DBRefAdjust(ptr,C) DBRefAdjust__(ptr PASS_REGS)
#define DBRefPAdjust(ptr) DBRefPAdjust__(ptr PASS_REGS)
#define DBTermAdjust(ptr) DBTermAdjust__(ptr PASS_REGS)
#define LUIndexAdjust(ptr) LUIndexAdjust__(ptr PASS_REGS)
#define SIndexAdjust(ptr) SIndexAdjust__(ptr PASS_REGS)
#define LocalAddrAdjust(ptr) LocalAddrAdjust__(ptr PASS_REGS)
#define GlobalAddrAdjust(ptr) GlobalAddrAdjust__(ptr PASS_REGS)
#define OpListAdjust(ptr) OpListAdjust__(ptr PASS_REGS)
#define PtoLUCAdjust(ptr) PtoLUCAdjust__(ptr PASS_REGS)
#define PtoStCAdjust(ptr) PtoStCAdjust__(ptr PASS_REGS)
#define PtoArrayEAdjust(ptr) PtoArrayEAdjust__(ptr PASS_REGS)
#define PtoArraySAdjust(ptr) PtoArraySAdjust__(ptr PASS_REGS)
#define PtoGlobalEAdjust(ptr) PtoGlobalEAdjust__(ptr PASS_REGS)
#define PtoDelayAdjust(ptr) PtoDelayAdjust__(ptr PASS_REGS)
#define PtoGloAdjust(ptr) PtoGloAdjust__(ptr PASS_REGS)
#define PtoLocAdjust(ptr) PtoLocAdjust__(ptr PASS_REGS)
#define PtoHeapCellAdjust(ptr) PtoHeapCellAdjust__(ptr PASS_REGS)
#define TermToGlobalAdjust(ptr) TermToGlobalAdjust__(ptr PASS_REGS)
#define PtoOpAdjust(ptr) PtoOpAdjust__(ptr PASS_REGS)
#define PtoLUClauseAdjust(ptr) PtoLUClauseAdjust__(ptr PASS_REGS)
#define PtoLUIndexAdjust(ptr) PtoLUIndexAdjust__(ptr PASS_REGS)
#define PtoDBTLAdjust(ptr) PtoDBTLAdjust__(ptr PASS_REGS)
#define PtoPredAdjust(ptr) PtoPredAdjust__(ptr PASS_REGS)
#define PtoPtoPredAdjust(ptr) PtoPtoPredAdjust__(ptr PASS_REGS)
#define OpRTableAdjust(ptr) OpRTableAdjust__(ptr PASS_REGS)
#define OpEntryAdjust(ptr) OpEntryAdjust__(ptr PASS_REGS)
#define PropAdjust(ptr) PropAdjust__(ptr PASS_REGS)
#define BlobTypeAdjust(ptr) BlobTypeAdjust__(ptr PASS_REGS)
#define TrailAddrAdjust(ptr) TrailAddrAdjust__(ptr PASS_REGS)
#define XAdjust(ptr) XAdjust__(ptr PASS_REGS)
#define YAdjust(ptr) YAdjust__(ptr PASS_REGS)
#define LocalAdjust(ptr) LocalAdjust__(ptr PASS_REGS)
#define TrailAdjust(ptr) TrailAdjust__(ptr PASS_REGS)
#define HoldEntryAdjust(ptr) HoldEntryAdjust__(ptr PASS_REGS)
#define CodeCharPAdjust(ptr) CodeCharPAdjust__(ptr PASS_REGS)
#define CodeConstCharPAdjust(ptr) CodeConstCharPAdjust__(ptr PASS_REGS)
#define CodeVoidPAdjust(ptr) CodeVoidPAdjust__(ptr PASS_REGS)
#define HaltHookAdjust(ptr) HaltHookAdjust__(ptr PASS_REGS)
#define TokEntryAdjust(ptr) TokEntryAdjust__(ptr PASS_REGS)
#define VarEntryAdjust(ptr) VarEntryAdjust__(ptr PASS_REGS)
#define ConsumerChoicePtrAdjust(ptr) ConsumerChoicePtrAdjust__(ptr PASS_REGS)
#define GeneratorChoicePtrAdjust(ptr) GeneratorChoicePtrAdjust__(ptr PASS_REGS)
#define IsHeapP(ptr) IsHeapP__(ptr PASS_REGS)
#define IsOldVarTableTrailPtr(ptr) IsOldVarTableTrailPtr__(ptr PASS_REGS)
#define IsOldTokenTrailPtr(ptr) IsOldTokenTrailPtr__(ptr PASS_REGS)

#include "inline-only.h"
INLINE_ONLY inline EXTERN int IsHeapP__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsHeapP__ (CELL * ptr USES_REGS)
{
#if USE_SYSTEM_MALLOC
  return (int) ((ptr < (CELL *) LOCAL_GlobalBase || ptr > (CELL *) LOCAL_TrailTop));
#else
  return (int) ((ptr >= (CELL *) Yap_HeapBase && ptr <= (CELL *) HeapTop));
#endif
}

#define OrArgAdjust(ptr) 
#define TabEntryAdjust(ptr) 

/* Adjusting cells and pointers to cells */

INLINE_ONLY inline EXTERN CELL *PtoGloAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
PtoGloAdjust__ (CELL * ptr USES_REGS)
{
  if (ptr < LOCAL_GSplit) {
    if (ptr < H0) 
      return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_DelayDiff)));
    else
      return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_GDiff0)));
  } else {
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_GDiff)));
  }
}



INLINE_ONLY inline EXTERN CELL *PtoDelayAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
PtoDelayAdjust__ (CELL * ptr USES_REGS)
{
  if (!LOCAL_GSplit || ptr < LOCAL_GSplit)
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_DelayDiff)));
  else
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_GDiff0)));
}

INLINE_ONLY inline EXTERN CELL *PtoBaseAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
PtoBaseAdjust__ (CELL * ptr USES_REGS)
{
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_BaseDiff)));
}



INLINE_ONLY inline EXTERN tr_fr_ptr PtoTRAdjust__ (tr_fr_ptr CACHE_TYPE);

INLINE_ONLY inline EXTERN tr_fr_ptr
PtoTRAdjust__ (tr_fr_ptr ptr USES_REGS)
{
  return (tr_fr_ptr) (((tr_fr_ptr) (CharP (ptr) + LOCAL_TrDiff)));
}



INLINE_ONLY inline EXTERN CELL *CellPtoTRAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
CellPtoTRAdjust__ (CELL * ptr USES_REGS)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_TrDiff)));
}



INLINE_ONLY inline EXTERN CELL *PtoLocAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
PtoLocAdjust__ (CELL * ptr USES_REGS)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_LDiff)));
}


INLINE_ONLY inline EXTERN struct cut_c_str *CutCAdjust__ (struct cut_c_str * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct cut_c_str *
CutCAdjust__ (struct  cut_c_str * ptr USES_REGS)
{
  return (struct cut_c_str *) (CharP (ptr) + LOCAL_LDiff);
}



INLINE_ONLY inline EXTERN choiceptr ChoicePtrAdjust__ (choiceptr CACHE_TYPE);

INLINE_ONLY inline EXTERN choiceptr
ChoicePtrAdjust__ (choiceptr ptr USES_REGS)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LOCAL_LDiff)));
}


#ifdef TABLING

INLINE_ONLY inline EXTERN choiceptr ConsumerChoicePtrAdjust__ (choiceptr CACHE_TYPE);

INLINE_ONLY inline EXTERN choiceptr
ConsumerChoicePtrAdjust__ (choiceptr ptr USES_REGS)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LOCAL_LDiff)));
}



INLINE_ONLY inline EXTERN choiceptr GeneratorChoicePtrAdjust__ (choiceptr CACHE_TYPE);

INLINE_ONLY inline EXTERN choiceptr
GeneratorChoicePtrAdjust__ (choiceptr ptr USES_REGS)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LOCAL_LDiff)));
}


#endif /* TABLING */


INLINE_ONLY inline EXTERN CELL GlobalAdjust__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL
GlobalAdjust__ (CELL val USES_REGS)
{
  if ((CELL *)val < LOCAL_GSplit) {
    if ((CELL *)val < H0) 
      return (CELL) (val + LOCAL_DelayDiff);
    else
      return (CELL) (val + LOCAL_GDiff0);
  } else {
    return (CELL) (val + LOCAL_GDiff);
  }
}



INLINE_ONLY inline EXTERN CELL DelayAdjust__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL
DelayAdjust__ (CELL val USES_REGS)
{
  if (!LOCAL_GSplit || (CELL *)val < LOCAL_GSplit)
    return (CELL) (val + LOCAL_DelayDiff);
  else
    return (CELL) (val + LOCAL_GDiff0);
}


INLINE_ONLY inline EXTERN ADDR GlobalAddrAdjust__ (ADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN ADDR
GlobalAddrAdjust__ (ADDR ptr USES_REGS)
{
  if ((CELL *)ptr < LOCAL_GSplit) {
    if ((CELL *)ptr < H0) 
      return (ADDR) (ptr + LOCAL_DelayDiff);
    else
      return (ADDR) ((ptr + LOCAL_GDiff0));
  } else {
    return (ADDR) ((ptr + LOCAL_GDiff));
  }
}




INLINE_ONLY inline EXTERN ADDR DelayAddrAdjust__ (ADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN ADDR
DelayAddrAdjust__ (ADDR ptr USES_REGS)
{
  if (!LOCAL_GSplit || (CELL *)ptr < LOCAL_GSplit)
    return (ADDR) ((ptr + LOCAL_DelayDiff));
  else
    return (ADDR) ((ptr + LOCAL_GDiff0));
}


INLINE_ONLY inline EXTERN ADDR BaseAddrAdjust__ (ADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN ADDR
BaseAddrAdjust__ (ADDR ptr USES_REGS)
{
  return (ADDR) ((ptr + LOCAL_BaseDiff));
}



INLINE_ONLY inline EXTERN CELL LocalAdjust__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL
LocalAdjust__ (CELL val USES_REGS)
{
  return (CELL) ((val + LOCAL_LDiff));
}



INLINE_ONLY inline EXTERN ADDR LocalAddrAdjust__ (ADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN ADDR
LocalAddrAdjust__ (ADDR ptr USES_REGS)
{
  return (ADDR) ((ptr + LOCAL_LDiff));
}



INLINE_ONLY inline EXTERN CELL TrailAdjust__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL
TrailAdjust__ (CELL val USES_REGS)
{
  return (CELL) ((val + LOCAL_TrDiff));
}



INLINE_ONLY inline EXTERN ADDR TrailAddrAdjust__ (ADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN ADDR
TrailAddrAdjust__ (ADDR ptr USES_REGS)
{
  return (ADDR) ((ptr + LOCAL_TrDiff));
}



INLINE_ONLY inline EXTERN TokEntry *TokEntryAdjust__ (TokEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN TokEntry *
TokEntryAdjust__ (TokEntry * ptr USES_REGS)
{
  return (TokEntry *) (((CELL) ptr + LOCAL_TrDiff));
}



INLINE_ONLY inline EXTERN VarEntry *VarEntryAdjust__ (VarEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN VarEntry *
VarEntryAdjust__ (VarEntry * ptr USES_REGS)
{
  return (VarEntry *) (((CELL) ptr + LOCAL_TrDiff));
}


/* heap data structures */

INLINE_ONLY inline EXTERN Functor FuncAdjust__ (Functor CACHE_TYPE);

INLINE_ONLY inline EXTERN Functor
FuncAdjust__ (Functor f USES_REGS)
{
  if (!IsExtensionFunctor(f)) 
    return (Functor) ((CharP (f) + LOCAL_HDiff));
  return f;
}

INLINE_ONLY inline EXTERN CELL *CellPtoHeapAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
CellPtoHeapAdjust__ (CELL * ptr USES_REGS)
{
  if (!ptr)
    return ptr;
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN HoldEntry *HoldEntryAdjust__ (HoldEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN HoldEntry *
HoldEntryAdjust__ (HoldEntry * ptr USES_REGS)
{
  return (HoldEntry *) (((HoldEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN struct record_list *DBRecordAdjust__ (struct record_list * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct record_list *
DBRecordAdjust__ (struct record_list * ptr USES_REGS)
{
  if (!ptr)
    return ptr;
  return (struct record_list *) (CharP (ptr) + LOCAL_HDiff);
}


#if	USE_OFFSETS

INLINE_ONLY inline EXTERN Atom AtomAdjust__ (Atom CACHE_TYPE);

INLINE_ONLY inline EXTERN Atom
AtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at));
}

INLINE_ONLY inline EXTERN Atom NoAGCAtomAdjust__ (Atom CACHE_TYPE);

INLINE_ONLY inline EXTERN Atom
NoAGCAtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at));
}



INLINE_ONLY inline EXTERN Prop PropAdjust__ (Prop CACHE_TYPE);

INLINE_ONLY inline EXTERN Prop
PropAdjust__ (Prop p USES_REGS)
{
  return (Prop) ((p));
}

#else

INLINE_ONLY inline EXTERN Atom AtomAdjust__ (Atom CACHE_TYPE);

INLINE_ONLY inline EXTERN Atom
AtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at == NULL ? (at) : (Atom) (CharP (at) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN Atom NoAGCAtomAdjust__ (Atom CACHE_TYPE);

INLINE_ONLY inline EXTERN Atom
NoAGCAtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at == NULL ? (at) : (Atom) (CharP (at) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN Prop PropAdjust__ (Prop CACHE_TYPE);

INLINE_ONLY inline EXTERN Prop
PropAdjust__ (Prop p USES_REGS)
{
  return (Prop) ((p == NULL ? (p) : (Prop) (CharP (p) + LOCAL_HDiff)));
}


#endif

INLINE_ONLY inline EXTERN YAP_blob_t *BlobTypeAdjust__ (YAP_blob_t *CACHE_TYPE);

INLINE_ONLY inline EXTERN YAP_blob_t *
BlobTypeAdjust__ ( YAP_blob_t *at USES_REGS)
{
  return ( YAP_blob_t *) ((at == NULL ? (at) : ( YAP_blob_t *) (CharP (at) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN PredEntry *PredEntryAdjust__ (PredEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN PredEntry *
PredEntryAdjust__ (PredEntry *p USES_REGS)
{
  return (PredEntry *) ((p == NULL ? (p) : (PredEntry *) (CharP (p) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN struct mod_entry *ModEntryPtrAdjust__ (struct mod_entry * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct mod_entry *
ModEntryPtrAdjust__ (struct mod_entry *p USES_REGS)
{
  return (struct mod_entry *) ((p == NULL ? (p) : (struct mod_entry *) (CharP (p) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN COUNT ConstantAdjust__ (COUNT CACHE_TYPE);

INLINE_ONLY inline EXTERN COUNT
ConstantAdjust__ (COUNT val USES_REGS)
{
  return val;
}

INLINE_ONLY inline EXTERN Int ArityAdjust__ (Int CACHE_TYPE);

INLINE_ONLY inline EXTERN Int
ArityAdjust__ (Int val USES_REGS)
{
  return val;
}

INLINE_ONLY inline EXTERN OPCODE OpcodeAdjust__ (OPCODE CACHE_TYPE);

INLINE_ONLY inline EXTERN OPCODE
OpcodeAdjust__ (OPCODE val USES_REGS)
{
  return Yap_opcode(Yap_op_from_opcode(val));
}

#define DoubleInCodeAdjust(D)

#define IntegerInCodeAdjust(D) 

#define IntegerAdjust(D)  (D)

#define ExternalFunctionAdjust(D) (D);

INLINE_ONLY inline EXTERN Term AtomTermAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
AtomTermAdjust__ (Term at USES_REGS)
{
  if (at == 0L)
    return at;
  return (Term)(CharP(at) + LOCAL_HDiff);
}

INLINE_ONLY inline EXTERN Term ModuleAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
ModuleAdjust__ (Term t USES_REGS)
{
  return AtomTermAdjust(t);
}

INLINE_ONLY inline EXTERN Term CodeVarAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
CodeVarAdjust__ (Term var USES_REGS)
{
  if (var == 0L)
    return var;
  return (Term)(CharP(var) + LOCAL_HDiff);
}


#if TAGS_FAST_OPS

INLINE_ONLY inline EXTERN Term BlobTermInCodeAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
BlobTermInCodeAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) - LOCAL_HDiff);
}


INLINE_ONLY inline EXTERN Term CodeComposedTermAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
CodeComposedTermAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) - LOCAL_HDiff);
}


#else

INLINE_ONLY inline EXTERN Term BlobTermInCodeAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
BlobTermInCodeAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) + LOCAL_HDiff);
}

INLINE_ONLY inline EXTERN Term CodeComposedTermAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
CodeComposedTermAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) + LOCAL_HDiff);
}


#endif

INLINE_ONLY inline EXTERN AtomEntry *AtomEntryAdjust__ (AtomEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN AtomEntry *
AtomEntryAdjust__ (AtomEntry * at USES_REGS)
{
  return (AtomEntry *) ((AtomEntry *) (CharP (at) + LOCAL_HDiff));
}

INLINE_ONLY inline EXTERN GlobalEntry *GlobalEntryAdjust__ (GlobalEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN GlobalEntry *
GlobalEntryAdjust__ (GlobalEntry * at USES_REGS)
{
  return (GlobalEntry *) ((GlobalEntry *) (CharP (at) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN union CONSULT_OBJ *ConsultObjAdjust__ (union CONSULT_OBJ * CACHE_TYPE);

INLINE_ONLY inline EXTERN union CONSULT_OBJ *
ConsultObjAdjust__ (union CONSULT_OBJ *co USES_REGS)
{
  return (union CONSULT_OBJ *) ((union CONSULT_OBJ *) (CharP (co) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN DBRef DBRefAdjust__ (DBRef CACHE_TYPE);

INLINE_ONLY inline EXTERN DBRef
DBRefAdjust__ (DBRef dbr USES_REGS)
{
  return (DBRef) ((DBRef) (CharP (dbr) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN DBRef *DBRefPAdjust__ (DBRef * CACHE_TYPE);

INLINE_ONLY inline EXTERN DBRef *
DBRefPAdjust__ (DBRef * dbrp USES_REGS)
{
  return (DBRef *) ((DBRef *) (CharP (dbrp) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN DBTerm *DBTermAdjust__ (DBTerm * CACHE_TYPE);

INLINE_ONLY inline EXTERN DBTerm *
DBTermAdjust__ (DBTerm * dbtp USES_REGS)
{
  return (DBTerm *) ((DBTerm *) (CharP (dbtp) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN struct static_index *SIndexAdjust__ (struct static_index * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct static_index *
SIndexAdjust__ (struct static_index *si USES_REGS)
{
  return (struct static_index
	  *) ((struct static_index *) (CharP (si) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN
struct logic_upd_index *LUIndexAdjust__ (struct logic_upd_index
					 *  CACHE_TYPE);

INLINE_ONLY inline EXTERN struct logic_upd_index *
LUIndexAdjust__ (struct logic_upd_index *lui USES_REGS)
{
  return (struct logic_upd_index
	  *) ((struct logic_upd_index *) (CharP (lui) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN Term CodeAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
CodeAdjust__ (Term dbr USES_REGS)
{
  return (Term) (CharP(dbr) + LOCAL_HDiff);
}



INLINE_ONLY inline EXTERN ADDR AddrAdjust__ (ADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN ADDR
AddrAdjust__ (ADDR addr USES_REGS)
{
  return (ADDR) ((ADDR) (CharP (addr) + LOCAL_HDiff));
}



INLINE_ONLY inline EXTERN CODEADDR CodeAddrAdjust__ (CODEADDR CACHE_TYPE);

INLINE_ONLY inline EXTERN CODEADDR
CodeAddrAdjust__ (CODEADDR addr USES_REGS)
{
  return (CODEADDR) ((CODEADDR) (CharP (addr) + LOCAL_HDiff));
}


INLINE_ONLY inline EXTERN char * CodeCharPAdjust__ (char * CACHE_TYPE);

INLINE_ONLY inline EXTERN char *
CodeCharPAdjust__ (char * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return addr + LOCAL_HDiff;
}

INLINE_ONLY inline EXTERN const char * CodeConstCharPAdjust__ (const char * CACHE_TYPE);

INLINE_ONLY inline EXTERN const char *
CodeConstCharPAdjust__ (const char * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return addr + LOCAL_HDiff;
}

INLINE_ONLY inline EXTERN void * CodeVoidPAdjust__ (void * CACHE_TYPE);

INLINE_ONLY inline EXTERN void *
CodeVoidPAdjust__ (void * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return (void *)((char *)addr + LOCAL_HDiff);
}

INLINE_ONLY inline EXTERN struct halt_hook *HaltHookAdjust__ (struct halt_hook * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct halt_hook *
HaltHookAdjust__ (struct halt_hook * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return  (struct halt_hook *) (CharP (addr) + LOCAL_HDiff);
}

INLINE_ONLY inline EXTERN BlockHeader *BlockAdjust__ (BlockHeader * CACHE_TYPE);

INLINE_ONLY inline EXTERN BlockHeader *
BlockAdjust__ (BlockHeader * addr USES_REGS)
{
  return (BlockHeader *) ((BlockHeader *) (CharP (addr) + LOCAL_HDiff));
}

INLINE_ONLY inline EXTERN yamop *PtoOpAdjust__ (yamop * CACHE_TYPE);

INLINE_ONLY inline EXTERN yamop *
PtoOpAdjust__ (yamop * ptr USES_REGS)
{
  if (ptr)
    return (yamop *) (CharP (ptr) + LOCAL_HDiff);
  return ptr;
}

INLINE_ONLY inline EXTERN struct operator_entry *OpListAdjust__ (struct operator_entry * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct operator_entry *
OpListAdjust__ (struct operator_entry * ptr USES_REGS)
{
  if (!ptr)
    return ptr;
  return (struct operator_entry *) (CharP (ptr) + LOCAL_HDiff);
}


INLINE_ONLY inline EXTERN struct logic_upd_clause *PtoLUClauseAdjust__ (struct logic_upd_clause * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct logic_upd_clause *
PtoLUClauseAdjust__ (struct logic_upd_clause * ptr USES_REGS)
{
  return (struct logic_upd_clause *) (CharP (ptr) + LOCAL_HDiff);
}

INLINE_ONLY inline EXTERN struct logic_upd_index *PtoLUIndexAdjust__ (struct logic_upd_index * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct logic_upd_index *
PtoLUIndexAdjust__ (struct logic_upd_index * ptr USES_REGS)
{
  return (struct logic_upd_index *) (CharP (ptr) + LOCAL_HDiff);
}



INLINE_ONLY inline EXTERN CELL *PtoHeapCellAdjust__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN CELL *
PtoHeapCellAdjust__ (CELL * ptr USES_REGS)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN AtomHashEntry *PtoAtomHashEntryAdjust__ (AtomHashEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN AtomHashEntry *
PtoAtomHashEntryAdjust__ (AtomHashEntry * ptr USES_REGS)
{
  return (AtomHashEntry *) (((AtomHashEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN Term TermToGlobalAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
TermToGlobalAdjust__ (Term t USES_REGS)
{
  if (t == 0L)
    return t;
  return AbsAppl(PtoGloAdjust(RepAppl(t)));
}

INLINE_ONLY inline EXTERN Term TermToGlobalOrAtomAdjust__ (Term CACHE_TYPE);

INLINE_ONLY inline EXTERN Term
TermToGlobalOrAtomAdjust__ (Term t USES_REGS)
{
  if (t == 0L)
    return t;
  if (IsAtomTerm(t))
    return AtomTermAdjust(t);
  if (IsApplTerm(t))
    return AbsAppl(PtoGloAdjust(RepAppl(t)));
  if (IsPairTerm(t))
    return AbsPair(PtoGloAdjust(RepPair(t)));
  return t;
}

#if USE_THREADED_CODE

INLINE_ONLY inline EXTERN op_entry *OpRTableAdjust__ (op_entry * CACHE_TYPE);

INLINE_ONLY inline EXTERN op_entry *
OpRTableAdjust__ (op_entry * ptr USES_REGS)
{
  return (op_entry *) (((op_entry *) (CharP (ptr) + LOCAL_HDiff)));
}

#endif // USE_THREADED_CODE

INLINE_ONLY inline EXTERN OpEntry *OpEntryAdjust__ (OpEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN OpEntry *
OpEntryAdjust__ (OpEntry * ptr USES_REGS)
{
  return (OpEntry *) (((OpEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN PredEntry *PtoPredAdjust__ (PredEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN PredEntry *
PtoPredAdjust__ (PredEntry * ptr USES_REGS)
{
  return (PredEntry *) (((PredEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

INLINE_ONLY inline EXTERN PredEntry **PtoPtoPredAdjust__ (PredEntry ** CACHE_TYPE);

INLINE_ONLY inline EXTERN PredEntry **
PtoPtoPredAdjust__ (PredEntry **ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (PredEntry **) (((PredEntry **) (CharP (ptr) + LOCAL_HDiff)));
}



INLINE_ONLY inline EXTERN ArrayEntry *PtoArrayEAdjust__ (ArrayEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN ArrayEntry *
PtoArrayEAdjust__ (ArrayEntry * ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (ArrayEntry *) (((ArrayEntry *) (CharP (ptr) + LOCAL_HDiff)));
}


INLINE_ONLY inline EXTERN GlobalEntry *PtoGlobalEAdjust__ (GlobalEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN GlobalEntry *
PtoGlobalEAdjust__ (GlobalEntry * ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (GlobalEntry *) (((GlobalEntry *) (CharP (ptr) + LOCAL_HDiff)));
}


INLINE_ONLY inline EXTERN StaticArrayEntry *PtoArraySAdjust__ (StaticArrayEntry * CACHE_TYPE);

INLINE_ONLY inline EXTERN StaticArrayEntry *
PtoArraySAdjust__ (StaticArrayEntry * ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (StaticArrayEntry *) (((StaticArrayEntry *) (CharP (ptr) + LOCAL_HDiff)));
}



INLINE_ONLY inline EXTERN struct logic_upd_clause *PtoLUCAdjust__ (struct logic_upd_clause* CACHE_TYPE);

INLINE_ONLY inline EXTERN struct logic_upd_clause *
PtoLUCAdjust__ (struct logic_upd_clause *ptr USES_REGS)
{
  return (struct logic_upd_clause
	  *) (((struct logic_upd_clause *) (CharP (ptr) + LOCAL_HDiff)));
}



INLINE_ONLY inline EXTERN struct static_clause *PtoStCAdjust__ (struct static_clause * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct static_clause *
PtoStCAdjust__ (struct static_clause *ptr USES_REGS)
{
  return (struct static_clause
	  *) (((struct static_upd_clause *) (CharP (ptr) + LOCAL_HDiff)));
}


INLINE_ONLY inline EXTERN struct dbterm_list *PtoDBTLAdjust__ (struct dbterm_list * CACHE_TYPE);

INLINE_ONLY inline EXTERN struct dbterm_list *
PtoDBTLAdjust__ (struct dbterm_list * addr USES_REGS)
{
  return (struct dbterm_list *) ((ADDR) (CharP (addr) + LOCAL_HDiff));
}


#if PRECOMPUTE_REGADDRESS

INLINE_ONLY inline EXTERN wamreg XAdjust__ (wamreg CACHE_TYPE);

INLINE_ONLY inline EXTERN wamreg
XAdjust__ (wamreg reg USES_REGS)
{
  return (wamreg) ((wamreg) ((reg) + LOCAL_XDiff));
}


#else

INLINE_ONLY inline EXTERN wamreg XAdjust__ (wamreg CACHE_TYPE);

INLINE_ONLY inline EXTERN wamreg
XAdjust__ (wamreg reg USES_REGS)
{
  return (wamreg) ((reg));
}


#endif

INLINE_ONLY inline EXTERN yslot YAdjust__ (yslot CACHE_TYPE);

INLINE_ONLY inline EXTERN yslot
YAdjust__ (yslot reg USES_REGS)
{
  return (yslot) ((reg));
}



INLINE_ONLY inline EXTERN int IsOldLocal__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldLocal__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldASP, reg, LOCAL_OldLCL0));
}



INLINE_ONLY inline EXTERN int IsOldLocalPtr__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldLocalPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldASP, ptr, LOCAL_OldLCL0));
}



/* require because the trail might contain dangling pointers */

INLINE_ONLY inline EXTERN int IsOldLocalInTR__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldLocalInTR__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldH, reg, LOCAL_OldLCL0));
}



INLINE_ONLY inline EXTERN int IsOldLocalInTRPtr__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldLocalInTRPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldH, ptr, LOCAL_OldLCL0));
}




INLINE_ONLY inline EXTERN int IsOldH__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldH__ (CELL reg USES_REGS)
{
  return (int) ((CharP (reg) == CharP (LOCAL_OldH)));
}





INLINE_ONLY inline EXTERN int IsOldGlobal__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldGlobal__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldGlobalBase, reg, LOCAL_OldH));
}


INLINE_ONLY inline EXTERN int IsOldDelay__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldDelay__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldGlobalBase, reg, LOCAL_OldH0));
}



INLINE_ONLY inline EXTERN int IsOldGlobalPtr__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldGlobalPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldGlobalBase, ptr, LOCAL_OldH));
}



INLINE_ONLY inline EXTERN int IsOldTrail__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldTrail__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, reg, LOCAL_OldTR));
}



INLINE_ONLY inline EXTERN int IsOldTrailPtr__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldTrailPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, ptr, LOCAL_OldTR));
}



INLINE_ONLY inline EXTERN int IsOldVarTableTrailPtr__ (struct VARSTRUCT * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldVarTableTrailPtr__ (struct VARSTRUCT *ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, ptr, LOCAL_OldTR));
}



INLINE_ONLY inline EXTERN int IsOldTokenTrailPtr__ (struct TOKEN * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldTokenTrailPtr__ (struct TOKEN *ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, ptr, LOCAL_OldTR));
}



INLINE_ONLY inline EXTERN int IsOldCode__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldCode__ (CELL reg USES_REGS)
{
#if USE_SYSTEM_MALLOC
  return reg < (CELL)LOCAL_OldGlobalBase || reg > (CELL)LOCAL_OldTrailTop;
#else 
  return (int) (IN_BETWEEN (LOCAL_OldHeapBase, reg, LOCAL_OldHeapTop));
#endif
}



INLINE_ONLY inline EXTERN int IsOldCodeCellPtr__ (CELL * CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsOldCodeCellPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldHeapBase, ptr, LOCAL_OldHeapTop));
}



INLINE_ONLY inline EXTERN int IsGlobal__ (CELL CACHE_TYPE);

INLINE_ONLY inline EXTERN int
IsGlobal__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_GlobalBase, reg, HR));
}


void Yap_AdjustStacksAndTrail(void);
void Yap_AdjustRegs(int);

#endif
