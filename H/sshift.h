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

#define REINIT_LOCK(P) INIT_LOCK(P) 
#define REINIT_RWLOCK(P) INIT_RWLOCK(P) 


#define CharP(ptr)	((char *) (ptr))


#define CodeAdjust(P) CodeAdjust__(P PASS_REGS)
#define PtoTRAdjust(P) PtoTRAdjust__(P PASS_REGS)
#define BaseAddrAdjust(P) BaseAddrAdjust__(P PASS_REGS)
#define CutCAdjust(P) CutCAdjust__(P PASS_REGS)
#define ChoicePtrAdjust(P) ChoicePtrAdjust__(P PASS_REGS)
#define FuncAdjust(P) FuncAdjust__(P PASS_REGS)
#define AtomTermAdjust(P) AtomTermAdjust__(P PASS_REGS)
#define TermToGlobalOrAtomAdjust(P) TermToGlobalOrAtomAdjust__(P PASS_REGS)
#define AtomAdjust(P) AtomAdjust__(P PASS_REGS)
#define IsOldCode(P) IsOldCode__(P PASS_REGS)
#define IsOldLocal(P) IsOldLocal__(P PASS_REGS)
#define IsOldLocalPtr(P) IsOldLocalPtr__(P PASS_REGS)
#define IsOldCodeCellPtr(P) IsOldCodeCellPtr__(P PASS_REGS)
#define IsOldDelay(P) IsOldDelay__(P PASS_REGS)
#define IsOldDelayPtr(P) IsOldDelayPtr__(P PASS_REGS)
#define IsOldLocalInTR(P) IsOldLocalInTR__(P PASS_REGS)
#define IsOldLocalInTRPtr(P) IsOldLocalInTRPtr__(P PASS_REGS)
#define IsOldGlobal(P) IsOldGlobal__(P PASS_REGS)
#define IsOldGlobalPtr(P) IsOldGlobalPtr__(P PASS_REGS)
#define IsOldTrail(P) IsOldTrail__(P PASS_REGS)
#define IsOldTrailPtr(P) IsOldTrailPtr__(P PASS_REGS)
#define NoAGCAtomAdjust(P) NoAGCAtomAdjust__(P PASS_REGS)
// #define OrArgAdjust(P) OrArgAdjust__(P PASS_REGS) 
// #define TabEntryAdjust(P) TabEntryAdjust__(P PASS_REGS)
// #define IntegerAdjust(D)  IntegerAdjust__(P PASS_REGS)
#define AddrAdjust(P) AddrAdjust__(P PASS_REGS)
#define BlockAdjust(P) BlockAdjust__(P PASS_REGS)
#define CodeVarAdjust(P) CodeVarAdjust__(P PASS_REGS)
#define ConstantAdjust(P) ConstantAdjust__(P PASS_REGS)
#define ArityAdjust(P) ArityAdjust__(P PASS_REGS)
// #define DoubleInCodeAdjust(P) DoubleInCodeAdjust__(P PASS_REGS)
// #define IntegerInCodeAdjust(P) IntegerInCodeAdjust__(P PASS_REGS)
#define OpcodeAdjust(P) OpcodeAdjust__(P PASS_REGS)
#define ModuleAdjust(P) ModuleAdjust__(P PASS_REGS)
// #define ExternalFunctionAdjust(P) ExternalFunctionAdjust__(P PASS_REGS)
#define DBRecordAdjust(P) DBRecordAdjust__(P PASS_REGS)
#define PredEntryAdjust(P) PredEntryAdjust__(P PASS_REGS)
#define ModEntryPtrAdjust(P) ModEntryPtrAdjust__(P PASS_REGS)
#define AtomEntryAdjust(P) AtomEntryAdjust__(P PASS_REGS)
#define GlobalEntryAdjust(P) GlobalEntryAdjust__(P PASS_REGS)
#define BlobTermInCodeAdjust(P) BlobTermInCodeAdjust__(P PASS_REGS)
#define CellPtoHeapAdjust(P) CellPtoHeapAdjust__(P PASS_REGS)
#define PtoAtomHashEntryAdjust(P) PtoAtomHashEntryAdjust__(P PASS_REGS)
#define CellPtoHeapCellAdjust(P) CellPtoHeapCellAdjust__(P PASS_REGS)
#define CellPtoTRAdjust(P) CellPtoTRAdjust__(P PASS_REGS)
#define CodeAddrAdjust(P) CodeAddrAdjust__(P PASS_REGS)
#define ConsultObjAdjust(P) ConsultObjAdjust__(P PASS_REGS)
#define DelayAddrAdjust(P) DelayAddrAdjust__(P PASS_REGS)
#define DelayAdjust(P) DelayAdjust__(P PASS_REGS)
#define GlobalAdjust(P) GlobalAdjust__(P PASS_REGS)
#define DBRefAdjust(P,C) DBRefAdjust__(P PASS_REGS)
#define DBRefPAdjust(P) DBRefPAdjust__(P PASS_REGS)
#define DBTermAdjust(P) DBTermAdjust__(P PASS_REGS)
#define LUIndexAdjust(P) LUIndexAdjust__(P PASS_REGS)
#define SIndexAdjust(P) SIndexAdjust__(P PASS_REGS)
#define LocalAddrAdjust(P) LocalAddrAdjust__(P PASS_REGS)
#define GlobalAddrAdjust(P) GlobalAddrAdjust__(P PASS_REGS)
#define OpListAdjust(P) OpListAdjust__(P PASS_REGS)
#define PtoLUCAdjust(P) PtoLUCAdjust__(P PASS_REGS)
#define PtoStCAdjust(P) PtoStCAdjust__(P PASS_REGS)
#define PtoArrayEAdjust(P) PtoArrayEAdjust__(P PASS_REGS)
#define PtoArraySAdjust(P) PtoArraySAdjust__(P PASS_REGS)
#define PtoGlobalEAdjust(P) PtoGlobalEAdjust__(P PASS_REGS)
#define PtoDelayAdjust(P) PtoDelayAdjust__(P PASS_REGS)
#define PtoGloAdjust(P) PtoGloAdjust__(P PASS_REGS)
#define PtoLocAdjust(P) PtoLocAdjust__(P PASS_REGS)
#define PtoHeapCellAdjust(P) PtoHeapCellAdjust__(P PASS_REGS)
#define TermToGlobalAdjust(P) TermToGlobalAdjust__(P PASS_REGS)
#define PtoOpAdjust(P) PtoOpAdjust__(P PASS_REGS)
#define PtoLUClauseAdjust(P) PtoLUClauseAdjust__(P PASS_REGS)
#define PtoLUIndexAdjust(P) PtoLUIndexAdjust__(P PASS_REGS)
#define PtoDBTLAdjust(P) PtoDBTLAdjust__(P PASS_REGS)
#define PtoPredAdjust(P) PtoPredAdjust__(P PASS_REGS)
#define PtoPtoPredAdjust(P) PtoPtoPredAdjust__(P PASS_REGS)
#define OpRTableAdjust(P) OpRTableAdjust__(P PASS_REGS)
#define OpEntryAdjust(P) OpEntryAdjust__(P PASS_REGS)
#define PropAdjust(P) PropAdjust__(P PASS_REGS)
#define BlobTypeAdjust(P) BlobTypeAdjust__(P PASS_REGS)
#define TrailAddrAdjust(P) TrailAddrAdjust__(P PASS_REGS)
#define XAdjust(P) XAdjust__(P PASS_REGS)
#define YAdjust(P) YAdjust__(P PASS_REGS)
#define LocalAdjust(P) LocalAdjust__(P PASS_REGS)
#define TrailAdjust(P) TrailAdjust__(P PASS_REGS)
#define HoldEntryAdjust(P) HoldEntryAdjust__(P PASS_REGS)
#define CodeCharPAdjust(P) CodeCharPAdjust__(P PASS_REGS)
#define CodeVoidPAdjust(P) CodeVoidPAdjust__(P PASS_REGS)
#define HaltHookAdjust(P) HaltHookAdjust__(P PASS_REGS)
#define TokEntryAdjust(P) TokEntryAdjust__(P PASS_REGS)
#define VarEntryAdjust(P) VarEntryAdjust__(P PASS_REGS)
#define ConsumerChoicePtrAdjust(P) ConsumerChoicePtrAdjust__(P PASS_REGS)
#define GeneratorChoicePtrAdjust(P) GeneratorChoicePtrAdjust__(P PASS_REGS)
#define IsHeapP(P) IsHeapP__(P PASS_REGS)
#define IsOldVarTableTrailPtr(P) IsOldVarTableTrailPtr__(P PASS_REGS)
#define IsOldTokenTrailPtr(P) IsOldTokenTrailPtr__(P PASS_REGS)

inline EXTERN int IsHeapP__ (CELL * CACHE_TYPE);

inline EXTERN int
IsHeapP__ (CELL * ptr USES_REGS)
{
#if USE_SYSTEM_MALLOC
  return (int) ((ptr < (CELL *) LOCAL_GlobalBase || ptr > (CELL *) LOCAL_TrailTop));
#else
  return (int) ((ptr >= (CELL *) Yap_HeapBase && ptr <= (CELL *) HeapTop));
#endif
}

#define OrArgAdjust(P) 
#define TabEntryAdjust(P) 

/* Adjusting cells and pointers to cells */

inline EXTERN CELL *PtoGloAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
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



inline EXTERN CELL *PtoDelayAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
PtoDelayAdjust__ (CELL * ptr USES_REGS)
{
  if (!LOCAL_GSplit || ptr < LOCAL_GSplit)
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_DelayDiff)));
  else
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_GDiff0)));
}

inline EXTERN CELL *PtoBaseAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
PtoBaseAdjust__ (CELL * ptr USES_REGS)
{
    return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_BaseDiff)));
}



inline EXTERN tr_fr_ptr PtoTRAdjust__ (tr_fr_ptr CACHE_TYPE);

inline EXTERN tr_fr_ptr
PtoTRAdjust__ (tr_fr_ptr ptr USES_REGS)
{
  return (tr_fr_ptr) (((tr_fr_ptr) (CharP (ptr) + LOCAL_TrDiff)));
}



inline EXTERN CELL *CellPtoTRAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
CellPtoTRAdjust__ (CELL * ptr USES_REGS)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_TrDiff)));
}



inline EXTERN CELL *PtoLocAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
PtoLocAdjust__ (CELL * ptr USES_REGS)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_LDiff)));
}


inline EXTERN struct cut_c_str *CutCAdjust__ (struct cut_c_str * CACHE_TYPE);

inline EXTERN struct cut_c_str *
CutCAdjust__ (struct  cut_c_str * ptr USES_REGS)
{
  return (struct cut_c_str *) (CharP (ptr) + LOCAL_LDiff);
}



inline EXTERN choiceptr ChoicePtrAdjust__ (choiceptr CACHE_TYPE);

inline EXTERN choiceptr
ChoicePtrAdjust__ (choiceptr ptr USES_REGS)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LOCAL_LDiff)));
}


#ifdef TABLING

inline EXTERN choiceptr ConsumerChoicePtrAdjust__ (choiceptr CACHE_TYPE);

inline EXTERN choiceptr
ConsumerChoicePtrAdjust__ (choiceptr ptr USES_REGS)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LOCAL_LDiff)));
}



inline EXTERN choiceptr GeneratorChoicePtrAdjust__ (choiceptr CACHE_TYPE);

inline EXTERN choiceptr
GeneratorChoicePtrAdjust__ (choiceptr ptr USES_REGS)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LOCAL_LDiff)));
}


#endif /* TABLING */


inline EXTERN CELL GlobalAdjust__ (CELL CACHE_TYPE);

inline EXTERN CELL
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



inline EXTERN CELL DelayAdjust__ (CELL CACHE_TYPE);

inline EXTERN CELL
DelayAdjust__ (CELL val USES_REGS)
{
  if (!LOCAL_GSplit || (CELL *)val < LOCAL_GSplit)
    return (CELL) (val + LOCAL_DelayDiff);
  else
    return (CELL) (val + LOCAL_GDiff0);
}


inline EXTERN ADDR GlobalAddrAdjust__ (ADDR CACHE_TYPE);

inline EXTERN ADDR
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




inline EXTERN ADDR DelayAddrAdjust__ (ADDR CACHE_TYPE);

inline EXTERN ADDR
DelayAddrAdjust__ (ADDR ptr USES_REGS)
{
  if (!LOCAL_GSplit || (CELL *)ptr < LOCAL_GSplit)
    return (ADDR) ((ptr + LOCAL_DelayDiff));
  else
    return (ADDR) ((ptr + LOCAL_GDiff0));
}


inline EXTERN ADDR BaseAddrAdjust__ (ADDR CACHE_TYPE);

inline EXTERN ADDR
BaseAddrAdjust__ (ADDR ptr USES_REGS)
{
  return (ADDR) ((ptr + LOCAL_BaseDiff));
}



inline EXTERN CELL LocalAdjust__ (CELL CACHE_TYPE);

inline EXTERN CELL
LocalAdjust__ (CELL val USES_REGS)
{
  return (CELL) ((val + LOCAL_LDiff));
}



inline EXTERN ADDR LocalAddrAdjust__ (ADDR CACHE_TYPE);

inline EXTERN ADDR
LocalAddrAdjust__ (ADDR ptr USES_REGS)
{
  return (ADDR) ((ptr + LOCAL_LDiff));
}



inline EXTERN CELL TrailAdjust__ (CELL CACHE_TYPE);

inline EXTERN CELL
TrailAdjust__ (CELL val USES_REGS)
{
  return (CELL) ((val + LOCAL_TrDiff));
}



inline EXTERN ADDR TrailAddrAdjust__ (ADDR CACHE_TYPE);

inline EXTERN ADDR
TrailAddrAdjust__ (ADDR ptr USES_REGS)
{
  return (ADDR) ((ptr + LOCAL_TrDiff));
}



inline EXTERN TokEntry *TokEntryAdjust__ (TokEntry * CACHE_TYPE);

inline EXTERN TokEntry *
TokEntryAdjust__ (TokEntry * ptr USES_REGS)
{
  return (TokEntry *) (((CELL) ptr + LOCAL_TrDiff));
}



inline EXTERN VarEntry *VarEntryAdjust__ (VarEntry * CACHE_TYPE);

inline EXTERN VarEntry *
VarEntryAdjust__ (VarEntry * ptr USES_REGS)
{
  return (VarEntry *) (((CELL) ptr + LOCAL_TrDiff));
}


/* heap data structures */

inline EXTERN Functor FuncAdjust__ (Functor CACHE_TYPE);

inline EXTERN Functor
FuncAdjust__ (Functor f USES_REGS)
{
  if (!IsExtensionFunctor(f)) 
    return (Functor) ((CharP (f) + LOCAL_HDiff));
  return f;
}

inline EXTERN CELL *CellPtoHeapAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
CellPtoHeapAdjust__ (CELL * ptr USES_REGS)
{
  if (!ptr)
    return ptr;
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN HoldEntry *HoldEntryAdjust__ (HoldEntry * CACHE_TYPE);

inline EXTERN HoldEntry *
HoldEntryAdjust__ (HoldEntry * ptr USES_REGS)
{
  return (HoldEntry *) (((HoldEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN struct record_list *DBRecordAdjust__ (struct record_list * CACHE_TYPE);

inline EXTERN struct record_list *
DBRecordAdjust__ (struct record_list * ptr USES_REGS)
{
  if (!ptr)
    return ptr;
  return (struct record_list *) (CharP (ptr) + LOCAL_HDiff);
}


#if	USE_OFFSETS

inline EXTERN Atom AtomAdjust__ (Atom CACHE_TYPE);

inline EXTERN Atom
AtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at));
}

inline EXTERN Atom NoAGCAtomAdjust__ (Atom CACHE_TYPE);

inline EXTERN Atom
NoAGCAtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at));
}



inline EXTERN Prop PropAdjust__ (Prop CACHE_TYPE);

inline EXTERN Prop
PropAdjust__ (Prop p USES_REGS)
{
  return (Prop) ((p));
}

#else

inline EXTERN Atom AtomAdjust__ (Atom CACHE_TYPE);

inline EXTERN Atom
AtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at == NULL ? (at) : (Atom) (CharP (at) + LOCAL_HDiff)));
}

inline EXTERN Atom NoAGCAtomAdjust__ (Atom CACHE_TYPE);

inline EXTERN Atom
NoAGCAtomAdjust__ (Atom at USES_REGS)
{
  return (Atom) ((at == NULL ? (at) : (Atom) (CharP (at) + LOCAL_HDiff)));
}



inline EXTERN Prop PropAdjust__ (Prop CACHE_TYPE);

inline EXTERN Prop
PropAdjust__ (Prop p USES_REGS)
{
  return (Prop) ((p == NULL ? (p) : (Prop) (CharP (p) + LOCAL_HDiff)));
}


#endif

inline EXTERN struct PL_blob_t *BlobTypeAdjust__ (struct PL_blob_t *CACHE_TYPE);

inline EXTERN struct PL_blob_t *
BlobTypeAdjust__ (struct PL_blob_t *at USES_REGS)
{
  return (struct PL_blob_t *) ((at == NULL ? (at) : (struct PL_blob_t *) (CharP (at) + LOCAL_HDiff)));
}

inline EXTERN PredEntry *PredEntryAdjust__ (PredEntry * CACHE_TYPE);

inline EXTERN PredEntry *
PredEntryAdjust__ (PredEntry *p USES_REGS)
{
  return (PredEntry *) ((p == NULL ? (p) : (PredEntry *) (CharP (p) + LOCAL_HDiff)));
}

inline EXTERN struct mod_entry *ModEntryPtrAdjust__ (struct mod_entry * CACHE_TYPE);

inline EXTERN struct mod_entry *
ModEntryPtrAdjust__ (struct mod_entry *p USES_REGS)
{
  return (struct mod_entry *) ((p == NULL ? (p) : (struct mod_entry *) (CharP (p) + LOCAL_HDiff)));
}

inline EXTERN COUNT ConstantAdjust__ (COUNT CACHE_TYPE);

inline EXTERN COUNT
ConstantAdjust__ (COUNT val USES_REGS)
{
  return val;
}

inline EXTERN Int ArityAdjust__ (Int CACHE_TYPE);

inline EXTERN Int
ArityAdjust__ (Int val USES_REGS)
{
  return val;
}

inline EXTERN OPCODE OpcodeAdjust__ (OPCODE CACHE_TYPE);

inline EXTERN OPCODE
OpcodeAdjust__ (OPCODE val USES_REGS)
{
  return Yap_opcode(Yap_op_from_opcode(val));
}

#define DoubleInCodeAdjust(D)

#define IntegerInCodeAdjust(D) 

#define IntegerAdjust(D)  (D)

#define ExternalFunctionAdjust(D) (D);

inline EXTERN Term AtomTermAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
AtomTermAdjust__ (Term at USES_REGS)
{
  if (at == 0L)
    return at;
  return (Term)(CharP(at) + LOCAL_HDiff);
}

inline EXTERN Term ModuleAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
ModuleAdjust__ (Term t USES_REGS)
{
  return AtomTermAdjust(t);
}

inline EXTERN Term CodeVarAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
CodeVarAdjust__ (Term var USES_REGS)
{
  if (var == 0L)
    return var;
  return (Term)(CharP(var) + LOCAL_HDiff);
}


#if TAGS_FAST_OPS

inline EXTERN Term BlobTermInCodeAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
BlobTermInCodeAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) - LOCAL_HDiff);
}


inline EXTERN Term CodeComposedTermAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
CodeComposedTermAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) - LOCAL_HDiff);
}


#else

inline EXTERN Term BlobTermInCodeAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
BlobTermInCodeAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) + LOCAL_HDiff);
}

inline EXTERN Term CodeComposedTermAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
CodeComposedTermAdjust__ (Term t USES_REGS)
{
  return (Term) (CharP(t) + LOCAL_HDiff);
}


#endif

inline EXTERN AtomEntry *AtomEntryAdjust__ (AtomEntry * CACHE_TYPE);

inline EXTERN AtomEntry *
AtomEntryAdjust__ (AtomEntry * at USES_REGS)
{
  return (AtomEntry *) ((AtomEntry *) (CharP (at) + LOCAL_HDiff));
}

inline EXTERN GlobalEntry *GlobalEntryAdjust__ (GlobalEntry * CACHE_TYPE);

inline EXTERN GlobalEntry *
GlobalEntryAdjust__ (GlobalEntry * at USES_REGS)
{
  return (GlobalEntry *) ((GlobalEntry *) (CharP (at) + LOCAL_HDiff));
}



inline EXTERN union CONSULT_OBJ *ConsultObjAdjust__ (union CONSULT_OBJ * CACHE_TYPE);

inline EXTERN union CONSULT_OBJ *
ConsultObjAdjust__ (union CONSULT_OBJ *co USES_REGS)
{
  return (union CONSULT_OBJ *) ((union CONSULT_OBJ *) (CharP (co) + LOCAL_HDiff));
}



inline EXTERN DBRef DBRefAdjust__ (DBRef CACHE_TYPE);

inline EXTERN DBRef
DBRefAdjust__ (DBRef dbr USES_REGS)
{
  return (DBRef) ((DBRef) (CharP (dbr) + LOCAL_HDiff));
}



inline EXTERN DBRef *DBRefPAdjust__ (DBRef * CACHE_TYPE);

inline EXTERN DBRef *
DBRefPAdjust__ (DBRef * dbrp USES_REGS)
{
  return (DBRef *) ((DBRef *) (CharP (dbrp) + LOCAL_HDiff));
}



inline EXTERN DBTerm *DBTermAdjust__ (DBTerm * CACHE_TYPE);

inline EXTERN DBTerm *
DBTermAdjust__ (DBTerm * dbtp USES_REGS)
{
  return (DBTerm *) ((DBTerm *) (CharP (dbtp) + LOCAL_HDiff));
}



inline EXTERN struct static_index *SIndexAdjust__ (struct static_index * CACHE_TYPE);

inline EXTERN struct static_index *
SIndexAdjust__ (struct static_index *si USES_REGS)
{
  return (struct static_index
	  *) ((struct static_index *) (CharP (si) + LOCAL_HDiff));
}



inline EXTERN struct logic_upd_index *LUIndexAdjust__ (struct logic_upd_index
						     *  CACHE_TYPE);

inline EXTERN struct logic_upd_index *
LUIndexAdjust__ (struct logic_upd_index *lui USES_REGS)
{
  return (struct logic_upd_index
	  *) ((struct logic_upd_index *) (CharP (lui) + LOCAL_HDiff));
}



inline EXTERN Term CodeAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
CodeAdjust__ (Term dbr USES_REGS)
{
  return (Term) (CharP(dbr) + LOCAL_HDiff);
}



inline EXTERN ADDR AddrAdjust__ (ADDR CACHE_TYPE);

inline EXTERN ADDR
AddrAdjust__ (ADDR addr USES_REGS)
{
  return (ADDR) ((ADDR) (CharP (addr) + LOCAL_HDiff));
}



inline EXTERN CODEADDR CodeAddrAdjust__ (CODEADDR CACHE_TYPE);

inline EXTERN CODEADDR
CodeAddrAdjust__ (CODEADDR addr USES_REGS)
{
  return (CODEADDR) ((CODEADDR) (CharP (addr) + LOCAL_HDiff));
}


inline EXTERN char * CodeCharPAdjust__ (char * CACHE_TYPE);

inline EXTERN char *
CodeCharPAdjust__ (char * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return addr + LOCAL_HDiff;
}

inline EXTERN void * CodeVoidPAdjust__ (void * CACHE_TYPE);

inline EXTERN void *
CodeVoidPAdjust__ (void * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return addr + LOCAL_HDiff;
}

inline EXTERN struct halt_hook *HaltHookAdjust__ (struct halt_hook * CACHE_TYPE);

inline EXTERN struct halt_hook *
HaltHookAdjust__ (struct halt_hook * addr USES_REGS)
{
  if (!addr)
    return NULL;
  return  (struct halt_hook *) (CharP (addr) + LOCAL_HDiff);
}

inline EXTERN BlockHeader *BlockAdjust__ (BlockHeader * CACHE_TYPE);

inline EXTERN BlockHeader *
BlockAdjust__ (BlockHeader * addr USES_REGS)
{
  return (BlockHeader *) ((BlockHeader *) (CharP (addr) + LOCAL_HDiff));
}

inline EXTERN yamop *PtoOpAdjust__ (yamop * CACHE_TYPE);

inline EXTERN yamop *
PtoOpAdjust__ (yamop * ptr USES_REGS)
{
  if (ptr)
    return (yamop *) (CharP (ptr) + LOCAL_HDiff);
  return ptr;
}

inline EXTERN struct operator_entry *OpListAdjust__ (struct operator_entry * CACHE_TYPE);

inline EXTERN struct operator_entry *
OpListAdjust__ (struct operator_entry * ptr USES_REGS)
{
  if (!ptr)
    return ptr;
  return (struct operator_entry *) (CharP (ptr) + LOCAL_HDiff);
}


inline EXTERN struct logic_upd_clause *PtoLUClauseAdjust__ (struct logic_upd_clause * CACHE_TYPE);

inline EXTERN struct logic_upd_clause *
PtoLUClauseAdjust__ (struct logic_upd_clause * ptr USES_REGS)
{
  return (struct logic_upd_clause *) (CharP (ptr) + LOCAL_HDiff);
}

inline EXTERN struct logic_upd_index *PtoLUIndexAdjust__ (struct logic_upd_index * CACHE_TYPE);

inline EXTERN struct logic_upd_index *
PtoLUIndexAdjust__ (struct logic_upd_index * ptr USES_REGS)
{
  return (struct logic_upd_index *) (CharP (ptr) + LOCAL_HDiff);
}



inline EXTERN CELL *PtoHeapCellAdjust__ (CELL * CACHE_TYPE);

inline EXTERN CELL *
PtoHeapCellAdjust__ (CELL * ptr USES_REGS)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN AtomHashEntry *PtoAtomHashEntryAdjust__ (AtomHashEntry * CACHE_TYPE);

inline EXTERN AtomHashEntry *
PtoAtomHashEntryAdjust__ (AtomHashEntry * ptr USES_REGS)
{
  return (AtomHashEntry *) (((AtomHashEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN Term TermToGlobalAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
TermToGlobalAdjust__ (Term t USES_REGS)
{
  if (t == 0L)
    return t;
  return AbsAppl(PtoGloAdjust(RepAppl(t)));
}

inline EXTERN Term TermToGlobalOrAtomAdjust__ (Term CACHE_TYPE);

inline EXTERN Term
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

inline EXTERN opentry *OpRTableAdjust__ (opentry * CACHE_TYPE);

inline EXTERN opentry *
OpRTableAdjust__ (opentry * ptr USES_REGS)
{
  return (opentry *) (((opentry *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN OpEntry *OpEntryAdjust__ (OpEntry * CACHE_TYPE);

inline EXTERN OpEntry *
OpEntryAdjust__ (OpEntry * ptr USES_REGS)
{
  return (OpEntry *) (((OpEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN PredEntry *PtoPredAdjust__ (PredEntry * CACHE_TYPE);

inline EXTERN PredEntry *
PtoPredAdjust__ (PredEntry * ptr USES_REGS)
{
  return (PredEntry *) (((PredEntry *) (CharP (ptr) + LOCAL_HDiff)));
}

inline EXTERN PredEntry **PtoPtoPredAdjust__ (PredEntry ** CACHE_TYPE);

inline EXTERN PredEntry **
PtoPtoPredAdjust__ (PredEntry **ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (PredEntry **) (((PredEntry **) (CharP (ptr) + LOCAL_HDiff)));
}



inline EXTERN ArrayEntry *PtoArrayEAdjust__ (ArrayEntry * CACHE_TYPE);

inline EXTERN ArrayEntry *
PtoArrayEAdjust__ (ArrayEntry * ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (ArrayEntry *) (((ArrayEntry *) (CharP (ptr) + LOCAL_HDiff)));
}


inline EXTERN GlobalEntry *PtoGlobalEAdjust__ (GlobalEntry * CACHE_TYPE);

inline EXTERN GlobalEntry *
PtoGlobalEAdjust__ (GlobalEntry * ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (GlobalEntry *) (((GlobalEntry *) (CharP (ptr) + LOCAL_HDiff)));
}


inline EXTERN StaticArrayEntry *PtoArraySAdjust__ (StaticArrayEntry * CACHE_TYPE);

inline EXTERN StaticArrayEntry *
PtoArraySAdjust__ (StaticArrayEntry * ptr USES_REGS)
{
  if (!ptr)
    return NULL;
  return (StaticArrayEntry *) (((StaticArrayEntry *) (CharP (ptr) + LOCAL_HDiff)));
}



inline EXTERN struct logic_upd_clause *PtoLUCAdjust__ (struct logic_upd_clause* CACHE_TYPE);

inline EXTERN struct logic_upd_clause *
PtoLUCAdjust__ (struct logic_upd_clause *ptr USES_REGS)
{
  return (struct logic_upd_clause
	  *) (((struct logic_upd_clause *) (CharP (ptr) + LOCAL_HDiff)));
}



inline EXTERN struct static_clause *PtoStCAdjust__ (struct static_clause * CACHE_TYPE);

inline EXTERN struct static_clause *
PtoStCAdjust__ (struct static_clause *ptr USES_REGS)
{
  return (struct static_clause
	  *) (((struct static_upd_clause *) (CharP (ptr) + LOCAL_HDiff)));
}


inline EXTERN struct dbterm_list *PtoDBTLAdjust__ (struct dbterm_list * CACHE_TYPE);

inline EXTERN struct dbterm_list *
PtoDBTLAdjust__ (struct dbterm_list * addr USES_REGS)
{
  return (struct dbterm_list *) ((ADDR) (CharP (addr) + LOCAL_HDiff));
}


#if PRECOMPUTE_REGADDRESS

inline EXTERN wamreg XAdjust__ (wamreg CACHE_TYPE);

inline EXTERN wamreg
XAdjust__ (wamreg reg USES_REGS)
{
  return (wamreg) ((wamreg) ((reg) + LOCAL_XDiff));
}


#else

inline EXTERN wamreg XAdjust__ (wamreg CACHE_TYPE);

inline EXTERN wamreg
XAdjust__ (wamreg reg USES_REGS)
{
  return (wamreg) ((reg));
}


#endif

inline EXTERN yslot YAdjust__ (yslot CACHE_TYPE);

inline EXTERN yslot
YAdjust__ (yslot reg USES_REGS)
{
  return (yslot) ((reg));
}



inline EXTERN int IsOldLocal__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldLocal__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldASP, reg, LOCAL_OldLCL0));
}



inline EXTERN int IsOldLocalPtr__ (CELL * CACHE_TYPE);

inline EXTERN int
IsOldLocalPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldASP, ptr, LOCAL_OldLCL0));
}



/* require because the trail might contain dangling pointers */

inline EXTERN int IsOldLocalInTR__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldLocalInTR__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldH, reg, LOCAL_OldLCL0));
}



inline EXTERN int IsOldLocalInTRPtr__ (CELL * CACHE_TYPE);

inline EXTERN int
IsOldLocalInTRPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldH, ptr, LOCAL_OldLCL0));
}




inline EXTERN int IsOldH__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldH__ (CELL reg USES_REGS)
{
  return (int) ((CharP (reg) == CharP (LOCAL_OldH)));
}





inline EXTERN int IsOldGlobal__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldGlobal__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldGlobalBase, reg, LOCAL_OldH));
}


inline EXTERN int IsOldDelay__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldDelay__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldGlobalBase, reg, LOCAL_OldH0));
}



inline EXTERN int IsOldGlobalPtr__ (CELL * CACHE_TYPE);

inline EXTERN int
IsOldGlobalPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldGlobalBase, ptr, LOCAL_OldH));
}



inline EXTERN int IsOldTrail__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldTrail__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, reg, LOCAL_OldTR));
}



inline EXTERN int IsOldTrailPtr__ (CELL * CACHE_TYPE);

inline EXTERN int
IsOldTrailPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, ptr, LOCAL_OldTR));
}



inline EXTERN int IsOldVarTableTrailPtr__ (struct VARSTRUCT * CACHE_TYPE);

inline EXTERN int
IsOldVarTableTrailPtr__ (struct VARSTRUCT *ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, ptr, LOCAL_OldTR));
}



inline EXTERN int IsOldTokenTrailPtr__ (struct TOKEN * CACHE_TYPE);

inline EXTERN int
IsOldTokenTrailPtr__ (struct TOKEN *ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldTrailBase, ptr, LOCAL_OldTR));
}



inline EXTERN int IsOldCode__ (CELL CACHE_TYPE);

inline EXTERN int
IsOldCode__ (CELL reg USES_REGS)
{
#if USE_SYSTEM_MALLOC
  return reg < (CELL)LOCAL_OldGlobalBase || reg > (CELL)LOCAL_OldTrailTop;
#else 
  return (int) (IN_BETWEEN (LOCAL_OldHeapBase, reg, LOCAL_OldHeapTop));
#endif
}



inline EXTERN int IsOldCodeCellPtr__ (CELL * CACHE_TYPE);

inline EXTERN int
IsOldCodeCellPtr__ (CELL * ptr USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_OldHeapBase, ptr, LOCAL_OldHeapTop));
}



inline EXTERN int IsGlobal__ (CELL CACHE_TYPE);

inline EXTERN int
IsGlobal__ (CELL reg USES_REGS)
{
  return (int) (IN_BETWEEN (LOCAL_GlobalBase, reg, H));
}


void STD_PROTO (Yap_AdjustStacksAndTrail, (void));
void STD_PROTO (Yap_AdjustRegs, (int));

