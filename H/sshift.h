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


inline EXTERN int IsHeapP (CELL *);

inline EXTERN int
IsHeapP (CELL * ptr)
{
#if USE_SYSTEM_MALLOC
  return (int) ((ptr < (CELL *) Yap_GlobalBase || ptr > (CELL *) Yap_TrailTop));
#else
  return (int) ((ptr >= (CELL *) Yap_HeapBase && ptr <= (CELL *) HeapTop));
#endif
}

#define OrArgAdjust(P) 
#define TabEntryAdjust(P) 

/* Adjusting cells and pointers to cells */

inline EXTERN CELL *PtoGloAdjust (CELL *);

inline EXTERN CELL *
PtoGloAdjust (CELL * ptr)
{
  if (ptr < GSplit) {
    if (ptr < H0) 
      return (CELL *) (((CELL *) (CharP (ptr) + DelayDiff)));
    else
      return (CELL *) (((CELL *) (CharP (ptr) + GDiff0)));
  } else {
    return (CELL *) (((CELL *) (CharP (ptr) + GDiff)));
  }
}



inline EXTERN CELL *PtoDelayAdjust (CELL *);

inline EXTERN CELL *
PtoDelayAdjust (CELL * ptr)
{
  if (!GSplit || ptr < GSplit)
    return (CELL *) (((CELL *) (CharP (ptr) + DelayDiff)));
  else
    return (CELL *) (((CELL *) (CharP (ptr) + GDiff0)));
}

inline EXTERN CELL *PtoBaseAdjust (CELL *);

inline EXTERN CELL *
PtoBaseAdjust (CELL * ptr)
{
    return (CELL *) (((CELL *) (CharP (ptr) + BaseDiff)));
}



inline EXTERN tr_fr_ptr PtoTRAdjust (tr_fr_ptr);

inline EXTERN tr_fr_ptr
PtoTRAdjust (tr_fr_ptr ptr)
{
  return (tr_fr_ptr) (((tr_fr_ptr) (CharP (ptr) + TrDiff)));
}



inline EXTERN CELL *CellPtoTRAdjust (CELL *);

inline EXTERN CELL *
CellPtoTRAdjust (CELL * ptr)
{
  return (CELL *) (((CELL *) (CharP (ptr) + TrDiff)));
}



inline EXTERN CELL *PtoLocAdjust (CELL *);

inline EXTERN CELL *
PtoLocAdjust (CELL * ptr)
{
  return (CELL *) (((CELL *) (CharP (ptr) + LDiff)));
}



inline EXTERN choiceptr ChoicePtrAdjust (choiceptr);

inline EXTERN choiceptr
ChoicePtrAdjust (choiceptr ptr)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LDiff)));
}


#ifdef TABLING

inline EXTERN choiceptr ConsumerChoicePtrAdjust (choiceptr);

inline EXTERN choiceptr
ConsumerChoicePtrAdjust (choiceptr ptr)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LDiff)));
}



inline EXTERN choiceptr GeneratorChoicePtrAdjust (choiceptr);

inline EXTERN choiceptr
GeneratorChoicePtrAdjust (choiceptr ptr)
{
  return (choiceptr) (((choiceptr) (CharP (ptr) + LDiff)));
}


#endif /* TABLING */


inline EXTERN CELL GlobalAdjust (CELL);

inline EXTERN CELL
GlobalAdjust (CELL val)
{
  if ((CELL *)val < GSplit) {
    if ((CELL *)val < H0) 
      return (CELL) (val + DelayDiff);
    else
      return (CELL) (val + GDiff0);
  } else {
    return (CELL) (val + GDiff);
  }
}



inline EXTERN CELL DelayAdjust (CELL);

inline EXTERN CELL
DelayAdjust (CELL val)
{
  if (!GSplit || (CELL *)val < GSplit)
    return (CELL) (val + DelayDiff);
  else
    return (CELL) (val + GDiff0);
}


inline EXTERN ADDR GlobalAddrAdjust (ADDR);

inline EXTERN ADDR
GlobalAddrAdjust (ADDR ptr)
{
  if ((CELL *)ptr < GSplit) {
    if ((CELL *)ptr < H0) 
      return (ADDR) (ptr + DelayDiff);
    else
      return (ADDR) ((ptr + GDiff0));
  } else {
    return (ADDR) ((ptr + GDiff));
  }
}




inline EXTERN ADDR DelayAddrAdjust (ADDR);

inline EXTERN ADDR
DelayAddrAdjust (ADDR ptr)
{
  if (!GSplit || (CELL *)ptr < GSplit)
    return (ADDR) ((ptr + DelayDiff));
  else
    return (ADDR) ((ptr + GDiff0));
}


inline EXTERN ADDR BaseAddrAdjust (ADDR);

inline EXTERN ADDR
BaseAddrAdjust (ADDR ptr)
{
  return (ADDR) ((ptr + BaseDiff));
}



inline EXTERN CELL LocalAdjust (CELL);

inline EXTERN CELL
LocalAdjust (CELL val)
{
  return (CELL) ((val + LDiff));
}



inline EXTERN ADDR LocalAddrAdjust (ADDR);

inline EXTERN ADDR
LocalAddrAdjust (ADDR ptr)
{
  return (ADDR) ((ptr + LDiff));
}



inline EXTERN CELL TrailAdjust (CELL);

inline EXTERN CELL
TrailAdjust (CELL val)
{
  return (CELL) ((val + TrDiff));
}



inline EXTERN ADDR TrailAddrAdjust (ADDR);

inline EXTERN ADDR
TrailAddrAdjust (ADDR ptr)
{
  return (ADDR) ((ptr + TrDiff));
}



inline EXTERN TokEntry *TokEntryAdjust (TokEntry *);

inline EXTERN TokEntry *
TokEntryAdjust (TokEntry * ptr)
{
  return (TokEntry *) (((CELL) ptr + TrDiff));
}



inline EXTERN VarEntry *VarEntryAdjust (VarEntry *);

inline EXTERN VarEntry *
VarEntryAdjust (VarEntry * ptr)
{
  return (VarEntry *) (((CELL) ptr + TrDiff));
}


/* heap data structures */

inline EXTERN Functor FuncAdjust (Functor);

inline EXTERN Functor
FuncAdjust (Functor f)
{
  if (!IsExtensionFunctor(f)) 
    return (Functor) ((CharP (f) + HDiff));
  return f;
}



inline EXTERN CELL *CellPtoHeapAdjust (CELL *);

inline EXTERN CELL *
CellPtoHeapAdjust (CELL * ptr)
{
  if (!ptr)
    return ptr;
  return (CELL *) (((CELL *) (CharP (ptr) + HDiff)));
}

inline EXTERN HoldEntry *HoldEntryAdjust (HoldEntry *);

inline EXTERN HoldEntry *
HoldEntryAdjust (HoldEntry * ptr)
{
  return (HoldEntry *) (((HoldEntry *) (CharP (ptr) + HDiff)));
}


#if	USE_OFFSETS

inline EXTERN Atom AtomAdjust (Atom);

inline EXTERN Atom
AtomAdjust (Atom at)
{
  return (Atom) ((at));
}



inline EXTERN Prop PropAdjust (Prop);

inline EXTERN Prop
PropAdjust (Prop p)
{
  return (Prop) ((p));
}


#else

inline EXTERN Atom AtomAdjust (Atom);

inline EXTERN Atom
AtomAdjust (Atom at)
{
  return (Atom) ((at == NULL ? (at) : (Atom) (CharP (at) + HDiff)));
}



inline EXTERN Prop PropAdjust (Prop);

inline EXTERN Prop
PropAdjust (Prop p)
{
  return (Prop) ((p == NULL ? (p) : (Prop) (CharP (p) + HDiff)));
}


#endif

inline EXTERN PredEntry *PredEntryAdjust (PredEntry *);

inline EXTERN PredEntry *
PredEntryAdjust (PredEntry *p)
{
  return (PredEntry *) ((p == NULL ? (p) : (PredEntry *) (CharP (p) + HDiff)));
}

inline EXTERN struct mod_entry *ModEntryPtrAdjust (struct mod_entry *);

inline EXTERN struct mod_entry *
ModEntryPtrAdjust (struct mod_entry *p)
{
  return (struct mod_entry *) ((p == NULL ? (p) : (struct mod_entry *) (CharP (p) + HDiff)));
}

inline EXTERN COUNT ConstantAdjust (COUNT);

inline EXTERN COUNT
ConstantAdjust (COUNT val)
{
  return val;
}

inline EXTERN Int ArityAdjust (Int);

inline EXTERN Int
ArityAdjust (Int val)
{
  return val;
}

inline EXTERN OPCODE OpcodeAdjust (OPCODE);

inline EXTERN OPCODE
OpcodeAdjust (OPCODE val)
{
  return Yap_opcode(Yap_op_from_opcode(val));
}

#define DoubleInCodeAdjust(D)

#define IntegerInCodeAdjust(D) 

#define IntegerAdjust(D)  (D)

#define ExternalFunctionAdjust(D) (D);

inline EXTERN Term AtomTermAdjust (Term);

inline EXTERN Term
AtomTermAdjust (Term at)
{
  return (Term)(CharP(at) + HDiff);
}

inline EXTERN Term ModuleAdjust (Term);

inline EXTERN Term
ModuleAdjust (Term t)
{
  return AtomTermAdjust(t);
}

inline EXTERN Term CodeVarAdjust (Term);

inline EXTERN Term
CodeVarAdjust (Term var)
{
  return (Term)(CharP(var) + HDiff);
}


#if TAGS_FAST_OPS

inline EXTERN Term BlobTermAdjust (Term);

inline EXTERN Term
BlobTermAdjust (Term t)
{
  return (Term) (CharP(t) - HDiff);
}


inline EXTERN Term CodeComposedTermAdjust (Term);

inline EXTERN Term
CodeComposedTermAdjust (Term t)
{
  return (Term) (CharP(t) - HDiff);
}


#else

inline EXTERN Term BlobTermAdjust (Term);

inline EXTERN Term
BlobTermAdjust (Term t)
{
  return (Term) (CharP(t) + HDiff);
}

inline EXTERN Term CodeComposedTermAdjust (Term);

inline EXTERN Term
CodeComposedTermAdjust (Term t)
{
  return (Term) (CharP(t) + HDiff);
}


#endif

inline EXTERN AtomEntry *AtomEntryAdjust (AtomEntry *);

inline EXTERN AtomEntry *
AtomEntryAdjust (AtomEntry * at)
{
  return (AtomEntry *) ((AtomEntry *) (CharP (at) + HDiff));
}

inline EXTERN struct mfile *MFileAdjust (struct mfile *);

inline EXTERN struct mfile *
MFileAdjust (struct mfile * at)
{
  return (struct mfile *) (CharP (at) + HDiff);
}

inline EXTERN GlobalEntry *GlobalEntryAdjust (GlobalEntry *);

inline EXTERN GlobalEntry *
GlobalEntryAdjust (GlobalEntry * at)
{
  return (GlobalEntry *) ((GlobalEntry *) (CharP (at) + HDiff));
}



inline EXTERN union CONSULT_OBJ *ConsultObjAdjust (union CONSULT_OBJ *);

inline EXTERN union CONSULT_OBJ *
ConsultObjAdjust (union CONSULT_OBJ *co)
{
  return (union CONSULT_OBJ *) ((union CONSULT_OBJ *) (CharP (co) + HDiff));
}



inline EXTERN DBRef DBRefAdjust (DBRef);

inline EXTERN DBRef
DBRefAdjust (DBRef dbr)
{
  return (DBRef) ((DBRef) (CharP (dbr) + HDiff));
}



inline EXTERN DBRef *DBRefPAdjust (DBRef *);

inline EXTERN DBRef *
DBRefPAdjust (DBRef * dbrp)
{
  return (DBRef *) ((DBRef *) (CharP (dbrp) + HDiff));
}



inline EXTERN DBTerm *DBTermAdjust (DBTerm *);

inline EXTERN DBTerm *
DBTermAdjust (DBTerm * dbtp)
{
  return (DBTerm *) ((DBTerm *) (CharP (dbtp) + HDiff));
}



inline EXTERN struct static_index *SIndexAdjust (struct static_index *);

inline EXTERN struct static_index *
SIndexAdjust (struct static_index *si)
{
  return (struct static_index
	  *) ((struct static_index *) (CharP (si) + HDiff));
}



inline EXTERN struct logic_upd_index *LUIndexAdjust (struct logic_upd_index
						     *);

inline EXTERN struct logic_upd_index *
LUIndexAdjust (struct logic_upd_index *lui)
{
  return (struct logic_upd_index
	  *) ((struct logic_upd_index *) (CharP (lui) + HDiff));
}



inline EXTERN Term CodeAdjust (Term);

inline EXTERN Term
CodeAdjust (Term dbr)
{
  return (Term) (CharP(dbr) + HDiff);
}



inline EXTERN ADDR AddrAdjust (ADDR);

inline EXTERN ADDR
AddrAdjust (ADDR addr)
{
  return (ADDR) ((ADDR) (CharP (addr) + HDiff));
}



inline EXTERN CODEADDR CodeAddrAdjust (CODEADDR);

inline EXTERN CODEADDR
CodeAddrAdjust (CODEADDR addr)
{
  return (CODEADDR) ((CODEADDR) (CharP (addr) + HDiff));
}


inline EXTERN char * CodeCharPAdjust (char *);

inline EXTERN char *
CodeCharPAdjust (char * addr)
{
  if (!addr)
    return NULL;
  return addr + HDiff;
}

inline EXTERN void * CodeVoidPAdjust (void *);

inline EXTERN void *
CodeVoidPAdjust (void * addr)
{
  if (!addr)
    return NULL;
  return addr + HDiff;
}



inline EXTERN BlockHeader *BlockAdjust (BlockHeader *);

inline EXTERN BlockHeader *
BlockAdjust (BlockHeader * addr)
{
  return (BlockHeader *) ((BlockHeader *) (CharP (addr) + HDiff));
}



inline EXTERN yamop *PtoOpAdjust (yamop *);

inline EXTERN yamop *
PtoOpAdjust (yamop * ptr)
{
  return (yamop *) (CharP (ptr) + HDiff);
}

inline EXTERN struct operator_entry *OpListAdjust (struct operator_entry *);

inline EXTERN struct operator_entry *
OpListAdjust (struct operator_entry * ptr)
{
  if (!ptr)
    return ptr;
  return (struct operator_entry *) (CharP (ptr) + HDiff);
}


inline EXTERN struct logic_upd_clause *PtoLUClauseAdjust (struct logic_upd_clause *);

inline EXTERN struct logic_upd_clause *
PtoLUClauseAdjust (struct logic_upd_clause * ptr)
{
  return (struct logic_upd_clause *) (CharP (ptr) + HDiff);
}

inline EXTERN struct logic_upd_index *PtoLUIndexAdjust (struct logic_upd_index *);

inline EXTERN struct logic_upd_index *
PtoLUIndexAdjust (struct logic_upd_index * ptr)
{
  return (struct logic_upd_index *) (CharP (ptr) + HDiff);
}



inline EXTERN CELL *PtoHeapCellAdjust (CELL *);

inline EXTERN CELL *
PtoHeapCellAdjust (CELL * ptr)
{
  return (CELL *) (((CELL *) (CharP (ptr) + HDiff)));
}

inline EXTERN AtomHashEntry *PtoAtomHashEntryAdjust (AtomHashEntry *);

inline EXTERN AtomHashEntry *
PtoAtomHashEntryAdjust (AtomHashEntry * ptr)
{
  return (AtomHashEntry *) (((AtomHashEntry *) (CharP (ptr) + HDiff)));
}



inline EXTERN opentry *OpRTableAdjust (opentry *);

inline EXTERN opentry *
OpRTableAdjust (opentry * ptr)
{
  return (opentry *) (((opentry *) (CharP (ptr) + HDiff)));
}

inline EXTERN PredEntry *PtoPredAdjust (PredEntry *);

inline EXTERN PredEntry *
PtoPredAdjust (PredEntry * ptr)
{
  return (PredEntry *) (((PredEntry *) (CharP (ptr) + HDiff)));
}

inline EXTERN PredEntry **PtoPtoPredAdjust (PredEntry **);

inline EXTERN PredEntry **
PtoPtoPredAdjust (PredEntry **ptr)
{
  if (!ptr)
    return NULL;
  return (PredEntry **) (((PredEntry **) (CharP (ptr) + HDiff)));
}



inline EXTERN ArrayEntry *PtoArrayEAdjust (ArrayEntry *);

inline EXTERN ArrayEntry *
PtoArrayEAdjust (ArrayEntry * ptr)
{
  return (ArrayEntry *) (((ArrayEntry *) (CharP (ptr) + HDiff)));
}


inline EXTERN GlobalEntry *PtoGlobalEAdjust (GlobalEntry *);

inline EXTERN GlobalEntry *
PtoGlobalEAdjust (GlobalEntry * ptr)
{
  return (GlobalEntry *) (((GlobalEntry *) (CharP (ptr) + HDiff)));
}


inline EXTERN StaticArrayEntry *PtoArraySAdjust (StaticArrayEntry *);

inline EXTERN StaticArrayEntry *
PtoArraySAdjust (StaticArrayEntry * ptr)
{
  return (StaticArrayEntry *) (((StaticArrayEntry *) (CharP (ptr) + HDiff)));
}



inline EXTERN struct logic_upd_clause *PtoLUCAdjust (struct logic_upd_clause
						     *);

inline EXTERN struct logic_upd_clause *
PtoLUCAdjust (struct logic_upd_clause *ptr)
{
  return (struct logic_upd_clause
	  *) (((struct logic_upd_clause *) (CharP (ptr) + HDiff)));
}



inline EXTERN struct static_clause *PtoStCAdjust (struct static_clause *);

inline EXTERN struct static_clause *
PtoStCAdjust (struct static_clause *ptr)
{
  return (struct static_clause
	  *) (((struct static_upd_clause *) (CharP (ptr) + HDiff)));
}


inline EXTERN struct dbterm_list *PtoDBTLAdjust (struct dbterm_list *);

inline EXTERN struct dbterm_list *
PtoDBTLAdjust (struct dbterm_list * addr)
{
  return (struct dbterm_list *) ((ADDR) (CharP (addr) + HDiff));
}


#if PRECOMPUTE_REGADDRESS

inline EXTERN wamreg XAdjust (wamreg);

inline EXTERN wamreg
XAdjust (wamreg reg)
{
  return (wamreg) ((wamreg) ((reg) + XDiff));
}


#else

inline EXTERN wamreg XAdjust (wamreg);

inline EXTERN wamreg
XAdjust (wamreg reg)
{
  return (wamreg) ((reg));
}


#endif

inline EXTERN yslot YAdjust (yslot);

inline EXTERN yslot
YAdjust (yslot reg)
{
  return (yslot) ((reg));
}



inline EXTERN int IsOldLocal (CELL);

inline EXTERN int
IsOldLocal (CELL reg)
{
  return (int) (IN_BETWEEN (OldASP, reg, OldLCL0));
}



inline EXTERN int IsOldLocalPtr (CELL *);

inline EXTERN int
IsOldLocalPtr (CELL * ptr)
{
  return (int) (IN_BETWEEN (OldASP, ptr, OldLCL0));
}



/* require because the trail might contain dangling pointers */

inline EXTERN int IsOldLocalInTR (CELL);

inline EXTERN int
IsOldLocalInTR (CELL reg)
{
  return (int) (IN_BETWEEN (OldH, reg, OldLCL0));
}



inline EXTERN int IsOldLocalInTRPtr (CELL *);

inline EXTERN int
IsOldLocalInTRPtr (CELL * ptr)
{
  return (int) (IN_BETWEEN (OldH, ptr, OldLCL0));
}




inline EXTERN int IsOldH (CELL);

inline EXTERN int
IsOldH (CELL reg)
{
  return (int) ((CharP (reg) == CharP (OldH)));
}





inline EXTERN int IsOldGlobal (CELL);

inline EXTERN int
IsOldGlobal (CELL reg)
{
  return (int) (IN_BETWEEN (OldGlobalBase, reg, OldH));
}


inline EXTERN int IsOldDelay (CELL);

inline EXTERN int
IsOldDelay (CELL reg)
{
  return (int) (IN_BETWEEN (OldGlobalBase, reg, OldH0));
}



inline EXTERN int IsOldGlobalPtr (CELL *);

inline EXTERN int
IsOldGlobalPtr (CELL * ptr)
{
  return (int) (IN_BETWEEN (OldGlobalBase, ptr, OldH));
}



inline EXTERN int IsOldTrail (CELL);

inline EXTERN int
IsOldTrail (CELL reg)
{
  return (int) (IN_BETWEEN (OldTrailBase, reg, OldTR));
}



inline EXTERN int IsOldTrailPtr (CELL *);

inline EXTERN int
IsOldTrailPtr (CELL * ptr)
{
  return (int) (IN_BETWEEN (OldTrailBase, ptr, OldTR));
}



inline EXTERN int IsOldVarTableTrailPtr (struct VARSTRUCT *);

inline EXTERN int
IsOldVarTableTrailPtr (struct VARSTRUCT *ptr)
{
  return (int) (IN_BETWEEN (OldTrailBase, ptr, OldTR));
}



inline EXTERN int IsOldTokenTrailPtr (struct TOKEN *);

inline EXTERN int
IsOldTokenTrailPtr (struct TOKEN *ptr)
{
  return (int) (IN_BETWEEN (OldTrailBase, ptr, OldTR));
}



inline EXTERN int IsOldCode (CELL);

inline EXTERN int
IsOldCode (CELL reg)
{
#if USE_SYSTEM_MALLOC
  return reg < (CELL)OldGlobalBase || reg > (CELL)OldTrailTop;
#else 
  return (int) (IN_BETWEEN (OldHeapBase, reg, OldHeapTop));
#endif
}



inline EXTERN int IsOldCodeCellPtr (CELL *);

inline EXTERN int
IsOldCodeCellPtr (CELL * ptr)
{
  return (int) (IN_BETWEEN (OldHeapBase, ptr, OldHeapTop));
}



inline EXTERN int IsGlobal (CELL);

inline EXTERN int
IsGlobal (CELL reg)
{
  return (int) (IN_BETWEEN (Yap_GlobalBase, reg, H));
}


void STD_PROTO (Yap_AdjustStacksAndTrail, (void));
void STD_PROTO (Yap_AdjustRegs, (int));

