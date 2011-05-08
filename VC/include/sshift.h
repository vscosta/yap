






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


/* The difference between the old stack pointers and the new ones */
extern Int LOCAL_HDiff,
  LOCAL_GDiff,
  LOCAL_LDiff,
  LOCAL_TrDiff,
  LOCAL_XDiff,
  LOCAL_DelayDiff;

/* The old stack pointers */
extern CELL    *LOCAL_OldASP, *LOCAL_OldLCL0;
extern tr_fr_ptr LOCAL_OldTR;
extern CELL    *LOCAL_OldGlobalBase, *LOCAL_OldH, *LOCAL_OldH0;
extern ADDR     LOCAL_OldTrailBase, LOCAL_OldTrailTop;
extern ADDR     LOCAL_OldHeapBase, LOCAL_OldHeapTop;

#define CharP(ptr)	((char *) (ptr))


inline EXTERN int IsHeapP(CELL *);

inline EXTERN int IsHeapP(CELL * ptr)
{
	return (int) ((ptr >= (CELL *)HeapBase && ptr <= (CELL *)HeapTop) );
}



/* Adjusting cells and pointers to cells */

inline EXTERN CELL * PtoGloAdjust(CELL *);

inline EXTERN CELL * PtoGloAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LOCAL_GDiff)) );
}



inline EXTERN CELL * PtoDelayAdjust(CELL *);

inline EXTERN CELL * PtoDelayAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LOCAL_DelayDiff)) );
}



inline EXTERN tr_fr_ptr PtoTRAdjust(tr_fr_ptr);

inline EXTERN tr_fr_ptr PtoTRAdjust(tr_fr_ptr ptr)
{
	return (tr_fr_ptr) (((tr_fr_ptr)(CharP(ptr) + LOCAL_TrDiff)) );
}



inline EXTERN CELL * CellPtoTRAdjust(CELL *);

inline EXTERN CELL * CellPtoTRAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LOCAL_TrDiff)) );
}



inline EXTERN CELL * PtoLocAdjust(CELL *);

inline EXTERN CELL * PtoLocAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LOCAL_LDiff)) );
}



inline EXTERN choiceptr ChoicePtrAdjust(choiceptr);

inline EXTERN choiceptr ChoicePtrAdjust(choiceptr ptr)
{
	return (choiceptr) (((choiceptr)(CharP(ptr) + LOCAL_LDiff)) );
}


#ifdef TABLING

inline EXTERN choiceptr ConsumerChoicePtrAdjust(choiceptr);

inline EXTERN choiceptr ConsumerChoicePtrAdjust(choiceptr ptr)
{
	return (choiceptr) (((choiceptr)(CharP(ptr) + LOCAL_LDiff)) );
}



inline EXTERN choiceptr GeneratorChoicePtrAdjust(choiceptr);

inline EXTERN choiceptr GeneratorChoicePtrAdjust(choiceptr ptr)
{
	return (choiceptr) (((choiceptr)(CharP(ptr) + LOCAL_LDiff)) );
}


#endif /* TABLING */


inline EXTERN CELL GlobalAdjust(CELL);

inline EXTERN CELL GlobalAdjust(CELL val)
{
	return (CELL) ((val+LOCAL_GDiff) );
}



inline EXTERN CELL DelayAdjust(CELL);

inline EXTERN CELL DelayAdjust(CELL val)
{
	return (CELL) ((val+LOCAL_DelayDiff) );
}



inline EXTERN ADDR GlobalAddrAdjust(ADDR);

inline EXTERN ADDR GlobalAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+LOCAL_GDiff) );
}



inline EXTERN ADDR DelayAddrAdjust(ADDR);

inline EXTERN ADDR DelayAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+LOCAL_DelayDiff) );
}



inline EXTERN CELL LocalAdjust(CELL);

inline EXTERN CELL LocalAdjust(CELL val)
{
	return (CELL) ((val+LOCAL_LDiff) );
}



inline EXTERN ADDR LocalAddrAdjust(ADDR);

inline EXTERN ADDR LocalAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+LOCAL_LDiff) );
}



inline EXTERN CELL TrailAdjust(CELL);

inline EXTERN CELL TrailAdjust(CELL val)
{
	return (CELL) ((val+LOCAL_TrDiff) );
}



inline EXTERN ADDR TrailAddrAdjust(ADDR);

inline EXTERN ADDR TrailAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+LOCAL_TrDiff) );
}


/* heap data structures */

inline EXTERN Functor FuncAdjust(Functor);

inline EXTERN Functor FuncAdjust(Functor f)
{
	return (Functor) ((Functor)(CharP(f)+LOCAL_HDiff) );
}



inline EXTERN CELL * CellPtoHeapAdjust(CELL *);

inline EXTERN CELL * CellPtoHeapAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LOCAL_HDiff)) );
}


#if	USE_OFFSETS

inline EXTERN Atom AtomAdjust(Atom);

inline EXTERN Atom AtomAdjust(Atom at)
{
	return (Atom) ((at) );
}



inline EXTERN Prop PropAdjust(Prop);

inline EXTERN Prop PropAdjust(Prop p)
{
	return (Prop) ((p) );
}


#else

inline EXTERN Atom AtomAdjust(Atom);

inline EXTERN Atom AtomAdjust(Atom at)
{
	return (Atom) ((at == NULL ? (at) : (Atom)(CharP(at)+LOCAL_HDiff) ));
}



inline EXTERN Prop PropAdjust(Prop);

inline EXTERN Prop PropAdjust(Prop p)
{
	return (Prop) ((p == NULL ? (p) : (Prop)(CharP(p)+LOCAL_HDiff)) );
}


#endif

inline EXTERN Term AtomTermAdjust(Term);

inline EXTERN Term AtomTermAdjust(Term at)
{
	return (Term) ((at) );
}


#if TAGS_FAST_OPS

inline EXTERN Term BlobTermAdjust(Term);

inline EXTERN Term BlobTermAdjust(Term t)
{
	return (Term) ((t-LOCAL_HDiff) );
}


#else

inline EXTERN Term BlobTermAdjust(Term);

inline EXTERN Term BlobTermAdjust(Term t)
{
	return (Term) ((t+LOCAL_HDiff) );
}


#endif

inline EXTERN AtomEntry * AtomEntryAdjust(AtomEntry *);

inline EXTERN AtomEntry * AtomEntryAdjust(AtomEntry * at)
{
	return (AtomEntry *) ((AtomEntry *)(CharP(at)+LOCAL_HDiff) );
}



inline EXTERN union CONSULT_OBJ * ConsultObjAdjust(union CONSULT_OBJ *);

inline EXTERN union CONSULT_OBJ * ConsultObjAdjust(union CONSULT_OBJ * co)
{
	return (union CONSULT_OBJ *) ((union CONSULT_OBJ *)(CharP(co)+LOCAL_HDiff) );
}



inline EXTERN DBRef DBRefAdjust(DBRef);

inline EXTERN DBRef DBRefAdjust(DBRef dbr)
{
	return (DBRef) ((DBRef)(CharP(dbr)+LOCAL_HDiff) );
}



inline EXTERN Term CodeAdjust(Term);

inline EXTERN Term CodeAdjust(Term dbr)
{
	return (Term) (((Term)(dbr)+LOCAL_HDiff) );
}



inline EXTERN ADDR AddrAdjust(ADDR);

inline EXTERN ADDR AddrAdjust(ADDR addr)
{
	return (ADDR) ((ADDR)(CharP(addr)+LOCAL_HDiff) );
}



inline EXTERN CODEADDR CodeAddrAdjust(CODEADDR);

inline EXTERN CODEADDR CodeAddrAdjust(CODEADDR addr)
{
	return (CODEADDR) ((CODEADDR)(CharP(addr)+LOCAL_HDiff) );
}



inline EXTERN BlockHeader * BlockAdjust(BlockHeader *);

inline EXTERN BlockHeader * BlockAdjust(BlockHeader * addr)
{
	return (BlockHeader *) ((BlockHeader *)(CharP(addr)+LOCAL_HDiff) );
}



inline EXTERN yamop * PtoOpAdjust(yamop *);

inline EXTERN yamop * PtoOpAdjust(yamop * ptr)
{
	return (yamop *) (((yamop *)(CharP(ptr) + LOCAL_HDiff)) );
}



inline EXTERN CELL * PtoHeapCellAdjust(CELL *);

inline EXTERN CELL * PtoHeapCellAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LOCAL_HDiff)) );
}



inline EXTERN PredEntry * PtoPredAdjust(PredEntry *);

inline EXTERN PredEntry * PtoPredAdjust(PredEntry * ptr)
{
	return (PredEntry *) (((PredEntry *)(CharP(ptr) + LOCAL_HDiff)) );
}



inline EXTERN ArrayEntry * PtoArrayEAdjust(ArrayEntry *);

inline EXTERN ArrayEntry * PtoArrayEAdjust(ArrayEntry * ptr)
{
	return (ArrayEntry *) (((ArrayEntry *)(CharP(ptr) + LOCAL_HDiff)) );
}


#if PRECOMPUTE_REGADDRESS

inline EXTERN AREG XAdjust(AREG);

inline EXTERN AREG XAdjust(AREG reg)
{
	return (AREG) ((AREG)((reg)+LOCAL_XDiff) );
}


#else

inline EXTERN AREG XAdjust(AREG);

inline EXTERN AREG XAdjust(AREG reg)
{
	return (AREG) ((reg) );
}


#endif

inline EXTERN YREG YAdjust(YREG);

inline EXTERN YREG YAdjust(YREG reg)
{
	return (YREG) ((reg) );
}




inline EXTERN int IsOldLocal(CELL);

inline EXTERN int IsOldLocal(CELL reg)
{
	return (int) (IN_BETWEEN(LOCAL_OldASP, reg, LOCAL_OldLCL0));
}



/* require because the trail might contain dangling pointers */

inline EXTERN int IsOldLocalInTR(CELL);

inline EXTERN int IsOldLocalInTR(CELL reg)
{
	return (int) (IN_BETWEEN(LOCAL_OldH, reg, LOCAL_OldLCL0) );
}



inline EXTERN int IsOldLocalInTRPtr(CELL *);

inline EXTERN int IsOldLocalInTRPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN(LOCAL_OldH, ptr, LOCAL_OldLCL0) );
}




inline EXTERN int IsOldH(CELL);

inline EXTERN int IsOldH(CELL reg)
{
	return (int) (( CharP(reg) == CharP(LOCAL_OldH) ) );
}





inline EXTERN int IsOldGlobal(CELL);

inline EXTERN int IsOldGlobal(CELL reg)
{
	return (int) (IN_BETWEEN(LOCAL_OldH0, reg, LOCAL_OldH) );
}



inline EXTERN int IsOldGlobalPtr(CELL *);

inline EXTERN int IsOldGlobalPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN( LOCAL_OldH0, ptr, LOCAL_OldH) );
}



inline EXTERN int IsOldDelay(CELL);

inline EXTERN int IsOldDelay(CELL reg)
{
	return (int) (IN_BETWEEN(LOCAL_OldGlobalBase, reg, LOCAL_OldH0) );
}



inline EXTERN int IsOldDelayPtr(CELL *);

inline EXTERN int IsOldDelayPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN( LOCAL_OldGlobalBase, ptr, LOCAL_OldH0) );
}



inline EXTERN int IsOldTrail(CELL);

inline EXTERN int IsOldTrail(CELL reg)
{
	return (int) (IN_BETWEEN(LOCAL_OldTrailBase, reg, LOCAL_OldTR) );
}



inline EXTERN int IsOldTrailPtr(CELL *);

inline EXTERN int IsOldTrailPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN(LOCAL_OldTrailBase, ptr, LOCAL_OldTR) );
}



inline EXTERN int IsOldCode(CELL);

inline EXTERN int IsOldCode(CELL reg)
{
	return (int) (IN_BETWEEN(LOCAL_OldHeapBase, reg, LOCAL_OldHeapTop) );
}



inline EXTERN int IsOldCodeCellPtr(CELL *);

inline EXTERN int IsOldCodeCellPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN(LOCAL_OldHeapBase, ptr, LOCAL_OldHeapTop) );
}



inline EXTERN int IsGlobal(CELL);

inline EXTERN int IsGlobal(CELL reg)
{
	return (int) (IN_BETWEEN(GlobalBase, reg, H) );
}



void STD_PROTO(AdjustStacksAndTrail, (void));
void STD_PROTO(AdjustRegs, (int));
