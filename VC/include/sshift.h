






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
extern Int HDiff,
  GDiff,
  LDiff,
  TrDiff,
  XDiff,
  DelayDiff;

/* The old stack pointers */
extern CELL    *OldASP, *OldLCL0;
extern tr_fr_ptr OldTR;
extern CELL    *OldGlobalBase, *OldH, *OldH0;
extern ADDR     OldTrailBase, OldTrailTop;
extern ADDR     OldHeapBase, OldHeapTop;

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
	return (CELL *) (((CELL *)(CharP(ptr) + GDiff)) );
}



inline EXTERN CELL * PtoDelayAdjust(CELL *);

inline EXTERN CELL * PtoDelayAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + DelayDiff)) );
}



inline EXTERN tr_fr_ptr PtoTRAdjust(tr_fr_ptr);

inline EXTERN tr_fr_ptr PtoTRAdjust(tr_fr_ptr ptr)
{
	return (tr_fr_ptr) (((tr_fr_ptr)(CharP(ptr) + TrDiff)) );
}



inline EXTERN CELL * CellPtoTRAdjust(CELL *);

inline EXTERN CELL * CellPtoTRAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + TrDiff)) );
}



inline EXTERN CELL * PtoLocAdjust(CELL *);

inline EXTERN CELL * PtoLocAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + LDiff)) );
}



inline EXTERN choiceptr ChoicePtrAdjust(choiceptr);

inline EXTERN choiceptr ChoicePtrAdjust(choiceptr ptr)
{
	return (choiceptr) (((choiceptr)(CharP(ptr) + LDiff)) );
}


#ifdef TABLING

inline EXTERN choiceptr ConsumerChoicePtrAdjust(choiceptr);

inline EXTERN choiceptr ConsumerChoicePtrAdjust(choiceptr ptr)
{
	return (choiceptr) (((choiceptr)(CharP(ptr) + LDiff)) );
}



inline EXTERN choiceptr GeneratorChoicePtrAdjust(choiceptr);

inline EXTERN choiceptr GeneratorChoicePtrAdjust(choiceptr ptr)
{
	return (choiceptr) (((choiceptr)(CharP(ptr) + LDiff)) );
}


#endif /* TABLING */


inline EXTERN CELL GlobalAdjust(CELL);

inline EXTERN CELL GlobalAdjust(CELL val)
{
	return (CELL) ((val+GDiff) );
}



inline EXTERN CELL DelayAdjust(CELL);

inline EXTERN CELL DelayAdjust(CELL val)
{
	return (CELL) ((val+DelayDiff) );
}



inline EXTERN ADDR GlobalAddrAdjust(ADDR);

inline EXTERN ADDR GlobalAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+GDiff) );
}



inline EXTERN ADDR DelayAddrAdjust(ADDR);

inline EXTERN ADDR DelayAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+DelayDiff) );
}



inline EXTERN CELL LocalAdjust(CELL);

inline EXTERN CELL LocalAdjust(CELL val)
{
	return (CELL) ((val+LDiff) );
}



inline EXTERN ADDR LocalAddrAdjust(ADDR);

inline EXTERN ADDR LocalAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+LDiff) );
}



inline EXTERN CELL TrailAdjust(CELL);

inline EXTERN CELL TrailAdjust(CELL val)
{
	return (CELL) ((val+TrDiff) );
}



inline EXTERN ADDR TrailAddrAdjust(ADDR);

inline EXTERN ADDR TrailAddrAdjust(ADDR ptr)
{
	return (ADDR) ((ptr+TrDiff) );
}


/* heap data structures */

inline EXTERN Functor FuncAdjust(Functor);

inline EXTERN Functor FuncAdjust(Functor f)
{
	return (Functor) ((Functor)(CharP(f)+HDiff) );
}



inline EXTERN CELL * CellPtoHeapAdjust(CELL *);

inline EXTERN CELL * CellPtoHeapAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + HDiff)) );
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
	return (Atom) ((at == NULL ? (at) : (Atom)(CharP(at)+HDiff) ));
}



inline EXTERN Prop PropAdjust(Prop);

inline EXTERN Prop PropAdjust(Prop p)
{
	return (Prop) ((p == NULL ? (p) : (Prop)(CharP(p)+HDiff)) );
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
	return (Term) ((t-HDiff) );
}


#else

inline EXTERN Term BlobTermAdjust(Term);

inline EXTERN Term BlobTermAdjust(Term t)
{
	return (Term) ((t+HDiff) );
}


#endif

inline EXTERN AtomEntry * AtomEntryAdjust(AtomEntry *);

inline EXTERN AtomEntry * AtomEntryAdjust(AtomEntry * at)
{
	return (AtomEntry *) ((AtomEntry *)(CharP(at)+HDiff) );
}



inline EXTERN union CONSULT_OBJ * ConsultObjAdjust(union CONSULT_OBJ *);

inline EXTERN union CONSULT_OBJ * ConsultObjAdjust(union CONSULT_OBJ * co)
{
	return (union CONSULT_OBJ *) ((union CONSULT_OBJ *)(CharP(co)+HDiff) );
}



inline EXTERN DBRef DBRefAdjust(DBRef);

inline EXTERN DBRef DBRefAdjust(DBRef dbr)
{
	return (DBRef) ((DBRef)(CharP(dbr)+HDiff) );
}



inline EXTERN Term CodeAdjust(Term);

inline EXTERN Term CodeAdjust(Term dbr)
{
	return (Term) (((Term)(dbr)+HDiff) );
}



inline EXTERN ADDR AddrAdjust(ADDR);

inline EXTERN ADDR AddrAdjust(ADDR addr)
{
	return (ADDR) ((ADDR)(CharP(addr)+HDiff) );
}



inline EXTERN CODEADDR CodeAddrAdjust(CODEADDR);

inline EXTERN CODEADDR CodeAddrAdjust(CODEADDR addr)
{
	return (CODEADDR) ((CODEADDR)(CharP(addr)+HDiff) );
}



inline EXTERN BlockHeader * BlockAdjust(BlockHeader *);

inline EXTERN BlockHeader * BlockAdjust(BlockHeader * addr)
{
	return (BlockHeader *) ((BlockHeader *)(CharP(addr)+HDiff) );
}



inline EXTERN yamop * PtoOpAdjust(yamop *);

inline EXTERN yamop * PtoOpAdjust(yamop * ptr)
{
	return (yamop *) (((yamop *)(CharP(ptr) + HDiff)) );
}



inline EXTERN CELL * PtoHeapCellAdjust(CELL *);

inline EXTERN CELL * PtoHeapCellAdjust(CELL * ptr)
{
	return (CELL *) (((CELL *)(CharP(ptr) + HDiff)) );
}



inline EXTERN PredEntry * PtoPredAdjust(PredEntry *);

inline EXTERN PredEntry * PtoPredAdjust(PredEntry * ptr)
{
	return (PredEntry *) (((PredEntry *)(CharP(ptr) + HDiff)) );
}



inline EXTERN ArrayEntry * PtoArrayEAdjust(ArrayEntry *);

inline EXTERN ArrayEntry * PtoArrayEAdjust(ArrayEntry * ptr)
{
	return (ArrayEntry *) (((ArrayEntry *)(CharP(ptr) + HDiff)) );
}


#if PRECOMPUTE_REGADDRESS

inline EXTERN AREG XAdjust(AREG);

inline EXTERN AREG XAdjust(AREG reg)
{
	return (AREG) ((AREG)((reg)+XDiff) );
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
	return (int) (IN_BETWEEN(OldASP, reg, OldLCL0));
}



/* require because the trail might contain dangling pointers */

inline EXTERN int IsOldLocalInTR(CELL);

inline EXTERN int IsOldLocalInTR(CELL reg)
{
	return (int) (IN_BETWEEN(OldH, reg, OldLCL0) );
}



inline EXTERN int IsOldLocalInTRPtr(CELL *);

inline EXTERN int IsOldLocalInTRPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN(OldH, ptr, OldLCL0) );
}




inline EXTERN int IsOldH(CELL);

inline EXTERN int IsOldH(CELL reg)
{
	return (int) (( CharP(reg) == CharP(OldH) ) );
}





inline EXTERN int IsOldGlobal(CELL);

inline EXTERN int IsOldGlobal(CELL reg)
{
	return (int) (IN_BETWEEN(OldH0, reg, OldH) );
}



inline EXTERN int IsOldGlobalPtr(CELL *);

inline EXTERN int IsOldGlobalPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN( OldH0, ptr, OldH) );
}



inline EXTERN int IsOldDelay(CELL);

inline EXTERN int IsOldDelay(CELL reg)
{
	return (int) (IN_BETWEEN(OldGlobalBase, reg, OldH0) );
}



inline EXTERN int IsOldDelayPtr(CELL *);

inline EXTERN int IsOldDelayPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN( OldGlobalBase, ptr, OldH0) );
}



inline EXTERN int IsOldTrail(CELL);

inline EXTERN int IsOldTrail(CELL reg)
{
	return (int) (IN_BETWEEN(OldTrailBase, reg, OldTR) );
}



inline EXTERN int IsOldTrailPtr(CELL *);

inline EXTERN int IsOldTrailPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN(OldTrailBase, ptr, OldTR) );
}



inline EXTERN int IsOldCode(CELL);

inline EXTERN int IsOldCode(CELL reg)
{
	return (int) (IN_BETWEEN(OldHeapBase, reg, OldHeapTop) );
}



inline EXTERN int IsOldCodeCellPtr(CELL *);

inline EXTERN int IsOldCodeCellPtr(CELL * ptr)
{
	return (int) (IN_BETWEEN(OldHeapBase, ptr, OldHeapTop) );
}



inline EXTERN int IsGlobal(CELL);

inline EXTERN int IsGlobal(CELL reg)
{
	return (int) (IN_BETWEEN(GlobalBase, reg, H) );
}



void STD_PROTO(AdjustStacksAndTrail, (void));
void STD_PROTO(AdjustRegs, (int));
