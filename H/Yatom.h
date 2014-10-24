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
* File:		YAtom.h.m4						 *
* Last rev:	19/2/88							 *
* mods:									 *
* comments:	atom properties header file for YAP			 *
*									 *
*************************************************************************/

/* This code can only be defined *after* including Regs.h!!! */

#ifndef YATOM_H
#define YATOM_H 1

#ifdef USE_OFFSETS

INLINE_ONLY inline EXTERN Atom AbsAtom (AtomEntry * p);

INLINE_ONLY inline EXTERN Atom
AbsAtom (AtomEntry * p)
{
  return (Atom) (Addr (p) - AtomBase);
}



INLINE_ONLY inline EXTERN AtomEntry *RepAtom (Atom a);

INLINE_ONLY inline EXTERN AtomEntry *
RepAtom (Atom a)
{
  return (AtomEntry *) (AtomBase + Unsigned (a);
}


#else

INLINE_ONLY inline EXTERN Atom AbsAtom (AtomEntry * p);

INLINE_ONLY inline EXTERN Atom
AbsAtom (AtomEntry * p)
{
  return (Atom) (p);
}



INLINE_ONLY inline EXTERN AtomEntry *RepAtom (Atom a);

INLINE_ONLY inline EXTERN AtomEntry *
RepAtom (Atom a)
{
  return (AtomEntry *) (a);
}


#endif

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN Prop AbsProp (PropEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsProp (PropEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}



INLINE_ONLY inline EXTERN PropEntry *RepProp (Prop p);

INLINE_ONLY inline EXTERN PropEntry *
RepProp (Prop p)
{
  return (PropEntry *) (AtomBase + Unsigned (p));
}


#else

INLINE_ONLY inline EXTERN Prop AbsProp (PropEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsProp (PropEntry * p)
{
  return (Prop) (p);
}



INLINE_ONLY inline EXTERN PropEntry *RepProp (Prop p);

INLINE_ONLY inline EXTERN PropEntry *
RepProp (Prop p)
{
  return (PropEntry *) (p);
}


#endif

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN FunctorEntry *RepFunctorProp (Prop p);

INLINE_ONLY inline EXTERN FunctorEntry *
RepFunctorProp (Prop p)
{
  return (FunctorEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsFunctorProp (FunctorEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsFunctorProp (FunctorEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN FunctorEntry *RepFunctorProp (Prop p);

INLINE_ONLY inline EXTERN FunctorEntry *
RepFunctorProp (Prop p)
{
  return (FunctorEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsFunctorProp (FunctorEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsFunctorProp (FunctorEntry * p)
{
  return (Prop) (p);
}


#endif


INLINE_ONLY inline EXTERN Int ArityOfFunctor (Functor);

INLINE_ONLY inline EXTERN Int
ArityOfFunctor (Functor Fun)
{
  return (Int) (((FunctorEntry *) Fun)->ArityOfFE);
}



INLINE_ONLY inline EXTERN Atom NameOfFunctor (Functor);

INLINE_ONLY inline EXTERN Atom
NameOfFunctor (Functor Fun)
{
  return (Atom) (((FunctorEntry *) Fun)->NameOfFE);
}




INLINE_ONLY inline EXTERN PropFlags IsFunctorProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsFunctorProperty (int flags)
{
  return (PropFlags) ((flags == FunctorProperty));
}



/* summary of property codes used

	00 00	predicate entry
	80 00	db property
	bb 00	functor entry 
	ff df	sparse functor
	ff ex	arithmetic property
	ff f4   translation
	ff f5   blob
	ff f6   hold
	ff f7   array
	ff f8   wide atom
	ff fa   module property
	ff fb   blackboard property
	ff fc	value property
	ff fd	global property
	ff ff	op property
*/


/*	Global Variable property						*/
typedef struct global_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t GRWLock;		/* a simple lock to protect this entry */
#if THREADS
  unsigned int owner_id;        /* owner thread */
#endif
#endif
  struct AtomEntryStruct *AtomOfGE; /* parent atom for deletion */
  struct global_entry *NextGE; /* linked list of global entries */
  Term  global;	/* index in module table                */
  Term  AttChain;	/* index in module table                */
} GlobalEntry;


#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN GlobalEntry *RepGlobalProp (Prop p);

INLINE_ONLY inline EXTERN GlobalEntry *
RepGlobalProp (Prop p)
{
  return (GlobalEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsGlobalProp (GlobalEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsGlobalProp (GlobalEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN GlobalEntry *RepGlobalProp (Prop p);

INLINE_ONLY inline EXTERN GlobalEntry *
RepGlobalProp (Prop p)
{
  return (GlobalEntry *) (p);
}

INLINE_ONLY inline EXTERN Prop AbsGlobalProp (GlobalEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsGlobalProp (GlobalEntry * p)
{
  return (Prop) (p);
}


#endif

#define GlobalProperty	((PropFlags)0xfffd)

INLINE_ONLY inline EXTERN PropFlags IsGlobalProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsGlobalProperty (int flags)
{
  return (PropFlags) ((flags == GlobalProperty));
}


/*	Wide Atom property 						*/
typedef struct
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  UInt  SizeOfAtom;	        /* index in module table                */
} WideAtomEntry;

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN WideAtomEntry *RepWideAtomProp (Prop p);

INLINE_ONLY inline EXTERN WideAtomEntry *
RepWideAtomProp (Prop p)
{
  return (WideAtomEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsWideAtomProp (WideAtomEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsWideAtomProp (WideAtomEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN WideAtomEntry *RepWideAtomProp (Prop p);

INLINE_ONLY inline EXTERN WideAtomEntry *
RepWideAtomProp (Prop p)
{
  return (WideAtomEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsWideAtomProp (WideAtomEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsWideAtomProp (WideAtomEntry * p)
{
  return (Prop) (p);
}


#endif

#define WideAtomProperty	((PropFlags)0xfff8)


INLINE_ONLY inline EXTERN PropFlags IsWideAtomProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsWideAtomProperty (int flags)
{
  return (PropFlags) ((flags == WideAtomProperty));
}

INLINE_ONLY inline EXTERN int IsWideAtom (Atom);

INLINE_ONLY inline EXTERN int
IsWideAtom (Atom at)
{
  return RepAtom(at)->PropsOfAE &&
    IsWideAtomProperty(RepWideAtomProp(RepAtom(at)->PropsOfAE)->KindOfPE);
}


/*	Module property 						*/
typedef struct mod_entry
{
  Prop NextOfPE;		 /* used to chain properties            */
  PropFlags KindOfPE;		 /* kind of property                    */
  struct pred_entry *PredForME;  /* index in module table               */
  Atom   AtomOfME;		 /* module's name	                */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ModRWLock;		/* a read-write lock to protect the entry */
#endif
  unsigned int     flags;        /* Module local flags (from SWI compat) */
  struct  mod_entry *NextME;   /* next module                         */
} ModEntry;

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN ModEntry *RepModProp (Prop p);

INLINE_ONLY inline EXTERN ModEntry *
RepModProp (Prop p)
{
  return (ModEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsModProp (ModEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsModProp (ModEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN ModEntry *RepModProp (Prop p);

INLINE_ONLY inline EXTERN ModEntry *
RepModProp (Prop p)
{
  return (ModEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsModProp (ModEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsModProp (ModEntry * p)
{
  return (Prop) (p);
}


#endif

#define ModProperty	((PropFlags)0xfffa)


INLINE_ONLY inline EXTERN PropFlags IsModProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsModProperty (int flags)
{
  return (PropFlags) ((flags == ModProperty));
}

/*	    operator property entry structure				*/
typedef struct operator_entry
{
  Prop NextOfPE;		/* used to chain properties     */
  PropFlags KindOfPE;		/* kind of property             */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t OpRWLock;		/* a read-write lock to protect the entry */
#endif
  Atom OpName;			/* atom name		        */
  Term OpModule;		/* module of predicate          */
  struct operator_entry  *OpNext; /* next in list of operators  */
  BITS16 Prefix, Infix, Posfix;	/* precedences                  */
} OpEntry;
#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN OpEntry *RepOpProp (Prop p);

INLINE_ONLY inline EXTERN OpEntry *
RepOpProp (Prop p)
{
  return (OpEntry *) (AtomBase + Unsigned (p));
}

INLINE_ONLY inline EXTERN Prop AbsOpProp (OpEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsOpProp (OpEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN OpEntry *RepOpProp (Prop p);

INLINE_ONLY inline EXTERN OpEntry *
RepOpProp (Prop p)
{
  return (OpEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsOpProp (OpEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsOpProp (OpEntry * p)
{
  return (Prop) (p);
}


#endif
#define	OpProperty  ((PropFlags)0xffff)


INLINE_ONLY inline EXTERN PropFlags IsOpProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsOpProperty (int flags)
{
  return (PropFlags) ((flags == OpProperty));
}

typedef enum
{
  INFIX_OP = 0,
  POSFIX_OP = 1,
  PREFIX_OP = 2
} op_type;


OpEntry   *Yap_GetOpProp(Atom, op_type CACHE_TYPE);

int	Yap_IsPrefixOp(Atom,int *,int *);
int	Yap_IsOp(Atom);
int	Yap_IsInfixOp(Atom,int *,int *,int *);
int	Yap_IsPosfixOp(Atom,int *,int *);

/* defines related to operator specifications				*/
#define	MaskPrio  0x0fff
#define	DcrlpFlag 0x1000
#define	DcrrpFlag 0x2000

typedef union arith_ret *eval_ret;

/*	    expression property	entry structure			*/
typedef struct
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  unsigned int ArityOfEE;
  BITS16 ENoOfEE;
  BITS16 FlagsOfEE;
  /* operations that implement the expression */
  int FOfEE;
} ExpEntry;
#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN ExpEntry *RepExpProp (Prop p);

INLINE_ONLY inline EXTERN ExpEntry *
RepExpProp (Prop p)
{
  return (ExpEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsExpProp (ExpEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsExpProp (ExpEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN ExpEntry *RepExpProp (Prop p);

INLINE_ONLY inline EXTERN ExpEntry *
RepExpProp (Prop p)
{
  return (ExpEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsExpProp (ExpEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsExpProp (ExpEntry * p)
{
  return (Prop) (p);
}


#endif
#define	ExpProperty  0xffe0

/* only unary and binary expressions are acceptable */

INLINE_ONLY inline EXTERN PropFlags IsExpProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsExpProperty (int flags)
{
  return (PropFlags) ((flags == ExpProperty));
}




/*		value property entry structure				*/
typedef struct
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t VRWLock;		/* a read-write lock to protect the entry */
#endif
  Term ValueOfVE;		/* (atomic) value associated with the atom */
} ValEntry;
#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN ValEntry *RepValProp (Prop p);

INLINE_ONLY inline EXTERN ValEntry *
RepValProp (Prop p)
{
  return (ValEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsValProp (ValEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsValProp (ValEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN ValEntry *RepValProp (Prop p);

INLINE_ONLY inline EXTERN ValEntry *
RepValProp (Prop p)
{
  return (ValEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsValProp (ValEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsValProp (ValEntry * p)
{
  return (Prop) (p);
}


#endif
#define	ValProperty ((PropFlags)0xfffc)


INLINE_ONLY inline EXTERN PropFlags IsValProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsValProperty (int flags)
{
  return (PropFlags) ((flags == ValProperty));
}

#if SIZEOF_INT_P==4
#define EXTRA_FLAG_BASE 0
#else
#define EXTRA_FLAG_BASE 33
#endif

/*	    predicate property entry structure				*/
/*  AsmPreds are things like var, nonvar, atom ...which are implemented
	    through dedicated machine instructions. In this case the 8 lower
	    bits of PredFlags are used to hold the machine instruction code
	    for	the pred.
    C_Preds are	things write, read, ...	implemented in C. In this case
	    CodeOfPred holds the address of the	correspondent C-function.

don;t forget to also add in qly.h
*/
typedef enum
{
  DiscontiguousPredFlag = ((UInt)0x00000010 << EXTRA_FLAG_BASE),	/* predicates whose clauses may be all-over the place.. */
  SysExportPredFlag = ((UInt)0x00000008 << EXTRA_FLAG_BASE),		/* reuse export list to prolog module. */
  NoTracePredFlag = ((UInt)0x00000004 << EXTRA_FLAG_BASE),		/* cannot trace this predicate */
  NoSpyPredFlag = ((UInt)0x00000002 << EXTRA_FLAG_BASE),		/* cannot spy this predicate */
  QuasiQuotationPredFlag = ((UInt)0x00000001 << EXTRA_FLAG_BASE),		/* SWI-like quasi quotations */
  MegaClausePredFlag =   (UInt)0x80000000, /* predicate is implemented as a mega-clause */
  ThreadLocalPredFlag = (UInt)0x40000000,	/* local to a thread */
  MultiFileFlag = (UInt)0x20000000,	/* is multi-file */
  UserCPredFlag = (UInt)0x10000000,	/* CPred defined by the user */
  LogUpdatePredFlag = (UInt)0x08000000,	/* dynamic predicate with log. upd. sem. */
  InUsePredFlag = (UInt)0x04000000,	/* count calls to pred */
  CountPredFlag = (UInt)0x02000000,	/* count calls to pred */
  HiddenPredFlag = (UInt)0x01000000,	/* invisible predicate */
  CArgsPredFlag = (UInt)0x00800000,	/* SWI-like C-interface pred. */
  SourcePredFlag = (UInt)0x00400000,	/* static predicate with source declaration */
  MetaPredFlag = (UInt)0x00200000,	/* predicate subject to a meta declaration */
  SyncPredFlag = (UInt)0x00100000,	/* has to synch before it can execute */
  NumberDBPredFlag = (UInt)0x00080000,	/* entry for a number key */
  AtomDBPredFlag = (UInt)0x00040000,	/* entry for an atom key */
  GoalExPredFlag = (UInt)0x00020000,	/* predicate that is called by goal_expand */
  TestPredFlag = (UInt)0x00010000,	/* is a test (optim. comit) */
  AsmPredFlag = (UInt)0x00008000,	/* inline */
  StandardPredFlag = (UInt)0x00004000,	/* system predicate */
  DynamicPredFlag = (UInt)0x00002000,	/* dynamic predicate */
  CPredFlag = (UInt)0x00001000,	/* written in C */
  SafePredFlag = (UInt)0x00000800,	/* does not alter arguments */
  CompiledPredFlag = (UInt)0x00000400,	/* is static */
  IndexedPredFlag = (UInt)0x00000200,	/* has indexing code */
  SpiedPredFlag = (UInt)0x00000100,	/* is a spy point */
  BinaryPredFlag = (UInt)0x00000080,	/* test predicate */
  TabledPredFlag = (UInt)0x00000040,	/* is tabled */
  SequentialPredFlag = (UInt)0x00000020,	/* may not create parallel choice points! */
  ProfiledPredFlag = (UInt)0x00000010,	/* pred is being profiled   */
  BackCPredFlag = (UInt)0x00000008,    /*	Myddas Imported pred  */
  ModuleTransparentPredFlag = (UInt)0x00000004,	/* ModuleTransparent pred  */
  SWIEnvPredFlag = (UInt)0x00000002,	/* new SWI interface */
  UDIPredFlag = (UInt)0x00000001	/* User Defined Indexing */
} pred_flag;

/* profile data */
typedef struct
{
  YAP_ULONG_LONG NOfEntries;	/* nbr of times head unification succeeded */
  YAP_ULONG_LONG NOfHeadSuccesses;	/* nbr of times head unification succeeded */
  YAP_ULONG_LONG NOfRetries;	/* nbr of times a clause for the pred
				   was retried */
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;			/* a simple lock to protect this entry */
#endif
} profile_data;

typedef enum {
  LUCALL_EXEC,
  LUCALL_ASSERT,
  LUCALL_RETRACT
} timestamp_type;

#define TIMESTAMP_EOT   ((UInt)(~0L))
#define TIMESTAMP_RESET (TIMESTAMP_EOT-1024)

typedef struct pred_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  struct yami *CodeOfPred;
  OPCODE OpcodeOfPred;		/* undefcode, indexcode, spycode, ....  */
#if SIZEOF_INT_P==4
  CELL PredFlags, ExtraPredFlags;
#else
  CELL PredFlags;
#define ExtraPredFlags   PredFlags
#endif
  UInt ArityOfPE;		/* arity of property                    */
  union
  {
    struct
    {
      struct yami *TrueCodeOfPred;	/* code address                         */
      struct yami *FirstClause;
      struct yami *LastClause;
      UInt NOfClauses;
      OPCODE ExpandCode;
    } p_code;
    CPredicate f_code;
    CmpPredicate d_code;
  } cs;				/* if needing to spy or to lock         */
  Functor FunctorOfPred;	/* functor for Predicate                */
  union
  {
    Atom OwnerFile;		/* File where the predicate was defined */
    Int IndxId;			/* Index for a certain key */
  } src;
#if defined(YAPOR) || defined(THREADS)
  lockvar PELock;		/* a simple lock to protect expansion */
#endif
#ifdef TABLING
  tab_ent_ptr TableOfPred;
#endif				/* TABLING */
#ifdef BEAM
  struct Predicates *beamTable;
#endif
  Term ModuleOfPred;		/* module for this definition           */
  UInt TimeStampOfPred;
  timestamp_type LastCallOfPred; 
  /* This must be at an odd number of cells, otherwise it
     will not be aligned on RISC machines */
  profile_data StatisticsForPred;	/* enable profiling for predicate  */
  struct pred_entry *NextPredOfModule;	/* next pred for same module   */
} PredEntry;
#define PEProp   ((PropFlags)(0x0000))

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN PredEntry *RepPredProp (Prop p);

INLINE_ONLY inline EXTERN PredEntry *
RepPredProp (Prop p)
{
  return (PredEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsPredProp (PredEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsPredProp (PredEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN PredEntry *RepPredProp (Prop p);

INLINE_ONLY inline EXTERN PredEntry *
RepPredProp (Prop p)
{
  return (PredEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsPredProp (PredEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsPredProp (PredEntry * p)
{
  return (Prop) (p);
}


#endif


INLINE_ONLY inline EXTERN PropFlags IsPredProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsPredProperty (int flags)
{
  return (PropFlags) ((flags == PEProp));
}



/* Flags for code or dbase entry */
/* There are several flags for code and data base entries */
typedef enum
{
  ExoMask = 0x1000000,		/* is  exo code */
  FuncSwitchMask = 0x800000,	/* is a switch of functors */
  HasDBTMask = 0x400000,	/* includes a pointer to a DBTerm */
  MegaMask = 0x200000,		/* mega clause */
  FactMask = 0x100000,		/* a fact */
  SwitchRootMask = 0x80000,	/* root for the index tree */
  SwitchTableMask = 0x40000,	/* switch table */
  HasBlobsMask = 0x20000,	/* blobs which may be in use */
  ProfFoundMask = 0x10000,	/* clause is being counted by profiler */
  DynamicMask = 0x8000,		/* dynamic predicate */
  InUseMask = 0x4000,		/* this block is being used */
  ErasedMask = 0x2000,		/* this block has been erased */
  IndexMask = 0x1000,		/* indexing code */
  DBClMask = 0x0800,		/* data base structure */
  LogUpdRuleMask = 0x0400,	/* code is for a log upd rule with env */
  LogUpdMask = 0x0200,		/* logic update index. */
  StaticMask = 0x0100,		/* static predicates */
  DirtyMask = 0x0080,		/* LUIndices  */
  HasCutMask = 0x0040,		/* ! */
  SrcMask = 0x0020,		/* has a source term, only for static references */
/* other flags belong to DB */
} dbentry_flags;

/* *********************** DBrefs **************************************/

typedef struct DB_TERM
{
#ifdef COROUTINING
  union {
    CELL attachments;		/* attached terms */
    Int line_number;
    struct DB_TERM *NextDBT;
  } ag;
#endif
  struct DB_STRUCT **DBRefs;	/* pointer to other references     */
  CELL NOfCells;		/* Size of Term                         */
  CELL Entry;			/* entry point                          */
  Term Contents[MIN_ARRAY];	/* stored term                      */
} DBTerm;

INLINE_ONLY inline EXTERN DBTerm *TermToDBTerm(Term);

INLINE_ONLY inline EXTERN DBTerm *TermToDBTerm(Term X)
{
  if (IsPairTerm(X)) {
    return(DBTerm *)((char *)RepPair(X) - (CELL) &(((DBTerm *) NULL)->Contents));
  } else {
    return(DBTerm *)((char *)RepAppl(X) - (CELL) &(((DBTerm *) NULL)->Contents));
  }
}


/* The ordering of the first 3 fields should be compatible with lu_clauses */
typedef struct DB_STRUCT
{
  Functor id;			/* allow pointers to this struct to id  */
  /*   as dbref                           */
  CELL Flags;			/* Term Flags                           */
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;			/* a simple lock to protect this entry */
#endif
#if MULTIPLE_STACKS
  Int ref_count;		/* how many branches are using this entry */
#endif
  CELL NOfRefsTo;		/* Number of references pointing here   */
  struct struct_dbentry *Parent;	/* key of DBase reference               */
  struct yami *Code;		/* pointer to code if this is a clause  */
  struct DB_STRUCT *Prev;	/* Previous element in chain            */
  struct DB_STRUCT *Next;	/* Next element in chain                */
  struct DB_STRUCT *p, *n;	/* entry's age, negative if from recorda,
				   positive if it was recordz  */
  CELL Mask;			/* parts that should be cleared         */
  CELL Key;			/* A mask that can be used to check before
				   you unify */
  DBTerm DBT;
} DBStruct;

#define DBStructFlagsToDBStruct(X) ((DBRef)((char *)(X) - (CELL) &(((DBRef) NULL)->Flags)))

#if MULTIPLE_STACKS
#define INIT_DBREF_COUNT(X) (X)->ref_count = 0
#define  INC_DBREF_COUNT(X) (X)->ref_count++
#define  DEC_DBREF_COUNT(X) (X)->ref_count--
#define     DBREF_IN_USE(X) ((X)->ref_count != 0)
#else
#define INIT_DBREF_COUNT(X)
#define  INC_DBREF_COUNT(X)
#define  DEC_DBREF_COUNT(X)
#define     DBREF_IN_USE(X) ((X)->Flags & InUseMask)
#endif

typedef DBStruct *DBRef;

/* extern Functor FunctorDBRef; */

INLINE_ONLY inline EXTERN int IsDBRefTerm (Term);

INLINE_ONLY inline EXTERN int
IsDBRefTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorDBRef);
}



INLINE_ONLY inline EXTERN Term MkDBRefTerm (DBRef);

INLINE_ONLY inline EXTERN Term
MkDBRefTerm (DBRef p)
{
  return (Term) ((AbsAppl (((CELL *) (p)))));
}



INLINE_ONLY inline EXTERN DBRef DBRefOfTerm (Term t);

INLINE_ONLY inline EXTERN DBRef
DBRefOfTerm (Term t)
{
  return (DBRef) (((DBRef) (RepAppl (t))));
}




INLINE_ONLY inline EXTERN int IsRefTerm (Term);

INLINE_ONLY inline EXTERN int
IsRefTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorDBRef);
}



INLINE_ONLY inline EXTERN CODEADDR RefOfTerm (Term t);

INLINE_ONLY inline EXTERN CODEADDR
RefOfTerm (Term t)
{
  return (CODEADDR) (DBRefOfTerm (t));
}



typedef struct struct_dbentry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  unsigned int ArityOfDB;	/* kind of property                     */
  Functor FunctorOfDB;		/* functor for this property            */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t DBRWLock;		/* a simple lock to protect this entry */
#endif
  DBRef First;			/* first DBase entry                    */
  DBRef Last;			/* last DBase entry                     */
  Term ModuleOfDB;		/* module for this definition           */
  DBRef F0, L0;			/* everyone                          */
} DBEntry;
typedef DBEntry *DBProp;
#define	DBProperty	   ((PropFlags)0x8000)

typedef struct
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  unsigned int ArityOfDB;	/* kind of property                     */
  Functor FunctorOfDB;		/* functor for this property            */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t DBRWLock;		/* a simple lock to protect this entry */
#endif
  DBRef First;			/* first DBase entry                    */
  DBRef Last;			/* last DBase entry                     */
  Term ModuleOfDB;		/* module for this definition           */
  Int NOfEntries;		/* age counter                          */
  DBRef Index;			/* age counter                          */
} LogUpdDBEntry;
typedef LogUpdDBEntry *LogUpdDBProp;
#define	CodeDBBit          0x2

#define	CodeDBProperty     (DBProperty|CodeDBBit)


INLINE_ONLY inline EXTERN PropFlags IsDBProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsDBProperty (int flags)
{
  return (PropFlags) ((flags & ~CodeDBBit) == DBProperty);
}



#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN DBProp RepDBProp (Prop p);

INLINE_ONLY inline EXTERN DBProp
RepDBProp (Prop p)
{
  return (DBProp) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsDBProp (DBProp p);

INLINE_ONLY inline EXTERN Prop
AbsDBProp (DBProp p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN DBProp RepDBProp (Prop p);

INLINE_ONLY inline EXTERN DBProp
RepDBProp (Prop p)
{
  return (DBProp) (p);
}



INLINE_ONLY inline EXTERN Prop AbsDBProp (DBProp p);

INLINE_ONLY inline EXTERN Prop
AbsDBProp (DBProp p)
{
  return (Prop) (p);
}


#endif


/* These are the actual flags for DataBase terms */
typedef enum
{
  DBAtomic = 0x1,
  DBVar = 0x2,
  DBNoVars = 0x4,
  DBComplex = 0x8,
  DBCode = 0x10,
  DBNoCode = 0x20,
  DBWithRefs = 0x40
} db_term_flags;

typedef struct
{
  Prop NextOfPE;		/* used to chain properties                */
  PropFlags KindOfPE;		/* kind of property                        */
  Atom KeyOfBB;			/* functor for this property               */
  Term Element;			/* blackboard element                      */
#if defined(YAPOR) || defined(THREADS)
  rwlock_t BBRWLock;		/* a read-write lock to protect the entry */
#endif
  Term ModuleOfBB;		/* module for this definition             */
} BlackBoardEntry;
typedef BlackBoardEntry *BBProp;

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN BlackBoardEntry *RepBBProp (Prop p);

INLINE_ONLY inline EXTERN BlackBoardEntry *
RepBBProp (Prop p)
{
  return (BlackBoardEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsBBProp (BlackBoardEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsBBProp (BlackBoardEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN BlackBoardEntry *RepBBProp (Prop p);

INLINE_ONLY inline EXTERN BlackBoardEntry *
RepBBProp (Prop p)
{
  return (BlackBoardEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsBBProp (BlackBoardEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsBBProp (BlackBoardEntry * p)
{
  return (Prop) (p);
}


#endif

#define BBProperty	((PropFlags)0xfffb)


INLINE_ONLY inline EXTERN PropFlags IsBBProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsBBProperty (int flags)
{
  return (PropFlags) ((flags == BBProperty));
}


/*		hold property entry structure				*/
typedef struct hold_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  UInt  RefsOfPE;		/* used to count the number of holds    */
} HoldEntry;

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN HoldEntry *RepHoldProp (Prop p);

INLINE_ONLY inline EXTERN HoldEntry *
RepHoldProp (Prop p)
{
  return (HoldEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsHoldProp (HoldEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsHoldProp (HoldEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN HoldEntry *RepHoldProp (Prop p);

INLINE_ONLY inline EXTERN HoldEntry *
RepHoldProp (Prop p)
{
  return (HoldEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsHoldProp (HoldEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsHoldProp (HoldEntry * p)
{
  return (Prop) (p);
}


#endif

#define	HoldProperty  0xfff6

/*		translation property entry structure				*/
typedef struct translation_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  Int  Translation;		/* used to hash the atom as an integer; */
}  TranslationEntry;

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN TranslationEntry *RepTranslationProp (Prop p);

INLINE_ONLY inline EXTERN TranslationEntry *
RepTranslationProp (Prop p)
{
  return (TranslationEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsTranslationProp (TranslationEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsTranslationProp (TranslationEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN TranslationEntry *RepTranslationProp (Prop p);

INLINE_ONLY inline EXTERN TranslationEntry *
RepTranslationProp (Prop p)
{
  return (TranslationEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsTranslationProp (TranslationEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsTranslationProp (TranslationEntry * p)
{
  return (Prop) (p);
}


#endif
#define	TranslationProperty  0xfff4

void Yap_PutAtomTranslation(Atom a, Int i);

/* get translation prop for atom;               */
static inline TranslationEntry *
Yap_GetTranslationProp(Atom at)
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);
  TranslationEntry *p;

  READ_LOCK(ae->ARWLock);
  p = RepTranslationProp(p0 = ae->PropsOfAE);
  while (p0 && p->KindOfPE != TranslationProperty)
    p = RepTranslationProp(p0 = p->NextOfPE);
  READ_UNLOCK(ae->ARWLock);
  if (p0 == NIL) return (TranslationEntry *)NULL;
  return p;
}


/* only unary and binary expressions are acceptable */

INLINE_ONLY inline EXTERN PropFlags IsTranslationProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsTranslationProperty (int flags)
{
  return (PropFlags) ((flags == TranslationProperty));
}


typedef enum {
  STATIC_ARRAY = 1,
  DYNAMIC_ARRAY = 2,
  MMAP_ARRAY = 4,
  FIXED_ARRAY = 8
} array_type;


/*		array property entry structure				*/
/*		first case is for dynamic arrays */
typedef struct array_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  Int ArrayEArity;		/* Arity of Array (positive)            */
  array_type TypeOfAE;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ArRWLock;		/* a read-write lock to protect the entry */
#if THREADS
  unsigned int owner_id;
#endif
#endif
  struct array_entry *NextAE;
  Term ValueOfVE;		/* Pointer to the actual array          */
} ArrayEntry;

/* second case is for static arrays */

/* first, the valid types */
typedef enum
{
  array_of_ints,
  array_of_chars,
  array_of_uchars,
  array_of_doubles,
  array_of_ptrs,
  array_of_atoms,
  array_of_dbrefs,
  array_of_nb_terms,
  array_of_terms
} static_array_types;

typedef  struct {
  Term tlive;
  Term tstore;
} live_term;


typedef union
{
  Int *ints;
  char *chars;
  unsigned char *uchars;
  Float *floats;
  AtomEntry **ptrs;
  Term *atoms;
  Term *dbrefs;
  DBTerm **terms;
  live_term *lterms;
} statarray_elements;

/* next, the actual data structure */
typedef struct static_array_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  Int ArrayEArity;		/* Arity of Array (negative)            */
  array_type TypeOfAE;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ArRWLock;		/* a read-write lock to protect the entry */
#endif
  struct static_array_entry *NextAE;
  static_array_types ArrayType;	/* Type of Array Elements.              */
  statarray_elements ValueOfVE;	/* Pointer to the Array itself  */
} StaticArrayEntry;


#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN ArrayEntry *RepArrayProp (Prop p);

INLINE_ONLY inline EXTERN ArrayEntry *
RepArrayProp (Prop p)
{
  return (ArrayEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsArrayProp (ArrayEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsArrayProp (ArrayEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}



INLINE_ONLY inline EXTERN StaticArrayEntry *RepStaticArrayProp (Prop p);

INLINE_ONLY inline EXTERN StaticArrayEntry *
RepStaticArrayProp (Prop p)
{
  return (StaticArrayEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN Prop AbsStaticArrayProp (StaticArrayEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsStaticArrayProp (StaticArrayEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN ArrayEntry *RepArrayProp (Prop p);

INLINE_ONLY inline EXTERN ArrayEntry *
RepArrayProp (Prop p)
{
  return (ArrayEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsArrayProp (ArrayEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsArrayProp (ArrayEntry * p)
{
  return (Prop) (p);
}



INLINE_ONLY inline EXTERN StaticArrayEntry *RepStaticArrayProp (Prop p);

INLINE_ONLY inline EXTERN StaticArrayEntry *
RepStaticArrayProp (Prop p)
{
  return (StaticArrayEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsStaticArrayProp (StaticArrayEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsStaticArrayProp (StaticArrayEntry * p)
{
  return (Prop) (p);
}


#endif
#define	ArrayProperty ((PropFlags)0xfff7)


INLINE_ONLY inline EXTERN int ArrayIsDynamic (ArrayEntry *);

INLINE_ONLY inline EXTERN int
ArrayIsDynamic (ArrayEntry * are)
{
  return (int) (((are)->TypeOfAE & DYNAMIC_ARRAY));
}




INLINE_ONLY inline EXTERN PropFlags IsArrayProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsArrayProperty (int flags)
{
  return (PropFlags) ((flags == ArrayProperty));
}



/*	SWI Blob property 						*/
typedef struct blob_atom_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  struct PL_blob_t *blob_t;     /* type of blob */
} BlobPropEntry;

#if USE_OFFSETS_IN_PROPS

INLINE_ONLY inline EXTERN BlobAtomEntry *RepBlobProp (Prop p);

INLINE_ONLY inline EXTERN BlobPropEntry *
RepBlobProp (Prop p)
{
  return (BlobPropEntry *) (AtomBase + Unsigned (p));
}



INLINE_ONLY inline EXTERN AtomEntry *AbsBlobProp (BlobPropEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsBlobProp (BlobPropEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

INLINE_ONLY inline EXTERN BlobPropEntry *RepBlobProp (Prop p);

INLINE_ONLY inline EXTERN BlobPropEntry *
RepBlobProp (Prop p)
{
  return (BlobPropEntry *) (p);
}



INLINE_ONLY inline EXTERN Prop AbsBlobProp (BlobPropEntry * p);

INLINE_ONLY inline EXTERN Prop
AbsBlobProp (BlobPropEntry * p)
{
  return (Prop) (p);
}


#endif

#define BlobProperty	((PropFlags)0xfff5)


INLINE_ONLY inline EXTERN PropFlags IsBlobProperty (int);

INLINE_ONLY inline EXTERN PropFlags
IsBlobProperty (int flags)
{
  return (PropFlags) ((flags == BlobProperty));
}

INLINE_ONLY inline EXTERN int IsBlob (Atom);

INLINE_ONLY inline EXTERN int
IsBlob (Atom at)
{
  return RepAtom(at)->PropsOfAE &&
    IsBlobProperty(RepBlobProp(RepAtom(at)->PropsOfAE)->KindOfPE);
}


/* Proto types */

/* cdmgr.c */
int Yap_RemoveIndexation(PredEntry *);
void Yap_UpdateTimestamps(PredEntry *);

/* dbase.c */
void Yap_ErDBE(DBRef);
DBTerm *Yap_StoreTermInDB(Term, int);
DBTerm *Yap_StoreTermInDBPlusExtraSpace(Term, UInt, UInt *);
Term Yap_FetchTermFromDB(DBTerm *);
Term Yap_FetchClauseTermFromDB(DBTerm *);
Term Yap_PopTermFromDB(DBTerm *);
void Yap_ReleaseTermFromDB(DBTerm *);

/* init.c */
Atom Yap_GetOp(OpEntry *, int *, int);

/* vsc: redefined to GetAProp to avoid conflicts with Windows header files */
Prop Yap_GetAProp(Atom, PropFlags);
Prop Yap_GetAPropHavingLock(AtomEntry *, PropFlags);

#define  PROLOG_MODULE 0

#include "YapHeap.h"

#define  PredHashInitialSize      ((UInt)1039)
#define  PredHashIncrement        ((UInt)7919)

INLINE_ONLY EXTERN inline UInt PRED_HASH(FunctorEntry *, Term, UInt);

INLINE_ONLY EXTERN inline UInt
PRED_HASH(FunctorEntry *fe, Term cur_mod, UInt size)
{
  return (((CELL)fe+cur_mod)>>2) % size;
}

INLINE_ONLY EXTERN inline Prop GetPredPropByFuncAndModHavingLock(FunctorEntry *, Term);
INLINE_ONLY EXTERN inline Prop PredPropByFuncAndMod(FunctorEntry *, Term);
INLINE_ONLY EXTERN inline Prop PredPropByAtomAndMod(Atom, Term);
INLINE_ONLY EXTERN inline Prop GetPredPropByFuncHavingLock(FunctorEntry *, Term);
INLINE_ONLY EXTERN inline Prop PredPropByFunc (Functor fe, Term cur_mod);
INLINE_ONLY EXTERN inline Prop PredPropByAtom (Atom at, Term cur_mod);

#ifdef THREADS

Prop Yap_NewThreadPred(struct pred_entry * CACHE_TYPE);
Prop Yap_NewPredPropByFunctor(Functor, Term);
INLINE_ONLY EXTERN inline struct pred_entry *Yap_GetThreadPred(struct pred_entry * CACHE_TYPE);

INLINE_ONLY EXTERN inline struct pred_entry *
Yap_GetThreadPred(struct pred_entry *ap USES_REGS)
{
  Functor f = ap->FunctorOfPred;
  Term  mod = ap->ModuleOfPred;
  Prop p0 = AbsPredProp(LOCAL_ThreadHandle.local_preds);

  while(p0) {
    PredEntry *ap = RepPredProp(p0);
    if (ap->FunctorOfPred == f &&
	ap->ModuleOfPred == mod) return ap;
    p0 = ap->NextOfPE;
  }
  return RepPredProp(Yap_NewThreadPred(ap PASS_REGS));
}
#endif


INLINE_ONLY EXTERN inline Prop
GetPredPropByFuncHavingLock (FunctorEntry *fe, Term cur_mod)
{
  PredEntry *p;

  if (!(p = RepPredProp(fe->PropsOfFE))) {
    return NIL;
  }
  if ((p->ModuleOfPred == cur_mod || !(p->ModuleOfPred))) {
#ifdef THREADS
    /* Thread Local Predicates */
    if (p->PredFlags & ThreadLocalPredFlag) {
      return AbsPredProp (Yap_GetThreadPred (p INIT_REGS));
    }
#endif
    return AbsPredProp(p);
  }
  if (p->NextOfPE) {
    UInt hash = PRED_HASH(fe,cur_mod,PredHashTableSize);
    READ_LOCK(PredHashRWLock);
    p = PredHash[hash];
    
    while (p) {
      if (p->FunctorOfPred == fe &&
	  p->ModuleOfPred == cur_mod)
	{
#ifdef THREADS
	  /* Thread Local Predicates */
	  if (p->PredFlags & ThreadLocalPredFlag) {
	    READ_UNLOCK(PredHashRWLock);
	    return AbsPredProp (Yap_GetThreadPred (p INIT_REGS));
	  }
#endif
	  READ_UNLOCK(PredHashRWLock);
	  return AbsPredProp(p);
	}
      p = RepPredProp(p->NextOfPE);
    }
    READ_UNLOCK(PredHashRWLock);
  }
  return NIL;
}

INLINE_ONLY EXTERN inline Prop
PredPropByFunc (Functor fe, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;

  FUNC_WRITE_LOCK (fe);
  p0 = GetPredPropByFuncHavingLock(fe, cur_mod);
  if (p0) {
    FUNC_WRITE_UNLOCK (fe);
    return p0;
  }
  return Yap_NewPredPropByFunctor (fe, cur_mod);
}

INLINE_ONLY EXTERN inline Prop
GetPredPropByFuncAndModHavingLock (FunctorEntry *fe, Term cur_mod)
{
  PredEntry *p;

  if (!(p = RepPredProp(fe->PropsOfFE))) {
    return NIL;
  }
  if (p->ModuleOfPred == cur_mod) {
#ifdef THREADS
    /* Thread Local Predicates */
    if (p->PredFlags & ThreadLocalPredFlag) {
      return AbsPredProp (Yap_GetThreadPred (p INIT_REGS));
    }
#endif
    return AbsPredProp(p);
  }
  if (p->NextOfPE) {
    UInt hash = PRED_HASH(fe,cur_mod,PredHashTableSize);
    READ_LOCK(PredHashRWLock);
    p = PredHash[hash];
    
    while (p) {
      if (p->FunctorOfPred == fe &&
	  p->ModuleOfPred == cur_mod)
	{
#ifdef THREADS
	  /* Thread Local Predicates */
	  if (p->PredFlags & ThreadLocalPredFlag) {
	    READ_UNLOCK(PredHashRWLock);
	    return AbsPredProp (Yap_GetThreadPred (p INIT_REGS));
	  }
#endif
	  READ_UNLOCK(PredHashRWLock);
	  return AbsPredProp(p);
	}
      p = RepPredProp(p->NextOfPE);
    }
    READ_UNLOCK(PredHashRWLock);
  }
  return NIL;
}

INLINE_ONLY EXTERN inline Prop
PredPropByFuncAndMod (Functor fe, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;

  FUNC_WRITE_LOCK (fe);
  p0 = GetPredPropByFuncAndModHavingLock(fe, cur_mod);
  if (p0) {
    FUNC_WRITE_UNLOCK (fe);
    return p0;
  }
  return Yap_NewPredPropByFunctor (fe, cur_mod);
}

INLINE_ONLY EXTERN inline Prop
PredPropByAtom (Atom at, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom (at);

  WRITE_LOCK (ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0)
    {
      PredEntry *pe = RepPredProp (p0);
      if (pe->KindOfPE == PEProp &&
	  (pe->ModuleOfPred == cur_mod || !pe->ModuleOfPred))
	{
#ifdef THREADS
	  /* Thread Local Predicates */
	  if (pe->PredFlags & ThreadLocalPredFlag)
	    {
	      WRITE_UNLOCK (ae->ARWLock);
	      return AbsPredProp (Yap_GetThreadPred (pe INIT_REGS));
	    }
#endif
	  WRITE_UNLOCK (ae->ARWLock);
	  return (p0);
	}
      p0 = pe->NextOfPE;
    }
  return Yap_NewPredPropByAtom (ae, cur_mod);
}

INLINE_ONLY EXTERN inline Prop
PredPropByAtomAndMod (Atom at, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom (at);

  WRITE_LOCK (ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0)
    {
      PredEntry *pe = RepPredProp (p0);
      if (pe->KindOfPE == PEProp &&
	  (pe->ModuleOfPred == cur_mod))
	{
#ifdef THREADS
	  /* Thread Local Predicates */
	  if (pe->PredFlags & ThreadLocalPredFlag)
	    {
	      WRITE_UNLOCK (ae->ARWLock);
	      return AbsPredProp (Yap_GetThreadPred (pe INIT_REGS));
	    }
#endif
	  WRITE_UNLOCK (ae->ARWLock);
	  return (p0);
	}
      p0 = pe->NextOfPE;
    }
  return Yap_NewPredPropByAtom (ae, cur_mod);
}

#if DEBUG_PELOCKING
#define PELOCK(I,Z)						\
  { LOCK((Z)->PELock); (Z)->StatisticsForPred.NOfEntries=(I);(Z)->StatisticsForPred.NOfHeadSuccesses=pthread_self(); }
#define UNLOCKPE(I,Z)						\
  ( (Z)->StatisticsForPred.NOfRetries=(I),  UNLOCK((Z)->PELock) )
#elif YAPOR || THREADS
#define PELOCK(I,Z) (LOCK((Z)->PELock))
#define UNLOCKPE(I,Z)	(UNLOCK((Z)->PELock))
#else
#define PELOCK(I,Z)
#define UNLOCKPE(I,Z)
#endif

INLINE_ONLY EXTERN inline void AddPropToAtom(AtomEntry *, PropEntry *p);

INLINE_ONLY EXTERN inline void
AddPropToAtom(AtomEntry *ae, PropEntry *p)
{
  /* old properties should be always last, and wide atom properties 
     should always be first */
  if (ae->PropsOfAE != NIL &&
      RepProp(ae->PropsOfAE)->KindOfPE == WideAtomProperty) {
    PropEntry *pp = RepProp(ae->PropsOfAE);    
    p->NextOfPE = pp->NextOfPE;
    pp->NextOfPE = AbsProp(p);
  } else {
    p->NextOfPE = ae->PropsOfAE;
    ae->PropsOfAE = AbsProp(p);
  }
}


#endif

