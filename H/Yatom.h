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

inline EXTERN Atom AbsAtom (AtomEntry * p);

inline EXTERN Atom
AbsAtom (AtomEntry * p)
{
  return (Atom) (Addr (p) - AtomBase);
}



inline EXTERN AtomEntry *RepAtom (Atom a);

inline EXTERN AtomEntry *
RepAtom (Atom a)
{
  return (AtomEntry *) (AtomBase + Unsigned (a));
}


#else

inline EXTERN Atom AbsAtom (AtomEntry * p);

inline EXTERN Atom
AbsAtom (AtomEntry * p)
{
  return (Atom) (p);
}



inline EXTERN AtomEntry *RepAtom (Atom a);

inline EXTERN AtomEntry *
RepAtom (Atom a)
{
  return (AtomEntry *) (a);
}


#endif

#if USE_OFFSETS_IN_PROPS

inline EXTERN Prop AbsProp (PropEntry * p);

inline EXTERN Prop
AbsProp (PropEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}



inline EXTERN PropEntry *RepProp (Prop p);

inline EXTERN PropEntry *
RepProp (Prop p)
{
  return (PropEntry *) (AtomBase + Unsigned (p));
}


#else

inline EXTERN Prop AbsProp (PropEntry * p);

inline EXTERN Prop
AbsProp (PropEntry * p)
{
  return (Prop) (p);
}



inline EXTERN PropEntry *RepProp (Prop p);

inline EXTERN PropEntry *
RepProp (Prop p)
{
  return (PropEntry *) (p);
}


#endif

#if USE_OFFSETS_IN_PROPS

inline EXTERN FunctorEntry *RepFunctorProp (Prop p);

inline EXTERN FunctorEntry *
RepFunctorProp (Prop p)
{
  return (FunctorEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsFunctorProp (FunctorEntry * p);

inline EXTERN Prop
AbsFunctorProp (FunctorEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN FunctorEntry *RepFunctorProp (Prop p);

inline EXTERN FunctorEntry *
RepFunctorProp (Prop p)
{
  return (FunctorEntry *) (p);
}



inline EXTERN Prop AbsFunctorProp (FunctorEntry * p);

inline EXTERN Prop
AbsFunctorProp (FunctorEntry * p)
{
  return (Prop) (p);
}


#endif


inline EXTERN Int ArityOfFunctor (Functor);

inline EXTERN Int
ArityOfFunctor (Functor Fun)
{
  return (Int) (((FunctorEntry *) Fun)->ArityOfFE);
}



inline EXTERN Atom NameOfFunctor (Functor);

inline EXTERN Atom
NameOfFunctor (Functor Fun)
{
  return (Atom) (((FunctorEntry *) Fun)->NameOfFE);
}




inline EXTERN PropFlags IsFunctorProperty (int);

inline EXTERN PropFlags
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

inline EXTERN GlobalEntry *RepGlobalProp (Prop p);

inline EXTERN GlobalEntry *
RepGlobalProp (Prop p)
{
  return (GlobalEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsGlobalProp (GlobalEntry * p);

inline EXTERN Prop
AbsGlobalProp (GlobalEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN GlobalEntry *RepGlobalProp (Prop p);

inline EXTERN GlobalEntry *
RepGlobalProp (Prop p)
{
  return (GlobalEntry *) (p);
}

inline EXTERN Prop AbsGlobalProp (GlobalEntry * p);

inline EXTERN Prop
AbsGlobalProp (GlobalEntry * p)
{
  return (Prop) (p);
}


#endif

#define GlobalProperty	((PropFlags)0xfffd)

inline EXTERN PropFlags IsGlobalProperty (int);

inline EXTERN PropFlags
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

inline EXTERN WideAtomEntry *RepWideAtomProp (Prop p);

inline EXTERN WideAtomEntry *
RepWideAtomProp (Prop p)
{
  return (WideAtomEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsWideAtomProp (WideAtomEntry * p);

inline EXTERN Prop
AbsWideAtomProp (WideAtomEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN WideAtomEntry *RepWideAtomProp (Prop p);

inline EXTERN WideAtomEntry *
RepWideAtomProp (Prop p)
{
  return (WideAtomEntry *) (p);
}



inline EXTERN Prop AbsWideAtomProp (WideAtomEntry * p);

inline EXTERN Prop
AbsWideAtomProp (WideAtomEntry * p)
{
  return (Prop) (p);
}


#endif

#define WideAtomProperty	((PropFlags)0xfff8)


inline EXTERN PropFlags IsWideAtomProperty (int);

inline EXTERN PropFlags
IsWideAtomProperty (int flags)
{
  return (PropFlags) ((flags == WideAtomProperty));
}

inline EXTERN int IsWideAtom (Atom);

inline EXTERN int
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
  struct  mod_entry *NextME;   /* next module                         */
} ModEntry;

#if USE_OFFSETS_IN_PROPS

inline EXTERN ModEntry *RepModProp (Prop p);

inline EXTERN ModEntry *
RepModProp (Prop p)
{
  return (ModEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsModProp (ModEntry * p);

inline EXTERN Prop
AbsModProp (ModEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN ModEntry *RepModProp (Prop p);

inline EXTERN ModEntry *
RepModProp (Prop p)
{
  return (ModEntry *) (p);
}



inline EXTERN Prop AbsModProp (ModEntry * p);

inline EXTERN Prop
AbsModProp (ModEntry * p)
{
  return (Prop) (p);
}


#endif

#define ModProperty	((PropFlags)0xfffa)


inline EXTERN PropFlags IsModProperty (int);

inline EXTERN PropFlags
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

inline EXTERN OpEntry *RepOpProp (Prop p);

inline EXTERN OpEntry *
RepOpProp (Prop p)
{
  return (OpEntry *) (AtomBase + Unsigned (p));
}

inline EXTERN Prop AbsOpProp (OpEntry * p);

inline EXTERN Prop
AbsOpProp (OpEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN OpEntry *RepOpProp (Prop p);

inline EXTERN OpEntry *
RepOpProp (Prop p)
{
  return (OpEntry *) (p);
}



inline EXTERN Prop AbsOpProp (OpEntry * p);

inline EXTERN Prop
AbsOpProp (OpEntry * p)
{
  return (Prop) (p);
}


#endif
#define	OpProperty  ((PropFlags)0xffff)


inline EXTERN PropFlags IsOpProperty (int);

inline EXTERN PropFlags
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


OpEntry   *STD_PROTO(Yap_GetOpProp,(Atom, op_type));

int	STD_PROTO(Yap_IsPrefixOp,(Atom,int *,int *));
int	STD_PROTO(Yap_IsOp,(Atom));
int	STD_PROTO(Yap_IsInfixOp,(Atom,int *,int *,int *));
int	STD_PROTO(Yap_IsPosfixOp,(Atom,int *,int *));

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

inline EXTERN ExpEntry *RepExpProp (Prop p);

inline EXTERN ExpEntry *
RepExpProp (Prop p)
{
  return (ExpEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsExpProp (ExpEntry * p);

inline EXTERN Prop
AbsExpProp (ExpEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN ExpEntry *RepExpProp (Prop p);

inline EXTERN ExpEntry *
RepExpProp (Prop p)
{
  return (ExpEntry *) (p);
}



inline EXTERN Prop AbsExpProp (ExpEntry * p);

inline EXTERN Prop
AbsExpProp (ExpEntry * p)
{
  return (Prop) (p);
}


#endif
#define	ExpProperty  0xffe0

/* only unary and binary expressions are acceptable */

inline EXTERN PropFlags IsExpProperty (int);

inline EXTERN PropFlags
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

inline EXTERN ValEntry *RepValProp (Prop p);

inline EXTERN ValEntry *
RepValProp (Prop p)
{
  return (ValEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsValProp (ValEntry * p);

inline EXTERN Prop
AbsValProp (ValEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN ValEntry *RepValProp (Prop p);

inline EXTERN ValEntry *
RepValProp (Prop p)
{
  return (ValEntry *) (p);
}



inline EXTERN Prop AbsValProp (ValEntry * p);

inline EXTERN Prop
AbsValProp (ValEntry * p)
{
  return (Prop) (p);
}


#endif
#define	ValProperty ((PropFlags)0xfffc)


inline EXTERN PropFlags IsValProperty (int);

inline EXTERN PropFlags
IsValProperty (int flags)
{
  return (PropFlags) ((flags == ValProperty));
}



/*	    predicate property entry structure				*/
/*  AsmPreds are things like var, nonvar, atom ...which are implemented
	    through dedicated machine instructions. In this case the 8 lower
	    bits of PredFlags are used to hold the machine instruction code
	    for	the pred.
    C_Preds are	things write, read, ...	implemented in C. In this case
	    CodeOfPred holds the address of the	correspondent C-function.
*/
typedef enum
{
  MegaClausePredFlag =   0x80000000L, /* predicate is implemented as a mega-clause */
  ThreadLocalPredFlag = 0x40000000L,	/* local to a thread */
  MultiFileFlag = 0x20000000L,	/* is multi-file */
  UserCPredFlag = 0x10000000L,	/* CPred defined by the user */
  LogUpdatePredFlag = 0x08000000L,	/* dynamic predicate with log. upd. sem. */
  InUsePredFlag = 0x04000000L,	/* count calls to pred */
  CountPredFlag = 0x02000000L,	/* count calls to pred */
  HiddenPredFlag = 0x01000000L,	/* invisible predicate */
  CArgsPredFlag = 0x00800000L,	/* SWI-like C-interface pred. */
  SourcePredFlag = 0x00400000L,	/* static predicate with source declaration */
  MetaPredFlag = 0x00200000L,	/* predicate subject to a meta declaration */
  SyncPredFlag = 0x00100000L,	/* has to synch before it can execute */
  NumberDBPredFlag = 0x00080000L,	/* entry for a number key */
  AtomDBPredFlag = 0x00040000L,	/* entry for an atom key */
  GoalExPredFlag = 0x00020000L,	/* predicate that is called by goal_expand */
  TestPredFlag = 0x00010000L,	/* is a test (optim. comit) */
  AsmPredFlag = 0x00008000L,	/* inline */
  StandardPredFlag = 0x00004000L,	/* system predicate */
  DynamicPredFlag = 0x00002000L,	/* dynamic predicate */
  CPredFlag = 0x00001000L,	/* written in C */
  SafePredFlag = 0x00000800L,	/* does not alter arguments */
  CompiledPredFlag = 0x00000400L,	/* is static */
  IndexedPredFlag = 0x00000200L,	/* has indexing code */
  SpiedPredFlag = 0x00000100L,	/* is a spy point */
  BinaryPredFlag = 0x00000080L,	/* test predicate */
  TabledPredFlag = 0x00000040L,	/* is tabled */
  SequentialPredFlag = 0x00000020L,	/* may not create parallel choice points! */
  ProfiledPredFlag = 0x00000010L,	/* pred is being profiled   */
  MyddasPredFlag = 0x00000008L,	/* Myddas Imported pred  */
  ModuleTransparentPredFlag = 0x00000004L,	/* ModuleTransparent pred  */
  SWIEnvPredFlag = 0x00000002L,	/* new SWI interface */
  UDIPredFlag = 0x00000001L	/* User Defined Indexing */
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
  CELL PredFlags;
  unsigned int ArityOfPE;	/* arity of property                    */
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
    struct mfile *file_srcs;	/* for multifile predicates */
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

inline EXTERN PredEntry *RepPredProp (Prop p);

inline EXTERN PredEntry *
RepPredProp (Prop p)
{
  return (PredEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsPredProp (PredEntry * p);

inline EXTERN Prop
AbsPredProp (PredEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN PredEntry *RepPredProp (Prop p);

inline EXTERN PredEntry *
RepPredProp (Prop p)
{
  return (PredEntry *) (p);
}



inline EXTERN Prop AbsPredProp (PredEntry * p);

inline EXTERN Prop
AbsPredProp (PredEntry * p)
{
  return (Prop) (p);
}


#endif


inline EXTERN PropFlags IsPredProperty (int);

inline EXTERN PropFlags
IsPredProperty (int flags)
{
  return (PropFlags) ((flags == PEProp));
}



/* Flags for code or dbase entry */
/* There are several flags for code and data base entries */
typedef enum
{
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
  DirtyMask = 0x0080		/* LUIndices  */
/* other flags belong to DB */
} dbentry_flags;

/* *********************** DBrefs **************************************/

typedef struct DB_TERM
{
#ifdef COROUTINING
  union {
    CELL attachments;		/* attached terms */
    struct DB_TERM *NextDBT;
  } ag;
#endif
  struct DB_STRUCT **DBRefs;	/* pointer to other references     */
  CELL NOfCells;		/* Size of Term                         */
  CELL Entry;			/* entry point                          */
  Term Contents[MIN_ARRAY];	/* stored term                      */
} DBTerm;

inline EXTERN DBTerm *TermToDBTerm(Term);

inline EXTERN DBTerm *TermToDBTerm(Term X)
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

#if defined(YAPOR) || defined(THREADS)
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

inline EXTERN int IsDBRefTerm (Term);

inline EXTERN int
IsDBRefTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorDBRef);
}



inline EXTERN Term MkDBRefTerm (DBRef);

inline EXTERN Term
MkDBRefTerm (DBRef p)
{
  return (Term) ((AbsAppl (((CELL *) (p)))));
}



inline EXTERN DBRef DBRefOfTerm (Term t);

inline EXTERN DBRef
DBRefOfTerm (Term t)
{
  return (DBRef) (((DBRef) (RepAppl (t))));
}




inline EXTERN int IsRefTerm (Term);

inline EXTERN int
IsRefTerm (Term t)
{
  return (int) (IsApplTerm (t) && FunctorOfTerm (t) == FunctorDBRef);
}



inline EXTERN CODEADDR RefOfTerm (Term t);

inline EXTERN CODEADDR
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


inline EXTERN PropFlags IsDBProperty (int);

inline EXTERN PropFlags
IsDBProperty (int flags)
{
  return (PropFlags) ((flags & ~CodeDBBit) == DBProperty);
}



#if USE_OFFSETS_IN_PROPS

inline EXTERN DBProp RepDBProp (Prop p);

inline EXTERN DBProp
RepDBProp (Prop p)
{
  return (DBProp) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsDBProp (DBProp p);

inline EXTERN Prop
AbsDBProp (DBProp p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN DBProp RepDBProp (Prop p);

inline EXTERN DBProp
RepDBProp (Prop p)
{
  return (DBProp) (p);
}



inline EXTERN Prop AbsDBProp (DBProp p);

inline EXTERN Prop
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

inline EXTERN BlackBoardEntry *RepBBProp (Prop p);

inline EXTERN BlackBoardEntry *
RepBBProp (Prop p)
{
  return (BlackBoardEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsBBProp (BlackBoardEntry * p);

inline EXTERN Prop
AbsBBProp (BlackBoardEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN BlackBoardEntry *RepBBProp (Prop p);

inline EXTERN BlackBoardEntry *
RepBBProp (Prop p)
{
  return (BlackBoardEntry *) (p);
}



inline EXTERN Prop AbsBBProp (BlackBoardEntry * p);

inline EXTERN Prop
AbsBBProp (BlackBoardEntry * p)
{
  return (Prop) (p);
}


#endif

#define BBProperty	((PropFlags)0xfffb)


inline EXTERN PropFlags IsBBProperty (int);

inline EXTERN PropFlags
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

inline EXTERN HoldEntry *RepHoldProp (Prop p);

inline EXTERN HoldEntry *
RepHoldProp (Prop p)
{
  return (HoldEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsHoldProp (HoldEntry * p);

inline EXTERN Prop
AbsHoldProp (HoldEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN HoldEntry *RepHoldProp (Prop p);

inline EXTERN HoldEntry *
RepHoldProp (Prop p)
{
  return (HoldEntry *) (p);
}



inline EXTERN Prop AbsHoldProp (HoldEntry * p);

inline EXTERN Prop
AbsHoldProp (HoldEntry * p)
{
  return (Prop) (p);
}


#endif
#define	HoldProperty  0xfff6

/* only unary and binary expressions are acceptable */

inline EXTERN PropFlags IsHoldProperty (int);

inline EXTERN PropFlags
IsHoldProperty (int flags)
{
  return (PropFlags) ((flags == HoldProperty));
}




/*		array property entry structure				*/
/*		first case is for dynamic arrays */
typedef struct array_entry
{
  Prop NextOfPE;		/* used to chain properties             */
  PropFlags KindOfPE;		/* kind of property                     */
  Int ArrayEArity;		/* Arity of Array (positive)            */
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
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ArRWLock;		/* a read-write lock to protect the entry */
#endif
  struct static_array_entry *NextAE;
  static_array_types ArrayType;	/* Type of Array Elements.              */
  statarray_elements ValueOfVE;	/* Pointer to the Array itself  */
} StaticArrayEntry;


#if USE_OFFSETS_IN_PROPS

inline EXTERN ArrayEntry *RepArrayProp (Prop p);

inline EXTERN ArrayEntry *
RepArrayProp (Prop p)
{
  return (ArrayEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsArrayProp (ArrayEntry * p);

inline EXTERN Prop
AbsArrayProp (ArrayEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}



inline EXTERN StaticArrayEntry *RepStaticArrayProp (Prop p);

inline EXTERN StaticArrayEntry *
RepStaticArrayProp (Prop p)
{
  return (StaticArrayEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN Prop AbsStaticArrayProp (StaticArrayEntry * p);

inline EXTERN Prop
AbsStaticArrayProp (StaticArrayEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN ArrayEntry *RepArrayProp (Prop p);

inline EXTERN ArrayEntry *
RepArrayProp (Prop p)
{
  return (ArrayEntry *) (p);
}



inline EXTERN Prop AbsArrayProp (ArrayEntry * p);

inline EXTERN Prop
AbsArrayProp (ArrayEntry * p)
{
  return (Prop) (p);
}



inline EXTERN StaticArrayEntry *RepStaticArrayProp (Prop p);

inline EXTERN StaticArrayEntry *
RepStaticArrayProp (Prop p)
{
  return (StaticArrayEntry *) (p);
}



inline EXTERN Prop AbsStaticArrayProp (StaticArrayEntry * p);

inline EXTERN Prop
AbsStaticArrayProp (StaticArrayEntry * p)
{
  return (Prop) (p);
}


#endif
#define	ArrayProperty ((PropFlags)0xfff7)


inline EXTERN int ArrayIsDynamic (ArrayEntry *);

inline EXTERN int
ArrayIsDynamic (ArrayEntry * are)
{
  return (int) (((are)->ArrayEArity > 0));
}




inline EXTERN PropFlags IsArrayProperty (int);

inline EXTERN PropFlags
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

inline EXTERN BlobAtomEntry *RepBlobProp (Prop p);

inline EXTERN BlobPropEntry *
RepBlobProp (Prop p)
{
  return (BlobPropEntry *) (AtomBase + Unsigned (p));
}



inline EXTERN AtomEntry *AbsBlobProp (BlobPropEntry * p);

inline EXTERN Prop
AbsBlobProp (BlobPropEntry * p)
{
  return (Prop) (Addr (p) - AtomBase);
}


#else

inline EXTERN BlobPropEntry *RepBlobProp (Prop p);

inline EXTERN BlobPropEntry *
RepBlobProp (Prop p)
{
  return (BlobPropEntry *) (p);
}



inline EXTERN Prop AbsBlobProp (BlobPropEntry * p);

inline EXTERN Prop
AbsBlobProp (BlobPropEntry * p)
{
  return (Prop) (p);
}


#endif

#define BlobProperty	((PropFlags)0xfff5)


inline EXTERN PropFlags IsBlobProperty (int);

inline EXTERN PropFlags
IsBlobProperty (int flags)
{
  return (PropFlags) ((flags == BlobProperty));
}

inline EXTERN int IsBlob (Atom);

inline EXTERN int
IsBlob (Atom at)
{
  return RepAtom(at)->PropsOfAE &&
    IsBlobProperty(RepBlobProp(RepAtom(at)->PropsOfAE)->KindOfPE);
}


/* Proto types */

/* cdmgr.c */
int STD_PROTO (Yap_RemoveIndexation, (PredEntry *));
void STD_PROTO (Yap_UpdateTimestamps, (PredEntry *));

/* dbase.c */
void STD_PROTO (Yap_ErDBE, (DBRef));
DBTerm *STD_PROTO (Yap_StoreTermInDB, (Term, int));
DBTerm *STD_PROTO (Yap_StoreTermInDBPlusExtraSpace, (Term, UInt, UInt *));
Term STD_PROTO (Yap_FetchTermFromDB, (DBTerm *));
Term STD_PROTO (Yap_PopTermFromDB, (DBTerm *));
void STD_PROTO (Yap_ReleaseTermFromDB, (DBTerm *));

/* init.c */
Atom STD_PROTO (Yap_GetOp, (OpEntry *, int *, int));

/* vsc: redefined to GetAProp to avoid conflicts with Windows header files */
Prop STD_PROTO (Yap_GetAProp, (Atom, PropFlags));
Prop STD_PROTO (Yap_GetAPropHavingLock, (AtomEntry *, PropFlags));

typedef enum
{
  PROLOG_MODULE = 0,
  USER_MODULE = 1,
  IDB_MODULE = 2,
  ATTRIBUTES_MODULE = 3,
  CHARSIO_MODULE = 4,
  TERMS_MODULE = 5
} default_modules;

#include "YapHeap.h"

#define  PredHashInitialSize      ((UInt)1039)
#define  PredHashIncrement        ((UInt)7919)

EXTERN inline UInt STD_PROTO(PRED_HASH, (FunctorEntry *, Term, UInt));

EXTERN inline UInt
PRED_HASH(FunctorEntry *fe, Term cur_mod, UInt size)
{
  return (((CELL)fe+cur_mod)>>2) % size;
}

EXTERN inline Prop STD_PROTO(GetPredPropByFuncHavingLock, (FunctorEntry *, Term));

#ifdef THREADS

Prop STD_PROTO(Yap_NewThreadPred, (struct pred_entry *));
Prop STD_PROTO(Yap_NewPredPropByFunctor, (Functor, Term));
EXTERN inline struct pred_entry *STD_PROTO(Yap_GetThreadPred, (struct pred_entry *));

EXTERN inline struct pred_entry *
Yap_GetThreadPred(struct pred_entry *ap)
{
  Functor f = ap->FunctorOfPred;
  Term  mod = ap->ModuleOfPred;
  Prop p0 = AbsPredProp(MY_ThreadHandle.local_preds);

  while(p0) {
    PredEntry *ap = RepPredProp(p0);
    if (ap->FunctorOfPred == f &&
	ap->ModuleOfPred == mod) return ap;
    p0 = ap->NextOfPE;
  }
  return RepPredProp(Yap_NewThreadPred(ap));
}
#endif


EXTERN inline Prop
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
      return AbsPredProp (Yap_GetThreadPred (p));
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
	    return AbsPredProp (Yap_GetThreadPred (p));
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

EXTERN inline Prop
PredPropByFunc (Functor fe, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;

  WRITE_LOCK (fe->FRWLock);
  p0 = GetPredPropByFuncHavingLock(fe, cur_mod);
  if (p0) {
    WRITE_UNLOCK (fe->FRWLock);
    return p0;
  }
  return Yap_NewPredPropByFunctor (fe, cur_mod);
}

EXTERN inline Prop
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
	      return AbsPredProp (Yap_GetThreadPred (pe));
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
#else
#define PELOCK(I,Z) LOCK((Z)->PELock)
#define UNLOCKPE(I,Z)	UNLOCK((Z)->PELock)
#endif

#endif

