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

#if USE_OFFSETS
Constructor(Atom, Abs, AtomEntry *, p, Addr(p) - AtomBase)
Destructor(Atom, Rep, AtomEntry *, a, AtomBase + Unsigned(a))
#else
Constructor(Atom, Abs, AtomEntry *, p, p)
Destructor(Atom, Rep, AtomEntry *, a, a)
#endif

#if USE_OFFSETS_IN_PROPS
Constructor(Prop,Abs,PropEntry *,p,Addr(p)-AtomBase)
Destructor(Prop,Rep,PropEntry *,p,AtomBase+Unsigned(p))
#else
Constructor(Prop,Abs,PropEntry *,p,p)
Destructor(Prop,Rep,PropEntry *,p,p)
#endif

#if USE_OFFSETS_IN_PROPS
Destructor(Prop, RepFunctor, FunctorEntry *, p, AtomBase + Unsigned(p))
Constructor(Prop, AbsFunctor, FunctorEntry *, p, Addr(p)-AtomBase)
#else
Destructor(Prop, RepFunctor, FunctorEntry *, p, p)
Constructor(Prop, AbsFunctor, FunctorEntry *, p, p)
#endif

Inline(ArityOfFunctor, Int, Functor, Fun, ((FunctorEntry *)Fun)->ArityOfFE)
Inline(NameOfFunctor, Atom, Functor, Fun, ((FunctorEntry *)Fun)->NameOfFE)

Inline(IsFunctorProperty, PropFlags, int, flags, (flags == FunctorProperty) )

/* summary of property codes used

	00 00	predicate entry
	80 00	db property
	bb 00	functor entry 
	ff df	sparse functor
	ff ex	arithmetic property
	ff f7   array
	ff fa   module property
	ff fb   blackboard property
	ff fc	value property
	ff ff	op property
*/

/*	Module property 						*/
typedef struct {
    Prop   	NextOfPE;	/* used	to chain properties		*/
    PropFlags  	KindOfPE;	/* kind	of property			*/
    SMALLUNSGN  IndexOfMod;     /* index in module table		*/
} ModEntry;

#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepMod,ModEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsMod,ModEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepMod,ModEntry *,p,p)
Constructor(Prop,AbsMod,ModEntry *,p,p)
#endif

#define ModProperty	((PropFlags)0xfffa)

Inline(IsModProperty, PropFlags, int, flags, (flags == ModProperty))

/*	    operator property entry structure				*/
typedef	struct {
    Prop	NextOfPE;		/* used	to chain properties	*/
    PropFlags   KindOfPE;		/* kind	of property		*/
#if defined(YAPOR) || defined(THREADS)
    rwlock_t OpRWLock;            /* a read-write lock to protect the entry */
#endif
    BITS16  Prefix, Infix, Posfix;      /* precedences			*/
    } OpEntry;
#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepOp,OpEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsOp,OpEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepOp,OpEntry *,p,p)
Constructor(Prop,AbsOp,OpEntry *,p,p)
#endif
#define	OpProperty  ((PropFlags)0xffff)

Inline(IsOpProperty, PropFlags, int, flags, (flags == OpProperty) )

/* defines related to operator specifications				*/
#define	MaskPrio  0x0fff
#define	DcrlpFlag 0x1000
#define	DcrrpFlag 0x2000

typedef union arith_ret *eval_ret;

/*	    expression property	entry structure			*/
typedef	struct {
  Prop	NextOfPE;	/* used	to chain properties		*/
  PropFlags   KindOfPE;	/* kind	of property			*/
  unsigned int ArityOfEE;
  BITS16	ENoOfEE;
  BITS16	FlagsOfEE;
  /* operations that implement the expression */
  union {
    blob_type    (*constant)(eval_ret);
    blob_type    (*unary)(Term, eval_ret);
    blob_type    (*binary)(Term, Term, eval_ret);
  } FOfEE;
} ExpEntry;
#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepExp,ExpEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsExp,ExpEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepExp,ExpEntry *,p,p)
Constructor(Prop,AbsExp,ExpEntry *,p,p)
#endif
#define	ExpProperty  0xffe0

/* only unary and binary expressions are acceptable */
Inline(IsExpProperty, PropFlags, int, flags, (flags == ExpProperty) )


/*		value property entry structure				*/
typedef	struct {
    Prop	NextOfPE;	/* used	to chain properties		*/
    PropFlags   KindOfPE;	/* kind of property			*/
#if defined(YAPOR) || defined(THREADS)
    rwlock_t VRWLock;            /* a read-write lock to protect the entry */
#endif
    Term	ValueOfVE;	/* (atomic) value associated with the atom */
    } ValEntry;
#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepVal,ValEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsVal,ValEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepVal,ValEntry *,p,p)
Constructor(Prop,AbsVal,ValEntry *,p,p)
#endif
#define	ValProperty ((PropFlags)0xfffc)

Inline(IsValProperty, PropFlags, int, flags, (flags == ValProperty) )

/*	    predicate property entry structure				*/
/*  AsmPreds are things like var, nonvar, atom ...which are implemented
	    through dedicated machine instructions. In this case the 8 lower
	    bits of PredFlags are used to hold the machine instruction code
	    for	the pred.
    C_Preds are	things write, read, ...	implemented in C. In this case
	    CodeOfPred holds the address of the	correspondent C-function.
*/
typedef enum {
 ThreadLocalPredFlag=0x40000000L, /* local to a thread		*/
  MultiFileFlag =    0x20000000L, /* is multi-file		*/
  UserCPredFlag =    0x10000000L, /* CPred defined by the user	*/
  LogUpdatePredFlag= 0x08000000L, /* dynamic predicate with log. upd. sem.*/
  InUsePredFlag =    0x04000000L, /* count calls to pred */
  CountPredFlag =    0x02000000L, /* count calls to pred */
  HiddenPredFlag =   0x01000000L, /* invisible predicate */
  CArgsPredFlag =    0x00800000L, /* SWI-like C-interface pred. */
  SourcePredFlag =   0x00400000L, /* static predicate with source declaration */
  MetaPredFlag =     0x00200000L, /* predicate subject to a meta declaration */
  SyncPredFlag =     0x00100000L, /* has to synch before it can execute*/
  NumberDBPredFlag = 0x00080000L, /* entry for a number key */
  AtomDBPredFlag   = 0x00040000L, /* entry for an atom key */
  FastPredFlag =     0x00020000L, /* native code		*/
  TestPredFlag =     0x00010000L, /* is a test (optim. comit)	*/
  AsmPredFlag =      0x00008000L, /* inline			*/
  StandardPredFlag=  0x00004000L, /* system predicate		*/
  DynamicPredFlag=   0x00002000L, /* dynamic predicate		*/
  CPredFlag =        0x00001000L, /* written in C			*/
  SafePredFlag =     0x00000800L, /* does not alter arguments	*/
  CompiledPredFlag=  0x00000400L, /* is static			*/
  IndexedPredFlag=   0x00000200L, /* has indexing code		*/
  SpiedPredFlag =    0x00000100L, /* is a spy point		*/
  BinaryTestPredFlag=0x00000080L, /* test predicate.              */
#ifdef TABLING
  TabledPredFlag =   0x00000040L, /* is tabled			*/
#endif /* TABLING */
#ifdef YAPOR
  SequentialPredFlag=0x00000020L, /* may not create par. choice points!*/
#endif /* YAPOR */
  ProfiledPredFlag = 0x00000010L  /* pred is being profiled	 */
} pred_flag;

/* profile data */
typedef struct {
  YAP_ULONG_LONG	NOfEntries;		/* nbr of times head unification succeeded*/
  YAP_ULONG_LONG   NOfHeadSuccesses;       /* nbr of times head unification succeeded*/
  YAP_ULONG_LONG   NOfRetries;             /* nbr of times a clause for the pred
				   was retried */
#if defined(YAPOR) || defined(THREADS)
  lockvar lock;                 /* a simple lock to protect this entry */
#endif
} profile_data;

typedef	struct pred_entry {
  Prop	NextOfPE;	/* used to chain properties	    	*/
  PropFlags	KindOfPE;	/* kind of property		    	*/
  struct yami   *CodeOfPred;
  OPCODE        OpcodeOfPred;	/* undefcode, indexcode, spycode, ....  */
  CELL	        PredFlags;
  unsigned int  ArityOfPE;	/* arity of property		    	*/
  union 	{
    struct {
      struct yami   *TrueCodeOfPred;	/* code address		    		*/
      struct yami   *FirstClause;
      struct yami   *LastClause;
      UInt 	     NOfClauses;
      OPCODE 	     ExpandCode;
    } p_code;
    CPredicate    f_code;
    CmpPredicate  d_code;
  } cs;	/* if needing to spy or to lock 	*/
  Functor       FunctorOfPred;	/* functor for Predicate        	*/
  union {
    Atom	        OwnerFile;	/* File where the predicate was defined */
    Int                 IndxId;         /* Index for a certain key */
    struct mfile       *file_srcs;      /* for multifile predicates */
  } src;
#if defined(YAPOR) || defined(THREADS)
  rwlock_t      PRWLock;        /* a simple lock to protect this entry */
  lockvar       PELock;         /* a simple lock to protect expansion */
#endif
#ifdef TABLING
  tab_ent_ptr   TableOfPred;
#endif /* TABLING */
  Term 	ModuleOfPred;	/* module for this definition		*/
  /* This must be at an odd number of cells, otherwise it
     will not be aligned on RISC machines */
  profile_data  StatisticsForPred; /* enable profiling for predicate  */
  struct pred_entry *NextPredOfModule; /* next pred for same module   */
} PredEntry;
#define PEProp   ((PropFlags)(0x0000))

#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepPred,PredEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsPred,PredEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepPred,PredEntry *,p,p)
Constructor(Prop,AbsPred,PredEntry *,p,p)
#endif

Inline(IsPredProperty, PropFlags, int, flags, (flags == PEProp) )

/* Flags for code or dbase entry */
/* There are several flags for code and data base entries */
typedef enum {
  FactMask = 0x100000,     /* informs this is a fact */
  SwitchRootMask= 0x80000, /* informs this is the root for the index tree */
  SwitchTableMask=0x40000, /* informs this is a switch table */
  HasBlobsMask  = 0x20000, /* informs this has blobs which may be in use */
  GcFoundMask   = 0x10000, /* informs this is a dynamic predicate */
  DynamicMask   =  0x8000, /* informs this is a dynamic predicate */
  InUseMask     =  0x4000, /* informs this block is being used */
  ErasedMask    =  0x2000, /* informs this block has been erased */
  IndexMask     =  0x1000, /* informs this is indexing code */
  DBClMask      =  0x0800, /* informs this is a data base structure */
  LogUpdRuleMask=  0x0400, /* informs the code is for a log upd rule with env */
  LogUpdMask    =  0x0200, /* informs this is a logic update index. */
  StaticMask    =  0x0100  /* dealing with static predicates */
/* other flags belong to DB */
} dbentry_flags;

/* *********************** DBrefs **************************************/

typedef struct DB_TERM {
#ifdef COROUTINING
  CELL    attachments;   /* attached terms */   
#endif
  struct DB_STRUCT **DBRefs; /* pointer to other references 	*/
  CELL NOfCells;	/* Size of Term				*/
  CELL Entry;		/* entry point 				*/
  Term Contents[MIN_ARRAY]; /* stored term	       		*/
} DBTerm;

/* The ordering of the first 3 fields should be compatible with lu_clauses */
typedef struct DB_STRUCT {
  Functor id;		/* allow pointers to this struct to id  */
			/*   as dbref                           */
  CELL Flags;	/* Term Flags				*/
#if defined(YAPOR) || defined(THREADS)
  lockvar   lock;         /* a simple lock to protect this entry */
  Int       ref_count;    /* how many branches are using this entry */
#endif
  CELL NOfRefsTo;	/* Number of references pointing here	*/
  struct struct_dbentry  *Parent;	/* key of DBase reference		*/
  struct yami *Code;	/* pointer to code if this is a clause 	*/
  struct DB_STRUCT *Prev; /* Previous element in chain            */
  struct DB_STRUCT *Next; /* Next element in chain                */
  struct DB_STRUCT *p, *n; /* entry's age, negative if from recorda,
			     positive if it was recordz  */
  CELL Mask;		/* parts that should be cleared		*/
  CELL Key;		/* A mask that can be used to check before
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
Inline(IsDBRefTerm, int, Term, t, IsApplTerm(t) && FunctorOfTerm(t) == FunctorDBRef)
Inline(MkDBRefTerm, Term, DBRef, p, (AbsAppl(((CELL *)(p)))))
Destructor(Term, DBRefOf, DBRef, t, ((DBRef)(RepAppl(t))))

Inline(IsRefTerm, int, Term, t, IsApplTerm(t) && FunctorOfTerm(t) == FunctorDBRef)
Destructor(Term, RefOf, CODEADDR, t, DBRefOfTerm(t))

typedef	struct struct_dbentry {
  Prop	NextOfPE;	/* used	to chain properties		*/
  PropFlags	KindOfPE;	/* kind of property			*/
  unsigned int	ArityOfDB;	/* kind of property			*/
  Functor	FunctorOfDB;	/* functor for this property		*/
#if defined(YAPOR) || defined(THREADS)
  rwlock_t    DBRWLock;         /* a simple lock to protect this entry */
#endif
  DBRef	First;		/* first DBase entry			*/
  DBRef	Last;		/* last DBase entry			*/
  Term 	ModuleOfDB;	/* module for this definition		*/
  DBRef         F0,L0;		/* everyone                          */
} DBEntry;
typedef DBEntry *DBProp;
#define	DBProperty	   ((PropFlags)0x8000)

typedef	struct {
  Prop	NextOfPE;	/* used	to chain properties		*/
  PropFlags	KindOfPE;	/* kind of property			*/
  unsigned int	ArityOfDB;	/* kind of property			*/
  Functor	FunctorOfDB;	/* functor for this property		*/
#if defined(YAPOR) || defined(THREADS)
  rwlock_t    DBRWLock;         /* a simple lock to protect this entry */
#endif 
  DBRef	First;		/* first DBase entry			*/
  DBRef	Last;		/* last DBase entry			*/
  Term	ModuleOfDB;	/* module for this definition		*/
  Int         NOfEntries;	/* age counter                          */
  DBRef       Index;		/* age counter                          */
} LogUpdDBEntry;
typedef LogUpdDBEntry *LogUpdDBProp;
#define	LogUpdDBBit        0x1
#define	CodeDBBit          0x2

#define	LogUpdDBProperty   ((PropFlags)(0x8000|LogUpdDBBit))
#define	CodeLogUpdDBProperty   (DBProperty|LogUpdDBBit|CodeDBBit)
#define	CodeDBProperty     (DBProperty|CodeDBBit)

Inline(IsDBProperty, PropFlags, int, flags, ((flags & ~(LogUpdDBBit|CodeDBBit))  == DBProperty) )

#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepDB,DBProp,p,AtomBase + Unsigned(p))
Constructor(Prop,AbsDB,DBProp,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepDB,DBProp,p,p)
Constructor(Prop,AbsDB,DBProp,p,p)
#endif


/* These are the actual flags for DataBase terms */
typedef enum {
  DBAtomic   =  0x1,
  DBVar      =  0x2,
  DBNoVars   =  0x4,
  DBComplex  =  0x8,
  DBCode     = 0x10,
  DBNoCode   = 0x20,
  DBWithRefs = 0x40
} db_term_flags;

typedef	struct {
  Prop	NextOfPE;	     /* used to chain properties		*/
  PropFlags	KindOfPE;    /* kind of property			*/
  Atom	KeyOfBB;	     /* functor for this property		*/
  DBTerm *Element;	     /* blackboard element			*/
#if defined(YAPOR) || defined(THREADS)
  rwlock_t BBRWLock;            /* a read-write lock to protect the entry */
#endif
  Term	ModuleOfBB;   /* module for this definition		*/
} BlackBoardEntry;
typedef BlackBoardEntry *BBProp;

#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepBB,BlackBoardEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsBB,BlackBoardEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepBB,BlackBoardEntry *,p,p)
Constructor(Prop,AbsBB,BlackBoardEntry *,p,p)
#endif

#define BBProperty	((PropFlags)0xfffb)

Inline(IsBBProperty, PropFlags, int, flags, (flags == BBProperty))


/*		array property entry structure				*/
/*		first case is for dynamic arrays */
typedef	struct array_entry {
  Prop	NextOfPE;	/* used	to chain properties		*/
  PropFlags   KindOfPE;	/* kind of property			*/
  Int		ArrayEArity;	/* Arity of Array (positive)		*/
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ArRWLock;            /* a read-write lock to protect the entry */
#endif 
  struct array_entry  *NextArrayE; /* Pointer to the actual array		*/
 Term  ValueOfVE;		/* Pointer to the actual array		*/
} ArrayEntry;

/* second case is for static arrays */

/* first, the valid types */
typedef enum {
  array_of_ints,
  array_of_chars,
  array_of_uchars,
  array_of_doubles,
  array_of_ptrs,
  array_of_atoms,
  array_of_dbrefs,
  array_of_terms
} static_array_types;

typedef union  {
  Int *ints;
  char *chars;
  unsigned char *uchars;
  Float *floats;
  AtomEntry **ptrs;
  Term   *atoms;
  Term  *dbrefs;
  DBTerm   **terms;
} statarray_elements;

/* next, the actual data structure */
typedef	struct {
  Prop	NextOfPE;	/* used	to chain properties		*/
  PropFlags   KindOfPE;	/* kind of property			*/
  Int		ArrayEArity;	/* Arity of Array (negative)		*/
#if defined(YAPOR) || defined(THREADS)
  rwlock_t ArRWLock;            /* a read-write lock to protect the entry */
#endif
  static_array_types		ArrayType;	/* Type of Array Elements.		*/
  statarray_elements      ValueOfVE;	/* Pointer to the Array itself  */
} StaticArrayEntry;


#if USE_OFFSETS_IN_PROPS
Destructor(Prop,RepArray,ArrayEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsArray,ArrayEntry *,p,Addr(p)-AtomBase)
Destructor(Prop,RepStaticArray,StaticArrayEntry *,p, AtomBase + Unsigned(p))
Constructor(Prop,AbsStaticArray,StaticArrayEntry *,p,Addr(p)-AtomBase)
#else
Destructor(Prop,RepArray,ArrayEntry *,p,p)
Constructor(Prop,AbsArray,ArrayEntry *,p,p)
Destructor(Prop,RepStaticArray,StaticArrayEntry *,p,p)
Constructor(Prop,AbsStaticArray,StaticArrayEntry *,p,p)
#endif
#define	ArrayProperty ((PropFlags)0xfff7)

Inline(ArrayIsDynamic, int, ArrayEntry *, are, ((are)->ArrayEArity > 0 ))

Inline(IsArrayProperty, PropFlags, int, flags, (flags == ArrayProperty) )

/* Proto types */

/* cdmgr.c */
int		STD_PROTO(Yap_RemoveIndexation,(PredEntry *));

/* dbase.c */
void		STD_PROTO(Yap_ErDBE,(DBRef));
DBTerm	       *STD_PROTO(Yap_StoreTermInDB,(Term,int));
DBTerm	       *STD_PROTO(Yap_StoreTermInDBPlusExtraSpace,(Term,UInt));
Term		STD_PROTO(Yap_FetchTermFromDB,(DBTerm *));
void		STD_PROTO(Yap_ReleaseTermFromDB,(DBTerm *));

/* init.c */
Atom		STD_PROTO(Yap_GetOp,(OpEntry *,int *,int));

/* vsc: redefined to GetAProp to avoid conflicts with Windows header files */
Prop	STD_PROTO(Yap_GetAProp,(Atom,PropFlags));
Prop	STD_PROTO(Yap_GetAPropHavingLock,(AtomEntry *,PropFlags));

#if THREADS
EXTERN inline PredEntry *STD_PROTO(Yap_GetThreadPred, (PredEntry *));
#endif

EXTERN inline Prop
PredPropByFunc(Functor f, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  FunctorEntry *fe = (FunctorEntry *)f;

  WRITE_LOCK(fe->FRWLock);
  p0 = fe->PropsOfFE;
  while (p0) {
    PredEntry *p = RepPredProp(p0);
    if (/* p->KindOfPE != 0 || only props */
	(p->ModuleOfPred == cur_mod || !(p->ModuleOfPred))) {
#if THREADS
      /* Thread Local Predicates */
      if (p->PredFlags & ThreadLocalPredFlag) {
	WRITE_UNLOCK(fe->FRWLock);
	return AbsPredProp(Yap_GetThreadPred(p));
      }
#endif
      WRITE_UNLOCK(fe->FRWLock);
      return (p0);
    }
    p0 = p->NextOfPE;
  }
  return Yap_NewPredPropByFunctor(fe,cur_mod);
}

EXTERN inline Prop
PredPropByAtom(Atom at, Term cur_mod)
/* get predicate entry for ap/arity; create it if neccessary.              */
{
  Prop p0;
  AtomEntry *ae = RepAtom(at);

  WRITE_LOCK(ae->ARWLock);
  p0 = ae->PropsOfAE;
  while (p0) {
    PredEntry *pe = RepPredProp(p0);
    if ( pe->KindOfPE == PEProp && 
	 (pe->ModuleOfPred == cur_mod || !pe->ModuleOfPred)) {
#if THREADS
      /* Thread Local Predicates */
      if (pe->PredFlags & ThreadLocalPredFlag) {
	WRITE_UNLOCK(ae->ARWLock);
	return AbsPredProp(Yap_GetThreadPred(pe));
      }
#endif
      WRITE_UNLOCK(ae->ARWLock);
      return(p0);
    }
    p0 = pe->NextOfPE;
  }
  return Yap_NewPredPropByAtom(ae,cur_mod);
}

typedef enum {
  PROLOG_MODULE = 0,
  USER_MODULE = 1,
  IDB_MODULE = 2,
  ATTRIBUTES_MODULE = 3,
  CHARSIO_MODULE = 4,
  TERMS_MODULE = 5
} default_modules;


