/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%		*
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		clause.h						 *
* Last rev:								 *
* mods:									 *
* comments:	clause info						 *
*									 *
*************************************************************************/

#include "Yatom.h"
#include "Heap.h"

/* consulting files */

typedef union CONSULT_OBJ {
  char *filename;
  int mode;
  Prop  p;
  UInt c;
} consult_obj;

/* Either we are assembling clauses or indexing code */

#define ASSEMBLING_CLAUSE	0
#define ASSEMBLING_INDEX	1

/* This information is put at the start of every clause */

#define VarCl		0x0000	/* The clause's first argument is a var */
#define ListCl		0x0001	/* The clause's first argument is a list */
#define ApplCl		0x0002	/* The clause's first argument is an Appl */
#define AtCl		0x0003	/* The clause's first argument is a const */

/* If the firs argument is a list, then we care about what 
   we have in its head */
#define FHeadVar	0x0000	/* The head of the first argument is a var */
#define FHeadList	0x0004	/* The head of the first argument is a list */
#define FHeadAppl	0x0008	/* The head of the first argument ia an Appl */
#define FHeadCons	0x000c	/* The head of the first argument is a cons */

/* If the first argument is a variable, then it may be tipified later */
#define FIsVar		0x0010	/* ... :- var(X)... */
#define FIsAtom		0x0020	/* ... :- atom(X) .... */
#define FIsNum		0x0040  /* ... :- integer(X) ...
				   ... :- number(X) ... */
#define FIsPrimi	0x0080	/* ... :- atomic(X) ...
				   ... :- primitive(X) ... */

#define FirstArgOfClType(X) ((X) & 0x03 )
#define HeadOfClType(X)    ( ((X) >> 2) & 0x03 )

#define KindOfArg(X)	   FirstArgOfClType(ClauseCodeToClause(X)->ClFlags)
#define KindOfListArg(X)   HeadOfClType(ClauseCodeToClause(X)->ClFlags)
#define KindOfBipArg(X)    ClauseCodeToClause(X)->ClFlags

#define NextClause(X)	(((yamop *)X)->u.ld.d)

#define PredFirstClause		0
#define PredMiddleClause	1
#define PredLastClause		2

typedef struct clause_struct {
 /* This info is used by the indexing algorithm and by the dynamic clauses.
    It is either the value of the first arg for static clauses or a pointer
    to the previous clause */
  union {
    CELL		ClValue;        /* indexable clause */
    yamop              *ClPrevious;     /* immediate update clause */
    CODEADDR            ClInfo;         /* indexing code for log. sem. */
    yamop              *ClVarChain;     /* log. sem. indexing code */
    struct clause_struct *NextCl;       /* dead clause */
  } u;
  /* the actual owner of the clause */
  Atom Owner;
  /* A set of flags describing info on the clause */
  CELL            ClFlags;
#if defined(YAPOR) || defined(THREADS)
  /* A lock for manipulating the clause */
  lockvar          ClLock;
  UInt             ref_count;
#endif
  union {
    /* extra clause information for logical update semantics, rules with envs */
    yamop           *ClExt;
    /* extra clause information for logical update indices and facts */
    Int             ClUse;
  } u2;
  /* The instructions, at least one of the form sl */
  yamop            ClCode[MIN_ARRAY];
} Clause;

#define ClauseCodeToClause(p)    ((Clause *)((CODEADDR)(p)-(CELL)(((Clause *)NULL)->ClCode)))
#define ClauseFlagsToClause(p)    ((Clause *)((CODEADDR)(p)-(CELL)(&(((Clause *)NULL)->ClFlags))))

#define DynamicFlags(X)		(ClauseCodeToClause(X)->ClFlags)

#define DynamicLock(X)		(ClauseCodeToClause(X)->ClLock)

#if defined(YAPOR) || defined(THREADS)
#define INIT_CLREF_COUNT(X) (X)->ref_count = 0
#define  INC_CLREF_COUNT(X) (X)->ref_count++
#define  DEC_CLREF_COUNT(X) (X)->ref_count--
#define        CL_IN_USE(X) ((X)->ref_count != 0)
#else
#define INIT_CLREF_COUNT(X)
#define  INC_CLREF_COUNT(X) 
#define  DEC_CLREF_COUNT(X) 
#define        CL_IN_USE(X) ((X)->ClFlags & InUseMask)
#endif

/* amasm.c */
wamreg	STD_PROTO(Yap_emit_x,(CELL));
wamreg  STD_PROTO(Yap_compile_cmp_flags,(PredEntry *));
void    STD_PROTO(Yap_InitComma,(void));

/* cdmgr.c */
void	STD_PROTO(Yap_RemoveLogUpdIndex,(Clause *));
void	STD_PROTO(Yap_IPred,(PredEntry *));
void	STD_PROTO(Yap_addclause,(Term,yamop *,int,int));

/* dbase.c */
void	STD_PROTO(Yap_ErCl,(Clause *));

/* exec.c */
Term    STD_PROTO(Yap_cp_as_integer,(choiceptr));

/* index.c */
yamop   *STD_PROTO(Yap_PredIsIndexable,(PredEntry *));


