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

#define NextClause(X)	(((yamop *)X)->u.ld.d)

#define PredFirstClause		0
#define PredMiddleClause	1
#define PredLastClause		2

typedef struct logic_upd_clause {
  /* A set of flags describing info on the clause */
  CELL            ClFlags;
#if defined(YAPOR) || defined(THREADS)
  /* A lock for manipulating the clause */
  lockvar          ClLock;
  UInt             ref_count;
#endif
  union {
    yamop          *ClVarChain;    /* indexing code for log. sem. */
  } u;
  /* extra clause information for logical update indices and facts */
  union {
    /* extra clause information for logical update semantics, rules with envs */
    yamop           *ClExt;
    /* extra clause information for logical update indices and facts */
    Int             ClUse;
  } u2;
  /* The instructions, at least one of the form sl */
  yamop            ClCode[MIN_ARRAY];
  Atom Owner;
} LogUpdClause;

typedef struct dynamic_clause {
  /* A set of flags describing info on the clause */
  CELL            ClFlags;
#if defined(YAPOR) || defined(THREADS)
  /* A lock for manipulating the clause */
  lockvar          ClLock;
  UInt             ref_count;
#endif
  Atom Owner;
  yamop              *ClPrevious;     /* immediate update clause */
  /* The instructions, at least one of the form sl */
  yamop            ClCode[MIN_ARRAY];
} DynamicClause;

typedef struct static_clause {
  /* A set of flags describing info on the clause */
  CELL            ClFlags;
  Atom Owner;
  /* The instructions, at least one of the form sl */
  yamop            ClCode[MIN_ARRAY];
} StaticClause;

typedef struct dead_clause {
  CELL            ClFlags;
  struct dead_clause *NextCl;       /* dead clause */
#if defined(YAPOR) || defined(THREADS)
  /* A lock for manipulating the clause */
  lockvar          ClLock;
  UInt             ref_count;
#endif
} DeadClause;

typedef union clause_obj {
  struct logic_upd_clause luc;
  struct dynamic_clause ic;
  struct static_clause sc;
} ClauseUnion;

#define ClauseCodeToDynamicClause(p)    ((DynamicClause *)((CODEADDR)(p)-(CELL)(((DynamicClause *)NULL)->ClCode)))
#define ClauseCodeToStaticClause(p)    ((StaticClause *)((CODEADDR)(p)-(CELL)(((StaticClause *)NULL)->ClCode)))
#define ClauseCodeToLogUpdClause(p)    ((LogUpdClause *)((CODEADDR)(p)-(CELL)(((LogUpdClause *)NULL)->ClCode)))

#define ClauseFlagsToDynamicClause(p)    ((DynamicClause *)(p))
#define ClauseFlagsToLogUpdClause(p)     ((LogUpdClause *)(p))
#define ClauseFlagsToStaticClause(p)     ((StaticClause *)(p))

#define DynamicFlags(X)		(ClauseCodeToDynamicClause(X)->ClFlags)

#define DynamicLock(X)		(ClauseCodeToDynamicClause(X)->ClLock)

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
wamreg  STD_PROTO(Yap_regnotoreg,(UInt));

/* cdmgr.c */
void	STD_PROTO(Yap_RemoveLogUpdIndex,(LogUpdClause *));
void	STD_PROTO(Yap_IPred,(PredEntry *));
void	STD_PROTO(Yap_addclause,(Term,yamop *,int,int));

/* dbase.c */
void	STD_PROTO(Yap_ErCl,(DynamicClause *));
void	STD_PROTO(Yap_ErLogUpdCl,(LogUpdClause *));

/* exec.c */
Term    STD_PROTO(Yap_cp_as_integer,(choiceptr));

/* index.c */
yamop   *STD_PROTO(Yap_PredIsIndexable,(PredEntry *));


#if LOW_PROF
/* profiling */
yamop *Yap_prof_end;
#endif /* LOW_PROF */
