/*************************************************************************
*									 *
*	 YAP Prolog 	%W% %G%
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		index.h							 *
* Last rev:								 *
* mods:									 *
* comments:	indexation info						 *
*									 *
*************************************************************************/

/* Minimum number of clauses needed to build an hash table */
#define	MinHashEntries	4

/* Four types of Clauses */
#define NonVarCl(X)	((X) != VarCl)
#define MaxOptions	(AtCl+1)

/* Some Flags */
#define LoneGroup	0x01		/* just a group */
#define	FirstIndex	0x02		/* we are working over first arg */
#define	HeadIndex	0x04		/* we are working over the head */
#define	LastFoundList	0x08		/* informs first arg is a list */
#define	LastGroup	0x10		/* this is the last group */
#define	IsAtom		0x20		/* the last value is an atom */
#define	IsStruct	0x40		/* the last value is a compound term */



/* Intermediate Data structures,
   used to build the indexing code */

/* Used to store all important information about a clause */
typedef struct StructClauseDef {
	int Kind;		/* type of first argument */
	Term Name;		/* if nonvar or nonlist, first argument */
	yamop *Code;		/* start of code for clause */
	struct StructClauseDef *Next; /* next clause in chain */
	} ClauseDef;


/* Relevant information for groups */
typedef struct {
	int Type[MaxOptions];	/* quantity of elements of each kind */
	int NCl;		/* total amount of clauses */
	int SInfo;		/* special info about group */
        int NofClausesAfter;	/* number of clauses after the group */
	ClauseDef *Start;	/* first clause of group */
	yamop *First,*Last;	/* first and last code of clauses in group */
	} GroupDef;

/* SInfo may be one of: */
#define OnlyNils	0x1
#define UsesBips	0x2


/* Different elements of the same kind in a group */
typedef	struct {
	Term Class;		/* description of element */
	CELL Code;		/* code that deals with it */
	ClauseDef *First, *Last; /* first and last clause with that term */
	} EntryDef;

#define IsVarClause(X)	( ClauseCodeToClause(X)->ClFlags & FIsVar )

#define TermOfCl(X)	( ClauseCodeToClause(X)->u.ClValue )
#define HeadOfList(X)	( ClauseCodeToClause(X)->u.ClValue )

#define	FinalGr(I)	((I) == NGroups - 1 && (I) != 0)

/*
 * Number of clauses before you disable extended single optimisation.
 */  
#define CLAUSES_FOR_EXTENDED_SINGLE 16


