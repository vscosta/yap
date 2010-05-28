/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		amiops.h						 *
* Last rev:								 *
* mods:									 *
* comments:	Basic abstract machine operations, such as	         *
*               dereferencing, binding, trailing, and unification.       *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif /* SCCS */

#define  IsArrayReference(a) ((a)->array_access_func == FunctorArrayAccess)


/* dereferencing macros */

/************************************************************

Dereferencing macros

*************************************************************/

/* For DEREFD, D has both the input and the exit argument */
/* A is only used locally */

#define deref_head(D,Label)  if (IsVarTerm(D)) goto Label

#define deref_body(D,A,LabelUnk,LabelNonVar)                 \
		do {                                         \
                   if(!IsVarTerm(D)) goto LabelNonVar;       \
		LabelUnk:                                    \
                   (A) = (CELL *)(D);                        \
                   (D) = *(CELL *)(D);                       \
		   } while (Unsigned(A) != (D))

#define derefa_body(D,A,LabelUnk,LabelNonVar)                \
		do {                                         \
                   (A) = (CELL *)(D);                        \
                   (D) = *(CELL *)(D);                       \
                   if(!IsVarTerm(D)) goto LabelNonVar;       \
		LabelUnk:      ;                             \
		} while (Unsigned(A) != (D))

#if UNIQUE_TAG_FOR_PAIRS

/* If you have an unique tag for pairs you can use these macros which will
   speed up detection of dereferenced pairs, but will be slow
   for the other cases.

   The only instruction where this seems useful is 
   switch_list_nl
*/

#define deref_list_head(D,Label)  if (!IsPairTerm(D)) goto Label

#define deref_list_body(D,A,LabelList,LabelNonVar)           \
		do {                                         \
		   if (!IsVarTerm(D)) goto LabelNonVar;      \
                   (A) = (CELL *)(D);                        \
                   (D) = *(A);                               \
		   if (Unsigned(A) == (D)) break;            \
		   if (IsPairTerm(D)) goto LabelList;        \
		} while (TRUE);

#endif /* UNIQUE_TAG_FOR_PAIRS */

EXTERN Term STD_PROTO(Deref,(Term));
EXTERN Term STD_PROTO(Derefa,(CELL *));

EXTERN inline Term Deref(Term a)
{
   while(IsVarTerm(a)) {
	Term *b = (Term *) a;
	a = *b;
	if(a==((Term) b)) return a;
   }
   return(a);
}

EXTERN inline Term
Derefa(CELL *b)
{
  Term a = *b;
 restart:
  if (!IsVarTerm(a)) {
    return(a);
  } else if (a == (CELL)b) {
    return(a);
  } else {
    b = (CELL *)a;
    a = *b;
    goto restart;
  }
}

/************************************************************

TRAIL VARIABLE

A contains the address of the variable that is to be trailed

*************************************************************/


#define RESET_VARIABLE(V)       (*(CELL *)(V) = Unsigned(V))

#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT

EXTERN inline void
AlignGlobalForDouble(void)
{
  /* Force Alignment for floats. Note that garbage collector may
     break the alignment; */
  if (!DOUBLE_ALIGNED(H)) {
    RESET_VARIABLE(H);
    H++;
  }
}

#endif

#ifdef TABLING

#define DO_TRAIL(TERM, VAL)      \
{                                \
  register tr_fr_ptr r;          \
  r = TR;                        \
  TR = r + 1;                    \
  TrailTerm(r) = (Term) (TERM);  \
  TrailVal(r) = (CELL) (VAL);    \
}

#ifdef BFZ_TRAIL_SCHEME

#define TRAIL(TERM, VAL)                                  \
        if (OUTSIDE(HBREG,TERM,B) ||                      \
            ((TERM) > (CELL *)B_FZ))                      \
          DO_TRAIL(TERM, VAL)

#define TRAIL_LOCAL(TERM, VAL)                            \
 	if ((TERM) > (CELL *)B || (TERM) > (CELL *)B_FZ)  \
          DO_TRAIL(TERM, VAL)

#ifdef TERM_EXTENSIONS

#define Trail(TERM, VAL, LAB)                             \
        if (IN_BETWEEN(HBREG,TERM,B) &&                   \
            ((TERM) < (CELL *)B_FZ))                      \
          goto LAB

#define TrailAndJump(TERM, VAL)                             \
        if (IN_BETWEEN(HBREG,TERM,B) &&                   \
            ((TERM) < (CELL *)B_FZ))                      \
          GONext();

#else
#define Trail(TERM, VAL, LAB)                             \
          TRAIL(TERM, VAL)

#define Trail(TERM, VAL, LAB)                             \
          TRAIL_AND_JUMP(TERM, VAL)
#endif

#else /* BBREG_TRAIL_SCHEME */

#define TRAIL(TERM, VAL)                                  \
        if (OUTSIDE(HBREG,TERM,BBREG))                    \
          DO_TRAIL(TERM, VAL)

#ifdef TERM_EXTENSIONS
#define Trail(TERM, VAL, LAB)                             \
        if (IN_BETWEEN(HBREG,TERM,BBREG))                 \
          goto LAB

#define TrailAndJump(TERM, VAL)                             \
        if (IN_BETWEEN(HBREG,TERM,BBREG))                 \
          GONext();

#else
#define Trail(TERM, VAL, LAB)                             \
          TRAIL(TERM, VAL)

#define TrailAndJump(TERM, VAL)                             \
          TRAIL_AND_JUMP(TERM, VAL)

#endif

#define TRAIL_LOCAL(TERM, VAL)                            \
 	if ((TERM) > (CELL *)BBREG) DO_TRAIL(TERM, VAL)

#endif /* TRAIL_SCHEME */

/* ------------------------------------------------------ */

#define TRAIL_GLOBAL(TERM, VAL)                  \
 	if ((TERM) < HBREG) DO_TRAIL(TERM, VAL)

#ifdef TERM_EXTENSIONS
#define Trail_Global(TERM, VAL, LAB)              \
 	if ((TERM) >= HBREG) goto LAB
#else
#define Trail_Global(TERM, VAL, LAB)              \
          TRAIL_GLOBAL(TERM, VAL)
#endif

#define DO_MATRAIL(TERM, OLDVAL, NEWVAL)                    \
{                                                           \
  register tr_fr_ptr r = TR;                                \
  TR = r + 2;                                               \
  TrailVal(r) = (OLDVAL);                                   \
  TrailTerm(r) = TrailTerm(r+1) = AbsAppl((CELL *)(TERM));  \
  TrailVal(r+1) = (NEWVAL);                                 \
}

#define MATRAIL(TERM, OVAL, VAL)                  \
        if (OUTSIDE(HBREG,TERM,B))                \
          DO_MATRAIL(TERM, OVAL, VAL)

#else /* TABLING */

#if defined(i386) && !defined(TERM_EXTENSIONS)

#define DO_TRAIL(A,D)                   \
{					\
  register tr_fr_ptr r; 		\
  r = TR;				\
  TR = r+1;				\
  TrailTerm(r) = (CELL)(A);		\
}

#define TRAIL(A,D)        if (OUTSIDE(HBREG,A,B))             \
				DO_TRAIL(A,D);

#define TRAIL_AND_JUMP(A,D)        if (!OUTSIDE(HBREG,A,B)) GONext();	\
				DO_TRAIL(A,D);

#define Trail(A, D, LAB)   TRAIL(A,D)

#define TRAIL_GLOBAL(A,D)	if ((A) < HBREG) DO_TRAIL(A,D);

#define Trail_Global(A,D,LAB)	if ((A) < HBREG) DO_TRAIL(A,D);

#define TRAIL_LOCAL(A,D)	if ((A) > (CELL *)B) DO_TRAIL(A,D);


#elif defined(__alpha) && !defined(TERM_EXTENSIONS)

/* alpha machines have a move conditional instruction, which avoids a
   branch when jumping */
#define TRAIL(A,D)        TrailTerm(TR) = (CELL)(A);                       \
                        if (OUTSIDE(HBREG,A,B))                            \
                            TR++

#define TRAIL(A,D)        TrailTerm(TR) = (CELL)(A);                       \
                        if (!OUTSIDE(HBREG,A,B))                            \
			  GONext();

#define Trail(A,D,LAB)    TRAIL(A,D)

#define TRAIL_GLOBAL(A,D)	TR[0] = (CELL)(A); if ((A) < HBREG) TR++

#define Trail_Global(A,D,LAB)   TRAIL_GLOBAL(A,D)

#define TRAIL_LOCAL(A,D)	TR[0] = (CELL)(A); if ((A) > ((CELL *)(B)))  TR++

#elif !defined(TERM_EXTENSIONS)

#define DO_TRAIL(A,D)     TrailTerm(TR++) = (CELL)(A)

#define TRAIL(A,D)        if (OUTSIDE(HBREG,A,B))            \
                              DO_TRAIL(A,D)

#define TRAIL_AND_JUMP(A,D)        if (IN_BETWEEN(HBREG,A,B)) GONext();	\
                              DO_TRAIL(A,D)

#define Trail(A,D,LAB)    TRAIL(A,D)

#define TRAIL_GLOBAL(A,D)	if ((A) < HBREG) DO_TRAIL(A,D)

#define Trail_Global(A,D,LAB)  TRAIL_GLOBAL(A,D)

#define Trail_Global2(A,D,LAB)  TRAIL_GLOBAL(A,D)

#define TRAIL_LOCAL(A,D)	if ((A) > ((CELL *)B))  DO_TRAIL(A,D)

#else

#define DO_TRAIL(A,D)     TrailTerm(TR++) = (CELL)(A)

#define TRAIL(A,D)        if (OUTSIDE(HBREG,A,B))            \
                              DO_TRAIL(A,D)

#define Trail(A,D,LAB)    if (IN_BETWEEN(HBREG,A,B))            \
                              goto LAB

#define TrailAndJump(A,D)    if (IN_BETWEEN(HBREG,A,B))            \
    GONext();

#define TRAIL_GLOBAL(A,D)	if ((A) < HBREG) DO_TRAIL(A,D)

#define Trail_Global(A,D,LAB) if ((A) >= HBREG) goto LAB

#define Trail_Global2(A,D,LAB) if ((A) < HBREG) goto LAB

#define TRAIL_LOCAL(A,D)	if ((A) > ((CELL *)B))  DO_TRAIL(A,D)

#endif

/************************************************************

Binding Macros for Multiple Assignment Variables.

************************************************************/

#define DO_MATRAIL(VP, OLDV, D)                                  \
        { TrailTerm(TR+1) = OLDV;                                \
          TrailTerm(TR) = TrailTerm(TR+2) = AbsAppl(VP);         \
          TR += 3;                                               \
        }

#define MATRAIL(VP,OLDV,D)    if (OUTSIDE(HBREG,VP,B))          \
                           DO_MATRAIL(VP, OLDV, D)

#endif /* TABLING */


#define REF_TO_TRENTRY(REF)    AbsPair(((CELL *)&((REF)->Flags)))
#define CLREF_TO_TRENTRY(REF)  AbsPair(((CELL *)&((REF)->ClFlags)))

#define TRAIL_REF(REF)         TrailTerm(TR++) = REF_TO_TRENTRY(REF)
#define TRAIL_CLREF(REF)       TrailTerm(TR++) = CLREF_TO_TRENTRY(REF)
#define TRAIL_LINK(REF)        TrailTerm(TR++) = AbsPair((CELL *)(REF))
#define TRAIL_FRAME(FR)        DO_TRAIL(AbsPair((CELL *)(Yap_TrailBase)), FR)

#define Bind(A,D)              TRAIL(A,D); *(A) = (D)
#define Bind_Global(A,D)       TRAIL_GLOBAL(A,D); *(A) = (D)
#define Bind_and_Trail(A,D)    DO_TRAIL(A,D); *(A) = (D)
#define BIND(A,D,L)            *(A) = (D); Trail(A,D,L)
#define BIND_AND_JUMP(A,D)     *(A) = (D); TrailAndJump(A,D)
#define BIND_GLOBAL(A,D,L)     *(A) = (D); Trail_Global(A,D,L)

#ifdef COROUTINING
#define BIND_GLOBAL2(A,D,LAB,LAB1)   *(A) = (D); if ((A) < HBREG) goto LAB; goto LAB1

#define BIND_GLOBALCELL(A,D)    *(A) = (D); \
				if ((A) >= HBREG) continue; \
                                TRAIL_GLOBAL(A,D); if (!IsAttVar(A)) continue; \
                                Yap_WakeUp((A)); continue

#define BIND_GLOBALCELL_NONATT(A,D)    *(A) = (D); \
				if ((A) >= HBREG) continue; \
                                TRAIL_GLOBAL(A,D);
#else
#define BIND_GLOBAL2(A,D,LAB,LAB1)   BIND_GLOBAL(A,D,LAB)

#define BIND_GLOBALCELL(A,D)    BIND_GLOBAL(A,D,L); continue

#define BIND_GLOBALCELL_NONATT(A,D)    BIND_GLOBALCELL; continue
#endif

#define Bind_Local(A,D)	   { TRAIL_LOCAL(A,D); *(A) = (D); }


#define MaBind(VP,D)    { MATRAIL((VP),*(VP),(D)); *(VP) = (D); }

#if defined(__GNUC__) && defined(i386) && !defined(TERM_EXTENSIONS) && !defined(TABLING)
/* destroy d0 and pt0 */
#define DBIND(A,D,L)                                                    \
{ register CELL *t1=HBREG;                                              \
__asm__("movl %4,(%0)\n\t"                                              \
        "movl %2,%4\n\t"                                                \
	"subl %1,%2\n\t"                                                \
	"subl %0,%4\n\t"                                                \
	"cmpl %2,%4\n\t"                                                \
	"jae  1f\n\t"                                                   \
	"movl %3,%4\n\t"                                                \
	"movl %0,(%4)\n\t"                                              \
	"addl $4,%4\n\t"                                                \
	"movl %4,%3\n\t"                                                \
	"1:"                                                            \
	: /* no outputs */                                              \
	: "r" (A), "m" (B), "r" (t1), "m" (TR), "r" (D) );              \
}

#else
#define DBIND(A,D,L)  BIND(A,D,L)
#endif


/************************************************************

Unification Routines

*************************************************************/

EXTERN Int STD_PROTO(Yap_unify,(Term,Term));

inline EXTERN void STD_PROTO(reset_trail,(tr_fr_ptr));

inline EXTERN void
reset_trail(tr_fr_ptr TR0) {
  while(TR != TR0) {
    CELL d1;
    --TR;
    d1 = TrailTerm(TR);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    if (IsVarTerm(d1)) {
#endif
      CELL *pt = (CELL *)d1;
      RESET_VARIABLE(pt);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    } else {
      CELL *pt = RepAppl(d1);
      /* AbsAppl means */
      /* multi-assignment variable */
      /* so the next cell is the old value */ 
#ifdef FROZEN_STACKS
      pt[0] = TrailVal(TR-1);
      TR -= 1;
#else
      pt[0] = TrailTerm(TR-1);
      TR -= 2;
#endif /* FROZEN_STACKS */
    }
#endif
  }
}

inline EXTERN void reset_attvars(CELL *dvarsmin, CELL *dvarsmax);

inline EXTERN void
reset_attvars(CELL *dvarsmin, CELL *dvarsmax) {
  if (dvarsmin) {
    dvarsmin += 1;
    do {
      CELL *newv;
      newv = CellPtr(*dvarsmin);
      RESET_VARIABLE(dvarsmin+1);
      if (IsUnboundVar(dvarsmin))
	break;
      RESET_VARIABLE(dvarsmin);
      dvarsmin = newv;
    } while (TRUE);
  }
}

inline EXTERN void close_attvar_chain(CELL *dvarsmin, CELL *dvarsmax);

inline EXTERN void
close_attvar_chain(CELL *dvarsmin, CELL *dvarsmax) {
  if (dvarsmin) {
    dvarsmin += 1;
    do {
      CELL *newv;
      Bind(dvarsmin+1, dvarsmin[1]);
      if (IsUnboundVar(dvarsmin))
	break;
      newv = CellPtr(*dvarsmin);
      RESET_VARIABLE(dvarsmin);
      dvarsmin = newv;
    } while (TRUE);
  }
}

EXTERN inline
Int Yap_unify(Term t0, Term t1)
{
  tr_fr_ptr TR0 = TR;

  if (Yap_IUnify(t0,t1)) {
    return TRUE;
  } else {
    reset_trail(TR0);
    return FALSE;
  }
}

EXTERN Int STD_PROTO(Yap_unify_constant,(Term,Term));

EXTERN inline Int
Yap_unify_constant(register Term a, register Term cons)
{
  CELL *pt;
  deref_head(a,unify_cons_unk);
 unify_cons_nonvar:
  {
    if (a == cons) return(TRUE);
    else if (IsApplTerm(a)) {
      Functor f;
      if (!IsApplTerm(cons))
	return(FALSE);
      f = FunctorOfTerm(a);
      if (f != FunctorOfTerm(cons))
	return(FALSE);
      if (IsExtensionFunctor(f)) {
	switch((CELL)f) {
	case (CELL)FunctorDBRef:
	  return(a == cons);
	case (CELL)FunctorLongInt:
	  {
	    CELL d0 = RepAppl(a)[1];
	    CELL d1 = RepAppl(cons)[1];
	    return d0 == d1;
	  }
	case (CELL)FunctorDouble:
	  {
	    Float d0 = FloatOfTerm(a);
	    Float d1 = FloatOfTerm(cons);
	    return d0 == d1;
	  }
	case (CELL)FunctorBigInt:
#ifdef USE_GMP
	  return (Yap_gmp_tcmp_big_big(a, cons) == 0);
#endif /* USE_GMP */
	default:
	  return FALSE;
	}
      }
    } else
      return FALSE;
  }

  deref_body(a,pt,unify_cons_unk,unify_cons_nonvar);
  BIND(pt,cons,wake_for_cons);
#ifdef COROUTINING
  DO_TRAIL(pt, cons);
  if (IsAttVar(pt)) Yap_WakeUp(pt);
 wake_for_cons:
#endif
  return(TRUE);
}


#define EQ_OK_IN_CMP 1
#define LT_OK_IN_CMP 2
#define GT_OK_IN_CMP 4

static inline int
do_cut(int i) {
#ifdef CUT_C
  while (POP_CHOICE_POINT(B->cp_b)) {
    cut_c_pop();
  }
#endif
  Yap_TrimTrail();
  B = B->cp_b;
  return i;
}

#define cut_succeed() return do_cut(TRUE)

#define cut_fail() return do_cut(FALSE)

