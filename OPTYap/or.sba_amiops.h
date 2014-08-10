/************************************************************************
**                                                                     **
**                   The YapTab/YapOr/OPTYap systems                   **
**                                                                     **
** YapTab extends the Yap Prolog engine to support sequential tabling  **
** YapOr extends the Yap Prolog engine to support or-parallelism       **
** OPTYap extends the Yap Prolog engine to support or-parallel tabling **
**                                                                     **
**                                                                     **
**      Yap Prolog was developed at University of Porto, Portugal      **
**                                                                     **
************************************************************************/

#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif /* SCCS */

// keep eclipse happy, avoiding collisions with amiops.h
#ifdef YAPOR_SBA

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
		   } while (0 != (D))

#define derefa_body(D,A,LabelUnk,LabelNonVar)                \
		do {                                         \
                   (A) = (CELL *)(D);                        \
                   (D) = *(CELL *)(D);                       \
                   if(!IsVarTerm(D)) goto LabelNonVar;       \
		LabelUnk:                                    \
		   } while (0 != (D))

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
		   if (0 == (D)) break;                      \
		   if (IsPairTerm(D)) goto LabelList;        \
		} while (TRUE);
#endif /* UNIQUE_TAG_FOR_PAIRS */

EXTERN inline Term Deref(Term a)
{
   while(IsVarTerm(a)) {
	Term *b = (Term *) a;
	a = *b;
	if(a==0) return (Term)b;
   }
   return(a);
}

EXTERN inline Term Derefa(CELL *b)
{
  Term a = *b;
 restart:
  if (!IsVarTerm(a)) {
    return(a);
  } else if (a == 0) {
    return((CELL)b);
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


/* #define TRAIL(A)	if ((A) < HBREG || (A) > B) TrailTerm(TR++) = Unsigned(A)
 */

#define RESET_VARIABLE(V)       (*(CELL *)(V) = 0)

#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
inline EXTERN void
AlignGlobalForDouble(void)
{
  /* Force Alignment for floats. Note that garbage collector may
     break the alignment; */
  if (!DOUBLE_ALIGNED(H)) {
    RESET_VARIABLE(H);
    H++;
  }
}
#endif /* SIZEOF_DOUBLE == 2*SIZEOF_INT_P */

#ifdef YAPOR

#define DO_TRAIL(TERM, VAL)      \
{                                \
  register tr_fr_ptr r;          \
  r = TR;                        \
  TR = r + 1;                    \
  TrailTerm(r) = (CELL) (TERM);  \
  TrailVal(r) = (VAL);           \
}

#define DO_MATRAIL(TERM, OLDVAL, NEWVAL)    \
{                                \
  register tr_fr_ptr r = TR+1;          \
  TrailTerm(TR) = (OLDVAL);  /* disgusting hack */                \
  TrailTerm(r) = AbsAppl(TERM);  \
  TrailVal(r) = (NEWVAL);        \
  TR = r+1;     \
}

#define TRAIL_REF(REF)     TrailTerm(TR++) = AbsPair(((CELL *)(REF)))

/* convert to offset */
#define STACK_TO_SBA(A) (CELL *)(((char *)(A)+sba_offset))

#define IN_SBA(A) ((CELL)((char *)(A)-binding_array) < sba_size) 

#define SBA_TO_STACK(A) (CELL *)(((char *)(A)-sba_offset))

/* put the binding in the SBA and force ptr to point there */
#define BIND_SHARED_VARIABLE(A, D) {               \
  CELL *ptr;                                       \
/*shared_binds++;*/ \
  if (IN_SBA(A)) {                                 \
    ptr = SBA_TO_STACK(A);                         \
    DO_TRAIL(ptr,D);	                           \
    *(A) = (D);                                    \
  } else {                                         \
    DO_TRAIL((A),D);	                           \
    ptr = STACK_TO_SBA(A);                         \
    *(A) = (CELL)ptr;                              \
    *ptr = (D);                                    \
  }                                                \
}

/* put the binding in the SBA and force ptr to point there */
#define MABIND_SHARED_VARIABLE(A, D) {             \
/*shared_binds++;*/                                \
  if (IN_SBA(A)) {                                 \
    CELL *sptr = SBA_TO_STACK(A);                  \
    DO_MATRAIL(sptr, *(A), D);                     \
    *(A) = (D);                                    \
  } else {                                         \
    CELL *ptr3;                                    \
    DO_MATRAIL((A), *(A), D);                      \
    ptr3 = STACK_TO_SBA(A);                        \
    *(A) = (CELL)ptr3;                             \
    *ptr3 = (D);                                   \
  }                                                \
}

extern int condit_binds, shared_binds, uncond_binds;

/* put the binding in the stacks even though it is conditional */
#define BIND_CONDITIONALLY(A, D) {                 \
  DO_TRAIL(A,D);	                           \
/*condit_binds++; */\
  *(A) = (D);                                      \
}

/* put the binding in the stacks even though it is conditional */
#define MABIND_CONDITIONALLY(A, D) {               \
  DO_MATRAIL(A,*(A),D);                            \
/*condit_binds++; */\
  *(A) = (D);                                      \
}
 
#define DO_CONDITIONAL_BINDING(A,D) {              \
  if (Unsigned((Int)(A)-(Int)(H_FZ)) >             \
      Unsigned((Int)(B_FZ)-(Int)(H_FZ)))           \
    { BIND_SHARED_VARIABLE(A, D); }                \
  else { BIND_CONDITIONALLY(A,D); }                \
}

#define DO_CONDITIONAL_MABINDING(A,D) {            \
  if (Unsigned((Int)(A)-(Int)(H_FZ)) >             \
      Unsigned((Int)(B_FZ)-(Int)(H_FZ)))           \
    { MABIND_SHARED_VARIABLE(A, D); }              \
  else { MABIND_CONDITIONALLY(A,D); }              \
}

#define YapBind(A,D)          {                       \
 if (Unsigned((Int)(A)-(Int)(HBREG)) >             \
      Unsigned(BBREG)-(Int)(HBREG))                \
    { DO_CONDITIONAL_BINDING(A, D); }              \
 else /* uncond_binds++, */ *(A) = (D);            \
}

#define BIND(A,D,L)   YapBind(A,D)

#define MaBind(A,D)        {                       \
 if (Unsigned((Int)(A)-(Int)(HBREG)) >             \
      Unsigned(BBREG)-(Int)(HBREG))                \
    { DO_CONDITIONAL_MABINDING(A, D); }            \
 else /* uncond_binds++, */ *(A) = (D);            \
}

/* I can't gain much here because of the frozen registers */
#define Bind_Global(A,D)   YapBind(A,D)

#define Bind_Local(A,D)	   YapBind(A,D)

#define BIND_GLOBAL(A,D,L)   YapBind(A,D)

#define BIND_GLOBAL2(A,D,L1,L2)   YapBind(A,D)

#define BIND_GLOBALCELL(A,D)   YapBind(A,D); continue

#else /* YAPOR */
#ifdef TABLING
#define DO_TRAIL(TERM, VAL)      \
{                                \
  register tr_fr_ptr r;          \
  r = TR;                        \
  TR = r + 1;                    \
  TrailTerm(r) = (CELL) (TERM);  \
  TrailVal(r) = (VAL);           \
}

#define DO_MATRAIL(TERM, OLDVAL, VAL)    \
{                                \
  register tr_fr_ptr r = TR+1;          \
  TrailTerm(TR) = (OLDVAL);  /* disgusting hack */                \
  TR = r + 1;                    \
  TrailTerm(r) = AbsAppl((CELL *)(TERM));  \
  TrailVal(r) = (NEWVAL);        \
}
 
#define TRAIL(TERM, VAL)                          \
        if (Unsigned((Int)(TERM)-(Int)(HBREG)) >  \
            Unsigned((Int)(B)-(Int)(HBREG)))      \
        DO_TRAIL(TERM, VAL)

#define MATRAIL(TERM, OVAL, VAL)                  \
        if (Unsigned((Int)(TERM)-(Int)(HBREG)) >  \
            Unsigned((Int)(B)-(Int)(HBREG)))      \
        DO_MATRAIL(TERM, OVAL, VAL)

#define TRAIL_GLOBAL(TERM, VAL)                   \
 	if ((TERM) < HBREG) DO_TRAIL(TERM, VAL)

#define TRAIL_LOCAL(TERM, VAL)                    \
 	if ((TERM) > (CELL *)B) DO_TRAIL(TERM, VAL)

#define TRAIL_REF(REF)     TrailTerm(TR++) = AbsPair(((CELL *)(REF)))

#define YapBind(A,D)          { TRAIL(A,D); *(A) = (D); }

#define MaBind(A,D)        { MATRAIL(A,*(A),D); *(A) = (D); }

#define Bind_Global(A,D)   { TRAIL_GLOBAL(A,D); *(A) = (D); }

#define Bind_Local(A,D)	   { TRAIL_LOCAL(A,D); *(A) = (D); }

#else /* TABLING */

#ifdef i386

#define DO_TRAIL(A)                     \
{					\
  register tr_fr_ptr r; 		\
  r = TR;				\
  TR = r+1;				\
  TrailTerm(r) = (CELL)(A);		\
}


#define TRAIL(A)        if (Unsigned((Int)(A)-(Int)(HBREG)) >            \
 			    Unsigned((Int)(B)-(Int)(HBREG)))             \
				DO_TRAIL(A);

#define TRAIL_GLOBAL(A)	if ((A) < HBREG) DO_TRAIL(A);

#define TRAIL_LOCAL(A)	if ((A) > (CELL *)B) DO_TRAIL(A);


#elif __alpha

/* alpha machines have a move conditional instruction, which avoids a
   branch when jumping */
#define TRAIL(A)        TrailTerm(TR) = (CELL)(A);                       \
                        if (Unsigned((Int)(A)-(Int)(HBREG)) >            \
 			    Unsigned((Int)(B)-(Int)(HBREG)))             \
                            TR++

#define TRAIL_GLOBAL(A)	TR[0] = (CELL)(A); if ((A) < HBREG) TR++

#define TRAIL_LOCAL(A)	TR[0] = (CELL)(A); if ((A) > ((CELL *)(B)))  TR++

#else

#define DO_TRAIL(A)     TrailTerm(TR++) = (CELL)(A)

#define TRAIL(A)        if (Unsigned((Int)(A)-(Int)(HBREG)) >           \
 			    Unsigned((Int)(B)-(Int)(HBREG)))            \
                              DO_TRAIL(A)

#define TRAIL_GLOBAL(A)	if ((A) < HBREG) DO_TRAIL(A)

#define TRAIL_LOCAL(A)	if ((A) > ((CELL *)B))  DO_TRAIL(A)

#endif /* i386, _alpha */

#define TRAIL_REF(Ref)  (TrailTerm(TR++) = AbsPair(((CELL *)(Ref))))

/************************************************************

BINDING MACROS

A contains the address of the variable that is to be bound
D contains the value it will be bound to

*************************************************************/

#define YapBind(A,D)          	 { TRAIL(A); *(A) = (D); }

#define Bind_Global(A,D)         { TRAIL_GLOBAL(A); *(A) = (D); }

#define Bind_Local(A,D)		 { TRAIL_LOCAL(A); *(A) = (D); }

/************************************************************

Binding Macros for Multiple Assignment Variables.

************************************************************/

#define MA_TRAIL(A)    if (Unsigned((Int)(A)-(Int)(HBREG)) >          \
 			    Unsigned((Int)(B)-(Int)(HBREG)))          \
                              { TrailTerm(TR++) = *(A);               \
                                TrailTerm(TR++) = AbsAppl(A);         \
                              }

#define MaBind(A,D)    { MA_TRAIL(A); *(A) = (D); }

#endif /* TABLING */
#endif /* YAPOR */

#ifdef YAPOR

/* these two fields are used for memory management with the
   clean_up_node instruction in the YAPOR/SBA implementation  */
#define CP_FREE(B)        ((int)((B)->cp_env))
#define CP_NEXT(B)        ((choiceptr)((B)->cp_cp))

#endif /* YAPOR */

#define DBIND(A,D,L)  BIND(A,D,L)

#define EQ_OK_IN_CMP 1
#define LT_OK_IN_CMP 2
#define GT_OK_IN_CMP 4

#endif /* YAPOR_SBA */
