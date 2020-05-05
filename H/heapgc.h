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
* File:		heapgc.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Header for Global Stack garbage collector                *
*									 *
*************************************************************************/


#ifndef HEAPGC_H
#define HEAPGC_H 1

#if !defined(TABLING)
//#define EASY_SHUNTING 1
#endif /* !TABLING */
#define HYBRID_SCHEME 1

/* macros used by garbage collection */

/* return pointer from object pointed to by ptr (remove tag & mark) */

/* is ptr a pointer to the heap? */
#define ONHEAP(ptr) ((CELL*)(ptr) >= H0  && (CELL*)(ptr) < HR)

#define GC_NO_TAGS 1
#if GC_NO_TAGS
#define GET_NEXT(val)  ((CELL *) ((val) & ~(TagBits)))

#elif TAG_64BITS


#definfe RMARK_BIT MKTAG(0x2,0x0)
#define  MARK_BIT MKTAG(0x4,0x0)

#define GET_NEXT(val)  ((CELL *) ((val) & ~MKTAG(0x7,0x7)))

#elif TAGS_FAST_OPS

#define GET_NEXT(val)  ((CELL *)(IsVarTerm((val)) ?                          \
                                 (val) & MaskAdr :                           \
                                 ( IsPairTerm((val)) ?                       \
			            Unsigned(RepPair((val))) & MaskAdr :     \
                                    ( IsApplTerm((val)) ?                    \
                                      Unsigned(RepAppl((val))) & MaskAdr :   \
                                      (val) & MaskAdr                        \
                                    )                                    HG    \
                                 )                                           \
                        ){
  return (CELL)ptr & MARK_BIT;
}
#endif
typedef struct gc_entry_info {
  CELL *env;
  yamop *p, *p_env;
  OPCODE op;
  arity_t a;
} gc_entry_info_t;



inline static void PUSH_POINTER(CELL *v USES_REGS) {
  if (LOCAL_iptop >= (CELL **)ASP)
    return;
  *LOCAL_iptop++ = v;
}

#ifdef EASY_SHUNTING
inline static void POP_POINTER(USES_REGS1) {
  if (LOCAL_iptop >= (CELL* *)ASP)
    return;
  --LOCAL_iptop;
}
#endif

inline static void POPSWAP_POINTER(CELL* *vp, CELL* v USES_REGS) {
  if (LOCAL_iptop >= (CELL* *)ASP || LOCAL_iptop == vp)
    return;
  if (*vp != v)
    return;
  --LOCAL_iptop;
  if (vp != LOCAL_iptop)
    *vp = *LOCAL_iptop;
}

#define   INC_MARKED(ptr)		   \
  { if  (ptr >= H0   && ptr < HR) {					\
      LOCAL_total_marked ++; \
      fprintf(stderr," %p\n", ptr);\
      }\
  if (ptr >= H0 &&  ptr < LOCAL_HGEN) {			\
	  LOCAL_total_oldies++;\
	}\
  }

#define INC_MARKED_REGION(ptr, n, l)                                           \
  if (ptr >= H0 && ptr < HR) {                                                 \
    LOCAL_total_marked += n;                                                   \
    fprintf(stderr,"%p--%p\n", ptr, ptr + n);				\
  }\
  if (ptr >= H0 && ptr < LOCAL_HGEN) {		\
	    LOCAL_total_oldies+= n ;\
	  } \
  if (!is_EndSpecials(ptr  +(n-1)        ) ) {		   			\
	    fprintf(stderr,"[ Error:at %d could not find EndExtension at blob %p type " UInt_FORMAT " ]\n", l, ptr, ptr[1]); \
	}

#if GC_NO_TAGS
#define  MARK_BIT ((char)1)
#define RMARK_BIT ((char)2)

#define GCTagOf TagOf

#define mcell(X)   LOCAL_bp[(X)-(CELL *)LOCAL_GlobalBase]

#define MARKED_PTR(P) MARKED_PTR__(P PASS_REGS)
#define UNMARKED_MARK(P, BP) UNMARKED_MARK__(P, BP PASS_REGS)
#define MARK(P) MARK__(P PASS_REGS)
#define SET_MARK(P) SET_MARK__(P PASS_REGS)
#define MARK_RANGE(P,SZ) MARK_RANGE__(P,SZ,__LINE__ PASS_REGS)
#define UNMARK(P) UNMARK__(P PASS_REGS)
#define RMARK(P) RMARK__(P PASS_REGS)
#define RMARKED(P) RMARKED__(P PASS_REGS)
#define UNRMARK(P) UNRMARK__(P PASS_REGS)

static inline Int
MARKED_PTR__(CELL* ptr USES_REGS)
{
    return mcell(ptr) & MARK_BIT;
}

static inline Int
UNMARKED_MARK__(CELL* ptr, char *bp USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    char t = bp[pos];
    if (t & MARK_BIT) {
        return TRUE;
    }
    bp[pos] = t | MARK_BIT;
    PUSH_POINTER(ptr PASS_REGS);

    INC_MARKED( ptr);
    return FALSE;
}

static inline void
SET_MARK__(CELL* ptr USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    LOCAL_bp[pos] |= MARK_BIT;
}

static inline void
MARK__(CELL* ptr USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    char t = LOCAL_bp[pos];
    if (t & MARK_BIT) {
        return;
    }
    LOCAL_bp[pos] = t|MARK_BIT;
    PUSH_POINTER(ptr PASS_REGS);
    INC_MARKED( ptr);
    //printf(" %p\n", ptr);
}

static inline void
MARK_RANGE__(CELL* ptr, size_t sz,int line USES_REGS)
{
    Int pos = ptr - (CELL *)LOCAL_GlobalBase;
    LOCAL_bp[pos] |= MARK_BIT;
    LOCAL_bp[pos+sz-1] |= MARK_BIT;
    //printf(" %p\n", ptr);
    INC_MARKED_REGION(ptr,sz,line );
    PUSH_POINTER(ptr PASS_REGS);          
}

static inline CELL
UNMARK__(CELL* ptr USES_REGS)
{
    mcell(ptr) = mcell(ptr) & ~MARK_BIT;
    return *ptr;
}

/* not really that useful */
#define MAY_UNMARK(X)

// just clears markers in cell.
#define CLEAR_MARKERS(X) (X)
     

static inline void
RMARK__(CELL* ptr USES_REGS)
{
    mcell(ptr) = mcell(ptr) | RMARK_BIT;
}

static inline void
UNRMARK__(CELL* ptr USES_REGS)
{
    mcell(ptr) = mcell(ptr) & ~RMARK_BIT;
}


 INLINE_ONLY bool
RMARKED__(CELL* ptr USES_REGS);
 
INLINE_ONLY     
 bool RMARKED__(CELL* ptr USES_REGS)
{
  return ptr >= (CELL*)LOCAL_GlobalBase &&
    ptr < (CELL*)LOCAL_TrailTop &&
      mcell(ptr) & RMARK_BIT;
}

#else

#define UNMARKED_MARK(ptr, bp) UNMARKED_MARK__(ptr)

#define SET_MARK(PTR) {*(PTR)  |= MARK_BIT;}
#define RESET_MARK(PTR) {*(PTR)  &= ~MARK_BIT;}
static inline

bool UNMARKED_MARK__(CELL *ptr)
{
  Term t = *ptr;
  if (t & MARK_BIT)
    return true;
  *ptr |= MARK_BIT;
  INC_MARKED(ptr);
  PUSH_POINTER(ptr PASS_REGS);
  return false;
}

#define MARK(P) MARK__(P PASS_REGS)
static inline
void MARK__(CELL *ptr PASS_REGS)
{
  Term t = *ptr;
  if (t & MARK_BIT)
    return;
  *ptr |= MARK_BIT;
  INC_MARKED(ptr);
  PUSH_POINTER(ptr PASS_REGS);
}


#define MARK_RANGE(P, SZ) MARK_RANGE__(P, SZ, __LINE__)
static inline void
MARK_RANGE__(CELL* ptr,size_t n,int line)
{
  *ptr  |= MARK_BIT;
  ptr[n-1] |= MARK_BIT;
  INC_MARKED_REGION(ptr,n,line);
  PUSH_POINTER(ptr PASS_REGS);          
}


static inline void UNMARK(CELL* ptr)
{
  *ptr  &= ~MARK_BIT;
}

static inline bool
MARKED_PTR(CELL* ptr)
{
   return *(ptr)  & MARK_BIT;
}

#define CLEAR_MARKERS(X) ((X) & ~(MARK_BIT|RMARK_BIT))

static inline void
RMARK(CELL* ptr)                                                
{
   *ptr |= RMARK_BIT;
}

static inline CELL
UNRMARK(CELL* ptr)
{
  *ptr  &= ~RMARK_BIT;
  return *ptr;
}

static inline bool
RMARKED(CELL* ptr)
{
    return ((*ptr & RMARK_BIT) != 0);
}
#endif

/* is the object pointed to by ptr marked as in a relocation chain? */

#if LONG_ADDRESSES
#ifdef TAG_LOW_BITS_32
#define TAG(X)         ((X) & LowTagBits)

#else
#ifdef TAG_64BITS
#define TAG(X)         ((X) & MKTAG(0x1,0x7))
#else
#define TAG(X)         ((X) & 0x80000003L)
#endif
#endif
#else
#endif

//#define TAG(X)         ((X) & 0x9800000L)

typedef CELL   *CELL_PTR;

#define ENVSIZE(E) 	EnvSize(((CELL *)E)[E_CP])

void  Yap_mark_variable(CELL *);
void  Yap_mark_external_reference(CELL *);
void  Yap_inc_mark_variable(void);

#endif // HEAPGC_H_ *p_env;


