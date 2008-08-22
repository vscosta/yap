/*************************************************************************
 *									 *
 *	 YAP Prolog    @(#)amidefs.h	1.3 3/15/90                      *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		amidefs.h						 *
 * comments:	Abstract machine peculiarities				 *
 *									 *
 * Last rev:     $Date: 2008-07-22 23:34:49 $							 *
 * $Log: not supported by cvs2svn $
 * Revision 1.33  2007/11/26 23:43:09  vsc
 * fixes to support threads and assert correctly, even if inefficiently.
 *
 * Revision 1.32  2006/10/10 14:08:17  vsc
 * small fixes on threaded implementation.
 *
 * Revision 1.31  2006/09/20 20:03:51  vsc
 * improve indexing on floats
 * fix sending large lists to DB
 *
 * Revision 1.30  2005/12/17 03:25:39  vsc
 * major changes to support online event-based profiling
 * improve error discovery and restart on scanner.
 *
 * Revision 1.29  2005/07/06 15:10:15  vsc
 * improvements to compiler: merged instructions and fixes for ->
 *
 * Revision 1.28  2005/05/30 06:07:35  vsc
 * changes to support more tagging schemes from tabulation.
 *
 * Revision 1.27  2005/04/10 04:01:13  vsc
 * bug fixes, I hope!
 *
 * Revision 1.26  2004/09/30 21:37:41  vsc
 * fixes for thread support
 *
 * Revision 1.25  2004/09/27 20:45:04  vsc
 * Mega clauses
 * Fixes to sizeof(expand_clauses) which was being overestimated
 * Fixes to profiling+indexing
 * Fixes to reallocation of memory after restoring
 * Make sure all clauses, even for C, end in _Ystop
 * Don't reuse space for Streams
 * Fix Stream_F on StreaNo+1
 *
 * Revision 1.24  2004/04/14 19:10:40  vsc
 * expand_clauses: keep a list of clauses to expand
 * fix new trail scheme for multi-assignment variables
 *
 * Revision 1.23  2004/03/31 01:03:10  vsc
 * support expand group of clauses
 *
 * Revision 1.22  2004/03/10 14:59:55  vsc
 * optimise -> for type tests
 *									 *
 *									 *
 *************************************************************************/

#if ALIGN_LONGS
/*   */ typedef Int DISPREG;
/*   */ typedef CELL SMALLUNSGN;
/*   */ typedef Int  OPREG;
/*   */ typedef CELL UOPREG;

#else
/*   */ typedef Short DISPREG;
/*   */ typedef BITS16 SMALLUNSGN;
/*   */ typedef SBITS16 OPREG;
/*   */ typedef SBITS16 UOPREG;

#endif


typedef Int (*CPredicate)(void);

typedef Int (*CmpPredicate)(Term, Term);


#define OpRegSize    sizeof(OPREG)

/*
  Possible arguments to YAP emulator:
  wamreg describes an A or X register;
  yslot describes an Y slot
  COUNT is a small number (eg, number of arguments to a choicepoint,
  number of permanent variables in a environment
*/

typedef OPREG  wamreg;
typedef OPREG  yslot;
typedef OPREG  COUNT;


/*
  This is a table with the codes for YAP instructions
*/
typedef enum {
#define OPCODE(OP,TYPE) _##OP
#include "YapOpcodes.h"
#undef  OPCODE
} op_numbers;

#define _std_top	_p_execute_tail

/* use similar trick for keeping instruction names */
#if defined(ANALYST) || defined(DEBUG)
extern char *Yap_op_names[_std_top + 1];
#endif

typedef enum {
  _atom,
  _atomic,
  _integer,
  _compound,
  _float,
  _nonvar,
  _number,
  _var,
  _cut_by,
  _db_ref,
  _primitive,
  _dif,
  _eq,
  _equal,
  _plus,
  _minus,
  _times,
  _div,
  _and,
  _or,
  _sll,
  _slr,
  _arg,
  _functor
} basic_preds;

#if USE_THREADED_CODE

#if ALIGN_LONGS
/*   */ typedef CELL OPCODE;
#else

#if LOW_ABSMI
/*   */ typedef BITS16 OPCODE;
#else
/*   */ typedef CELL OPCODE;
#endif
#endif /* ALIGN_LONGS */
#else /* if does not USE_THREADED_CODE */
/*   */ typedef op_numbers OPCODE;
#endif
#define OpCodeSize   sizeof(OPCODE)


/*

  Types of possible YAAM instructions.

  The meaning of the symbols in a abstract machine instruction is:

  c: constant, is a Term
  d: double (functor + unaligned)
  l: label, yamop *


  d: predicate definition
  f: functor
  n: small number
  x: argument or temporary register
  y: environment slot

*/
typedef struct yami {
  OPCODE opc;
  union {
    struct {
      CELL next;
    } e;
    struct {
      Term                c;
      CELL next;
    } c;
    struct {
      Term                c1;
      Term                c2;
      CELL next;
    } cc;
    struct {
      Term                c1;
      Term                c2;
      Term                c3;
      CELL next;
    } ccc;
    struct {
      Term                c1;
      Term                c2;
      Term                c3;
      Term                c4;
      CELL next;
    } cccc;
    struct {
      Term                c1;
      Term                c2;
      Term                c3;
      Term                c4;
      Term                c5;
      CELL next;
    } ccccc;
    struct {
      Term                c1;
      Term                c2;
      Term                c3;
      Term                c4;
      Term                c5;
      Term                c6;
      CELL next;
    } cccccc;
    struct {
      Term                c;
      struct yami        *l1;
      struct yami        *l2;
      struct yami        *l3;
      CELL next;
    } clll;
    struct {
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      CELL next;
    } d;
    struct {
      Int  ClTrail;
      Int  ClENV;
      Int  ClRefs;
      struct logic_upd_clause *ClBase;
#if defined(THREADS) || defined(YAPOR)
      struct pred_entry        *p;
#endif
      CELL  next;
    } EC;
    struct {
      Functor             f;
      Int                 a;
      CELL next;
    } f;
    struct {
      Functor             f;
      CODEADDR            l1;
      CODEADDR            l2;
      CELL next;
    } fll;
    struct {
      wamreg               x;
      struct yami         *f;
      CELL next;
    } fx;
    struct {
      yslot                y;
      struct yami         *f;
      CELL next;
    } fy;
    struct {
      OPCODE              opcw;
      CELL    i[2];
      CELL next;
    } i;
    struct {
      struct logic_upd_index  *I;
      struct yami             *l1;
      struct yami             *l2;
      COUNT                    s;
#if defined(YAPOR) || defined(THREADS)
      struct pred_entry        *p;
#endif
      CELL next;
    } Ill;
    struct {
      struct yami   *l;
      CELL next;
    } l;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif /* YAPOR */
#ifdef TABLING
      struct table_entry *te; /* pointer to table entry */
#endif /* TABLING */
      COUNT               s;
      struct pred_entry  *p;
      struct yami              *d;
      CELL next;
    } ld;
    struct {
#ifdef YAPOR
      unsigned int               or_arg;
#endif /* YAPOR */
#ifdef TABLING
      struct table_entry        *te; /* pointer to table entry */
#endif /* TABLING */
      union {
	/* either number of arguments or */
	COUNT                    s;
	/* timestamp it was entered */
	struct logic_upd_index  *block;
      } t;
      struct logic_upd_clause   *d;
      struct yami               *n;
      CELL                       next;
    } lld;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif /* YAPOR */
#ifdef TABLING
      struct table_entry *te; /* pointer to table entry */
#endif /* TABLING */
      COUNT               s;
      struct pred_entry  *p;
      struct yami        *d;
      struct yami        *bl;
      CELL next;
    } ldl;
    struct {
      CODEADDR            l;
      SMALLUNSGN          flags;
      CELL next;
    } lf;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif /* YAPOR */
#ifdef TABLING
      struct table_entry *te; /* pointer to table entry */
#endif /* TABLING */
      COUNT               s;
      struct pred_entry  *p;
      CPredicate          f;
      COUNT               extra;
      CELL next;
    } lds;
    struct {
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      CELL next;
    } lll;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif /* YAPOR */
#ifdef TABLING
      struct table_entry *te; /* pointer to table entry */
#endif /* TABLING */
      COUNT               s;
      struct pred_entry  *p;
      struct yami        *l1;
      struct yami        *l2;
      struct yami        *l3;
      CELL next;
    } slll;
    struct {
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      struct yami               *l4;
      CELL next;
    } llll;
    struct {
      wamreg                     x;
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      struct yami               *l4;
      CELL next;
    } xllll;
    struct {
      COUNT                      s;
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      struct yami               *l4;
      CELL next;
    } sllll;
    struct {
      struct pred_entry    *p;
      struct yami          *f;
      wamreg                x1;
      wamreg                x2;
      wamreg                flags;
      CELL next;
    } llxx;
    struct {
      struct pred_entry    *p;
      struct yami          *f;
      wamreg                x;
      yslot                 y;
      wamreg                flags;
      CELL next;
    } llxy;
    struct {
      struct pred_entry    *p;
      struct yami          *f;
      wamreg                y1;
      yslot                 y2;
      wamreg                flags;
      CELL next;
    } llyy;
    struct {
      OPCODE              pop;
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      struct yami               *l4;
      CELL next;
    } ollll;
    struct {
      OPCODE              opcw;
      CELL next;
    } o;
    struct {
      OPCODE              opcw;
      CELL                c;
      CELL next;
    } oc;
    struct {
      OPCODE              opcw;
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      CELL next;
    } od;
    struct {
      OPCODE              opcw;
      Functor             f;
      Int                 a;
      CELL next;
    } of;
    struct {
      OPCODE              opcw;
      CELL		     i[2];
      CELL next;
    } oi;
    struct {
      OPCODE              opcw;
      COUNT               s;
      CELL                c;
      CELL next;
    } osc;
    struct {
      OPCODE              opcw;
      COUNT               s;
      CELL next;
    } os;
    struct {
      OPCODE              opcw;
      wamreg                x;
      CELL next;
    } ox;
    struct {
      OPCODE              opcw;
      wamreg                xl;
      wamreg                xr;
      CELL next;
    } oxx;
    struct {
      OPCODE              opcw;
      yslot                y;
      CELL next;
    } oy;
    struct {
      struct pred_entry   *p;
      CELL next;
    } p;
    struct {
      struct pred_entry   *p;
      struct pred_entry   *p0;
      CELL next;
    } pp;
    struct {
      COUNT               s;
      CELL next;
    } s;
    struct {
      COUNT               s1;
      COUNT               s2;
      COUNT               s3;
      struct yami  *sprev, *snext;
      struct pred_entry  *p;
      CELL next;
    } sp;
    struct {
      COUNT               s;
      CELL                c;
      CELL next;
    } sc;
    struct {
      COUNT               s;
      CPredicate          d;
      struct yami        *l;
      struct pred_entry  *p;
      CELL next;
    } sdl;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif /* YAPOR */
      COUNT               s;
      CELL               *bmap;
      union {
	struct yami *l;
	struct pred_entry  *p;
	Term  mod;
      } sla_u;
      struct pred_entry  *p0;
      CELL next;
    } sla; /* also check env for yes and trustfail code before making any changes */
    struct {
      COUNT               s;  /* size of table */
      COUNT               e;  /* live entries */
      COUNT               w;  /* pending suspended blocks */
      struct yami        *l;
      CELL next;
    } sssl;
    struct {
      wamreg                x;
      CELL next;
    } x;
    struct {
      wamreg                x;
      CELL                c;
      CELL next;
    } xc;
    struct {
      wamreg                x;
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      CELL next;
    } xd;
    struct {
      wamreg                x;
      Functor             f;
      Int                 a;
      CELL next;
    } xf;
    struct {
      wamreg                x;
      struct yami          *F;
      CELL next;
    } xF;
    struct {
      wamreg                x;
      CELL    i[2];
      CELL next;
    } xi;
    struct {
      wamreg                x;
      struct yami	       *l1;
      struct yami	       *l2;
      CELL next;
    } xll; 
    struct {
      wamreg                xl;
      wamreg                xr;
      CELL next;
    } xx;
    struct {
      wamreg                x;
      wamreg                x1;
      wamreg                x2;
      CELL next;
    } xxx;
    struct {
      wamreg                xl1;
      wamreg                xl2;
      wamreg                xr1;
      wamreg                xr2;
      CELL next;
    } xxxx;
    struct {
      wamreg                x;
      Int                 c;
      wamreg                xi;
      CELL next;
    } xcx, xxc;
    struct {
      wamreg                x;
      yslot                y;
      CELL next;
    } xy;
    struct {
      wamreg                x;
      yslot                y2;
      wamreg                x1;
      CELL next;
    } xyx;
    struct {
      yslot                y;
      CELL next;
    } y;
    struct {
      yslot                y;
      struct yami         *F;
      CELL next;
    } yF;
    struct {
      yslot                y;
      wamreg                x;
      CELL next;
    } yx;
    struct {
      yslot                y;
      wamreg                x1;
      wamreg                x2;
      CELL next;
    } yxx;
    struct {
      yslot                y1;
      yslot                y2;
      wamreg                x;
      CELL next;
    } yyx;
    struct {
      yslot                y;
      yslot                y1;
      yslot                y2;
      CELL next;
    } yyy;
    struct {
      yslot                y;
      Int                 c;
      wamreg                xi;
      CELL next;
    } ycx, yxc;
  } u;
} yamop;

typedef yamop yamopp;

#define OPCR                opc
#define OPCW                u.ox.opcw


#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->u.TYPE.next)))

#define PREVOP(V,TYPE)    ((yamop *)((CODEADDR)(V)-(CELL)NEXTOP((yamop *)NULL,TYPE)))

#if defined(TABLING) || defined(SBA)
typedef struct trail_frame {
  Term term;
  CELL value;
} *tr_fr_ptr;

#define TrailTerm(X)   ((X)->term)
#define TrailVal(X)    ((X)->value)
#else
typedef Term *tr_fr_ptr;

#define TrailTerm(X)   (*(X))
#define TrailVal(X)    OOOOOOPS: this program should not compile
#endif /* TABLING || SBA  */


/*
  Choice Point Structure

  6 fixed fields (TR,AP,H,B,ENV,CP) plus arguments

*/
typedef struct choicept {
  tr_fr_ptr cp_tr;
  CELL *cp_h;
  struct choicept *cp_b;
#ifdef DEPTH_LIMIT
  CELL cp_depth;
#endif /* DEPTH_LIMIT */
  yamop *cp_cp;
#ifdef YAPOR
  int cp_lub;           /* local untried branches */
  struct or_frame *cp_or_fr;  /* or-frame pointer */
#endif /* YAPOR */
  yamop *cp_ap;
#if MIN_ARRAY == 0
  CELL *cp_env;
  /* GNUCC understands empty arrays */
  CELL cp_args[MIN_ARRAY];
#define cp_a1		cp_args[0]
#define cp_a2		cp_args[1]
#define cp_a3		cp_args[2]
#define cp_a4		cp_args[3]
#define cp_a5		cp_args[4]
#define cp_a6		cp_args[5]
#define cp_a7		cp_args[6]
#define cp_a8		cp_args[7]
#define EXTRA_CBACK_ARG(Arity,Offset)  B->cp_args[(Arity)+(Offset)-1]
#else
  /* Otherwise, we need a very dirty trick to access the arguments */
  union {
    CELL *cp_uenv;
    CELL  cp_args[1];
  } cp_last;
#define cp_env		cp_last.cp_uenv
#define cp_a1		cp_last.cp_args[1]
#define cp_a2		cp_last.cp_args[2]
#define cp_a3		cp_last.cp_args[3]
#define cp_a4		cp_last.cp_args[4]
#define cp_a5		cp_last.cp_args[5]
#define cp_a6		cp_last.cp_args[6]
#define cp_a7		cp_last.cp_args[7]
#define cp_a8		cp_last.cp_args[8]
#define EXTRA_CBACK_ARG(Arity,Offset)  B->cp_last.cp_args[(Arity)+(Offset)]
#endif
} *choiceptr;

/* This has problems with \+ \+ a, !, b. */
#define SHOULD_CUT_UP_TO(X,Y)  ((X) != (Y))
/* #define SHOULD_CUT_UP_TO(X,Y)  ((X)  (Y)) */

#ifdef SBA
#define SHARED_CP(CP)  ((CP) >= B_FZ || (CP) < (choiceptr)H_FZ)

#define YOUNGER_CP(CP1, CP2)						\
  (SHARED_CP(CP1) ?							\
   (SHARED_CP(CP2) ? OrFr_depth((CP1)->cp_or_fr) > OrFr_depth((CP2)->cp_or_fr) : FALSE)	\
   :									\
   (SHARED_CP(CP2) ? TRUE : CP1 < CP2)					\
   )

#define EQUAL_OR_YOUNGER_CP(CP1, CP2)					\
  (SHARED_CP(CP1) ?							\
   (SHARED_CP(CP2) ? OrFr_depth((CP1)->cp_or_fr) >= OrFr_depth((CP2)->cp_or_fr) : FALSE) \
   :									\
   (SHARED_CP(CP2) ? TRUE : CP1 <= CP2)					\
   )

#define YOUNGER_H(H1, H2) FIXMEE!!!!


#else /* ENV_COPY || ACOW */
#define YOUNGER_CP(CP1, CP2)           ((CP1) <  (CP2))
#define EQUAL_OR_YOUNGER_CP(CP1, CP2)  ((CP1) <= (CP2))

#define YOUNGER_H(H1, H2)           ((CELL *)(H1) > (CELL *)(H2))

#endif /* SBA */

#define YOUNGEST_CP(CP1, CP2)           (YOUNGER_CP(CP1,CP2) ? (CP1) : (CP2))

#define YOUNGEST_H(H1, H2)           (YOUNGER_H(H1,H2) ? (CELL *)(H1) : (CELL *)(H2))



/*
  Environment Structure (CP, E, and CUT_B). Yap always saves the B
  where to cut to, even if not needed.
*/
#define E_CP		-1
#define E_E		-2
#define E_CB		-3
#ifdef TABLING
#define E_B		-4
#ifdef  DEPTH_LIMIT
#define E_DEPTH         -5
#define EnvSizeInCells   5
#else
#define EnvSizeInCells   4
#endif  /* DEPTH_LIMIT */
#else   /* TABLING */
#ifdef  DEPTH_LIMIT
#define E_DEPTH         -4
#define EnvSizeInCells   4
#else
#define EnvSizeInCells   3
#endif  /* DEPTH_LIMIT */
#endif  /* TABLING */

#if MSHIFTOFFS
#define FixedEnvSize		EnvSizeInCells
#else
#define FixedEnvSize		(EnvSizeInCells*sizeof(CELL))
#endif
#define RealEnvSize	(EnvSizeInCells*sizeof(CELL))

#define ENV_Size(cp)       (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NULL,sla)))->u.sla.s)
#define ENV_ToP(cp)        (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NULL,sla)))->u.sla.sla_u.p)
#define ENV_ToOp(cp)       (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NULL,sla)))->opc)
#define EnvSize(cp)        ((-ENV_Size(cp))/(OPREG)sizeof(CELL))
#define EnvBMap(p)         (((yamop *)((CODEADDR)(p) - (CELL)NEXTOP((yamop *)NULL,sla)))->u.sla.bmap)
#define EnvPreg(p)         (((yamop *)((CODEADDR)(p) - (CELL)NEXTOP((yamop *)NULL,sla)))->u.sla.p0)

/* access to instructions */

#if USE_THREADED_CODE
extern void **Yap_ABSMI_OPCODES;

#define absmadr(i) ((OPCODE)(Yap_ABSMI_OPCODES[(i)]))
#else
#define absmadr(i) ((OPCODE)(i))
#endif

/* used to find out how many instructions of each kind are executed */
#ifdef ANALYST
extern YAP_ULONG_LONG Yap_opcount[_std_top + 1];

extern YAP_ULONG_LONG Yap_2opcount[_std_top + 1][_std_top + 1];
#endif /* ANALYST */

#if DEPTH_LIMIT
/*
  Make this into an even number so that the system will know
  it should ignore the depth limit
*/   
#define RESET_DEPTH() DEPTH = MkIntTerm(MAX_ABS_INT-1)
#else

#endif


