/*************************************************************************
*									 *
*	 YAP Prolog    @(#)amidefs.h	1.3 3/15/90
*									 *
*	Yiap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		amidefs.h						 *
* Last rev:								 *
* mods:									 *
* comments:	Abstract machine peculiarities				 *
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

#define OpRegSize    sizeof(OPREG)

/*
   Possible arguments to YAP emulator:
   AREG describes an A or X register;
   YREG describes an Y register
   COUNT is a small number (eg, number of arguments to a choicepoint,
   number of permanent variables in a environment
*/

typedef OPREG  AREG;
typedef OPREG  YREG;
typedef OPREG  COUNT;


/*
   This is a table with the codes for YAP instructions
*/
typedef enum {
#define OPCODE(OP,TYPE) _##OP
#include "YapOpcodes.h"
#undef  OPCODE
} op_numbers;


#define _std_top	_p_last_execute_within

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
	_equal,
	_dif,
	_eq,
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

   c: constant
   d: predicate definition
   f: functor
   n: small number
   l: label
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
	 CELL                c;
	 CELL next;
       } c;
       struct {
	 CELL                c;
	 CODEADDR            l1;
	 CODEADDR            l2;
	 CELL next;
       } cll;
       struct {
	 CODEADDR            d;
	 CELL next;
       } d;
       struct {
	 Int  ClTrail;
	 Int  ClENV;
	 Int  ClRefs;
	 CODEADDR  ClBase;
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
	 CODEADDR            l;
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
	 CODEADDR	     p;
	 CODEADDR            d;
	 CELL next;
       } ld;
       struct {
#ifdef YAPOR
         unsigned int        or_arg;
#endif /* YAPOR */
#ifdef TABLING
         struct table_entry *te; /* pointer to table entry */
#endif /* TABLING */
	 COUNT               s;
	 CODEADDR	     p;
	 CODEADDR            d;
	 CODEADDR            bl;
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
	 CODEADDR	     p;
	 CODEADDR            d;
	 COUNT               extra;
	 CELL next;
       } lds;
       struct {
	 CODEADDR            l1;
	 CODEADDR            l2;
	 CODEADDR            l3;
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
	 CODEADDR	     p;
	 CODEADDR            l1;
	 CODEADDR            l2;
	 CODEADDR            l3;
	 CELL next;
       } slll;
       struct {
	 CODEADDR            l1;
	 CODEADDR            l2;
	 CODEADDR            l3;
	 CODEADDR            l4;
	 CELL next;
       } llll;
       struct {
	 CODEADDR            p;
	 CODEADDR            l;
	 AREG                x1;
	 AREG                x2;
	 AREG                flags;
	 CELL next;
       } lxx;
       struct {
	 CODEADDR            p;
	 CODEADDR            l;
	 AREG                x;
	 YREG                y;
	 AREG                flags;
	 CELL next;
       } lxy;
       struct {
	 CODEADDR            p;
	 CODEADDR            l;
	 AREG                y1;
	 YREG                y2;
	 AREG                flags;
	 CELL next;
       } lyy;
       struct {
	 OPCODE              pop;
	 CODEADDR            l1;
	 CODEADDR            l2;
	 CODEADDR            l3;
	 CODEADDR            l4;
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
	 Functor             f;
	 Int                 a;
	 CELL next;
       } of;
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
	 AREG                x;
	 CELL next;
       } ox;
       struct {
	 OPCODE              opcw;
	 AREG                xl;
	 AREG                xr;
	 CELL next;
       } oxx;
       struct {
	 OPCODE              opcw;
	 YREG                y;
	 CELL next;
       } oy;
       struct {
	 COUNT               s;
	 CELL next;
       } s;
       struct {
	 COUNT               s;
	 CELL                c;
	 CELL next;
       } sc;
       struct {
	 COUNT               s;
	 CODEADDR            d;
	 CODEADDR            l;
	 CODEADDR            p;
	 CELL next;
       } sdl;
       struct {
	 COUNT               s;
	 CODEADDR            l;
	 CELL next;
       } sl;
       struct {
#ifdef YAPOR
         unsigned int        or_arg;
#endif /* YAPOR */
	 COUNT               s;
	 CODEADDR            l;
	 CELL               *l2;
	 struct pred_entry  *p;
         struct pred_entry  *p0;
	 CELL next;
       } sla; /* also check env for yes and trustfail code before making any changes */
       struct {
	 AREG                x;
	 CELL next;
       } x;
       struct {
	 AREG                x;
	 CELL                c;
	 CELL next;
       } xc;
       struct {
	 AREG                x;
	 Functor             f;
	 Int                 a;
	 CELL next;
       } xf;
       struct {
	 AREG                xl;
	 AREG                xr;
	 CELL next;
       } xx;
       struct {
	 AREG                x;
	 AREG                x1;
	 AREG                x2;
	 CELL next;
       } xxx;
       struct {
	 AREG                x;
	 Int                 c;
	 AREG                xi;
	 CELL next;
       } xcx, xxc;
       struct {
	 AREG                x;
	 YREG                y;
	 CELL next;
       } xy;
       struct {
	 AREG                x;
	 YREG                y2;
	 AREG                x1;
	 CELL next;
       } xyx;
       struct {
	 YREG                y;
	 CELL next;
       } y;
       struct {
	 YREG                y;
	 AREG                x;
	 CELL next;
       } yx;
       struct {
	 YREG                y;
	 AREG                x1;
	 AREG                x2;
	 CELL next;
       } yxx;
       struct {
	 YREG                y1;
	 YREG                y2;
	 AREG                x;
	 CELL next;
       } yyx;
       struct {
	 YREG                y;
	 YREG                y1;
	 YREG                y2;
	 CELL next;
       } yyy;
       struct {
	 YREG                y;
	 Int                 c;
	 AREG                xi;
	 CELL next;
       } ycx, yxc;
     } u;
} yamop;

typedef yamop yamopp;

#define OPCR                opc
#define OPCW                u.ox.opcw


#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->u.TYPE.next)))

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

#define YOUNGER_CP(CP1, CP2)                                                                    \
        (SHARED_CP(CP1) ?                                                                       \
          (SHARED_CP(CP2) ? OrFr_depth((CP1)->cp_or_fr) > OrFr_depth((CP2)->cp_or_fr) : FALSE)  \
        :                                                                                    \
          (SHARED_CP(CP2) ? TRUE : CP1 < CP2)                                                   \
        )

#define EQUAL_OR_YOUNGER_CP(CP1, CP2)                                                            \
        (SHARED_CP(CP1) ?                                                                        \
          (SHARED_CP(CP2) ? OrFr_depth((CP1)->cp_or_fr) >= OrFr_depth((CP2)->cp_or_fr) : FALSE)  \
        :                                                                                     \
          (SHARED_CP(CP2) ? TRUE : CP1 <= CP2)                                                   \
        )
#else /* ENV_COPY || ACOW */
#define YOUNGER_CP(CP1, CP2)           ((CP1) <  (CP2))
#define EQUAL_OR_YOUNGER_CP(CP1, CP2)  ((CP1) <= (CP2))
#endif /* SBA */

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

#define ENV_Size(cp)       (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NIL,sla)))->u.sla.s)
#define ENV_ToP(cp)        ((PredEntry *)(((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NIL,sla)))->u.sla.p))
#define ENV_ToOp(cp)       (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NIL,sla)))->opc)
#define EnvSize(cp)        ((-ENV_Size(cp))/(OPREG)sizeof(CELL))
#define EnvBMap(p)         (((yamop *)((CODEADDR)(p) - (CELL)NEXTOP((yamop *)NIL,sla)))->u.sla.l2)
#define EnvPreg(p)         (((yamop *)((CODEADDR)(p) - (CELL)NEXTOP((yamop *)NIL,sla)))->u.sla.p0)

/* access to instructions */

#if USE_THREADED_CODE
extern void **ABSMI_OPCODES;

#define absmadr(i) ((OPCODE)(ABSMI_OPCODES[(i)]))
#else
#define absmadr(i) ((OPCODE)(i))
#endif

/* used to find out how many instructions of each kind are executed */
#ifdef ANALYST
extern int opcount[_std_top+1];
#endif /* ANALYST */

#if DEPTH_LIMIT
/*
  Make this into an even number so that the system will know
   it should ignore the depth limit
*/   
#define RESET_DEPTH() DEPTH = MkIntTerm(MAX_ABS_INT-1)
#else

#endif



