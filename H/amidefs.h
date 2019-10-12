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
 *						    			 *
 *************************************************************************/

#ifndef AMIDEFS_H
#define AMIDEFS_H 1

#ifndef NULL
#include <stdio.h>
#endif

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

#if THREADS

typedef struct regstore_t *regstruct_ptr;

#define CACHE_TYPE1 regstruct_ptr
#define CACHE_TYPE  , regstruct_ptr

#else

#define CACHE_TYPE
#define CACHE_TYPE1 void

#endif


typedef Int (*CPredicate)(CACHE_TYPE1);

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

#define _std_top	_or_last

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
  _save_by,
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
  _functor,
  _p_put_fi,
  _p_put_i,
  _p_put_f,
  _p_a_eq_float,
  _p_a_eq_int,
  _p_a_eq,
  _p_ltc_float,
  _p_ltc_int,
  _p_lt,
  _p_gtc_float,
  _p_gtc_int,
  _p_get_fi,
  _p_get_i,
  _p_get_f,
  _p_add_float_c,
  _p_add_int_c,
  _p_add,
  _p_sub_float_c,
  _p_sub_int_c,
  _p_sub,
  _p_mul_float_c,
  _p_mul_int_c,
  _p_mul,
  _p_fdiv_c1,
  _p_fdiv_c2,
  _p_fdiv,
  _p_idiv_c1,
  _p_idiv_c2,
  _p_idiv,
  _p_mod_c1,
  _p_mod_c2,
  _p_mod,
  _p_rem_c1,
  _p_rem_c2,
  _p_rem,
  _p_land_c,
  _p_land,
  _p_lor_c,
  _p_lor,
  _p_xor_c,
  _p_xor,
  _p_uminus,
  _p_sr_c1,
  _p_sr_c2,
  _p_sr,
  _p_sl_c1,
  _p_sl_c2,
  _p_sl,
  _p_label_ctl
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

/**

  Types of possible YAAM instructions.

  The meaning and type of the symbols in a abstract machine instruction is:

  A: Atom
  b: arity (Int)
  b: bitmap (CELL *)
  c: constant, is a Term
  d: double (functor + unaligned double)
  f: functor
  F: Function, CPredicate
  J: JIT interface
  i: large integer (functor + long)
  I: logic upd index (struct logic_upd_index *)
  l: label, yamop *
  L: logic upd clause, logic_upd_clause *
  m: module, Term
  n: number, Integer
  N: bigint, Blob (Term)
  o: opcode, OPCODE
  O: OR-parallel information, used by YAPOR, unsigned int
  p: predicate, struct pred_entry *
  s: small integer, COUNT
  t: pointer to table entry, used by yaptab, struct table_entry *
  u: utf-8 string
  x: wam register, wamreg
  y: environment slot

 This declaration is going to be parsed by a Prolog program, so:
   comments are welcome, but they should take a whole line,
   every field declaration should also take a single line,
   please check the Prolog program if you come up with a complicated C-type that does not start by unsigned or struct.
*/
typedef struct yami {
  OPCODE opc;
#if YAP_JIT
  CELL next_native_r;
  CELL next_native_w;
#endif
  union {
    struct {
      CELL next;
    } e;
    struct {
      Term                c;
      CELL next;
    } c;
    struct {
      Term                D;
      CELL next;
    } D;
    struct {
      Term                b;
      CELL next;
    } N;
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
      struct logic_upd_clause *ClBase;
      CELL  next;
    } L;
    struct {
      Functor             f;
      Int                 a;
      CELL next;
    } fa;
    struct {
      CELL    i[2];
      CELL next;
    } i;
    struct {
      struct logic_upd_index  *I;
      struct yami             *l1;
      struct yami             *l2;
      COUNT                    s;
      COUNT                    e;
      CELL next;
    } Illss;
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
      Int               s;
      struct pred_entry  *p;
      struct yami              *d;
      CELL next;
    } Otapl;
    struct {
    /* jit_handler */
#if YAP_JIT
      struct jit_handl_context *jh;
#endif
      CELL next;
    } J;
    /* The next two instructions are twin: they both correspond to the old ldd. */
    /* The first one, aLl, handles try_logical and retry_logical, */
    /* Ill handles trust_logical. */
    /* They must have the same fields. */

    struct {
#ifdef YAPOR
      unsigned int               or_arg;
#endif /* YAPOR */
#ifdef TABLING
      /* pointer to table entry */
      struct table_entry        *te;
#endif
      /* number of arguments */
      COUNT                    s;
      struct logic_upd_clause   *d;
      struct yami               *n;
      CELL                       next;
    } OtaLl;
    struct {
#ifdef YAPOR
      unsigned int               or_arg;
#endif
#ifdef TABLING
      /* pointer to table entry */
      struct table_entry        *te;
#endif /* TABLING */
      /* number of arguments */
      struct logic_upd_index  *block;
      struct logic_upd_clause   *d;
      struct yami               *n;
      CELL                       next;
    } OtILl;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif
#ifdef TABLING
       /* pointer to table entry */
      struct table_entry *te;
#endif
      Int               s;
      struct pred_entry  *p;
      CPredicate          f;
      COUNT               extra;
      CELL next;
    } OtapFs;
    struct {
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      CELL next;
    } lll;
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
      COUNT                flags;
      CELL next;
    } plxxs;
    struct {
      struct pred_entry    *p;
      struct yami          *f;
      wamreg                x;
      yslot                 y;
      COUNT                 flags;
      CELL next;
    } plxys;
    struct {
      struct pred_entry    *p;
      struct yami          *f;
      wamreg                y1;
      yslot                 y2;
      COUNT                 flags;
      CELL next;
    } plyys;
    struct {
      OPCODE              pop;
      struct yami               *l1;
      struct yami               *l2;
      struct yami               *l3;
      struct yami               *l4;
      CELL next;
    } ollll;
    struct {
      Int		  i;
      struct pred_entry  *p;
      CELL next;
    } ip;
    struct {
      struct yami        *l;
      struct pred_entry  *p;
      CELL next;
    } lp;
    struct {
      OPCODE              opcw;
      CELL next;
    } o;
    struct {
      OPCODE              opcw;
      Term                c;
      CELL next;
    } oc;
    struct {
      OPCODE              opcw;
      Term                b;
      CELL next;
    } oN;
    struct {
      OPCODE              opcw;
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      CELL next;
    } od;
    struct {
      OPCODE              opcw;
      Term                  D;
      CELL next;
    } oD;
    struct {
      OPCODE              opcw;
      Functor             f;
      Int                 a;
      CELL next;
    } ofa;
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
      Term    ut;
      CELL next;
    } ou;
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
      COUNT               s;
      CELL next;
    } s;
    /* format of expand_clauses */
    struct {
      COUNT               s1;
      COUNT               s2;
      COUNT               s3;
      struct yami  *sprev;
      struct yami  *snext;
      struct pred_entry  *p;
      CELL next;
    } sssllp;
    struct {
      COUNT               s;
      CELL                c;
      CELL next;
    } sc;
    struct {
      COUNT               s;
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      struct yami        *F;
      struct yami        *T;
      CELL next;
    } sdll;
    struct {
      COUNT               s;
      struct yami        *l;
      struct pred_entry  *p;
      struct pred_entry  *p0;
      CELL next;
    } slpp;
    struct {
      COUNT               s;
      Int                 I;
      struct yami        *F;
      struct yami        *T;
      CELL next;
    } snll;
    struct {
      COUNT               s0;
      COUNT               s1;
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      CELL next;
    } ssd;
    struct {
      COUNT               s0;
      COUNT               s1;
      Int		  n;
      CELL next;
    } ssn;
    struct {
      COUNT               s0;
      COUNT               s1;
      COUNT               s2;
      CELL next;
    } sss;
    struct {
      COUNT               s1;
      COUNT               s2;
      struct yami        *F;
      struct yami        *T;
      CELL next;
    } ssll;
    struct {
      COUNT               s;
      wamreg              x;
      struct yami        *l;
      CELL next;
    } sxl;
    struct {
      COUNT               s;
      wamreg              x;
      struct yami        *F;
      struct yami        *T;
      CELL next;
    } sxll;
    struct {
      COUNT               s;
      yslot               y;
      struct yami        *l;
      CELL next;
    } syl;
    struct {
      COUNT               s;
      yslot               y;
      struct yami        *F;
      struct yami        *T;
      CELL next;
    } syll;
    /* the next 3 instructions must have same size and have fields in same order! */
    /* also check env for yes and trustfail code before making any changes */
    /* last, Osblp is known to the buildops script */
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif
      COUNT               s;
      CELL               *bmap;
      struct yami *l;
      struct pred_entry  *p0;
      CELL next;
    } Osblp;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif
      COUNT               s;
      CELL               *bmap;
      struct pred_entry  *p;
      Int		  i;
      CELL next;
    } Osbpa;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif
      COUNT               s;
      CELL               *bmap;
      struct pred_entry  *p;
      struct pred_entry  *p0;
      CELL next;
    } Osbpp;
    struct {
#ifdef YAPOR
      unsigned int        or_arg;
#endif
      COUNT               s;
      CELL               *bmap;
      Term  mod;
      struct pred_entry  *p0;
      CELL next;
    } Osbmp;
    struct {
      /* size of table */
      COUNT               s;
      /* live entries */
      COUNT               e;
      /* pending suspended blocks */
      COUNT               w;
      struct yami        *l;
      CELL next;
    } sssl;
    struct {
      wamreg                x;
      CELL next;
    } x;
    struct {
      wamreg                x;
      struct pred_entry    *p0;
      COUNT               s;
      CELL next;
    } xps;
    struct {
      wamreg                x;
      CELL                  c;
      CELL next;
    } xc;
    struct {
      wamreg                x;
      Term                  b;
      CELL next;
    } xN;
    struct {
      wamreg                x;
      CELL    d[1+SIZEOF_DOUBLE/SIZEOF_INT_P];
      CELL next;
    } xd;
    struct {
      wamreg                x;
      Term                  D;
      CELL next;
    } xD;
    struct {
      wamreg                x;
      Functor             f;
      Int                 a;
      CELL next;
    } xfa;
    struct {
      wamreg                x;
      struct yami          *F;
      CELL next;
    } xl;
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
      Term                  ut;
      CELL next;
    } xu;
    struct {
      wamreg                x;
      wamreg                xi;
      Term                  c;
      CELL next;
    } xxc;
    struct {
      wamreg                x;
      wamreg                xi;
      Int                   c;
      CELL next;
    } xxn;
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
      wamreg                x1;
      yslot                y2;
      CELL next;
    } xxy;
    struct {
      yslot                y;
      CELL next;
    } y;
    struct {
      yslot                y;
      struct pred_entry   *p0;
      COUNT               s;
      CELL next;
    } yps;
    struct {
      yslot                y;
      struct yami         *F;
      CELL next;
    } yl;
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
      yslot                y1;
      yslot                y2;
      wamreg               x1;
      wamreg               x2;
      CELL next;
    } yyxx;
    struct {
      yslot                y;
      yslot                y1;
      yslot                y2;
      CELL next;
    } yyy;
    struct {
      yslot                y;
      wamreg               xi;
      Int                  c;
      CELL next;
    } yxn;
    struct {
      yslot                y;
      wamreg               xi;
      Term                 c;
      CELL next;
    } yxc;
  } y_u;
} yamop;

typedef yamop yamopp;

#define OPCR                opc
#define OPCW                u.ox.opcw


#define NEXTOP(V,TYPE)    ((yamop *)(&((V)->y_u.TYPE.next)))

#define PREVOP(V,TYPE)    ((yamop *)((CODEADDR)(V)-(CELL)NEXTOP((yamop *)NULL,TYPE)))

#if defined(TABLING) || defined(YAPOR_SBA)
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
#endif /* TABLING || YAPOR_SBA  */


/*
  Choice Point Structure

  6 fixed fields (TR,AP,H,B,ENV,CP) plus arguments
*/

#ifdef DETERMINISTIC_TABLING
struct deterministic_choicept {
  yamop *cp_ap;
  struct choicept *cp_b;
  tr_fr_ptr cp_tr;
#ifdef DEPTH_LIMIT
  CELL cp_depth;
#endif /* DEPTH_LIMIT */
#ifdef YAPOR
  int cp_lub;           /* local untried branches */
  struct or_frame *cp_or_fr;  /* or-frame pointer */
#endif /* YAPOR */
  CELL *cp_h;  /* necessary, otherwise we get in trouble */
};

typedef struct choicept {
  yamop *cp_ap;
  struct choicept *cp_b;
  tr_fr_ptr cp_tr;
#ifdef DEPTH_LIMIT
  CELL cp_depth;
#endif /* DEPTH_LIMIT */
#ifdef YAPOR
  int cp_lub;           /* local untried branches */
  struct or_frame *cp_or_fr;  /* or-frame pointer */
#endif /* YAPOR */
  CELL *cp_h;
  yamop *cp_cp;
#else
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
#endif /* DETERMINISTIC_TABLING */
#if MIN_ARRAY == 0
  CELL *cp_env;
  /* GNUCC understands empty arrays */
  CELL cp_args[MIN_ARRAY];
#else
  /* Otherwise, we need a very dirty trick to access the arguments */
  union {
    CELL *cp_uenv;
    CELL  cp_xargs[1];
  } cp_last;
#define cp_env		cp_last.cp_uenv
#define cp_args		cp_last.cp_xargs
#endif
#define cp_a1		cp_args[0]
#define cp_a2		cp_args[1]
#define cp_a3		cp_args[2]
#define cp_a4		cp_args[3]
#define cp_a5		cp_args[4]
#define cp_a6		cp_args[5]
#define cp_a7		cp_args[6]
#define cp_a8		cp_args[7]
#define cp_a9		cp_args[8]
#define cp_a10		cp_args[9]
#define EXTRA_CBACK_ARG(Arity,Offset)  B->cp_args[(Arity)+(Offset)-1]
} *choiceptr;

/* This has problems with \+ \+ a, !, b. */
#define SHOULD_CUT_UP_TO(X,Y)  ((X) != (Y))
/* #define SHOULD_CUT_UP_TO(X,Y)  ((X)  (Y)) */

#ifdef YAPOR_SBA
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


#else /* YAPOR_COPY || YAPOR_COW */
#define YOUNGER_CP(CP1, CP2)           ((CP1) <  (CP2))
#define EQUAL_OR_YOUNGER_CP(CP1, CP2)  ((CP1) <= (CP2))

#define YOUNGER_H(H1, H2)           ((CELL *)(H1) > (CELL *)(H2))

#endif /* YAPOR_SBA */

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

static inline
CELL *ENV_Parent(CELL *env)
{
  return (CELL *)env[E_E];
}

static inline
Int ENV_Size(yamop *cp)
{
  return (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NULL,Osbpp)))->y_u.Osbpp.s);
}

static inline
struct pred_entry *ENV_ToP(yamop *cp)
{
  return (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NULL,Osbpp)))->y_u.Osbpp.p);
}

static inline
OPCODE ENV_ToOp(yamop*cp)
{
  return (((yamop *)((CODEADDR)(cp) - (CELL)NEXTOP((yamop *)NULL,Osbpp)))->opc);
}

static inline
int64_t EnvSize(yamop *cp)
{
  return (-ENV_Size(cp)/sizeof(CELL));
}

static inline
CELL *EnvBMap(yamop *p)
{
  return (((yamop *)((CODEADDR)(p) - (CELL)NEXTOP((yamop *)NULL,Osbpp)))->y_u.Osbpp.bmap);
}

static inline
struct pred_entry *EnvPreg(yamop *p)
{
  return (((yamop *)((CODEADDR)(p) - (CELL)NEXTOP((yamop *)NULL,Osbpp)))->y_u.Osbpp.p0);
}

/* access to instructions */

#if USE_THREADED_CODE
extern void **Yap_ABSMI_OPCODES;

#define absmadr(i) ((OPCODE)(Yap_ABSMI_OPCODES[(i)]))
#else
#define absmadr(i) ((OPCODE)(i))
#endif

  bool  is_cleanup_cp(choiceptr cp_b);

#if DEPTH_LIMIT
/*
  Make this into an even number so that the system will know
  it should ignore the depth limit
*/
#define RESET_DEPTH() MkIntTerm(MAX_ABS_INT-1)
#else

#endif

/// Debugging Support


extern void Yap_track_cpred( void *i );

typedef enum {
DEBUG_CREEP_LEAP_OR_ZIP = 0,
DEBUG_GOAL_NUMBER = 1,
DEBUG_SPY = 2,
DEBUG_TRACE =  3,
DEBUG_DEBUG = 4,
DEBUG_NUMBER_OF_OPTS = 5
} debug_key_t ;

#endif
