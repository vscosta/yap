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
* File:		amasm.c							 *
* comments:	abstract machine assembler				 *
*									 *
* Last rev:     $Date: 2008-08-12 01:27:22 $
**
* $Log: not supported by cvs2svn $
* Revision 1.103  2008/08/07 20:51:16  vsc
* more threadin  fixes
*
* Revision 1.102  2008/07/11 17:02:07  vsc
* fixes by Bart and Tom: mostly libraries but nasty one in indexing
* compilation.
*
* Revision 1.101  2008/04/01 22:28:41  vsc
* put YAPOR back to life.
*
* Revision 1.100  2008/03/25 16:45:52  vsc
* make or-parallelism compile again
*
* Revision 1.99  2008/01/23 17:57:44  vsc
* valgrind it!
* enable atom garbage collection.
*
* Revision 1.98  2007/11/26 23:43:07  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.97  2007/11/07 09:25:27  vsc
* speedup meta-calls
*
* Revision 1.96  2007/11/06 17:02:09  vsc
* compile ground terms away.
*
* Revision 1.95  2007/06/23 17:31:50  vsc
* pin cluses with floats.
*
* Revision 1.94  2006/12/27 01:32:37  vsc
* diverse fixes
*
* Revision 1.93  2006/12/13 16:10:14  vsc
* several debugger and CLP(BN) improvements.
*
* Revision 1.92  2006/11/15 00:13:36  vsc
* fixes for indexing code.
*
* Revision 1.91  2006/11/06 18:35:03  vsc
* 1estranha
*
* Revision 1.90  2006/10/11 14:53:57  vsc
* fix memory leak
* fix overflow handling
* VS: ----------------------------------------------------------------------
*
* Revision 1.89  2006/10/10 14:08:16  vsc
* small fixes on threaded implementation.
*
* Revision 1.88  2006/09/20 20:03:51  vsc
* improve indexing on floats
* fix sending large lists to DB
*
* Revision 1.87  2006/03/24 17:13:41  rslopes
* New update to BEAM engine.
* BEAM now uses YAP Indexing (JITI)
*
* Revision 1.86  2006/01/02 02:16:17  vsc
* support new interface between YAP and GMP, so that we don't rely on our own
* allocation routines.
* Several big fixes.
*
* Revision 1.85  2005/12/17 03:25:39  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.84  2005/09/08 22:06:44  rslopes
* BEAM for YAP update...
*
* Revision 1.83  2005/08/02 03:09:49  vsc
* fix debugger to do well nonsource predicates.
*
* Revision 1.82  2005/07/06 15:10:02  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.81  2005/06/01 21:23:44  vsc
* inline compare
*
* Revision 1.80  2005/06/01 20:25:23  vsc
* == and \= should not need a choice-point in ->
*
* Revision 1.79  2005/06/01 16:42:30  vsc
* put switch_list_nl back
*
* Revision 1.78  2005/06/01 14:02:47  vsc
* get_rid of try_me?, retry_me? and trust_me? instructions: they are not
* significantly used nowadays.
*
* Revision 1.77  2005/05/31 19:42:27  vsc
* insert some more slack for indices in LU
* Use doubly linked list for LU indices so that updating is less cumbersome.
*
* Revision 1.76  2005/05/30 05:33:43  vsc
* get rid of annoying debugging message.
*
* Revision 1.75  2005/05/30 05:26:49  vsc
* fix tabling
* allow atom gc again for now.
*
* Revision 1.74  2005/05/25 21:43:32  vsc
* fix compiler bug in 1 << X, found by Nuno Fonseca.
* compiler internal errors get their own message.
*
* Revision 1.73  2005/04/10 04:01:09  vsc
* bug fixes, I hope!
*
* Revision 1.72  2005/03/04 20:30:10  ricroc
* bug fixes for YapTab support
*
* Revision 1.71  2005/01/28 23:14:34  vsc
* move to Yap-4.5.7
* Fix clause size
*
* Revision 1.70  2004/12/28 22:20:35  vsc
* some extra bug fixes for trail overflows: some cannot be recovered that
*easily,
* some can.
*
* Revision 1.69  2004/12/20 21:44:56  vsc
* more fixes to CLPBN
* fix some Yap overflows.
*
* Revision 1.68  2004/12/07 16:54:57  vsc
* fix memory overflow
*
* Revision 1.67  2004/12/05 05:01:23  vsc
* try to reduce overheads when running with goal expansion enabled.
* CLPBN fixes
* Handle overflows when allocating big clauses properly.
*
* Revision 1.66  2004/11/19 22:08:41  vsc
* replace SYSTEM_ERROR_INTERNAL by out OUT_OF_WHATEVER_ERROR whenever
*appropriate.
*
* Revision 1.65  2004/10/26 20:15:48  vsc
* More bug fixes for overflow handling
*
* Revision 1.64  2004/09/30 21:37:40  vsc
* fixes for thread support
*
* Revision 1.63  2004/09/27 20:45:02  vsc
* Mega clauses
* Fixes to sizeof(expand_clauses) which was being overestimated
* Fixes to profiling+indexing
* Fixes to reallocation of memory after restoring
* Make sure all clauses, even for C, end in _Ystop
* Don't reuse space for Streams
* Fix Stream_F on StreaNo+1
*
* Revision 1.62  2004/08/20 16:16:23  vsc
* growheap was not checking some compiler instructions
* source was getting confused in reconsult
*
* Revision 1.61  2004/04/29 03:45:50  vsc
* fix garbage collection in execute_tail
*
* Revision 1.60  2004/04/22 20:07:04  vsc
* more fixes for USE_SYSTEM_MEMORY
*
* Revision 1.59  2004/03/31 01:03:09  vsc
* support expand group of clauses
*
* Revision 1.58  2004/03/10 14:59:55  vsc
* optimise -> for type tests
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "@(#)amasm.c	1.3 3/15/90";

#endif

#include "Yap.h"
#include "clause.h"
#include "YapCompile.h"
#include "yapio.h"

#ifdef BEAM
#include "eam.h"
#endif
#ifdef YAPOR
#include "or.macros.h"
#endif /* YAPOR */
#if HAVE_STRING_H
#include <string.h>
#endif

/* info on compare built-ins */
#define TYPE_XX 0
#define TYPE_CX 1
#define TYPE_XC 2

typedef struct cmp_op_info_struct {
  wamreg x1_arg, x2_arg;
  Int c_arg;
  int c_type;
  struct clause_info_struct *cl_info;
} cmp_op_info;

typedef struct clause_info_struct {
  int alloc_found, dealloc_found;
  struct pred_entry *CurrentPred;
} clause_info;

static OPREG Var_Ref(Ventry *, int);
static wamreg emit_xreg(CELL);
static yslot emit_yreg(CELL);
static wamreg emit_x(CELL);
static yslot emit_y(Ventry *);
static yamop *emit_a(CELL);
static CELL *emit_bmlabel(CELL, struct intermediates *);
static yamop *emit_ilabel(CELL, struct intermediates *);
static Functor emit_f(CELL);
static CELL emit_c(CELL);
static COUNT emit_count(CELL);
static OPCODE emit_op(op_numbers);
static yamop *a_cle(op_numbers, yamop *, int, struct intermediates *);
static yamop *a_e(op_numbers, yamop *, int);
static yamop *a_ue(op_numbers, op_numbers, yamop *, int);
static yamop *a_v(op_numbers, op_numbers, yamop *, int, struct PSEUDO *);
static yamop *a_uv(Ventry *, op_numbers, op_numbers, op_numbers, op_numbers,
                   yamop *, int);
static yamop *a_vr(op_numbers, op_numbers, yamop *, int,
                   struct intermediates *);
static yamop *a_rv(op_numbers, op_numbers, OPREG, yamop *, int,
                   struct PSEUDO *);
static yamop *a_vv(op_numbers, op_numbers, yamop *, int,
                   struct intermediates *);
static yamop *a_glist(int *, yamop *, int, struct intermediates *);
static void a_pair(CELL *, int, struct intermediates *);
static yamop *a_f(CELL, op_numbers, yamop *, int);
static yamop *a_c(CELL, op_numbers, yamop *, int);
static yamop *a_uc(CELL, op_numbers, op_numbers, yamop *, int);
static yamop *a_n(op_numbers, int, yamop *, int);
static yamop *a_un(op_numbers, op_numbers, int, yamop *, int);
static yamop *a_nc(CELL, op_numbers, int, yamop *, int);
static yamop *a_unc(CELL, op_numbers, op_numbers, int, yamop *, int);
static yamop *a_r(CELL, op_numbers, yamop *, int);
static yamop *a_p(op_numbers, clause_info *, yamop *, int,
                  struct intermediates *);
static yamop *a_pl(op_numbers, PredEntry *, yamop *, int);
static yamop *a_l(CELL, op_numbers, yamop *, int, struct intermediates *);
static yamop *a_hx(op_numbers, union clause_obj *, int, yamop *, int,
                   struct intermediates *);
static yamop *a_if(op_numbers, union clause_obj *, int, yamop *, int,
                   struct intermediates *cip);
static yamop *a_cut(clause_info *, yamop *, int, struct intermediates *);
#ifdef YAPOR
static yamop *a_try(op_numbers, CELL, CELL, int, int, yamop *, int,
                    struct intermediates *);
static yamop *a_either(op_numbers, COUNT, CELL, int, yamop *, int,
                       struct intermediates *);
#else
static yamop *a_try(op_numbers, CELL, CELL, yamop *, int,
                    struct intermediates *);
static yamop *a_either(op_numbers, COUNT, CELL, yamop *, int,
                       struct intermediates *);
#endif /* YAPOR */
static yamop *a_gl(op_numbers, yamop *, int, struct PSEUDO *,
                   struct intermediates *CACHE_TYPE);
static COUNT compile_cmp_flags(unsigned char *);
static yamop *a_igl(CELL, op_numbers, yamop *, int, struct intermediates *);
static yamop *a_xigl(op_numbers, yamop *, int, struct PSEUDO *);
static yamop *a_ucons(int *, compiler_vm_op, yamop *, int,
                      struct intermediates *);
static yamop *a_uvar(yamop *, int, struct intermediates *);
static yamop *a_wvar(yamop *, int, struct intermediates *);
static yamop *do_pass(int, yamop **, int, int *, int *, struct intermediates *,
                      UInt CACHE_TYPE);
#ifdef DEBUG_OPCODES
static void DumpOpCodes(void);
#endif
#ifdef SFUNC
static void a_vsf(int, yamop *, int, struct PSEUDO *);
static void a_asf(int, yamop *, int, struct PSEUDO *);
#endif
static yamop *check_alloc(clause_info *, yamop *, int, struct intermediates *);
static yamop *a_deallocate(clause_info *, yamop *, int, struct intermediates *);
static yamop *a_bmap(yamop *, int, struct PSEUDO *);
static void a_fetch_vv(cmp_op_info *, int, struct intermediates *);
static void a_fetch_cv(cmp_op_info *, int, struct intermediates *);
static void a_fetch_vc(cmp_op_info *, int, struct intermediates *);
static yamop *a_f2(cmp_op_info *, yamop *, int, struct intermediates *);

profile_data *Yap_initProfiler(PredEntry *p) {
  profile_data *ptr;
  if (p->StatisticsForPred)
    return p->StatisticsForPred;
  if ((ptr = (profile_data *)Yap_AllocCodeSpace(sizeof(profile_data))) ==
      NULL) {
    return NULL;
  }
  INIT_LOCK(ptr->lock);
  ptr->NOfEntries = 0;
  ptr->NOfHeadSuccesses = 0;
  ptr->NOfRetries = 0;
  p->StatisticsForPred = ptr;
  return ptr;
}

#define GONEXT(TYPE) code_p = ((yamop *)(&(code_p->y_u.TYPE.next)))

inline static yslot emit_y(Ventry *ve) {
#if MSHIFTOFFS
  return (-FixedEnvSize - ((ve->NoOfVE) & MaskVarAdrs) - 1);
#else
  return (-FixedEnvSize - (((ve->NoOfVE) & MaskVarAdrs) * CELLSIZE) - CELLSIZE);
#endif
}

inline static OPREG Var_Ref(Ventry *ve, int is_y_var) {
  if (is_y_var) {
#if MSHIFTOFFS
    return -FixedEnvSize - ((ve->NoOfVE) & MaskVarAdrs) - 1;
#else
    return -FixedEnvSize - (((ve->NoOfVE) & MaskVarAdrs) * CELLSIZE) - CELLSIZE;
#endif
  } else {
#if PRECOMPUTE_REGADDRESS
    return (CELL)(XREGS + ((ve->NoOfVE) & MaskVarAdrs));
#else
#if MSHIFTOFFS
    return ((ve->NoOfVE) & MaskVarAdrs);
#else
    return CELLSIZE * ((ve->NoOfVE) & MaskVarAdrs);
#endif
#endif /* PRECOMPUTE_REGADDRESS */
  }
}

#define is_void_var() (((Ventry *)(cip->cpc->rnd1))->KindOfVE == VoidVar)
#define is_a_void(X) (((Ventry *)(X))->KindOfVE == VoidVar)

#define is_temp_var() (((Ventry *)(cip->cpc->rnd1))->KindOfVE == TempVar)
#define is_atemp_var(p) (((Ventry *)(p->rnd1))->KindOfVE == TempVar)

#define no_ref_var() (((Ventry *)(cip->cpc->rnd1))->NoOfVE == 1)
#define no_ref(X) (((Ventry *)(X))->NoOfVE == 1)

inline static yamop *fill_a(CELL a, yamop *code_p, int pass_no) {
  CELL *ptr = ((CELL *)(code_p));

  if (pass_no)
    *ptr = a;
  return (yamop *)(++ptr);
}

inline static wamreg emit_xreg(CELL w) { return (wamreg)w; }

inline static yslot emit_yreg(CELL w) { return (yslot)w; }

inline static wamreg emit_x(CELL xarg) {
#if PRECOMPUTE_REGADDRESS
  return (emit_xreg((CELL)(XREGS + xarg)));
#else
#if MSHIFTOFFS
  return (emit_xreg(xarg));
#else
  return (emit_xreg(CELLSIZE * (xarg)));
#endif
#endif /* PRECOMPUTE_REGADDRESS */
}

wamreg Yap_emit_x(CELL xarg) { return emit_x(xarg); }

inline static yamop *emit_a(CELL a) { return ((yamop *)(a)); }

inline static struct pred_entry *emit_pe(struct pred_entry *a) { return a; }

inline static yamop *emit_ilabel(register CELL addr,
                                 struct intermediates *cip) {
  if (addr & 1)
    return (emit_a(Unsigned(cip->code_addr) + cip->label_offset[addr]));
  else {
    return (emit_a(addr));
  }
}

inline static CELL *emit_bmlabel(register CELL addr,
                                 struct intermediates *cip) {
  return (CELL *)(emit_a(Unsigned(cip->code_addr) + cip->label_offset[addr]));
}

inline static Functor emit_f(CELL a) { return (Functor)(a); }

inline static CELL emit_c(CELL a) { return a; }

static inline COUNT emit_count(CELL count) { return count; }

#ifdef DEBUG_OPCODES
inline static void DumpOpCodes(void) {
  int i = 0, j;

  while (i < 30) {
    for (j = i; j <= _std_top; j += 25)
      fprintf(GLOBAL_stderr, "%5d %6lx", j, absmadr(j));
    fputc('\n', GLOBAL_stderr);
    ++i;
  }
}
#endif

static inline OPCODE emit_op(op_numbers op) { return absmadr((Int)op); }

static OPCODE opcode(op_numbers op) { return (emit_op(op)); }

OPCODE
Yap_opcode(op_numbers op) { return opcode(op); }

static void add_clref(CELL clause_code, int pass_no) {
  if (pass_no) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(clause_code);
    cl->ClRefCount++;
  }
}

static void add_to_dbtermsl(struct intermediates *cip, Term t) {
  DBTerm *dbt = TermToDBTerm(t);
  dbt->ag.NextDBT = cip->dbterml->dbterms;
  cip->dbterml->dbterms = dbt;
}

static yamop *a_lucl(op_numbers opcode, yamop *code_p, int pass_no,
                     struct intermediates *cip, clause_info *cla) {
  if (pass_no) {
    LogUpdIndex *lcl = (LogUpdIndex *)cip->code_addr;
    code_p->opc = emit_op(opcode);
    code_p->y_u.Illss.I = lcl;
    cip->cpc->rnd4 = (CELL)code_p;
    cip->current_try_lab = &code_p->y_u.Illss.l1;
    cip->current_trust_lab = &code_p->y_u.Illss.l2;
    code_p->y_u.Illss.l1 = NULL;
    code_p->y_u.Illss.l2 = NULL;
    code_p->y_u.Illss.s = cip->cpc->rnd3;
    code_p->y_u.Illss.e = 0;
  }
  GONEXT(Illss);
  return code_p;
}

static yamop *a_cle(op_numbers opcode, yamop *code_p, int pass_no,
                    struct intermediates *cip) {
  if (pass_no) {
    LogUpdClause *cl = (LogUpdClause *)cip->code_addr;

    code_p->opc = emit_op(opcode);
    code_p->y_u.L.ClBase = cl;
    cl->ClExt = code_p;
    cl->ClFlags |= LogUpdRuleMask;
  }
  GONEXT(L);
  return code_p;
}

inline static yamop *a_e(op_numbers opcode, yamop *code_p, int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
  }
  GONEXT(e);
  return code_p;
}

inline static yamop *a_p0(op_numbers opcode, yamop *code_p, int pass_no,
                          PredEntry *p0) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.p.p = p0;
  }
  GONEXT(p);
  return code_p;
}

inline static yamop *a_lp(op_numbers opcode, yamop *code_p, int pass_no,
                          struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.lp.p = (PredEntry *)cip->cpc->rnd1;
    code_p->y_u.lp.l = (yamop *)cip->cpc->rnd2;
  }
  GONEXT(lp);
  return code_p;
}

inline static yamop *a_ue(op_numbers opcode, op_numbers opcodew, yamop *code_p,
                          int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.o.opcw = emit_op(opcodew);
  }
  GONEXT(o);
  return code_p;
}

inline static yamop *emit_fail(struct intermediates *cip) {
  if (cip->failure_handler) {
    return emit_a(Unsigned(cip->code_addr) +
                  cip->label_offset[cip->failure_handler]);
  } else {
    return FAILCODE;
  }
}

inline static yamop *a_v(op_numbers opcodex, op_numbers opcodey, yamop *code_p,
                         int pass_no, struct PSEUDO *cpc) {
  Ventry *ve = (Ventry *)cpc->rnd1;
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op(opcodey);
      code_p->y_u.y.y = emit_yreg(var_offset);
    }
    GONEXT(y);
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcodex);
      code_p->y_u.x.x = emit_xreg(var_offset);
    }
    GONEXT(x);
  }
  return code_p;
}

inline static yamop *a_vp(op_numbers opcodex, op_numbers opcodey, yamop *code_p,
                          int pass_no, struct PSEUDO *cpc,
                          clause_info *clinfo) {
  Ventry *ve = (Ventry *)cpc->rnd1;
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op(opcodey);
      code_p->y_u.yps.y = emit_yreg(var_offset);
      code_p->y_u.yps.p0 = clinfo->CurrentPred;
      code_p->y_u.yps.s = -Signed(RealEnvSize) - CELLSIZE * cpc->rnd2;
    }
    GONEXT(yps);
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcodex);
      code_p->y_u.xps.x = emit_xreg(var_offset);
      code_p->y_u.xps.p0 = clinfo->CurrentPred;
      code_p->y_u.xps.s = -Signed(RealEnvSize) - CELLSIZE * cpc->rnd2;
    }
    GONEXT(xps);
  }
  return code_p;
}

inline static yamop *a_uv(Ventry *ve, op_numbers opcodex, op_numbers opcodexw,
                          op_numbers opcodey, op_numbers opcodeyw,
                          yamop *code_p, int pass_no) {
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op(opcodey);
      code_p->y_u.oy.opcw = emit_op(opcodeyw);
      code_p->y_u.oy.y = emit_yreg(var_offset);
    }
    GONEXT(oy);
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcodex);
      code_p->y_u.ox.opcw = emit_op(opcodexw);
      code_p->y_u.ox.x = emit_xreg(var_offset);
    }
    GONEXT(ox);
  }
  return code_p;
}

inline static yamop *a_vv(op_numbers opcode, op_numbers opcodew, yamop *code_p,
                          int pass_no, struct intermediates *cip) {
  Ventry *ve = (Ventry *)cip->cpc->rnd1;
  int is_y_var = (ve->KindOfVE == PermVar);

  if (pass_no) {
    OPREG var_offset = Var_Ref(ve, is_y_var);
    code_p->opc = emit_op(opcode);
    code_p->y_u.oxx.opcw = emit_op(opcodew);
    code_p->y_u.oxx.xl = emit_xreg(var_offset);
  }
  cip->cpc = cip->cpc->nextInst;
  if (pass_no) {
    OPREG var_offset;
    int is_y_var;

    ve = (Ventry *)cip->cpc->rnd1;
    is_y_var = (ve->KindOfVE == PermVar);
    var_offset = Var_Ref(ve, is_y_var);
    code_p->y_u.oxx.xr = emit_xreg(var_offset);
  }
  GONEXT(oxx);
  return code_p;
}

inline static yamop *a_vr(op_numbers opcodex, op_numbers opcodey, yamop *code_p,
                          int pass_no, struct intermediates *cip) {
  struct PSEUDO *cpc = cip->cpc;
  Ventry *ve = (Ventry *)cpc->rnd1;
  int is_y_var = (ve->KindOfVE == PermVar);

  if (is_y_var) {
    if (opcodey == _put_y_val) {
      struct PSEUDO *ncpc = cpc->nextInst;
      if (ncpc->op == put_val_op &&
          ((Ventry *)ncpc->rnd1)->KindOfVE == PermVar) {
        /* peephole! two put_y_vars in a row */
        if (pass_no) {
          OPREG var_offset;
          OPREG var_offset2;
          Ventry *ve2 = (Ventry *)ncpc->rnd1;

          var_offset = Var_Ref(ve, is_y_var);
          code_p->opc = emit_op(_put_y_vals);
          code_p->y_u.yyxx.y1 = emit_yreg(var_offset);
          code_p->y_u.yyxx.x1 = emit_x(cpc->rnd2);
          var_offset2 = Var_Ref(ve2, is_y_var);
          code_p->y_u.yyxx.y2 = emit_yreg(var_offset2);
          code_p->y_u.yyxx.x2 = emit_x(ncpc->rnd2);
        }
        cip->cpc = ncpc;
        GONEXT(yyxx);
        return code_p;
        /* simplify unification  code */
      } else if (FALSE && cpc->rnd2 == 0 && ncpc->op == get_var_op &&
                 ncpc->rnd2 == 0 &&
                 ((Ventry *)ncpc->rnd1)->KindOfVE != PermVar) {
        if (pass_no) {
          OPREG var_offset;
          OPREG var_offset2;
          Ventry *ve2 = (Ventry *)ncpc->rnd1;

          code_p->opc = emit_op(_put_y_var);
          var_offset = Var_Ref(ve, is_y_var);
          var_offset2 = Var_Ref(ve2, !is_y_var);
          code_p->y_u.yx.x = emit_xreg(var_offset2);
          code_p->y_u.yx.y = emit_yreg(var_offset);
        }
        cip->cpc = ncpc;
        GONEXT(yx);
        return code_p;
      }
    } else if (opcodey == _get_y_var) {
      struct PSEUDO *ncpc = cpc->nextInst;
      if (ncpc->op == get_var_op &&
          ((Ventry *)ncpc->rnd1)->KindOfVE == PermVar) {
        /* peephole! two put_y_vars in a row */
        if (pass_no) {
          OPREG var_offset;
          OPREG var_offset2;
          Ventry *ve2 = (Ventry *)ncpc->rnd1;

          var_offset = Var_Ref(ve, is_y_var);
          code_p->opc = emit_op(_get_yy_var);
          code_p->y_u.yyxx.y1 = emit_yreg(var_offset);
          code_p->y_u.yyxx.x1 = emit_x(cpc->rnd2);
          var_offset2 = Var_Ref(ve2, is_y_var);
          code_p->y_u.yyxx.y2 = emit_yreg(var_offset2);
          code_p->y_u.yyxx.x2 = emit_x(ncpc->rnd2);
        }
        cip->cpc = ncpc;
        GONEXT(yyxx);
        return code_p;
      }
    }
    if (pass_no) {
      OPREG var_offset;
      var_offset = Var_Ref(ve, is_y_var);
      code_p->opc = emit_op(opcodey);
      code_p->y_u.yx.y = emit_yreg(var_offset);
      code_p->y_u.yx.x = emit_x(cpc->rnd2);
    }
    GONEXT(yx);
    return code_p;
  }
  if (opcodex == _put_x_val && cpc->nextInst) {
    if (cpc->nextInst->op == put_val_op &&
        !(((Ventry *)cpc->nextInst->rnd1)->KindOfVE == PermVar)) {
      PInstr *ncpc = cpc->nextInst;
      /* peephole! two put_x_vars in a row */
      if (pass_no) {
        OPREG var_offset;
        OPREG var_offset2;
        Ventry *ve2 = (Ventry *)ncpc->rnd1;

        var_offset = Var_Ref(ve, is_y_var);
        code_p->opc = emit_op(_put_xx_val);
        code_p->y_u.xxxx.xl1 = emit_xreg(var_offset);
        code_p->y_u.xxxx.xr1 = emit_x(cpc->rnd2);
        var_offset2 = Var_Ref(ve2, is_y_var);
        code_p->y_u.xxxx.xl2 = emit_xreg(var_offset2);
        code_p->y_u.xxxx.xr2 = emit_x(ncpc->rnd2);
      }
      cip->cpc = ncpc;
      GONEXT(xxxx);
      return code_p;
      /* simplify unification */
    } else if (cpc->rnd2 == 0 && cpc->nextInst->rnd2 == 0) {
      OPREG var_offset;
      OPREG var_offset2;
      Ventry *ve2;
      int is_y_var2;
      PInstr *ncpc;

      ncpc = cpc->nextInst;
      ve2 = (Ventry *)ncpc->rnd1;
      is_y_var2 = (ve2->KindOfVE == PermVar);
      /* put + get */
      if (ncpc->op == get_var_op || ncpc->op == get_val_op) {
        if (is_y_var2) {
          if (pass_no) {
            var_offset = Var_Ref(ve, is_y_var);
            var_offset2 = Var_Ref(ve2, is_y_var2);
            if (ncpc->op == get_var_op)
              code_p->opc = emit_op(_get_y_var);
            else
              code_p->opc = emit_op(_get_y_val);
            code_p->y_u.yx.x = emit_xreg(var_offset);
            code_p->y_u.yx.y = emit_yreg(var_offset2);
          }
          GONEXT(yx);
          cip->cpc = ncpc;
          return code_p;
        } else {
          if (pass_no) {
            var_offset = Var_Ref(ve, is_y_var);
            var_offset2 = Var_Ref(ve2, is_y_var2);
            code_p->y_u.xx.xl = emit_xreg(var_offset);
            code_p->y_u.xx.xr = emit_xreg(var_offset2);
            if (ncpc->op == get_var_op)
              code_p->opc = emit_op(_put_x_val);
            else {
              code_p->opc = emit_op(_get_x_val);
            }
          }
          GONEXT(xx);
          cip->cpc = ncpc;
          return code_p;
        }
      }
    }
  }
  if (pass_no) {
    OPREG var_offset;

    var_offset = Var_Ref(ve, is_y_var);
    code_p->opc = emit_op(opcodex);
    code_p->y_u.xx.xl = emit_xreg(var_offset);
    code_p->y_u.xx.xr = emit_x(cpc->rnd2);
    /* a small trick, usualy the lower argument is the one bound */
    if (opcodex == _get_x_val && code_p->y_u.xx.xl > code_p->y_u.xx.xr) {
      wamreg x1 = code_p->y_u.xx.xl;
      code_p->y_u.xx.xl = code_p->y_u.xx.xr;
      code_p->y_u.xx.xr = x1;
    }
  }
  GONEXT(xx);
  return code_p;
}

inline static yamop *a_rv(op_numbers opcodex, op_numbers opcodey,
                          OPREG var_offset, yamop *code_p, int pass_no,
                          struct PSEUDO *cpc) {
  Ventry *ve = (Ventry *)cpc->rnd1;
  int is_y_var = (ve->KindOfVE == PermVar);

  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op(opcodey);
      code_p->y_u.yx.x = emit_x(cpc->rnd2);
      code_p->y_u.yx.y = emit_yreg(var_offset);
    }
    GONEXT(yx);
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcodex);
      code_p->y_u.xx.xl = emit_x(cpc->rnd2);
      code_p->y_u.xx.xr = emit_xreg(var_offset);
    }
    GONEXT(xx);
  }
  return code_p;
}

#ifdef SFUNC

/* vsc: I don't understand these instructions */

inline static void a_vsf(int opcode, yamop *code_p, int pass_no,
                         struct PSEUDO *cpc) {
  Ventry *ve = (Ventry *)cpc->rnd1;
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->y_u.fy.f = emit_f(cpc->rnd2);
      code_p->y_u.fy.a = ArityOfFunctor(emit_f(cpc->rnd2));
      code_p->y_u.fy.y = emit_yreg(var_offset);
    }
    GONEXT(fy);
  } else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->y_u.fx.f = emit_f(cpc->rnd2);
      code_p->y_u.fx.a = ArityOfFunctor(emit_f(cpc->rnd2));
      code_p->y_u.fx.x = emit_xreg(var_offset);
    }
    GONEXT(fx);
  }
  return code_p;
}

inline static void a_asf(int opcode, yamop *code_p, int pass_no,
                         struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
    code_p->y_u.fn.f = emit_f(cpc->rnd2);
    code_p->y_u.fn.a = ArityOfFunctor(emit_f(cpc->rnd2));
    code_p->y_u.fn.n = emit_count(cpc->rnd1);
  }
  GONEXT(fn);
  return code_p;
}
#endif

inline static void a_pair(CELL *seq_ptr, int pass_no,
                          struct intermediates *cip) {
  if (pass_no) {
    CELL lab, lab0 = seq_ptr[1];
    lab = (CELL)emit_ilabel(lab0, cip);
    seq_ptr[0] = (CELL)emit_a(seq_ptr[0]);
    seq_ptr[1] = lab;
  }
}

inline static yamop *a_n(op_numbers opcode, int count, yamop *code_p,
                         int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.s.s = count;
  }
  GONEXT(s);
  return code_p;
}

#ifdef BEAM
inline static yamop *a_eam(op_numbers opcode, int pred, long cl, yamop *code_p,
                           int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.os.opcw = cl;
    code_p->y_u.os.s = pred;
  }
  GONEXT(os);
  return code_p;
}
#endif

inline static yamop *a_un(op_numbers opcode, op_numbers opcodew, int count,
                          yamop *code_p, int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.os.opcw = emit_op(opcodew);
    code_p->y_u.os.s = count;
  }
  GONEXT(os);
  return code_p;
}

inline static yamop *a_f(CELL rnd1, op_numbers opcode, yamop *code_p,
                         int pass_no) {
  if (pass_no) {
    Functor f = emit_f(rnd1);

    code_p->opc = emit_op(opcode);
    code_p->y_u.fa.f = f;
    code_p->y_u.fa.a = ArityOfFunctor(f);
  }
  GONEXT(fa);
  return code_p;
}

inline static yamop *a_uf(CELL rnd1, op_numbers opcode, op_numbers opcodew,
                          yamop *code_p, int pass_no) {
  if (pass_no) {
    Functor f = emit_f(rnd1);

    code_p->opc = emit_op(opcode);
    code_p->y_u.ofa.opcw = emit_op(opcodew);
    code_p->y_u.ofa.f = f;
    code_p->y_u.ofa.a = ArityOfFunctor(f);
  }
  GONEXT(ofa);
  return code_p;
}

inline static yamop *a_c(CELL rnd1, op_numbers opcode, yamop *code_p,
                         int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.c.c = emit_c(rnd1);
  }
  GONEXT(c);
  return code_p;
}

inline static yamop *a_uc(CELL rnd1, op_numbers opcode, op_numbers opcode_w,
                          yamop *code_p, int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.oc.opcw = emit_op(opcode_w);
    code_p->y_u.oc.c = emit_c(rnd1);
  }
  GONEXT(oc);
  return code_p;
}

inline static yamop *a_wblob(CELL rnd1, op_numbers opcode,
                             int *clause_has_blobsp, yamop *code_p, int pass_no,
                             struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.N.b =
        AbsAppl((CELL *)(Unsigned(cip->code_addr) + cip->label_offset[rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(N);
  return code_p;
}

static yamop *a_ensure_space(op_numbers opcode, yamop *code_p, int pass_no,
                             struct intermediates *cip, clause_info *clinfo) {
  if (cip->cpc->rnd1 > 4096) {
    if (pass_no) {
      code_p->opc = emit_op(opcode);
      code_p->y_u.Osbpa.i = sizeof(CELL) * cip->cpc->rnd1;
      code_p->y_u.Osbpa.p = clinfo->CurrentPred;
      code_p->y_u.Osbpa.bmap = NULL;
      code_p->y_u.Osbpa.s = emit_count(-Signed(RealEnvSize));
    }
    GONEXT(Osbpa);
  }
  return code_p;
}

inline static yamop *a_wdbt(CELL rnd1, op_numbers opcode,
                            int *clause_has_dbtermp, yamop *code_p, int pass_no,
                            struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.D.D = rnd1;
    add_to_dbtermsl(cip, cip->cpc->rnd1);
  }
  *clause_has_dbtermp = TRUE;
  GONEXT(D);
  return code_p;
}

inline static yamop *a_ublob(CELL rnd1, op_numbers opcode, op_numbers opcode_w,
                             int *clause_has_blobsp, yamop *code_p, int pass_no,
                             struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.oN.opcw = emit_op(opcode_w);
    code_p->y_u.oN.b =
        AbsAppl((CELL *)(Unsigned(cip->code_addr) + cip->label_offset[rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(oN);
  return code_p;
}

// strings are blobs
inline static yamop *a_ustring(CELL rnd1, op_numbers opcode,
                               op_numbers opcode_w, int *clause_has_blobsp,
                               yamop *code_p, int pass_no,
                               struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.ou.opcw = emit_op(opcode_w);
    code_p->y_u.ou.ut =
        AbsAppl((CELL *)(Unsigned(cip->code_addr) + cip->label_offset[rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(ou);
  return code_p;
}

inline static yamop *a_udbt(CELL rnd1, op_numbers opcode, op_numbers opcode_w,
                            int *clause_has_dbtermp, yamop *code_p, int pass_no,
                            struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.oD.opcw = emit_op(opcode_w);
    code_p->y_u.oD.D = cip->cpc->rnd1;
    add_to_dbtermsl(cip, cip->cpc->rnd1);
  }
  *clause_has_dbtermp = TRUE;
  GONEXT(oD);
  return code_p;
}

inline static yamop *a_ud(op_numbers opcode, op_numbers opcode_w, yamop *code_p,
                          int pass_no, struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.od.opcw = emit_op(opcode_w);
    code_p->y_u.od.d[0] = (CELL)FunctorDouble;
    code_p->y_u.od.d[1] = RepAppl(cpc->rnd1)[1];
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    code_p->y_u.od.d[2] = RepAppl(cpc->rnd1)[2];
#endif
  }
  GONEXT(od);
  return code_p;
}

inline static yamop *a_ui(op_numbers opcode, op_numbers opcode_w, yamop *code_p,
                          int pass_no, struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.oi.opcw = emit_op(opcode_w);
    code_p->y_u.oi.i[0] = (CELL)FunctorLongInt;
    code_p->y_u.oi.i[1] = RepAppl(cpc->rnd1)[1];
  }
  GONEXT(oi);
  return code_p;
}

inline static yamop *a_wd(op_numbers opcode, yamop *code_p, int pass_no,
                          struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.d.d[0] = (CELL)FunctorDouble;
    code_p->y_u.d.d[1] = RepAppl(cpc->rnd1)[1];
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    code_p->y_u.d.d[2] = RepAppl(cpc->rnd1)[2];
#endif
  }
  GONEXT(d);
  return code_p;
}

inline static yamop *a_wi(op_numbers opcode, yamop *code_p, int pass_no,
                          struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.i.i[0] = (CELL)FunctorLongInt;
    code_p->y_u.i.i[1] = RepAppl(cpc->rnd1)[1];
  }
  GONEXT(i);
  return code_p;
}

inline static yamop *a_nc(CELL rnd1, op_numbers opcode, int i, yamop *code_p,
                          int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.sc.s = i;
    code_p->y_u.sc.c = emit_c(rnd1);
  }
  GONEXT(sc);
  return code_p;
}

inline static yamop *a_unc(CELL rnd1, op_numbers opcode, op_numbers opcodew,
                           int i, yamop *code_p, int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.osc.opcw = emit_op(opcodew);
    code_p->y_u.osc.s = i;
    code_p->y_u.osc.c = emit_c(rnd1);
  }
  GONEXT(osc);
  return code_p;
}

inline static yamop *a_rf(op_numbers opcode, yamop *code_p, int pass_no,
                          struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xfa.x = emit_x(cpc->rnd2);
    code_p->y_u.xfa.f = emit_f(cpc->rnd1);
    code_p->y_u.xfa.a = ArityOfFunctor(emit_f(cpc->rnd1));
  }
  GONEXT(xfa);
  return code_p;
}

inline static yamop *a_rd(op_numbers opcode, yamop *code_p, int pass_no,
                          struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xd.x = emit_x(cpc->rnd2);
    code_p->y_u.xd.d[0] = (CELL)FunctorDouble;
    code_p->y_u.xd.d[1] = RepAppl(cpc->rnd1)[1];
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
    code_p->y_u.xd.d[2] = RepAppl(cpc->rnd1)[2];
#endif
  }
  GONEXT(xd);
  return code_p;
}

inline static yamop *a_ri(op_numbers opcode, yamop *code_p, int pass_no,
                          struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xi.x = emit_x(cpc->rnd2);
    code_p->y_u.xi.i[0] = (CELL)FunctorLongInt;
    code_p->y_u.xi.i[1] = RepAppl(cpc->rnd1)[1];
  }
  GONEXT(xi);
  return code_p;
}

static yamop *a_rc(op_numbers opcode, yamop *code_p, int pass_no,
                   struct intermediates *cip) {
  if (cip->cpc->rnd2 == 1 && cip->cpc->nextInst->rnd2 == 2 &&
      (cip->cpc->nextInst->op == get_atom_op ||
       cip->cpc->nextInst->op == get_num_op)) {
    struct PSEUDO *next;
    next = cip->cpc->nextInst;
    if (next->nextInst->rnd2 == 3 && (next->nextInst->op == get_atom_op ||
                                      next->nextInst->op == get_num_op)) {
      struct PSEUDO *snext = next->nextInst;

      if (snext->nextInst->rnd2 == 4 && (snext->nextInst->op == get_atom_op ||
                                         snext->nextInst->op == get_num_op)) {
        struct PSEUDO *s2next = snext->nextInst;
        if (s2next->nextInst->rnd2 == 5 &&
            (s2next->nextInst->op == get_atom_op ||
             s2next->nextInst->op == get_num_op)) {
          struct PSEUDO *s3next = s2next->nextInst;
          if (s3next->nextInst->rnd2 == 6 &&
              (s3next->nextInst->op == get_atom_op ||
               s3next->nextInst->op == get_num_op)) {
            if (pass_no) {
              code_p->opc = emit_op(_get_6atoms);
              code_p->y_u.cccccc.c1 = emit_c(cip->cpc->rnd1);
              code_p->y_u.cccccc.c2 = emit_c(next->rnd1);
              code_p->y_u.cccccc.c3 = emit_c(snext->rnd1);
              code_p->y_u.cccccc.c4 = emit_c(s2next->rnd1);
              code_p->y_u.cccccc.c5 = emit_c(s3next->rnd1);
              code_p->y_u.cccccc.c6 = emit_c(s3next->nextInst->rnd1);
            }
            cip->cpc = s3next->nextInst;
            GONEXT(cccccc);
          } else {
            if (pass_no) {
              code_p->opc = emit_op(_get_5atoms);
              code_p->y_u.ccccc.c1 = emit_c(cip->cpc->rnd1);
              code_p->y_u.ccccc.c2 = emit_c(next->rnd1);
              code_p->y_u.ccccc.c3 = emit_c(snext->rnd1);
              code_p->y_u.ccccc.c4 = emit_c(s2next->rnd1);
              code_p->y_u.ccccc.c5 = emit_c(s3next->rnd1);
            }
            cip->cpc = s3next;
            GONEXT(ccccc);
          }
        } else {
          if (pass_no) {
            code_p->opc = emit_op(_get_4atoms);
            code_p->y_u.cccc.c1 = emit_c(cip->cpc->rnd1);
            code_p->y_u.cccc.c2 = emit_c(next->rnd1);
            code_p->y_u.cccc.c3 = emit_c(snext->rnd1);
            code_p->y_u.cccc.c4 = emit_c(s2next->rnd1);
          }
          cip->cpc = s2next;
          GONEXT(cccc);
        }
      } else {
        if (pass_no) {
          code_p->opc = emit_op(_get_3atoms);
          code_p->y_u.ccc.c1 = emit_c(cip->cpc->rnd1);
          code_p->y_u.ccc.c2 = emit_c(next->rnd1);
          code_p->y_u.ccc.c3 = emit_c(snext->rnd1);
        }
        cip->cpc = snext;
        GONEXT(ccc);
      }
    } else {
      if (pass_no) {
        code_p->opc = emit_op(_get_2atoms);
        code_p->y_u.cc.c1 = emit_c(cip->cpc->rnd1);
        code_p->y_u.cc.c2 = emit_c(next->rnd1);
      }
      cip->cpc = next;
      GONEXT(cc);
    }
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcode);
      code_p->y_u.xc.x = emit_x(cip->cpc->rnd2);
      code_p->y_u.xc.c = emit_c(cip->cpc->rnd1);
    }
    GONEXT(xc);
  }
  return code_p;
}

inline static yamop *a_rb(op_numbers opcode, int *clause_has_blobsp,
                          yamop *code_p, int pass_no,
                          struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xN.x = emit_x(cip->cpc->rnd2);
    code_p->y_u.xN.b = AbsAppl(
        (CELL *)(Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(xN);
  return code_p;
}

inline static yamop *a_rstring(op_numbers opcode, int *clause_has_blobsp,
                               yamop *code_p, int pass_no,
                               struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xu.x = emit_x(cip->cpc->rnd2);
    code_p->y_u.xu.ut = AbsAppl(
        (CELL *)(Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(xu);
  return code_p;
}

inline static yamop *a_dbt(op_numbers opcode, int *clause_has_dbtermp,
                           yamop *code_p, int pass_no,
                           struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xD.x = emit_x(cip->cpc->rnd2);
    code_p->y_u.xD.D = cip->cpc->rnd1;
    add_to_dbtermsl(cip, cip->cpc->rnd1);
  }
  *clause_has_dbtermp = TRUE;
  GONEXT(xD);
  return code_p;
}

inline static yamop *a_r(CELL arnd2, op_numbers opcode, yamop *code_p,
                         int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.x.x = emit_x(arnd2);
  }
  GONEXT(x);
  return code_p;
}

static yamop *check_alloc(clause_info *clinfo, yamop *code_p, int pass_no,
                          struct intermediates *cip) {
  if (clinfo->alloc_found == 2) {
    if (clinfo->CurrentPred->PredFlags & LogUpdatePredFlag)
      code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
    code_p = a_e(_allocate, code_p, pass_no);
    clinfo->alloc_found = 1;
  }
  return code_p;
}

static yamop *a_l(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no,
                  struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.l.l =
        emit_a(Unsigned(cip->code_addr) + cip->label_offset[rnd1]);
  }
  GONEXT(l);
  return code_p;
}

static yamop *a_il(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no,
                   struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.l.l = emit_ilabel(rnd1, cip);
  }
  GONEXT(l);
  return code_p;
}

static yamop *
a_p(op_numbers opcode, clause_info *clinfo, yamop *code_p, int pass_no,
    struct intermediates *cip) { /* emit opcode & predicate code address */
  Prop fe = (Prop)(cip->cpc->rnd1);
  CELL Flags = RepPredProp(fe)->PredFlags;
  if (Flags & AsmPredFlag) {
    op_numbers op;
    int is_test = FALSE;

    switch (Flags & 0x7f) {
    case _equal:
      op = _p_equal;
      break;
#if INLINE_BIG_COMPARISONS
    case _dif:
      op = _p_dif;
      is_test = true;
      break;
    case _eq:
      op = _p_eq;
      is_test = true;
      break;
#endif
    case _functor:
      code_p = check_alloc(clinfo, code_p, pass_no, cip);
      op = _p_functor;
      break;
    default:
      // op = _p_equal;  /* just to make some compilers happy */
      Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
                "internal assembler error for built-in (%d)", (Flags & 0x7f));
      save_machine_regs();
      siglongjmp(cip->CompilerBotch, 1);
    }
    if (is_test) {
      UInt lab;

      if ((lab = cip->failure_handler)) {
        return a_l(lab, op, code_p, pass_no, cip);
      } else {
        return a_il((CELL)FAILCODE, op, code_p, pass_no, cip);
      }
    } else {
      return a_e(op, code_p, pass_no);
    }
  }
  if (Flags & CPredFlag && opcode == _call) {
    code_p = check_alloc(clinfo, code_p, pass_no, cip);
    if (cip->failure_handler && (Flags & TestPredFlag)) {
      if (pass_no) {
        if (Flags & UserCPredFlag) {
          Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
                    "user defined predicate cannot be a test predicate");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
        } else
          code_p->opc = emit_op(_call_c_wfail);
        code_p->y_u.slpp.s =
            emit_count(-Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2);
        code_p->y_u.slpp.l = emit_fail(cip);
        code_p->y_u.slpp.p0 = clinfo->CurrentPred;
        code_p->y_u.slpp.p = emit_pe(RepPredProp(fe));
      }
      GONEXT(slpp);
    } else {
      if (pass_no) {
        code_p->y_u.Osbpp.p = RepPredProp(fe);
        if (Flags & UserCPredFlag) {
          code_p->opc = emit_op(_call_usercpred);
        } else {
          if (RepPredProp(fe)->FunctorOfPred == FunctorExecuteInMod) {
            code_p->y_u.Osbmp.mod = cip->cpc->rnd4;
            code_p->opc = emit_op(_p_execute);
          } else if (RepPredProp(fe)->FunctorOfPred == FunctorExecute2InMod) {
            code_p->opc = emit_op(_p_execute2);
          } else {
            code_p->opc = emit_op(_call_cpred);
          }
        }
        code_p->y_u.Osbpp.s =
            emit_count(-Signed(RealEnvSize) - CELLSIZE * (cip->cpc->rnd2));
        code_p->y_u.Osbpp.p0 = clinfo->CurrentPred;
        if (cip->cpc->rnd2) {
          code_p->y_u.Osbpp.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
        } else {
          /* there is no bitmap as there are no variables in the environment */
          code_p->y_u.Osbpp.bmap = NULL;
        }
      }
      GONEXT(Osbpp);
    }
    return code_p;
  }

  if (opcode == _call && clinfo->alloc_found == 2) {
    if (clinfo->CurrentPred->PredFlags & LogUpdatePredFlag)
      code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
    if (pass_no) {
      code_p->opc = emit_op(_fcall);
    }
    clinfo->alloc_found = 1;
  } else {
    code_p = check_alloc(clinfo, code_p, pass_no, cip);
    if (pass_no)
      code_p->opc = emit_op(opcode);
  }
  if (opcode == _call) {
    if (pass_no) {
      code_p->y_u.Osbpp.s =
          emit_count(-Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2);
      code_p->y_u.Osbpp.p = RepPredProp(fe);
      code_p->y_u.Osbpp.p0 = clinfo->CurrentPred;
      if (cip->cpc->rnd2)
        code_p->y_u.Osbpp.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
      else
        /* there is no bitmap as there are no variables in the environment */
        code_p->y_u.Osbpp.bmap = NULL;
    }
    GONEXT(Osbpp);
  } else if (opcode == _execute || opcode == _dexecute) {
    if (pass_no) {
      if (Flags & CPredFlag) {
        code_p->opc = emit_op(_execute_cpred);
      }
      code_p->y_u.Osbpp.p = RepPredProp(fe);
      code_p->y_u.Osbpp.p0 = clinfo->CurrentPred;
      code_p->y_u.Osbpp.s = 0;
        code_p->y_u.Osbpp.bmap = NULL;
    }
    GONEXT(Osbpp);
  } else {
    if (pass_no)
      code_p->y_u.p.p = RepPredProp(fe);
    GONEXT(p);
  }
  return code_p;
}

/*
  emit a false call so that the garbage collector and friends will find
  reasonable information on the stack.
*/
static yamop *a_empty_call(clause_info *clinfo, yamop *code_p, int pass_no,
                           struct intermediates *cip) {
  if (clinfo->alloc_found == 1 && !clinfo->dealloc_found) {
    /* we have a solid environment under us, just trust it */
    if (pass_no)
      code_p->opc = emit_op(_call);
  } else {
    /** oops, our environment is crap */
    if (pass_no)
      code_p->opc = emit_op(_fcall);
  }
  if (pass_no) {
    PredEntry *pe = RepPredProp(Yap_GetPredPropByAtom(AtomTrue, 0));
    code_p->y_u.Osbpp.s =
        emit_count(-Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2);
    code_p->y_u.Osbpp.p = pe;
    code_p->y_u.Osbpp.p0 = clinfo->CurrentPred;
    if (cip->cpc->rnd2)
      code_p->y_u.Osbpp.bmap = emit_bmlabel(cip->cpc->rnd1, cip);
    else
      /* there is no bitmap as there are no variables in the environment */
      code_p->y_u.Osbpp.bmap = NULL;
  }
  GONEXT(Osbpp);
  return code_p;
}

static yamop *a_pl(op_numbers opcode, PredEntry *pred, yamop *code_p,
                   int pass_no) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.p.p = pred;
  }
  GONEXT(p);
  return code_p;
}

static COUNT

compile_cmp_flags(unsigned char *s0) {
  char *s = (char *)s0;
  if (strcmp(s, "=<") == 0)
    return EQ_OK_IN_CMP | LT_OK_IN_CMP;
  if (strcmp(s, "is") == 0)
    return EQ_OK_IN_CMP;
  if (strcmp(s, "@=<") == 0)
    return EQ_OK_IN_CMP | LT_OK_IN_CMP;
  if (strcmp(s, "<") == 0)
    return LT_OK_IN_CMP;
  if (strcmp(s, "@<") == 0)
    return LT_OK_IN_CMP;
  if (strcmp(s, ">=") == 0)
    return EQ_OK_IN_CMP | GT_OK_IN_CMP;
  if (strcmp(s, "@>=") == 0)
    return EQ_OK_IN_CMP | GT_OK_IN_CMP;
  if (strcmp(s, ">") == 0)
    return GT_OK_IN_CMP;
  if (strcmp(s, "@>") == 0)
    return GT_OK_IN_CMP;
  if (strcmp(s, "=:=") == 0)
    return EQ_OK_IN_CMP;
  if (strcmp(s, "=\\=") == 0)
    return GT_OK_IN_CMP | LT_OK_IN_CMP;
  if (strcmp(s, "\\==") == 0)
    return GT_OK_IN_CMP | LT_OK_IN_CMP;
  Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
            "internal assembler error, %s/2 not recognised as binary op", s);
  return 0;
}

COUNT
Yap_compile_cmp_flags(PredEntry *pred) {
  return compile_cmp_flags(
      RepAtom(NameOfFunctor(pred->FunctorOfPred))->UStrOfAE);
}

static yamop *a_bfunc(CELL a1, CELL a2, PredEntry *pred, clause_info *clinfo,
                      yamop *code_p, int pass_no, struct intermediates *cip) {
  Ventry *ve1 = (Ventry *)a1;
  Ventry *ve2 = (Ventry *)a2;
  OPREG var_offset1;
  int is_y_var = (ve1->KindOfVE == PermVar);

  var_offset1 = Var_Ref(ve1, is_y_var);
  if (ve1->KindOfVE == PermVar) {
    yslot v1 = emit_yreg(var_offset1);
    bool is_y_var2 = (ve2->KindOfVE == PermVar);
    OPREG var_offset2 = Var_Ref(ve2, is_y_var2);
    if (is_y_var2) {
      if (pass_no) {
        code_p->opc = emit_op(_call_bfunc_yy);
        code_p->y_u.plyys.p = pred;
        code_p->y_u.plyys.f = emit_fail(cip);
        code_p->y_u.plyys.y1 = v1;
        code_p->y_u.plyys.y2 = emit_yreg(var_offset2);
        code_p->y_u.plyys.flags = compile_cmp_flags(
            RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))
                ->UStrOfAE);
      }
      GONEXT(plyys);
    } else {
      if (pass_no) {
        code_p->opc = emit_op(_call_bfunc_yx);
        code_p->y_u.plxys.p = pred;
        code_p->y_u.plxys.f = emit_fail(cip);
        code_p->y_u.plxys.x = emit_xreg(var_offset2);
        code_p->y_u.plxys.y = v1;
        code_p->y_u.plxys.flags = compile_cmp_flags(
            RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))
                ->UStrOfAE);
      }
      GONEXT(plxys);
    }
  } else {
    wamreg x1 = emit_xreg(var_offset1);
    OPREG var_offset2;

    bool is_y_var2 = (ve2->KindOfVE == PermVar);
    var_offset2 = Var_Ref(ve2, is_y_var2);
    if (is_y_var2) {
      if (pass_no) {
        code_p->opc = emit_op(_call_bfunc_xy);
        code_p->y_u.plxys.p = pred;
        code_p->y_u.plxys.f = emit_fail(cip);
        code_p->y_u.plxys.x = x1;
        code_p->y_u.plxys.y = emit_yreg(var_offset2);
        code_p->y_u.plxys.flags = compile_cmp_flags(
            RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))
                ->UStrOfAE);
      }
      GONEXT(plxys);
    } else {
      if (pass_no) {
        //	printf(" %p --- %p\n", x1, emit_xreg(var_offset2) );
        code_p->opc = emit_op(_call_bfunc_xx);
        code_p->y_u.plxxs.p = pred;
        code_p->y_u.plxxs.f = emit_fail(cip);
        code_p->y_u.plxxs.x1 = x1;
        code_p->y_u.plxxs.x2 = emit_xreg(var_offset2);
        code_p->y_u.plxxs.flags = compile_cmp_flags(
            RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))
                ->UStrOfAE);
      }
      GONEXT(plxxs);
    }
  }
  return code_p;
}

static yamop *a_igl(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no,
                    struct intermediates *cip) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.l.l = emit_ilabel(rnd1, cip);
  }
  GONEXT(l);
  return code_p;
}

static yamop *a_xigl(op_numbers opcode, yamop *code_p, int pass_no,
                     struct PSEUDO *cpc) {
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xll.x = emit_x(cpc->rnd2);
    code_p->y_u.xll.l1 = emit_a(cpc->rnd1);
    code_p->y_u.xll.l2 = NEXTOP(code_p, xll);
  }
  GONEXT(xll);
  return code_p;
}

/* enable peephole optimisation for switch_on_term to switch_on_list */
static int is_switch_on_list(op_numbers opcode, struct intermediates *cip) {
  struct PSEUDO *cpc = cip->cpc, *ncpc, *n2cpc;
  CELL *if_table;

  /* only do this is indexing code is stable */
  if (cip->CurrentPred->PredFlags & LogUpdatePredFlag)
    return FALSE;
  /* check if we are transforming a switch_on_type */
  if (opcode != _switch_on_type)
    return FALSE;
  /* should have two instructions next */
  if ((ncpc = cpc->nextInst) == NULL || (n2cpc = ncpc->nextInst) == NULL)
    return FALSE;
  /* one a label, the other an if_constant */
  if (ncpc->op != label_op || n2cpc->op != if_c_op)
    return FALSE;
  /* the label for the constant case should be the if_c label
     (this should always hold) */
  if (cpc->arnds[1] != ncpc->rnd1)
    return FALSE;
  if_table = (CELL *)(n2cpc->rnd2);
  /* the constant switch should only have the empty list */
  if (n2cpc->rnd1 != 1 || if_table[0] != TermNil)
    return FALSE;
  /*
    should be pointing to a clause so that we can push the clause opcode,
    this should be fixable;
    also, we need to go what's in there, so it cannot be suspend code!
   */
  if (cpc->arnds[0] & 1 ||
      (yamop *)(cpc->arnds[0]) ==
          (yamop *)(&(cip->CurrentPred->cs.p_code.ExpandCode)))
    return FALSE;
  /* Appl alternative should be pointing to same point as [] alternative,
     usually FAILCODE */
  if (if_table[3] != cpc->arnds[2])
    return FALSE;
  /* yesss!! */
  return TRUE;
}

static yamop *a_4sw(op_numbers opcode, yamop *code_p, int pass_no,
                    struct intermediates *cip) {
  CELL *seq_ptr;

  if (is_switch_on_list(opcode, cip)) {
    if (pass_no) {
      CELL *ars = (CELL *)(cip->cpc->nextInst->nextInst->rnd2);
      code_p->opc = emit_op(_switch_list_nl);
      seq_ptr = cip->cpc->arnds;
      code_p->y_u.ollll.pop = ((yamop *)(seq_ptr[0]))->opc;
      code_p->y_u.ollll.l1 = emit_ilabel(seq_ptr[0], cip);
      code_p->y_u.ollll.l2 = emit_ilabel(ars[1], cip);
      code_p->y_u.ollll.l3 = emit_ilabel(seq_ptr[2], cip);
      code_p->y_u.ollll.l4 = emit_ilabel(seq_ptr[3], cip);
      if (cip->CurrentPred->PredFlags & LogUpdatePredFlag) {
        LogUpdIndex *icl = ClauseCodeToLogUpdIndex(ars);

        Yap_LUIndexSpace_Tree -= icl->ClSize;
        Yap_FreeCodeSpace((char *)icl);
      } else {
        StaticIndex *icl = ClauseCodeToStaticIndex(ars);

        Yap_IndexSpace_Tree -= icl->ClSize;
        Yap_FreeCodeSpace((char *)icl);
      }
    }
    GONEXT(ollll);
    /* skip if_cons */
    cip->cpc = cip->cpc->nextInst->nextInst;
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcode);
      seq_ptr = cip->cpc->arnds;
      code_p->y_u.llll.l1 = emit_ilabel(seq_ptr[0], cip);
      code_p->y_u.llll.l2 = emit_ilabel(seq_ptr[1], cip);
      code_p->y_u.llll.l3 = emit_ilabel(seq_ptr[2], cip);
      code_p->y_u.llll.l4 = emit_ilabel(seq_ptr[3], cip);
    }
    GONEXT(llll);
  }
  return code_p;
}

static yamop *a_4sw_x(op_numbers opcode, yamop *code_p, int pass_no,
                      struct intermediates *cip) {
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.xllll.x = emit_x(cip->cpc->rnd2);
    cip->cpc = cip->cpc->nextInst;
    seq_ptr = cip->cpc->arnds;
    code_p->y_u.xllll.l1 = emit_ilabel(seq_ptr[0], cip);
    code_p->y_u.xllll.l2 = emit_ilabel(seq_ptr[1], cip);
    code_p->y_u.xllll.l3 = emit_ilabel(seq_ptr[2], cip);
    code_p->y_u.xllll.l4 = emit_ilabel(seq_ptr[3], cip);
  } else {
    /* skip one */
    cip->cpc = cip->cpc->nextInst;
  }
  GONEXT(xllll);
  return code_p;
}

static yamop *a_4sw_s(op_numbers opcode, yamop *code_p, int pass_no,
                      struct intermediates *cip) {
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.sllll.s = cip->cpc->rnd2;
    cip->cpc = cip->cpc->nextInst;
    seq_ptr = cip->cpc->arnds;
    code_p->y_u.sllll.l1 = emit_ilabel(seq_ptr[0], cip);
    code_p->y_u.sllll.l2 = emit_ilabel(seq_ptr[1], cip);
    code_p->y_u.sllll.l3 = emit_ilabel(seq_ptr[2], cip);
    code_p->y_u.sllll.l4 = emit_ilabel(seq_ptr[3], cip);
  } else {
    /* skip one */
    cip->cpc = cip->cpc->nextInst;
  }
  GONEXT(sllll);
  return code_p;
}

static void init_log_upd_table(LogUpdIndex *ic, union clause_obj *cl_u) {
  /* insert myself in the indexing code chain */
  ic->SiblingIndex = cl_u->lui.ChildIndex;
  if (ic->SiblingIndex) {
    ic->SiblingIndex->PrevSiblingIndex = ic;
  }
  cl_u->lui.ChildIndex = ic;
  ic->PrevSiblingIndex = NULL;
  ic->ChildIndex = NULL;
  ic->ClRefCount = 0;
  ic->ParentIndex = (LogUpdIndex *)cl_u;
  //  INIT_LOCK(ic->ClLock);
  cl_u->lui.ChildIndex = ic;
  cl_u->lui.ClRefCount++;
}

static void init_static_table(StaticIndex *ic, union clause_obj *cl_u) {
  /* insert myself in the indexing code chain */
  ic->SiblingIndex = cl_u->si.ChildIndex;
  ic->ChildIndex = NULL;
  cl_u->si.ChildIndex = ic;
}

static yamop *a_hx(op_numbers opcode, union clause_obj *cl_u, int log_update,
                   yamop *code_p, int pass_no, struct intermediates *cip) {
  register CELL i, imax;
  register CELL *seq_ptr = (CELL *)cip->cpc->rnd2;
  int j = 0;

  imax = cip->cpc->rnd1;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.sssl.s = emit_c(imax);
    code_p->y_u.sssl.l = emit_a(cip->cpc->rnd2);
    if (log_update) {
      init_log_upd_table(ClauseCodeToLogUpdIndex(cip->cpc->rnd2), cl_u);
    } else {
      init_static_table(ClauseCodeToStaticIndex(cip->cpc->rnd2), cl_u);
    }
  }
  if (pass_no) {
    for (i = 0; i < imax; i++) {
      yamop *ipc = (yamop *)seq_ptr[1];
      a_pair(seq_ptr, pass_no, cip);
      if (ipc != FAILCODE) {
        j++;
      }
      seq_ptr += 2;
    }
    code_p->y_u.sssl.e = j;
    code_p->y_u.sssl.w = 0;
  }
  GONEXT(sssl);
  return code_p;
}

static yamop *a_if(op_numbers opcode, union clause_obj *cl_u, int log_update,
                   yamop *code_p, int pass_no, struct intermediates *cip) {
  register CELL i, imax;
  register CELL *seq_ptr = (CELL *)cip->cpc->rnd2;

  imax = cip->cpc->rnd1;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.sssl.s = code_p->y_u.sssl.e = emit_count(imax);
    code_p->y_u.sssl.w = 0;
    code_p->y_u.sssl.l = emit_a(cip->cpc->rnd2);
    if (log_update) {
      init_log_upd_table(ClauseCodeToLogUpdIndex(cip->cpc->rnd2), cl_u);
    } else {
      init_static_table(ClauseCodeToStaticIndex(cip->cpc->rnd2), cl_u);
    }
  }
  GONEXT(sssl);
  if (pass_no) {
    CELL lab, lab0;
    for (i = 0; i < imax; i++) {
      a_pair(seq_ptr, pass_no, cip);
      seq_ptr += 2;
    }
    lab0 = seq_ptr[1];
    lab = (CELL)emit_ilabel(lab0, cip);
    seq_ptr[1] = lab;
  }
  return code_p;
}

static yamop *a_ifnot(op_numbers opcode, yamop *code_p, int pass_no,
                      struct intermediates *cip) {
  CELL *seq_ptr = cip->cpc->arnds;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.clll.c = seq_ptr[0];                    /* tag */
    code_p->y_u.clll.l1 = emit_ilabel(seq_ptr[1], cip); /* success point */
    code_p->y_u.clll.l2 = emit_ilabel(seq_ptr[2], cip); /* fail point */
    code_p->y_u.clll.l3 = emit_ilabel(seq_ptr[3], cip); /* delay point */
  }
  GONEXT(clll);
  return code_p;
}

static yamop *a_cut(clause_info *clinfo, yamop *code_p, int pass_no,
                    struct intermediates *cip) {
  cip->clause_has_cut = TRUE;
  code_p = check_alloc(clinfo, code_p, pass_no, cip);
  if (clinfo->dealloc_found) {
    return a_n(_cut_e, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2, code_p,
               pass_no);
  } else if (clinfo->alloc_found == 1) {
    return a_n(_cut, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2, code_p,
               pass_no);
  } else {
    return a_n(_cut_t, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2, code_p,
               pass_no);
  }
}

static yamop *
#ifdef YAPOR
a_try(op_numbers opcode, CELL lab, CELL opr, int nofalts, int hascut,
      yamop *code_p, int pass_no, struct intermediates *cip)
#else
a_try(op_numbers opcode, CELL lab, CELL opr, yamop *code_p, int pass_no,
      struct intermediates *cip)
#endif /* YAPOR */
{
  PredEntry *ap = cip->CurrentPred;

  /* if predicates are logical do it in a different way */
  if (ap->PredFlags & LogUpdatePredFlag) {
    yamop *newcp;
    /* emit a special instruction and then a label for backpatching */
    if (pass_no) {
      UInt size = (UInt)NEXTOP((yamop *)NULL, OtaLl);
      if ((newcp = (yamop *)Yap_AllocCodeSpace(size)) == NULL) {
        /* OOOPS, got in trouble, must do a longjmp and recover space */
        save_machine_regs();
        siglongjmp(cip->CompilerBotch, 2);
      }
      Yap_inform_profiler_of_clause(newcp, (char *)(newcp) + size, ap,
                                    GPROF_INDEX);
      Yap_LUIndexSpace_CP += size;
#ifdef DEBUG
      Yap_NewCps++;
      Yap_LiveCps++;
#endif
      newcp->y_u.OtaLl.n = NULL;
      *cip->current_try_lab = newcp;
      if (opcode == _try_clause) {
        newcp->opc = emit_op(_try_logical);
        newcp->y_u.OtaLl.s = emit_count(opr);
      } else if (opcode == _retry) {
        if (ap->PredFlags & CountPredFlag)
          newcp->opc = emit_op(_count_retry_logical);
        else if (ap->PredFlags & ProfiledPredFlag) {
          if (!Yap_initProfiler(ap)) {
            return NULL;
          }
          newcp->opc = emit_op(_profiled_retry_logical);
        } else
          newcp->opc = emit_op(_retry_logical);
        newcp->y_u.OtaLl.s = emit_count(opr);
      } else {
        /* trust */
        if (ap->PredFlags & CountPredFlag) {
          newcp->opc = emit_op(_count_trust_logical);
        } else if (ap->PredFlags & ProfiledPredFlag) {
          if (!Yap_initProfiler(ap)) {
            return NULL;
          }
          newcp->opc = emit_op(_profiled_trust_logical);
        } else {
          newcp->opc = emit_op(_trust_logical);
        }
        newcp->y_u.OtILl.block = (LogUpdIndex *)(cip->code_addr);
        *cip->current_trust_lab = newcp;
      }
      newcp->y_u.OtaLl.d = ClauseCodeToLogUpdClause(emit_a(lab));
      cip->current_try_lab = &(newcp->y_u.OtaLl.n);
    }
    return code_p;
  }
#ifndef YAPOR
  switch (opr) {
  case 2:
    if (opcode == _try_clause) {
      if (pass_no) {
        code_p->opc = emit_op(_try_clause2);
        code_p->y_u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    } else if (opcode == _retry) {
      if (pass_no) {
        code_p->opc = emit_op(_retry2);
        code_p->y_u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    }
  case 3:
    if (opcode == _try_clause) {
      if (pass_no) {
        code_p->opc = emit_op(_try_clause3);
        code_p->y_u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    } else if (opcode == _retry) {
      if (pass_no) {
        code_p->opc = emit_op(_retry3);
        code_p->y_u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    }
  case 4:
    if (opcode == _try_clause) {
      if (pass_no) {
        code_p->opc = emit_op(_try_clause4);
        code_p->y_u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    } else if (opcode == _retry) {
      if (pass_no) {
        code_p->opc = emit_op(_retry4);
        code_p->y_u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    }
  }
#endif
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.Otapl.d = emit_a(lab);
    code_p->y_u.Otapl.s = emit_count(opr);
    code_p->y_u.Otapl.p = ap;
#ifdef TABLING
    code_p->y_u.Otapl.te = ap->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, nofalts);
    if (cip->clause_has_cut)
      PUT_YAMOP_CUT(code_p);
    if (ap->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
#endif /* YAPOR */
  }
  GONEXT(Otapl);
  return code_p;
}

static yamop *
#ifdef YAPOR
a_either(op_numbers opcode, COUNT opr, CELL lab, int nofalts, yamop *code_p,
         int pass_no, struct intermediates *cip)
#else
a_either(op_numbers opcode, COUNT opr, CELL lab, yamop *code_p, int pass_no,
         struct intermediates *cip)
#endif /* YAPOR */
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->y_u.Osblp.s = emit_count(opr);
    code_p->y_u.Osblp.l = emit_a(lab);
    code_p->y_u.Osblp.p0 = cip->CurrentPred;
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, nofalts);
    if (cip->clause_has_cut)
      PUT_YAMOP_CUT(code_p);
    if (cip->CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
    if (opcode != _or_last) {
      code_p->y_u.Osblp.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
    }
#else
    code_p->y_u.Osblp.bmap = emit_bmlabel(cip->cpc->rnd3, cip);
#endif /* YAPOR */
  }
  GONEXT(Osblp);
  return code_p;
}

static yamop *a_gl(op_numbers opcode, yamop *code_p, int pass_no,
                   struct PSEUDO *cpc, struct intermediates *cip USES_REGS) {
#ifdef YAPOR
  return a_try(opcode, cpc->rnd1, LOCAL_IPredArity, cpc->rnd2 >> 1,
               cpc->rnd2 & 1, code_p, pass_no, cip);
#else
  return a_try(opcode, cpc->rnd1, LOCAL_IPredArity, code_p, pass_no, cip);
#endif /* YAPOR */
}

/*
 * optimizes several unify_cons for the same constant. It must be avoided for
 * the head of the first argument, because of indexing
 */
static yamop *a_ucons(int *do_not_optimise_uatomp, compiler_vm_op opcode,
                      yamop *code_p, int pass_no, struct intermediates *cip) {
#if AGGREGATE_OPS
  PInstr *np = cip->cpc->nextInst;
  register int i = 0;
  CELL my_cons = cip->cpc->rnd1;

  if (*do_not_optimise_uatomp) {
    *do_not_optimise_uatomp = FALSE;
    if (opcode == unify_atom_op)
      return a_uc(cip->cpc->rnd1, _unify_atom, _unify_atom_write, code_p,
                  pass_no);
    else
      return a_c(cip->cpc->rnd1, _write_atom, code_p, pass_no);
  } else {
    while (np->op == opcode && np->rnd1 == my_cons) {
      i++;
      cip->cpc = np;
      np = np->nextInst;
    }
    if (i == 0) {
      if (opcode == unify_atom_op)
        return a_uc(cip->cpc->rnd1, _unify_atom, _unify_atom_write, code_p,
                    pass_no);
      else
        return a_c(cip->cpc->rnd1, _write_atom, code_p, pass_no);
    } else {
      if (opcode == unify_atom_op)
        return a_unc(cip->cpc->rnd1, _unify_n_atoms, _unify_n_atoms_write,
                     i + 1, code_p, pass_no);
      else
        return a_nc(cip->cpc->rnd1, _write_n_atoms, i + 1, code_p, pass_no);
    }
  }
#else
  *do_not_optimise_uatomp = FALSE;
  if (opcode == unify_atom_op)
    return a_uc(cip->cpc->rnd1, _unify_atom, _unify_atom_write, code_p);
  else
    return a_c(cip->cpc->rnd1, _write_atom, code_p);
#endif
}

static yamop *a_uvar(yamop *code_p, int pass_no, struct intermediates *cip) {
  if (!is_void_var()) {
#if AGGREGATE_OPS
    if (is_temp_var()) {
      PInstr *np = cip->cpc->nextInst;

      if (np->op == unify_var_op && is_atemp_var(np)) {
        return a_vv(_unify_x_var2, _unify_x_var2_write, code_p, pass_no, cip);
      } else if (np->op == unify_last_var_op && is_atemp_var(np)) {
        return a_vv(_unify_l_x_var2, _unify_l_x_var2_write, code_p, pass_no,
                    cip);
      }
    }
#endif /* AGGREGATE_OPS */
    return a_uv((Ventry *)cip->cpc->rnd1, _unify_x_var, _unify_x_var_write,
                _unify_y_var, _unify_y_var_write, code_p, pass_no);
  } else {
#if AGGREGATE_OPS
    int i = 1;
    PInstr *np = cip->cpc->nextInst;

    /* skip void vars */
    while (np->op == unify_var_op && is_a_void(np->rnd1)) {
      i++;
      cip->cpc = np;
      np = np->nextInst;
    }
    if (np->op == unify_last_var_op && is_a_void(np->rnd1)) {
      if (i == 0)
        code_p = a_ue(_unify_l_void, _unify_l_void_write, code_p, pass_no);
      else
        code_p = a_un(_unify_l_n_voids, _unify_l_n_voids_write, i + 1, code_p,
                      pass_no);
      cip->cpc = np;
    } else if (i == 1)
      return a_ue(_unify_void, _unify_void_write, code_p, pass_no);
    else {
      return a_un(_unify_n_voids, _unify_n_voids_write, i, code_p, pass_no);
    }
#else
    return a_ue(_unify_void, _unify_void_write);
#endif
  }
  return code_p;
}

static yamop *a_wvar(yamop *code_p, int pass_no, struct intermediates *cip) {
  if (!no_ref_var())
    return a_v(_write_x_var, _write_y_var, code_p, pass_no, cip->cpc);
  else {
#if AGGREGATE_OPS
    int i = 0;
    PInstr *np = cip->cpc->nextInst;

    while (np->op == write_var_op && no_ref(np->rnd1)) {
      i++;
      cip->cpc = np;
      np = np->nextInst;
    }
    if (i == 0)
      return a_e(_write_void, code_p, pass_no);
    else {
      return a_n(_write_n_voids, i + 1, code_p, pass_no);
    }
#else
    return a_e(_write_void, pass_no);
#endif
  }
}

static yamop *a_glist(int *do_not_optimise_uatomp, yamop *code_p, int pass_no,
                      struct intermediates *cip) {
#if AGGREGATE_OPS
  PInstr *pnext = cip->cpc->nextInst;

  if (cip->cpc->rnd2 != 1 && pnext->op == unify_val_op) {
    Ventry *ve = (Ventry *)pnext->rnd1;
    int is_y_var;
    OPREG var_offset;

    pnext->rnd2 = cip->cpc->rnd2;
    cip->cpc = pnext;
    is_y_var = (ve->KindOfVE == PermVar);
    var_offset = Var_Ref(ve, is_y_var);
    return a_rv(_glist_valx, _glist_valy, var_offset, code_p, pass_no,
                cip->cpc);
  } else if (cip->cpc->rnd2 == 1 && pnext->op == unify_atom_op) {
    *do_not_optimise_uatomp = TRUE;
    return a_r(cip->cpc->rnd2, _get_list, code_p, pass_no);
  } else if (cip->cpc->rnd2 != 1 && pnext->op == unify_var_op &&
             is_a_void(pnext->rnd1)) {
    PInstr *ppnext = pnext->nextInst;

    if (ppnext &&
        (ppnext->op == unify_last_var_op || ppnext->op == unify_last_val_op)) {
      Ventry *ve = (Ventry *)ppnext->rnd1;
      int is_y_var = (ve->KindOfVE == PermVar);
      OPREG var_offset;

      ppnext->rnd2 = cip->cpc->rnd2;
      cip->cpc = ppnext;
      var_offset = Var_Ref(ve, is_y_var);
      if (cip->cpc->op == unify_last_var_op)
        return a_rv(_gl_void_varx, _gl_void_vary, var_offset, code_p, pass_no,
                    cip->cpc);
      else
        return a_rv(_gl_void_valx, _gl_void_valy, var_offset, code_p, pass_no,
                    cip->cpc);
    } else {
      return a_r(cip->cpc->rnd2, _get_list, code_p, pass_no);
    }
  } else
#endif /* AGGREGATE_OPS */
    return a_r(cip->cpc->rnd2, _get_list, code_p, pass_no);
}

#define NEXTOPC (cip->cpc->nextInst->op)

static yamop *a_deallocate(clause_info *clinfo, yamop *code_p, int pass_no,
                           struct intermediates *cip) {
  if (clinfo->alloc_found == 1) {
    if (NEXTOPC == execute_op &&
        !(RepPredProp((Prop)(cip->cpc->nextInst->rnd1))->PredFlags &
          CPredFlag)) {
      cip->cpc = cip->cpc->nextInst;
      code_p = a_p(_dexecute, clinfo, code_p, pass_no, cip);
    } else
      code_p = a_p0(_deallocate, code_p, pass_no, cip->CurrentPred);
    clinfo->dealloc_found = TRUE;
  }
  return code_p;
}

static yamop *a_bmap(yamop *code_p, int pass_no, struct PSEUDO *cpc) {
  /* how much space do we need to reserve */
  int i, max = (cpc->rnd1) / (8 * sizeof(CELL));
  for (i = 0; i <= max; i++)
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}

static yamop *a_bregs(yamop *code_p, int pass_no, struct PSEUDO *cpc) {
  /* how much space do we need to reserve */
  int i, max = (cpc->rnd1) / (8 * sizeof(CELL));
  code_p = fill_a(cpc->rnd1, code_p, pass_no);
  for (i = 0; i <= max; i++)
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}

static yamop *copy_blob(yamop *code_p, int pass_no, struct PSEUDO *cpc) {
  /* copy the blob to code space, making no effort to align if a double */
  int max = cpc->rnd1, i;
  for (i = 0; i < max; i++)
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}

static yamop *copy_string(yamop *code_p, int pass_no, struct PSEUDO *cpc) {
  /* copy the blob to code space, making no effort to align if a double */
  int max = cpc->rnd1, i;
  for (i = 0; i < max; i++)
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}

static void a_fetch_vv(cmp_op_info *cmp_info, int pass_no,
                       struct intermediates *cip) {
  /* the next three instructions must be a get_val, get_val, and BIP */
  if (pass_no == 0) {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *)p->rnd1;
    if (ve->KindOfVE != PermVar && p->op != nop_op && p->op != put_var_op) {
      p->rnd2 = ve->NoOfVE & MaskVarAdrs;
      p->op = nop_op;
    }
    p = p->nextInst;
    ve = (Ventry *)p->rnd1;
    if (ve->KindOfVE != PermVar && p->op != nop_op && p->op != put_var_op) {
      p->rnd2 = ve->NoOfVE & MaskVarAdrs;
      p->op = nop_op;
    }
  } else {
    PInstr *p = cip->cpc->nextInst;

    cmp_info->c_type = TYPE_XX;
    /* don't get rid of get_val_op */
    cmp_info->x1_arg = emit_x(p->rnd2);
    p = p->nextInst;
    cmp_info->x2_arg = emit_x(p->rnd2);
  }
}

static void a_fetch_vc(cmp_op_info *cmp_info, int pass_no,
                       struct intermediates *cip) {
  /* the next two instructions must be a get_val and BIP */
  if (pass_no == 0) {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *)p->rnd1;
    if (ve->KindOfVE != PermVar && p->op != nop_op && p->op != put_var_op) {
      p->rnd2 = ve->NoOfVE & MaskVarAdrs;
      p->op = nop_op;
    }
  } else {
    PInstr *p = cip->cpc->nextInst;

    cmp_info->c_type = TYPE_XC;
    cmp_info->c_arg = cip->cpc->rnd1;
    cmp_info->x1_arg = emit_x(p->rnd2);
  }
}

static void a_fetch_cv(cmp_op_info *cmp_info, int pass_no,
                       struct intermediates *cip) {
  /* the next two instructions must be a get_val and BIP */
  if (pass_no == 0) {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *)p->rnd1;
    if (ve->KindOfVE != PermVar && p->op != nop_op && p->op != put_var_op) {
      p->rnd2 = ve->NoOfVE & MaskVarAdrs;
      p->op = nop_op;
    }
  } else {
    PInstr *p = cip->cpc->nextInst;

    cmp_info->c_type = TYPE_CX;
    cmp_info->c_arg = cip->cpc->rnd1;
    cmp_info->x1_arg = emit_x(p->rnd2);
  }
}

static yamop *a_f2(cmp_op_info *cmp_info, yamop *code_p, int pass_no,
                   struct intermediates *cip) {
  Int opc = cip->cpc->rnd2;
  Ventry *ve = (Ventry *)(cip->cpc->rnd1);
  int is_y_var = FALSE;
  Int xpos = 0;

  if (ve) {
    is_y_var = (ve->KindOfVE == PermVar);
    xpos = ve->NoOfVE & MaskVarAdrs;
  }

  if (opc <= _primitive) {
    if (is_y_var) {
      if (pass_no) {
        code_p->y_u.yl.y = emit_y(ve);
        switch (opc) {
        case _atom:
          code_p->opc = opcode(_p_atom_y);
          break;
        case _atomic:
          code_p->opc = opcode(_p_atomic_y);
          break;
        case _compound:
          code_p->opc = opcode(_p_compound_y);
          break;
        case _float:
          code_p->opc = opcode(_p_float_y);
          break;
        case _integer:
          code_p->opc = opcode(_p_integer_y);
          break;
        case _nonvar:
          code_p->opc = opcode(_p_nonvar_y);
          break;
        case _number:
          code_p->opc = opcode(_p_number_y);
          break;
        case _var:
          code_p->opc = opcode(_p_var_y);
          break;
        case _db_ref:
          code_p->opc = opcode(_p_db_ref_y);
          break;
        case _cut_by:
          Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
                    "internal assembler error: cut_by should be handled as ->");
          break;
        case _primitive:
          code_p->opc = opcode(_p_primitive_y);
          break;
        }
        code_p->y_u.yl.F = emit_fail(cip);
      }
      GONEXT(yl);
      return code_p;
    } else {
      if (pass_no) {
        code_p->y_u.xl.x = emit_x(xpos);
        switch (opc) {
        case _atom:
          code_p->opc = opcode(_p_atom_x);
          break;
        case _atomic:
          code_p->opc = opcode(_p_atomic_x);
          break;
        case _compound:
          code_p->opc = opcode(_p_compound_x);
          break;
        case _float:
          code_p->opc = opcode(_p_float_x);
          break;
        case _integer:
          code_p->opc = opcode(_p_integer_x);
          break;
        case _nonvar:
          code_p->opc = opcode(_p_nonvar_x);
          break;
        case _number:
          code_p->opc = opcode(_p_number_x);
          break;
        case _var:
          code_p->opc = opcode(_p_var_x);
          break;
        case _db_ref:
          code_p->opc = opcode(_p_db_ref_x);
          break;
        case _cut_by:
          Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
                    "internal assembler error: cut_by should be handled as ->");
          break;
        case _primitive:
          code_p->opc = opcode(_p_primitive_x);
          break;
        }
        code_p->y_u.xl.F = emit_fail(cip);
      }
      GONEXT(xl);
      return code_p;
    }
  }
  if (opc == _functor && (cip->cpc->nextInst->op == f_var_op ||
                          cip->cpc->nextInst->op == f_0_op)) {
    Ventry *nve;
    int is_y_nvar = FALSE;
    Int nxpos = 0;

    cip->cpc = cip->cpc->nextInst;
    nve = (Ventry *)(cip->cpc->rnd1);
    if (nve) {
      is_y_nvar = (nve->KindOfVE == PermVar);
      nxpos = nve->NoOfVE & MaskVarAdrs;
    }
    if (is_y_var) {
      if (is_y_nvar) {
        if (pass_no) {
          code_p->opc = emit_op(_p_func2f_yy);
          code_p->y_u.yyx.y1 = emit_y(ve);
          code_p->y_u.yyx.y2 = emit_y(nve);
          code_p->y_u.yyx.x = cmp_info->x1_arg;
        }
        GONEXT(yyx);
        return code_p;
      } else {
        if (pass_no) {
          code_p->opc = emit_op(_p_func2f_yx);
          code_p->y_u.yxx.y = emit_y(ve);
          code_p->y_u.yxx.x1 = emit_x(nxpos);
          code_p->y_u.yxx.x2 = cmp_info->x1_arg;
        }
        GONEXT(yxx);
        return code_p;
      }
    } else {
      if (is_y_nvar) {
        if (pass_no) {
          code_p->opc = emit_op(_p_func2f_xy);
          code_p->y_u.xxy.x1 = emit_x(xpos);
          code_p->y_u.xxy.y2 = emit_y(nve);
          code_p->y_u.xxy.x = cmp_info->x1_arg;
        }
        GONEXT(xxy);
        return code_p;
      } else {
        if (pass_no) {
          code_p->opc = emit_op(_p_func2f_xx);
          code_p->y_u.xxx.x1 = emit_x(xpos);
          code_p->y_u.xxx.x2 = emit_x(nxpos);
          code_p->y_u.xxx.x = cmp_info->x1_arg;
        }
        GONEXT(xxx);
        return code_p;
      }
    }
  }
  if (is_y_var) {
    switch (cmp_info->c_type) {
    case TYPE_XX:
      if (pass_no) {
        switch (opc) {
        case _plus:
          code_p->opc = emit_op(_p_plus_y_vv);
          break;
        case _minus:
          code_p->opc = emit_op(_p_minus_y_vv);
          break;
        case _times:
          code_p->opc = emit_op(_p_times_y_vv);
          break;
        case _div:
          code_p->opc = emit_op(_p_div_y_vv);
          break;
        case _and:
          code_p->opc = emit_op(_p_and_y_vv);
          break;
        case _or:
          code_p->opc = emit_op(_p_or_y_vv);
          break;
        case _sll:
          code_p->opc = emit_op(_p_sll_y_vv);
          break;
        case _slr:
          code_p->opc = emit_op(_p_slr_y_vv);
          break;
        case _arg:
          code_p->opc = emit_op(_p_arg_y_vv);
          break;
        case _functor:
          code_p->opc = emit_op(_p_func2s_y_vv);
          break;
        }
        code_p->y_u.yxx.y = emit_y(ve);
        code_p->y_u.yxx.x1 = cmp_info->x1_arg;
        code_p->y_u.yxx.x2 = cmp_info->x2_arg;
      }
      GONEXT(yxx);
      break;
    case TYPE_CX:
      if (pass_no) {
        switch (opc) {
        case _plus:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for +/2 (should be XC)");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _minus:
          code_p->opc = emit_op(_p_minus_y_cv);
          break;
        case _times:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for */2 (should be XC)");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _div:
          code_p->opc = emit_op(_p_div_y_cv);
          break;
        case _and:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for /\\/2 (should be XC)");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _or:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for \\//2 (should be XC)");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _sll:
          code_p->opc = emit_op(_p_sll_y_cv);
          break;
        case _slr:
          code_p->opc = emit_op(_p_slr_y_cv);
          break;
        case _arg:
          code_p->opc = emit_op(_p_arg_y_cv);
          break;
        case _functor:
          code_p->opc = emit_op(_p_func2s_y_cv);
          break;
        }
        code_p->y_u.yxn.y = emit_y(ve);
        code_p->y_u.yxn.c = cmp_info->c_arg;
        code_p->y_u.yxn.xi = cmp_info->x1_arg;
      }
      GONEXT(yxn);
      break;
    case TYPE_XC:
      if (pass_no) {
        switch (opc) {
        case _plus:
          code_p->opc = emit_op(_p_plus_y_vc);
          break;
        case _minus:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x2_arg,
                    "internal assembler error XC for -/2");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _times:
          code_p->opc = emit_op(_p_times_y_vc);
          break;
        case _div:
          code_p->opc = emit_op(_p_div_y_vc);
          break;
        case _and:
          code_p->opc = emit_op(_p_and_y_vc);
          break;
        case _or:
          code_p->opc = emit_op(_p_or_y_vc);
          break;
        case _sll:
          if ((Int)cmp_info->c_arg < 0) {
            code_p->opc = emit_op(_p_slr_y_vc);
            cmp_info->c_arg = -(Int)cmp_info->c_arg;
          } else {
            code_p->opc = emit_op(_p_sll_y_vc);
          }
          break;
        case _slr:
          if ((Int)cmp_info->c_arg < 0) {
            code_p->opc = emit_op(_p_sll_y_vc);
            cmp_info->c_arg = -(Int)cmp_info->c_arg;
          } else {
            code_p->opc = emit_op(_p_slr_y_vc);
          }
          break;
        case _arg:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x2_arg,
                    "internal assembler error for arg/3");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _functor:
          code_p->opc = emit_op(_p_func2s_y_vc);
          break;
        }
        code_p->y_u.yxn.y = emit_y(ve);
        code_p->y_u.yxn.c = cmp_info->c_arg;
        code_p->y_u.yxn.xi = cmp_info->x1_arg;
      }
      GONEXT(yxn);
      break;
    }
  } else {
    switch (cmp_info->c_type) {
    case TYPE_XX:
      if (pass_no) {
        switch (opc) {
        case _plus:
          code_p->opc = emit_op(_p_plus_vv);
          break;
        case _minus:
          code_p->opc = emit_op(_p_minus_vv);
          break;
        case _times:
          code_p->opc = emit_op(_p_times_vv);
          break;
        case _div:
          code_p->opc = emit_op(_p_div_vv);
          break;
        case _and:
          code_p->opc = emit_op(_p_and_vv);
          break;
        case _or:
          code_p->opc = emit_op(_p_or_vv);
          break;
        case _sll:
          code_p->opc = emit_op(_p_sll_vv);
          break;
        case _slr:
          code_p->opc = emit_op(_p_slr_vv);
          break;
        case _arg:
          code_p->opc = emit_op(_p_arg_vv);
          break;
        case _functor:
          code_p->opc = emit_op(_p_func2s_vv);
          break;
        }
        code_p->y_u.xxx.x = emit_x(xpos);
        code_p->y_u.xxx.x1 = cmp_info->x1_arg;
        code_p->y_u.xxx.x2 = cmp_info->x2_arg;
      }
      GONEXT(xxx);
      break;
    case TYPE_CX:
      if (pass_no) {
        switch (opc) {
        case _plus:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for +/2");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _minus:
          code_p->opc = emit_op(_p_minus_cv);
          break;
        case _times:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for */2");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _div:
          code_p->opc = emit_op(_p_div_cv);
          break;
        case _and:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for /\\/2");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _or:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x1_arg,
                    "internal assembler error CX for \\//2");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _sll:
          code_p->opc = emit_op(_p_sll_cv);
          break;
        case _slr:
          code_p->opc = emit_op(_p_slr_cv);
          break;
        case _arg:
          code_p->opc = emit_op(_p_arg_cv);
          break;
        case _functor:
          code_p->opc = emit_op(_p_func2s_cv);
          break;
        }
        code_p->y_u.xxn.x = emit_x(xpos);
        code_p->y_u.xxn.c = cmp_info->c_arg;
        code_p->y_u.xxn.xi = cmp_info->x1_arg;
      }
      GONEXT(xxn);
      break;
    case TYPE_XC:
      if (pass_no) {
        switch (opc) {
        case _plus:
          code_p->opc = emit_op(_p_plus_vc);
          break;
        case _minus:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x2_arg,
                    "internal assembler error XC for -/2");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _times:
          code_p->opc = emit_op(_p_times_vc);
          break;
        case _div:
          code_p->opc = emit_op(_p_div_vc);
          break;
        case _and:
          code_p->opc = emit_op(_p_and_vc);
          break;
        case _or:
          code_p->opc = emit_op(_p_or_vc);
          break;
        case _sll:
          if ((Int)cmp_info->c_arg < 0) {
            code_p->opc = emit_op(_p_slr_vc);
            cmp_info->c_arg = -(Int)cmp_info->c_arg;
          } else {
            code_p->opc = emit_op(_p_sll_vc);
          }
          break;
        case _slr:
          if ((Int)cmp_info->c_arg < 0) {
            code_p->opc = emit_op(_p_sll_vc);
            cmp_info->c_arg = -(Int)cmp_info->c_arg;
          } else {
            code_p->opc = emit_op(_p_slr_vc);
          }
          break;
        case _arg:
          Yap_Error(SYSTEM_ERROR_COMPILER, cmp_info->x2_arg,
                    "internal assembler error for arg/3");
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 1);
          break;
        case _functor:
          code_p->opc = emit_op(_p_func2s_vc);
          break;
        }
        code_p->y_u.xxn.x = emit_x(xpos);
        code_p->y_u.xxn.c = cmp_info->c_arg;
        code_p->y_u.xxn.xi = cmp_info->x1_arg;
      }
      GONEXT(xxn);
      break;
    }
  }
  return code_p;
}

static yamop *a_special_label(yamop *code_p, int pass_no,
                              struct intermediates *cip) {
  special_label_op lab_op = cip->cpc->rnd1;
  special_label_id lab_id = cip->cpc->rnd2;
  UInt lab_val = cip->cpc->rnd3;

  switch (lab_op) {
  case SPECIAL_LABEL_INIT:
    switch (lab_id) {
    case SPECIAL_LABEL_EXCEPTION:
      cip->exception_handler = lab_val;
      break;
    case SPECIAL_LABEL_SUCCESS:
      cip->success_handler = lab_val;
      break;
    case SPECIAL_LABEL_FAILURE:
      cip->failure_handler = lab_val;
      break;
    }
  case SPECIAL_LABEL_SET:
    break;
  case SPECIAL_LABEL_CLEAR:
    switch (lab_id) {
    case SPECIAL_LABEL_EXCEPTION:
      cip->exception_handler = 0;
      break;
    case SPECIAL_LABEL_SUCCESS:
      cip->success_handler = 0;
      break;
    case SPECIAL_LABEL_FAILURE:
      cip->failure_handler = 0;
      break;
    }
  }
  return code_p;
}

#ifdef YAPOR
#define TRYCODE(G, P)                                                          \
  a_try((G), Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1],     \
        LOCAL_IPredArity, cip->cpc->rnd2 >> 1, cip->cpc->rnd2 & 1, code_p,     \
        pass_no, cip)
#define TABLE_TRYCODE(G)                                                       \
  a_try((G), (CELL)emit_ilabel(cip->cpc->rnd1, cip), LOCAL_IPredArity,         \
        cip->cpc->rnd2 >> 1, cip->cpc->rnd2 & 1, code_p, pass_no, cip)
#else
#define TRYCODE(G, P)                                                          \
  a_try((G), Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1],     \
        LOCAL_IPredArity, code_p, pass_no, cip)
#define TABLE_TRYCODE(G)                                                       \
  a_try((G), (CELL)emit_ilabel(cip->cpc->rnd1, cip), LOCAL_IPredArity, code_p, \
        pass_no, cip)
#endif /* YAPOR */

static yamop *do_pass(int pass_no, yamop **entry_codep, int assembling,
                      int *clause_has_blobsp, int *clause_has_dbtermp,
                      struct intermediates *cip, UInt size USES_REGS) {
#ifdef YAPOR
#define MAX_DISJ_BRANCHES 256
  yamop *either_inst[MAX_DISJ_BRANCHES];
  int either_cont = 0;
#endif /* YAPOR */
  bool log_update;
  bool dynamic;
  bool tabled;
  int ystop_found = FALSE;
  union clause_obj *cl_u;
  yamop *code_p;
  cmp_op_info cmp_info;
  clause_info clinfo;
  int do_not_optimise_uatom;

  code_p = cip->code_addr;
  cl_u = (union clause_obj *)code_p;
  cip->cpc = cip->CodeStart;
  clinfo.alloc_found = 0;
  clinfo.dealloc_found = FALSE;
  clinfo.CurrentPred = cip->CurrentPred;
  cip->current_try_lab = NULL;
  cip->exception_handler = 0;
  cip->success_handler = 0;
  cip->failure_handler = 0;
  cip->try_instructions = NULL;
  cmp_info.c_type = TYPE_XX;
  cmp_info.cl_info = &clinfo;
  do_not_optimise_uatom = FALSE;

  /* Space while for the clause flags */
  log_update = cip->CurrentPred->PredFlags & LogUpdatePredFlag;
  dynamic = cip->CurrentPred->PredFlags & DynamicPredFlag;
  tabled = cip->CurrentPred->PredFlags & TabledPredFlag;
  if (assembling == ASSEMBLING_CLAUSE) {
    if (log_update) {
      if (pass_no) {
        cl_u->luc.Id = FunctorDBRef;
        cl_u->luc.ClFlags = LogUpdMask;
        if (cip->clause_has_cut)
          cl_u->luc.ClFlags |= HasCutMask;
        cl_u->luc.ClRefCount = 0;
        cl_u->luc.ClPred = cip->CurrentPred;
        /* Support for timestamps */
        if (cip->CurrentPred->LastCallOfPred != LUCALL_ASSERT) {
          if (cip->CurrentPred->TimeStampOfPred >= TIMESTAMP_RESET)
            Yap_UpdateTimestamps(cip->CurrentPred);
          ++cip->CurrentPred->TimeStampOfPred;
          /* fprintf(stderr,"+
           * %x--%d--%ul\n",cip->CurrentPred,cip->CurrentPred->TimeStampOfPred,cip->CurrentPred->ArityOfPE);*/
          cip->CurrentPred->LastCallOfPred = LUCALL_ASSERT;
        }
        cl_u->luc.ClTimeStart = cip->CurrentPred->TimeStampOfPred;
        cl_u->luc.ClTimeEnd = TIMESTAMP_EOT;
        if (*clause_has_blobsp) {
          cl_u->luc.ClFlags |= HasBlobsMask;
        }
        if (*clause_has_dbtermp) {
          cl_u->luc.ClFlags |= HasDBTMask;
        }
        cl_u->luc.ClExt = NULL;
        cl_u->luc.ClPrev = cl_u->luc.ClNext = NULL;
#if MULTIPLE_STACKS
        // INIT_LOCK(cl_u->luc.ClLock);
        INIT_CLREF_COUNT(&(cl_u->luc));
#endif
      }
      code_p = cl_u->luc.ClCode;
    } else if (dynamic) {
      if (pass_no) {
        cl_u->ic.ClFlags = DynamicMask;
        if (*clause_has_blobsp) {
          cl_u->ic.ClFlags |= HasBlobsMask;
        }
        if (*clause_has_dbtermp) {
          cl_u->ic.ClFlags |= HasDBTMask;
        }
        cl_u->ic.ClSize = size;
        cl_u->ic.ClRefCount = 0;
#if defined(YAPOR) || defined(THREADS)
        INIT_LOCK(cl_u->ic.ClLock);
#endif
#ifdef MULTIPLE_STACKS
        INIT_CLREF_COUNT(&(cl_u->ic));
#endif
      }
      code_p = cl_u->ic.ClCode;
    } else {
      /* static clause */
      if (pass_no) {
        cl_u->sc.ClFlags = StaticMask;
        if (cip->clause_has_cut)
          cl_u->sc.ClFlags |= HasCutMask;
        cl_u->sc.ClNext = NULL;
        cl_u->sc.ClSize = size;
        cl_u->sc.usc.ClLine = Yap_source_line_no();
        if (*clause_has_blobsp) {
          cl_u->sc.ClFlags |= HasBlobsMask;
        }
        if (*clause_has_dbtermp) {
          cl_u->sc.ClFlags |= HasDBTMask;
        }
      }
      code_p = cl_u->sc.ClCode;
    }
    LOCAL_IPredArity = cip->CurrentPred->ArityOfPE; /* number of args */
    *entry_codep = code_p;
    if (tabled) {
#if TABLING
#ifdef YAPOR
      code_p = a_try(_table_try_single, (CELL)NEXTOP(code_p, Otapl),
                     LOCAL_IPredArity, 1, 0, code_p, pass_no, cip);
#else
      code_p = a_try(_table_try_single, (CELL)NEXTOP(code_p, Otapl),
                     LOCAL_IPredArity, code_p, pass_no, cip);
#endif
#endif
    }
    if (dynamic) {
#ifdef YAPOR
      code_p = a_try(_try_me, 0, LOCAL_IPredArity, 1, 0, code_p, pass_no, cip);
#else
      code_p = a_try(_try_me, 0, LOCAL_IPredArity, code_p, pass_no, cip);
#endif /* YAPOR */
    }
#if THREADS || YAPOR
    if (log_update) {
      // separate from indexing code,
      // clauses are protected by time-stamps
      code_p = a_e(_unlock_lu, code_p, pass_no);
    }
#endif
  } else {
    /* index code */
    if (log_update) {
      if (pass_no) {
        cl_u->lui.ClFlags =
            LogUpdMask | IndexedPredFlag | IndexMask | SwitchRootMask;
        cl_u->lui.ChildIndex = NULL;
        cl_u->lui.SiblingIndex = NULL;
        cl_u->lui.PrevSiblingIndex = NULL;
        cl_u->lui.ClPred = cip->CurrentPred;
        cl_u->lui.ParentIndex = NULL;
        cl_u->lui.ClSize = size;
        cl_u->lui.ClRefCount = 0;
//	INIT_LOCK(cl_u->lui.ClLock);
#if MULTIPLE_STACKS
        INIT_CLREF_COUNT(&(cl_u->lui));
#endif
      }
      code_p = cl_u->lui.ClCode;
      *entry_codep = code_p;
    } else {
      if (pass_no) {
        cl_u->si.ClSize = size;
        cl_u->si.ClFlags = IndexMask;
        cl_u->si.ChildIndex = NULL;
        cl_u->si.SiblingIndex = NULL;
        cl_u->si.ClPred = cip->CurrentPred;
      }
      code_p = cl_u->si.ClCode;
      *entry_codep = code_p;
    }
  }
  while (cip->cpc) {
    switch ((int)cip->cpc->op) {
#ifdef YAPOR
    case sync_op:
      code_p = a_try(_sync, cip->cpc->rnd1, cip->cpc->rnd2, 1, Zero, code_p,
                     pass_no, cip);
      break;
#endif /* YAPOR */
#ifdef TABLING
    case table_new_answer_op:
      code_p = a_n(_table_new_answer, (int)cip->cpc->rnd2, code_p, pass_no);
      break;
    case table_try_single_op:
      code_p =
          a_gl(_table_try_single, code_p, pass_no, cip->cpc, cip PASS_REGS);
      break;
#endif /* TABLING */
#ifdef TABLING_INNER_CUTS
    case clause_with_cut_op:
      code_p = a_e(_clause_with_cut, code_p, pass_no);
      break;
#endif /* TABLING_INNER_CUTS */
#ifdef SFUNC
    case get_s_f_op:
      code_p = a_rf(_get_s_f, code_p, cip->cpc);
      break;
    case put_s_f_op:
      code_p = a_rf(_put_s_f, code_p, cip->cpc);
      break;
    case unify_s_f_op:
      code_p = a_d(_unify_s_f, code_p);
      break;
    case write_s_f_op:
      code_p = a_f(cip->cpc->rnd1, _write_s_f);
      break;
    case unify_s_var_op:
      code_p = a_vsf(_unify_s_xvar);
      break;
    case write_s_var_op:
      code_p = a_vsf(_write_s_xvar);
      break;
    case unify_s_val_op:
      code_p = a_vsf(_unify_s_xval);
      break;
    case write_s_val_op:
      code_p = a_vsf(_write_s_xval);
      break;
    case unify_s_a_op:
      code_p = a_asf(_unify_s_a);
      break;
    case write_s_a_op:
      code_p = a_asf(_write_s_a);
      break;
    case get_s_end_op:
      code_p = a_n(_get_s_end, Unsigned(0));
      break;
    case put_s_end_op:
      code_p = a_n(_put_s_end, Unsigned(0));
      break;
    case unify_s_end_op:
      code_p = a_n(_write_s_end, Unsigned(0));
      break;
    case write_s_end_op:
      code_p = a_n(_write_s_end, Unsigned(0));
      break;
#endif
    case get_var_op:
      code_p = a_vr(_get_x_var, _get_y_var, code_p, pass_no, cip);
      break;
    case put_var_op:
      code_p = a_vr(_put_x_var, _put_y_var, code_p, pass_no, cip);
      break;
    case get_val_op:
      code_p = a_vr(_get_x_val, _get_y_val, code_p, pass_no, cip);
      break;
    case put_val_op:
      code_p = a_vr(_put_x_val, _put_y_val, code_p, pass_no, cip);
      break;
    case get_num_op:
    case get_atom_op:
      code_p = a_rc(_get_atom, code_p, pass_no, cip);
      break;
    case get_float_op:
      *clause_has_blobsp = TRUE;
      code_p = a_rd(_get_float, code_p, pass_no, cip->cpc);
      break;
    case label_ctl_op:
      code_p = a_special_label(code_p, pass_no, cip);
      break;
    case get_longint_op:
      *clause_has_blobsp = TRUE;
      code_p = a_ri(_get_longint, code_p, pass_no, cip->cpc);
      break;
    case get_bigint_op:
      code_p = a_rb(_get_bigint, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case get_string_op:
      code_p = a_rstring(_get_string, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case get_dbterm_op:
      code_p = a_dbt(_get_dbterm, clause_has_dbtermp, code_p, pass_no, cip);
      break;
    case put_num_op:
    case put_atom_op:
      code_p = a_rc(_put_atom, code_p, pass_no, cip);
      break;
    case put_float_op:
      *clause_has_blobsp = TRUE;
      code_p = a_rd(_put_float, code_p, pass_no, cip->cpc);
      break;
    case put_longint_op:
      *clause_has_blobsp = TRUE;
      code_p = a_ri(_put_longint, code_p, pass_no, cip->cpc);
      break;
    case put_bigint_op:
      code_p = a_rb(_put_bigint, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case put_string_op:
      code_p = a_rstring(_put_bigint, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case put_dbterm_op:
      code_p = a_dbt(_put_dbterm, clause_has_dbtermp, code_p, pass_no, cip);
      break;
    case get_list_op:
      code_p = a_glist(&do_not_optimise_uatom, code_p, pass_no, cip);
      break;
    case put_list_op:
      code_p = a_r(cip->cpc->rnd2, _put_list, code_p, pass_no);
      break;
    case get_struct_op:
      code_p = a_rf(_get_struct, code_p, pass_no, cip->cpc);
      break;
    case put_struct_op:
      code_p = a_rf(_put_struct, code_p, pass_no, cip->cpc);
      break;
    case put_unsafe_op:
      code_p = a_vr(_put_unsafe, _put_unsafe, code_p, pass_no, cip);
      break;
    case unify_var_op:
      code_p = a_uvar(code_p, pass_no, cip);
      break;
    case unify_last_var_op:
      code_p =
          a_uv((Ventry *)cip->cpc->rnd1, _unify_l_x_var, _unify_l_x_var_write,
               _unify_l_y_var, _unify_l_y_var_write, code_p, pass_no);
      break;
    case write_var_op:
      code_p = a_wvar(code_p, pass_no, cip);
      break;
    case unify_local_op:
      code_p = a_uv((Ventry *)cip->cpc->rnd1, _unify_x_loc, _unify_x_loc_write,
                    _unify_y_loc, _unify_y_loc_write, code_p, pass_no);
      break;
    case unify_val_op:
      code_p = a_uv((Ventry *)cip->cpc->rnd1, _unify_x_val, _unify_x_val_write,
                    _unify_y_val, _unify_y_val_write, code_p, pass_no);
      break;
    case unify_last_local_op:
      code_p =
          a_uv((Ventry *)cip->cpc->rnd1, _unify_l_x_loc, _unify_l_x_loc_write,
               _unify_l_y_loc, _unify_l_y_loc_write, code_p, pass_no);
      break;
    case unify_last_val_op:
      code_p =
          a_uv((Ventry *)cip->cpc->rnd1, _unify_l_x_val, _unify_l_x_val_write,
               _unify_l_y_val, _unify_l_y_val_write, code_p, pass_no);
      break;
    case write_local_op:
      code_p = a_v(_write_x_loc, _write_y_loc, code_p, pass_no, cip->cpc);
      break;
    case write_val_op:
      code_p = a_v(_write_x_val, _write_y_val, code_p, pass_no, cip->cpc);
      break;
    case unify_num_op:
    case unify_atom_op:
      code_p =
          a_ucons(&do_not_optimise_uatom, unify_atom_op, code_p, pass_no, cip);
      break;
    case unify_float_op:
      *clause_has_blobsp = TRUE;
      code_p =
          a_ud(_unify_float, _unify_float_write, code_p, pass_no, cip->cpc);
      break;
    case unify_longint_op:
      *clause_has_blobsp = TRUE;
      code_p =
          a_ui(_unify_longint, _unify_longint_write, code_p, pass_no, cip->cpc);
      break;
    case unify_bigint_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_bigint, _unify_atom_write,
                       clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_string_op:
      code_p = a_ustring(cip->cpc->rnd1, _unify_string, _unify_atom_write,
                         clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_dbterm_op:
      code_p = a_udbt(cip->cpc->rnd1, _unify_dbterm, _unify_atom_write,
                      clause_has_dbtermp, code_p, pass_no, cip);
      break;
    case unify_last_num_op:
    case unify_last_atom_op:
      code_p = a_uc(cip->cpc->rnd1, _unify_l_atom, _unify_l_atom_write, code_p,
                    pass_no);
      break;
    case unify_last_float_op:
      *clause_has_blobsp = TRUE;
      code_p =
          a_ud(_unify_l_float, _unify_l_float_write, code_p, pass_no, cip->cpc);
      break;
    case unify_last_longint_op:
      *clause_has_blobsp = TRUE;
      code_p = a_ui(_unify_l_longint, _unify_l_longint_write, code_p, pass_no,
                    cip->cpc);
      break;
    case unify_last_bigint_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_l_bigint, _unify_l_atom_write,
                       clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_last_string_op:
      code_p = a_ustring(cip->cpc->rnd1, _unify_l_bigint, _unify_l_atom_write,
                         clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_last_dbterm_op:
      code_p = a_udbt(cip->cpc->rnd1, _unify_l_dbterm, _unify_l_atom_write,
                      clause_has_dbtermp, code_p, pass_no, cip);
      break;
    case write_num_op:
    case write_atom_op:
      code_p =
          a_ucons(&do_not_optimise_uatom, write_atom_op, code_p, pass_no, cip);
      break;
    case write_float_op:
      *clause_has_blobsp = TRUE;
      code_p = a_wd(_write_float, code_p, pass_no, cip->cpc);
      break;
    case write_longint_op:
      *clause_has_blobsp = TRUE;
      code_p = a_wi(_write_longint, code_p, pass_no, cip->cpc);
      break;
    case write_bigint_op:
      code_p = a_wblob(cip->cpc->rnd1, _write_bigint, clause_has_blobsp, code_p,
                       pass_no, cip);
      break;
    case write_string_op:
      code_p = a_wblob(cip->cpc->rnd1, _write_bigint, clause_has_blobsp, code_p,
                       pass_no, cip);
      break;
    case write_dbterm_op:
      code_p = a_wdbt(cip->cpc->rnd1, _write_dbterm, clause_has_dbtermp, code_p,
                      pass_no, cip);
      break;
    case unify_list_op:
      code_p = a_ue(_unify_list, _unify_list_write, code_p, pass_no);
      break;
    case unify_last_list_op:
      code_p = a_ue(_unify_l_list, _unify_l_list_write, code_p, pass_no);
      break;
    case write_list_op:
      code_p = a_e(_write_list, code_p, pass_no);
      break;
    case write_last_list_op:
      code_p = a_e(_write_l_list, code_p, pass_no);
      break;
    case unify_struct_op:
      code_p = a_uf(cip->cpc->rnd1, _unify_struct, _unify_struct_write, code_p,
                    pass_no);
      break;
    case unify_last_struct_op:
      code_p = a_uf(cip->cpc->rnd1, _unify_l_struc, _unify_l_struc_write,
                    code_p, pass_no);
      break;
    case write_struct_op:
      code_p = a_f(cip->cpc->rnd1, _write_struct, code_p, pass_no);
      break;
    case write_last_struct_op:
      code_p = a_f(cip->cpc->rnd1, _write_l_struc, code_p, pass_no);
      break;
    case save_b_op:
    case patch_b_op:
      code_p = a_v(_save_b_x, _save_b_y, code_p, pass_no, cip->cpc);
      break;
    case commit_b_op:
      cip->clause_has_cut = TRUE;
      code_p =
          a_vp(_commit_b_x, _commit_b_y, code_p, pass_no, cip->cpc, &clinfo);
      break;
    case save_pair_op:
      code_p = a_uv((Ventry *)cip->cpc->rnd1, _save_pair_x, _save_pair_x_write,
                    _save_pair_y, _save_pair_y_write, code_p, pass_no);
      break;
    case save_appl_op:
      code_p = a_uv((Ventry *)cip->cpc->rnd1, _save_appl_x, _save_appl_x_write,
                    _save_appl_y, _save_appl_y_write, code_p, pass_no);
      break;
    case fail_op:
      code_p = a_e(_op_fail, code_p, pass_no);
      code_p = a_pl(_procceed, cip->CurrentPred, code_p, pass_no);
      break;
    case cut_op:
      code_p = a_cut(&clinfo, code_p, pass_no, cip);
      break;
    case cutexit_op:
      cip->clause_has_cut = TRUE;
      if (cip->CurrentPred->PredFlags & LogUpdatePredFlag &&
          (*clause_has_blobsp || *clause_has_dbtermp) && !clinfo.alloc_found)
        code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
      code_p = a_cut(&clinfo, code_p, pass_no, cip);
      break;
    case allocate_op:
      clinfo.alloc_found = 2;
      break;
    case deallocate_op:
      code_p = a_deallocate(&clinfo, code_p, pass_no, cip);
      break;
    case tryme_op:
#ifdef TABLING
      if (tabled)
        code_p = TABLE_TRYCODE(_table_try_me);
      else
#endif
        code_p = TRYCODE(_try_me, _try_me0);
      break;
    case retryme_op:
#ifdef TABLING
      if (tabled)
        code_p = TABLE_TRYCODE(_table_retry_me);
      else
#endif
        code_p = TRYCODE(_retry_me, _retry_me0);
      break;
    case trustme_op:
#ifdef TABLING
      if (tabled)
        code_p = TABLE_TRYCODE(_table_trust_me);
      else
#endif
        code_p = TRYCODE(_trust_me, _trust_me0);
      break;
    case enter_lu_op:
      code_p = a_lucl(_enter_lu_pred, code_p, pass_no, cip, &clinfo);
      break;
    case try_op:
      if (log_update) {
        add_clref(cip->cpc->rnd1, pass_no);
      }
#ifdef TABLING
      if (tabled)
        code_p = a_gl(_table_try, code_p, pass_no, cip->cpc, cip PASS_REGS);
      else
#endif
        code_p = a_gl(_try_clause, code_p, pass_no, cip->cpc, cip PASS_REGS);
      break;
    case retry_op:
      if (log_update) {
        add_clref(cip->cpc->rnd1, pass_no);
      }
#ifdef TABLING
      if (tabled)
        code_p = a_gl(_table_retry, code_p, pass_no, cip->cpc, cip PASS_REGS);
      else
#endif
        code_p = a_gl(_retry, code_p, pass_no, cip->cpc, cip PASS_REGS);
      break;
    case trust_op:
      if (log_update) {
        add_clref(cip->cpc->rnd1, pass_no);
      }
#ifdef TABLING
      if (tabled)
        code_p = a_gl(_table_trust, code_p, pass_no, cip->cpc, cip PASS_REGS);
      else
#endif
        code_p = a_gl(_trust, code_p, pass_no, cip->cpc, cip PASS_REGS);
      break;
    case try_in_op:
      code_p = a_il(cip->cpc->rnd1, _try_in, code_p, pass_no, cip);
      break;
    case jump_op:
      /* don't assemble jumps to next instruction */
      if (cip->cpc->nextInst == NULL || cip->cpc->nextInst->op != label_op ||
          cip->cpc->rnd1 != cip->cpc->nextInst->rnd1) {
        code_p = a_l(cip->cpc->rnd1, _jump, code_p, pass_no, cip);
      }
      break;
    case jumpi_op:
      code_p = a_il(cip->cpc->rnd1, _jump, code_p, pass_no, cip);
      break;
    case restore_tmps_op:
      code_p = a_l(cip->cpc->rnd1, _move_back, code_p, pass_no, cip);
      break;
    case restore_tmps_and_skip_op:
      code_p = a_l(cip->cpc->rnd1, _skip, code_p, pass_no, cip);
      break;
    case procceed_op:
      if (cip->CurrentPred->PredFlags & LogUpdatePredFlag &&
          (*clause_has_blobsp || *clause_has_dbtermp) && !clinfo.alloc_found)
        code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
      code_p = a_pl(_procceed, cip->CurrentPred, code_p, pass_no);
      break;
    case call_op:
      code_p = a_p(_call, &clinfo, code_p, pass_no, cip);
      break;
    case execute_op:
      code_p = a_p(_execute, &clinfo, code_p, pass_no, cip);
      break;
    case safe_call_op:
      code_p = a_p(_call, &clinfo, code_p, pass_no, cip);
      break;
    case label_op:
      if (!ystop_found && cip->cpc->nextInst != NULL &&
          (cip->cpc->nextInst->op == mark_initialized_pvars_op ||
           cip->cpc->nextInst->op == mark_live_regs_op ||
           cip->cpc->nextInst->op == blob_op ||
           cip->cpc->nextInst->op == string_op)) {
        ystop_found = TRUE;
        code_p = a_il((CELL)*entry_codep, _Ystop, code_p, pass_no, cip);
      }
      if (!pass_no) {
#if !USE_SYSTEM_MALLOC
        if (CellPtr(cip->label_offset + cip->cpc->rnd1) > ASP - 256) {
          LOCAL_Error_Size =
              256 + ((char *)(cip->label_offset + cip->cpc->rnd1) - (char *)HR);
          save_machine_regs();
          siglongjmp(cip->CompilerBotch, 3);
        }
        if ((char *)(cip->label_offset + cip->cpc->rnd1) >= cip->freep)
          cip->freep = (char *)(cip->label_offset + (cip->cpc->rnd1 + 1));
#endif

        cip->label_offset[cip->cpc->rnd1] = (CELL)code_p;
      }
      /* reset dealloc_found in case there was a branch */
      clinfo.dealloc_found = FALSE;
      break;
    case ensure_space_op:
      code_p = a_ensure_space(_ensure_space, code_p, pass_no, cip, &clinfo);
      break;
    case pop_op:
      if (cip->cpc->rnd1 == 1)
        code_p = a_e(_pop, code_p, pass_no);
      else {
        code_p =
            a_n(_pop_n, 2 * CELLSIZE * (cip->cpc->rnd1 - 1), code_p, pass_no);
      }
      break;
    case either_op:
      code_p = check_alloc(&clinfo, code_p, pass_no, cip);
#ifdef YAPOR
      if (pass_no)
        either_inst[either_cont++] = code_p;
      if (either_cont == MAX_DISJ_BRANCHES) {
        Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                  "Too Many Branches in  disjunction: please increase "
                  "MAX_DISJ_BRANCHES in amasm.c\n");
        exit(1);
      }
      code_p =
          a_either(_either, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
                   Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1],
                   0, code_p, pass_no, cip);
#else
      code_p =
          a_either(_either, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
                   Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1],
                   code_p, pass_no, cip);
#endif /* YAPOR */
      break;
    case orelse_op:
#ifdef YAPOR
      if (pass_no)
        either_inst[either_cont++] = code_p;
      code_p =
          a_either(_or_else, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
                   Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1],
                   0, code_p, pass_no, cip);
#else
      code_p =
          a_either(_or_else, -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
                   Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1],
                   code_p, pass_no, cip);
#endif /* YAPOR */
      clinfo.dealloc_found = FALSE;
      break;
    case orlast_op:
#ifdef YAPOR
      if (pass_no)
        either_inst[either_cont++] = code_p;
      code_p = a_either(_or_last, 0, 0, 0, code_p, pass_no, cip);
      if (pass_no) {
        int cont = 1;
        do {
          either_cont--;
          PUT_YAMOP_LTT(either_inst[either_cont], cont++);
        } while (either_inst[either_cont]->opc != opcode(_either));
      }
#else
      code_p = a_pl(_or_last, cip->CurrentPred, code_p, pass_no);
#endif /* YAPOR */
      clinfo.dealloc_found = FALSE;
      break;
    case cache_arg_op:
      code_p = a_4sw_x(_switch_on_arg_type, code_p, pass_no, cip);
      break;
    case cache_sub_arg_op:
      code_p = a_4sw_s(_switch_on_sub_arg_type, code_p, pass_no, cip);
      break;
    case jump_v_op:
      code_p = a_igl(cip->cpc->rnd1, _jump_if_var, code_p, pass_no, cip);
      break;
    case jump_nv_op:
      code_p = a_xigl(_jump_if_nonvar, code_p, pass_no, cip->cpc);
      break;
    case user_switch_op:
      code_p = a_lp(_user_switch, code_p, pass_no, cip);
      break;
    case switch_on_type_op:
      code_p = a_4sw(_switch_on_type, code_p, pass_no, cip);
      break;
    case switch_c_op:
      code_p = a_hx(_switch_on_cons, cl_u, log_update, code_p, pass_no, cip);
      break;
    case switch_f_op:
      code_p = a_hx(_switch_on_func, cl_u, log_update, code_p, pass_no, cip);
      break;
    case if_c_op:
      if (cip->cpc->rnd1 == 1) {
        code_p = a_if(_go_on_cons, cl_u, log_update, code_p, pass_no, cip);
      } else {
        code_p = a_if(_if_cons, cl_u, log_update, code_p, pass_no, cip);
      }
      break;
    case if_f_op:
      if (cip->cpc->rnd1 == 1) {
        code_p = a_if(_go_on_func, cl_u, log_update, code_p, pass_no, cip);
      } else {
        code_p = a_if(_if_func, cl_u, log_update, code_p, pass_no, cip);
      }
      break;
    case if_not_op:
      code_p = a_ifnot(_if_not_then, code_p, pass_no, cip);
      break;
    case index_dbref_op:
      code_p = a_e(_index_dbref, code_p, pass_no);
      break;
    case index_blob_op:
      code_p = a_e(_index_blob, code_p, pass_no);
      break;
    case index_long_op:
      code_p = a_e(_index_long, code_p, pass_no);
      break;
    case mark_initialized_pvars_op:
      if (!ystop_found) {
        code_p = a_il((CELL)*entry_codep, _Ystop, code_p, pass_no, cip);
        ystop_found = TRUE;
      }
      code_p = a_bmap(code_p, pass_no, cip->cpc);
      break;
    case mark_live_regs_op:
      if (!ystop_found) {
        code_p = a_il((CELL)*entry_codep, _Ystop, code_p, pass_no, cip);
        ystop_found = TRUE;
      }
      code_p = a_bregs(code_p, pass_no, cip->cpc);
      break;
    case fetch_args_vv_op:
      a_fetch_vv(&cmp_info, pass_no, cip);
      break;
    case fetch_args_vc_op:
    case fetch_args_vi_op:
      a_fetch_vc(&cmp_info, pass_no, cip);
      break;
    case fetch_args_cv_op:
    case fetch_args_iv_op:
      a_fetch_cv(&cmp_info, pass_no, cip);
      break;
    case f_val_op:
      code_p = a_f2(&cmp_info, code_p, pass_no, cip);
      break;
    case f_var_op:
      code_p = a_f2(&cmp_info, code_p, pass_no, cip);
      break;
    case f_0_op:
      code_p = a_f2(&cmp_info, code_p, pass_no, cip);
      break;
    case enter_profiling_op: {
      PredEntry *pe = (PredEntry *)(cip->cpc->rnd1);
      if ((pe->PredFlags & (CPredFlag | UserCPredFlag | AsmPredFlag)) ||
          !pe->ModuleOfPred) {
        code_p = a_pl(_enter_profiling, pe, code_p, pass_no);
        Yap_initProfiler(pe);
      }
    } break;
    case retry_profiled_op:
      if (!Yap_initProfiler(cip->CurrentPred)) {
        return NULL;
      }
      code_p =
          a_pl(_retry_profiled, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      break;
    case count_call_op: {
      PredEntry *pe = (PredEntry *)(cip->cpc->rnd1);
      if ((pe->PredFlags & (CPredFlag | UserCPredFlag | AsmPredFlag)) ||
          !pe->ModuleOfPred) {
        code_p =
            a_pl(_count_call, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      }
    } break;
    case count_retry_op:
      code_p =
          a_pl(_count_retry, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      break;
    case bccall_op:
      code_p =
          a_bfunc(cip->cpc->rnd1, cip->cpc->rnd3, (PredEntry *)(cip->cpc->rnd5),
                  &clinfo, code_p, pass_no, cip);
      break;
    case align_float_op:
/* install a blob */
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      if (!((CELL)code_p & 0x4))
        GONEXT(e);
#endif
      break;
    case blob_op:
      /* install a blob */
      code_p = copy_blob(code_p, pass_no, cip->cpc);
      break;
    case string_op:
      /* install a blob */
      code_p = copy_string(code_p, pass_no, cip->cpc);
      break;
    case empty_call_op:
      /* create an empty call */
      code_p = a_empty_call(&clinfo, code_p, pass_no, cip);
      break;
    case push_or_op:
      /* be sure to allocate if we have an ;, even if it is
         compiled inline.
       */
      code_p = check_alloc(&clinfo, code_p, pass_no, cip);
    case pushpop_or_op:
    case pop_or_op:
    case nop_op:
    case name_op:
      break;
#ifdef BEAM
    case body_op:
    case endgoal_op:
      break;
    case run_op:
      code_p = a_eam(_run_eam, cip->cpc->rnd2,
                     (long)((PredEntry *)cip->cpc->rnd2)->beamTable->last,
                     code_p, pass_no);
      break;
#endif
    default:
      Yap_Error(SYSTEM_ERROR_COMPILER, TermNil,
                "instruction %d found while assembling", (int)cip->cpc->op);
      save_machine_regs();
      siglongjmp(cip->CompilerBotch, 1);
    }
    cip->cpc = cip->cpc->nextInst;
  }
  if (!ystop_found)
    code_p = a_il((CELL)*entry_codep, _Ystop, code_p, pass_no, cip);
  return code_p;
}

static DBTerm *fetch_clause_space(Term *tp, UInt size,
                                  struct intermediates *cip,
                                  UInt *osizep USES_REGS) {
  CELL *h0 = HR;
  DBTerm *x;

  /* This stuff should be just about fetching the space from the data-base,
     unfortunately we have to do all sorts of error handling :-( */
  HR = (CELL *)cip->freep;
  while ((x = Yap_StoreTermInDBPlusExtraSpace(*tp, size, osizep)) == NULL) {

    HR = h0;
    switch (LOCAL_Error_TYPE) {
    case RESOURCE_ERROR_STACK:
      LOCAL_Error_Size = 256 + ((char *)cip->freep - (char *)HR);
      save_machine_regs();
      siglongjmp(cip->CompilerBotch, 3);
    case RESOURCE_ERROR_TRAIL:
      /* don't just return NULL */
      ARG1 = *tp;
      if (!Yap_growtrail(K64, FALSE)) {
        return NULL;
      }
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      *tp = ARG1;
      break;
    case RESOURCE_ERROR_AUXILIARY_STACK:
      ARG1 = *tp;
      if (!Yap_ExpandPreAllocCodeSpace(LOCAL_Error_Size, (void *)cip, TRUE)) {
        return NULL;
      }
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      *tp = ARG1;
      break;
    case RESOURCE_ERROR_HEAP:
      /* don't just return NULL */
      ARG1 = *tp;
      if (!Yap_growheap(TRUE, size, cip)) {
        return NULL;
      }
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      *tp = ARG1;
      break;
    default:
      return NULL;
    }
    h0 = HR;
    HR = (CELL *)cip->freep;
  }
  HR = h0;
  return x;
}

static DBTermList *init_dbterms_list(yamop *code_p, PredEntry *ap) {
  DBTermList *new;
  if ((new = (DBTermList *)Yap_AllocCodeSpace(sizeof(DBTermList))) == NULL) {
    return NULL;
  }
  new->dbterms = NULL;
  new->clause_code = code_p;
  new->p = ap;
  LOCK(DBTermsListLock);
  new->next_dbl = DBTermsList;
  DBTermsList = new;
  UNLOCK(DBTermsListLock);
  return new;
}

#define DEFAULT_NLABELS 4096

yamop *Yap_assemble(int mode, Term t, PredEntry *ap, int is_fact,
                    struct intermediates *cip, UInt max_label) {
  CACHE_REGS
  /*
   * the assembly proccess is done in two passes: 1 - a first pass
   * computes labels offsets and total code size 2 - the second pass
   * produces the final version of the code
   */
  UInt size = 0;
  yamop *entry_code;
  yamop *code_p;
  int clause_has_blobs = FALSE;
  int clause_has_dbterm = FALSE;

#if USE_SYSTEM_MALLOC
  if (!cip->label_offset) {
    if (!LOCAL_LabelFirstArray && max_label <= DEFAULT_NLABELS) {
      LOCAL_LabelFirstArray =
          (Int *)Yap_AllocCodeSpace(sizeof(Int) * DEFAULT_NLABELS);
      LOCAL_LabelFirstArraySz = DEFAULT_NLABELS;
      if (!LOCAL_LabelFirstArray) {
        save_machine_regs();
        siglongjmp(cip->CompilerBotch, OUT_OF_HEAP_BOTCH);
      }
    }
    if (LOCAL_LabelFirstArray && max_label <= LOCAL_LabelFirstArraySz) {
      cip->label_offset = LOCAL_LabelFirstArray;
    } else {
      cip->label_offset = (Int *)Yap_AllocCodeSpace(sizeof(Int) * max_label);
      if (!cip->label_offset) {
        save_machine_regs();
        siglongjmp(cip->CompilerBotch, OUT_OF_HEAP_BOTCH);
      }
    }
  }
#else
  cip->label_offset = (Int *)cip->freep;
#endif
  cip->clause_has_cut = FALSE;
  cip->code_addr = NULL;
  code_p = do_pass(0, &entry_code, mode, &clause_has_blobs, &clause_has_dbterm,
                   cip, size PASS_REGS);
  if (clause_has_dbterm) {
    cip->dbterml = init_dbterms_list(code_p, ap);
  }
  if (ap->PredFlags & DynamicPredFlag) {
    size = (CELL)NEXTOP(
        NEXTOP(NEXTOP((yamop *)(((DynamicClause *)NULL)->ClCode), Otapl),
               Osbpp),
        e);
  }
  if ((CELL)code_p > size)
    size = (CELL)code_p;
  if (mode == ASSEMBLING_CLAUSE && ap->PredFlags & LogUpdatePredFlag &&
      !is_fact) {
    DBTerm *x;
    LogUpdClause *cl;
    UInt osize;

    if (!(x = fetch_clause_space(&t, size, cip, &osize PASS_REGS))) {
      return NULL;
    }
    cl = (LogUpdClause *)((CODEADDR)x - (UInt)size);
    cl->lusl.ClSource = x;
    cl->ClFlags |= SrcMask;
    x->ag.line_number = Yap_source_line_no();
    cl->ClSize = osize;
    cip->code_addr = (yamop *)cl;
  } else if (mode == ASSEMBLING_CLAUSE &&
             (ap->PredFlags & (SourcePredFlag | MultiFileFlag) ||
              trueGlobalPrologFlag(SOURCE_FLAG)) &&
             !is_fact) {
    DBTerm *x;
    StaticClause *cl;
    UInt osize;

    if (!(x = fetch_clause_space(&t, size, cip, &osize PASS_REGS))) {
      return NULL;
    }
    cl = (StaticClause *)((CODEADDR)x - (UInt)size);
    cip->code_addr = (yamop *)cl;
    code_p = do_pass(1, &entry_code, mode, &clause_has_blobs,
                     &clause_has_dbterm, cip, size PASS_REGS);
    /* make sure we copy after second pass */
    cl->usc.ClSource = x;
    cl->ClFlags |= SrcMask;
    x->ag.line_number = Yap_source_line_no();
    cl->ClSize = osize;
    LOCAL_ProfEnd = code_p;
    Yap_inform_profiler_of_clause(cl, LOCAL_ProfEnd, ap, GPROF_CLAUSE);
    return entry_code;
  } else {
    while ((cip->code_addr = (yamop *)Yap_AllocCodeSpace(size)) == NULL) {

      if (!Yap_growheap(TRUE, size, cip)) {
        LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
        LOCAL_Error_Size = size;
        return NULL;
      }
    }
    Yap_inform_profiler_of_clause(
        cip->code_addr, (char *)(cip->code_addr) + size, ap,
        (mode == ASSEMBLING_INDEX ? GPROF_INDEX : GPROF_CLAUSE));
    if (mode == ASSEMBLING_CLAUSE) {
      if (ap->PredFlags & LogUpdatePredFlag) {
        ((LogUpdClause *)(cip->code_addr))->ClSize = size;
        Yap_LUClauseSpace += size;
      } else {
        StaticClause *cl = ((StaticClause *)(cip->code_addr));
        cl->usc.ClSource = NULL;
        cl->ClSize = size;
        cl->ClFlags = 0;
        Yap_ClauseSpace += size;
      }
    } else {
      if (ap->PredFlags & LogUpdatePredFlag) {
        Yap_LUIndexSpace_Tree += size;
      } else
        Yap_IndexSpace_Tree += size;
    }
  }
  do_pass(1, &entry_code, mode, &clause_has_blobs, &clause_has_dbterm, cip,
          size PASS_REGS);
  return entry_code;
}

void Yap_InitComma(void) {
  yamop *code_p = COMMA_CODE;
  code_p->opc = opcode(_call);
  code_p->y_u.Osbpp.s = emit_count(-Signed(RealEnvSize) - sizeof(CELL) * 3);
  code_p->y_u.Osbpp.p = code_p->y_u.Osbpp.p0 =
      RepPredProp(PredPropByFunc(FunctorComma, 0));
  code_p->y_u.Osbpp.bmap = NULL;
  GONEXT(Osbpp);
  code_p->opc = opcode(_p_execute_tail);
  code_p->y_u.Osbmp.s = emit_count(-Signed(RealEnvSize) - 3 * sizeof(CELL));
  code_p->y_u.Osbmp.bmap = NULL;
  code_p->y_u.Osbmp.mod = MkAtomTerm(AtomUser);
  code_p->y_u.Osbmp.p0 = RepPredProp(PredPropByFunc(FunctorComma, 0));
  GONEXT(Osbmp);
  code_p->opc = emit_op(_deallocate);
  code_p->y_u.p.p = PredMetaCall;
  GONEXT(p);
  code_p->opc = emit_op(_procceed);
  code_p->y_u.p.p = PredMetaCall;
  GONEXT(p);
}

yamop *Yap_InitCommaContinuation(PredEntry *pe) {
  arity_t arity = pe->ArityOfPE, i;
  yamop *code_p = NULL;

  GONEXT(Osbmp);
  for (i = 0; i < arity; i++)
    GONEXT(yx);
  GONEXT(Osbmp);
pe->MetaEntryOfPred = code_p =
Yap_AllocCodeSpace((size_t)code_p);
  code_p->opc = opcode(_call);
  code_p->y_u.Osbpp.s = emit_count(-Signed(RealEnvSize) - sizeof(CELL) * pe->ArityOfPE);
  code_p->y_u.Osbpp.p =
    code_p->y_u.Osbpp.p0 = PredMetaCall;
 code_p->y_u.Osbpp.bmap = NULL;
 GONEXT(Osbmp);
  for (i = 0; i < arity; i++) {
    code_p->opc = opcode(_put_y_var);
    code_p->y_u.yx.y = -i - Signed(RealEnvSize) / sizeof(CELL);
    code_p->y_u.yx.x = emit_xreg(i + 1);
    GONEXT(yx);
  }
  code_p->opc = opcode(_dexecute);
  code_p->y_u.Osbpp.p0 = PredMetaCall;
  code_p->y_u.Osbpp.p = pe;
  GONEXT(Osbpp);
    return pe->MetaEntryOfPred;
}
