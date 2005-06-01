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
* Last rev:     $Date: 2005-06-01 14:02:47 $							 *
* $Log: not supported by cvs2svn $
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
* some extra bug fixes for trail overflows: some cannot be recovered that easily,
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
* replace SYSTEM_ERROR by out OUT_OF_WHATEVER_ERROR whenever appropriate.
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
#include "yapio.h"
#include "compile.h"
#include "clause.h"
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#if HAVE_STRING_H
#include <string.h>
#endif

/* info on compare built-ins */
#define TYPE_XX  0
#define TYPE_CX  1
#define TYPE_XC  2

typedef struct cmp_op_info_struct {
  wamreg x1_arg, x2_arg;
  Int c_arg;
  int c_type;
  struct clause_info_struct *cl_info;
} cmp_op_info;

typedef struct clause_info_struct {
  int alloc_found, dealloc_found;
  CELL commit_lab;
  struct pred_entry *CurrentPred;
} clause_info;

STATIC_PROTO(OPREG Var_Ref, (Ventry *, int));
STATIC_PROTO(wamreg emit_xreg, (CELL));
STATIC_PROTO(yslot emit_yreg, (CELL));
STATIC_PROTO(wamreg emit_x, (CELL));
STATIC_PROTO(yslot emit_y, (Ventry *));
STATIC_PROTO(yamop *emit_a, (CELL));
STATIC_PROTO(CELL *emit_bmlabel, (CELL, struct intermediates *));
STATIC_PROTO(yamop *emit_ilabel, (CELL, struct intermediates *));
STATIC_PROTO(Functor emit_f, (CELL));
STATIC_PROTO(CELL emit_c, (CELL));
STATIC_PROTO(COUNT emit_count, (CELL));
STATIC_PROTO(OPCODE emit_op, (op_numbers));
STATIC_PROTO(yamop *a_cl, (op_numbers, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_cle, (op_numbers, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_e, (op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_ue, (op_numbers, op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_v, (op_numbers, yamop *, int, struct PSEUDO *));
STATIC_PROTO(yamop *a_uv, (Ventry *,op_numbers, op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_vr, (op_numbers, yamop *, int, struct PSEUDO *));
STATIC_PROTO(yamop *a_rv, (op_numbers, OPREG, yamop *, int, struct PSEUDO *));
STATIC_PROTO(yamop *a_vv, (op_numbers, op_numbers, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_glist, (int *, yamop *, int, struct intermediates *));
STATIC_PROTO(void   a_pair, (CELL *, int, struct intermediates *));
STATIC_PROTO(yamop *a_f, (CELL, op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_c, (CELL, op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_uc, (CELL, op_numbers, op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_n, (op_numbers, int, yamop *, int));
STATIC_PROTO(yamop *a_un, (op_numbers, op_numbers, int, yamop *, int));
STATIC_PROTO(yamop *a_nc, (CELL, op_numbers, int, yamop *, int));
STATIC_PROTO(yamop *a_unc, (CELL, op_numbers, op_numbers, int, yamop *, int));
STATIC_PROTO(yamop *a_r, (CELL, op_numbers, yamop *, int));
STATIC_PROTO(yamop *a_p, (op_numbers, clause_info *, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_pl, (op_numbers, PredEntry *, yamop *, int));
STATIC_PROTO(yamop *a_l, (CELL, op_numbers, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_hx, (op_numbers, union clause_obj *, int, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_if, (op_numbers, union clause_obj *, int, yamop *, int, struct intermediates *cip));
STATIC_PROTO(yamop *a_cut, (clause_info *,yamop *, int, struct intermediates *));
#ifdef YAPOR
STATIC_PROTO(yamop *a_try, (op_numbers, CELL, CELL, *clause_info, int, int, yamop *, int));
STATIC_PROTO(yamop *a_either, (op_numbers, CELL, CELL, int, int, yamop *, int, struct intermediates *));
#else
STATIC_PROTO(yamop *a_try, (op_numbers, CELL, CELL, clause_info *, yamop *, int));
STATIC_PROTO(yamop *a_either, (op_numbers, CELL, CELL, yamop *,  int, struct intermediates *));
#endif	/* YAPOR */
STATIC_PROTO(yamop *a_gl, (op_numbers, clause_info *, yamop *, int, struct PSEUDO *));
STATIC_PROTO(yamop *a_bfunc, (CELL, clause_info *, yamop *, int, struct intermediates *));
STATIC_PROTO(wamreg compile_cmp_flags, (char *));
STATIC_PROTO(yamop *a_igl, (CELL, op_numbers, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_xigl, (op_numbers, yamop *, int, struct PSEUDO *));
STATIC_PROTO(yamop *a_ucons, (int *, compiler_vm_op, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_uvar, (yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_wvar, (yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *do_pass, (int, yamop **, int, int *, struct intermediates *, UInt));
#ifdef DEBUG_OPCODES
STATIC_PROTO(void DumpOpCodes, (void));
#endif
#ifdef SFUNC
STATIC_PROTO(void a_vsf, (int, yamop *, int, struct PSEUDO *));
STATIC_PROTO(void a_asf, (int, yamop *, int, struct PSEUDO *));
#endif
STATIC_PROTO(yamop *check_alloc, (clause_info *, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_deallocate, (clause_info *, yamop *, int, struct intermediates *));
STATIC_PROTO(yamop *a_bmap, (yamop *, int, struct PSEUDO *));
STATIC_PROTO(void a_fetch_vv, (cmp_op_info *, int, struct intermediates *));
STATIC_PROTO(void a_fetch_cv, (cmp_op_info *, int, struct intermediates *));
STATIC_PROTO(void a_fetch_vc, (cmp_op_info *, int, struct intermediates *));
STATIC_PROTO(yamop *a_f2, (int, cmp_op_info *, yamop *, int, struct intermediates *));

#define CELLSIZE sizeof(CELL)

#define GONEXT(TYPE)      code_p = ((yamop *)(&(code_p->u.TYPE.next)))

inline static yslot
emit_y(Ventry *ve)
{
#if MSHIFTOFFS
  return(-FixedEnvSize - ((ve->NoOfVE) & MaskVarAdrs) - 1);
#else
  return(-FixedEnvSize - (((ve->NoOfVE) & MaskVarAdrs) * CELLSIZE) - CELLSIZE);
#endif
}

inline static OPREG
Var_Ref(Ventry *ve, int is_y_var)
{
  if (is_y_var) {
#if MSHIFTOFFS
    return -FixedEnvSize - ((ve->NoOfVE) & MaskVarAdrs) - 1;
#else
    return -FixedEnvSize - (((ve->NoOfVE) & MaskVarAdrs) * CELLSIZE) - CELLSIZE;
#endif
  }
  else {
#if PRECOMPUTE_REGADDRESS
    return (CELL) (XREGS + ((ve->NoOfVE) & MaskVarAdrs));
#else
#if MSHIFTOFFS
    return ((ve->NoOfVE) & MaskVarAdrs);
#else
    return CELLSIZE * ((ve->NoOfVE) & MaskVarAdrs);
#endif
#endif /* PRECOMPUTE_REGADDRESS */
  }
}

#define is_void_var() (((Ventry *) (cip->cpc->rnd1))->KindOfVE == VoidVar)
#define is_a_void(X) (((Ventry *) (X))->KindOfVE == VoidVar)

#define is_temp_var() (((Ventry *) (cip->cpc->rnd1))->KindOfVE == TempVar)
#define is_atemp_var(p) (((Ventry *) (p->rnd1))->KindOfVE == TempVar)

#define no_ref_var()   (((Ventry *) (cip->cpc->rnd1))->NoOfVE == 1)
#define no_ref(X) (((Ventry *) (X))->NoOfVE == 1)

inline static yamop *
fill_small(CELL w, yamop *code_p, int pass_no)
{
  SMALLUNSGN *ptr = ((SMALLUNSGN *) (code_p));

  if (pass_no)
    *ptr = (SMALLUNSGN) w;
  return (yamop *) (++ptr);
}

inline static yamop *
fill_a(CELL a, yamop *code_p, int pass_no)
{
  CELL *ptr = ((CELL *) (code_p));

  if (pass_no)
    *ptr = a;
  return (yamop *) (++ptr);
}

inline static wamreg
emit_xreg(CELL w)
{
  return (wamreg) w;
}

inline static yslot
emit_yreg(CELL w)
{
  return (yslot) w;
}

inline static wamreg
emit_x(CELL xarg)
{
#if PRECOMPUTE_REGADDRESS
  return (emit_xreg((CELL) (XREGS + xarg)));
#else
#if MSHIFTOFFS
  return (emit_xreg(xarg));
#else
  return (emit_xreg(CELLSIZE * (xarg)));
#endif
#endif /* PRECOMPUTE_REGADDRESS */
}

wamreg
Yap_emit_x(CELL xarg)
{
  return emit_x(xarg);
}

inline static yamop *
emit_a(CELL a)
{
  return ((yamop *) (a));
}

inline static struct pred_entry *
emit_pe(struct pred_entry *a)
{
  return a;
}

inline static yamop *
emit_ilabel(register CELL addr, struct intermediates *cip)
{
  if (addr & 1)
    return (emit_a(Unsigned(cip->code_addr) + cip->label_offset[addr]));
  else {
    return (emit_a(addr));
  }
}

inline static CELL *
emit_bmlabel(register CELL addr, struct intermediates *cip)
{
  return (CELL *)(emit_a(Unsigned(cip->code_addr) + cip->label_offset[addr]));
}

inline static Functor
emit_f(CELL a)
{
  return (Functor) (a);
}

inline static CELL
emit_c(CELL a)
{
  return a;
}

static inline COUNT
emit_count(CELL count)
{
  return count;
}

#ifdef DEBUG_OPCODES
inline static void
DumpOpCodes(void)
{
  int i = 0, j;

  while (i < 30) {
    for (j = i; j <= _std_top; j += 25)
      fprintf(Yap_stderr, "%5d %6lx", j, absmadr(j));
    fputc('\n',Yap_stderr);
    ++i;
  }
}
#endif

static inline OPCODE
emit_op(op_numbers op)
{
  return absmadr((Int) op);
}

static OPCODE
opcode(op_numbers op)
{
  return (emit_op(op));
}

OPCODE
Yap_opcode(op_numbers op)
{
  return opcode(op);
}

static void
add_clref(CELL clause_code, int pass_no)
{
  if (pass_no) {
    LogUpdClause *cl = ClauseCodeToLogUpdClause(clause_code);
    cl->ClRefCount++;
  }
}

static yamop *
a_cl(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = cip->code_addr;
  }
  GONEXT(l);
  return code_p;
}

static yamop *
a_lucl(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.Ill.I = (LogUpdIndex *)cip->code_addr;
    code_p->u.Ill.l1 = emit_ilabel(cip->cpc->rnd1, cip);
    code_p->u.Ill.l2 = emit_ilabel(cip->cpc->rnd2, cip);
    code_p->u.Ill.s  = cip->cpc->rnd3;
#if defined(YAPOR) || defined(THREADS)
    code_p->u.Ill.p = cip->CurrentPred;
#endif
  }
  GONEXT(Ill);
  return code_p;
}

static yamop *
a_cle(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    LogUpdClause *cl = (LogUpdClause *)cip->code_addr;

    code_p->opc = emit_op(opcode);
    code_p->u.EC.ClTrail = 0;
    code_p->u.EC.ClENV = 0;
    code_p->u.EC.ClRefs = 0;
    code_p->u.EC.ClBase = cip->code_addr;
    cl->ClExt = code_p;
    cl->ClFlags |= LogUpdRuleMask;
  }
  GONEXT(EC);
  return code_p;
}

inline static yamop *
a_e(op_numbers opcode, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
  }
  GONEXT(e);  
  return code_p;
}

inline static yamop *
a_ue(op_numbers opcode, op_numbers opcodew, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.o.opcw = emit_op(opcodew);
  }
  GONEXT(o);
  return code_p;
}

inline static yamop *
a_v(op_numbers opcode, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  Ventry *ve = (Ventry *) cpc->rnd1;
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (ve->KindOfVE == PermVar) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.y.y = emit_yreg(var_offset);
    }
    GONEXT(y);
  }
  else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.x.x = emit_xreg(var_offset);
    }
    GONEXT(x);
  }
  return code_p;
}

inline static yamop *
a_uv(Ventry *ve, op_numbers opcode, op_numbers opcodew, yamop *code_p, int pass_no)
{
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.oy.opcw = emit_op((op_numbers)((int)opcodew + is_y_var));
      code_p->u.oy.y = emit_yreg(var_offset);
    }
    GONEXT(oy);
  }
  else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.ox.opcw = emit_op((op_numbers)((int)opcodew + is_y_var));
      code_p->u.ox.x = emit_xreg(var_offset);
    }
    GONEXT(ox);
  }
  return code_p;
}

inline static yamop *
a_vv(op_numbers opcode, op_numbers opcodew, yamop *code_p, int pass_no, struct intermediates *cip)
{
  Ventry *ve = (Ventry *) cip->cpc->rnd1;
  int is_y_var = (ve->KindOfVE == PermVar);

  if (pass_no) {
    OPREG var_offset = Var_Ref(ve, is_y_var);
    code_p->opc = emit_op(opcode);
    code_p->u.oxx.opcw = emit_op(opcodew);
    code_p->u.oxx.xl = emit_xreg(var_offset);
  }
  cip->cpc = cip->cpc->nextInst;
  if (pass_no) {
    OPREG var_offset;
    int is_y_var;

    ve = (Ventry *) cip->cpc->rnd1;
    is_y_var = (ve->KindOfVE == PermVar);
    var_offset = Var_Ref(ve, is_y_var);
    code_p->u.oxx.xr = emit_xreg(var_offset);
  }
  GONEXT(oxx);
  return code_p;
}

inline static yamop *
a_vr(op_numbers opcode, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  Ventry *ve = (Ventry *) cpc->rnd1;
  int is_y_var = (ve->KindOfVE == PermVar);

  if (is_y_var) {
    if (pass_no) {
      OPREG var_offset;

      var_offset = Var_Ref(ve, is_y_var);
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.yx.y = emit_yreg(var_offset);
      code_p->u.yx.x = emit_x(cpc->rnd2);
    }
    GONEXT(yx);
  }
  else {
    if (pass_no) {
      OPREG var_offset;

      var_offset = Var_Ref(ve, is_y_var);
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.xx.xl = emit_xreg(var_offset);
      code_p->u.xx.xr = emit_x(cpc->rnd2);
    }
    GONEXT(xx);
  }
  return code_p;
}

inline static yamop *
a_rv(op_numbers opcode, OPREG var_offset, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  Ventry *ve = (Ventry *) cpc->rnd1;
  int is_y_var = (ve->KindOfVE == PermVar);

  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.xy.x = emit_x(cpc->rnd2);
      code_p->u.xy.y = emit_yreg(var_offset);
    }
    GONEXT(xy);
  }
  else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.xx.xl = emit_x(cpc->rnd2);
      code_p->u.xx.xr = emit_xreg(var_offset);
    }
    GONEXT(xx);
  }
  return code_p;
}

#ifdef SFUNC

/* vsc: I don't understand these instructions */

inline static void
a_vsf(int opcode, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  Ventry *ve = (Ventry *) cpc->rnd1;
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);

  var_offset = Var_Ref(ve, is_y_var);
  if (is_y_var) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.fy.f = emit_f(cpc->rnd2);
      code_p->u.fy.a = ArityOfFunctor(emit_f(cpc->rnd2));
      code_p->u.fy.y = emit_yreg(var_offset);
    }
    GONEXT(fy);
  }
  else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.fx.f = emit_f(cpc->rnd2);
      code_p->u.fx.a = ArityOfFunctor(emit_f(cpc->rnd2));
      code_p->u.fx.x = emit_xreg(var_offset);
    }
    GONEXT(fx);
  }
  return code_p;
}

inline static void
a_asf(int opcode, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  if (pass_no) {
    code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
    code_p->u.fn.f = emit_f(cpc->rnd2);
    code_p->u.fn.a = ArityOfFunctor(emit_f(cpc->rnd2));
    code_p->u.fn.n = emit_count(cpc->rnd1);
  }
  GONEXT(fn);
  return code_p;
}
#endif

inline static void
a_pair(CELL *seq_ptr, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    CELL lab, lab0 = seq_ptr[1];
    lab = (CELL) emit_ilabel(lab0, cip);
    seq_ptr[0] = (CELL) emit_a(seq_ptr[0]);
    seq_ptr[1] = lab;
  }
}

inline static yamop *
a_n(op_numbers opcode, int count, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.s.s = count;
  }
  GONEXT(s);
  return code_p;
}

inline static yamop *
a_un(op_numbers opcode, op_numbers opcodew, int count, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.os.opcw = emit_op(opcodew);
    code_p->u.os.s = count;
  }
  GONEXT(os);
  return code_p;
}

inline static yamop *
a_f(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no)
{
  if (pass_no) {
    Functor f = emit_f(rnd1);

    code_p->opc = emit_op(opcode);
    code_p->u.f.f = f;
    code_p->u.f.a = ArityOfFunctor(f);
  }
  GONEXT(f);
  return code_p;
}

inline static yamop *
a_uf(CELL rnd1, op_numbers opcode, op_numbers opcodew, yamop *code_p, int pass_no)
{
  if (pass_no) {
    Functor f = emit_f(rnd1);

    code_p->opc = emit_op(opcode);
    code_p->u.of.opcw = emit_op(opcodew);
    code_p->u.of.f = f;
    code_p->u.of.a = ArityOfFunctor(f);
  }
  GONEXT(of);
  return code_p;
}

inline static yamop *
a_c(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.c.c = emit_c(rnd1);
  }
  GONEXT(c);
  return code_p;
}

inline static yamop *
a_uc(CELL rnd1, op_numbers opcode, op_numbers opcode_w, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.oc.opcw = emit_op(opcode_w);
    code_p->u.oc.c = emit_c(rnd1);
  }
  GONEXT(oc);
  return code_p;
}

inline static yamop *
a_blob(CELL rnd1, op_numbers opcode, int *clause_has_blobsp, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.c.c =
      AbsAppl((CELL *)(Unsigned(cip->code_addr) + cip->label_offset[rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(c);
  return code_p;
}

inline static yamop *
a_ublob(CELL rnd1, op_numbers opcode, op_numbers opcode_w, int *clause_has_blobsp, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.oc.opcw = emit_op(opcode_w);
    code_p->u.oc.c = 
      AbsAppl((CELL *)(Unsigned(cip->code_addr) + cip->label_offset[rnd1]));
      
  }
  *clause_has_blobsp = TRUE;
  GONEXT(oc);
  return code_p;
}

inline static yamop *
a_nc(CELL rnd1, op_numbers opcode, int i, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sc.s = i;
    code_p->u.sc.c = emit_c(rnd1);
  }
  GONEXT(sc);
  return code_p;
}

inline static yamop *
a_unc(CELL rnd1, op_numbers opcode, op_numbers opcodew, int i, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.osc.opcw = emit_op(opcodew);
    code_p->u.osc.s = i;
    code_p->u.osc.c = emit_c(rnd1);
  }
  GONEXT(osc);
  return code_p;
}

inline static yamop *
a_rf(op_numbers opcode, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xf.x = emit_x(cpc->rnd2);
    code_p->u.xf.f = emit_f(cpc->rnd1);
    code_p->u.xf.a = ArityOfFunctor(emit_f(cpc->rnd1));
  }
  GONEXT(xf);
  return code_p;
}

static yamop *
a_rc(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (cip->cpc->rnd2 == 1 &&
      cip->cpc->nextInst->rnd2 == 2 &&
      (cip->cpc->nextInst->op == get_atom_op ||
       cip->cpc->nextInst->op == get_num_op)) {
    struct PSEUDO *next;

    next = cip->cpc->nextInst;
    if (next->nextInst->rnd2 == 3 &&
	(next->nextInst->op == get_atom_op ||
	 next->nextInst->op == get_num_op)) {
      struct PSEUDO *snext = next->nextInst;

      if (snext->nextInst->rnd2 == 4 &&
	  (snext->nextInst->op == get_atom_op ||
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
	      code_p->u.cccccc.c1 = emit_c(cip->cpc->rnd1);
	      code_p->u.cccccc.c2 = emit_c(next->rnd1);
	      code_p->u.cccccc.c3 = emit_c(snext->rnd1);
	      code_p->u.cccccc.c4 = emit_c(s2next->rnd1);
	      code_p->u.cccccc.c5 = emit_c(s3next->rnd1);
	      code_p->u.cccccc.c6 = emit_c(s3next->nextInst->rnd1);
	    }
	    cip->cpc = s3next->nextInst;
	    GONEXT(cccccc);
	  } else {
	    if (pass_no) {
	      code_p->opc = emit_op(_get_5atoms);
	      code_p->u.ccccc.c1 = emit_c(cip->cpc->rnd1);
	      code_p->u.ccccc.c2 = emit_c(next->rnd1);
	      code_p->u.ccccc.c3 = emit_c(snext->rnd1);
	      code_p->u.ccccc.c4 = emit_c(s2next->rnd1);
	      code_p->u.ccccc.c5 = emit_c(s3next->rnd1);
	    }
	    cip->cpc = s3next;
	    GONEXT(ccccc);
	  }
	} else {
	  if (pass_no) {
	    code_p->opc = emit_op(_get_4atoms);
	    code_p->u.cccc.c1 = emit_c(cip->cpc->rnd1);
	    code_p->u.cccc.c2 = emit_c(next->rnd1);
	    code_p->u.cccc.c3 = emit_c(snext->rnd1);
	    code_p->u.cccc.c4 = emit_c(s2next->rnd1);
	  }
	  cip->cpc = s2next;
	  GONEXT(cccc);
	}
      } else {
	if (pass_no) {
	  code_p->opc = emit_op(_get_3atoms);
	  code_p->u.ccc.c1 = emit_c(cip->cpc->rnd1);
	  code_p->u.ccc.c2 = emit_c(next->rnd1);
	  code_p->u.ccc.c3 = emit_c(snext->rnd1);
	}      
	cip->cpc = snext;
	GONEXT(ccc);
      }
    } else {
      if (pass_no) {
	code_p->opc = emit_op(_get_2atoms);
	code_p->u.cc.c1 = emit_c(cip->cpc->rnd1);
	code_p->u.cc.c2 = emit_c(next->rnd1);
      }      
      cip->cpc = next;
      GONEXT(cc);
    }
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcode);
      code_p->u.xc.x = emit_x(cip->cpc->rnd2);
      code_p->u.xc.c = emit_c(cip->cpc->rnd1);
    }
    GONEXT(xc);
  }
  return code_p;
}

inline static yamop *
a_rb(op_numbers opcode, int *clause_has_blobsp, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xc.x = emit_x(cip->cpc->rnd2);
    code_p->u.xc.c = AbsAppl((CELL *)(Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1]));
  }
  *clause_has_blobsp = TRUE;
  GONEXT(xc);
  return code_p;
}

inline static yamop *
a_r(CELL arnd2, op_numbers opcode, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.x.x = emit_x(arnd2);
  }
  GONEXT(x);
  return code_p;
}

static yamop *
check_alloc(clause_info *clinfo, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (clinfo->alloc_found == 2) {
    if (clinfo->CurrentPred->PredFlags & LogUpdatePredFlag)
      code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
    code_p = a_e(_allocate, code_p, pass_no);
    clinfo->alloc_found = 1;
  }
  return code_p;
}

static yamop *
a_p(op_numbers opcode, clause_info *clinfo, yamop *code_p, int pass_no, struct intermediates *cip)
{				/* emit opcode & predicate code address */
  Prop fe = (Prop) (cip->cpc->rnd1);
  CELL Flags = RepPredProp(fe)->PredFlags;
  if (Flags & AsmPredFlag) {
    op_numbers op;

    code_p = check_alloc(clinfo, code_p, pass_no, cip);
    switch (Flags & 0x7f) {
    case _equal:
      op = _p_equal;
      break;
    case _dif:
      op = _p_dif;
      break;
    case _eq:
      op = _p_eq;
      break;
    case _functor:
      op = _p_functor;
      break;
    default:
      op = _p_equal;  /* just to make some compilers happy */
      Yap_Error(INTERNAL_COMPILER_ERROR, TermNil, "internal assembler error for built-in (%d)", (Flags & 0x7f));
      save_machine_regs();
      longjmp(cip->CompilerBotch, 1);
    }
    return a_e(op, code_p, pass_no);
  }
  if (Flags & CPredFlag) {
    code_p = check_alloc(clinfo, code_p, pass_no, cip);
    if (clinfo->commit_lab && (Flags & TestPredFlag)) {
      if (pass_no) {
	if (Flags & UserCPredFlag) {
	  Yap_Error(INTERNAL_COMPILER_ERROR, TermNil,
		"user defined predicate cannot be a test predicate");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	} else
	  code_p->opc = emit_op(_call_c_wfail);
	code_p->u.sdl.s =
	  emit_count(-Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2);
	code_p->u.sdl.l =
	  emit_a(Unsigned(cip->code_addr) + cip->label_offset[clinfo->commit_lab]);
	code_p->u.sdl.p =
	  emit_pe(RepPredProp(fe));
      }
      GONEXT(sdl);
      clinfo->commit_lab = 0;
    } else {
      if (pass_no) {
	if (Flags & UserCPredFlag) {
	  code_p->opc = emit_op(_call_usercpred);
	} else {
	  if (RepPredProp(fe)->FunctorOfPred == FunctorExecuteInMod) {
	    code_p->opc = emit_op(_p_execute);
	  } else {
	    code_p->opc = emit_op(_call_cpred);
	  }
	}
	code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - CELLSIZE
				     * (cip->cpc->rnd2));
	if (RepPredProp(fe)->FunctorOfPred != FunctorExecuteInMod) {
	  code_p->u.sla.sla_u.p =  RepPredProp(fe);
	} else {
	  code_p->u.sla.sla_u.mod =  cip->cpc->rnd4;
	}
	code_p->u.sla.p0 =  clinfo->CurrentPred;
	if (cip->cpc->rnd2) {
	  code_p->u.sla.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
	} else {
	  /* there is no bitmap as there are no variables in the environment */
	  code_p->u.sla.bmap = NULL;
	}
      }
      GONEXT(sla);
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
  }
  else {
    code_p = check_alloc(clinfo, code_p, pass_no, cip);
    if (pass_no)
      code_p->opc = emit_op(opcode);
  }
  if (opcode == _call) {
    if (pass_no) {
      code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - CELLSIZE *
				   cip->cpc->rnd2);
      code_p->u.sla.sla_u.p = RepPredProp(fe);
      code_p->u.sla.p0 = clinfo->CurrentPred;
      if (cip->cpc->rnd2)
	code_p->u.sla.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
      else
	/* there is no bitmap as there are no variables in the environment */
	code_p->u.sla.bmap = NULL;
    }
    GONEXT(sla);
  }
  else {
    if (pass_no)
      code_p->u.p.p = RepPredProp(fe);
    GONEXT(p);
  }
  return code_p;
}

/*
  emit a false call so that the garbage collector and friends will find
  reasonable information on the stack.
*/
static yamop *
a_empty_call(clause_info *clinfo, yamop *code_p, int pass_no, struct  intermediates *cip)
{			
  if (clinfo->alloc_found == 1 && !clinfo->dealloc_found) {
    /* we have a solid environment under us, just trust it */
    if (pass_no)
      code_p->opc = emit_op(_call);
  }
  else {
    /** oops, our environment is crap */
    if (pass_no)
      code_p->opc = emit_op(_fcall);
  }
  if (pass_no) {
    PredEntry *pe = RepPredProp(Yap_GetPredPropByAtom(AtomTrue,0));
    code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - CELLSIZE *
				   cip->cpc->rnd2);
    code_p->u.sla.sla_u.p = pe;
    code_p->u.sla.p0 = clinfo->CurrentPred;
    if (cip->cpc->rnd2)
      code_p->u.sla.bmap = emit_bmlabel(cip->cpc->rnd1, cip);
    else
      /* there is no bitmap as there are no variables in the environment */
      code_p->u.sla.bmap = NULL;
  }
  GONEXT(sla);
  return code_p;
}

static yamop *
a_l(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = emit_a(Unsigned(cip->code_addr) + cip->label_offset[rnd1]);
  }
  GONEXT(l);
  return code_p;
}

static yamop *
a_il(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = emit_ilabel(rnd1, cip);
  }
  GONEXT(l);
  return code_p;
}

static yamop *
a_pl(op_numbers opcode, PredEntry *pred, yamop *code_p, int pass_no)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.p.p = pred;
  }
  GONEXT(p);
  return code_p;
}

static wamreg
compile_cmp_flags(char *s)
{
  if (strcmp(s,"=<") == 0)
    return(EQ_OK_IN_CMP|LT_OK_IN_CMP);
  if (strcmp(s,"<") == 0)
    return(LT_OK_IN_CMP);
  if (strcmp(s,">=") == 0)
    return(EQ_OK_IN_CMP|GT_OK_IN_CMP);
  if (strcmp(s,">") == 0)
    return(GT_OK_IN_CMP);
  if (strcmp(s,"=:=") == 0)
    return(EQ_OK_IN_CMP);
  if (strcmp(s,"=\\=") == 0)
    return(GT_OK_IN_CMP|LT_OK_IN_CMP);
  Yap_Error(INTERNAL_COMPILER_ERROR, TermNil, "internal assembler error in flags for %s", s);
  return(0);
}

wamreg
Yap_compile_cmp_flags(PredEntry *pred)
{
  return compile_cmp_flags(RepAtom(NameOfFunctor(pred->FunctorOfPred))->StrOfAE);
}

static yamop *
a_bfunc(CELL pred, clause_info *clinfo, yamop *code_p, int pass_no, struct intermediates *cip)
{
  Ventry *ve = (Ventry *) cip->cpc->rnd1;
  OPREG var_offset;
  int is_y_var = (ve->KindOfVE == PermVar);
  
  var_offset = Var_Ref(ve, is_y_var);
  if (ve->KindOfVE == PermVar) {
    yslot v1 = emit_yreg(var_offset);
    cip->cpc = cip->cpc->nextInst;
    ve = (Ventry *) cip->cpc->rnd1;
    is_y_var = (ve->KindOfVE == PermVar);
    var_offset = Var_Ref(ve, is_y_var);
    if (is_y_var) {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_yy);
	code_p->u.llyy.p = RepPredProp(((Prop)pred));
	if (clinfo->commit_lab) {
	  code_p->u.llyy.f = 
	    emit_a(Unsigned(cip->code_addr) + cip->label_offset[clinfo->commit_lab]);
	  clinfo->commit_lab = 0;
	} else {
	  code_p->u.llyy.f = FAILCODE;
	}
	code_p->u.llyy.y1 = v1;
	code_p->u.llyy.y2 = emit_yreg(var_offset);
	code_p->u.llyy.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(llyy);
    } else {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_yx);
	code_p->u.llxy.p = RepPredProp(((Prop)pred));
	if (clinfo->commit_lab) {
	  code_p->u.llxy.f = 
	    emit_a(Unsigned(cip->code_addr) + cip->label_offset[clinfo->commit_lab]);
	  clinfo->commit_lab = 0;
	} else {
	  code_p->u.llxy.f = FAILCODE;
	}
	code_p->u.llxy.x = emit_xreg(var_offset);
	code_p->u.llxy.y = v1;
	code_p->u.llxy.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(llxy);
    }
  } else {
    wamreg x1 = emit_xreg(var_offset);
    OPREG var_offset;

    cip->cpc = cip->cpc->nextInst;
    ve = (Ventry *) cip->cpc->rnd1;
    is_y_var = (ve->KindOfVE == PermVar);
    var_offset = Var_Ref(ve, is_y_var);
    if (is_y_var) {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_xy);
	code_p->u.llxy.p = RepPredProp(((Prop)pred));
	if (clinfo->commit_lab) {
	  code_p->u.llxy.f = 
	    emit_a(Unsigned(cip->code_addr) + cip->label_offset[clinfo->commit_lab]);
	  clinfo->commit_lab = 0;
	} else {
	  code_p->u.llxy.f = FAILCODE;
	}
	code_p->u.llxy.x = x1;
	code_p->u.llxy.y = emit_yreg(var_offset);
	code_p->u.llxy.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(llxy);
    } else {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_xx);
	code_p->u.llxx.p = RepPredProp(((Prop)pred));
	if (clinfo->commit_lab) {
	  code_p->u.llxx.f = 
	    emit_a(Unsigned(cip->code_addr) + cip->label_offset[clinfo->commit_lab]);
	  clinfo->commit_lab = 0;
	} else {
	  code_p->u.llxx.f = FAILCODE;
	}
	code_p->u.llxx.x1 = x1;
	code_p->u.llxx.x2 = emit_xreg(var_offset);
	code_p->u.llxx.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(llxx);
    }
  }
  return code_p;
}

static yamop *
a_igl(CELL rnd1, op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = emit_ilabel(rnd1, cip);
  }
  GONEXT(l);
  return code_p;
}

static yamop *
a_xigl(op_numbers opcode, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xll.x = emit_x(cpc->rnd2);
    code_p->u.xll.l1 = emit_a(cpc->rnd1);
    code_p->u.xll.l2 = NEXTOP(code_p,xll);
  }
  GONEXT(xll);
  return code_p;
}

static yamop *
a_4sw(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  CELL *seq_ptr;

  if (opcode == _switch_on_type &&
      cip->cpc->nextInst != NULL &&
      cip->cpc->nextInst->op == label_op &&
      cip->cpc->arnds[1] == cip->cpc->nextInst->rnd1 &&
      !(cip->cpc->arnds[0] & 1) &&
      cip->cpc->nextInst->nextInst != NULL &&
      cip->cpc->nextInst->nextInst->op == if_c_op &&
      cip->cpc->nextInst->nextInst->rnd1 == 1 &&
      cip->cpc->nextInst->nextInst->arnds[1] == TermNil &&
      cip->cpc->nextInst->nextInst->arnds[0] == cip->cpc->arnds[2]) {
    if (pass_no) {
      code_p->opc = emit_op(_switch_list_nl);
      seq_ptr = cip->cpc->arnds;
      code_p->u.ollll.pop = ((yamop *)(seq_ptr[0]))->opc;
      code_p->u.ollll.l1 = emit_ilabel(seq_ptr[0], cip);
      code_p->u.ollll.l2 = emit_ilabel(cip->cpc->nextInst->nextInst->arnds[2], cip);
      code_p->u.ollll.l3 = emit_ilabel(seq_ptr[2], cip);
      code_p->u.ollll.l4 = emit_ilabel(seq_ptr[3], cip);
    }
    GONEXT(ollll);
    cip->cpc = cip->cpc->nextInst->nextInst;
  } else {
    if (pass_no) {
      code_p->opc = emit_op(opcode);
      seq_ptr = cip->cpc->arnds;
      code_p->u.llll.l1 = emit_ilabel(seq_ptr[0], cip);
      code_p->u.llll.l2 = emit_ilabel(seq_ptr[1], cip);
      code_p->u.llll.l3 = emit_ilabel(seq_ptr[2], cip);
      code_p->u.llll.l4 = emit_ilabel(seq_ptr[3], cip);
    }
    GONEXT(llll);
  }
  return code_p;
}

static yamop *
a_4sw_x(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xllll.x = emit_x(cip->cpc->rnd2);
    cip->cpc = cip->cpc->nextInst;
    seq_ptr = cip->cpc->arnds;
    code_p->u.xllll.l1 = emit_ilabel(seq_ptr[0], cip);
    code_p->u.xllll.l2 = emit_ilabel(seq_ptr[1], cip);
    code_p->u.xllll.l3 = emit_ilabel(seq_ptr[2], cip);
    code_p->u.xllll.l4 = emit_ilabel(seq_ptr[3], cip);
  } else {
    /* skip one */
    cip->cpc = cip->cpc->nextInst;
  }
  GONEXT(xllll);
  return code_p;
}

static yamop *
a_4sw_s(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sllll.s = cip->cpc->rnd2;
    cip->cpc = cip->cpc->nextInst;
    seq_ptr = cip->cpc->arnds;
    code_p->u.sllll.l1 = emit_ilabel(seq_ptr[0], cip);
    code_p->u.sllll.l2 = emit_ilabel(seq_ptr[1], cip);
    code_p->u.sllll.l3 = emit_ilabel(seq_ptr[2], cip);
    code_p->u.sllll.l4 = emit_ilabel(seq_ptr[3], cip);
  } else {
    /* skip one */
    cip->cpc = cip->cpc->nextInst;
  }
  GONEXT(sllll);
  return code_p;
}

static void
init_log_upd_table(LogUpdIndex *ic, union clause_obj *cl_u)
{
  /* insert myself in the indexing code chain */ 
  ic->SiblingIndex = cl_u->lui.ChildIndex;
  if (ic->SiblingIndex) {
    ic->SiblingIndex->PrevSiblingIndex = ic;
  }
  cl_u->lui.ChildIndex = ic;
  ic->PrevSiblingIndex = NULL;
  ic->ChildIndex = NULL;
  ic->ClRefCount = 0;
  ic->u.ParentIndex = (LogUpdIndex *)cl_u;
  INIT_LOCK(ic->ClLock);
  cl_u->lui.ChildIndex = ic;
  cl_u->lui.ClRefCount++;
}

static void
init_static_table(StaticIndex *ic, union clause_obj *cl_u)
{
  /* insert myself in the indexing code chain */ 
  ic->SiblingIndex = cl_u->si.ChildIndex;
  ic->ChildIndex = NULL;
  cl_u->si.ChildIndex = ic;
}

static yamop *
a_hx(op_numbers opcode, union clause_obj *cl_u, int log_update, yamop *code_p, int pass_no, struct intermediates *cip)
{
  register CELL i, imax;
  register CELL *seq_ptr = (CELL *)cip->cpc->rnd2;
  int j = 0;

  imax = cip->cpc->rnd1;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sssl.s = emit_c(imax);
    code_p->u.sssl.l = emit_a(cip->cpc->rnd2);
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
    code_p->u.sssl.e = j;
    code_p->u.sssl.w = 0;    
  }
  GONEXT(sssl);
  return code_p;
}

static yamop *
a_if(op_numbers opcode, union clause_obj *cl_u, int log_update, yamop *code_p, int pass_no, struct intermediates *cip)
{
  register CELL i, imax;
  register CELL *seq_ptr = (CELL *)cip->cpc->rnd2;

  imax = cip->cpc->rnd1;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sssl.s = code_p->u.sssl.e = emit_count(imax);
    code_p->u.sssl.w = 0;
    code_p->u.sssl.l = emit_a(cip->cpc->rnd2);
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
    lab = (CELL) emit_ilabel(lab0, cip);
    seq_ptr[1] = lab;
  }
  return code_p;
}

static yamop *
a_ifnot(op_numbers opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
  CELL *seq_ptr = cip->cpc->arnds;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.clll.c = seq_ptr[0];		    /* tag */
    code_p->u.clll.l1 = emit_ilabel(seq_ptr[1], cip);  /* success point */
    code_p->u.clll.l2 = emit_ilabel(seq_ptr[2], cip);  /* fail point */
    code_p->u.clll.l3 = emit_ilabel(seq_ptr[3], cip);  /* delay point */
  }
  GONEXT(clll);
  return code_p;
}

static yamop *
a_cut(clause_info *clinfo, yamop *code_p, int pass_no, struct intermediates *cip)
{
  code_p = check_alloc(clinfo, code_p, pass_no, cip);
  if (clinfo->dealloc_found) {
    return a_e(_cut_e, code_p, pass_no);
  } else if (clinfo->alloc_found) {
    return a_e(_cut, code_p, pass_no);
  } else {
    return a_e(_cut_t, code_p, pass_no);
  }
}

static yamop *
#ifdef YAPOR
a_try(op_numbers opcode, CELL lab, CELL opr, clause_info *clinfo, int nofalts, int hascut, yamop *code_p, int pass_no)
#else
a_try(op_numbers opcode, CELL lab, CELL opr, clause_info *clinfo, yamop *code_p, int pass_no)
#endif	/* YAPOR */
{
  switch (opr) {
  case 2:
    if (opcode == _try_clause) {
      if (pass_no) {
	code_p->opc = emit_op(_try_clause2);
	code_p->u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    } else if (opcode == _retry) {
      if (pass_no) {
	code_p->opc = emit_op(_retry2);
	code_p->u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    }
  case 3:
    if (opcode == _try_clause) {
      if (pass_no) {
	code_p->opc = emit_op(_try_clause3);
	code_p->u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    } else if (opcode == _retry) {
      if (pass_no) {
	code_p->opc = emit_op(_retry3);
	code_p->u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    }
  case 4:
    if (opcode == _try_clause) {
      if (pass_no) {
	code_p->opc = emit_op(_try_clause4);
	code_p->u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    } else if (opcode == _retry) {
      if (pass_no) {
	code_p->opc = emit_op(_retry4);
	code_p->u.l.l = emit_a(lab);
      }
      GONEXT(l);
      return code_p;
    }
  }
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.ld.d = emit_a(lab);
    code_p->u.ld.s = emit_count(opr);
    code_p->u.ld.p = clinfo->CurrentPred;
#ifdef TABLING
    code_p->u.ld.te = clinfo->CurrentPred->TableOfPred;
#endif
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, nofalts);
    if (hascut)
      PUT_YAMOP_CUT(code_p);
    if (clinfo->CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
#endif /* YAPOR */
  }
  GONEXT(ld);
  return code_p;
}

static yamop *
#ifdef YAPOR
a_either(op_numbers opcode, CELL opr, CELL lab, int nofalts, int hascut, yamop *code_p, int pass_no, struct intermediates *cip)
#else
a_either(op_numbers opcode, CELL opr, CELL lab, yamop *code_p, int pass_no, struct intermediates *cip)
#endif /* YAPOR */
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sla.s = emit_count(opr);
    code_p->u.sla.sla_u.l = emit_a(lab);
    code_p->u.sla.p0 =  cip->CurrentPred;
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, nofalts);
    if (hascut)
      PUT_YAMOP_CUT(code_p);
    if (clinfo->CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
    if(opcode != _or_last) {
      code_p->u.sla.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
    }
#else
    code_p->u.sla.bmap = emit_bmlabel(cip->cpc->arnds[1], cip);
#endif /* YAPOR */
  }
  GONEXT(sla);
  return code_p;
}

static yamop *
a_gl(op_numbers opcode, clause_info *clinfo, yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
#ifdef YAPOR
  return a_try(opcode, cpc->rnd1, IPredArity, clinfo, cpc->rnd2 >> 1, cpc->rnd2 & 1, code_p, pass_no);
#else
  return a_try(opcode, cpc->rnd1, IPredArity, clinfo, code_p, pass_no);
#endif /* YAPOR */
}

/*
 * optimizes several unify_cons for the same constant. It must be avoided for
 * the head of the first argument, because of indexing 
 */
static yamop *
a_ucons(int *do_not_optimise_uatomp, compiler_vm_op opcode, yamop *code_p, int pass_no, struct intermediates *cip)
{
#if AGGREGATE_OPS
  PInstr *np = cip->cpc->nextInst;
  register int i = 0;
  CELL my_cons = cip->cpc->rnd1;
  

  if (*do_not_optimise_uatomp) {
    *do_not_optimise_uatomp = FALSE;
    if (opcode == unify_atom_op)
      return a_uc(cip->cpc->rnd1, _unify_atom, _unify_atom_write, code_p, pass_no);
    else
      return a_c(cip->cpc->rnd1, _write_atom, code_p, pass_no);
  }
  else {
    while (np->op == opcode && np->rnd1 == my_cons) {
      i++;
      cip->cpc = np;
      np = np->nextInst;
    }
    if (i == 0) {
      if (opcode == unify_atom_op)
	return a_uc(cip->cpc->rnd1, _unify_atom, _unify_atom_write, code_p, pass_no);
      else
	return a_c(cip->cpc->rnd1, _write_atom, code_p, pass_no);
    } else {
      if (opcode == unify_atom_op)
	return a_unc(cip->cpc->rnd1, _unify_n_atoms, _unify_n_atoms_write, i + 1, code_p, pass_no);
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

static yamop *
a_uvar(yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (!is_void_var()) {
#if AGGREGATE_OPS
    if (is_temp_var()) {
      PInstr *np = cip->cpc->nextInst;

      if (np->op == unify_var_op &&
	  is_atemp_var(np)) {
	return a_vv(_unify_x_var2, _unify_x_var2_write, code_p, pass_no, cip);
      }
      else if (np->op == unify_last_var_op &&
	       is_atemp_var(np)) {
	return a_vv(_unify_l_x_var2,
	     _unify_l_x_var2_write, code_p, pass_no, cip);
      }
    }
#endif /* AGGREGATE_OPS */
    return a_uv((Ventry *) cip->cpc->rnd1, _unify_x_var, _unify_x_var_write, code_p, pass_no);
  }
  else {
#if AGGREGATE_OPS
    int i = 1;
    PInstr *np = cip->cpc->nextInst;

    /* skip void vars */
    while (np->op == unify_var_op && is_a_void(np->rnd1)) {
      i++;
      cip->cpc = np;
      np = np->nextInst;
    }
    if (np->op == unify_last_var_op &&
	is_a_void(np->rnd1)) {
      if (i == 0) 
	code_p = a_ue(_unify_l_void, _unify_l_void_write, code_p, pass_no);
      else
	code_p = a_un(_unify_l_n_voids, _unify_l_n_voids_write, i + 1, code_p, pass_no);
      cip->cpc = np;
    }
    else if (i == 1)
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

static yamop *
a_wvar(yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (!no_ref_var())
    return a_v(_write_x_var, code_p, pass_no, cip->cpc);
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

static yamop *
a_glist(int *do_not_optimise_uatomp, yamop *code_p, int pass_no, struct intermediates *cip)
{
#if AGGREGATE_OPS
  PInstr *pnext = cip->cpc->nextInst;

  if (cip->cpc->rnd2 != 1 && pnext->op == unify_val_op) {
    Ventry *ve = (Ventry *) pnext->rnd1;
    int is_y_var;
    OPREG var_offset;
    
    pnext->rnd2 = cip->cpc->rnd2;
    cip->cpc = pnext;
    is_y_var = (ve->KindOfVE == PermVar);
    var_offset = Var_Ref(ve, is_y_var);
    return a_rv(_glist_valx, var_offset, code_p, pass_no, cip->cpc);
  } else if (cip->cpc->rnd2 == 1 && pnext->op == unify_atom_op) {
    *do_not_optimise_uatomp = TRUE;
    return a_r(cip->cpc->rnd2, _get_list, code_p, pass_no);
  } else if (cip->cpc->rnd2 != 1 && pnext->op == unify_var_op
	   && is_a_void(pnext->rnd1)) {
    PInstr *ppnext = pnext->nextInst;

    if (ppnext && (ppnext->op == unify_last_var_op
		   || ppnext->op == unify_last_val_op)) {
      Ventry *ve = (Ventry *) ppnext->rnd1;
      int is_y_var = (ve->KindOfVE == PermVar);
      OPREG var_offset;

      ppnext->rnd2 = cip->cpc->rnd2;
      cip->cpc = ppnext;
      var_offset = Var_Ref(ve, is_y_var);
      return a_rv((op_numbers)((int)_gl_void_varx + (cip->cpc->op == unify_last_var_op ? 0 : 2)), var_offset, code_p, pass_no, cip->cpc);
    } else {
      return a_r(cip->cpc->rnd2, _get_list, code_p, pass_no);
    }
  } else
#endif /* AGGREGATE_OPS */
    return a_r(cip->cpc->rnd2, _get_list, code_p, pass_no);
}

#define NEXTOPC   (cip->cpc->nextInst)->op

static yamop *
a_deallocate(clause_info *clinfo, yamop *code_p, int pass_no, struct intermediates *cip)
{
  if (clinfo->alloc_found == 2) {
    /* this should never happen */
    if (clinfo->CurrentPred->PredFlags & LogUpdatePredFlag)
      code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
    code_p = a_e(_allocate, code_p, pass_no);
  }
  if (NEXTOPC == execute_op) {
    cip->cpc = cip->cpc->nextInst;
    code_p = a_p(_dexecute, clinfo, code_p, pass_no, cip);
  } else
    code_p = a_e(_deallocate, code_p, pass_no);
  clinfo->dealloc_found = TRUE;
  return code_p;
}

static yamop *
a_bmap(yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  /* how much space do we need to reserve */
  int i, max = (cpc->rnd1)/(8*sizeof(CELL));
  for (i = 0; i <= max; i++)
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}

static yamop *
a_bregs(yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  /* how much space do we need to reserve */
  int i, max = (cpc->rnd1)/(8*sizeof(CELL));
  code_p = fill_a(cpc->rnd1, code_p, pass_no);
  for (i = 0; i <= max; i++) 
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}


static yamop *
copy_blob(yamop *code_p, int pass_no, struct PSEUDO *cpc)
{
  /* copy the blob to code space, making no effort to align if a double */
  int max = cpc->rnd1, i;
  for (i = 0; i < max; i++)
    code_p = fill_a(cpc->arnds[i], code_p, pass_no);
  return code_p;
}


static void
a_fetch_vv(cmp_op_info *cmp_info, int pass_no, struct intermediates *cip)
{
  /* the next three instructions must be a get_val, get_val, and BIP */
  if (pass_no == 0) {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
    p = p->nextInst;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
  } else {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;

    cmp_info->c_type = TYPE_XX;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      cmp_info->x1_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      cmp_info->x1_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
    p = p->nextInst;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      cmp_info->x2_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      cmp_info->x2_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
  }
}

static void
a_fetch_vc(cmp_op_info *cmp_info, int pass_no, struct intermediates *cip)
{
  /* the next two instructions must be a get_val and BIP */
  if (pass_no == 0) {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
  } else {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;

    cmp_info->c_type = TYPE_XC;
    cmp_info->c_arg = cip->cpc->rnd1;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      cmp_info->x1_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      cmp_info->x1_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
  }
}

static void
a_fetch_cv(cmp_op_info *cmp_info, int pass_no, struct intermediates *cip)
{
  /* the next two instructions must be a get_val and BIP */
  if (pass_no == 0) {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
  } else {
    PInstr *p = cip->cpc->nextInst;
    Ventry *ve;

    cmp_info->c_type = TYPE_CX;
    cmp_info->c_arg = cip->cpc->rnd1;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      cmp_info->x1_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      cmp_info->x1_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
  }
}

static yamop *
a_f2(int var, cmp_op_info *cmp_info, yamop *code_p, int pass_no, struct intermediates *cip)
{
  Int opc = cip->cpc->rnd2;
  Ventry *ve = (Ventry *)(cip->cpc->rnd1);
  int is_y_var = (ve->KindOfVE == PermVar);

  if (opc <= _primitive) {
    if (is_y_var) {
      if (pass_no) {
	code_p->u.yF.y = emit_y(ve);
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
	  code_p->opc = opcode(_p_cut_by_y);
	  break;
	case _primitive:
	  code_p->opc = opcode(_p_primitive_y);
	  break;
	}
	if (cmp_info->cl_info->commit_lab) {
	  code_p->u.yF.F = 
	    emit_a(Unsigned(cip->code_addr) + cip->label_offset[cmp_info->cl_info->commit_lab]);
	  cmp_info->cl_info->commit_lab = 0;
	} else {
	  code_p->u.yF.F = FAILCODE;
	}
      }
      GONEXT(yF);
      return code_p;
    } else {
      if (pass_no) {
	code_p->u.xF.x = emit_x(ve->NoOfVE & MaskVarAdrs);
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
	  code_p->opc = opcode(_p_cut_by_x);
	  break;
	case _primitive:
	  code_p->opc = opcode(_p_primitive_x);
	  break;
	}
	if (cmp_info->cl_info->commit_lab) {
	  code_p->u.xF.F = 
	    emit_a(Unsigned(cip->code_addr) + cip->label_offset[cmp_info->cl_info->commit_lab]);
	  cmp_info->cl_info->commit_lab = 0;
	} else {
	  code_p->u.xF.F = FAILCODE;
	}
      }
      GONEXT(xF);
      return code_p;
    }
  }
  if (opc == _functor && cip->cpc->nextInst->op == f_var_op) {
    Ventry *nve;

    cip->cpc = cip->cpc->nextInst;
    nve = (Ventry *)(cip->cpc->rnd1);
    if (is_y_var) {
      if (nve->KindOfVE == PermVar) {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_yy);
	  code_p->u.yyx.y1 = emit_y(ve);
	  code_p->u.yyx.y2 = emit_y(nve);
	  code_p->u.yyx.x = cmp_info->x1_arg;
	}
	GONEXT(yyx);
	return code_p;
      } else {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_yx);
	  code_p->u.yxx.y = emit_y(ve);
	  code_p->u.yxx.x1 = emit_x(nve->NoOfVE & MaskVarAdrs);
	  code_p->u.yxx.x2 = cmp_info->x1_arg;
	}
	GONEXT(yxx);
	return code_p;
      }
    } else {
      if (nve->KindOfVE == PermVar) {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_xy);
	  code_p->u.xyx.x1 = emit_x(ve->NoOfVE & MaskVarAdrs);
	  code_p->u.xyx.y2 = emit_y(nve);
	  code_p->u.xyx.x = cmp_info->x1_arg;
	}
	GONEXT(xyx);
	return code_p;
      } else {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_xx);
	  code_p->u.xxx.x1 = emit_x(ve->NoOfVE & MaskVarAdrs);
	  code_p->u.xxx.x2 = emit_x(nve->NoOfVE & MaskVarAdrs);
	  code_p->u.xxx.x = cmp_info->x1_arg;
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
	code_p->u.yxx.y = emit_y(ve);
	code_p->u.yxx.x1 = cmp_info->x1_arg;
	code_p->u.yxx.x2 = cmp_info->x2_arg;
      }
      GONEXT(yxx);
      break;
    case TYPE_CX:
      if (pass_no) {
	switch (opc) {
	case _plus:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for +/2 (should be XC)");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _minus:
	  code_p->opc = emit_op(_p_minus_y_cv);
	  break;
	case _times:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for */2 (should be XC)");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _div:
	  code_p->opc = emit_op(_p_div_y_cv);
	  break;
	case _and:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for /\\/2 (should be XC)");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _or:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for \\//2 (should be XC)");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
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
	code_p->u.ycx.y = emit_y(ve);
	code_p->u.ycx.c = cmp_info->c_arg;
	code_p->u.ycx.xi = cmp_info->x1_arg;
      }
      GONEXT(ycx);
      break;	  
    case TYPE_XC:
      if (pass_no) {
	switch (opc) {
	case _plus:
	  code_p->opc = emit_op(_p_plus_y_vc);
	  break;
	case _minus:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x2_arg, "internal assembler error XC for -/2");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
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
	  code_p->opc = emit_op(_p_sll_y_vc);
	  break;
	case _slr:
	  code_p->opc = emit_op(_p_slr_y_vc);
	  break;
	case _arg:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x2_arg, "internal assembler error for arg/3");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _functor:
	  code_p->opc = emit_op(_p_func2s_y_vc);
	  break;
	}
	code_p->u.yxc.y = emit_y(ve);
	code_p->u.yxc.c = cmp_info->c_arg;
	code_p->u.yxc.xi = cmp_info->x1_arg;
      }
      GONEXT(yxc);
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
	code_p->u.xxx.x = emit_x(ve->NoOfVE & MaskVarAdrs);
	code_p->u.xxx.x1 = cmp_info->x1_arg;
	code_p->u.xxx.x2 = cmp_info->x2_arg;
      }
      GONEXT(xxx);
      break;
    case TYPE_CX:
      if (pass_no) {
	switch (opc) {
	case _plus:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for +/2");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _minus:
	  code_p->opc = emit_op(_p_minus_cv);
	  break;
	case _times:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for */2");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _div:
	  code_p->opc = emit_op(_p_div_cv);
	  break;
	case _and:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for /\\/2");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _or:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x1_arg, "internal assembler error CX for \\//2");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
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
	code_p->u.xxc.x = emit_x(ve->NoOfVE & MaskVarAdrs);
	code_p->u.xxc.c = cmp_info->c_arg;
	code_p->u.xxc.xi = cmp_info->x1_arg;
      }
      GONEXT(xxc);
      break;	  
    case TYPE_XC:
      if (pass_no) {
	switch (opc) {
	case _plus:
	  code_p->opc = emit_op(_p_plus_vc);
	  break;
	case _minus:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x2_arg, "internal assembler error XC for -/2");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
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
	  code_p->opc = emit_op(_p_sll_vc);
	  break;
	case _slr:
	  code_p->opc = emit_op(_p_slr_vc);
	  break;
	case _arg:
	  Yap_Error(INTERNAL_COMPILER_ERROR, cmp_info->x2_arg, "internal assembler error for arg/3");
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 1);
	  break;
	case _functor:
	  code_p->opc = emit_op(_p_func2s_vc);
	  break;
	}
	code_p->u.xcx.x = emit_x(ve->NoOfVE & MaskVarAdrs);
	code_p->u.xcx.c = cmp_info->c_arg;
	code_p->u.xcx.xi = cmp_info->x1_arg;
      }
      GONEXT(xcx);
      break;	  
    }
  }
  return code_p;
}

#ifdef YAPOR
#define TRYCODE(G,P) a_try((G), Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1], IPredArity, &clinfo, cip->cpc->rnd2 >> 1, cip->cpc->rnd2 & 1, code_p, pass_no)
#define TABLE_TRYCODE(G) a_try((G), (CELL)emit_ilabel(cip->cpc->rnd1, cip), IPredArity, cip->cpc->rnd2 >> 1, cip->cpc->rnd2 & 1, code_p, pass_no)
#else
#define TRYCODE(G,P) a_try((G), Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1], IPredArity, &clinfo, code_p, pass_no)
#define TABLE_TRYCODE(G) a_try((G), (CELL)emit_ilabel(cip->cpc->rnd1, cip), IPredArity, &clinfo, code_p, pass_no)
#endif /* YAPOR */

static yamop *
do_pass(int pass_no, yamop **entry_codep, int assembling, int *clause_has_blobsp, struct intermediates *cip, UInt size)
{
#ifdef YAPOR
#define EITHER_INST 50
  yamop *either_inst[EITHER_INST];
  int either_cont = 0;
#endif	/* YAPOR */
  int log_update;
  int dynamic;
  int tabled;
  int ystop_found = FALSE;
  union clause_obj *cl_u;
  yamop *code_p;
  cmp_op_info cmp_info;
  clause_info clinfo;
  int do_not_optimise_uatom;


  code_p = cip->code_addr;
  cl_u = (union clause_obj *)code_p;
  cip->cpc = cip->CodeStart;
  clinfo.alloc_found = clinfo.dealloc_found = FALSE;
  clinfo.commit_lab = 0L;
  clinfo.CurrentPred = cip->CurrentPred;
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
	cl_u->luc.ClRefCount = 0;
	cl_u->luc.ClPred = cip->CurrentPred;
	cl_u->luc.ClSize = size;
	if (*clause_has_blobsp) {
	  cl_u->luc.ClFlags |= HasBlobsMask;
	}
	cl_u->luc.ClExt = NULL;
	cl_u->luc.ClPrev = cl_u->luc.ClNext = NULL;
#if defined(YAPOR) || defined(THREADS)
	INIT_LOCK(cl_u->luc.ClLock);
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
	cl_u->ic.ClSize = size;
	cl_u->ic.ClRefCount = 0;
#if defined(YAPOR) || defined(THREADS)
	INIT_LOCK(cl_u->ic.ClLock);
	INIT_CLREF_COUNT(&(cl_u->ic));
#endif
      }
      code_p = cl_u->ic.ClCode;
    } else {
      /* static clause */
      if (pass_no) {
	cl_u->sc.ClFlags = StaticMask;
	cl_u->sc.ClNext = NULL;
	cl_u->sc.ClSize = size;
	if (*clause_has_blobsp) {
	  cl_u->sc.ClFlags |= HasBlobsMask;
	}
      }
      code_p = cl_u->sc.ClCode;
    }
    IPredArity = cip->cpc->rnd2;	/* number of args */
    *entry_codep = code_p;
    if (tabled) {
#if TABLING
      code_p = a_try(_table_try_single, (CELL)NEXTOP(code_p,ld), IPredArity, &clinfo, code_p, pass_no);
#endif
    }
    if (dynamic) {
#ifdef YAPOR
      code_p = a_try(_try_me, 0, IPredArity, &clinfo, 1, 0, code_p, pass_no);
#else
      code_p = a_try(_try_me, 0, IPredArity, &clinfo, code_p, pass_no);
#endif	/* YAPOR */
    }
  } else {
    /* index code */
    if (log_update) {
      if (pass_no) {
	cl_u->lui.ClFlags = LogUpdMask|IndexedPredFlag|IndexMask|SwitchRootMask;
	cl_u->lui.ChildIndex = NULL;
	cl_u->lui.SiblingIndex = NULL;
	cl_u->lui.PrevSiblingIndex = NULL;
	cl_u->lui.u.pred = cip->CurrentPred;
	cl_u->lui.ClSize = size;
	cl_u->lui.ClRefCount =  0;
	INIT_LOCK(cl_u->lui.ClLock);
#if defined(YAPOR) || defined(THREADS)
	INIT_CLREF_COUNT(&(cl_u->lui));
#endif
      }
      code_p = cl_u->lui.ClCode;
      *entry_codep = code_p;
#if defined(YAPOR) || defined(THREADS)
      if (assembling == ASSEMBLING_INDEX &&
	  !(cip->CurrentPred->PredFlags & ThreadLocalPredFlag)) {
	if (pass_no) {
	  code_p->opc = opcode(_lock_lu);
	  code_p->u.p.p = cip->CurrentPred;
	}
	GONEXT(p);
      }
#endif
    } else {
      if (pass_no) {
	cl_u->si.ClSize = size;
	cl_u->si.ClFlags = IndexMask; 
	cl_u->si.ChildIndex = NULL;
	cl_u->si.SiblingIndex = NULL;
      }
      code_p = cl_u->si.ClCode;
      *entry_codep = code_p;
    }
  }
  while (cip->cpc) {

    switch ((int) cip->cpc->op) {
#ifdef YAPOR
    case sync_op:
      code_p = a_try(_sync, cip->cpc->rnd1, cip->cpc->rnd2, 1, Zero, code_p);
      break;
#endif /* YAPOR */
#ifdef TABLING
    case table_new_answer_op:
      code_p = a_n(_table_new_answer, (int) cip->cpc->rnd2, code_p, pass_no);
      break;
    case table_try_single_op:
      code_p = a_gl(_table_try_single, &clinfo, code_p, pass_no, cip->cpc);
      break;
#endif /* TABLING */
#ifdef TABLING_INNER_CUTS
    case clause_with_cut_op:
      code_p = a_e(_clause_with_cut, code_p);
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
      code_p = a_vr(_get_x_var, code_p, pass_no, cip->cpc);
      break;
    case put_var_op:
      code_p = a_vr(_put_x_var, code_p, pass_no, cip->cpc);
      break;
    case get_val_op:
      code_p = a_vr(_get_x_val, code_p, pass_no, cip->cpc);
      break;
    case put_val_op:
      code_p = a_vr(_put_x_val, code_p, pass_no, cip->cpc);
      break;
    case get_num_op:
    case get_atom_op:
      code_p = a_rc(_get_atom, code_p, pass_no, cip);
      break;
    case get_float_op:
      code_p = a_rb(_get_float, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case get_longint_op:
      code_p = a_rb(_get_longint, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case get_bigint_op:
      code_p = a_rb(_get_bigint, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case put_num_op:
    case put_atom_op:
      code_p = a_rc(_put_atom, code_p, pass_no, cip);
      break;
    case put_float_op:
    case put_longint_op:
    case put_bigint_op:
      code_p = a_rb(_put_atom, clause_has_blobsp, code_p, pass_no, cip);
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
      code_p = a_vr((op_numbers)((int)_put_unsafe - 1), code_p, pass_no, cip->cpc);
      break;
    case unify_var_op:
      code_p = a_uvar(code_p, pass_no, cip);
      break;
    case unify_last_var_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _unify_l_x_var, _unify_l_x_var_write, code_p, pass_no);
      break;
    case write_var_op:
      code_p = a_wvar(code_p, pass_no, cip);
      break;
    case unify_local_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _unify_x_loc, _unify_x_loc_write, code_p, pass_no);
      break;
    case unify_val_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _unify_x_val, _unify_x_val_write, code_p, pass_no);
      break;
    case unify_last_local_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _unify_l_x_loc, _unify_l_x_loc_write, code_p, pass_no);
      break;
    case unify_last_val_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _unify_l_x_val, _unify_l_x_val_write, code_p, pass_no);
      break;
    case write_local_op:
      code_p = a_v(_write_x_loc, code_p, pass_no, cip->cpc);
      break;
    case write_val_op:
      code_p = a_v(_write_x_val, code_p, pass_no, cip->cpc);
      break;
    case unify_num_op:
    case unify_atom_op:
      code_p = a_ucons(&do_not_optimise_uatom, unify_atom_op, code_p, pass_no, cip);
      break;
    case unify_float_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_float, _unify_atom_write, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_longint_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_longint, _unify_atom_write, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_bigint_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_bigint, _unify_atom_write, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_last_num_op:
    case unify_last_atom_op:
      code_p = a_uc(cip->cpc->rnd1, _unify_l_atom, _unify_l_atom_write, code_p, pass_no);
      break;
    case unify_last_float_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_l_float, _unify_l_atom_write, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_last_longint_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_l_longint, _unify_l_atom_write, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case unify_last_bigint_op:
      code_p = a_ublob(cip->cpc->rnd1, _unify_l_bigint, _unify_l_atom_write, clause_has_blobsp, code_p, pass_no, cip);
      break;
    case write_num_op:
    case write_atom_op:
      code_p = a_ucons(&do_not_optimise_uatom, write_atom_op, code_p, pass_no, cip);
      break;
    case write_float_op:
    case write_longint_op:
    case write_bigint_op:
      code_p = a_blob(cip->cpc->rnd1, _write_atom, clause_has_blobsp, code_p, pass_no, cip);
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
      code_p = a_uf(cip->cpc->rnd1, _unify_struct, _unify_struct_write, code_p, pass_no);
      break;
    case unify_last_struct_op:
      code_p = a_uf(cip->cpc->rnd1, _unify_l_struc, _unify_l_struc_write, code_p, pass_no);
      break;
    case write_struct_op:
      code_p = a_f(cip->cpc->rnd1, _write_struct, code_p, pass_no);
      break;
    case write_last_struct_op:
      code_p = a_f(cip->cpc->rnd1, _write_l_struc, code_p, pass_no);
      break;
    case save_b_op:
    case patch_b_op:
      code_p = a_v(_save_b_x, code_p, pass_no, cip->cpc);
      break;
    case commit_b_op:
      code_p = a_v(_commit_b_x, code_p, pass_no, cip->cpc);
#ifdef YAPOR
      if (pass_no)
	PUT_YAMOP_CUT(*entry_codep);
#endif /* YAPOR */
      break;
    case save_pair_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _save_pair_x, _save_pair_x_write, code_p, pass_no);
      break;
    case save_appl_op:
      code_p = a_uv((Ventry *) cip->cpc->rnd1, _save_appl_x, _save_appl_x_write, code_p, pass_no);
      break;
    case fail_op:
      code_p = a_e(_op_fail, code_p, pass_no);
      break;
    case cut_op:
      code_p = a_cut(&clinfo, code_p, pass_no, cip);
#ifdef YAPOR
      if (pass_no)
	PUT_YAMOP_CUT(*entry_codep);
#endif	/* YAPOR */
      break;
    case cutexit_op:
      code_p = a_cut(&clinfo, code_p, pass_no, cip);
     if (cip->CurrentPred->PredFlags & LogUpdatePredFlag &&
	  *clause_has_blobsp &&
	  !clinfo.alloc_found)
	code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
#if THREADS
     else
       if (cip->CurrentPred->PredFlags & LogUpdatePredFlag &&
	   !(cip->CurrentPred->PredFlags & ThreadLocalPredFlag))
	code_p = a_e(_unlock_lu, code_p, pass_no);
#endif
      code_p = a_e(_procceed, code_p, pass_no);
#ifdef YAPOR
      if (pass_no)
	PUT_YAMOP_CUT(*entry_codep);
#endif	/* YAPOR */
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
      if (log_update &&
	  (assembling == ASSEMBLING_INDEX ||
	   assembling == ASSEMBLING_EINDEX)) {
	code_p = a_cl(_trust_logical_pred, code_p, pass_no, cip);
      }
#ifdef TABLING
      if (tabled)
	code_p = TABLE_TRYCODE(_table_trust_me);
      else
#endif
	code_p = TRYCODE(_trust_me, _trust_me0);
      break;
    case enter_lu_op:
      code_p = a_lucl(_enter_lu_pred, code_p, pass_no, cip);
      break;
    case try_op:
#ifdef TABLING
      if (tabled)
	code_p = a_gl(_table_try, &clinfo, code_p, pass_no, cip->cpc);
      else
#endif
	code_p = a_gl(_try_clause, &clinfo, code_p, pass_no, cip->cpc);
      break;
    case retry_op:
      if (log_update) {
	add_clref(cip->cpc->rnd1, pass_no);
      }
#ifdef TABLING
      if (tabled)
	code_p = a_gl(_table_retry, &clinfo, code_p, pass_no, cip->cpc);
      else
#endif
	code_p = a_gl(_retry, &clinfo, code_p, pass_no, cip->cpc);
      break;
    case trust_op:
      if (log_update) {
	add_clref(cip->cpc->rnd1, pass_no);
	code_p = a_cl(_trust_logical_pred, code_p, pass_no, cip);
      }
#ifdef TABLING
      if (tabled)
	code_p = a_gl(_table_trust, &clinfo, code_p, pass_no, cip->cpc);
      else
#endif
	code_p = a_gl(_trust, &clinfo, code_p, pass_no, cip->cpc);
      break;
    case try_in_op:
      code_p = a_il(cip->cpc->rnd1, _try_in, code_p, pass_no, cip);
      break;
    case jump_op:
      /* don't assemble jumps to next instruction */
      if (cip->cpc->nextInst == NULL ||
	  cip->cpc->nextInst->op != label_op ||
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
	  *clause_has_blobsp &&
	  !clinfo.alloc_found)
	code_p = a_cle(_alloc_for_logical_pred, code_p, pass_no, cip);
#if THREADS
     else
       if (cip->CurrentPred->PredFlags & LogUpdatePredFlag &&
	   !(cip->CurrentPred->PredFlags & ThreadLocalPredFlag))
	code_p = a_e(_unlock_lu, code_p, pass_no);
#endif
      code_p = a_e(_procceed, code_p, pass_no);
      break;
    case call_op:
      code_p = a_p(_call, &clinfo, code_p, pass_no, cip);
      break;
    case execute_op:
#if THREADS
      if (cip->CurrentPred->PredFlags & LogUpdatePredFlag &&
	  !(cip->CurrentPred->PredFlags & ThreadLocalPredFlag))
	code_p = a_e(_unlock_lu, code_p, pass_no);
#endif
      code_p = a_p(_execute, &clinfo, code_p, pass_no, cip);
      break;
    case safe_call_op:
      code_p = a_p(_call, &clinfo, code_p, pass_no, cip);
      break;
    case label_op:
      if (!ystop_found &&
	  cip->cpc->nextInst != NULL &&
	  (cip->cpc->nextInst->op == mark_initialised_pvars_op ||
	   cip->cpc->nextInst->op == blob_op)) {
	ystop_found = TRUE;
	code_p = a_e(_Ystop, code_p, pass_no);
      }
      if (!pass_no) {
	if (CellPtr(cip->label_offset+cip->cpc->rnd1) > ASP-256) {
	  Yap_Error_Size = 256+((char *)(cip->label_offset+cip->cpc->rnd1) - (char *)H);
	  save_machine_regs();
	  longjmp(cip->CompilerBotch, 3);	  
	}
	
	if ( (char *)(cip->label_offset+cip->cpc->rnd1) >= cip->freep)
	  cip->freep = (char *)(cip->label_offset+(cip->cpc->rnd1+1));
	cip->label_offset[cip->cpc->rnd1] = (CELL) code_p;
      }
      /* reset dealloc_found in case there was a branch */
      clinfo.dealloc_found = FALSE;
      break;
    case pop_op:
      if (cip->cpc->rnd1 == 1)
	code_p = a_e(_pop, code_p, pass_no);
      else {
	code_p = a_n(_pop_n, 2 * CELLSIZE * (cip->cpc->rnd1 - 1), code_p, pass_no);
      }
      break;
    case either_op:
      code_p = check_alloc(&clinfo, code_p, pass_no, cip);
#ifdef YAPOR
      if (pass_no)
	either_inst[either_cont++] = code_p;
      code_p = a_either(_either,
	       -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
	       Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1], 0, 0, code_p, pass_no, cip);
#else
      code_p = a_either(_either,
	       -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
	       Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1], code_p, pass_no, cip);
#endif	/* YAPOR */
      break;
    case orelse_op:
#ifdef YAPOR
      if (pass_no)
	either_inst[either_cont++] = code_p;
      code_p = a_either(_or_else,
	       -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
	       Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1], 0, 0, code_p, pass_no, cip);
#else
      code_p = a_either(_or_else,
	       -Signed(RealEnvSize) - CELLSIZE * cip->cpc->rnd2,
	       Unsigned(cip->code_addr) + cip->label_offset[cip->cpc->rnd1], code_p, pass_no, cip);
#endif	/* YAPOR */
      clinfo.dealloc_found = FALSE;
      break;
    case orlast_op:
#ifdef YAPOR
      if (pass_no)
	either_inst[either_cont++] = code_p;
      code_p = a_either(_or_last, 0, 0, 0, 0, code_p, pass_no, cpc);
      if (pass_no) {
	int cont = 1;
	do {
	  either_cont--;
	  PUT_YAMOP_LTT(either_inst[either_cont], cont++);
	} while (either_inst[either_cont]->opc != opcode(_either));
      }
#else
      code_p = a_pl(_or_last, cip->CurrentPred, code_p, pass_no);
#endif	/* YAPOR */
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
    case mark_initialised_pvars_op:
      code_p = a_bmap(code_p, pass_no, cip->cpc);
      break;
    case mark_live_regs_op:
      code_p = a_bregs(code_p, pass_no, cip->cpc);
      break;
    case commit_opt_op:
      clinfo.commit_lab = cip->cpc->rnd1;
      break;
    case fetch_args_vv_op:
      a_fetch_vv(&cmp_info, pass_no, cip);
      break;
    case fetch_args_vc_op:
      a_fetch_vc(&cmp_info, pass_no, cip);
      break;
    case fetch_args_cv_op:
      a_fetch_cv(&cmp_info, pass_no, cip);
      break;
    case f_val_op:
      code_p = a_f2(FALSE, &cmp_info, code_p, pass_no, cip);
      break;
    case f_var_op:
      code_p = a_f2(TRUE, &cmp_info, code_p, pass_no, cip);
      break;
    case enter_profiling_op:
      code_p = a_pl(_enter_profiling, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      break;
    case retry_profiled_op:
      code_p = a_pl(_retry_profiled, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      break;
    case count_call_op:
      code_p = a_pl(_count_call, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      break;
    case count_retry_op:
      code_p = a_pl(_count_retry, (PredEntry *)(cip->cpc->rnd1), code_p, pass_no);
      break;
    case fetch_args_for_bccall:
      if (cip->cpc->nextInst->op != bccall_op) {
	Yap_Error(INTERNAL_COMPILER_ERROR, TermNil, "compiling binary test", (int) cip->cpc->op);
	save_machine_regs();
	longjmp(cip->CompilerBotch, 1);
      }
      code_p = a_bfunc(cip->cpc->nextInst->rnd2, &clinfo, code_p, pass_no, cip);
      break;
    case align_float_op:
      /* install a blob */
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
      if (!((CELL)code_p & 0x4))
	GONEXT(e);
#endif
      break;
    case blob_op:
      /* install a blob */
      code_p = copy_blob(code_p, pass_no, cip->cpc);
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
    default:
      Yap_Error(INTERNAL_COMPILER_ERROR, TermNil, "instruction %d found while assembling", (int) cip->cpc->op);
      save_machine_regs();
      longjmp(cip->CompilerBotch, 1);
    }
    cip->cpc = cip->cpc->nextInst;
  }
  if (!ystop_found)
    code_p = a_e(_Ystop, code_p, pass_no);
  return code_p;
}


static DBTerm *
fetch_clause_space(Term* tp, UInt size, struct intermediates *cip)
{
  CELL *h0 = H;
  DBTerm *x;

  /* This stuff should be just about fetching the space from the data-base,
     unfortunately we have to do all sorts of error handling :-( */
  H = (CELL *)cip->freep;
  while ((x = Yap_StoreTermInDBPlusExtraSpace(*tp, size)) == NULL) {

    H = h0;
    switch (Yap_Error_TYPE) {
    case OUT_OF_STACK_ERROR:
      Yap_Error_Size = 256+((char *)cip->freep - (char *)H);
      save_machine_regs();
      longjmp(cip->CompilerBotch,3);
    case OUT_OF_TRAIL_ERROR:
      /* don't just return NULL */
      ARG1 = *tp;
      if (!Yap_growtrail(64 * 1024L, FALSE)) {
	return NULL;
      }
      Yap_Error_TYPE = YAP_NO_ERROR;
      *tp = ARG1;
      break;
    case OUT_OF_AUXSPACE_ERROR:
      ARG1 = *tp;
      if (!Yap_ExpandPreAllocCodeSpace(Yap_Error_Size, (void *)cip)) {
	return NULL;
      }
      Yap_Error_TYPE = YAP_NO_ERROR;
      *tp = ARG1;
      break;
    case OUT_OF_HEAP_ERROR:
      /* don't just return NULL */
      ARG1 = *tp;
      if (!Yap_growheap(TRUE, size, cip)) {
	return NULL;
      }
      Yap_Error_TYPE = YAP_NO_ERROR;
      *tp = ARG1;
      break;
    default:
      return NULL;
    }
    h0 = H;
    H = (CELL *)cip->freep;
  }
  H = h0;
  return x;
}


yamop *
Yap_assemble(int mode, Term t, PredEntry *ap, int is_fact, struct intermediates *cip)
{
  /*
   * the assembly proccess is done in two passes: 1 - a first pass
   * computes labels offsets and total code size 2 - the second pass
   * produces the final version of the code 
   */
  UInt size = 0;
  yamop *entry_code;
  yamop *code_p;
  int clause_has_blobs = FALSE;

  cip->label_offset = (int *)cip->freep;
  cip->code_addr = NULL;
  code_p = do_pass(0, &entry_code, mode, &clause_has_blobs, cip, size);
  if (ap->PredFlags & DynamicPredFlag) {
    size =
      (CELL)NEXTOP(NEXTOP(NEXTOP((yamop *)(((DynamicClause *)NULL)->ClCode),ld),sla),e);
  }
  if ((CELL)code_p > size)
    size = (CELL)code_p;
  if (mode == ASSEMBLING_CLAUSE && 
      ap->PredFlags & LogUpdatePredFlag &&
      !is_fact) {
    DBTerm *x;
    LogUpdClause *cl;

    if(!(x = fetch_clause_space(&t,size,cip))){
      return NULL;
    }
    cl = (LogUpdClause *)((CODEADDR)x-(UInt)size);
    cl->ClSize += sizeof(DBTerm) + sizeof(CELL)*x->NOfCells;
    cl->ClSource = x;
    cip->code_addr = (yamop *)cl;
  } else if (mode == ASSEMBLING_CLAUSE && 
      (ap->PredFlags & SourcePredFlag ||
       yap_flags[SOURCE_MODE_FLAG]) &&
      !is_fact) {
    DBTerm *x;
    StaticClause *cl;
    if(!(x = fetch_clause_space(&t,size,cip))) {
      return NULL;
    }
    cl = (StaticClause *)((CODEADDR)x-(UInt)size);
    cip->code_addr = (yamop *)cl;
    code_p = do_pass(1, &entry_code, mode, &clause_has_blobs, cip, size);
    /* make sure we copy after second pass */
    cl->ClSize += sizeof(DBTerm) + sizeof(CELL)*x->NOfCells;
    cl->usc.ClSource = x;
    ProfEnd=code_p;
    return entry_code;
  } else {
    while ((cip->code_addr = (yamop *) Yap_AllocCodeSpace(size)) == NULL) {

      if (!Yap_growheap(TRUE, size, cip)) {
	Yap_Error_TYPE = OUT_OF_HEAP_ERROR;
	Yap_Error_Size = size;
	return NULL;
      }
    }
  }
  code_p = do_pass(1, &entry_code, mode, &clause_has_blobs, cip, size);
  ProfEnd=code_p;
  return entry_code;
}

void
Yap_InitComma(void)
{
  yamop *code_p = COMMA_CODE;
  code_p->opc = opcode(_call);
  code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - sizeof(CELL) * 3);
  code_p->u.sla.sla_u.p = 
    code_p->u.sla.p0 =
    RepPredProp(PredPropByFunc(FunctorComma,0));
  code_p->u.sla.bmap = NULL;
  GONEXT(sla);
  if (PRED_GOAL_EXPANSION_ON) {
    Functor fp = Yap_MkFunctor(Yap_FullLookupAtom("$generate_pred_info"),4);
    code_p->opc = emit_op(_call_cpred);
    code_p->u.sla.s = emit_count(-Signed(RealEnvSize));
    code_p->u.sla.sla_u.p =  RepPredProp(Yap_GetPredPropByFunc(fp,0));
    code_p->u.sla.bmap = NULL;
    GONEXT(sla);
    code_p->opc = emit_op(_call);
    code_p->u.sla.s = emit_count(-Signed(RealEnvSize));
    code_p->u.sla.sla_u.p =  PredMetaCall;
    code_p->u.sla.bmap = NULL;
    GONEXT(sla);
    code_p->opc = emit_op(_deallocate);
    GONEXT(e);
    code_p->opc = emit_op(_procceed);
    GONEXT(e);
  } else {
    if (PROFILING) {
      code_p->opc = opcode(_enter_a_profiling);
      GONEXT(e);
    }
    if (CALL_COUNTING) {
      code_p->opc = opcode(_count_a_call);
      GONEXT(e);
    }
    code_p->opc = opcode(_p_execute_tail);
    code_p->u.sla.s = emit_count(-Signed(RealEnvSize)-3*sizeof(CELL));
    code_p->u.sla.bmap = NULL;
    code_p->u.sla.sla_u.p = 
      code_p->u.sla.p0 =
      RepPredProp(PredPropByFunc(FunctorComma,0));
    GONEXT(sla);
  }
}
