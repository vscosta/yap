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
* Last rev:								 *
* mods:									 *
* comments:	abstract machine assembler				 *
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

STATIC_PROTO(void Var_Ref, (Ventry *));
STATIC_PROTO(AREG emit_xreg, (CELL));
STATIC_PROTO(YREG emit_yreg, (CELL));
STATIC_PROTO(AREG emit_xreg2, (void));
STATIC_PROTO(AREG emit_x, (CELL));
STATIC_PROTO(YREG emit_y, (Ventry *));
STATIC_PROTO(CODEADDR emit_a, (CELL));
STATIC_PROTO(CELL *emit_bmlabel, (CELL));
STATIC_PROTO(CODEADDR emit_ilabel, (CELL));
STATIC_PROTO(Functor emit_f, (CELL));
STATIC_PROTO(CELL emit_c, (CELL));
STATIC_PROTO(COUNT emit_count, (CELL));
STATIC_PROTO(OPCODE emit_op, (op_numbers));
STATIC_PROTO(void a_cl, (op_numbers));
STATIC_PROTO(void a_cle, (op_numbers));
STATIC_PROTO(void a_cld, (op_numbers));
STATIC_PROTO(void a_e, (op_numbers));
STATIC_PROTO(void a_ue, (op_numbers, op_numbers));
STATIC_PROTO(void a_v, (op_numbers));
STATIC_PROTO(void a_uv, (op_numbers, op_numbers));
STATIC_PROTO(void a_vr, (op_numbers));
STATIC_PROTO(void a_rv, (op_numbers));
STATIC_PROTO(void a_vv, (op_numbers, op_numbers));
STATIC_PROTO(void a_glist, (void));
STATIC_PROTO(void a_pair, (CELL *));
STATIC_PROTO(void a_f, (op_numbers));
STATIC_PROTO(void a_c, (op_numbers));
STATIC_PROTO(void a_uc, (op_numbers, op_numbers));
STATIC_PROTO(void a_n, (op_numbers, int));
STATIC_PROTO(void a_un, (op_numbers, op_numbers, int));
STATIC_PROTO(void a_nc, (op_numbers, int));
STATIC_PROTO(void a_unc, (op_numbers, op_numbers, int));
STATIC_PROTO(void a_r, (op_numbers));
STATIC_PROTO(void a_p, (op_numbers));
STATIC_PROTO(void a_pl, (op_numbers,PredEntry *));
STATIC_PROTO(void a_l, (op_numbers));
STATIC_PROTO(void a_3sw, (op_numbers));
STATIC_PROTO(void a_3sws, (op_numbers));
STATIC_PROTO(void a_4sw, (op_numbers));
#if USE_THREADED_CODE
STATIC_PROTO(void a_4_lsw, (op_numbers));
#endif
STATIC_PROTO(void a_hx, (op_numbers));
STATIC_PROTO(void a_if, (op_numbers));
STATIC_PROTO(void a_go, (op_numbers));
STATIC_PROTO(void a_cut, (void));
#ifdef YAPOR
STATIC_PROTO(void a_try, (op_numbers, CELL, CELL, int, int));
STATIC_PROTO(void a_either, (op_numbers, CELL, CELL, int, int));
#else
STATIC_PROTO(void a_try, (op_numbers, CELL, CELL));
STATIC_PROTO(void a_either, (op_numbers, CELL, CELL));
#endif	/* YAPOR */
STATIC_PROTO(void a_gl_in, (op_numbers));
STATIC_PROTO(void a_gl, (op_numbers));
STATIC_PROTO(void a_bfunc, (CELL));
STATIC_PROTO(AREG compile_cmp_flags, (char *));
STATIC_PROTO(void a_igl, (op_numbers));
STATIC_PROTO(void a_ucons, (compiler_vm_op));
STATIC_PROTO(void a_uvar, (void));
STATIC_PROTO(void a_wvar, (void));
STATIC_PROTO(void do_pass, (void));
#ifdef DEBUG_OPCODES
STATIC_PROTO(void DumpOpCodes, (void));
#endif
#ifdef SFUNC
STATIC_PROTO(void a_vsf, (int));
STATIC_PROTO(void a_asf, (int));
#endif
STATIC_PROTO(void check_alloc, (void));
STATIC_PROTO(void a_deallocate, (void));
STATIC_PROTO(void a_bmap, (void));
STATIC_PROTO(void a_fetch_vv, (void));
STATIC_PROTO(void a_fetch_cv, (void));
STATIC_PROTO(void a_fetch_vc, (void));
STATIC_PROTO(void a_f2, (int));

#define CELLSIZE sizeof(CELL)

#define MaxLabels	2048

static yamop *code_p;

#define GONEXT(TYPE)      code_p = ((yamop *)(&(code_p->u.TYPE.next)))

static CODEADDR code_addr;
static int pass_no;
int *label_offset;
static OPREG var_offset;
static int is_y_var;

static int alloc_found, dealloc_found;

static int asm_error = FALSE;

static int assembling;

static CELL comit_lab;

static int do_not_optimize_uatom = FALSE;

static AREG x1_arg, x2_arg;

static Int c_arg;

#define TYPE_XX  0
#define TYPE_CX  1
#define TYPE_XC  2
static int c_type;

static int clause_has_blobs;

inline static YREG
emit_y(Ventry *ve)
{
#if MSHIFTOFFS
  return(-FixedEnvSize - ((ve->NoOfVE) & MaskVarAdrs) - 1);
#else
  return(-FixedEnvSize - (((ve->NoOfVE) & MaskVarAdrs) * CELLSIZE) - CELLSIZE);
#endif
}

inline static void
Var_Ref(Ventry *ve)
{
  if (ve->KindOfVE == PermVar) {
    is_y_var = 1;
#if MSHIFTOFFS
    var_offset = -FixedEnvSize - ((ve->NoOfVE) & MaskVarAdrs) - 1;
#else
    var_offset = -FixedEnvSize - (((ve->NoOfVE) & MaskVarAdrs) * CELLSIZE) - CELLSIZE;
#endif
  }
  else {
    is_y_var = 0;
#if PRECOMPUTE_REGADDRESS
    var_offset = (CELL) (XREGS + ((ve->NoOfVE) & MaskVarAdrs));
#else
#if MSHIFTOFFS
    var_offset = ((ve->NoOfVE) & MaskVarAdrs);
#else
    var_offset = CELLSIZE * ((ve->NoOfVE) & MaskVarAdrs);
#endif
#endif /* PRECOMPUTE_REGADDRESS */
  }
}

#define is_void_var() (((Ventry *) (cpc->rnd1))->KindOfVE == VoidVar)
#define is_a_void(X) (((Ventry *) (X))->KindOfVE == VoidVar)

#define is_temp_var() (((Ventry *) (cpc->rnd1))->KindOfVE == TempVar)
#define is_atemp_var(p) (((Ventry *) (p->rnd1))->KindOfVE == TempVar)

#define no_ref_var()   (((Ventry *) (cpc->rnd1))->NoOfVE == 1)
#define no_ref(X) (((Ventry *) (X))->NoOfVE == 1)

inline static void
fill_small(CELL w)
{
  SMALLUNSGN *ptr = ((SMALLUNSGN *) (code_p));

  if (pass_no)
    *ptr = (SMALLUNSGN) w;
  code_p = (yamop *) (++ptr);
}

inline static void
fill_a(CELL a)
{
  CELL *ptr = ((CELL *) (code_p));

  if (pass_no)
    *ptr = a;
  code_p = (yamop *) (++ptr);
}

inline static AREG
emit_xreg(CELL w)
{
  return ((AREG) w);
}

inline static YREG
emit_yreg(CELL w)
{
  return ((YREG) w);
}

inline static AREG
emit_xreg2(void)
{
#if PRECOMPUTE_REGADDRESS
  return (emit_xreg((CELL) (XREGS + cpc->rnd2)));
#else
#if MSHIFTOFFS
  return (emit_xreg(cpc->rnd2));
#else
  return (emit_xreg(CELLSIZE * (cpc->rnd2)));
#endif
#endif /* ALIGN_LONGS */
}

inline static AREG
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

inline static CODEADDR
emit_a(CELL a)
{
  return ((CODEADDR) (a));
}

inline static struct pred_entry *
emit_pe(struct pred_entry *a)
{
  return (a);
}

inline static CODEADDR
emit_ilabel(register CELL addr)
{
  if (addr & 1)
    return (emit_a(Unsigned(code_addr) + label_offset[addr]));
  else
    return (emit_a(addr));
}

inline static CELL *
emit_bmlabel(register CELL addr)
{
  return ((CELL *)(emit_a(Unsigned(code_addr) + label_offset[addr])));
}

inline static Functor
emit_f(CELL a)
{
  return ((Functor) (a));
}

inline static CELL
emit_c(CELL a)
{
  return (a);
}

static inline COUNT
emit_count(CELL count)
{
  return (count);
}

#ifdef DEBUG_OPCODES
inline static void
DumpOpCodes(void)
{
  int i = 0, j;

  while (i < 30) {
    for (j = i; j <= _std_top; j += 25)
      YP_fprintf(YP_stderr, "%5d %6lx", j, absmadr(j));
    YP_putchar('\n');
    ++i;
  }
}
#endif

static inline OPCODE
emit_op(op_numbers op)
{
  return (absmadr((Int) op));
}

OPCODE
opcode(op_numbers op)
{
  return (emit_op(op));
}

static void
a_cl(op_numbers opcode)
{
  if (pass_no) {
    Clause *cl = (Clause *)code_addr;
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = code_addr;
    cl->u.ClVarChain = (yamop *)(Unsigned(code_addr) + label_offset[1]);
  }
  GONEXT(l);
}

static void
a_cle(op_numbers opcode)
{
  if (pass_no) {
    Clause *cl = (Clause *)code_addr;

    code_p->opc = emit_op(opcode);
    code_p->u.EC.ClTrail = 0;
    code_p->u.EC.ClENV = 0;
    code_p->u.EC.ClRefs = 0;
    code_p->u.EC.ClBase = code_addr;
    cl->u2.ClExt = code_p;
    cl->ClFlags |= LogUpdRuleMask;
  }
  GONEXT(EC);
}

static void
a_cld(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = (CODEADDR)(((Clause *)code_addr)->u2.ClExt);
  }
  GONEXT(l);
}

inline static void
a_e(op_numbers opcode)
{
  if (pass_no)
    code_p->opc = emit_op(opcode);
  GONEXT(e);
}

inline static void
a_ue(op_numbers opcode, op_numbers opcodew)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.o.opcw = emit_op(opcodew);
  }
  GONEXT(o);
}

inline static void
a_v(op_numbers opcode)
{
  Ventry *ve = (Ventry *) cpc->rnd1;

  Var_Ref(ve);
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
}

inline static void
a_uv(op_numbers opcode, op_numbers opcodew)
{
  Ventry *ve = (Ventry *) cpc->rnd1;

  Var_Ref(ve);
  if (ve->KindOfVE == PermVar) {
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
}

inline static void
a_vv(op_numbers opcode, op_numbers opcodew)
{
  Ventry *ve = (Ventry *) cpc->rnd1;
  Var_Ref(ve);
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.oxx.opcw = emit_op(opcodew);
    code_p->u.oxx.xl = emit_xreg(var_offset);
  }
  cpc = cpc->nextInst;
  ve = (Ventry *) cpc->rnd1;
  Var_Ref(ve);
  if (pass_no)
    code_p->u.oxx.xr = emit_xreg(var_offset);
  GONEXT(oxx);
}

inline static void
a_vr(op_numbers opcode)
{
  Ventry *ve = (Ventry *) cpc->rnd1;

  Var_Ref(ve);
  if (ve->KindOfVE == PermVar) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.yx.y = emit_yreg(var_offset);
      code_p->u.yx.x = emit_xreg2();
    }
    GONEXT(yx);
  }
  else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.xx.xl = emit_xreg(var_offset);
      code_p->u.xx.xr = emit_xreg2();
    }
    GONEXT(xx);
  }
}

inline static void
a_rv(op_numbers opcode)
{
  Ventry *ve = (Ventry *) cpc->rnd1;

  if (ve->KindOfVE == PermVar) {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.xy.x = emit_xreg2();
      code_p->u.xy.y = emit_yreg(var_offset);
    }
    GONEXT(xy);
  }
  else {
    if (pass_no) {
      code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
      code_p->u.xx.xl = emit_xreg2();
      code_p->u.xx.xr = emit_xreg(var_offset);
    }
    GONEXT(xx);
  }
}

#ifdef SFUNC

/* vsc: I don't understand these instructions */

inline static void
a_vsf(opcode)
     int opcode;
{
  Ventry *ve = (Ventry *) cpc->rnd1;

  Var_Ref(ve);
  if (ve->KindOfVE == PermVar) {
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
}

inline static void
a_asf(opcode)
     int opcode;
{
  if (pass_no) {
    code_p->opc = emit_op((op_numbers)((int)opcode + is_y_var));
    code_p->u.fn.f = emit_f(cpc->rnd2);
    code_p->u.fn.a = ArityOfFunctor(emit_f(cpc->rnd2));
    code_p->u.fn.n = emit_count(cpc->rnd1);
  }
  GONEXT(fn);
}
#endif

inline static void
a_pair(CELL *seq_ptr)
{
  CELL *ptr = ((CELL *) (code_p));

  code_p = (yamop *) (ptr + 2);
  if (pass_no) {
    ptr[0] = (CELL) emit_a(*seq_ptr);
    ptr[1] = (CELL) emit_ilabel(seq_ptr[1]);
  }
}

inline static void
a_n(op_numbers opcode, int count)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.s.s = count;
  }
  GONEXT(s);
}

inline static void
a_un(op_numbers opcode, op_numbers opcodew, int count)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.os.opcw = emit_op(opcodew);
    code_p->u.os.s = count;
  }
  GONEXT(os);
}

inline static void
a_f(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.f.f = emit_f(cpc->rnd1);
    code_p->u.f.a = ArityOfFunctor(emit_f(cpc->rnd1));
  }
  GONEXT(f);
}

inline static void
a_uf(op_numbers opcode, op_numbers opcodew)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.of.opcw = emit_op(opcodew);
    code_p->u.of.f = emit_f(cpc->rnd1);
    code_p->u.of.a = ArityOfFunctor(emit_f(cpc->rnd1));
  }
  GONEXT(of);
}

inline static void
a_c(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.c.c = emit_c(cpc->rnd1);
  }
  GONEXT(c);
}

inline static void
a_uc(op_numbers opcode, op_numbers opcode_w)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.oc.opcw = emit_op(opcode_w);
    code_p->u.oc.c = emit_c(cpc->rnd1);
  }
  GONEXT(oc);
}

inline static void
a_blob(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.c.c =
      AbsAppl((CELL *)(Unsigned(code_addr) + label_offset[cpc->rnd1]));
  }
  clause_has_blobs = TRUE;
  GONEXT(c);
}

inline static void
a_ublob(op_numbers opcode, op_numbers opcode_w)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.oc.opcw = emit_op(opcode_w);
    code_p->u.oc.c = 
      AbsAppl((CELL *)(Unsigned(code_addr) + label_offset[cpc->rnd1]));
      
  }
  clause_has_blobs = TRUE;
  GONEXT(oc);
}

inline static void
a_nc(op_numbers opcode, int i)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sc.s = i;
    code_p->u.sc.c = emit_c(cpc->rnd1);
  }
  GONEXT(sc);
}

inline static void
a_unc(op_numbers opcode, op_numbers opcodew, int i)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.osc.opcw = emit_op(opcodew);
    code_p->u.osc.s = i;
    code_p->u.osc.c = emit_c(cpc->rnd1);
  }
  GONEXT(osc);
}

inline static void
a_rf(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xf.x = emit_xreg2();
    code_p->u.xf.f = emit_f(cpc->rnd1);
    code_p->u.xf.a = ArityOfFunctor(emit_f(cpc->rnd1));
  }
  GONEXT(xf);
}

inline static void
a_rc(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xc.x = emit_xreg2();
    code_p->u.xc.c = emit_c(cpc->rnd1);
  }
  GONEXT(xc);
}

inline static void
a_rb(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.xc.x = emit_xreg2();
    code_p->u.xc.c = AbsAppl((CELL *)(Unsigned(code_addr) + label_offset[cpc->rnd1]));
  }
  GONEXT(xc);
}

inline static void
a_r(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.x.x = emit_xreg2();
  }
  GONEXT(x);
}

static void
check_alloc(void)
{
  if (alloc_found == 2) {
    if (CurrentPred->PredFlags & LogUpdatePredFlag)
      a_cle(_alloc_for_logical_pred);
    a_e(_allocate);
    alloc_found = 1;
  }
}

static void
a_p(op_numbers opcode)
{				/* emit opcode & predicate code address */
  int comit_ok = (comit_lab == 0);
  Prop fe = (Prop) (cpc->rnd1);
  CELL Flags = RepPredProp(fe)->PredFlags;
  if (Flags & AsmPredFlag) {
    op_numbers op;

    check_alloc();
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
      Error(SYSTEM_ERROR, TermNil, "internal assembler error for built-in (%d)", (Flags & 0x7f));
      save_machine_regs();
      longjmp(CompilerBotch, 1);
    }
    a_e(op);
    if (!comit_ok) {
      Error(SYSTEM_ERROR, TermNil,"internal assembler error for commit");
      save_machine_regs();
      longjmp(CompilerBotch, 1);
    }
    return;
  }
  if (Flags & CPredFlag) {
    check_alloc();
    if (!comit_ok && (Flags & TestPredFlag)) {
      if (pass_no) {
	if (Flags & UserCPredFlag) {
	  Error(SYSTEM_ERROR, TermNil,
		"user defined predicate cannot be a test predicate");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	} else
	  code_p->opc = emit_op(_call_c_wfail);
	code_p->u.sdl.s =
	  emit_count(-Signed(RealEnvSize) - CELLSIZE * cpc->rnd2);
	code_p->u.sdl.d =
	  emit_a((CELL) RepPredProp(fe)->TrueCodeOfPred);
	code_p->u.sdl.l =
	  emit_a(Unsigned(code_addr) + label_offset[comit_lab]);
	code_p->u.sdl.p =
	  emit_pe(RepPredProp(fe));
      }
      GONEXT(sdl);
      comit_lab = 0;
      comit_ok = TRUE;
    }
    else {
      if (pass_no) {
	if (Flags & UserCPredFlag) {
	  code_p->opc = emit_op(_call_usercpred);
	} else {
	  if (RepPredProp(fe)->FunctorOfPred == FunctorExecuteInMod)
	    code_p->opc = emit_op(_p_execute);
	  else if (RepPredProp(fe)->FunctorOfPred == FunctorExecuteWithin)
	    code_p->opc = emit_op(_p_execute_within);
	  else if (RepPredProp(fe)->FunctorOfPred == FunctorLastExecuteWithin)
	    code_p->opc = emit_op(_p_last_execute_within);
	  else
	    code_p->opc = emit_op(_call_cpred);
	}
	code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - CELLSIZE
				     * (cpc->rnd2));
	code_p->u.sla.l = emit_a((CELL)
				 RepPredProp(fe)->TrueCodeOfPred);
	code_p->u.sla.p =  RepPredProp(fe);
	code_p->u.sla.p0 =  CurrentPred;
	if (cpc->rnd2)
	  code_p->u.sla.l2 = emit_bmlabel(cpc->arnds[1]);
	else
	  /* there is no bitmap as there are no variables in the environment */
	  code_p->u.sla.l2 = NULL;
      }
      GONEXT(sla);
    }
    if (!comit_ok) {
      Error(SYSTEM_ERROR, TermNil, "internal assembler error for commit");
      save_machine_regs();
      longjmp(CompilerBotch,1);
    }
    return;
  }

  if (opcode == _call && alloc_found == 2) {
    if (CurrentPred->PredFlags & LogUpdatePredFlag)
      a_cle(_alloc_for_logical_pred);
    if (pass_no) {
      code_p->opc = emit_op(_fcall);
    }
    alloc_found = 1;
  }
  else {
    check_alloc();
    if (pass_no)
      code_p->opc = emit_op(opcode);
  }
  if (opcode == _call) {
    if (pass_no) {
      code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - CELLSIZE *
				   cpc->rnd2);
      code_p->u.sla.l = emit_a((CELL) &
			       RepPredProp(fe)->StateOfPred);
      code_p->u.sla.p = RepPredProp(fe);
      code_p->u.sla.p0 = CurrentPred;
      if (cpc->rnd2)
	code_p->u.sla.l2 = emit_bmlabel(cpc->arnds[1]);
      else
	/* there is no bitmap as there are no variables in the environment */
	code_p->u.sla.l2 = NULL;
    }
    GONEXT(sla);
  }
  else {
    if (pass_no)
      code_p->u.l.l = emit_a((CELL) &RepPredProp(fe)->StateOfPred);
    GONEXT(l);
  }
  if (!comit_ok) {
    Error(SYSTEM_ERROR, TermNil, "internal assembler error for commit");
    save_machine_regs();
    longjmp(CompilerBotch,1);
  }
}

/*
  emit a false call so that the garbage collector and friends will find
  reasonable information on the stack.
*/
static void
a_empty_call(void)
{			
  if (alloc_found == 1 && !dealloc_found) {
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
    PredEntry *pe = RepPredProp(GetPredPropByAtom(AtomTrue,0));
    code_p->u.sla.s = emit_count(-Signed(RealEnvSize) - CELLSIZE *
				   cpc->rnd2);
    code_p->u.sla.l = emit_a((CELL)&(pe->StateOfPred));
    code_p->u.sla.p = pe;
    code_p->u.sla.p0 = CurrentPred;
    if (cpc->rnd2)
      code_p->u.sla.l2 = emit_bmlabel(cpc->rnd1);
    else
      /* there is no bitmap as there are no variables in the environment */
      code_p->u.sla.l2 = NULL;
  }
  GONEXT(sla);
}

static void
a_l(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = emit_a(Unsigned(code_addr) + label_offset[cpc->rnd1]);
  }
  GONEXT(l);
}

static void
a_pl(op_numbers opcode, PredEntry *pred)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = emit_a((CELL)pred);
  }
  GONEXT(l);
}

static AREG
compile_cmp_flags(char *s)
{
  if (strcmp(s,"=<") == 0)  return(EQ_OK_IN_CMP|LT_OK_IN_CMP);
  if (strcmp(s,"<") == 0)   return(LT_OK_IN_CMP);
  if (strcmp(s,">=") == 0)  return(EQ_OK_IN_CMP|GT_OK_IN_CMP);
  if (strcmp(s,">") == 0)   return(GT_OK_IN_CMP);
  if (strcmp(s,"=:=") == 0) return(EQ_OK_IN_CMP);
  if (strcmp(s,"=\\=") == 0) return(GT_OK_IN_CMP|LT_OK_IN_CMP);
  Error(SYSTEM_ERROR, x1_arg, "internal assembler error in flags for %s", s);
  return(0);
}


static void
a_bfunc(CELL pred)
{
  Ventry *ve = (Ventry *) cpc->rnd1;
  
  Var_Ref(ve);
  if (ve->KindOfVE == PermVar) {
    YREG v1 = emit_yreg(var_offset);
    cpc = cpc->nextInst;
    ve = (Ventry *) cpc->rnd1;
    Var_Ref(ve);
    if (ve->KindOfVE == PermVar) {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_yy);
	code_p->u.lxy.p = RepPredProp(((Prop)pred));
	code_p->u.lyy.l = RepPredProp(((Prop)pred))->TrueCodeOfPred;
	code_p->u.lyy.y1 = v1;
	code_p->u.lyy.y2 = emit_yreg(var_offset);
	code_p->u.lyy.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(lyy);
    } else {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_yx);
	code_p->u.lxy.l = RepPredProp(((Prop)pred))->TrueCodeOfPred;
	code_p->u.lxy.p = RepPredProp(((Prop)pred));
	code_p->u.lxy.x = emit_xreg(var_offset);
	code_p->u.lxy.y = v1;
	code_p->u.lxy.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(lxy);
    }
  } else {
    AREG x1 = emit_xreg(var_offset);
    cpc = cpc->nextInst;
    ve = (Ventry *) cpc->rnd1;
    Var_Ref(ve);
    if (ve->KindOfVE == PermVar) {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_xy);
	code_p->u.lxy.l = RepPredProp(((Prop)pred))->TrueCodeOfPred;
	code_p->u.lxy.p = RepPredProp(((Prop)pred));
	code_p->u.lxy.x = x1;
	code_p->u.lxy.y = emit_yreg(var_offset);
	code_p->u.lxy.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(lxy);
    } else {
      if (pass_no) {
	code_p->opc = emit_op(_call_bfunc_xx);
	code_p->u.lxy.p = RepPredProp(((Prop)pred));
	code_p->u.lxx.l = RepPredProp(((Prop)pred))->TrueCodeOfPred;
	code_p->u.lxx.x1 = x1;
	code_p->u.lxx.x2 = emit_xreg(var_offset);
	code_p->u.lxx.flags = compile_cmp_flags(RepAtom(NameOfFunctor(RepPredProp(((Prop)pred))->FunctorOfPred))->StrOfAE);
      }
      GONEXT(lxx);
    }
  }
}

static void
a_igl(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.l.l = emit_a(cpc->rnd1);
  }
  GONEXT(l);
}

static void
a_3sw(op_numbers opcode)
{
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    seq_ptr = cpc->arnds;
    code_p->u.lll.l1 = emit_ilabel(seq_ptr[0]);
    code_p->u.lll.l2 = emit_ilabel(seq_ptr[1]);
    code_p->u.lll.l3 = emit_ilabel(seq_ptr[2]);
  }
  GONEXT(lll);
}

static void
a_3sws(op_numbers opcode)
{
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    seq_ptr = cpc->arnds;
    code_p->u.slll.s  = IPredArity;
    code_p->u.slll.p = CurrentPred;
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, cpc->rnd1 >> 1);
    if (cpc->rnd1 & 1)
      PUT_YAMOP_CUT(code_p);
    if (CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
#endif	/* YAPOR */
    code_p->u.slll.l1 = emit_ilabel(seq_ptr[0]);
    code_p->u.slll.l2 = emit_ilabel(seq_ptr[1]);
    code_p->u.slll.l3 = emit_ilabel(seq_ptr[2]);
  }
  GONEXT(slll);
}

static void
a_4sw(op_numbers opcode)
{
  CELL *seq_ptr;

  if (pass_no) {
    code_p->opc = emit_op(opcode);
    seq_ptr = cpc->arnds;
    code_p->u.llll.l1 = emit_ilabel(seq_ptr[0]);
    code_p->u.llll.l2 = emit_ilabel(seq_ptr[1]);
    code_p->u.llll.l3 = emit_ilabel(seq_ptr[2]);
    code_p->u.llll.l4 = emit_ilabel(seq_ptr[3]);
  }
  GONEXT(llll);
}

#if USE_THREADED_CODE
/* specialised code for fast switch_on_list, taking advantage of the
   fact that in this case we are sure it is a list */
static void
a_4_lsw(op_numbers opcode)
{
  CELL *seq_ptr;

  seq_ptr = cpc->arnds;
  if (opcode == _switch_list_nl && (seq_ptr[0] & 1)) {
    /* local address, don't do anything because we
       don't know what is supposed to be there */
    if (pass_no) {
      code_p->opc = emit_op(opcode);
      code_p->u.llll.l1 = emit_ilabel(seq_ptr[0]);
      code_p->u.llll.l2 = emit_ilabel(seq_ptr[1]);
      code_p->u.llll.l3 = emit_ilabel(seq_ptr[2]);
      code_p->u.llll.l4 = emit_ilabel(seq_ptr[3]);
    }
    GONEXT(llll);
  } else {
    /* optimise direct jumps to list like code, by prefetching the
       first address for lists */
    if (pass_no) {
      code_p->opc = emit_op(_switch_list_nl_prefetch);
      code_p->u.ollll.pop = ((yamop *)(seq_ptr[0]))->opc;
      code_p->u.ollll.l1 = emit_ilabel(seq_ptr[0]);
      code_p->u.ollll.l2 = emit_ilabel(seq_ptr[1]);
      code_p->u.ollll.l3 = emit_ilabel(seq_ptr[2]);
      code_p->u.ollll.l4 = emit_ilabel(seq_ptr[3]);
    }
    GONEXT(ollll);
  }
}
#endif

static void
a_hx(op_numbers opcode)
{
  register CELL i, imax;
  register CELL *seq_ptr = cpc->arnds;

  imax = cpc->rnd1;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.c.c = emit_c(imax);
  }
  GONEXT(c);
  for (i = 0; i < imax; i++) {
    a_pair(seq_ptr);
    seq_ptr += 2;
  }
}

static void
a_if(op_numbers opcode)
{
  register CELL i, imax;
  register CELL *seq_ptr = cpc->arnds + 1;

  imax = cpc->rnd1;
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.sl.s = emit_count(imax);
    code_p->u.sl.l = emit_ilabel(cpc->arnds[0]);
  }
  GONEXT(sl);
  for (i = 0; i < imax; i++) {
    a_pair(seq_ptr);
    seq_ptr += 2;
  }
}

static void
a_go(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.cll.c = emit_count(cpc->arnds[0]);
    code_p->u.cll.l1 = emit_ilabel(cpc->arnds[1]);
    code_p->u.cll.l2 = emit_ilabel(cpc->arnds[2]);
  }
  GONEXT(cll);
}

static void
a_cut(void)
{
  check_alloc();
  if (dealloc_found) {
    a_e(_cut_e);
  } else if (alloc_found)
    a_e(_cut);
  else
    a_e(_cut_t);
}

static void
#ifdef YAPOR
a_try(op_numbers opcode, CELL lab, CELL opr, int nofalts, int hascut)
#else
a_try(op_numbers opcode, CELL lab, CELL opr)
#endif	/* YAPOR */
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.ld.d = emit_a(lab);
    code_p->u.ld.s = emit_count(opr);
    code_p->u.ld.p = CurrentPred;
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, nofalts);
    if (hascut)
      PUT_YAMOP_CUT(code_p);
    if (CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
#endif /* YAPOR */
  }
  GONEXT(ld);
}

static void
a_gl_in(op_numbers opcode)
{
  if (pass_no) {
    code_p->opc = emit_op(opcode);
    code_p->u.ldl.d = emit_a(cpc->rnd1);
    code_p->u.ldl.s = emit_count(IPredArity);
    code_p->u.ldl.p = CurrentPred;
#ifdef YAPOR
    INIT_YAMOP_LTT(code_p, cpc->rnd2 >> 1);
    if (cpc->rnd2 & 1)
      PUT_YAMOP_CUT(code_p);
    if (CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
#endif	/* YAPOR */
    /* next op is a jump, with the jump giving the address to fail to
       after this alternative */
    cpc = cpc->nextInst;
    code_p->u.ldl.bl = emit_ilabel(cpc->rnd1);
  } else
    cpc = cpc->nextInst;
  GONEXT(ldl);
}

static void
#ifdef YAPOR
a_either(op_numbers opcode, CELL opr, CELL lab, int nofalts, int hascut)
#else
a_either(op_numbers opcode, CELL opr, CELL lab)
#endif /* YAPOR */
{
  if (pass_no) {
    Prop fe = GetPredPropByAtom(AtomTrue,0);
    code_p->opc = emit_op(opcode);
    code_p->u.sla.s = emit_count(opr);
    code_p->u.sla.l = emit_a(lab);
    /* use code for atom true so that we won't try to do anything smart */
    code_p->u.sla.p = RepPredProp(fe);
    code_p->u.sla.p0 =  CurrentPred;
#ifdef YAPOR
    /* code_p->u.sla.p = (CODEADDR)CurrentPred; */
    INIT_YAMOP_LTT(code_p, nofalts);
    if (hascut)
      PUT_YAMOP_CUT(code_p);
    if (CurrentPred->PredFlags & SequentialPredFlag)
      PUT_YAMOP_SEQ(code_p);
    if(opcode != _or_last) {
      code_p->u.sla.l2 = emit_bmlabel(cpc->arnds[1]);
    }
#else
    code_p->u.sla.l2 = emit_bmlabel(cpc->arnds[1]);
#endif /* YAPOR */
  }
  GONEXT(sla);
}

static void
a_gl(op_numbers opcode)
{
#ifdef YAPOR
  a_try(opcode, cpc->rnd1, IPredArity, cpc->rnd2 >> 1, cpc->rnd2 & 1);
#else
  a_try(opcode, cpc->rnd1, IPredArity);
#endif /* YAPOR */
}

/*
 * optimizes several unify_cons for the same constant. It must be avoided for
 * the head of the first argument, because of indexing 
 */
static void
a_ucons(compiler_vm_op opcode)
{
#if AGGREGATE_OPS
  PInstr *np = cpc->nextInst;
  register int i = 0;
  CELL my_cons = cpc->rnd1;

  if (do_not_optimize_uatom) {
    do_not_optimize_uatom = FALSE;
    if (opcode == unify_atom_op)
      a_uc(_unify_atom, _unify_atom_write);
    else
      a_c(_write_atom);
  }
  else {
    while (np->op == opcode && np->rnd1 == my_cons) {
      i++;
      cpc = np;
      np = np->nextInst;
    }
    if (i == 0) {
      if (opcode == unify_atom_op)
	a_uc(_unify_atom, _unify_atom_write);
      else
	a_c(_write_atom);
    }
    else {
      if (opcode == unify_atom_op)
	a_unc(_unify_n_atoms, _unify_n_atoms_write, i + 1);
      else
	a_nc(_write_n_atoms, i + 1);
    }
  }
#else
  do_not_optimize_uatom = FALSE;
  if (opcode == unify_atom_op)
    a_uc(_unify_atom, _unify_atom_write);
  else
    a_c(_write_atom);
#endif
}

static void
a_uvar(void)
{
  if (!is_void_var()) {
#if AGGREGATE_OPS
    if (is_temp_var()) {
      PInstr *np = cpc->nextInst;

      if (np->op == unify_var_op &&
	  is_atemp_var(np)) {
	a_vv(_unify_x_var2, _unify_x_var2_write);
	return;
      }
      else if (np->op == unify_last_var_op &&
	       is_atemp_var(np)) {
	a_vv(_unify_l_x_var2,
	     _unify_l_x_var2_write);
	return;
      }
    }
#endif /* AGGREGATE_OPS */
    a_uv(_unify_x_var, _unify_x_var_write);
  }
  else {
#if AGGREGATE_OPS
    int i = 1;
    PInstr *np = cpc->nextInst;

    /* skip void vars */
    while (np->op == unify_var_op && is_a_void(np->rnd1)) {
      i++;
      cpc = np;
      np = np->nextInst;
    }
    if (np->op == unify_last_var_op &&
	is_a_void(np->rnd1)) {
      if (i == 0) 
	a_ue(_unify_l_void, _unify_l_void_write);
      else
	a_un(_unify_l_n_voids, _unify_l_n_voids_write, i + 1);
      cpc = np;
    }
    else if (i == 1)
      a_ue(_unify_void, _unify_void_write);
    else {
      a_un(_unify_n_voids, _unify_n_voids_write, i);
    }
#else
    a_ue(_unify_void, _unify_void_write);
#endif
  }
}

static void
a_wvar(void)
{
  if (!no_ref_var())
    a_v(_write_x_var);
  else {
#if AGGREGATE_OPS
    int i = 0;
    PInstr *np = cpc->nextInst;

    while (np->op == write_var_op && no_ref(np->rnd1)) {
      i++;
      cpc = np;
      np = np->nextInst;
    }
    if (i == 0)
      a_e(_write_void);
    else {
      a_n(_write_n_voids, i + 1);
    }
#else
    a_e(_write_void);
#endif
  }
}

static void
a_glist(void)
{
#if AGGREGATE_OPS
  PInstr *pnext = cpc->nextInst;

  if (cpc->rnd2 != 1 && pnext->op == unify_val_op) {
    Ventry *ve = (Ventry *) pnext->rnd1;
    pnext->rnd2 = cpc->rnd2;
    cpc = pnext;
    Var_Ref(ve);
    a_rv(_glist_valx);
  }
  else
    if (cpc->rnd2 == 1 && pnext->op == unify_atom_op) {
    do_not_optimize_uatom = TRUE;
    a_r(_get_list);
  }
  else if (cpc->rnd2 != 1 && pnext->op == unify_var_op
	   && is_a_void(pnext->rnd1)) {
    PInstr *ppnext = pnext->nextInst;

    if (ppnext && (ppnext->op == unify_last_var_op
		   || ppnext->op == unify_last_val_op)) {
      Ventry *ve = (Ventry *) ppnext->rnd1;
      ppnext->rnd2 = cpc->rnd2;
      cpc = ppnext;
      Var_Ref(ve);
      a_rv((op_numbers)((int)_gl_void_varx + (cpc->op == unify_last_var_op ? 0 : 2)));
    }
    else
      a_r(_get_list);
  }
  else
#endif /* AGGREGATE_OPS */
    a_r(_get_list);
}

#define NEXTOPC   (cpc->nextInst)->op

static void
a_deallocate(void)
{
  if (alloc_found == 2) {
    /* this should never happen */
    if (CurrentPred->PredFlags & LogUpdatePredFlag)
      a_cle(_alloc_for_logical_pred);
    a_e(_allocate);
  }
  if (CurrentPred->PredFlags & LogUpdatePredFlag)
    a_cld(_dealloc_for_logical_pred);
  if (NEXTOPC == execute_op) {
    cpc = cpc->nextInst;
    a_p(_dexecute);
  } else
    a_e(_deallocate);
  dealloc_found = TRUE;
}

static void
a_bmap(void)
{
  /* how much space do we need to reserve */
  int i, max = (cpc->rnd1)/(8*sizeof(CELL));
  for (i = 0; i <= max; i++) fill_a(cpc->arnds[i]);    
}

static void
a_bregs(void)
{
  /* how much space do we need to reserve */
  int i, max = (cpc->rnd1)/(8*sizeof(CELL));
  fill_a(cpc->rnd1);
  for (i = 0; i <= max; i++) fill_a(cpc->arnds[i]);    
}


static void
copy_blob(void)
{
  /* copy the blob to code space, making no effort to align if a double */
  int max = cpc->rnd1, i;
  for (i = 0; i < max; i++)
    fill_a(cpc->arnds[i]);
}


static void
a_fetch_vv(void)
{
  /* the next three instructions must be a get_val, get_val, and BIP */
  if (pass_no == 0) {
    PInstr *p = cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
    p = p->nextInst;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
  } else {
    PInstr *p = cpc->nextInst;
    Ventry *ve;

    c_type = TYPE_XX;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      x1_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      x1_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
    p = p->nextInst;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      x2_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      x2_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
  }
}

static void
a_fetch_vc(void)
{
  /* the next two instructions must be a get_val and BIP */
  if (pass_no == 0) {
    PInstr *p = cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
  } else {
    PInstr *p = cpc->nextInst;
    Ventry *ve;

    c_type = TYPE_XC;
    c_arg = (Int)(cpc->rnd1);
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      x1_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      x1_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
  }
}

static void
a_fetch_cv(void)
{
  /* the next two instructions must be a get_val and BIP */
  if (pass_no == 0) {
    PInstr *p = cpc->nextInst;
    Ventry *ve;
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE != PermVar)
      p->op = nop_op;
  } else {
    PInstr *p = cpc->nextInst;
    Ventry *ve;

    c_type = TYPE_CX;
    c_arg = (Int)(cpc->rnd1);
    ve = (Ventry *) p->rnd1;
    if (ve->KindOfVE == PermVar) {
      /* don't get rid of get_val_op */
      x1_arg = emit_x(p->rnd2);
    } else {
      /* and use it directly as a second argument */
      x1_arg = emit_x(ve->NoOfVE & MaskVarAdrs);
    }
  }
}

static void
a_f2(int var)
{
  Int opc = cpc->rnd2;
  Ventry *ve = (Ventry *)(cpc->rnd1);
  int is_y_var = (ve->KindOfVE == PermVar);

  if (opc <= _primitive) {
    if (is_y_var) {
      if (pass_no) {
	code_p->u.y.y = emit_y(ve);
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
      }
      GONEXT(y);
      return;
    } else {
      if (pass_no) {
	code_p->u.x.x = emit_x(ve->NoOfVE & MaskVarAdrs);
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
      }
      GONEXT(x);
      return;
    }
  }
  if (opc == _functor && cpc->nextInst->op == f_var_op) {
    Ventry *nve;

    cpc = cpc->nextInst;
    nve = (Ventry *)(cpc->rnd1);
    if (is_y_var) {
      if (nve->KindOfVE == PermVar) {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_yy);
	  code_p->u.yyx.y1 = emit_y(ve);
	  code_p->u.yyx.y2 = emit_y(nve);
	  code_p->u.yyx.x = x1_arg;
	}
	GONEXT(yyx);
	return;
      } else {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_yx);
	  code_p->u.yxx.y = emit_y(ve);
	  code_p->u.yxx.x1 = emit_x(nve->NoOfVE & MaskVarAdrs);
	  code_p->u.yxx.x2 = x1_arg;
	}
	GONEXT(yxx);
	return;
      }
    } else {
      if (nve->KindOfVE == PermVar) {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_xy);
	  code_p->u.xyx.x1 = emit_x(ve->NoOfVE & MaskVarAdrs);
	  code_p->u.xyx.y2 = emit_y(nve);
	  code_p->u.xyx.x = x1_arg;
	}
	GONEXT(xyx);
	return;
      } else {
	if (pass_no) {
	  code_p->opc = emit_op(_p_func2f_xx);
	  code_p->u.xxx.x1 = emit_x(ve->NoOfVE & MaskVarAdrs);
	  code_p->u.xxx.x2 = emit_x(nve->NoOfVE & MaskVarAdrs);
	  code_p->u.xxx.x = x1_arg;
	}
	GONEXT(xxx);
	return;
      }
    }
  }
  if (is_y_var) {
    switch (c_type) {
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
	code_p->u.yxx.x1 = x1_arg;
	code_p->u.yxx.x2 = x2_arg;
      }
      GONEXT(yxx);
      break;
    case TYPE_CX:
      if (pass_no) {
	switch (opc) {
	case _plus:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for +/2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _minus:
	  code_p->opc = emit_op(_p_minus_y_cv);
	  break;
	case _times:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for */2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _div:
	  code_p->opc = emit_op(_p_div_y_cv);
	  break;
	case _and:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for /\\/2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _or:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for \\//2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _sll:
	  code_p->opc = emit_op(_p_sll_y_cv);
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _slr:
	  code_p->opc = emit_op(_p_slr_y_cv);
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _arg:
	  code_p->opc = emit_op(_p_arg_y_cv);
	  break;
	case _functor:
	  code_p->opc = emit_op(_p_func2s_y_cv);
	  break;
	}
	code_p->u.ycx.y = emit_y(ve);
	code_p->u.ycx.c = c_arg;
	code_p->u.ycx.xi = x1_arg;
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
	  Error(SYSTEM_ERROR, x2_arg, "internal assembler error XC for -/2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
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
	  Error(SYSTEM_ERROR, x2_arg, "internal assembler error for arg/3");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _functor:
	  code_p->opc = emit_op(_p_func2s_y_vc);
	  break;
	}
	code_p->u.yxc.y = emit_y(ve);
	code_p->u.yxc.c = c_arg;
	code_p->u.yxc.xi = x1_arg;
      }
      GONEXT(yxc);
      break;	  
    }
  } else {
    switch (c_type) {
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
	code_p->u.xxx.x1 = x1_arg;
	code_p->u.xxx.x2 = x2_arg;
      }
      GONEXT(xxx);
      break;
    case TYPE_CX:
      if (pass_no) {
	switch (opc) {
	case _plus:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for +/2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _minus:
	  code_p->opc = emit_op(_p_minus_cv);
	  break;
	case _times:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for */2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _div:
	  code_p->opc = emit_op(_p_div_cv);
	  break;
	case _and:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for /\\/2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _or:
	  Error(SYSTEM_ERROR, x1_arg, "internal assembler error CX for \\//2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
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
	code_p->u.xxc.c = c_arg;
	code_p->u.xxc.xi = x1_arg;
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
	  Error(SYSTEM_ERROR, x2_arg, "internal assembler error XC for -/2");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
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
	  Error(SYSTEM_ERROR, x2_arg, "internal assembler error for arg/3");
	  save_machine_regs();
	  longjmp(CompilerBotch, 1);
	  break;
	case _functor:
	  code_p->opc = emit_op(_p_func2s_vc);
	  break;
	}
	code_p->u.xcx.x = emit_x(ve->NoOfVE & MaskVarAdrs);
	code_p->u.xcx.c = c_arg;
	code_p->u.xcx.xi = x1_arg;
      }
      GONEXT(xcx);
      break;	  
    }
  }
}

#define TRYOP(G,P)   (IPredArity<5 ? (op_numbers)((int)(P)+(IPredArity*3)) : (G))
#ifdef YAPOR
#define TRYCODE(G,P) a_try(TRYOP(G,P), Unsigned(code_addr) + label_offset[cpc->rnd1], IPredArity, cpc->rnd2 >> 1, cpc->rnd2 & 1);
#else
#define TRYCODE(G,P) a_try(TRYOP(G,P), Unsigned(code_addr) + label_offset[cpc->rnd1], IPredArity);
#endif /* YAPOR */

static void
do_pass(void)
{
#ifdef YAPOR
#define EITHER_INST 50
  yamop *entry_code;
  yamop *either_inst[EITHER_INST];
  int either_cont = 0;
#endif	/* YAPOR */
  int log_update;
#if defined(YAPOR) || defined(THREADS)
  int dynamic;
#endif
  int ystop_found = FALSE;

  alloc_found = dealloc_found = FALSE;
  code_p = (yamop *) code_addr;
  cpc = CodeStart;
  comit_lab = 0L;
  /* Space while for the clause flags */
  log_update = CurrentPred->PredFlags & LogUpdatePredFlag;
#if defined(YAPOR) || defined(THREADS)
  dynamic = CurrentPred->PredFlags & DynamicPredFlag;
#endif
  if (assembling != ASSEMBLING_INDEX) {
    Clause *cl_p = (Clause *)code_p;
    if (pass_no) {
      cl_p->u.ClValue = c_store;
      cl_p->ClFlags = c_mask;
      if (log_update)
	cl_p->ClFlags |= LogUpdMask;
      if (clause_has_blobs) {
	cl_p->ClFlags |= HasBlobsMask;
      }
      cl_p->u2.ClExt = NULL;
      cl_p->Owner = YapConsultingFile();
    }
    code_p = (yamop *)(cl_p->ClCode);
    IPredArity = cpc->rnd2;	/* number of args */
#if defined(YAPOR) || defined(THREADS)
    if ((dynamic||log_update) && pass_no) {
      INIT_LOCK(cl_p->ClLock);
      INIT_CLREF_COUNT(cl_p);
    }
#endif
#ifdef YAPOR
    entry_code = code_p;
    a_try(TRYOP(_try_me, _try_me0), 0, IPredArity, 1, 0);
#else
    a_try(TRYOP(_try_me, _try_me0), 0, IPredArity);
#endif	/* YAPOR */
  } else {
    Clause *cl_p = (Clause *)code_p;
    if (pass_no) {
      cl_p->u.ClValue = TermNil;
      if (log_update) {
	cl_p->u2.ClUse =  0;
	cl_p->ClFlags = LogUpdatePredFlag|IndexedPredFlag|IndexMask;
      } else {
	cl_p->u2.ClExt = NULL;
	cl_p->ClFlags = c_mask|IndexMask;
      }
      cl_p->Owner = CurrentPred->OwnerFile;
    }
    code_p = (yamop *)(cl_p->ClCode);
#if defined(YAPOR) || defined(THREADS)
    if ((dynamic||log_update) && pass_no) {
      INIT_LOCK(cl_p->ClLock);
      INIT_CLREF_COUNT(cl_p);
    }
#endif
#ifdef YAPOR
    entry_code = code_p;
#endif
  }
  while (cpc) {

    switch ((int) cpc->op) {
#ifdef YAPOR
    case sync_op:
      a_try(_sync, cpc->rnd1, cpc->rnd2, 1, Zero);
      break;
#endif /* YAPOR */
#ifdef TABLING
    case table_new_answer_op:
      a_n(_table_new_answer, (int) cpc->rnd2);
      break;
#endif /* TABLING */
#ifdef SFUNC
    case get_s_f_op:
      a_rf(_get_s_f);
      break;
    case put_s_f_op:
      a_rf(_put_s_f);
      break;
    case unify_s_f_op:
      a_d(_unify_s_f);
      break;
    case write_s_f_op:
      a_f(_write_s_f);
      break;
    case unify_s_var_op:
      a_vsf(_unify_s_xvar);
      break;
    case write_s_var_op:
      a_vsf(_write_s_xvar);
      break;
    case unify_s_val_op:
      a_vsf(_unify_s_xval);
      break;
    case write_s_val_op:
      a_vsf(_write_s_xval);
      break;
    case unify_s_a_op:
      a_asf(_unify_s_a);
      break;
    case write_s_a_op:
      a_asf(_write_s_a);
      break;
    case get_s_end_op:
      a_n(_get_s_end, Unsigned(0));
      break;
    case put_s_end_op:
      a_n(_put_s_end, Unsigned(0));
      break;
    case unify_s_end_op:
      a_n(_write_s_end, Unsigned(0));
      break;
    case write_s_end_op:
      a_n(_write_s_end, Unsigned(0));
      break;
#endif
    case get_var_op:
      a_vr(_get_x_var);
      break;
    case put_var_op:
      a_vr(_put_x_var);
      break;
    case get_val_op:
      a_vr(_get_x_val);
      break;
    case put_val_op:
      a_vr(_put_x_val);
      break;
    case get_num_op:
    case get_atom_op:
      a_rc(_get_atom);
      break;
    case get_float_op:
      a_rb(_get_float);
      break;
    case get_longint_op:
      a_rb(_get_longint);
      break;
    case get_bigint_op:
      a_rb(_get_bigint);
      break;
    case put_num_op:
    case put_atom_op:
      a_rc(_put_atom);
      break;
    case put_float_op:
    case put_longint_op:
    case put_bigint_op:
      a_rb(_put_atom);
      break;
    case get_list_op:
      a_glist();
      break;
    case put_list_op:
      a_r(_put_list);
      break;
    case get_struct_op:
      a_rf(_get_struct);
      break;
    case put_struct_op:
      a_rf(_put_struct);
      break;
    case put_unsafe_op:
      a_vr((op_numbers)((int)_put_unsafe - 1));
      break;
    case unify_var_op:
      a_uvar();
      break;
    case unify_last_var_op:
      a_uv(_unify_l_x_var, _unify_l_x_var_write);
      break;
    case write_var_op:
      a_wvar();
      break;
    case unify_local_op:
      a_uv(_unify_x_loc, _unify_x_loc_write);
      break;
    case unify_val_op:
      a_uv(_unify_x_val, _unify_x_val_write);
      break;
    case unify_last_local_op:
      a_uv(_unify_l_x_loc, _unify_l_x_loc_write);
      break;
    case unify_last_val_op:
      a_uv(_unify_l_x_val, _unify_l_x_val_write);
      break;
    case write_local_op:
      a_v(_write_x_loc);
      break;
    case write_val_op:
      a_v(_write_x_val);
      break;
    case unify_num_op:
    case unify_atom_op:
      a_ucons(unify_atom_op);
      break;
    case unify_float_op:
      a_ublob(_unify_float, _unify_atom_write);
      break;
    case unify_longint_op:
      a_ublob(_unify_longint, _unify_atom_write);
      break;
    case unify_bigint_op:
      a_ublob(_unify_bigint, _unify_atom_write);
      break;
    case unify_last_num_op:
    case unify_last_atom_op:
      a_uc(_unify_l_atom, _unify_l_atom_write);
      break;
    case unify_last_float_op:
      a_ublob(_unify_l_float, _unify_l_atom_write);
      break;
    case unify_last_longint_op:
      a_ublob(_unify_l_longint, _unify_l_atom_write);
      break;
    case unify_last_bigint_op:
      a_ublob(_unify_l_bigint, _unify_l_atom_write);
      break;
    case write_num_op:
    case write_atom_op:
      a_ucons(write_atom_op);
      break;
    case write_float_op:
    case write_longint_op:
    case write_bigint_op:
      a_blob(_write_atom);
      break;
    case unify_list_op:
      a_ue(_unify_list, _unify_list_write);
      break;
    case unify_last_list_op:
      a_ue(_unify_l_list, _unify_l_list_write);
      break;
    case write_list_op:
      a_e(_write_list);
      break;
    case write_last_list_op:
      a_e(_write_l_list);
      break;
    case unify_struct_op:
      a_uf(_unify_struct, _unify_struct_write);
      break;
    case unify_last_struct_op:
      a_uf(_unify_l_struc, _unify_l_struc_write);
      break;
    case write_struct_op:
      a_f(_write_struct);
      break;
    case write_last_struct_op:
      a_f(_write_l_struc);
      break;
    case save_b_op:
    case patch_b_op:
      a_v(_save_b_x);
      break;
    case comit_b_op:
      a_v(_comit_b_x);
    #ifdef YAPOR
      if (pass_no)
	PUT_YAMOP_CUT(entry_code);
    #endif /* YAPOR */
      break;
    case save_pair_op:
      a_uv(_save_pair_x, _save_pair_x_write);
      break;
    case save_appl_op:
      a_uv(_save_appl_x, _save_appl_x_write);
      break;
    case fail_op:
      a_e(_op_fail);
      break;
    case cut_op:
      a_cut();
#ifdef YAPOR
      if (pass_no)
	PUT_YAMOP_CUT(entry_code);
#endif	/* YAPOR */
      break;
    case cutexit_op:
      a_cut();
      a_e(_procceed);
#ifdef YAPOR
      if (pass_no)
	PUT_YAMOP_CUT(entry_code);
#endif	/* YAPOR */
      break;
    case allocate_op:
      alloc_found = 2;
      break;
    case deallocate_op:
      a_deallocate();
      break;
    case tryme_op:
      TRYCODE(_try_me, _try_me0);
      break;
    case retryme_op:
      TRYCODE(_retry_me, _retry_me0);
      break;
    case trustme_op:
      TRYCODE(_trust_me, _trust_me0);
      break;
    case try_op:
      if (log_update)
	a_cl(_try_logical_pred);
      a_gl(_try_clause);
      break;
    case retry_op:
      a_gl(_retry);
      break;
    case trust_op:
      if (log_update)
	a_cl(_trust_logical_pred);
      a_gl(_trust);
      break;
    case tryin_op:
      a_igl(_try_in);
      break;
    case retryin_op:
      a_gl(_retry);
      break;
    case trustin_op:
      a_gl_in(_trust_in);
      break;
    case tryf_op:
      if (log_update)
	a_cl(_try_logical_pred);
      a_gl(_try_clause);
      break;
    case retryf_op:
      a_gl(_retry_first);
      break;
    case trustf_op:
      if (log_update)
	a_cl(_trust_logical_pred);
      a_gl(_trust_first);
      break;
    case tryfin_op:
      a_igl(_try_in);
      break;
    case retryfin_op:
      a_gl(_retry_first);
      break;
    case trustfin_op:
      a_gl_in(_trust_first_in);
      break;
    case tryt_op:
      if (log_update)
	a_cl(_try_logical_pred);
      a_gl(_try_clause);
      break;
    case retryt_op:
      a_gl(_retry_tail);
      break;
    case trustt_op:
      if (log_update)
	a_cl(_trust_logical_pred);
      a_gl(_trust_tail);
      break;
    case trytin_op:
      a_igl(_try_in);
      break;
    case retrytin_op:
      a_gl(_retry_tail);
      break;
    case trusttin_op:
      a_gl_in(_trust_tail_in);
      break;
    case tryh_op:
      if (log_update)
	a_cl(_try_logical_pred);
      a_gl(_try_clause);
      break;
    case retryh_op:
      a_gl(_retry_head);
      break;
    case trusth_op:
      if (log_update)
	a_cl(_trust_logical_pred);
      a_gl(_trust_head);
      break;
    case tryhin_op:
      a_igl(_try_in);
      break;
    case retryhin_op:
      a_gl(_retry_head);
      break;
    case trusthin_op:
      a_gl_in(_trust_head_in);
      break;
    case trylf_op:
      /* now that we don't need to save the arguments this is just a
	 simple retry */
      a_gl(_retry);
      break;
      /* ibd */
    case trylh_op:
      a_gl(_retry);
      break;
    case jump_op:
      a_l(_jump);
      break;
    case restore_tmps_op:
      a_l(_move_back);
      break;
    case restore_tmps_and_skip_op:
      a_l(_skip);
      break;
    case procceed_op:
      a_e(_procceed);
      break;
    case call_op:
      a_p(_call);
      break;
    case execute_op:
      a_p(_execute);
      break;
    case safe_call_op:
      a_p(_call);
      break;
    case label_op:
      if (!ystop_found &&
	  cpc->nextInst != NULL &&
	  (cpc->nextInst->op == mark_initialised_pvars_op ||
	   cpc->nextInst->op == blob_op)) {
	ystop_found = TRUE;
	a_e(_Ystop);
      }
      if (!pass_no) {
	if (CellPtr(label_offset+cpc->rnd1) > ASP-256) {
	  save_machine_regs();
	  longjmp(CompilerBotch,3);	  
	}
	
	if ( (char *)(label_offset+cpc->rnd1) > freep)
	  freep = (char *)(label_offset+(cpc->rnd1+1));
	label_offset[cpc->rnd1] = (CELL) code_p;
      }
      /* reset dealloc_found in case there was a branch */
      dealloc_found = FALSE;
      break;
    case pop_op:
      if (cpc->rnd1 == 1)
	a_e(_pop);
      else {
	a_n(_pop_n, 2 * CELLSIZE * (cpc->rnd1 - 1));
      }
      break;
    case either_op:
      check_alloc();
#ifdef YAPOR
      if (pass_no)
	either_inst[either_cont++] = code_p;
      a_either(_either,
	       -Signed(RealEnvSize) - CELLSIZE * cpc->rnd2,
	       Unsigned(code_addr) + label_offset[cpc->rnd1], 0, 0);
#else
      a_either(_either,
	       -Signed(RealEnvSize) - CELLSIZE * cpc->rnd2,
	       Unsigned(code_addr) + label_offset[cpc->rnd1]);
#endif	/* YAPOR */
      break;
    case orelse_op:
#ifdef YAPOR
      if (pass_no)
	either_inst[either_cont++] = code_p;
      a_either(_or_else,
	       -Signed(RealEnvSize) - CELLSIZE * cpc->rnd2,
	       Unsigned(code_addr) + label_offset[cpc->rnd1], 0, 0);
#else
      a_either(_or_else,
	       -Signed(RealEnvSize) - CELLSIZE * cpc->rnd2,
	       Unsigned(code_addr) + label_offset[cpc->rnd1]);
#endif	/* YAPOR */
      dealloc_found = FALSE;
      break;
    case orlast_op:
#ifdef YAPOR
      if (pass_no)
	either_inst[either_cont++] = code_p;
      a_either(_or_last, 0, 0, 0, 0);
      if (pass_no) {
	int cont = 1;
	do {
	  either_cont--;
	  PUT_YAMOP_LTT(either_inst[either_cont], cont++);
	} while (either_inst[either_cont]->opc != opcode(_either));
      }
#else
      a_e(_or_last);
#endif	/* YAPOR */
      dealloc_found = FALSE;
      break;
    case jump_v_op:
      a_igl(_jump_if_var);
      break;
    case switch_t_op:
      a_4sw(_switch_on_type);
      break;
    case switch_nv_op:
      a_3sw(_switch_on_nonv);
      break;
    case switch_l_op:
      a_3sws(_switch_last);
      break;
    case switch_h_op:
      a_4sw(_switch_on_head);
      break;
    case switch_lnl_op:
#if USE_THREADED_CODE
      a_4_lsw(_switch_list_nl);
#else
      a_4sw(_switch_list_nl);
#endif
      break;
    case switch_nvl_op:
      a_3sw(_switch_nv_list);
      break;
    case switch_ll_op:
      a_3sws(_switch_l_list);
      break;
    case switch_c_op:
      a_hx(_switch_on_cons);
      break;
    case switch_f_op:
      a_hx(_switch_on_func);
      break;
    case if_c_op:
      a_if(_if_cons);
      break;
    case if_f_op:
      a_if(_if_func);
      break;
    case go_c_op:
      a_go(_go_on_cons);
      break;
    case go_f_op:
      a_go(_go_on_func);
      break;
    case if_not_op:
      a_go(_if_not_then);
      break;
    case mark_initialised_pvars_op:
      a_bmap();
      break;
    case mark_live_regs_op:
      a_bregs();
      break;
    case comit_opt_op:
      comit_lab = cpc->rnd1;
      break;
    case fetch_args_vv_op:
      a_fetch_vv();
      break;
    case fetch_args_vc_op:
      a_fetch_vc();
      break;
    case fetch_args_cv_op:
      a_fetch_cv();
      break;
    case f_val_op:
      a_f2(FALSE);
      break;
    case f_var_op:
      a_f2(TRUE);
      break;
    case enter_profiling_op:
      a_pl(_enter_profiling, (PredEntry *)(cpc->rnd1));
      break;
    case retry_profiled_op:
      a_pl(_retry_profiled, (PredEntry *)(cpc->rnd1));
      break;
    case fetch_args_for_bccall:
      if (cpc->nextInst->op != bccall_op) {
	Error(SYSTEM_ERROR, TermNil, "compiling binary test", (int) cpc->op);
	save_machine_regs();
	longjmp(CompilerBotch, 1);
      }
      a_bfunc(cpc->nextInst->rnd2);
      break;
    case blob_op:
      /* install a blob */
      copy_blob();
      break;
    case empty_call_op:
      /* create an empty call */
      a_empty_call();
      break;
    case push_or_op:
    case pop_or_op:
    case pushpop_or_op:
    case nop_op:
    case name_op:
      break;
    default:
      Error(SYSTEM_ERROR, TermNil, "instruction %d found while assembling", (int) cpc->op);
      save_machine_regs();
      longjmp(CompilerBotch, 1);
    }
    cpc = cpc->nextInst;
  }
  if (!ystop_found)
    a_e(_Ystop);
}

CODEADDR
assemble(int mode)
{
  /*
   * the assembly proccess is done in two passes: 1 - a first pass
   * computes labels offsets and total code size 2 - the second pass
   * produces the final version of the code 
   */
  CELL size;

  code_addr = NIL;
  assembling = mode;
  clause_has_blobs = FALSE;
  label_offset = (int *)freep;
  pass_no = 0;
  asm_error = FALSE;
  do_pass();
  if (asm_error) {
    Error_TYPE = SYSTEM_ERROR;
    ErrorMessage = "internal assembler error";
    return (NIL);
  }
  pass_no = 1;
  YAPEnterCriticalSection();
#ifdef KEEP_ENTRY_AGE
  {
    size =
      (CELL)NEXTOP(NEXTOP(NEXTOP((yamop *)(((Clause *)NULL)->ClCode),ld),sla),e);
    if ((CELL)code_p > size)
      size = (CELL)code_p;
  }
#else
  size = (CELL)code_p;
#endif
  while ((code_addr = (CODEADDR) AllocCodeSpace(size)) == NULL) {
    growheap(TRUE);
  }
  do_pass();
  YAPLeaveCriticalSection();
  {
    Clause *cl = (Clause *)code_addr;  /* lcc, why? */
    return((CODEADDR)(cl->ClCode));
  }
}

