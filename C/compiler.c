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
* File:		compiler.c						 *
* comments:	Clause compiler						 *
*									 *
* Last rev:     $Date: 2008-08-06 17:32:18 $,$Author: vsc $
**
* $Log: not supported by cvs2svn $
* Revision 1.88  2008/03/13 14:37:58  vsc
* update chr
*
* Revision 1.87  2007/12/18 17:46:58  vsc
* purge_clauses does not need to do anything if there are no clauses
* fix gprof bugs.
*
* Revision 1.86  2007/11/26 23:43:08  vsc
* fixes to support threads and assert correctly, even if inefficiently.
*
* Revision 1.85  2007/11/06 17:02:11  vsc
* compile ground terms away.
*
* Revision 1.84  2007/03/27 13:48:51  vsc
* fix number of overflows (comments by Bart Demoen).
*
* Revision 1.83  2007/03/26 15:18:43  vsc
* debugging and clause/3 over tabled predicates would kill YAP.
*
* Revision 1.82  2006/11/06 18:35:03  vsc
* 1estranha
*
* Revision 1.81  2006/10/11 15:08:03  vsc
* fix bb entries
* comment development code for timestamp overflow.
*
* Revision 1.80  2006/09/20 20:03:51  vsc
* improve indexing on floats
* fix sending large lists to DB
*
* Revision 1.79  2006/08/01 13:14:17  vsc
* fix compilation of |
*
* Revision 1.78  2006/07/27 19:04:56  vsc
* fix nasty overflows in and add some very preliminary support for very large
* clauses with lots
* of disjuncts (eg, query packs).
*
* Revision 1.77  2006/05/19 14:31:31  vsc
* get rid of IntArrays and FloatArray code.
* include holes when calculating memory usage.
*
* Revision 1.76  2006/05/19 13:48:11  vsc
* help to make Yap work with dynamic libs
*
* Revision 1.75  2006/05/16 18:37:30  vsc
* WIN32 fixes
* compiler bug fixes
* extend interface
*
* Revision 1.74  2006/04/13 02:04:24  vsc
* fix debugging typo
*
* Revision 1.73  2006/04/12 20:08:51  vsc
* make it sure that making vars safe does not propagate across branches of
*disjunctions.
*
* Revision 1.72  2006/04/05 00:16:54  vsc
* Lots of fixes (check logfile for details
*
* Revision 1.71  2006/03/24 17:13:41  rslopes
* New update to BEAM engine.
* BEAM now uses YAP Indexing (JITI)
*
* Revision 1.70  2005/12/17 03:25:39  vsc
* major changes to support online event-based profiling
* improve error discovery and restart on scanner.
*
* Revision 1.69  2005/09/08 22:06:44  rslopes
* BEAM for YAP update...
*
* Revision 1.68  2005/07/06 15:10:03  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.67  2005/05/25 21:43:32  vsc
* fix compiler bug in 1 << X, found by Nuno Fonseca.
* compiler internal errors get their own message.
*
* Revision 1.66  2005/05/12 03:36:32  vsc
* debugger was making predicates meta instead of testing
* fix handling of dbrefs in facts and in subarguments.
*
* Revision 1.65  2005/04/10 04:01:10  vsc
* bug fixes, I hope!
*
* Revision 1.64  2005/03/13 06:26:10  vsc
* fix excessive pruning in meta-calls
* fix Term->int breakage in compiler
* improve JPL (at least it does something now for amd64).
*
* Revision 1.63  2005/03/04 20:30:11  ricroc
* bug fixes for YapTab support
*
* Revision 1.62  2005/02/21 16:49:39  vsc
* amd64 fixes
* library fixes
*
* Revision 1.61  2005/01/28 23:14:35  vsc
* move to Yap-4.5.7
* Fix clause size
*
* Revision 1.60  2005/01/14 20:55:16  vsc
* improve register liveness calculations.
*
* Revision 1.59  2005/01/04 02:50:21  vsc
* - allow MegaClauses with blobs
* - change Diffs to be thread specific
* - include Christian's updates
*
* Revision 1.58  2005/01/03 17:06:03  vsc
* fix discontiguous stack overflows in parser
*
* Revision 1.57  2004/12/20 21:44:57  vsc
* more fixes to CLPBN
* fix some Yap overflows.
*
* Revision 1.56  2004/12/16 05:57:32  vsc
* fix overflows
*
* Revision 1.55  2004/12/05 05:01:23  vsc
* try to reduce overheads when running with goal expansion enabled.
* CLPBN fixes
* Handle overflows when allocating big clauses properly.
*
* Revision 1.54  2004/11/19 22:08:41  vsc
* replace SYSTEM_ERROR_INTERNAL by out OUT_OF_WHATEVER_ERROR whenever
*appropriate.
*
* Revision 1.53  2004/09/03 03:11:08  vsc
* memory management fixes
*
* Revision 1.52  2004/07/15 17:20:23  vsc
* fix error message
* change makefile and configure for clpbn
*
* Revision 1.51  2004/06/29 19:04:41  vsc
* fix multithreaded version
* include new version of Ricardo's profiler
* new predicat atomic_concat
* allow multithreaded-debugging
* small fixes
*
* Revision 1.50  2004/04/22 20:07:04  vsc
* more fixes for USE_SYSTEM_MEMORY
*
* Revision 1.49  2004/03/10 16:27:39  vsc
* skip compilation steps for ground facts.
*
* Revision 1.48  2004/03/08 19:31:01  vsc
* move to 4.5.3
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif /* SCCS */
#include "Yap.h"
#include "alloc.h"
#include "clause.h"
#include "YapCompile.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef BEAM
extern int EAM;
// extern PInstr *CodeStart, *ppc, *ppc1, *BodyStart, *ppc_body;
#endif

typedef struct branch_descriptor {
  int id;  /* the branch id */
  Term cm; /* if a banch is associated with a commit */
} branch;

typedef struct compiler_struct_struct {
  branch parent_branches[256];
  branch *branch_pointer;
  PInstr *BodyStart;
  Ventry *vtable;
  CExpEntry *common_exps;
  int is_a_fact;
  int hasdbrefs;
  int n_common_exps;
  int goalno;
  int onlast;
  int onhead;
  int onbranch;
  int curbranch;
  Int space_used;
  PInstr *space_op;
  Prop current_p0;
#ifdef TABLING_INNER_CUTS
  PInstr *cut_mark;
#endif /* TABLING_INNER_CUTS */
#ifdef DEBUG
  int pbvars;
#endif /* DEBUG */
  int nvars;
  UInt labelno;
  int or_found;
  UInt max_args;
  int MaxCTemps;
  UInt tmpreg;
  Int vreg;
  Int vadr;
  Int *Uses;
  Term *Contents;
  Term body;
  int needs_env;
  CIntermediates cint;
} compiler_struct;

static int active_branch(int, int);
static void c_var(Term, Int, unsigned int, unsigned int, compiler_struct *);
static void reset_vars(Ventry *);
static Term optimize_ce(Term, unsigned int, unsigned int, compiler_struct *);
static void c_arg(Int, Term, unsigned int, unsigned int, compiler_struct *);
static void c_args(Term, unsigned int, compiler_struct *);
static void c_eq(Term, Term, compiler_struct *);
static void c_test(Int, Term, compiler_struct *);
static void c_bifun(basic_preds, Term, Term, Term, Term, Term,
                    compiler_struct *);
static void c_goal(Term, Term, compiler_struct *);
static void c_body(Term, Term, compiler_struct *);
static void c_head(Term, compiler_struct *);
static bool usesvar(compiler_vm_op);
static CELL *init_bvarray(int, compiler_struct *);
#ifdef DEBUG
static void clear_bvarray(int, CELL *, compiler_struct *);
#else
static void clear_bvarray(int, CELL *);
#endif
static void add_bvarray_op(PInstr *, CELL *, int, compiler_struct *);
static void AssignPerm(PInstr *, compiler_struct *);
static void CheckUnsafe(PInstr *, compiler_struct *);
static void CheckVoids(compiler_struct *);
static int checktemp(Int, Int, compiler_vm_op, compiler_struct *);
static Int checkreg(Int, Int, compiler_vm_op, int, compiler_struct *);
static void c_layout(compiler_struct *);
static void c_optimize(PInstr *);
#ifdef SFUNC
static void compile_sf_term(Term, int);
#endif

static void push_branch(int id, Term cmvar, compiler_struct *cglobs) {
  cglobs->branch_pointer->id = id;
  cglobs->branch_pointer->cm = cmvar;
  cglobs->branch_pointer++;
}

static int pop_branch(compiler_struct *cglobs) {
  cglobs->branch_pointer--;
  return (cglobs->branch_pointer->id);
}

#ifdef TABLING
#define is_tabled(pe) (pe->PredFlags & TabledPredFlag)
#endif /* TABLING */

static inline int active_branch(int i, int onbranch) {
  /*  register int *bp;*/

  return (i == onbranch);
  /*  bp = cglobs->branch_pointer;
  while (bp > parent_branches) {
    if (*--bp == onbranch)
      return (TRUE);
  }
  return(i==onbranch);*/
}

#define FAIL(M, T, E)                                                          \
  {                                                                            \
    LOCAL_Error_TYPE = T;                                                      \
    return;                                                                    \
  }

#if USE_SYSTEM_MALLOC
#define IsNewVar(v) ((CELL *)(v) >= H0 && (CELL *)(v) < LCL0)
#else
#define IsNewVar(v)                                                            \
  (Addr(v) < cglobs->cint.freep0 || Addr(v) > cglobs->cint.freep)
#endif

inline static void pop_code(unsigned int, compiler_struct *);

inline static void pop_code(unsigned int level, compiler_struct *cglobs) {
  if (level == 0)
    return;
  if (cglobs->cint.cpc->op == pop_op)
    ++(cglobs->cint.cpc->rnd1);
  else {
    Yap_emit(pop_op, One, Zero, &cglobs->cint);
  }
}

static void adjust_current_commits(compiler_struct *cglobs) {
  branch *bp = cglobs->branch_pointer;
  while (bp > cglobs->parent_branches) {
    bp--;
    if (bp->cm != TermNil) {
      c_var(bp->cm, patch_b_flag, 1, 0, cglobs);
    }
  }
}

static int check_var(Term t, unsigned int level, Int argno,
                     compiler_struct *cglobs) {
  CACHE_REGS
  int flags, new = FALSE;
  Ventry *v = (Ventry *)t;

  if (IsNewVar(v)) { /* new var */
    v = (Ventry *)Yap_AllocCMem(sizeof(*v), &cglobs->cint);
#if YAPOR_SBA
    v->SelfOfVE = 0;
#else
    v->SelfOfVE = (CELL)v;
#endif
    v->AdrsOfVE = t;
    *CellPtr(t) = (CELL)v;
    v->KindOfVE = v->NoOfVE = Unassigned;
    flags = 0;
    /* Be careful with eithers. I may make a variable global in a branch,
       and not in another.
       a :- (b([X]) ; c), go(X).
       This variable will not be globalised if we are coming from
       the second branch.

       I also need to protect the onhead because Luis uses that to
       optimise unification in the body of a clause, eg
       a :- (X = 2 ; c), go(X).

       And, yes, there is code like this...
     */
    if (((level > 0 || cglobs->onhead) && cglobs->curbranch == 0) ||
        argno == save_pair_flag || argno == save_appl_flag)
      flags |= SafeVar;
    if ((level > 0 && cglobs->curbranch == 0) || argno == save_pair_flag ||
        argno == save_appl_flag)
      flags |= GlobalVal;
    v->FlagsOfVE = flags;
    v->BranchOfVE = cglobs->onbranch;
    v->NextOfVE = cglobs->vtable;
    v->RCountOfVE = 0;
    v->AgeOfVE = v->FirstOfVE = cglobs->goalno;
    new = TRUE;
    cglobs->vtable = v;
  } else {
    v->FlagsOfVE |= NonVoid;
    if (v->BranchOfVE > 0) {
      if (!active_branch(v->BranchOfVE, cglobs->onbranch)) {
        v->AgeOfVE = v->FirstOfVE = 1;
        new = FALSE;
        v->FlagsOfVE |= BranchVar;
        /* set the original instruction correctly */
        switch (v->FirstOpForV->op) {
        case get_var_op:
          v->FirstOpForV->op = get_val_op;
          break;
        case unify_var_op:
          v->FirstOpForV->op = unify_val_op;
          break;
        case unify_last_var_op:
          v->FirstOpForV->op = unify_last_val_op;
          break;
        case put_var_op:
          v->FirstOpForV->op = put_val_op;
          break;
        case write_var_op:
          v->FirstOpForV->op = write_val_op;
          break;
        default:
          break;
        }
      }
    }
  }
  if (cglobs->onhead)
    v->FlagsOfVE |= OnHeadFlag;
  return new;
}

static void tag_var(Term t, int new, compiler_struct *cglobs) {
  Ventry *v = (Ventry *)t;

  if (new) {
    v->FirstOpForV = cglobs->cint.cpc;
  }
  v->LastOpForV = cglobs->cint.cpc;
  ++(v->RCountOfVE);
  if (cglobs->onlast)
    v->FlagsOfVE |= OnLastGoal;
  if (v->AgeOfVE < cglobs->goalno)
    v->AgeOfVE = cglobs->goalno;
}

static void c_var(Term t, Int argno, unsigned int arity, unsigned int level,
                  compiler_struct *cglobs) {
  int new = check_var(Deref(t), level, argno, cglobs);
  t = Deref(t);

  switch (argno) {
  case save_b_flag:
    Yap_emit(save_b_op, t, Zero, &cglobs->cint);
    break;
  case commit_b_flag:
    Yap_emit(commit_b_op, t, Zero, &cglobs->cint);
    Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
    Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
    break;
  case patch_b_flag:
    Yap_emit(patch_b_op, t, 0, &cglobs->cint);
    break;
  case save_pair_flag:
    Yap_emit(save_pair_op, t, 0, &cglobs->cint);
    break;
  case save_appl_flag:
    Yap_emit(save_appl_op, t, 0, &cglobs->cint);
    break;
  case f_flag:
    if (new) {
      ++cglobs->nvars;
      Yap_emit(f_var_op, t, (CELL)arity, &cglobs->cint);
    } else {
      Yap_emit(f_val_op, t, (CELL)arity, &cglobs->cint);
    }
    break;
  default:
#ifdef SFUNC
    if (argno < 0) {
      if (new)
        Yap_emit((cglobs->onhead ? unify_s_var_op : write_s_var_op), v, -argno,
                 &cglobs->cint);
      else
        Yap_emit((cglobs->onhead ? unify_s_val_op : write_s_val_op), v, -argno,
                 &cglobs->cint);
    } else
#endif
        if (cglobs->onhead) {
      cglobs->space_used++;
      if (level == 0)
        Yap_emit((new ? (++cglobs->nvars, get_var_op) : get_val_op), t, argno,
                 &cglobs->cint);
      else
        Yap_emit(
            (new ? (++cglobs->nvars,
                    (argno == (Int)arity ? unify_last_var_op : unify_var_op))
                 : (argno == (Int)arity ? unify_last_val_op : unify_val_op)),
            t, Zero, &cglobs->cint);
    } else {
      if (level == 0)
        Yap_emit((new ? (++cglobs->nvars, put_var_op) : put_val_op), t, argno,
                 &cglobs->cint);
      else
        Yap_emit((new ? (++cglobs->nvars, write_var_op) : write_val_op), t,
                 Zero, &cglobs->cint);
    }
  }
  tag_var(t, new, cglobs);
}

// built-in like X >= Y.
static void c_2vars(int op, Term t1, Int argno1, Term t2, Int argno2,
                    CELL extra, unsigned int arity, unsigned int level,
                    compiler_struct *cglobs) {
  int new1 = check_var((t1 = Deref(t1)), level, argno1, cglobs);
  int new2 = check_var((t2 = Deref(t2)), level, argno2, cglobs);

  switch (op) {
  case bt_flag:
    Yap_emit_5ops(bccall_op, t1, argno1, t2, argno2, extra, &cglobs->cint);
    break;
  default:
    return;
  }
  tag_var(t1, new1, cglobs);
  tag_var(t2, new2, cglobs);
}

static void reset_vars(Ventry *vtable) {
  Ventry *v = vtable;
  CELL *t;

  while (v != NIL) {
    t = (CELL *)v->AdrsOfVE;
    RESET_VARIABLE(t);
    v = v->NextOfVE;
  }
}

static Term optimize_ce(Term t, unsigned int arity, unsigned int level,
                        compiler_struct *cglobs) {
  CACHE_REGS
  CExpEntry *p = cglobs->common_exps;
  int cmp = 0;

#ifdef BEAM
  if (EAM)
    return t;
#endif

  if (IsApplTerm(t) && IsExtensionFunctor(FunctorOfTerm(t)))
    return (t);
  while (p != NULL) {
    CELL *oldH = HR;
    HR = (CELL *)cglobs->cint.freep;
    cmp = Yap_compare_terms(t, (p->TermOfCE));
    HR = oldH;

    if (cmp) {
      p = p->NextCE;
    } else {
      break;
    }
  }
  if (p != NULL) { /* already there */
    return (p->VarOfCE);
  }
  /* first occurrence */
  if (cglobs->onbranch || level > 1) {
    return t;
  }
  ++(cglobs->n_common_exps);
  p = (CExpEntry *)Yap_AllocCMem(sizeof(CExpEntry), &cglobs->cint);

  p->TermOfCE = t;
  p->VarOfCE = MkVarTerm();
  if (HR >= (CELL *)cglobs->cint.freep0) {
    /* oops, too many new variables */
    save_machine_regs();
    siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
  }
  p->NextCE = cglobs->common_exps;
  cglobs->common_exps = p;
  if (IsApplTerm(t))
    c_var(p->VarOfCE, save_appl_flag, arity, level, cglobs);
  else if (IsPairTerm(t))
    c_var(p->VarOfCE, save_pair_flag, arity, level, cglobs);
  return (t);
}

#ifdef SFUNC
static void compile_sf_term(Term t, int argno, int level) {
  Functor f = FunctorOfTerm(t);
  CELL *p = ArgsOfSFTerm(t) - 1;
  SFEntry *pe = RepSFProp(Yap_GetAProp(NameOfFunctor(f), SFProperty));
  Term nullvalue = pe->NilValue;

  if (level == 0)
    Yap_emit((cglobs->onhead ? get_s_f_op : put_s_f_op), f, argno,
             &cglobs->cint);
  else
    Yap_emit((cglobs->onhead ? unify_s_f_op : write_s_f_op), f, Zero,
             &cglobs->cint);
  ++level;
  while ((argno = *++p)) {
    t = Derefa(++p);
    if (t != nullvalue) {
      if (IsAtomicTerm(t))
        Yap_emit((cglobs->onhead ? unify_s_a_op : write_s_a_op), t, (CELL)argno,
                 &cglobs->cint);
      else if (!IsVarTerm(t)) {
        LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
        LOCAL_ErrorMessage = "illegal argument of soft functor";
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
      } else
        c_var(t, -argno, arity, level, cglobs);
    }
  }
  --level;
  if (level == 0)
    Yap_emit((cglobs->onhead ? get_s_end_op : put_s_end_op), Zero, Zero,
             &cglobs->cint);
  else
    Yap_emit((cglobs->onhead ? unify_s_end_op : write_s_end_op), Zero, Zero,
             &cglobs->cint);
}
#endif

inline static void c_args(Term app, unsigned int level,
                          compiler_struct *cglobs) {
  CACHE_REGS
  Functor f = FunctorOfTerm(app);
  unsigned int Arity = ArityOfFunctor(f);
  unsigned int i;

  if (level == 0) {
    if (Arity >= MaxTemps) {
      LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
      LOCAL_ErrorMessage = "exceed maximum arity of compiled goal";
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
    }
    if (Arity > cglobs->max_args)
      cglobs->max_args = Arity;
  }
  for (i = 1; i <= Arity; ++i)
    c_arg(i, ArgOfTerm(i, app), Arity, level, cglobs);
}

static int try_store_as_dbterm(Term t, Int argno, unsigned int arity, int level,
                               compiler_struct *cglobs) {
  CACHE_REGS
  DBTerm *dbt;
  int g;
  CELL *h0 = HR;

  while ((g = Yap_SizeGroundTerm(t, TRUE)) < 0) {
    /* oops, too deep a term */
    save_machine_regs();
    LOCAL_Error_Size = 0;
    siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_AUX_BOTCH);
  }
  // if (g < 16)
  return FALSE;
  /* store ground term away */
  HR = CellPtr(cglobs->cint.freep);
  if ((dbt = Yap_StoreTermInDB(t, -1)) == NULL) {
    HR = h0;
    switch (LOCAL_Error_TYPE) {
    case RESOURCE_ERROR_STACK:
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_STACK_BOTCH);
    case RESOURCE_ERROR_TRAIL:
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TRAIL_BOTCH);
    case RESOURCE_ERROR_HEAP:
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH);
    case RESOURCE_ERROR_AUXILIARY_STACK:
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_AUX_BOTCH);
    default:
      siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
    }
  }
  HR = h0;
  if (level == 0)
    Yap_emit((cglobs->onhead ? get_dbterm_op : put_dbterm_op), dbt->Entry,
             argno, &cglobs->cint);
  else
    Yap_emit((cglobs->onhead ? (argno == (Int)arity ? unify_last_dbterm_op
                                                    : unify_dbterm_op)
                             : write_dbterm_op),
             dbt->Entry, Zero, &cglobs->cint);
  return TRUE;
}

static void c_arg(Int argno, Term t, unsigned int arity, unsigned int level,
                  compiler_struct *cglobs) {
restart:
  if (IsVarTerm(t))
    c_var(t, argno, arity, level, cglobs);
  else if (IsAtomTerm(t)) {
    if (level == 0) {
      Yap_emit((cglobs->onhead ? get_atom_op : put_atom_op), (CELL)t, argno,
               &cglobs->cint);
    } else
      Yap_emit((cglobs->onhead
                    ? (argno == (Int)arity ? unify_last_atom_op : unify_atom_op)
                    : write_atom_op),
               (CELL)t, Zero, &cglobs->cint);
  } else if (IsIntegerTerm(t) || IsFloatTerm(t) || IsBigIntTerm(t) ||
             IsStringTerm(t)) {
    if (!IsIntTerm(t)) {
      if (IsFloatTerm(t)) {
        if (level == 0)
          Yap_emit((cglobs->onhead ? get_float_op : put_float_op), t, argno,
                   &cglobs->cint);
        else
          Yap_emit((cglobs->onhead ? (argno == (Int)arity ? unify_last_float_op
                                                          : unify_float_op)
                                   : write_float_op),
                   t, Zero, &cglobs->cint);
      } else if (IsLongIntTerm(t)) {
        if (level == 0)
          Yap_emit((cglobs->onhead ? get_longint_op : put_longint_op), t, argno,
                   &cglobs->cint);
        else
          Yap_emit((cglobs->onhead
                        ? (argno == (Int)arity ? unify_last_longint_op
                                               : unify_longint_op)
                        : write_longint_op),
                   t, Zero, &cglobs->cint);
      } else if (IsStringTerm(t)) {
        /* we are taking a string, that is supposed to be
         guarded in the clause itself. . */
        CELL l1 = ++cglobs->labelno;
        CELL *src = RepAppl(t);
        PInstr *ocpc = cglobs->cint.cpc, *OCodeStart = cglobs->cint.CodeStart;
        Int sz = (3 + src[1]) * sizeof(CELL);
        CELL *dest;

        /* use a special list to store the blobs */
        cglobs->cint.cpc = cglobs->cint.icpc;
        /*      if (IsFloatTerm(t)) {
                Yap_emit(align_float_op, Zero, Zero, &cglobs->cint);
                }*/
        Yap_emit(label_op, l1, Zero, &cglobs->cint);
        dest = Yap_emit_extra_size(blob_op, sz / CellSize, sz, &cglobs->cint);

        /* copy the bignum */
        memmove(dest, src, sz);
        /* note that we don't need to copy size info, unless we wanted
         to garbage collect clauses ;-) */
        cglobs->cint.icpc = cglobs->cint.cpc;
        if (cglobs->cint.BlobsStart == NULL)
          cglobs->cint.BlobsStart = cglobs->cint.CodeStart;
        cglobs->cint.cpc = ocpc;
        cglobs->cint.CodeStart = OCodeStart;
        /* The argument to pass to the structure is now the label for
           where we are storing the blob */
        if (level == 0)
          Yap_emit((cglobs->onhead ? get_string_op : put_string_op), l1, argno,
                   &cglobs->cint);
        else
          Yap_emit((cglobs->onhead ? (argno == (Int)arity ? unify_last_string_op
                                                          : unify_string_op)
                                   : write_string_op),
                   l1, Zero, &cglobs->cint);
      } else {
        /* we are taking a blob, that is a binary that is supposed to be
         guarded in the clause itself. Possible examples include
         floats, long ints, bignums, bitmaps.... */
        CELL l1 = ++cglobs->labelno;
        CELL *src = RepAppl(t);
        PInstr *ocpc = cglobs->cint.cpc, *OCodeStart = cglobs->cint.CodeStart;
        Int sz =
            2 * sizeof(CELL) + sizeof(Functor) + sizeof(MP_INT) +
            ((((MP_INT *)(RepAppl(t) + 2))->_mp_alloc) * sizeof(mp_limb_t));
        CELL *dest;

        /* use a special list to store the blobs */
        cglobs->cint.cpc = cglobs->cint.icpc;
        /*      if (IsFloatTerm(t)) {
                Yap_emit(align_float_op, Zero, Zero, &cglobs->cint);
                }*/
        Yap_emit(label_op, l1, Zero, &cglobs->cint);
        dest = Yap_emit_extra_size(blob_op, sz / CellSize, sz, &cglobs->cint);

        /* copy the bignum */
        memmove(dest, src, sz);
        /* note that we don't need to copy size info, unless we wanted
         to garbage collect clauses ;-) */
        cglobs->cint.icpc = cglobs->cint.cpc;
        if (cglobs->cint.BlobsStart == NULL)
          cglobs->cint.BlobsStart = cglobs->cint.CodeStart;
        cglobs->cint.cpc = ocpc;
        cglobs->cint.CodeStart = OCodeStart;
        /* The argument to pass to the structure is now the label for
           where we are storing the blob */
        if (level == 0)
          Yap_emit((cglobs->onhead ? get_bigint_op : put_bigint_op), l1, argno,
                   &cglobs->cint);
        else
          Yap_emit((cglobs->onhead ? (argno == (Int)arity ? unify_last_bigint_op
                                                          : unify_bigint_op)
                                   : write_bigint_op),
                   l1, Zero, &cglobs->cint);
      }
      /* That's it folks! */
      return;
    }
    if (level == 0)
      Yap_emit((cglobs->onhead ? get_num_op : put_num_op), (CELL)t, argno,
               &cglobs->cint);
    else
      Yap_emit((cglobs->onhead
                    ? (argno == (Int)arity ? unify_last_num_op : unify_num_op)
                    : write_num_op),
               (CELL)t, Zero, &cglobs->cint);
  } else if (IsPairTerm(t)) {
    cglobs->space_used += 2;
    if (optimizer_on && level < 6) {
#if !defined(THREADS) && !defined(YAPOR)
      /* discard code sharing because we cannot write on shared stuff */
      if (FALSE &&
          !(cglobs->cint.CurrentPred->PredFlags &
            (DynamicPredFlag | LogUpdatePredFlag))) {
        if (try_store_as_dbterm(t, argno, arity, level, cglobs))
          return;
      }
#endif
      t = optimize_ce(t, arity, level, cglobs);
      if (IsVarTerm(t)) {
        c_var(t, argno, arity, level, cglobs);
        return;
      }
    }
    if (level == 0)
      Yap_emit((cglobs->onhead ? get_list_op : put_list_op), Zero, argno,
               &cglobs->cint);
    else if (argno == (Int)arity)
      Yap_emit((cglobs->onhead ? unify_last_list_op : write_last_list_op), Zero,
               Zero, &cglobs->cint);
    else
      Yap_emit((cglobs->onhead ? unify_list_op : write_list_op), Zero, Zero,
               &cglobs->cint);
    ++level;
    c_arg(1, HeadOfTerm(t), 2, level, cglobs);
    if (argno == (Int)arity) {
      /* optimise for tail recursion */
      t = TailOfTerm(t);
      goto restart;
    }
    c_arg(2, TailOfTerm(t), 2, level, cglobs);
    --level;
    if (argno != (Int)arity) {
      pop_code(level, cglobs);
    }
  } else if (IsRefTerm(t)) {
    PELOCK(40, cglobs->cint.CurrentPred);
    if (!(cglobs->cint.CurrentPred->PredFlags &
          (DynamicPredFlag | LogUpdatePredFlag))) {
      CACHE_REGS
      UNLOCK(cglobs->cint.CurrentPred->PELock);
      FAIL("can not compile data base reference", TYPE_ERROR_CALLABLE, t);
    } else {
      UNLOCK(cglobs->cint.CurrentPred->PELock);
      cglobs->hasdbrefs = TRUE;
      if (level == 0)
        Yap_emit((cglobs->onhead ? get_atom_op : put_atom_op), (CELL)t, argno,
                 &cglobs->cint);
      else
        Yap_emit((cglobs->onhead ? (argno == (Int)arity ? unify_last_atom_op
                                                        : unify_atom_op)
                                 : write_atom_op),
                 (CELL)t, Zero, &cglobs->cint);
    }
  } else {

#ifdef SFUNC
    if (SFTerm(t)) {
      compile_sf_term(t, argno);
      return;
    }
#endif

    if (optimizer_on) {
      if (!(cglobs->cint.CurrentPred->PredFlags &
            (DynamicPredFlag | LogUpdatePredFlag))) {
        if (try_store_as_dbterm(t, argno, arity, level, cglobs))
          return;
      }
      t = optimize_ce(t, arity, level, cglobs);
      if (IsVarTerm(t)) {
        c_var(t, argno, arity, level, cglobs);
        return;
      }
    }
    cglobs->space_used += 1 + arity;
    if (level == 0)
      Yap_emit((cglobs->onhead ? get_struct_op : put_struct_op),
               (CELL)FunctorOfTerm(t), argno, &cglobs->cint);
    else if (argno == (Int)arity)
      Yap_emit((cglobs->onhead ? unify_last_struct_op : write_last_struct_op),
               (CELL)FunctorOfTerm(t), Zero, &cglobs->cint);
    else
      Yap_emit((cglobs->onhead ? unify_struct_op : write_struct_op),
               (CELL)FunctorOfTerm(t), Zero, &cglobs->cint);
    ++level;
    c_args(t, level, cglobs);
    --level;
    if (argno != (Int)arity) {
      pop_code(level, cglobs);
    }
  }
}

static void c_eq(Term t1, Term t2, compiler_struct *cglobs) {
  CACHE_REGS
  if (t1 == t2) {
    Yap_emit(nop_op, Zero, Zero, &cglobs->cint);
    return;
  }
  if (IsNonVarTerm(t1)) {
    if (IsVarTerm(t2)) {
      Term t = t1;
      t1 = t2;
      t2 = t;
    } else {
      /* compile unification */
      if (IsAtomicTerm(t1)) {
        /* just check if they unify */
        if (!IsAtomicTerm(t2) || !Yap_unify(t1, t2)) {
          /* they don't */
          Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
          return;
        }
        /* they do */
        Yap_emit(nop_op, Zero, Zero, &cglobs->cint);
        return;
      } else if (IsPairTerm(t1)) {
        /* just check if they unify */
        if (!IsPairTerm(t2)) {
          /* they don't */
          Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
          return;
        }
        /* they might */
        c_eq(HeadOfTerm(t1), HeadOfTerm(t2), cglobs);
        c_eq(TailOfTerm(t1), TailOfTerm(t2), cglobs);
        return;
      } else if (IsRefTerm(t1)) {
        /* just check if they unify */
        if (t1 != t2) {
          /* they don't */
          Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
          return;
        }
        /* they do */
        Yap_emit(nop_op, Zero, Zero, &cglobs->cint);
        return;
      } else {
        /* compound terms */
        Functor f = FunctorOfTerm(t1);
        UInt i, max;
        /* just check if they unify */
        if (!IsApplTerm(t2) || FunctorOfTerm(t2) != f) {
          /* they don't */
          Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
          return;
        }
        /* they might */
        max = ArityOfFunctor(f);
        for (i = 0; i < max; i++) {
          c_eq(ArgOfTerm(i + 1, t1), ArgOfTerm(i + 1, t2), cglobs);
        }
        return;
      }
    }
  }
  /* first argument is an unbound var */
  if (IsNewVar(t1) && !IsVarTerm(t2) &&
      !(cglobs->cint.CurrentPred->PredFlags & TabledPredFlag)) {
    Int v;

    v = --cglobs->tmpreg;
    c_arg(v, t2, 0, 0, cglobs);
    cglobs->onhead = TRUE;
    c_var(t1, v, 0, 0, cglobs);
    cglobs->onhead = FALSE;
  } else {
    if (IsVarTerm(t2)) {
      c_var(t1, 0, 0, 0, cglobs);
      cglobs->onhead = TRUE;
      c_var(t2, 0, 0, 0, cglobs);
    } else {
      Int v = --cglobs->tmpreg;
      c_var(t1, v, 0, 0, cglobs);
      cglobs->onhead = TRUE;
      c_arg(v, t2, 0, 0, cglobs);
    }
    cglobs->onhead = FALSE;
  }
}

static void c_test(Int Op, Term t1, compiler_struct *cglobs) {
  CACHE_REGS
  Term t = Deref(t1);

  /* be caareful, has to be first occurrence */
  if (Op == _save_by) {
    if (!IsNewVar(t)) {

      LOCAL_Error_TYPE = UNINSTANTIATION_ERROR;
      sprintf(LOCAL_ErrorMessage, "compiling %s/2 on bound variable", Yap_bip_name(Op));
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, 1);
    }
    c_var(t, save_b_flag, 1, 0, cglobs);
    return;
  }
  if (!IsVarTerm(t) || IsNewVar(t)) {
    Term tn = MkVarTerm();
    c_eq(t, tn, cglobs);
    t = tn;
  }
  if (Op == _cut_by)
    c_var(t, commit_b_flag, 1, 0, cglobs);
  else
    c_var(t, f_flag, (unsigned int)Op, 0, cglobs);
}

/* Arithmetic builtins will be compiled in the form:

   fetch_args_vv   Xi,Xj
   put_val	   Xi,Ri
   put_val	   Xj,Rj
   put_var	   Xk,Ak
   bip_body	   Op,Xk

The put_var should always be disposable, and the put_vals can be disposed of if
R is an X.
This, in the best case, Ri and Rj are WAM temp registers and this will reduce
to:

   bip		Op,Ak,Ri,Rj

meaning a single WAM op will call the clause.


If one of the arguments is a constant, the result will be:

   fetch_args_vc   Xi,C
   put_val	   Xi,Ri
   put_var	   Xk,Ak
   bip_body	   Op,Xk

and this should reduce to :

bip_cons	   Op,Xk,Ri,C

 */
static void c_bifun(basic_preds Op, Term t1, Term t2, Term t3, Term Goal,
                    Term mod, compiler_struct *cglobs) {
  CACHE_REGS
  /* compile Z = X Op Y  arithmetic function */
  /* first we fetch the arguments */

  if (IsVarTerm(t1)) {
    if (IsVarTerm(t2)) {
      /* first temp */
      Int v1 = --cglobs->tmpreg;
      /* second temp */
      Int v2 = --cglobs->tmpreg;

      Yap_emit(fetch_args_vv_op, Zero, Zero, &cglobs->cint);
      /* these should be the arguments */
      c_var(t1, v1, 0, 0, cglobs);
      c_var(t2, v2, 0, 0, cglobs);
      /* now we know where the arguments are */
    } else {
      if (Op == _arg) {
        /* we know the second argument is bound */
        if (IsPrimitiveTerm(t2) || IsNumTerm(t2)) {
          Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
          return;
        } else {
          Term tn = MkVarTerm();
          Int v1 = --cglobs->tmpreg;
          Int v2 = --cglobs->tmpreg;

          c_eq(t2, tn, cglobs);
          Yap_emit(fetch_args_vv_op, Zero, Zero, &cglobs->cint);
          /* these should be the arguments */
          c_var(t1, v1, 0, 0, cglobs);
          c_var(tn, v2, 0, 0, cglobs);
        }
        /* it has to be either an integer or a floating point */
      } else if (IsIntegerTerm(t2)) {
        /* first temp */
        Int v1 = 0;

        Yap_emit(fetch_args_vi_op, IntegerOfTerm(t2), 0L, &cglobs->cint);
        /* these should be the arguments */
        c_var(t1, v1, 0, 0, cglobs);
        /* now we know where the arguments are */
      } else {
        Yap_Error(TYPE_ERROR_NUMBER, t2,
                       "compiling %s/2 with output bound", Yap_bip_name(Op));
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, 1);
      }
    }
  } else { /* t1 is bound */
    /* it has to be either an integer or a floating point */
    if (IsVarTerm(t2)) {
      if (IsNewVar(t2)) {

        Yap_Error(INSTANTIATION_ERROR, t2, "compiling %s/3", Yap_bip_name(Op));
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, 1);
      }
    } else {
      if (Op == _functor) {
        /* both arguments are bound, we must perform unification */
        Int i2;

        if (!IsIntegerTerm(t2)) {
          Yap_Error(TYPE_ERROR_INTEGER, t2,  "compiling functor/3");
          save_machine_regs();
          siglongjmp(cglobs->cint.CompilerBotch, 1);
        }
        i2 = IntegerOfTerm(t2);
        if (i2 < 0) {

          Yap_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2,
                         "compiling functor/3");
          save_machine_regs();
          siglongjmp(cglobs->cint.CompilerBotch, 1);
        }
        if (IsNumTerm(t1)) {
          /* we will always fail */
          if (i2)
            c_goal(MkAtomTerm(AtomFalse), mod, cglobs);
        } else if (!IsAtomTerm(t1)) {
            Yap_Error(TYPE_ERROR_ATOM, t2, "compiling functor/3");
        }
          save_machine_regs();
          siglongjmp(cglobs->cint.CompilerBotch, 1);
        if (i2 == 0)
          c_eq(t1, t3, cglobs);
        else {
          CELL *hi = HR;
          Int i;

          if (t1 == TermDot && i2 == 2) {
            if (HR + 2 >= (CELL *)cglobs->cint.freep0) {
              /* oops, too many new variables */
              save_machine_regs();
              siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
            }
            RESET_VARIABLE(HR);
            RESET_VARIABLE(HR + 1);
            HR += 2;
            c_eq(AbsPair(HR - 2), t3, cglobs);
          } else if (i2 < 256 && IsAtomTerm(t1)) {
            *HR++ = (CELL)Yap_MkFunctor(AtomOfTerm(t1), i2);
            for (i = 0; i < i2; i++) {
              if (HR >= (CELL *)cglobs->cint.freep0) {
                /* oops, too many new variables */
                save_machine_regs();
                siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
              }
              RESET_VARIABLE(HR);
              HR++;
            }
            c_eq(AbsAppl(hi), t3, cglobs);
          } else {
            /* compile as default */
            Functor f = FunctorOfTerm(Goal);
            Prop p0 = PredPropByFunc(f, mod);
            if (EndOfPAEntr(p0)) {
              save_machine_regs();
              siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH);
            }
            c_args(Goal, 0, cglobs);
            Yap_emit(safe_call_op, (CELL)p0, Zero, &cglobs->cint);
            Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
            Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
            return;
          }
        }
      } else if (Op == _arg) {
        Int i1;
        if (IsIntegerTerm(t1))
          i1 = IntegerOfTerm(t1);
        else {
          Yap_Error(TYPE_ERROR_INTEGER, t1,  "compiling %s/2", Yap_bip_name(Op));
          save_machine_regs();
          siglongjmp(cglobs->cint.CompilerBotch, 1);
        }
        if (IsAtomicTerm(t2) ||
            (IsApplTerm(t2) && IsExtensionFunctor(FunctorOfTerm(t2)))) {

          LOCAL_Error_TYPE = TYPE_ERROR_COMPOUND;
          Yap_Error(TYPE_ERROR_COMPOUND, t2,  "compiling %s/2", 1, Yap_bip_name(Op));

          save_machine_regs();
          siglongjmp(cglobs->cint.CompilerBotch, 1);
        } else if (IsApplTerm(t2)) {
          Functor f = FunctorOfTerm(t2);
          if (i1 < 1 || i1 > ArityOfFunctor(f)) {
            c_goal(MkAtomTerm(AtomFalse), mod, cglobs);
          } else {
            c_eq(ArgOfTerm(i1, t2), t3, cglobs);
          }
          return;
        } else if (IsPairTerm(t2)) {
          switch (i1) {
          case 1:
            c_eq(HeadOfTerm(t2), t3, cglobs);
            return;
          case 2:
            c_eq(TailOfTerm(t2), t3, cglobs);
            return;
          default:
            c_goal(MkAtomTerm(AtomFalse), mod, cglobs);
            return;
          }
        }
      } else {
         LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
        sprintf(LOCAL_ErrorMessage, "compiling %s/2", Yap_bip_name(Op));
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, 1);
      }
    }
    if (Op == _functor) {
      if (!IsAtomicTerm(t1)) {

        LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
        sprintf(LOCAL_ErrorMessage, "compiling %s/2", Yap_bip_name(Op));
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, 1);
      } else {
        if (!IsVarTerm(t2)) {
          Int arity;

          /* We actually have the term ready, so let's just do the unification
           * now */
          if (!IsIntegerTerm(t2)) {

            LOCAL_Error_TYPE = TYPE_ERROR_INTEGER;
            sprintf(LOCAL_ErrorMessage, "compiling %s/2", Yap_bip_name(Op));
            save_machine_regs();
            siglongjmp(cglobs->cint.CompilerBotch, 1);
          }
          arity = IntOfTerm(t2);
          if (arity < 0) {
            /* fail straight away */
            Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
          }
          if (arity) {
            Term tnew;
            if (!IsAtomTerm(t1)) {

              LOCAL_Error_TYPE = TYPE_ERROR_ATOM;
              sprintf(LOCAL_ErrorMessage, "compiling %s/2", Yap_bip_name(Op));
              save_machine_regs();
              siglongjmp(cglobs->cint.CompilerBotch, 1);
            }
            if (HR + 1 + arity >= (CELL *)cglobs->cint.freep0) {
              /* oops, too many new variables */
              save_machine_regs();
              siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
            }
            tnew = AbsAppl(HR);
            *HR++ = (CELL)Yap_MkFunctor(AtomOfTerm(t1), arity);
            while (arity--) {
              RESET_VARIABLE(HR);
              HR++;
            }
            c_eq(tnew, t3, cglobs);
          } else {
            /* just unify the two arguments */
            c_eq(t1, t3, cglobs);
          }
          return;
        } else {
          /* first temp */
          Int v1 = 0;
          Yap_emit(fetch_args_cv_op, t1, Zero, &cglobs->cint);
          /* these should be the arguments */
          c_var(t2, v1, 0, 0, cglobs);
          /* now we know where the arguments are */
        }
      }
    } else if (IsIntegerTerm(t1)) {
      /* first temp */
      Int v1 = 0;
      Yap_emit(fetch_args_iv_op, IntegerOfTerm(t1), 0L, &cglobs->cint);
      /* these should be the arguments */
      c_var(t2, v1, 0, 0, cglobs);
      /* now we know where the arguments are */
    } else {

      LOCAL_Error_TYPE = UNINSTANTIATION_ERROR;
      sprintf(LOCAL_ErrorMessage, "compiling %s/2 with output bound", Yap_bip_name(Op));
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, 1);
    }
  }
  /* then we compile the opcode/result */
  if (!IsVarTerm(t3)) {
    if (Op == _arg) {
      Term tmpvar = MkVarTerm();
      if (HR == (CELL *)cglobs->cint.freep0) {
        /* oops, too many new variables */
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
      }
      c_var(tmpvar, f_flag, (unsigned int)Op, 0, cglobs);
      c_eq(tmpvar, t3, cglobs);
    } else {

      LOCAL_Error_TYPE = UNINSTANTIATION_ERROR;
      sprintf(LOCAL_ErrorMessage, "compiling %s/2 with input unbound", Yap_bip_name(Op));
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, 1);
    }
  } else if (IsNewVar(t3) && cglobs->curbranch == 0 &&
             cglobs->cint.CurrentPred->PredFlags & TabledPredFlag) {
    Term nv = MkVarTerm();
    c_var(nv, f_flag, (unsigned int)Op, 0, cglobs);
    if (Op == _functor) {
      Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
      Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
    }
    /* make sure that we first get the true t3, and then bind it to nv. That way
     * it will be confitional */
    c_eq(t3, nv, cglobs);
  } else if (
      IsNewVar(t3) &&
      cglobs->curbranch ==
          0 /* otherwise you may have trouble with z(X) :- ( Z is X*2 ; write(Z)) */) {
    c_var(t3, f_flag, (unsigned int)Op, 0, cglobs);
    if (Op == _functor) {
      Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
      Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
    }
  } else {
    /* generate code for a temp and then unify temp with previous variable */
    Yap_emit(f_0_op, 0, (unsigned int)Op, &cglobs->cint);
    /* I have to do it here, before I do the unification */
    if (Op == _functor) {
      Yap_emit(empty_call_op, Zero, (unsigned int)Op, &cglobs->cint);
      Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
    }
    cglobs->onhead = TRUE;
    c_var(t3, 0, 0, 0, cglobs);
    cglobs->onhead = FALSE;
  }
}

static void c_functor(Term Goal, Term mod, compiler_struct *cglobs) {
  CACHE_REGS
  Term t1 = ArgOfTerm(1, Goal);
  Term t2 = ArgOfTerm(2, Goal);
  Term t3 = ArgOfTerm(3, Goal);

  if (IsVarTerm(t1) && IsNewVar(t1)) {
    c_bifun(_functor, t2, t3, t1, Goal, mod, cglobs);
  } else if (IsNonVarTerm(t1)) {
    /* just split the structure */
    if (IsAtomicTerm(t1)) {
      c_eq(t1, t2, cglobs);
      c_eq(t3, MkIntTerm(0), cglobs);
    } else if (IsApplTerm(t1)) {
      Functor f = FunctorOfTerm(t1);
      c_eq(t2, MkAtomTerm(NameOfFunctor(f)), cglobs);
      c_eq(t3, MkIntegerTerm(ArityOfFunctor(f)), cglobs);
    } else /* list */ {
      c_eq(t2, TermDot, cglobs);
      c_eq(t3, MkIntTerm(2), cglobs);
    }
  } else if (IsVarTerm(t2) && IsNewVar(t2) && IsVarTerm(t3) && IsNewVar(t3)) {
    Int v1 = --cglobs->tmpreg;
    Yap_emit(fetch_args_vi_op, Zero, Zero, &cglobs->cint);
    c_var(t1, v1, 0, 0, cglobs);
    c_var(t2, f_flag, (unsigned int)_functor, 0, cglobs);
    c_var(t3, f_flag, (unsigned int)_functor, 0, cglobs);
  } else {
    Functor f = FunctorOfTerm(Goal);
    Prop p0 = PredPropByFunc(f, mod);

    if (EndOfPAEntr(p0)) {
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH);
    }
    if (profiling) {
      Yap_emit(enter_profiling_op, (CELL)RepPredProp(p0), Zero, &cglobs->cint);
    } else if (call_counting)
      Yap_emit(count_call_op, (CELL)RepPredProp(p0), Zero, &cglobs->cint);
    c_args(Goal, 0, cglobs);
    Yap_emit(safe_call_op, (CELL)p0, Zero, &cglobs->cint);
    Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
    Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
  }
}

static int IsTrueGoal(Term t) {
  if (IsVarTerm(t))
    return (FALSE);
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorModule) {
      return (IsTrueGoal(ArgOfTerm(2, t)));
    }
    if (f == FunctorComma || f == FunctorOr || f == FunctorVBar ||
        f == FunctorArrow) {
      return (IsTrueGoal(ArgOfTerm(1, t)) && IsTrueGoal(ArgOfTerm(2, t)));
    }
    return (FALSE);
  }
  return (t == MkAtomTerm(AtomTrue));
}

static void emit_special_label(Term Goal, compiler_struct *cglobs) {
  special_label_op lab_op = IntOfTerm(ArgOfTerm(1, Goal));
  special_label_id lab_id = IntOfTerm(ArgOfTerm(2, Goal));
  UInt label_name;

  switch (lab_op) {
  case SPECIAL_LABEL_INIT:
    label_name = ++cglobs->labelno;
    switch (lab_id) {
    case SPECIAL_LABEL_EXCEPTION:
      cglobs->cint.exception_handler = label_name;
      break;
    case SPECIAL_LABEL_SUCCESS:
      cglobs->cint.success_handler = label_name;
      break;
    case SPECIAL_LABEL_FAILURE:
      cglobs->cint.failure_handler = label_name;
      break;
    }
    Yap_emit_3ops(label_ctl_op, lab_op, lab_id, label_name, &cglobs->cint);
    break;
  case SPECIAL_LABEL_SET:
    switch (lab_id) {
    case SPECIAL_LABEL_EXCEPTION:
      Yap_emit(label_op, cglobs->cint.exception_handler, Zero, &cglobs->cint);
      break;
    case SPECIAL_LABEL_SUCCESS:
      Yap_emit(label_op, cglobs->cint.success_handler, Zero, &cglobs->cint);
      break;
    case SPECIAL_LABEL_FAILURE:
      Yap_emit(label_op, cglobs->cint.failure_handler, Zero, &cglobs->cint);
      break;
    }
  case SPECIAL_LABEL_CLEAR:
    switch (lab_id) {
    case SPECIAL_LABEL_EXCEPTION:
      cglobs->cint.exception_handler = 0L;
      break;
    case SPECIAL_LABEL_SUCCESS:
      cglobs->cint.success_handler = 0L;
      break;
    case SPECIAL_LABEL_FAILURE:
      cglobs->cint.failure_handler = 0L;
      break;
    }
  }
}

static void c_goal(Term Goal, Term mod, compiler_struct *cglobs) {
  Functor f;
  PredEntry *p;
  Prop p0;

  Goal = Yap_YapStripModule(Goal, &mod);
  if (IsVarTerm(Goal)) {
    Goal = Yap_MkApplTerm(FunctorCall, 1, &Goal);
  } else if (IsNumTerm(Goal)) {
    CACHE_REGS
      Yap_ThrowError(TYPE_ERROR_CALLABLE, cglobs->body, "goal can not be a number");
  } else if (IsRefTerm(Goal)) {
    CACHE_REGS
    Yap_ThrowError(TYPE_ERROR_CALLABLE, cglobs->body,"goal argument in static procedure can not be a data base reference");
  } else if (IsPairTerm(Goal)) {
    Goal = Yap_MkApplTerm(FunctorCall, 1, &Goal);
  }
  if (IsAtomTerm(Goal)) {
    Atom atom = AtomOfTerm(Goal);

    if (atom == AtomFail || atom == AtomFalse) {
      Yap_emit(fail_op, Zero, Zero, &cglobs->cint);
      return;
    } else if (atom == AtomTrue || atom == AtomOtherwise) {
      if (cglobs->onlast) {
        Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        PELOCK(41, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred))
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        else
#endif /* TABLING */
          Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      }
      return;
    } else if (atom == AtomCut) {
      if (profiling)
        Yap_emit(enter_profiling_op,
                 (CELL)RepPredProp(PredPropByAtom(AtomCut, 0)), Zero,
                 &cglobs->cint);
      else if (call_counting)
        Yap_emit(count_call_op, (CELL)RepPredProp(PredPropByAtom(AtomCut, 0)),
                 Zero, &cglobs->cint);
      if (cglobs->onlast) {
        /* never a problem here with a -> b, !, c ; d */
        Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        PELOCK(42, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred)) {
          Yap_emit_3ops(cut_op, Zero, Zero, Zero, &cglobs->cint);
          /* needs to adjust previous commits */
          Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
          Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        } else
#endif /* TABLING */
        {
          Yap_emit_3ops(cutexit_op, Zero, Zero, Zero, &cglobs->cint);
          /* needs to adjust previous commits */
          Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
          Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
          Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
        }
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      } else {
        Yap_emit_3ops(cut_op, Zero, Zero, Zero, &cglobs->cint);
        /* needs to adjust previous commits */
        Yap_emit(empty_call_op, Zero, Zero, &cglobs->cint);
        Yap_emit(restore_tmps_and_skip_op, Zero, Zero, &cglobs->cint);
        adjust_current_commits(cglobs);
      }
      return;
    }
#ifndef YAPOR
    else if (atom == AtomRepeat) {
      CELL l1 = ++cglobs->labelno;
      CELL l2 = ++cglobs->labelno;

      /* I need an either_me */
      cglobs->needs_env = TRUE;
      if (profiling)
        Yap_emit(enter_profiling_op,
                 (CELL)RepPredProp(PredPropByAtom(AtomRepeat, 0)), Zero,
                 &cglobs->cint);
      else if (call_counting)
        Yap_emit(count_call_op,
                 (CELL)RepPredProp(PredPropByAtom(AtomRepeat, 0)), Zero,
                 &cglobs->cint);
      cglobs->or_found = TRUE;
      push_branch(cglobs->onbranch, TermNil, cglobs);
      cglobs->curbranch++;
      cglobs->onbranch = cglobs->curbranch;
      Yap_emit_3ops(push_or_op, l1, Zero, Zero, &cglobs->cint);
      Yap_emit_3ops(either_op, l1, Zero, Zero, &cglobs->cint);
      Yap_emit(restore_tmps_op, Zero, Zero, &cglobs->cint);
      Yap_emit(jump_op, l2, Zero, &cglobs->cint);
      Yap_emit(label_op, l1, Zero, &cglobs->cint);
      Yap_emit(pushpop_or_op, Zero, Zero, &cglobs->cint);
      Yap_emit_3ops(orelse_op, l1, Zero, Zero, &cglobs->cint);
      Yap_emit(label_op, l2, Zero, &cglobs->cint);
      if (cglobs->onlast) {
#ifdef TABLING
        PELOCK(43, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred)) {
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        } else {
#endif
          Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
          Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        }
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      } else {
        ++cglobs->goalno;
      }
      cglobs->onbranch = pop_branch(cglobs);
      Yap_emit(pop_or_op, Zero, Zero, &cglobs->cint);
      /*                      --cglobs->onbranch; */
      return;
    }
#endif /* YAPOR */
    p = RepPredProp(p0 = Yap_PredPropByAtomNonThreadLocal(atom, mod));
    if (EndOfPAEntr(p0)) {
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH);
    }
    /* if we are profiling, make sure we register we entered this predicate */
    if (profiling)
      Yap_emit(enter_profiling_op, (CELL)p, Zero, &cglobs->cint);
    if (call_counting)
      Yap_emit(count_call_op, (CELL)p, Zero, &cglobs->cint);
  } else {
    f = FunctorOfTerm(Goal);
    p = RepPredProp(p0 = Yap_PredPropByFunctorNonThreadLocal(f, mod));
    if (EndOfPAEntr(p0)) {
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH);
    }
    if (f == FunctorOr || f == FunctorVBar) {
      Term arg;
      CELL l = ++cglobs->labelno;
      CELL m = ++cglobs->labelno;
      int save = cglobs->onlast;
      int savegoalno = cglobs->goalno;
      int frst = TRUE;
      int commitflag = 0;
      int looking_at_commit = FALSE;
      int optimizing_commit = FALSE;
      Term commitvar = 0;
      PInstr *FirstP = cglobs->cint.cpc, *savecpc, *savencpc;

      push_branch(cglobs->onbranch, TermNil, cglobs);
      ++cglobs->curbranch;
      cglobs->onbranch = cglobs->curbranch;
      cglobs->or_found = TRUE;
      do {
        arg = ArgOfTerm(1, Goal);
        looking_at_commit =
            IsApplTerm(arg) && FunctorOfTerm(arg) == FunctorArrow;
        if (frst) {
          if (optimizing_commit) {
            Yap_emit(label_op, l, Zero, &cglobs->cint);
            l = ++cglobs->labelno;
          }
          Yap_emit_3ops(push_or_op, l, Zero, Zero, &cglobs->cint);
          if (looking_at_commit && Yap_is_a_test_pred(ArgOfTerm(1, arg), mod)) {
            /*
             * let them think they are still the
             * first
             */
            //	    Yap_emit(commit_opt_op, l, Zero, &cglobs->cint);
            optimizing_commit = TRUE;
            Yap_emit_3ops(label_ctl_op, SPECIAL_LABEL_INIT,
                          SPECIAL_LABEL_FAILURE, l, &cglobs->cint);
          } else {
            optimizing_commit = FALSE;
            cglobs->needs_env = TRUE;
            Yap_emit_3ops(either_op, l, Zero, Zero, &cglobs->cint);
            Yap_emit(restore_tmps_op, Zero, Zero, &cglobs->cint);
            frst = FALSE;
          }
        } else {
          optimizing_commit = FALSE;
          Yap_emit(label_op, l, Zero, &cglobs->cint);
          Yap_emit(pushpop_or_op, Zero, Zero, &cglobs->cint);
          Yap_emit_3ops(orelse_op, l = ++cglobs->labelno, Zero, Zero,
                        &cglobs->cint);
          cglobs->needs_env = TRUE;
        }
        /*
         * if(IsApplTerm(arg) &&
         * FunctorOfTerm(arg)==FunctorArrow) {
         */
        if (looking_at_commit) {
          if (!optimizing_commit && !commitflag) {
            CACHE_REGS
            /* This instruction is placed before
             * the disjunction. This means that
             * the program counter must point
             * correctly, and also that the age
             * of variable is older than the
             * current branch.
             */
            int my_goalno = cglobs->goalno;

            cglobs->goalno = savegoalno;
            commitflag = cglobs->labelno;
            commitvar = MkVarTerm();
            if (HR == (CELL *)cglobs->cint.freep0) {
              /* oops, too many new variables */
              save_machine_regs();
              siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
            }
            savecpc = cglobs->cint.cpc;
            savencpc = FirstP->nextInst;
            cglobs->cint.cpc = FirstP;
            cglobs->onbranch = pop_branch(cglobs);
            c_var(commitvar, save_b_flag, 1, 0, cglobs);
            push_branch(cglobs->onbranch, commitvar, cglobs);
            cglobs->onbranch = cglobs->curbranch;
            cglobs->cint.cpc->nextInst = savencpc;
            cglobs->cint.cpc = savecpc;
            cglobs->goalno = my_goalno;
          }
          save = cglobs->onlast;
          cglobs->onlast = FALSE;
          c_goal(ArgOfTerm(1, arg), mod, cglobs);
          if (!optimizing_commit) {
            c_var((Term)commitvar, commit_b_flag, 1, 0, cglobs);
          } else {
            Yap_emit_3ops(label_ctl_op, SPECIAL_LABEL_CLEAR,
                          SPECIAL_LABEL_FAILURE, l, &cglobs->cint);
          }
          cglobs->onlast = save;
          c_goal(ArgOfTerm(2, arg), mod, cglobs);
        } else {
          /* standard disjunction */
          c_goal(ArgOfTerm(1, Goal), mod, cglobs);
        }
        if (!cglobs->onlast) {
          Yap_emit(jump_op, m, Zero, &cglobs->cint);
        } else {
        }
        if (!optimizing_commit || !cglobs->onlast) {
          cglobs->goalno = savegoalno + 1;
        }
        Goal = ArgOfTerm(2, Goal);
        ++cglobs->curbranch;
        cglobs->onbranch = cglobs->curbranch;
      } while (IsNonVarTerm(Goal) && IsApplTerm(Goal) &&
               (FunctorOfTerm(Goal) == FunctorOr ||
                FunctorOfTerm(Goal) == FunctorVBar));
      Yap_emit(pushpop_or_op, Zero, Zero, &cglobs->cint);
      Yap_emit(label_op, l, Zero, &cglobs->cint);
      if (!optimizing_commit) {
        Yap_emit(orlast_op, Zero, Zero, &cglobs->cint);
      } else {
        optimizing_commit = FALSE; /* not really necessary */
      }
      c_goal(Goal, mod, cglobs);
      /*              --cglobs->onbranch; */
      cglobs->onbranch = pop_branch(cglobs);
      if (!cglobs->onlast) {
        Yap_emit(label_op, m, Zero, &cglobs->cint);
        if ((cglobs->onlast = save))
          c_goal(MkAtomTerm(AtomTrue), mod, cglobs);
      }
      Yap_emit(pop_or_op, Zero, Zero, &cglobs->cint);
      return;
    } else if (f == FunctorComma) {
      int save = cglobs->onlast;
      Term t2 = ArgOfTerm(2, Goal);

      cglobs->onlast = FALSE;
      c_goal(ArgOfTerm(1, Goal), mod, cglobs);
      cglobs->onlast = save;
      c_goal(t2, mod, cglobs);
      return;
    } else if (f == FunctorNot || f == FunctorAltNot) {
      CACHE_REGS
      CELL label = (cglobs->labelno += 2);
      CELL end_label = (cglobs->labelno += 2);
      int save = cglobs->onlast;
      Term commitvar;

      /* for now */
      cglobs->needs_env = TRUE;
      commitvar = MkVarTerm();
      if (HR == (CELL *)cglobs->cint.freep0) {
        /* oops, too many new variables */
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
      }
      push_branch(cglobs->onbranch, commitvar, cglobs);
      ++cglobs->curbranch;
      cglobs->onbranch = cglobs->curbranch;
      cglobs->or_found = TRUE;
      cglobs->onlast = FALSE;
      c_var(commitvar, save_b_flag, 1, 0, cglobs);
      Yap_emit_3ops(push_or_op, label, Zero, Zero, &cglobs->cint);
      Yap_emit_3ops(either_op, label, Zero, Zero, &cglobs->cint);
      Yap_emit(restore_tmps_op, Zero, Zero, &cglobs->cint);
      c_goal(ArgOfTerm(1, Goal), mod, cglobs);
      c_var(commitvar, commit_b_flag, 1, 0, cglobs);
      cglobs->onlast = save;
      Yap_emit(fail_op, end_label, Zero, &cglobs->cint);
      Yap_emit(pushpop_or_op, Zero, Zero, &cglobs->cint);
      Yap_emit(label_op, label, Zero, &cglobs->cint);
      Yap_emit(orlast_op, Zero, Zero, &cglobs->cint);
      Yap_emit(label_op, end_label, Zero, &cglobs->cint);
      cglobs->onlast = save;
      /*              --cglobs->onbranch; */
      cglobs->onbranch = pop_branch(cglobs);
      c_goal(MkAtomTerm(AtomTrue), mod, cglobs);
      ++cglobs->goalno;
      Yap_emit(pop_or_op, Zero, Zero, &cglobs->cint);
      return;
    } else if (f == FunctorArrow) {
      CACHE_REGS
      Term commitvar;
      int save = cglobs->onlast;

      commitvar = MkVarTerm();
      if (HR == (CELL *)cglobs->cint.freep0) {
        /* oops, too many new variables */
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
      }
      cglobs->onlast = FALSE;
      c_var(commitvar, save_b_flag, 1, 0, cglobs);
      c_goal(ArgOfTerm(1, Goal), mod, cglobs);
      c_var(commitvar, commit_b_flag, 1, 0, cglobs);
      cglobs->onlast = save;
      c_goal(ArgOfTerm(2, Goal), mod, cglobs);
      return;
    } else if (f == FunctorEq) {
      if (profiling)
        Yap_emit(enter_profiling_op, (CELL)p, Zero, &cglobs->cint);
      else if (call_counting)
        Yap_emit(count_call_op, (CELL)p, Zero, &cglobs->cint);
      c_eq(ArgOfTerm(1, Goal), ArgOfTerm(2, Goal), cglobs);
      if (cglobs->onlast) {
        Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        PELOCK(44, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred))
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        else
#endif /* TABLING */
          Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      }
      return;
    } else if (f == FunctorSafe) {
      Ventry *v = (Ventry *)ArgOfTerm(1, Goal);
      /* This variable must be known before */
      v->FlagsOfVE |= SafeVar;
      return;
    } else if (p->PredFlags & (AsmPredFlag)) {
      basic_preds op = p->PredFlags & 0x7f;
      if (profiling)
        Yap_emit(enter_profiling_op, (CELL)p, Zero, &cglobs->cint);
      else if (call_counting)
        Yap_emit(count_call_op, (CELL)p, Zero, &cglobs->cint);
      if (op >= _atom && op <= _primitive) {
        c_test(op, ArgOfTerm(1, Goal), cglobs);
        if (cglobs->onlast) {
          Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
          PELOCK(45, cglobs->cint.CurrentPred);
          if (is_tabled(cglobs->cint.CurrentPred))
            Yap_emit(table_new_answer_op, Zero,
                     cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
          else
#endif /* TABLING */
            Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
          UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
        }
        return;
      } else if (op >= _plus && op <= _functor) {
        if (profiling)
          Yap_emit(enter_profiling_op, (CELL)p, Zero, &cglobs->cint);
        else if (call_counting)
          Yap_emit(count_call_op, (CELL)p, Zero, &cglobs->cint);
        if (op == _functor) {
          c_functor(Goal, mod, cglobs);
        } else {
          c_bifun(op, ArgOfTerm(1, Goal), ArgOfTerm(2, Goal),
                  ArgOfTerm(3, Goal), Goal, mod, cglobs);
        }
        if (cglobs->onlast) {
          Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
          PELOCK(46, cglobs->cint.CurrentPred);
          if (is_tabled(cglobs->cint.CurrentPred))
            Yap_emit(table_new_answer_op, Zero,
                     cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
          else
#endif /* TABLING */
            Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
          UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
        }
        return;
      } else if (op == _p_label_ctl) {
        emit_special_label(Goal, cglobs);
        return;
      } else {
        c_args(Goal, 0, cglobs);
      }
    }
#ifdef BEAM
    else if (p->PredFlags & BinaryPredFlag && !EAM) {
#else
    else if (p->PredFlags & BinaryPredFlag) {
#endif
      CACHE_REGS
      Term a1 = ArgOfTerm(1, Goal);

      if (IsVarTerm(a1) && !IsNewVar(a1)) {
        Term a2 = ArgOfTerm(2, Goal);
        if (IsVarTerm(a2) && !IsNewVar(a2)) {
          cglobs->current_p0 = p0;
          c_2vars(bt_flag, a1, 0, a2, 0, (CELL)p0, 0, 0, cglobs);
        } else {
          Term t2 = MkVarTerm();
          // c_var(t2, --cglobs->tmpreg, 0, 0, cglobs);
          if (HR == (CELL *)cglobs->cint.freep0) {
            /* oops, too many new variables */
            save_machine_regs();
            siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
          }
          cglobs->current_p0 = p0;
          c_eq(t2, a2, cglobs);
          c_2vars(bt_flag, a1, 0, t2, 0, (CELL)p0, 0, 0, cglobs);
        }
      } else {
        Term a2 = ArgOfTerm(2, Goal);
        Term t1 = MkVarTerm();
        // c_var(t1, --cglobs->tmpreg, 0, 0, cglobs);
        if (HR == (CELL *)cglobs->cint.freep0) {
          /* oops, too many new variables */
          save_machine_regs();
          siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
        }
        c_eq(t1, a1, cglobs);

        if (IsVarTerm(a2) && !IsNewVar(a2)) {
          cglobs->current_p0 = p0;
          c_2vars(bt_flag, t1, 0, a2, 0, (CELL)p0, 0, 0, cglobs);
        } else {
          Term t2 = MkVarTerm();
          //	  c_var(t2, --cglobs->tmpreg, 0, 0, cglobs);
          if (HR == (CELL *)cglobs->cint.freep0) {
            /* oops, too many new variables */
            save_machine_regs();
            siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_TEMPS_BOTCH);
          }
          c_eq(t2, a2, cglobs);
          cglobs->current_p0 = p0;
          c_2vars(bt_flag, t1, 0, t2, 0, (CELL)p0, 0, 0, cglobs);
        }
      }
      if (cglobs->onlast) {
        Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        PELOCK(47, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred))
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        else
#endif /* TABLING */
          Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      }
      return;
    } else {
      if (profiling)
        Yap_emit(enter_profiling_op, (CELL)p, Zero, &cglobs->cint);
      else if (call_counting)
        Yap_emit(count_call_op, (CELL)p, Zero, &cglobs->cint);
      if (f == FunctorExecuteInMod) {
        /* compile the first argument only */
        c_arg(1, ArgOfTerm(1, Goal), 1, 0, cglobs);
      } else {
        c_args(Goal, 0, cglobs);
      }
    }
  }

  if (p->PredFlags & SafePredFlag
#ifdef YAPOR
      /* synchronisation means saving the state, so it is never safe in YAPOR */
      && !(p->PredFlags & SyncPredFlag)
#endif /* YAPOR */
          ) {
    Yap_emit(safe_call_op, (CELL)p0, Zero, &cglobs->cint);
    if (cglobs->onlast) {
      Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
      PELOCK(48, cglobs->cint.CurrentPred);
      if (is_tabled(cglobs->cint.CurrentPred))
        Yap_emit(table_new_answer_op, Zero, cglobs->cint.CurrentPred->ArityOfPE,
                 &cglobs->cint);
      else
#endif /* TABLING */
        Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
      UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
    }
  } else {
    if ((p->PredFlags &
         (AsmPredFlag | ModuleTransparentPredFlag | UserCPredFlag)) ||
        p->FunctorOfPred == FunctorExecuteInMod) {
#ifdef YAPOR
      if (p->PredFlags & SyncPredFlag)
        Yap_emit(sync_op, (CELL)p, (CELL)(p->ArityOfPE), &cglobs->cint);
#endif /* YAPOR */
      if (p->FunctorOfPred == FunctorExecuteInMod) {
        cglobs->needs_env = TRUE;
        Yap_emit_4ops(call_op, (CELL)p0, Zero, Zero, ArgOfTerm(2, Goal),
                      &cglobs->cint);
      } else {
        cglobs->needs_env = TRUE;
        Yap_emit_3ops(call_op, (CELL)p0, Zero, Zero, &cglobs->cint);
      }
      /* functor is allowed to call the garbage collector */
      if (cglobs->onlast) {
        Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
        cglobs->or_found = TRUE;
#ifdef TABLING
        PELOCK(49, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred))
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        else
#endif /* TABLING */
          Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      }
    } else {
      if (cglobs->onlast) {
        Yap_emit(deallocate_op, Zero, Zero, &cglobs->cint);
#ifdef TABLING
        PELOCK(50, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred)) {
          cglobs->needs_env = TRUE;
          Yap_emit_3ops(call_op, (CELL)p0, Zero, Zero, &cglobs->cint);
          Yap_emit(table_new_answer_op, Zero,
                   cglobs->cint.CurrentPred->ArityOfPE, &cglobs->cint);
        } else
#endif /* TABLING */
          Yap_emit(execute_op, (CELL)p0, Zero, &cglobs->cint);
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      } else {
        cglobs->needs_env = TRUE;
        Yap_emit_3ops(call_op, (CELL)p0, Zero, Zero, &cglobs->cint);
      }
    }
    if (!cglobs->onlast)
      ++cglobs->goalno;
  }
}

static void c_body(Term Body, Term mod, compiler_struct *cglobs) {
  cglobs->body = Body;
  cglobs->onhead = FALSE;
  cglobs->BodyStart = cglobs->cint.cpc;
  cglobs->goalno = 1;
  while (IsNonVarTerm(Body) && IsApplTerm(Body) &&
         FunctorOfTerm(Body) == FunctorComma) {
    Term t2 = ArgOfTerm(2, Body);
    if (!cglobs->cint.success_handler && IsTrueGoal(t2)) {
      /* optimise the case where some idiot left trues at the end
         of the clause.
      */
      Body = ArgOfTerm(1, Body);
      break;
    }
    c_goal(ArgOfTerm(1, Body), mod, cglobs);
    Body = t2;
#ifdef BEAM
    if (EAM)
      Yap_emit(endgoal_op, Zero, Zero, &cglobs->cint);
#endif
  }
  cglobs->onlast = TRUE;
  c_goal(Body, mod, cglobs);
#ifdef BEAM
  if (EAM && cglobs->goalno > 1) {
    if (cglobs->cint.cpc->op == procceed_op) {
      cglobs->cint.cpc->op = endgoal_op;
      Yap_emit(procceed_op, Zero, Zero, &cglobs->cint);
    } else
      Yap_emit(endgoal_op, Zero, Zero, &cglobs->cint);
  }
#endif
}

static void c_head(Term t, compiler_struct *cglobs) {
  Functor f;

  cglobs->goalno = 0;
  cglobs->onhead = TRUE;
  cglobs->onlast = FALSE;
  cglobs->curbranch = cglobs->onbranch = 0;
  cglobs->branch_pointer = cglobs->parent_branches;
  cglobs->space_used = 0;
  cglobs->space_op = NULL;
  if (IsAtomTerm(t)) {
    Yap_emit(name_op, (CELL)AtomOfTerm(t), Zero, &cglobs->cint);
#ifdef BEAM
    if (EAM) {
      Yap_emit(run_op, Zero, (UInt)cglobs->cint.CurrentPred, &cglobs->cint);
    }
#endif
    Yap_emit(ensure_space_op, Zero, Zero, &cglobs->cint);
    cglobs->space_op = cglobs->cint.cpc;
    return;
  }
  f = FunctorOfTerm(t);
  Yap_emit(name_op, (CELL)NameOfFunctor(f), ArityOfFunctor(f), &cglobs->cint);
#ifdef BEAM
  if (EAM) {
    Yap_emit(run_op, Zero, (UInt)cglobs->cint.CurrentPred, &cglobs->cint);
  }
#endif
  if (Yap_ExecutionMode == MIXED_MODE_USER)
    Yap_emit(native_op, 0, 0, &cglobs->cint);
  Yap_emit(ensure_space_op, Zero, Zero, &cglobs->cint);
  cglobs->space_op = cglobs->cint.cpc;
#ifdef BEAM
  if (EAM) {
    Yap_emit(run_op, Zero, (UInt)cglobs->cint.CurrentPred, &cglobs->cint);
  }
#endif
  if (Yap_ExecutionMode == MIXED_MODE || Yap_ExecutionMode == COMPILED)
#if YAP_JIT
    Yap_emit(native_op, 0, 0, &cglobs->cint);
#else
  {
    if (Yap_ExecutionMode == MIXED_MODE)
      Yap_NilError(SYSTEM_ERROR_JIT_NOT_AVAILABLE, "mixed");
    else /* Yap_ExecutionMode == COMPILED */
      Yap_NilError(SYSTEM_ERROR_JIT_NOT_AVAILABLE, "just compiled");
  }
#endif
  c_args(t, 0, cglobs);
}

inline static bool usesvar(compiler_vm_op ic) {
  if (ic >= get_var_op && ic <= put_val_op)
    return true;
  switch (ic) {
  case save_b_op:
  case commit_b_op:
  case patch_b_op:
  case save_appl_op:
  case save_pair_op:
  case f_val_op:
  case f_var_op:
  case bccall_op:
    return true;
  default:
    break;
  }
#ifdef SFUNC
  if (ic >= unify_s_var_op && ic <= write_s_val_op)
    return true;
#endif
  return ((ic >= unify_var_op && ic <= write_val_op) ||
          (ic >= unify_last_var_op && ic <= unify_last_val_op));
}

/*
inline static bool
  uses_this_var(PInstr *pc, Term arg)
{
  compiler_vm_op ic = pc->op;

  if (pc->rnd1 != arg)
    return arg == pc->rnd3 && ic == bccall_op;
  return usesvar( ic );
}
*/

inline static bool usesvar2(compiler_vm_op ic) { return ic == bccall_op; }

/*
 * Do as in the traditional WAM and make sure voids are in
 * environments
 */
#define LOCALISE_VOIDS 1

#ifdef LOCALISE_VOIDS
typedef struct env_tmp {
  Ventry *Var;
  struct env_tmp *Next;
} EnvTmp;
#endif

static void tag_use(Ventry *v USES_REGS) {
#ifdef BEAM
  if (EAM) {
    if (v->NoOfVE == Unassigned || v->KindOfVE != PermVar) {
      v->NoOfVE = PermVar | (LOCAL_nperm++);
      v->KindOfVE = PermVar;
      v->FlagsOfVE |= PermFlag;
    }
  }
#endif
  if (v->NoOfVE == Unassigned) {
    if ((v->AgeOfVE > 1 && (v->AgeOfVE > v->FirstOfVE)) ||
        v->KindOfVE == PermVar /*
					 * * || (v->FlagsOfVE & NonVoid && !(v->FlagsOfVE &
					 * * OnHeadFlag))
					 */) {
      v->NoOfVE = PermVar | (LOCAL_nperm++);
      v->KindOfVE = PermVar;
      v->FlagsOfVE |= PermFlag;
    } else {
      v->NoOfVE = v->KindOfVE = TempVar;
    }
  }
}

static void AssignPerm(PInstr *pc, compiler_struct *cglobs) {
  CACHE_REGS
  int uses_var;
  PInstr *opc = NULL;
#ifdef LOCALISE_VOIDS
  EnvTmp *EnvTmps = NULL;
#endif

  /* The WAM tries to keep voids on the
   * environment. Traditionally, YAP liberally globalises
   * voids.
   *
   * The new version goes to some length to keep void variables
   * in environments, but it is dubious that improves
   * performance, and may actually slow down the system
   */
  while (pc != NULL) {
    PInstr *tpc = pc->nextInst;
#ifdef LOCALISE_VOIDS
    if (pc->op == put_var_op) {
      Ventry *v = (Ventry *)(pc->rnd1);
      if (v->AgeOfVE == v->FirstOfVE &&
          !(v->FlagsOfVE & (GlobalVal | OnHeadFlag | OnLastGoal | NonVoid))) {
        EnvTmp *x = (EnvTmp *)Yap_AllocCMem(sizeof(*x), &cglobs->cint);
        x->Next = EnvTmps;
        x->Var = v;
        EnvTmps = x;
      }
    } else
#endif
        if (pc->op == call_op || pc->op == either_op || pc->op == orelse_op ||
            pc->op == push_or_op) {
#ifdef LOCALISE_VOIDS
      pc->ops.opseqt[1] = (CELL)EnvTmps;
      if (EnvTmps)
        EnvTmps = NULL;
#endif
    }
    pc->nextInst = opc;
    opc = pc;
    pc = tpc;
  }
  pc = opc;
  opc = NULL;
  do {
    PInstr *npc = pc->nextInst;

    pc->nextInst = opc;
    uses_var = usesvar(pc->op);
    if (uses_var) {
      Ventry *v = (Ventry *)(pc->rnd1);

      tag_use(v PASS_REGS);
      if (usesvar2(pc->op)) {
        Ventry *v2 = (Ventry *)(pc->rnd3);
        tag_use(v2 PASS_REGS);
      }

    } else if (pc->op == empty_call_op) {
      pc->rnd2 = LOCAL_nperm;
    } else if (pc->op == call_op || pc->op == either_op ||
               pc->op == orelse_op || pc->op == push_or_op) {
#ifdef LOCALISE_VOIDS
      EnvTmps = (EnvTmp *)(pc->ops.opseqt[1]);
      while (EnvTmps) {
        Ventry *v = EnvTmps->Var;
        v->NoOfVE = PermVar | (LOCAL_nperm++);
        v->KindOfVE = PermVar;
        v->FlagsOfVE |= (PermFlag | SafeVar);
        EnvTmps = EnvTmps->Next;
      }
#endif
      pc->rnd2 = LOCAL_nperm;
    } else if (pc->op == cut_op || pc->op == cutexit_op ||
               pc->op == commit_b_op) {
      pc->rnd2 = LOCAL_nperm;
    }
    opc = pc;
    pc = npc;
  } while (pc != NULL);
}

static CELL *init_bvarray(int nperm, compiler_struct *cglobs) {
  CELL *vinfo = NULL;
  size_t sz = sizeof(CELL) * (1 + nperm / (8 * sizeof(CELL)));
  vinfo = (CELL *)Yap_AllocCMem(sz, &cglobs->cint);
  memset((void *)vinfo, 0, sz);
  return vinfo;
}

static void clear_bvarray(int var, CELL *bvarray
#ifdef DEBUG
                          ,
                          compiler_struct *cglobs
#endif
                          ) {
  int max = 8 * sizeof(CELL);
  CELL nbit;

  /* get to the array position */
  while (var >= max) {
    bvarray++;
    var -= max;
  }
  /* now put a 0 on it, from now on the variable is initialized */
  nbit = ((CELL)1 << var);
#ifdef DEBUG
  if (*bvarray & nbit) {
    CACHE_REGS
    /* someone had already marked this variable: complain */
    LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
    LOCAL_ErrorMessage = "compiler internal error: variable initialized twice";
    save_machine_regs();
    siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
  }
  cglobs->pbvars++;
#endif
  *bvarray |= nbit;
}

/* copy the current state of the perm variable state array to code space */
static void add_bvarray_op(PInstr *cp, CELL *bvarray, int env_size,
                           compiler_struct *cglobs) {
  int i, size = env_size / (8 * sizeof(CELL));
  CELL *dest;

  dest = Yap_emit_extra_size(mark_initialized_pvars_op, (CELL)env_size,
                             (size + 1) * sizeof(CELL), &cglobs->cint);
  /* copy the cells to dest */
  for (i = 0; i <= size; i++)
    *dest++ = *bvarray++;
}

/* vsc: this code is not working, as it is too complex */

typedef struct {
  int lab;
  int last;
  PInstr *pc;
} bventry;

#define MAX_DISJUNCTIONS 128
static bventry *bvstack;
static int bvindex = 0;

static void push_bvmap(int label, PInstr *pcpc, compiler_struct *cglobs) {
  if (bvindex == MAX_DISJUNCTIONS) {
    CACHE_REGS
    LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
    LOCAL_ErrorMessage = "Too many embedded disjunctions";
    save_machine_regs();
    siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
  }
  /* the label instruction */
  bvstack[bvindex].lab = label;
  bvstack[bvindex].last = -1;
  /* where we have the code */
  bvstack[bvindex].pc = pcpc;
  bvindex++;
}

static void reset_bvmap(CELL *bvarray, int nperm, compiler_struct *cglobs) {
  int size, size1, env_size, i;
  CELL *source;

  if (bvarray == NULL)

    if (bvindex == 0) {
      CACHE_REGS
      LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
      LOCAL_ErrorMessage = "No embedding in disjunctions";
      save_machine_regs();
      siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
    }
  env_size = (bvstack[bvindex - 1].pc)->rnd1;
  size = env_size / (8 * sizeof(CELL));
  size1 = nperm / (8 * sizeof(CELL));
  source = (bvstack[bvindex - 1].pc)->arnds;
  for (i = 0; i <= size; i++)
    *bvarray++ = *source++;
  for (i = size + 1; i <= size1; i++)
    *bvarray++ = (CELL)(0);
}

static void pop_bvmap(CELL *bvarray, int nperm, compiler_struct *cglobs) {
  if (bvindex == 0) {
    CACHE_REGS
    LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
    LOCAL_ErrorMessage = "Too few embedded disjunctions";
    /*  save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH); */
  }
  reset_bvmap(bvarray, nperm, cglobs);
  bvindex--;
}

typedef struct {
  PInstr *p;
  Ventry *v;
} UnsafeEntry;

/* extend to also support variable usage bitmaps for garbage collection */
static void CheckUnsafe(PInstr *pc, compiler_struct *cglobs) {
  CACHE_REGS
  int pending = 0;

  /* say that all variables are yet to initialize */
  CELL *vstat = init_bvarray(LOCAL_nperm, cglobs);
  UnsafeEntry *UnsafeStack = (UnsafeEntry *)Yap_AllocCMem(
      LOCAL_nperm * sizeof(UnsafeEntry), &cglobs->cint);
  /* keep a copy of previous cglobs->cint.cpc and CodeStart */
  PInstr *opc = cglobs->cint.cpc;
  PInstr *OldCodeStart = cglobs->cint.CodeStart;

  cglobs->cint.CodeStart = cglobs->cint.BlobsStart;
  cglobs->cint.cpc = cglobs->cint.icpc;
  bvindex = 0;
  bvstack = (bventry *)Yap_AllocCMem(MAX_DISJUNCTIONS * sizeof(bventry),
                                     &cglobs->cint);
  while (pc != NIL) {
    switch (pc->op) {
    case put_val_op: {
      Ventry *v = (Ventry *)(pc->rnd1);
      if ((v->FlagsOfVE & PermFlag) && !(v->FlagsOfVE & SafeVar)) {
        UnsafeStack[pending].p = pc;
        UnsafeStack[pending++].v = v;
        v->FlagsOfVE |= SafeVar;
      }
      break;
    }
    case bccall_op: {
      Ventry *v = (Ventry *)(pc->rnd1), *v3 = (Ventry *)(pc->rnd3);

      if ((v->FlagsOfVE & PermFlag && pc == v->FirstOpForV) ||
          (v3->FlagsOfVE & PermFlag && pc == v3->FirstOpForV)) {
        CACHE_REGS
        LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
        LOCAL_ErrorMessage =
            "comparison should not have first instance of variables";
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
      }
    } break;
    case put_var_op:
    case get_var_op:
    case save_b_op:
    case unify_var_op:
    case unify_last_var_op:
    case write_var_op:
    case save_appl_op:
    case save_pair_op:
    case f_var_op: {
      Ventry *v = (Ventry *)(pc->rnd1);

      if (v->FlagsOfVE & PermFlag && pc == v->FirstOpForV) {
        /* the second condition covers cases such as save_b_op
           in a disjunction */
        clear_bvarray((v->NoOfVE & MaskVarAdrs), vstat
#ifdef DEBUG
                      ,
                      cglobs
#endif
                      );
      }
    } break;
    case push_or_op:
      Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
      pc->rnd3 = (CELL)cglobs->labelno;
      add_bvarray_op(pc, vstat, pc->rnd2, cglobs);
      push_bvmap((CELL)cglobs->labelno, cglobs->cint.cpc, cglobs);
      break;
    case either_op:
      /* add a first entry to the array */
      Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
      pc->rnd3 = (CELL)cglobs->labelno;
      add_bvarray_op(pc, vstat, pc->rnd2, cglobs);
      break;
    case pushpop_or_op:
      reset_bvmap(vstat, LOCAL_nperm, cglobs);
      goto reset_safe_map;
    case orelse_op:
      Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
      pc->rnd3 = (CELL)cglobs->labelno;
      add_bvarray_op(pc, vstat, pc->rnd2, cglobs);
      break;
    case pop_or_op:
      pop_bvmap(vstat, LOCAL_nperm, cglobs);
      goto reset_safe_map;
      break;
    case empty_call_op:
      /* just get ourselves a label describing how
         many permanent variables are alive */
      Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
      pc->rnd1 = (CELL)cglobs->labelno;
      add_bvarray_op(pc, vstat, pc->rnd2, cglobs);
      break;
    case cut_op:
    case cutexit_op:
      /* just get ourselves a label describing how
         many permanent variables are alive */
      Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
      pc->rnd1 = (CELL)cglobs->labelno;
      add_bvarray_op(pc, vstat, pc->rnd2, cglobs);
      break;
    case call_op:
      Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
      pc->ops.opseqt[1] = (CELL)cglobs->labelno;
      add_bvarray_op(pc, vstat, pc->rnd2, cglobs);
    case deallocate_op:
    reset_safe_map : {
      int n = pc->op == call_op ? pc->rnd2 : 0;
      int no;

      while (pending) {
        Ventry *v = UnsafeStack[--pending].v;

        v->FlagsOfVE &= ~SafeVar;
        no = (v->NoOfVE) & MaskVarAdrs;
        if (no >= n)
          UnsafeStack[pending].p->op = put_unsafe_op;
      }
    }
    default:
      break;
    }
    pc = pc->nextInst;
  }
  cglobs->cint.icpc = cglobs->cint.cpc;
  cglobs->cint.cpc = opc;
  cglobs->cint.BlobsStart = cglobs->cint.CodeStart;
  cglobs->cint.CodeStart = OldCodeStart;
}

static void
CheckVoids(compiler_struct *cglobs) { /* establish voids in the head and initial
                                       * uses        */
  Ventry *ve;
  compiler_vm_op ic;
  struct PSEUDO *cpc;

  cpc = cglobs->cint.CodeStart;
  while ((ic = cpc->op) != allocate_op) {
    switch (ic) {
    case get_var_op:
    case unify_var_op:
    case unify_last_var_op:
#ifdef SFUNC
    case unify_s_var_op:
#endif
    case save_pair_op:
    case save_appl_op:
      ve = ((Ventry *)cpc->rnd1);
      if ((ve->FlagsOfVE & PermFlag) == 0 && ve->RCountOfVE <= 1) {
        ve->NoOfVE = ve->KindOfVE = VoidVar;
        if (ic == get_var_op || ic == save_pair_op || ic == save_appl_op
#ifdef SFUNC
            || ic == unify_s_var_op
#endif
            ) {
          cpc->op = nop_op;
          break;
        }
      }
      if (ic != get_var_op)
        break;
    case get_val_op:
    case get_atom_op:
    case get_num_op:
    case get_float_op:
    case get_dbterm_op:
    case get_longint_op:
    case get_string_op:
    case get_bigint_op:
    case get_list_op:
    case get_struct_op:
      cglobs->Uses[cpc->rnd2] = 1;
      break;
    case bccall_op:
      cglobs->Uses[cpc->rnd2] = 1;
      cglobs->Uses[cpc->rnd4] = 1;
    default:
      break;
    }
    cpc = cpc->nextInst;
  }
}

static int checktemp(Int arg, Int rn, compiler_vm_op ic,
                     compiler_struct *cglobs) {
  Ventry *v = (Ventry *)arg;
  PInstr *q;
  Int Needed[MaxTemps];
  Int r, target1, target2;
  Int n, *np, *rp;
  CELL *cp;
  Int vadr;
  Int vreg;

  cglobs->vadr = vadr = (v->NoOfVE);
  cglobs->vreg = vreg = vadr & MaskVarAdrs;
  if (v->KindOfVE == PermVar || v->KindOfVE == VoidVar)
    return 0;
  if (v->RCountOfVE == 1)
    return 0;
  if (vreg) {
    --cglobs->Uses[vreg];
    return 1;
  }
  /* follow the life of the variable                                       */
  q = cglobs->cint.cpc;
  /*
   * for(r=0; r<cglobs->MaxCTemps; ++r) Needed[r] = cglobs->Uses[r]; might be
   * written
   * as:
   */
  np = Needed;
  rp = cglobs->Uses;
  for (r = 0; r < cglobs->MaxCTemps; ++r)
    *np++ = *rp++;
  if (rn > 0 && (ic == get_var_op || ic == put_var_op)) {
    if (ic == put_var_op)
      Needed[rn] = 1;
    target1 = rn; /* try to leave it where it is   */
  } else
    target1 = cglobs->MaxCTemps;
  target2 = cglobs->MaxCTemps;
  n = v->RCountOfVE - 1;
  while (q != v->LastOpForV && (q = q->nextInst) != NIL) {
    if (q->rnd2 <= 0)
      ; /* don't try to reuse REGISTER 0 */
    else if ((usesvar(ic = q->op) && arg == q->rnd1) ||
             (ic == bccall_op && arg == q->rnd3) /*uses_this_var(q, arg)*/) {
      ic = q->op;
      --n;
      if (ic == put_val_op) {
        if (target1 == cglobs->MaxCTemps && Needed[q->rnd2] == 0)
          target1 = q->rnd2;
        else if (target1 != (r = q->rnd2)) {
          if (target2 == cglobs->MaxCTemps && Needed[r] == 0)
            target2 = r;
          else if (target2 > r && cglobs->Uses[r] == 0 && Needed[r] == 0)
            target2 = r;
        }
      }
    }
#ifdef SFUNC
    else if ((ic >= get_var_op && ic <= put_unsafe_op) || ic == get_s_f_op ||
             ic == put_s_f_op)
      Needed[q->rnd2] = 1;
#else
    else if (ic >= get_var_op && ic <= put_unsafe_op)
      Needed[q->rnd2] = 1;
#endif
    if ((ic == call_op || ic == safe_call_op) && n == 0)
      break;
  }
  if (target2 < target1) {
    r = target2;
    target2 = target1;
    target1 = r;
  }
  if (target1 == cglobs->MaxCTemps || cglobs->Uses[target1] || Needed[target1])
    if ((target1 = target2) == cglobs->MaxCTemps || cglobs->Uses[target1] ||
        Needed[target1]) {
      target1 = cglobs->MaxCTemps;
      do
        --target1;
      while (target1 && cglobs->Uses[target1] == 0 && Needed[target1] == 0);
      ++target1;
    }
  if (target1 == cglobs->MaxCTemps) {
    CACHE_REGS
    LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;

    LOCAL_ErrorMessage = "too many temporaries";
    save_machine_regs();
    siglongjmp(cglobs->cint.CompilerBotch, COMPILER_ERR_BOTCH);
  }
  v->NoOfVE = cglobs->vadr = vadr = TempVar | target1;
  v->KindOfVE = TempVar;
  cglobs->Uses[cglobs->vreg = vreg = target1] = v->RCountOfVE - 1;
  /*
   * for(r=0; r<cglobs->MaxCTemps; ++r) if(cglobs->Contents[r]==vadr)
   * cglobs->Contents[r] =
   * NIL;
   */
  cp = cglobs->Contents;
  for (r = 0; r < cglobs->MaxCTemps; ++r)
    if (*cp++ == (Term)vadr)
      cp[-1] = NIL;
  cglobs->Contents[vreg] = vadr;
  return 1;
}

static Int checkreg(Int arg, Int rn, compiler_vm_op ic, int var_arg,
                    compiler_struct *cglobs) {
  PInstr *p = cglobs->cint.cpc;
  Int vreg;

  if (rn >= 0)
    return rn;
  if (var_arg) {
    Ventry *v = (Ventry *)arg;

    vreg = (v->NoOfVE) & MaskVarAdrs;
    if (v->KindOfVE == PermVar)
      vreg = 0;
    else if (vreg == 0) {
      checktemp(arg, rn, ic, cglobs);
      vreg = (v->NoOfVE) & MaskVarAdrs;
      ++cglobs->Uses[vreg];
    }
    if (!vreg) {
      vreg = 1;
      while (cglobs->Uses[vreg] != 0) {
        ++vreg;
      }
      cglobs->Uses[vreg] = v->RCountOfVE;
    }
  } else {
    vreg = 1;
    while (cglobs->Uses[vreg] != 0) {
      ++vreg;
    }
  }
  while (p) {
    if (p->op >= get_var_op && p->op <= put_unsafe_op && p->rnd2 == rn)
      p->rnd2 = vreg;
    /* only copy variables until you reach a call */
    if (p->op == procceed_op || p->op == call_op || p->op == push_or_op ||
        p->op == pushpop_or_op)
      break;
    p = p->nextInst;
  }
  return vreg;
}

/* Create a bitmap with all live variables */
static CELL copy_live_temps_bmap(int max, compiler_struct *cglobs) {
  unsigned int size = AdjustSize((max | 7) / 8 + 1);
  int i;
  CELL *dest = Yap_emit_extra_size(mark_live_regs_op, max, size, &cglobs->cint);
  CELL *ptr = dest;
  *ptr = 0L;
  for (i = 1; i <= max; i++) {
    /* move to next cell */
    if (i % (8 * CellSize) == 0) {
      ptr++;
      *ptr = 0L;
    }
    /* set the register live bit */
    if (cglobs->Contents[i]) {
      int j = i % (8 * CellSize);
      *ptr |= (1 << j);
    }
  }
  return ((CELL)dest);
}

static void c_layout(compiler_struct *cglobs) {
  CACHE_REGS
  PInstr *savepc = cglobs->BodyStart->nextInst;
  register Ventry *v = cglobs->vtable;
  Int *up = cglobs->Uses;
  CELL *cop = cglobs->Contents;
  /* tell put_values used in bip optimisation */
  int rn_kills = 0;
  Int rn_to_kill[2];
  int needs_either = 0;

  rn_to_kill[0] = rn_to_kill[1] = 0;
  cglobs->cint.cpc = cglobs->BodyStart;
  /*
#ifdef BEAM
  if (!cglobs->is_a_fact || EAM) {
#else
  */
  if (!cglobs->is_a_fact) {
    while (v != NIL) {
      if (v->FlagsOfVE & BranchVar) {
        v->AgeOfVE = v->FirstOfVE + 1; /* force permanent */
        ++(v->RCountOfVE);
        Yap_emit(put_var_op, (CELL)v, Zero, &cglobs->cint);
        v->FlagsOfVE &= ~GlobalVal;
        v->FirstOpForV = cglobs->cint.cpc;
      }
      v = v->NextOfVE;
    }
    cglobs->cint.cpc->nextInst = savepc;

#ifdef BEAM
    if (cglobs->needs_env || EAM) {
#else
    if (cglobs->needs_env) {
#endif
      LOCAL_nperm = 0;
      AssignPerm(cglobs->cint.CodeStart, cglobs);
#ifdef DEBUG
      cglobs->pbvars = 0;
#endif
      CheckUnsafe(cglobs->cint.CodeStart, cglobs);
#ifdef DEBUG
      if (cglobs->pbvars != LOCAL_nperm) {
        CACHE_REGS
        LOCAL_Error_TYPE = SYSTEM_ERROR_COMPILER;
        LOCAL_ErrorMessage = "wrong number of variables found in bitmap";
        save_machine_regs();
        siglongjmp(cglobs->cint.CompilerBotch, OUT_OF_HEAP_BOTCH);
      }
#endif
    }
  }

  cglobs->MaxCTemps = cglobs->nvars + cglobs->max_args - cglobs->tmpreg +
                      cglobs->n_common_exps + 2;
  if (cglobs->MaxCTemps >= MaxTemps)
    cglobs->MaxCTemps = MaxTemps;
  {
    Int rn;
    for (rn = 0; rn < cglobs->MaxCTemps; ++rn) {
      /* cglobs->Uses[rn] = 0; cglobs->Contents[rn] = NIL; */
      *up++ = 0;
      *cop++ = NIL;
    }
  }

  CheckVoids(cglobs);

  /* second scan: allocate registers                                       */
  cglobs->cint.cpc = cglobs->cint.CodeStart;
  while (cglobs->cint.cpc) {
    compiler_vm_op ic = cglobs->cint.cpc->op;
    Int arg = cglobs->cint.cpc->rnd1;
    Int rn = cglobs->cint.cpc->rnd2;
    switch (ic) {
    case pop_or_op:
      if (needs_either)
        needs_either--;
    case either_op:
      needs_either++;
      break;
#ifdef TABLING_INNER_CUTS
    case cut_op:
    case cutexit_op:
      cglobs->cut_mark->op = clause_with_cut_op;
      break;
#else
    case cut_op:
    case cutexit_op: {
      int i, max;

      max = 0;
      for (i = 1; i < cglobs->MaxCTemps; ++i) {
        if (cglobs->Contents[i])
          max = i;
      }
      cglobs->cint.cpc->ops.opseqt[1] = max;
    } break;
#endif /* TABLING_INNER_CUTS */
    case allocate_op:
    case deallocate_op:
      if (!cglobs->needs_env) {
        cglobs->cint.cpc->op = nop_op;
      } else {
#ifdef TABLING
        PELOCK(51, cglobs->cint.CurrentPred);
        if (is_tabled(cglobs->cint.CurrentPred))
          cglobs->cint.cpc->op = nop_op;
        else
#endif /* TABLING */
            if (cglobs->goalno == 1 && !cglobs->or_found && LOCAL_nperm == 0)
          cglobs->cint.cpc->op = nop_op;
#ifdef TABLING
        UNLOCK(cglobs->cint.CurrentPred->PELock);
#endif
      }
      break;
    case pop_op:
      ic = (cglobs->cint.cpc->nextInst)->op;
      if (ic >= get_var_op && ic <= put_unsafe_op)
        cglobs->cint.cpc->op = nop_op;
      break;
    case get_var_op:
      --cglobs->Uses[rn];
      if (checktemp(arg, rn, ic, cglobs)) {
#ifdef BEAM
        if (cglobs->vreg == rn && !EAM)
#else
        if (cglobs->vreg == rn)
#endif
          cglobs->cint.cpc->op = nop_op;
      }
      if (!cglobs->Uses[rn])
        cglobs->Contents[rn] = cglobs->vadr;
      break;
    case get_val_op:
      --cglobs->Uses[rn];
      checktemp(arg, rn, ic, cglobs);
      if (!cglobs->Uses[rn])
        cglobs->Contents[rn] = cglobs->vadr;
      break;
    case f_0_op:
      if (rn_to_kill[0])
        --cglobs->Uses[rn_to_kill[0]];
      rn_to_kill[1] = rn_to_kill[0] = 0;
      break;
    case f_var_op:
      if (rn_to_kill[0])
        --cglobs->Uses[rn_to_kill[0]];
      rn_to_kill[1] = rn_to_kill[0] = 0;
    case unify_var_op:
    case unify_val_op:
    case unify_last_var_op:
    case unify_last_val_op:
#ifdef SFUNC
    case unify_s_var_op:
    case unify_s_val_op:
#endif
      checktemp(arg, rn, ic, cglobs);
      break;
    case bccall_op:
      checktemp(arg, rn, ic, cglobs);
      checktemp(cglobs->cint.cpc->rnd3, cglobs->cint.cpc->rnd4, ic, cglobs);
      break;
    case get_atom_op:
    case get_num_op:
    case get_float_op:
    case get_longint_op:
    case get_string_op:
    case get_dbterm_op:
    case get_bigint_op:
      --cglobs->Uses[rn];
      /* This is not safe if we are in the middle of a disjunction and there
         is something ahead.
       */
      if (!cglobs->Uses[rn])
        cglobs->Contents[rn] = arg;
      break;
    case get_list_op:
    case get_struct_op:
      --cglobs->Uses[rn];
      if (!cglobs->Uses[rn])
        cglobs->Contents[rn] = NIL;
      break;
    case put_var_op:
    case put_unsafe_op:
      rn = checkreg(arg, rn, ic, TRUE, cglobs);
      checktemp(arg, rn, ic, cglobs);
      cglobs->Contents[rn] = cglobs->vadr;
      ++cglobs->Uses[rn];
      break;
    case put_val_op:
      rn = checkreg(arg, rn, ic, TRUE, cglobs);
      checktemp(arg, rn, ic, cglobs);
#ifdef BEAM
      if (rn && cglobs->Contents[rn] == (Term)cglobs->vadr && !EAM)
#else
      if (rn && cglobs->Contents[rn] == (Term)cglobs->vadr)
#endif
      {
        cglobs->cint.cpc->op = nop_op;
      }
      cglobs->Contents[rn] = cglobs->vadr;
      ++cglobs->Uses[rn];
      if (rn_kills) {
        rn_kills--;
        rn_to_kill[rn_kills] = rn;
      }
      break;
    case fetch_args_cv_op:
    case fetch_args_vc_op:
    case fetch_args_iv_op:
    case fetch_args_vi_op:
      rn_to_kill[1] = rn_to_kill[0] = 0;
      if (cglobs->cint.cpc->nextInst &&
          cglobs->cint.cpc->nextInst->op == put_val_op &&
          cglobs->cint.cpc->nextInst->nextInst &&
          (cglobs->cint.cpc->nextInst->nextInst->op == f_var_op ||
           cglobs->cint.cpc->nextInst->nextInst->op == f_0_op))
        rn_kills = 1;
      break;
    case f_val_op:
#ifdef SFUNC
    case write_s_var_op: {
      Ventry *ve = (Ventry *)arg;

      if ((ve->FlagsOfVE & PermFlag) == 0 && ve->RCountOfVE <= 1)
        cglobs->cint.cpc->op = nop_op;
    } break;
    case write_s_val_op:
#endif
    case write_var_op:
    case write_val_op:
      checktemp(arg, rn, ic, cglobs);
      break;
#ifdef SFUNC
    case put_s_f_op:
      cglobs->Contents[rn] = arg;
      ++cglobs->Uses[rn];
      break;
#endif
    case put_atom_op:
    case put_num_op:
    case put_float_op:
    case put_longint_op:
    case put_string_op:
    case put_dbterm_op:
    case put_bigint_op:
      rn = checkreg(arg, rn, ic, FALSE, cglobs);
      if (cglobs->Contents[rn] == arg)
        cglobs->cint.cpc->op = nop_op;
      cglobs->Contents[rn] = arg;
      ++cglobs->Uses[rn];
      break;
    case put_list_op:
    case put_struct_op:
      rn = checkreg(arg, rn, ic, FALSE, cglobs);
      cglobs->Contents[rn] = NIL;
      ++cglobs->Uses[rn];
      break;
    case commit_b_op:
#ifdef TABLING_INNER_CUTS
      cglobs->cut_mark->op = clause_with_cut_op;
#endif /* TABLING_INNER_CUTS */
    case save_b_op:
    case patch_b_op:
    case save_appl_op:
    case save_pair_op:
      checktemp(arg, rn, ic, cglobs);
      break;
    case safe_call_op:
      /*
        vsc: The variables will be in use after this!!!!
        {
          UInt Arity = RepPredProp((Prop) arg)->ArityOfPE;
          for (rn = 1; rn <= Arity; ++rn)
          --cglobs->Uses[rn];
        }
      */
      break;
    case call_op:
    case orelse_op:
    case orlast_op: {
      up = cglobs->Uses;
      cop = cglobs->Contents;
      for (rn = 1; rn < cglobs->MaxCTemps; ++rn) {
        *up++ = *cop++ = NIL;
      }
    } break;
    case label_op: {
      up = cglobs->Uses;
      cop = cglobs->Contents;
      for (rn = 0; rn < cglobs->MaxCTemps; ++rn) {
        if (*cop != (TempVar | rn)) {
          *up++ = *cop++ = NIL;
        } else {
          up++;
          cop++;
        }
      }
    } break;
    case restore_tmps_and_skip_op:
    case restore_tmps_op:
      /*
        This instruction is required by the garbage collector to find out
        how many temporaries are live right now. It is also useful when
        waking up goals before an either or ! instruction.
      */
      {
        PInstr *mycpc = cglobs->cint.cpc,
               *oldCodeStart = cglobs->cint.CodeStart;
        int i, max;

        /* instructions must be placed at BlobsStart */
        cglobs->cint.CodeStart = cglobs->cint.BlobsStart;
        cglobs->cint.cpc = cglobs->cint.icpc;
        max = 0;
        for (i = 1; i < cglobs->MaxCTemps; ++i) {
          if (cglobs->Contents[i])
            max = i;
        }
        Yap_emit(label_op, ++cglobs->labelno, Zero, &cglobs->cint);
        mycpc->rnd1 = cglobs->labelno;
        rn = copy_live_temps_bmap(max, cglobs);
        cglobs->cint.icpc = cglobs->cint.cpc;
        cglobs->cint.BlobsStart = cglobs->cint.CodeStart;
        cglobs->cint.cpc = mycpc;
        cglobs->cint.CodeStart = oldCodeStart;
      }
    default:
      break;
    }
    if (cglobs->cint.cpc->nextInst)
      cglobs->cint.cpc = cglobs->cint.cpc->nextInst;
    else
      return;
  }
}

static void push_allocate(PInstr *pc, PInstr *oldpc) {
  /*
    The idea is to push an allocate forward as much as we can. This
    delays work in the emulated code, and in the best case we may get rid of
    allocates altogether.
   */
  /* we can push the allocate */
  int safe = TRUE;
  PInstr *initial = oldpc, *dealloc_founds[16];
  int d_founds = 0;
  int level = 0;

  while (pc) {
    switch (pc->op) {
    case jump_op:
      return;
    case call_op:
    case safe_call_op:
      if (!safe)
        return;
      else {
        PInstr *where = initial->nextInst->nextInst;
        while (d_founds)
          dealloc_founds[--d_founds]->op = nop_op;
        if (where == pc || oldpc == initial->nextInst)
          return;
        oldpc->nextInst = initial->nextInst;
        initial->nextInst->nextInst = pc;
        initial->nextInst = where;
        return;
      }
    case push_or_op:
      /* we cannot just put an allocate here, because it may never be executed
       */
      level++;
      safe = FALSE;
      break;
    case pushpop_or_op:
      /* last branch and we did not need an allocate so far, cool! */
      level--;
      if (!level)
        safe = TRUE;
      break;
    case cut_op:
    case either_op:
    case execute_op:
      return;
    case deallocate_op:
      dealloc_founds[d_founds++] = pc;
      if (d_founds == 16)
        return;
    default:
      break;
    }
    oldpc = pc;
    pc = pc->nextInst;
  }
}

static void c_optimize(PInstr *pc) {
  char onTail;
  Ventry *v;
  PInstr *opc = NULL;
  PInstr *inpc = pc;

  pc = inpc;
  opc = NULL;
  /* first reverse the pointers */
  while (pc != NULL) {
    PInstr *tpc = pc->nextInst;
    pc->nextInst = opc;
    opc = pc;
    pc = tpc;
  }
  pc = opc;
  opc = NULL;
  onTail = 1;
  do {
    PInstr *npc = pc->nextInst;
    pc->nextInst = opc;
    switch (pc->op) {
    case get_var_op:
      /* handle clumsy either branches */
      if (npc->op == f_0_op) {
        npc->rnd1 = pc->rnd1;
        npc->op = f_var_op;
        pc->op = nop_op;
        break;
      }
    case put_val_op:
    case get_val_op: {
      Ventry *ve = (Ventry *)pc->rnd1;

      if (ve->KindOfVE == TempVar) {
        UInt argno = ve->NoOfVE & MaskVarAdrs;
        if (argno && argno == pc->rnd2) {
          pc->op = nop_op;
        }
      }
    }
      onTail = 1;
      break;
    case save_pair_op: {
      Term ve = (Term)pc->rnd1;
      PInstr *npc = pc->nextInst;

      if (((Ventry *)ve)->RCountOfVE <= 1)
        pc->op = nop_op;
      else {
        *pc = *npc;
        pc->nextInst = npc;
        npc->op = save_pair_op;
        npc->rnd1 = (CELL)ve;
      }
    } break;
    case save_appl_op: {
      Term ve = (Term)pc->rnd1;
      PInstr *npc = pc->nextInst;

      if (((Ventry *)ve)->RCountOfVE <= 1)
        pc->op = nop_op;
      else {
        *pc = *npc;
        pc->nextInst = npc;
        npc->op = save_appl_op;
        npc->rnd1 = (CELL)ve;
      }
      break;
    }
    case nop_op:
      break;
    case unify_var_op:
    case unify_last_var_op:
#ifdef OLD_SYSTEM
      /* In the good old days Yap would remove lots of small void
       * instructions for a structure. This is not such a
       * good idea nowadays, as we need to know where we
       * finish the structure for the last instructions to
       * work correctly. Instead, we will use unify_void
       * with very little overhead */
      v = (Ventry *)(pc->rnd1);
      if (v->KindOfVE == VoidVar && onTail) {
        pc->op = nop_op;
      } else
#endif /* OLD_SYSTEM */
        onTail = 0;
      break;
    case unify_val_op:
      v = (Ventry *)(pc->rnd1);
      if (!(v->FlagsOfVE & GlobalVal))
        pc->op = unify_local_op;
      onTail = 0;
      break;
    case unify_last_val_op:
      v = (Ventry *)(pc->rnd1);
      if (!(v->FlagsOfVE & GlobalVal))
        pc->op = unify_last_local_op;
      onTail = 0;
      break;
    case write_val_op:
      v = (Ventry *)(pc->rnd1);
      if (!(v->FlagsOfVE & GlobalVal))
        pc->op = write_local_op;
      onTail = 0;
      break;
    case pop_op:
      if (FALSE && onTail == 1) {
        pc->op = nop_op;
        onTail = 1;
        break;
      } else {
        PInstr *p = pc->nextInst;

        while (p != NIL && p->op == nop_op)
          p = p->nextInst;
        if (p != NIL && p->op == pop_op) {
          pc->rnd1 += p->rnd1;
          pc->nextInst = p->nextInst;
        }
        onTail = 2;
        break;
      }
    case write_var_op:
    case unify_atom_op:
    case unify_last_atom_op:
    case write_atom_op:
    case unify_num_op:
    case unify_last_num_op:
    case write_num_op:
    case unify_float_op:
    case unify_last_float_op:
    case write_float_op:
    case unify_longint_op:
    case unify_string_op:
    case unify_bigint_op:
    case unify_last_longint_op:
    case unify_last_string_op:
    case unify_last_bigint_op:
    case write_longint_op:
    case write_string_op:
    case write_bigint_op:
    case unify_list_op:
    case write_list_op:
    case unify_struct_op:
    case write_struct_op:
    case write_unsafe_op:
    case unify_last_list_op:
    case write_last_list_op:
    case unify_last_struct_op:
    case write_last_struct_op:
#ifdef SFUNC
    case unify_s_f_op:
    case write_s_f_op:
#endif
      onTail = 0;
      break;
    default:
      onTail = 1;
      break;
    }
    opc = pc;
    pc = npc;
  } while (pc != NULL);
  pc = inpc;
  opc = NULL;
  while (pc != NULL) {
    if (pc->op == allocate_op) {
      push_allocate(pc, opc);
      break;
    }
    opc = pc;
    pc = pc->nextInst;
  }
}

yamop *Yap_cclause(volatile Term inp_clause, Int NOfArgs, Term mod,
                   volatile Term src) { /* compile a prolog clause, copy of
                                           clause myst be in ARG1 */
  CACHE_REGS
  /* returns address of code for clause */
  Term head, body;
  yamop *acode;
  Term my_clause;

  volatile int maxvnum = 512;
  int botch_why;
  /* may botch while doing a different module */
  /* first, initialize cglobs->cint.CompilerBotch to handle all cases of
   * interruptions */
  compiler_struct cglobs;

#ifdef TABLING_INNER_CUTS
  PInstr cglobs_cut_mark;
  cglobs.cut_mark = &cglobs_cut_mark;
#endif /* TABLING_INNER_CUTS */

  /* make sure we know there was no error yet */
  LOCAL_ErrorMessage = NULL;
  if ((botch_why = sigsetjmp(cglobs.cint.CompilerBotch, 0))) {
    restore_machine_regs();
    reset_vars(cglobs.vtable);
    Yap_ReleaseCMem(&cglobs.cint);
    if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
        Yap_ThrowExistingError();
  }
    switch (botch_why) {
    case OUT_OF_STACK_BOTCH:
      /* out of local stack, just duplicate the stack */
      {
        Int osize = 2 * sizeof(CELL) * (ASP - HR);
        ARG1 = inp_clause;
        ARG3 = src;

        YAPLeaveCriticalSection();
        if (!Yap_gcl(LOCAL_Error_Size, NOfArgs, ENV, gc_P(P, CP))) {
          LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
        }
        if (osize > ASP - HR) {
          if (!Yap_growstack(2 * sizeof(CELL) * (ASP - HR))) {
            LOCAL_Error_TYPE = RESOURCE_ERROR_STACK;
          }
        }
        YAPEnterCriticalSection();
        src = ARG3;
        inp_clause = ARG1;
      }
      break;
    case OUT_OF_AUX_BOTCH:
      /* out of local stack, just duplicate the stack */
      YAPLeaveCriticalSection();
      ARG1 = inp_clause;
      ARG3 = src;
      if (!Yap_ExpandPreAllocCodeSpace(LOCAL_Error_Size, NULL, TRUE)) {
        LOCAL_Error_TYPE = RESOURCE_ERROR_AUXILIARY_STACK;
      }
      YAPEnterCriticalSection();
      src = ARG3;
      inp_clause = ARG1;
      break;
    case OUT_OF_TEMPS_BOTCH:
      /* out of temporary cells */
      if (maxvnum < 16 * 1024) {
        maxvnum *= 2;
      } else {
        maxvnum += 4096;
      }
      break;
    case OUT_OF_HEAP_BOTCH:
      /* not enough heap */
      ARG1 = inp_clause;
      ARG3 = src;
      YAPLeaveCriticalSection();
      if (!Yap_growheap(FALSE, LOCAL_Error_Size, NULL)) {
        LOCAL_Error_TYPE = RESOURCE_ERROR_HEAP;
        return NULL;
      }
      YAPEnterCriticalSection();
      src = ARG3;
      inp_clause = ARG1;
      break;
    case OUT_OF_TRAIL_BOTCH:
      /* not enough trail */
      ARG1 = inp_clause;
      ARG3 = src;
      YAPLeaveCriticalSection();
      if (!Yap_growtrail(LOCAL_TrailTop - (ADDR)TR, FALSE)) {
        LOCAL_Error_TYPE = RESOURCE_ERROR_TRAIL;
        return NULL;
      }
      YAPEnterCriticalSection();
      src = ARG3;
      inp_clause = ARG1;
      break;
    default:
      return NULL;
    }
  }
  my_clause = inp_clause;
  HB = HR;
  LOCAL_ErrorMessage = NULL;
  LOCAL_Error_Size = 0;
  LOCAL_Error_TYPE = YAP_NO_ERROR;
  /* initialize variables for code generation                              */

  cglobs.cint.CodeStart = cglobs.cint.cpc = NULL;
  cglobs.cint.BlobsStart = cglobs.cint.icpc = NULL;
  cglobs.cint.dbterml = NULL;
  cglobs.cint.blks = NULL;
  cglobs.cint.label_offset = NULL;
  cglobs.cint.freep = cglobs.cint.freep0 =
      (char *)(HR + maxvnum + (sizeof(Int) / sizeof(CELL)) * MaxTemps +
               MaxTemps);
  cglobs.cint.success_handler = 0L;
  if (ASP <= CellPtr(cglobs.cint.freep) + 256) {
    cglobs.vtable = NULL;
    LOCAL_Error_Size = (256 + maxvnum) * sizeof(CELL);
    save_machine_regs();
    siglongjmp(cglobs.cint.CompilerBotch, 3);
  }
  cglobs.Uses = (Int *)(HR + maxvnum);
  cglobs.Contents =
      (Term *)(HR + maxvnum + (sizeof(Int) / sizeof(CELL)) * MaxTemps);
  cglobs.curbranch = cglobs.onbranch = 0;
  cglobs.branch_pointer = cglobs.parent_branches;
  cglobs.or_found = FALSE;
  cglobs.max_args = 0;
  cglobs.nvars = 0;
  cglobs.tmpreg = 0;
  cglobs.needs_env = FALSE;
  /*
   * 2000 added to H in case we need to construct call(G) when G is a
   * variable used as a goal
   */
  cglobs.vtable = NULL;
  cglobs.common_exps = NULL;
  cglobs.n_common_exps = 0;
  cglobs.labelno = 0L;
  cglobs.is_a_fact = FALSE;
  cglobs.hasdbrefs = FALSE;
  if (IsVarTerm(my_clause)) {
   Yap_ThrowError(INSTANTIATION_ERROR, my_clause,  "in compiling clause");
 }
  if (IsApplTerm(my_clause) && FunctorOfTerm(my_clause) == FunctorAssert) {
    head = ArgOfTerm(1, my_clause);
    body = ArgOfTerm(2, my_clause);
  } else {
    head = my_clause, body = MkAtomTerm(AtomTrue);
  }
  if (IsVarTerm(head) || IsPairTerm(head) || IsIntTerm(head) ||
      IsFloatTerm(head) || IsRefTerm(head)) {
     Yap_ThrowError(TYPE_ERROR_CALLABLE, my_clause,
             "in compiling clause: clause head should be atom or compound term");
  } else {
  loop:
    /* find out which predicate we are compiling for */
    if (IsAtomTerm(head)) {
      Atom ap = AtomOfTerm(head);
      cglobs.cint.CurrentPred = RepPredProp(PredPropByAtom(ap, mod));
    } else {
      Functor f = FunctorOfTerm(head);
      if (f == FunctorModule) {
	mod = ArgOfTerm(1,head);
	head = ArgOfTerm(2,head);
	goto loop;
      }
      cglobs.cint.CurrentPred =
          RepPredProp(PredPropByFunc(f, mod));
    }
    /* insert extra instructions to count calls */
    PELOCK(52, cglobs.cint.CurrentPred);
    if ((cglobs.cint.CurrentPred->PredFlags & ProfiledPredFlag) ||
        (PROFILING &&
         (cglobs.cint.CurrentPred->FirstClause == NIL))) {
      profiling = TRUE;
      call_counting = FALSE;
    } else if ((cglobs.cint.CurrentPred->PredFlags & CountPredFlag) ||
               (CALL_COUNTING &&
                (cglobs.cint.CurrentPred->FirstClause == NIL))) {
      call_counting = TRUE;
      profiling = FALSE;
    } else {
      profiling = FALSE;
      call_counting = FALSE;
    }
    UNLOCK(cglobs.cint.CurrentPred->PELock);
  }
  cglobs.is_a_fact = (body == MkAtomTerm(AtomTrue));
  /* phase 1 : produce skeleton code and variable information              */

  c_head(head, &cglobs);

  if (cglobs.is_a_fact && !cglobs.vtable) {
#ifdef TABLING
    PELOCK(53, cglobs.cint.CurrentPred);
    if (is_tabled(cglobs.cint.CurrentPred))
      Yap_emit(table_new_answer_op, Zero, cglobs.cint.CurrentPred->ArityOfPE,
               &cglobs.cint);
    else
#endif /* TABLING */
      Yap_emit(procceed_op, Zero, Zero, &cglobs.cint);
#ifdef TABLING
    UNLOCK(cglobs.cint.CurrentPred->PELock);
#endif
    /* ground term, do not need much more work */
    if (cglobs.cint.BlobsStart != NULL) {
      cglobs.cint.cpc->nextInst = cglobs.cint.BlobsStart;
      cglobs.cint.BlobsStart = NULL;
    }
    if (LOCAL_ErrorMessage)
      return (0);
    /* make sure we give enough space for the  fact */
    if (cglobs.space_op)
      cglobs.space_op->rnd1 = cglobs.space_used;

#ifdef DEBUG
    if (GLOBAL_Option['g' - 96] )
      Yap_ShowCode(&cglobs.cint);
#endif
  } else {
#ifdef TABLING_INNER_CUTS
    Yap_emit(nop_op, Zero, Zero, &cglobs.cint);
    cglobs.cut_mark->op = clause_with_cut_op;
#endif /* TABLING_INNER_CUTS */
    Yap_emit(allocate_op, Zero, Zero, &cglobs.cint);

#ifdef BEAM
    if (EAM)
      Yap_emit(body_op, Zero, Zero, &cglobs.cint);
#endif

    c_body(body, mod, &cglobs);
    /* Insert blobs at the very end */

    if (cglobs.space_op)
      cglobs.space_op->rnd1 = cglobs.space_used;

    if (cglobs.cint.BlobsStart != NULL) {
      cglobs.cint.cpc->nextInst = cglobs.cint.BlobsStart;
      cglobs.cint.BlobsStart = NULL;
    }

    reset_vars(cglobs.vtable);
    HR = HB;
    if (B != NULL) {
      HB = B->cp_h;
    }
    if (LOCAL_ErrorMessage)
      return (0);
#ifdef DEBUG
    if (GLOBAL_Option['g' - 96])
      Yap_ShowCode(&cglobs.cint);
#endif
    /* phase 2: classify variables and optimize temporaries          */
    c_layout(&cglobs);
    /* Insert blobs at the very end */
    if (cglobs.cint.BlobsStart != NULL) {
      cglobs.cint.cpc->nextInst = cglobs.cint.BlobsStart;
      cglobs.cint.BlobsStart = NULL;
      while (cglobs.cint.cpc->nextInst != NULL)
        cglobs.cint.cpc = cglobs.cint.cpc->nextInst;
    }
  }
  /* eliminate superfluous pop's and unify_var's                   */
  c_optimize(cglobs.cint.CodeStart);
#ifdef DEBUG
  if (GLOBAL_Option['f' - 96])
    Yap_ShowCode(&cglobs.cint);
#endif

#ifdef BEAM
  {
    void codigo_eam(compiler_struct *);

    if (EAM)
      codigo_eam(&cglobs);
  }
#endif

  /* phase 3: assemble code                                                */
  acode = Yap_assemble(ASSEMBLING_CLAUSE, src, cglobs.cint.CurrentPred,
                       (cglobs.is_a_fact && !cglobs.hasdbrefs &&
                        !(cglobs.cint.CurrentPred->PredFlags & TabledPredFlag)),
                       &cglobs.cint, cglobs.labelno + 1);
  /* check first if there was space for us */
  Yap_ReleaseCMem(&cglobs.cint);
  if (acode == NULL) {
    return NULL;
  } else {
    return acode;
  }
}

#ifdef BEAM
#include "toeam.c"
#endif
