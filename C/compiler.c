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
* Last rev:	4/03/88							 *
* mods:									 *
* comments:	Clause compiler						 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";

#endif	/* SCCS */
#include "Yap.h"
#include "compile.h"
#include "clause.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif

STATIC_PROTO(int active_branch, (int));
STATIC_PROTO(void c_var, (Term, Int, unsigned int, unsigned int));
STATIC_PROTO(void reset_vars, (void));
STATIC_PROTO(Term optimize_ce, (Term, unsigned int, unsigned int));
STATIC_PROTO(void c_arg, (Int, Term, unsigned int, unsigned int));
STATIC_PROTO(void c_args, (Term, unsigned int));
STATIC_PROTO(void c_eq, (Term, Term));
STATIC_PROTO(void c_test, (Int, Term));
STATIC_PROTO(void c_bifun, (Int, Term, Term, Term, int));
STATIC_PROTO(void c_goal, (Term, int));
STATIC_PROTO(void c_body, (Term, int));
STATIC_PROTO(void c_head, (Term));
STATIC_PROTO(int usesvar, (int));
STATIC_PROTO(CELL *init_bvarray, (int));
STATIC_PROTO(void clear_bvarray, (int, CELL *));
STATIC_PROTO(void add_bvarray_op, (PInstr *,CELL *, int));
STATIC_PROTO(void AssignPerm, (PInstr *));
STATIC_PROTO(void CheckUnsafe, (PInstr *));
STATIC_PROTO(void CheckVoids, (void));
STATIC_PROTO( int checktemp, (void));
STATIC_PROTO(void checkreg, (int));
STATIC_PROTO(void c_layout, (void));
STATIC_PROTO(void c_optimize, (PInstr *));

#ifdef SFUNC
STATIC_PROTO(void compile_sf_term, (Term, int));

#endif

static PInstr *BodyStart;

static Ventry *vtable;

static CExpEntry *common_exps;

static int   n_common_exps;

static int goalno, onlast, onhead, onbranch, cur_branch;

typedef struct branch_descriptor {
  int    id;                /* the branch id */
  Term   cm;               /* if a banch is associated with a commit */
} branch;

static branch parent_branches[256], *branch_pointer;

static  Prop current_p0;

static void
push_branch(int id, Term cmvar) {
  branch_pointer->id = onbranch;
  branch_pointer->cm = cmvar;
  branch_pointer++;
}

static int
pop_branch(void) {
  branch_pointer--;
  return(branch_pointer->id);
}

#ifdef TABLING
#define is_tabled(pe)   (pe->PredFlags & TabledPredFlag)
#endif /* TABLING */

#ifdef DEBUG
static int pbvars;
#endif /* DEBUG */

static inline int 
active_branch(int i)
{
  /*  register int *bp;*/

  return (i == onbranch);
  /*  bp = branch_pointer;
  while (bp > parent_branches) {
    if (*--bp == onbranch)
      return (TRUE);
  }
  return(i==onbranch);*/
}

static Int labelno;

static int or_found;

static Int rn, ic, vreg, vadr;
static Term arg;
static Int Uses[MaxTemps];
static Term Contents[MaxTemps];

static Int tmpreg;

static int nvars, MaxCTemps;
static unsigned int max_args;

#define FAIL(M,T,E) { Yap_ErrorMessage=M; Yap_Error_TYPE = T; Yap_Error_Term = E; return; }

#define IsNewVar(v) (Addr(v)<freep0 || Addr(v)>freep)

inline static void pop_code(unsigned int);

inline static void
pop_code(unsigned int level)
{
  if (level == 0)
    return;
  if (cpc->op == pop_op)
    ++(cpc->rnd1);
  else {
    Yap_emit(pop_op, One, Zero);
  }
}

static void
adjust_current_commits(void) {
  branch *bp = branch_pointer;
  while (bp > parent_branches) {
    bp--;
    if (bp->cm != TermNil) {
      c_var(bp->cm, patch_b_flag, 1, 0);
    }
  }
}

static void
c_var(Term t, Int argno, unsigned int arity, unsigned int level)
{
  int flags, new = FALSE;
  Ventry *v = (Ventry *) Deref(t);

  if (IsNewVar(v)) {		/* new var */
    v = (Ventry *) Yap_AllocCMem(sizeof(*v));
#if SBA
    v->SelfOfVE = 0;
#else
    v->SelfOfVE = (CELL) v;
#endif
    v->AdrsOfVE = t;
    *CellPtr(t) = (CELL) v;
    v->KindOfVE = v->NoOfVE = Unassigned;
    flags = 0;
    /* Be careful with eithers. I may make a variable global in a branch,
       and not in another.
       a :- (b([X]) ; c), go(X).
       This variaiable will not be globalised if we are coming from
       the second branch.

       I also need to protect the onhead because Luis uses that to
       optimise unification in the body of a clause, eg
       a :- (X = 2 ; c), go(X).

       And, yes, there is code like this...
     */
    if (((level > 0 || onhead) && cur_branch == 0)
	|| argno == save_pair_flag ||
	argno == save_appl_flag)
      flags |= SafeVar;
    if ((level > 0  && cur_branch == 0) || argno == save_pair_flag ||
	argno == save_appl_flag)
      flags |= GlobalVal;
    v->FlagsOfVE = flags;
    v->BranchOfVE = onbranch;
    v->NextOfVE = vtable;
    v->RCountOfVE = 0;
    v->AgeOfVE = v->FirstOfVE = goalno;
    new = TRUE;
    vtable = v;
  } else {
    v->FlagsOfVE |= NonVoid;
    if (v->BranchOfVE > 0) {
      if (!active_branch(v->BranchOfVE)) {
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
  if (onhead)
    v->FlagsOfVE |= OnHeadFlag;
  switch (argno) {
  case save_b_flag:
    Yap_emit(save_b_op, (CELL) v, Zero);
    break;
  case comit_b_flag:
    Yap_emit(comit_b_op, (CELL) v, Zero);
    Yap_emit(empty_call_op, Zero, Zero);
    Yap_emit(restore_tmps_and_skip_op, Zero, Zero);
    break;
  case patch_b_flag:
    Yap_emit(patch_b_op, (CELL) v, 0);
    break;
  case save_pair_flag:
    Yap_emit(save_pair_op, (CELL) v, 0);
    break;
  case save_appl_flag:
    Yap_emit(save_appl_op, (CELL) v, 0);
    break;
  case f_flag:
    if (new) {
      ++nvars;
      Yap_emit(f_var_op, (CELL) v, (CELL)arity);
    } else
      Yap_emit(f_val_op, (CELL) v, (CELL)arity);
    break;
  case bt1_flag:
    Yap_emit(fetch_args_for_bccall, (CELL)v, 0);
    break;
  case bt2_flag:
    Yap_emit(bccall_op, (CELL)v, (CELL)current_p0);
    break;
  default:
#ifdef SFUNC
    if (argno < 0) {
      if (new)
	Yap_emit((onhead ? unify_s_var_op : write_s_var_op), v, -argno);
      else
	Yap_emit((onhead ? unify_s_val_op : write_s_val_op), v, -argno);
    } else
#endif
    if (onhead) {
      if (level == 0)
	Yap_emit((new ? (++nvars, get_var_op) : get_val_op), (CELL) v, argno);
      else
	Yap_emit((new ? (++nvars, (argno == (Int)arity ?
			       unify_last_var_op :
			       unify_var_op)) :
	      (argno == (Int)arity ? unify_last_val_op :
	       unify_val_op)),
	     (CELL) v, Zero);
    }
    else {
      if (level == 0)
	Yap_emit((new ? (++nvars, put_var_op) : put_val_op), (CELL) v, argno);
      else
	Yap_emit((new ? (++nvars, write_var_op) : write_val_op), (CELL) v, Zero);
    }
  }
  if (new) {
    v->FirstOpForV = cpc;
  }
  ++(v->RCountOfVE);
  if (onlast)
    v->FlagsOfVE |= OnLastGoal;
  if (v->AgeOfVE < goalno)
    v->AgeOfVE = goalno;
}

static void
reset_vars(void)
{
  Ventry *v = vtable;
  CELL *t;

  while (v != NIL) {
    t = (CELL *) v->AdrsOfVE;
    RESET_VARIABLE(t);
    v = v->NextOfVE;
  }
}

static Term
optimize_ce(Term t, unsigned int arity, unsigned int level)
{
  CExpEntry *p = common_exps, *parent = common_exps;
  int cmp = 0;

  if (IsApplTerm(t) && IsExtensionFunctor(FunctorOfTerm(t)))
    return (t);
  while (p != NULL) {
    CELL *OldH = H;
    H = (CELL *)freep;
    cmp = Yap_compare_terms(t, (p->TermOfCE));
    H = OldH;

    if (cmp > 0) {
      parent = p;
      p = p->RightCE;
    }
    else if (cmp < 0) {
      parent = p;
      p = p->LeftCE;
    }
    else
      break;
  }
  if (p != NULL) {		/* already there */
    return (p->VarOfCE);
  }
  /* first occurrence */
  if (onbranch)
    return (t);
  ++n_common_exps;
  p = (CExpEntry *) Yap_AllocCMem(sizeof(CExpEntry));

  p->TermOfCE = t;
  p->VarOfCE = MkVarTerm();
  if (H >= (CELL *)freep0) {
    /* oops, too many new variables */
    save_machine_regs();
    longjmp(Yap_CompilerBotch,4);
  }
  p->RightCE = NULL;
  p->LeftCE = NULL;
  if (parent == NULL)
    common_exps = p;
  else if (cmp > 0)
    parent->RightCE = p;
  else				/* if (cmp < 0) */
    parent->LeftCE = p;
  if (IsApplTerm(t))
    c_var(p->VarOfCE, save_appl_flag, arity, level);
  else if (IsPairTerm(t))
    c_var(p->VarOfCE, save_pair_flag, arity, level);
  return (t);
}

#ifdef SFUNC
static void
compile_sf_term(Term t, int argno, int level)
{
  Functor f = FunctorOfTerm(t);
  CELL *p = ArgsOfSFTerm(t) - 1;
  SFEntry *pe = RepSFProp(Yap_GetAProp(NameOfFunctor(f), SFProperty));
  Term nullvalue = pe->NilValue;

  if (level == 0)
    Yap_emit((onhead ? get_s_f_op : put_s_f_op), f, argno);
  else
    Yap_emit((onhead ? unify_s_f_op : write_s_f_op), f, Zero);
  ++level;
  while ((argno = *++p)) {
    t = Derefa(++p);
    if (t != nullvalue) {
      if (IsAtomicTerm(t))
	Yap_emit((onhead ? unify_s_a_op : write_s_a_op), t, (CELL) argno);
      else if (!IsVarTerm(t)) {
	Yap_Error_TYPE = SYSTEM_ERROR;
	Yap_Error_Term = TermNil;
	Yap_ErrorMessage = "illegal argument of soft functor";
	save_machine_regs();
	longjmp(Yap_CompilerBotch, 2);
      }
      else
	c_var(t, -argno, arity, level);
    }
  }
  --level;
  if (level == 0)
    Yap_emit((onhead ? get_s_end_op : put_s_end_op), Zero, Zero);
  else
    Yap_emit((onhead ? unify_s_end_op : write_s_end_op), Zero, Zero);
}
#endif

inline static void
c_args(Term app, unsigned int level)
{
  Functor f = FunctorOfTerm(app);
  unsigned int Arity = ArityOfFunctor(f);
  unsigned int i;

  if (level == 0) {
    if (Arity >= MaxTemps) {
      Yap_Error_TYPE = SYSTEM_ERROR;
      Yap_Error_Term = TermNil;
      Yap_ErrorMessage = "exceed maximum arity of compiled goal";
      save_machine_regs();
      longjmp(Yap_CompilerBotch, 2);
    }
    if (Arity > max_args)
      max_args = Arity;
  }
  for (i = 1; i <= Arity; ++i)
    c_arg(i, ArgOfTerm(i, app), Arity, level);
}

static void
c_arg(Int argno, Term t, unsigned int arity, unsigned int level)
{
 restart:
  if (IsVarTerm(t))
    c_var(t, argno, arity, level);
  else if (IsAtomTerm(t)) {
    if (level == 0)
      Yap_emit((onhead ? get_atom_op : put_atom_op), (CELL) t, argno);
    else
      Yap_emit((onhead ? (argno == (Int)arity ? unify_last_atom_op
		      : unify_atom_op) :
	    write_atom_op), (CELL) t, Zero);
  }
  else if (IsIntegerTerm(t) || IsFloatTerm(t) || IsBigIntTerm(t)) {
    if (!IsIntTerm(t)) {
      /* we are taking a blob, that is a binary that is supposed to be
	 guarded in the clause itself. Possible examples include
	 floats, long ints, bignums, bitmaps.... */
      CELL l1 = ++labelno;
      CELL *src = RepAppl(t);
      PInstr *ocpc = cpc, *OCodeStart = CodeStart;

      /* use a special list to store the blobs */
      cpc = icpc;
      Yap_emit(label_op, l1, Zero);
      if (IsFloatTerm(t)) {
	/* let us do floats first */
	CELL *dest = 
	  Yap_emit_extra_size(blob_op,
			  (CELL)(SIZEOF_DOUBLE/SIZEOF_LONG_INT+1),
			  (1+SIZEOF_DOUBLE/SIZEOF_LONG_INT)*CellSize);
	/* copy the float bit by bit */
	dest[0] = src[0];
	dest[1] = src[1];
#if SIZEOF_DOUBLE == 2*SIZEOF_LONG_INT
	dest[2] = src[2];
#endif
	/* note that we don't need to copy size info, unless we wanted
	   to garbage collect clauses ;-) */
	icpc = cpc;
	if (BlobsStart == NULL)
	  BlobsStart = CodeStart;
	cpc = ocpc;
	CodeStart = OCodeStart;
	/* The argument to pass to the structure is now the label for
	   where we are storing the blob */
	if (level == 0)
	  Yap_emit((onhead ? get_float_op : put_float_op), l1, argno);
	else
	  Yap_emit((onhead ? (argno == (Int)arity ? unify_last_float_op
			  : unify_float_op) :
		write_float_op), l1, Zero);
#if USE_GMP
      } else if (IsBigIntTerm(t)) {
	/* next, let us do bigints */
	Int sz = sizeof(CELL)+
	  sizeof(MP_INT)+
	   ((((MP_INT *)(RepAppl(t)+1))->_mp_alloc)*sizeof(mp_limb_t));
	CELL *dest = 
	  Yap_emit_extra_size(blob_op, sz/CellSize, sz);
	/* copy the bignum */
	memcpy(dest, src, sz);
	/* note that we don't need to copy size info, unless we wanted
	 to garbage collect clauses ;-) */
	icpc = cpc;
	if (BlobsStart == NULL)
	  BlobsStart = CodeStart;
	cpc = ocpc;
	CodeStart = OCodeStart;
	/* The argument to pass to the structure is now the label for
	   where we are storing the blob */
	if (level == 0)
	  Yap_emit((onhead ? get_bigint_op : put_bigint_op), l1, argno);
	else
	  Yap_emit((onhead ? (argno == (Int)arity ? unify_last_bigint_op
			  : unify_bigint_op) :
		write_bigint_op), l1, Zero);
#endif
      } else {
	/* for now, it's just a long int */
	CELL *dest = 
	  Yap_emit_extra_size(blob_op,
			  2,
			  2*CellSize);
	/* copy the long int in one fell swoop */
	dest[0] = src[0];
	dest[1] = src[1];
	icpc = cpc;
	if (BlobsStart == NULL)
	  BlobsStart = CodeStart;
	cpc = ocpc;
	CodeStart = OCodeStart;
	if (level == 0)
	  Yap_emit((onhead ? get_longint_op : put_longint_op), l1, argno);
	else
	  Yap_emit((onhead ? (argno == (Int)arity ? unify_last_longint_op
			  : unify_longint_op) :
		write_longint_op), l1, Zero);
      }
      /* That's it folks! */
      return;
    } 
    if (level == 0)
      Yap_emit((onhead ? get_num_op : put_num_op), (CELL) t, argno);
    else
      Yap_emit((onhead ? (argno == (Int)arity ? unify_last_num_op
		      : unify_num_op) :
	    write_num_op), (CELL) t, Zero);
  }
  else if (IsPairTerm(t)) {
    if (optimizer_on && (!onhead || argno != 1 || level > 1) && level < 6) {
      t = optimize_ce(t, arity, level);
      if (IsVarTerm(t)) {
	c_var(t, argno, arity, level);
	return;
      }
    }
    if (level == 0)
      Yap_emit((onhead ? get_list_op : put_list_op), Zero, argno);
    else if (argno == (Int)arity)
      Yap_emit((onhead ? unify_last_list_op : write_last_list_op), Zero, Zero);
    else
      Yap_emit((onhead ? unify_list_op : write_list_op), Zero, Zero);
    ++level;
    c_arg(1, HeadOfTerm(t), 2, level);
    if (argno == (Int)arity) {
      /* optimise for tail recursion */
      t = TailOfTerm(t);
      goto restart;
    }
    c_arg(2, TailOfTerm(t), 2, level);
    --level;
    if (argno != (Int)arity) {
      pop_code(level);
    }
  } else if (IsRefTerm(t)) {
    READ_LOCK(CurrentPred->PRWLock);
    if (!(CurrentPred->PredFlags & (DynamicPredFlag|LogUpdatePredFlag))) {
      READ_UNLOCK(CurrentPred->PRWLock);
      FAIL("can not compile data base reference",TYPE_ERROR_CALLABLE,t);
    } else {
      Yap_emit((onhead ? get_atom_op : put_atom_op), (CELL) t, argno);      
    }
  } else {

#ifdef SFUNC
    if (SFTerm(t)) {
      compile_sf_term(t, argno);
      return;
    }
#endif

    if (optimizer_on && (!onhead || argno != 1 || level > 1)) {
      t = optimize_ce(t, arity, level);
      if (IsVarTerm(t)) {
	c_var(t, argno, arity, level);
	return;

      }
    }
    if (level == 0)
      Yap_emit((onhead ? get_struct_op : put_struct_op),
	   (CELL) FunctorOfTerm(t), argno);
    else if (argno == (Int)arity)
      Yap_emit((onhead ? unify_last_struct_op : write_last_struct_op),
	   (CELL) FunctorOfTerm(t), Zero);
    else
      Yap_emit((onhead ? unify_struct_op : write_struct_op),
	   (CELL) FunctorOfTerm(t), Zero);
    ++level;
    c_args(t, level);
    --level;
    if (argno != (Int)arity) {
      pop_code(level);
    }
  }
}

static void
c_eq(Term t1, Term t2)
{
  Term t;

  --tmpreg;
  if (IsVarTerm(t2))
    t = t2, t2 = t1, t1 = t;
  if (IsVarTerm(t1)) {
    if (IsVarTerm(t2)) {	/* both are variables */
      if (IsNewVar(t2))
	t = t2, t2 = t1, t1 = t;
      c_var(t2, tmpreg, 2, 0);
      onhead = 1;
      c_var(t1, tmpreg, 2, 0);
      onhead = 0;
    }
    else if (IsNewVar(t1)) {
      c_arg(tmpreg, t2, 0, 0);
      onhead = 1;
      c_var(t1, tmpreg, 2, 0);
      onhead = 0;
    }
    else {			/* t2 is non var */
      c_var(t1, tmpreg, 2, 0);
      onhead = 1;
      c_arg(tmpreg, t2, 0, 0);
      onhead = 0;
    }
  }
  else {
    c_arg(tmpreg, t1, 0, 0);
    onhead = 1;
    c_arg(tmpreg, t2, 0, 0);
    onhead = 0;
  }
}

static void
c_test(Int Op, Term t1) {
  Term t = Deref(t1);

  if (!IsVarTerm(t)) {
    char s[32];

    Yap_Error_TYPE = TYPE_ERROR_VARIABLE;
    Yap_Error_Term = t;
    Yap_ErrorMessage = Yap_ErrorSay;
    Yap_bip_name(Op, s);
    sprintf(Yap_ErrorMessage, "when compiling %s/1", s);
    save_machine_regs();
    longjmp(Yap_CompilerBotch, 1);
  }
  if (IsNewVar(t)) {
    /* in this case, var trivially succeeds and the others trivially fail */
    if (Op != _var)
      Yap_emit(fail_op, Zero, Zero);
  } else {
    c_var(t,f_flag,(unsigned int)Op, 0);
  }
}

/* Arithmetic builtins will be compiled in the form:

   fetch_args_vv   Xi,Xj
   put_val	   Xi,Ri
   put_val	   Xj,Rj
   put_var	   Xk,Ak
   bip_body	   Op,Xk

The put_var should always be disposable, and the put_vals can be disposed of if R is an X.
This, in the best case, Ri and Rj are WAM temp registers and this will reduce to:

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
static void
c_bifun(Int Op, Term t1, Term t2, Term t3, int mod)
{
  /* compile Z = X Op Y  arithmetic function */
  /* first we fetch the arguments */
  if (IsVarTerm(t1)) {
    if (IsNewVar(t1)) {
      char s[32];

      Yap_Error_TYPE = INSTANTIATION_ERROR;
      Yap_Error_Term = t1;
      Yap_ErrorMessage = Yap_ErrorSay;
      Yap_bip_name(Op, s);
      sprintf(Yap_ErrorMessage, "when compiling %s/2",  s);
      save_machine_regs();
      longjmp(Yap_CompilerBotch, 1);
    } else if (IsVarTerm(t2)) {
      if (IsNewVar(t2)) {
	char s[32];

	Yap_Error_TYPE = INSTANTIATION_ERROR;
	Yap_Error_Term = t2;
	Yap_ErrorMessage = Yap_ErrorSay;
	Yap_bip_name(Op, s);
	sprintf(Yap_ErrorMessage, "when compiling %s/2",  s);
	save_machine_regs();
	longjmp(Yap_CompilerBotch, 1);
      } else {
	/* first temp */
	Int v1 = --tmpreg;
	/* second temp */
	Int v2 = --tmpreg;
	Yap_emit(fetch_args_vv_op, Zero, Zero);
	/* these should be the arguments */
	c_var(t1, v1, 0, 0);
	c_var(t2, v2, 0, 0);
	/* now we know where the arguments are */
      }
    } else {
      if (Op == _arg) {
	Term tn = MkVarTerm();
	Int v1 = --tmpreg;
	Int v2 = --tmpreg;
	c_arg(t2, v2, 0, 0);
	Yap_emit(fetch_args_vv_op, Zero, Zero);
	/* these should be the arguments */
	c_var(t1, v1, 0, 0);
	c_var(tn, v2, 0, 0);
      /* it has to be either an integer or a floating point */
      } else if (IsIntTerm(t2)) {
	/* first temp */
	Int v1 = --tmpreg;
	Yap_emit(fetch_args_vc_op, (CELL)IntOfTerm(t2), Zero);
	/* these should be the arguments */
	c_var(t1, v1, 0, 0);
	/* now we know where the arguments are */
      } else if (IsLongIntTerm(t2)) {
	/* first temp */
	Int v1 = --tmpreg;
	Yap_emit(fetch_args_vc_op, (CELL)LongIntOfTerm(t2), Zero);
	/* these should be the arguments */
	c_var(t1, v1, 0, 0);
	/* now we know where the arguments are */
      } else {
	char s[32];

	Yap_Error_TYPE = TYPE_ERROR_NUMBER;
	Yap_Error_Term = t2;
	Yap_ErrorMessage = Yap_ErrorSay;
	Yap_bip_name(Op, s);
	sprintf(Yap_ErrorMessage, "compiling %s/2 with output bound", s);
	save_machine_regs();
	longjmp(Yap_CompilerBotch,1);
      }
    }
  } else { /* t1 is bound */
    /* it has to be either an integer or a floating point */
    if (IsVarTerm(t2)) {
      if (IsNewVar(t2)) {
	char s[32];

	Yap_Error_TYPE = INSTANTIATION_ERROR;
	Yap_Error_Term = t2;
	Yap_ErrorMessage = Yap_ErrorSay;
	Yap_bip_name(Op, s);
	sprintf(Yap_ErrorMessage, "compiling functor/3");
	save_machine_regs();
	longjmp(Yap_CompilerBotch,1);
      }
    } else {
      if (Op == _functor) {
	/* both arguments are bound, we must perform unification */
	Int i2;
	
	if (!IsIntegerTerm(t2)) {
	  char s[32];

	  Yap_Error_TYPE = TYPE_ERROR_INTEGER;
	  Yap_Error_Term = t2;
	  Yap_ErrorMessage = Yap_ErrorSay;
	  Yap_bip_name(Op, s);
	  sprintf(Yap_ErrorMessage, "compiling functor/3");
	  save_machine_regs();
	  longjmp(Yap_CompilerBotch,1);
	}
	i2 = IntegerOfTerm(t2);
	if (i2 < 0) {
	  char s[32];

	  Yap_Error_TYPE = DOMAIN_ERROR_NOT_LESS_THAN_ZERO;
	  Yap_Error_Term = t2;
	  Yap_ErrorMessage = Yap_ErrorSay;
	  Yap_bip_name(Op, s);
	  sprintf(Yap_ErrorMessage, "compiling functor/3");
	  save_machine_regs();
	  longjmp(Yap_CompilerBotch,1);
	}
	if (IsNumTerm(t1)) {
	  /* we will always fail */
	  if (i2)
	    c_goal(MkAtomTerm(AtomFalse), mod);
	} else if (!IsAtomTerm(t1)) {
	  char s[32];

	  Yap_Error_TYPE = TYPE_ERROR_ATOM;
	  Yap_Error_Term = t2;
	  Yap_ErrorMessage = Yap_ErrorSay;
	  Yap_bip_name(Op, s);
	  sprintf(Yap_ErrorMessage, "compiling functor/3");
	  save_machine_regs();
	  longjmp(Yap_CompilerBotch,1);
	}
	if (i2 == 0)
	  c_eq(t1, t3);
	else {
	  CELL *hi = H;
	  Int i;

	  if (t1 == TermDot && i2 == 2) {
	    if (H+2 >= (CELL *)freep0) {
	      /* oops, too many new variables */
	      save_machine_regs();
	      longjmp(Yap_CompilerBotch,4);
	    }
	    RESET_VARIABLE(H);
	    RESET_VARIABLE(H+1);
	    H += 2;
	    c_eq(AbsPair(H-2),t3);
	  } else {
	    *H++ = (CELL)Yap_MkFunctor(AtomOfTerm(t1),i2);
	    for (i=0; i < i2; i++) {
	      if (H >= (CELL *)freep0) {
		/* oops, too many new variables */
		save_machine_regs();
		longjmp(Yap_CompilerBotch,4);
	      }
	      RESET_VARIABLE(H);
	      H++;	    
	    }
	    c_eq(AbsAppl(hi),t3);
	  }
	}
      } else if (Op == _arg) {
	Int i1;
	if (IsIntegerTerm(t1))
	  i1 = IntegerOfTerm(t1);
	else {
	  char s[32];

	  Yap_Error_TYPE = TYPE_ERROR_INTEGER;
	  Yap_Error_Term = t2;
	  Yap_ErrorMessage = Yap_ErrorSay;
	  Yap_bip_name(Op, s);
	  sprintf(Yap_ErrorMessage, "compiling %s/2",  s);
	  save_machine_regs();
	  longjmp(Yap_CompilerBotch,1);
	}
	if (IsAtomicTerm(t2) ||
	    (IsApplTerm(t2) && IsExtensionFunctor(FunctorOfTerm(t2)))) {
	  char s[32];

	  Yap_Error_TYPE = TYPE_ERROR_COMPOUND;
	  Yap_Error_Term = t2;
	  Yap_ErrorMessage = Yap_ErrorSay;
	  Yap_bip_name(Op, s);
	  sprintf(Yap_ErrorMessage, "compiling %s/2",  s);
	  save_machine_regs();
	  longjmp(Yap_CompilerBotch,1);
	} else if (IsApplTerm(t2)) {
	  Functor f = FunctorOfTerm(t2);
	  if (i1 < 1 || i1 > ArityOfFunctor(f)) {
	    c_goal(MkAtomTerm(AtomFalse), mod);
	  } else {
	    c_eq(ArgOfTerm(i1, t2), t3);
	  }
	  return;
	} else if (IsPairTerm(t2)) {
	  switch (i1) {
	  case 1:
	    c_eq(HeadOfTerm(t2), t3);
	    return;
	  case 2:
	    c_eq(TailOfTerm(t2), t3);
	    return;
	  default:
	    c_goal(MkAtomTerm(AtomFalse), mod);
	    return;
	  }
	}
      } else {
	char s[32];

	Yap_Error_TYPE = TYPE_ERROR_INTEGER;
	Yap_Error_Term = t2;
	Yap_ErrorMessage = Yap_ErrorSay;
	Yap_bip_name(Op, s);
	sprintf(Yap_ErrorMessage, "compiling %s/2",  s);
	save_machine_regs();
	longjmp(Yap_CompilerBotch,1);
      }
    }
    if (Op == _functor) {
      if (!IsAtomicTerm(t1)) {
	char s[32];

	Yap_Error_TYPE = TYPE_ERROR_ATOM;
	Yap_Error_Term = t1;
	Yap_ErrorMessage = Yap_ErrorSay;
	Yap_bip_name(Op, s);
	sprintf(Yap_ErrorMessage, "compiling %s/2",  s);
	save_machine_regs();
	longjmp(Yap_CompilerBotch,1);
      } else {
	if (!IsVarTerm(t2)) {
	  Int arity;

	  /* We actually have the term ready, so let's just do the unification now */
	  if (!IsIntegerTerm(t2)) {
	    char s[32];

	    Yap_Error_TYPE = TYPE_ERROR_INTEGER;
	    Yap_Error_Term = t2;
	    Yap_ErrorMessage = Yap_ErrorSay;
	    Yap_bip_name(Op, s);
	    sprintf(Yap_ErrorMessage, "compiling %s/2",  s);
	    save_machine_regs();
	    longjmp(Yap_CompilerBotch,1);
	  }
	  arity = IntOfTerm(t2);
	  if (arity < 0) {
	    /* fail straight away */
	    Yap_emit(fail_op, Zero, Zero);
	  }
	  if (arity) {
	    Term tnew;
	    if (!IsAtomTerm(t1)) {
	      char s[32];

	      Yap_Error_TYPE = TYPE_ERROR_ATOM;
	      Yap_Error_Term = t1;
	      Yap_ErrorMessage = Yap_ErrorSay;
	      Yap_bip_name(Op, s);
	      sprintf(Yap_ErrorMessage, "compiling %s/2",  s);
	      save_machine_regs();
	      longjmp(Yap_CompilerBotch,1);
	    }
	    if (H+1+arity >= (CELL *)freep0) {
	      /* oops, too many new variables */
	      save_machine_regs();
	      longjmp(Yap_CompilerBotch,4);
	    }
	    tnew = AbsAppl(H);
	    *H++ = (CELL)Yap_MkFunctor(AtomOfTerm(t1),arity);
	    while (arity--) {
	      RESET_VARIABLE(H);
	      H++;
	    }
	    c_eq(tnew, t3);
	  } else {
	    /* just unify the two arguments */
	    c_eq(t1,t3);
	  }
	  return;
	} else {
	  /* first temp */
	  Int v1 = --tmpreg;
	  Yap_emit(fetch_args_cv_op, t1, Zero);
	  /* these should be the arguments */
	  c_var(t2, v1, 0, 0);
	  /* now we know where the arguments are */
	}
      }
    } else if (IsIntTerm(t1)) {
      /* first temp */
      Int v1 = --tmpreg;
      Yap_emit(fetch_args_cv_op, (CELL)IntOfTerm(t1), Zero);
      /* these should be the arguments */
      c_var(t2, v1, 0, 0);
      /* now we know where the arguments are */
    } else if (IsLongIntTerm(t1)) {
      /* first temp */
      Int v1 = --tmpreg;
      Yap_emit(fetch_args_cv_op, (CELL)LongIntOfTerm(t1), Zero);
      /* these should be the arguments */
      c_var(t2, v1, 0, 0);
      /* now we know where the arguments are */
    } else {
      char s[32];

      Yap_Error_TYPE = TYPE_ERROR_VARIABLE;
      Yap_Error_Term = t1;
      Yap_ErrorMessage = Yap_ErrorSay;
      Yap_bip_name(Op, s);
      sprintf(Yap_ErrorMessage, "compiling %s/2 with output bound",  s);
      save_machine_regs();
      longjmp(Yap_CompilerBotch,1);
    }
  }      
  /* then we compile the opcode/result */
  if (!IsVarTerm(t3)) {
    if (Op == _arg) {
      Term tmpvar = MkVarTerm();
      if (H == (CELL *)freep0) {
	/* oops, too many new variables */
	save_machine_regs();
	longjmp(Yap_CompilerBotch,4);
      }
      c_var(tmpvar,f_flag,(unsigned int)Op, 0);
      c_eq(tmpvar,t3);
    } else {
      char s[32];

      Yap_Error_TYPE = TYPE_ERROR_VARIABLE;
      Yap_Error_Term = t3;
      Yap_ErrorMessage = Yap_ErrorSay;
      Yap_bip_name(Op, s);
      sprintf(Yap_ErrorMessage, "compiling %s/2 with input unbound",  s);
      save_machine_regs();
      longjmp(Yap_CompilerBotch,1);
    }
  } else if (IsNewVar(t3) && cur_branch == 0 /* otherwise you may have trouble with z(X) :- ( Z is X*2 ; write(Z)) */) {
    c_var(t3,f_flag,(unsigned int)Op, 0);
    if (Op == _functor) {
      Yap_emit(empty_call_op, Zero, Zero);
      Yap_emit(restore_tmps_and_skip_op, Zero, Zero);
    }
  } else {
    /* generate code for a temp and then unify temp with previous variable */ 
    Term tmpvar = MkVarTerm();
    if (H == (CELL *)freep0) {
      /* oops, too many new variables */
      save_machine_regs();
      longjmp(Yap_CompilerBotch,4);
    }
    c_var(tmpvar,f_flag,(unsigned int)Op, 0);
    /* I have to dit here, before I do the unification */
    if (Op == _functor) {
      Yap_emit(empty_call_op, Zero, Zero);
      Yap_emit(restore_tmps_and_skip_op, Zero, Zero);
    }
    c_eq(tmpvar,t3);
  }
}

static void
c_functor(Term Goal, int mod)
{
  Term t1 = ArgOfTerm(1, Goal);
  Term t2 = ArgOfTerm(2, Goal);
  Term t3 = ArgOfTerm(3, Goal);
  if (IsVarTerm(t1) && IsNewVar(t1)) {
    c_bifun(_functor, t2, t3, t1, mod);
  } else if (IsNonVarTerm(t1)) {
    /* just split the structure */
    if (IsAtomicTerm(t1)) {
      c_eq(t1,t2);
      c_eq(t3,MkIntTerm(0));
    } else if (IsApplTerm(t1)) {
      Functor f = FunctorOfTerm(t1);
      c_eq(t2,MkAtomTerm(NameOfFunctor(f)));
      c_eq(t3,MkIntegerTerm(ArityOfFunctor(f)));
    } else /* list */ {
      c_eq(t2,TermDot);
      c_eq(t3,MkIntTerm(2));
    }
  } else if (IsVarTerm(t2) && IsNewVar(t2) &&
	     IsVarTerm(t3) && IsNewVar(t3)) {
    Int v1 = --tmpreg;
    Yap_emit(fetch_args_vc_op, Zero, Zero);
    c_var(t1, v1, 0, 0);
    c_var(t2,f_flag,(unsigned int)_functor, 0);
    c_var(t3,f_flag,(unsigned int)_functor, 0);
  } else {
    Functor f = FunctorOfTerm(Goal);
    Prop p0 = PredPropByFunc(f, mod);
    if (profiling)
      Yap_emit(enter_profiling_op, (CELL)RepPredProp(p0), Zero);
    else if (call_counting)
      Yap_emit(count_call_op, (CELL)RepPredProp(p0), Zero);
    c_args(Goal, 0);
    Yap_emit(safe_call_op, (CELL)p0 , Zero);
    Yap_emit(empty_call_op, Zero, Zero);
    Yap_emit(restore_tmps_and_skip_op, Zero, Zero);
  }
}

static int
IsTrueGoal(Term t) {
  if (IsVarTerm(t)) return(FALSE);
  if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (f == FunctorModule) {
      return(IsTrueGoal(ArgOfTerm(2,t)));
    }
    if (f == FunctorComma || f == FunctorOr || f == FunctorArrow) {
      return(IsTrueGoal(ArgOfTerm(1,t)) && IsTrueGoal(ArgOfTerm(2,t)));
    }
    return(FALSE);
  }
  return(t == MkAtomTerm(AtomTrue));
}

static void
c_goal(Term Goal, int mod)
{
  Functor f;
  PredEntry *p;
  Prop p0;

  if (IsVarTerm(Goal)) {
    Goal = Yap_MkApplTerm(FunctorCall, 1, &Goal);
  }
  if (IsApplTerm(Goal) && FunctorOfTerm(Goal) == FunctorModule) {
    Term M = ArgOfTerm(1, Goal);

    if (IsVarTerm(M) || !IsAtomTerm(M)) {
      if (IsVarTerm(M)) {	
	Yap_Error_TYPE = INSTANTIATION_ERROR;
      } else {
	Yap_Error_TYPE = TYPE_ERROR_ATOM;
      }
      Yap_Error_Term = M;
      Yap_ErrorMessage = "in module name";
      save_machine_regs();
      longjmp(Yap_CompilerBotch, 1);
    }
    Goal = ArgOfTerm(2, Goal);
    mod = Yap_LookupModule(M);
  }
  if (IsVarTerm(Goal)) {
    Goal = Yap_MkApplTerm(FunctorCall, 1, &Goal);
  } else if (IsNumTerm(Goal)) {
    FAIL("goal can not be a number", TYPE_ERROR_CALLABLE, Goal);
  } else if (IsRefTerm(Goal)) {
    Yap_Error_TYPE = TYPE_ERROR_DBREF;
    Yap_Error_Term = Goal;
    FAIL("goal argument in static procedure can not be a data base reference", TYPE_ERROR_CALLABLE, Goal);
  }
  else if (IsPairTerm(Goal)) {
    Goal = Yap_MkApplTerm(FunctorCall, 1, &Goal);
  }
  if (IsAtomTerm(Goal)) {
    Atom atom = AtomOfTerm(Goal);

    if (atom == AtomFail || atom == AtomFalse) {
      Yap_emit(fail_op, Zero, Zero);
      return;
    }
    else if (atom == AtomTrue || atom == AtomOtherwise) {
      if (onlast) {
	Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	if (is_tabled(CurrentPred))
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	else
#endif /* TABLING */
	  Yap_emit(procceed_op, Zero, Zero);
      }
      return;
    }
    else if (atom == AtomCut) {
      if (profiling)
	Yap_emit(enter_profiling_op, (CELL)RepPredProp(PredPropByAtom(AtomCut,0)), Zero);
      else if (call_counting)
	Yap_emit(count_call_op, (CELL)RepPredProp(PredPropByAtom(AtomCut,0)), Zero);
      if (onlast) {
	/* never a problem here with a -> b, !, c ; d */
	Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	READ_LOCK(CurrentPred->PRWLock);
	if (is_tabled(CurrentPred)) {
	  Yap_emit(cut_op, Zero, Zero);
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	}
	else
#endif /* TABLING */
	  {
	    Yap_emit(cutexit_op, Zero, Zero);
	  }
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      }
      else {
	Yap_emit(cut_op, Zero, Zero);
	/* needs to adjust previous commits */
	adjust_current_commits();
      }
      return;
    }
#ifndef YAPOR
    else if (atom == AtomRepeat) {
      CELL l1 = ++labelno;
      CELL l2 = ++labelno;

      if (profiling)
	Yap_emit(enter_profiling_op, (CELL)RepPredProp(PredPropByAtom(AtomRepeat,0)), Zero);
      else if (call_counting)
	Yap_emit(count_call_op, (CELL)RepPredProp(PredPropByAtom(AtomRepeat,0)), Zero);
      or_found = 1;
      push_branch(onbranch, TermNil);
      cur_branch++;
      onbranch = cur_branch;
      if (onlast)
	Yap_emit(deallocate_op, Zero, Zero);
      Yap_emit_3ops(push_or_op, l1, Zero, Zero);
      Yap_emit_3ops(either_op, l1, Zero, Zero);
      Yap_emit(restore_tmps_op, Zero, Zero);
      Yap_emit(jump_op, l2, Zero);
      Yap_emit(label_op, l1, Zero);
      Yap_emit(pushpop_or_op, Zero, Zero);
      Yap_emit_3ops(orelse_op, l1, Zero, Zero);
      Yap_emit(label_op, l2, Zero);
      if (onlast) {
#ifdef TABLING
	READ_LOCK(CurrentPred->PRWLock);
	if (is_tabled(CurrentPred))
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	else
#endif /* TABLING */
	  Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      }
      else
	++goalno;
      onbranch = pop_branch();
      Yap_emit(pop_or_op, Zero, Zero);
      /*                      --onbranch; */
      return;
    }
#endif /* YAPOR */
    p = RepPredProp(p0 = PredPropByAtom(atom, mod));
    /* if we are profiling, make sure we register we entered this predicate */
    if (profiling)
      Yap_emit(enter_profiling_op, (CELL)p, Zero);
    if (call_counting)
      Yap_emit(count_call_op, (CELL)p, Zero);
  }
  else {
    f = FunctorOfTerm(Goal);
    p = RepPredProp(p0 = PredPropByFunc(f, mod));
    if (f == FunctorOr) {
      CELL l = ++labelno;
      CELL m = ++labelno;
      Term arg;
      int save = onlast;
      int savegoalno = goalno;
      int frst = TRUE;
      int comitflag = 0;
      int looking_at_comit = FALSE;
      int optimizing_comit = FALSE;
      Term comitvar = 0;
      PInstr *FirstP = cpc, *savecpc, *savencpc;

      push_branch(onbranch, TermNil);
      ++cur_branch;
      onbranch = cur_branch;
      or_found = 1;
      do {
	arg = ArgOfTerm(1, Goal);
	looking_at_comit = IsApplTerm(arg) &&
		FunctorOfTerm(arg) == FunctorArrow;
	if (frst) {
	  if (optimizing_comit) {
	    Yap_emit(label_op, l, Zero);
	    l = ++labelno;
	  }
	  Yap_emit_3ops(push_or_op, l, Zero, Zero);
	  if (looking_at_comit &&
	      Yap_is_a_test_pred(ArgOfTerm(1, arg), mod)) {
	    /*
	     * let them think they are still the
	     * first 
	     */
	    Yap_emit(comit_opt_op, l, Zero);
	    optimizing_comit = TRUE;
	  }
	  else {
	    optimizing_comit = FALSE;
	    Yap_emit_3ops(either_op, l,  Zero, Zero);
	    Yap_emit(restore_tmps_op, Zero, Zero);
	    frst = FALSE;
	  }
	}
	else {
	  optimizing_comit = FALSE;
	  Yap_emit(label_op, l, Zero);
	  Yap_emit(pushpop_or_op, Zero, Zero);
	  Yap_emit_3ops(orelse_op, l = ++labelno, Zero, Zero);
	}
	/*
	 * if(IsApplTerm(arg) &&
	 * FunctorOfTerm(arg)==FunctorArrow) { 
	 */
	if (looking_at_comit) {
	  if (!optimizing_comit && !comitflag) {
	    /* This instruction is placed before
	     * the disjunction. This means that
	     * the program counter must point
	     * correctly, and also that the age
	     * of variable is older than the
	     * current branch.
	     */
	    int my_goalno = goalno;

	    goalno = savegoalno;
	    comitflag = labelno;
	    comitvar = MkVarTerm();
	    if (H == (CELL *)freep0) {
	      /* oops, too many new variables */
	      save_machine_regs();
	      longjmp(Yap_CompilerBotch,4);
	    }
	    savecpc = cpc;
	    savencpc = FirstP->nextInst;
	    cpc = FirstP;
	    onbranch = pop_branch();
	    c_var(comitvar, save_b_flag, 1, 0);
	    push_branch(onbranch,  comitvar);
	    onbranch = cur_branch;
	    cpc->nextInst = savencpc;
	    cpc = savecpc;
	    goalno = my_goalno;
	  }
	  save = onlast;
	  onlast = FALSE;
	  c_goal(ArgOfTerm(1, arg), mod);
	  if (!optimizing_comit) {
	    c_var((Term) comitvar, comit_b_flag,
		  1, 0);
	  }
	  onlast = save;
	  c_goal(ArgOfTerm(2, arg), mod);
	}
	else
	  c_goal(ArgOfTerm(1, Goal), mod);
	if (!onlast) {
	  Yap_emit(jump_op, m, Zero);
	}
	goalno = savegoalno + 1;
	Goal = ArgOfTerm(2, Goal);
	++cur_branch;
	onbranch = cur_branch;
      } while (IsNonVarTerm(Goal) && IsApplTerm(Goal)
	       && FunctorOfTerm(Goal) == FunctorOr);
      Yap_emit(pushpop_or_op, Zero, Zero);
      Yap_emit(label_op, l, Zero);
      if (!optimizing_comit)
	Yap_emit(orlast_op, Zero, Zero);
      else {
	optimizing_comit = FALSE;	/* not really necessary */
      }
      c_goal(Goal, mod);
      /*              --onbranch; */
      onbranch = pop_branch();
      if (!onlast) {
	Yap_emit(label_op, m, Zero);
	if ((onlast = save))
	  c_goal(MkAtomTerm(AtomTrue), mod);
      }
      Yap_emit(pop_or_op, Zero, Zero);
      return;
    }
    else if (f == FunctorComma) {
      int save = onlast;
      int t2 = ArgOfTerm(2, Goal);

      onlast = FALSE;
      c_goal(ArgOfTerm(1, Goal), mod);
      onlast = save;
      c_goal(t2, mod);
      return;
    }
    else if (f == FunctorNot || f == FunctorAltNot) {
      CELL label = (labelno += 2);
      CELL end_label = (labelno += 2);
      int save = onlast;
      Term comitvar;

      comitvar = MkVarTerm();
      if (H == (CELL *)freep0) {
	/* oops, too many new variables */
	save_machine_regs();
	longjmp(Yap_CompilerBotch,4);
      }
      push_branch(onbranch, comitvar);
      ++cur_branch;
      onbranch = cur_branch;
      or_found = 1;
      onlast = FALSE;
      c_var(comitvar, save_b_flag, 1, 0);
      Yap_emit_3ops(push_or_op, label, Zero, Zero);
      Yap_emit_3ops(either_op, label,  Zero, Zero);
      Yap_emit(restore_tmps_op, Zero, Zero);
      c_goal(ArgOfTerm(1, Goal), mod);
      c_var(comitvar, comit_b_flag, 1, 0);
      onlast = save;
      Yap_emit(fail_op, end_label, Zero);
      Yap_emit(pushpop_or_op, Zero, Zero);
      Yap_emit(label_op, label, Zero);
      Yap_emit(orlast_op, Zero, Zero);
      Yap_emit(label_op, end_label, Zero);
      onlast = save;
      /*              --onbranch; */
      onbranch = pop_branch();
      c_goal(MkAtomTerm(AtomTrue), mod);
      ++goalno;
      Yap_emit(pop_or_op, Zero, Zero);
      return;
    }
    else if (f == FunctorArrow) {
      Term comitvar;
      int save = onlast;

      comitvar = MkVarTerm();
      if (H == (CELL *)freep0) {
	/* oops, too many new variables */
	save_machine_regs();
	longjmp(Yap_CompilerBotch,4);
      }
      onlast = FALSE;
      c_var(comitvar, save_b_flag, 1, 0);
      c_goal(ArgOfTerm(1, Goal), mod);
      c_var(comitvar, comit_b_flag, 1, 0);
      onlast = save;
      c_goal(ArgOfTerm(2, Goal), mod);
      return;
    }
    else if (f == FunctorEq) {
      if (profiling)
	Yap_emit(enter_profiling_op, (CELL)p, Zero);
      else if (call_counting)
	Yap_emit(count_call_op, (CELL)p, Zero);
      c_eq(ArgOfTerm(1, Goal), ArgOfTerm(2, Goal));
      if (onlast) {
	Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	READ_LOCK(CurrentPred->PRWLock);
	if (is_tabled(CurrentPred))
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	else
#endif /* TABLING */
	  Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      }
      return;
    }
    else if (p->PredFlags & AsmPredFlag) {
      int op = p->PredFlags & 0x7f;
      if (profiling)
	Yap_emit(enter_profiling_op, (CELL)p, Zero);
      else if (call_counting)
	Yap_emit(count_call_op, (CELL)p, Zero);
      if (op >= _atom && op <= _primitive) {
	c_test(op, ArgOfTerm(1, Goal));
	if (onlast) {
	  Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	  READ_LOCK(CurrentPred->PRWLock);
	  if (is_tabled(CurrentPred))
	    Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	  else
#endif /* TABLING */
	    Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
	  READ_UNLOCK(CurrentPred->PRWLock);
#endif
	}
	return;
      }
      else if (op >= _plus && op <= _functor) {
	if (op == _functor) {
	  c_functor(Goal, mod);
	}
	else {
	  c_bifun(op,
		  ArgOfTerm(1, Goal),
		  ArgOfTerm(2, Goal),
		  ArgOfTerm(3, Goal),
		  mod);
	}
	if (onlast) {
	  Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	  READ_LOCK(CurrentPred->PRWLock);
	  if (is_tabled(CurrentPred))
	    Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	  else
#endif /* TABLING */
	    Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
	  READ_UNLOCK(CurrentPred->PRWLock);
#endif
	}
	return;
      }
      else {
	c_args(Goal, 0);
      }
    }
    else if (p->PredFlags & BinaryTestPredFlag) {
      Term a1 = ArgOfTerm(1,Goal);
      if (IsVarTerm(a1) && !IsNewVar(a1)) {
	Term a2 = ArgOfTerm(2,Goal);
	if (IsVarTerm(a2) && !IsNewVar(a2)) {
	  if (IsNewVar(a2))  {
	    Yap_Error_TYPE = INSTANTIATION_ERROR;
	    Yap_Error_Term = a2;
	    Yap_ErrorMessage = Yap_ErrorSay;
	    sprintf(Yap_ErrorMessage, "compiling %s/2 with second arg unbound",  RepAtom(NameOfFunctor(p->FunctorOfPred))->StrOfAE);
	    save_machine_regs();
	    longjmp(Yap_CompilerBotch,1);
	  }
	  c_var(a1, bt1_flag, 2, 0);
	  current_p0 = p0;
	  c_var(a2, bt2_flag, 2, 0);
	}
	else {
	  Term t2 = MkVarTerm();
	  if (H == (CELL *)freep0) {
	    /* oops, too many new variables */
	    save_machine_regs();
	    longjmp(Yap_CompilerBotch,4);
	  }

	  c_eq(t2, a2);
	  c_var(a1, bt1_flag, 2, 0);
	  current_p0 = p0;
	  c_var(t2, bt2_flag, 2, 0);
	}
      }
      else {
	Term a2 = ArgOfTerm(2,Goal);
	Term t1 = MkVarTerm();
	if (H == (CELL *)freep0) {
	  /* oops, too many new variables */
	  save_machine_regs();
	  longjmp(Yap_CompilerBotch,4);
	}

	c_eq(t1, a1);
	if (IsVarTerm(a2) && !IsNewVar(a2)) {
	  c_var(t1, bt1_flag, 2, 0);
	  current_p0 = p0;
	  c_var(a2, bt2_flag, 2, 0);
	}
	else {
	  Term t2 = MkVarTerm();
	  if (H == (CELL *)freep0) {
	    /* oops, too many new variables */
	    save_machine_regs();
	    longjmp(Yap_CompilerBotch,4);
	  }

	  c_eq(t2, a2);
	  c_var(t1, bt1_flag, 2, 0);
	  current_p0 = p0;
	  c_var(t2, bt2_flag, 2, 0);
	}
      }
      if (onlast) {
	Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	READ_LOCK(CurrentPred->PRWLock);
	if (is_tabled(CurrentPred))
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	else
#endif /* TABLING */
	  Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      }
      return;
    } else {
      if (profiling)
	Yap_emit(enter_profiling_op, (CELL)p, Zero);
      else if (call_counting)
	Yap_emit(count_call_op, (CELL)p, Zero);
      if (f == FunctorExecuteInMod) {
	/* compile the first argument only */
	c_arg(1, ArgOfTerm(1,Goal), 1, 0);
      } else {
	c_args(Goal, 0);
      }
    }
  }

  if (p->PredFlags & SafePredFlag
#ifdef YAPOR
      /* synchronisation means saving the state, so it is never safe in YAPOR */
      && !(p->PredFlags & SyncPredFlag)
#endif /* YAPOR */
      ) {
    if (onlast)
      Yap_emit(deallocate_op, Zero, Zero);
    Yap_emit(safe_call_op, (CELL) p0, Zero);
    if (onlast) {
#ifdef TABLING
      READ_LOCK(CurrentPred->PRWLock);
      if (is_tabled(CurrentPred))
	Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
      else
#endif /* TABLING */
	Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
      READ_UNLOCK(CurrentPred->PRWLock);
#endif
    }
  }
  else {
    if (p->PredFlags & (CPredFlag | AsmPredFlag)) {
#ifdef YAPOR
      if (p->PredFlags & SyncPredFlag)
	Yap_emit(sync_op, (CELL)p, (CELL)(p->ArityOfPE));
#endif /* YAPOR */
      if (p->FunctorOfPred == FunctorExecuteInMod) {
	Yap_emit_4ops(call_op, (CELL) p0, Zero, Zero, ArgOfTerm(2,Goal));
      } else {
	Yap_emit_3ops(call_op, (CELL) p0, Zero, Zero);
      }
      /* functor is allowed to call the garbage collector */
      if (onlast) {
	Yap_emit(deallocate_op, Zero, Zero);
	or_found = 1;
#ifdef TABLING
	READ_LOCK(CurrentPred->PRWLock);
	if (is_tabled(CurrentPred))
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	else
#endif /* TABLING */
	  Yap_emit(procceed_op, Zero, Zero);
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      }
    }
    else {
      if (onlast) {
	Yap_emit(deallocate_op, Zero, Zero);
#ifdef TABLING
	READ_LOCK(CurrentPred->PRWLock);
	if (is_tabled(CurrentPred)) {
	  Yap_emit_3ops(call_op, (CELL) p0, Zero, Zero);
	  Yap_emit(table_new_answer_op, Zero, CurrentPred->ArityOfPE);
	}
	else
#endif /* TABLING */
	  Yap_emit(execute_op, (CELL) p0, Zero);
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      }
      else {
	Yap_emit_3ops(call_op, (CELL) p0, Zero, Zero);
      }
    }
    if (!onlast)
      ++goalno;
  }
}

static void
c_body(Term Body, int mod)
{
  onhead = FALSE;
  BodyStart = cpc;
  goalno = 1;
  while (IsNonVarTerm(Body) && IsApplTerm(Body)
	 && FunctorOfTerm(Body) == FunctorComma) {
    Term t2 = ArgOfTerm(2, Body);
    if (IsTrueGoal(t2)) {
      /* optimise the case where some idiot left trues at the end
	 of the clause.
      */
      Body = ArgOfTerm(1, Body);
      break;
    }
    c_goal(ArgOfTerm(1, Body), mod);
    Body = t2;
  }
  onlast = TRUE;
  c_goal(Body, mod);
}

static void
c_head(Term t)
{
  Functor f;

  goalno = 0;
  onhead = TRUE;
  onlast = FALSE;
  cur_branch = onbranch = 0;
  branch_pointer = parent_branches;
  if (IsAtomTerm(t)) {
    Yap_emit(name_op, (CELL) AtomOfTerm(t), Zero);
    return;
  }
  f = FunctorOfTerm(t);
  Yap_emit(name_op, (CELL) NameOfFunctor(f), ArityOfFunctor(f));
  c_args(t, 0);
}

/* number of permanent variables in the clause */
static int nperm;

inline static int
usesvar(int ic)
{
  if (ic >= get_var_op && ic <= put_val_op)
    return (TRUE);
  switch (ic) {
  case save_b_op:
  case comit_b_op:
  case patch_b_op:
  case save_appl_op:
  case save_pair_op:
  case f_val_op:
  case f_var_op:
  case fetch_args_for_bccall:
  case bccall_op:
    return (TRUE);
  }
#ifdef SFUNC
  if (ic >= unify_s_var_op && ic <= write_s_val_op)
    return (TRUE);
#endif
  return ((ic >= unify_var_op && ic <= write_val_op)
	  ||
	  (ic >= unify_last_var_op && ic <= unify_last_val_op));
}

/*
 * Do as in the traditional WAM and make sure voids are in
 * environments
 */
#define LOCALISE_VOIDS 1

#ifdef LOCALISE_VOIDS
typedef  struct env_tmp {
  Ventry * Var;
  struct env_tmp *Next;
}  EnvTmp;
#endif

static void
AssignPerm(PInstr *pc)
{
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
      Ventry *v = (Ventry *) (pc->rnd1);
      if (v->AgeOfVE == v->FirstOfVE
	  && !(v->FlagsOfVE & (GlobalVal|OnHeadFlag|OnLastGoal|NonVoid)) ) {
	EnvTmp *x = (EnvTmp *)Yap_AllocCMem(sizeof(*x));
	x->Next = EnvTmps;
	x->Var = v;
	EnvTmps = x;
      }
    }
#endif
    if (pc->op == call_op || pc->op == either_op || pc->op == orelse_op || pc->op == push_or_op) {
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
      Ventry *v = (Ventry *) (pc->rnd1);

      if (v->NoOfVE == Unassigned) {
	if ((v->AgeOfVE > 1 && (v->AgeOfVE > v->FirstOfVE))
	    || v->KindOfVE == PermVar	/*
					 * * || (v->FlagsOfVE & NonVoid && !(v->FlagsOfVE &
					 * * OnHeadFlag)) 
					 */ ) {
	  v->NoOfVE = PermVar | (nperm++);
	  v->KindOfVE = PermVar;
	  v->FlagsOfVE |= PermFlag;
	}
	else
	  v->NoOfVE = v->KindOfVE = TempVar;
      }
    } else if (pc->op == empty_call_op) {
      pc->rnd2 = nperm;
    } else if (pc->op == call_op || pc->op == either_op || pc->op == orelse_op || pc->op == push_or_op) {
#ifdef LOCALISE_VOIDS
      EnvTmps = (EnvTmp *)(pc->ops.opseqt[1]);
      while (EnvTmps) {
	Ventry *v = EnvTmps->Var;
	v->NoOfVE = PermVar | (nperm++);
	v->KindOfVE = PermVar;
	v->FlagsOfVE |= (PermFlag|SafeVar);
	EnvTmps = EnvTmps->Next;
      }
#endif
      pc->rnd2 = nperm;
    }
    opc = pc;
    pc = npc;
  } while (pc != NULL);
}

static CELL *
init_bvarray(int nperm)
{
  CELL *vinfo = NULL;
  unsigned int i;
  CELL *vptr;

  vptr = vinfo = (CELL *)Yap_AllocCMem(sizeof(CELL)*(1+nperm/(8*sizeof(CELL))));
  for (i = 0; i <= nperm/(8*sizeof(CELL)); i++) {
    *vptr++ = (CELL)(0L);
  }
  return(vinfo);
}

static void
clear_bvarray(int var, CELL *bvarray)
{
  int max = 8*sizeof(CELL);
  CELL nbit;

  /* get to the array position */
  while (var >= max) {
    bvarray++;
    var -= max;
  }
  /* now put a 0 on it, from now on the variable is initialised */
  nbit = (1 << var);
#ifdef DEBUG
  if (*bvarray & nbit) {
    /* someone had already marked this variable: complain */
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    Yap_ErrorMessage = "repeated bit for variable";
    save_machine_regs();
    longjmp(Yap_CompilerBotch, 2);
  }
  pbvars++;
#endif
  *bvarray |= nbit;
}

/* copy the current state of the perm variable state array to code space */
static void
add_bvarray_op(PInstr *cp, CELL *bvarray, int env_size)
{
  int i, size = env_size/(8*sizeof(CELL));
  CELL *dest;

  dest = 
    Yap_emit_extra_size(mark_initialised_pvars_op, (CELL)env_size, (size+1)*sizeof(CELL));
  /* copy the cells to dest */
  for (i = 0; i <= size; i++)
    *dest++ = *bvarray++;
}

/* vsc: this code is not working, as it is too complex */

typedef struct {
  int lab;
  int last;
  PInstr *pc;
}  bventry;

#define MAX_DISJUNCTIONS 32
static bventry *bvstack;
static int bvindex = 0;

static void
push_bvmap(int label, PInstr *pcpc)
{
  if (bvindex == MAX_DISJUNCTIONS) {
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    Yap_ErrorMessage = "Too many embedded disjunctions";
    save_machine_regs();
    longjmp(Yap_CompilerBotch, 2);
  }
  /* the label instruction */
  bvstack[bvindex].lab = label;
  bvstack[bvindex].last = -1;
  /* where we have the code */
  bvstack[bvindex].pc = pcpc;
  bvindex++;
}

static void
reset_bvmap(CELL *bvarray, int nperm)
{
  int size, size1, env_size, i;
  CELL *source;
  if (bvindex == 0) {
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    Yap_ErrorMessage = "No embedding in disjunctions";
    save_machine_regs();
    longjmp(Yap_CompilerBotch, 2);
  }
  env_size = (bvstack[bvindex-1].pc)->rnd1;
  size = env_size/(8*sizeof(CELL));
  size1 = nperm/(8*sizeof(CELL));
  source = (bvstack[bvindex-1].pc)->arnds;
  for (i = 0; i <= size; i++)
    *bvarray++ = *source++;
  for (i = size+1; i<= size1; i++)
    *bvarray++ = (CELL)(0);
}

static void
pop_bvmap(CELL *bvarray, int nperm)
{
  if (bvindex == 0) {
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    Yap_ErrorMessage = "Too few embedded disjunctions";
    /*  save_machine_regs();
	longjmp(Yap_CompilerBotch, 2); */
  }
  reset_bvmap(bvarray, nperm);
  bvindex--;
}

typedef struct {
  PInstr *p;
  Ventry *v;
} UnsafeEntry;

/* extend to also support variable usage bitmaps for garbage collection */
static void
CheckUnsafe(PInstr *pc)
{
  int pending = 0;

  /* say that all variables are yet to initialise */
  CELL *vstat = init_bvarray(nperm);
  UnsafeEntry *UnsafeStack =
    (UnsafeEntry *) Yap_AllocCMem(nperm * sizeof(UnsafeEntry));
  /* keep a copy of previous cpc and CodeStart */
  PInstr *opc = cpc;
  PInstr *OldCodeStart = CodeStart;
  
  CodeStart = BlobsStart;
  cpc = icpc;
  bvindex = 0;
  bvstack = (bventry *)Yap_AllocCMem(MAX_DISJUNCTIONS * sizeof(bventry));
  while (pc != NIL) {
    switch(pc->op) {
    case put_val_op:
      {
	Ventry *v = (Ventry *) (pc->rnd1);

	if ((v->FlagsOfVE & PermFlag) && !(v->FlagsOfVE & SafeVar)) {
	  UnsafeStack[pending].p = pc;
	  UnsafeStack[pending++].v = v;
	  v->FlagsOfVE |= SafeVar;
	}
	break;
      }
    case put_var_op:
    case get_var_op:
    case save_b_op:
    case unify_var_op:
    case unify_last_var_op:
    case write_var_op:
    case save_appl_op:
    case save_pair_op:
    case f_var_op:
      {
	Ventry *v = (Ventry *) (pc->rnd1);

	if (v->FlagsOfVE & PermFlag && pc == v->FirstOpForV) {
	  /* the second condition covers cases such as save_b_op
	     in a disjunction */
	  clear_bvarray((v->NoOfVE & MaskVarAdrs), vstat);
	}
      }
      break;
    case push_or_op:
      Yap_emit(label_op, ++labelno, Zero);
      pc->ops.opseqt[1] = (CELL)labelno;
      add_bvarray_op(pc, vstat, pc->rnd2);
      push_bvmap((CELL)labelno, cpc);
      break;
    case either_op:
      /* add a first entry to the array */
      Yap_emit(label_op, ++labelno, Zero);
      pc->ops.opseqt[1] = (CELL)labelno;
      add_bvarray_op(pc, vstat, pc->rnd2);
      break;
    case pushpop_or_op:
      reset_bvmap(vstat, nperm);
      break;
    case orelse_op:
      Yap_emit(label_op, ++labelno, Zero);
      pc->ops.opseqt[1] = (CELL)labelno;
      add_bvarray_op(pc, vstat, pc->rnd2);
      break;
    case pop_or_op:
      pop_bvmap(vstat, nperm);
      break;
    case empty_call_op:
      /* just get ourselves a label describing how
	 many permanent variables are alive */
      Yap_emit(label_op, ++labelno, Zero);
      pc->rnd1 = (CELL)labelno;
      add_bvarray_op(pc, vstat, pc->rnd2);
      break;
    case call_op:
      Yap_emit(label_op, ++labelno, Zero);
      pc->ops.opseqt[1] = (CELL)labelno;
      add_bvarray_op(pc, vstat, pc->rnd2);
    case deallocate_op:
      {
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
  icpc = cpc;
  cpc = opc;
  BlobsStart = CodeStart;
  CodeStart = OldCodeStart;
}

static void
CheckVoids(void)
{				/* establish voids in the head and initial
				 * uses        */
  Ventry *ve;

  cpc = CodeStart;
  while ((ic = cpc->op) != allocate_op) {
    ic = cpc->op;
#ifdef M_WILLIAMS
    switch ((int) ic) {
#else
    switch (ic) {
#endif
    case get_var_op:
    case unify_var_op:
    case unify_last_var_op:
#ifdef SFUNC
    case unify_s_var_op:
#endif
    case save_pair_op:
    case save_appl_op:
      ve = ((Ventry *) cpc->rnd1);
      if ((ve->FlagsOfVE & PermFlag) == 0 && ve->RCountOfVE <= 1) {
	ve->NoOfVE = ve->KindOfVE = VoidVar;
	if (ic == get_var_op || ic ==
	    save_pair_op || ic == save_appl_op
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
    case get_longint_op:
    case get_bigint_op:
    case get_list_op:
    case get_struct_op:
      Uses[cpc->rnd2] = 1;
    }
    cpc = cpc->nextInst;
  }
}

static int
checktemp(void)
{
  Ventry *v = (Ventry *) arg;
  PInstr *q;
  Int Needed[MaxTemps];
  Int r, target1, target2;
  Int n, *np, *rp;
  CELL *cp;

  vadr = (v->NoOfVE);
  vreg = vadr & MaskVarAdrs;
  if (v->KindOfVE == PermVar || v->KindOfVE == VoidVar)
    return (0);
  if (v->RCountOfVE == 1)
     return(0);
  if (vreg) {
    --Uses[vreg];
    return (1);
  }
  /* follow the life of the variable                                       */
  q = cpc;
  /*
   * for(r=0; r<MaxCTemps; ++r) Needed[r] = Uses[r]; might be written
   * as: 
   */
  np = Needed;
  rp = Uses;
  for (r = 0; r < MaxCTemps; ++r)
    *np++ = *rp++;
  if (rn > 0 && (ic == get_var_op || ic == put_var_op)) {
    if (ic == put_var_op)
      Needed[rn] = 1;
    target1 = rn;		/* try to leave it where it is   */
  }
  else
    target1 = MaxCTemps;
  target2 = MaxCTemps;
  n = v->RCountOfVE - 1;
  while ((q = q->nextInst) != NIL) {
    if (q->rnd2 < 0);
    else if (usesvar(ic = q->op) && arg == q->rnd1) {
      --n;
      if (ic == put_val_op) {
	if (target1 == MaxCTemps && Needed[q->rnd2] == 0)
	  target1 = q->rnd2;
	else if (target1 != (r = q->rnd2)) {
	  if (target2 == MaxCTemps && Needed[r] == 0)
	    target2 = r;
	  else if (target2 > r && Uses[r] == 0 && Needed[r] == 0)
	    target2 = r;
	}
      }
    }
#ifdef SFUNC
    else if ((ic >= get_var_op && ic <= put_unsafe_op)
	     || ic == get_s_f_op || ic == put_s_f_op)
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
  if (target1 == MaxCTemps || Uses[target1] || Needed[target1])
    if ((target1 = target2) == MaxCTemps || Uses[target1] || Needed[target1]) {
      target1 = MaxCTemps;
      do
	--target1;
      while (target1 && Uses[target1] == 0 && Needed[target1] == 0);
      ++target1;
    }
  if (target1 == MaxCTemps) {
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    Yap_ErrorMessage = "too many temporaries";
    save_machine_regs();
    longjmp(Yap_CompilerBotch, 1);
  }
  v->NoOfVE = vadr = TempVar | target1;
  v->KindOfVE = TempVar;
  Uses[vreg = target1] = v->RCountOfVE - 1;
  /*
   * for(r=0; r<MaxCTemps; ++r) if(Contents[r]==vadr) Contents[r] =
   * NIL; 
   */
  cp = Contents;
  for (r = 0; r < MaxCTemps; ++r)
    if (*cp++ == (Term)vadr)
      cp[-1] = NIL;
  Contents[vreg] = vadr;
  ic = cpc->op;
  return (1);
}

static void
checkreg(int var_arg)
{
  PInstr *p = cpc;

  if (rn >= 0)
    return;
  vreg = 0;
  if (var_arg) {
    Ventry *v = (Ventry *) arg;

    vreg = (v->NoOfVE) & MaskVarAdrs;
    if (v->KindOfVE == PermVar)
      vreg = 0;
    else if (vreg == 0) {
      checktemp();
      ++Uses[vreg];
    }
  }
  if (vreg == 0) {
    vreg = MaxCTemps;
    do
      --vreg;
    while (vreg && Uses[vreg] == 0);
    ++vreg;
    ++Uses[vreg];
  }
  while (p) {
    if (p->op >= get_var_op && p->op <= put_unsafe_op && p->rnd2 == rn)
      p->rnd2 = vreg;
    /* only copy variables until you reach a call */
    if (p->op == procceed_op || p->op == call_op || p->op == push_or_op || p->op == pushpop_or_op)
      break;
    p = p->nextInst;
  }
  rn = vreg;
}

/* Create a bitmap with all live variables */
static CELL
copy_live_temps_bmap(int max)
{
  unsigned int size = (max|7)/8+1;
  int i;
  CELL *dest = Yap_emit_extra_size(mark_live_regs_op, max, size);
  CELL *ptr=dest;
  *ptr = 0L;
  for (i=1; i <= max; i++) {
    /* move to next cell */
    if (i%(8*CellSize) == 0) {
      ptr++;
      *ptr = 0L;
    }
    /* set the register live bit */
    if (Contents[i]) {
      int j = i%(8*CellSize);
      *ptr |= (1<<j);
    }
  }
  return((CELL)dest);  
}

static void
c_layout(void)
{
  PInstr *savepc = BodyStart->nextInst;
  register Ventry *v = vtable;
  Int *up = Uses, Arity;
  CELL *cop = Contents;

  cpc = BodyStart;
  while (v != NIL) {
    if (v->FlagsOfVE & BranchVar) {
      v->AgeOfVE = v->FirstOfVE + 1;	/* force permanent */
      ++(v->RCountOfVE);
      Yap_emit(put_var_op, (CELL) v, Zero);
      v->FlagsOfVE &= ~GlobalVal;
      v->FirstOpForV = cpc;
    }
    v = v->NextOfVE;
  }
  cpc->nextInst = savepc;

  nperm = 0;
  AssignPerm(CodeStart);
   /* vsc: need to do it from the beginning to find which perm vars are active */
  /* CheckUnsafe(BodyStart); */
#ifdef DEBUG
  pbvars = 0;
#endif
  CheckUnsafe(CodeStart);
#ifdef DEBUG
  if (pbvars != nperm) {
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    Yap_ErrorMessage = "wrong number of variables found in bitmap";
    save_machine_regs();
    longjmp(Yap_CompilerBotch, 2);
  }    
#endif
  MaxCTemps = nvars + max_args - tmpreg + n_common_exps + 2;
  if (MaxCTemps >= MaxTemps)
    MaxCTemps = MaxTemps;
  for (rn = 0; rn < MaxCTemps; ++rn) {
    /* Uses[rn] = 0; Contents[rn] = NIL; */
    *up++ = 0;
    *cop++ = NIL;
  }
  CheckVoids();
  /* second scan: allocate registers                                       */
  cpc = CodeStart;
  while (cpc) {
    ic = cpc->op;
    arg = cpc->rnd1;
    rn = cpc->rnd2;
#ifdef M_WILLIAMS
    switch ((int) ic) {
#else
    switch (ic) {
#endif
    case allocate_op:
    case deallocate_op:
#ifdef TABLING
      READ_LOCK(CurrentPred->PRWLock);
      if (is_tabled(CurrentPred))
	cpc->op = nop_op;
      else
#endif /* TABLING */
	if (goalno == 1 && or_found == 0 && nperm == 0)
	  cpc->op = nop_op;
#ifdef TABLING
	READ_UNLOCK(CurrentPred->PRWLock);
#endif
      break;
    case pop_op:
      ic = (cpc->nextInst)->op;
      if (ic >= get_var_op && ic <= put_unsafe_op)
	cpc->op = nop_op;
      break;
    case get_var_op:
      --Uses[rn];
      if (checktemp()) {
	if (vreg == rn)
	  cpc->op = nop_op;
      }
      Contents[rn] = vadr;
      break;
    case get_val_op:
      --Uses[rn];
      checktemp();
      Contents[rn] = vadr;
      break;
    case f_var_op:
    case unify_var_op:
    case unify_val_op:
    case unify_last_var_op:
    case unify_last_val_op:
#ifdef SFUNC
    case unify_s_var_op:
    case unify_s_val_op:
#endif
    case fetch_args_for_bccall:
    case bccall_op:
      checktemp();
      break;
    case get_atom_op:
    case get_num_op:
    case get_float_op:
    case get_longint_op:
    case get_bigint_op:
      --Uses[rn];
      Contents[rn] = arg;
      break;
    case get_list_op:
    case get_struct_op:
      Contents[rn] = NIL;
      --Uses[rn];
      break;
    case put_var_op:
    case put_unsafe_op:
      checkreg(TRUE);
      checktemp();
      Contents[rn] = vadr;
      ++Uses[rn];
      break;
    case put_val_op:
      checkreg(TRUE);
      checktemp();
      if (Contents[rn] == (Term)vadr)
	cpc->op = nop_op;
      Contents[rn] = vadr;
      ++Uses[rn];
      break;
#ifdef SFUNC
    case write_s_var_op:
      {
	Ventry *ve = (Ventry *) arg;

	if ((ve->FlagsOfVE & PermFlag) == 0 && ve->RCountOfVE <= 1)
	  cpc->op = nop_op;
      }
      break;
    case write_s_val_op:
#endif
    case write_var_op:
    case write_val_op:
    case f_val_op:
      checktemp();
      break;
#ifdef SFUNC
    case put_s_f_op:
      Contents[rn] = arg;
      ++Uses[rn];
      break;
#endif
    case put_atom_op:
    case put_num_op:
    case put_float_op:
    case put_longint_op:
    case put_bigint_op:
      checkreg(FALSE);
      if (Contents[rn] == arg)
	cpc->op = nop_op;
      Contents[rn] = arg;
      ++Uses[rn];
      break;
    case put_list_op:
    case put_struct_op:
      checkreg(FALSE);
      Contents[rn] = NIL;
      ++Uses[rn];
      break;
    case save_b_op:
    case comit_b_op:
    case patch_b_op: 
    case save_appl_op:
    case save_pair_op:
      checktemp();
      break;
    case safe_call_op:
      Arity = RepPredProp((Prop) arg)->ArityOfPE;
      for (rn = 1; rn <= Arity; ++rn)
	--Uses[rn];
      break;
    case call_op:
    case label_op:
      /*
       * for(rn=1; rn<MaxCTemps; ++rn) Uses[rn] =
       * Contents[rn] = NIL; 
       */
      up = Uses;
      cop = Contents;
      for (rn = 1; rn < MaxCTemps; ++rn)
	*up++ = *cop++ = NIL;
      break;
    case restore_tmps_and_skip_op:
    case restore_tmps_op:
      /*
	This instruction is required by the garbage collector to find out
	how many temporaries are live right now. It is also useful when
	waking up goals before an either or ! instruction.
      */
     {
	PInstr *mycpc = cpc, *oldCodeStart = CodeStart;
	int i, max;

	/* instructions must be placed at BlobsStart */
	CodeStart = BlobsStart;
	cpc = icpc;
	max = 0;
	for (i = 1; i < MaxCTemps; ++i) {
	  if (Contents[i]) max = i;
	}
	Yap_emit(label_op, ++labelno, Zero);
	mycpc->rnd1 = labelno;
	rn = copy_live_temps_bmap(max);
	icpc = cpc;
	BlobsStart = CodeStart;
	cpc = mycpc;
	CodeStart = oldCodeStart;
      }
      break;
    }
    if (cpc->nextInst)
      cpc = cpc->nextInst;
    else return;
  }
}

static void
c_optimize(PInstr *pc)
{
  char onTail;
  Ventry *v;
  PInstr *opc = NULL;

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
    case save_pair_op:
	{
	  Term ve = (Term) pc->rnd1;
	  PInstr *npc = pc->nextInst;

	  if (((Ventry *) ve)->RCountOfVE <= 1)
	    pc->op = nop_op;
	  else {
	    *pc = *npc;
	    pc->nextInst = npc;
	    npc->op = save_pair_op;
	    npc->rnd1 = (CELL) ve;
	  }
	}
	break;
    case save_appl_op:
      {
	Term ve = (Term) pc->rnd1;
	PInstr *npc = pc->nextInst;

	if (((Ventry *) ve)->RCountOfVE <= 1)
	  pc->op = nop_op;
	else {
	  *pc = *npc;
	  pc->nextInst = npc;
	  npc->op = save_appl_op;
	  npc->rnd1 = (CELL) ve;
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
      v = (Ventry *) (pc->rnd1);
      if (v->KindOfVE == VoidVar && onTail) {
	pc->op = nop_op;
      }
      else
#endif	/* OLD_SYSTEM */
	onTail = 0;
      break;
    case unify_val_op:
      v = (Ventry *) (pc->rnd1);
      if (!(v->FlagsOfVE & GlobalVal))
	pc->op = unify_local_op;
      onTail = 0;
      break;
    case unify_last_val_op:
      v = (Ventry *) (pc->rnd1);
      if (!(v->FlagsOfVE & GlobalVal))
	pc->op = unify_last_local_op;
      onTail = 0;
      break;
    case write_val_op:
      v = (Ventry *) (pc->rnd1);
      if (!(v->FlagsOfVE & GlobalVal))
	pc->op = write_local_op;
      onTail = 0;
      break;
    case pop_op:
      if (FALSE && onTail == 1) {
	pc->op = nop_op;
	onTail = 1;
	break;
      }
      else {
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
    case unify_bigint_op:
    case unify_last_longint_op:
    case unify_last_bigint_op:
    case write_longint_op:
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
}

yamop *
Yap_cclause(Term inp_clause, int NOfArgs, int mod)
{				/* compile a prolog clause, copy of clause myst be in ARG1 */
  /* returns address of code for clause */
  Term head, body;
  yamop *acode;

  volatile int maxvnum = 512;
  int botch_why;
  volatile Term my_clause = inp_clause;
  /* may botch while doing a different module */

  /* first, initialise Yap_CompilerBotch to handle all cases of interruptions */
  Yap_ErrorMessage = NULL;
  Yap_Error_Size = 0;
  if ((botch_why = setjmp(Yap_CompilerBotch)) == 3) {
    /* out of local stack, just duplicate the stack */
    restore_machine_regs();
    reset_vars();
    {
      Int osize = 2*sizeof(CELL)*(ASP-H);
      ARG1 = my_clause;
      if (!Yap_gcl(Yap_Error_Size, 2, ENV, P)) {
	Yap_Error_TYPE = OUT_OF_STACK_ERROR;
	Yap_Error_Term = my_clause;
      }
      if (osize > ASP-H) {
	if (!Yap_growstack(2*sizeof(CELL)*(ASP-H))) {
	  Yap_Error_TYPE = SYSTEM_ERROR;
	  Yap_Error_Term = my_clause;
	}
      }
      my_clause = ARG1;
    }
  } else if (botch_why == 4) {
    /* out of temporary cells */
    restore_machine_regs();
    reset_vars();
    if (maxvnum < 16*1024) {
      maxvnum *= 2;
    } else {
      maxvnum += 4096;
    }
  } else if (botch_why == 2) {
    /* not enough heap */
    restore_machine_regs();
    reset_vars();
    Yap_Error_TYPE = SYSTEM_ERROR;
    Yap_Error_Term = TermNil;
    return(0);
  }
 restart_compilation:
  if (Yap_ErrorMessage != NULL) {
    reset_vars();
    return (0);
  }
  HB = H;
  or_found = 0;
  Yap_ErrorMessage = NULL;
  /* initialize variables for code generation                              */
  CodeStart = cpc = NULL;
  BlobsStart = icpc = NULL;
  freep = freep0 = (char *) (H + maxvnum);
  if (ASP <= CellPtr (freep) + 256) {
    vtable = NULL;
    Yap_Error_Size = (256+maxvnum)*sizeof(CELL);
    save_machine_regs();
    longjmp(Yap_CompilerBotch,3);
  }
  common_exps = NULL;
  n_common_exps = 0;
  cur_branch = onbranch = 0;
  branch_pointer = parent_branches;
  tmpreg = 0;
  nvars = 0;
  max_args = 0;
  /*
   * 2000 added to H in case we need to construct call(G) when G is a
   * variable used as a goal                                       
   */
  vtable = NULL;
  labelno = 0L;

  if (IsVarTerm(my_clause)) {
    Yap_Error_TYPE = INSTANTIATION_ERROR;
    Yap_Error_Term = my_clause;
    Yap_ErrorMessage = "in compiling clause";
    return (0);
  }
  if (IsApplTerm(my_clause) && FunctorOfTerm(my_clause) == FunctorAssert) {
    head = ArgOfTerm(1, my_clause);
    body = ArgOfTerm(2, my_clause);
  }
  else {
    head = my_clause, body = MkAtomTerm(AtomTrue);
  }
  if (IsVarTerm(head) || IsPairTerm(head) || IsIntTerm(head) || IsFloatTerm(head) || IsRefTerm(head)) {
    Yap_Error_TYPE = TYPE_ERROR_CALLABLE;
    Yap_Error_Term = my_clause;
    Yap_ErrorMessage = "clause should be atom or term";
    return (0);
  } else {
    
    /* find out which predicate we are compiling for */
    if (IsAtomTerm(head)) {
      Atom ap = AtomOfTerm(head);
      CurrentPred = RepPredProp(PredPropByAtom(ap, mod));
    } else {
      CurrentPred = RepPredProp(PredPropByFunc(FunctorOfTerm(head),mod));
    }
    /* insert extra instructions to count calls */
    READ_LOCK(CurrentPred->PRWLock);
    if ((CurrentPred->PredFlags & ProfiledPredFlag) ||
	(PROFILING && (CurrentPred->cs.p_code.FirstClause == NIL))) {
      profiling = TRUE;
      call_counting = FALSE;
    } else if ((CurrentPred->PredFlags & CountPredFlag) ||
	       (CALL_COUNTING && (CurrentPred->cs.p_code.FirstClause == NIL))) {
      call_counting = TRUE;
      profiling = FALSE;
    } else {
      profiling = FALSE;
      call_counting = FALSE;
    }
    READ_UNLOCK(CurrentPred->PRWLock);
  }
  /* phase 1 : produce skeleton code and variable information              */
  c_head(head);
  Yap_emit(allocate_op, Zero, Zero);
  c_body(body, mod);
  /* Insert blobs at the very end */ 
  if (BlobsStart != NULL) {
    cpc->nextInst = BlobsStart;
    BlobsStart = NULL;
  }
  reset_vars();
  H = HB;
  if (B != NULL) {
    HB = B->cp_h;
  }
  if (Yap_ErrorMessage)
    return (0);
#ifdef DEBUG
  if (Yap_Option['g' - 96])
    Yap_ShowCode();
#endif
  /* phase 2: classify variables and optimize temporaries          */
  c_layout();
  /* Insert blobs at the very end */ 
  if (BlobsStart != NULL) {
    cpc->nextInst = BlobsStart;
    BlobsStart = NULL;
    while (cpc->nextInst != NULL)
      cpc = cpc->nextInst;
  }
  /* eliminate superfluous pop's and unify_var's                   */
  c_optimize(CodeStart);
#ifdef DEBUG
  if (Yap_Option['f' - 96])
    Yap_ShowCode();
#endif
  /* phase 3: assemble code                                                */
  acode = Yap_assemble(ASSEMBLING_CLAUSE);

#ifdef LOW_PROF
 {
  static int pred_count=0;
  FILE *f,*init;
  extern int PROFSIZE;

  if (!pred_count++) { 
      f=fopen("PROFPREDS","w"); 
      init=fopen("PROFINIT","r");
      if (init!=NULL) {
        size_t nc; char buffer[4100];
	do {
          nc=fread(buffer,1,4096,init);
          fwrite(buffer,1,nc,f);
        } while(nc>0);
      fclose(init);
      }
  } else {
      f=fopen("PROFPREDS","a");
  }
  if (f!=NULL) {  
      fprintf(f,"%x - %x - Pred(%ld) - %s/%d\n",acode,PROFSIZE, CodeStart->rnd1, RepAtom(AtomOfTerm(MkAtomTerm((Atom) CodeStart->rnd1)))->StrOfAE, CodeStart->rnd2);
      fclose(f);
  }
 }
#endif


  /* check first if there was space for us */
  if (acode == NIL) {
    /* make sure we have enough space */
    reset_vars();
    if (!Yap_growheap(FALSE, Yap_Error_Size)) {
      save_machine_regs();
      my_clause = Deref(ARG1);
      longjmp(Yap_CompilerBotch, 2);
      return(NULL);
    } else {
      my_clause = Deref(ARG1);
      goto restart_compilation;
    }
  } else
    return(acode);
}

