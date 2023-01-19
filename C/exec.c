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
 * File:		exec.c *
 * Last rev:	8/2/88							 *
 * mods: *
 * comments:	Execute Prolog code					 *
 *									 *
 *************************************************************************/



#ifdef SCCS
static char SccsId[] = "@(#)cdmgr.c	1.1 05/02/98";
#endif

#include "absmi.h"

#include "amidefs.h"

#include "attvar.h"
#include "cut_c.h"
#include "yapio.h"
#include "heapgc.h"

static bool CallPredicate(PredEntry *, choiceptr, yamop *CACHE_TYPE);

// must hold thread worker comm lock at call.
static bool EnterCreepMode(Term, Term CACHE_TYPE);


static Int execute(USES_REGS1);

static Int execute0(USES_REGS1);

static Term cp_as_integer(choiceptr cp USES_REGS)
{
  return (MkIntegerTerm(LCL0 - (CELL *)cp));
}

static choiceptr cp_from_integer(Term cpt USES_REGS)
{
  return (choiceptr)(LCL0 - IntegerOfTerm(cpt));
}

/**
 * Represents a choice-point as an offset to the top of local stack. This should
 * *be stable acroos gc or stack shifts.
 * @param  cp                pointer to choice-point
 * @return                   Term with offset
 */
Term Yap_cp_as_integer(choiceptr cp)
{
  CACHE_REGS
  return cp_as_integer(cp PASS_REGS);
}

PredEntry *Yap_track_cpred(op_numbers op, yamop *ip, size_t min, void *v)
{
  CACHE_REGS
    gc_entry_info_t *i = v;
  if (ip == NULL)
    ip = P;
  if (ip==YESCODE || ip== NOCODE || ip == FAILCODE || ip == TRUSTFAILCODE) {
    op = P->opc;
    i->env = ENV; // YENV should be tracking ENV
    i->p = ip;
    i->p_env = ip;
    i->a = 0;
    i->env_size = EnvSizeInCells;
    i->caller = NULL;
    return i->pe = ip == YESCODE ? PredTrue : PredFail;
  } else if (op == _op_fail) {
    i->env = ENV; // YENV should be tracking ENV
    i->p = ip;
    i->p_env = ip;
    i->a = 0;
    i->env_size = EnvSizeInCells;
    i->caller = NULL;
    return i->pe = PredFail;
  }
  i->at_yaam = true;
  CalculateStackGap(PASS_REGS1);
  i->gc_min = 2 * MinStackGap;
  yamop *ip0 = PREVOP(ip, Osbpp);
  if (!op)
    {
      op_numbers op1 = Yap_op_from_opcode(ip0->opc);
      i->at_yaam = false;
      if (op1 == _call_cpred || op1 == _call_usercpred)
	op = op1;
      else
	{
	  op = Yap_op_from_opcode(ip->opc);
	  ip0 = ip;
	}
    }
  else if (ip->opc == Yap_opcode(op))
    {
      ip0 = ip;
    }
  switch (op)
    {
    case _call:
      i->env = YENV; // YENV should be tracking ENV
      i->p = ip;
      i->p_env = NEXTOP(ip, Osbpp);
      i->a = i->p->y_u.Osbpp.p->ArityOfPE;
      i->env_size = -i->p->y_u.Osbpp.s / sizeof(CELL);
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe = i->p->y_u.Osbpp.p;
    case _call_cpred:
    case _call_usercpred:
      i->env = YENV; // YENV should be tracking ENV
      i->p_env = NEXTOP(ip0, Osbpp);
      i->a = ip0->y_u.Osbpp.p->ArityOfPE;
      i->p = ip0;
      i->env_size = -ip0->y_u.Osbpp.s / sizeof(CELL);
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe =  ip0->y_u.Osbpp.p;
    case _execute_cpred:
    case _execute:
      i->a = ip0->y_u.Osbpp.p->ArityOfPE;
      i->p_env = CP;
      i->env = ENV;
      i->p = P;
      i->env_size = -PREVOP(CP,Osbpp)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe =  ip->y_u.Osbpp.p;

    case _dexecute:
      i->a = P->y_u.Osbpp.p->ArityOfPE;
      i->p_env = (yamop*)ENV[E_CP];
      i->env = ENV;
      i->p = P;
      i->env_size = EnvSizeInCells;
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe =  ip->y_u.Osbpp.p;
    case _try_c:
    case _retry_c:
    case _try_userc:
    case _retry_userc:
      i->p = P;
      i->a = i->p->y_u.OtapFs.s + i->p->y_u.OtapFs.extra;
      i->p_env = CP;
      i->env = ENV;
      i->env_size = EnvSizeInCells;
      i->caller = PREVOP(CP,OtapFs)->y_u.OtapFs.p;
      return i->pe =  i->caller;
    case _copy_idb_term:
      i->env = ENV; // YENV should be tracking ENV
      i->p = P;
      i->p_env = CP;
      i->a = 3;
      i->env_size =  -PREVOP(CP,Osbpp)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = NULL;
      return i->pe =  NULL;
    case _ensure_space:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a =  P->y_u.Osbpa.p->ArityOfPE;
      i->op = _ensure_space;
      i->env_size = EnvSizeInCells;
      i->caller =   P->y_u.Osbpa.p;
      return i->pe =  NULL;
    case _p_func2s_vv:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = _p_func2s_vv;
      i->env_size = -NEXTOP(P, xxx)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = NEXTOP(P, xxx)->y_u.Osbpp.p0;
      return i->pe = NEXTOP(P, xxx)->y_u.Osbpp.p;
    case _p_func2s_cv:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = _p_func2s_vc;
      i->env_size = -NEXTOP(P, xxc)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = NULL;
      return i->pe =  NULL;
    case _p_func2s_vc:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = _p_func2s_cv;
      i->env_size = -NEXTOP(P, xxn)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = NEXTOP(P, xxn)->y_u.Osbpp.p0;
      return i->pe = NEXTOP(P, xxn)->y_u.Osbpp.p;
    case _p_func2s_y_vv:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = _p_func2s_y_vv;
      i->env_size = -NEXTOP(P, yxx)->y_u.Osbpp.s / sizeof(CELL);
      return i->caller =  NEXTOP(P, yxx)->y_u.Osbpp.p0;
      return i->pe =  NEXTOP(P, yxx)->y_u.Osbpp.p;
    case _p_func2s_y_vc:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = _p_func2s_y_vc;
      i->env_size = -NEXTOP(P, yxc)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = NEXTOP(P, yxc)->y_u.Osbpp.p0;
      return i->pe =  NEXTOP(P, yxc)->y_u.Osbpp.p;
    case _p_func2s_y_cv:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = _p_func2s_y_cv;
      i->env_size = -NEXTOP(P, yxn)->y_u.Osbpp.s / sizeof(CELL);
      i->caller =NEXTOP(P, yxn)->y_u.Osbpp.p0;
	return i->pe =  NEXTOP(P, yxn)->y_u.Osbpp.p;
    case _p_functor:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 3;
      i->op = _p_functor;
      i->env_size = -NEXTOP(P, yxx)->y_u.Osbpp.s / sizeof(CELL);
      i->caller = NEXTOP(P, yxx)->y_u.Osbpp.p0;
      return i->pe = NEXTOP(P, yxx)->y_u.Osbpp.p;
    case _cut_t:
    case _cut:
      i->env = ENV;
      i->p = NEXTOP(P,s);
      i->p_env = CP;
      i->a = 0;
      i->op = op;
      i->env_size = -i->p->y_u.Osbpp.s / sizeof(CELL);
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe =  i->p->y_u.Osbpp.p;
    case _commit_b_x:
      i->env = ENV;
      i->p = NEXTOP(P,xps);
      i->p_env = CP;
      i->a = 0;
      i->op = op;
      i->env_size = -i->p->y_u.Osbpp.s / sizeof(CELL);
      i->caller =i->p->y_u.Osbpp.p0;
      return i->pe = i->p->y_u.Osbpp.p;
    case _commit_b_y:
      i->env = ENV;
      i->p = NEXTOP(P,yps);
      i->p_env = CP;
      i->a = 0;
      i->op = op;
      i->env_size = -i->p->y_u.Osbpp.s / sizeof(CELL);
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe =  i->p->y_u.Osbpp.p;
    case _cut_e:
      i->env = ENV;
      i->p = NEXTOP(P,s);
      i->p_env = CP;
      i->a = 0;
      i->op = op;
      i->env_size = EnvSizeInCells;
      i->caller = i->p->y_u.Osbpp.p0;
      return i->pe =  i->p->y_u.Osbpp.p;
    default:
      i->env = ENV;
      i->p = P;
      i->p_env = CP;
      i->a = 0;
      i->op = 0;
      i->env_size = EnvSizeInCells;
      i->caller = NULL;
      return i->pe =  NULL;
    }
}

static bool is_connective(Functor f)
{
  return f == FunctorComma ||
    f == FunctorOr ||
    f == FunctorArrow ||
    f == FunctorSoftCut ;
}


/**
 * Sets up the engine to run a different predicate.
 * @param  pen           the new code
 * @param  cut_pt        cut boundary
 * @param  USES_REGS     thread support
 * @return               success
 */
static inline bool CallPredicate(PredEntry *pen, choiceptr cut_pt,
                                 yamop *code USES_REGS)
{
#ifdef LOW_LEVEL_TRACER
  if (Yap_do_low_level_trace)
    low_level_trace(enter_pred, pen, XREGS + 1);
#endif /* LOW_LEVEL_TRACE */
#ifdef DEPTH_LIMIT
  if (DEPTH <= MkIntTerm(1))
  { /* I assume Module==0 is prolog */
    if (pen->ModuleOfPred)
    {
      if (DEPTH == MkIntTerm(0))
      {
        UNLOCK(pen->PELock);
        return false;
      }
      else
        DEPTH = RESET_DEPTH();
    }
  }
  else if (pen->ModuleOfPred)
    DEPTH -= MkIntConstant(2);
#endif /* DEPTH_LIMIT */
  if (P->opc != EXECUTE_CPRED_OPCODE)
  {
    //	YENV[E_CP] = CP;
    //      YENV[E_E] = ENV;
    //#ifdef DEPTH_LIMIT
    //	YENV[E_DEPTH] = DEPTH;
    //#endif
    //        ENV = YENV;
    ENV = YENV;
    YENV = ASP;
    CP = P;
  }
  /* make sure we have access to the user given cut */
  YENV[E_CB] = (CELL)cut_pt;
  P = code;
  return true;
}

/**
 * calls a meta-predicate or anything weird
 * @param  t            the called goal
 * @param  USES_REGS    MT
 * @return              did we fiid it?
 */
inline static bool CallMetaCall(Term t, Term mod USES_REGS)
{
  // we have a creep request waiting

  ARG1 = t;
  ARG2 = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
  ARG3 = t;
  if (mod)
  {
    ARG4 = mod;
  }
  else
  {
    ARG4 = TermProlog;
  }
  if (Yap_GetGlobal(AtomDebugMeta) == TermOn)
  {
    return CallPredicate(PredTraceMetaCall, B,
                         PredTraceMetaCall->CodeOfPred PASS_REGS);
  }
  else
  {
    return CallPredicate(PredMetaCall, B, PredMetaCall->CodeOfPred PASS_REGS);
  }
}

/**
 * Transfer control to a meta-call in ARG1, cut up to B.
 * @param  mod                     current module
 * @return                         su
 */
Term Yap_ExecuteCallMetaCall(Term g, Term mod)
{
  CACHE_REGS
  Term ts[4];
  ts[0] = g;
  ts[1] = cp_as_integer(B PASS_REGS); /* p_current_choice_point */
  ts[2] = g;
  ts[3] = mod;
  if (Yap_GetGlobal(AtomDebugMeta) == TermOn)
  {
    return Yap_MkApplTerm(PredTraceMetaCall->FunctorOfPred, 3, ts);
  }
  return Yap_MkApplTerm(PredMetaCall->FunctorOfPred, 4, ts);
}

Term Yap_PredicateIndicator(Term t, Term mod)
{
  CACHE_REGS
  // generate predicate indicator in this case
  Term ti[2];
  t = Yap_YapStripModule(t, &mod);
  if (IsApplTerm(t) && !IsExtensionFunctor(FunctorOfTerm(t)))
  {
    ti[0] = MkAtomTerm(NameOfFunctor(FunctorOfTerm(t)));
    ti[1] = MkIntegerTerm(ArityOfFunctor(FunctorOfTerm(t)));
  }
  else if (IsPairTerm(t))
  {
    ti[0] = MkAtomTerm(AtomDot);
    ti[1] = MkIntTerm(2);
  }
  else
  {
    ti[0] = t;
    ti[1] = MkIntTerm(0);
  }
  t = Yap_MkApplTerm(FunctorSlash, 2, ti);
  if (mod != CurrentModule)
  {
    ti[0] = mod;
    ti[1] = t;
    return Yap_MkApplTerm(FunctorModule, 2, ti);
  }
  return t;
}

static bool CallError(yap_error_number err, Term t, Term mod USES_REGS)
{
    if (err == TYPE_ERROR_CALLABLE)
    {
      t = Yap_YapStripModule(t, &mod);
    }
    Yap_ThrowError(err, t, "call/1");
    return false;
}



static Int save_env_b(USES_REGS1)
{
  Term t = Deref(ARG1);
  Term td;
#if SHADOW_HB
  register CELL *HBREG = HB;
#endif
  if (!IsVarTerm(t))
    return (FALSE);
  td = cp_as_integer((choiceptr)YENV[E_CB] PASS_REGS);
  YapBind((CELL *)t, td);
  return true;
}


bool comma_goal(Term t1, Term t0[4], bool first) {
  CACHE_REGS
  Term ts[2], m1 = t0[3];
    if (IsVarTerm(t1)) {
      if (first) {
	CallError(INSTANTIATION_ERROR, t0[0], t0[3] PASS_REGS);
    } else {
      ts[0] = m1;
        ts[1] = t1;
        t1 = Yap_MkApplTerm(FunctorModule,2,ts);
	t0[1] = Yap_MkApplTerm(FunctorCall,1,&t1);
	return false;
    }

    } else if (IsAtomTerm(t1)) {
	if (t1 == TermCut) {
	  if (first) t1 = TermTrue;
	}
        t0[1] = t1;
	return false;
      }
    else if (IsPairTerm(t1)) {
     Term ts[2];
    ts[0] = t1;
    ts[1] = (CurrentModule == 0 ? TermProlog : CurrentModule);
    t0[1]  = Yap_MkApplTerm(FunctorCsult, 2, ts);
    return false;
    }    else if (IsApplTerm(t1)) {
      Functor f = FunctorOfTerm(t1);
      if (f==FunctorComma) {
	Term l = Yap_YapStripModule(ArgOfTerm(1,t1),t0+3);
	comma_goal(l, t0, first);
	t0[1] = l;
	t0[2] = ArgOfTerm(2,t1);
	return true;
      } else if (IsExtensionFunctor(f)) {
	return CallError(TYPE_ERROR_CALLABLE, t0[0], t0[3] PASS_REGS);
      }
    }
    t0[1] = t1;
    return false;
}

/**
 * remove choice points created since a call to top-goal.
 *
 */
static void prune_inner_computation(choiceptr parent)
{
  CACHE_REGS
  /* code */
  choiceptr cut_pt;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;

  cut_pt = B;
  while (cut_pt->cp_b && cut_pt->cp_b < parent)
  {
    cut_pt = cut_pt->cp_b;
  }
#ifdef YAPOR
  CUT_prune_to(cut_pt);
#endif
  B = cut_pt;
  Yap_TrimTrail();
  LOCAL_AllowRestart = FALSE;
  P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  B = parent;
}

/**
 * restore abstract machine state
 * after completing a computation.
 */
static void complete_inner_computation(choiceptr old_B)
{
  CACHE_REGS
  choiceptr myB = B;
  if (myB == NULL)
  {
    return;
  }
  else if (myB->cp_b == old_B)
  {
    B = old_B;
#ifdef DEPTH_LIMIT
    DEPTH = myB->cp_depth;
#endif
  }
  else if (myB->cp_b && myB->cp_b < old_B)
  {
    while (myB->cp_b < old_B)
    {
      // we're recovering from a non-deterministic computation...
      myB = myB->cp_b;
    }
  }
  else
  {
    return;
  }
  // restore environment at call...
  CP = myB->cp_cp;
  ENV = myB->cp_env;
}

struct pred_entry ** CommaPredicates;

PredEntry *Yap_MkConjunction(Term t, Int MyB)
{
  Term t0 = t;
  Term mod = CurrentModule;
     arity_t i=1;
     bool loop = true;
     do {
       Term t1;
       if (!loop)
	 t1 = t;
       else
	 t1 = ArgOfTerm(1,t);
        if (IsVarTerm (t1)) {
	            Yap_ThrowError(INSTANTIATION_ERROR, t0,  NULL);
        }
       if (IsNumTerm(t1)) {
          Yap_ThrowError(TYPE_ERROR_CALLABLE, t0,  NULL);
       }

       if (t1 == TermCut) {
	 Term tcut = MkIntTerm(MyB);
          XREGS[i++] = Yap_MkApplTerm(FunctorCutBy, 1, &tcut);
       } else if (IsApplTerm(t1) && is_connective(FunctorOfTerm(t1))) {
	 Term ts[4];
	 ts[0]=t1;
	 ts[1]=MkIntTerm(MyB);
	 ts[2]=t0;
	 ts[3]=mod;
	 XREGS[i++] = Yap_MkApplTerm(FunctorMetaCall,4,ts);
	 
       }else {
	 PredEntry*pi=Yap_get_pred(t1,mod,"call");
	 Term ts[3];
	 ts[0]=t1;
	 ts[1]=MkAddressTerm(pi);
	 ts[2] = TermNil;
	 XREGS[i++] = Yap_MkApplTerm(FunctorInnerCall,3,ts);
        }
       if (!loop) {
	 t=t1;
	 break;
       }
       t = Yap_StripModule(ArgOfTerm(2,t),&mod);
        if (IsVarTerm (t)) {
          Yap_ThrowError(INSTANTIATION_ERROR, t0,  NULL);
        }
       if (IsNumTerm(t)) {
          Yap_ThrowError(TYPE_ERROR_CALLABLE, t0,  NULL);
        }
       if (i==5  ||
	   !IsApplTerm(t)||
	   FunctorOfTerm(t) != FunctorComma) {
	 loop = false;
	    }
	    }    while(true);
     i--;
        return CommaPredicates[i-2];
	}

inline static bool do_execute(Term t, Term mod USES_REGS)
{
    register CELL *pt;
    PredEntry *pen;
    arity_t i, arity;
  Term t0 = t, mod0 = mod;
  choiceptr B1 = B;
  /* first do predicate expansion, even before you process signals.
     This way you don't get to spy goal_expansion(). */
  if (Yap_has_a_signal() && !LOCAL_InterruptsDisabled &&
      !(LOCAL_PrologMode & (AbortMode | InterruptMode | SystemMode)))
  {
    return EnterCreepMode(t, mod PASS_REGS);
  }
   t = Yap_YapStripModule(t, &mod0);
  mod = mod0;
  if (IsVarTerm(t) || IsVarTerm(mod))
    {
      Yap_ThrowError(INSTANTIATION_ERROR, t0, NULL PASS_REGS);
      return false;
    }
  if (IsPairTerm(t)) {
     Term ts[2];
    ts[0] = t;
    ts[1] = (CurrentModule == 0 ? TermProlog : CurrentModule);
    t  = Yap_MkApplTerm(FunctorCsult, 2, ts);
      }
 if (IsApplTerm(t))
  {
    Term MyB = LCL0-(CELL*)B1;
    Functor f = FunctorOfTerm(t);
    if (f == FunctorComma) {
      pen = Yap_MkConjunction(t, MyB);
        arity = pen->ArityOfPE;
	pt = NULL;
   } else if (f == FunctorInnerCall) {
      B1 = (choiceptr)(LCL0-IntOfTerm(ArgOfTerm(2,t)));
      pen =(PredEntry*)AddressOfTerm(ArgOfTerm(3,t));
      t = ArgOfTerm(1,t);
      arity = pen->ArityOfPE;
    return (CallPredicate(pen, B, pen->CodeOfPred PASS_REGS));
    }
    if (is_connective(f)) {
	 ARG1 = t;
	 ARG2 =MkIntTerm(MyB);
	 ARG3 = t0;
	 ARG4 = mod0;
	 pen = PredMetaCall;
	 
    return CallPredicate(pen, B1, pen->CodeOfPred PASS_REGS);
    } else  {
    if (IsExtensionFunctor(f)) {
      return CallError(TYPE_ERROR_CALLABLE, t0, mod0 PASS_REGS);
    }
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps)
    {
      return CallError(TYPE_ERROR_CALLABLE, t0, mod0 PASS_REGS);
    }
    pen = RepPredProp(PredPropByFunc(f, mod));
    }
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    /* now let us do what we wanted to do from the beginning !! */
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; i++)
    {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else

      XREGS[i] = *pt++;
#endif
    }
  
    return CallPredicate(pen, B1, pen->CodeOfPred PASS_REGS);
  }
 else if (IsAtomTerm(t))
  {
    PredEntry *pen;
    Atom a = AtomOfTerm(t);
    if (a==AtomCut||a==AtomTrue)
      return true;
    if (a==AtomFail)
      return false;
    pen = RepPredProp(PredPropByAtom(a, mod));


    return (CallPredicate(pen, B, pen->CodeOfPred PASS_REGS));
  }
  Yap_ThrowError(TYPE_ERROR_CALLABLE,t0, NULL);
  return false;
}

// enter locked
static bool EnterCreepMode(Term t, Term mod USES_REGS)
{
  PredEntry *PredCreep;

  if (Yap_get_signal(YAP_CDOVF_SIGNAL))
  {
    ARG1 = t;
    if (!Yap_locked_growheap(FALSE, 0, NULL))
    {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil,
                "YAP failed to grow heap at meta-call");
    }
    if (!Yap_has_a_signal())
    {
      return do_execute(ARG1, mod PASS_REGS);
    }
  }
  PredCreep = RepPredProp(PredPropByFunc(FunctorCreep, 1));
  PP = PredCreep;
  if (!IsVarTerm(t) && IsApplTerm(t) && FunctorOfTerm(t) == FunctorModule)
  {
    ARG1 = t;
  }
  else
  {
    Term ts[2];
    if (mod)
    {
      ts[0] = mod;
    }
    else
    {
      ts[0] = TermProlog;
    }
    ts[1] = t;
    ARG1 = Yap_MkApplTerm(FunctorModule, 2, ts);
  }
  CalculateStackGap(PASS_REGS1);
  P_before_spy = P;
  return CallPredicate(PredCreep, B, PredCreep->CodeOfPred PASS_REGS);
}

static Int execute(USES_REGS1)
{ /* 'call'(Goal)	 */
  Term t = Deref(ARG1);
  return do_execute(t, CurrentModule PASS_REGS);
}

bool Yap_Execute(Term t USES_REGS)
{ /* 'call'(Goal)	 */
  return do_execute(t, CurrentModule PASS_REGS);
}

static Int do_execute_n(arity_t n, Term g, Term mod USES_REGS)
{
  Atom name;
  arity_t arity;
  g = Yap_YapStripModule(g, &mod);
  if (IsVarTerm(g)) {
    Yap_ThrowError(INSTANTIATION_ERROR, g, "in call(G,...)");
  }

  if (IsApplTerm(g)) {
    Functor f = FunctorOfTerm(g);
      if (IsExtensionFunctor(f)) {
	Yap_ThrowError(TYPE_ERROR_CALLABLE, g,  "in call(G,...)");
      }
      arity = f->ArityOfFE;
      name = NameOfFunctor(f);
      if (name==AtomDot && n==1 && arity==1) {
	name = AtomCsult;
      }
      memmove( &ARG1+arity, &ARG2, n*sizeof(CELL));
      memcpy(&ARG1,RepAppl(g)+1, arity*sizeof(CELL));
  } else if (IsAtomTerm(g)) {
      arity = 0;
      name = AtomOfTerm(g);
      if (name==AtomDot && n==2)
	name = AtomCsult;
    memmove( &ARG1, &ARG2, n*sizeof(CELL));
  } else {
    Yap_ThrowError(TYPE_ERROR_CALLABLE,g, "in call(G,...)");
    return false;
  }
  Functor f = Yap_MkFunctor(name, arity+n);
  PredEntry *  pen = RepPredProp(PredPropByFunc(f, mod));
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    /* now let us do what we wanted to do from the beginning !! */
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    return CallPredicate(pen, B, pen->CodeOfPred PASS_REGS);
}

static Int execute2(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(1, ARG1, CurrentModule PASS_REGS);
}

static Int execute3(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(2, ARG1, CurrentModule PASS_REGS);
}

static Int execute4(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(3, ARG1, CurrentModule PASS_REGS);
}

static Int execute5(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(4, ARG1, CurrentModule PASS_REGS);
}

static Int execute6(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(5, ARG1, CurrentModule PASS_REGS);
}

static Int execute7(USES_REGS1)
{ /* 'call'(Goal)	 */
    return do_execute_n(6, ARG1, CurrentModule PASS_REGS);
}

static Int execute8(USES_REGS1)
{ /* 'call'(Goal)	 */
    return do_execute_n(7, ARG1, CurrentModule PASS_REGS);
}

static Int execute9(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(8, ARG1, CurrentModule PASS_REGS);
}

static Int execute10(USES_REGS1)
{ /* 'call'(Goal)	 */
    return do_execute_n(9, ARG1, CurrentModule PASS_REGS);
}

static Int execute11(USES_REGS1)
{ /* 'call'(Goal)	 */
    return do_execute_n(10, ARG1, CurrentModule PASS_REGS);
}

static Int execute12(USES_REGS1)
{ /* 'call'(Goal)	 */
  return do_execute_n(11, ARG1, CurrentModule PASS_REGS);
}

static Int execute_clause(USES_REGS1)
{ /* 'call_clause'(Goal)	 */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  choiceptr cut_cp = cp_from_integer(Deref(ARG4) PASS_REGS);
  unsigned int arity;
  Prop pe;
  yamop *code;
  Term clt = Deref(ARG3);

restart_exec:
  if (IsVarTerm(t))
  {
    Yap_Error(INSTANTIATION_ERROR, ARG3, "call/1");
    return FALSE;
  }
  else if (IsAtomTerm(t))
  {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  }
  else if (IsApplTerm(t))
  {
    register Functor f = FunctorOfTerm(t);
    register unsigned int i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return (FALSE);
    if (f == FunctorModule)
    {
      Term tmod = ArgOfTerm(1, t);
      if (!IsVarTerm(tmod) && IsAtomTerm(tmod))
      {
        mod = tmod;
        t = ArgOfTerm(2, t);
        goto restart_exec;
      }
    }
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps)
    {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i)
    {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  }
  else
  {
    return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (RepPredProp(pe)->PredFlags & MegaClausePredFlag)
  {
    code = Yap_MegaClauseFromTerm(clt);
  }
  else
  {
    code = Yap_ClauseFromTerm(clt)->ClCode;
  }
  if (Yap_get_signal(YAP_CREEP_SIGNAL))
  {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  return CallPredicate(RepPredProp(pe), cut_cp, code PASS_REGS);
}

static Int execute_in_mod(USES_REGS1)
{ /* '$execute'(Goal)	 */
  return do_execute(Deref(ARG1), Deref(ARG2) PASS_REGS);
}


 bool Yap_exists(Term t, bool succeed USES_REGS)
{
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  Int oB = LCL0 - (CELL *)B;
  SET_ASP(YENV,EnvSizeInCells);
  {
    bool rc = Yap_RunTopGoal(t, true);

    if (!rc)
    {
                 complete_inner_computation((choiceptr)(LCL0 - oB));
    }
    else
    {
      prune_inner_computation((choiceptr)(LCL0 - oB));
    }
    // We'll pass it through
    P = oP;
    CP = oCP;
    ENV = LCL0 - oENV;
    YENV = LCL0 - oYENV;
    choiceptr nb = (choiceptr)(LCL0 - oB);
    if (nb > B)
    {
      B = nb;
    }
  return rc ||succeed;
    } 
}

extern void *Yap_blob_info(Term t);

#include "execstruct.c"

#if 0
static bool set_watch(Int Bv, Term task)
{
  CACHE_REGS
  Term t = Yap_AllocExternalDataInStack(2);
  if (t == TermNil)
    return false;
  RepAppl(t)[1] = (CELL)setup_call_catcher_cleanup_tag;
  RepAppl(t)[2] =  Bv;
  *HR++ = t;
  *HR++ = task;
  TrailTerm(TR) = AbsPair(HR - 2);
  TR++;
  return true;
}

static bool watch_cut(Term ext)
{
  CACHE_REGS
  // called after backtracking..
  //
  Term task = TailOfTerm(ext);
  Term cleanup = ArgOfTerm(3, task);
  Term e = 0;
  bool complete = IsNonVarTerm(Deref(ArgOfTerm(4, task)));
  bool active = ArgOfTerm(5, task) == TermTrue;
  bool ex_mode = false;

  if (complete)
  {
    return true;
  }
  CELL *port_pt = deref_ptr(RepAppl(task) + 2);
  CELL *completion_pt = deref_ptr(RepAppl(task) + 4);
  if ((ex_mode = Yap_HasException(PASS_REGS1)))
  {

    e = MkAddressTerm(LOCAL_ActiveError);
    Term t;
    if (active)
    {
      t = Yap_MkApplTerm(FunctorException, 1, &e);
    }
    else
    {
      t = Yap_MkApplTerm(FunctorExternalException, 1, &e);
    }
    port_pt[0] = t;
    completion_pt[0] = TermException;
  }
  else
  {
    port_pt[0] = TermCut;
  }
  yap_error_descriptor_t old;
  if (Yap_PeekException()) {
  memcpy(&old,LOCAL_ActiveError,sizeof(yap_error_descriptor_t));
    LOCAL_ActiveError->errorNo =YAP_NO_ERROR;
  } else {
    old.errorNo = YAP_NO_ERROR;
  }
  Yap_exists(cleanup, true PASS_REGS);
  if (old.errorNo) {
    Yap_RestartException(&old);
    LOCAL_PrologMode  |=   InErrorMode;
  }

else  if (Yap_RaiseException())
    return
      false;
  return true;
}

/**
 * external backtrack to current stack frame: call method
 * and control backtracking.
 *
 * @param  USES_REGS1                 [env for threaded execution]
 * @return                       c
 */
static bool watch_retry(Term d0 )
{
  CACHE_REGS
    // called after backtracking..
  //
  Term task = TailOfTerm(d0);
  bool box = ArgOfTerm(1, task) == TermTrue;
  Term cleanup = ArgOfTerm(3, task);
  bool complete = !IsVarTerm(ArgOfTerm(4, task));
  bool active = ArgOfTerm(5, task) == TermTrue;
  choiceptr B0 = (choiceptr)(LCL0 - IntegerOfTerm(ArgOfTerm(6, task)));
  yap_error_descriptor_t old;
  if (complete)
    return true;
  CELL *port_pt = deref_ptr(RepAppl(Deref(task)) + 2);
  CELL *complete_pt = deref_ptr(RepAppl(Deref(task)) + 4);
  Term t, e = 0;
  bool ex_mode = false;
  while (B->cp_ap->opc == FAIL_OPCODE ||
	 B->cp_ap == TRUSTFAILCODE)
    B = B->cp_b;

  // just do the simplest
  if (B >= B0 && !ex_mode && !active)
    return true;
  if ((ex_mode = Yap_HasException(PASS_REGS1)))
  {
  memcpy(&old,LOCAL_ActiveError,sizeof(yap_error_descriptor_t));
  e = Yap_MkErrorTerm(&old);
    if (active)
      {
	t = Yap_SaveTerm(Yap_MkApplTerm(FunctorException, 1, &e));
    }
    else
    {
      t = Yap_MkApplTerm(FunctorExternalException, 1, &e);
    }
    LOCAL_ActiveError->errorNo =YAP_NO_ERROR;
  }
  else if (B >= B0)
  {
    t = TermFail;
    complete_pt[0] = t;
  }
  else if (box)
  {
    t = TermRedo;
  }
  else
  {
    return true;
  }
  port_pt[0] = t;
  DO_TRAIL(port_pt,t);
  Yap_exists(cleanup, true PASS_REGS);
  RESET_VARIABLE(port_pt);
  // Yap_PutException(e);
   if (ex_mode) {
    Yap_RestartException(&old);
    LOCAL_PrologMode  |=   InErrorMode;
  } else if (Yap_RaiseException())
    return
      false;
 return true ;
}

/**
 * First call to non deterministic predicate. Just leaves a choice-point
 * hanging about for the future.
 *
 * @param  USES_REGS1    [env for threaded execution]
 * @return               [always succeed]
 */

static Int setup_call_catcher_cleanup(USES_REGS1)
{
  Term Setup = Deref(ARG1);
  choiceptr B0 = B;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  bool rc;

  Yap_DisableInterrupts(worker_id);
  rc = Yap_RunTopGoal(Setup, true);
  Yap_EnableInterrupts(worker_id);

  if (Yap_RaiseException())
  {
    return false;
  }
  if (!rc)
  {
    complete_inner_computation(B0);
    // We'll pass it throughs

    return false;
  }
  else
  {
    prune_inner_computation(B0);
  }
  P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  YENV = LCL0 - oYENV;
  return rc;
}

static Int tag_cleanup(USES_REGS1)
{
  Int iB = LCL0 - (CELL *)B;
  set_watch(iB, Deref(ARG2));
  return Yap_unify(ARG1, MkIntegerTerm(iB));
}

static Int cleanup_on_exit(USES_REGS1)
{
  
  choiceptr B0 = (choiceptr)(LCL0 - IntegerOfTerm(Deref(ARG1)));
  Term task = Deref(ARG2);
  bool box = ArgOfTerm(1, task) == TermTrue;
  Term cleanup = ArgOfTerm(3, task);
  Term complete = IsNonVarTerm(ArgOfTerm(4, task));

  while (B && (
	       B->cp_ap->opc == FAIL_OPCODE ||
	       B->cp_ap == TRUSTFAILCODE ||
	       B->cp_ap == NOCODE
	       ))
    B = B->cp_b;
  Term tq;
  if ((tq = Yap_ReadTimedVar(LOCAL_WokenGoals)) != 0 &&
      tq != TermNil) {
    if (! Yap_ExecuteCallMetaCall(tq, CurrentModule) ) {
      return false;
    }
}
  if (complete)
  {
    return true;
  }
  CELL *catcher_pt = deref_ptr(RepAppl(Deref(task)) + 2);
  CELL *complete_pt = deref_ptr(RepAppl(Deref(task)) + 4);
  if (B < B0)
  {
    // non-deterministic
    set_watch(LCL0 - (CELL *)B, task);
    if (!box)
    {
      return true;
    }
    catcher_pt[0] = TermAnswer;
  }
  else
  {
    catcher_pt[0] = TermExit;
    complete_pt[0] = TermExit;
  }
  Yap_exists(cleanup, true PASS_REGS);
  if (Yap_HasException(PASS_REGS1))
  {
    Yap_JumpToEnv();
    return false;
  }
  return true;
}

static bool complete_ge(bool out, Term omod, yhandle_t sl, bool creeping)
{
  CACHE_REGS
  if (creeping)
  {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  CurrentModule = omod;
  Yap_CloseSlots(sl);
  return out;
}

static Int _user_expand_goal(USES_REGS1)
{
  yhandle_t sl = Yap_StartSlots();
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  Term cmod = CurrentModule;
  Term g = Deref(ARG1);
  yhandle_t h1 = Yap_InitSlot(g),
    h2 = Yap_InitSlot(ARG2);
  /* CurMod:goal_expansion(A,B) */
  if ((pe = RepPredProp(Yap_GetPredPropByFunc(FunctorGoalExpansion2, cmod))) &&
      pe->OpcodeOfPred != FAIL_OPCODE  &&
      pe->OpcodeOfPred != UNDEF_OPCODE  &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
    {
    return complete_ge( true, cmod, sl, creeping);
  }
  /* user:goal_expansion(A,B) */
  ARG1 = Yap_GetFromSlot(h1);
  ARG2 = Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorGoalExpansion2, USER_MODULE))) &&
	pe->OpcodeOfPred != UNDEF_OPCODE  &&
      pe->OpcodeOfPred != FAIL_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
  {
    return complete_ge( true, cmod, sl, creeping);
  }
  /* user:goal_expansion(A,CurMod,B) */
  ARG1 = Yap_GetFromSlot(h1);
  ARG2 = cmod;
  ARG3 = Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
			Yap_GetPredPropByFunc(FunctorGoalExpansion, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE &&
      pe->OpcodeOfPred != UNDEF_OPCODE  &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
  {
    return complete_ge( true, cmod, sl, creeping);
  }
  ARG1 = Yap_GetFromSlot(h1);
  ARG2 = Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorGoalExpansion2, SYSTEM_MODULE))) &&
	pe->OpcodeOfPred != UNDEF_OPCODE  &&
      pe->OpcodeOfPred != FAIL_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
  {
        return complete_ge( true, cmod, sl, creeping);
  }
  return  complete_ge(false, cmod, sl, creeping);
}


static Int do_term_expansion(USES_REGS1)
{
  yhandle_t sl = Yap_StartSlots();
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  Term cmod = CurrentModule, omod = cmod;
  Term g = Deref(ARG1), o = Deref(ARG2);
  yhandle_t h1 = Yap_InitSlot(g), h2 = Yap_InitSlot(o);
  /* user:term_expansion(A,B) */
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorTermExpansion, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
  {
            return complete_ge(true, omod, sl, creeping);
  }
  ARG1 =
    Yap_GetFromSlot(h1);
  ARG2 = cmod;
  ARG3 =  Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorTermExpansion3, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred )
  {

        return complete_ge(true, omod, sl, creeping);

  }
  /* CurMod:term_expansion(A,B) */
  ARG1 =   Yap_GetFromSlot(h1);
  ARG2 =  Yap_GetFromSlot(h2);
  if (cmod != USER_MODULE &&
      (pe = RepPredProp(Yap_GetPredPropByFunc(FunctorTermExpansion, cmod))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
            Yap_execute_pred(pe, NULL, true PASS_REGS))
  {

        return complete_ge(true, omod, sl, creeping);
  }
  /* system:term_expansion(A,B) */
  ARG1 =   Yap_GetFromSlot(h1);
  ARG2 =  Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
           Yap_GetPredPropByFunc(FunctorTermExpansion, SYSTEM_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE &&
      pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
  {
    return complete_ge(true, omod, sl, creeping);
  }

 return complete_ge(
		    false , omod, sl, creeping);
}

#endif

static Int execute0(USES_REGS1)
{ /* '$execute0'(Goal,Mod)	 */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  arity_t i, arity;
  PredEntry *pe;

  /* if (Yap_has_a_signal() && !LOCAL_InterruptsDisabled) { */
  /*   pe = Yap_interrupt_execute(P PASS_REGS); */
  /*   return pe->OpcodeOfPred != FAILCODE; */
  /* } */
 
  pe = Yap_get_pred(t, mod, "call");
  if (!pe)
    return false;
  arity = pe->ArityOfPE;
  if (arity)
  {
    if (arity > MaxTemps)
    {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
    CELL *pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i)
    {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  return CallPredicate(pe, B,
                       pe->CodeOfPred PASS_REGS);
}

static Int creep_step(USES_REGS1)
{ /* '$execute_nonstop'(Goal,Mod)
                                     */
  Term t = Deref(ARG1);
  Term mod = Deref(ARG2);
  arity_t arity, i;
  bool rc;
  PredEntry *pe = Yap_get_pred(t, mod, "execute0");
  if (!pe)
    return false;
  arity = pe->ArityOfPE;
  if (arity)
  {
    CELL *pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i)
    {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (pe->PredFlags & SpiedPredFlag)
  {
    if (!LOCAL_InterruptsDisabled && Yap_get_signal(YAP_CREEP_SIGNAL))
    {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
#if defined(YAPOR) || defined(THREADS)
    if (pe->PredFlags & LogUpdatePredFlag)
    {
      PP = pe;
      PELOCK(80, PP);
    }
#endif
    rc = CallPredicate(pe, B,
                       pe->cs.p_code.TrueCodeOfPred PASS_REGS);
  }
  else
  {
    rc = CallPredicate(pe, B,
                       pe->CodeOfPred PASS_REGS);
  }
  if (!LOCAL_InterruptsDisabled &&
      (!(pe->PredFlags & (AsmPredFlag | CPredFlag)) ||
       pe->OpcodeOfPred == Yap_opcode(_call_bfunc_xx)))
  {
    Yap_signal(YAP_CREEP_SIGNAL);
  }
  return rc;
}

static Int execute_nonstop(USES_REGS1)
{ /* '$execute_nonstop'(Goal,Mod)
                                          */
  Term t = Deref(ARG1);
  Term mod = CurrentModule;
  unsigned int arity;
  Prop pe;

  t = Yap_YapStripModule(t, &mod);
  if (IsVarTerm(mod))
  {
    mod = CurrentModule;
  }
  else if (!IsAtomTerm(mod))
  {
    Yap_Error(TYPE_ERROR_ATOM, ARG2, "call/1");
    return FALSE;
  }
  if (IsVarTerm(t))
  {
    Yap_Error(INSTANTIATION_ERROR, ARG1, "call/1");
    return FALSE;
  }
  else if (IsAtomTerm(t))
  {
    Atom a = AtomOfTerm(t);
    pe = PredPropByAtom(a, mod);
  }
  else if (IsApplTerm(t))
  {
    register Functor f = FunctorOfTerm(t);
    register unsigned int i;
    register CELL *pt;

    if (IsExtensionFunctor(f))
      return false;
    pe = PredPropByFunc(f, mod);
    arity = ArityOfFunctor(f);
    if (arity > MaxTemps)
    {
      return CallError(TYPE_ERROR_CALLABLE, t, mod PASS_REGS);
    }
    /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
    pt = RepAppl(t) + 1;
    for (i = 1; i <= arity; ++i)
    {
#if YAPOR_SBA
      Term d0 = *pt++;
      if (d0 == 0)
        XREGS[i] = (CELL)(pt - 1);
      else
        XREGS[i] = d0;
#else
      XREGS[i] = *pt++;
#endif
    }
  }
  else
  {
    Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
    return FALSE;
  }
  /*	N = arity; */
  /* call may not define new system predicates!! */
  if (RepPredProp(pe)->PredFlags & SpiedPredFlag)
  {
    if (!LOCAL_InterruptsDisabled && Yap_get_signal(YAP_CREEP_SIGNAL))
    {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
#if defined(YAPOR) || defined(THREADS)
    if (RepPredProp(pe)->PredFlags & LogUpdatePredFlag)
    {
      PP = RepPredProp(pe);
      PELOCK(80, PP);
    }
#endif
    return CallPredicate(RepPredProp(pe), B,
                         RepPredProp(pe)->cs.p_code.TrueCodeOfPred PASS_REGS);

    
  }
  else
  {
    if (Yap_get_signal(YAP_CREEP_SIGNAL) && !LOCAL_InterruptsDisabled &&
        (!(RepPredProp(pe)->PredFlags & (AsmPredFlag | CPredFlag)) ||
         RepPredProp(pe)->OpcodeOfPred == Yap_opcode(_call_bfunc_xx)))
    {
      Yap_signal(YAP_CREEP_SIGNAL);
    }
    return CallPredicate(RepPredProp(pe), B,
                         RepPredProp(pe)->CodeOfPred PASS_REGS);
  }
}

static Int execute_0(USES_REGS1)
{ /* '$execute_0'(Goal)	 */
  Term mod = CurrentModule;
  Term t = Yap_YapStripModule(Deref(ARG1), &mod);
  if (t == 0)
    return false;
  return do_execute(t, mod PASS_REGS);
}

static bool call_with_args(int i USES_REGS)
{
  Term mod = CurrentModule, t;
  Atom name;

  t = Yap_YapStripModule(Deref(ARG1), &mod);
  if (t == 0)
    return false;
  if (IsVarTerm(t) )
    Yap_ThrowError(INSTANTIATION_ERROR,t,"call/%d",i+1);
  if (!IsAtomTerm(t)) {
    Yap_ThrowError(TYPE_ERROR_ATOM,t,"call/%d",i+1);
  }
  name = AtomOfTerm(t);
    if (name == AtomDot && i==2) {
     name = AtomCsult;
    }
    memmove(XREGS+(1),XREGS+2,i*sizeof(CELL));
  PredEntry *  pen = RepPredProp(PredPropByFunc(Yap_MkFunctor(name,i), mod));
    /* You thought we would be over by now */
    /* but no meta calls require special preprocessing */
    /* now let us do what we wanted to do from the beginning !! */
    /* I cannot use the standard macro here because
       otherwise I would dereference the argument and
       might skip a svar */
    return CallPredicate(pen, B, pen->CodeOfPred PASS_REGS);

}

static Int execute_1(USES_REGS1)
{ /* '$execute_0'(Goal)	 */
  return call_with_args(1 PASS_REGS);
}

static Int execute_2(USES_REGS1)
{ /* '$execute_2'(Goal)	 */
  return call_with_args(2 PASS_REGS);
}

static Int execute_3(USES_REGS1)
{ /* '$execute_3'(Goal)	 */
  return call_with_args(3 PASS_REGS);
}

static Int execute_4(USES_REGS1)
{ /* '$execute_4'(Goal)	 */
  return call_with_args(4 PASS_REGS);
}

static Int execute_5(USES_REGS1)
{ /* '$execute_5'(Goal)	 */
  return call_with_args(5 PASS_REGS);
}

static Int execute_6(USES_REGS1)
{ /* '$execute_6'(Goal)	 */
  return call_with_args(6 PASS_REGS);
}

static Int execute_7(USES_REGS1)
{ /* '$execute_7'(Goal)	 */
  return call_with_args(7 PASS_REGS);
}

static Int execute_8(USES_REGS1)
{ /* '$execute_8'(Goal)	 */
  return call_with_args(8 PASS_REGS);
}

static Int execute_9(USES_REGS1)
{ /* '$execute_9'(Goal)	 */
  return call_with_args(9 PASS_REGS);
}

static Int execute_10(USES_REGS1)
{ /* '$execute_10'(Goal)	 */
  return call_with_args(10 PASS_REGS);
}

#ifdef DEPTH_LIMIT

static Int execute_depth_limit(USES_REGS1)
{
  Term d = Deref(ARG2);
  if (IsVarTerm(d))
  {
    Yap_Error(INSTANTIATION_ERROR, d, "depth_bound_call/2");
    return false;
  }
  else if (!IsIntegerTerm(d))
  {
    if (IsFloatTerm(d) && isinf(FloatOfTerm(d)))
    {
      DEPTH = RESET_DEPTH();
    }
    else
    {
      Yap_Error(TYPE_ERROR_INTEGER, d, "depth_bound_call/2");
      return false;
   }
  }
  else
  {
    DEPTH = MkIntegerTerm(IntegerOfTerm(d) * 2);
  }
  return execute(PASS_REGS1);
}

#endif

static int exec_absmi(bool top, yap_reset_t reset_mode USES_REGS)
{
  int lval, out;

   Int OldBorder = LOCAL_CBorder;
   //Int OldSlots = LOCAL_CurSlot;
   LOCAL_CurSlot = 0;
  LOCAL_CBorder = LCL0 - (CELL *)B;
  sigjmp_buf signew, *sighold = LOCAL_RestartEnv;
  LOCAL_RestartEnv = &signew;
  volatile int top_stream =  Yap_FirstFreeStreamD();

   lval = sigsetjmp(signew, 0);
    switch (lval)
    {
    case 0:
    { /* restart */
      /* otherwise, SetDBForThrow will fail entering critical mode */
      /* find out where to cut to */
      /* siglongjmp resets the TR hardware register */
      /* TR and B are crucial, they might have been changed, or pnot */
      restore_TR();
      restore_B();
      /* H is not so important, because we're gonna backtrack */
      restore_H();
      /* set stack */
      ASP = (CELL *)PROTECT_FROZEN_B(B);
      /* forget any signals active, we're reborne */
      LOCAL_PrologMode |= UserMode;
      LOCAL_PrologMode &= ~(BootMode | CCallMode | UnifyMode | UserCCallMode);
    YENV[E_CB] = Unsigned(B);
    if (Yap_get_signal(YAP_FAIL_SIGNAL))
      P = FAILCODE;
    if (!Yap_has_a_signal()) {
    } else {
      CalculateStackGap(PASS_REGS1);
    }
    out = Yap_absmi(0);
    break;
    case 1:
      { /* restart */
      /* otherwise, SetDBForThrow will fail entering critical mode */
      /* find out where to cut to */
      /* siglongjmp resets the TR hardware register */
      /* TR and B are crucial, they might have been changed, or pnot */
      restore_TR();
      restore_B();
      /* H is not so important, because we're gonna backtrack */
      restore_H();
      /* set stack */
      ASP = (CELL *)PROTECT_FROZEN_B(B);
      /* forget any signals active, we're reborne */
      P = (yamop *)FAILCODE;
      LOCAL_PrologMode |= UserMode;
      LOCAL_PrologMode &= ~(BootMode | CCallMode | UnifyMode | UserCCallMode);
    YENV[E_CB] = Unsigned(B);
    break;
    /* make sure we don't leave a FAIL signal hanging around */
    if (Yap_get_signal(YAP_FAIL_SIGNAL))
      P = FAILCODE;
    if (!Yap_has_a_signal())
      CalculateStackGap(PASS_REGS1);

    out = Yap_absmi(0);
    }
    break;
    case 2:
    {
      /* arithmetic exception */
      /* must be done here, otherwise siglongjmp will clobber all the
                 * registers
                 */
      /* reset the registers so that we don't have trash in abstract
                 * machine */
      Yap_set_fpu_exceptions(
          getAtomicGlobalPrologFlag(ARITHMETIC_EXCEPTIONS_FLAG));
      }
    break;
    case 3:
    { /* saved state */
      LOCAL_CurSlot = 0;
      LOCAL_CBorder = OldBorder;
      LOCAL_Error_TYPE = YAP_NO_ERROR;
      LOCAL_RestartEnv = sighold;
      return false;
    }
    case 5:
    case 6:
      // going up, unless there is no up to go to. or someone
      // but we should inform the caller on what happened.
      out = false;
      P = FAILCODE;
        if (LOCAL_CBorder < LCL0-CellPtr(B)) {
	  	  out = Yap_absmi(0);
	      }
    }
    }
     Yap_CloseTemporaryStreams(top_stream);
    LOCAL_CBorder = OldBorder;
    LOCAL_RestartEnv = sighold;
    //if (LOCAL_RestartEnv && LOCAL_PrologMode & AbortMode)
    //   Yap_RestartYap(6);
    LOCAL_PrologMode &= ~AbortMode;

    return out;

    }
    
void Yap_PrepGoal(arity_t arity, CELL *pt, choiceptr saved_b USES_REGS)
{
  /* create an initial pseudo environment so that when garbage
       collection is going up in the environment chain it doesn't get
       confused */
  //  Yap_ResetException(worker_id);
  //  sl = Yap_InitSlot(t);
  YENV = ASP;
  YENV[E_CP] = (CELL)YESCODE;
  YENV[E_CB] = (CELL)B;
  YENV[E_E] = (CELL)ENV;
#ifdef TABLING
  YENV[E_B] = (CELL)B;
#endif
#ifdef DEPTH_LIMIT
  YENV[E_DEPTH] = DEPTH;
#endif
  ENV = YENV;
  ASP -= EnvSizeInCells;
  /* and now create a pseudo choicepoint for much the same reasons */
  /* CP = YESCODE; */
  /* keep a place where you can inform you had an exception */
  if (pt)
  {
    int i;
    for (i = 0; i < arity; i++)
    {
      XREGS[i + 1] = *pt++;
    }
  }
  B = (choiceptr)ASP;
  B--;
  B->cp_h = HR;
  B->cp_tr = TR;
  B->cp_cp = CP;
  B->cp_ap = NOCODE;
  B->cp_env = ENV;
  B->cp_b = saved_b;
#ifdef DEPTH_LIMIT
  B->cp_depth = DEPTH;
#endif /* DEPTH_LIMIT */
  YENV = ASP = (CELL *)B;
  YENV[E_CB] = (CELL)B;
  HB = HR;
  CP = YESCODE;
}

static int do_goal(yamop *CodeAdr, int arity, CELL *pt, bool top USES_REGS)
{

 restart:
  {
  Int out = false;
  Yap_PrepGoal(arity, pt, B PASS_REGS);
  CACHE_A1();
  P = (yamop *)CodeAdr;
  //  S = CellPtr(RepPredProp(
  //    PredPropByFunc(Yap_MkFunctor(AtomCall, 1), 0))); /* A1 mishaps */
    out = exec_absmi(top, YAP_EXEC_ABSMI PASS_REGS);
     if (Yap_has_a_signal()) {
      Yap_dispatch_interrupts(PASS_REGS1);
      goto restart;
    } else {
      CalculateStackGap(PASS_REGS1);
    }

  //  if (out) {
  //    out = Yap_GetFromSlot(sl);
  //  }
  //  Yap_RecoverSlots(1);
  LOCAL_PrologMode &= ~TopGoalMode;
  return out;
  }

}

bool Yap_exec_absmi(bool top, yap_reset_t has_reset)
{
  CACHE_REGS
  return exec_absmi(top, has_reset PASS_REGS);
}

/**
 * Fails computation up to choice-point bb
 * @param  USES_REGS    [description]
 */
void Yap_fail_all(choiceptr bb USES_REGS)
{
  yamop *saved_p, *saved_cp;

  saved_p = P;
  saved_cp = CP;
  /* prune away choicepoints */
  while (B->cp_b && B->cp_b != bb && B->cp_ap != NOCODE)
  {
    B = B->cp_b;
#ifdef YAPOR
    CUT_prune_to(B);
#endif
  }
  P = FAILCODE;
  int a = -1;
  while (a < 0)
  {
    a = exec_absmi(true, YAP_EXEC_ABSMI PASS_REGS);
  }
  /* recover stack space */
  HR = B->cp_h;
  TR = B->cp_tr;
#ifdef DEPTH_LIMIT
  DEPTH = B->cp_depth;
#endif /* DEPTH_LIMIT */
  YENV = ENV = B->cp_env;
/* recover local stack */
#ifdef DEPTH_LIMIT
  DEPTH = ENV[E_DEPTH];
#endif
  /* make sure we prune C-choicepoints */
  if (POP_CHOICE_POINT(B->cp_b))
  {
    POP_EXECUTE();
  }
  ENV = (CELL *)(ENV[E_E]);
  /* ASP should be set to the top of the local stack when we
       did the call */
  ASP = B->cp_env;
  /* YENV should be set to the current environment */
  YENV = ENV = (CELL *)((B->cp_env)[E_E]);
  if (B->cp_b)
  {
    B = B->cp_b;
  }
  // SET_BB(B);
  HB = PROTECT_FROZEN_H(B);
  CP = saved_cp;
  P = saved_p;
}

bool Yap_execute_pred(PredEntry *ppe, CELL *pt, bool pass_ex USES_REGS)
{
  yamop *saved_p, *saved_cp;
  yamop *CodeAdr;
  bool out;

  saved_p = P;
  saved_cp = CP;
  LOCAL_PrologMode |= TopGoalMode;

  PELOCK(81, ppe);
  CodeAdr = ppe->CodeOfPred;
  UNLOCK(ppe->PELock);
  out = do_goal(CodeAdr, ppe->ArityOfPE, pt, true PASS_REGS);

  if (out)
  {
    choiceptr cut_B;
    /* we succeeded, let's prune */
    /* restore the old environment */
    /* get to previous environment */
    cut_B = (choiceptr)ENV[E_CB];
    {
      /* Note that
               cut_B == (choiceptr)ENV[E_CB] */
      while (POP_CHOICE_POINT(ENV[E_CB]))
      {
        POP_EXECUTE();
      }
    }
#ifdef YAPOR
    CUT_prune_to(cut_B);
#endif /* YAPOR */
#ifdef TABLING
    if (B != cut_B)
    {
      while (B->cp_b < cut_B)
      {
        B = B->cp_b;
      }
#ifdef TABLING
      abolish_incomplete_subgoals(B);
#endif
    }
#endif /* TABLING */
    B = cut_B;
    CP = saved_cp;
    P = saved_p;
    ASP = ENV;
#ifdef DEPTH_LIMIT
    DEPTH = ENV[E_DEPTH];
#endif
    ENV = (CELL *)(ENV[E_E]);
    /* we have failed, and usually we would backtrack to this B,
           trouble is, we may also have a delayed cut to do */
    if (B != NULL)

      HB = B->cp_h;
    YENV = ENV;
    // should we catch the exception or pass it through?
    // We'll pass it through
    if (pass_ex && Yap_HasException(PASS_REGS1))
    {
        Yap_RaiseException();
      return false;
    }
    return true;
  }
  else if (out == 0)
  {
    P = saved_p;
    CP = saved_cp;
    HR = B->cp_h;
#ifdef DEPTH_LIMIT
    DEPTH = B->cp_depth;
#endif
    /* ASP should be set to the top of the local stack when we
           did the call */
    ASP = B->cp_env;
    /* YENV should be set to the current environment */
    YENV = ENV = (CELL *)((B->cp_env)[E_E]);
    B = B->cp_b;
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
    // should we catch the exception or pass it through?
    // We'll pass it through
    if (pass_ex && Yap_RaiseException())
      return false;
    return false;
  }
  else
  {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "emulator crashed");
    return false;
  }
}

bool Yap_execute_goal(Term t, int nargs, Term mod, bool pass_ex)
{
  CACHE_REGS
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  /* preserve the current restart environment */
  /* visualc*/
  /* just keep the difference because of possible garbage collections
     */
  if (IsAtomTerm(t))
  {
    Atom a = AtomOfTerm(t);
    pt = NULL;
    pe = PredPropByAtom(a, mod);
  }
  else if (IsApplTerm(t))
  {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f))
    {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
      return false;
    }
    /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
    pt = RepAppl(t) + 1;
    pe = PredPropByFunc(f, mod);
  }
  else
  {
    Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
    return false;
  }
  ppe = RepPredProp(pe);
  if (pe == NIL)
  {
    return CallMetaCall(t, mod PASS_REGS);
  }
  return Yap_execute_pred(ppe, pt, pass_ex PASS_REGS);
}

void Yap_trust_last(void)
{
 CACHE_REGS
  ASP = B->cp_env;
  CP = B->cp_cp;
  HR = B->cp_h;
#ifdef DEPTH_LIMIT
  DEPTH = B->cp_depth;
#endif
  YENV = ASP = B->cp_env;
  ENV = (CELL *)((B->cp_env)[E_E]);
  B = B->cp_b;
  P = (yamop *)(ENV[E_CP]);
  if (B)
  {
    SET_BB(B);
    HB = PROTECT_FROZEN_H(B);
  }
}

Term Yap_RunTopGoal(Term t, bool handle_errors)
{
  CACHE_REGS
  yamop *CodeAdr;
  Prop pe;
  PredEntry *ppe;
  CELL *pt;
  UInt arity;
  Term tmod = CurrentModule;
  Term goal_out = 0;
  LOCAL_PrologMode |= TopGoalMode;

  t = Yap_YapStripModule(t, &tmod);
  if (IsVarTerm(t))
  {
    Yap_Error(INSTANTIATION_ERROR, t, "call/1");
    LOCAL_PrologMode &= ~TopGoalMode;
    return (FALSE);
  }
  if (IsPairTerm(t))
  {
     Term ts[2];
    ts[0] = t;
    ts[1] = (CurrentModule == 0 ? TermProlog : CurrentModule);
    t = Yap_MkApplTerm(FunctorCsult, 2, ts);
  }
  if (IsAtomTerm(t))
  {
    Atom a = AtomOfTerm(t);
    pt = NULL;
    pe = Yap_GetPredPropByAtom(a, tmod);
    arity = 0;
  }
  else if (IsApplTerm(t))
  {
    Functor f = FunctorOfTerm(t);

    if (IsBlobFunctor(f))
    {
      Yap_Error(TYPE_ERROR_CALLABLE, t, "call/1");
      LOCAL_PrologMode &= ~TopGoalMode;
      return (FALSE);
    }
    /* I cannot use the standard macro here because
           otherwise I would dereference the argument and
           might skip a svar */
    pe = Yap_GetPredPropByFunc(f, tmod);
    pt = RepAppl(t) + 1;
    arity = ArityOfFunctor(f);
  }
  else
  {
    Yap_ThrowError(TYPE_ERROR_CALLABLE, Yap_PredicateIndicator(t, tmod), "call/1");
    LOCAL_PrologMode &= ~TopGoalMode;
    return (FALSE);
  }
  ppe = RepPredProp(pe);
  if (pe == NIL  ||
      (ppe->PredFlags & (MetaPredFlag)))
  {
    // we're in a meta-call, rake care about modules
    //
    Term ts[2];
    ts[0] = tmod;
    ts[1] = t;
    Functor f = Yap_MkFunctor(Yap_LookupAtom("call"), 1);

    pt = &t;
    t = Yap_MkApplTerm(FunctorModule, 2, ts);
    pe = Yap_GetPredPropByFunc(f, tmod);
    ppe = RepPredProp(pe);
    arity = 1;
  }
  PELOCK(82, ppe);
  CodeAdr = ppe->CodeOfPred;
  UNLOCK(ppe->PELock);

#if !USE_SYSTEM_MALLOC
  if (LOCAL_TrailTop - HeapTop < 2048)
  {
    Yap_Error(RESOURCE_ERROR_TRAIL, TermNil,
              "unable to boot because of too little Trail space");
  }
#endif
    LOCAL_PrologMode &= ~TopGoalMode;
    goal_out = do_goal(CodeAdr, arity, pt, handle_errors PASS_REGS);
  return goal_out;
}

static void do_restore_regs(Term t, int restore_all USES_REGS)
{
  if (IsApplTerm(t))
  {
    Int i;
    Int max = ArityOfFunctor(FunctorOfTerm(t)) - 4;
    CELL *ptr = RepAppl(t) + 5;

    P = (yamop *)IntegerOfTerm(ptr[-4]);
    CP = (yamop *)IntegerOfTerm(ptr[-3]);
    ENV = (CELL *)(LCL0 - IntegerOfTerm(ptr[-2]));
    YENV = (CELL *)(LCL0 - IntegerOfTerm(ptr[-1]));
    for (i = 0; i < max; i += 2)
    {
      Int j = IntOfTerm(ptr[0]);
      XREGS[j] = ptr[1];
      ptr += 2;
    }
  }
}

/* low level voodoo to restore temporary registers after a call */
static Int restore_regs(USES_REGS1)
{
  Term t = Deref(ARG1);
  if (IsVarTerm(t))
 {
    Yap_Error(INSTANTIATION_ERROR, t, "support for coroutining");
    return (FALSE);
  }
  if (t == TermFail) {
    P = FAILCODE;
    return false;

}
  if (IsAtomTerm(t))
    return (TRUE);
  do_restore_regs(t, FALSE PASS_REGS);
  return (TRUE);
}

bool Yap_restore_regs(Term t USES_REGS)
{
  t = Deref(t);
  if (IsVarTerm(t))
  {
    Yap_Error(INSTANTIATION_ERROR, t, "support for coroutining");
    return false;
  }
  if (IsAtomTerm(t))
    return true;
  do_restore_regs(t, FALSE PASS_REGS);
  return true;
}







/* low level voodoo to cut and then restore temporary registers after
 * a
 * call */
static Int restore_regs2(USES_REGS1)
{

  Term t = Deref(ARG1), d0;
  choiceptr pt0;
  Int d;

  if (IsVarTerm(t))
  {
    Yap_Error(INSTANTIATION_ERROR, t, "support for coroutining");
    return (FALSE);
  }
  d0 = Deref(ARG2);
  if (!IsAtomTerm(t))
  {
    do_restore_regs(t, TRUE PASS_REGS);
  }
  if (IsVarTerm(d0))
  {
    Yap_Error(INSTANTIATION_ERROR, d0, "support for coroutining");
    return (FALSE);
  }
  if (!IsIntegerTerm(d0))
  {
    return (FALSE);
  }
  d = IntegerOfTerm(d0);
  if (!d)
    return TRUE;
#if YAPOR_SBA
  pt0 = (choiceptr)d;
#else
  pt0 = (choiceptr)(LCL0 - d);
#endif
  /* find where to cut to */
  if ((CELL *)pt0 != LCL0 && pt0 > B)
  {
    /* Wow, we're gonna cut!!! */
    while (B->cp_b < pt0)
    {
      while (POP_CHOICE_POINT(B->cp_b))
      {
        POP_EXECUTE();
      }
      HB = B->cp_h;
      Yap_TrimTrail();
      B = B->cp_b;
    }
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif
#ifdef YAPOR
    CUT_prune_to(pt0);
#endif /* YAPOR */
    B = pt0;
  }
  return (TRUE);
}

static bool cut_at(choiceptr pt0 USES_REGS)
{
  if (pt0 < B)
  {
    /* t0his should never happen */
    return false;
  }
  else if (pt0 == B)
  {
    prune(pt0, B PASS_REGS);
  }
  else
  {
    choiceptr b = B;
    while (b != pt0 && b->cp_b != pt0 && b->cp_b)
      b = b->cp_b;
    if (b == B)
      pt0->cp_ap = (yamop *)TRUSTFAILCODE;
  }
  return true;
}

static Int cut_at1(USES_REGS1)
{
  Term t0 = Deref(ARG1);
  choiceptr pt0;

  must_be_integer(t0);
#if YAPOR_SBA
  pt0 = (choiceptr)IntegerOfTerm(t0);
#else
  pt0 = cp_from_integer(t0 PASS_REGS);
#endif
  return cut_at(pt0 PASS_REGS);
}


static Int clean_ifcp(USES_REGS1)
{
  Term t = Deref(ARG2);
  Term t0 = Deref(ARG1);
  choiceptr pt0;
  
  must_be_integer(t0);
  must_be_integer(t);
  if (t0 == t)
    return true;
#if YAPOR_SBA
  pt0 = (choiceptr)IntegerOfTerm(t0);
#else
  pt0 = cp_from_integer(t0 PASS_REGS);
#endif
  return cut_at(pt0 PASS_REGS);
}

static int disj_marker(yamop *apc)
{
  op_numbers opnum = Yap_op_from_opcode(apc->opc);

  return opnum == _or_else || opnum == _or_last;
}

static Int cut_up_to_next_disjunction(USES_REGS1)
{
  choiceptr pt0 = B;
  CELL *qenv = (CELL *)ENV[E_E];

  while (pt0 && !(qenv == pt0->cp_env && disj_marker(pt0->cp_ap)))
  {
    pt0 = pt0->cp_b;
  }
  if (!pt0)
    return TRUE;
#ifdef YAPOR
  CUT_prune_to(pt0);
#endif /* YAPOR */
  /* find where to cut to */
  if (SHOULD_CUT_UP_TO(B, pt0))
  {
    B = pt0;
#ifdef TABLING
    abolish_incomplete_subgoals(B);
#endif /* TABLING */
  }
  HB = B->cp_h;
  Yap_TrimTrail();
  return TRUE;
}

/**
 * Reset the Prolog engine . If _Hard_ resèt the global stack_el. If
 * p_no_use_'soft_float keei
 *
 * @param mode
 *xf @param hard
 *
 * @return
 */
bool Yap_Reset(yap_reset_t mode, bool hard)
{
  CACHE_REGS
  int res = TRUE;

  Yap_ResetException(NULL);
  /* first, backtrack to the root */
  while (B)
  {
    P = FAILCODE;
    int a = -1;
    while (a < 0)
    {
      a = exec_absmi(true, mode PASS_REGS);
    }
    B = B->cp_b;
  }
  /* reinitialize the engine */
  Yap_InitYaamRegs(worker_id, false);
  GLOBAL_Initialised = true;
  ENV = LCL0;
  ASP = (CELL *)B;
  /* the first real choice-point will also have AP=FAIL */
  /* always have an empty slots for people to use */
  P = CP = YESCODE;
  // ensure that we have slots where we need them
  Yap_RebootSlots(worker_id);
  return res;
}

bool is_cleanup_cp(choiceptr cp_b)
{
  PredEntry *pe;

  if (cp_b->cp_ap->opc != ORLAST_OPCODE)
    return FALSE;
#ifdef YAPOR
  pe = cp_b->cp_ap->y_u.Osblp.p0;
#else
  pe = cp_b->cp_ap->y_u.p.p;
#endif /* YAPOR */
  /*
      it has to be a cleanup and it has to be a completed goal,
      otherwise the throw will be caught anyway.
    */
  return pe == PredSafeCallCleanup;
}


void Yap_InitYaamRegs(int myworker_id, bool full_reset)
{
  //  getchar();
#if PUSH_REGS
  /* Guarantee that after a longjmp we go back to the original abstract
     machine registers */
#ifdef THREADS
  if (myworker_id)
    {
      REGSTORE *rs = REMOTE_ThreadHandle(myworker_id).default_yaam_regs;
      pthread_setspecific(Yap_yaamregs_key, (const void *)rs);
      REMOTE_ThreadHandle(myworker_id).current_yaam_regs = rs;
    }
  /* may be run by worker_id on behalf on myworker_id */
#else
  Yap_regp = &Yap_standard_regs;
#endif
#endif /* PUSH_REGS */
  CACHE_REGS
    Yap_PutValue(AtomBreak, MkIntTerm(0));

  REMOTE_CurHandle(myworker_id) =
    REMOTE_NSlots(myworker_id) = 0;
  Yap_InitPreAllocCodeSpace(myworker_id);
  TR = (tr_fr_ptr)REMOTE_TrailBase(myworker_id);
  HR = H0 = ((CELL *)REMOTE_GlobalBase(myworker_id)) ;
  LCL0 = ASP = (CELL *)REMOTE_LocalBase(myworker_id);
  CurrentTrailTop = (tr_fr_ptr)(REMOTE_TrailTop(myworker_id) - MinTrailGap);
  /* notice that an initial choice-point and environment
   *must* be created  for the garbage collector to work */
  B = NULL;
  ENV = NULL;
  P = CP = YESCODE;
#ifdef DEPTH_LIMIT
  DEPTH = RESET_DEPTH();
#endif
  STATIC_PREDICATES_MARKED = FALSE;
  HR = H0;
  if (full_reset)
    {
      Yap_AllocateDefaultArena(128 * 128, 0, NULL);
    }
  else
    {
      HR = Yap_ArenaLimit(REMOTE_GlobalArena(myworker_id));
    }
#ifdef FROZEN_STACKS
  H_FZ = HR;
#ifdef YAPOR_SBA
  BSEG =
#endif /* YAPOR_SBA */
    BBREG = B_FZ = (choiceptr)REMOTE_LocalBase(myworker_id);
  TR = TR_FZ = (tr_fr_ptr)REMOTE_TrailBase(myworker_id);
#endif /* FROZEN_STACKS */
  REMOTE_GcGeneration(myworker_id) = Yap_NewCompactTimedVar(MkIntTerm(0));
  REMOTE_GcCurrentPhase(myworker_id) = MkIntTerm(0L);
  REMOTE_GcPhase(myworker_id) = Yap_NewTimedVar(MkIntTerm(0L));
  REMOTE_WokenGoals(myworker_id) = Yap_NewTimedVar(TermTrue);
  REMOTE_AttsMutableList(myworker_id) = Yap_NewTimedVar(TermNil);

  CalculateStackGap(PASS_REGS1);
  /* the first real choice-point will also have AP=FAIL */
  /* always have an empty slots for people to use */
#if defined(YAPOR) || defined(THREADS)
//  LOCAL = REMOTE(myworker_id);
  regcache->worker_local_ = REMOTE(myworker_id);
  worker_id = myworker_id;
#endif /* THREADS */
#if defined(YAPOR) || defined(THREADS)
  PP = NULL;
  PREG_ADDR = NULL;
#endif
  cut_c_initialize(myworker_id);
  Yap_PrepGoal(0, NULL, NULL PASS_REGS);
#ifdef FROZEN_STACKS
  H_FZ = HR;
#ifdef YAPOR_SBA
  BSEG =
#endif /* YAPOR_SBA */
    BBREG = B_FZ = (choiceptr)REMOTE_LocalBase(myworker_id);
  TR = TR_FZ = (tr_fr_ptr)REMOTE_TrailBase(myworker_id);
#endif /* FROZEN_STACKS */
  CalculateStackGap(PASS_REGS1);
#ifdef TABLING
  /* ensure that LOCAL_top_dep_fr is always valid */
  if (REMOTE_top_dep_fr(myworker_id))
    DepFr_cons_cp(REMOTE_top_dep_fr(myworker_id)) = NORM_CP(B);
#endif
}

static void InitCommaPreds(void) {
  arity_t i;
  CommaPredicates = malloc(8*sizeof(struct pred_entry *));
  for (i=2;i<10;i++) {
    CommaPredicates[i-2] = RepPredProp(Yap_NewPredPropByFunctor(Yap_MkFunctor((AtomInnerComma), i), TermProlog));
  }
}

void Yap_InitExecFs(void)
{
  CACHE_REGS
  YAP_opaque_handler_t catcher_ops;
  memset(&catcher_ops, 0, sizeof(catcher_ops));
  catcher_ops.cut_handler = watch_cut;
  catcher_ops.fail_handler = watch_retry;
  setup_call_catcher_cleanup_tag = YAP_NewOpaqueType(&catcher_ops);

  Term cm = CurrentModule;
InitCommaPreds();
  Yap_InitCPred("$execute", 1, execute, 0);
Yap_InitCPred("$execute0", 1, execute, 0);
  Yap_InitCPred("call", 1, execute, 0);
  Yap_InitCPred("call", 2, execute2, 0);
  Yap_InitCPred("call", 3, execute3, 0);
  Yap_InitCPred("call", 4, execute4, 0);
  Yap_InitCPred("call", 5, execute5, 0);
  Yap_InitCPred("call", 6, execute6, 0);
  Yap_InitCPred("call", 7, execute7, 0);
  Yap_InitCPred("call", 8, execute8, 0);
  Yap_InitCPred("call", 9, execute9, 0);
  Yap_InitCPred("call", 10, execute10, 0);
  Yap_InitCPred("call", 11, execute11, 0);
  Yap_InitCPred("call", 12, execute12, 0);
  Yap_InitCPred("call_in_mod", 2, execute_in_mod, 0);
  Yap_InitCPred("call_wo_mod", 2, execute_in_mod, 0);
  Yap_InitCPred("call_with_args", 1, execute_0, 0);
  Yap_InitCPred("call_with_args", 2, execute_1, 0);
  Yap_InitCPred("call_with_args", 3, execute_2, 0);
  Yap_InitCPred("call_with_args", 4, execute_3, 0);
  Yap_InitCPred("call_with_args", 5, execute_4, 0);
  Yap_InitCPred("call_with_args", 6, execute_5, 0);
  Yap_InitCPred("call_with_args", 7, execute_6, 0);
  Yap_InitCPred("call_with_args", 8, execute_7, 0);
  Yap_InitCPred("call_with_args", 9, execute_8, 0);
  Yap_InitCPred("call_with_args", 10, execute_9, 0);
  Yap_InitCPred("call_with_args", 11, execute_10, 0);
#ifdef DEPTH_LIMIT
  Yap_InitCPred("$execute_under_depth_limit", 2, execute_depth_limit, 0);
#endif
  Yap_InitCPred("$execute0", 2, execute0, NoTracePredFlag);
  Yap_InitCPred("$execute_non_stop", 1, execute_nonstop, NoTracePredFlag);
  Yap_InitCPred("$creep_step", 2, creep_step, NoTracePredFlag);
  Yap_InitCPred("$execute_clause", 4, execute_clause, NoTracePredFlag);
  Yap_InitCPred("cut_at", 2, clean_ifcp, SafePredFlag);
  Yap_InitCPred("cut_at", 1, cut_at1, SafePredFlag);
  CurrentModule = HACKS_MODULE;
  Yap_InitCPred("env_choice_point", 1, save_env_b, 0);
  CurrentModule = cm;
  Yap_InitCPred("$restore_regs", 1, restore_regs,
                NoTracePredFlag | SafePredFlag);
  Yap_InitCPred("$restore_regs", 2, restore_regs2,
                NoTracePredFlag | SafePredFlag);
  Yap_InitCPred("$clean_ifcp", 2, clean_ifcp, SafePredFlag);
  Yap_InitCPred("qpack_clean_up_to_disjunction", 0, cut_up_to_next_disjunction,
                SafePredFlag);
  //    Yap_InitCPred("$generate_pred_info", 4, generate_pred_info, 0);
  Yap_InitCPred("$do_user_expansion", 2, _user_expand_goal, 0);
  Yap_InitCPred("$do_term_expansion", 2, do_term_expansion, 0);
  Yap_InitCPred("$setup_call_catcher_cleanup", 1, setup_call_catcher_cleanup,
                0);
  Yap_InitCPred("$cleanup_on_exit", 2, cleanup_on_exit, 0);
  Yap_InitCPred("$tag_cleanup", 2, tag_cleanup, 0);

  Yap_InitDebugFs();
}
