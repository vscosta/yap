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



#include "absmi.h"

#include "amidefs.h"
#include "attvar.h"
#include "cut_c.h"
#include "yapio.h"
#include "heapgc.h"

static bool deterministic(Int BRef)
{
  CACHE_REGS
  choiceptr cutB = B;
  choiceptr target = (choiceptr)(LCL0-BRef);
  while (cutB &&  cutB->cp_b < target) {
      yamop *altp = cutB->cp_ap;
      if (altp==FAILCODE || altp == TRUSTFAILCODE || altp == NOCODE || altp == EXITCODE)
	cutB = cutB->cp_b;
      else
	break;
  }
  return cutB == target;
  }
    
    

/**  
 * remove choice points created since a call to top-goal.
 *
 */
void Yap_prune_inner_computation(choiceptr parent USES_REGS)
{
  /* code */
  choiceptr cut_pt;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;

  cut_pt = B;
  if (B==parent)
    return;
  while (cut_pt->cp_b && cut_pt->cp_b < parent)
    {
#ifdef YAPOR
      CUT_prune_to(cut_pt);
#endif
      cut_pt = cut_pt->cp_b;
    }
  B = cut_pt;
  Yap_TrimTrail();
  LOCAL_AllowRestart = FALSE;
  P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  B = parent;
}

bool Yap_exists(Term t, ex_handler_t handle_sigs USES_REGS)
{
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  Int oB = LCL0 - (CELL *)B;
  SET_ASP(YENV,EnvSizeInCells);
  t=Deref(t);
  Yap_must_be_callable(t,CurrentModule);
  {
    bool rc = Yap_RunTopGoal(t, handle_sigs);

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
    return rc;
  }
}

static bool gate(Term t USES_REGS)
{
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  Int oB = LCL0 - (CELL *)B;
  //  SET_ASP(YENV,EnvSizeInCells);
  t=Deref(t);
  Yap_must_be_callable(t,CurrentModule);
  yap_error_descriptor_t *old=NULL;

  bool rc = Yap_RunTopGoal(t, LOCAL_EX);
  if (old) 
   LOCAL_ActiveError=old;
  if (rc) {
    LOCAL_PrologMode |= ErrorHandlingMode;
    Yap_prune_inner_computation((choiceptr)(LCL0-oB) PASS_REGS);
    LOCAL_PrologMode &= ErrorHandlingMode;
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
  return rc;
}

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
  TrailVal(TR) = 0;
  TR++;
  return true;
}

static bool watch_cut(Term ext)
{
  CACHE_REGS
    // called after backtracking..
    //
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  Term task = Deref(TailOfTerm(ext));
  Term cleanup = ArgOfTerm(3, task);
  Term e = 0;
  bool active = ArgOfTerm(5, task) == TermTrue;
  bool ex_mode;
 CELL *port_pt = deref_ptr(RepAppl(task)+2);
 if (IsNonVarTerm(port_pt[0])) {
     return true;
 }
  CELL *completion_pt = deref_ptr(RepAppl(task)+4);
  if ((ex_mode = Yap_HasException(PASS_REGS1)))
    {
      Term t;
      if (LOCAL_PrologMode & ErrorHandlingMode)
	return true;
      e = MkAddressTerm(LOCAL_ActiveError);
      if (active)
	{
	  t = Yap_MkApplTerm(FunctorException, 1, &e);
	}
      else
	{
	  t = Yap_MkApplTerm(FunctorExternalException, 1, &e);
	}
      MaBind(port_pt, t);
      completion_pt[0] = TermException;
          CP = FAILCODE;
    }
  else
    {
      completion_pt[0] = TermCut;
      MaBind(port_pt, TermCut);
    }
  gate(cleanup PASS_REGS);
//    RESET_VARIABLE(port_pt);
    P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  YENV = LCL0 - oYENV;
  if ( Yap_PeekException()) {
    Yap_JumpToEnv();
    return false;
  }
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
    Term task = TailOfTerm(d0);
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
 CELL *port_pt = deref_ptr(RepAppl(task)+2);
  CELL *completion_pt = deref_ptr(RepAppl(task)+4);
  Term cleanup = ArgOfTerm(3, task);
 bool bottom = ArgOfTerm(5, task) == TermTrue;
  Int BRef = IntOfTerm(ArgOfTerm(6, task));
  bool det = deterministic(BRef);
  Term e;
  if (IsNonVarTerm(*completion_pt))
    return true;
  CP = FAILCODE;
  bool ex_mode = false;
  //  choiceptr Bl = B;
    
  // just do the simplest
  if ((ex_mode = Yap_PeekException()))
    {
      if (LOCAL_PrologMode & ErrorHandlingMode)
	return true;
      e = MkAddressTerm(LOCAL_ActiveError);
      if (bottom)
	{
	  MaBind(port_pt,Yap_MkApplTerm(FunctorException, 1, &e));
	}
      else
	{
	  MaBind(port_pt, Yap_MkApplTerm(FunctorExternalException, 1, &e) );
	}
      completion_pt[0] = TermException;
    }
  else
    {
      if (det)
	{
	  MaBind(port_pt, TermFail);
	  completion_pt[0] = TermFail;
	}
      else 
	{
	  MaBind(port_pt, TermRedo) ;
	}
    }
  gate(cleanup PASS_REGS);
  //RESET_VARIABLE(port_pt);
  P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  YENV = LCL0 - oYENV;
  if ( Yap_PeekException()) {
    Yap_JumpToEnv();
    return false;
  }
  if (Yap_may_creep(true)) {
    return true;
  }
  return true ;
}

/**
 * First call to non deterministic predicate. Just leaves a choice-point
 * hanging about for the future.
 v *
 * @param  USES_REGS1    [env for threaded execution]
 * @return               [always succeed]
 */

static Int setup_call_catcher_cleanup(USES_REGS1)
{
  Term Setup = Deref(ARG1);
  Int B0 = LCL0-(CELL *)B;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  bool rc;

  Yap_DisableInterrupts(worker_id);
  rc = Yap_RunTopGoal(Setup, LOCAL_EX);
  Yap_EnableInterrupts(worker_id);
  if (!rc)
    {
      Yap_fail_all((choiceptr)(LCL0-B0) PASS_REGS);
      // We'll pass it throughs

    }
  else
    {
      Yap_prune_inner_computation((choiceptr)(LCL0-B0) PASS_REGS);
    }
  B=(choiceptr)(LCL0-B0);
  if (Yap_PeekException())
    {
      Yap_JumpToEnv();
      return false;
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
  Yap_unify(ARG1, MkIntegerTerm(iB));
  return true;
}

static Int cleanup_on_exit(USES_REGS1)
{
  Term task = Deref(ARG2);
  CELL *port_pt = deref_ptr(RepAppl(task)+2);
  CELL *completion_pt = deref_ptr(RepAppl(task)+4);
   Term cleanup = ArgOfTerm(3, task);
  Int BEntry = IntegerOfTerm(ArgOfTerm(6,task));
  if (!Yap_dispatch_interrupts( PASS_REGS1 ))
    return false;
  if (IsNonVarTerm(completion_pt[0]))
    {

      return true;
    }
  if ( !deterministic(BEntry))
    {
      // non-deterministic
      MaBind(port_pt,  TermAnswer);
    }
  else
    {
      MaBind(port_pt, TermExit);
      completion_pt[0] = TermExit;
    }
  gate(cleanup PASS_REGS);
  if(       port_pt[0]==TermAnswer) {
    if(IsVarTerm(completion_pt[0]))
      {
      
	set_watch(LCL0 - (CELL *)B, task);
      }
  }
  //
//  RESET_VARIABLE(port_pt);
  if (Yap_PeekException())
    {
      Yap_JumpToEnv();
      return false;
    }
  if (Yap_may_creep(true)) {
    return true;
  }


  return true;
}

static bool complete_ge(bool out, Term omod, yamop *oP, yhandle_t sl, bool creeping)
{
  CACHE_REGS
    if (creeping)
      {
	Yap_signal(YAP_CREEP_SIGNAL);
      }
  CurrentModule = omod;
  if (!out) out = Yap_unify(Yap_GetFromHandle(sl),Yap_GetFromHandle(sl+1));
  Yap_CloseSlots(sl);
  return out;
}

//volatile static int vsc_stop;
static Int _user_expand_goal(USES_REGS1)
{
  //  while (vsc_stop==0);

  yhandle_t sl = Yap_StartSlots();
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  Term cmod = CurrentModule;
  Term g = Deref(ARG1);
  yamop * oP = P;
  if (IsVarTerm(g))
    return false;
  yhandle_t h1 = Yap_InitSlot(MkGlobal(g)),
    h2 = Yap_InitSlot(MkGlobal(ARG2));
  /* CurMod:goal_expansion(A,B) */
  if ((pe = RepPredProp(Yap_GetPredPropByFunc(FunctorGoalExpansion2, cmod))) &&
      pe->OpcodeOfPred != FAIL_OPCODE  &&
      pe->OpcodeOfPred != UNDEF_OPCODE  &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
    {
      return complete_ge( true, cmod,oP, sl, creeping);
    }
  if (Yap_HasException(PASS_REGS1)){ // if (throw) {
    //  Yap_JumpToEnv();
    Yap_ResetException(NULL);
    return complete_ge( false, cmod,oP, sl, creeping);
  }

  /* user:goal_expansion(A,B) */
  ARG2 = Yap_GetFromSlot(h2);
  ARG1 = Yap_GetFromSlot(h1);
  if ((pe = RepPredProp(
			Yap_GetPredPropByFunc(FunctorGoalExpansion2, USER_MODULE))) &&
      pe->OpcodeOfPred != UNDEF_OPCODE  &&
      pe->OpcodeOfPred != FAIL_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
    {
      return complete_ge( true, cmod,oP, sl, creeping);
    }
  if (Yap_HasException(PASS_REGS1))  { // if (throw) {
    //  Yap_JumpToEnv();
    Yap_ResetException(NULL);
    return complete_ge( false, cmod,oP, sl, creeping);
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
      return complete_ge( true, cmod,oP, sl, creeping);
    }
  if (Yap_HasException(PASS_REGS1)){ // if (throw) {
    //  Yap_JumpToEnv();
    Yap_ResetException(NULL);
    return complete_ge( false, cmod,oP, sl, creeping);
  }

  ARG1 = Yap_GetFromSlot(h1);
  ARG2 = Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
			Yap_GetPredPropByFunc(FunctorGoalExpansion2, SYSTEM_MODULE))) &&
      pe->OpcodeOfPred != UNDEF_OPCODE  &&
      pe->OpcodeOfPred != FAIL_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
    {
      return complete_ge( true, cmod,oP, sl, creeping);
    }
  if (Yap_HasException(PASS_REGS1)){ // if (throw) {
    //  Yap_JumpToEnv();
    Yap_ResetException(NULL);
  }
  return  complete_ge(false, cmod,oP, sl, creeping);
}


static Int do_term_expansion(USES_REGS1)
{
  yhandle_t sl = Yap_StartSlots();
  Int creeping = Yap_get_signal(YAP_CREEP_SIGNAL);
  PredEntry *pe;
  yamop *oP=P;
  Term cmod = CurrentModule, omod = cmod;
  Term g = Deref(ARG1), o = Deref(ARG2);
  yhandle_t h1 = Yap_InitSlot(g), h2 = Yap_InitSlot(o);
  /* user:term_expansion(A,B) */
  if ((pe = RepPredProp(
			Yap_GetPredPropByFunc(FunctorTermExpansion, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
    {
      return complete_ge(true, omod, oP, sl, creeping);
    }
  ARG1 =
    Yap_GetFromSlot(h1);
  ARG2 = cmod;
  ARG3 =  Yap_GetFromSlot(h2);
  if ((pe = RepPredProp(
			Yap_GetPredPropByFunc(FunctorTermExpansion3, USER_MODULE))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred )
    {

      return complete_ge(true, omod, oP, sl, creeping);

    }
  /* CurMod:term_expansion(A,B) */
  ARG1 =   Yap_GetFromSlot(h1);
  ARG2 =  Yap_GetFromSlot(h2);
  if (cmod != USER_MODULE &&
      (pe = RepPredProp(Yap_GetPredPropByFunc(FunctorTermExpansion, cmod))) &&
      pe->OpcodeOfPred != FAIL_OPCODE && pe->OpcodeOfPred != UNDEF_OPCODE &&
      Yap_execute_pred(pe, NULL, true PASS_REGS))
    {

      return complete_ge(true, omod, oP, sl, creeping);
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
      return complete_ge(true, omod,oP, sl, creeping);
    }

  return complete_ge(
		     false , omod, oP, sl, creeping);
}



void Yap_InitExecStruct(void)
{
  YAP_opaque_handler_t catcher_ops;
  memset(&catcher_ops, 0, sizeof(catcher_ops));
  catcher_ops.cut_handler = watch_cut;
  catcher_ops.fail_handler = watch_retry;
  setup_call_catcher_cleanup_tag = YAP_NewOpaqueType(&catcher_ops);
  Yap_InitCPred("$do_user_expansion", 2, _user_expand_goal, 0);
  Yap_InitCPred("$do_term_expansion", 2, do_term_expansion, 0);
  Yap_InitCPred("$tag_cleanup", 2, tag_cleanup, 0);
  Yap_InitCPred("$setup_call_catcher_cleanup", 1, setup_call_catcher_cleanup,
                0);
  Yap_InitCPred("$cleanup_on_exit", 2, cleanup_on_exit, 0);
}
