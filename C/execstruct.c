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


/**
 * remove choice points created since a call to top-goal.
 *
 */
static void prune_inner_computation(choiceptr parent USES_REGS)
{
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

 bool Yap_exists(Term t, bool succeed USES_REGS)
{
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  Int oB = LCL0 - (CELL *)B;
  SET_ASP(YENV,EnvSizeInCells);
  t=Deref(t);
  Yap_must_be_callable(t,CurrentModule);
  {
    bool rc = Yap_RunTopGoal(t, succeed);

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
 Int B0 = LCL0-(CELL *)B;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  Term task = Deref(TailOfTerm(ext));
  Term cleanup = ArgOfTerm(3, task);
  Term e = 0;
  bool active = ArgOfTerm(5, task) == TermTrue;
  yap_error_descriptor_t *old, *new;
  bool ex_mode;
  CELL port = Deref(ArgOfTerm(2,task));
  CELL *port_pt = VarOfTerm(port);
  CELL completion             = Deref(ArgOfTerm(4,task));
  CELL *completion_pt = VarOfTerm(completion);
  if (IsNonVarTerm(completion))
    return true;
  if ((ex_mode = Yap_HasException(PASS_REGS1)))
    {
      Term t;
    e = MkAddressTerm(LOCAL_ActiveError);
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

       old = LOCAL_ActiveError;
      LOCAL_ActiveError = new = malloc(sizeof( yap_error_descriptor_t ));
      Yap_ResetException(new);
     }
  else
    {
      completion_pt[0] = port_pt[0] = TermCut;
    }
  bool rc = Yap_RunTopGoal(cleanup, true);

  if (!rc)
    {
      Yap_fail_all((choiceptr)(LCL0-B0) PASS_REGS);
      // We'll pass it throughs

    }
  else
    {
      prune_inner_computation((choiceptr)(LCL0-B0) PASS_REGS);
    }
  if (Yap_RaiseException())
    {
      return false;
    }
   if (ex_mode) {
    free(new);
    LOCAL_ActiveError = old;
    LOCAL_PrologMode  |=   InErrorMode;
  }
   if ( Yap_PeekException()) {
     B= (choiceptr)(LCL0-B0);
      Yap_JumpToEnv();

    return false;
  }
 P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  YENV = LCL0 - oYENV;
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
    Term task = TailOfTerm(d0),
    *taskp = RepAppl(task);
Int B0 = LCL0-(CELL *)B;
    Term cleanup = ArgOfTerm(3, task);
bool box = ArgOfTerm(1, task) == TermTrue;
  CELL port = Deref(ArgOfTerm(2,task));
  CELL *port_pt = VarOfTerm(port);
  CELL complete             = Deref(ArgOfTerm(4,task));
  CELL *complete_pt = VarOfTerm(Deref(complete));
  bool active = ArgOfTerm(5, task) == TermTrue;

  yap_error_descriptor_t *old, *new;
  Term e;
  if (!IsVarTerm(*complete_pt))
    return true;
  Term t;
  bool ex_mode = false;

  // just do the simplest
  if ((ex_mode = Yap_PeekException()))
    {
    e = MkAddressTerm(LOCAL_ActiveError);
      if (active)
	{
	  t = Yap_MkApplTerm(FunctorException, 1, &e);
	}
      else
	{
	  t = Yap_MkApplTerm(FunctorExternalException, 1, &e);
	}
      port_pt[0] = t;
      complete_pt[0] = TermException;
     old = LOCAL_ActiveError;
      LOCAL_ActiveError = new = malloc(sizeof( yap_error_descriptor_t ));
      Yap_ResetException(new);
    } else {
  if (B->cp_h <= taskp)
    {
      t = TermFail;
      complete_pt[0] = t;
    }
  else 
    {
      t = TermRedo;
    }
   }
    Yap_RunTopGoal(cleanup, true);
   //if ( (choiceptr)(LCL0-B0)  B)
   B  = (choiceptr)(LCL0-B0);
      // We'll pass it throughs

       RESET_VARIABLE(port_pt);

  // Yap_PutException(e);
  if (ex_mode) {
    free(new);
    LOCAL_ActiveError = old;
    LOCAL_PrologMode  |=   InErrorMode;
  } else   if ( Yap_PeekException()) {
      Yap_RestartYap(5);
    return false;
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
  rc = Yap_RunTopGoal(Setup, true);
  Yap_EnableInterrupts(worker_id);

  if (!rc)
    {
      Yap_fail_all((choiceptr)(LCL0-B0) PASS_REGS);
      // We'll pass it throughs

    }
  else
    {
      prune_inner_computation((choiceptr)(LCL0-B0) PASS_REGS);
    }
  B=(choiceptr)(LCL0-B0);
  if (Yap_PeekException())
    {
      Yap_RestartYap(5);
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
  return Yap_unify(ARG1, MkIntegerTerm(iB));
}

static Int cleanup_on_exit(USES_REGS1)
{
 Int B0 = LCL0-(CELL *)B;
  yamop *oP = P, *oCP = CP;
  Int oENV = LCL0 - ENV;
  Int oYENV = LCL0 - YENV;
  bool rc;
  Term task = Deref(ARG2);
   CELL port = ArgOfTerm(2,task);
  CELL *port_pt   =IsVarTerm(port) ? VarOfTerm(port) : NULL;
  CELL complete             = ArgOfTerm(4,task);
  CELL *complete_pt = IsVarTerm(complete) ? VarOfTerm(complete) : NULL;
Term cleanup = ArgOfTerm(3, task);
  if (!Yap_dispatch_interrupts( PASS_REGS1 ))
    return false;
  if (IsNonVarTerm(complete))
    {
      return true;
    }
      if (B->cp_h>RepAppl(task))
    {
      // non-deterministic
       port_pt[0]=TermAnswer;
    }
  else
    {
      port_pt[0] = TermExit;
      complete_pt[0] = TermExit;
    }
      yap_error_descriptor_t *old, *new=NULL;
  if (Yap_PeekException()) {
      old = LOCAL_ActiveError;
      LOCAL_ActiveError = new = calloc(1,sizeof( yap_error_descriptor_t ));
      Yap_ResetException(new);
  }
 rc = Yap_RunTopGoal(cleanup, true);
 if(       port_pt[0]==TermAnswer) {
   RESET_VARIABLE(port_pt);
 }
  if (!rc)
    {
      Yap_fail_all((choiceptr)(LCL0-B0) PASS_REGS);
          // We'll pass it through
    }
  else
    {
      prune_inner_computation((choiceptr)(LCL0-B0) PASS_REGS);
    }
  if (new) {
    free(new);
    LOCAL_ActiveError = old;
    LOCAL_PrologMode  |=   InErrorMode;
      Yap_RestartYap(5);
    return false;
  } else   if ( Yap_PeekException()) {
      Yap_RestartYap(5);
    return false;
  }
      if(IsVarTerm(complete_pt[0]))
    {
      
      set_watch(LCL0 - (CELL *)B, task);
    }
  P = oP;
  CP = oCP;
  ENV = LCL0 - oENV;
  YENV = LCL0 - oYENV;
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
