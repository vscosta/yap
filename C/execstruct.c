
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

  if (Yap_RaiseException())
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
	  t = Yap_MkApplTerm(FunctorException, 1, &e);
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
  if (!Yap_dispatch_interrupts( PASS_REGS1 ))
    return false;



  while (B && (
	       B->cp_ap->opc == FAIL_OPCODE ||
	       B->cp_ap == TRUSTFAILCODE ||
	       B->cp_ap == NOCODE
	       ))
    B = B->cp_b;
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
