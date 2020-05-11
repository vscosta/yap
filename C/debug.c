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

#include "Yap.h"
#include "absmi.h"
#include "attvar.h"
#include "cut_c.h"
#include "yapio.h"

static Int get_debugger_state(USES_REGS1) {
  const char *s = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  if (!strcmp(s, "creep")) {
    return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP]);
  }
  if (!strcmp(s, "goal_number")) {
    return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_GOAL_NUMBER]);
  }
  if (!strcmp(s, "spy")) {
    return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_SPY]);
  }
  if (!strcmp(s, "trace")) {
    return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_TRACE]);
  }
  if (!strcmp(s, "debug")) {
    return Yap_unify(ARG2, LOCAL_debugger_state[DEBUG_DEBUG]);
  }
  return false;
}

static Int set_debugger_state(USES_REGS1) {
  const char *s = RepAtom(AtomOfTerm(Deref(ARG1)))->StrOfAE;
  if (!strcmp(s, "creep")) {
    LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = Deref(ARG2);
    return true;
  }
  if (!strcmp(s, "goal_number")) {
    LOCAL_debugger_state[DEBUG_GOAL_NUMBER] = Deref(ARG2);
    return true;
  }
  if (!strcmp(s, "spy")) {
    LOCAL_debugger_state[DEBUG_SPY] = Deref(ARG2);
    return true;
  }
  if (!strcmp(s, "trace")) {
    LOCAL_debugger_state[DEBUG_TRACE] = Deref(ARG2);
    return true;
  }
  if (!strcmp(s, "debug")) {
    LOCAL_debugger_state[DEBUG_DEBUG] = Deref(ARG2);
    return true;
  }
  return false;
}

static void init_debugger_state(void) {

  LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = TermCreep;
  LOCAL_debugger_state[DEBUG_GOAL_NUMBER] = MkIntTerm(0);
  LOCAL_debugger_state[DEBUG_SPY] = TermOff;
  LOCAL_debugger_state[DEBUG_TRACE] = TermFalse;
  LOCAL_debugger_state[DEBUG_DEBUG] = TermFalse;
}

static Int set_debugger_state5(USES_REGS1) {
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {

    LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP] = t1;
  }
  t1 = Deref(ARG2);
  if (!IsVarTerm(t1)) {
    LOCAL_debugger_state[DEBUG_GOAL_NUMBER] = t1;
  }
  t1 = Deref(ARG3);
  if (!IsVarTerm(t1)) {
    LOCAL_debugger_state[DEBUG_SPY] = t1;
  }
  t1 = Deref(ARG4);
  if (!IsVarTerm(t1)) {
    LOCAL_debugger_state[DEBUG_TRACE] = t1;
  }
  t1 = Deref(ARG5);
  if (!IsVarTerm(t1)) {
    LOCAL_debugger_state[DEBUG_DEBUG] = t1;
  }
  return true;
}

static Int get_debugger_state5(USES_REGS1) {
  Term t1 = Deref(ARG1);
  if (!Yap_unify(LOCAL_debugger_state[DEBUG_CREEP_LEAP_OR_ZIP], t1))
    return false;
  t1 = Deref(ARG2);
  if (!Yap_unify(LOCAL_debugger_state[DEBUG_GOAL_NUMBER], t1))
    return false;
  t1 = Deref(ARG3);
  if (!Yap_unify(LOCAL_debugger_state[DEBUG_SPY], t1))
    return false;
  t1 = Deref(ARG4);
  if (!Yap_unify(LOCAL_debugger_state[DEBUG_TRACE], t1))
    return false;
  t1 = Deref(ARG5);
  if (!Yap_unify(LOCAL_debugger_state[DEBUG_DEBUG], t1))
    return false;
  return true;
}

void Yap_InitDebugFs(void) {
  CACHE_REGS

    init_debugger_state();
     Yap_InitCPred("$get_debugger_state", 2, get_debugger_state, NoTracePredFlag);
  Yap_InitCPred("$get_debugger_state", 5, get_debugger_state5, NoTracePredFlag);
  Yap_InitCPred("$set_debugger_state", 2, set_debugger_state, NoTracePredFlag);
  Yap_InitCPred("$set_debugger_state", 5, set_debugger_state5, NoTracePredFlag);
}
