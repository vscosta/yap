/*************************************************************************
 *									 *
 *	 Yap Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		attvar.c * Last rev:
 ** mods: * comments:	YAP support for attributed vars *
 *									 *
 *************************************************************************/

#
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

#include "Yap.h"

#include "YapHeap.h"
#include "Yatom.h"
#include "attvar.h"
#include "heapgc.h"

#include "YapSignals.h"

/**
    @defgroup AttributedVariables_Builtins Low-level support for Attributed
   Variables

     @brief Implementation of Attribute Declarations
    @ingroup AttributedVariables
    @{
*/

void Yap_wake_goal(Term tg USES_REGS) {
  /* follow the chain */
  Term WGs = Deref(Yap_ReadTimedVar(LOCAL_WokenGoals));
  Term *n, t = AbsPair((n=HR));
  HR+=2;
  n[0] = tg;
  n[1] = WGs;
  Yap_UpdateTimedVar(LOCAL_WokenGoals, t);
  Yap_signal(YAP_WAKEUP_SIGNAL);
}

void AddToQueue(attvar_record *attv USES_REGS) {
  Term t[2];
  Term ng;

  t[0] = (CELL) & (attv->Done);
  t[1] = attv->Future;
  t[1] = Yap_MkApplTerm(FunctorAttGoal, 2, t);
  t[0] = TermAttributes;
  ng = Yap_MkApplTerm(FunctorModule, 2, t);
  Yap_wake_goal(ng PASS_REGS);
}
void AddCompareToQueue(Term Cmp, Term t1, Term t2 USES_REGS) {
  Term ts[3];
  ts[0] = Cmp;
  ts[1] = MkGlobal(t1);
  ts[2] = MkGlobal(t2);
  Term tg = Yap_MkApplTerm(FunctorCompare, 3, ts);
  Yap_wake_goal(tg PASS_REGS);
}

void AddUnifToQueue(Term t1, Term t2 USES_REGS) {
  Term ts[2];
  ts[0] = MkGlobal(t1);
  ts[1] = MkGlobal(t2);
  Term tg = Yap_MkApplTerm(FunctorEq, 2, ts);
  Yap_wake_goal(tg PASS_REGS);
}

static attvar_record *BuildNewAttVar(USES_REGS1) {
  attvar_record *newv;

  /* add a new attributed variable */
  newv = (attvar_record *)HR;
  HR = (CELL *)(newv + 1);
  newv->AttFunc = FunctorAttVar;
  RESET_VARIABLE(&(newv->Future));
  RESET_VARIABLE(&(newv->Done));
  RESET_VARIABLE(&(newv->Atts));
  return newv;
}

static attvar_record *SetNewAttVar(Term t USES_REGS) {
  attvar_record *newv;

  /* add a new attributed variable */
  newv = (attvar_record *)HR;
  HR = (CELL *)(newv + 1);
  newv->AttFunc = FunctorAttVar;
  RESET_VARIABLE(&(newv->Future));
  RESET_VARIABLE(&(newv->Done));
  newv->Atts = t;
  return newv;
}

typedef struct cp_frame {
  CELL *start_cp;
  CELL *end_cp;
  CELL *to;
#ifdef RATIONAL_TREES
  CELL oldv;
  int ground;
#endif
} copy_frame;

static int CopyAttVar(CELL *orig, void *tvp, CELL *res USES_REGS) {
  struct cp_frame **to_visit_ptr = tvp;
  register attvar_record *attv = RepAttVar(orig);
  register attvar_record *newv;
  struct cp_frame *to_visit = *to_visit_ptr;
  CELL *vt;

  if (!(newv = BuildNewAttVar(PASS_REGS1)))
    return FALSE;
  vt = &(attv->Atts);
  to_visit->start_cp = vt - 1;
  to_visit->end_cp = vt;
  if (IsVarTerm(attv->Atts)) {
    Bind_Global_NonAtt(&newv->Atts, (CELL)HR);
    to_visit->to = HR;
    HR++;
  } else {
    to_visit->to = &(newv->Atts);
  }
  to_visit->oldv = vt[-1];
  to_visit->ground = FALSE;
  *to_visit_ptr = to_visit + 1;
  *res = (CELL) & (newv->Done);
  return TRUE;
}

static Term AttVarToTerm(CELL *orig) {
  attvar_record *attv = RepAttVar(orig);

  return attv->Atts;
}

static int IsEmptyWakeUp(Term atts) {
  if (IsVarTerm(atts))
    return false;
  Atom name = NameOfFunctor(FunctorOfTerm(atts));
  Atom *pt = EmptyWakeups;
  int i = 0;
  while (i < MaxEmptyWakeups) {
    if (pt[i++] == name)
      return TRUE;
  }
  return FALSE;
}

void Yap_MkEmptyWakeUp(Atom mod) {
  if (MaxEmptyWakeups == MAX_EMPTY_WAKEUPS)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermTrue,
              "too many modules that do not wake up");
  EmptyWakeups[MaxEmptyWakeups++] = mod;
}

static int TermToAttVar(Term attvar, Term to USES_REGS) {
  attvar_record *attv = BuildNewAttVar(PASS_REGS1);
  if (!attv)
    return FALSE;
  Bind_Global_NonAtt(&attv->Atts, attvar);
  *VarOfTerm(to) = AbsAttVar(attv);
  return TRUE;
}

static void WakeAttVar(CELL *pt1, CELL reg2 USES_REGS) {
  
  RESET_VARIABLE(pt1);
  // get record
  attvar_record *attv = RepAttVar(pt1);
  reg2 = MkGlobal(Deref(reg2));

  Term td = Deref(attv->Done);
  if (IsEmptyWakeUp(attv->Atts)) {
    /* no attributes to wake */
    Bind_Global_NonAtt(&(attv->Done), reg2);
    return;
  }
  // next case is impossible>
  if (!IsUnboundVar(&attv->Future)) {
    if (td != reg2) {
      AddUnifToQueue(td, reg2 PASS_REGS);
    }
    return;
  }
  if (!IsVarTerm(reg2)) {
    if (IsVarTerm(attv->Future)) {
      Bind_Global_NonAtt(&(attv->Future), reg2);
      AddToQueue(attv PASS_REGS);
    } else {
      AddUnifToQueue((Term)pt1, reg2 PASS_REGS);
    }
    return;
  }
  CELL *pt2 = VarOfTerm(reg2);
  if (pt1 == pt2 || attv->Future == reg2) {
    LOCAL_DoNotWakeUp = false;
    return;
  }
  if (!IsAttVar(pt2)) {
    Bind_Global_NonAtt(pt2, attv->Done);
    LOCAL_DoNotWakeUp = false;
    return;
  }
  attvar_record *susp2 = RepAttVar(pt2);
  Term td2 = Deref(susp2->Done);
  if (td2 == td || td2 == attv->Future) {
    return;
  }
  if (IsEmptyWakeUp(susp2->Atts)) {
    /* no attributes to wake */
    Bind_Global_NonAtt(pt2, attv->Done);
    LOCAL_DoNotWakeUp = false;
    return;
  }
  reg2 = Deref(susp2->Future);
  if (!IsVarTerm(reg2)) {
    Bind_Global_NonAtt(pt1, reg2);
    AddToQueue(attv PASS_REGS);
  } else {
  if (attv > susp2) {
    Bind_Global_NonAtt(&attv->Future, susp2->Done);
    AddToQueue(attv PASS_REGS);
  } else {
    Bind_Global_NonAtt(&susp2->Future, attv->Done);
    AddToQueue(susp2 PASS_REGS);

  }
  }

}

void Yap_WakeUp(CELL *pt0) {
  CACHE_REGS
    if (LOCAL_DoNotWakeUp)
      return;
  CELL d0 = *pt0;
  RESET_VARIABLE(pt0);
#if DEBUG
  CELL *pt1 = S;
  if (pt1>=HB && pt1 <HR)
    while (pt1<HR) {
      RESET_VARIABLE(pt1);
      pt1++;
    }
#endif
  WakeAttVar(pt0, d0 PASS_REGS);
}

static void mark_attvar(CELL *orig) { return; }

static Term BuildAttTerm(Functor mfun, UInt ar USES_REGS) {
  CELL *h0 = HR;
  UInt i;

  if (HR + (1024 + ar) > ASP) {
    LOCAL_Error_Size = ar * sizeof(CELL);
    return 0L;
  }
  HR += 1+ar;
  h0[0] = (CELL)mfun;
  RESET_VARIABLE(h0 + 1);
  h0 += 2;
  for (i = 1; i < ar; i++,h0++) {
    *h0 = TermVoidAtt;
  }
  return AbsAppl(HR-(ar+1));
}

// SICStus Style
static Term SearchAttsForModule(Term start, Functor mfun) {
  do {
    if (IsVarTerm(start))
      return start;
    if (!IsApplTerm(start))
      return TermNil;
    if ( FunctorOfTerm(start) == mfun) {
      return start;
    }
    Term a1 = ArgOfTerm(1, start);
    if (FunctorOfTerm(start) == FunctorAtt1) {
      if (IsAtomTerm(a1) && NameOfFunctor(mfun) == AtomOfTerm(a1))
        return TermNil;
      start = ArgOfTerm(3, start);
    } else {
      start = a1;
    }
  } while (TRUE);
}

static Term SearchAttsForModuleFunctorName(Term start, Atom mname) {
  do {
    Atom at;
    if (IsVarTerm(start))
      return start;
    if (!IsApplTerm(start))
      return TermNil;
    if ((at = NameOfFunctor(FunctorOfTerm(start))) == mname) {
      return start;
    }
    Term a1 = ArgOfTerm(1, start);
    if (FunctorOfTerm(start) == FunctorAtt1) {
      if (IsAtomTerm(a1) && at == AtomOfTerm(a1))
        return TermNil;
      start = ArgOfTerm(3, start);
    } else {
      start = a1;
    }
  } while (TRUE);
}

// SWI style
static Term SearchAttsForModuleName(Term start, Atom mname) {
  do {
    if (IsVarTerm(start))
      return 0;
    if (!IsApplTerm(start))
      return 0;
    if (NameOfFunctor(FunctorOfTerm(start)) == mname) {
      return start;
    }
    Term a1 = ArgOfTerm(1, start);
    if (FunctorOfTerm(start) == FunctorAtt1) {
      if (IsAtomTerm(a1) && mname == AtomOfTerm(a1))
        return ArgOfTerm(2, start);
      start = ArgOfTerm(3, start);
    } else {
      start = a1;
    }
  } while (TRUE);
}

/// SICStus add new set of constraints
static void AddNewModule(attvar_record *attv, Term t, int new,
                         int do_it USES_REGS) {
  CELL *newp = RepAppl(t) + 2;
  UInt i, ar = ArityOfFunctor((Functor)newp[-2]);

  for (i = 1; i < ar; i++) {
    Term n = Deref(*newp);
    if (n == TermFreeTerm) {
      *newp = TermVoidAtt;
    } else {
      if (n != TermVoidAtt)
        do_it = TRUE;
    }
    newp++;
  }
  if (!do_it)
    return;
  if (new) {
    attv->Atts = t;
  } else if (IsVarTerm(attv->Atts)) {
    MaBind(&(attv->Atts), t);
  } else {
    Term *wherep = &attv->Atts;

    do {
      if (IsVarTerm(*wherep)) {
        Bind_Global_NonAtt(wherep, t);
        return;
      } else {
        wherep = RepAppl(Deref(*wherep)) + 1;
      }
    } while (TRUE);
  }
}

#if 0
static void ReplaceAtts(attvar_record *attv, Term oatt, Term att USES_REGS) {
  UInt ar = ArityOfFunctor(FunctorOfTerm(oatt)), i;
  CELL *oldp = RepAppl(oatt) + 1;
  CELL *newp;

  if (oldp > HB) {
    oldp++;
    newp = RepAppl(att) + 2;
    /* if deterministic */

    for (i = 1; i < ar; i++) {
      Term n = Deref(*newp);
      if (n != TermFreeTerm) {
        *oldp = n;
      }
      oldp++;
      newp++;
    }
    return;
  }
  newp = RepAppl(att) + 1;
  *newp++ = *oldp++;
  for (i = 1; i < ar; i++) {
    Term n = Deref(*newp);

    if (n == TermFreeTerm) {
      *newp = Deref(*oldp);
    }
    oldp++;
    newp++;
  }
  if (attv->Atts == oatt) {
    if (RepAppl(attv->Atts) >= HB)
      attv->Atts = att;
    else
      MaBind(&(attv->Atts), att);
  } else {
    Term *wherep = &attv->Atts;

    do {
      if (*wherep == oatt) {
        MaBind(wherep, att);
        return;
      } else {
        wherep = RepAppl(Deref(*wherep)) + 1;
      }
    } while (TRUE);
  }
}
#endif

static void DelAllAtts(attvar_record *attv USES_REGS) {
  MaBind(&(attv->Done), attv->Future);
}

static void DelAtts(attvar_record *attv, Term oatt USES_REGS) {
  Term t = ArgOfTerm(1, oatt);
  if (attv->Atts == oatt) {
    if (IsVarTerm(t)) {
      DelAllAtts(attv PASS_REGS);
      return;
    }
    if (RepAppl(attv->Atts) >= HB)
      attv->Atts = t;
    else
      MaBind(&(attv->Atts), t);
  } else {
    Term *wherep = &attv->Atts;

    do {
      if (*wherep == oatt) {
        MaBind(wherep, t);
        return;
      } else {
        wherep = RepAppl(Deref(*wherep)) + 1;
      }
    } while (TRUE);
  }
}

static void PutAtt(Int pos, Term atts, Term att USES_REGS) {
  if (IsVarTerm(att) && VarOfTerm(att) > HR && VarOfTerm(att) < LCL0) {
    /* globalise locals */
    Term tnew = MkVarTerm();
    Bind_NonAtt(VarOfTerm(att), tnew);
    att = tnew;
  }
  MaBind(RepAppl(atts) + pos, att);
}

static Int BindAttVar(attvar_record *attv USES_REGS) {
  Term done = Deref(attv->Done);
  Term value = Deref(attv->Future);
  if (value != done)
    Bind_Global_NonAtt(&(attv->Done), value);
  return true;
  if (IsVarTerm(done) && IsUnboundVar(&attv->Done)) {
    /* make sure we are not trying to bind a variable against itself */
    if (!IsVarTerm(value)) {
      Bind_Global_NonAtt(&(attv->Done), value);
    } else if (IsVarTerm(value)) {
      Term t = value;
      if (IsVarTerm(t)) {
        if (IsAttachedTerm(t)) {
          attvar_record *attv2 = RepAttVar(VarOfTerm(t));
          if (attv2 < attv) {
            Bind_Global_NonAtt(&(attv2->Done), AbsAttVar(attv));
          } else if (attv2 != attv) {
            Bind_Global_NonAtt(&(attv->Done), AbsAttVar(attv2));
          }
        } else {
          Yap_Error(SYSTEM_ERROR_INTERNAL, value,
                    "attvar was bound when unset");
          return false;
        }
      } else {
        Bind_Global_NonAtt(&(attv->Done), t);
      }
    }
    return true;
  } else {
    Yap_Error(SYSTEM_ERROR_INTERNAL, (CELL) & (attv->Done),
              "attvar was bound when set");
    return (FALSE);
  }
}

static Int UnBindAttVar(attvar_record *attv) {
  RESET_VARIABLE(&(attv->Done));
  return (TRUE);
}

static Term GetAllAtts(attvar_record *attv) {
  /* check if we are already there */
  return attv->Atts;
}

/// @}

/// @defgroup SICStusCConst SICStus-style Constraints in C
/// @ingroup AttributedVariables_Builtins
/// @{

/// put a SICStus style attribute
static Int put_att(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;
    Atom modname = AtomOfTerm(Deref(ARG2));
    UInt ar = IntegerOfTerm(Deref(ARG3));
    Functor mfun;
    Term tatts;
    int new = FALSE;

    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
    } else {
      while (!(attv = BuildNewAttVar(PASS_REGS1))) {
        LOCAL_Error_Size = sizeof(attvar_record);
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      new = TRUE;
    }
    mfun = Yap_MkFunctor(modname, ar);
    if ((tatts = SearchAttsForModule(attv->Atts, mfun)) == 0) {
      while (!(tatts = BuildAttTerm(mfun, ar PASS_REGS))) {
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      {
        CELL *ptr = VarOfTerm(Deref(ARG1));
        CELL d0 = AbsAttVar(attv);
        Bind_NonAtt(ptr, d0);
      }
      AddNewModule(attv, tatts, new, TRUE PASS_REGS);
    }
    PutAtt(IntegerOfTerm(Deref(ARG4)), tatts, Deref(ARG5) PASS_REGS);
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of put_attributes/2");
    return FALSE;
  }
}

static Int rm_att(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;
    Atom modname = AtomOfTerm(Deref(ARG2));
    UInt ar = IntegerOfTerm(Deref(ARG3));
    Functor mfun;
    Term tatts;
    int new = FALSE;

    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
    } else {
      while (!(attv = BuildNewAttVar(PASS_REGS1))) {
        LOCAL_Error_Size = sizeof(attvar_record);
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      new = TRUE;
      Yap_unify(ARG1, AbsAttVar(attv));
    }
    mfun = Yap_MkFunctor(modname, ar);
    if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts, mfun))) {
      while (!(tatts = BuildAttTerm(mfun, ar PASS_REGS))) {
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      AddNewModule(attv, tatts, new, FALSE PASS_REGS);
    } else {
      PutAtt(IntegerOfTerm(Deref(ARG4)), tatts, TermVoidAtt PASS_REGS);
    }
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp, "first argument of rm_att/2");
    return (FALSE);
  }
}

/* h-Prolog, SWI code */
static Int put_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);

  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttVar(VarOfTerm(inp))) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      MaBind(&(attv->Atts),Deref(ARG2));
      return true;
    } else {
    /*   attvar_record *attv0 = NULL;
    Term t = Deref(ARG1);
    Term tatts = Deref(ARG2);
    if (IsAttVar(VarOfTerm(t))) {
      attv0 = RepAttVar(VarOfTerm(t));
      MaBind(&(attv0->Atts),tatts);
      return true;
      } */
      attvar_record *attv;     
      while (!(attv = SetNewAttVar(Deref(ARG2) PASS_REGS))) {
	LOCAL_Error_Size = sizeof(attvar_record);
	if (!Yap_dogc(PASS_REGS1)) {
	  Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}
      }
    return Yap_unify(ARG1, AbsAttVar(attv));
  }
  }else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of put_att/2");
    return FALSE;
  }
}

static Int del_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  Term otatts;

  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;
    Term tatts = Deref(ARG2);
    Functor mfun = FunctorOfTerm(tatts);

    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
    } else {
      return TRUE;
    }
    if (IsVarTerm(otatts = SearchAttsForModule(attv->Atts, mfun))) {
      return TRUE;
    } else {
      DelAtts(attv, otatts PASS_REGS);
    }
    return TRUE;
  } else {
    return TRUE;
  }
}

static Int del_all_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);

  /* if this is unbound, ok */
  if (IsVarTerm(inp) && IsAttachedTerm(inp)) {
    attvar_record *attv;

    attv = RepAttVar(VarOfTerm(inp));
    DelAllAtts(attv PASS_REGS);
  }
  return TRUE;
}

static Int get_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tatts;
      Term access = Deref(ARG2);
      Functor mfun = FunctorOfTerm(access);
      UInt ar, i;
      CELL *old, *new;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts, mfun)))
        return FALSE;

      ar = ArityOfFunctor(mfun);
      new = RepAppl(access) + 2;
      old = RepAppl(tatts) + 2;
      for (i = 1; i < ar; i++, new ++, old++) {
        if (*new != TermFreeTerm) {
          if (*old == TermVoidAtt && *new != TermVoidAtt)
            return FALSE;
          if (*new == TermVoidAtt &&*old != TermVoidAtt)
            return FALSE;
          if (!Yap_unify(*new, *old))
            return FALSE;
        }
      }
      return TRUE;
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    return (FALSE);
  }
}

static Int free_att(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    Atom modname = AtomOfTerm(Deref(ARG2));

    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tout, tatts;

      attv = RepAttVar(VarOfTerm(inp));
      if ((tatts = SearchAttsForModuleFunctorName(attv->Atts, modname)) == 0)
        return TRUE;
      tout = ArgOfTerm(IntegerOfTerm(Deref(ARG3)), tatts);
      return (tout == TermVoidAtt);
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return TRUE;
    }
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of free_att/2");
    return (FALSE);
  }
}

//
// SICStus style attribute search
//

static Int get_att(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tatts;
      Term access = Deref(ARG2);
      Functor mfun = FunctorOfTerm(access);
      UInt ar, i;
      CELL *old, *new;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts, mfun)))
        return FALSE;

      ar = ArityOfFunctor(mfun);
      new = RepAppl(access) + 2;
      old = RepAppl(tatts) + 2;
      for (i = 1; i < ar; i++, new ++, old++) {
        if (*new != TermFreeTerm) {
          if (*old == TermVoidAtt && *new != TermVoidAtt)
            return FALSE;
          if (*new == TermVoidAtt &&*old != TermVoidAtt)
            return FALSE;
          if (!Yap_unify(*new, *old))
            return FALSE;
        }
      }
      return TRUE;
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    return (FALSE);
  }
}

static Int has_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term access = Deref(ARG2);
      Functor mfun = FunctorOfTerm(access);

      attv = RepAttVar(VarOfTerm(inp));
      return SearchAttsForModule(attv->Atts, mfun) != 0;
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of has_atts/2");
    return (FALSE);
  }
}

static Int bind_attvar(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return (BindAttVar(attv PASS_REGS));
    }
    return (true);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of bind_attvar/2");
    return (false);
  }
}

static Int set_attvar(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      Bind_Global_NonAtt(VarOfTerm(inp), MkGlobal(Deref(ARG2)));
    return (true);
    }
  }
  
  Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
	    "first argument of bind_attvar/2");
  return (false);
}

static Int wake_up_done(USES_REGS1) {
  LOCAL_DoNotWakeUp = false;
  if (Yap_ReadTimedVar(LOCAL_WokenGoals) != TermTrue)
    Yap_signal(YAP_WAKEUP_SIGNAL);
  return true;
}

static Int wake_up_start(USES_REGS1) {
     LOCAL_DoNotWakeUp = true;
  return true;
}
static Int unbind_attvar(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return (UnBindAttVar(attv));
    }
    return (TRUE);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of bind_attvar/2");
    return (FALSE);
  }
}

static Int get_all_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return Yap_unify(ARG2, GetAllAtts(attv));
    }
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of get_all_atts/2");
    return FALSE;
  }
}

static int ActiveAtt(Term tatt, UInt ar) {
  CELL *cp = RepAppl(tatt) + 1;
  UInt i;

  for (i = 1; i < ar; i++) {
    if (cp[i] != TermVoidAtt)
      return TRUE;
  }
  return FALSE;
}

static Int modules_with_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      CELL *h0 = HR;
      Term tatt;

      if (IsVarTerm(tatt = attv->Atts))
        return Yap_unify(ARG2, TermNil);
      while (!IsVarTerm(tatt)) {
        Functor f = FunctorOfTerm(tatt);
        if (HR != h0)
          HR[-1] = AbsPair(HR);
        if (ActiveAtt(tatt, ArityOfFunctor(f))) {
          *HR = MkAtomTerm(NameOfFunctor(f));
          HR += 2;
        }
        tatt = ArgOfTerm(1, tatt);
      }
      if (h0 != HR) {
        HR[-1] = TermNil;
        return Yap_unify(ARG2, AbsPair(h0));
      }
    }
    return Yap_unify(ARG2, TermNil);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of modules_with_attributes/2");
    return FALSE;
  }
}

/// @}

/// @defgroup SWICConst SWI-style Constraints in C
/// @ingroup AttributedVariables_Builtins
/// @{

/** @pred get_attr( + Var,+ Module,- Value)

Request the current  _value_ for the attribute named  _Module_.  If
 _Var_ is not an attributed variable or the named attribute is not
associated to  _Var_ this predicate fails silently.  If  _Module_
is not an atom, a type error is raised.


*/
static Int get_attr(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  Atom modname = AtomOfTerm(must_be_module(ARG2));
  if (!IsVarTerm(inp))
    return false;

  if (IsAttachedTerm(inp)) {
    attvar_record *attv;
    Term tatts;

    attv = RepAttVar(VarOfTerm(inp));
    if ((tatts = SearchAttsForModuleName(attv->Atts, modname)) == 0)
      return false;
    return Yap_unify(tatts, ARG3);
  } else {
    return false;
  }
}

/**
 @pred get_attrs(+ _Var_,- _Attributes_)

Get all attributes of  _Var_.  _Attributes_ is a term of the form
`att( _Module_,  _Value_,  _MoreAttributes_)`, where  _MoreAttributes_ is
`[]` for the last attribute.

*/
static Int get_attrs(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (!IsVarTerm(inp) || !IsAttachedTerm(inp)) {
    return false;
  }
  attvar_record *attv;
  attv = RepAttVar(VarOfTerm(inp));
  return Yap_unify(ARG2, attv->Atts);
  return TRUE;
}

/**
 @pred put_attr(+ _Var_,+ _Module_,+ _Value_)

If  _Var_ is a variable or attributed variable, set the value for the
attribute named  _Module_ to  _Value_. If an attribute with this
name is already associated with  _Var_, the old value is replaced.
Backtracking will restore the old value (i.e., an attribute is a samutable
term. See also `setarg/3`). This predicate raises a representation error if
 _Var_ is not a variable and a type error if  _Module_ is not an atom.
1

*/
static Int put_attr(USES_REGS1) {
  /* receive a variable in ARG1 */
  attvar_record *attv;
  Term inp = must_be_unbound(ARG1);
  Term ts[3];
  ts[0] = must_be_module(ARG2);

  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {

    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
      ts[2] = attv->Atts;
      MaBind(&attv->Atts,  Yap_MkApplTerm(FunctorAtt1, 3, ts));
      Term start = attv->Atts;
  do {
    if (IsVarTerm(start))
      break;
    if (!IsApplTerm(start))
      break;
    if (FunctorOfTerm(start) != FunctorAtt1) {
      start = ArgOfTerm(1, start);
      continue;
    }
    if (ts[0] != ArgOfTerm(1, start)) {
      start = ArgOfTerm(3, start);
      continue;
    }
    // got it
    MaBind(RepAppl(start)+2, Deref(ARG3));
    return true;
  } while (TRUE);
  ts[1] = MkGlobal(ARG3);
    if (IsVarTerm(attv->Atts))
      ts[2] = TermNil;
    else
      ts[2] = attv->Atts;
   

    MaBind(&attv->Atts, Yap_MkApplTerm(FunctorAtt1,3,ts))
  } else {
    
      while (!(attv = BuildNewAttVar(PASS_REGS1))) {
        LOCAL_Error_Size = sizeof(attvar_record);
        if (!Yap_dogc(PASS_REGS1)) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
	S = HR;
	HR+=4;
	*S++ = (CELL)FunctorAtt1;
	*S++ = Deref(ARG2);
      *S++= MkGlobal(ARG3);
      *S++ = TermNil;
      attv->Atts = AbsAppl(S-4);
        inp = Deref(ARG1);
	MaBind(VarOfTerm(inp),attv->Done);

	
    }
    
 } 
    return true;
}

/** @pred put_attrs(+ _Var_,+ _Attributes_)


Set all attributes of  _Var_. _Attributes_ is a term of the form
`att( _Module_,  _Value_,  _MoreAttributes_)`, where  _MoreAttributes_ is
`[]` for the last attribute.

*/
static Int put_attrs(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (!IsVarTerm(inp) || IsAttachedTerm(inp)) {
    return false;
  }
  attvar_record *attv;
  attv = RepAttVar(VarOfTerm(inp));
  MaBind(&attv->Atts, Deref(ARG2));
  return TRUE;
}

/** @pred del_attr(+ _Var_,+ _Module_)


Delete the named attribute.  If  _Var_ loses its last attribute it
is transformed back into a traditional Prolog variable.  If  _Module_
is not an atom, a type error is raised. In all other cases this
predicate succeeds regardless whether or not the named attribute is
present.


*/
static Int del_attr(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  Term mod = must_be_module(ARG2);
  /* if this is unbound, ok */
  if (!IsVarTerm(inp) || IsAttachedTerm(inp)) {
    return true;
  }
  attvar_record *attv = RepAttVar(VarOfTerm(inp));
  Term start = attv->Atts, *outside = &attv->Atts;
  do {
    if (IsVarTerm(start))
      return true;
    if (!IsApplTerm(start))
      return true;
    if (FunctorOfTerm(start) != FunctorAtt1) {
      outside = RepAppl(start) + 1;
      start = ArgOfTerm(1, start);
      continue;
    }
    if (mod != ArgOfTerm(1, start)) {
      outside = RepAppl(start) + 3;
      start = ArgOfTerm(3, start);
      continue;
    }
    // got it
    Term next = ArgOfTerm(3, start);
    if (outside == &attv->Atts && next == TermNil) {
      DelAllAtts(attv PASS_REGS);
    } else {
      MaBind(outside, next);
    }
    return true;
  } while (TRUE);
  return TRUE;
}

/** @pred del_attrs(+ _Var_)


If  _Var_ is an attributed variable, delete <em>all</em> its
attributes.  In all other cases, this predicate succeeds without
side-effects.


*/
static Int del_attrs(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (!IsVarTerm(inp) || !IsAttachedTerm(inp)) {
    return false;
  }
  attvar_record *attv;
  attv = RepAttVar(VarOfTerm(inp));
  DelAllAtts(attv PASS_REGS);
  return TRUE;
}

/// just get everything
static Int swi_all_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return Yap_unify(ARG2, attv->Atts);
    }
    return Yap_unify(ARG2, TermNil);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of get_all_swi_atts/2");
    return FALSE;
  }
}

static Term AllAttVars(USES_REGS1) {
  CELL *pt = H0;
  CELL *myH = HR;

  while (pt < myH) {
    Term reg = *pt;
    Functor f = (Functor)reg;
    if (reg == (CELL)FunctorAttVar) {
      if (IsUnboundVar(pt + 1)) {
        if (ASP - myH < 1024) {
          LOCAL_Error_Size = (ASP - HR) * sizeof(CELL);
          return 0L;
        }
        if (myH != HR) {
          myH[-1] = AbsPair(myH);
        }
        myH[0] = AbsAttVar((attvar_record *)pt);
        myH += 2;
      }
      pt += (1 + ATT_RECORD_ARITY);
    } else if (IsExtensionFunctor(f) && reg > 0 && reg % sizeof(CELL) == 0) {
      ssize_t bigsz = SizeOfOpaqueTerm(pt, reg);
      if (bigsz <= 0 || pt + bigsz > HR || !IsAtomTerm(pt[bigsz - 1])) {
        *pt++ = reg;
        continue;
      }
      CELL end = CloseExtension(pt);
      pt += bigsz - 1;
      *pt = end;
    } else {
      pt++;
    }
  }
  if (myH != HR) {
    Term out = AbsPair(HR);
    myH[-1] = TermNil;
    HR = myH;
    return out;
  } else {
    return TermNil;
  }
}

static Int all_attvars(USES_REGS1) {
  do {
    Term out;

    if (!(out = AllAttVars(PASS_REGS1))) {
      Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
      return FALSE;
    } else {
      return Yap_unify(ARG1, out);
    }
  } while (TRUE);
}

/** @pred attvar( _-Var_)


    Succeed if  _Var_ is an attributed variable.



*/
static Int is_attvar(USES_REGS1) {
  Term t = Deref(ARG1);
  return (IsVarTerm(t) && IsAttVar(VarOfTerm(t)));
}

/* check if we are not redoing effort */
static Int attvar_bound(USES_REGS1) {
  Term t = Deref(ARG1);
  return IsVarTerm(t) && IsAttachedTerm(t) &&
         !IsUnboundVar(&(RepAttVar(VarOfTerm(t))->Done));
}

static Int void_term(USES_REGS1) { return Yap_unify(ARG1, TermVoidAtt); }

static Int free_term(USES_REGS1) { return Yap_unify(ARG1, TermFreeTerm); }

static Int fast_unify(USES_REGS1) {
  /*
    Case we want to unify two variables, but we do not
    think there is a point in waking them up
  */
  Term t1, t2;
  CELL *a, *b;
  if (!IsVarTerm(t1 = Deref(ARG1)))
    return FALSE;
  if (!IsVarTerm(t2 = Deref(ARG2)))
    return FALSE;
  a = VarOfTerm(t1);
  b = VarOfTerm(t2);
  if (a > b) {
    Bind_Global_NonAtt(a, t2);
  } else if ((a) < (b)) {
    Bind_Global_NonAtt(b, t1);
  }
  return TRUE;
}

void Yap_InitAttVarPreds(void) {
  CACHE_REGS
  Yap_InitCPred("get_all_swi_atts", 2, swi_all_atts, SafePredFlag);
  Yap_InitCPred("put_attr", 3, put_attr, 0);
  Yap_InitCPred("put_attrs", 2, put_attrs, 0);
  Yap_InitCPred("get_attr", 3, get_attr, SafePredFlag);
  Yap_InitCPred("get_attrs", 2, get_attrs, SafePredFlag);
  Yap_InitCPred("del_attr", 2, del_attr, SafePredFlag);
  Yap_InitCPred("del_attrs", 1, del_attrs, SafePredFlag);
  Yap_InitCPred("get_att", 4, get_att, SafePredFlag);
  Yap_InitCPred("free_att", 3, free_att, SafePredFlag);
  Yap_InitCPred("put_att", 5, put_att, 0);
  Yap_InitCPred("rm_att", 4, rm_att, 0);
  GLOBAL_attas[attvars_ext].bind_op = WakeAttVar;
  GLOBAL_attas[attvars_ext].copy_term_op = CopyAttVar;
  GLOBAL_attas[attvars_ext].to_term_op = AttVarToTerm;
  GLOBAL_attas[attvars_ext].term_to_op = TermToAttVar;
  GLOBAL_attas[attvars_ext].mark_op = mark_attvar;
  Yap_InitCPredInModule("has_module_atts", 2, has_atts, SafePredFlag, ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("get_all_atts", 2, get_all_atts, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("get_module_atts", 2, get_atts, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("put_module_atts", 2, put_atts, 0 , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("del_all_module_atts", 2, del_atts, 0 , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("del_all_atts", 1, del_all_atts, 0 , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("set_attvar", 2, set_attvar, SafePredFlag , ATTRIBUTES_MODULE);
  // Yap_InitCPredInModule("bind_attvar", 1, bind_attvar, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPred("bind_attvar", 1, bind_attvar, SafePredFlag);
  Yap_InitCPredInModule("unbind_attvar", 1, unbind_attvar, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("modules_with_attributes", 2, modules_with_atts, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("void_term", 1, void_term, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("free_term", 1, free_term, SafePredFlag , ATTRIBUTES_MODULE);
  Yap_InitCPredInModule("fast_unify_attributed", 2, fast_unify, 0 , ATTRIBUTES_MODULE);
  Yap_InitCPred("all_attvars", 1, all_attvars, 0);
  Yap_InitCPred("attvar", 1, is_attvar, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$att_bound", 1, attvar_bound, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$wake_up_start", 0, wake_up_start, 0);
  Yap_InitCPred("$wake_up_done", 0, wake_up_done, 0);
}
/** @} */
