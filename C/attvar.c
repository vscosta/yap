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
 * File:		attvar.c * Last rev:
 ** mods: * comments:	YAP support for attributed vars *
 *									 *
 *************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/**
 * @file   attvar.c
 * @author VITOR SANTOS COSTA <vsc@VITORs-MBP-2.lan>
 * @date   Mon Apr 30 09:31:59 2018
 *
 * @brief  attributed variables
 * @namespace prolog
 *
 */
#include "Yap.h"

#include "YapHeap.h"
#include "Yatom.h"
#include "attvar.h"
#include "heapgc.h"
#ifndef NULL
#define NULL (void *)0
#endif

/**
    @defgroup AttributedVariables_Builtins Low-level support for Attributed
   Variables

     @brief Implementation of Attribute Declarations
    @ingroup AttributedVariables
    @{
*/

/**
    @defgroup WakeUp WakeUp for goalreactivation

     @brief Implementation of Attribute Declarations
    @ingroup AttributedVariables

    @{
*/

#ifdef COROUTINING

#define TermVoidAtt TermFoundVar

static CELL *AddToQueue(attvar_record *attv USES_REGS) {
  Term t[2];
  Term WGs, ng;

  t[0] = (CELL) & (attv->Done);
  t[1] = attv->Value;
  /* follow the chain */
  WGs = Yap_ReadTimedVar(LOCAL_WokenGoals);
  ng = Yap_MkApplTerm(FunctorAttGoal, 2, t);

  Yap_UpdateTimedVar(LOCAL_WokenGoals, MkPairTerm(ng, WGs));
  if ((Term)WGs == TermNil) {
    /* from now on, we have to start waking up goals */
    Yap_signal(YAP_WAKEUP_SIGNAL);
  }
  return (RepAppl(ng) + 2);
}

static void AddFailToQueue(USES_REGS1) {
  Term WGs;

  /* follow the chain */
  WGs = Yap_ReadTimedVar(LOCAL_WokenGoals);

  Yap_UpdateTimedVar(LOCAL_WokenGoals, MkPairTerm(TermFail, WGs));
  if ((Term)WGs == TermNil) {
    /* from now on, we have to start waking up goals */
    Yap_signal(YAP_WAKEUP_SIGNAL);
  }
}

/**
   @defgroup Attributed Variables core routines:

   - BuildNewAttvar: V -> ATTV(), SWI/SICStus
*/


static attvar_record *BuildNewAttVar(USES_REGS1) {
  attvar_record *newv;
  /* add a new attributed variable */
  newv = (attvar_record *)HR;
  HR = (CELL *)(newv + 1);
  newv->AttFunc = FunctorAttVar;
  RESET_VARIABLE(&(newv->Value));
  RESET_VARIABLE(&(newv->Done));
  RESET_VARIABLE(&(newv->Atts));
  return newv;
 }

static int CopyAttVar(CELL *orig, void *sttp,
                      CELL *res USES_REGS) {
  register attvar_record *attv = RepAttVar(orig);
  register attvar_record *newv;
  Ystack_t stt = *(Ystack_t *)sttp;
  CELL *vt;

  if (!(newv = BuildNewAttVar(PASS_REGS1)))
    return
      FALSE;
  vt = &(attv->Atts);
  to_visit->pt0 = vt - 1;
  to_visit->pt0_end = vt;
  if (IsVarTerm(attv->Atts)) {
    Bind_Global_NonAtt(&newv->Atts, (CELL)HR);
    to_visit->ptf = HR;
    HR++;
  } else {
    to_visit->ptf = &(newv->Atts);

                           }
  to_visit->oldp = vt-1;
  to_visit->ground = FALSE;
  to_visit++;
  *res = (CELL) & (newv->Done);
  return TRUE;
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
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "too many modules that do not wake up");
  EmptyWakeups[MaxEmptyWakeups++] = mod;
}

static Term AttVarToTerm(CELL *orig) {
  attvar_record *attv = RepAttVar(orig);

  return attv->Atts;
}


static attvar_record *AttsFromTerm(Term inp)
{
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv;

      attv = RepAttVar(VarOfTerm(inp));
      return attv;
    }
    return NULL;
  } else {
    Yap_ThrowError(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of get_all_swi_atts/2");
  }
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

  /* if bound to someone else, follow until we find the last one */
  attvar_record *attv = RepAttVar(pt1);
  CELL *myH = HR;
  CELL *bind_ptr;

  if (IsVarTerm(Deref(attv->Atts))) {
    /* no attributes to wake */
    return;
  }
  if (IsVarTerm(reg2)) {
    if (pt1 == VarOfTerm(reg2))
      return;
    if (IsAttachedTerm(reg2)) {
      attvar_record *susp2 = RepAttVar(VarOfTerm(reg2));

      /* binding two suspended variables, be careful */
      if (susp2 >= attv) {
        if (!IsVarTerm(susp2->Value) || !IsUnboundVar(&susp2->Value)) {
          /* oops, our goal is on the queue to be woken */
          if (!Yap_unify(susp2->Value, (CELL)pt1)) {
            AddFailToQueue(PASS_REGS1);
          }
        }
        Bind_Global_NonAtt(&(susp2->Value), (CELL)pt1);
        AddToQueue(susp2 PASS_REGS);
        return;
      }
    } else {
      Bind_NonAtt(VarOfTerm(reg2), (CELL)pt1);
      return;
    }
  }
  if (IsEmptyWakeUp(attv->Atts)) {
    Bind_Global_NonAtt(&(attv->Value), reg2);
    Bind_Global_NonAtt(&(attv->Done), attv->Value);
    return;
  }
  if (!IsVarTerm(attv->Value) || !IsUnboundVar(&attv->Value)) {
    /* oops, our goal is on the queue to be woken */
    if (!Yap_unify(attv->Value, reg2)) {
     AddFailToQueue(PASS_REGS1);
    }
    return;
  }
  bind_ptr = AddToQueue(attv PASS_REGS);
  if (IsNonVarTerm(reg2)) {
    if (IsPairTerm(reg2) && RepPair(reg2) == myH)
      reg2 = AbsPair(HR);
    else if (IsApplTerm(reg2) && RepAppl(reg2) == myH)
      reg2 = AbsAppl(HR);
  }
  *bind_ptr = reg2;
  Bind_Global_NonAtt(&(attv->Value), reg2);
}

void Yap_WakeUp(CELL *pt0) {
  CACHE_REGS
  CELL d0 = *pt0;
  RESET_VARIABLE(pt0);
  WakeAttVar(pt0, d0 PASS_REGS);
}

bool Yap_WakeUpUnsafe(CELL *pt0) {
  CACHE_REGS
  attvar_record *attv = RepAttVar(pt0);
  CELL d0 = *pt0;
  RESET_VARIABLE(pt0);
  if (IsNonVarTerm(attv->Value)) {
  Bind_NonAtt(pt0, (attv->Value));
    return false;
  }
  WakeAttVar(pt0, d0 PASS_REGS);
  return true;
}

static void mark_attvar(CELL *orig) { return; }

static Term BuildAttTerm(Functor mfun, UInt ar USES_REGS) {
  CELL *h0 = HR;
  UInt i;

  if (HR + (1024 + ar) > ASP) {
    LOCAL_Error_Size = ar * sizeof(CELL);
    return 0L;
  }
  HR[0] = (CELL)mfun;
  RESET_VARIABLE(HR + 1);
  HR += 2;
  for (i = 1; i < ar; i++) {
    *HR = TermVoidAtt;
    HR++;
  }
  return AbsAppl(h0);
}

/** Helper routines */
static Term SearchAttsForModule(Term start, Functor mfun) {
  do {
    if (IsVarTerm(start) || start == TermNil ||
	FunctorOfTerm(start) == mfun)
      return start;
    start = ArgOfTerm(3, start);
  } while (TRUE);
}

static Term SearchAttsForModuleName(Term start, Atom mname) {
  do {
    if (IsVarTerm(start) || NameOfFunctor(FunctorOfTerm(start)) == mname)
      return start;
    start = ArgOfTerm(1, start);
  } while (TRUE);
}



static Term SearchAttsForModuleAsNameTerm(Term start, Term mname, Term *p) {
  do {
    if (IsVarTerm(start) || ArgOfTerm(1,start) == mname ||
	NameOfFunctor(FunctorOfTerm(start)) == AtomOfTerm(mname))
      return start;
    else {
      *p = start;
      start = ArgOfTerm(3, start);
    }
  } while (TRUE);
}

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

static void DelAllAtts(attvar_record *attv USES_REGS) {
  MaBind(&(attv->Done), attv->Value);
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
  if (IsVarTerm(attv->Done) && IsUnboundVar(&attv->Done)) {
    /* make sure we are not trying to bind a variable against itself */
    if (!IsVarTerm(attv->Value)) {
      Bind_Global_NonAtt(&(attv->Done), attv->Value);
    } else if (IsVarTerm(attv->Value)) {
      Term t = Deref(attv->Value);
      if (IsVarTerm(t)) {
        if (IsAttachedTerm(t)) {
          attvar_record *attv2 = RepAttVar(VarOfTerm(t));
          if (attv2 < attv) {
            Bind_Global_NonAtt(&(attv->Done), t);
          } else {
            Bind_Global_NonAtt(&(attv2->Done), AbsAttVar(attv));
          }
        } else {
          Yap_Error(SYSTEM_ERROR_INTERNAL, (CELL) & (attv->Done),
                    "attvar was bound when unset");
          return (FALSE);
        }
      } else {
        Bind_Global_NonAtt(&(attv->Done), t);
      }
    }
    return (TRUE);
  } else {
    Yap_Error(SYSTEM_ERROR_INTERNAL, (CELL) & (attv->Done),
              "attvar was bound when set");
    return (FALSE);
  }
}

static Int UnBindAttVar(attvar_record *attv) {
  RESET_VARIABLE(&(attv->Value));
  return (TRUE);
}

static Term GetAllAtts(attvar_record *attv) {
  /* check if we are already there */
  return attv->Atts;
}

static Int get_att(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    Atom modname = AtomOfTerm(Deref(ARG2));

    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tout, tatts;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(tatts = SearchAttsForModuleName(attv->Atts, modname)))
        return FALSE;
      tout = ArgOfTerm(IntegerOfTerm(Deref(ARG3)), tatts);
      if (tout == TermVoidAtt)
        return FALSE;
      return Yap_unify(tout, ARG4);
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of get_att/2");
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
      Term tatts;
      Term access = Deref(ARG2);
      Functor mfun = FunctorOfTerm(access);

      attv = RepAttVar(VarOfTerm(inp));
      return !IsVarTerm(tatts = SearchAttsForModule(attv->Atts, mfun));
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

static Int swi_all_atts(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  Functor attf = FunctorAtt1;

  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      CELL *h0 = HR;
      Term tatt;

      if (IsVarTerm(tatt = attv->Atts))
        return Yap_unify(ARG2, TermNil);
      while (!IsVarTerm(tatt) && tatt != TermNil) {
        Functor f = FunctorOfTerm(tatt);
        UInt ar = ArityOfFunctor(f);

        if (HR != h0)
          HR[-1] = AbsAppl(HR);
        HR[0] = (CELL)attf;
        HR[1] = MkAtomTerm(NameOfFunctor(f));
        /* SWI */
        if (ar == 3)
          HR[2] = ArgOfTerm(2, tatt);
        else
          HR[2] = tatt;
        HR += 4;
        HR[-1] = AbsAppl(HR);
        tatt = ArgOfTerm(3, tatt);
      }
      if (h0 != HR) {
        HR[-1] = TermNil;
        return Yap_unify(ARG2, AbsAppl(h0));
      }
    }
    return Yap_unify(ARG2, TermNil);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of get_all_swi_atts/2");
    return FALSE;
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
        if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      new = TRUE;
    }
    mfun = Yap_MkFunctor(modname, ar);
    if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts, mfun))) {
      while (!(tatts = BuildAttTerm(mfun, ar PASS_REGS))) {
        if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P, CP))) {
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

/**
 @{
End of core routines
*/

/**
 @pred put_attr(+ _Var_,+ _Module_,+ _Value_)

If  _Var_ is a variable or attributed variable, set the value for the
attribute named  _Module_ to  _Value_. If an attribute with this
name is already associated with  _Var_, the old value is replaced.
Backtracking will restore the old value (i.e., an attribute is a mutable
term. See also `setarg/3`). This predicate raises a representation error if
 _Var_ is not a variable and a type error if  _Module_ is not an atom.


*/

static Int put_attr(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;
    Term modname = Deref(ARG2);
    Term tatts = Deref(ARG3);
    Functor f;
    bool new=false;

    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
    } else {
      while (!(attv = BuildNewAttVar(PASS_REGS1))) {
        LOCAL_Error_Size = sizeof(attvar_record);
        if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
        {
        CELL *ptr = VarOfTerm(Deref(ARG1));
        CELL d0 = AbsAttVar(attv);
        Bind_NonAtt(ptr, d0);
	new = true;
      }
        }
    f = FunctorAtt1;
    Term otatts;
    Term tgt = 0;
    if (IsVarTerm((otatts = SearchAttsForModuleAsNameTerm(attv->Atts, modname, &tgt)))||otatts == TermNil) {
     if (IsVarTerm(otatts)) {
      	Term ts[3];
	ts[0] = modname;
	ts[2] = MkVarTerm();
	  ts[1] = tatts;
	  Term tx = Yap_MkApplTerm(f, 3, ts);
      if (new) {
	attv->Atts = tx;
	return true;
      }
      return Yap_unify(otatts,tx);
     } else {
      MaBind(RepAppl(otatts)+2, tatts );
      return true;
     }
    } else {
      MaBind(RepAppl(tgt)+2, tatts );
      return TRUE;
    }      
  } else {
    Yap_ThrowError(REPRESENTATION_ERROR_VARIABLE, inp,
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
        if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P, CP))) {
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
        if (!Yap_gcl(LOCAL_Error_Size, 4, ENV, gc_P(P, CP))) {
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



/** @pred put_attrs(+ _Var_,+ _Attributes_)


Set all attributes of  _Var_.  See get_attrs/2 for a description of
 _Attributes_.


*/

static Int put_attrs(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);

  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;
    Term tatts = Deref(ARG3);


    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
    } else {
      while (!(attv = BuildNewAttVar(PASS_REGS1))) {
        LOCAL_Error_Size = sizeof(attvar_record);
        if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return FALSE;
        }
      }
      Yap_unify(ARG1, AbsAttVar(attv));
    }
    /* we may have a stack shift meanwhile!! */
    tatts = Deref(ARG3);
    if (IsVarTerm(tatts)) {
      Yap_Error(INSTANTIATION_ERROR, tatts, "second argument of put_att/2");
      return FALSE;
    } else if (!IsApplTerm(tatts)) {
      Yap_Error(TYPE_ERROR_COMPOUND, tatts, "second argument of put_att/2");
      return FALSE;
    }
    do {
      Term modname = Deref(ARG2);
    Functor f = FunctorAttVar;
    Term otatts, p;
    if (IsVarTerm((otatts = SearchAttsForModuleAsNameTerm(attv->Atts, modname, &p)))) {
      	Term ts[3];
	ts[0] = modname;
	  ts[2] = TermNil;
	  ts[1] = tatts;
	  Yap_unify(otatts, Yap_MkApplTerm(f, 3, ts));
	} else {
	  MaBind(RepAppl(otatts)+2, tatts );
      }      
      tatts = ArgOfTerm(3,tatts);
    } while(!IsVarTerm(tatts));
    Yap_DebugPlWriteln(attv->Atts);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE, inp,
              "first argument of put_att/2");
    return FALSE;
  }
  return true;
  }

static  Int del_attr(USES_REGS1) {
    /* receive a variable in ARG1 */
    Term inp = Deref(ARG1), *pt;
    /* if this is unbound, ok */
    attvar_record *attv = AttsFromTerm(inp);
     if (!attv) return false;
    Term tatts = attv->Atts;
    pt = &attv->Atts;
            Term mod = Deref(ARG2);
    while(!IsVarTerm(tatts) && tatts != TermNil) {
        if (ArgOfTerm(1,tatts) == mod) {
            if (Yap_unify(ARG3,ArgOfTerm(2, tatts))) {
                MaBind(pt,ArgOfTerm(3, tatts));
                return true;
            } else {
                return false;
            }
        } else {
           pt = RepAppl(tatts)+3;
           tatts =  ArgOfTerm(3, tatts);

        }
    }
    return false;
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
  if (IsVarTerm(inp) && IsAttachedTerm(inp)) {
      attvar_record *attv;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(attv->Atts) || attv->Atts == TermNil)
          return false;
      if (Yap_unify(ARG2, attv->Atts)) {
          MaBind(&attv->Atts, attv->Atts);
          return true;
      } else {
          return false;
      }
  }
    return false;
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
  attvar_record *attv = AttsFromTerm(inp);
  if (!attv) {
      Yap_ThrowError(REPRESENTATION_ERROR_VARIABLE, inp,
                     "first argument of get_attrs/2");
  } else {
    return Yap_unify(ARG2,attv->Atts);
  }
}


static Int get_attr(USES_REGS1) {
    /* receive a variable in ARG1 */
    Term inp = Deref(ARG1);
    /* if this is unbound, ok */
    attvar_record *attv = AttsFromTerm(inp);
    if (!attv) return false;
    Term tatts = Deref(ARG3);
    Term mod = Deref(ARG2), p;
    if (IsVarTerm(tatts = SearchAttsForModuleAsNameTerm(attv->Atts, mod, &p))) {
        return FALSE;
    } else {
        return Yap_unify(ARG3,ArgOfTerm(2,tatts));
    }
}

static Int free_att(USES_REGS1) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    Term modname = (Deref(ARG2));

    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tout, tatts, p;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(tatts = SearchAttsForModuleAsNameTerm(attv->Atts, modname, &p)))
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

static Term AllAttVars(USES_REGS1) {
  CELL *pt = H0;
  CELL *myH = HR;

  while (pt < myH) {
    switch (*pt) {
    case (CELL)FunctorAttVar:
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
      break;
    case (CELL)FunctorDouble:
#if SIZEOF_DOUBLE == 2 * SIZEOF_INT_P
      pt += 4;
#else
      pt += 3;
#endif
      break;
    case (CELL)FunctorString:
      pt += 3 + pt[1];
      break;
    case (CELL)FunctorBigInt: {
      Int sz = 3 + (sizeof(MP_INT) +
                    (((MP_INT *)(pt + 2))->_mp_alloc * sizeof(mp_limb_t))) /
                       sizeof(CELL);
      pt += sz;
    } break;
    case (CELL)FunctorLongInt:
      pt += 3;
      break;
    default:
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
      if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, gc_P(P, CP))) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return FALSE;
      }
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

#else

static Int all_attvars(USES_REGS1) { return FALSE; }

static Int is_attvar(USES_REGS1) { return FALSE; }

static Int attvar_bound(USES_REGS1) { return FALSE; }

#endif /* COROUTINING */

void Yap_InitAttVarPreds(void) {
  CACHE_REGS
  Term OldCurrentModule = CurrentModule;
  CurrentModule = ATTRIBUTES_MODULE;
#ifdef COROUTINING
  GLOBAL_attas[attvars_ext].bind_op = WakeAttVar;
  GLOBAL_attas[attvars_ext].copy_term_op = CopyAttVar;
  GLOBAL_attas[attvars_ext].to_term_op = AttVarToTerm;
  GLOBAL_attas[attvars_ext].term_to_op = TermToAttVar;
  GLOBAL_attas[attvars_ext].mark_op = mark_attvar;
  Yap_InitCPred("get_att", 4, get_att, SafePredFlag);
    Yap_InitCPred("has_module_atts", 2, has_atts, SafePredFlag);Yap_InitCPred("get_all_atts", 2, get_all_atts, SafePredFlag);
  Yap_InitCPred("get_all_swi_atts", 2, swi_all_atts, SafePredFlag);
  Yap_InitCPred("free_att", 3, free_att, SafePredFlag);
  Yap_InitCPred("put_att", 5, put_att, 0);
  Yap_InitCPred("del_all_module_atts", 2, del_attr, 0);
  Yap_InitCPred("rm_att", 4, rm_att, 0);
  Yap_InitCPred("bind_attvar", 1, bind_attvar, SafePredFlag);
  Yap_InitCPred("unbind_attvar", 1, unbind_attvar, SafePredFlag);
  Yap_InitCPred("modules_with_attributes", 2, modules_with_atts, SafePredFlag);
  Yap_InitCPred("void_term", 1, void_term, SafePredFlag);
  Yap_InitCPred("free_term", 1, free_term, SafePredFlag);
  Yap_InitCPred("fast_unify_attributed", 2, fast_unify, 0);
#endif /* COROUTINING */
  Yap_InitCPred("all_attvars", 1, all_attvars, 0);
  CurrentModule = OldCurrentModule;
    Yap_InitCPred("get_attr", 3, get_attr, SafePredFlag);
    Yap_InitCPred("del_attr", 3, del_attr, SafePredFlag);
    Yap_InitCPred("put_attr", 3, put_attr, 0);
    Yap_InitCPred("get_attrs", 2, get_attrs, SafePredFlag);
    Yap_InitCPred("del_attrs", 2, del_attrs, SafePredFlag);
    Yap_InitCPred("put_attrs", 2, put_attrs, 0);
  Yap_InitCPred("attvar", 1, is_attvar, SafePredFlag | TestPredFlag);
  Yap_InitCPred("$att_bound", 1, attvar_bound, SafePredFlag | TestPredFlag);
}

/** @} */

