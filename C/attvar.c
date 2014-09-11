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
* File:		attvar.c						 *
* Last rev:								 *
* mods:									 *
* comments:	YAP support for attributed vars				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[]="%W% %G%";
#endif

#include "Yap.h"

#include "Yatom.h"
#include "YapHeap.h"
#include "heapgc.h"
#include "attvar.h"
#ifndef NULL
#define NULL (void *)0
#endif

#ifdef COROUTINING

#define TermVoidAtt TermFoundVar

static CELL *
AddToQueue(attvar_record *attv USES_REGS)
{
  Term t[2];
  Term WGs, ng;

  t[0] = (CELL)&(attv->Done);
  t[1] = attv->Value;
  /* follow the chain */
  WGs = Yap_ReadTimedVar(LOCAL_WokenGoals);
  ng = Yap_MkApplTerm(FunctorAttGoal, 2, t);

  Yap_UpdateTimedVar(LOCAL_WokenGoals, MkPairTerm(ng, WGs));
  if ((Term)WGs == TermNil) {
    /* from now on, we have to start waking up goals */
    Yap_signal(YAP_WAKEUP_SIGNAL);
  }
  return(RepAppl(ng)+2);
}

static void
AddFailToQueue( USES_REGS1 )
{
  Term WGs;

  /* follow the chain */
  WGs = Yap_ReadTimedVar(LOCAL_WokenGoals);

  Yap_UpdateTimedVar(LOCAL_WokenGoals, MkPairTerm(MkAtomTerm(AtomFail), WGs));
  if ((Term)WGs == TermNil) {
    /* from now on, we have to start waking up goals */
    Yap_signal(YAP_WAKEUP_SIGNAL);
  }
}

static attvar_record *
BuildNewAttVar( USES_REGS1 )
{
  attvar_record *newv;

  /* add a new attributed variable */
  newv = (attvar_record *)HR;
  HR = (CELL *)(newv+1);
  newv->AttFunc = FunctorAttVar;
  RESET_VARIABLE(&(newv->Value));
  RESET_VARIABLE(&(newv->Done));
  RESET_VARIABLE(&(newv->Atts));
  return newv;
}

static int
CopyAttVar(CELL *orig, struct cp_frame **to_visit_ptr, CELL *res USES_REGS)
{
  register attvar_record *attv = RepAttVar(orig);
  register attvar_record *newv;
  struct cp_frame *to_visit = *to_visit_ptr;
  CELL *vt;

  
  if (!(newv = BuildNewAttVar( PASS_REGS1 )))
    return FALSE;
  vt = &(attv->Atts);
  to_visit->start_cp = vt-1;
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
  *to_visit_ptr = to_visit+1;
  *res = (CELL)&(newv->Done);
  return TRUE;
}

static Term
AttVarToTerm(CELL *orig)
{
  attvar_record *attv = RepAttVar(orig);

  return attv->Atts;
}

static int
IsEmptyWakeUp(Term atts)
{
  Atom name = NameOfFunctor(FunctorOfTerm(atts));
  Atom *pt = EmptyWakeups;
  int i = 0;
  while (i < MaxEmptyWakeups) {
    if (pt[i++] == name) return TRUE;
  }
  return FALSE;
}

void
Yap_MkEmptyWakeUp(Atom mod)
{
  if (MaxEmptyWakeups == MAX_EMPTY_WAKEUPS)
    Yap_Error(SYSTEM_ERROR, TermNil, "too many modules that do not wake up");
  EmptyWakeups[MaxEmptyWakeups++] = mod;
}


static int
TermToAttVar(Term attvar, Term to USES_REGS)
{
  attvar_record *attv = BuildNewAttVar( PASS_REGS1 );
  if (!attv)
    return FALSE;
  Bind_Global_NonAtt(&attv->Atts, attvar);
  *VarOfTerm(to) = AbsAttVar(attv);
  return TRUE;
}

static void
WakeAttVar(CELL* pt1, CELL reg2 USES_REGS)
{
  
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
	    AddFailToQueue( PASS_REGS1 );
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
      AddFailToQueue( PASS_REGS1 );
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

void
Yap_WakeUp(CELL *pt0) {
  CACHE_REGS
  CELL d0 = *pt0;
  RESET_VARIABLE(pt0);
  WakeAttVar(pt0, d0 PASS_REGS);
}


static void
mark_attvar(CELL *orig)
{
  return;
}

static Term
BuildAttTerm(Functor mfun, UInt ar USES_REGS)
{
  CELL *h0 = HR;
  UInt i;

  if (HR+(1024+ar) > ASP) {
    LOCAL_Error_Size=ar*sizeof(CELL);
    return 0L;
  }
  HR[0] = (CELL)mfun;
  RESET_VARIABLE(HR+1);
  HR += 2;
  for (i = 1; i< ar; i++) {
    *HR = TermVoidAtt;
    HR++;
  }
  return AbsAppl(h0);
}

static Term 
SearchAttsForModule(Term start, Functor mfun)
{
  do {
    if (IsVarTerm(start) ||
	FunctorOfTerm(start) == mfun)
      return start;
    start = ArgOfTerm(1,start);
  } while (TRUE);
}

static Term 
SearchAttsForModuleName(Term start, Atom mname)
{
  do {
    if (IsVarTerm(start) ||
	NameOfFunctor(FunctorOfTerm(start)) == mname)
      return start;
    start = ArgOfTerm(1,start);
  } while (TRUE);
}

static void 
AddNewModule(attvar_record *attv, Term t, int new, int do_it USES_REGS)
{
  CELL *newp = RepAppl(t)+2;
  UInt i, ar = ArityOfFunctor((Functor)newp[-2]);

  for (i=1; i< ar; i++) {
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
    MaBind(&(attv->Atts),t);
  } else {
    Term *wherep = &attv->Atts;

    do {
      if (IsVarTerm(*wherep)) {
	Bind_Global_NonAtt(wherep,t);      
	return;
      } else {
	wherep = RepAppl(Deref(*wherep))+1;
      }
    } while (TRUE);
  }
}

static void 
ReplaceAtts(attvar_record *attv, Term oatt, Term att USES_REGS)
{
  UInt ar = ArityOfFunctor(FunctorOfTerm(oatt)), i;
  CELL *oldp = RepAppl(oatt)+1;
  CELL *newp;

  if (oldp > HB) {
    oldp++;
    newp = RepAppl(att)+2;
    /* if deterministic */

    for (i=1; i< ar; i++) {
      Term n = Deref(*newp);
      if (n != TermFreeTerm) {
	*oldp = n;
      }
      oldp++;
      newp++;
    }
    return;
  }
  newp = RepAppl(att)+1;
  *newp++ = *oldp++;
  for (i=1; i< ar; i++) {
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
	wherep = RepAppl(Deref(*wherep))+1;
      }
    } while (TRUE);
  }
}

static void 
DelAllAtts(attvar_record *attv USES_REGS)
{
  MaBind(&(attv->Done), attv->Value);
}

static void 
DelAtts(attvar_record *attv, Term oatt USES_REGS)
{
  Term t = ArgOfTerm(1,oatt);
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
	wherep = RepAppl(Deref(*wherep))+1;
      }
    } while (TRUE);
  }
}

static void 
PutAtt(Int pos, Term atts, Term att USES_REGS)
{
  if (IsVarTerm(att) && VarOfTerm(att) > HR && VarOfTerm(att) < LCL0) {
    /* globalise locals */
    Term tnew = MkVarTerm();
    Bind_NonAtt(VarOfTerm(att), tnew);
    att = tnew;
  }
  MaBind(RepAppl(atts)+pos, att);
}

static Int
BindAttVar(attvar_record *attv USES_REGS) {
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
	  Yap_Error(SYSTEM_ERROR,(CELL)&(attv->Done),"attvar was bound when unset");
	  return(FALSE);
	}
      } else {
	Bind_Global_NonAtt(&(attv->Done), t);
      }
    }
    return(TRUE);
  } else {
    Yap_Error(SYSTEM_ERROR,(CELL)&(attv->Done),"attvar was bound when set");
    return(FALSE);
  }
}

static Int
UnBindAttVar(attvar_record *attv) {
  RESET_VARIABLE(&(attv->Value));
  return(TRUE);
}

static Term
GetAllAtts(attvar_record *attv) {
  /* check if we are already there */
  return attv->Atts;
}

static Int
p_put_att( USES_REGS1 ) {
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
      while (!(attv = BuildNewAttVar( PASS_REGS1 ))) {
	LOCAL_Error_Size = sizeof(attvar_record);
	if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}    
	inp = Deref(ARG1);
      }
      new = TRUE;
    }
    mfun= Yap_MkFunctor(modname,ar);
    if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts,mfun))) {
      while (!(tatts = BuildAttTerm(mfun,ar PASS_REGS))) {
	if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
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
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of put_attributes/2");
    return FALSE;
  }
}

static Int
p_put_att_term( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;

    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
      MaBind(&(attv->Atts), Deref(ARG2));
    } else {
      while (!(attv = BuildNewAttVar( PASS_REGS1 ))) {
	LOCAL_Error_Size = sizeof(attvar_record);
	if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}    
	inp = Deref(ARG1);
      }
      Bind_NonAtt(VarOfTerm(inp), AbsAttVar(attv));
      attv->Atts = Deref(ARG2);
    }
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of put_att_term/2");
    return(FALSE);
  }
}

static Int
p_rm_att( USES_REGS1 ) {
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
      while (!(attv = BuildNewAttVar( PASS_REGS1 ))) {
	LOCAL_Error_Size = sizeof(attvar_record);
	if (!Yap_gcl(LOCAL_Error_Size, 5, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}    
	inp = Deref(ARG1);
      }
      new = TRUE;
      Yap_unify(ARG1, AbsAttVar(attv));
    }
    mfun= Yap_MkFunctor(modname,ar);
    if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts,mfun))) {
      while (!(tatts = BuildAttTerm(mfun, ar PASS_REGS))) {
	if (!Yap_gcl(LOCAL_Error_Size, 4, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}    
      }
      AddNewModule(attv,tatts,new, FALSE PASS_REGS);
    } else {
      PutAtt(IntegerOfTerm(Deref(ARG4)), tatts, TermVoidAtt PASS_REGS);
    }
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of rm_att/2");
    return(FALSE);
  }
}

static Int
p_put_atts( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);

  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    attvar_record *attv;
    Term otatts;
    Term tatts = Deref(ARG2);
    Functor mfun = FunctorOfTerm(tatts);
    int new = FALSE;

    tatts = Deref(ARG2);
    if (IsAttachedTerm(inp)) {
      attv = RepAttVar(VarOfTerm(inp));
    } else {
      while (!(attv = BuildNewAttVar( PASS_REGS1 ))) {
	LOCAL_Error_Size = sizeof(attvar_record);
	if (!Yap_gcl(LOCAL_Error_Size, 2, ENV, gc_P(P,CP))) {
	  Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	  return FALSE;
	}    
	tatts = Deref(ARG2);
      }
      new = TRUE;
      Yap_unify(ARG1, AbsAttVar(attv));
    }
    /* we may have a stack shift meanwhile!! */ 
    tatts = Deref(ARG2);
    if (IsVarTerm(tatts)) {
      Yap_Error(INSTANTIATION_ERROR,tatts,"second argument of put_att/2");
      return FALSE;
    } else if (!IsApplTerm(tatts)) {
      Yap_Error(TYPE_ERROR_COMPOUND,tatts,"second argument of put_att/2");
      return FALSE;
    }
    if (IsVarTerm(otatts = SearchAttsForModule(attv->Atts,mfun))) {
      AddNewModule(attv, tatts, new, FALSE PASS_REGS);
    } else {
      ReplaceAtts(attv, otatts, tatts PASS_REGS);
    }
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of put_att/2");
    return FALSE;
  }
}

static Int
p_del_atts( USES_REGS1 ) {
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
    if (IsVarTerm(otatts = SearchAttsForModule(attv->Atts,mfun))) {
      return TRUE;
    } else {
      DelAtts(attv, otatts PASS_REGS);
    }
    return TRUE;
  } else {
    return TRUE;
  }
}

static Int
p_del_all_atts( USES_REGS1 ) {
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

static Int
p_get_att( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    Atom modname = AtomOfTerm(Deref(ARG2));

    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tout, tatts;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(tatts = SearchAttsForModuleName(attv->Atts,modname)))
	return FALSE;
      tout = ArgOfTerm(IntegerOfTerm(Deref(ARG3)),tatts);
      if (tout == TermVoidAtt) return FALSE;
      return Yap_unify(tout, ARG4);      
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of get_att/2");
    return(FALSE);
  }
}

static Int
p_free_att( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    Atom modname = AtomOfTerm(Deref(ARG2));

    if (IsAttachedTerm(inp)) {
      attvar_record *attv;
      Term tout, tatts;

      attv = RepAttVar(VarOfTerm(inp));
      if (IsVarTerm(tatts = SearchAttsForModuleName(attv->Atts,modname)))
	return TRUE;
      tout = ArgOfTerm(IntegerOfTerm(Deref(ARG3)),tatts);
      return (tout == TermVoidAtt);
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return TRUE;
    }
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of free_att/2");
    return(FALSE);
  }
}

static Int
p_get_atts( USES_REGS1 ) {
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
      if (IsVarTerm(tatts = SearchAttsForModule(attv->Atts,mfun)))
	return FALSE;
      
      ar = ArityOfFunctor(mfun);
      new = RepAppl(access)+2;
      old = RepAppl(tatts)+2;
      for (i = 1; i < ar; i++,new++,old++) {
	if (*new != TermFreeTerm) {
	  if (*old == TermVoidAtt && *new != TermVoidAtt)
	    return FALSE;
	  if (*new == TermVoidAtt && *old != TermVoidAtt)
	    return FALSE;
	  if (!Yap_unify(*new,*old)) return FALSE;
	}
      }
      return TRUE;
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    return(FALSE);
  }
}

static Int
p_has_atts( USES_REGS1 ) {
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
      return !IsVarTerm(tatts = SearchAttsForModule(attv->Atts,mfun));
    } else {
      /* Yap_Error(INSTANTIATION_ERROR,inp,"get_att/2"); */
      return FALSE;
    }
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of has_atts/2");
    return(FALSE);
  }
}

static Int
p_bind_attvar( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term  inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return(BindAttVar(attv PASS_REGS));
    }
    return(TRUE);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of bind_attvar/2");
    return(FALSE);
  }
}

static Int
p_unbind_attvar( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term  inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return(UnBindAttVar(attv));
    }
    return(TRUE);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of bind_attvar/2");
    return(FALSE);
  }
}

static Int
p_get_all_atts( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      return Yap_unify(ARG2,GetAllAtts(attv));
    }
    return TRUE;
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of get_all_atts/2");
    return FALSE;
  }
}

static int
ActiveAtt(Term tatt, UInt ar)
{
  CELL *cp = RepAppl(tatt)+1;
  UInt i;

  for (i = 1; i < ar; i++) {
    if (cp[i] != TermVoidAtt)
      return TRUE;
  }
  return FALSE;
}

static Int
p_modules_with_atts( USES_REGS1 ) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = RepAttVar(VarOfTerm(inp));
      CELL *h0 = HR;
      Term tatt;

      if (IsVarTerm(tatt = attv->Atts))
	  return Yap_unify(ARG2,TermNil);
      while (!IsVarTerm(tatt)) {
	Functor f = FunctorOfTerm(tatt);
	if (HR != h0)
	  HR[-1] = AbsPair(HR);
	if (ActiveAtt(tatt, ArityOfFunctor(f))) {
	  *HR = MkAtomTerm(NameOfFunctor(f));
	  HR+=2;
	}
	tatt = ArgOfTerm(1,tatt);
      }
      if (h0 != HR) {
	HR[-1] = TermNil;
	return Yap_unify(ARG2,AbsPair(h0));
      }
    }
    return Yap_unify(ARG2,TermNil);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of modules_with_attributes/2");
    return FALSE;
  }
}

static Int
p_swi_all_atts( USES_REGS1 ) {
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
	  return Yap_unify(ARG2,TermNil);
      while (!IsVarTerm(tatt)) {
	Functor f = FunctorOfTerm(tatt);
	UInt ar = ArityOfFunctor(f);

	if (HR != h0)
	  HR[-1] = AbsAppl(HR);
	HR[0] = (CELL) attf;
	HR[1] = MkAtomTerm(NameOfFunctor(f));
	/* SWI */
	if (ar == 2) 
	  HR[2] =  ArgOfTerm(2,tatt);
	else
	  HR[2] =  tatt;
	HR += 4;
	HR[-1] = AbsAppl(HR);
	tatt = ArgOfTerm(1,tatt);
      }
      if (h0 != HR) {
	HR[-1] = TermNil;
	return Yap_unify(ARG2,AbsAppl(h0));
      }
    }
    return Yap_unify(ARG2,TermNil);
  } else {
    Yap_Error(REPRESENTATION_ERROR_VARIABLE,inp,"first argument of get_all_swi_atts/2");
    return FALSE;
  }
}


static Term
AllAttVars( USES_REGS1 ) {
  CELL *pt = H0;
  CELL *myH = HR;
  
  while (pt < myH) {
    switch(*pt) {
    case (CELL)FunctorAttVar:
      if (IsUnboundVar(pt+1)) {
	if (ASP - myH < 1024) {
	  LOCAL_Error_Size = (ASP-HR)*sizeof(CELL);
	  return 0L;
	}
	if (myH != HR) {
	  myH[-1] = AbsPair(myH);
	}
	myH[0] = AbsAttVar((attvar_record *)pt);
	myH += 2;
      }
      pt += (1+ATT_RECORD_ARITY);
      break;
    case (CELL)FunctorDouble:
#if SIZEOF_DOUBLE == 2*SIZEOF_INT_P
      pt += 4;
#else
      pt += 3;
#endif
      break;
    case (CELL)FunctorString:
      pt += 3+pt[1];
      break;
    case (CELL)FunctorBigInt:
      {
	Int sz = 3 +
	  (sizeof(MP_INT)+
	   (((MP_INT *)(pt+2))->_mp_alloc*sizeof(mp_limb_t)))/sizeof(CELL);
	pt += sz;
      }
      break;
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
  
static Int
p_all_attvars( USES_REGS1 )
{
  do {
    Term out;

    if (!(out = AllAttVars( PASS_REGS1 ))) {
      if (!Yap_gcl(LOCAL_Error_Size, 1, ENV, gc_P(P,CP))) {
	Yap_Error(OUT_OF_STACK_ERROR, TermNil, LOCAL_ErrorMessage);
	return FALSE;
      }    
    } else {
      return Yap_unify(ARG1,out);
    }
  } while (TRUE);
}

static Int
p_is_attvar( USES_REGS1 )
{
  Term t = Deref(ARG1);
  return(IsVarTerm(t) &&
	 IsAttVar(VarOfTerm(t)));
}

/* check if we are not redoing effort */
static Int
p_attvar_bound( USES_REGS1 )
{
  Term t = Deref(ARG1);
  return
    IsVarTerm(t) &&
    IsAttachedTerm(t) &&
    !IsUnboundVar(&(RepAttVar(VarOfTerm(t))->Done));
}

static Int
p_void_term( USES_REGS1 )
{
  return Yap_unify(ARG1,TermVoidAtt);
}

static Int
p_free_term( USES_REGS1 )
{
  return Yap_unify(ARG1,TermFreeTerm);
}

static Int
p_fast_unify( USES_REGS1 )
{
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
  if(a > b) {						 
    Bind_Global_NonAtt(a,t2);				 
  } else if((a) < (b)){						 
    Bind_Global_NonAtt(b,t1);				 
  }
  return TRUE;
}

#else

static Int
p_all_attvars( USES_REGS1 )
{
  return FALSE;
}

static Int
p_is_attvar( USES_REGS1 )
{
  return FALSE;
}

static Int
p_attvar_bound( USES_REGS1 )
{
  return FALSE;
}

#endif /* COROUTINING */

void Yap_InitAttVarPreds(void)
{
  CACHE_REGS
  Term OldCurrentModule = CurrentModule;
  CurrentModule = ATTRIBUTES_MODULE;
#ifdef COROUTINING
  GLOBAL_attas[attvars_ext].bind_op = WakeAttVar;
  GLOBAL_attas[attvars_ext].copy_term_op = CopyAttVar;
  GLOBAL_attas[attvars_ext].to_term_op = AttVarToTerm;
  GLOBAL_attas[attvars_ext].term_to_op = TermToAttVar;
  GLOBAL_attas[attvars_ext].mark_op = mark_attvar;
  Yap_InitCPred("get_att", 4, p_get_att, SafePredFlag);
  Yap_InitCPred("get_module_atts", 2, p_get_atts, SafePredFlag);
  Yap_InitCPred("has_module_atts", 2, p_has_atts, SafePredFlag);
  Yap_InitCPred("get_all_atts", 2, p_get_all_atts, SafePredFlag);
  Yap_InitCPred("get_all_swi_atts", 2, p_swi_all_atts, SafePredFlag);
  Yap_InitCPred("free_att", 3, p_free_att, SafePredFlag);
  Yap_InitCPred("put_att", 5, p_put_att, 0);
  Yap_InitCPred("put_att_term", 2, p_put_att_term, 0);
  Yap_InitCPred("put_module_atts", 2, p_put_atts, 0);
  Yap_InitCPred("del_all_module_atts", 2, p_del_atts, 0);
  Yap_InitCPred("del_all_atts", 1, p_del_all_atts, 0);
  Yap_InitCPred("rm_att", 4, p_rm_att, 0);
  Yap_InitCPred("bind_attvar", 1, p_bind_attvar, SafePredFlag);
  Yap_InitCPred("unbind_attvar", 1, p_unbind_attvar, SafePredFlag);
  Yap_InitCPred("modules_with_attributes", 2, p_modules_with_atts, SafePredFlag);
  Yap_InitCPred("void_term", 1, p_void_term, SafePredFlag);
  Yap_InitCPred("free_term", 1, p_free_term, SafePredFlag);
  Yap_InitCPred("fast_unify_attributed", 2, p_fast_unify, 0);
#endif /* COROUTINING */
  Yap_InitCPred("all_attvars", 1, p_all_attvars, 0);
  CurrentModule = OldCurrentModule;
  Yap_InitCPred("attvar", 1, p_is_attvar, SafePredFlag|TestPredFlag);
/** @pred attvar( _-Var_) 


Succeed if  _Var_ is an attributed variable.



 */
  Yap_InitCPred("$att_bound", 1, p_attvar_bound, SafePredFlag|TestPredFlag);
}



