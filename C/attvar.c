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

#ifdef COROUTINING

#include "Yatom.h"
#include "Heap.h"
#include "heapgc.h"
#include "attvar.h"
#ifndef NULL
#define NULL (void *)0
#endif

STATIC_PROTO(Int  InitVarTime, (void));
STATIC_PROTO(Int  CurrentTime, (void));

static CELL *
AddToQueue(attvar_record *attv)
{
  Term t[2];
  sus_record *WGs;
  sus_record *new;

  t[0] = (CELL)&(attv->Done);
  t[1] = attv->Value;
  /* follow the chain */
  WGs = (sus_record *)ReadTimedVar(WokenGoals);
  new = (sus_record *)H;
  H = (CELL *)(new+1);
  new->NR = (sus_record *)(&(new->NR));
  new->SG = MkApplTerm(FunctorAttGoal, 2, t);
  new->NS = new;

  if ((Term)WGs == TermNil) {
    UpdateTimedVar(WokenGoals, (CELL)new);
    /* from now on, we have to start waking up goals */
    if (CreepFlag != Unsigned(LCL0) - Unsigned(H0))
      CreepFlag = Unsigned(LCL0);
  } else {
    /* add to the end of the current list of suspended goals */
    CELL *where_to = (CELL *)Deref((CELL)WGs);
    Bind_Global(where_to, (CELL)new);
  }
  return(RepAppl(new->SG)+2);
}

static CELL *
AddFailToQueue(void)
{
  sus_record *WGs;
  sus_record *new;

  /* follow the chain */
  WGs = (sus_record *)ReadTimedVar(WokenGoals);
  new = (sus_record *)H;
  H = (CELL *)(new+1);
  new->NR = (sus_record *)(&(new->NR));
  new->SG = MkAtomTerm(AtomFail);
  new->NS = new;

  if ((Term)WGs == TermNil) {
    UpdateTimedVar(WokenGoals, (CELL)new);
    /* from now on, we have to start waking up goals */
    if (CreepFlag != Unsigned(LCL0) - Unsigned(H0))
      CreepFlag = Unsigned(LCL0);
  } else {
    /* add to the end of the current list of suspended goals */
    CELL *where_to = (CELL *)Deref((CELL)WGs);
    Bind_Global(where_to, (CELL)new);
  }
  return(RepAppl(new->SG)+2);
}

static int
CopyAttVar(Term orig, CELL ***to_visit_ptr)
{
  register attvar_record *attv = (attvar_record *)orig;
  register attvar_record *newv;
  CELL **to_visit = *to_visit_ptr;
  Term ttime;
  Term time = InitVarTime();
  Int j;

  /* add a new attributed variable */
  newv = (attvar_record *)ReadTimedVar(DelayedVars);
  if (H0 - (CELL *)newv < 1024+(2*NUM_OF_ATTS))
    return(FALSE);
  RESET_VARIABLE(&(newv->Done));
  newv->sus_id = attvars_ext;
  RESET_VARIABLE(&(newv->Value));
  newv->NS = UpdateTimedVar(AttsMutableList, (CELL)&(newv->Done));
  ttime = MkIntegerTerm(time);
  for (j = 0; j < NUM_OF_ATTS; j++) {
    newv->Atts[2*j] = ttime;
    to_visit[0] = attv->Atts+2*j;
    to_visit[1] = attv->Atts+2*j+1;
    to_visit[2] = newv->Atts+2*j+1;
    to_visit += 3;
  }
  *to_visit_ptr = to_visit;
  UpdateTimedVar(DelayedVars, (CELL)(newv->Atts+2*j));
  return(TRUE);
}

static void
WakeAttVar(CELL* pt1, CELL reg2)
{
  
  /* if bound to someone else, follow until we find the last one */
  attvar_record *attv = (attvar_record *)pt1;
  CELL *myH = H;
  CELL *bind_ptr;

  if (!IsVarTerm(attv->Value) || !IsUnboundVar(attv->Value)) {
    /* oops, our goal is on the queue to be woken */
    if (!unify(attv->Value, reg2)) {
      AddFailToQueue();
    }
    return;
  }
  if (IsVarTerm(reg2)) {
    if (IsAttachedTerm(reg2)) {
      attvar_record *susp2 = (attvar_record *)VarOfTerm(reg2);

      /* binding two suspended variables, be careful */
      if (susp2->sus_id != attvars_ext) {
	/* joining two different kinds of suspensions */
	Error(SYSTEM_ERROR, TermNil, "joining two different suspensions not implemented");
	return;
      }
      if (susp2 >= attv) {
	if (susp2 == attv) return;
	if (!IsVarTerm(susp2->Value) || !IsUnboundVar(susp2->Value)) {
	  /* oops, our goal is on the queue to be woken */
	  if (!unify(susp2->Value, (CELL)pt1)) {
	    AddFailToQueue();
	  }
	}
	Bind_Global(&(susp2->Value), (CELL)pt1);
	AddToQueue(susp2);
	return;
      }
    } else {
      Bind(VarOfTerm(reg2), (CELL)pt1);
      return;
    }
  }
  bind_ptr = AddToQueue(attv);
  if (IsNonVarTerm(reg2)) {
    if (IsPairTerm(reg2) && RepPair(reg2) == myH)
      reg2 = AbsPair(H);
    else if (IsApplTerm(reg2) && RepAppl(reg2) == myH)
      reg2 = AbsAppl(H);
  }
  *bind_ptr = reg2;
  Bind_Global(&(attv->Value), reg2);
}

#ifndef FIXED_STACKS

static void
mark_attvar(CELL *orig)
{
  register attvar_record *attv = (attvar_record *)orig;
  Int i;

  mark_external_reference(&(attv->Value));
  mark_external_reference(&(attv->Done));
  for (i = 0; i < NUM_OF_ATTS; i++) {
    mark_external_reference(attv->Atts+2*i+1);
  }
}

#endif /* FIXED_STACKS */

static Int
CurrentTime(void) {
  return((CELL *)(TR)-(CELL *)TrailBase);
}

static Int
InitVarTime(void) {
  if (B->cp_tr == TR) {
    /* we run the risk of not making non-determinate bindings before
       the end of the night */
    /* so we just init a TR cell that will not harm anyone */
    Bind((CELL *)(TR+1),AbsAppl(H-1));
  }
  return((CELL *)(B->cp_tr)-(CELL *)TrailBase);
}

static Int
PutAtt(attvar_record *attv, Int i, Term tatt) {
  Int pos = i*2;
  tr_fr_ptr timestmp = (tr_fr_ptr)((CELL *)TrailBase+IntegerOfTerm(attv->Atts[pos]));
  if (B->cp_tr <= timestmp
#if defined(SBA) || defined(TABLING)
      && timestmp <= TR
#endif
    ) {
#if defined(SBA)
    if (Unsigned((Int)(attv)-(Int)(H_FZ)) >
	Unsigned((Int)(B_FZ)-(Int)(H_FZ))) {
      CELL *ptr = STACK_TO_SBA(attv->Atts+pos+1);
      *ptr = tatt;
    } else
#endif
      attv->Atts[pos+1] = tatt;
#if defined(SBA) || defined(TABLING)
    if (Unsigned((Int)(attv)-(Int)(HBREG)) >
      Unsigned(BBREG)-(Int)(HBREG))
    TrailVal(timestmp-1) = tatt;
#endif    
  } else {
    Term tnewt;
    MaBind(attv->Atts+pos+1, tatt);
    tnewt = MkIntegerTerm(CurrentTime());
    MaBind(attv->Atts+pos, tnewt);    
  }
  return(TRUE);
}

static Int
RmAtt(attvar_record *attv, Int i) {
  Int pos = i *2;
  if (!IsVarTerm(attv->Atts[pos+1])) {
    tr_fr_ptr timestmp = (tr_fr_ptr)((CELL *)TrailBase+IntegerOfTerm(attv->Atts[pos]));
    if (B->cp_tr <= timestmp
#if defined(SBA) || defined(TABLING)
	&& timestmp <= TR
#endif
	) {
      RESET_VARIABLE(attv->Atts+(pos+1));
#if defined(SBA) || defined(TABLING)
    if (Unsigned((Int)(attv)-(Int)(HBREG)) >
      Unsigned(BBREG)-(Int)(HBREG))
    TrailVal(timestmp-1) = attv->Atts[pos+1];
#endif    
    } else {
      /* reset the variable */
      Term tnewt;
#ifdef SBA
      MaBind(attv->Atts+(pos+1), 0L);    
#else
      MaBind(attv->Atts+(pos+1), (CELL)(attv->Atts+(pos+1)));    
#endif
     tnewt = MkIntegerTerm(CurrentTime());
     MaBind(attv->Atts+pos, tnewt);    
    }
  }
  return(TRUE);
}

static Int
BuildNewAttVar(Term t, Int i, Term tatt)
{
  /* allocate space in Heap */
  Term time = InitVarTime();
  int j;
  Term ttime;

  attvar_record *attv = (attvar_record *)ReadTimedVar(DelayedVars);
  if (H0 - (CELL *)attv < 1024+(2*NUM_OF_ATTS)) {
    ARG1 = t;
    ARG2 = tatt;
    growglobal();
    t = ARG1;
    tatt = ARG2;
  }
  RESET_VARIABLE(&(attv->Value));
  RESET_VARIABLE(&(attv->Done));
  attv->sus_id = attvars_ext;
  ttime = MkIntegerTerm(time);
  for (j = 0; j < NUM_OF_ATTS; j++) {
    attv->Atts[2*j] = ttime;
    RESET_VARIABLE(attv->Atts+2*j+1);
  }
  attv->NS = UpdateTimedVar(AttsMutableList, (CELL)&(attv->Done));
  Bind((CELL *)t,(CELL)attv);
  UpdateTimedVar(DelayedVars,(CELL)(attv->Atts+2*j));
  return(PutAtt(attv, i, tatt));
}

static Int
GetAtt(attvar_record *attv, int i) {
  Int pos = i *2;
#if SBA
  if (IsUnboundVar(attv->Atts[pos+1]))
    return((CELL)&(attv->Atts[pos+1]));
#endif  
  return(attv->Atts[pos+1]);
}

static Int
FreeAtt(attvar_record *attv, int i) {
  Int pos = i *2;
  return(IsVarTerm(attv->Atts[pos+1]));
}

static Int
BindAttVar(attvar_record *attv) {
  if (IsVarTerm(attv->Done) && IsUnboundVar(attv->Done)) {
    Bind_Global(&(attv->Done), attv->Value);
    return(TRUE);
  } else {
    Error(SYSTEM_ERROR,(CELL)&(attv->Done),"attvar was bound when set");
    return(FALSE);
  }
}

static Term
GetAllAtts(attvar_record *attv) {
  Int i;
  Term t = TermNil;
  for (i = NUM_OF_ATTS-1; i >= 0; i --) {
    if (!IsVarTerm(attv->Atts[2*i+1]))
      t = MkPairTerm(MkPairTerm(MkIntegerTerm(i),attv->Atts[2*i+1]), t);
  }
  return(t);
}

static Term
AllAttVars(Term t) {
  if (t == TermNil) {
    return(t);
  } else {
    attvar_record *attv = (attvar_record *)VarOfTerm(t);
    if (!IsVarTerm(attv->Done) || !IsUnboundVar(attv->Done))
      return(AllAttVars(attv->NS));
    else return(MkPairTerm(t,AllAttVars(attv->NS)));
  }
}

Term
CurrentAttVars(void) {
  return(AllAttVars(ReadTimedVar(AttsMutableList)));

}

static Int
p_put_att(void) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = (attvar_record *)VarOfTerm(inp);
      exts id = (exts)attv->sus_id;

      if (id != attvars_ext) {
	Error(TYPE_ERROR_VARIABLE,inp,"put_attributes/2");
	return(FALSE);
      }
      return(PutAtt(attv, IntegerOfTerm(Deref(ARG2)), Deref(ARG3)));
    }
    return(BuildNewAttVar(inp, IntegerOfTerm(Deref(ARG2)), Deref(ARG3)));
  } else {
    Error(TYPE_ERROR_VARIABLE,inp,"put_attributes/2");
    return(FALSE);
  }
}

static Int
p_rm_att(void) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = (attvar_record *)VarOfTerm(inp);
      exts id = (exts)attv->sus_id;

      if (id != attvars_ext) {
	Error(TYPE_ERROR_VARIABLE,inp,"delete_attribute/2");
	return(FALSE);
      }
      return(RmAtt(attv, IntegerOfTerm(Deref(ARG2))));
    }
    return(TRUE);
  } else {
    Error(TYPE_ERROR_VARIABLE,inp,"delete_attribute/2");
    return(FALSE);
  }
}

static Int
p_get_att(void) {
  /* receive a variable in ARG1 */
  Term  inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = (attvar_record *)VarOfTerm(inp);
      Term out;
      exts id = (exts)attv->sus_id;

      if (id != attvars_ext) {
	Error(TYPE_ERROR_VARIABLE,inp,"get_att/2");
	return(FALSE);
      }
      out = GetAtt(attv,IntegerOfTerm(Deref(ARG2)));
      return(!IsVarTerm(out) && unify(ARG3,out));
    }
    /*    Error(INSTANTIATION_ERROR,inp,"get_att/2");*/
    return(FALSE);
  } else {
    Error(TYPE_ERROR_VARIABLE,inp,"get_att/2");
    return(FALSE);
  }
}

static Int
p_free_att(void) {
  /* receive a variable in ARG1 */
  Term  inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = (attvar_record *)VarOfTerm(inp);
      exts id = (exts)attv->sus_id;

      if (id != attvars_ext) {
	Error(TYPE_ERROR_VARIABLE,inp,"get_att/2");
	return(FALSE);
      }
      return(FreeAtt(attv,IntegerOfTerm(Deref(ARG2))));
    }
    return(TRUE);
  } else {
    Error(TYPE_ERROR_VARIABLE,inp,"free_att/2");
    return(FALSE);
  }
}

static Int
p_bind_attvar(void) {
  /* receive a variable in ARG1 */
  Term  inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = (attvar_record *)VarOfTerm(inp);
      exts id = (exts)attv->sus_id;

      if (id != attvars_ext) {
	Error(TYPE_ERROR_VARIABLE,inp,"get_att/2");
	return(FALSE);
      }
      return(BindAttVar(attv));
    }
    return(TRUE);
  } else {
    Error(TYPE_ERROR_VARIABLE,inp,"bind_att/2");
    return(FALSE);
  }
}

static Int
p_get_all_atts(void) {
  /* receive a variable in ARG1 */
  Term inp = Deref(ARG1);
  /* if this is unbound, ok */
  if (IsVarTerm(inp)) {
    if (IsAttachedTerm(inp)) {
      attvar_record *attv = (attvar_record *)VarOfTerm(inp);
      exts id = (exts)(attv->sus_id);

      if (id != attvars_ext) {
	Error(TYPE_ERROR_VARIABLE,inp,"get_att/2");
	return(FALSE);
      }
      return(unify(ARG2,GetAllAtts(attv)));
    }
    return(TRUE);
  } else {
    Error(TYPE_ERROR_VARIABLE,inp,"get_att/2");
    return(FALSE);
  }
}

static Int
p_inc_atts(void)
{
  Term t = MkIntegerTerm(NUM_OF_ATTS);
  NUM_OF_ATTS++;
  return(unify(ARG1,t));
}

static Int
p_n_atts(void)
{
  Term t = MkIntegerTerm(NUM_OF_ATTS);
  return(unify(ARG1,t));
}

static Int
p_all_attvars(void)
{
  Term t = ReadTimedVar(AttsMutableList);
  return(unify(ARG1,AllAttVars(t)));
}

static Int
p_is_attvar(void)
{
  Term t = Deref(ARG1);
  return(IsVarTerm(t) &&
	 IsAttachedTerm(t) &&
	 ((attvar_record *)VarOfTerm(t))->sus_id == attvars_ext);
}

void InitAttVarPreds(void)
{
  attas[attvars_ext].bind_op = WakeAttVar;
  attas[attvars_ext].copy_term_op = CopyAttVar;
#ifndef FIXED_STACKS
  attas[attvars_ext].mark_op = mark_attvar;
#endif
  InitCPred("get_att", 3, p_get_att, SafePredFlag);
  InitCPred("get_all_atts", 2, p_get_all_atts, SafePredFlag);
  InitCPred("free_att", 2, p_free_att, SafePredFlag);
  InitCPred("put_att", 3, p_put_att, 0);
  InitCPred("rm_att", 2, p_rm_att, SafePredFlag);
  InitCPred("inc_n_of_atts", 1, p_inc_atts, SafePredFlag);
  InitCPred("n_of_atts", 1, p_n_atts, SafePredFlag);
  InitCPred("bind_attvar", 1, p_bind_attvar, SafePredFlag);
  InitCPred("$all_attvars", 1, p_all_attvars, SafePredFlag);
  InitCPred("$is_att_variable", 1, p_is_attvar, SafePredFlag);
}

#endif /* COROUTINING */


