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
* File:		corout.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Co-routining from within YAP				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[]="%W% %G%";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "heapgc.h"
#ifndef NULL
#define NULL (void *)0
#endif

/*

These are simple routines to support co-routining in YAP. The idea is
to make the interface as simple as possible.

The interface for co-routines is:

$freeze(+X,+G) -> execute G only when V is *bound* (not
necessarily ground.

The data-structures are:


                       	|-------------------|
Ref    ---------------->|  V   |   SG\      |
			|-------------\-----|
				       \
					\ |------------/
					 >| |  | G | NS|
					  | |  |   |   |
					  |-|----------|
					    |
					    V
                                          |------------/
					->|    | G | NS|
					| | |  |   |   |
					| |-|----------|
					|   |
					|----

Where V is an indicator for the term, SG is a pointer for the list of
suspended goals, G is the suspended goal, and NS is a pointer to a
list of suspended goals.

When suspend_on is called, it executes the following operations:
	if (X is a reference):
		add a record containing G to the tail
		of the current SG list for V
	if (X is unbound)
		create a record R containing G and a self-reference.
		create a suspension register containing a free
		variable V and a pointer to R (sus goal list)
		Bind V to a Ref to the new structure.
	if (X is nonvar)
		Oooppssss!!!! The Prolog interface should have
		prevented this.

When trying to unify a nonvar to a suspension variable, the following
actions are taken:

	o Bind V to to the nonvar. This is done within absmi.c and
	depends a lot on the surrounding code.

	o Make the list SG the head of the list WokenGoals.

	o Activate the Interrupt Flag, so that the system will process
	the suspended goals at the next "call" absmiop.

	At the next "call":
	 + Save the current goal on the heap (C).
	 + Take the first member of the WokenGoals list.
	 + set up '$resume_and_continue'(?G,?C), which should execute
	   G and then C.
	 + If WokenGoals is empty, down Interrupt Flag
	 + jump to the code for '$resume_and_continue'(?G,?C)
	 + Note, the system will fetch the next goal at the next
	 "call" op.

When trying to unify two suspended variables X and Y, we just bind X
to Y, and include Y's goals in X's list.

The standard definition for resume_and_continue:

'$wake_up_goal'(C,G) :- call(G), call(C).

Advantages:

	o Implementation is simple (the main work is changing absmi).

	o Does not need updatable variables.

	o No special support in backtracking.

	o Data structures spend little space.

Disadvantages

	o We create a goal frame for every suspended goal. This is
	avoided by storing both P and the arguments in the suspension
	record, and then jumping.

	o We do a lot of meta-calls. This can be avoided by
	manipulating P and CP directly.

*/

STATIC_PROTO(Int p_read_svar_list, (void));
STATIC_PROTO(Int p_set_svar_list, (void));
STATIC_PROTO(Int p_frozen_goals, (void));
STATIC_PROTO(Int p_all_frozen_goals, (void));
STATIC_PROTO(Int p_freeze_on_first, (void));
STATIC_PROTO(Int p_freeze, (void));
STATIC_PROTO(Int p_can_unify, (void));
STATIC_PROTO(Int p_non_ground, (void));

#ifdef COROUTINING

STATIC_PROTO(void Wake, (CELL *, CELL));
STATIC_PROTO(sus_record *UpdateSVarList, (sus_record *));
STATIC_PROTO(sus_record *GetSVarList, (void));
STATIC_PROTO(void mark_sus_record, (sus_record *));
STATIC_PROTO(void mark_suspended_goal, (CELL *));
STATIC_PROTO(void AddSuspendedGoals, (sus_record *, sus_record *));
STATIC_PROTO(void ReleaseGoals, (sus_record *));
STATIC_PROTO(void wake_if_binding_vars_in_frozen_goal, (Term, sus_record *));
STATIC_PROTO(void AddSuspendedGoals, (sus_record *, sus_record *));
STATIC_PROTO(sus_record *has_been_suspended, (Term, sus_record *));
STATIC_PROTO(void AddSuspendedGoal, (Term, sus_record *));
STATIC_PROTO(Term AddSusToList, (Term, Term));
STATIC_PROTO(Term AddSusSubGoals, (Term, CELL *, int));
STATIC_PROTO(Int freeze_goal, (Term, Term));
STATIC_PROTO(Term AddVarIfNotThere, (Term, Term));
STATIC_PROTO(int can_unify_complex, (CELL *, CELL *, CELL *, Term *));
STATIC_PROTO(int can_unify, (Term, Term, Term *));
STATIC_PROTO(int non_ground_complex, (CELL *, CELL *, Term *));
STATIC_PROTO(int non_ground, (Term, Term *));
#ifdef FOLLOW_ENVIRONMENTS_FOR_SUSPENDED_GOALS
STATIC_PROTO(Term FindFrozenGoals, (Term, CELL *, int));
#endif

#ifdef MULTI_ASSIGNMENT_VARIABLES

inline static sus_record *
UpdateSVarList(sus_record *sl)
{
  /* make sl the new head of the suspension list, and update the list
     to use the old one. Note that the list is only bound once,
     MutableList is the one variable being updated all the time */
  return((sus_record *)Yap_UpdateTimedVar(MutableList, (CELL)sl));
}

inline static sus_record *
GetSVarList(void)
{
  Term t = Yap_ReadTimedVar(MutableList);
  /* just return the start of the list */
  if (t == TermNil)
    return(NULL);
  else
    return((sus_record *)t);
}

#endif

/* dif (and eventually others) may have the same goal suspended on the
   several variables. If this is the case, whenever we bind two
   variables we may need to wake the goals. That's implemented by
   going to the other guy's list, and checking if the same goal
   appears there.

*/

static Term
ListOfWokenGoals(void) {
  sus_record *pt = (sus_record *)Yap_ReadTimedVar(WokenGoals);
  Term t;

  t = TermNil;
  while (pt->NR != (sus_record *)(&(pt->NR))) {
    t = MkPairTerm(pt->SG, t);
    pt = pt->NR;
  }
  t = MkPairTerm(pt->SG, t);  
  return(t);
}

Term
Yap_ListOfWokenGoals(void) {
  return ListOfWokenGoals();
}


static void ReleaseGoals(sus_record *from)
{
  /* follow the chain */
  sus_record *WGs = (sus_record *)Yap_ReadTimedVar(WokenGoals);

  if ((Term)WGs == TermNil) {
    Yap_UpdateTimedVar(WokenGoals, (CELL)from);
  } else {
    /* add to the end of the current list of suspended goals */
    CELL *where_to = (CELL *)Deref((CELL)WGs);
    Bind_Global(where_to, (CELL)from);
  }
  /* from now on, we have to start waking up goals */
  if (CreepFlag != Unsigned(LCL0) - Unsigned(H0))
    CreepFlag = Unsigned(LCL0);
}

static void
wake_if_binding_vars_in_frozen_goal(Term goal, sus_record *from)
{
  do {
    if (from->SG == goal) {
      sus_record *gf;

      /* A dif like goal has suspended on both variables. We cannot
	 wake it up directly, because it may have other goals
	 suspended on the same variable. So we'll just wake up a copy,
	 and wake up the copy.
	 */
      gf = (sus_record *)H;
      H += sizeof(sus_record)/sizeof(CELL);
      gf->NR = (sus_record *)&(gf->NR);
      gf->SG = goal;
#ifdef MULTI_ASSIGNMENT_VARIABLES
      gf->NS = UpdateSVarList(gf);
#endif
      ReleaseGoals(gf);
      /* done */
      return;
    }
    if (from->NR == (sus_record *)&(from->NR))
      return;
    else from = from->NR;
  } while (TRUE);
}

inline static void AddSuspendedGoals(sus_record *to, sus_record *from)
{
  /* deref the chain */
  do {
    if (IsApplTerm(to->SG))
      wake_if_binding_vars_in_frozen_goal(to->SG, from);
    if (to->NR == (sus_record *)&(to->NR))
      break;
    else to = to->NR;
  } while (TRUE);
  /* and bind it */
  Bind_Global((CELL *)(to->NR), (CELL)from);
}


static sus_record *
has_been_suspended(Term goal, sus_record *from)
{
  do {
    if (from->SG == goal) {
      /* we found it */
      return (NULL);
    }
    if (from->NR == (sus_record *)&(from->NR))
      return (from);
    else from = from->NR;
  } while (TRUE);
  /* make lcc happy */
  return(NULL);
}

/* This is a simplified version for the case we add a goal to a
   suspended goal queue. It avoids having the same copy of the goal
   all over the place!
*/
inline static void AddSuspendedGoal(Term goal, sus_record *from)
{
  sus_record *gf;

  /* do nothing if we suspended before on the same goal! */
  if (IsApplTerm(goal) && ((from = has_been_suspended(goal, from)) == NULL))
    return;
  /* else add goal to the queue */
  gf = (sus_record *)H;
  H += sizeof(sus_record)/sizeof(CELL);
  gf->NR = (sus_record *)&(gf->NR);
  gf->SG = goal;
#ifdef MULTI_ASSIGNMENT_VARIABLES
  gf->NS = UpdateSVarList(gf);
#endif
  Bind_Global((CELL *)&(from->NR), (CELL)gf);
}

static sus_record *
copy_suspended_goals(sus_record *pt, CELL ***to_visit_ptr)
{
  CELL **to_visit = *to_visit_ptr;
  sus_record *gf;
  gf = (sus_record *)H;
  H += sizeof(sus_record)/sizeof(CELL);
  to_visit[0] = &(pt->SG)-1;
  to_visit[1] = &(pt->SG);
  to_visit[2] = &(gf->SG);
  *to_visit_ptr = to_visit+3;
#ifdef MULTI_ASSIGNMENT_VARIABLES
  gf->NS = UpdateSVarList(gf);
#endif
  if (pt->NR == (sus_record *)(&(pt->NR))) {
    gf->NR = (sus_record *)&(gf->NR);
  } else {
    gf->NR = copy_suspended_goals(pt->NR, to_visit_ptr);
  }
  return(gf);
}

static int
CopySuspendedVar(CELL *orig, CELL ***to_visit_ptr, CELL *res)
{
  register sus_tag *sreg = (sus_tag *)orig, *vs;

  /* add a new suspension */
  vs = (sus_tag *)Yap_ReadTimedVar(DelayedVars);
  if (H0 - (CELL *)vs < 1024)
    return(FALSE);
  RESET_VARIABLE(&(vs->ActiveSus));
  vs->sus_id = susp_ext;
  vs->SG = copy_suspended_goals(sreg->SG, to_visit_ptr);
  *res = (CELL)&(vs->ActiveSus);
  Yap_UpdateTimedVar(DelayedVars, (CELL)(vs+1));
  return(TRUE);
}

static Term
mk_sus_var_list(sus_record *sr, sus_record *osr)
{
  if (sr == osr)
    return(TermNil);
  return(MkPairTerm(sr->SG, mk_sus_var_list(sr->NR, sr)));
}

static Term
SuspendedVarToTerm(CELL *orig)
{
  register sus_tag *sreg = (sus_tag *)orig;

  return(MkPairTerm(sreg->SG->SG, mk_sus_var_list(sreg->SG->NR, sreg->SG)));
}

static sus_record *
terms_to_suspended_goals(Term gl)
{
  sus_record *gf;
  gf = (sus_record *)H;
  H += sizeof(sus_record)/sizeof(CELL);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  gf->NS = UpdateSVarList(gf);
#endif
  gf->SG = HeadOfTerm(gl);
  gl = TailOfTerm(gl);
  if (gl == TermNil) {
    gf->NR = (sus_record *)&(gf->NR);
  } else {
    gf->NR = terms_to_suspended_goals(gl);
  }
  return(gf);
}

static int
TermToSuspendedVar(Term gs, Term var)
{
  register sus_tag *vs;
  /* add a new suspension */
  vs = (sus_tag *)Yap_ReadTimedVar(DelayedVars);
  if (H0 - (CELL *)vs < 1024)
    return(FALSE);
  RESET_VARIABLE(&(vs->ActiveSus));
  vs->sus_id = susp_ext;
  vs->SG = terms_to_suspended_goals(gs);
  Yap_unify(var,(CELL)&(vs->ActiveSus));
  Yap_UpdateTimedVar(DelayedVars, (CELL)(vs+1));
  return(TRUE);
}


static void
mark_sus_record(sus_record *sg)
{
  if (MARKED(((CELL)(sg->NR))))
    return;
  MARK(((CELL *)&(sg->NR)));
  Yap_inc_mark_variable();
  Yap_mark_variable((CELL *)&(sg->SG));
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Yap_inc_mark_variable();
  if (!IsAtomTerm((CELL)(sg->NS)))
    mark_sus_record(sg->NS);
  MARK(((CELL *)&(sg->NS)));
#endif
}

static void mark_suspended_goal(CELL *orig)
{
  register sus_tag *sreg = (sus_tag *)orig;

  mark_sus_record(sreg->SG);
  Yap_mark_external_reference(((CELL *)&(sreg->SG)));
}


void
Yap_mark_all_suspended_goals(void)
{
  sus_record *sg = GetSVarList();
  if (sg == NULL)
    return;
  /* okay, we are on top of the list of variables. Let's burn rubber!
   */
  while (sg != (sus_record *)TermNil) {
    CELL tmp;
    mark_sus_record(sg);
    tmp = (CELL)(sg->NS);
    if (MARKED(tmp))
      sg = (sus_record *)UNMARK_CELL(tmp);
    else
      sg = (sus_record *)tmp;
  }
}


/*

   This routine does most of the work. It is called after
   someone tries to instantiate a suspension reference.

   Three operations are possible:

   SBIND:   trying to bind it to a constructed non-var term, most
	   often a primitive term;
   SISPAIR: the term is *going* to be bound to a list. We need to
	   return where.
   SISAPPL: the term is *going* to be bound to a compound term. We
	   need to return where, if we allow the binding.

*/

static void
Wake(CELL *pt1, CELL reg2)
{
  
  /* if bound to someone else, follow until we find the last one */
  register sus_tag *susp = (sus_tag *)pt1;
  CELL *myH = H;

  if (IsVarTerm(reg2)) {
    if (IsAttachedTerm(reg2)) {
      sus_tag *susp2 = (sus_tag *)VarOfTerm(reg2);

      /* binding two suspended variables, be careful */
      if (susp2->sus_id != susp_ext) {
	/* joining two suspensions */
	Yap_Error(SYSTEM_ERROR, TermNil, "joining two suspensions not implemented");
	return;
      }
      /* join the two suspended lists */
      if (susp2 > susp) {
	AddSuspendedGoals(susp->SG, susp2->SG);
	Bind_Global(VarOfTerm(reg2), (CELL)pt1);
	return;
      } else {
	AddSuspendedGoals(susp2->SG, susp->SG);
	Bind_Global(pt1, reg2);
	return;
      }
    } else {
      Bind(VarOfTerm(reg2), (CELL)pt1);
    }
  } else {
    /* release the variable into the WokenGoals list */
    ReleaseGoals(susp->SG);
    if (IsPairTerm(reg2) && RepPair(reg2) == myH)
	reg2 = AbsPair(H);
    else if (IsApplTerm(reg2) && RepAppl(reg2) == myH)
	reg2 = AbsAppl(H);
    /* bind it to t1's value */
    Bind_Global(pt1, reg2);
  }
  return;
}

/* find all goals frozen in the current chain of environments */

/* This will also mark them as bound, in order that goal lists
   won't be displayed twice */
static Term
AddSusToList(Term t, Term t1)
{
  if (IsVarTerm(t1)) {
    /* we found an active suspension variable */
    sus_tag * susp = (sus_tag *)VarOfTerm(t);    
    sus_record *s = susp->SG;
    while (s->NR != (sus_record *)&(s->NR)) {
      t = MkPairTerm(s->SG,t);
      s = s->NR;
    } while (s->NR != (sus_record *)&(s->NR));
    t = MkPairTerm(s->SG,t);
    Bind_Global((CELL *)(susp->ActiveSus), TermNil);
  } else if (IsApplTerm(t1)) {
    int args = ArityOfFunctor(FunctorOfTerm(t1));
    t = AddSusSubGoals(t, RepAppl(t1)+1, args);
  } else if (IsPairTerm(t1)) {
    t = AddSusSubGoals(t, RepPair(t1), 2);
  }
  return(t);
}

/* used to search from subarguments from within a compound term */
static Term
AddSusSubGoals(Term t, CELL *saved_var, int max)
{
  int i;

  for (i = 0; i < max; i++)
    {
      Term t1 = Derefa(saved_var);
      if (!IsVarTerm(t1)) {
	if (IsApplTerm(t1)) {
	  Functor f = FunctorOfTerm(t1);
	  
	  if (!IsExtensionFunctor(f)) {
	    int args = ArityOfFunctor(f);

	    t = AddSusSubGoals(t, RepAppl(t1)+1, args);
	  }
	} else if (IsPairTerm(t1)) {
	  t = AddSusSubGoals(t, RepPair(t1), 2);
	}
      } else {
	if (IsAttachedTerm(t1)) {
	  t = AddSusToList(t, t1);
	}
      }
    }
  return(t);
}

static Int
freeze_goal(Term t, Term g)
{
  if (IsVarTerm(t)) {
    sus_record *gf;
    sus_tag *vs;

    if (IsAttachedTerm(t)) {
      sus_tag *susp = (sus_tag *)VarOfTerm(t);
      exts id;

      id = (exts)(susp->sus_id);
      if (id != susp_ext) {
	/* obtain the term */
	Yap_Error(SYSTEM_ERROR,TermNil,"multiple suspensions not supported");
	return(FALSE);
      }

      AddSuspendedGoal(g, susp->SG);
      return(TRUE);
    }
    vs = (sus_tag *)Yap_ReadTimedVar(DelayedVars);
    if (H0 - (CELL *)vs < 1024) {
      ARG1 = t;
      ARG2 = g;
      if (!Yap_growglobal(NULL)) {
	Yap_Error(SYSTEM_ERROR, t, Yap_ErrorMessage);
	return FALSE;
      }
      t = ARG1;
      g = ARG2;
    }
    /* create a new suspension record */
    gf = (sus_record *)H;
    /* I assume here sus_record has size multiple of CELL !!!! */
    H += sizeof(sus_record)/sizeof(CELL);
    gf->NR = (sus_record *)&(gf->NR);
    gf->SG = g;
#ifdef MULTI_ASSIGNMENT_VARIABLES
    gf->NS = UpdateSVarList(gf);
#endif
    vs->sus_id = susp_ext;
    vs->SG = gf;
    RESET_VARIABLE(&(vs->ActiveSus));
    Yap_UpdateTimedVar(DelayedVars, (CELL)(vs+1));
    Bind_Global((CELL *)t,(CELL)&(vs->ActiveSus));
    return(TRUE);
  }
  else {
    /* Oops, first argument was bound :-( */
    Yap_Error(TYPE_ERROR_VARIABLE, t, "freeze/2");
    return(FALSE);
  }
}

#endif /* COROUTINING */

static Int
p_read_svar_list(void)
{
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
  return(Yap_unify(ARG1, MutableList) && Yap_unify(ARG2, AttsMutableList));
#else
  return(TRUE);
#endif
#else
  return(TRUE);
#endif
}

static Int
p_set_svar_list(void)
{
#ifdef COROUTINING
#ifdef MULTI_ASSIGNMENT_VARIABLES
  MutableList = Deref(ARG1);
  AttsMutableList = Deref(ARG2);
#endif
#endif
  return(TRUE);
}

static Int
p_freeze(void)
{
#ifdef COROUTINING
  Term t = Deref(ARG1);
  return(freeze_goal(t, Deref(ARG2)));
#else
  return(FALSE);
#endif /* COROUTINING */
}

/* The idea here is that we are trying to freeze on a list of
   variables. If we can freeze on the first one, we create a
   suspension record and are off to see the wizard of Oz. Otherwise,
   the goal fails, indicating we did not have to freeze (look at code
   for wait and for block to understand why.
*/
static Int p_freeze_on_first(void)
{
#ifdef COROUTINING
  Term r = Deref(ARG1);
  int i;
  CELL *pt;

  if (!IsApplTerm(r)) return(FALSE);
  i = ArityOfFunctor(FunctorOfTerm(r));
  pt = RepAppl(r)+1;
  do {
    if (IsNonVarTerm(Derefa(pt)))
      return(FALSE);
    i --;
    pt++;
  } while(i);
  /* we can freeze on the first variable */
  return(freeze_goal(Derefa(RepAppl(r)+1), Deref(ARG2)));
#else
  return(FALSE);
#endif
}


/* return a queue with goals currently frozen on the first argument */
static Int p_frozen_goals(void)
{
  /* initially, we do not know of any frozen goals */
  Term t = TermNil;
#ifdef COROUTINING
  Term t1 = Deref(ARG1);
  CELL *pt1;
  tr_fr_ptr pt0;
  /* make B and HB point to H to guarantee all bindings will
     be trailed
     */
  pt1 = (CELL *)B;
  pt0 = TR;
  HB = H;
  B = (choiceptr)H;
  /* look at the first argument */
  if (!IsVarTerm(t1)) {
    if (IsApplTerm(t1)) {
      Functor f = FunctorOfTerm(t1);
      int args;

      if (!IsExtensionFunctor(f)) {
	args = ArityOfFunctor(f);
	t = AddSusSubGoals(t, RepAppl(t1)+1, args);
      }
    } else if (IsPairTerm(t1)) {
      t = AddSusSubGoals(t, RepPair(t1), 2);
    }
  } else {
    if (IsAttachedTerm(t1)) {
      t = AddSusToList(t, t1);
    }
  }
  B = (choiceptr)pt1;
  /* untrail all bindings made by IUnify */
  while (TR != pt0) {
    pt1 = (CELL *)(TrailTerm(--TR));
    RESET_VARIABLE(pt1);
  }
  HB = B->cp_h;
#endif
  return(Yap_unify(ARG2,t));
}

/* return a queue with all goals frozen in the system */
static Int p_all_frozen_goals(void)
{
#ifdef COROUTINING
  /* initially, we do not know of any goals frozen */
  Term t = Yap_CurrentAttVars();
#ifdef MULTI_ASSIGNMENT_VARIABLES
  sus_record *x = GetSVarList();
  if (x == NULL)
    return(Yap_unify(ARG1,t));
  /* okay, we are on top of the list of variables. Let's burn rubber!
   */
  while ((CELL)x != TermNil) {
    t = MkPairTerm(x->SG,t);
    x = x->NS;
  }
#endif
  return(Yap_unify(ARG1,t));
#else
  return(Yap_unify(ARG1,TermNil));
#endif
}

#ifdef COROUTINING

/* check if variable was there */
static Term AddVarIfNotThere(Term var , Term dest)
{
  Term test = dest;
  while (test != TermNil) {
    if ((RepPair(test))[0] == var) return(dest);
    else test = (RepPair(test))[1];
  }
  return(MkPairTerm(var,dest));
}


/* This routine verifies whether two complex structures can unify. */
static int can_unify_complex(register CELL *pt0,
		register CELL *pt0_end,
		register CELL *pt1,
		Term  *Vars)
{

  /* This is really just unification, folks */
  tr_fr_ptr saved_TR;
  CELL *saved_HB;
  choiceptr saved_B;

  register CELL **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  CELL **to_visit_base = to_visit;

  /* make sure to trail all bindings */
  saved_TR = TR;
  saved_B = B;
  saved_HB = HB;
  HB = H;

 loop:
  while (pt0 < pt0_end) {
    register CELL d0, d1;
    ++ pt0;
    ++ pt1;
    d0 = Derefa(pt0);
    d1 = Derefa(pt1);
    if (IsVarTerm(d0)) {
      if (IsVarTerm(d1)) {
	if (d0 != d1) {
	  /* we need to suspend on both variables ! */
	  *Vars = AddVarIfNotThere(d0, AddVarIfNotThere(d1,*Vars));
	  /* bind the two variables, we would have to do that to unify
	     them */
	  if (d1 > d0) { /* youngest */
	    /* we don't want to wake up goals */
	    Bind_Global((CELL *)d1, d0);
	  } else {
	    Bind_Global((CELL *)d0, d1);
	  }
	}
	/* continue the loop */
	continue;
      }
      else {
	/* oh no, some more variables! */
	*Vars = AddVarIfNotThere(d0, *Vars);
      }
      /* now bind it */
      Bind_Global((CELL *)d0, d1);
      /* continue the loop */
    } else if (IsVarTerm(d1))  {
      *Vars = AddVarIfNotThere(d1, *Vars);
      /* and bind it */
      Bind_Global((CELL *)d1, d0);
      /* continue the loop */
    } else {
      if (d0 == d1) continue;
      if (IsAtomOrIntTerm(d0) || IsAtomOrIntTerm(d1)) {
	  if (d0 != d1) goto comparison_failed;
	/* else continue the loop */
      }
      else if (IsPairTerm(d0)) {
	if (!IsPairTerm(d1)) goto comparison_failed;
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	to_visit += 4;
	*pt0 = d1;
#else
	/* store the terms to visit */
	if (pt0 < pt0_end) {
	  to_visit[0] = pt0;
	  to_visit[1] = pt0_end;
	  to_visit[2] = pt1;
	  to_visit += 3;
	}
#endif
	pt0 = RepPair(d0) - 1;
	pt0_end = RepPair(d0) + 1;
	pt1 = RepPair(d1) - 1;
	continue;
      }
      else if (IsApplTerm(d0)) {
	register Functor f;
	register CELL *ap2, *ap3;
	if (!IsApplTerm(d1)) {
	  goto comparison_failed;
	} else {
	  /* store the terms to visit */
	  ap2 = RepAppl(d0);
	  ap3 = RepAppl(d1);
	  f = (Functor)(*ap2);
	  /* compare functors */
	  if (f != (Functor)*ap3) {
	    goto comparison_failed;
	  }
	  if (IsExtensionFunctor(f)) {
	    switch((CELL)f) {
	    case (CELL)FunctorDBRef:
	      if (d0 == d1) continue;
	      goto comparison_failed;
	    case (CELL)FunctorLongInt:
	      if (ap2[1] == ap3[1]) continue;
	      goto comparison_failed;
	    case (CELL)FunctorDouble:
	      if (FloatOfTerm(d0) == FloatOfTerm(d1)) continue;
	      goto comparison_failed;
#ifdef USE_GMP
	    case (CELL)FunctorBigInt:
	      if (mpz_cmp(Yap_BigIntOfTerm(d0),Yap_BigIntOfTerm(d1)) == 0) continue;
	      goto comparison_failed;
#endif /* USE_GMP */
	    default:
	      goto comparison_failed;
	    }
	  }
#ifdef RATIONAL_TREES
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = pt1;
	to_visit[3] = (CELL *)*pt0;
	to_visit += 4;
	*pt0 = d1;
#else
	  /* store the terms to visit */
	  if (pt0 < pt0_end) {
	    to_visit[0] = pt0;
	    to_visit[1] = pt0_end;
	    to_visit[2] = pt1;
	    to_visit += 3;
	  }
#endif
	  d0 = ArityOfFunctor(f);
	  pt0 = ap2;
	  pt0_end = ap2 + d0;
	  pt1 = ap3;
	  continue;
	}
      }

    }

  }
  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **)to_visit_base) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
#endif
    goto loop;
  }
  /* success */
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
  /* restore B, and later HB */
  B = saved_B;
  HB = saved_HB;
  /* untrail all bindings made by IUnify */
  while (TR != saved_TR) {
    pt1 = (CELL *)(TrailTerm(--TR));
    RESET_VARIABLE(pt1);
  }
  return(TRUE);

 comparison_failed:
  /* failure */
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
#ifdef RATIONAL_TREES
  while (to_visit > (CELL **)to_visit_base) {
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    pt1 = to_visit[2];
    *pt0 = (CELL)to_visit[3];
  }
#endif
  /* restore B, and later HB */
  B  = saved_B;
  HB = saved_HB;
  /* the system will take care of TR for me, no need to worry here! */
  return(FALSE);
}

static int
can_unify(Term t1, Term t2, Term *Vars)
{
  t1 = Deref(t1);
  t2 = Deref(t2);
  if (t1 == t2)
    return (TRUE);
  if (IsVarTerm(t1)) {
    /* we know for sure  they can't be different */
    if (IsVarTerm(t2)) {
      /* we need to suspend on both variables because otherwise
	 Y = susp(_) would not wakeup susp ! */
      *Vars = MkPairTerm(t1,MkPairTerm(t2,TermNil));
      return(TRUE);
    } else {
      *Vars = MkPairTerm(t1,TermNil);
      return(TRUE);
    }
  } else if (IsVarTerm(t2)) {
    /* wait until t2 is bound */
    *Vars = MkPairTerm(t2,TermNil);
    return(TRUE);
  }
  /* Two standard terms at last! */
  if (IsAtomOrIntTerm(t1) || IsAtomOrIntTerm(t2)) {
    /* Two primitive terms can only be equal if they are
       the same. If they are, $eq succeeds without further ado.
       */
    if (t1 != t2)
      return(FALSE);
    else {
      *Vars = TermNil;
      return(TRUE);
    }
  } else if (IsPairTerm(t1)) {
    if (IsPairTerm(t2)) {
      return(can_unify_complex(RepPair(t1)-1, RepPair(t1)+1,
			       RepPair(t2)-1, Vars));
    } else return(FALSE);
  } else {
    Functor f = FunctorOfTerm(t1);
    if (f != FunctorOfTerm(t2))
      return (FALSE);
    if (IsExtensionFunctor(f)) {
      switch((CELL)f) {
      case (CELL)FunctorDBRef:
	if (t1 == t2) return(FALSE);
	return(FALSE);
      case (CELL)FunctorLongInt:
	if (RepAppl(t1)[1] == RepAppl(t2)[1]) return(TRUE);
	return(FALSE);
      case (CELL)FunctorDouble:
	if (FloatOfTerm(t1) == FloatOfTerm(t2)) return(TRUE);
	return(FALSE);
#ifdef USE_GMP
      case (CELL)FunctorBigInt:
	if (mpz_cmp(Yap_BigIntOfTerm(t1),Yap_BigIntOfTerm(t2)) == 0) return(TRUE);
	return(FALSE);
#endif /* USE_GMP */
      default:
	return(FALSE);
      }
    }
    /* Two complex terms with the same functor */
    return(can_unify_complex(RepAppl(t1),
			     RepAppl(t1)+ArityOfFunctor(f),
			     RepAppl(t2), Vars));
  }
}

/* This routine verifies whether a complex has variables. */
static int non_ground_complex(register CELL *pt0,
		register CELL *pt0_end,
		Term  *Var)
{

  register CELL **to_visit = (CELL **)Yap_PreAllocCodeSpace();
  CELL **to_visit_base = to_visit;

 loop:
  while (pt0 < pt0_end) {
    register CELL d0;
    ++ pt0;
    d0 = Derefa(pt0);
    if (IsVarTerm(d0)) {
      *Var = d0;
      goto var_found;
    }
    if (IsPairTerm(d0)) {
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = (CELL *)*pt0;
      to_visit += 3;
      *pt0 = TermNil;
#else
      /* store the terms to visit */
      if (pt0 < pt0_end) {
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit += 2;
      }
#endif
      pt0 = RepPair(d0) - 1;
      pt0_end = RepPair(d0) + 1;
    }
    else if (IsApplTerm(d0)) {
      register Functor f;
      register CELL *ap2;

      /* store the terms to visit */
      ap2 = RepAppl(d0);
      f = (Functor)(*ap2);

      if (IsExtensionFunctor(f)) {
	continue;
      }
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = (CELL *)*pt0;
      to_visit += 3;
      *pt0 = TermNil;
#else
      /* store the terms to visit */
      if (pt0 < pt0_end) {
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit += 2;
      }
#endif
      d0 = ArityOfFunctor(f);
      pt0 = ap2;
      pt0_end = ap2 + d0;
    }
    /* just continue the loop */
  }

  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **)to_visit_base) {
#ifdef RATIONAL_TREES
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
#else
    to_visit -= 2;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
#endif
    goto loop;
  }

  /* the term is ground */
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
  return(FALSE);

 var_found:
  /* the term is non-ground */
  Yap_ReleasePreAllocCodeSpace((ADDR)to_visit);
#ifdef RATIONAL_TREES
  while (to_visit > (CELL **)to_visit_base) {
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    *pt0 = (CELL)to_visit[2];
  }
#endif
  /* the system will take care of TR for me, no need to worry here! */
  return(TRUE);
}

static int
non_ground(Term t, Term *Var)
{
  t = Deref(t);
  if (IsVarTerm(t)) {
    /* we found a variable */
    *Var = t;
    return(TRUE);
  }
  if (IsPrimitiveTerm(t)) {
    return(FALSE);
  } else if (IsPairTerm(t)) {
    return(non_ground_complex(RepPair(t)-1, RepPair(t)+1, Var));
  } else {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      return(FALSE);
    }
    return(non_ground_complex(RepAppl(t),
			  RepAppl(t)+ArityOfFunctor(FunctorOfTerm(t)),
			      Var));
  }
}

#endif

/* check whether the two terms unify and return what variables should
   be bound before the terms are execatly equal */
static Int p_can_unify(void)
{
#ifdef COROUTINING
  Term r = TermNil;
  if (!can_unify(ARG1, ARG2, &r))
    return(FALSE);
  return (Yap_unify(ARG3, r));
#else
  return(FALSE);
#endif
}

/* if the term is not ground return a variable in the term */
static Int p_non_ground(void)
{
#ifdef COROUTINING
  Term r;
  if (!non_ground(ARG1, &r))
    return(FALSE);
  return (Yap_unify(ARG2, r));
#else
  return(FALSE);
#endif
}

/* if the term is not ground return a variable in the term */
static Int p_coroutining(void)
{
#ifdef COROUTINING
  return(TRUE);
#else
  return(FALSE);
#endif
}

/* return a list of awoken goals */
static Int p_awoken_goals(void)
{
#ifdef COROUTINING
  Term WGs = Yap_ReadTimedVar(WokenGoals);
  if (WGs == TermNil) {
    return(FALSE);
  }
  WGs = ListOfWokenGoals();
  Yap_UpdateTimedVar(WokenGoals, TermNil);
  return(Yap_unify(ARG1,WGs));
#else
  return(FALSE);
#endif
}

#ifdef COROUTINING
void
Yap_WakeUp(CELL *pt0) {
  CELL d0 = *pt0;
  RESET_VARIABLE(pt0);
  TR--;
  attas[ExtFromCell(pt0)].bind_op(pt0, d0);
}
#endif


void Yap_InitCoroutPreds(void)
{
#ifdef COROUTINING
  Atom            at;
  PredEntry      *pred;

  attas[susp_ext].bind_op = Wake;
  attas[susp_ext].copy_term_op = CopySuspendedVar;
  attas[susp_ext].to_term_op = SuspendedVarToTerm;
  attas[susp_ext].term_to_op = TermToSuspendedVar;
  attas[susp_ext].mark_op = mark_suspended_goal;
  at = Yap_LookupAtom("$wake_up_goal");
  pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 2),0));
  WakeUpCode = pred;
  Yap_InitAttVarPreds();
#endif /* COROUTINING */
  Yap_InitCPred("$read_svar_list", 2, p_read_svar_list, SafePredFlag);
  Yap_InitCPred("$set_svar_list", 2, p_set_svar_list, SafePredFlag);
  Yap_InitCPred("$freeze", 2, p_freeze, 0);
  Yap_InitCPred("freeze_on_first", 2, p_freeze_on_first, TestPredFlag);
  Yap_InitCPred("$frozen_goals", 2, p_frozen_goals, SafePredFlag);
  Yap_InitCPred("$all_frozen_goals", 1, p_all_frozen_goals, SafePredFlag);
  Yap_InitCPred("$can_unify", 3, p_can_unify, SafePredFlag);
  Yap_InitCPred("$non_ground", 2, p_non_ground, SafePredFlag);
  Yap_InitCPred("$coroutining", 0, p_coroutining, SafePredFlag);
  Yap_InitCPred("$awoken_goals", 1, p_awoken_goals, SafePredFlag);
}


