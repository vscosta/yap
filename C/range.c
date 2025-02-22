/*************************************************************************
*									 *
*	 YAP Prolog 							 *
*									 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V. Santos Costa and Universidade do Porto 1985--	 *
*									 *
**************************************************************************
*									 *
* File:		range.c							 *
* comments:	Arithmetic interval computation				 *
*									 *
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "YapEval.h"

static Int
p_in_range( USES_REGS1 ) {
  Term t;
  double i,j;
  double d1;
  double d2;
  double d3;

  t = Deref(ARG1);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(ARG4);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  d1 = i-j;
  t = Deref(ARG2);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(ARG5);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  d2 = i-j;
  t = Deref(ARG3);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(ARG6);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  d3 = i-j;
  t = Deref(ARG7);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(ARG8);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  
  return fabs(sqrt(d1*d1 + d2*d2 + d3*d3)-i) <= j; 

}

static Int
p_in_range2( USES_REGS1 ) {
  CELL *p1, *p2;
  Term t;
  double i,j;
  double d1;
  double d2;
  double d3;
  UInt arity;
  p1 = RepAppl(Deref(ARG1));
  arity = ArityOfFunctor((Functor)*p1);
  p1 += arity-2;
  p2 = RepAppl(Deref(ARG2))+(arity-2);;

  t = Deref(p1[0]);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(p2[0]);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  d1 = i-j;
  t = Deref(p1[1]);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(p2[1]);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  d2 = i-j;
  t = Deref(p1[2]);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(p2[2]);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  d3 = i-j;
  t = Deref(ARG3);
  if (IsFloatTerm(t)) i = FloatOfTerm(t); else i = IntegerOfTerm(t); 
  t = Deref(ARG4);
  if (IsFloatTerm(t)) j = FloatOfTerm(t); else j = IntegerOfTerm(t);
  
  return fabs(sqrt(d1*d1 + d2*d2 + d3*d3)-i) <= j; 
}

static Int
p_euc_dist( USES_REGS1 ) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  double d1 = (double)(IntegerOfTerm(ArgOfTerm(1,t1))-IntegerOfTerm(ArgOfTerm(1,t2)));
  double d2 = (double)(IntegerOfTerm(ArgOfTerm(2,t1))-IntegerOfTerm(ArgOfTerm(2,t2)));
  double d3 = (double)(IntegerOfTerm(ArgOfTerm(3,t1))-IntegerOfTerm(ArgOfTerm(3,t2)));
  Int result = (Int)sqrt(d1*d1+d2*d2+d3*d3);
  return(Yap_unify(ARG3,MkIntegerTerm(result)));
}

#if DEBUG
volatile int loop_counter = 0;

static Int
p_loop( USES_REGS1 ) {
  while (loop_counter == 0);
  return(TRUE);
}
#endif

void 
Yap_InitRange(void)
{
  CACHE_REGS
  Term cm = CurrentModule;
  CurrentModule = RANGE_MODULE;
  Yap_InitCPred("euclidean_distance", 3, p_euc_dist, SafePredFlag);
#ifdef DEBUG
  Yap_InitCPred("loop", 0, p_loop, SafePredFlag);
#endif
  Yap_InitCPred("in_range", 8, p_in_range, TestPredFlag|SafePredFlag);
  Yap_InitCPred("in_range", 4, p_in_range2, TestPredFlag|SafePredFlag);
  CurrentModule = cm;
}
