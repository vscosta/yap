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
* File:		eval.c							 *
* Last rev:								 *
* mods:									 *
* comments:	arithmetical expression evaluation			 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

/*
 * This file implements arithmetic operations 
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"


#define E_FUNC   blob_type
#define E_ARGS   arith_retptr o

#define RINT(v)     (o)->Int = v; return(long_int_e)
#define RFLOAT(v)   (o)->dbl = v; return(double_e)
#define RERROR()    return(db_ref_e)

#ifndef PI
#ifdef M_PI
#define PI M_PI
#else
#define PI 3.14159265358979323846
#endif
#endif

static E_FUNC
p_pi(E_ARGS)
{
  RFLOAT(PI);
}

#ifndef M_E
#define M_E 2.7182818284590452354
#endif

static E_FUNC
p_e(E_ARGS)
{
  RFLOAT(M_E);
}

#ifndef INFINITY
#define INFINITY (1.0/0.0)
#endif

static E_FUNC
p_inf(E_ARGS)
{
#ifdef _MSC_VER /* Microsoft's Visual C++ Compiler */
    Error(TYPE_ERROR_EVALUABLE, TermNil, "evaluating infinity");
    P = (yamop *)FAILCODE;
    RERROR();
#else
  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {/* iso */
    Error(TYPE_ERROR_EVALUABLE, TermNil, "evaluating infinity");
    P = (yamop *)FAILCODE;
    RERROR();
  } else {
    RFLOAT(INFINITY);
  }
#endif
}

#ifndef NAN
#define NAN      (0.0/0.0)
#endif


static E_FUNC
p_nan(E_ARGS)
{
#ifdef _MSC_VER /* Microsoft's Visual C++ Compiler */
    Error(TYPE_ERROR_EVALUABLE, TermNil, "evaluating infinity");
    P = (yamop *)FAILCODE;
    RERROR();
#else
  if (yap_flags[LANGUAGE_MODE_FLAG] == 1) {/* iso */
    Error(TYPE_ERROR_EVALUABLE, TermNil, "evaluating not-a-number");
    P = (yamop *)FAILCODE;
    RERROR();
  } else {
    RFLOAT(NAN);
  }
#endif
}

static E_FUNC
p_random(E_ARGS)
{
  RFLOAT(yap_random());
}

static E_FUNC
p_cputime(E_ARGS)
{
  RFLOAT((Float)cputime()/1000.0);
}

static E_FUNC
p_heapused(E_ARGS)
{
  RINT(HeapUsed);
}

static E_FUNC
p_localsp(E_ARGS)
{
#if SBA
  RINT((Int)ASP);
#else
  RINT(LCL0 - ASP);
#endif
}

static E_FUNC
p_b(E_ARGS)
{
#if SBA
  RINT((Int)B);
#else
  RINT(LCL0 - (CELL *)B);
#endif
}

static E_FUNC
p_env(E_ARGS)
{
#if SBA
  RINT((Int)YENV);
#else
  RINT(LCL0 - YENV);
#endif
}

static E_FUNC
p_tr(E_ARGS)
{
#if SBA
  RINT(TR);
#else
  RINT(((CELL *)TR)-LCL0);
#endif
}

static E_FUNC
p_globalsp(E_ARGS)
{
#if SBA
  RINT((Int)H);
#else
  RINT(H - H0);
#endif
}

static E_FUNC
p_stackfree(E_ARGS)
{
  RINT(Unsigned(ASP) - Unsigned(H));
}

typedef blob_type (*f_constexp)(arith_retptr);

typedef struct init_const_eval {
  char          *OpName;
  f_constexp        f;
} InitConstEntry;


static InitConstEntry InitConstTab[] = {
  {"pi", p_pi},
  {"e", p_e},
  {"inf", p_inf},
  {"nan", p_nan},
  {"random", p_random},
  {"cputime", p_cputime},
  {"heapused", p_heapused},
  {"local_sp", p_localsp},
  {"global_sp", p_globalsp},
  {"$last_choice_pt", p_b},
  {"$env", p_env},
  {"$tr", p_tr},
  {"stackfree", p_stackfree},
};

void
InitConstExps(void)
{
  unsigned int    i;
  ExpEntry       *p;

  for (i = 0; i < sizeof(InitConstTab)/sizeof(InitConstEntry); ++i) {
    AtomEntry *ae = RepAtom(LookupAtom(InitConstTab[i].OpName));
    WRITE_LOCK(ae->ARWLock);
    if (GetExpPropHavingLock(ae, 0)) {
      WRITE_UNLOCK(ae->ARWLock);
      break;
    }
    p = (ExpEntry *) AllocAtomSpace(sizeof(ExpEntry));
    p->KindOfPE = ExpProperty;
    p->ArityOfEE = 0;
    p->ENoOfEE = 0;
    p->FOfEE.constant = InitConstTab[i].f;
    p->NextOfPE = ae->PropsOfAE;
    ae->PropsOfAE = AbsExpProp(p);
    WRITE_UNLOCK(ae->ARWLock);
  }
}

/* This routine is called from Restore to make sure we have the same arithmetic operators */
int
ReInitConstExps(void)
{
  unsigned int i;
  Prop p;

  for (i = 0; i < sizeof(InitConstTab)/sizeof(InitConstEntry); ++i) {
    AtomEntry *ae = RepAtom(FullLookupAtom(InitConstTab[i].OpName));

    WRITE_LOCK(ae->ARWLock);
    if ((p = GetExpPropHavingLock(ae, 0)) == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      return(FALSE);
    }
    RepExpProp(p)->FOfEE.constant = InitConstTab[i].f;
    WRITE_UNLOCK(ae->ARWLock);
  }
  return(TRUE);
}

