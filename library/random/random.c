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
* File:		regexp.c						 *
* Last rev:								 *
* mods:									 *
* comments:	regular expression interpreter                           *
*									 *
*************************************************************************/

#include "config.h"
#include "c_interface.h"
#include <math.h>
#if defined(__MINGW32__) || _MSC_VER
#include <windows.h>
#endif

void PROTO(init_random, (void));

static short a1 = 27314, b1 = 9213, c1 = 17773;

static int
p_random(void)
{
  flt fli;
  Int t1, t2, t3;

  t1 = (a1 * 171) % 30269;
  t2 = (b1 * 172) % 30307;
  t3 = (c1 * 170) % 30323;
  fli = (t1/30269.0) + (t2/30307.0) + (t3/30323.0);
  a1 = t1;
  b1 = t2;
  c1 = t3;
  return(unify(ARG1, MkFloatTerm(fli-(int)(fli))));
}

static int
p_setrand(void)
{
  a1 = IntOfTerm(ARG1);
  b1 = IntOfTerm(ARG2);
  c1 = IntOfTerm(ARG3);
  return(TRUE);
}

static int
p_getrand(void)
{
  return(unify(ARG1,MkIntTerm(a1)) &&
	 unify(ARG2,MkIntTerm(b1)) &&
	 unify(ARG3,MkIntTerm(c1)));
}

void
init_random(void)
{
  UserCPredicate("random", p_random, 1);
  UserCPredicate("setrand", p_setrand, 3);
  UserCPredicate("getrand", p_getrand, 3);
}

#ifdef _WIN32

int WINAPI PROTO(win_random, (HANDLE, DWORD, LPVOID));

int WINAPI win_random(HANDLE hinst, DWORD reason, LPVOID reserved)
{
  switch (reason) 
    {
    case DLL_PROCESS_ATTACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    }
  return 1;
}
#endif
