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
 * File:		random.c * Last rev:
 ** mods: * comments:	regular expression interpreter *
 *									 *
 *************************************************************************/

#include "YapInterface.h"
#include <math.h>
#if defined(__MINGW32__) || _MSC_VER
#include <windows.h>
#endif

X_API void init_random(void);

static short a1 = 27314, b1 = 9213, c1 = 17773;

static YAP_Bool p_random(void) {
  double fli;
  long int t1, t2, t3;

  t1 = (a1 * 171) % 30269;
  t2 = (b1 * 172) % 30307;
  t3 = (c1 * 170) % 30323;
  fli = (t1 / 30269.0) + (t2 / 30307.0) + (t3 / 30323.0);
  a1 = t1;
  b1 = t2;
  c1 = t3;
  return (YAP_Unify(YAP_ARG1, YAP_MkFloatTerm(fli - (int)(fli))));
}

static YAP_Bool p_setrand(void) {
  a1 = YAP_IntOfTerm(YAP_ARG1);
  b1 = YAP_IntOfTerm(YAP_ARG2);
  c1 = YAP_IntOfTerm(YAP_ARG3);
  return (TRUE);
}

static YAP_Bool p_getrand(void) {
  return (YAP_Unify(YAP_ARG1, YAP_MkIntTerm(a1)) &&
          YAP_Unify(YAP_ARG2, YAP_MkIntTerm(b1)) &&
          YAP_Unify(YAP_ARG3, YAP_MkIntTerm(c1)));
}

X_API void init_random(void) {
  YAP_UserCPredicate("random", p_random, 1);
  YAP_UserCPredicate("setrand", p_setrand, 3);
  YAP_UserCPredicate("getrand", p_getrand, 3);
}

#ifdef _WIN32

int WINAPI win_random(HANDLE, DWORD, LPVOID);

int WINAPI win_random(HANDLE hinst, DWORD reason, LPVOID reserved) {
  switch (reason) {
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
