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
* File:		random.c						 *
* Last rev:								 *
* mods:									 *
* comments:	regular expression interpreter                           *
*									 *
*************************************************************************/

#include "config.h"
#include "YapInterface.h"
#include <math.h>
#if defined(__MINGW32__) || _MSC_VER
#include <windows.h>
#endif

void PROTO(init_matrices, (void));

static int
int_min_of_matrix(void)
{
  Term t = YAP_ARG1;
  YAP_UInt dim = YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)), i, pos;
  YAP_Int *mat = (YAP_Int *)malloc( dim*sizeof(YAP_Int) ), min;
  if (!mat)
    return FALSE;
  if (!YAP_ArgsToIntArray(t, dim, mat))
    return FALSE;
  
  min = mat[0];
  pos = 0;
  for (i = 1; i < dim; i++) {
    if (mat[i] < min) {
      min = mat[i];
      pos = i;
    }
  }
  return YAP_unify(YAP_ARG2, YAP_MkIntTerm(min)) &&
    YAP_unify(YAP_ARG3, YAP_MkIntTerm(pos));
}

static int
float_min_of_matrix(void)
{
  Term t = YAP_ARG1;
  YAP_UInt dim = YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)), i, pos;
  YAP_Float *mat = (YAP_Float *)malloc( dim*sizeof(YAP_Float) ), min;

  if (!mat)
    return FALSE;
  if (!YAP_ArgsToFloatArray(t, dim, mat))
    return FALSE;
  
  min = mat[0];
  pos = 0;
  for (i = 1; i < dim; i++) {
    if (mat[i] < min) {
      min = mat[i];
      pos = i;
    }
  }
  return YAP_unify(YAP_ARG2, YAP_MkFloatTerm(min)) &&
    YAP_unify(YAP_ARG3, YAP_MkIntTerm(pos));
}

static int
int_max_of_matrix(void)
{
  Term t = YAP_ARG1;
  YAP_UInt dim = YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)), i, pos;
  YAP_Int *mat = (YAP_Int *)malloc( dim*sizeof(YAP_Int) ), max;

  if (!mat)
    return FALSE;
  if (!YAP_ArgsToIntArray(t, dim, mat))
    return FALSE;
  
  max = mat[0];
  pos = 0;
  for (i = 1; i < dim; i++) {
    if (mat[i] > max) {
      max = mat[i];
      pos = i;
    }
  }
  return YAP_unify(YAP_ARG2, YAP_MkIntTerm(min)) &&
    YAP_unify(YAP_ARG3, YAP_MkIntTerm(pos));
}

static int
float_max_of_matrix(void)
{
  Term t = YAP_ARG1;
  YAP_UInt dim = YAP_ArityOfFunctor(YAP_FunctorOfTerm(t)), i, pos;
  YAP_Float *mat = (YAP_Float *)malloc( dim*sizeof(YAP_Float) ), max;

  if (!mat)
    return FALSE; 
 if (!YAP_ArgsToFloatArray(t, dim, mat))
    return FALSE;
  
  max = mat[0];
  pos = 0;
  for (i = 1; i < dim; i++) {
    if (mat[i] > max) {
      max = mat[i];
      pos = i;
    }
  }
  return YAP_unify(YAP_ARG2, YAP_MkFloatTerm(min)) &&
    YAP_unify(YAP_ARG3, YAP_MkIntTerm(pos));
}

void
init_matrices(void)
{
  YAP_UserCPredicate("int_max_of_matrix", int_max_of_matrix, 3);
  YAP_UserCPredicate("float_max_of_matrix", float_max_of_matrix, 3);
  YAP_UserCPredicate("int_min_of_matrix", int_min_of_matrix, 3);
  YAP_UserCPredicate("float_min_of_matrix", float_min_of_matrix, 3);
}

#ifdef _WIN32

int WINAPI PROTO(win_matrices, (HANDLE, DWORD, LPVOID));

int WINAPI win_matrices(HANDLE hinst, DWORD reason, LPVOID reserved)
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
