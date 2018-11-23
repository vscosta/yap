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
 * File:		regexp.c * Last rev:
 ** mods: * comments:	regular expression interpreter *
 *									 *
 *************************************************************************/

/**
 * @file regexp.c
 *
 * A port of the Unix regular expression compiler.
 *
 * @namespace regexp
 *
 */

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "YapInterface.h"
#if HAVE_REGEXEC
#include "regex.h"
#define yap_regcomp(A, B, C) regcomp(A, B, C)
#define yap_regexec(A, B, C, D, E) regexec(A, B, C, D, E)
#define yap_regfree(A) regfree(A)
#define yap_regerror(A, B, C, D) regfree(A, B, C, D)
#else
#include "yapregex.h"
#endif
/* for the sake of NULL */
#include <stdio.h>

void init_regexp(void);

static YAP_Bool check_regexp(void) {
  unsigned int buflen = (unsigned int)YAP_IntOfTerm(YAP_ARG2) + 1;
  unsigned int sbuflen = (unsigned int)YAP_IntOfTerm(YAP_ARG4) + 1;
  char *buf, *sbuf;
  regex_t reg;
  int out;
  int yap_flags = YAP_IntOfTerm(YAP_ARG5);
  int regcomp_flags = REG_NOSUB | REG_EXTENDED;

  if ((buf = (char *)YAP_AllocSpaceFromYap(buflen)) == NULL) {
    /* early exit */
    return (FALSE);
  }
  if (YAP_StringToBuffer(YAP_ARG1, buf, buflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    YAP_FreeSpaceFromYap(buf);
    return (FALSE);
  }
  if (yap_flags & 1)
    regcomp_flags |= REG_ICASE;
  /* cool, now I have my string in the buffer, let's have some fun */
  if (yap_regcomp(&reg, buf, regcomp_flags) != 0)
    return (FALSE);
  if ((sbuf = (char *)YAP_AllocSpaceFromYap(sbuflen)) == NULL) {
    /* early exit */
    yap_regfree(&reg);
    YAP_FreeSpaceFromYap(buf);
    return (FALSE);
  }
  if (YAP_StringToBuffer(YAP_ARG3, sbuf, sbuflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    yap_regfree(&reg);
    YAP_FreeSpaceFromYap(buf);
    YAP_FreeSpaceFromYap(sbuf);
    return (FALSE);
  }
  out = yap_regexec(&reg, sbuf, 0, NULL, 0);
  yap_regfree(&reg);
  YAP_FreeSpaceFromYap(buf);
  YAP_FreeSpaceFromYap(sbuf);
  if (out != 0 && out != REG_NOMATCH) {
    return (FALSE);
  }
  return (out == 0);
}

static YAP_Bool regexp(void) {
  unsigned int buflen = (unsigned int)YAP_IntOfTerm(YAP_ARG2) + 1;
  unsigned int sbuflen = (unsigned int)YAP_IntOfTerm(YAP_ARG4) + 1;
  char *buf, *sbuf;
  regex_t reg;
  int out;
  size_t nmatch;
  regmatch_t *pmatch;
  long int tout;
  int yap_flags = YAP_IntOfTerm(YAP_ARG5);
  int regcomp_flags = REG_EXTENDED;

  if ((buf = (char *)YAP_AllocSpaceFromYap(buflen)) == NULL) {
    /* early exit */
    return (FALSE);
  }
  if (YAP_StringToBuffer(YAP_ARG1, buf, buflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    YAP_FreeSpaceFromYap(buf);
    return (FALSE);
  }
  if (yap_flags & 1)
    regcomp_flags |= REG_ICASE;
  /* cool, now I have my string in the buffer, let's have some fun */
  if (yap_regcomp(&reg, buf, regcomp_flags) != 0) {
    YAP_FreeSpaceFromYap(buf);
    return (FALSE);
  }
  if (YAP_IsVarTerm(YAP_ARG7)) {
    nmatch = reg.re_nsub;
  } else {
    nmatch = YAP_IntOfTerm(YAP_ARG7);
  }
  if ((sbuf = (char *)YAP_AllocSpaceFromYap(sbuflen)) == NULL) {
    /* early exit */
    yap_regfree(&reg);
    YAP_FreeSpaceFromYap(buf);
    return (FALSE);
  }
  if (YAP_StringToBuffer(YAP_ARG3, sbuf, sbuflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    yap_regfree(&reg);
    YAP_FreeSpaceFromYap(buf);
    YAP_FreeSpaceFromYap(sbuf);
    return (FALSE);
  }
  pmatch = YAP_AllocSpaceFromYap(sizeof(regmatch_t) * (nmatch));
  out = yap_regexec(&reg, sbuf, nmatch, pmatch, 0);
  if (out == 0) {
    /* match succeed, let's fill the match in */
    long int i;
    YAP_Term TNil = YAP_MkAtomTerm(YAP_LookupAtom("[]"));
    YAP_Functor FDiff = YAP_MkFunctor(YAP_LookupAtom("-"), 2);

    tout = TNil;
    for (i = nmatch - 1; i >= 0; --i) {
      int j;
      YAP_Term t = TNil;

      if (pmatch[i].rm_so != -1) {
        if (yap_flags & 2) {
          YAP_Term to[2];
          to[0] = YAP_MkIntTerm(pmatch[i].rm_so);
          to[1] = YAP_MkIntTerm(pmatch[i].rm_eo);
          t = YAP_MkApplTerm(FDiff, 2, to);
        } else {
          for (j = pmatch[i].rm_eo - 1; j >= pmatch[i].rm_so; j--) {
            t = YAP_MkPairTerm(YAP_MkIntTerm(sbuf[j]), t);
          }
        }
        tout = YAP_MkPairTerm(t, tout);
      }
    }
    out = !YAP_Unify(tout, YAP_ARG6);
  } else if (out != REG_NOMATCH) {
    out = 0;
  }
  yap_regfree(&reg);
  YAP_FreeSpaceFromYap(buf);
  YAP_FreeSpaceFromYap(sbuf);
  YAP_FreeSpaceFromYap(pmatch);
  return (out == 0);
}

void init_regexp(void) {
  YAP_UserCPredicate("check_regexp", check_regexp, 5);
  YAP_UserCPredicate("check_regexp", regexp, 7);
}

#if __WINDOWS__

#include <windows.h>

int WINAPI win_regexp(HANDLE, DWORD, LPVOID);

int WINAPI win_regexp(HANDLE hinst, DWORD reason, LPVOID reserved) {
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
