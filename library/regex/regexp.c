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
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include "c_interface.h"
#if HAVE_REGEX_H
#include "regex.h"
#define yap_regcomp(A,B,C) regcomp(A,B,C)
#define yap_regexec(A,B,C,D,E) regexec(A,B,C,D,E)
#define yap_regfree(A) regfree(A)
#define yap_regerror(A,B,C,D) regfree(A,B,C,D)
#else
#include "yapregex.h"
#endif
/* for the sake of NULL */
#include <stdio.h>

void PROTO(init_regexp, (void));

static int check_regexp(void) 
{
  unsigned int buflen = (unsigned int)IntOfTerm(ARG2)+1;
  unsigned int sbuflen = (unsigned int)IntOfTerm(ARG4)+1;
  char *buf, *sbuf;
  regex_t reg;
  int out;
  int yap_flags = IntOfTerm(ARG5), regcomp_flags = REG_NOSUB|REG_EXTENDED;
  
  if ((buf = (char *)AllocSpaceFromYap(buflen)) == NULL) {
    /* early exit */
    return(FALSE);
  }
  if (StringToBuffer(ARG1,buf,buflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    FreeSpaceFromYap(buf);
    return(FALSE);
  }
  if (yap_flags & 1)
    regcomp_flags |= REG_ICASE;
  /* cool, now I have my string in the buffer, let's have some fun */
  if (yap_regcomp(&reg,buf, regcomp_flags) != 0)
    return(FALSE);
  if ((sbuf = (char *)AllocSpaceFromYap(sbuflen)) == NULL) {
    /* early exit */
    yap_regfree(&reg);
    FreeSpaceFromYap(buf);
    return(FALSE);
  }
  if (StringToBuffer(ARG3,sbuf,sbuflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    yap_regfree(&reg);
    FreeSpaceFromYap(buf);
    FreeSpaceFromYap(sbuf); 
    return(FALSE);
  }
  out = yap_regexec(&reg,sbuf,0,NULL,0);
  yap_regfree(&reg);
  FreeSpaceFromYap(buf);
  FreeSpaceFromYap(sbuf); 
  if (out != 0 && out != REG_NOMATCH) {
    return(FALSE);
  }
  return(out == 0);
}

static int regexp(void) 
{
  unsigned int buflen = (unsigned int)IntOfTerm(ARG2)+1;
  unsigned int sbuflen = (unsigned int)IntOfTerm(ARG4)+1;
  char *buf, *sbuf;
  regex_t reg;
  int out;
  Int nmatch = IntOfTerm(ARG7);
  regmatch_t *pmatch;
  Term tout;
  int yap_flags = IntOfTerm(ARG5), regcomp_flags = REG_EXTENDED;
  
  if ((buf = (char *)AllocSpaceFromYap(buflen)) == NULL) {
    /* early exit */
    return(FALSE);
  }
  if (StringToBuffer(ARG1,buf,buflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    FreeSpaceFromYap(buf);
    return(FALSE);
  }
  if (yap_flags & 1)
    regcomp_flags |= REG_ICASE;
  /* cool, now I have my string in the buffer, let's have some fun */
  if (yap_regcomp(&reg,buf, regcomp_flags) != 0)
    return(FALSE);
  if ((sbuf = (char *)AllocSpaceFromYap(sbuflen)) == NULL) {
    /* early exit */
    yap_regfree(&reg);
    FreeSpaceFromYap(buf);
    return(FALSE);
  }
  if (StringToBuffer(ARG3,sbuf,sbuflen) == FALSE) {
    /* something went wrong, possibly a type checking error */
    yap_regfree(&reg);
    FreeSpaceFromYap(buf);
    FreeSpaceFromYap(sbuf); 
    return(FALSE);
  }
  pmatch = AllocSpaceFromYap(sizeof(regmatch_t)*nmatch);
  out = yap_regexec(&reg,sbuf,(int)nmatch,pmatch,0);
  if (out == 0) {
    /* match succeed, let's fill the match in */
    Int i;
    Term TNil = MkAtomTerm(LookupAtom("[]"));
    Functor FDiff = MkFunctor(LookupAtom("-"),2);

    tout = ARG6;
    for (i = 0; i < nmatch; i++) {
      int j;
      Term t = TNil;

      if (pmatch[i].rm_so == -1) break;
      if (yap_flags & 2) {
	Term to[2];
	to[0] = MkIntTerm(pmatch[i].rm_so);
	to[1] = MkIntTerm(pmatch[i].rm_eo);
	t = MkApplTerm(FDiff,2,to);
      } else {
	for (j = pmatch[i].rm_eo-1; j >= pmatch[i].rm_so; j--) {
	  t = MkPairTerm(MkIntTerm(sbuf[j]),t);
	}
      }
      unify(t,HeadOfTerm(tout));
      tout = TailOfTerm(tout);
    }
  }
  else if (out != REG_NOMATCH) {
    return(FALSE);
  }
  yap_regfree(&reg);
  FreeSpaceFromYap(buf);
  FreeSpaceFromYap(sbuf); 
  FreeSpaceFromYap(pmatch); 
  return(out == 0);
}

void
init_regexp(void)
{
  UserCPredicate("check_regexp", check_regexp, 5);
  UserCPredicate("check_regexp", regexp, 7);
}

#if defined(_WIN32) || defined(__MINGW32__)

#include <windows.h>

int WINAPI PROTO(win_regexp, (HANDLE, DWORD, LPVOID));

int WINAPI win_regexp(HANDLE hinst, DWORD reason, LPVOID reserved)
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
