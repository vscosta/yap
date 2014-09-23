/*************************************************************************
*									 *
*	 Yap Prolog 							 *
*									 *
*	Yap Prolog Was Developed At Nccup - Universidade Do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa And Universidade Do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		Yap.C							 *
* Last Rev:								 *
* Mods:									 *
* Comments:	Yap's Main File						 *
*									 *
*************************************************************************/
/* static char SccsId[] = "X 4.3.3"; */

#include "config.h"
#include "YapInterface.h"

#include "cut_c.h"

#ifdef _MSC_VER /* Microsoft's Visual C++ Compiler */
#ifdef  HAVE_UNISTD_H
#undef  HAVE_UNISTD_H
#endif
#endif

#include <stdio.h>
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_CTYPE_H
#include <ctype.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

static void do_top_goal(YAP_Term Goal);
static void exec_top_level(int BootMode, YAP_init_args *iap);

#ifdef lint
/* VARARGS1 */
#endif

#ifdef M_WILLIAMS
long _stksize = 32000;
#endif

#ifdef USE_MYPUTC
static void
myputc (int ch)
{
  putc(ch,stderr);
}
#endif

static void
do_top_goal (YAP_Term Goal)
{
  YAP_RunGoalOnce(Goal);
}

static int
init_standard_system(int argc, char *argv[], YAP_init_args *iap)
{
  int BootMode;

  BootMode = YAP_parse_yap_arguments(argc,argv,iap);

  /* init memory */
  if (BootMode == YAP_BOOT_FROM_PROLOG ||
      BootMode == YAP_FULL_BOOT_FROM_PROLOG) {
    int NewBootMode = YAP_Init(iap);
    if (NewBootMode != YAP_BOOT_FROM_PROLOG && BootMode != YAP_FULL_BOOT_FROM_PROLOG)
      BootMode = NewBootMode;
  } else {
    BootMode = YAP_Init(iap);
  }
  if (iap->ErrorNo) {
    /* boot failed */
    YAP_Error(iap->ErrorNo,0L,iap->ErrorCause);
  }
  return BootMode;
}


static void
exec_top_level(int BootMode, YAP_init_args *iap)
{
  YAP_Term atomfalse;
  YAP_Atom livegoal;

  if (BootMode == YAP_BOOT_FROM_SAVED_STACKS)
    {
      /* continue executing from the frozen stacks */
      YAP_ContinueGoal();
    }
  /* the top-level is now ready */

  /* read it before case someone, that is, Ashwin, hides
     the atom false away ;-).
  */
  livegoal = YAP_FullLookupAtom("$live");
  atomfalse = YAP_MkAtomTerm (YAP_FullLookupAtom("$false"));
  while (YAP_GetValue (livegoal) != atomfalse) {
    YAP_Reset( YAP_FULL_RESET );
    do_top_goal (YAP_MkAtomTerm (livegoal));
  }
  YAP_Exit(EXIT_SUCCESS);
}

FILE *debugf;

#ifdef LIGHT
int
_main (int argc, char **argv)
#else
int
main (int argc, char **argv)
#endif
{
  int BootMode;
  YAP_init_args init_args;
  int i;

#if DEBUG_LOCKS
  char buf[1024];
  sprintf(buf, "/tmp/yap%d", getpid());
  debugf= fopen(buf, "w");
  if (!debugf) fprintf(stderr,"ERROR %s\n", strerror(errno));
  setvbuf( debugf,NULL, _IOLBF, 1024);
#endif
  BootMode = init_standard_system(argc, argv, &init_args);
  if (BootMode == YAP_BOOT_ERROR) {
    fprintf(stderr,"[ FATAL ERROR: could not find saved state ]\n");
    exit(1);
  }
  /* Begin preprocessor code */
  if (BootMode != YAP_BOOT_FROM_SAVED_STACKS) {
    // process the definitions
    for(i=0;i<init_args.def_c;++i) {
      YAP_Term t_args[2],t_goal;
      t_args[0] = YAP_MkAtomTerm(YAP_LookupAtom(init_args.def_var[i]));
      t_args[1] = YAP_MkAtomTerm(YAP_LookupAtom(init_args.def_value[i])); 
      t_goal  = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("ypp_define"),2), 2, t_args); 
      YAP_RunGoalOnce(t_goal);
    }
  }
  YAP_Reset( YAP_FULL_RESET );
  /* End preprocessor code */

  exec_top_level(BootMode, &init_args);

  return(0);
}

