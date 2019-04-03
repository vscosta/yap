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
 * File:		Yap.C * Last Rev:
 ** Mods: * Comments:	Yap's Main File *
 *									 *
 *************************************************************************/
/* static char SccsId[] = "X 4.3.3"; */

#include "YapConfig.h"
#include "YapInterface.h"

#include "cut_c.h"

#ifdef _MSC_VER /* Microsoft's Visual C++ Compiler */
#undef HAVE_UNISTD_H
#endif
#ifdef _WIN32 /* Microsoft's Visual C++ Compiler */
#include <io.h>
#include <windows.h>
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

static bool do_top_goal(YAP_Term Goal);
static bool exec_top_level(int BootMode, YAP_init_args *iap);

#ifdef lint
/* VARARGS1 */
#endif

#ifdef M_WILLIAMS
long _stksize = 32000;
#endif

static bool do_top_goal(YAP_Term Goal) { return YAP_RunGoalOnce(Goal); }

static int init_standard_system(int argc, char *argv[], YAP_init_args *iap) {

  YAP_file_type_t BootMode;

  BootMode = YAP_parse_yap_arguments(argc, argv, iap);
  iap->Embedded = false;
  /* init memory */
  iap->boot_file_type = BootMode;
  YAP_Init(iap);
  if (iap->ErrorNo) {
    /* boot failed */
    YAP_Error(iap->ErrorNo, 0L, iap->ErrorCause);
  }
  return BootMode;
}

static bool exec_top_level(int BootMode, YAP_init_args *iap) {
  YAP_Term atomfalse;
  YAP_Atom livegoal;

  if (iap->install)
    return true;
  if (BootMode == YAP_BOOT_FROM_SAVED_STACKS) {
    /* continue executing from the frozen stacks */
    YAP_ContinueGoal();
  }
  livegoal = YAP_FullLookupAtom("live");
  /* the top-level is now ready */

  /* read it before case someone, that is, Ashwin, hides
     the atom false away ;-).
  */
  atomfalse = YAP_MkAtomTerm(YAP_FullLookupAtom("false"));
  while (YAP_GetValue(livegoal) != atomfalse) {
    YAP_Reset(YAP_FULL_RESET, false);
    if (!do_top_goal(YAP_MkAtomTerm(livegoal))) {
      return false;
    };
    livegoal = YAP_FullLookupAtom("live");
  }
  return true;
  // YAP_Exit(EXIT_SUCCESS);
}

// FILE *debugf;

#ifdef LIGHT

int _main(int argc, char **argv)
#else
int main(int argc, char **argv)
#endif
{
  int BootMode;
  int i;
  YAP_init_args init_args;
  BootMode = init_standard_system(argc, argv, &init_args);

  if (BootMode == YAP_BOOT_ERROR) {
    fprintf(stderr, "[ FATAL ERROR: could not find saved state ]\n");
    exit(1);
  }
  /* Begin preprocessor code */
  if (BootMode != YAP_BOOT_FROM_SAVED_STACKS) {
    // process the definitions
    for (i = 0; i < init_args.def_c; ++i) {
      YAP_Term t_args[2], t_goal;
      t_args[0] = YAP_MkAtomTerm(YAP_LookupAtom(init_args.def_var[i]));
      t_args[1] = YAP_MkAtomTerm(YAP_LookupAtom(init_args.def_value[i]));
      t_goal = YAP_MkApplTerm(YAP_MkFunctor(YAP_LookupAtom("ypp_define"), 2), 2,
                              t_args);
      YAP_RunGoalOnce(t_goal);
    }
  }

  YAP_Reset(YAP_FULL_RESET, false);
  /* End preprocessor code */

  //mtrace();
  bool rc = exec_top_level(BootMode, &init_args);
  if (!rc)
    return 1;
  return 0;
}
