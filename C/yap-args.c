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
* Comments:	Yap's Main File: parse arguments			 *
*									 *
*************************************************************************/
/* static char SccsId[] = "X 4.3.3"; */

#include "Yap.h"
#include "YapHeap.h"
#include "YapInterface.h"
#include "config.h"
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STDINT_H
#include <stdint.h>
#endif
#include <stddef.h>
#include <stdlib.h>
#ifdef _MSC_VER /* Microsoft's Visual C++ Compiler */
#ifdef HAVE_UNISTD_H
#undef HAVE_UNISTD_H
#endif
#endif

#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif
#if HAVE_ERRNO_H
#include <errno.h>
#endif
#if HAVE_DIRECT_H
#include <direct.h>
#endif

static void print_usage(void) {
  fprintf(stderr, "\n[ Valid switches for command line arguments: ]\n");
  fprintf(stderr, "  -?   Shows this screen\n");
  fprintf(stderr, "  -b   Boot file \n");
  fprintf(stderr, "  -dump-runtime-variables\n");
  fprintf(stderr, "  -f   initialization file or \"none\"\n");
  fprintf(stderr, "  -g   Run Goal Before Top-Level \n");
  fprintf(stderr, "  -z   Run Goal Before Top-Level \n");
  fprintf(stderr, "  -q   start with informational messages off\n");
  fprintf(stderr, "  -l   load Prolog file\n");
  fprintf(stderr, "  -L   run Prolog file and exit\n");
  fprintf(stderr, "  -p   extra path for file-search-path\n");
  fprintf(stderr, "  -hSize   Heap area in Kbytes (default: %d, minimum: %d)\n",
          DefHeapSpace, MinHeapSpace);
  fprintf(stderr,
          "  -sSize   Stack area in Kbytes (default: %d, minimum: %d)\n",
          DefStackSpace, MinStackSpace);
  fprintf(stderr,
          "  -tSize   Trail area in Kbytes (default: %d, minimum: %d)\n",
          DefTrailSpace, MinTrailSpace);
  fprintf(stderr, "  -GSize  Max Area for Global Stack\n");
  fprintf(stderr,
          "  -LSize   Max Area for Local Stack (number must follow L)\n");
  fprintf(stderr, "  -TSize   Max Area for Trail (number must follow T)\n");
  fprintf(stderr, "  -nosignals   disable signal handling from Prolog\n");
  fprintf(stderr, "\n[Execution Modes]\n");
  fprintf(stderr, "  -J0  Interpreted mode (default)\n");
  fprintf(stderr, "  -J1  Mixed mode only for user predicates\n");
  fprintf(stderr, "  -J2  Mixed mode for all predicates\n");
  fprintf(stderr, "  -J3  Compile all user predicates\n");
  fprintf(stderr, "  -J4  Compile all predicates\n");

#ifdef TABLING
  fprintf(stderr,
          "  -ts  Maximum table space area in Mbytes (default: unlimited)\n");
#endif /* TABLING */
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA) ||         \
    defined(YAPOR_THREADS)
  fprintf(stderr, "  -w   Number of workers (default: %d)\n",
          DEFAULT_NUMBERWORKERS);
  fprintf(stderr, "  -sl  Loop scheduler executions before look for hiden "
                  "shared work (default: %d)\n",
          DEFAULT_SCHEDULERLOOP);
  fprintf(stderr, "  -d   Value of delayed release of load (default: %d)\n",
          DEFAULT_DELAYEDRELEASELOAD);
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA || YAPOR_THREADS */
  /* nf: Preprocessor */
  /* fprintf(stderr,"  -DVar=Name   Persistent definition\n"); */
  fprintf(stderr, "\n");
}

static int myisblank(int c) {
  switch (c) {
  case ' ':
  case '\t':
  case '\n':
  case '\r':
    return TRUE;
  default:
    return FALSE;
  }
}

static char *add_end_dot(char arg[]) {
  int sz = strlen(arg), i;
  i = sz;
  while (i && myisblank(arg[--i]))
    ;
  if (i && arg[i] != ',') {
    char *p = (char *)malloc(sz + 2);
    if (!p)
      return NULL;
    strncpy(p, arg, sz);
    p[sz] = '.';
    p[sz + 1] = '\0';
    return p;
  }
  return arg;
}

static int dump_runtime_variables(void) {
  fprintf(stdout, "CC=\"%s\"\n", C_CC);
  fprintf(stdout, "YAP_ROOTDIR=\"%s\"\n", YAP_ROOTDIR);
  fprintf(stdout, "YAP_LIBS=\"%s\"\n", C_LIBS);
  fprintf(stdout, "YAP_SHLIB_SUFFIX=\"%s\"\n", SO_EXT);
  fprintf(stdout, "YAP_VERSION=%s\n", YAP_NUMERIC_VERSION);
  exit(0);
  return 1;
}

YAP_file_type_t Yap_InitDefaults(YAP_init_args *iap, char saved_state[],
                                 int argc, char *argv[]) {
  memset(iap, 0, sizeof(YAP_init_args));
#if __ANDROID__
  iap->boot_file_type = YAP_BOOT_PL;
  iap->SavedState = NULL;
#else
  iap->boot_file_type = YAP_QLY;
  iap->SavedState = saved_state;
#endif
  iap->Argc = argc;
  iap->Argv = argv;
  return iap->boot_file_type;
}

X_API YAP_file_type_t YAP_parse_yap_arguments(int argc, char *argv[],
                                              YAP_init_args *iap) {
  char *p;
  size_t *ssize;

  Yap_InitDefaults(iap, NULL, argc, argv);
  while (--argc > 0) {
    p = *++argv;
    if (*p == '-')
      switch (*++p) {
      case 'b':
        iap->boot_file_type = YAP_PL;
        if (p[1])
          iap->YapPrologBootFile = p + 1;
        else if (argv[1] && *argv[1] != '-') {
          iap->YapPrologBootFile = *++argv;
          argc--;
        } else {
          iap->YapPrologBootFile = "boot.yap";
        }
        break;
      case 'B':
        iap->boot_file_type = YAP_BOOT_PL;
        if (p[1])
          iap->YapPrologBootFile = p + 1;
        else if (argv[1] && *argv[1] != '-') {
          iap->YapPrologBootFile = *++argv;
          argc--;
        } else {
          iap->YapPrologBootFile = "boot.yap";
        }
        break;
      case '?':
        print_usage();
        exit(EXIT_SUCCESS);
      case 'q':
        iap->QuietMode = TRUE;
        break;
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA) ||         \
    defined(YAPOR_THREADS)
      case 'w':
        ssize = &(iap->NumberWorkers);
        goto GetSize;
      case 'd':
        if (!strcmp("dump-runtime-variables", p))
          return dump_runtime_variables();
        ssize = &(iap->DelayedReleaseLoad);
        goto GetSize;
#else
      case 'd':
        if (!strcmp("dump-runtime-variables", p))
          return dump_runtime_variables();
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA || YAPOR_THREADS */
      case 'F':
        /* just ignore for now */
        argc--;
        argv++;
        break;
      case 'f':
        iap->FastBoot = TRUE;
        if (argc > 1 && argv[1][0] != '-') {
          argc--;
          argv++;
          if (strcmp(*argv, "none")) {
            iap->YapPrologRCFile = *argv;
          }
          break;
        }
        break;
      // execution mode
      case 'J':
        switch (p[1]) {
        case '0':
          iap->ExecutionMode = YAPC_INTERPRETED;
          break;
        case '1':
          iap->ExecutionMode = YAPC_MIXED_MODE_USER;
          break;
        case '2':
          iap->ExecutionMode = YAPC_MIXED_MODE_ALL;
          break;
        case '3':
          iap->ExecutionMode = YAPC_COMPILE_USER;
          break;
        case '4':
          iap->ExecutionMode = YAPC_COMPILE_ALL;
          break;
        default:
          fprintf(stderr, "[ YAP unrecoverable error: unknown switch -%c%c ]\n",
                  *p, p[1]);
          exit(EXIT_FAILURE);
        }
        p++;
        break;
      case 'G':
        ssize = &(iap->MaxGlobalSize);
        goto GetSize;
        break;
      case 's':
      case 'S':
        ssize = &(iap->StackSize);
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA) ||         \
    defined(YAPOR_THREADS)
        if (p[1] == 'l') {
          p++;
          ssize = &(iap->SchedulerLoop);
        }
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA || YAPOR_THREADS */
        goto GetSize;
      case 'a':
      case 'A':
        ssize = &(iap->AttsSize);
        goto GetSize;
      case 'T':
        ssize = &(iap->MaxTrailSize);
        goto get_trail_size;
      case 't':
        ssize = &(iap->TrailSize);
#ifdef TABLING
        if (p[1] == 's') {
          p++;
          ssize = &(iap->MaxTableSpaceSize);
        }
#endif /* TABLING */
      get_trail_size:
        if (*++p == '\0') {
          if (argc > 1)
            --argc, p = *++argv;
          else {
            fprintf(stderr,
                    "[ YAP unrecoverable error: missing size in flag %s ]",
                    argv[0]);
            print_usage();
            exit(EXIT_FAILURE);
          }
        }
        {
          unsigned long int i = 0, ch;
          while ((ch = *p++) >= '0' && ch <= '9')
            i = i * 10 + ch - '0';
          switch (ch) {
          case 'M':
          case 'm':
            i *= 1024;
            ch = *p++;
            break;
          case 'g':
            i *= 1024 * 1024;
            ch = *p++;
            break;
          case 'k':
          case 'K':
            ch = *p++;
            break;
          }
          if (ch) {
            iap->YapPrologTopLevelGoal = add_end_dot(*argv);
          } else {
            *ssize = i;
          }
        }
        break;
      case 'h':
      case 'H':
        ssize = &(iap->HeapSize);
      GetSize:
        if (*++p == '\0') {
          if (argc > 1)
            --argc, p = *++argv;
          else {
            fprintf(stderr,
                    "[ YAP unrecoverable error: missing size in flag %s ]",
                    argv[0]);
            print_usage();
            exit(EXIT_FAILURE);
          }
        }
        {
          unsigned long int i = 0, ch;
          while ((ch = *p++) >= '0' && ch <= '9')
            i = i * 10 + ch - '0';
          switch (ch) {
          case 'M':
          case 'm':
            i *= 1024;
            ch = *p++;
            break;
          case 'g':
          case 'G':
            i *= 1024 * 1024;
            ch = *p++;
            break;
          case 'k':
          case 'K':
            ch = *p++;
            break;
          }
          if (ch) {
            fprintf(
                stderr,
                "[ YAP unrecoverable error: illegal size specification %s ]",
                argv[-1]);
            Yap_exit(1);
          }
          *ssize = i;
        }
        break;
#ifdef DEBUG
      case 'P':
        if (p[1] != '\0') {
          while (p[1] != '\0') {
            int ch = p[1];
            if (ch >= 'A' && ch <= 'Z')
              ch += ('a' - 'A');
            if (ch >= 'a' && ch <= 'z')
              GLOBAL_Option[ch - 96] = 1;
            p++;
          }
        } else {
          YAP_SetOutputMessage();
        }
        break;
#endif
      case 'L':
        if (p[1] && p[1] >= '0' &&
            p[1] <= '9') /* hack to emulate SWI's L local option */
        {
          ssize = &(iap->MaxStackSize);
          goto GetSize;
        }
        iap->QuietMode = TRUE;
        iap->HaltAfterConsult = TRUE;
      case 'l':
        p++;
        if (!*++argv) {
          fprintf(stderr,
                  "%% YAP unrecoverable error: missing load file name\n");
          exit(1);
        } else if (!strcmp("--", *argv)) {
          /* shell script, the next entry should be the file itself */
          iap->YapPrologRCFile = argv[1];
          argc = 1;
          break;
        } else {
          iap->YapPrologRCFile = *argv;
          argc--;
        }
        if (*p) {
          /* we have something, usually, of the form:
             -L --
             FileName
             ExtraArgs
          */
          /* being called from a script */
          while (*p && (*p == ' ' || *p == '\t'))
            p++;
          if (p[0] == '-' && p[1] == '-') {
            /* ignore what is next */
            argc = 1;
          }
        }
        break;
      /* run goal before top-level */
      case 'g':
        if ((*argv)[0] == '\0')
          iap->YapPrologGoal = *argv;
        else {
          argc--;
          if (argc == 0) {
            fprintf(stderr, " [ YAP unrecoverable error: missing "
                            "initialization goal for option 'g' ]\n");
            exit(EXIT_FAILURE);
          }
          argv++;
          iap->YapPrologGoal = *argv;
        }
        break;
      /* run goal as top-level */
      case 'z':
        if ((*argv)[0] == '\0')
          iap->YapPrologTopLevelGoal = *argv;
        else {
          argc--;
          if (argc == 0) {
            fprintf(
                stderr,
                " [ YAP unrecoverable error: missing goal for option 'z' ]\n");
            exit(EXIT_FAILURE);
          }
          argv++;
          iap->YapPrologTopLevelGoal = add_end_dot(*argv);
        }
        break;
      case 'n':
        if (!strcmp("nosignals", p)) {
          iap->PrologCannotHandleInterrupts = true;
          break;
        }
        break;
      case '-':
        if (!strcmp("-nosignals", p)) {
          iap->PrologCannotHandleInterrupts = true;
          break;
        } else if (!strncmp("-home=", p, strlen("-home="))) {
          GLOBAL_Home = p + strlen("-home=");
        } else if (!strncmp("-cwd=", p, strlen("-cwd="))) {
#if __WINDOWS__
          if (_chdir(p + strlen("-cwd=")) < 0) {
#else
          if (chdir(p + strlen("-cwd=")) < 0) {
#endif
            fprintf(stderr, " [ YAP unrecoverable error in setting cwd: %s ]\n",
                    strerror(errno));
          }
        } else if (!strncmp("-stack=", p, strlen("-stack="))) {
          ssize = &(iap->StackSize);
          p += strlen("-stack=");
          goto GetSize;
        } else if (!strncmp("-trail=", p, strlen("-trail="))) {
          ssize = &(iap->TrailSize);
          p += strlen("-trail=");
          goto GetSize;
        } else if (!strncmp("-heap=", p, strlen("-heap="))) {
          ssize = &(iap->HeapSize);
          p += strlen("-heap=");
          goto GetSize;
        } else if (!strncmp("-goal=", p, strlen("-goal="))) {
          iap->YapPrologGoal = p + strlen("-goal=");
        } else if (!strncmp("-top-level=", p, strlen("-top-level="))) {
          iap->YapPrologTopLevelGoal = p + strlen("-top-level=");
        } else if (!strncmp("-table=", p, strlen("-table="))) {
          ssize = &(iap->MaxTableSpaceSize);
          p += strlen("-table=");
          goto GetSize;
        } else if (!strncmp("-", p, strlen("-="))) {
          ssize = &(iap->MaxTableSpaceSize);
          p += strlen("-table=");
          /* skip remaining arguments */
          argc = 1;
        }
        break;
      case 'p':
        if ((*argv)[0] == '\0')
          iap->YapPrologAddPath = *argv;
        else {
          argc--;
          if (argc == 0) {
            fprintf(
                stderr,
                " [ YAP unrecoverable error: missing paths for option 'p' ]\n");
            exit(EXIT_FAILURE);
          }
          argv++;
          iap->YapPrologAddPath = *argv;
        }
        break;
      /* nf: Begin preprocessor code */
      case 'D': {
        char *var, *value;
        ++p;
        var = p;
        if (var == NULL || *var == '\0')
          break;
        while (*p != '=' && *p != '\0')
          ++p;
        if (*p == '\0')
          break;
        *p = '\0';
        ++p;
        value = p;
        if (*value == '\0')
          break;
        if (iap->def_c == YAP_MAX_YPP_DEFS)
          break;
        iap->def_var[iap->def_c] = var;
        iap->def_value[iap->def_c] = value;
        ++(iap->def_c);
        break;
      }
      /* End preprocessor code */
      default: {
        fprintf(stderr, "[ YAP unrecoverable error: unknown switch -%c ]\n",
                *p);
        print_usage();
        exit(EXIT_FAILURE);
      }
      }
    else {
      iap->SavedState = p;
    }
  }
  return iap->boot_file_type;
}
