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
 * File:		Yap.C * Last
 *Rev:
 * Mods:
 ** Comments:	Yap's Main File: parse arguments			 *
 *									 *
 *************************************************************************/
/* static char SccsId[] = "X 4.3.3"; */

#include "Yap.h"
#include "YapHeap.h"
#include "YapInterface.h"
#include "YapStreams.h"
#include "config.h"
#include "iopreds.h"

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

#if HAVE_LIBGEN_H
#include <libgen.h>

#endif

X_API bool YAP_initialized = false;
static int n_mdelays = 0;
static YAP_delaymodule_t *m_delays;

static void init_globals(YAP_init_args *yap_init) {
  GLOBAL_FAST_BOOT_FLAG = yap_init->FastBoot;
#if defined(YAPOR) || defined(TABLING)

  Yap_init_root_frames();

#endif /* YAPOR || TABLING */
#ifdef YAPOR
  Yap_init_yapor_workers();
#if YAPOR_THREADS
  if (Yap_thread_self() != 0) {
#else
  if (worker_id != 0) {
#endif
#if defined(YAPOR_COPY) || defined(YAPOR_SBA)
    /*
      In the SBA we cannot just happily inherit registers
      from the other workers
    */
    Yap_InitYaamRegs(worker_id, true);
#endif /* YAPOR_COPY || YAPOR_SBA */
#ifndef YAPOR_THREADS
    Yap_InitPreAllocCodeSpace(0);
#endif /* YAPOR_THREADS */
    /* slaves, waiting for work */
    CurrentModule = USER_MODULE;
    P = GETWORK_FIRST_TIME;
    Yap_exec_absmi(FALSE, YAP_EXEC_ABSMI);
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "abstract machine unexpected exit (YAP_Init)");
  }
#endif /* YAPOR */
  RECOVER_MACHINE_REGS();
  /* make sure we do this after restore */
  if (yap_init->MaxStackSize) {
    GLOBAL_AllowLocalExpansion = FALSE;
  } else {
    GLOBAL_AllowLocalExpansion = TRUE;
  }
  if (yap_init->MaxGlobalSize) {
    GLOBAL_AllowGlobalExpansion = FALSE;
  } else {
    GLOBAL_AllowGlobalExpansion = TRUE;
  }
  if (yap_init->MaxTrailSize) {
    GLOBAL_AllowTrailExpansion = FALSE;
  } else {
    GLOBAL_AllowTrailExpansion = TRUE;
  }
  if (yap_init->PrologRCFile) {
    Yap_PutValue(AtomConsultOnBoot,
                 MkAtomTerm(Yap_LookupAtom(yap_init->PrologRCFile)));
    /*
      This must be done again after restore, as yap_flags
      has been overwritten ....
    */
    setBooleanGlobalPrologFlag(HALT_AFTER_CONSULT_FLAG,
                               yap_init->HaltAfterConsult);
  }
  if (yap_init->PrologTopLevelGoal) {
    Yap_PutValue(AtomTopLevelGoal,
                 MkAtomTerm(Yap_LookupAtom(yap_init->PrologTopLevelGoal)));
  }
  if (yap_init->PrologGoal) {
    Yap_PutValue(AtomInitGoal,
                 MkAtomTerm(Yap_LookupAtom(yap_init->PrologGoal)));
  }
  if (yap_init->PrologAddPath) {
    Yap_PutValue(AtomExtendFileSearchPath,
                 MkAtomTerm(Yap_LookupAtom(yap_init->PrologAddPath)));
  }

  if (yap_init->QuietMode) {
    setVerbosity(TermSilent);
  }
}


const char *Yap_BINDIR, *Yap_ROOTDIR, *Yap_SHAREDIR, *Yap_LIBDIR, *Yap_DLLDIR,
    *Yap_PLDIR, *Yap_BOOTSTRAP, *Yap_COMMONSDIR,
  *Yap_STARTUP, *Yap_INPUT_STARTUP, *Yap_OUTPUT_STARTUP, *Yap_BOOTFILE, *Yap_INCLUDEDIR;

/* do initial boot by consulting the file boot.yap */
static void consult(const char *b_file USES_REGS) {
  Term t;
  int c_stream, osno, oactive;
  Functor functor_query = Yap_MkFunctor(Yap_LookupAtom("?-"), 1);
  Functor functor_command1 = Yap_MkFunctor(Yap_LookupAtom(":-"), 1);
  Functor functor_compile2 = Yap_MkFunctor(Yap_LookupAtom("c_compile"), 1);

  /* consult boot.pl */
  int lvl = push_text_stack();
  char *full = Malloc(YAP_FILENAME_MAX + 1);
  full[0] = '\0';
  /* the consult mode does not matter here, really */
  if ((osno = Yap_CheckAlias(AtomLoopStream)) < 0)
    osno = 0;
  c_stream = YAP_InitConsult(YAP_BOOT_MODE, b_file, full, &oactive);
  if (c_stream < 0) {
    fprintf(stderr, "[ FATAL ERROR: could not open file %s ]\n", b_file);
      pop_text_stack(lvl);
    exit(1);
  }
  if (!Yap_AddAlias(AtomLoopStream, c_stream)) {
    pop_text_stack(lvl);
    return;
  }

  do {
    CACHE_REGS
    YAP_Reset(YAP_FULL_RESET, false);
    Yap_StartSlots();
      __android_log_print(ANDROID_LOG_INFO, "YAPDroid", "read %s <%d>", b_file, GLOBAL_Stream[c_stream].linecount);
    Term vs = YAP_MkVarTerm(), pos = MkVarTerm();
    t = YAP_ReadClauseFromStream(c_stream, vs, pos);
    // Yap_GetNèwSlot(t);
    if (t == 0) {
      fprintf(stderr, "[ SYNTAX ERROR: while parsing stream %s at line %ld ]\n",
              b_file, GLOBAL_Stream[c_stream].linecount);
    } else if (IsVarTerm(t) || t == TermNil) {
      fprintf(stderr, "[ line: " Int_FORMAT ": term cannot be compiled ]",
              GLOBAL_Stream[c_stream].linecount);
    } else if (IsApplTerm(t) && (FunctorOfTerm(t) == functor_query ||
                                 FunctorOfTerm(t) == functor_command1)) {
      t = ArgOfTerm(1, t);
      if (IsApplTerm(t) && FunctorOfTerm(t) == functor_compile2) {
        consult(RepAtom(AtomOfTerm(ArgOfTerm(1, t)))->StrOfAE);
      } else {
        YAP_RunGoalOnce(t);
      }
    } else {
      char *ErrorMessage;
      ErrorMessage = YAP_CompileClause(t);
      if (ErrorMessage) {
        fprintf(stderr, "%s", ErrorMessage);
      }
    }
  } while (t != TermEof);
  BACKUP_MACHINE_REGS();
  YAP_EndConsult(c_stream, &osno, full);
  pop_text_stack(lvl);
}

///
///
  static const char * sel(bool dir, ...) {
  CACHE_REGS
  va_list args;
  va_start(args ,dir);
  while (true) {
  bool init = va_arg(args, int);
    const char *fmt = va_arg(args, char *);
    if (init) {
      va_end(args);
      return fmt;

    }
  }
  return NULL;
}

static const char * join(const char *s, ...) {
  CACHE_REGS
  va_list args;
  va_start(args , s);
  int lvl = push_text_stack();
  char *buf = Malloc(FILENAME_MAX+1);
  if (s && s[0])
    strcpy(buf, s);
  while (true) {
    const char *fmt = va_arg(args, char *);
    if (fmt == NULL) {
      va_end(args);
      return pop_output_text_stack(lvl,buf);	
    }
    strcat(buf, fmt);
  }
}

 static void Yap_set_locations(YAP_init_args *iap) {

  /// ROOT_DIR is the home of the YAP system. It can be:
  /// -- provided by the user;
  /// -- obtained  from DESTDIR + DE=efalkRoot
  ///
  /// It is:
  //  --_not useful in Android, WIN32;
  /// -- DESTDIR/ in Anaconda
  /// -- /usr/local in most Unix style systems
  Yap_ROOTDIR = sel(true, iap->ROOTDIR != NULL, iap->ROOTDIR,
		       true,
#if __ANDROID__
			NULL,
#else
		    join(getenv("DESTDIR"),YAP_ROOTDIR, NULL),
#endif
		    false
		    );
  /// BINDIR: where the OS stores header files, namely libYap...
  Yap_BINDIR = sel(true,
		   iap->BINDIR != NULL, iap->BINDIR,
		   true,
#if __ANDROID__
		   NULL,
#else
		   join(getenv("DESTDIR"),YAP_BINDIR, NULL),
#endif
		      false    );
  /// LIBDIR: where the OS stores dynamic libraries, namely libYap...
  Yap_LIBDIR = sel(true,
		      iap->LIBDIR != NULL, iap->LIBDIR,
		      true,
#if __ANDROID__
		      NULL,
#else
		   join(getenv("DESTDIR"),YAP_LIBDIR,NULL),
#endif
		      false    );
  /// DLLDIR: where libraries can find expicitely loaded DLLs
  Yap_DLLDIR = sel(true,
		      iap->DLLDIR != NULL, iap->DLLDIR,
		      true,
#if __ANDROID__
			NULL
#else
		   join(getenv("DESTDIR"),YAP_DLLDIR, NULL),
#endif
			false    );
  /// INCLUDEDIR: where the OS stores header files, namely libYap...
  Yap_INCLUDEDIR = sel(true,
		      iap->INCLUDEDIR != NULL, iap->INCLUDEDIR,
		      true,
#if __ANDROID__
		      NULL,
#else
		       join(getenv("DESTDIR"),YAP_INCLUDEDIR, NULL),
#endif
		      false    );
  /// SHAREDIR: where OS & ARCH independent files live
  Yap_SHAREDIR = sel(true,
		      iap->SHAREDIR != NULL, iap->SHAREDIR,
		      true,
#if __ANDROID__
			"/assets",
#else
		      join(getenv("DESTDIR"),YAP_SHAREDIR, NULL),
#endif
			false    );
  /// PLDIR: where we can find Prolog files
  Yap_PLDIR = sel(true,
		      iap->PLDIR != NULL, iap->PLDIR,
		      true,
#if __ANDROID__
		      "/assets/Yap",
#else
		  join(getenv("DESTDIR"),YAP_PLDIR, NULL),
#endif
		     false    );
  /// ``COMMONSDIR: Prolog Commons
 Yap_COMMONSDIR = sel(true,
		      iap->COMMONSDIR != NULL, iap->COMMONSDIR,
		      true,
#if __ANDROID__
		      "/assets/PrologCommons",
#else
		      join(getenv("DESTDIR"),YAP_SHAREDIR "/PrologCommons", NULL),
#endif
			false    );
 /// BOOTPLDIR: where we can find Prolog bootstrap files
   Yap_BOOTSTRAP = sel(true,
		      iap->BOOTSTRAP != NULL, iap->BOOTSTRAP,
		      true,
#if __ANDROID__
		      "/assets/Yap/pl",
#else
		     join(getenv("DESTDIR"),YAP_BOOTSTRAP, NULL),
#endif
			false    );
 /// BOOTFILE: where we can find the core Prolog bootstrap file
 Yap_BOOTFILE = sel(false,
		      iap->BOOTFILE != NULL, iap->BOOTFILE,
		      true,
#if __ANDROID__
		      "/assets/Yap/pl/boot.yap",
#else
		    join(getenv("DESTDIR"),YAP_BOOTFILE, NULL),
#endif
			false    );
 /// STARTUP: where we can find the core Prolog bootstrap file
   Yap_OUTPUT_STARTUP = sel(false,
                            iap->OUTPUT_STARTUP != NULL, iap->OUTPUT_STARTUP,
                            true,
#if __ANDROID__
           NULL,
#else
                            join(getenv("DESTDIR"),YAP_OUTPUT_STARTUP,NULL),
#endif
                            false    );
   Yap_INPUT_STARTUP = sel(false,
                            iap->INPUT_STARTUP != NULL, iap->INPUT_STARTUP,
                            true,
#if __ANDROID__
           NULL,
#else
                            join(getenv("DESTDIR"),YAP_INPUT_STARTUP,NULL),
#endif
                            false    );
 if (Yap_ROOTDIR)
    setAtomicGlobalPrologFlag(HOME_FLAG,
                              MkAtomTerm(Yap_LookupAtom(Yap_ROOTDIR)));
  if (Yap_PLDIR)
    setAtomicGlobalPrologFlag(PROLOG_LIBRARY_DIRECTORY_FLAG,
                              MkAtomTerm(Yap_LookupAtom(Yap_PLDIR)));
  if (Yap_DLLDIR)
    setAtomicGlobalPrologFlag(PROLOG_FOREIGN_DIRECTORY_FLAG,
                              MkAtomTerm(Yap_LookupAtom(Yap_DLLDIR)));
}

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
  fprintf(stderr,
          "  -sl  Loop scheduler executions before look for hiden "
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

X_API YAP_file_type_t Yap_InitDefaults(void *x, char *saved_state, int argc,
                                       char *argv[]) {

  if (!LOCAL_TextBuffer)
    LOCAL_TextBuffer = Yap_InitTextAllocator();
  YAP_init_args *iap = x;
  memset(iap, 0, sizeof(YAP_init_args));
#if __ANDROID__
  iap->boot_file_type = YAP_BOOT_PL;
  iap->SavedState = NULL;
  iap->assetManager = NULL;
#else
  iap->boot_file_type = YAP_QLY;
  iap->INPUT_STARTUP = saved_state;
#endif
  iap->Argc = argc;
  iap->Argv = argv;
  return YAP_QLY;
}

/**
 * @short Paese command line
 * @param argc number of arguments
 * @param argv arguments
 * @param iap options, see YAP_init_args
 * @return boot from saved state or restore; error
 */
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
          iap->BOOTFILE = p + 1;
        else if (argv[1] && *argv[1] != '-') {
          iap->BOOTFILE = *++argv;
          argc--;
        }
        break;
      case 'B':
        iap->boot_file_type = YAP_BOOT_PL;
        if (p[1])
          iap->BOOTSTRAP = p + 1;
        else if (argv[1] && *argv[1] != '-') {
          iap->BOOTSTRAP = *++argv;
          argc--;
        }
        iap->install = true;
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
            iap->PrologRCFile = *argv;
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
            iap->PrologTopLevelGoal = add_end_dot(*argv);
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
          iap->PrologRCFile = argv[1];
          argc = 1;
          break;
        } else {
          iap->PrologRCFile = *argv;
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
          iap->PrologGoal = *argv;
        else {
          argc--;
          if (argc == 0) {
            fprintf(stderr, " [ YAP unrecoverable error: missing "
                            "initialization goal for option 'g' ]\n");
            exit(EXIT_FAILURE);
          }
          argv++;
          iap->PrologGoal = *argv;
        }
        break;
      /* run goal as top-level */
      case 'z':
        if ((*argv)[0] == '\0')
          iap->PrologTopLevelGoal = *argv;
        else {
          argc--;
          if (argc == 0) {
            fprintf(stderr, " [ YAP unrecoverable error: missing goal for "
                            "option 'z' ]\n");
            exit(EXIT_FAILURE);
          }
          argv++;
          iap->PrologTopLevelGoal = add_end_dot(*argv);
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
        } else if (!strncmp("-output-saved-state=", p,
                            strlen("-output-saved-state="))) {
          iap->OUTPUT_STARTUP = p + strlen("-output-saved-state=");
        } else if (!strncmp("-home=", p, strlen("-home="))) {
          iap->ROOTDIR = p + strlen("-home=");
        } else if (!strncmp("-system-library-directory=", p,
                            strlen("-system-library-directory="))) {
          iap->LIBDIR = p + strlen("-system-library-directory=");
        } else if (!strncmp("-system-shared-directory=", p,
                            strlen("-system-shared-directory="))) {
          iap->SHAREDIR = p + strlen("-system-shared-directory=");
        } else if (!strncmp("-prolog-library-directory=", p,
                            strlen("-prolog-library-directory="))) {
          iap->PLDIR = p + strlen("-prolog-library-directory=");
        } else if (!strncmp("-dll-library-directory=", p,
                            strlen("-dll-library-directory="))) {
          iap->DLLDIR = p + strlen("-dll-library-directory=");
        } else if (!strncmp("-home=", p, strlen("-home="))) {
          iap->ROOTDIR = p + strlen("-home=");
        } else if (!strncmp("-cwd=", p, strlen("-cwd="))) {
          if (!Yap_ChDir(p + strlen("-cwd="))) {
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
        } else if (!strncmp("-max-stack=", p, strlen("-max-stack="))) {
          ssize = &(iap->MaxStackSize);
          p += strlen("-max-stack=");
          goto GetSize;
        } else if (!strncmp("-max-trail=", p, strlen("-max-trail="))) {
          ssize = &(iap->MaxTrailSize);
          p += strlen("-max-trail=");
          goto GetSize;
        } else if (!strncmp("-max-heap=", p, strlen("-max-heap="))) {
          ssize = &(iap->MaxHeapSize);
          p += strlen("-max-heap=");
          goto GetSize;
        } else if (!strncmp("-goal=", p, strlen("-goal="))) {
          iap->PrologGoal = p + strlen("-goal=");
        } else if (!strncmp("-top-level=", p, strlen("-top-level="))) {
          iap->PrologTopLevelGoal = p + strlen("-top-level=");
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
          iap->PrologAddPath = *argv;
        else {
          argc--;
          if (argc == 0) {
            fprintf(stderr, " [ YAP unrecoverable error: missing paths for "
                            "option 'p' ]\n");
            exit(EXIT_FAILURE);
          }
          argv++;
          iap->PrologAddPath = *argv;
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
      iap->INPUT_STARTUP = p;
    }
  }
  return iap->boot_file_type;
}

/**
   YAP_DelayInit()

   ensures initialization is done after engine creation.
   It receives a pointer to function and a string describing
   the module.
*/

X_API bool YAP_DelayInit(YAP_ModInit_t f, const char s[]) {
  if (m_delays) {
    m_delays = realloc(m_delays, (n_mdelays + 1) * sizeof(YAP_delaymodule_t));
  } else {
    m_delays = malloc(sizeof(YAP_delaymodule_t));
  }
  m_delays[n_mdelays].f = f;
  m_delays[n_mdelays].s = s;
  n_mdelays++;
  return true;
}

bool Yap_LateInit(const char s[]) {
  int i;
  for (i = 0; i < n_mdelays; i++) {
    if (!strcmp(m_delays[i].s, s)) {
      m_delays[i].f();
      return true;
    }
  }
  return false;
}

/// whether Yap is under control of some other system
bool Yap_embedded = true;

struct ssz_t {
  size_t Heap, Stack, Trail;
};

static void init_hw(YAP_init_args *yap_init, struct ssz_t *spt) {
  Yap_page_size = Yap_InitPageSize(); /* init memory page size, required by
                                           later functions */
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
  Yap_init_yapor_global_local_memory();
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */
  if (!yap_init->Embedded) {
    GLOBAL_PrologShouldHandleInterrupts =
        !yap_init->PrologCannotHandleInterrupts;
    Yap_InitSysbits(0); /* init signal handling and time, required by later
                           functions */
    GLOBAL_argv = yap_init->Argv;
    GLOBAL_argc = yap_init->Argc;
  }

#if __ANDROID__

  // if (yap_init->assetManager)
  //Yap_InitAssetManager();

#endif

  if (yap_init->TrailSize == 0) {
    if (spt->Trail == 0)
      spt->Trail = DefTrailSpace;
  } else {
    spt->Trail = yap_init->TrailSize;
  }
  // Atts = yap_init->AttsSize;
  if (yap_init->StackSize == 0) {
    spt->Stack = DefStackSpace;
  } else {
    spt->Stack = yap_init->StackSize;
  }
#ifndef USE_SYSTEM_MALLOC
  if (yap_init->HeapSize == 0) {
    if (spt->Heap == 0)
      spt->Heap = DefHeapSpace;
  } else {
    spt->Heap = yap_init->HeapSize;
  }
#endif
}

static YAP_file_type_t end_init(YAP_init_args *yap_init, YAP_file_type_t rc) {
  YAP_initialized = true;
  LOCAL_PrologMode &= ~BootMode;
  return rc;
}

 static void start_modules(void) {
  Term cm = CurrentModule;
  size_t i;
  for (i = 0; i < n_mdelays; i++) {
    CurrentModule = MkAtomTerm(YAP_LookupAtom(m_delays[i].s));
    m_delays[i].f();
  }
  CurrentModule = cm;
}


/* this routine is supposed to be called from an external program
   that wants to control Yap */

X_API YAP_file_type_t YAP_Init(YAP_init_args *yap_init) {
  YAP_file_type_t restore_result = yap_init->boot_file_type;
  bool do_bootstrap = (restore_result & YAP_CONSULT_MODE);
  struct ssz_t minfo;

  if (YAP_initialized)
  /* ignore repeated calls to YAP_Init */
  return YAP_FOUND_BOOT_ERROR;
  if (!LOCAL_TextBuffer)
    LOCAL_TextBuffer = Yap_InitTextAllocator();

  Yap_embedded = yap_init->Embedded;

  minfo.Trail = 0, minfo.Stack = 0, minfo.Trail = 0;
  init_hw(yap_init, &minfo);
  Yap_InitWorkspace(yap_init, minfo.Heap, minfo.Stack, minfo.Trail, 0,
                    yap_init->MaxTableSpaceSize, yap_init->NumberWorkers,
                    yap_init->SchedulerLoop, yap_init->DelayedReleaseLoad);
  //

  CACHE_REGS
  if (Yap_embedded)
    if (yap_init->QuietMode) {
      setVerbosity(TermSilent);
    }
  if (yap_init->PrologRCFile != NULL) {
    /*
      This must be done before restore, otherwise
      restore will print out messages ....
    */
    setBooleanGlobalPrologFlag(HALT_AFTER_CONSULT_FLAG,
                               yap_init->HaltAfterConsult);
  }
  /* tell the system who should cope with interrupts */
  Yap_ExecutionMode = yap_init->ExecutionMode;
  Yap_set_locations(yap_init);

  if (!do_bootstrap && Yap_INPUT_STARTUP && yap_init->boot_file_type != YAP_BOOT_PL &&
      Yap_SavedInfo(Yap_INPUT_STARTUP, &minfo.Trail, &minfo.Stack, &minfo.Heap) &&
      Yap_Restore(Yap_INPUT_STARTUP)) {
    setBooleanGlobalPrologFlag(SAVED_PROGRAM_FLAG, true);
    CurrentModule = LOCAL_SourceModule = USER_MODULE;
    init_globals(yap_init);
    YAP_RunGoalOnce(TermInitProlog);

    start_modules();
    return end_init(yap_init, YAP_QLY);
  } else {
    init_globals(yap_init);

    start_modules();
    consult(Yap_BOOTSTRAP PASS_REGS);
    if (yap_init->install && Yap_OUTPUT_STARTUP) {
      Term t = MkAtomTerm(Yap_LookupAtom(Yap_OUTPUT_STARTUP));
      Term g = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("qsave_program"), 1),
                              1, &t);

      YAP_RunGoalOnce(g);
    }
    setAtomicGlobalPrologFlag(RESOURCE_DATABASE_FLAG,
                              MkAtomTerm(Yap_LookupAtom(Yap_BOOTFILE)));
    setBooleanGlobalPrologFlag(SAVED_PROGRAM_FLAG, false);
    return end_init(yap_init, YAP_BOOT_PL);
  }
}

#if (DefTrailSpace < MinTrailSpace)
#undef DefTrailSpace
#define DefTrailSpace MinTrailSpace
#endif

#if (DefStackSpace < MinStackSpace)
#undef DefStackSpace
#define DefStackSpace MinStackSpace
#endif

#if (DefHeapSpace < MinHeapSpace)
#undef DefHeapSpace
#define DefHeapSpace MinHeapSpace
#endif

#define DEFAULT_NUMBERWORKERS 1
#define DEFAULT_SCHEDULERLOOP 10
#define DEFAULT_DELAYEDRELEASELOAD 3

X_API YAP_file_type_t YAP_FastInit(char *saved_state, int argc, char *argv[]) {
  YAP_init_args init_args;
  YAP_file_type_t out;

  if ((out = Yap_InitDefaults(&init_args, saved_state, argc, argv)) !=
      YAP_FOUND_BOOT_ERROR)
    out = YAP_Init(&init_args);
  if (out == YAP_FOUND_BOOT_ERROR) {
    Yap_Error(init_args.ErrorNo, TermNil, init_args.ErrorCause);
  }
  return out;
}
