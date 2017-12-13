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

const char *Yap_BINDIR, *Yap_ROOTDIR, *Yap_SHAREDIR, *Yap_LIBDIR, *Yap_DLLDIR,
    *Yap_PLDIR, *Yap_BOOTPLDIR, *Yap_BOOTSTRAPPLDIR, *Yap_COMMONSDIR,
    *Yap_STARTUP, *Yap_BOOTFILE;

static int yap_lineno = 0;

/* do initial boot by consulting the file boot.yap */
static void do_bootfile(const char *b_file USES_REGS) {
  Term t;
  int boot_stream, osno;
  Functor functor_query = Yap_MkFunctor(Yap_LookupAtom("?-"), 1);
  Functor functor_command1 = Yap_MkFunctor(Yap_LookupAtom(":-"), 1);

  /* consult boot.pl */
  char *full = malloc(YAP_FILENAME_MAX + 1);
  full[0] = '\0';
  /* the consult mode does not matter here, really */
  boot_stream = YAP_InitConsult(YAP_BOOT_MODE, b_file, full, &osno);
  if (boot_stream < 0) {
    fprintf(stderr, "[ FATAL ERROR: could not open boot_stream %s ]\n", b_file);
    exit(1);
  }
  free(full);
  setAtomicGlobalPrologFlag(RESOURCE_DATABASE_FLAG,
                            MkAtomTerm(GLOBAL_Stream[boot_stream].name));
  do {
    CACHE_REGS
    YAP_Reset(YAP_FULL_RESET);
    Yap_StartSlots();
    t = YAP_ReadClauseFromStream(boot_stream);

    // Yap_DebugPlWriteln(t);
    if (t == 0) {
      fprintf(stderr,
              "[ SYNTAX ERROR: while parsing boot_stream %s at line %d ]\n",
              b_file, yap_lineno);
    } else if (YAP_IsVarTerm(t) || t == TermNil) {
      fprintf(stderr, "[ line %d: term cannot be compiled ]", yap_lineno);
    } else if (YAP_IsPairTerm(t)) {
      fprintf(stderr, "[ SYSTEM ERROR: consult not allowed in boot file ]\n");
      fprintf(stderr, "error found at line %d and pos %d", yap_lineno,
              fseek(GLOBAL_Stream[boot_stream].file, 0L, SEEK_CUR));
    } else if (IsApplTerm(t) && (FunctorOfTerm(t) == functor_query ||
                                 FunctorOfTerm(t) == functor_command1)) {
      YAP_RunGoalOnce(ArgOfTerm(1, t));
    } else {
      Term ts[2];
      char *ErrorMessage;
      Functor fun = Yap_MkFunctor(Yap_LookupAtom("$prepare_clause"), 2);
      PredEntry *pe = RepPredProp(PredPropByFunc(fun, PROLOG_MODULE));

      if (pe->OpcodeOfPred != UNDEF_OPCODE && pe->OpcodeOfPred != FAIL_OPCODE) {
        ts[0] = t;
        RESET_VARIABLE(ts + 1);
        if (YAP_RunGoal(Yap_MkApplTerm(fun, 2, ts)))
          t = ts[1];
      }
      ErrorMessage = YAP_CompileClause(t);
      if (ErrorMessage) {
        fprintf(stderr, "%s", ErrorMessage);
      }
    }
  } while (t != TermEof);
  BACKUP_MACHINE_REGS();

  YAP_EndConsult(boot_stream, &osno);
#if DEBUG
  if (Yap_output_msg)
    fprintf(stderr, "Boot loaded\n");
#endif
}

/** @brief A simple language for detecting where YAP stuff cn be found
 *
 * @long The options are
 *  `[V]` use a configuration variable YAP_XXXDIR, prefixed by "DESTDIR"
 *  `(V)PATH` compute V and add /PATH,
 *  `$V` search the envurinment
 *  `?V` search the WINDOWS registry
 *  ~` search HOME
 *  `@` query user option.
 *
 */
const char *rootdirs[] = {"[root]", "(execdir)..", "/usr/local", "~", NULL};

const char *bindirs[] = {"[bin]", "(root)bin", NULL};

const char *libdirs[] = {
#if __ANDROID__
    "/assets/lib",
#endif
    "[lib]", "(root)lib", NULL};

const char *sharedirs[] = {
#if __ANDROID__
    "/assets/share",
#endif
    "[share]", "(root)share", NULL};

const char *dlldirs[] = {"$YAPLIBDIR", "(lib)Yap", ".", NULL};

const char *ssdirs[] = {".", "$YAPLIBDIR", "(lib)Yap", NULL};

const char *pldirs[] = {"$YAPSHAREDIR", "?library", "(share)Yap", ".", NULL};

const char *bootpldirs[] = {"(pl)pl", ".", NULL};

const char *bootstrappldirs[] = {YAP_PL_SRCDIR, NULL};

const char *commonsdirs[] = {"(share)PrologCommons", ".", NULL};

const char *ssnames[] = {"@SavedState", YAP_STARTUP, "startup.yss", NULL};

const char *plnames[] = {"@YapPrologBootFile", YAP_BOOTFILE, "boot.yap", NULL};

/**
 * Search
 */
char *location(YAP_init_args *iap, const char *inp, char *out) {
  if (inp == NULL || inp[0] == '\0') {
    return NULL;
  }
  out[0] = '\0';
  if (inp[0] == '(') {
    if (strstr(inp + 1, "root") == inp + 1 && Yap_ROOTDIR &&
        Yap_ROOTDIR[0] != '\0') {
      strcpy(out, Yap_ROOTDIR);
      strcat(out, "/");
      strcat(out, inp + strlen("(root)"));
    } else if (strstr(inp + 1, "bin") == inp + 1 && Yap_BINDIR &&
               Yap_BINDIR[0] != '\0') {
      strcpy(out, Yap_BINDIR);
      strcat(out, "/");
      strcat(out, inp + strlen("(bin)"));
    } else if (strstr(inp + 1, "lib") == inp + 1 && Yap_LIBDIR &&
               Yap_LIBDIR[0] != '\0') {
      strcpy(out, Yap_LIBDIR);
      strcat(out, "/");
      strcat(out, inp + strlen("(lib)"));
    } else if (strstr(inp + 1, "share") == inp + 1 && Yap_SHAREDIR &&
               Yap_SHAREDIR[0] != '\0') {
      strcpy(out, Yap_SHAREDIR);
      strcat(out, "/");
      strcat(out, inp + strlen("(share)"));
    } else if (strstr(inp + 1, "pl") == inp + 1 && Yap_PLDIR &&
               Yap_PLDIR[0] != '\0') {
      strcpy(out, Yap_PLDIR);
      strcat(out, "/");
      strcat(out, inp + strlen("(pl)"));
    } else if (strstr(inp + 1, "execdir") == inp + 1) {
      char *buf = Malloc(YAP_FILENAME_MAX+1);
      const char *ex = Yap_AbsoluteFile(Yap_FindExecutable(), buf, false);
      if (ex != NULL) {
        strcpy(out, dirname((char *)ex));
        strcat(out, "/");
        strcat(out, inp + strlen("(execdir)"));
      }
    }
  } else if (inp[0] == '@') {

    if (strstr(inp + 1, "YapPrologBootFile") == inp + 1) {
      const char *tmp;
      tmp = iap->YapPrologBootFile;
      if (tmp && tmp[0])
        strcpy(out, tmp);
    } else if (strstr(inp + 1, "SavedState") == inp + 1) {
      const char *tmp = iap->SavedState;
      if (tmp && tmp[0])
        strcpy(out, tmp);
    }
  } else if (inp[0] == '$') {
    char *e;
    if ((e = getenv(inp + 1)) != NULL) {
      strcpy(out, e);
    }
  } else if (inp[0] == '?') {
#if _WINDOWS_
    char *e;
    if ((e = Yap_RegistryGetString(inp + 1)) != NULL) {
      strcpy(out, e);
    }
#endif
  } else if (inp[0] == '~') {
    char *e;
    if ((e = getenv("HOME")) != NULL) {
      if (inp[1] == '\0') {
        strcpy(out, e);
      } else if (inp[1] == '/') {
        strcpy(out, e);
        strcat(out, inp + 1);
      }
    }
  } else if (inp[0] == '[') {
      char *o = out;
      const char *e;
      if ((e = getenv("DESTDIR"))) {
        strcpy(out, e);
        o += strlen(e);
      }
      if (strstr(inp + 1, "root") == inp + 1) {
#ifdef YAP_ROOTDIR
        strcpy(o, YAP_ROOTDIR);
#endif
      } else if (strstr(inp + 1, "lib") == inp + 1) {
#ifdef YAP_LIBDIR
        strcpy(o, YAP_LIBDIR);
#endif
      } else if (strstr(inp + 1, "share") == inp + 1) {
#ifdef YAP_SHAREDIR
        strcpy(o, YAP_SHAREDIR);
#endif
      } else if (strstr(inp + 1, "dll") == inp + 1) {
#ifdef YAP_DLLDIR
        strcpy(o, YAP_DLLDIR);
#endif
      } else if (strstr(inp + 1, "pl") == inp + 1) {
#ifdef YAP_PLDIR
        strcpy(o, YAP_PLDIR);
#endif
      } else if (strstr(inp + 1, "commons") == inp + 1) {
#ifdef YAP_COMMONSDIR
        strcpy(o, YAP_COMMONSDIR);
#endif
      }
    } else {
      strcpy(out, inp);
    }
  if (out[0]) {
    return out;
  }
  return NULL;
}

/**
 * @brief find default paths for main YAP variables
 *
 * This function is called once at boot time to set the main paths; it
 * searches a list of paths to instantiate a number of variables. Paths must
 * be directories.
 *
 * It treats the following variables as :
 *  ROOTDIR, SHAREDIR, LIBDIR, EXECUTABLE
 *
 * @return
 */
static const char *find_directory(YAP_init_args *iap, const char *paths[],
                                  const char *names[]) {
  int lvl = push_text_stack();
  char *out = Malloc(YAP_FILENAME_MAX + 1);
  const char *inp;
  char *full;
  if (names) {
    full = Malloc(YAP_FILENAME_MAX + 1);
  }
  int i = 0;
  while ((inp = paths[i++]) != NULL) {
    printf("%s\n", inp);
    out[0] = '\0';
    char *o = location(iap, inp, out), *no;
    if (o && o[0] && Yap_isDirectory(o)) {
      if (names) {
        size_t s = strlen(o);
        o[s++] = '/';
        const char *p;
        int j = 0;
        while ((p = names[j++])) {
          char *io = o + s;
          printf("-> %s\n", o);
          if ((no = location(iap, p, io)) && io[0] != '\0' && Yap_Exists(o))
            return pop_output_text_stack(lvl, realpath(o, full));
        }
      } else {
        printf("-> %s\n", o);
        return pop_output_text_stack(lvl, o);
      }
    }
  }
  pop_text_stack(lvl);
  return NULL;
}

static void Yap_set_locations(YAP_init_args *iap) {
#if CONDA_BUILD
  if (!getenv("DESTDIR")) {
    char *buf = Malloc( YAP_FILENAME_MAX + 1);
    const char *o = Yap_FindExecutable();
    if (!o)
      return;
    o = Yap_AbsoluteFile(o, buf, false);
      Int i = strlen(o);
      while (--i) {
	if (Yap_dir_separator((int)o[i]))
	  break;
      }
      if (i == 0) {     setenv("DESTDIR", "/", 1); }
      else {
     while (--i) {
	if (Yap_dir_separator((int)o[i]))
	  break;
      }
           if (i == 0) {     setenv("DESTDIR", "/", 1); }
	   else  {     setenv("DESTDIR", o, 1); }
      }


    setenv("DESTDIR", buf, 1);
  }
#endif
  Yap_ROOTDIR = find_directory(iap, rootdirs, NULL);
  Yap_LIBDIR = find_directory(iap, libdirs, NULL);
  Yap_BINDIR = find_directory(iap, bindirs, NULL);
  Yap_SHAREDIR = find_directory(iap, sharedirs, NULL);
  Yap_DLLDIR = find_directory(iap, dlldirs, NULL);
  Yap_PLDIR = find_directory(iap, pldirs, NULL);
  Yap_COMMONSDIR = find_directory(iap, commonsdirs, NULL);
  Yap_STARTUP = find_directory(iap, ssdirs, ssnames);
  if (iap->bootstrapping)
    Yap_BOOTFILE = find_directory(iap, bootstrappldirs, plnames);
  else
    Yap_BOOTFILE = find_directory(iap, bootpldirs, plnames);
  if (Yap_ROOTDIR)
    setAtomicGlobalPrologFlag(HOME_FLAG, MkAtomTerm(Yap_LookupAtom(Yap_ROOTDIR)));
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
  iap->SavedState = saved_state;
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
          iap->YapPrologBootFile = NULL;
        }
        iap->bootstrapping = true;
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
            fprintf(stderr, " [ YAP unrecoverable error: missing goal for "
                            "option 'z' ]\n");
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
            fprintf(stderr, " [ YAP unrecoverable error: missing paths for "
                            "option 'p' ]\n");
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

/**
   YAP_DelayInit()

   ensures initialization is done after engine creation.
   It receives a pointer to function and a string describing
   the module.
*/

X_API bool YAP_initialized = false;
static int n_mdelays = 0;
static YAP_delaymodule_t *m_delays;

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

static void start_modules(void) {
  Term cm = CurrentModule;
  size_t i;
  for (i = 0; i < n_mdelays; i++) {
    CurrentModule = MkAtomTerm(YAP_LookupAtom(m_delays[i].s));
    m_delays[i].f();
  }
  CurrentModule = cm;
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
  Yap_InitAssetManager();

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
    Yap_InitYaamRegs(worker_id);
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
  if (yap_init->YapPrologRCFile) {
    Yap_PutValue(AtomConsultOnBoot,
                 MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologRCFile)));
    /*
      This must be done again after restore, as yap_flags
      has been overwritten ....
    */
    setBooleanGlobalPrologFlag(HALT_AFTER_CONSULT_FLAG,
                               yap_init->HaltAfterConsult);
  }
  if (yap_init->YapPrologTopLevelGoal) {
    Yap_PutValue(AtomTopLevelGoal,
                 MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologTopLevelGoal)));
  }
  if (yap_init->YapPrologGoal) {
    Yap_PutValue(AtomInitGoal,
                 MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologGoal)));
  }
  if (yap_init->YapPrologAddPath) {
    Yap_PutValue(AtomExtendFileSearchPath,
                 MkAtomTerm(Yap_LookupAtom(yap_init->YapPrologAddPath)));
  }

  if (yap_init->QuietMode) {
    setVerbosity(TermSilent);
  }
}

static YAP_file_type_t end_init(YAP_init_args *yap_init, YAP_file_type_t rc) {
  init_globals(yap_init);
  LOCAL_PrologMode &= ~BootMode;

  start_modules();

  YAP_initialized = true;
  return rc;
}

/* this routine is supposed to be called from an external program
   that wants to control Yap */

X_API YAP_file_type_t YAP_Init(YAP_init_args *yap_init) {
  YAP_file_type_t restore_result = yap_init->boot_file_type;
  bool do_bootstrap = (restore_result & YAP_CONSULT_MODE);
  struct ssz_t minfo;

  if (YAP_initialized)
    return YAP_FOUND_BOOT_ERROR;
  if (!LOCAL_TextBuffer)
    LOCAL_TextBuffer = Yap_InitTextAllocator();

  /* ignore repeated calls to YAP_Init */
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
  if (yap_init->YapPrologRCFile != NULL) {
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
  if (!do_bootstrap && Yap_STARTUP && yap_init->boot_file_type != YAP_BOOT_PL &&
      Yap_SavedInfo(Yap_STARTUP, &minfo.Trail, &minfo.Stack, &minfo.Heap) &&
      Yap_Restore(Yap_STARTUP)) {
    setBooleanGlobalPrologFlag(SAVED_PROGRAM_FLAG, true);
    CurrentModule = LOCAL_SourceModule = USER_MODULE;
    return end_init(yap_init, YAP_QLY);
  } else {
    do_bootfile(Yap_BOOTFILE PASS_REGS);
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
