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
#include "iopreds.h"

#if HAVE_UNISTD_H

#include <unistd.h>

#endif
#if HAVE_STDINT_H

#include <stdint.h>

#endif

#include <stdlib.h>

#include <stddef.h>
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
  if (
#if YAPOR_THREADS
      Yap_thread_self() != 0
#else
      worker_id != 0
#endif
      ) {
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
			       yap_init->HaltAfterBoot);
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
    setBooleanLocalPrologFlag(VERBOSE_LOAD_FLAG, TermFalse);
  }
}


const char *Yap_BINDIR, *Yap_ROOTDIR, *Yap_SHAREDIR, *Yap_LIBDIR, *Yap_DLLDIR,
  *Yap_PLDIR, *Yap_BOOTSTRAP, *Yap_COMMONSDIR, *Yap_INPUT_STARTUP,
  *Yap_OUTPUT_STARTUP, *Yap_SOURCEBOOT, *Yap_INCLUDEDIR, *Yap_PLBOOTDIR;

/**
 * consult loop in C: used to boot the system, butt supports goal execution and
 * recursive consulting.
 *
 * */
static bool load_file(const char *b_file USES_REGS) {
  Term t;
  int c_stream, osno, oactive;

  Functor functor_query = Yap_MkFunctor(Yap_LookupAtom("?-"), 1);
  Functor functor_command1 = Yap_MkFunctor(Yap_LookupAtom(":-"), 1);
  Functor functor_compile2 = Yap_MkFunctor(Yap_LookupAtom("c_compile"), 1);

  /* consult in C */
  int lvl = push_text_stack();
  
  char *full;
  /* the consult mode does not matter here, really */
  if ((osno = Yap_CheckAlias(AtomLoopStream)) < 0) {
    osno = 0;
  }
  c_stream = YAP_InitConsult(YAP_BOOT_MODE, b_file, &full, &oactive);
  __android_log_print(
		      ANDROID_LOG_INFO, "YAPDroid", "done init_consult %s ",b_file);
  if (c_stream < 0) {
    fprintf(stderr, "[ FATAL ERROR: could not open file %s\n", b_file);
    pop_text_stack(lvl);
    exit(1);
  }
  if (!Yap_AddAlias(AtomLoopStream, c_stream)) {
    pop_text_stack(lvl);
    return false;
  }
  __android_log_print(
		      ANDROID_LOG_INFO, "YAPDroid", "do reset %s ",b_file);
  t = 0;
  while (t != TermEof) {
    CACHE_REGS
          YAP_Reset(YAP_FULL_RESET, false);
     Yap_StartSlots();
    Term vs = MkVarTerm(), pos = MkVarTerm();

    t = YAP_ReadClauseFromStream(c_stream, vs, pos);
    // Yap_GetNèwSlot(t);
    if (t == TermEof || t == TermNil) {
      continue;
    } else if (t == 0) {
      fprintf(stderr, "%s:" Int_FORMAT " :0: error: SYNTAX ERROR\n",
	      b_file, GLOBAL_Stream[c_stream].linecount);
      //
      //      {
      //          char buu[1024];
      //1
      //          YAP_WriteBuffer(t,  buu, 1023, 0);
      //          fprintf(stderr, "[ %s ]\n" , buu);
      //      }
      continue;
    } else if (IsVarTerm(t)) {
      fprintf(stderr, "%s:" Int_FORMAT ":0: error: unbound or NULL parser  output\n\n",
	      b_file,
	      GLOBAL_Stream[c_stream].linecount);
      continue;
    } else if (IsApplTerm(t) &&
	       (FunctorOfTerm(t) == functor_query ||
		FunctorOfTerm(t) == functor_command1)) {
      t = ArgOfTerm(1, t);
      if (IsApplTerm(t) && FunctorOfTerm(t) == functor_compile2) {
	load_file(RepAtom(AtomOfTerm(ArgOfTerm(1, t)))->StrOfAE);
	Yap_ResetException(LOCAL_ActiveError);
	continue;
      } else {
	YAP_RunGoalOnce(t);
      }
    } else {
      YAP_CompileClause(t);
    }
  
    yap_error_descriptor_t *errd;
    if ((errd = Yap_GetException(LOCAL_ActiveError)) &&
	(errd->errorNo != YAP_NO_ERROR)) {
      fprintf(stderr, "%s:" Int_FORMAT ":0: error: %s/%s %s\n\n", b_file, errd->errorLine, errd->errorAsText, errd->classAsText, errd->errorMsg);
    }
  }
  BACKUP_MACHINE_REGS();
  YAP_EndConsult(c_stream, &osno, full);
  if (!Yap_AddAlias(AtomLoopStream, osno)) {
    pop_text_stack(lvl);
    return false;
  }
  pop_text_stack(lvl);
  return t == TermEof;
}

static const char * EOLIST ="EOLINE";
static bool is_install;

static bool is_dir( const char *path, const void  *info) {
  if (is_install)
    return true;

  if (Yap_isDirectory( path ))
    return true;
  char s[YAP_FILENAME_MAX + 1];
  Int i = strlen(path);
  strncpy(s, path, YAP_FILENAME_MAX);
  while (--i) {
    if (Yap_dir_separator((int)path[i]))
      break;
  }
  if (i == 0) {
    s[0] = '.';
    i = 1;
  }
  s[i] = '\0';
  if (info == NULL)
    return  true;
  return
    strcmp(info,s) == 0 ||
    Yap_isDirectory( s );
}
																			       
static bool is_file( const char *path, const void  *info) {
  if (is_install)
    return true;
  return Yap_Exists( path );
}
																			       
static bool is_wfile( const char *path, const void  *info) {
																				   
  return true;
}

typedef bool testf(const char *s, const void *info);


///
///
static const char *sel(
		       testf test, const void *info, const char *s1, ...) {
  const char *fmt = s1;
  va_list ap;
  char *buf = malloc(FILENAME_MAX + 1);

  va_start(ap, s1);
  while (fmt != EOLIST) {
    if (fmt == NULL || fmt[0]=='\0') {
      fmt = va_arg(ap, const char *);
      continue;
    }
    strncpy(buf, fmt, FILENAME_MAX); // Yap_AbsoluteFile(fmt,true), FILENAME_MAX);
    if (test(buf,info)) {
      buf = realloc(buf, strlen(buf) + 1);
      va_end(ap);
      return buf;
    }
    fmt = va_arg(ap, const char *);
  }

  va_end(ap);
  free(buf);
  return NULL;
}


static const char *join(const char *s0, const char *s1) {
  CACHE_REGS

    if (!s0 || s0[0] == '\0') {
      if (s1 && s1[0])
	return s1;
      else
	return NULL;
    }
  if (!s1 || s1[0] == '\0')
    return s0;
  //  int lvl = push_text_stack();
  char *buf = malloc(strlen(s0)+strlen(s1) + 2);
  strcpy(buf, s0);
  if (Yap_dir_separator(s0[strlen(s0)-1])) {
    if (Yap_dir_separator(s1[0])) {
      s1 += 1;
    }
  } else {
    if (!Yap_dir_separator(s1[0]-1)) {
      strcat(buf, "/");
    }
  }
  strcat(buf, s1);
  return buf;
}

static void Yap_set_locations(YAP_init_args *iap) {
  is_install= iap->install;
  /// ROOT_DIR is the home of the YAP system. It can be:
  /// -- provided by the user;
  /// -- obtained  from DESTDIR + DE=efalkRoot
  ///
  /// It is:
  //  --_not useful in Android, WIN32;
  /// -- DESTDIR/ in Anaconda
  /// -- /usr/loca77l in most Unix style systems
  Yap_ROOTDIR = sel( is_dir, NULL,
		     iap->ROOTDIR,
		     getenv("YAPROOTDIR"),
		     join(getenv("DESTDIR"), YAP_ROOTDIR),

#if __ANDROID__
		     "/",
#else
		     join(getenv("DESTDIR"), YAP_ROOTDIR),
		     join(getenv("DESTDIR"), join(getenv("ḦOME"),".local")),
		     join(getenv("DESTDIR"), "/usr/local"),
		     join(getenv("DESTDIR"), "/usr"),
		     join(getenv("DESTDIR"), "/opt"),
#endif
		     EOLIST
		     );
  __android_log_print(
		      ANDROID_LOG_INFO,"YAPDroid", "Yap_ROOTDIR %s", Yap_ROOTDIR);

  /// BINDIR: where the OS stores header files, namely libYap...
  Yap_BINDIR = sel( is_dir, Yap_ROOTDIR, iap->BINDIR,
		    getenv("YAPBINDIR"),
#if !defined(__ANDROID__)
		    join(getenv("DESTDIR"), YAP_BINDIR),
#endif
		    join(Yap_ROOTDIR, "bin"),
		    EOLIST);

  /// LIBDIR: where the OS stores dynamic libraries, namely libYap...
  Yap_LIBDIR = sel( is_dir, Yap_ROOTDIR, iap->LIBDIR,
#if !defined(__ANDROID__)
		    join(getenv("DESTDIR"), YAP_LIBDIR),
#endif
		    join(Yap_ROOTDIR, "lib"),
		    EOLIST);

  /// DLLDIR: where libraries can find expicitely loaded DLLs
  Yap_DLLDIR = sel(is_dir, Yap_LIBDIR, iap->DLLDIR,
		   getenv("YAPLIBDIR"),
		   join(getenv("DESTDIR"), YAP_DLLDIR),
		   join(Yap_DLLDIR, "Yap"),
		   EOLIST);

  /// INCLUDEDIR: where the OS stores header files, namely libYap...
  Yap_INCLUDEDIR = sel(is_dir, Yap_ROOTDIR, iap->INCLUDEDIR,
#if !defined(__ANDROID__)
		       join(getenv("DESTDIR"), YAP_INCLUDEDIR),
#endif
		       join(Yap_ROOTDIR, "include"),
		       EOLIST);


  /// SHAREDIR: where OS & ARCH independent files live
  Yap_SHAREDIR = sel( is_dir, Yap_ROOTDIR, iap->SHAREDIR,
		      getenv("YAPSHAREDIR"),
#if __ANDROID__
		      "/data/data/pt.up.yap/files",
		      "/assets",
#endif
		      join(getenv("DESTDIR"), YAP_SHAREDIR),
		      join(Yap_ROOTDIR, "share"),
		      join(Yap_ROOTDIR, "files"),
		      EOLIST);
  __android_log_print(
		      ANDROID_LOG_INFO,"YAPDroid", "Yap_SHAREDIR %s", Yap_SHAREDIR);



  /// PLDIR: where we can find Prolog files
  Yap_PLDIR = sel( is_dir, Yap_SHAREDIR, iap->PLDIR,
		   join(getenv("DESTDIR"), join(Yap_SHAREDIR, "Yap")),
		   EOLIST);

  __android_log_print(
		      ANDROID_LOG_INFO, "YAPDroid","Yap_PLDIR %s", Yap_PLDIR);

  /// ``COMMONSDIR: Prolog Commons
  Yap_COMMONSDIR = sel(is_dir, Yap_SHAREDIR, iap->COMMONSDIR,
		       join(getenv("DESTDIR"), join(Yap_SHAREDIR, "PrologCommons")),
		       EOLIST);
  /// SOURCEBOOT: booting from the Prolog boot file at compilation-time so we should not assume pl is installed.
  Yap_SOURCEBOOT = sel(  is_file, Yap_AbsoluteFile("pl",false), iap->SOURCEBOOT,
			 YAP_SOURCEBOOT,
			 "boot.yap",
			 "../pl/boot.yap",
			 EOLIST);
  __android_log_print(
		      ANDROID_LOG_INFO, "YAPDroid","Yap_SOURCEBOOT %s", Yap_SOURCEBOOT);

  Yap_PLBOOTDIR = sel(  is_dir, Yap_PLDIR, iap->BOOTDIR,
			join(getenv("DESTDIR"),join(Yap_PLDIR, "pl")),
			EOLIST);
  __android_log_print(
		      ANDROID_LOG_INFO, "YAPDroid","Yap_BOOTSTRAP %s", Yap_BOOTSTRAP);
  /// BOOTSTRAP: booting from the Prolog boot file after YAP is installed
  Yap_BOOTSTRAP = sel(  is_file, Yap_PLBOOTDIR, iap->BOOTSTRAP,
			join(getenv("DESTDIR"),YAP_BOOTSTRAP),
			join(getenv("DESTDIR"),join(Yap_PLBOOTDIR, "boot.yap")),
			EOLIST);
  __android_log_print(
		      ANDROID_LOG_INFO,"YAPDroid", "Yap_BOOTSTRAP %s", Yap_PLBOOTDIR);
  /// STARTUP: where we can find the core Prolog bootstrap file
  Yap_OUTPUT_STARTUP =
    sel( is_wfile, ".", iap->OUTPUT_STARTUP,
	 YAP_OUTPUT_STARTUP,
	 join(getenv("DESTDIR"), join(Yap_DLLDIR, "startup.yss")),
	 join(getenv("DESTDIR"), join(Yap_DLLDIR,iap->OUTPUT_STARTUP)),
	 "startup.yss",
	 EOLIST);

  Yap_INPUT_STARTUP =
    sel( is_file, Yap_DLLDIR, iap->INPUT_STARTUP,
	 "startup.yss",
	 join(getenv("DESTDIR"), join(Yap_DLLDIR, "startup.yss")),
#if !defined(__ANDROID__)
	 join(getenv("DESTDIR"), YAP_INPUT_STARTUP),
#endif
	 "/usr/local/lib/Yap/startup.yss",
	 "/usr/lib/Yap/startup.yss",
	 EOLIST);

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
  fprintf(stderr, "  -B   Used during compilation: boot from ../pl/boot.yap and generate a saved state. \n");
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
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA) ||	\
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
  iap->Argc = argc;
  iap->Argv = argv;
#if __ANDROID__
  iap->boot_file_type = YAP_PL;
  iap->INPUT_STARTUP = NULL;
  iap->assetManager = NULL;
  return YAP_PL;
#else
  iap->boot_file_type = YAP_QLY;
  iap->INPUT_STARTUP = saved_state;
  return YAP_QLY;
#endif
}

/**
 * @short Parse command line
 * @param argc number of arguments
 * @param argv arguments
 * @param iap options, see YAP_init_args
 * @return boot from saved state or restore; error
 */
X_API YAP_file_type_t YAP_parse_yap_arguments(int argc, char *argv[], YAP_init_args *iap) {
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
	  iap->BOOTSTRAP = p + 1;
	else if (argv[1] && *argv[1] != '-') {
	  iap->BOOTSTRAP = *++argv;
	  argc--;
	}
	break;
      case 'B':
	iap->boot_file_type = YAP_SOURCE_PL;
	if (p[1])
	  iap->SOURCEBOOT = p + 1;
	else if (argv[1] && *argv[1] != '-') {
	  iap->SOURCEBOOT = *++argv;
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
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA) ||	\
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
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA) ||	\
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
	iap->HaltAfterBoot = true;
      case 'l':
	p++;
	iap->QuietMode = TRUE;
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
	iap->HaltAfterBoot = true;
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

struct ssz_t {
  size_t Heap, Stack, Trail;
};

bool Yap_Embedded;

static void init_hw(YAP_init_args *yap_init, struct ssz_t *spt) {
  Yap_page_size = Yap_InitPageSize(); /* init memory page size, required by
					 later functions */
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
  Yap_init_yapor_global_local_memory();
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */
  if (yap_init->Embedded) {
    yap_init->install = false;
    GLOBAL_PrologShouldHandleInterrupts =
      yap_init->PrologCannotHandleInterrupts = true;
      Yap_Embedded = true;
  } else {
    GLOBAL_PrologShouldHandleInterrupts =
      !yap_init->PrologCannotHandleInterrupts;

       GLOBAL_PL_Argv = yap_init->Argv;

  }
  Yap_InitSysbits(0); /* init signal handling and time, required by later
			 functions */
  GLOBAL_argv = yap_init->Argv;
  GLOBAL_argc = yap_init->Argc;

#if __ANDROID__

  // if (yap_init->assetManager)
  // Yap_InitAssetManager();

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

static void end_init(YAP_init_args *iap) {
  YAP_initialized = true;
  if (iap->HaltAfterBoot)
    Yap_exit(0);
  LOCAL_PrologMode &= ~BootMode;
  CurrentModule = USER_MODULE;
  LOCAL_SourceModule = USER_MODULE;
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

X_API void YAP_Init(YAP_init_args *yap_init) {
  bool try_restore = yap_init->boot_file_type == YAP_QLY;
  bool do_bootstrap = yap_init->boot_file_type == YAP_PL ||
    yap_init->boot_file_type == YAP_SOURCE_PL;
  struct ssz_t minfo;
  __android_log_print(
		      ANDROID_LOG_INFO, "YAPDroid", "start init  ");
  if (YAP_initialized)
    /* ignore repeated calls to YAP_Init */
    return;
  if (!LOCAL_TextBuffer)
    LOCAL_TextBuffer = Yap_InitTextAllocator();

  Yap_Embedded = yap_init->Embedded;

  minfo.Trail = 0, minfo.Stack = 0, minfo.Trail = 0;
  init_hw(yap_init, &minfo);
  Yap_InitWorkspace(yap_init, minfo.Heap, minfo.Stack, minfo.Trail, 0,
		    yap_init->MaxTableSpaceSize, yap_init->NumberWorkers,
		    yap_init->SchedulerLoop, yap_init->DelayedReleaseLoad);
  //

  CACHE_REGS
    CurrentModule = PROLOG_MODULE;

  if (yap_init->QuietMode) {
    setVerbosity(TermSilent);
  }
  if (yap_init->PrologRCFile != NULL) {
    /*
      This must be done before restore, otherwise
      restore will print out messages ....
    */
    setBooleanGlobalPrologFlag(HALT_AFTER_CONSULT_FLAG,
			       yap_init->HaltAfterBoot);
  }
  /* tell the system who should cope with interrupts */
  Yap_ExecutionMode = yap_init->ExecutionMode;
  Yap_set_locations(yap_init);

  if (Yap_INPUT_STARTUP==NULL)
    try_restore = false;
  if (do_bootstrap || !try_restore ||
      !Yap_SavedInfo(Yap_INPUT_STARTUP, &minfo.Trail, &minfo.Stack,
		     &minfo.Heap)) {
    init_globals(yap_init);

    start_modules();
    TermEof = MkAtomTerm(Yap_LookupAtom("end_of_file"));
    LOCAL_consult_level = -1;
    __android_log_print(
			ANDROID_LOG_INFO, "YAPDroid", "init %s ", Yap_BOOTSTRAP);
    if (yap_init->install) {
      load_file(Yap_SOURCEBOOT PASS_REGS);
      setAtomicGlobalPrologFlag(RESOURCE_DATABASE_FLAG,
				MkAtomTerm(Yap_LookupAtom(Yap_SOURCEBOOT)));
    }
    else {
      load_file(Yap_BOOTSTRAP PASS_REGS);
      setAtomicGlobalPrologFlag(RESOURCE_DATABASE_FLAG,
				MkAtomTerm(Yap_LookupAtom(Yap_BOOTSTRAP)));
    }

    CurrentModule = LOCAL_SourceModule = TermUser;
    setBooleanGlobalPrologFlag(SAVED_PROGRAM_FLAG, false);
  } else {
    if (yap_init->QuietMode) {
      setVerbosity(TermSilent);
    }
    __android_log_print(
			ANDROID_LOG_INFO, "YAPDroid", "restore %s ",Yap_INPUT_STARTUP );
    Yap_Restore(Yap_INPUT_STARTUP);
    CurrentModule = LOCAL_SourceModule = TermUser;
    init_globals(yap_init);

    start_modules();
    if (yap_init->install && Yap_OUTPUT_STARTUP) {
      setAtomicGlobalPrologFlag(RESOURCE_DATABASE_FLAG,
				MkAtomTerm(Yap_LookupAtom(Yap_INPUT_STARTUP)));
      setBooleanGlobalPrologFlag(SAVED_PROGRAM_FLAG, true);
    }
    LOCAL_consult_level = -1;
  }

  YAP_RunGoalOnce(TermInitProlog);
  if (yap_init->install && Yap_OUTPUT_STARTUP) {
    Term t = MkAtomTerm(Yap_LookupAtom(Yap_OUTPUT_STARTUP));
    Term g = Yap_MkApplTerm(Yap_MkFunctor(Yap_LookupAtom("qsave_program"), 1),
			    1, &t);

    YAP_RunGoalOnce(g);
  }

  end_init(yap_init);
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

X_API void YAP_FastInit(char *saved_state, int argc, char *argv[]) {
  YAP_init_args init_args;
  YAP_file_type_t out;

  if ((out = Yap_InitDefaults(&init_args, saved_state, argc, argv)) !=
      YAP_FOUND_BOOT_ERROR)
    YAP_Init(&init_args);
  if (out == YAP_FOUND_BOOT_ERROR) {
    Yap_Error(init_args.ErrorNo, TermNil, init_args.ErrorCause);
  }
}
