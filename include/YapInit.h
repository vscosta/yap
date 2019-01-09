 /**
  * 
  * @file YapInit.h
  * 
  * * Utilities for Booting YAP
  */

#ifndef YAPINIT_H
#define YAPINIT_H

#define YAP_BOOT_FROM_SAVED_CODE 1
#define YAP_BOOT_FROM_SAVED_STACKS 2
#define YAP_BOOT_ERROR -1

#define YAP_CONSULT_MODE 0
#define YAP_RECONSULT_MODE 1
#define YAP_BOOT_MODE 2

X_API YAP_file_type_t Yap_InitDefaults(void *init_args, char saved_state[],
                                       int Argc, char *Argv[]);

typedef struct yap_boot_params {
    //> struct marker
    void *start;
  //> boot type as suggested by the user
  YAP_file_type_t boot_file_type;
  //> how files are organised: NULL is GNU/Linux way
  // const char *directory_structure;
  //> if NON-NULL, set value for Yap_ROOTDIR
  const char *ROOTDIR;
  //> if NON-NULL, location of yaap, sets Yap_BINDIR
  const char *BINDIR;
  //> if NON-NULL, location of libYap, sets Yap_LIBDIR
  const char *LIBDIR;
  //> if NON-NULL, architecture independent files, sets Yap_SHAREDIR
  const char *SHAREDIR;
  //> if NON-NULL, include files, sets Yap_INCLUDEDIR
  const char *INCLUDEDIR;
  //> if NON-NULL, Prolog DLL location, sets Yap_DLLDIR
  const char *DLLDIR;
  //> if NON-NULL, Prolog library, sets Yap_DLLDIR
  const char *PLDIR;
  //> if NON-NULL, Prolog library, sets Yap_COMMONSDIR
  const char *COMMONSDIR;
    //> if NON-NULL, name for a Prolog file to use when booting at run-time
    const char *BOOTDIR;
    //> if NON-NULL, name for a Prolog directory that we shall use to start booting
    const char *SOURCEBOOT;
    //> if NON-NULL, name for a Prolog file to use when booting at compile-time
  const char *BOOTSTRAP;
  //> if NON-NULL, path where we can find the saved state
  const char *INPUT_STARTUP;
  //> bootstrapping mode: YAP is not properly installed
  bool install;
  //> jupyter mode: YAP is in space
  bool jupyter;
  //>  generats a saved space at this path
  const char *OUTPUT_STARTUP;
  //> if NON-0, minimal size for Heap or Code Area
  size_t HeapSize;
  //> if NON-0, maximal size for Heap or Code Area
  size_t MaxHeapSize;
  //> if NON-0, minimal size for Local+Global Stack
  size_t StackSize;
  //> if NON-0, maximal size for Local+Global Stack
  size_t MaxStackSize;
  //*> deprecated
  size_t MaxGlobalSize;
  //> if NON-0, minimal size for Trail
  size_t TrailSize;
  //> if NON-0, maximal size for Trail
  size_t MaxTrailSize;
  //> if NON-0, minimal size for AttributeVarStack
  size_t AttsSize;
  //> if NON-0, maximal size for AttributeVarStack
  size_t MaxAttsSize;
  //> if NON-NULL, name for a Prolog file to use when initializing
  const char *YapPrologInitGoal;
  //> if NON-NULL, name for a Prolog file to consult before entering top-level
  const char *PrologRCFile;
  //> if NON-NULL, a goal to run before top-level
  const char *PrologGoal;
  //> if NON-NULL, a goal to run as top-level
  const char *PrologTopLevelGoal;
  //> if NON-NULL, a path to extend file-search-path
  const char *PrologAddPath;
  //> if previous NON-NULL and TRUE, halt after consulting that file
  bool HaltAfterBoot;
  //> ignore .yaprc, .prolog.ini, etc. files.
  bool FastBoot;
  //> the next field only interest YAPTAB
  //> if NON-0, maximum size for Table Space
  size_t MaxTableSpaceSize;
  /* the next three fields only interest YAPOR, but we keep them so that
     users don't need to recompile DLL in order to use YAPOR */
  //> if NON-0, number of workers we want to have (default=1)
  unsigned long int NumberWorkers;
  //> if NON-0, manage the inner scheduler loop (default = 10)
  unsigned long int SchedulerLoop;
  //> if NON-0, say how long to keep nodes (default = 3)
  unsigned long int DelayedReleaseLoad;
  //> end of YAPOR fields
  /* whether Prolog should handle interrupts. Note that
    interrupts will always be disabled in embedded mode. */
  bool PrologCannotHandleInterrupts;
  //> flag for JIT mode
  int ExecutionMode;
  //> number of arguments that Prolog will see
  int Argc;
  //> array of arguments as seen by Prolog
  char **Argv;
  //> embedded in some other system: no signals, readline, etc
  bool Embedded;
  //> QuietMode
  int QuietMode;
  //> 0, maintain default, > 0 use fd-1, < 0 close
  int inp, out, err;
#if __ANDROID__
  //> android asset support
  AAssetManager *assetManager;
#endif
/* support nf's ypp preprocessor code */
#define YAP_MAX_YPP_DEFS 100
  char *def_var[YAP_MAX_YPP_DEFS];
  char *def_value[YAP_MAX_YPP_DEFS];
  int def_c;
  /* End preprocessor code */

#ifdef MYDDAS_MYSQL
  //> If any myddas option was given
  short myddas;
  //> MYDDAS Fields
  char *myddas_user;
  char *myddas_pass;
  char *myddas_db;
  char *myddas_host;
#endif
  /* errornumber */
  int ErrorNo;
  //> errorstring
  char *ErrorCause;
} YAP_init_args;

#endif
