/*************************************************************************
*									 *
*	 YAP Prolog 	@(#)c_interface.h	2.2			 *
*	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		yap_structs.h						 *
* Last rev:	15/5/2000						 *
* mods:									 *
* comments:	Data structures and defines used in the Interface	 *
*									 *
*************************************************************************/

#if  defined(__STDC__) || defined(_MSC_VER)

#define PROTO(X,ARGS) X ARGS
#define CONST const

#else

#define PROTO(X,ARGS) X()
#define CONST /* empty */
#endif

#ifndef HAS_YAP_H

/* Type definitions */

#if _WIN64
typedef unsigned long long YAP_CELL;  
#else
typedef unsigned long YAP_CELL;  
#endif

typedef int YAP_Bool;

typedef YAP_CELL YAP_Term;

typedef YAP_CELL YAP_Arity;

typedef YAP_Term YAP_Module;

typedef struct FunctorEntry *YAP_Functor;

typedef struct AtomEntry *YAP_Atom;

#if _WIN64
typedef long long int YAP_Int;  

typedef unsigned long long int  YAP_UInt;

#else
typedef long int  YAP_Int;

typedef unsigned long int  YAP_UInt;
#endif

typedef double YAP_Float;

#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif

#endif

#define YAP_BOOT_FROM_PROLOG       0
#define YAP_BOOT_FROM_SAVED_CODE   1
#define YAP_BOOT_FROM_SAVED_STACKS 2
#define YAP_FULL_BOOT_FROM_PROLOG  4
#define YAP_BOOT_ERROR            -1

#define YAP_WRITE_QUOTED		0
#define YAP_WRITE_HANDLE_VARS		1
#define YAP_WRITE_IGNORE_OPS		2

#define YAP_CONSULT_MODE           0
#define YAP_RECONSULT_MODE         1

typedef struct {
  /* if NON-NULL, path where we can find the saved state */
  char *SavedState;
  /* if NON-0, minimal size for Heap or Code Area */
  unsigned long int HeapSize;
  /* if NON-0, maximal size for Heap or Code Area */
  unsigned long int MaxHeapSize;
  /* if NON-0, minimal size for Local+Global Stack */
  unsigned long int StackSize;
  /* if NON-0, maximal size for Local+Global Stack */
  unsigned long int MaxStackSize;
  unsigned long int MaxGlobalSize;
  /* if NON-0, minimal size for Trail */
  unsigned long int TrailSize;
  /* if NON-0, maximal size for Trail */
  unsigned long int MaxTrailSize;
  /* if NON-0, minimal size for AttributeVarStack */
  unsigned long int AttsSize;
  /* if NON-0, maximal size for AttributeVarStack */
  unsigned long int MaxAttsSize;
  /* if NON-NULL, value for YAPLIBDIR */
  char *YapLibDir;
  /* if NON-NULL, name for a Prolog file to use when booting  */
  char *YapPrologBootFile;
  /* if NON-NULL, name for a Prolog file to use when initialising  */
  char *YapPrologInitFile;
  /* if NON-NULL, name for a Prolog file to consult before entering top-level  */
  char *YapPrologRCFile;
  /* if NON-NULL, a goal to run before top-level  */
  char *YapPrologGoal;
  /* if NON-NULL, a goal to run as top-level  */
  char *YapPrologTopLevelGoal;
  /* if NON-NULL, a path to extend file-search-path   */
  char *YapPrologAddPath;
  /* if previous NON-NULL and TRUE, halt after consulting that file  */
  int HaltAfterConsult;
  /* ignore .yaprc, .prolog.ini, etc. files.  */
  int FastBoot;
  /* the next field only interest YAPTAB */
  /* if NON-0, maximum size for Table Space */
  unsigned long int MaxTableSpaceSize;
  /* the next three fields only interest YAPOR, but we keep them so that
     users don't need to recompile DLL in order to use YAPOR */
  /* if NON-0, number of workers we want to have (default=1) */
  unsigned long int NumberWorkers;
  /* if NON-0, manage the inner scheduler loop (default = 10) */
  unsigned long int SchedulerLoop;
  /* if NON-0, say how long to keep nodes (default = 3) */
  unsigned long int DelayedReleaseLoad;
  /* end of YAPOR fields */
  /* whether Prolog should handle interrupts */
  int PrologShouldHandleInterrupts;
  /* flag for JIT mode */
  int ExecutionMode;
  /* number of arguments that Prolog will see */
  int Argc;
  /* array of arguments as seen by Prolog */
  char **Argv;
  /* QuietMode */
  int QuietMode;
#ifdef MYDDAS_MYSQL
  /* If any myddas option was given */
  short myddas;
  /* MYDDAS Fields */
  char *myddas_user;
  char *myddas_pass;
  char *myddas_db;
  char *myddas_host;
#endif
  /* errornumber */
  int ErrorNo;
  /* errorstring */
  char *ErrorCause;
} YAP_init_args;


/* from thread.h */
typedef struct {
  unsigned long int		    ssize;
  unsigned long int		    tsize;
  YAP_Term		   alias;
  int		   (*cancel)(int);
} YAP_thread_attr;

typedef struct YAP_pred_entry *YAP_PredEntryPtr;

/* this should be opaque to the user */ 
typedef struct {
  unsigned long  b;
  struct yami *p;
} YAP_dogoalinfo;

typedef int  (*YAP_agc_hook)(void *_Atom);

typedef void  (*YAP_halt_hook)(int exit_code, void *closure);

/********* execution mode ***********************/

typedef enum
  {
    YAPC_INTERPRETED,         /* interpreted */
    YAPC_MIXED_MODE_USER,       /* mixed mode only for user predicates */
    YAPC_MIXED_MODE_ALL,        /* mixed mode for all predicates */
    YAPC_COMPILE_USER,          /* compile all user predicates*/
    YAPC_COMPILE_ALL          /* compile all predicates */
  } yapc_exec_mode;

/********* YAP C-Flags ***********************/

typedef enum
  {
    YAPC_ENABLE_GC,                 /* enable or disable garbage collection */
    YAPC_ENABLE_AGC                 /* enable or disable atom garbage collection */
  } yap_flag_t;

