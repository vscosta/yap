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
#include "c_interface.h"

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
#if HAVE_STRING_H
#include <string.h>
#endif

static int  PROTO(mygetc, (void));
static void PROTO(do_bootfile, (char *));
static void PROTO(do_top_goal,(Term));
static void PROTO(exec_top_level,(int, char *));

#ifndef LIGHT
void PROTO (exit, (int));
#endif

#ifdef LIGHT
#include <unix.h>
int
  _main (int, char **);
#else
int PROTO (main, (int, char **));
#endif

#ifdef DEBUG
static int output_msg;
#endif

static char BootFile[] = "boot.yap";
static char StartUpFile[] = "startup";

#ifdef lint
/* VARARGS1 */
#endif

#ifdef M_WILLIAMS
long _stksize = 32000;
#endif

static FILE *bootfile;

static int eof_found = FALSE;
static int yap_lineno = 0;

#ifdef DEBUG
static void myputc (int ch)
{
  putc(ch,stderr);
}
#endif

static int
mygetc (void)
{
  int ch;
  if (eof_found)
    return (EOF);
  ch = getc (bootfile);
  if (ch == EOF)
    eof_found = TRUE;
  if (ch == '\n') {
#ifdef MPW
    ch = 10;
#endif
    yap_lineno++;
  }
  return (ch);
}

static void
do_top_goal (Term Goal)
{
#ifdef DEBUG
  if (output_msg)
    fprintf(stderr,"Entering absmi\n");
#endif
  /* PlPutc(0,'a'); PlPutc(0,'\n'); */
  YapRunGoal(Goal);
}

/* do initial boot by consulting the file boot.yap */
static void
do_bootfile (char *bootfilename)
{
  Term t;
  Term term_nil = MkAtomTerm(YapLookupAtom("[]"));
  Term term_end_of_file = MkAtomTerm(YapLookupAtom("end_of_file"));
  Term term_true = MkAtomTerm(YapLookupAtom("true"));
  Term functor_query = MkFunctor(YapLookupAtom("?-"),1);


  fprintf(stderr,"Entering Yap\n");
  /* consult boot.pl */
  bootfile = fopen (bootfilename, "r");
  if (bootfile == NULL)
    {
      fprintf(stderr, "[ FATAL ERROR: could not open bootfile %s ]\n", bootfilename);
      exit(1);
    }
  /* the consult mode does not matter here, really */
  /*
    To be honest, YapInitConsult does not really do much,
    it's here for the future. It also makes what we want to do clearer.
  */
  YapInitConsult(YAP_CONSULT_MODE,bootfilename);
  while (!eof_found)
    {
      t = YapRead(mygetc);
      if (eof_found) {
	break;
      }
      if (t == 0)
        {
	  fprintf(stderr, "[ SYNTAX ERROR: while parsing bootfile %s at line %d ]\n", bootfilename, yap_lineno);
	  exit(1);
        }
      if (IsVarTerm (t) || t == term_nil)
	{
	  continue;
	}
      else if (t == term_true)
	{
	  YapExit(0);
	}
      else if (t == term_end_of_file)
	{
	  break;
	}
      else if (IsPairTerm (t))
        {
	  fprintf(stderr, "[ SYSTEM ERROR: consult not allowed in boot file ]\n");
	  fprintf(stderr, "error found at line %d and pos %d", yap_lineno, fseek(bootfile,0L,SEEK_CUR));
	}
      else if (IsApplTerm (t) && FunctorOfTerm (t) == functor_query) 
	{ 
	  do_top_goal(ArgOfTerm (1, t));
        }
      else
	{
	  char *ErrorMessage = YapCompileClause(t);
	  if (ErrorMessage)
	    fprintf(stderr, ErrorMessage);
	}
      /* do backtrack */
      YapReset();
    }
  YapEndConsult();
  fclose (bootfile);
#ifdef DEBUG
  if (output_msg)
    fprintf(stderr,"Boot loaded\n");
#endif
}

static char *filename;

/*
 * proccess command line arguments: valid switches are: -b    boot -s
 * stack area size (K) -h    heap area size -a    aux stack size -e
 * emacs_mode -m    reserved memory for alloc IF DEBUG -p    if you
 * want to check out startup IF MAC -mpw  if we are using the mpw
 * shell
 */
static int
parse_yap_arguments(int argc, char *argv[], yap_init_args *init_args)
{
  char *p;
  int BootMode = YAP_BOOT_FROM_SAVED_CODE;
  int *ssize;

  while (--argc > 0)
    {
      p = *++argv;
      if (*p == '-')
	switch (*++p)
	  {
	  case 'b':
	    BootMode = YAP_BOOT_FROM_PROLOG;
	    break;
          case '?':
            fprintf(stderr,"\n[ Valid switches for command line arguments: ]\n");
            fprintf(stderr,"  -?   Shows this screen\n");
            fprintf(stderr,"  -b   Boot file (%s)\n", StartUpFile);
            fprintf(stderr,"  -l   Prolog file\n");
            fprintf(stderr,"  -h   Heap area in Kbytes (default: %d)\n", DefHeapSpace);
	    fprintf(stderr,"  -s   Stack area in Kbytes (default: %d)\n", DefStackSpace);
	    fprintf(stderr,"  -t   Trail area in Kbytes (default: %d)\n", DefTrailSpace);
            fprintf(stderr,"  -w   YapOr option: Number of workers (default: %d)\n", init_args->NumberWorkers);
            fprintf(stderr,"  -sl  YapOr option: Loop scheduler executions before look for hiden shared work (default: %d)\n", init_args->SchedulerLoop);
            fprintf(stderr,"  -d   YapOr option: Value of delayed release of load (default: %d)\n", init_args->DelayedReleaseLoad);
            fprintf(stderr,"\n");
            exit(0);
	  case 'w':
	    ssize = &(init_args->NumberWorkers);
	    goto GetSize;
          case 'd':
            ssize = &(init_args->DelayedReleaseLoad);
	    goto GetSize;
#ifdef USE_SOCKET
          case 'c':          /* running as client */
	    {
	      char *host, *p1;
	      long port;
	      char *ptr;

	      host = *++argv;
	      argc--;
	      if (host != NULL && host[0] == '-')
		YapError("sockets must receive host to connect to");
	      p1 = *++argv;
	      argc--;
	      if (p1[0] == '-')
		YapError("sockets must receive port to connect to");
	      port = strtol(p1, &ptr, 10);
	      if (ptr == NULL || ptr[0] != '\0')
		YapError("port argument to socket must be a number");
	      YapInitSocks(host,port);
	    }
	    break;
#endif
#ifdef EMACS
	  case 'e':
	    emacs_mode = TRUE;
	    {
	      File fd;
	      strcpy (emacs_tmp, ++p);
	      if ((fd = fopen (emacs_tmp, "w")) == NIL)
		fprintf(stderr, "[ Warning: unable to communicate with emacs: failed to open %s ]\n", emacs_tmp);
	      fclose (fd);
	      unlink (emacs_tmp);
	      p = *++argv;
	      --argc;
	      strcpy (emacs_tmp2, p);
	      if ((fd = fopen (emacs_tmp2, "w")) == NIL)
		fprintf(stderr, "Unable to communicate with emacs: failed to open %s\n", emacs_tmp2);
	      fclose (fd);
	      unlink (emacs_tmp2);
	    }
	    break;
#endif /* EMACS */
	  case 'f':
	    init_args->FastBoot = TRUE;
	    break;
#ifdef MPWSHELL
	  case 'm':
	    if (*++p == 'p' && *++p == 'w' && *++p == '\0')
	      mpwshell = TRUE;
	    break;
#endif
	  case 's':
	    ssize = &(init_args->StackSize);
	    if (p[1] == 'l') {
	      p++;
	      ssize = &(init_args->SchedulerLoop);
	    }
	    goto GetSize;
	  case 'h':
	    ssize = &(init_args->HeapSize);
	    goto GetSize;
	  case 't':
	    ssize = &(init_args->TrailSize);
	  GetSize:
	    if (*++p == '\0')
	      {
		if (argc > 1)
		  --argc, p = *++argv;
		else
		  {
		    fprintf(stderr,"[ YAP unrecoverable error: missing size in flag %s ]", argv[-1]);
		  YapExit(1);
		  }
	      }
	    {
	      int i = 0, ch;
	      while ((ch = *p++) >= '0' && ch <= '9')
		i = i * 10 + ch - '0';
	      if (ch)
		{
		  fprintf(stderr,"[ YAP unrecoverable error: illegal size specification %s ]", argv[-1]);
		  YapExit(1);
		}
	      *ssize = i;
	    }
	    break;
#ifdef DEBUG
	  case 'p':
	    YapSetOutputMessage();
	    output_msg = TRUE;
	    break;
#endif
	  case 'L':
	    p++;
	    while (*p != '\0' && (*p == ' ' || *p == '\t'))
	      p++;
	    /* skip zeroth argument */
	    argc--;
	    if (argc == 0) {
	      fprintf(stderr," [ YAP unrecoverable error: missing file name with option 'l' ]\n");
	      exit(1);
	    }
	    argv++;
	    if (p[0] == '-' && p[1] == '-'&& p[2] == '\0') {
	      /* we're done here */
	      argc = 1;
	    }
	    init_args->YapPrologBootFile = *argv;
	    init_args->HaltAfterConsult = TRUE;
	    break;
	  case 'l':
	    if ((*argv)[0] == '\0') 
	      init_args->YapPrologBootFile = *argv;
	    else {
	      argc--;
	      if (argc == 0) {
		fprintf(stderr," [ YAP unrecoverable error: missing file name with option 'l' ]\n");
		exit(1);
	      }
	      argv++;
	      init_args->YapPrologBootFile = *argv;
	    }
	    break;
	  case '-':
	    /* skip remaining arguments */
	    argc = 1;
	    break;
	  default:
	    {
	      fprintf(stderr,"[ YAP unrecoverable error: unknown switch -%c ]\n", *p);
	      exit(1);
	    }
	  }
      else {
	filename = p;
      }
    }
  return(BootMode);
}

static int
init_standard_system(int argc, char *argv[])
{
  int BootMode;
  yap_init_args init_args;

  init_args.SavedState = NULL;
  init_args.HeapSize = 0;
  init_args.StackSize = 0;
  init_args.TrailSize = 0;
  init_args.YapLibDir = NULL;
  init_args.YapPrologBootFile = NULL;
  init_args.HaltAfterConsult = FALSE;
  init_args.FastBoot = FALSE;
  init_args.NumberWorkers = 1;
  init_args.SchedulerLoop = 10;
  init_args.DelayedReleaseLoad = 3;
  init_args.Argc = argc;
  init_args.Argv = argv;

  BootMode = parse_yap_arguments(argc,argv,&init_args);

  /* init memory */
  if (BootMode == YAP_BOOT_FROM_PROLOG)
    {

      YapInit(&init_args);

    }
  else
    {
      if (filename == NULL)
	init_args.SavedState = StartUpFile;
      else
	init_args.SavedState = filename;

      BootMode = YapInit(&init_args);

    }

  return(BootMode);

}


static void
exec_top_level(int BootMode, char *filename)
{
  Term atomfalse;
  Atom livegoal;

  if (BootMode == YAP_BOOT_FROM_SAVED_STACKS)
    {
      /* continue executing from the frozen stacks */
      YapContinueGoal();
    }
  else if (BootMode == YAP_BOOT_FROM_PROLOG)
    {
      Atom livegoal;
      /* read the bootfile */
      do_bootfile (filename ? filename : BootFile);
      livegoal = FullLookupAtom("$live");
      /* initialise the top-level */
      YapPutValue(livegoal, MkAtomTerm (LookupAtom("true")));
    }
      /* the top-level is now ready */

  /* read it before case someone, that is, Ashwin, hides
     the atom false away ;-).
  */
  livegoal = FullLookupAtom("$live");
  atomfalse = MkAtomTerm (LookupAtom("false"));
  while (YapGetValue (livegoal) != atomfalse) {
    do_top_goal (MkAtomTerm (livegoal));
  }
  YapExit(0);
}

#ifdef LIGHT
int
_main (int argc, char **argv)
#else
int
main (int argc, char **argv)
#endif
{
  int BootMode;

#ifdef SIMICS
  fprintf(stdout,"Entering YAP\n");
#endif /* SIMICS */
  BootMode = init_standard_system(argc, argv);
  if (BootMode == YAP_BOOT_FROM_SAVED_ERROR) {
    fprintf(stderr,"[ FATAL ERROR: could not find saved state ]\n");
    exit(1);
  }
#if defined(YAPOR) || defined(TABLING)
  start_workers();
#endif /* YAPOR || TABLING */
  exec_top_level(BootMode, filename);

  return(0);
}

