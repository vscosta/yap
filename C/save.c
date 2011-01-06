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
* File:		save.c							 *
* Last rev:								 *
* mods:									 *
* comments:	saving and restoring a Prolog computation		 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "@(#)save.c	1.3 3/15/90";
#endif

#if _MSC_VER || defined(__MINGW32__)
#include <windows.h>
#include <psapi.h>
#endif
#include "absmi.h"
#include "alloc.h"
#if USE_DL_MALLOC
#include "dlmalloc.h"
#endif
#include "yapio.h"
#include "sshift.h"
#include "Foreign.h"
#if HAVE_STRING_H
#include <string.h>
#endif
#if !HAVE_STRNCAT
#define strncat(X,Y,Z) strcat(X,Y)
#endif
#if !HAVE_STRNCPY
#define strncpy(X,Y,Z) strcpy(X,Y)
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#include "iopreds.h"

/*********  hack for accesing several kinds of terms. Should be cleaned **/

static char StartUpFile[] = "startup.yss";

static char end_msg[256] ="*** End of YAP saved state *****";


#ifdef DEBUG

/*
 * 
 #FOR DEBUGGING define DEBUG_RESTORE0 to check the file stuff,
 #define DEBUG_RESTORE1 to see if it is able to prepare the chain,
 #define DEBUG_RESTORE2 to see how things are going,
 #define DEBUG_RESTORE3 to check if the atom chain is still a working chain,
 * define DEBUG_RESTORE4 if you want to set the output for some
 * particular file,
 * define DEBUG_RESTORE5 if you want to see how the stacks are being
 * cleaned up,
 * define DEBUG_RESTORE6 if you want to follow the execution in 
 *
 * Also a file is defined where you can write things, by default stderr 
 *
 * Good Luck 
 */

#endif

STATIC_PROTO(int   myread, (int, char *, Int));
STATIC_PROTO(Int   mywrite, (int, char *, Int));
STATIC_PROTO(int   open_file, (char *, int));
STATIC_PROTO(int   close_file, (void));
STATIC_PROTO(Int   putout, (CELL));
STATIC_PROTO(Int   putcellptr, (CELL *));
STATIC_PROTO(CELL  get_cell, (void));
STATIC_PROTO(CELL  *get_cellptr, ( /* CELL * */ void));
STATIC_PROTO(int   put_info, (int, int));
STATIC_PROTO(int   save_regs, (int));
STATIC_PROTO(int   save_code_info, (void));
STATIC_PROTO(int   save_heap, (void));
STATIC_PROTO(int   save_stacks, (int));
STATIC_PROTO(int   save_crc, (void));
STATIC_PROTO(Int   do_save, (int));
STATIC_PROTO(Int   p_save2, (void));
STATIC_PROTO(Int   p_save_program, (void));
STATIC_PROTO(int   check_header, (CELL *, CELL *, CELL *, CELL *));
STATIC_PROTO(int   get_heap_info, (void));
STATIC_PROTO(int   get_regs, (int));
STATIC_PROTO(int   get_insts, (OPCODE []));
STATIC_PROTO(int   get_hash, (void));
STATIC_PROTO(int   CopyCode, (void));
STATIC_PROTO(int   CopyStacks, (void));
STATIC_PROTO(int   get_coded, (int, OPCODE []));
STATIC_PROTO(void  restore_codes, (void));
STATIC_PROTO(Term  AdjustDBTerm, (Term, Term *));
STATIC_PROTO(void  RestoreDB, (DBEntry *));
STATIC_PROTO(void  RestoreDBTerm, (DBTerm *, int));
STATIC_PROTO(void  CleanClauses, (yamop *, yamop *,PredEntry *));
STATIC_PROTO(void  rehash, (CELL *, int, int));
STATIC_PROTO(void  CleanCode, (PredEntry *));
STATIC_PROTO(void  RestoreEntries, (PropEntry *, int));
STATIC_PROTO(void  RestoreFreeSpace, (void));
STATIC_PROTO(void  restore_heap, (void));
#ifdef DEBUG_RESTORE3
STATIC_PROTO(void  ShowAtoms, (void));
STATIC_PROTO(void  ShowEntries, (PropEntry *));
#endif
STATIC_PROTO(int   OpenRestore, (char *, char *, CELL *, CELL *, CELL *, CELL *));
STATIC_PROTO(void  CloseRestore, (void));
#ifndef _WIN32
STATIC_PROTO(int  check_opcodes, (OPCODE []));
#endif
STATIC_PROTO(void  RestoreHeap, (OPCODE []));
STATIC_PROTO(Int  p_restore, (void));
STATIC_PROTO(void  restore_heap_regs, (void));
STATIC_PROTO(void  restore_regs, (int));
#ifdef MACYAP
STATIC_PROTO(void NewFileInfo, (long, long));
extern int      DefVol;
#endif

#ifdef _WIN32
#if HAVE_IO_H
#include <io.h>
#endif
#endif

#ifdef LIGHT

#include <unix.h>
#include <strings.h>

void 
LightBug(char *);

static void 
LightBug(s)
	char           *s;
{
}

#endif				/* LIGHT */

static Int
do_system_error(yap_error_number etype, const char *msg)
{
#if HAVE_SNPRINTF
#if HAVE_STRERROR
  snprintf(Yap_ErrorSay,MAX_ERROR_MSG_SIZE,"%s (%s when reading %s)", msg, strerror(errno), Yap_FileNameBuf);
#else
  snprintf(Yap_ErrorSay,MAX_ERROR_MSG_SIZE,"%s, (system error %d when reading %s)",msg,errno,Yap_FileNameBuf);  
#endif
#else
#if HAVE_STRERROR
  sprintf(Yap_ErrorSay,"%s, (%s when reading %s)",msg,strerror(errno),Yap_FileNameBuf);
#else
  sprintf(Yap_ErrorSay,"%s, (system error %d when reading %s)",msg,errno,Yap_FileNameBuf);  
#endif
#endif
  Yap_ErrorMessage = Yap_ErrorSay;
  Yap_Error_TYPE = etype;
  return -1;
}


inline static
int myread(int fd, char *buffer, Int len) {
  ssize_t nread;

  while (len > 0) {
    nread = read(fd, buffer,  (int)len);
    if (nread < 1) {
      return do_system_error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,"bad read on saved state");
    }
    buffer += nread;
    len -= nread;
  }
  return len;
}

inline static
Int
mywrite(int fd, char *buff, Int len) {
  ssize_t nwritten;

  while (len > 0) {
    nwritten = write(fd, buff, (size_t)len);
    if (nwritten < 0) {
      return do_system_error(SYSTEM_ERROR,"bad write on saved state");
    }
    buff += nwritten;
    len -= nwritten;
  }
  return len;
}

#define FullSaved		1

/* Where the code was before */



typedef CELL   *CELLPOINTER;

static int      splfild = 0;

#ifdef DEBUG

#ifdef DEBUG_RESTORE4
static FILE    *errout;
#else
#define errout Yap_stderr
#endif

#endif				/* DEBUG */

static Int      OldHeapUsed;

static CELL     which_save;

/* Open a file to read or to write */
static int 
open_file(char *my_file, int flag)
{
  int splfild;

#ifdef M_WILLIAMS
  if (flag & O_CREAT)
    splfild = creat(my_file, flag);
  else
    splfild = open(my_file, flag);
  if (splfild < 0) {
#else
#ifdef O_BINARY
#if _MSC_VER
    if ((splfild = _open(my_file, flag | O_BINARY), _S_IREAD | _S_IWRITE) < 0)
#else
    if ((splfild = open(my_file, flag | O_BINARY, 0775)) < 0)
#endif
#else  /* O_BINARY */
    if ((splfild = open(my_file, flag, 0755)) < 0)
#endif  /* O_BINARY */
#endif 	/* M_WILLIAMS */
      {
	splfild = 0;	/* We do not have an open file */
	return -1;
      }
#ifdef undf0
      fprintf(errout, "Opened file %s\n", my_file);
#endif
      return splfild;
}

static int 
close_file(void)
{
  if (splfild == 0)
    return 0;
  if (close(splfild) < 0)
    return do_system_error(SYSTEM_ERROR,"bad close on saved state");
  splfild = 0;
  return 1;
}

/* stores a cell in a file */
static Int 
putout(CELL l)
{
  return mywrite(splfild, (char *) &l, sizeof(CELL));
}

/* stores a pointer to a cell in a file */
static Int 
putcellptr(CELL *l)
{
  return mywrite(splfild, (char *) &l, sizeof(CELLPOINTER));
}

/* gets a cell from a file */
static CELL 
get_cell(void)
{
  CELL            l;
  myread(splfild, (char *) &l, Unsigned(sizeof(CELL)));
  return (l);
}

/* gets a cell from a file */
static CELL 
get_header_cell(void)
{
  CELL l;
  int count = 0, n;
  while (count < sizeof(CELL)) {
    if ((n = read(splfild, &l, sizeof(CELL)-count)) < 0) {
      do_system_error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,"failed to read saved state header");
      return 0L;
    }
    count += n;
  }
  return l;
}

/* gets a pointer to cell from a file */
static CELL    *
get_cellptr(void)
{
  CELL           *l;

  if (myread(splfild, (char *) &l, Unsigned(sizeof(CELLPOINTER))) < 0)
    return NULL;
  return (l);
}

/*
 * writes the header (at the moment YAPV*), info about what kind of saved
 * set, the work size, and the space ocuppied 
 */
static int 
put_info(int info, int mode)
{
  char     msg[256];

  sprintf(msg, "#!/bin/sh\nexec_dir=${YAPBINDIR:-%s}\nexec $exec_dir/yap $0 \"$@\"\n%cYAP-%s", YAP_BINDIR, 1, YAP_SVERSION);
  if (mywrite(splfild, msg, strlen(msg) + 1))
    return -1;
  if (putout(Unsigned(info)) < 0)
    return -1;
  /* say whether we just saved the heap or everything */
  if (putout(mode) < 0)
    return -1;
  /* current state of stacks, to be used by SavedInfo */
  /* space available in heap area */
  if (putout(Unsigned(Yap_GlobalBase)-Unsigned(Yap_HeapBase)) < 0)
    return -1;
  /* space available for stacks */
  if (putout(Unsigned(Yap_LocalBase)-Unsigned(Yap_GlobalBase)) < 0)
    return -1;
  /* space available for trail */
  if (putout(Unsigned(Yap_TrailTop)-Unsigned(Yap_TrailBase)) < 0)
    return -1;
  /* Space used in heap area */
  if (putout(Unsigned(HeapTop)-Unsigned(Yap_HeapBase)) < 0)
    return -1;
  /* Space used for local stack */
  if (putout(Unsigned(LCL0)-Unsigned(ASP)) < 0)
    return -1;
  /* Space used for global stack */
  if (putout(Unsigned(H) - Unsigned(Yap_GlobalBase)) < 0)
    return -1;
  /* Space used for trail */
  if (putout(Unsigned(TR) - Unsigned(Yap_TrailBase)) < 0)
    return -1;
  return 0;
}

static int
save_regs(int mode)
{
  /* save all registers */
  if (putout((CELL)compile_arrays) < 0)
    return -1;    
  if (mode == DO_EVERYTHING) {
    if (putcellptr((CELL *)CP) < 0)
      return -1;
    if (putcellptr(ENV) < 0)
      return -1;
    if (putcellptr(ASP) < 0)
      return -1;
    /* if (putout((CELL)N) < 0)
       return -1; */
    if (putcellptr(H0) < 0)
      return -1;
    if (putcellptr(LCL0) < 0)
      return -1;
    if (putcellptr(H) < 0)
      return -1;
    if (putcellptr(HB) < 0)
      return -1;
    if (putcellptr((CELL *)B) < 0)
      return -1;
    if (putcellptr((CELL *)TR) < 0)
      return -1;
    if (putcellptr(YENV) < 0)
      return -1;
    if (putcellptr(S) < 0)
      return -1;
    if (putcellptr((CELL *)P) < 0)
      return -1;
    if (putout(CreepFlag) < 0)
      return -1;
    if (putcellptr((CELL *)EX) < 0)
      return -1;
#if defined(SBA) || defined(TABLING)
    if (putcellptr(H_FZ) < 0)
      return -1;
    if (putcellptr((CELL *)B_FZ) < 0)
      return -1;
    if (putcellptr((CELL *)TR_FZ) < 0)
      return -1;
#endif /* SBA || TABLING */
  }
  if (putout(CurrentModule) < 0)
    return -1;
  if (mode == DO_EVERYTHING) {
#ifdef COROUTINING
    if (putout(WokenGoals) < 0)
      return -1;
#endif
#ifdef  DEPTH_LIMIT
    if (putout(DEPTH) < 0)
      return -1;
#endif
    if (putout(GcGeneration) < 0)
      return -1;
    if (putout(GcPhase) < 0)
      return -1;
    if (putout(GcCurrentPhase) < 0)
      return -1;
  }
  /* The operand base */
  if (putcellptr(CellPtr(XREGS)) < 0)
    return -1;
  if (putout(which_save) < 0)
    return -1;
  /* Now start by saving the code */
  /* the heap boundaries */
  if (putcellptr(CellPtr(Yap_HeapBase)) < 0)
    return -1;
  if (putcellptr(CellPtr(HeapTop)) < 0)
    return -1;
  /* and the space it ocuppies */
  if (putout(Unsigned(Yap_heap_regs->heap_used)) < 0)
    return -1;
  /* Then the start of the free code */
  if (putcellptr(CellPtr(FreeBlocks)) < 0)
    return -1;
  if (putcellptr(CellPtr(AuxBase)) < 0)
    return -1;
  if (putcellptr(AuxSp) < 0)
    return -1;
  if (putcellptr(CellPtr(AuxTop)) < 0)
    return -1;
  if (putcellptr(CellPtr(ScratchPad.ptr)) < 0)
    return -1;
  if (putout(ScratchPad.sz) < 0)
    return -1;
  if (putout(ScratchPad.msz) < 0)
    return -1;
  if (mode == DO_EVERYTHING) {
    /* put the old trail base, just in case it moves again */
    if (putout(ARG1) < 0)
      return -1;
    if (which_save == 2) {
      if (putout(ARG2) < 0)
	return -1;
    }
    if (putcellptr(CellPtr(Yap_TrailBase)) < 0)
      return -1;
  }
  return 0;
}

static int
save_code_info(void)
{

  /* First the instructions */
  {
    op_numbers i;

    OPCODE my_ops[_std_top+1];
    for (i = _Ystop; i <= _std_top; ++i)
      my_ops[i] = Yap_opcode(i);
    if (mywrite(splfild, (char *)my_ops, sizeof(OPCODE)*(_std_top+1)) < 0)
      return -1;
  }
  /* and the current character codes */
  if (mywrite(splfild, Yap_chtype, NUMBER_OF_CHARS) < 0)
    return -1;
  return 0;
}

static int
save_heap(void)
{
#ifdef USE_SYSTEM_MALLOC
  return -1;
#endif
  int j;
  /* Then save the whole heap */
  Yap_ResetConsultStack();
  j = Unsigned(HeapTop) - Unsigned(Yap_HeapBase);
  /* store 10 more cells because of the memory manager */
  if (mywrite(splfild, (char *) Yap_HeapBase, j) < 0)
    return -1;
  return 0;
}

static int
save_stacks(int mode)
{
  int j;
  
  switch (mode) {
  case DO_EVERYTHING:
    /* Now, go and save the state */
    /* Save the local stack */
    j = Unsigned(LCL0) - Unsigned(ASP);
    if (mywrite(splfild, (char *) ASP, j) < 0)
      return -1;
    /* Save the global stack */
    j = Unsigned(H) - Unsigned(Yap_GlobalBase);
    if (mywrite(splfild, (char *) Yap_GlobalBase, j) < 0)
      return -1;
    /* Save the trail */
    j = Unsigned(TR) - Unsigned(Yap_TrailBase);
    if (mywrite(splfild, (char *) Yap_TrailBase, j) < 0)
      return -1;
    break;
  case DO_ONLY_CODE:
    {
      tr_fr_ptr tr_ptr = TR; 
      while (tr_ptr != (tr_fr_ptr)Yap_TrailBase) {
	CELL val = TrailTerm(tr_ptr-1);
	if (IsVarTerm(val)) {
	  CELL *d1 = VarOfTerm(val);
	  if (d1 < (CELL *)HeapTop) {
	    if (putout(val) < 0)
	      return -1;
	  }
	} else if (IsPairTerm(val)) {
	  CELL *d1 = RepPair(val);
	  if (d1 < (CELL *)HeapTop) {
	    if (putout(val) < 0)
	      return -1;
	  }
	}
	tr_ptr--;
      }
    }
    if (putcellptr(NULL) < 0)
      return -1;
    break;
  }
  return 0;
}

static int
save_crc(void)
{
  /* Save a CRC */
  return mywrite(splfild, end_msg, 256);
}

static Int
do_save(int mode) {
  Term t1 = Deref(ARG1);

  if (Yap_HoleSize) {
    Yap_Error(SYSTEM_ERROR,MkAtomTerm(Yap_LookupAtom(Yap_FileNameBuf)),
	      "restore/1: address space has holes of size %ld, cannot save", (long int)Yap_HoleSize);
    return FALSE;
  }
  if (!Yap_GetName(Yap_FileNameBuf, YAP_FILENAME_MAX, t1)) {
    Yap_Error(TYPE_ERROR_LIST,t1,"save/1");
    return FALSE;
  }
  Yap_CloseStreams(TRUE);
  if ((splfild = open_file(Yap_FileNameBuf, O_WRONLY | O_CREAT)) < 0) {
    Yap_Error(SYSTEM_ERROR,MkAtomTerm(Yap_LookupAtom(Yap_FileNameBuf)),
	  "restore/1, open(%s)", strerror(errno));
    return(FALSE);
  }
  if (put_info(FullSaved, mode) < 0)
    return -1;
  if (save_regs(mode) < 0)
    return -1;
  if (save_code_info() < 0)
    return -1;
  if (save_heap() < 0)
    return -1;
  if (save_stacks(mode) < 0)
    return -1;
  if (save_crc() < 0)
    return -1;
  close_file();
  return (TRUE);
}

/* Saves a complete prolog environment */
static Int 
p_save2(void)
{
  Int res;

  Term t;
#if defined(YAPOR) && !defined(THREADS)
  if (number_workers != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,
	       "cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#elif defined(THREADS)
  if (NOfThreads != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,
	       "cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#endif
  /* avoid double saves */
  if (IsNonVarTerm(t = Deref(ARG2)))
    return TRUE;
  if (!Yap_unify(ARG2,MkIntTerm(1)))
    return FALSE;
  which_save = 2;
  Yap_StartSlots();
  res = do_save(DO_EVERYTHING);
  Yap_CloseSlots();
  return res;
}

/* Just save the program, not the stacks */
static Int 
p_save_program(void)
{
  which_save = 0;
  return do_save(DO_ONLY_CODE);
}

/* Now, to restore the saved code */

/* First check out if we are dealing with a valid file */
static int 
check_header(CELL *info, CELL *ATrail, CELL *AStack, CELL *AHeap)
{
  char pp[256];
  char msg[256];
  CELL hp_size, gb_size, lc_size, tr_size, mode;
  int n;

  /* make sure we always check if there are enough bytes */
  /* skip the first line */
  pp[0] = '\0';
  do {
    if ((n = read(splfild, pp, 1)) <= 0) {
      do_system_error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,"failed to scan first line from saved state");
      return FAIL_RESTORE;
    }
  } while (pp[0] != 1);
  /* now check the version */
  sprintf(msg, "YAP-%s", YAP_SVERSION);
  {
    int count = 0, n, to_read = Unsigned(strlen(msg) + 1);
    while (count < to_read) {
      if ((n = read(splfild, pp, to_read-count)) <= 0) {
	do_system_error(PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM,"failed to scan version info from saved state");
	return FAIL_RESTORE;
      }
      count += n;
    }
  }
  if (strcmp(pp, msg) != 0) {
    Yap_ErrorMessage = Yap_ErrorSay;
    strncpy(Yap_ErrorMessage, "saved state ", MAX_ERROR_MSG_SIZE);
    strncat(Yap_ErrorMessage, Yap_FileNameBuf, MAX_ERROR_MSG_SIZE);
    strncat(Yap_ErrorMessage, " failed to match version ID", MAX_ERROR_MSG_SIZE);
    Yap_Error_TYPE = CONSISTENCY_ERROR;
    return FAIL_RESTORE;
  }
  /* check info on header */
  /* ignore info on saved state */
  *info = get_header_cell();
  if (Yap_ErrorMessage)
     return FAIL_RESTORE;
  /* check the restore mode */
  mode = get_header_cell();
  if (Yap_ErrorMessage)
     return FAIL_RESTORE;
  if (mode != DO_EVERYTHING && mode != DO_ONLY_CODE) {
    return FAIL_RESTORE;
  }
  /* ignore info on stacks size */
  *AHeap = get_header_cell();
  if (Yap_ErrorMessage) {
     return FAIL_RESTORE;
  }
  *AStack = get_header_cell();
  if (Yap_ErrorMessage) {
     return FAIL_RESTORE;
  }
  *ATrail = get_header_cell();
  if (Yap_ErrorMessage) {
     return FAIL_RESTORE;
  }
  /* now, check whether we got enough enough space to load the
     saved space */
  hp_size = get_cell();
  if (Yap_ErrorMessage)
     return FAIL_RESTORE;
  while (Yap_HeapBase != NULL &&
	 hp_size > Unsigned(HeapLim) - Unsigned(Yap_HeapBase)) {
    if(!Yap_growheap(FALSE, hp_size, NULL)) {
      return FAIL_RESTORE;
    }
  }
  if (mode == DO_EVERYTHING) {
    lc_size = get_cell();
    if (Yap_ErrorMessage)
      return FAIL_RESTORE;
    gb_size=get_cell();
    if (Yap_ErrorMessage)
      return FAIL_RESTORE;
    if (Yap_HeapBase != NULL && lc_size+gb_size > Unsigned(Yap_LocalBase) - Unsigned(Yap_GlobalBase)) {
      if (Yap_ErrorMessage != NULL)
	Yap_ErrorMessage = "could not allocate enough stack space";
      return FAIL_RESTORE;
    }
    if (Yap_HeapBase != NULL && (tr_size = get_cell()) > Unsigned(Yap_TrailTop) - Unsigned(Yap_TrailBase)) {
      if (Yap_ErrorMessage != NULL)
	Yap_ErrorMessage = "could not allocate enough trail space";
      return FAIL_RESTORE;
    }
  } else {
    /* skip cell size */
    get_header_cell();
    if (Yap_ErrorMessage)
      return FAIL_RESTORE;
    get_header_cell();
    if (Yap_ErrorMessage)
      return FAIL_RESTORE;
    get_header_cell();
    if (Yap_ErrorMessage)
      return FAIL_RESTORE;
  }
  return(mode);
}

/* Gets the state of the heap, and evaluates the related variables */
static int 
get_heap_info(void)
{
  OldHeapBase = (ADDR) get_cellptr();
  if (Yap_ErrorMessage)
      return -1;
  OldHeapTop = (ADDR) get_cellptr();

  if (Yap_ErrorMessage)
      return -1;
  OldHeapUsed = (Int) get_cell();
  if (Yap_ErrorMessage)
      return -1;
  FreeBlocks = (BlockHeader *) get_cellptr();
  if (Yap_ErrorMessage)
      return -1;
  AuxBase = (ADDR)get_cellptr();
  if (Yap_ErrorMessage)
      return -1;
  AuxSp = get_cellptr();
  if (Yap_ErrorMessage)
      return -1;
  AuxTop = (ADDR)get_cellptr();
  if (Yap_ErrorMessage)
      return -1;
  ScratchPad.ptr = (ADDR)get_cellptr();
  if (Yap_ErrorMessage)
      return -1;
  ScratchPad.sz = get_cell();
  if (Yap_ErrorMessage)
      return -1;
  ScratchPad.msz = get_cell();
  if (Yap_ErrorMessage)
      return -1;
  HDiff = Unsigned(Yap_HeapBase) - Unsigned(OldHeapBase);
  return 1;
}

/* Gets the register array */
/* Saves the old bases for the work areas */
/* and evaluates the difference from the old areas to the new ones */
static int 
get_regs(int flag)
{
  CELL           *NewGlobalBase = (CELL *)Yap_GlobalBase;
  CELL           *NewLCL0 = LCL0;
  CELL           *OldXREGS;

  /* Get regs */
  compile_arrays = (int)get_cell();
  if (Yap_ErrorMessage)
      return -1;
  if (flag == DO_EVERYTHING) {
    CP = (yamop *)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    ENV = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    ASP = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    /* N = get_cell(); */
    H0 = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    LCL0 = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    H = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    HB = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    B = (choiceptr)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    TR = (tr_fr_ptr)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    YENV = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    S = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    P = (yamop *)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    CreepFlag = get_cell();
    if (Yap_ErrorMessage)
      return -1;
    EX = (struct DB_TERM *)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
#if defined(SBA) || defined(TABLING)
    H_FZ = get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    B_FZ = (choiceptr)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    TR_FZ = (tr_fr_ptr)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
#endif /* SBA || TABLING */
  }
  CurrentModule = get_cell();
    if (Yap_ErrorMessage)
      return -1;
  if (flag == DO_EVERYTHING) {
#ifdef COROUTINING
    WokenGoals = get_cell();
    if (Yap_ErrorMessage)
      return -1;
#endif
#ifdef  DEPTH_LIMIT
    DEPTH = get_cell();
    if (Yap_ErrorMessage)
      return -1;
#endif
    GcGeneration = get_cell();
    if (Yap_ErrorMessage)
      return -1;
    GcPhase = get_cell();
    if (Yap_ErrorMessage)
      return -1;
    GcCurrentPhase = get_cell();
    if (Yap_ErrorMessage)
      return -1;
  }
  /* Get the old bases */
  OldXREGS = get_cellptr();
  if (Yap_ErrorMessage)
    return -1;
  which_save = get_cell();
  if (Yap_ErrorMessage)
    return -1;
  XDiff =  (CELL)XREGS - (CELL)OldXREGS;
  if (Yap_ErrorMessage)
    return -1;
  if (get_heap_info() < 0)
    return -1;
  if (flag == DO_EVERYTHING) {
    ARG1 = get_cell();
    if (Yap_ErrorMessage)
      return -1;
    if (which_save == 2) {
      ARG2 = get_cell();
      if (Yap_ErrorMessage)
	return -1;
    }
    /* get old trail base */
    OldTrailBase = (ADDR)get_cellptr();
    if (Yap_ErrorMessage)
      return -1;
    /* Save the old register where we can easily access them */
    OldASP = ASP;
    OldLCL0 = LCL0;
    OldGlobalBase = (CELL *)Yap_GlobalBase;
    OldH = H;
    OldTR = TR;
    GDiff = Unsigned(NewGlobalBase) - Unsigned(Yap_GlobalBase);
    GDiff0 = 0;
    LDiff = Unsigned(NewLCL0) - Unsigned(LCL0);
    TrDiff = LDiff;
    Yap_GlobalBase = (ADDR)NewGlobalBase;
    LCL0 = NewLCL0;
  }
  return 1;
}

/* Get the old opcodes and place them in a hash table */
static int 
get_insts(OPCODE old_ops[])
{
  return myread(splfild, (char *)old_ops, sizeof(OPCODE)*(_std_top+1));
}

/* Get the old atoms hash table */
static int 
get_hash(void)
{
  return myread(splfild, Yap_chtype , NUMBER_OF_CHARS);
}

/* Copy all of the old code to the new Heap */
static int 
CopyCode(void)
{
  if (myread(splfild, (char *) Yap_HeapBase, (Unsigned(OldHeapTop) - Unsigned(OldHeapBase))) < 0) {
    return -1;
  }
  return 1;
}

/* Copy the local and global stack and also the trail to their new home */
/* In REGS we still have nonadjusted values !! */
static int 
CopyStacks(void)
{
  Int             j;
  char           *NewASP;

  j = Unsigned(OldLCL0) - Unsigned(ASP);
  NewASP = (char *) (Unsigned(ASP) + (Unsigned(LCL0) - Unsigned(OldLCL0)));
  if (myread(splfild, (char *) NewASP, j) < 0)
    return -1;
  j = Unsigned(H) - Unsigned(OldGlobalBase);
  if (myread(splfild, (char *) Yap_GlobalBase, j) < 0)
    return -1;
  j = Unsigned(TR) - Unsigned(OldTrailBase);
  if (myread(splfild, Yap_TrailBase, j))
    return -1;
  return 1;
}

/* Copy the local and global stack and also the trail to their new home */
/* In REGS we still have nonadjusted values !! */
static int
CopyTrailEntries(void)
{
  CELL           entry, *Entries;

  Entries = (CELL *)Yap_TrailBase;
  do {
    *Entries++ = entry = get_cell();
    if (Yap_ErrorMessage)
      return -1;
  } while ((CODEADDR)entry != NULL);
  return 1;
}

/* get things which are saved in the file */
static int 
get_coded(int flag, OPCODE old_ops[])
{
  char my_end_msg[256];
  
  if (get_regs(flag) < 0)
    return -1;
  if (get_insts(old_ops) < 0)
    return -1;
  if (get_hash() < 0)
    return -1;
  if (CopyCode() < 0)
    return -1;
  switch (flag) {
  case DO_EVERYTHING:
    if (CopyStacks() < 0)
      return -1;
    break;
  case DO_ONLY_CODE:
    if (CopyTrailEntries() < 0)
      return -1;
    break;
  }
  /* Check CRC */
  if (myread(splfild, my_end_msg, 256) < 0)
    return -1;
  if (strcmp(end_msg,my_end_msg) != 0) {
    Yap_ErrorMessage = "bad trailing CRC in saved state";
    return -1;
  }
  return 1;
}

/* restore some heap registers */
static void 
restore_heap_regs(void)
{
  if (HeapTop) {
    HeapTop = AddrAdjust(HeapTop);
    *((YAP_SEG_SIZE *) HeapTop) = InUseFlag;
  }
  HeapMax = Yap_heap_regs->heap_used = OldHeapUsed;
  HeapLim = Yap_GlobalBase;
}

/* adjust abstract machine registers */
static void 
restore_regs(int flag)
{
  restore_heap_regs();
  if (CurrentModule) {
    CurrentModule = AtomTermAdjust(CurrentModule);;
  }
  if (flag == DO_EVERYTHING) {
    CP = PtoOpAdjust(CP);
    ENV = PtoLocAdjust(ENV);
    ASP = PtoLocAdjust(ASP);
    H = PtoGloAdjust(H);
    B = (choiceptr)PtoLocAdjust(CellPtr(B));
    TR = PtoTRAdjust(TR);
    P = PtoOpAdjust(P);
    HB = PtoLocAdjust(HB);
    YENV = PtoLocAdjust(YENV);
    S = PtoGloAdjust(S);
    if (EX) {
      EX = DBTermAdjust(EX);
      RestoreDBTerm(EX, TRUE);
    }
    WokenGoals = AbsAppl(PtoGloAdjust(RepAppl(WokenGoals)));
  }
}

static void
recompute_mask(DBRef dbr)
{
  if (dbr->Flags & DBNoVars) {
    dbr->Mask = Yap_EvalMasks((Term) dbr->DBT.Entry, &(dbr->Key));
  } else if (dbr->Flags & DBComplex) {
    /* This is quite nasty, we want to recalculate the mask but
       we don't want to rebuild the whole term. We'll just build whatever we
       need to recompute the mask.
    */
    CELL *x = (CELL *)HeapTop, *tp;
    unsigned int Arity, i;
    Term out;
    char *tbase = CharP(dbr->DBT.Contents-1);

    if (IsPairTerm(dbr->DBT.Entry)) {

      out = AbsPair(x);
      Arity = 2;
      tp = (CELL *)(tbase + (CELL) RepPair(dbr->DBT.Entry));
    } else {
      Functor f;
    
      tp = (CELL *)(tbase + (CELL) RepAppl(dbr->DBT.Entry));
      f = (Functor)(*tp++);
      out = AbsAppl(x);
      Arity = ArityOfFunctor(f);
      *x++ = (CELL)f;
      if (Arity > 3) Arity = 3;
    }
    for (i = 0; i < Arity; i++) {
      register Term   tw = *tp++;
      if (IsVarTerm(tw)) {
	RESET_VARIABLE(x);
      } else if (IsApplTerm(tw)) {
	/* just fetch the functor from where it is in the data-base.
	   This guarantees we have access to references and friends. */
	CELL offset = (CELL)RepAppl(tw);
	if (offset > dbr->DBT.NOfCells*sizeof(CELL))
	  *x = tw;
	else
	  *x = AbsAppl((CELL *)(tbase + offset));
      } else if (IsAtomicTerm(tw)) {
	*x = tw;
      } else if (IsPairTerm(tw)) {
	*x = AbsPair(x);
      }
      x++;
    }
    dbr->Mask = Yap_EvalMasks(out, &(dbr->Key));
  }
}

#define HASH_SHIFT 6

/*
 * This is used to make an hash table correct, after displacing its elements,
 * HCEnd should point to an area of free space, usually in the heap. The
 * routine is very dependent on the hash function used, and it destroys the
 * previous "hit" order 
 */
static void 
rehash(CELL *oldcode, int NOfE, int KindOfEntries)
{
  register CELL  *savep, *basep;
  CELL           *oldp = oldcode;
  int             TableSize = NOfE - 1, NOfEntries;
  register int    i;
  int             hash;
  CELL            WorkTerm, failplace = 0;
  CELL           *Base = oldcode;

  if (HDiff == 0)
      return;
  basep = H;
  if (H + (NOfE*2) > ASP) {
    basep = (CELL *)TR;
    if (basep + (NOfE*2) > (CELL *)Yap_TrailTop) {
      if (!Yap_growtrail((ADDR)(basep + (NOfE*2))-Yap_TrailTop, TRUE)) {
	Yap_Error(OUT_OF_TRAIL_ERROR, TermNil,
	      "not enough space to restore hash tables for indexing");
	Yap_exit(1);
      }
    }
  }
  for (i = 0; i < NOfE; ++i) {
    if (*oldp == 0) {
      failplace = oldp[1];
      break;
    }
    oldp += 2;
  }
  savep = basep;
  oldp = oldcode;
  for (i = 0; i < NOfE; ++i) {
    if (*oldp != 0) {
      savep[0] = oldp[0];
      savep[1] = oldp[1];
      oldp[0] = 0;
      oldp[1] = failplace;
      savep += 2;
    }
    oldp += 2;
  }
  NOfEntries = (savep - basep)/2;
  savep = basep;
  for (i = 0; i < NOfEntries; ++i) {
    register Int    d;
    CELL *hentry;

    WorkTerm = savep[i*2];
    hash = (Unsigned(WorkTerm) >> HASH_SHIFT) & TableSize;
    hentry = Base + hash * 2;
    d = TableSize & (Unsigned(WorkTerm) | 1);
    while (*hentry) {
#ifdef DEBUG
#ifdef CLASHES
      ++clashes;
#endif /* CLASHES */
#endif /* DEBUG */
      hash = (hash + d) & TableSize;
      hentry = Base + hash * 2;
    }
    hentry[0] = WorkTerm;
    hentry[1] = savep[i*2+1];
  }
}

static void
RestoreSWIHash(void)
{
  Yap_InitSWIHash();
}


#include "rheap.h"

/* restore the atom entries which are invisible for the user */
static void 
RestoreIOStructures(void)
{
  Yap_InitStdStreams();
}

static void 
RestoreFreeSpace(void)
{
#if USE_DL_MALLOC
  Yap_av = (struct malloc_state *)AddrAdjust((ADDR)Yap_av);
  Yap_RestoreDLMalloc();
  if (AuxSp != NULL) {
    if (AuxBase < OldHeapBase || AuxBase > OldHeapTop) {
      AuxSp = NULL;
      AuxBase = NULL;
      AuxTop = NULL;
    } else {
      AuxSp = PtoHeapCellAdjust(AuxSp);
      AuxBase = AddrAdjust(AuxBase);
      AuxTop = AddrAdjust(AuxTop);
      ScratchPad.ptr = AddrAdjust(ScratchPad.ptr);
    }
  }
#else
  /* restores the list of free space, with its curious structure */
  BlockHeader *bpt, *bsz;

  if (FreeBlocks != NULL)
    FreeBlocks = BlockAdjust(FreeBlocks);
  bpt = FreeBlocks;
  if (AuxSp != NULL)
    AuxSp = CellPtoHeapAdjust(AuxSp);
  if (AuxTop != NULL)
    AuxTop = AddrAdjust(AuxTop);
  while (bpt != NULL) {
    if (bpt->b_next != NULL) {
      bsz = bpt->b_next = BlockAdjust(bpt->b_next);
      while (bsz != NULL) {
	if (bsz->b_next_size != NULL)
	  bsz->b_next_size = BlockAdjust(bsz->b_next_size);
	if (bsz->b_next != NULL)
	  bsz->b_next = BlockAdjust(bsz->b_next);
	bsz = bsz->b_next;
      }
    }
    if (bpt->b_next_size != NULL)
      bpt->b_next_size = BlockAdjust(bpt->b_next_size);
    bpt = bpt->b_next_size;
  }
  *((YAP_SEG_SIZE *) HeapTop) = InUseFlag;
#endif
}

static void
RestoreAtomList(Atom atm)
{
  AtomEntry      *at;

  at = RepAtom(atm);
  if (EndOfPAEntr(at))
    return;
  do {
    RestoreAtom(at);
    at = RepAtom(at->NextOfAE);
  } while (!EndOfPAEntr(at));
}


static void
RestoreHashPreds(void)
{
  UInt size = PredHashTableSize;
  int malloced = FALSE;
  PredEntry **np;
  UInt i;
  PredEntry **oldp = PredHash;

  np = (PredEntry **) Yap_AllocAtomSpace(sizeof(PredEntry **)*size);
  if (!np) {
    if (!(np = (PredEntry **) malloc(sizeof(PredEntry **)*size))) {
	Yap_Error(FATAL_ERROR,TermNil,"Could not allocate space for pred table");
	return;
      }
    malloced = TRUE;
  }
  for (i = 0; i < size; i++) {
    np[i] = NULL;
  }
  for (i = 0; i < PredHashTableSize; i++) {
    PredEntry *p = oldp[i];

    if (p)
      p = PredEntryAdjust(p);
    while (p) {
      Prop nextp;
      UInt hsh;
      
      if (p->NextOfPE)
	p->NextOfPE = PropAdjust(p->NextOfPE);
      nextp = p->NextOfPE;
      CleanCode(p);
      hsh = PRED_HASH(p->FunctorOfPred, p->ModuleOfPred, size);
      p->NextOfPE = AbsPredProp(np[hsh]);
      np[hsh] = p;
      p = RepPredProp(nextp);
    }
  }
  for (i = 0; i < size; i++) {
    PredHash[i] = np[i];
  }
  if (malloced)
    free((ADDR)np);
  else
    Yap_FreeAtomSpace((ADDR)np);
}

/*
 * This is the really tough part, to restore the whole of the heap 
 */
static void 
restore_heap(void)
{
  restore_codes();
  RestoreIOStructures();
}


#ifdef DEBUG_RESTORE3
static void 
ShowEntries(pp)
	PropEntry      *pp;
{
  while (!EndOfPAEntr(pp)) {
    fprintf(Yap_stderr,"Estou a ver a prop %x em %x\n", pp->KindOfPE, pp);
    pp = RepProp(pp->NextOfPE);
  }
}

static void 
ShowAtoms()
{
  AtomHashEntry  *HashPtr = HashChain;
  register int    i;
  for (i = 0; i < AtomHashTableSize; ++i) {
    if (HashPtr->Entry != NIL) {
      AtomEntry      *at;
      at = RepAtom(HashPtr->Entry);
      do {
	fprintf(Yap_stderr,"Passei ao %s em %x\n", at->StrOfAE, at);
	ShowEntries(RepProp(at->PropsOfAE));
      } while (!EndOfPAEntr(at = RepAtom(at->NextOfAE)));
    }
    HashPtr++;
  }
  HashPtr = WideHashChain;
  for (i = 0; i < WideAtomHashTableSize; ++i) {
    if (HashPtr->Entry != NIL) {
      AtomEntry      *at;
      at = RepAtom(HashPtr->Entry);
      do {
	fprintf(Yap_stderr,"Passei ao %s em %x\n", at->StrOfAE, at);
	ShowEntries(RepProp(at->PropsOfAE));
      } while (!EndOfPAEntr(at = RepAtom(at->NextOfAE)));
    }
    HashPtr++;
  }
}

#endif /* DEBUG_RESTORE3 */

#include <stdio.h>

static int
commit_to_saved_state(char *s, CELL *Astate, CELL *ATrail, CELL *AStack, CELL *AHeap) {
  int mode;

  if ((mode = check_header(Astate,ATrail,AStack,AHeap)) == FAIL_RESTORE)
    return(FAIL_RESTORE);
  Yap_PrologMode = BootMode;
  if (Yap_HeapBase) {
    if (!yap_flags[HALT_AFTER_CONSULT_FLAG] && !yap_flags[QUIET_MODE_FLAG]) {
      Yap_TrueFileName(s,Yap_FileNameBuf2, YAP_FILENAME_MAX);
      fprintf(stderr, "%% Restoring file %s\n", Yap_FileNameBuf2);
    }
    Yap_CloseStreams(TRUE);
  }
#ifdef DEBUG_RESTORE4
  /*
   * This should be another file, like the log file 
   */
  errout = Yap_stderr;
#endif
  return mode;
}

static void
cat_file_name(char *s, char *prefix, char *name, unsigned int max_length)
{
  strncpy(s, prefix, max_length);
#if _MSC_VER || defined(__MINGW32__)
  strncat(s,"\\", max_length);
#else
  strncat(s,"/", max_length);
#endif
  strncat(s, name, max_length-1);
}

static int try_open(char *inpf, CELL *Astate, CELL *ATrail, CELL *AStack, CELL *AHeap, char *buf) {
  int mode;

  
  if ((splfild = open_file(inpf, O_RDONLY)) < 0) {
    return FAIL_RESTORE;
  }
  if (buf[0] == '\0')
    strncpy(buf, inpf, YAP_FILENAME_MAX);
  if ((mode = commit_to_saved_state(inpf,Astate,ATrail,AStack,AHeap)) != FAIL_RESTORE) {
    Yap_ErrorMessage = NULL;
    return mode;
  }
  return mode;
}

static int 
OpenRestore(char *inpf, char *YapLibDir, CELL *Astate, CELL *ATrail, CELL *AStack, CELL *AHeap)
{
  int mode = FAIL_RESTORE;
  char save_buffer[YAP_FILENAME_MAX+1];

  //  Yap_ErrorMessage = NULL;
  if (inpf == NULL) {
#if _MSC_VER || defined(__MINGW32__)
    if (!(inpf = Yap_RegistryGetString("startup")))
#endif
      inpf = StartUpFile;
  }
  /* careful it starts from the root */
  if (inpf[0] != '/') {
#if __simplescalar__
    /* does not implement getcwd */
    strncpy(Yap_FileNameBuf,yap_pwd,YAP_FILENAME_MAX);
#elif HAVE_GETCWD
    if (getcwd (Yap_FileNameBuf, YAP_FILENAME_MAX) == NULL)
      Yap_FileNameBuf[0] = '\0';
#else
    if (getwd (Yap_FileNameBuf) == NULL)
      Yap_FileNameBuf[0] = '\0';
#endif
    strncat(Yap_FileNameBuf, "/", YAP_FILENAME_MAX-1);
    strncat(Yap_FileNameBuf, inpf, YAP_FILENAME_MAX-1);
  } else {
    strncat(Yap_FileNameBuf, inpf, YAP_FILENAME_MAX-1);
  }
  if (inpf != NULL && (splfild = open_file(inpf, O_RDONLY)) > 0) {
    if ((mode = try_open(inpf,Astate,ATrail,AStack,AHeap,save_buffer)) != FAIL_RESTORE) {
      return mode;
    }
  }
  if (!Yap_dir_separator(inpf[0]) && !Yap_volume_header(inpf)) {
    /*
      we have a relative path for the file, try to do somewhat better 
      using YAPLIBDIR or friends.
    */
    if (YapLibDir != NULL) {
      cat_file_name(Yap_FileNameBuf, Yap_LibDir, inpf, YAP_FILENAME_MAX);
      if ((mode = try_open(Yap_FileNameBuf,Astate,ATrail,AStack,AHeap,save_buffer)) != FAIL_RESTORE) {
	return mode;
      }
    } else {
      if ((mode = try_open(Yap_FileNameBuf,Astate,ATrail,AStack,AHeap,save_buffer)) != FAIL_RESTORE) {
	return mode;
      }
    }
#if HAVE_GETENV
    {
      char *yap_env = getenv("YAPLIBDIR");
      if (yap_env != NULL) {
	cat_file_name(Yap_FileNameBuf, yap_env, inpf, YAP_FILENAME_MAX);
	if ((mode = try_open(Yap_FileNameBuf,Astate,ATrail,AStack,AHeap,save_buffer)) != FAIL_RESTORE) {
	  return mode;
	}
      }
    }
#endif
    if (YAP_LIBDIR != NULL) {
      cat_file_name(Yap_FileNameBuf, YAP_LIBDIR, inpf, YAP_FILENAME_MAX);
      if ((splfild = open_file(Yap_FileNameBuf, O_RDONLY)) > 0) {
	if ((mode = try_open(Yap_FileNameBuf,Astate,ATrail,AStack,AHeap,save_buffer)) != FAIL_RESTORE) {
	  return mode;
	}
      }
    }
  }
#if _MSC_VER || defined(__MINGW32__)
  {
    DWORD fatts;
    int buflen;
    char *pt;

    /* try to get it from current executable */
    if ((fatts = GetFileAttributes(Yap_FileNameBuf)) == 0xFFFFFFFFL ||
	!(fatts & FILE_ATTRIBUTE_DIRECTORY)) {
      /* couldn't find it where it was supposed to be,
	 let's try using the executable */
      if (!GetModuleFileNameEx( GetCurrentProcess(), NULL, Yap_FileNameBuf, YAP_FILENAME_MAX)) {
	/* do nothing */
	goto end;
      }
      buflen = strlen(Yap_FileNameBuf);
      pt = Yap_FileNameBuf+strlen(Yap_FileNameBuf);
      while (*--pt != '\\') {
	/* skip executable */
	if (pt == Yap_FileNameBuf) {
	  /* do nothing */
	  goto end;
	}
      }
      while (*--pt != '\\') {
	/* skip parent directory "bin\\" */
	if (pt == Yap_FileNameBuf) {
	  goto end;
	}
      }
      /* now, this is a possible location for the ROOT_DIR, let's look for a share directory here */
      pt[1] = '\0';
      strncat(Yap_FileNameBuf,"lib/Yap/startup.yss",YAP_FILENAME_MAX);
    }
    if ((mode = try_open(Yap_FileNameBuf,Astate,ATrail,AStack,AHeap,save_buffer)) != FAIL_RESTORE) {
      return mode;
    }
  }
 end:
#endif
  /* try to open from current directory */
  /* could not open file */
  if (Yap_ErrorMessage == NULL) {
    if (save_buffer[0]) {
      strncpy(Yap_FileNameBuf, save_buffer, YAP_FILENAME_MAX-1);
      do_system_error(PERMISSION_ERROR_OPEN_SOURCE_SINK,"incorrect saved state");
    } else {
      strncpy(Yap_FileNameBuf, inpf, YAP_FILENAME_MAX-1);
      do_system_error(PERMISSION_ERROR_OPEN_SOURCE_SINK,"could not open saved state");
    }
  }
  return FAIL_RESTORE;
}

static void 
CloseRestore(void)
{
#ifdef DEBUG_RESTORE3
  ShowAtoms();
#endif
  close_file();
  Yap_PrologMode = UserMode;
}

#if !defined(_WIN32)
static int 
check_opcodes(OPCODE old_ops[])
{
#if USE_THREADED_CODE
  int have_shifted = FALSE;
  op_numbers op = _Ystop;
  for (op = _Ystop; op < _std_top; op++) {
    if (Yap_opcode(op) != old_ops[op]) {
      have_shifted = TRUE;
      break;
    }
  }
  return have_shifted;
#else
  /* be conservative */
  return TRUE;
#endif
}
#endif

static void 
RestoreHeap(OPCODE old_ops[])
{
  int heap_moved = (OldHeapBase != Yap_HeapBase ||
		    XDiff), opcodes_moved;
  Term mod = CurrentModule;

  CurrentModule = PROLOG_MODULE;
#if defined(_WIN32)
  /* It seems that under WIN32 opcodes may not have moved but the
     remaining code may have bmoved */ 
  opcodes_moved = TRUE;
#else
  opcodes_moved = check_opcodes(old_ops);
#endif
  /* opcodes_moved has side-effects and should be tried first */
  if (heap_moved) {
    opcodes_moved = TRUE;
    RestoreFreeSpace();
  }
  if (heap_moved || opcodes_moved) {
    restore_heap();
  }
  /* This must be done after restore_heap */
  Yap_InitAbsmi();
  if (opcodes_moved) {
    Yap_InitCPreds();
    Yap_InitBackCPreds();
  }
  if (!(Yap_ReInitConstExps() &&
	Yap_ReInitUnaryExps() &&
	Yap_ReInitBinaryExps())) {
    Yap_Error(SYSTEM_ERROR, TermNil, "arithmetic operator not in saved state");
  }
#ifdef DEBUG_RESTORE1
  fprintf(errout, "phase 1 done\n");
#endif
  CurrentModule = mod;
}

/*
 * This function is called to know about the parameters of the last saved
 * state 
 */
int 
Yap_SavedInfo(char *FileName, char *YapLibDir, CELL *ATrail, CELL *AStack, CELL *AHeap)
{
  CELL MyTrail, MyStack, MyHeap, MyState;
  int             mode;

  mode = OpenRestore(FileName, YapLibDir, &MyState, &MyTrail, &MyStack, &MyHeap);
  if (mode == FAIL_RESTORE) {
    return -1;
  }
  close_file();
  if (! *AHeap)
    *AHeap = MyHeap / 1024;
  if (mode != DO_ONLY_CODE && *AStack)
    *AStack = MyStack / 1024;
  if (mode != DO_ONLY_CODE && *ATrail)
    *ATrail = MyTrail / 1024;
  return (MyState);
}

static void
UnmarkTrEntries(void)
{
  CELL           entry, *Entries;

  /* initialise a choice point */
  B = (choiceptr)LCL0;
  B--;
  B->cp_ap = NOCODE;
  Entries = (CELL *)Yap_TrailBase;
  while ((entry = *Entries++) != (CELL)NULL) {
    if (!IsVarTerm(entry)) {
      if(IsPairTerm(entry)) {
	CELL *ent = CellPtoHeapAdjust(RepPair(entry));
	register CELL flags;

	flags = *ent;
	ResetFlag(InUseMask, flags);
	*ent = flags;
	if (FlagOn((DirtyMask|ErasedMask), flags)) {
	  if (FlagOn(DBClMask, flags)) {
	    Yap_ErDBE(DBStructFlagsToDBStruct(ent));
	  } else {
	    if (flags & LogUpdMask) {
	      if (flags & IndexMask) {
		if (FlagOn(ErasedMask, flags))
		  Yap_ErLogUpdIndex(ClauseFlagsToLogUpdIndex(ent));
		else
		  Yap_CleanUpIndex(ClauseFlagsToLogUpdIndex(ent));
	      } else {
		Yap_ErLogUpdCl(ClauseFlagsToLogUpdClause(ent));
	      }
	    } else {
	      Yap_ErCl(ClauseFlagsToDynamicClause(ent));
	    }
	  }
	}
#ifdef MULTI_ASSIGNMENT_VARIABLES
      } else /* if (IsApplTerm(d1)) */ {
	Entries += 2;
#endif
      }
    }
  }
  B = NULL;
}


int in_limbo = FALSE;

/* cleanup any records we had in the saved state. They are now inaccessible */
static void
FreeRecords(void) {
  struct record_list *ptr;

  ptr = Yap_Records;
  Yap_Records = NULL;
  while (ptr) {
    struct record_list *optr = ptr;
    Yap_ReleaseTermFromDB(ptr->dbrecord);
    ptr = ptr->next_rec;
    Yap_FreeCodeSpace((void *)optr);
  }
}

/*
 * This function is called when wanting only to restore the heap and
 * associated registers 
 */
static int 
Restore(char *s, char *lib_dir)
{
  int restore_mode;

  OPCODE old_ops[_std_top+1];
  CELL MyTrail, MyStack, MyHeap, MyState;

  if ((restore_mode = OpenRestore(s, lib_dir, &MyState, &MyTrail, &MyStack, &MyHeap)) == FAIL_RESTORE)
    return(FALSE);
  Yap_ShutdownLoadForeign();
  in_limbo = TRUE;
  if (get_coded(restore_mode, old_ops) < 0)
     return FAIL_RESTORE;  
  restore_regs(restore_mode);
  in_limbo = FALSE;
  /*#endif*/
  RestoreHeap(old_ops);
  switch(restore_mode) {
  case DO_EVERYTHING:
    if (OldHeapBase != Yap_HeapBase ||
	OldLCL0 != LCL0 ||
	OldGlobalBase != (CELL *)Yap_GlobalBase ||
	OldTrailBase != Yap_TrailBase) {
      Yap_AdjustStacksAndTrail();
      if (which_save == 2) {
	Yap_AdjustRegs(2);
      } else {
	Yap_AdjustRegs(1);
      }
      break;
#ifdef DEBUG_RESTORE2
      fprintf(errout, "phase 2 done\n");
#endif
    }
    break;
  case DO_ONLY_CODE:
    UnmarkTrEntries();
    Yap_InitYaamRegs();
    break;
  }

  Yap_ReOpenLoadForeign();
  Yap_InitPlIO();
  /* reset time */
  Yap_ReInitWallTime();
  Yap_InitSysPath();
#if USE_DL_MALLOC || USE_SYSTEM_MALLOC
  if (!AuxSp) {
    Yap_InitPreAllocCodeSpace();
  }
#endif
  FreeRecords();
  CloseRestore();
  if (which_save == 2) {
    Yap_unify(ARG2, MkIntTerm(0));
  }
  return restore_mode;
}

int 
Yap_Restore(char *s, char *lib_dir)
{
  return Restore(s, lib_dir);
}

static Int 
p_restore(void)
{
  int mode;
  char s[YAP_FILENAME_MAX+1];

  Term t1 = Deref(ARG1);
#if defined(YAPOR) && !defined(THREADS)
  if (number_workers != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,"cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#elif defined(THREADS)
  if (NOfThreads != 1) {
    Yap_Error(SYSTEM_ERROR,TermNil,"cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#endif
  if (!Yap_GetName(s, YAP_FILENAME_MAX, t1)) {
    Yap_Error(TYPE_ERROR_LIST,t1,"restore/1");
    return(FALSE);
  }
  if ((mode = Restore(s, NULL)) == DO_ONLY_CODE) {
#if PUSH_REGS
    restore_absmi_regs(&Yap_standard_regs);
#endif
    /* back to the top level we go */
    siglongjmp(Yap_RestartEnv,3);
  }
  return(mode != FAIL_RESTORE);
}

void 
Yap_InitSavePreds(void)
{
  Yap_InitCPred("$save", 2, p_save2, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$save_program", 1, p_save_program, SyncPredFlag|HiddenPredFlag);
  Yap_InitCPred("$restore", 1, p_restore, SyncPredFlag|HiddenPredFlag);
}
