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
#endif
#include "absmi.h"
#include "alloc.h"
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

extern char     StartUpFile[];

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

STATIC_PROTO(void  myread, (int, char *, Int));
STATIC_PROTO(void  mywrite, (int, char *, Int));
STATIC_PROTO(int   open_file, (char *, int));
STATIC_PROTO(void  close_file, (void));
STATIC_PROTO(void  putout, (CELL));
STATIC_PROTO(void  putcellptr, (CELL *));
STATIC_PROTO(CELL  get_cell, (void));
STATIC_PROTO(CELL  *get_cellptr, ( /* CELL * */ void));
STATIC_PROTO(void  put_info, (int, int));
STATIC_PROTO(void  save_regs, (int));
STATIC_PROTO(void  save_code_info, (void));
STATIC_PROTO(void  save_heap, (void));
STATIC_PROTO(void  save_stacks, (int));
STATIC_PROTO(void  save_crc, (void));
STATIC_PROTO(Int   do_save, (int));
STATIC_PROTO(Int   p_save, (void));
STATIC_PROTO(Int   p_save_program, (void));
STATIC_PROTO(int   check_header, (void));
STATIC_PROTO(void  get_heap_info, (void));
STATIC_PROTO(void  get_regs, (int));
STATIC_PROTO(void  get_insts, (OPCODE []));
STATIC_PROTO(void  get_hash, (void));
STATIC_PROTO(void  CopyCode, (void));
STATIC_PROTO(void  CopyStacks, (void));
STATIC_PROTO(int   get_coded, (int, OPCODE []));
STATIC_PROTO(void  restore_codes, (void));
STATIC_PROTO(void  ConvDBList, (Term, char *,CELL));
STATIC_PROTO(Term  AdjustDBTerm, (Term));
STATIC_PROTO(void  RestoreDB, (DBEntry *));
STATIC_PROTO(void  RestoreClause, (Clause *,int));
STATIC_PROTO(void  CleanClauses, (CODEADDR, CODEADDR));
STATIC_PROTO(void  rehash, (CELL *, int, int));
STATIC_PROTO(void  CleanCode, (PredEntry *));
STATIC_PROTO(void  RestoreEntries, (PropEntry *));
STATIC_PROTO(void  RestoreInvisibleAtoms, (void));
STATIC_PROTO(void  RestoreFreeSpace, (void));
STATIC_PROTO(void  restore_heap, (void));
#ifdef DEBUG_RESTORE3
STATIC_PROTO(void  ShowAtoms, (void));
STATIC_PROTO(void  ShowEntries, (PropEntry *));
#endif
STATIC_PROTO(int   OpenRestore, (char *));
STATIC_PROTO(void  CloseRestore, (void));
STATIC_PROTO(int  check_opcodes, (OPCODE []));
STATIC_PROTO(void  RestoreHeap, (OPCODE [], int));
STATIC_PROTO(Int  p_restore, (void));
STATIC_PROTO(void  restore_heap_regs, (void));
STATIC_PROTO(void  restore_regs, (int));
STATIC_PROTO(void  ConvDBStruct, (Term, char *, CELL));
#ifdef MACYAP
STATIC_PROTO(void NewFileInfo, (long, long));
extern int      DefVol;
#endif

#if HAVE_IO_H
#include <io.h>
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

#if SHORT_INTS

#ifdef M_WILLIAMS
#include <fcntl.h>
#endif

static void 
myread(int fd, char *buff, Int len)
{
  while (len > 16000) {
    int nchars = read(fd, buff, 16000);
    if (nchars <= 0)
      Error(FATAL_ERROR,TermNil,"bad saved state, system corrupted");
    len -= 16000;
    buff += 16000;
  }
  read(fd, buff, (unsigned) len);
}

static void 
mywrite(int fd, char *buff, Int len)
{
  while (len > 16000) {
    write(fd, buff, 16000);
    len -= 16000;
    buff += 16000;
  }
  write(fd, buff, (unsigned) len);
}

#else				/* SHORT_INTS */

inline static
void myread(int fd, char *buffer, Int len) {
  int nread;

  while (len > 0) {
    nread = read(fd, buffer,  (int)len);
    if (nread < 1) {
      Error(FATAL_ERROR,TermNil,"bad saved state, system corrupted");
    }
    buffer += nread;
    len -= nread;
  }
}

inline static
void mywrite(int fd, char *buff, Int len) {
  Int nwritten;

  while (len > 0) {
    nwritten = (Int)write(fd, buff, (int)len);
    if (nwritten == -1) {
      Error(SYSTEM_ERROR,TermNil,"write error while saving");
    }
    buff += nwritten;
    len -= nwritten;
  }
}

#endif				/* SHORT_INTS */

#define FullSaved		1

/* Where the code was before */



typedef CELL   *CELLPOINTER;

int             splfild = 0;

#ifdef DEBUG

#ifdef DEBUG_RESTORE4
static FILE    *errout;
#else
#define errout YP_stderr
#endif

#endif				/* DEBUG */

static Int      OldHeapUsed;

static CELL     which_save;

/* Open a file to read or to write */
static int 
open_file(char *ss, int flag)
{
  int splfild;

#ifdef M_WILLIAMS
  if (flag & O_CREAT)
    splfild = creat(ss, flag);
  else
    splfild = open(ss, flag);
  if (splfild < 0) {
#else
#ifdef O_BINARY
#if _MSC_VER
    if ((splfild = _open(ss, flag | O_BINARY), _S_IREAD | _S_IWRITE) < 0)
#else
    if ((splfild = open(ss, flag | O_BINARY), 0755) < 0)
#endif
#else  /* O_BINARY */
    if ((splfild = open(ss, flag, 0755)) < 0)
#endif  /* O_BINARY */
#endif 	/* M_WILLIAMS */
      {
	splfild = 0;	/* We do not have an open file */
	return(-1);
      }
#ifdef undf0
      YP_fprintf(errout, "Opened file %s\n", ss);
#endif
      return(splfild);
}

static void 
close_file(void)
{
  close(splfild);
  splfild = 0;
}

/* stores a cell in a file */
static void 
putout(CELL l)
{
	mywrite(splfild, (char *) &l, sizeof(CELL));
}

/* stores a pointer to a cell in a file */
static void 
putcellptr(CELL *l)
{
	mywrite(splfild, (char *) &l, sizeof(CELLPOINTER));
}

/* gets a cell from a file */
static CELL 
get_cell(void)
{
	CELL            l;
	myread(splfild, (char *) &l, Unsigned(sizeof(CELL)));
	return (l);
}

/* gets a pointer to cell from a file */
static CELL    *
get_cellptr(void)
{
	CELL           *l;
	myread(splfild, (char *) &l, Unsigned(sizeof(CELLPOINTER)));
	return (l);
}

/*
 * writes the header (at the moment YAPV*), info about what kind of saved
 * set, the work size, and the space ocuppied 
 */
static void 
put_info(int info, int mode)
{
  char     msg[256];

  sprintf(msg, "#!/bin/sh\nexec_dir=${YAPBINDIR:-%s}\nexec $exec_dir/yap $0 \"$@\"\n%cYAPV%s", BIN_DIR, 1, version_number);
  mywrite(splfild, msg, strlen(msg) + 1);
  putout(Unsigned(info));
  /* say whether we just saved the heap or everything */
  putout(mode);
  /* current state of stacks, to be used by SavedInfo */
#if defined(YAPOR) || defined(TABLING)
  /* space available in heap area */
  putout(Unsigned(GlobalBase)-Unsigned(HeapBase));
  /* space available for stacks */
  putout(Unsigned(LocalBase)-Unsigned(GlobalBase)+CellSize);
#else
  /* space available in heap area */
  putout(Unsigned(GlobalBase)-Unsigned(HeapBase));
  /* space available for stacks */
  putout(Unsigned(LocalBase)-Unsigned(GlobalBase));
#endif /* YAPOR || TABLING */
  /* space available for trail */
  putout(Unsigned(TrailTop)-Unsigned(TrailBase));
  /* Space used in heap area */
  putout(Unsigned(HeapTop)-Unsigned(HeapBase));
  /* Space used for local stack */
  putout(Unsigned(LCL0)-Unsigned(ASP));
  /* Space used for global stack */
  putout(Unsigned(H) - Unsigned(GlobalBase));
  /* Space used for trail */
  putout(Unsigned(TR) - Unsigned(TrailBase));
}

static void
save_regs(int mode)
{
  /* save all registers */
  putout((CELL)compile_arrays);
  if (mode == DO_EVERYTHING) {
    putcellptr((CELL *)CP);
    putcellptr(ENV);
    putcellptr(ASP);
    /* putout((CELL)N); */
    putcellptr(H0);
    putcellptr(LCL0);
    putcellptr(H);
    putcellptr(HB);
    putcellptr((CELL *)B);
    putcellptr((CELL *)TR);
    putcellptr(YENV);
    putcellptr(S);
    putcellptr((CELL *)P);
    putcellptr((CELL *)MyTR);
    putout(CreepFlag);
    putout(FlipFlop);
    putout(EX);
#ifdef COROUTINING
    putout(DelayedVars);
#endif
  }
  putout(CurrentModule);
  putcellptr((CELL *)HeapPlus);
  if (mode == DO_EVERYTHING) {
#ifdef COROUTINING
    putout(WokenGoals);
#endif
#ifdef  DEPTH_LIMIT
    putout(DEPTH);
#endif
  }
  /* The operand base */
  putcellptr(CellPtr(XREGS));
  putout(which_save);
  /* Now start by saving the code */
  /* the heap boundaries */
  putcellptr(CellPtr(HeapBase));
  putcellptr(CellPtr(HeapTop));
  /* and the space it ocuppies */
  putout(Unsigned(HeapUsed));
  /* Then the start of the free code */
  putcellptr(CellPtr(FreeBlocks));
  if (mode == DO_EVERYTHING) {
    /* put the old trail base, just in case it moves again */
    putout(ARG1);
    if (which_save == 2) {
      putout(ARG2);
    }
    putcellptr(CellPtr(TrailBase));
  }
}

static void
save_code_info(void)
{

  /* First the instructions */
  {
    op_numbers i;

    OPCODE my_ops[_std_top+1];
    for (i = _Ystop; i <= _std_top; ++i)
      my_ops[i] = opcode(i);
    mywrite(splfild, (char *)my_ops, sizeof(OPCODE)*(_std_top+1));
  }
  /* Then the c-functions */
  putout(NUMBER_OF_CPREDS);
  {
    UInt i;
    for (i = 0; i < NUMBER_OF_CPREDS; ++i)
      putcellptr(CellPtr(c_predicates[i]));
  }
  /* Then the cmp-functions */
  putout(NUMBER_OF_CMPFUNCS);
  {
    UInt i;
    for (i = 0; i < NUMBER_OF_CMPFUNCS; ++i) {
      putcellptr(CellPtr(cmp_funcs[i].p));
      putcellptr(CellPtr(cmp_funcs[i].f));
    }
  }
  /* and the current character codes */
  mywrite(splfild, chtype, NUMBER_OF_CHARS);
}

static void
save_heap(void)
{
  int j;
  /* Then save the whole heap */
#if defined(YAPOR) || defined(TABLING)
  /* skip the local and global data structures */
  j = Unsigned(&GLOBAL) - Unsigned(HeapBase);
  putout(j);
  mywrite(splfild, (char *) HeapBase, j);
#ifdef USE_HEAP
  j = Unsigned(HeapTop) - Unsigned(&HashChain);
  putout(j);
  mywrite(splfild, (char *) &HashChain, j);
#else
  j = Unsigned(BaseAllocArea) - Unsigned(&HashChain);
  putout(j);
  mywrite(splfild, (char *) &HashChain, j);
  j = Unsigned(HeapTop) - Unsigned(TopAllocBlockArea);
  putout(j);
  mywrite(splfild, (char *) TopAllocBlockArea, j);
#endif
#else
  j = Unsigned(HeapTop) - Unsigned(HeapBase);
  /* store 10 more cells because of the memory manager */
  mywrite(splfild, (char *) HeapBase, j);
#endif
}

static void
save_stacks(int mode)
{
  int j;
  
  switch (mode) {
  case DO_EVERYTHING:
    /* Now, go and save the state */
    /* Save the local stack */
    j = Unsigned(LCL0) - Unsigned(ASP);
    mywrite(splfild, (char *) ASP, j);
    /* Save the global stack */
    j = Unsigned(H) - Unsigned(GlobalBase);
    mywrite(splfild, (char *) GlobalBase, j);
    /* Save the trail */
    j = Unsigned(TR) - Unsigned(TrailBase);
    mywrite(splfild, (char *) TrailBase, j);
    break;
  case DO_ONLY_CODE:
    {
      tr_fr_ptr tr_ptr = TR; 
      while (tr_ptr != (tr_fr_ptr)TrailBase) {
	CELL val = TrailTerm(tr_ptr-1);
	if (IsVarTerm(val)) {
	  CELL *d1 = VarOfTerm(val);
	  if (d1 < (CELL *)HeapTop)
	    putout(val);
	} else if (IsPairTerm(val)) {
	  CELL *d1 = RepPair(val);
	  if (d1 < (CELL *)HeapTop)
	    putout(val);
	}
	tr_ptr--;
      }
    }
    putcellptr(NULL);
    break;
  }
}

static void
save_crc(void)
{
  /* Save a CRC */
  mywrite(splfild, end_msg, 256);
#ifdef MACYAP
  NewFileInfo('TEXT', 'MYap');
  if (DefVol)
    SetVol(0l, DefVol);
  DefVol = 0;
#endif
}

static Int
do_save(int mode) {
#ifdef MACYAP
  NewFileInfo('YAPS', 'MYap');
#endif
  Term t1 = Deref(ARG1);
  if (!GetName(FileNameBuf, YAP_FILENAME_MAX, t1)) {
    Error(TYPE_ERROR_LIST,t1,"save/1");
    return(FALSE);
  }
  CloseStreams(TRUE);
  if ((splfild = open_file(FileNameBuf, O_WRONLY | O_CREAT)) < 0) {
    Error(SYSTEM_ERROR,MkAtomTerm(LookupAtom(FileNameBuf)),
	  "restore/1, open(%s)", strerror(errno));
    return(FALSE);
  }
  put_info(FullSaved, mode);
  save_regs(mode);
  save_code_info();
  save_heap();
  save_stacks(mode);
  save_crc();
  close_file();
  return (TRUE);
}

/* Saves a complete prolog environment */
static Int 
p_save(void)
{
#if defined(YAPOR) || defined(THREADS)
  if (NOfThreads != 1) {
    Error(SYSTEM_ERROR,TermNil,"cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#endif
  which_save = 1;
  return(do_save(DO_EVERYTHING));
}

/* Saves a complete prolog environment */
static Int 
p_save2(void)
{
#if defined(YAPOR) || defined(THREADS)
  if (NOfThreads != 1) {
    Error(SYSTEM_ERROR,TermNil,"cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#endif
  which_save = 2;
  return(do_save(DO_EVERYTHING) && unify(ARG2,MkIntTerm(1)));
}

/* Just save the program, not the stacks */
static Int 
p_save_program(void)
{
  which_save = 0;
  return(do_save(DO_ONLY_CODE));
}

/* Now, to restore the saved code */

/* First check out if we are dealing with a valid file */
static int 
check_header(void)
{
  char            pp[80];
  char msg[256];
  CELL hp_size, gb_size, lc_size, tr_size, mode;

  /* skip the first line */
  do {
    myread(splfild, pp, 1);
  } while (pp[0] != 1);
  /* now check the version */
  sprintf(msg, "YAPV%s", version_number);
  myread(splfild, pp, Unsigned(strlen(msg) + 1));
  if (strcmp(pp, msg) != 0) {
    Error(SYSTEM_ERROR,TermNil,"not a saved Prolog state");
    return(FAIL_RESTORE);
  }
  /* check info on header */
  /* ignore info on saved state */
  get_cell();
  /* check the restore mode */
  if ((mode = get_cell()) != DO_EVERYTHING && mode != DO_ONLY_CODE) {
    Error(SYSTEM_ERROR,TermNil,"corrupted saved state");
    return(FAIL_RESTORE);
  }
  /* ignore info on stacks size */
  get_cell();
  get_cell();
  get_cell();
  /* now, check whether we got enough enough space to load the
     saved space */
  hp_size = get_cell();
  while (hp_size > Unsigned(AuxTop) - Unsigned(HeapBase)) {
    if(!growheap(FALSE)) {
      Error(SYSTEM_ERROR,TermNil,ErrorMessage);
      return(FALSE);      
    }
  }
  if (mode == DO_EVERYTHING) {
    if ((lc_size = get_cell())+(gb_size=get_cell()) > Unsigned(LocalBase) - Unsigned(GlobalBase)) {
      Error(SYSTEM_ERROR,TermNil,"out of stack space, Yap needs %d", lc_size+gb_size);
      return(FALSE);
    }
    if ((tr_size = get_cell()) > Unsigned(TrailTop) - Unsigned(TrailBase)) {
      Error(SYSTEM_ERROR,TermNil,"out of trail space, Yap needs %d", tr_size);
      return(FAIL_RESTORE);
    }
  } else {
    /* skip cell size */
    get_cell();
    get_cell();
    get_cell();
  }
  return(mode);
}

/* Gets the state of the heap, and evaluates the related variables */
static void 
get_heap_info(void)
{
  OldHeapBase = (ADDR) get_cellptr();
  OldHeapTop = (ADDR) get_cellptr();
  OldHeapUsed = (Int) get_cell();
  FreeBlocks = (BlockHeader *) get_cellptr();
  HDiff = Unsigned(HeapBase) - Unsigned(OldHeapBase);
}

/* Gets the register array */
/* Saves the old bases for the work areas */
/* and evaluates the difference from the old areas to the new ones */
static void 
get_regs(int flag)
{
  CELL           *NewGlobalBase = (CELL *)GlobalBase;
  CELL           *NewLCL0 = LCL0;
  CELL           *OldXREGS;

  /* Get regs */
  compile_arrays = (int)get_cell();
  if (flag == DO_EVERYTHING) {
    CP = (yamop *)get_cellptr();
    ENV = get_cellptr();
    ASP = get_cellptr();
    /* N = get_cell(); */
    H0 = get_cellptr();
    LCL0 = get_cellptr();
    H = get_cellptr();
    HB = get_cellptr();
    B = (choiceptr)get_cellptr();
    TR = (tr_fr_ptr)get_cellptr();
    YENV = get_cellptr();
    S = get_cellptr();
    P = (yamop *)get_cellptr();
    MyTR = (tr_fr_ptr)get_cellptr();
    CreepFlag = get_cell();
    FlipFlop = get_cell();
#ifdef COROUTINING
    DelayedVars = get_cell();
#endif
  }
  CurrentModule = get_cell();
  HeapPlus = (ADDR)get_cellptr();
  if (flag == DO_EVERYTHING) {
#ifdef COROUTINING
    WokenGoals = get_cell();
#endif
#ifdef  DEPTH_LIMIT
    DEPTH = get_cell();
#endif
  }
  /* Get the old bases */
  OldXREGS = get_cellptr();
  which_save = get_cell();
  XDiff =  (CELL)XREGS - (CELL)OldXREGS;
  get_heap_info();
  if (flag == DO_EVERYTHING) {
    ARG1 = get_cell();
    if (which_save == 2) {
      ARG2 = get_cell();
    }
    /* get old trail base */
    OldTrailBase = (ADDR)get_cellptr();
    /* Save the old register where we can easily access them */
    OldASP = ASP;
    OldLCL0 = LCL0;
    OldGlobalBase = (CELL *)GlobalBase;
    OldH = H;
    OldTR = TR;
    GDiff = Unsigned(NewGlobalBase) - Unsigned(GlobalBase);
    LDiff = Unsigned(NewLCL0) - Unsigned(LCL0);
    TrDiff = LDiff;
    GlobalBase = (ADDR)NewGlobalBase;
    LCL0 = NewLCL0;
  }
}

/* Get the old opcodes and place them in a hash table */
static void 
get_insts(OPCODE old_ops[])
{
  myread(splfild, (char *)old_ops, sizeof(OPCODE)*(_std_top+1));
}

/* check if the old functions are the same as the new ones, or if they
   have moved around. Note that we don't need these functions afterwards */
static int 
check_funcs(void)
{
  UInt old_NUMBER_OF_CPREDS, old_NUMBER_OF_CMPFUNCS;
  int out = FALSE;

  if ((old_NUMBER_OF_CPREDS = get_cell()) != NUMBER_OF_CPREDS) {
    Error(SYSTEM_ERROR,TermNil,"bad saved state, different number of functions (%d vs %d), system corrupted, old_NUMBER_OF_CPREDS, NUMBER_OF_CPREDS");
  }
  {
    unsigned int i;
    for (i = 0; i < old_NUMBER_OF_CPREDS; ++i) {
      CELL *old_pred = get_cellptr();
      out = (out || old_pred != CellPtr(c_predicates[i]));
    }
  }
  if ((old_NUMBER_OF_CMPFUNCS = get_cell()) != NUMBER_OF_CMPFUNCS) {
    Error(SYSTEM_ERROR,TermNil,"bad saved state, different number of comparison functions (%d vs %d), system corrupted", old_NUMBER_OF_CMPFUNCS, NUMBER_OF_CMPFUNCS);
  }
  {
    unsigned int i;
    for (i = 0; i < old_NUMBER_OF_CMPFUNCS; ++i) {
      CELL *old_p = get_cellptr();
      CELL *old_f = get_cellptr();
      /*      if (AddrAdjust((ADDR)old_p) != cmp_funcs[i].p) {
	
	Error(SYSTEM_ERROR,TermNil,"bad saved state, comparison function is in wrong place (%p vs %p), system corrupted", AddrAdjust((ADDR)old_p), cmp_funcs[i].p);
	} */
      cmp_funcs[i].p = (PredEntry *)AddrAdjust((ADDR)old_p);
      out = (out ||
	     old_f != CellPtr(cmp_funcs[i].f));
    }
  }
  return(out);
}

/* Get the old atoms hash table */
static void 
get_hash(void)
{
  myread(splfild, chtype , NUMBER_OF_CHARS);
}

/* Copy all of the old code to the new Heap */
static void 
CopyCode(void)
{
#if defined(YAPOR) || defined(TABLING)
  /* skip the local and global data structures */
  CELL j = get_cell();
  if (j != Unsigned(&GLOBAL) - Unsigned(HeapBase)) {
    Error(FATAL_ERROR,TermNil,"bad saved state, system corrupted");
  }
  myread(splfild, (char *) HeapBase, j);
#ifdef USE_HEAP
  j = get_cell();
  myread(splfild, (char *) &HashChain, j);
#else
  j = get_cell();
  if (j != Unsigned(BaseAllocArea) - Unsigned(&HashChain)) {
    Error(FATAL_ERROR,TermNil,"bad saved state, system corrupted");
  }
  myread(splfild, (char *) &HashChain, j);
  j = get_cell();
  myread(splfild, (char *) TopAllocBlockArea, j);
#endif
#else
  myread(splfild, (char *) HeapBase,
	 (Unsigned(OldHeapTop) - Unsigned(OldHeapBase)));
#endif
}

/* Copy the local and global stack and also the trail to their new home */
/* In REGS we still have nonadjusted values !! */
static void 
CopyStacks(void)
{
  Int             j;
  char           *NewASP;

  j = Unsigned(OldLCL0) - Unsigned(ASP);
  NewASP = (char *) (Unsigned(ASP) + (Unsigned(LCL0) - Unsigned(OldLCL0)));
  myread(splfild, (char *) NewASP, j);
  j = Unsigned(H) - Unsigned(OldGlobalBase);
  myread(splfild, (char *) GlobalBase, j);
  j = Unsigned(TR) - Unsigned(OldTrailBase);
  myread(splfild, TrailBase, j);
}

/* Copy the local and global stack and also the trail to their new home */
/* In REGS we still have nonadjusted values !! */
static void 
CopyTrailEntries(void)
{
  CELL           entry, *Entries;

  Entries = (CELL *)TrailBase;
  do {
    *Entries++ = entry = get_cell();
  } while ((CODEADDR)entry != NULL);
}

/* get things which are saved in the file */
static int 
get_coded(int flag, OPCODE old_ops[])
{
  char my_end_msg[256];
  int funcs_moved = FALSE;

  get_regs(flag);
  get_insts(old_ops);
  funcs_moved = check_funcs();
  get_hash();
  CopyCode();
  switch (flag) {
  case DO_EVERYTHING:
    CopyStacks();
    break;
  case DO_ONLY_CODE:
    CopyTrailEntries();
    break;
  }
  /* Check CRC */
  myread(splfild, my_end_msg, 256);
  if (strcmp(end_msg,my_end_msg) != 0)
    Error(FATAL_ERROR,TermNil,"bad saved state, system corrupted");
  return(funcs_moved);
}

/* restore some heap registers */
static void 
restore_heap_regs(void)
{
  HeapPlus = AddrAdjust(HeapPlus);
  HeapTop = AddrAdjust(HeapTop);
  *((YAP_SEG_SIZE *) HeapTop) = InUseFlag;
  HeapMax = HeapUsed = OldHeapUsed;
  restore_codes();
}

/* adjust abstract machine registers */
static void 
restore_regs(int flag)
{
  restore_heap_regs();
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
    HeapPlus = AddrAdjust(HeapPlus);
    if (MyTR)
      MyTR = PtoTRAdjust(MyTR);
#ifdef COROUTINING
    DelayedVars = AbsAppl(PtoGloAdjust(RepAppl(DelayedVars)));
#ifdef MULTI_ASSIGNMENT_VARIABLES
    WokenGoals = AbsAppl(PtoGloAdjust(RepAppl(WokenGoals)));
#endif
#endif
  }
}

static void
recompute_mask(DBRef dbr)
{
  if (dbr->Flags & DBNoVars) {
    dbr->Mask = EvalMasks((Term) dbr->Entry, &(dbr->Key));
  } else if (dbr->Flags & DBComplex) {
    /* This is quite nasty, we want to recalculate the mask but
       we don't want to rebuild the whole term. We'll just build whatever we
       need to recompute the mask.
    */
    CELL *x = (CELL *)HeapTop, *tp;
    unsigned int Arity, i;
    Term out;
    char *tbase = CharP(dbr->Contents-1);

    if (IsPairTerm(dbr->Entry)) {

      out = AbsPair(x);
      Arity = 2;
      tp = (CELL *)(tbase + (CELL) RepPair(dbr->Entry));
    } else {
      Functor f;
    
      tp = (CELL *)(tbase + (CELL) RepAppl(dbr->Entry));
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
	if (offset > dbr->NOfCells*sizeof(CELL))
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
    dbr->Mask = EvalMasks(out, &(dbr->Key));
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
    if (basep + (NOfE*2) > (CELL *)TrailTop) {
      if (!growtrail((ADDR)(basep + (NOfE*2))-TrailTop)) {
	Error(SYSTEM_ERROR, TermNil,
	      "not enough space to restore hash tables for indexing");
	exit_yap(1);
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

static CODEADDR
CCodeAdjust(PredEntry *pe, CODEADDR c)
{
  /* add this code to a list of ccalls that must be adjusted */
  
  return ((CODEADDR)(c_predicates[pe->StateOfPred]));
}

static CODEADDR
NextCCodeAdjust(PredEntry *pe, CODEADDR c)
{
  /* add this code to a list of ccalls that must be adjusted */
  
  return ((CODEADDR)(c_predicates[pe->StateOfPred+1]));
}


static CODEADDR
DirectCCodeAdjust(PredEntry *pe, CODEADDR c)
{
  /* add this code to a list of ccalls that must be adjusted */
  unsigned int i;
  for (i = 0; i < NUMBER_OF_CMPFUNCS; i++) {
    if (cmp_funcs[i].p == pe) {
      return((CODEADDR)(cmp_funcs[i].f));
    }
  }
  Error(FATAL_ERROR,TermNil,"bad saved state, ccalls corrupted");
  return(NULL);
}


#include "rheap.h"

/* restore the atom entries which are invisible for the user */
static void 
RestoreForeignCodeStructure(void)
{
  ForeignObj *f_code;

  if (ForeignCodeLoaded != NULL) 
    ForeignCodeLoaded = (void *)AddrAdjust((ADDR)ForeignCodeLoaded);
  f_code = ForeignCodeLoaded;
  while (f_code != NULL) {
    StringList objs, libs;
    if (f_code->objs != NULL)
      f_code->objs = (StringList)AddrAdjust((ADDR)f_code->objs);
    objs = f_code->objs;
    while (objs != NULL) {
      if (objs->next != NULL)
	objs->next = (StringList)AddrAdjust((ADDR)objs->next);
      if (objs->s != NULL)
	objs->s = (char *)AddrAdjust((ADDR)objs->s);
      objs = objs->next;
    }
    if (f_code->libs != NULL)
      f_code->libs = (StringList)AddrAdjust((ADDR)f_code->libs);
    libs = f_code->libs;
    while (libs != NULL) {
      if (libs->next != NULL)
	libs->next = (StringList)AddrAdjust((ADDR)libs->next);
      if (libs->s != NULL)
	libs->s = (char *)AddrAdjust((ADDR)libs->s);
      libs = libs->next;
    }
    if (f_code->f != NULL)
      f_code->f = (char *)AddrAdjust((ADDR)f_code->f);
    if (f_code->next != NULL)
      f_code->next = (ForeignObj *)AddrAdjust((ADDR)f_code->f);
    f_code = f_code->next;
  }
}

/* restore the atom entries which are invisible for the user */
static void 
RestoreIOStructures(void)
{
  InitStdStreams();
}

/* restores the list of free space, with its curious structure */
static void 
RestoreFreeSpace(void)
{
  register BlockHeader *bpt, *bsz;
  if (FreeBlocks != NULL)
    FreeBlocks = BlockAdjust(FreeBlocks);
  bpt = FreeBlocks;
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
}

/* restore the atom entries which are invisible for the user */
static void 
RestoreInvisibleAtoms(void)
{
  AtomEntry      *at;
  Atom            atm = INVISIBLECHAIN.Entry;

  INVISIBLECHAIN.Entry = atm = AtomAdjust(atm);
  at = RepAtom(atm);
  if (EndOfPAEntr(at))
    return;
  do {
#ifdef DEBUG_RESTORE2		/* useful during debug */
    YP_fprintf(errout, "Restoring %s\n", at->StrOfAE);
#endif
    at->PropsOfAE = PropAdjust(at->PropsOfAE);
    RestoreEntries(RepProp(at->PropsOfAE));
    atm = at->NextOfAE;
    at->NextOfAE = atm = AtomAdjust(atm);
    at = RepAtom(atm);
  }
  while (!EndOfPAEntr(at));
}

/*
 * This is the really tough part, to restore the whole of the heap 
 */
static void 
restore_heap(void)
{
  AtomHashEntry *HashPtr = HashChain;
  register int    i;
  for (i = 0; i < MaxHash; ++i) {
    Atom atm = HashPtr->Entry;
    if (atm) {
      AtomEntry      *at;
      HashPtr->Entry = atm = AtomAdjust(atm);
      at =  RepAtom(atm);
      do {
#ifdef DEBUG_RESTORE2			/* useful during debug */
	YP_fprintf(errout, "Restoring %s\n", at->StrOfAE);
#endif
	at->PropsOfAE = PropAdjust(at->PropsOfAE);
	RestoreEntries(RepProp(at->PropsOfAE));
	atm = at->NextOfAE = AtomAdjust(at->NextOfAE);
	at = RepAtom(atm);
      } while (!EndOfPAEntr(at));
    }
    HashPtr++;
  }
  RestoreInvisibleAtoms();
  RestoreForeignCodeStructure();
  RestoreIOStructures();
}


#ifdef DEBUG_RESTORE3
static void 
ShowEntries(pp)
	PropEntry      *pp;
{
  while (!EndOfPAEntr(pp)) {
    YP_fprintf(YP_stderr,"Estou a ver a prop %x em %x\n", pp->KindOfPE, pp);
    pp = RepProp(pp->NextOfPE);
  }
}

static void 
ShowAtoms()
{
  AtomHashEntry  *HashPtr = HashChain;
  register int    i;
  for (i = 0; i < MaxHash; ++i) {
    if (HashPtr->Entry != NIL) {
      AtomEntry      *at;
      at = RepAtom(HashPtr->Entry);
      do {
	YP_fprintf(YP_stderr,"Passei ao %s em %x\n", at->StrOfAE, at);
	ShowEntries(RepProp(at->PropsOfAE));
      } while (!EndOfPAEntr(at = RepAtom(at->NextOfAE)));
    }
    HashPtr++;
  }
}

#endif /* DEBUG_RESTORE3 */

#include <stdio.h>

static int 
OpenRestore(char *s)
{
  int mode;

  /*  if (strcmp(s, StartUpFile) == 0)
      YP_fprintf(YP_stderr, "[ YAP version %s ]\n\n", version_number);*/
  CloseStreams(TRUE);
  if ((splfild = open_file(s, O_RDONLY)) < 0) {
    if (!dir_separator(s[0]) && !volume_header(s)) {

      /*
	we have a relative path for the file, try to do somewhat better 
	using YAPLIBDIR or friends.
      */
      if (Yap_LibDir != NULL) {
	strncpy(FileNameBuf, Yap_LibDir, YAP_FILENAME_MAX);
#if HAVE_GETENV
      } else {
	char *yap_env = getenv("YAPLIBDIR");
	if (yap_env != NULL) {
	  strncpy(FileNameBuf, yap_env, YAP_FILENAME_MAX);
#endif
	} else {
	  strncpy(FileNameBuf, LIB_DIR, YAP_FILENAME_MAX);
	}
#if HAVE_GETENV
      }
#endif
#if _MSC_VER || defined(__MINGW32__)
      strncat(FileNameBuf,"\\", YAP_FILENAME_MAX);
#else
      strncat(FileNameBuf,"/", YAP_FILENAME_MAX);
#endif
      strncat(FileNameBuf,s, YAP_FILENAME_MAX);
      if ((splfild = open_file(FileNameBuf, O_RDONLY)) < 0) {
	if (PrologMode != BootMode) {
	  Error(SYSTEM_ERROR,MkAtomTerm(LookupAtom(s)),
		"save/1, open(%s)", strerror(errno));
	}
	return(FAIL_RESTORE);
      }
    } else {
      return(FAIL_RESTORE);
    }
  }
  PrologMode = BootMode;
  if ((mode = check_header()) == FAIL_RESTORE)
    return(FAIL_RESTORE);
  if (!yap_flags[HALT_AFTER_CONSULT_FLAG]) {
    YP_fprintf(YP_stderr, "[ Restoring file %s ]\n", s);
  }
#ifdef DEBUG_RESTORE4
  /*
   * This should be another file, like the log file 
   */
  errout = YP_stderr;
#endif
  return(mode);
}

static void 
CloseRestore(void)
{
#ifdef DEBUG_RESTORE3
  ShowAtoms();
#endif
  close_file();
  PrologMode = UserMode;
}

static int 
check_opcodes(OPCODE old_ops[])
{
#if USE_THREADED_CODE
  int have_shifted = FALSE;
  op_numbers op = _Ystop;
  for (op = _Ystop; op < _std_top; op++) {
    if (opcode(op) != old_ops[op]) {
      have_shifted = TRUE;
      break;
    }
  }
  return(have_shifted);
#else
  return(FALSE);
#endif
}

static void 
RestoreHeap(OPCODE old_ops[], int functions_moved)
{
  int heap_moved = (OldHeapBase != HeapBase), opcodes_moved;

  opcodes_moved = check_opcodes(old_ops);
  /* opcodes_moved has side-effects and should be tried first */
  if (heap_moved || opcodes_moved || functions_moved) {
    restore_heap();
  }
  /* This must be done after restore_heap */
  if (heap_moved) {
    RestoreFreeSpace();
  }
  InitAbsmi();
  if (!(ReInitConstExps() && ReInitUnaryExps() && ReInitBinaryExps()))
    Error(SYSTEM_ERROR, TermNil, "arithmetic operator not in saved state");
#ifdef DEBUG_RESTORE1
  YP_fprintf(errout, "phase 1 done\n");
#endif
}

/*
 * This function is called to know about the parameters of the last saved
 * state 
 */
int 
SavedInfo(char *FileName, int *ATrail, int *AStack, int *AHeap, char *YapLibDir)
{
  char            pp[80];
  char		  msg[256];
  char		  NameBuf[YAP_FILENAME_MAX];
  int             result, mode;

  if ((splfild = open_file(FileName, O_RDONLY)) < 0) {
    if (!dir_separator(FileName[0]) && !volume_header(FileName)) {

      /* we have a relative path for the file, try to do somewhat better */
      if  (YapLibDir != NULL) {
	strncpy(FileNameBuf, YapLibDir, YAP_FILENAME_MAX);
#if HAVE_GETENV
      } else {
	char* my_env=getenv("YAPLIBDIR");
	if (my_env != NULL) {
	  strncpy(NameBuf, my_env, YAP_FILENAME_MAX);
#endif
	} else {
	  strncpy(NameBuf, LIB_DIR, YAP_FILENAME_MAX);
	}
#if HAVE_GETENV
      }
#endif
#if _MSC_VER || defined(__MINGW32__)
      strncat(NameBuf,"\\", YAP_FILENAME_MAX);
#else
      strncat(NameBuf,"/", YAP_FILENAME_MAX);
#endif
      strncat(NameBuf,FileName, YAP_FILENAME_MAX);
      if ((splfild = open_file(NameBuf, O_RDONLY)) < 0) {
	return(FALSE);
      }
    } else {
      return(FALSE);
    }
  }
  /* skip the first line */
  do {
    myread(splfild, pp, 1);
  } while (pp[0] != 1);
  sprintf(msg, "YAPV%s", version_number);
  myread(splfild, pp, Unsigned(strlen(msg) + 1));
  if (strcmp(pp, msg) != 0) {
    if (PrologMode != BootMode)
      Error(SYSTEM_ERROR, TermNil,
	    "file %s is not a saved Prolog state", FileName);
    return(0);
  }
  result = get_cell();
  mode = get_cell();
  if (mode != DO_ONLY_CODE && mode != DO_EVERYTHING) {
    Error(SYSTEM_ERROR, TermNil,
	  "file %s is not a saved Prolog state", FileName);
    return(0);
  }
  if (*AHeap)
    get_cell();
  else
    *AHeap = get_cell() / 1024;
  if (mode == DO_ONLY_CODE || *AStack)
    get_cell();
  else
    *AStack = get_cell() / 1024;
  if (mode == DO_ONLY_CODE || *ATrail)
    get_cell();
  else
    *ATrail = get_cell() / 1024;
  close_file();
  return (result);
}

static void
UnmarkTrEntries(void)
{
  CELL           entry, *Entries;

  /* initialise a choice point */
  B = (choiceptr)LCL0;
  B--;
  B->cp_ap = NOCODE;
  Entries = (CELL *)TrailBase;
  while ((entry = *Entries++) != (CELL)NULL) {
    if (IsVarTerm(entry)) {
      RESET_VARIABLE((CELL *)entry);
    } else if (IsPairTerm(entry)) {
      CODEADDR ent = CodeAddrAdjust((CODEADDR)RepPair(entry));
      register CELL flags;

      flags = Flags(ent);
      ResetFlag(InUseMask, flags);
      Flags(ent) = flags;
      if (FlagOn(ErasedMask, flags)) {
	if (FlagOn(DBClMask, flags)) {
	  ErDBE((DBRef) (ent - (CELL) &(((DBRef) NIL)->Flags)));
	} else {
	  ErCl(ClauseFlagsToClause(ent));
	}
      }
    }
  }
  B = NULL;
}


int in_limbo = FALSE;


/*
 * This function is called when wanting only to restore the heap and
 * associated registers 
 */
int 
Restore(char *s)
{
  int restore_mode;
  int funcs_moved;

  OPCODE old_ops[_std_top+1];

  if ((restore_mode = OpenRestore(s)) == FAIL_RESTORE)
    return(FALSE);
  ShutdownLoadForeign();
  in_limbo = TRUE;
  funcs_moved = get_coded(restore_mode, old_ops);
  restore_regs(restore_mode);
  in_limbo = FALSE;
  /*#endif*/
  RestoreHeap(old_ops, funcs_moved);
  switch(restore_mode) {
  case DO_EVERYTHING:
    if (OldHeapBase != HeapBase ||
	OldLCL0 != LCL0 ||
	OldGlobalBase != (CELL *)GlobalBase ||
	OldTrailBase != TrailBase) {
      AdjustStacksAndTrail();
      if (which_save == 2) {
	AdjustRegs(2);
      } else {
	AdjustRegs(1);
      }
      break;
#ifdef DEBUG_RESTORE2
      YP_fprintf(errout, "phase 2 done\n");
#endif
    }
    break;
  case DO_ONLY_CODE:
    UnmarkTrEntries();
    InitYaamRegs();
    break;
  }
  ReOpenLoadForeign();
  InitPlIO();
  /* reset time */
  ReInitWallTime();
  CloseRestore();
  if (which_save == 2) {
    unify(ARG2, MkIntTerm(0));
  }
  return(restore_mode);
}

static Int 
p_restore(void)
{
  int mode;

  Term t1 = Deref(ARG1);
#if defined(YAPOR) || defined(THREADS)
  if (NOfThreads != 1) {
    Error(SYSTEM_ERROR,TermNil,"cannot perform save: more than a worker/thread running");
    return(FALSE);
  }
#endif
  if (!GetName(FileNameBuf, YAP_FILENAME_MAX, t1)) {
    Error(TYPE_ERROR_LIST,t1,"restore/1");
    return(FALSE);
  }
  if ((mode = Restore(FileNameBuf)) == DO_ONLY_CODE) {
#if PUSH_REGS
    restore_absmi_regs(&standard_regs);
#endif
    /* back to the top level we go */
    siglongjmp(RestartEnv,3);
  }
  return(mode != FAIL_RESTORE);
}

void 
InitSavePreds(void)
{
  InitCPred("$save", 1, p_save, SafePredFlag|SyncPredFlag);
  InitCPred("$save", 2, p_save2, SafePredFlag|SyncPredFlag);
  InitCPred("$save_program", 1, p_save_program, SafePredFlag|SyncPredFlag);
  InitCPred("$restore", 1, p_restore, SyncPredFlag);
}
