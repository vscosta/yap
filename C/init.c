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
* File:		init.c							 *
* Last rev:								 *
* mods:									 *
* comments:	initializing a prolog session				 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif

/*
 * The code from this file is used to initialize the environment for prolog 
 *
 */

#include "Yap.h"
#include "yapio.h"
#include "clause.h"
#ifdef LOW_LEVEL_TRACER
#include "tracer.h"
#endif
#ifdef YAPOR
#include "or.macros.h"
#endif	/* YAPOR */
#if defined(YAPOR) || defined(TABLING)
#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif
#endif	/* YAPOR || TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef SBA
#ifdef YAPOR
#ifndef FROZEN_REGS
#define FROZEN_REGS 1
#endif
#endif
#endif

#ifdef TABLING
#ifndef FROZEN_REGS
#define FROZEN_REGS 1
#endif
#endif

#ifdef DEBUG
#ifdef MACC
STATIC_PROTO(void  InTTYLine, (char *));
#endif
#endif
STATIC_PROTO(void  SetOp, (int, int, char *));
STATIC_PROTO(void  InitOps, (void));
STATIC_PROTO(void  InitDebug, (void));
STATIC_PROTO(void  CleanBack, (PredEntry *, CPredicate, CPredicate));
STATIC_PROTO(void  InitStdPreds,(void));
STATIC_PROTO(void  InitFlags, (void));
STATIC_PROTO(void  InitCodes, (void));
STATIC_PROTO(void  InitVersion, (void));

STD_PROTO(void  InitModules, (void));
STD_PROTO(void  exit, (int));

#ifdef DEBUG
char            Option[20];
#endif

/**************	YAP PROLOG GLOBAL VARIABLES *************************/

/************* variables related to memory allocation ***************/
ADDR            HeapBase, LocalBase, GlobalBase, TrailBase, TrailTop, ForeignCodeBase, ForeignCodeTop, ForeignCodeMax;

/* Functor		FunctorDouble, FunctorLongInt, FunctorDBRef; */

/************ variables	concerned with Error Handling *************/
char           *ErrorMessage;	/* used to pass error messages */
Term           Error_Term;	/* used to pass error terms */
yap_error_number  Error_TYPE;	/* used to pass the error */

/********* flags for comunication with prolog system predicates	*****/
int             Parm[10];

#ifdef MPWSHELL
/********** informing if we are in the MPW shell ********************/

int             mpwshell = FALSE;

#endif

#ifdef EMACS

int             emacs_mode = FALSE;
char            emacs_tmp[256], emacs_tmp2[256];

#endif

#if HAVE_SIGNAL

int             snoozing = FALSE;

#endif

/************ variable concerned with version number *****************/
char           version_number[] = YAP_VERSION;

/******************** compiler information ***************************/

int             optimizer_on = TRUE;

/******************* the line for the current parse **********************/

int             StartLine = 1;
int             StartCh = 0;
int             CurFileNo = 0;

int             compile_mode = 0;

/******************* intermediate buffers **********************/

char FileNameBuf[YAP_FILENAME_MAX], FileNameBuf2[YAP_FILENAME_MAX];

/********* Prolog State ********************************************/

int             PrologMode = BootMode;

#if PUSH_REGS
REGSTORE standard_regs;

REGSTORE *regp;

#else

REGSTORE REGS;

#endif

/* module data */

SMALLUNSGN      CurrentModule = 0;


/************** Access to yap initial arguments ***************************/

char          **yap_args;
int             yap_argc;

/************** Extensions to Terms ***************************************/

#ifdef COROUTINING
/* array with the ops for your favourite extensions */
ext_op attas[attvars_ext+1];
#endif

/************** Debugger support ***************************************/

#ifdef DEBUG
int output_msg = FALSE;
#endif

/************ variables	concerned with Error Handling *************/
sigjmp_buf         RestartEnv;	/* used to restart after an abort execution */

/************ table of C-Predicates *************/
CPredicate    c_predicates[MAX_C_PREDS];
cmp_entry     cmp_funcs[MAX_CMP_FUNCS];

/**************	declarations local to init.c ************************/
static char    *optypes[] =
{"", "xfx", "xfy", "yfx", "xf", "yf", "fx", "fy"};

#define	xfx	1
#define	xfy	2
#define	yfx	3
#define	xf	4
#define	yf	5
#define	fx	6
#define	fy	7

#define Nill NULL

#ifdef DEBUG

#define	LOGFILE	"logfile"

 /* static */ YP_FILE *logfile;

int 
DebugPutc(int sno, int ch)
{
  if (Option['l' - 96])
    (void) putc(ch, logfile);
  return (YP_putc(ch, YP_stderr));
}

static char     my_line[200] = {0};
static char    *lp = my_line;

static YP_File     curfile;

#ifdef MACC

static void 
InTTYLine(char *line)
{
	char           *p = line;
	char            ch;
	while ((ch = InKey()) != '\n' && ch != '\r')
		if (ch == 8) {
			if (line < p)
				BackupTTY(*--p);
		} else
			TTYChar(*p++ = ch);
	TTYChar('\n');
	*p = 0;
}

#endif

void 
DebugSetIFile(char *fname)
{
  if (curfile)
    YP_fclose(curfile);
  curfile = YP_fopen(fname, "r");
  if (curfile == Nill) {
    curfile = stdin;
    YP_fprintf(YP_stderr,"[ Warning: can not open %s for input]\n", fname);
  }
}

void 
DebugEndline()
{
	*lp = 0;

}

static       int   eolflg = 1;

int 
DebugGetc()
{
  int             ch;
  if (eolflg) {
    if (curfile != Nill) {
      if (YP_fgets(my_line, 200, curfile) == 0)
	curfile = Nill;
    }
    if (curfile == Nill)
      YP_fgets(my_line, 200, stdin);
    eolflg = 0;
    lp = my_line;
  }
  if ((ch = *lp++) == 0)
    ch = '\n', eolflg = 1;
  if (Option['l' - 96])
    putc(ch, logfile);
  return (ch);
}

#endif


void 
UserCPredicate(char *name, CPredicate def, unsigned int arity)
{
  InitCPred(name, arity, def, UserCPredFlag);
}

void 
UserBackCPredicate(char *name, CPredicate init, CPredicate cont,
		   unsigned int arity, int extra)
{
  InitCPredBack(name, arity, extra, init, cont, UserCPredFlag);
}

int IsOpType(char *type)
{
  int i;

  for (i = 1; i <= 7; ++i)
    if (strcmp(type, optypes[i]) == 0)
      break;
  return (i <= 7);
}

int 
OpDec(int p, char *type, Atom a)
{
  int             i;
  AtomEntry      *ae = RepAtom(a);
  OpEntry        *info;

  for (i = 1; i <= 7; ++i)
    if (strcmp(type, optypes[i]) == 0)
      break;
  if (i > 7) {
    Error(DOMAIN_ERROR_OPERATOR_SPECIFIER,MkAtomTerm(LookupAtom(type)),"op/3");
    return(FALSE);
  }
  if (p) {
    if (i == 1 || i == 2 || i == 4)
      p |= DcrlpFlag;
    if (i == 1 || i == 3 || i == 6)
      p |= DcrrpFlag;
  }
  WRITE_LOCK(ae->ARWLock);
  info = RepOpProp(LockedGetAProp(ae, OpProperty));
  if (EndOfPAEntr(info)) {
    info = (OpEntry *) AllocAtomSpace(sizeof(OpEntry));
    info->KindOfPE = Ord(OpProperty);
    info->NextOfPE = RepAtom(a)->PropOfAE;
    RepAtom(a)->PropOfAE = AbsOpProp(info);
    INIT_RWLOCK(info->OpRWLock);
    WRITE_LOCK(info->OpRWLock);
    WRITE_UNLOCK(ae->ARWLock);
    info->Prefix = info->Infix = info->Posfix = 0;
  } else {
    WRITE_LOCK(info->OpRWLock);
    WRITE_UNLOCK(ae->ARWLock);
  }
  if (i <= 3) {
    if (info->Posfix != 0) /* there is a posfix operator */ {
      /* ISO dictates */
      WRITE_UNLOCK(info->OpRWLock);
      Error(PERMISSION_ERROR_CREATE_OPERATOR,MkAtomTerm(a),"op/3");
      return(FALSE);
    }
    info->Infix = p;
  } else if (i <= 5) {
    if (info->Infix != 0) /* there is an infix operator */ {
      /* ISO dictates */
      WRITE_UNLOCK(info->OpRWLock);
      Error(PERMISSION_ERROR_CREATE_OPERATOR,MkAtomTerm(a),"op/3");
      return(FALSE);
    }
    info->Posfix = p;
  } else {
    info->Prefix = p;
  }
  WRITE_UNLOCK(info->OpRWLock);
  return (TRUE);
}

static void 
SetOp(int p, int type, char *at)
{
#ifdef DEBUG
  if (Option[5])
    YP_fprintf(YP_stderr,"[setop %d %s %s]\n", p, optypes[type], at);
#endif
  OpDec(p, optypes[type], LookupAtom(at));
}

/* Gets the info about an operator in a prop */
Atom 
GetOp(OpEntry *pp, int *prio, int fix)
{
  int             n;
  SMALLUNSGN      p;

  if (fix == 0) {
    p = pp->Prefix;
    if (p & DcrrpFlag)
      n = 6, *prio = (p ^ DcrrpFlag);
    else
      n = 7, *prio = p;
  } else if (fix == 1) {
    p = pp->Posfix;
    if (p & DcrlpFlag)
      n = 4, *prio = (p ^ DcrlpFlag);
    else
      n = 5, *prio = p;
  } else {
    p = pp->Infix;
    if ((p & DcrrpFlag) && (p & DcrlpFlag))
      n = 1, *prio = (p ^ (DcrrpFlag | DcrlpFlag));
    else if (p & DcrrpFlag)
      n = 3, *prio = (p ^ DcrrpFlag);
    else if (p & DcrlpFlag)
      n = 2, *prio = (p ^ DcrlpFlag);
    else
      n = 4, *prio = p;
  }
  return (LookupAtom(optypes[n]));
}

typedef struct OPSTRUCT {
	char           *opName;
	short int       opType, opPrio;
}               Opdef;

static Opdef    Ops[] = {
  {":-", xfx, 1200},
  {"-->", xfx, 1200},
  {"?-", fx, 1200},
  {":-", fx, 1200},
  {"dynamic", fx, 1150},
  {"initialization", fx, 1150},
  {"mode", fx, 1150},
  {"public", fx, 1150},
  {"multifile", fx, 1150},
  {"meta_predicate", fx, 1150},
  {"discontiguous", fx, 1150},
#ifdef YAPOR
  {"sequential", fx, 1150},
#endif /* YAPOR */
#ifdef TABLING
  {"table", fx, 1150},
#endif /* TABLING */
  {";", xfy, 1100},
  {"|", xfy, 1100},
  /*  {";", yf, 1100}, not allowed in ISO */
  {"->", xfy, 1050},
  {",", xfy, 1000},
  {".", xfy, 999},
  {"\\+", fy, 900},
  {"not", fy, 900},
  {"=", xfx, 700},
  {"\\=", xfx, 700},
  {"is", xfx, 700},
  {"=..", xfx, 700},
  {"==", xfx, 700},
  {"\\==", xfx, 700},
  {"@<", xfx, 700},
  {"@>", xfx, 700},
  {"@=<", xfx, 700},
  {"@>=", xfx, 700},
  {"=:=", xfx, 700},
  {"=\\=", xfx, 700},
  {"<", xfx, 700},
  {">", xfx, 700},
  {"=<", xfx, 700},
  {">=", xfx, 700},
  {":", xfy, 600},
  {"+", yfx, 500},
  {"-", yfx, 500},
  {"/\\", yfx, 500},
  {"\\/", yfx, 500},
  {"#", yfx, 500},
  {"+", fx, 500},
  {"-", fx, 500},
  {"\\", fx, 500},
  {"*", yfx, 400},
  {"/", yfx, 400},
  {"<<", yfx, 400},
  {">>", yfx, 400},
  {"mod", xfx, 300},
  {"//", yfx, 400},
  {"**", xfx, 200},
  {"^", xfy, 200}
};

static void 
InitOps(void)
{
  unsigned int             i;
  for (i = 0; i < sizeof(Ops) / sizeof(*Ops); ++i)
    SetOp(Ops[i].opPrio, Ops[i].opType, Ops[i].opName);
}

static void 
InitDebug(void)
{
  Atom            At;
#ifdef DEBUG
  int i;

  for (i = 1; i < 20; ++i)
    Option[i] = 0;
  if (output_msg) {
    char            ch;
    opcode(_Ystop);
    fprintf(stderr,"absmi address:%p\n", FunAdr(absmi));
    fprintf(stderr,"Set	Trace Options:\n");
    fprintf(stderr,"a getch\t\tb token\t\tc Lookup\td LookupVar\ti Index\n");
    fprintf(stderr,"e SetOp\t\tf compile\tg icode\t\th boot\t\tl log\n");
    fprintf(stderr,"m Machine\n");
    while ((ch = YP_putchar(YP_getchar())) != '\n')
      if (ch >= 'a' && ch <= 'z')
	Option[ch - 'a' + 1] = 1;
    if (Option['l' - 96]) {
      logfile = fopen(LOGFILE, "w");
      if (logfile == Nill) {
	fprintf(stderr,"can not open %s\n", LOGFILE);
	getchar();
	exit(0);
      }
      fprintf(stderr,"logging session to file 'logfile'\n");
#ifdef MAC
      SetTextFile(LOGFILE);
      lp = my_line;
      curfile = Nill;
#endif
    }
  }
#endif
  /* Set at half leash */
  At = LookupAtom("$leash");
  PutValue(At, MkIntTerm(10));
}


void 
InitCPred(char *Name, int Arity, CPredicate code, int flags)
{
  Atom            atom = LookupAtom(Name);
  PredEntry      *pe = RepPredProp(PredProp(atom, Arity));

  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  pe->TrueCodeOfPred = pe->CodeOfPred = (CODEADDR) code;
  pe->OpcodeOfPred = opcode(_Ystop);
  pe->ModuleOfPred = CurrentModule;
  if (!(flags & UserCPredFlag)) {
    c_predicates[NUMBER_OF_CPREDS] = code;
    pe->StateOfPred = NUMBER_OF_CPREDS;
    NUMBER_OF_CPREDS++;
  }
}

void 
InitCmpPred(char *Name, int Arity, CmpPredicate cmp_code, CPredicate code, int flags)
{
  Atom            atom = LookupAtom(Name);
  PredEntry      *pe = RepPredProp(PredProp(atom, Arity));

  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  pe->CodeOfPred = (CODEADDR) code;
  c_predicates[NUMBER_OF_CPREDS] = code;
  pe->StateOfPred = NUMBER_OF_CPREDS;
  NUMBER_OF_CPREDS++;
  pe->TrueCodeOfPred = (CODEADDR) cmp_code;
  cmp_funcs[NUMBER_OF_CMPFUNCS].p = pe;
  cmp_funcs[NUMBER_OF_CMPFUNCS].f = cmp_code;
  NUMBER_OF_CMPFUNCS++;
  pe->OpcodeOfPred = opcode(_Ystop);
}

void 
InitAsmPred(char *Name,  int Arity, int code, CPredicate def, int flags)
{
  Atom            atom = LookupAtom(Name);
  PredEntry      *pe = RepPredProp(PredProp(atom, Arity));
	
  pe->PredFlags = flags | StandardPredFlag | (code);
  pe->FirstClause = pe->LastClause = NIL;
  if (def != NULL) {
    pe->CodeOfPred = pe->TrueCodeOfPred = (CODEADDR)def;
    c_predicates[NUMBER_OF_CPREDS] = def;
    pe->StateOfPred = NUMBER_OF_CPREDS;
    NUMBER_OF_CPREDS++;
    pe->OpcodeOfPred = opcode(_Ystop);
  } else {
    pe->OpcodeOfPred = opcode(_undef_p);
    pe->TrueCodeOfPred = pe->CodeOfPred =
      (CODEADDR)(&(pe->OpcodeOfPred)); 
  }
}


static void 
CleanBack(PredEntry *pe, CPredicate Start, CPredicate Cont)
{
  yamop   *code;
  if (pe->FirstClause != pe->LastClause || pe->TrueCodeOfPred !=
      pe->FirstClause || pe->CodeOfPred != pe->FirstClause) {
    Error(SYSTEM_ERROR,TermNil,
	  "initiating a C Pred with backtracking");
    return;
  }
  code = (yamop *)(pe->FirstClause);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = opcode(_try_userc);
  else
    code->opc = opcode(_try_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 2);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  c_predicates[pe->StateOfPred] = Start;
  code->u.lds.d = (CODEADDR) Start;
  code = NEXTOP(code,lds);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = opcode(_retry_userc);
  else
    code->opc = opcode(_retry_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 1);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  c_predicates[pe->StateOfPred+1] = Cont;
  code->u.lds.d = (CODEADDR) Cont;
}


void 
InitCPredBack(char *Name, int Arity, int Extra, CPredicate Start, CPredicate Cont, int flags)
{
  PredEntry      *pe;
  Atom            atom = LookupAtom(Name);

  pe = RepPredProp(PredProp(atom, Arity));
  if (pe->FirstClause != NIL)
    CleanBack(pe, Start, Cont);
  else {
    Clause *cl;
    yamop      *code = ((Clause *)NIL)->ClCode;
    pe->PredFlags = CompiledPredFlag | StandardPredFlag;
#ifdef YAPOR
    pe->PredFlags |= SequentialPredFlag;
#endif /* YAPOR */
    cl = (Clause
	  *)AllocCodeSpace((CELL)NEXTOP(NEXTOP(NEXTOP(code,lds),lds),e));
    if (cl == NIL) {
      Error(SYSTEM_ERROR,TermNil,"No Heap Space in InitCPredBack");
      return;
    }
    cl->u.ClValue = 0;
    cl->ClFlags = 0;
    cl->Owner = LookupAtom("user");
    code = cl->ClCode;
    pe->TrueCodeOfPred = pe->CodeOfPred =
      pe->FirstClause = pe->LastClause = (CODEADDR)code;
    if (flags & UserCPredFlag)
      pe->OpcodeOfPred = code->opc = opcode(_try_userc);
    else
      pe->OpcodeOfPred = code->opc = opcode(_try_c);
    code->u.lds.d = (CODEADDR) Start;
    pe->StateOfPred = NUMBER_OF_CPREDS;
    c_predicates[NUMBER_OF_CPREDS] = Start;
    NUMBER_OF_CPREDS++;
    code->u.lds.p = (CODEADDR) pe;
    code->u.lds.s = Arity;
    code->u.lds.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 2);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code,lds);
    if (flags & UserCPredFlag)
      code->opc = opcode(_retry_userc);
    else
      code->opc = opcode(_retry_c);
    code->u.lds.d = (CODEADDR) Cont;
    c_predicates[NUMBER_OF_CPREDS] = Cont;
    NUMBER_OF_CPREDS++;
    code->u.lds.p = (CODEADDR) pe;
    code->u.lds.s = Arity;
    code->u.lds.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 1);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code,lds);
    code->opc = opcode(_Ystop);
  }
}


static void 
InitStdPreds(void)
{
  InitCPreds();
  InitBackCPreds();
}

static void
InitFlags(void)
{
  /* note that heap_regs must be set first */

#if USE_GMP
  yap_flags[YAP_INT_BOUNDED_FLAG] = 0;
#else
  yap_flags[YAP_INT_BOUNDED_FLAG] = 1;
#endif
  yap_flags[MAX_ARITY_FLAG] = -1;
  yap_flags[INTEGER_ROUNDING_FLAG] = 0;
  yap_flags[YAP_MAX_INTEGER_FLAG] = (Int)(~((CELL)1 << (sizeof(Int)*8-1)));
  yap_flags[YAP_MIN_INTEGER_FLAG] = (Int)(((CELL)1 << (sizeof(Int)*8-1)));
  yap_flags[CHAR_CONVERSION_FLAG] = 1;
  yap_flags[YAP_DOUBLE_QUOTES_FLAG] = 1;
  yap_flags[YAP_TO_CHARS_FLAG] = QUINTUS_TO_CHARS;
  yap_flags[LANGUAGE_MODE_FLAG] = 0;
  yap_flags[STRICT_ISO_FLAG] = FALSE;
  yap_flags[SPY_CREEP_FLAG] = 0;
  yap_flags[SOURCE_MODE_FLAG] = FALSE;
  yap_flags[CHARACTER_ESCAPE_FLAG] = ISO_CHARACTER_ESCAPES;
  yap_flags[WRITE_QUOTED_STRING_FLAG] = FALSE;
#if defined(YAPOR) || defined(THREADS)
  yap_flags[ALLOW_ASSERTING_STATIC_FLAG] = FALSE;
#else
  yap_flags[ALLOW_ASSERTING_STATIC_FLAG] = TRUE;
#endif
}

static void 
InitCodes(void)
{
  Atom   
    AtomAltNot,
#ifdef COROUTINING
    AtomArrayAccess,
#endif
    AtomArrow,
    AtomBraces,
    AtomEq,
    AtomGVar,
    AtomNot,
    AtomQuery,
    AtomSemic,
    AtomSpiedMetaCall,
    AtomStream,
    AtomStreamPos,
    AtomVar;

#ifdef YAPOR
  heap_regs->seq_def = TRUE;
  heap_regs->getworkfirsttimecode.opc = opcode(_getwork_first_time);
  heap_regs->getworkcode.opc = opcode(_getwork);
  heap_regs->getworkcode.u.ld.p = (CODEADDR)RepPredProp(PredProp(LookupAtom("$getwork"), 0));
  INIT_YAMOP_LTT(&(heap_regs->getworkcode), 0);
  heap_regs->getworkcode_seq.opc = opcode(_getwork_seq);
  INIT_YAMOP_LTT(&(heap_regs->getworkcode_seq), 0);
  heap_regs->getworkcode_seq.u.ld.p = (CODEADDR)RepPredProp(PredProp(LookupAtom("$getwork_seq"), 0));
#endif /* YAPOR */
#ifdef TABLING
  heap_regs->tablecompletioncode.opc = opcode(_table_completion);
  heap_regs->tableanswerresolutioncode.opc = opcode(_table_answer_resolution);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(heap_regs->tablecompletioncode), 0);
  INIT_YAMOP_LTT(&(heap_regs->tableanswerresolutioncode), 0);
#endif /* YAPOR */
#endif /* TABLING */
  heap_regs->failcode = opcode(_op_fail);
  
  heap_regs->trustfailcode = opcode(_trust_fail);

  heap_regs->env_for_yes_code.op = opcode(_call);
  heap_regs->env_for_yes_code.s = -Signed(RealEnvSize);
  heap_regs->env_for_yes_code.l = NULL;
  heap_regs->env_for_yes_code.l2 = NULL;
  heap_regs->yescode = opcode(_Ystop);
  heap_regs->undef_op = opcode(_undef_p);
  heap_regs->index_op = opcode(_index_pred);

#ifdef YAPOR
  heap_regs->nocode.opc = opcode(_Nstop);
  INIT_YAMOP_LTT(&(heap_regs->nocode), 1);
#else
  heap_regs->nocode = opcode(_Nstop);
#endif /* YAPOR */

  ((yamop *)(&heap_regs->rtrycode))->opc = opcode(_retry_and_mark);
  ((yamop *)(&heap_regs->rtrycode))->u.ld.s = 0;
  ((yamop *)(&heap_regs->rtrycode))->u.ld.d = NIL;
#ifdef YAPOR
  INIT_YAMOP_LTT(&(heap_regs->rtrycode), 1);
#endif /* YAPOR */

#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(heap_regs->free_blocks_lock);
  INIT_LOCK(heap_regs->heap_used_lock);
  INIT_LOCK(heap_regs->heap_top_lock);
  INIT_LOCK(heap_regs->dead_clauses_lock);
#endif /* YAPOR */
  heap_regs->clausecode.arity = 0;
  heap_regs->clausecode.clause = NIL;
  heap_regs->clausecode.func = NIL;

  heap_regs->invisiblechain.Entry = 0;
  INIT_RWLOCK(heap_regs->invisiblechain.AERWLock);
  
  heap_regs->consultlow = (consult_obj *)AllocCodeSpace(sizeof(consult_obj)*InitialConsultCapacity);
  if (heap_regs->consultlow == NIL) {
    Error(SYSTEM_ERROR,TermNil,"No Heap Space in InitCodes");
   return;
  }
  heap_regs->consultcapacity = InitialConsultCapacity;
  heap_regs->profiling = FALSE;
  heap_regs->update_mode = 0;
  heap_regs->consultbase = heap_regs->consultsp =
    heap_regs->consultlow + heap_regs->consultcapacity;
  heap_regs->maxdepth = 0;
  heap_regs->maxlist  = 0;

  heap_regs->atprompt = 0;
#ifdef COROUTINING
  heap_regs->num_of_atts = 0; /* initially no attributes have been defined */
#endif

  /* system boots in compile mode */
  heap_regs->static_predicates_marked = TRUE;
  /* use Quintus compatible atom_chars and number_chars, not ISO compatible */
  heap_regs->static_predicates_marked = FALSE;

  heap_regs->int_keys_size = INT_KEYS_DEFAULT_SIZE;
  heap_regs->int_keys_timestamp = 0;
  heap_regs->IntKeys = NULL;
  heap_regs->int_bb_keys_size = INT_KEYS_DEFAULT_SIZE;
  heap_regs->IntBBKeys = NULL;
  heap_regs->char_conversion_table = NULL;
  heap_regs->char_conversion_table2 = NULL;
  heap_regs->number_of_cpreds = 0;
  heap_regs->number_of_cmpfuncs = 0;
  heap_regs->no_of_modules = 1;
  heap_regs->primitives_module = 0;
  heap_regs->user_module = 1;
  heap_regs->atom_abol = LookupAtom("$abol");
  AtomAltNot = LookupAtom("not");
  heap_regs->atom_append = LookupAtom ("append");
  heap_regs->atom_array = LookupAtom("$array");
#ifdef COROUTINING
  AtomArrayAccess = LookupAtom("$array_arg");
#endif
  AtomArrow = LookupAtom("->");
  heap_regs->atom_assert = LookupAtom(":-");
  heap_regs->atom_alarm = LookupAtom("$alarm");
  AtomBraces = LookupAtom("{}");
  heap_regs->atom_b = LookupAtom("$last_choice_pt");
  heap_regs->atom_break = LookupAtom("$break");
  heap_regs->atom_call = LookupAtom("call");
  heap_regs->atom_catch = LookupAtom("$catch");
  heap_regs->atom_comma = LookupAtom(",");
  heap_regs->atom_cpu_time = LookupAtom("cputime");
  heap_regs->atom_csult = LookupAtom("$csult");
  heap_regs->atom_cut = LookupAtom("!");
  heap_regs->atom_cut_by = LookupAtom("$cut_by");
#ifdef EUROTRA
#ifdef SFUNC
  heap_regs->atom_dollar_undef = MkAtomTerm(LookupAtom("$undef"));
#endif
#endif
  heap_regs->atom_e = LookupAtom("e");
  heap_regs->atom_e_q = LookupAtom("=");
  heap_regs->atom_eof = LookupAtom ("end_of_file");
  AtomEq = LookupAtom("=");
#ifdef EUROTRA
  heap_regs->atom_f_b = LookupAtom("fb");
#endif
  heap_regs->atom_fail = LookupAtom("fail");
  heap_regs->atom_false = LookupAtom("false");
  heap_regs->atom_fast = LookupAtom("$fast");
  heap_regs->atom_g_t = LookupAtom(">");
  heap_regs->atom_gc = LookupAtom("$gc");
  heap_regs->atom_gc_margin = LookupAtom("$gc_margin");
  heap_regs->atom_gc_trace = LookupAtom("$gc_trace");
  heap_regs->atom_gc_verbose = LookupAtom("$gc_verbose");
  AtomGVar = LookupAtom("var");
  heap_regs->atom_global = LookupAtom("global_sp");
  heap_regs->atom_heap_used = LookupAtom("heapused");
  heap_regs->atom_index = LookupAtom("$doindex");
  heap_regs->atom_inf = LookupAtom("inf");
  heap_regs->atom_l_t = LookupAtom("<");
  heap_regs->atom_local = LookupAtom("local_sp");
  heap_regs->atom_meta_call = LookupAtom("$call");
  heap_regs->atom_minus = LookupAtom("-");
  heap_regs->atom_nan = LookupAtom("nan");
  AtomNot = LookupAtom("\\+");
  heap_regs->atom_otherwise = LookupAtom("otherwise");
  heap_regs->atom_pi = LookupAtom("pi");
  heap_regs->atom_plus = LookupAtom("+");
  heap_regs->atom_portray = LookupAtom("$portray");
  heap_regs->atom_profile = LookupAtom("$profile");
  AtomQuery = LookupAtom("?-");
  heap_regs->atom_random = LookupAtom("random");
  heap_regs->atom_read = LookupAtom("read");
  heap_regs->atom_repeat = LookupAtom("repeat");
  heap_regs->atom_restore_regs = LookupAtom("$restore_regs");
  AtomSemic = LookupAtom(";");
  heap_regs->atom_stack_free = LookupAtom("stackfree");
  AtomStream = LookupAtom ("$stream");
  AtomStreamPos = LookupAtom ("$stream_position");
  heap_regs->atom_throw = LookupAtom("$throw");
  heap_regs->atom_true = LookupAtom("true");
  AtomSpiedMetaCall = LookupAtom("$spied_meta_call");
  heap_regs->atom_user = LookupAtom ("user");
  heap_regs->atom_usr_err = LookupAtom ("user_error");
  heap_regs->atom_usr_in = LookupAtom ("user_input");
  heap_regs->atom_usr_out = LookupAtom ("user_output");
  AtomVar = LookupAtom("$VAR");
  heap_regs->atom_version_number = LookupAtom("$version_name");
  heap_regs->atom_write = LookupAtom ("write");
#ifdef   USE_SOCKET
  heap_regs->functor_af_inet = MkFunctor(LookupAtom("AF_INET"),2);
  heap_regs->functor_af_local = MkFunctor(LookupAtom("AF_LOCAL"),1);
  heap_regs->functor_af_unix = MkFunctor(LookupAtom("AF_UNIX"),1);
#endif
  heap_regs->functor_alt_not = MkFunctor(AtomAltNot, 1);
#ifdef COROUTINING
  heap_regs->functor_array_entry = MkFunctor(AtomArrayAccess, 3);
#endif
  heap_regs->functor_arrow = MkFunctor(AtomArrow, 2);
  heap_regs->functor_assert = MkFunctor(AtomAssert, 2);
#ifdef COROUTINING
  heap_regs->functor_att_goal = MkFunctor(LookupAtom("$att_do"),2);
#endif
  heap_regs->functor_braces = MkFunctor(AtomBraces, 1);
  heap_regs->functor_call = MkFunctor(AtomCall, 1);
  heap_regs->functor_cut_by = MkFunctor(AtomCutBy, 1);
  heap_regs->functor_comma = MkFunctor(AtomComma, 2);
  heap_regs->functor_csult = MkFunctor(AtomCsult, 1);
  heap_regs->functor_eq = MkFunctor(AtomEq, 2);
  heap_regs->functor_g_atom = MkFunctor(LookupAtom("atom"), 1);
  heap_regs->functor_g_atomic = MkFunctor(LookupAtom("atomic"), 1);
  heap_regs->functor_g_compound = MkFunctor(LookupAtom("compound"), 1);
  heap_regs->functor_g_float = MkFunctor(LookupAtom("float"), 1);
  heap_regs->functor_g_integer = MkFunctor(LookupAtom("integer"), 1);
  heap_regs->functor_g_number = MkFunctor(LookupAtom("number"), 1);
  heap_regs->functor_g_primitive = MkFunctor(LookupAtom("primitive"), 1);
  heap_regs->functor_g_var = MkFunctor(AtomGVar, 1);
  heap_regs->functor_list = MkFunctor(LookupAtom("."), 2);
  heap_regs->functor_module = MkFunctor(LookupAtom(":"), 2);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  heap_regs->functor_mutable = MkFunctor(LookupAtom("$mutable_variable"),
					 sizeof(timed_var)/sizeof(CELL));
#endif
  heap_regs->functor_not = MkFunctor(AtomNot, 1);
  heap_regs->functor_or = MkFunctor(AtomSemic, 2);
  heap_regs->functor_portray = MkFunctor(AtomPortray, 1);
  heap_regs->functor_query = MkFunctor(AtomQuery, 1);
  heap_regs->functor_spied_meta_call = MkFunctor(AtomSpiedMetaCall, 1);
  heap_regs->functor_stream = MkFunctor (AtomStream, 1);
  heap_regs->functor_stream_pos = MkFunctor (AtomStreamPos, 3);
  heap_regs->functor_stream_eOS = MkFunctor (LookupAtom("end_of_stream"), 1);
  heap_regs->functor_v_bar = MkFunctor(LookupAtom("|"), 2);
  heap_regs->functor_var = MkFunctor(AtomVar, 1);
#ifdef EUROTRA
  heap_regs->term_dollar_u = MkAtomTerm(LookupAtom("$u"));
#endif
  heap_regs->term_refound_var = MkAtomTerm(LookupAtom("$I_FOUND_THE_VARIABLE_AGAIN"));
  heap_regs->n_of_file_aliases = 0;
  heap_regs->file_aliases = NULL;
  heap_regs->foreign_code_loaded = NULL;
  heap_regs->yap_lib_dir = NULL;
  heap_regs->size_of_overflow  = 0;
  /* make sure no one else can use these two atoms */
  CurrentModule = 1;
  heap_regs->pred_goal_expansion = RepPredProp(PredProp(LookupAtom("goal_expansion"),3));
  CurrentModule = 0;
  heap_regs->dead_clauses = NULL;
  heap_regs->pred_meta_call = RepPredProp(PredProp(heap_regs->atom_meta_call,3));
  ReleaseAtom(AtomOfTerm(heap_regs->term_refound_var));
}

static void 
InitVersion(void)
{
  PutValue(LookupAtom("$version_name"),
	   MkAtomTerm(LookupAtom(version_number)));
}

void
InitYaamRegs(void)
{
  Atom at;
  PredEntry *undefpe;

#if PUSH_REGS
  /* Guarantee that after a longjmp we go back to the original abstract
     machine registers */
  regp = &standard_regs;
#endif /* PUSH_REGS */
  PutValue (AtomBreak, MkIntTerm (0));
  PutValue (AtomIndex, MkAtomTerm (AtomTrue));
  AuxSp = (CELL *)AuxTop;
  TR = (tr_fr_ptr)TrailBase;
#ifdef COROUTINING
  H = H0 = ((CELL *) GlobalBase)+2048;
#else
  H = H0 = (CELL *) GlobalBase;
#endif
  LCL0 = ASP = (CELL *) LocalBase;
  /* notice that an initial choice-point and environment
   *must* be created since for the garbage collector to work */
  B = NULL;
  ENV = NULL;
  P = CP = (yamop *)YESCODE;
#ifdef DEPTH_LIMIT
  DEPTH = RESET_DEPTH();
#endif
  at = FullLookupAtom("$undefp");
  {
    Prop p = GetPredProp (at, 1);
    if (p == NIL) {
      UndefCode = NULL;
    } else {
      undefpe = RepPredProp (p);
      UndefCode = (CELL *) & (undefpe->CodeOfPred);
    }
  }
  STATIC_PREDICATES_MARKED = FALSE;
#ifdef FROZEN_REGS
  H = HB = H0 = H_FZ = H_BASE;
#ifdef SBA
  BSEG =
#endif /* SBA */
  BBREG = B_FZ = B_BASE;
  TR = TR_FZ = TR_BASE;
#endif /* FROZEN_REGS */
  CreepFlag = CalculateStackGap();

}


#if defined(YAPOR) || defined(TABLING)
static void
InitYapOr(int Heap,
	  int Stack,
	  int Trail,
	  int aux_number_workers,
	  int aux_scheduler_loop,
	  int aux_delayed_release_load) {

#ifdef YAPOR
  worker_id = 0;
#endif

  /* starting message */
#ifdef YAPOR
  if (aux_number_workers > MAX_WORKERS)
    abort_optyap("excessive number of workers");
#ifdef ENV_COPY
  INFORMATION_MESSAGE("YapOr: copy model - %d worker%s", aux_number_workers, aux_number_workers == 1 ? "":"s");
#elif ACOW
  INFORMATION_MESSAGE("YapOr: acow model - %d worker%s", aux_number_workers, aux_number_workers == 1 ? "":"s");
#else /* SBA */
  INFORMATION_MESSAGE("YapOr: sba model - %d worker%s", aux_number_workers, aux_number_workers == 1 ? "":"s");
#endif /* ENV_COPY - ACOW - SBA */
#endif /* YAPOR */
#ifdef TABLING
#ifdef TABLING_BATCHED_SCHEDULING
  INFORMATION_MESSAGE("YapTab: batched scheduling");
#else /* TABLING_LOCAL_SCHEDULING */
  INFORMATION_MESSAGE("YapTab: local scheduling");
#endif /* TABLING_SCHEDULING */
#endif /* TABLING */
#ifdef YAPOR
  map_memory(Heap, Stack, Trail, aux_number_workers);
#else
  InitMemory (Trail, Heap, Stack);
#endif
  /* global initializations */ 
  init_global(aux_number_workers, aux_scheduler_loop, aux_delayed_release_load);
  init_signals();
}
#endif

void
InitStacks(int Heap,
	   int Stack,
	   int Trail,
	   int aux_number_workers,
	   int aux_scheduler_loop,
	   int aux_delayed_release_load
)
{
  int             i;

  /* initialise system stuff */

#if PUSH_REGS
    /* In this case we need to initialise the abstract registers */
  regp = &standard_regs;
  /* the emulator will eventually copy them to its own local
     register array, but for now they exist */
#endif /* PUSH_REGS */

  /* Init signal handling and time */
  /* also init memory page size, required by later functions */
  InitSysbits ();

  /* sanity checking for data areas */
  if (Trail < MinTrailSpace)
    Trail = MinTrailSpace;
  //  if (Stack < MinStackSpace)
  //  Stack = MinStackSpace;
#if defined(YAPOR) || defined(TABLING)
  {
#ifdef USE_HEAP
    int OKHeap = MinHeapSpace+(sizeof(struct global_data) + aux_number_workers*sizeof(struct local_data))/1024;
#else
    int OKHeap = MinHeapSpace+(sizeof(struct global_data) + aux_number_workers*sizeof(struct local_data)+OPT_CHUNK_SIZE)/1024;
#endif
    if (Heap < OKHeap)
      Heap = OKHeap;
  }
#else
  if (Heap < MinHeapSpace)
    Heap = MinHeapSpace;
#endif

#if defined(YAPOR) || defined(TABLING)
  InitYapOr(Heap,
	    Stack,
	    Trail,
	    aux_number_workers,
	    aux_scheduler_loop,
	    aux_delayed_release_load);
#else /* Yap */
  InitMemory (Trail, Heap, Stack);
#endif /* YAPOR || TABLING */
  for (i = 0; i < MaxHash; ++i) {
    INIT_RWLOCK(HashChain[i].AERWLock);
    HashChain[i].Entry = NIL;
  }
  LookupAtomWithAddress("FoundVar",&(SF_STORE->AtFoundVar));
  ReleaseAtom(AtomFoundVar);
  LookupAtomWithAddress("[]",&(SF_STORE->AtNil));
  LookupAtomWithAddress(".",&(SF_STORE->AtDot));
  PutValue(LookupAtom("$catch_counter"),
	   MkIntTerm(0));
  /* InitAbsmi must be done before InitCodes */
#ifdef MPW
  InitAbsmi(REGS, FunctorList);
#else
  InitAbsmi();
#endif
  InitCodes();
  InitOps();
  InitDebug();
  InitVersion();
  InitSysPath();
  InitFlags();
  InitModules();
  InitStdPreds();
}

