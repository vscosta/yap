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
#include "alloc.h"
#include "clause.h"
#include "Foreign.h"
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

#ifdef DEBUG

#define	LOGFILE	"logfile"

int  Yap_output_msg = FALSE;

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

STD_PROTO(void  exit, (int));

/**************	YAP PROLOG GLOBAL VARIABLES *************************/

/************* variables related to memory allocation ***************/

ADDR Yap_HeapBase;

#ifdef THREADS

struct thread_globs Yap_thread_gl[MAX_WORKERS];

#else
ADDR Yap_HeapBase,
  Yap_LocalBase,
  Yap_GlobalBase,
  Yap_TrailBase,
  Yap_TrailTop;

/************ variables	concerned with Error Handling *************/
char           *Yap_ErrorMessage;	/* used to pass error messages */
Term              Yap_Error_Term;	/* used to pass error terms */
yap_error_number  Yap_Error_TYPE;	/* used to pass the error */
UInt             Yap_Error_Size;	/* used to pass a size associated with an error */

/******************* storing error messages ****************************/
char      Yap_ErrorSay[MAX_ERROR_MSG_SIZE];

/* if we botched in a LongIO operation */
jmp_buf Yap_IOBotch;

/* if we botched in the compiler */
jmp_buf Yap_CompilerBotch;

/************ variables	concerned with Error Handling *************/
sigjmp_buf         Yap_RestartEnv;	/* used to restart after an abort execution */

/********* IO support	*****/

/********* parsing ********************************************/

TokEntry *Yap_tokptr, *Yap_toktide;
VarEntry *Yap_VarTable, *Yap_AnonVarTable;
int Yap_eot_before_eof = FALSE;

/******************* intermediate buffers **********************/

char     Yap_FileNameBuf[YAP_FILENAME_MAX],
         Yap_FileNameBuf2[YAP_FILENAME_MAX];

#endif /* THREADS */

/********* readline support	*****/
#if HAVE_LIBREADLINE

char *_line = (char *) NULL;

#endif

#ifdef MPWSHELL
/********** informing if we are in the MPW shell ********************/

int             mpwshell = FALSE;

#endif

#ifdef EMACS

int             emacs_mode = FALSE;
char            emacs_tmp[256], emacs_tmp2[256];

#endif

/********* Prolog State ********************************************/

prolog_exec_mode      Yap_PrologMode = BootMode;

int      Yap_CritLocks = 0;

/********* streams ********************************************/

int Yap_c_input_stream, Yap_c_output_stream, Yap_c_error_stream;

YP_FILE *Yap_stdin;
YP_FILE *Yap_stdout;
YP_FILE *Yap_stderr;


/************** Access to yap initial arguments ***************************/

char          **Yap_argv;
int             Yap_argc;

/************** Extensions to Terms ***************************************/

#ifdef COROUTINING
/* array with the ops for your favourite extensions */
ext_op attas[attvars_ext+1];
#endif

/**************	declarations local to init.c ************************/
static char    *optypes[] =
{"", "xfx", "xfy", "yfx", "xf", "yf", "fx", "fy"};

/* OS page size for memory allocation */
int Yap_page_size;

#if USE_THREADED_CODE
/* easy access to instruction opcodes */
void **Yap_ABSMI_OPCODES;
#endif

#if   USE_SOCKET
int Yap_sockets_io=0;
#endif

#if ANALYST
int Yap_opcount[_std_top + 1];
#endif

#if DEBUG
#if COROUTINING
int  Yap_Portray_delays = FALSE;
#endif
#endif

#ifdef LOW_LEVEL_TRACER
int Yap_do_low_level_trace = FALSE;
#endif

#define	xfx	1
#define	xfy	2
#define	yfx	3
#define	xf	4
#define	yf	5
#define	fx	6
#define	fy	7

int
Yap_IsOpType(char *type)
{
  int i;

  for (i = 1; i <= 7; ++i)
    if (strcmp(type, optypes[i]) == 0)
      break;
  return (i <= 7);
}

static int 
OpDec(int p, char *type, Atom a)
{
  int             i;
  AtomEntry      *ae = RepAtom(a);
  OpEntry        *info;

  for (i = 1; i <= 7; ++i)
    if (strcmp(type, optypes[i]) == 0)
      break;
  if (i > 7) {
    Yap_Error(DOMAIN_ERROR_OPERATOR_SPECIFIER,MkAtomTerm(Yap_LookupAtom(type)),"op/3");
    return(FALSE);
  }
  if (p) {
    if (i == 1 || i == 2 || i == 4)
      p |= DcrlpFlag;
    if (i == 1 || i == 3 || i == 6)
      p |= DcrrpFlag;
  }
  WRITE_LOCK(ae->ARWLock);
  info = RepOpProp(Yap_GetAPropHavingLock(ae, OpProperty));
  if (EndOfPAEntr(info)) {
    info = (OpEntry *) Yap_AllocAtomSpace(sizeof(OpEntry));
    info->KindOfPE = Ord(OpProperty);
    info->NextOfPE = RepAtom(a)->PropsOfAE;
    RepAtom(a)->PropsOfAE = AbsOpProp(info);
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
      Yap_Error(PERMISSION_ERROR_CREATE_OPERATOR,MkAtomTerm(a),"op/3");
      return(FALSE);
    }
    info->Infix = p;
  } else if (i <= 5) {
    if (info->Infix != 0) /* there is an infix operator */ {
      /* ISO dictates */
      WRITE_UNLOCK(info->OpRWLock);
      Yap_Error(PERMISSION_ERROR_CREATE_OPERATOR,MkAtomTerm(a),"op/3");
      return(FALSE);
    }
    info->Posfix = p;
  } else {
    info->Prefix = p;
  }
  WRITE_UNLOCK(info->OpRWLock);
  return (TRUE);
}

int 
Yap_OpDec(int p, char *type, Atom a)
{
  return(OpDec(p,type,a));
}

static void 
SetOp(int p, int type, char *at)
{
#ifdef DEBUG
  if (Yap_Option[5])
    fprintf(stderr,"[setop %d %s %s]\n", p, optypes[type], at);
#endif
  OpDec(p, optypes[type], Yap_LookupAtom(at));
}

/* Gets the info about an operator in a prop */
Atom 
Yap_GetOp(OpEntry *pp, int *prio, int fix)
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
  return (Yap_LookupAtom(optypes[n]));
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
#ifndef UNCUTABLE
  {"uncutable", fx, 1150},
#endif /*UNCUTABLE ceh:*/
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
  {"mod", yfx, 400},
  {"rem", yfx, 400},
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

#ifdef DEBUG
#ifdef HAVE_ISATTY
#include <unistd.h>
#endif
#endif

static void 
InitDebug(void)
{
  Atom            At;
#ifdef DEBUG
  int i;

  for (i = 1; i < 20; ++i)
    Yap_Option[i] = 0;
  if (Yap_output_msg) {
    char            ch;

#if HAVE_ISATTY
    if (!isatty (0)) {
      return;
    }
#endif
    fprintf(stderr,"absmi address:%p\n", FunAdr(Yap_absmi));
    fprintf(stderr,"Set	Trace Options:\n");
    fprintf(stderr,"a getch\t\tb token\t\tc Lookup\td LookupVar\ti Index\n");
    fprintf(stderr,"e SetOp\t\tf compile\tg icode\t\th boot\t\tl log\n");
    fprintf(stderr,"m Machine\n");
    while ((ch = YP_putchar(YP_getchar())) != '\n')
      if (ch >= 'a' && ch <= 'z')
	Yap_Option[ch - 'a' + 1] = 1;
    if (Yap_Option['l' - 96]) {
      Yap_logfile = fopen(LOGFILE, "w");
      if (Yap_logfile == NULL) {
	fprintf(stderr,"can not open %s\n", LOGFILE);
	getchar();
	exit(0);
      }
      fprintf(stderr,"logging session to file 'logfile'\n");
#ifdef MAC
      Yap_SetTextFile(LOGFILE);
      lp = my_line;
      curfile = Nill;
#endif
    }
  }
#endif
  /* Set at full leash */
  At = Yap_FullLookupAtom("$leash");
  Yap_PutValue(At, MkIntTerm(15));
}

void 
Yap_InitCPred(char *Name, unsigned long int Arity, CPredicate code, int flags)
{
  Atom              atom = Yap_FullLookupAtom(Name);
  PredEntry        *pe;
  yamop            *p_code;
  StaticClause     *cl = NULL;

  if (Arity)
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  if (pe->PredFlags & CPredFlag) {
    /* already exists */
    cl = ClauseCodeToStaticClause(pe->CodeOfPred);
    if ((flags | StandardPredFlag | CPredFlag) != pe->PredFlags) {
      Yap_FreeCodeSpace((ADDR)cl);
      cl = NULL;
    }
  }
  p_code = cl->ClCode;
  while (!cl) {
    UInt sz;

    if (flags & SafePredFlag) {
      sz = (CELL)NEXTOP(NEXTOP(NEXTOP(p_code,sla),e),e);
    } else {
      sz = (CELL)NEXTOP(NEXTOP(NEXTOP(NEXTOP(NEXTOP(p_code,e),sla),e),e),e);
    }
    cl = (StaticClause *)Yap_AllocCodeSpace(sz);
    if (!cl) {
      if (!Yap_growheap(FALSE, sz, NULL)) {
	Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"while initialising %s", Name);
	return;
      }
    } else {
      cl->ClFlags = 0;
      cl->ClSize = sz-sizeof(StaticClause);
      cl->usc.ClPred = pe;
      p_code = cl->ClCode;
    }
  }

  pe->CodeOfPred = p_code;
  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  pe->cs.f_code = code;
  if (!(flags & SafePredFlag)) {
    p_code->opc = Yap_opcode(_allocate);
    p_code = NEXTOP(p_code,e);
  }
  if (flags & UserCPredFlag)
    p_code->opc = Yap_opcode(_call_usercpred);
  else
    p_code->opc = Yap_opcode(_call_cpred);
  p_code->u.sla.bmap = NULL;
  p_code->u.sla.s = -Signed(RealEnvSize);
  p_code->u.sla.sla_u.p = pe;
  p_code = NEXTOP(p_code,sla);
  if (!(flags & SafePredFlag)) {
    p_code->opc = Yap_opcode(_deallocate);
    p_code = NEXTOP(p_code,e);
  }
  p_code->opc = Yap_opcode(_procceed);
  p_code = NEXTOP(p_code,e);
  p_code->opc = Yap_opcode(_Ystop);
  pe->OpcodeOfPred = pe->CodeOfPred->opc;
  pe->ModuleOfPred = CurrentModule;
}

void 
Yap_InitCmpPred(char *Name, unsigned long int Arity, CmpPredicate cmp_code, int flags)
{
  Atom              atom = Yap_LookupAtom(Name);
  PredEntry        *pe;
  yamop            *p_code = NULL;
  StaticClause     *cl = NULL; 

  if (Arity) {
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(atom, Arity),CurrentModule));
  } else {
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  }
  if (pe->PredFlags & CPredFlag) {
    p_code = pe->CodeOfPred;
    /* already exists */
  } else {
    while (!cl) {
      UInt sz = sizeof(StaticClause)+(CELL)NEXTOP(NEXTOP(NEXTOP(((yamop *)NULL),llxx),e),e);
      cl = (StaticClause *)Yap_AllocCodeSpace(sz); 
      if (!cl) {
	if (!Yap_growheap(FALSE, sz, NULL)) {
	  Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"while initialising %s", Name);
	  return;
	}
      } else {
	cl->ClSize = sz-sizeof(StaticClause);
	cl->usc.ClPred = pe;
	cl->ClFlags = 0;
	p_code = cl->ClCode;
	break;
      }
    }
  }
  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  pe->CodeOfPred = p_code;
  pe->cs.d_code = cmp_code;
  pe->ModuleOfPred = CurrentModule;
  p_code->opc = pe->OpcodeOfPred = Yap_opcode(_call_bfunc_xx);
  p_code->u.llxx.p = pe;
  p_code->u.llxx.f = FAILCODE;
  p_code->u.llxx.x1 = Yap_emit_x(1);
  p_code->u.llxx.x2 = Yap_emit_x(2);
  p_code->u.llxx.flags = Yap_compile_cmp_flags(pe);
  p_code = NEXTOP(p_code,llxx);
  p_code->opc = Yap_opcode(_procceed);
  p_code = NEXTOP(p_code,e);
  p_code->opc = Yap_opcode(_Ystop);
}

void 
Yap_InitAsmPred(char *Name,  unsigned long int Arity, int code, CPredicate def, int flags)
{
  Atom            atom = Yap_FullLookupAtom(Name);
  PredEntry      *pe;
	
  if (Arity)
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  pe->PredFlags = flags | AsmPredFlag | StandardPredFlag | (code);
  pe->cs.f_code =  def;
  pe->ModuleOfPred = CurrentModule;
  if (def != NULL) {
    yamop      *p_code = ((StaticClause *)NULL)->ClCode;
    StaticClause     *cl = (StaticClause *)Yap_AllocCodeSpace((CELL)NEXTOP(NEXTOP(NEXTOP(((yamop *)p_code),sla),e),e)); 

    cl->ClFlags = 0;
    p_code = cl->ClCode;
    pe->CodeOfPred = p_code;
    p_code->opc = pe->OpcodeOfPred = Yap_opcode(_call_cpred);
    p_code->u.sla.bmap = NULL;
    p_code->u.sla.s = -Signed(RealEnvSize);
    p_code->u.sla.sla_u.p = pe;
    p_code = NEXTOP(p_code,sla);
    p_code->opc = Yap_opcode(_procceed);
    p_code = NEXTOP(p_code,e);
    p_code->opc = Yap_opcode(_Ystop);
  } else {
    pe->OpcodeOfPred = Yap_opcode(_undef_p);
    pe->CodeOfPred =  (yamop *)(&(pe->OpcodeOfPred)); 
  }
}


static void 
CleanBack(PredEntry *pe, CPredicate Start, CPredicate Cont)
{
  yamop   *code;
  if (pe->cs.p_code.FirstClause != pe->cs.p_code.LastClause ||
      pe->cs.p_code.TrueCodeOfPred != pe->cs.p_code.FirstClause ||
      pe->CodeOfPred != pe->cs.p_code.FirstClause) {
    Yap_Error(SYSTEM_ERROR,TermNil,
	  "initiating a C Pred with backtracking");
    return;
  }
  code = (yamop *)(pe->cs.p_code.FirstClause);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = Yap_opcode(_try_userc);
  else
    code->opc = Yap_opcode(_try_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 2);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  code->u.lds.f = Start;
  code = NEXTOP(code,lds);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = Yap_opcode(_retry_userc);
  else
    code->opc = Yap_opcode(_retry_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 1);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  code->u.lds.f = Cont;
}


void 
Yap_InitCPredBack(char *Name, unsigned long int Arity, unsigned int Extra, CPredicate Start, CPredicate Cont, int flags)
{
  PredEntry      *pe;
  Atom            atom = Yap_FullLookupAtom(Name);

  if (Arity)
    pe = RepPredProp(PredPropByFunc(Yap_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  if (pe->cs.p_code.FirstClause != NIL)
    CleanBack(pe, Start, Cont);
  else {
    StaticClause *cl;
    yamop      *code = ((StaticClause *)NULL)->ClCode;
    pe->PredFlags = CompiledPredFlag | StandardPredFlag;
#ifdef YAPOR
    pe->PredFlags |= SequentialPredFlag;
#endif /* YAPOR */
    cl = (StaticClause *)Yap_AllocCodeSpace((CELL)NEXTOP(NEXTOP(NEXTOP(code,lds),lds),e));
    if (cl == NULL) {
      Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"No Heap Space in InitCPredBack");
      return;
    }
    cl->ClFlags = 0;
    code = cl->ClCode;
    pe->cs.p_code.TrueCodeOfPred = pe->CodeOfPred =
      pe->cs.p_code.FirstClause = pe->cs.p_code.LastClause = code;
    if (flags & UserCPredFlag)
      pe->OpcodeOfPred = code->opc = Yap_opcode(_try_userc);
    else
      pe->OpcodeOfPred = code->opc = Yap_opcode(_try_c);
    code->u.lds.f = Start;
    code->u.lds.p = pe;
    code->u.lds.s = Arity;
    code->u.lds.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 2);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code,lds);
    if (flags & UserCPredFlag)
      code->opc = Yap_opcode(_retry_userc);
    else
      code->opc = Yap_opcode(_retry_c);
    code->u.lds.f = Cont;
    code->u.lds.p = pe;
    code->u.lds.s = Arity;
    code->u.lds.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 1);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code,lds);
    code->opc = Yap_opcode(_Ystop);
  }
}


static void 
InitStdPreds(void)
{
  Yap_InitCPreds();
  Yap_InitBackCPreds();
}

static void
InitFlags(void)
{
  /* note that Yap_heap_regs must be set first */

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
  yap_flags[CHARACTER_ESCAPE_FLAG] = SICSTUS_CHARACTER_ESCAPES;
  yap_flags[WRITE_QUOTED_STRING_FLAG] = FALSE;
#if (defined(YAPOR) || defined(THREADS)) && PUREe_YAPOR
  yap_flags[ALLOW_ASSERTING_STATIC_FLAG] = FALSE;
#else
  yap_flags[ALLOW_ASSERTING_STATIC_FLAG] = TRUE;
#endif
  /* current default */
  yap_flags[INDEXING_MODE_FLAG] = INDEX_MODE_MULTI;
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
    AtomCreep,
    AtomStream,
    AtomStreamPos,
    AtomVar;
  Functor
    FunctorThrow;

  Yap_heap_regs->term_prolog = MkAtomTerm(Yap_LookupAtom("prolog"));
  Yap_heap_regs->user_module = MkAtomTerm(Yap_LookupAtom("user"));
  Yap_heap_regs->idb_module = MkAtomTerm(Yap_LookupAtom("idb"));
  Yap_heap_regs->attributes_module = MkAtomTerm(Yap_LookupAtom("attributes"));
  Yap_heap_regs->charsio_module = MkAtomTerm(Yap_LookupAtom("charsio"));
  Yap_heap_regs->terms_module = MkAtomTerm(Yap_LookupAtom("terms"));
  Yap_InitModules();
#ifdef YAPOR
  Yap_heap_regs->seq_def = TRUE;
  Yap_heap_regs->getworkfirsttimecode.opc = Yap_opcode(_getwork_first_time);
  Yap_heap_regs->getworkcode.opc = Yap_opcode(_getwork);
  INIT_YAMOP_LTT(&(Yap_heap_regs->getworkcode), 0);
  Yap_heap_regs->getworkcode_seq.opc = Yap_opcode(_getwork_seq);
  INIT_YAMOP_LTT(&(Yap_heap_regs->getworkcode_seq), 0);
#endif /* YAPOR */
#ifdef TABLING
  Yap_heap_regs->tablecompletioncode.opc = Yap_opcode(_table_completion);
  Yap_heap_regs->tableanswerresolutioncode.opc = Yap_opcode(_table_answer_resolution);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(Yap_heap_regs->tablecompletioncode), 0);
  INIT_YAMOP_LTT(&(Yap_heap_regs->tableanswerresolutioncode), 0);
#endif /* YAPOR */
#endif /* TABLING */
  Yap_heap_regs->expand_op_code = Yap_opcode(_expand_index);
  Yap_heap_regs->expand_clauses_first = NULL;
  Yap_heap_regs->expand_clauses_last = NULL;
  Yap_heap_regs->failcode->opc = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_1 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_2 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_3 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_4 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_5 = Yap_opcode(_op_fail);
  Yap_heap_regs->failcode_6 = Yap_opcode(_op_fail);
  
  Yap_heap_regs->env_for_trustfail_code.op = Yap_opcode(_call);
  Yap_heap_regs->env_for_trustfail_code.s = -Signed(RealEnvSize);
  Yap_heap_regs->env_for_trustfail_code.l2 = NULL;
  Yap_heap_regs->trustfailcode->opc = Yap_opcode(_trust_fail);

  Yap_heap_regs->env_for_yes_code.op = Yap_opcode(_call);
  Yap_heap_regs->env_for_yes_code.s = -Signed(RealEnvSize);
  Yap_heap_regs->env_for_yes_code.l2 = NULL;
  Yap_heap_regs->yescode->opc = Yap_opcode(_Ystop);
  Yap_heap_regs->undef_op = Yap_opcode(_undef_p);
  Yap_heap_regs->index_op = Yap_opcode(_index_pred);
  Yap_heap_regs->fail_op = Yap_opcode(_op_fail);

  Yap_heap_regs->nocode->opc = Yap_opcode(_Nstop);

  ((yamop *)(&Yap_heap_regs->rtrycode))->opc = Yap_opcode(_retry_and_mark);
  ((yamop *)(&Yap_heap_regs->rtrycode))->u.ld.s = 0;
  ((yamop *)(&Yap_heap_regs->rtrycode))->u.ld.d = NIL;
#ifdef YAPOR
  INIT_YAMOP_LTT(&(Yap_heap_regs->rtrycode), 1);
#endif /* YAPOR */

#ifdef  THREADS
  INIT_LOCK(Yap_heap_regs->thread_handles_lock);
  {
    int i;
    for (i=0; i < MAX_WORKERS; i++) {
      Yap_heap_regs->thread_handle[i].in_use = FALSE;
      Yap_heap_regs->thread_handle[i].local_preds = NULL;
    }
  }
  Yap_heap_regs->thread_handle[0].id = 0;
  Yap_heap_regs->thread_handle[0].in_use = TRUE;
  Yap_heap_regs->thread_handle[0].default_yaam_regs = 
    &Yap_standard_regs;
  Yap_heap_regs->thread_handle[0].handle = pthread_self();
  Yap_heap_regs->thread_handle[0].handle = pthread_self();
  pthread_mutex_init(&ThreadHandle[0].tlock, NULL);
  Yap_heap_regs->n_of_threads = 1;
  Yap_heap_regs->n_of_threads_created = 1;
  Yap_heap_regs->threads_total_time = 0;
#endif
#if defined(YAPOR) || defined(THREADS)
  INIT_LOCK(Yap_heap_regs->bgl);
  INIT_LOCK(Yap_heap_regs->free_blocks_lock);
  INIT_LOCK(Yap_heap_regs->heap_used_lock);
  INIT_LOCK(Yap_heap_regs->heap_top_lock);
  INIT_LOCK(Yap_heap_regs->dead_clauses_lock);
  Yap_heap_regs->heap_top_owner = -1;
  {
    int i;
    for (i=0; i < MAX_WORKERS; i++) {
      Yap_heap_regs->wl[i].scratchpad.ptr = NULL;
      Yap_heap_regs->wl[i].scratchpad.sz = SCRATCH_START_SIZE;
      Yap_heap_regs->wl[i].scratchpad.msz = SCRATCH_START_SIZE;
    }
  }
#endif /* YAPOR */
  Yap_heap_regs->clausecode->arity = 0;
  Yap_heap_regs->clausecode->clause = NULL;
  Yap_heap_regs->clausecode->func = NIL;

  Yap_heap_regs->invisiblechain.Entry = NIL;
  INIT_RWLOCK(Yap_heap_regs->invisiblechain.AERWLock);
  
  Yap_heap_regs->consultlow = (consult_obj *)Yap_AllocCodeSpace(sizeof(consult_obj)*InitialConsultCapacity);
  if (Yap_heap_regs->consultlow == NULL) {
    Yap_Error(OUT_OF_HEAP_ERROR,TermNil,"No Heap Space in InitCodes");
   return;
  }
  Yap_heap_regs->consultcapacity = InitialConsultCapacity;
  {
    Atom            at;
    PredEntry      *pred;

    at = Yap_FullLookupAtom("$creep");
    pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),PROLOG_MODULE));
    Yap_heap_regs->creep_code = pred;
    at = Yap_FullLookupAtom("$undefp");
    pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),PROLOG_MODULE));
    Yap_heap_regs->undef_code = pred;
    at = Yap_FullLookupAtom("$spy");
    pred = RepPredProp(PredPropByFunc(Yap_MkFunctor(at, 1),0));
    Yap_heap_regs->spy_code = pred;
  }
  Yap_heap_regs->system_profiling = FALSE;
  Yap_heap_regs->system_call_counting = FALSE;
  Yap_heap_regs->system_pred_goal_expansion_all = FALSE;
  Yap_heap_regs->system_pred_goal_expansion_func = FALSE;
  Yap_heap_regs->system_pred_goal_expansion_on = FALSE;
  Yap_heap_regs->update_mode = UPDATE_MODE_LOGICAL;
  Yap_heap_regs->consultbase = Yap_heap_regs->consultsp =
    Yap_heap_regs->consultlow + Yap_heap_regs->consultcapacity;
  Yap_heap_regs->compiler_compile_mode = 0; /* fast will be for native code */
  Yap_heap_regs->maxdepth = 0;
  Yap_heap_regs->maxlist  = 0;

  Yap_heap_regs->atprompt = 0;
#ifdef COROUTINING
  Yap_heap_regs->num_of_atts = 1; /* initially only coroutining is supported */
#endif
  /* system boots in compile mode */
  Yap_heap_regs->static_predicates_marked = TRUE;
  /* use Quintus compatible atom_chars and number_chars, not ISO compatible */
  Yap_heap_regs->static_predicates_marked = FALSE;

  Yap_heap_regs->int_keys_size = INT_KEYS_DEFAULT_SIZE;
  Yap_heap_regs->int_keys_timestamp = 0;
  Yap_heap_regs->IntKeys = NULL;
  Yap_heap_regs->int_bb_keys_size = INT_KEYS_DEFAULT_SIZE;
  Yap_heap_regs->IntBBKeys = NULL;
  Yap_heap_regs->char_conversion_table = NULL;
  Yap_heap_regs->char_conversion_table2 = NULL;
  /*
    don't initialise this here, this is initialised by Yap_InitModules!!!!
     Yap_heap_regs->no_of_modules = 1;
  */
  Yap_heap_regs->atom_abol = Yap_FullLookupAtom("$abol");
  AtomAltNot = Yap_LookupAtom("not");
  Yap_heap_regs->atom_append = Yap_LookupAtom ("append");
  Yap_heap_regs->atom_array = Yap_FullLookupAtom("$array");
#ifdef COROUTINING
  AtomArrayAccess = Yap_FullLookupAtom("$array_arg");
#endif
  AtomArrow = Yap_LookupAtom("->");
  Yap_heap_regs->atom_assert = Yap_LookupAtom(":-");
  Yap_heap_regs->atom_alarm = Yap_FullLookupAtom("$alarm");
#if HAVE_SIGACTION
  Yap_heap_regs->atom_sig_pending = Yap_FullLookupAtom("$sig_pending");
#endif
  AtomBraces = Yap_LookupAtom("{}");
  Yap_heap_regs->atom_b = Yap_FullLookupAtom("$last_choice_pt");
  Yap_heap_regs->atom_break = Yap_FullLookupAtom("$break");
  Yap_heap_regs->atom_call = Yap_LookupAtom("call");
  Yap_heap_regs->atom_catch = Yap_FullLookupAtom("$catch");
  Yap_heap_regs->atom_comma = Yap_LookupAtom(",");
  Yap_heap_regs->atom_cpu_time = Yap_LookupAtom("cputime");
  Yap_heap_regs->atom_csult = Yap_FullLookupAtom("$csult");
  Yap_heap_regs->atom_cut = Yap_LookupAtom("!");
  Yap_heap_regs->atom_cut_by = Yap_FullLookupAtom("$cut_by");
#ifdef EUROTRA
#ifdef SFUNC
  Yap_heap_regs->atom_dollar_undef = MkAtomTerm(Yap_FullLookupAtom("$undef"));
#endif
#endif
  Yap_heap_regs->atom_dbref = Yap_FullLookupAtom ("$dbref");
  Yap_heap_regs->atom_e = Yap_LookupAtom("e");
  Yap_heap_regs->atom_e_q = Yap_LookupAtom("=");
  Yap_heap_regs->atom_eof = Yap_LookupAtom ("end_of_file");
  AtomEq = Yap_LookupAtom("=");
#ifdef EUROTRA
  Yap_heap_regs->atom_f_b = Yap_LookupAtom("fb");
#endif
  Yap_heap_regs->atom_fail = Yap_LookupAtom("fail");
  Yap_heap_regs->atom_false = Yap_LookupAtom("false");
  Yap_heap_regs->atom_fast = Yap_FullLookupAtom("$fast");
  Yap_heap_regs->atom_g_t = Yap_LookupAtom(">");
  Yap_heap_regs->atom_gc = Yap_FullLookupAtom("$gc");
  Yap_heap_regs->atom_gc_margin = Yap_FullLookupAtom("$gc_margin");
  Yap_heap_regs->atom_gc_trace = Yap_FullLookupAtom("$gc_trace");
  Yap_heap_regs->atom_gc_verbose = Yap_FullLookupAtom("$gc_verbose");
  Yap_heap_regs->atom_gc_very_verbose = Yap_FullLookupAtom("$gc_very_verbose");
  AtomGVar = Yap_LookupAtom("var");
  Yap_heap_regs->atom_global = Yap_LookupAtom("global_sp");
  Yap_heap_regs->atom_heap_used = Yap_LookupAtom("heapused");
  Yap_heap_regs->atom_inf = Yap_LookupAtom("inf");
  Yap_heap_regs->atom_l_t = Yap_LookupAtom("<");
  Yap_heap_regs->atom_local = Yap_LookupAtom("local_sp");
  Yap_heap_regs->atom_meta_call = Yap_FullLookupAtom("$call");
  Yap_heap_regs->atom_minus = Yap_LookupAtom("-");
  Yap_heap_regs->atom_multi_file = Yap_FullLookupAtom("$mf");
  Yap_heap_regs->atom_nan = Yap_LookupAtom("nan");
  AtomNot = Yap_LookupAtom("\\+");
  Yap_heap_regs->atom_otherwise = Yap_LookupAtom("otherwise");
  Yap_heap_regs->atom_pi = Yap_LookupAtom("pi");
  Yap_heap_regs->atom_plus = Yap_LookupAtom("+");
  Yap_heap_regs->atom_portray = Yap_FullLookupAtom("$portray");
  Yap_heap_regs->atom_profile = Yap_FullLookupAtom("$profile");
  AtomQuery = Yap_LookupAtom("?-");
  Yap_heap_regs->atom_random = Yap_LookupAtom("random");
  Yap_heap_regs->atom_read = Yap_LookupAtom("read");
  Yap_heap_regs->atom_repeat = Yap_LookupAtom("repeat");
  Yap_heap_regs->atom_restore_regs = Yap_FullLookupAtom("$restore_regs");
  AtomSemic = Yap_LookupAtom(";");
  Yap_heap_regs->atom_stack_free = Yap_LookupAtom("stackfree");
  AtomStream = Yap_FullLookupAtom("$stream");
  AtomStreamPos = Yap_FullLookupAtom("$stream_position");
  Yap_heap_regs->atom_true = Yap_LookupAtom("true");
  AtomCreep = Yap_LookupAtom("$creep");
  Yap_heap_regs->atom_user = Yap_LookupAtom ("user");
  Yap_heap_regs->atom_usr_err = Yap_LookupAtom ("user_error");
  Yap_heap_regs->atom_usr_in = Yap_LookupAtom ("user_input");
  Yap_heap_regs->atom_usr_out = Yap_LookupAtom ("user_output");
  AtomVar = Yap_FullLookupAtom("$VAR");
  Yap_heap_regs->atom_version_number = Yap_FullLookupAtom("$version_name");
  Yap_heap_regs->atom_write = Yap_LookupAtom ("write");
#ifdef   USE_SOCKET
  Yap_heap_regs->functor_af_inet = Yap_MkFunctor(Yap_LookupAtom("AF_INET"),2);
  Yap_heap_regs->functor_af_local = Yap_MkFunctor(Yap_LookupAtom("AF_LOCAL"),1);
  Yap_heap_regs->functor_af_unix = Yap_MkFunctor(Yap_LookupAtom("AF_UNIX"),1);
#endif
  Yap_heap_regs->functor_alt_not = Yap_MkFunctor(AtomAltNot, 1);
#ifdef COROUTINING
  Yap_heap_regs->functor_array_entry = Yap_MkFunctor(AtomArrayAccess, 3);
#endif
  Yap_heap_regs->functor_arrow = Yap_MkFunctor(AtomArrow, 2);
  Yap_heap_regs->functor_assert = Yap_MkFunctor(AtomAssert, 2);
#ifdef COROUTINING
  Yap_heap_regs->functor_att_goal = Yap_MkFunctor(Yap_FullLookupAtom("$att_do"),2);
#endif
  Yap_heap_regs->functor_braces = Yap_MkFunctor(AtomBraces, 1);
  Yap_heap_regs->functor_call = Yap_MkFunctor(AtomCall, 1);
  Yap_heap_regs->functor_cut_by = Yap_MkFunctor(AtomCutBy, 1);
  Yap_heap_regs->functor_clist = Yap_MkFunctor(Yap_FullLookupAtom("$when"), 4);
  Yap_heap_regs->functor_comma = Yap_MkFunctor(AtomComma, 2);
  Yap_heap_regs->functor_csult = Yap_MkFunctor(AtomCsult, 1);
  Yap_heap_regs->functor_eq = Yap_MkFunctor(AtomEq, 2);
  Yap_heap_regs->functor_execute_in_mod = Yap_MkFunctor(Yap_FullLookupAtom("$execute_in_mod"), 2);
  Yap_heap_regs->functor_execute_within = Yap_MkFunctor(Yap_FullLookupAtom("$execute_within"), 1);
  Yap_heap_regs->functor_g_atom = Yap_MkFunctor(Yap_LookupAtom("atom"), 1);
  Yap_heap_regs->functor_g_atomic = Yap_MkFunctor(Yap_LookupAtom("atomic"), 1);
  Yap_heap_regs->functor_g_compound = Yap_MkFunctor(Yap_LookupAtom("compound"), 1);
  Yap_heap_regs->functor_g_float = Yap_MkFunctor(Yap_LookupAtom("float"), 1);
  Yap_heap_regs->functor_g_integer = Yap_MkFunctor(Yap_LookupAtom("integer"), 1);
  Yap_heap_regs->functor_g_number = Yap_MkFunctor(Yap_LookupAtom("number"), 1);
  Yap_heap_regs->functor_g_primitive = Yap_MkFunctor(Yap_LookupAtom("primitive"), 1);
  Yap_heap_regs->functor_g_var = Yap_MkFunctor(AtomGVar, 1);
  Yap_heap_regs->functor_last_execute_within = Yap_MkFunctor(Yap_FullLookupAtom("$last_execute_within"), 1);
  Yap_heap_regs->functor_list = Yap_MkFunctor(Yap_LookupAtom("."), 2);
  Yap_heap_regs->functor_mega_clause = Yap_MkFunctor (Yap_FullLookupAtom("$mega_clause"), 2);
  Yap_heap_regs->functor_module = Yap_MkFunctor(Yap_LookupAtom(":"), 2);
  Yap_heap_regs->functor_multi_file_clause = Yap_MkFunctor(Yap_FullLookupAtom("$mf_clause"), 5);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  Yap_heap_regs->functor_mutable = Yap_MkFunctor(Yap_FullLookupAtom("$mutable_variable"),
					 sizeof(timed_var)/sizeof(CELL));
#endif
  Yap_heap_regs->functor_not = Yap_MkFunctor(AtomNot, 1);
  Yap_heap_regs->functor_or = Yap_MkFunctor(AtomSemic, 2);
  Yap_heap_regs->functor_portray = Yap_MkFunctor(AtomPortray, 1);
  Yap_heap_regs->functor_query = Yap_MkFunctor(AtomQuery, 1);
  Yap_heap_regs->functor_creep = Yap_MkFunctor(AtomCreep, 1);
  Yap_heap_regs->functor_static_clause = Yap_MkFunctor (Yap_FullLookupAtom("$startic_clause"), 1);
  Yap_heap_regs->functor_stream = Yap_MkFunctor (AtomStream, 1);
  Yap_heap_regs->functor_stream_pos = Yap_MkFunctor (AtomStreamPos, 3);
  Yap_heap_regs->functor_stream_eOS = Yap_MkFunctor (Yap_LookupAtom("end_of_stream"), 1);
  Yap_heap_regs->functor_thread_run = Yap_MkFunctor (Yap_FullLookupAtom("$top_thread_goal"), 2);
  Yap_heap_regs->functor_change_module = Yap_MkFunctor (Yap_FullLookupAtom("$change_module"), 1);
  Yap_heap_regs->functor_current_module = Yap_MkFunctor (Yap_FullLookupAtom("$current_module"), 1);
  FunctorThrow = Yap_MkFunctor( Yap_FullLookupAtom("throw"), 1);
  Yap_heap_regs->functor_u_minus = Yap_MkFunctor (Yap_heap_regs->atom_minus, 1);
  Yap_heap_regs->functor_u_plus = Yap_MkFunctor (Yap_heap_regs->atom_plus, 1);
  Yap_heap_regs->functor_v_bar = Yap_MkFunctor(Yap_LookupAtom("|"), 2);
  Yap_heap_regs->functor_var = Yap_MkFunctor(AtomVar, 1);
#ifdef EUROTRA
  Yap_heap_regs->term_dollar_u = MkAtomTerm(Yap_FullLookupAtom("$u"));
#endif
  Yap_heap_regs->term_refound_var = MkAtomTerm(Yap_FullLookupAtom("$I_FOUND_THE_VARIABLE_AGAIN"));
  Yap_heap_regs->dyn_array_list = NULL;
  Yap_heap_regs->n_of_file_aliases = 0;
  Yap_heap_regs->file_aliases = NULL;
  Yap_heap_regs->foreign_code_loaded = NULL;
  Yap_heap_regs->yap_lib_dir = NULL;
  Yap_heap_regs->agc_hook = NULL;
  Yap_heap_regs->parser_error_style = EXCEPTION_ON_PARSER_ERROR;
  Yap_heap_regs->size_of_overflow  = 0;
  /* make sure no one else can use these two atoms */
  CurrentModule = 0;
  Yap_heap_regs->dead_clauses = NULL;
  Yap_ReleaseAtom(AtomOfTerm(Yap_heap_regs->term_refound_var));
  /* make sure we have undefp defined */
  /* predicates can only be defined after this point */
  Yap_heap_regs->env_for_yes_code.p =
    Yap_heap_regs->env_for_yes_code.p0 =
    RepPredProp(PredPropByAtom(Yap_heap_regs->atom_true,0));
  Yap_heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_heap_regs->atom_meta_call,4),PROLOG_MODULE));
  Yap_heap_regs->pred_dollar_catch = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_FullLookupAtom("$catch"),3),PROLOG_MODULE));
  Yap_heap_regs->pred_recorded_with_key = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_FullLookupAtom("$recorded_with_key"),3),PROLOG_MODULE));
  Yap_heap_regs->pred_log_upd_clause = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_FullLookupAtom("$do_log_upd_clause"),5),PROLOG_MODULE));
  Yap_heap_regs->pred_log_upd_clause0 = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_FullLookupAtom("$do_log_upd_clause"),4),PROLOG_MODULE));
  Yap_heap_regs->pred_static_clause = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_FullLookupAtom("$do_static_clause"),5),PROLOG_MODULE));
  Yap_heap_regs->pred_throw = RepPredProp(PredPropByFunc(FunctorThrow,PROLOG_MODULE));
  Yap_heap_regs->pred_handle_throw = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_FullLookupAtom("$handle_throw"),3),PROLOG_MODULE));
  Yap_heap_regs->pred_goal_expansion = RepPredProp(PredPropByFunc(Yap_MkFunctor(Yap_LookupAtom("goal_expansion"),3),USER_MODULE));
  Yap_heap_regs->env_for_trustfail_code.p =
    Yap_heap_regs->env_for_trustfail_code.p0 =
    RepPredProp(PredPropByAtom(Yap_heap_regs->atom_false,PROLOG_MODULE));
  {
    /* make sure we know about the module predicate */
    PredEntry *modp = RepPredProp(PredPropByFunc(Yap_heap_regs->functor_module,PROLOG_MODULE));
    modp->PredFlags |= MetaPredFlag;
  }
#ifdef YAPOR
  Yap_heap_regs->getworkcode.u.ld.p = RepPredProp(PredPropByAtom(Yap_FullLookupAtom("$getwork"), PROLOG_MODULE));
  Yap_heap_regs->getworkcode_seq.u.ld.p = RepPredProp(PredPropByAtom(Yap_FullLookupAtom("$getwork_seq"), PROLOG_MODULE));
#endif
  Yap_heap_regs->db_erased_marker =
    (DBRef)Yap_AllocCodeSpace(sizeof(DBStruct));
  Yap_heap_regs->db_erased_marker->id = FunctorDBRef;
  Yap_heap_regs->db_erased_marker->Flags = ErasedMask;
  Yap_heap_regs->db_erased_marker->Code = NULL;
  Yap_heap_regs->db_erased_marker->DBT.DBRefs = NULL;
  Yap_heap_regs->db_erased_marker->Parent = NULL;
  INIT_LOCK(Yap_heap_regs->db_erased_marker->lock);
  INIT_DBREF_COUNT(Yap_heap_regs->db_erased_marker);
  Yap_heap_regs->yap_streams = NULL;
#if DEBUG
  Yap_heap_regs->expand_clauses_sz = 0L;
#endif
}


static void 
InitVersion(void)
{
  Yap_PutValue(Yap_FullLookupAtom("$version_name"),
	   MkAtomTerm(Yap_LookupAtom(YAP_VERSION)));
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
#endif /* YAPOR */

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
#ifdef YAPOR
#ifdef ALLOC_BEFORE_CHECK
  INFORMATION_MESSAGE("YapTab: batched scheduling (TLWL-ABC)");
#endif
#if defined(TABLE_LOCK_AT_WRITE_LEVEL) && ! defined(ALLOC_BEFORE_CHECK)
  INFORMATION_MESSAGE("YapTab: batched scheduling (TLWL)");
#endif
#ifdef TABLE_LOCK_AT_NODE_LEVEL
  INFORMATION_MESSAGE("YapTab: batched scheduling (TLNL)");
#endif
#ifdef TABLE_LOCK_AT_ENTRY_LEVEL
  INFORMATION_MESSAGE("YapTab: batched scheduling (TLEL)");
#endif
#else
  INFORMATION_MESSAGE("YapTab: batched scheduling");
#endif /* YAPOR */
#else /* TABLING_LOCAL_SCHEDULING */
#ifdef YAPOR
#ifdef ALLOC_BEFORE_CHECK
  INFORMATION_MESSAGE("YapTab: local scheduling (TLWL-ABC)");
#endif
#if defined(TABLE_LOCK_AT_WRITE_LEVEL) && ! defined(ALLOC_BEFORE_CHECK)
  INFORMATION_MESSAGE("YapTab: local scheduling (TLWL)");
#endif
#ifdef TABLE_LOCK_AT_NODE_LEVEL
  INFORMATION_MESSAGE("YapTab: local scheduling (TLNL)");
#endif
#ifdef TABLE_LOCK_AT_ENTRY_LEVEL
  INFORMATION_MESSAGE("YapTab: local scheduling (TLEL)");
#endif
#else
  INFORMATION_MESSAGE("YapTab: local scheduling");
#endif /* YAPOR */
#endif /* TABLING_SCHEDULING */
#endif /* TABLING */
#ifdef YAPOR
  map_memory(Heap, Stack, Trail, aux_number_workers);
#else
  Yap_InitMemory (Trail, Heap, Stack);
#endif /* YAPOR */
  /* global initializations */ 
  init_global(aux_number_workers, aux_scheduler_loop, aux_delayed_release_load);
  init_signals();
}
#endif /* YAPOR || TABLING */


void
Yap_InitWorkspace(int Heap,
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
#ifdef THREADS
  pthread_key_create(&Yap_yaamregs_key, NULL);
  pthread_setspecific(Yap_yaamregs_key, (const void *)&Yap_standard_regs);
#else
  /* In this case we need to initialise the abstract registers */
  Yap_regp = &Yap_standard_regs;
  /* the emulator will eventually copy them to its own local
     register array, but for now they exist */
#endif
#endif /* PUSH_REGS */

#ifdef THREADS
  Yap_regp->worker_id_ = 0;
#endif
  /* Init signal handling and time */
  /* also init memory page size, required by later functions */
  Yap_InitSysbits ();

#if defined(YAPOR) || defined(TABLING)
  InitYapOr(Heap,
	    Stack,
	    Trail,
	    aux_number_workers,
	    aux_scheduler_loop,
	    aux_delayed_release_load);
#else /* Yap */
  Yap_InitMemory (Trail, Heap, Stack);
#endif /* YAPOR || TABLING */
  Yap_InitTime ();

  AtomHashTableSize = MaxHash;
  HashChain = (AtomHashEntry *)Yap_AllocAtomSpace(sizeof(AtomHashEntry) * MaxHash);
  if (HashChain == NULL) {
    Yap_Error(FATAL_ERROR,MkIntTerm(0),"allocating initial atom table");
  }
  for (i = 0; i < MaxHash; ++i) {
    INIT_RWLOCK(HashChain[i].AERWLock);
    HashChain[i].Entry = NIL;
  }
  Yap_LookupAtomWithAddress("FoundVar",&(SF_STORE->AtFoundVar));
  Yap_ReleaseAtom(AtomFoundVar);
  Yap_LookupAtomWithAddress("[]",&(SF_STORE->AtNil));
  Yap_LookupAtomWithAddress(".",&(SF_STORE->AtDot));
  /* InitAbsmi must be done before InitCodes */
#ifdef MPW
  Yap_InitAbsmi(REGS, FunctorList);
#else
  Yap_InitAbsmi();
#endif
  InitCodes();
  InitOps();
  InitDebug();
  InitVersion();
  Yap_InitSysPath();
  InitFlags();
  InitStdPreds();
  /* make sure tmp area is available */
  {
    Yap_ReleasePreAllocCodeSpace(Yap_PreAllocCodeSpace());
  }
}

void
Yap_exit (int value)
{
#if defined(YAPOR)
  unmap_memory();
#endif /* YAPOR */

#ifdef LOW_PROF
  remove("PROFPREDS");
  remove("PROFILING");
#endif

  if (! (Yap_PrologMode & BootMode) )
    Yap_ShutdownLoadForeign();
  exit(value);
}

