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
ADDR            _YAP_HeapBase,
		_YAP_LocalBase,
                _YAP_GlobalBase,
                _YAP_TrailBase,
                _YAP_TrailTop;

/* Functor		FunctorDouble, FunctorLongInt, FunctorDBRef; */

/************ variables	concerned with Error Handling *************/
char           *_YAP_ErrorMessage;	/* used to pass error messages */
Term              _YAP_Error_Term;	/* used to pass error terms */
yap_error_number  _YAP_Error_TYPE;	/* used to pass the error */

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

/******************* storing error messages ****************************/
char      _YAP_ErrorSay[MAX_ERROR_MSG_SIZE];

/******************* intermediate buffers **********************/

char     _YAP_FileNameBuf[YAP_FILENAME_MAX],
         _YAP_FileNameBuf2[YAP_FILENAME_MAX];

/********* Prolog State ********************************************/

prolog_exec_mode      _YAP_PrologMode = BootMode;

int      _YAP_CritLocks = 0;

/********* streams ********************************************/

int _YAP_c_input_stream, _YAP_c_output_stream, _YAP_c_error_stream;

YP_FILE *_YAP_stdin;
YP_FILE *_YAP_stdout;
YP_FILE *_YAP_stderr;

/********* parsing ********************************************/

TokEntry *_YAP_tokptr, *_YAP_toktide;
VarEntry *_YAP_VarTable, *_YAP_AnonVarTable;
int _YAP_eot_before_eof = FALSE;

/************** Access to yap initial arguments ***************************/

char          **_YAP_argv;
int             _YAP_argc;

/************** Extensions to Terms ***************************************/

#ifdef COROUTINING
/* array with the ops for your favourite extensions */
ext_op attas[attvars_ext+1];
#endif

/************ variables	concerned with Error Handling *************/
sigjmp_buf         _YAP_RestartEnv;	/* used to restart after an abort execution */

/**************	declarations local to init.c ************************/
static char    *optypes[] =
{"", "xfx", "xfy", "yfx", "xf", "yf", "fx", "fy"};

/* if we botched in a LongIO operation */
jmp_buf _YAP_IOBotch;

/* if we botched in the compiler */
jmp_buf _YAP_CompilerBotch;

/* OS page size for memory allocation */
int _YAP_page_size;

#if USE_THREADED_CODE
/* easy access to instruction opcodes */
void **_YAP_ABSMI_OPCODES;
#endif

#if   USE_SOCKET
int _YAP_sockets_io=0;
#endif

#if ANALYST
int _YAP_opcount[_std_top + 1];
#endif

#if DEBUG
#if COROUTINING
int  _YAP_Portray_delays = FALSE;
#endif
#endif

#ifdef LOW_LEVEL_TRACER
int _YAP_do_low_level_trace = FALSE;
#endif

#define	xfx	1
#define	xfy	2
#define	yfx	3
#define	xf	4
#define	yf	5
#define	fx	6
#define	fy	7

int
_YAP_IsOpType(char *type)
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
    _YAP_Error(DOMAIN_ERROR_OPERATOR_SPECIFIER,MkAtomTerm(_YAP_LookupAtom(type)),"op/3");
    return(FALSE);
  }
  if (p) {
    if (i == 1 || i == 2 || i == 4)
      p |= DcrlpFlag;
    if (i == 1 || i == 3 || i == 6)
      p |= DcrrpFlag;
  }
  WRITE_LOCK(ae->ARWLock);
  info = RepOpProp(_YAP_GetAPropHavingLock(ae, OpProperty));
  if (EndOfPAEntr(info)) {
    info = (OpEntry *) _YAP_AllocAtomSpace(sizeof(OpEntry));
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
      _YAP_Error(PERMISSION_ERROR_CREATE_OPERATOR,MkAtomTerm(a),"op/3");
      return(FALSE);
    }
    info->Infix = p;
  } else if (i <= 5) {
    if (info->Infix != 0) /* there is an infix operator */ {
      /* ISO dictates */
      WRITE_UNLOCK(info->OpRWLock);
      _YAP_Error(PERMISSION_ERROR_CREATE_OPERATOR,MkAtomTerm(a),"op/3");
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
_YAP_OpDec(int p, char *type, Atom a)
{
  return(OpDec(p,type,a));
}

static void 
SetOp(int p, int type, char *at)
{
#ifdef DEBUG
  if (_YAP_Option[5])
    fprintf(stderr,"[setop %d %s %s]\n", p, optypes[type], at);
#endif
  OpDec(p, optypes[type], _YAP_LookupAtom(at));
}

/* Gets the info about an operator in a prop */
Atom 
_YAP_GetOp(OpEntry *pp, int *prio, int fix)
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
  return (_YAP_LookupAtom(optypes[n]));
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
    _YAP_Option[i] = 0;
  if (output_msg) {
    char            ch;

#if HAVE_ISATTY
    if (!isatty (0)) {
      return;
    }
#endif
    fprintf(stderr,"absmi address:%p\n", FunAdr(_YAP_absmi));
    fprintf(stderr,"Set	Trace Options:\n");
    fprintf(stderr,"a getch\t\tb token\t\tc Lookup\td LookupVar\ti Index\n");
    fprintf(stderr,"e SetOp\t\tf compile\tg icode\t\th boot\t\tl log\n");
    fprintf(stderr,"m Machine\n");
    while ((ch = YP_putchar(YP_getchar())) != '\n')
      if (ch >= 'a' && ch <= 'z')
	_YAP_Option[ch - 'a' + 1] = 1;
    if (_YAP_Option['l' - 96]) {
      _YAP_logfile = fopen(LOGFILE, "w");
      if (_YAP_logfile == NULL) {
	fprintf(stderr,"can not open %s\n", LOGFILE);
	getchar();
	exit(0);
      }
      fprintf(stderr,"logging session to file 'logfile'\n");
#ifdef MAC
      _YAP_SetTextFile(LOGFILE);
      lp = my_line;
      curfile = Nill;
#endif
    }
  }
#endif
  /* Set at full leash */
  At = _YAP_LookupAtom("$leash");
  _YAP_PutValue(At, MkIntTerm(15));
}

void 
_YAP_InitCPred(char *Name, unsigned long int Arity, CPredicate code, int flags)
{
  Atom            atom = _YAP_LookupAtom(Name);
  PredEntry      *pe;
  yamop      *p_code = ((Clause *)NULL)->ClCode;
  Clause     *cl = (Clause *)_YAP_AllocCodeSpace((CELL)NEXTOP(NEXTOP(((yamop *)p_code),sla),e)); 

  cl->u.ClValue = 0;
  cl->ClFlags = 0;
  cl->Owner = _YAP_LookupAtom("user");
  p_code = cl->ClCode;

  if (Arity)
    pe = RepPredProp(PredPropByFunc(_YAP_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  p_code->u.sla.l = pe->TrueCodeOfPred = (CODEADDR) code;
  pe->CodeOfPred = pe->FirstClause = pe->LastClause = (CODEADDR) p_code;
  if (flags & UserCPredFlag)
    p_code->opc = pe->OpcodeOfPred = _YAP_opcode(_call_usercpred);
  else
    p_code->opc = pe->OpcodeOfPred = _YAP_opcode(_call_cpred);
  p_code->u.sla.l2 = NULL;
  p_code->u.sla.s = -Signed(RealEnvSize);
  p_code->u.sla.p = pe;
  p_code->u.sla.p0 = pe;
  p_code = NEXTOP(p_code,sla);
  p_code->opc = _YAP_opcode(_procceed);
  { 
    Term mod = CurrentModule;
    pe->ModuleOfPred = mod;
  }
  if (!(flags & UserCPredFlag)) {
    _YAP_c_predicates[NumberOfCPreds] = code;
    pe->StateOfPred = NumberOfCPreds;
    NumberOfCPreds++;
    if (NumberOfCPreds >= MAX_C_PREDS) {
      _YAP_Error(SYSTEM_ERROR, TermNil, "Too Many C-Predicates");
    }
  }
}

void 
_YAP_InitCmpPred(char *Name, unsigned long int Arity, CmpPredicate cmp_code, CPredicate code, int flags)
{
  Atom            atom = _YAP_LookupAtom(Name);
  PredEntry      *pe;
  yamop      *p_code = ((Clause *)NULL)->ClCode;
  Clause     *cl = (Clause *)_YAP_AllocCodeSpace((CELL)NEXTOP(NEXTOP(((yamop *)p_code),sla),e)); 

  cl->u.ClValue = 0;
  cl->ClFlags = 0;
  cl->Owner = _YAP_LookupAtom("user");
  p_code = cl->ClCode;
  if (Arity)
    pe = RepPredProp(PredPropByFunc(_YAP_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  p_code->u.sla.l = pe->TrueCodeOfPred = (CODEADDR) code;
  pe->CodeOfPred = pe->FirstClause = pe->LastClause = (CODEADDR) p_code;
  pe->ModuleOfPred = CurrentModule;
  p_code->opc = pe->OpcodeOfPred = _YAP_opcode(_call_cpred);
  p_code->u.sla.l2 = NULL;
  p_code->u.sla.s = -Signed(RealEnvSize);
  p_code->u.sla.p = p_code->u.sla.p0 = pe;
  p_code = NEXTOP(p_code,sla);
  p_code->opc = _YAP_opcode(_procceed);
  _YAP_c_predicates[NumberOfCPreds] = code;
  pe->StateOfPred = NumberOfCPreds;
  NumberOfCPreds++;
  if (NumberOfCPreds == MAX_C_PREDS) {
    _YAP_Error(SYSTEM_ERROR, TermNil, "not enough table for c-predicates");
  }
  pe->TrueCodeOfPred = (CODEADDR) cmp_code;
  _YAP_cmp_funcs[NumberOfCmpFuncs].p = pe;
  _YAP_cmp_funcs[NumberOfCmpFuncs].f = cmp_code;
  NumberOfCmpFuncs++;
  if (NumberOfCmpFuncs == MAX_CMP_FUNCS) {
    _YAP_Error(SYSTEM_ERROR, TermNil, "not enough table for comparison predicates");
  }
}

void 
_YAP_InitAsmPred(char *Name,  unsigned long int Arity, int code, CPredicate def, int flags)
{
  Atom            atom = _YAP_LookupAtom(Name);
  PredEntry      *pe;
	
  if (Arity)
    pe = RepPredProp(PredPropByFunc(_YAP_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
  pe->PredFlags = flags | AsmPredFlag | StandardPredFlag | (code);
  if (def != NULL) {
    yamop      *p_code = ((Clause *)NULL)->ClCode;
    Clause     *cl = (Clause *)_YAP_AllocCodeSpace((CELL)NEXTOP(NEXTOP(((yamop *)p_code),sla),e)); 

    cl->u.ClValue = 0;
    cl->ClFlags = 0;
    cl->Owner = _YAP_LookupAtom("user");
    p_code = cl->ClCode;
    p_code->u.sla.l = pe->TrueCodeOfPred = (CODEADDR) def;
    pe->CodeOfPred = pe->FirstClause = pe->LastClause = (CODEADDR) p_code;
    pe->ModuleOfPred = CurrentModule;
    p_code->opc = pe->OpcodeOfPred = _YAP_opcode(_call_cpred);
    p_code->u.sla.l2 = NULL;
    p_code->u.sla.s = -Signed(RealEnvSize);
    p_code->u.sla.p = p_code->u.sla.p0 = pe;
    p_code = NEXTOP(p_code,sla);
    p_code->opc = _YAP_opcode(_procceed);
    _YAP_c_predicates[NumberOfCPreds] = def;
    pe->StateOfPred = NumberOfCPreds;
    NumberOfCPreds++;
  } else {
    pe->FirstClause = pe->LastClause = NULL;
    pe->OpcodeOfPred = _YAP_opcode(_undef_p);
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
    _YAP_Error(SYSTEM_ERROR,TermNil,
	  "initiating a C Pred with backtracking");
    return;
  }
  code = (yamop *)(pe->FirstClause);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = _YAP_opcode(_try_userc);
  else
    code->opc = _YAP_opcode(_try_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 2);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  _YAP_c_predicates[pe->StateOfPred] = Start;
  code->u.lds.d = (CODEADDR) Start;
  code = NEXTOP(code,lds);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = _YAP_opcode(_retry_userc);
  else
    code->opc = _YAP_opcode(_retry_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 1);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  _YAP_c_predicates[pe->StateOfPred+1] = Cont;
  code->u.lds.d = (CODEADDR) Cont;
}


void 
_YAP_InitCPredBack(char *Name, unsigned long int Arity, unsigned int Extra, CPredicate Start, CPredicate Cont, int flags)
{
  PredEntry      *pe;
  Atom            atom = _YAP_LookupAtom(Name);

  if (Arity)
    pe = RepPredProp(PredPropByFunc(_YAP_MkFunctor(atom, Arity),CurrentModule));
  else
    pe = RepPredProp(PredPropByAtom(atom,CurrentModule));
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
	  *)_YAP_AllocCodeSpace((CELL)NEXTOP(NEXTOP(NEXTOP(code,lds),lds),e));
    if (cl == NIL) {
      _YAP_Error(SYSTEM_ERROR,TermNil,"No Heap Space in InitCPredBack");
      return;
    }
    cl->u.ClValue = 0;
    cl->ClFlags = 0;
    cl->Owner = _YAP_LookupAtom("user");
    code = cl->ClCode;
    pe->TrueCodeOfPred = pe->CodeOfPred =
      pe->FirstClause = pe->LastClause = (CODEADDR)code;
    if (flags & UserCPredFlag)
      pe->OpcodeOfPred = code->opc = _YAP_opcode(_try_userc);
    else
      pe->OpcodeOfPred = code->opc = _YAP_opcode(_try_c);
    code->u.lds.d = (CODEADDR) Start;
    pe->StateOfPred = NumberOfCPreds;
    _YAP_c_predicates[NumberOfCPreds] = Start;
    NumberOfCPreds++;
    code->u.lds.p = pe;
    code->u.lds.s = Arity;
    code->u.lds.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 2);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code,lds);
    if (flags & UserCPredFlag)
      code->opc = _YAP_opcode(_retry_userc);
    else
      code->opc = _YAP_opcode(_retry_c);
    code->u.lds.d = (CODEADDR) Cont;
    _YAP_c_predicates[NumberOfCPreds] = Cont;
    NumberOfCPreds++;
    code->u.lds.p = pe;
    code->u.lds.s = Arity;
    code->u.lds.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 1);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code,lds);
    code->opc = _YAP_opcode(_Ystop);
  }
}


static void 
InitStdPreds(void)
{
  _YAP_InitCPreds();
  _YAP_InitBackCPreds();
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
#if (defined(YAPOR) || defined(THREADS)) && PUREe_YAPOR
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
    AtomSpy,
    AtomStream,
    AtomStreamPos,
    AtomVar;
  Functor
    FunctorThrow;

#ifdef YAPOR
  heap_regs->seq_def = TRUE;
  heap_regs->getworkfirsttimecode.opc = _YAP_opcode(_getwork_first_time);
  heap_regs->getworkcode.opc = _YAP_opcode(_getwork);
  INIT_YAMOP_LTT(&(heap_regs->getworkcode), 0);
  heap_regs->getworkcode_seq.opc = _YAP_opcode(_getwork_seq);
  INIT_YAMOP_LTT(&(heap_regs->getworkcode_seq), 0);
#endif /* YAPOR */
#ifdef TABLING
  heap_regs->tablecompletioncode.opc = _YAP_opcode(_table_completion);
  heap_regs->tableanswerresolutioncode.opc = _YAP_opcode(_table_answer_resolution);
#ifdef YAPOR
  INIT_YAMOP_LTT(&(heap_regs->tablecompletioncode), 0);
  INIT_YAMOP_LTT(&(heap_regs->tableanswerresolutioncode), 0);
#endif /* YAPOR */
#endif /* TABLING */
  heap_regs->failcode = _YAP_opcode(_op_fail);
  heap_regs->failcode_1 = _YAP_opcode(_op_fail);
  heap_regs->failcode_2 = _YAP_opcode(_op_fail);
  heap_regs->failcode_3 = _YAP_opcode(_op_fail);
  heap_regs->failcode_4 = _YAP_opcode(_op_fail);
  heap_regs->failcode_5 = _YAP_opcode(_op_fail);
  heap_regs->failcode_6 = _YAP_opcode(_op_fail);
  
  heap_regs->env_for_trustfail_code.op = _YAP_opcode(_call);
  heap_regs->env_for_trustfail_code.s = -Signed(RealEnvSize);
  heap_regs->env_for_trustfail_code.l = NULL;
  heap_regs->env_for_trustfail_code.l2 = NULL;
  heap_regs->trustfailcode = _YAP_opcode(_trust_fail);

  heap_regs->env_for_yes_code.op = _YAP_opcode(_call);
  heap_regs->env_for_yes_code.s = -Signed(RealEnvSize);
  heap_regs->env_for_yes_code.l = NULL;
  heap_regs->env_for_yes_code.l2 = NULL;
  heap_regs->yescode.opc = _YAP_opcode(_Ystop);
  heap_regs->undef_op = _YAP_opcode(_undef_p);
  heap_regs->index_op = _YAP_opcode(_index_pred);
  heap_regs->fail_op = _YAP_opcode(_op_fail);

  heap_regs->nocode.opc = _YAP_opcode(_Nstop);

  ((yamop *)(&heap_regs->rtrycode))->opc = _YAP_opcode(_retry_and_mark);
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
  heap_regs->n_of_threads = 1;
  heap_regs->heap_top_owner = -1;
#endif /* YAPOR */
  heap_regs->clausecode.arity = 0;
  heap_regs->clausecode.clause = NIL;
  heap_regs->clausecode.func = NIL;

  heap_regs->invisiblechain.Entry = NIL;
  INIT_RWLOCK(heap_regs->invisiblechain.AERWLock);
  
  heap_regs->consultlow = (consult_obj *)_YAP_AllocCodeSpace(sizeof(consult_obj)*InitialConsultCapacity);
  if (heap_regs->consultlow == NIL) {
    _YAP_Error(SYSTEM_ERROR,TermNil,"No Heap Space in InitCodes");
   return;
  }
  heap_regs->consultcapacity = InitialConsultCapacity;
  heap_regs->system_profiling = FALSE;
  heap_regs->system_call_counting = FALSE;
  heap_regs->update_mode = 0;
  heap_regs->consultbase = heap_regs->consultsp =
    heap_regs->consultlow + heap_regs->consultcapacity;
  heap_regs->compiler_compile_mode = 1;
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
  /*
    don't initialise this here, this is initialised by _YAP_InitModules!!!!
     heap_regs->no_of_modules = 1;
  */
  heap_regs->primitives_module = 0;
  heap_regs->user_module = 1;
  heap_regs->db_queues = NULL;
  heap_regs->db_queues_cache = NULL;
  heap_regs->atom_abol = _YAP_LookupAtom("$abol");
  AtomAltNot = _YAP_LookupAtom("not");
  heap_regs->atom_append = _YAP_LookupAtom ("append");
  heap_regs->atom_array = _YAP_LookupAtom("$array");
#ifdef COROUTINING
  AtomArrayAccess = _YAP_LookupAtom("$array_arg");
#endif
  AtomArrow = _YAP_LookupAtom("->");
  heap_regs->atom_assert = _YAP_LookupAtom(":-");
  heap_regs->atom_alarm = _YAP_LookupAtom("$alarm");
#if HAVE_SIGACTION
  heap_regs->atom_sig_pending = _YAP_LookupAtom("$sig_pending");
#endif
  AtomBraces = _YAP_LookupAtom("{}");
  heap_regs->atom_b = _YAP_LookupAtom("$last_choice_pt");
  heap_regs->atom_break = _YAP_LookupAtom("$break");
  heap_regs->atom_call = _YAP_LookupAtom("call");
  heap_regs->atom_catch = _YAP_LookupAtom("$catch");
  heap_regs->atom_comma = _YAP_LookupAtom(",");
  heap_regs->atom_cpu_time = _YAP_LookupAtom("cputime");
  heap_regs->atom_csult = _YAP_LookupAtom("$csult");
  heap_regs->atom_cut = _YAP_LookupAtom("!");
  heap_regs->atom_cut_by = _YAP_LookupAtom("$cut_by");
#ifdef EUROTRA
#ifdef SFUNC
  heap_regs->atom_dollar_undef = MkAtomTerm(_YAP_LookupAtom("$undef"));
#endif
#endif
  heap_regs->atom_e = _YAP_LookupAtom("e");
  heap_regs->atom_e_q = _YAP_LookupAtom("=");
  heap_regs->atom_eof = _YAP_LookupAtom ("end_of_file");
  AtomEq = _YAP_LookupAtom("=");
#ifdef EUROTRA
  heap_regs->atom_f_b = _YAP_LookupAtom("fb");
#endif
  heap_regs->atom_fail = _YAP_LookupAtom("fail");
  heap_regs->atom_false = _YAP_LookupAtom("false");
  heap_regs->atom_fast = _YAP_LookupAtom("$fast");
  heap_regs->atom_g_t = _YAP_LookupAtom(">");
  heap_regs->atom_gc = _YAP_LookupAtom("$gc");
  heap_regs->atom_gc_margin = _YAP_LookupAtom("$gc_margin");
  heap_regs->atom_gc_trace = _YAP_LookupAtom("$gc_trace");
  heap_regs->atom_gc_verbose = _YAP_LookupAtom("$gc_verbose");
  heap_regs->atom_gc_very_verbose = _YAP_LookupAtom("$gc_very_verbose");
  AtomGVar = _YAP_LookupAtom("var");
  heap_regs->atom_global = _YAP_LookupAtom("global_sp");
  heap_regs->atom_heap_used = _YAP_LookupAtom("heapused");
  heap_regs->atom_index = _YAP_LookupAtom("$doindex");
  heap_regs->atom_inf = _YAP_LookupAtom("inf");
  heap_regs->atom_l_t = _YAP_LookupAtom("<");
  heap_regs->atom_local = _YAP_LookupAtom("local_sp");
  heap_regs->atom_meta_call = _YAP_LookupAtom("$call");
  heap_regs->atom_minus = _YAP_LookupAtom("-");
  heap_regs->atom_nan = _YAP_LookupAtom("nan");
  AtomNot = _YAP_LookupAtom("\\+");
  heap_regs->atom_otherwise = _YAP_LookupAtom("otherwise");
  heap_regs->atom_pi = _YAP_LookupAtom("pi");
  heap_regs->atom_plus = _YAP_LookupAtom("+");
  heap_regs->atom_portray = _YAP_LookupAtom("$portray");
  heap_regs->atom_profile = _YAP_LookupAtom("$profile");
  AtomQuery = _YAP_LookupAtom("?-");
  heap_regs->atom_random = _YAP_LookupAtom("random");
  heap_regs->atom_read = _YAP_LookupAtom("read");
  heap_regs->atom_repeat = _YAP_LookupAtom("repeat");
  heap_regs->atom_restore_regs = _YAP_LookupAtom("$restore_regs");
  AtomSemic = _YAP_LookupAtom(";");
  heap_regs->atom_stack_free = _YAP_LookupAtom("stackfree");
  AtomStream = _YAP_LookupAtom ("$stream");
  AtomStreamPos = _YAP_LookupAtom ("$stream_position");
  heap_regs->atom_true = _YAP_LookupAtom("true");
  AtomSpy = _YAP_LookupAtom("$spy");
  heap_regs->atom_user = _YAP_LookupAtom ("user");
  heap_regs->atom_usr_err = _YAP_LookupAtom ("user_error");
  heap_regs->atom_usr_in = _YAP_LookupAtom ("user_input");
  heap_regs->atom_usr_out = _YAP_LookupAtom ("user_output");
  AtomVar = _YAP_LookupAtom("$VAR");
  heap_regs->atom_version_number = _YAP_LookupAtom("$version_name");
  heap_regs->atom_write = _YAP_LookupAtom ("write");
#ifdef   USE_SOCKET
  heap_regs->functor_af_inet = _YAP_MkFunctor(_YAP_LookupAtom("AF_INET"),2);
  heap_regs->functor_af_local = _YAP_MkFunctor(_YAP_LookupAtom("AF_LOCAL"),1);
  heap_regs->functor_af_unix = _YAP_MkFunctor(_YAP_LookupAtom("AF_UNIX"),1);
#endif
  heap_regs->functor_alt_not = _YAP_MkFunctor(AtomAltNot, 1);
#ifdef COROUTINING
  heap_regs->functor_array_entry = _YAP_MkFunctor(AtomArrayAccess, 3);
#endif
  heap_regs->functor_arrow = _YAP_MkFunctor(AtomArrow, 2);
  heap_regs->functor_assert = _YAP_MkFunctor(AtomAssert, 2);
#ifdef COROUTINING
  heap_regs->functor_att_goal = _YAP_MkFunctor(_YAP_LookupAtom("$att_do"),2);
#endif
  heap_regs->functor_braces = _YAP_MkFunctor(AtomBraces, 1);
  heap_regs->functor_call = _YAP_MkFunctor(AtomCall, 1);
  heap_regs->functor_cut_by = _YAP_MkFunctor(AtomCutBy, 1);
  heap_regs->functor_clist = _YAP_MkFunctor(_YAP_LookupAtom("$when"), 4);
  heap_regs->functor_comma = _YAP_MkFunctor(AtomComma, 2);
  heap_regs->functor_csult = _YAP_MkFunctor(AtomCsult, 1);
  heap_regs->functor_eq = _YAP_MkFunctor(AtomEq, 2);
  heap_regs->functor_execute_in_mod = _YAP_MkFunctor(_YAP_LookupAtom("$execute_in_mod"), 2);
  heap_regs->functor_execute_within = _YAP_MkFunctor(_YAP_LookupAtom("$execute_within"), 1);
  heap_regs->functor_g_atom = _YAP_MkFunctor(_YAP_LookupAtom("atom"), 1);
  heap_regs->functor_g_atomic = _YAP_MkFunctor(_YAP_LookupAtom("atomic"), 1);
  heap_regs->functor_g_compound = _YAP_MkFunctor(_YAP_LookupAtom("compound"), 1);
  heap_regs->functor_g_float = _YAP_MkFunctor(_YAP_LookupAtom("float"), 1);
  heap_regs->functor_g_integer = _YAP_MkFunctor(_YAP_LookupAtom("integer"), 1);
  heap_regs->functor_g_number = _YAP_MkFunctor(_YAP_LookupAtom("number"), 1);
  heap_regs->functor_g_primitive = _YAP_MkFunctor(_YAP_LookupAtom("primitive"), 1);
  heap_regs->functor_g_var = _YAP_MkFunctor(AtomGVar, 1);
  heap_regs->functor_last_execute_within = _YAP_MkFunctor(_YAP_LookupAtom("$last_execute_within"), 1);
  heap_regs->functor_list = _YAP_MkFunctor(_YAP_LookupAtom("."), 2);
  heap_regs->functor_module = _YAP_MkFunctor(_YAP_LookupAtom(":"), 2);
#ifdef MULTI_ASSIGNMENT_VARIABLES
  heap_regs->functor_mutable = _YAP_MkFunctor(_YAP_LookupAtom("$mutable_variable"),
					 sizeof(timed_var)/sizeof(CELL));
#endif
  heap_regs->functor_not = _YAP_MkFunctor(AtomNot, 1);
  heap_regs->functor_or = _YAP_MkFunctor(AtomSemic, 2);
  heap_regs->functor_portray = _YAP_MkFunctor(AtomPortray, 1);
  heap_regs->functor_query = _YAP_MkFunctor(AtomQuery, 1);
  heap_regs->functor_spy = _YAP_MkFunctor(AtomSpy, 1);
  heap_regs->functor_stream = _YAP_MkFunctor (AtomStream, 1);
  heap_regs->functor_stream_pos = _YAP_MkFunctor (AtomStreamPos, 3);
  heap_regs->functor_stream_eOS = _YAP_MkFunctor (_YAP_LookupAtom("end_of_stream"), 1);
  heap_regs->functor_change_module = _YAP_MkFunctor (_YAP_LookupAtom("$change_module"), 1);
  heap_regs->functor_current_module = _YAP_MkFunctor (_YAP_LookupAtom("$current_module"), 1);
  FunctorThrow = _YAP_MkFunctor( _YAP_LookupAtom("throw"), 1);
  heap_regs->functor_u_minus = _YAP_MkFunctor (heap_regs->atom_minus, 1);
  heap_regs->functor_u_plus = _YAP_MkFunctor (heap_regs->atom_plus, 1);
  heap_regs->functor_v_bar = _YAP_MkFunctor(_YAP_LookupAtom("|"), 2);
  heap_regs->functor_var = _YAP_MkFunctor(AtomVar, 1);
#ifdef EUROTRA
  heap_regs->term_dollar_u = MkAtomTerm(_YAP_LookupAtom("$u"));
#endif
  heap_regs->term_prolog = MkAtomTerm(_YAP_LookupAtom("prolog"));
  heap_regs->term_refound_var = MkAtomTerm(_YAP_LookupAtom("$I_FOUND_THE_VARIABLE_AGAIN"));
  heap_regs->dyn_array_list = NULL;
#if DEBUG
  heap_regs->debugger_output_msg = FALSE;
#endif
  heap_regs->n_of_file_aliases = 0;
  heap_regs->file_aliases = NULL;
  heap_regs->foreign_code_loaded = NULL;
  heap_regs->yap_lib_dir = NULL;
  heap_regs->agc_hook = NULL;
  heap_regs->parser_error_style = EXCEPTION_ON_PARSER_ERROR;
  heap_regs->size_of_overflow  = 0;
  /* make sure no one else can use these two atoms */
  CurrentModule = 0;
  heap_regs->dead_clauses = NULL;
  _YAP_ReleaseAtom(AtomOfTerm(heap_regs->term_refound_var));
  /* make sure we have undefp defined */
  {
    Atom undefp_at = _YAP_FullLookupAtom("$undefp");
    Prop p = _YAP_GetPredPropByFunc(_YAP_MkFunctor(undefp_at, 1),0);
    if (p == NIL) {
      UndefCode = NULL;
    } else {
      PredEntry *undefpe;

      undefpe = RepPredProp (p);
      UndefCode = undefpe;
      /* undefp is originally undefined */
      undefpe->OpcodeOfPred = UNDEF_OPCODE;
    }
  }
  /* predicates can only be defined after this point */
  heap_regs->env_for_yes_code.p =
    heap_regs->env_for_yes_code.p0 =
    RepPredProp(PredPropByAtom(heap_regs->atom_true,0));
  heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(_YAP_MkFunctor(heap_regs->atom_meta_call,4),0));
  heap_regs->pred_dollar_catch = RepPredProp(PredPropByFunc(_YAP_MkFunctor(_YAP_LookupAtom("$catch"),3),0));
  heap_regs->pred_throw = RepPredProp(PredPropByFunc(FunctorThrow,0));
  heap_regs->pred_handle_throw = RepPredProp(PredPropByFunc(_YAP_MkFunctor(_YAP_LookupAtom("$handle_throw"),3),0));
  heap_regs->pred_goal_expansion = RepPredProp(PredPropByFunc(_YAP_MkFunctor(_YAP_LookupAtom("goal_expansion"),3),1));
  heap_regs->env_for_trustfail_code.p =
    heap_regs->env_for_trustfail_code.p0 =
    RepPredProp(PredPropByAtom(_YAP_LookupAtom("false"),0));
  {
    /* make sure we know about the module predicate */
    PredEntry *modp = RepPredProp(PredPropByFunc(heap_regs->functor_module,0));
    modp->PredFlags |= MetaPredFlag;
  }
#ifdef YAPOR
  heap_regs->getworkcode.u.ld.p = (CODEADDR)RepPredProp(PredPropByAtom(_YAP_LookupAtom("$getwork"), 0));
  heap_regs->getworkcode_seq.u.ld.p = (CODEADDR)RepPredProp(PredPropByAtom(_YAP_LookupAtom("$getwork_seq"), 0));
#endif
  heap_regs->db_erased_marker =
    (DBRef)_YAP_AllocCodeSpace(sizeof(DBStruct));
  heap_regs->db_erased_marker->id = FunctorDBRef;
  heap_regs->db_erased_marker->Flags = ErasedMask;
  heap_regs->db_erased_marker->Code = NULL;
  heap_regs->db_erased_marker->Parent = NULL;
  INIT_LOCK(heap_regs->db_erased_marker->lock);
  INIT_DBREF_COUNT(heap_regs->db_erased_marker);
}


static void 
InitVersion(void)
{
  _YAP_PutValue(_YAP_LookupAtom("$version_name"),
	   MkAtomTerm(_YAP_LookupAtom(YAP_VERSION)));
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
  _YAP_InitMemory (Trail, Heap, Stack);
#endif
  /* global initializations */ 
  init_global(aux_number_workers, aux_scheduler_loop, aux_delayed_release_load);
  init_signals();
}
#endif

void
_YAP_InitStacks(int Heap,
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
  _YAP_regp = &_YAP_standard_regs;
  /* the emulator will eventually copy them to its own local
     register array, but for now they exist */
#endif /* PUSH_REGS */

#ifdef THREADS
  regp->worker_id_ = 0;
#endif
  /* Init signal handling and time */
  /* also init memory page size, required by later functions */
  _YAP_InitSysbits ();

  /* sanity checking for data areas */
  if (Trail < MinTrailSpace)
    Trail = MinTrailSpace;
  if (Stack < MinStackSpace)
    Stack = MinStackSpace;
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
  _YAP_InitMemory (Trail, Heap, Stack);
#endif /* YAPOR || TABLING */
  for (i = 0; i < MaxHash; ++i) {
    INIT_RWLOCK(HashChain[i].AERWLock);
    HashChain[i].Entry = NIL;
  }
  _YAP_LookupAtomWithAddress("FoundVar",&(SF_STORE->AtFoundVar));
  _YAP_ReleaseAtom(AtomFoundVar);
  _YAP_LookupAtomWithAddress("[]",&(SF_STORE->AtNil));
  _YAP_LookupAtomWithAddress(".",&(SF_STORE->AtDot));
  /* InitAbsmi must be done before InitCodes */
#ifdef MPW
  _YAP_InitAbsmi(REGS, FunctorList);
#else
  _YAP_InitAbsmi();
#endif
  _YAP_InitModules();
  InitCodes();
  InitOps();
  InitDebug();
  InitVersion();
  _YAP_InitSysPath();
  InitFlags();
  InitStdPreds();
}

void
_YAP_exit (int value)
{
#if defined(YAPOR)
  unmap_memory();
#endif /* YAPOR || TABLING */
  if (! (_YAP_PrologMode & BootMode) )
    _YAP_ShutdownLoadForeign();
  exit(value);
}

