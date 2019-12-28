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
static char SccsId[] = "%W% %G%";
#endif

/*
 * The code from this file is used to initialize the environment for prolog
 *
 */

#define __INIT_C__ 1

#include "Yap.h"
#include "alloc.h"
#include "clause.h"
#include "yapio.h"
#include <stdlib.h>

#include "Foreign.h"

#ifdef LOW_LEVEL_TRACER
#include "tracer.h"
#endif
#ifdef YAPOR
#ifdef YAPOR_COW
#include <signal.h>
#endif /* YAPOR_COW */
#include "or.macros.h"
#endif /* YAPOR */
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
#endif /* YAPOR || TABLING */
#if HAVE_STRING_H
#include <string.h>
#endif

#ifndef YAPOR
Atom AtomFoundVar, AtomFreeTerm, AtomNil, AtomDot;
#endif // !YAPOR

int Yap_output_msg = FALSE;

#if DEBUG

#define LOGFILE "logfile"

#ifdef MACC
static void InTTYLine(char *);
#endif
#endif
static void SetOp(int, int, char *, Term);
static void InitOps(void);
static void InitDebug(void);
static void CleanBack(PredEntry *, CPredicate, CPredicate, CPredicate);
static void InitStdPreds(struct yap_boot_params *yapi);
static void InitCodes(struct yap_boot_params *yapi);
static void InitVersion(void);
void exit(int);
static void InitWorker(int wid);

/**************	YAP PROLOG GLOBAL VARIABLES *************************/

/************* variables related to memory allocation ***************/
ADDR Yap_HeapBase;

/**************	declarations local to init.c ************************/
static char *optypes[] = {"", "xfx", "xfy", "yfx", "xf", "yf", "fx", "fy"};

/* OS page size for memory allocation */
size_t Yap_page_size;

#if DEBUG
#if COROUTINING
int Yap_Portray_delays = FALSE;
#endif
#endif

void *YAP_save;

/**

@defgroup Operators Summary of YAP Predefined Operators
@ingroup YAPSyntax
@{

The Prolog syntax caters for operators of three main kinds:

  + prefix;
  + infix;
  + postfix.


Each operator has precedence in the range 1 to 1200, and this
precedence is used to disambiguate expressions where the structure of the
term denoted is not made explicit using brackets. The operator of higher
precedence is the main functor.

If there are two operators with the highest precedence, the ambiguity
is solved analyzing the types of the operators. The possible infix types are:
 _xfx_,  _xfy_, and  _yfx_.

With an operator of type  _xfx_ both sub-expressions must have lower
precedence than the operator itself, unless they are bracketed (which
assigns to them zero precedence). With an operator type  _xfy_ only the
left-hand sub-expression must have lower precedence. The opposite happens
for  _yfx_ type.

A prefix operator can be of type  _fx_ or  _fy_.
A postfix operator can be of type  _xf_ or  _yf_.
The meaning of the notation is analogous to the above.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a + b * c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
means

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a + (b * c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as + and \* have the following types and precedences:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(500,yfx,'+').
:-op(400,yfx,'*').
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now defining

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(700,xfy,'++').
:-op(700,xfx,'=:=').
a ++ b =:= c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 means

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
a ++ (b =:= c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following is the list of the declarations of the predefined operators:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
:-op(1200,fx,['?-', ':-']).
:-op(1200,xfx,[':-','-->']).
:-op(1150,fx,[block,
	discontiguous,dynamic,
        initialization,mode,multifile,meta_predicate,
              public,sequential,table]).
:-op(1100,xfy,[';','|']).
:-op(1050,xfy,->).
:-op(1000,xfy,',').
:-op(999,xfy,'.').
:-op(900,fy,['\+', not]).
:-op(900,fx,[nospy, spy]).
:-op(700,xfx,[@>=,@=<,@<,@>,<,=,>,=:=,=\=,\==,>=,=<,==,\=,=..,is]).
:-op(500,yfx,['\/','/\','+','-']).
:-op(500,fx,['+','-']).
:-op(400,yfx,['<<','>>','//','*','/']).
:-op(300,xfx,mod).
:-op(200,xfy,['^','**']).
:-op(50,xfx,same).
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*/

#define xfx 1
#define xfy 2
#define yfx 3
#define xf 4
#define yf 5
#define fx 6
#define fy 7

int Yap_IsOpType(char *type) {
  int i;

  for (i = 1; i <= 7; ++i)
    if (strcmp(type, optypes[i]) == 0)
      break;
  return (i <= 7);
}

static int OpDec(int p, const char *type, Atom a, Term m) {
  int i;
  AtomEntry *ae = RepAtom(a);
  OpEntry *info;

#if defined(MODULE_INDEPENDENT_OPERATORS_FLAG)
  if (booleanFlag(MODULE_INDEPENDENT_OPERATORS_FLAG)) {
    m = PROLOG_MODULE;
  } else
#endif
  {
    if (m == TermProlog)
      m = PROLOG_MODULE;
    else if (m == USER_MODULE)
      m = PROLOG_MODULE;
  }
  for (i = 1; i <= 7; ++i)
    if (strcmp(type, optypes[i]) == 0)
      break;
  if (i > 7) {
    Yap_Error(DOMAIN_ERROR_OPERATOR_SPECIFIER, MkAtomTerm(Yap_LookupAtom(type)),
              "op/3");
    return (FALSE);
  }
  if (p) {
    if (i == 1 || i == 2 || i == 4)
      p |= DcrlpFlag;
    if (i == 1 || i == 3 || i == 6)
      p |= DcrrpFlag;
  }
  WRITE_LOCK(ae->ARWLock);
  info = Yap_GetOpPropForAModuleHavingALock(ae, m);
  if (EndOfPAEntr(info)) {
    ModEntry *me;
    info = (OpEntry *)Yap_AllocAtomSpace(sizeof(OpEntry));
    if (!info)
      return false;
    info->KindOfPE = Ord(OpProperty);
    info->NextForME = (me = Yap_GetModuleEntry(m))->OpForME;
    me->OpForME = info;
    info->OpModule = m;
    info->OpName = a;
    // LOCK(OpListLock);
    info->OpNext = OpList;
    OpList = info;
    // UNLOCK(OpListLock);
    AddPropToAtom(ae, (PropEntry *)info);
    INIT_RWLOCK(info->OpRWLock);
    WRITE_LOCK(info->OpRWLock);
    WRITE_UNLOCK(ae->ARWLock);
    info->Prefix = info->Infix = info->Posfix = 0;
  } else {
    WRITE_LOCK(info->OpRWLock);
    WRITE_UNLOCK(ae->ARWLock);
  }
  if (i <= 3) {
    if (trueGlobalPrologFlag(ISO_FLAG) &&
        info->Posfix != 0) /* there is a posfix operator */ {
      /* ISO dictates */
      WRITE_UNLOCK(info->OpRWLock);
      Yap_Error(PERMISSION_ERROR_CREATE_OPERATOR, MkAtomTerm(a), "op/3");
      return false;
    }
    info->Infix = p;
  } else if (i <= 5) {

    if (trueGlobalPrologFlag(ISO_FLAG) &&
        info->Infix != 0) /* there is an infix operator */ {
      /* ISO dictates */
      WRITE_UNLOCK(info->OpRWLock);
      Yap_Error(PERMISSION_ERROR_CREATE_OPERATOR, MkAtomTerm(a), "op/3");
      return false;
    }
    info->Posfix = p;
  } else {
    info->Prefix = p;
  }
  WRITE_UNLOCK(info->OpRWLock);
  return true;
}

int Yap_OpDec(int p, char *type, Atom a, Term m) {
  return (OpDec(p, type, a, m));
}

static void SetOp(int p, int type, char *at, Term m) {
#if DEBUG
  if (GLOBAL_Option[5])
    fprintf(stderr, "[setop %d %s %s]\n", p, optypes[type], at);
#endif
  OpDec(p, optypes[type], Yap_LookupAtom(at), m);
}

bool Yap_dup_op(OpEntry *op, ModEntry *she) {
  AtomEntry *ae = RepAtom(op->OpName);
  OpEntry *info = (OpEntry *)Yap_AllocAtomSpace(sizeof(OpEntry));
  if (!info)
    return false;
  memmove(info, op, sizeof(OpEntry));
  info->NextForME = she->OpForME;
  she->OpForME = info;
  info->OpModule = MkAtomTerm(she->AtomOfME);
  AddPropToAtom(ae, AbsOpProp(info));
  INIT_RWLOCK(info->OpRWLock);
  return true;
}

/* Gets the info about an operator in a prop */
Atom Yap_GetOp(OpEntry *pp, int *prio, int fix) {
  int n;
  SMALLUNSGN p;

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
  return Yap_LookupAtom(optypes[n]);
}

typedef struct OPSTRUCT {
  char *opName;
  short int opType, opPrio;
} Opdef;

static Opdef Ops[] = {{":-", xfx, 1200},
                      {"-->", xfx, 1200},
                      {"?-", fx, 1200},
                      {":-", fx, 1200},
                      {"dynamic", fx, 1150},
                      {"thread_local", fx, 1150},
                      {"initialization", fx, 1150},
                      {"volatile", fx, 1150},
                      {"mode", fx, 1150},
                      {"public", fx, 1150},
                      {"multifile", fx, 1150},
                      {"meta_predicate", fx, 1150},
                      {"module_transparent", fx, 1150},
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
                      {"|", xfy, 1105},
                      {";", xfy, 1100},
                      /*  {";", yf, 1100}, not allowed in ISO */
                      {"->", xfy, 1050},
                      {"*->", xfy, 1050},
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
                      {"=@=", xfx, 700},
                      {"\\=@=", xfx, 700},
                      {"=:=", xfx, 700},
                      {"=\\=", xfx, 700},
                      {"<", xfx, 700},
                      {">", xfx, 700},
                      {"=<", xfx, 700},
                      {">=", xfx, 700},
                      {"as", xfx, 600},
                      {":", xfy, 600},
                      {"+", yfx, 500},
                      {"-", yfx, 500},
                      {"/\\", yfx, 500},
                      {"\\/", yfx, 500},
                      {"><", yfx, 500},
                      {"#", yfx, 500},
                      {"rdiv", yfx, 400},
                      {"div", yfx, 400},
                      {"xor", yfx, 400},
                      {"*", yfx, 400},
                      {"/", yfx, 400},
                      {"//", yfx, 400},
                      {"<<", yfx, 400},
                      {">>", yfx, 400},
                      {"mod", yfx, 400},
                      {"rem", yfx, 400},
                      {"+", fy, 200},
                      {"-", fy, 200},
                      {"\\", fy, 200},
                      {"//", yfx, 400},
                      {"**", xfx, 200},
                      {"^", xfy, 200}};

static void InitOps(void) {
  unsigned int i;
  for (i = 0; i < sizeof(Ops) / sizeof(*Ops); ++i)
    SetOp(Ops[i].opPrio, Ops[i].opType, Ops[i].opName, PROLOG_MODULE);
}

/// @}

#if DEBUG
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#endif

static void InitDebug(void) {
  Atom At;
#if DEBUG
  int i;

  for (i = 1; i < 20; ++i)
    GLOBAL_Option[i] = 0;
  if (Yap_output_msg) {
    char ch;

#if _WIN32
    if (!_isatty(_fileno(stdin))) {
      return;
    }
#elif HAVE_ISATTY
    if (!isatty(0)) {
      return;
    }
#endif
    fprintf(stderr, "absmi address:%p\n", FunAdr(Yap_absmi));
    fprintf(stderr, "Set	Trace Options:\n");
    fprintf(stderr, "a getch\t\tb token\t\tc Lookup\td LookupVar\ti Index\n");
    fprintf(stderr, "e SetOp\t\tf compile\tg icode\t\th boot\t\tl log\n");
    fprintf(stderr, "m Machine\t p parser\n");
    while ((ch = putchar(getchar())) != '\n' && ch != '\r') {
      if (ch >= 'a' && ch <= 'z')
        GLOBAL_Option[ch - 'a' + 1] = 1;
      GLOBAL_Option[ch - 'a' + 1] = 1;
    }
    if (GLOBAL_Option['l' - 96]) {
      GLOBAL_logfile = fopen(LOGFILE, "w");
      if (GLOBAL_logfile == NULL) {
        fprintf(stderr, "can not open %s\n", LOGFILE);
        getchar();
        exit(0);
      }
      fprintf(stderr, "logging session to file 'logfile'\n");
#ifdef MAC
      Yap_SetTextFile(LOGFILE);
      lp = my_line;
      curfile = Nill;
#endif
    }
  }
#endif
  /* Set at full leash */
  At = AtomLeash;
  Yap_PutValue(At, MkIntTerm(15));
}

static UInt update_flags_from_prolog(UInt flags, PredEntry *pe) {
  if (pe->PredFlags & MetaPredFlag)
    flags |= MetaPredFlag;
  if (pe->PredFlags & SourcePredFlag)
    flags |= SourcePredFlag;
  if (pe->PredFlags & SequentialPredFlag)
    flags |= SequentialPredFlag;
  if (pe->PredFlags & UDIPredFlag)
    flags |= UDIPredFlag;
  if (pe->PredFlags & ModuleTransparentPredFlag)
    flags |= ModuleTransparentPredFlag;
  if (pe->PredFlags & StandardPredFlag)
    flags |= StandardPredFlag;
  return flags;
}

void Yap_InitCPred(const char *Name, arity_t Arity, CPredicate code,
                   pred_flags_t flags) {
  CACHE_REGS
  Atom atom = NIL;
  PredEntry *pe = NULL;
  yamop *p_code;
  StaticClause *cl = NULL;
  Functor f = NULL;

  while (atom == NIL) {
    if (flags & UserCPredFlag)
      atom = Yap_LookupAtom(Name);
    else
      atom = Yap_FullLookupAtom(Name);
    if (atom == NIL && !Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (Arity) {
    while (!f) {
      f = Yap_MkFunctor(atom, Arity);
      if (!f && !Yap_growheap(FALSE, 0L, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
        return;
      }
    }
  }
  while (pe == NULL) {
    if (Arity)
      pe = RepPredProp(PredPropByFunc(f, CurrentModule));
    else
      pe = RepPredProp(PredPropByAtom(atom, CurrentModule));
          if (pe && (CurrentModule == 0 || CurrentModule == TermProlog) &&
	pe->ModuleOfPred && pe->ModuleOfPred != TermProlog) {
      Yap_ThrowError(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, TermNil, "pre-existing predicate in  module %s while initializing prolog:%s", RepAtom(AtomOfTerm(pe->ModuleOfPred))->StrOfAE, Name);
  }

        if (pe && (CurrentModule == 0 || CurrentModule == TermProlog) &&
	pe->ModuleOfPred && pe->ModuleOfPred != TermProlog) {
      Yap_ThrowError(PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE, TermNil, "pre-existing predicate in  module %s while initializing prolog:%s", RepAtom(AtomOfTerm(pe->ModuleOfPred))->StrOfAE, Name);
  }
if (!pe && !Yap_growheap(FALSE, sizeof(PredEntry), NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (pe->PredFlags & CPredFlag) {
    /* already exists */
    flags = update_flags_from_prolog(flags, pe);
    cl = ClauseCodeToStaticClause(pe->CodeOfPred);
    if ((flags | StandardPredFlag | CPredFlag) != pe->PredFlags) {
      Yap_ClauseSpace -= cl->ClSize;
      Yap_FreeCodeSpace((ADDR)cl);
      cl = NULL;
    }
  }
  p_code = cl->ClCode;
  while (!cl) {
    UInt sz;

    if (flags & SafePredFlag) {
      sz = (CELL)NEXTOP(NEXTOP(NEXTOP(p_code, Osbpp), p), l);
    } else {
      sz = (CELL)NEXTOP(NEXTOP(NEXTOP(NEXTOP(NEXTOP(p_code, e), p), Osbpp), p),
                        l);
    }
    cl = (StaticClause *)Yap_AllocCodeSpace(sz);
    if (!cl) {
      if (!Yap_growheap(FALSE, sz, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
        return;
      }
    } else {
      Yap_ClauseSpace += sz;
      cl->ClFlags = StaticMask;
      cl->ClNext = NULL;
      cl->ClSize = sz;
      cl->usc.ClLine = Yap_source_line_no();
      p_code = cl->ClCode;
    }
  }
  pe->CodeOfPred = p_code;
  pe->PredFlags = flags | StandardPredFlag | CPredFlag;
  pe->src.OwnerFile = Yap_ConsultingFile(PASS_REGS1);
  pe->cs.f_code = code;
  if (!(flags & SafePredFlag)) {
    p_code->opc = Yap_opcode(_allocate);
    p_code = NEXTOP(p_code, e);
  }
  if (flags & UserCPredFlag)
    p_code->opc = Yap_opcode(_call_usercpred);
  else
    p_code->opc = Yap_opcode(_call_cpred);
  p_code->y_u.Osbpp.bmap = NULL;
  p_code->y_u.Osbpp.s = -Signed(RealEnvSize);
  p_code->y_u.Osbpp.p = p_code->y_u.Osbpp.p0 = pe;
  p_code = NEXTOP(p_code, Osbpp);
  if (!(flags & SafePredFlag)) {
    p_code->opc = Yap_opcode(_deallocate);
    p_code->y_u.p.p = pe;
    p_code = NEXTOP(p_code, p);
  }
  p_code->opc = Yap_opcode(_procceed);
  p_code->y_u.p.p = pe;
  p_code = NEXTOP(p_code, p);
  p_code->opc = Yap_opcode(_Ystop);
  p_code->y_u.l.l = cl->ClCode;
  pe->OpcodeOfPred = pe->CodeOfPred->opc;
}

bool Yap_AddCallToFli(PredEntry *pe, CPredicate call) {
  yamop *p_code;

  if (pe->PredFlags & BackCPredFlag) {
    p_code = (yamop *)(pe->FirstClause);
    p_code->y_u.OtapFs.f = call;
    return true;
  } else if (pe->PredFlags & CPredFlag) {
    pe->cs.f_code = call;
    return true;
  } else {
    return false;
  }
}

bool Yap_AddRetryToFli(PredEntry *pe, CPredicate re) {
  yamop *p_code;

  if (pe->PredFlags & BackCPredFlag) {
    p_code = (yamop *)(pe->FirstClause);
    p_code = NEXTOP(p_code, OtapFs);
    p_code->y_u.OtapFs.f = re;
    return true;
  } else {
    return false;
  }
}

bool Yap_AddCutToFli(PredEntry *pe, CPredicate CUT) {
  yamop *p_code;

  if (pe->PredFlags & BackCPredFlag) {
    p_code = (yamop *)(pe->FirstClause);
    p_code = NEXTOP(p_code, OtapFs);
    p_code = NEXTOP(p_code, OtapFs);
    p_code->y_u.OtapFs.f = CUT;
    return true;
  } else {
    return false;
  }
}

void Yap_InitCmpPred(const char *Name, arity_t Arity, CmpPredicate cmp_code,
                     pred_flags_t flags) {
  CACHE_REGS
  Atom atom = NIL;
  PredEntry *pe = NULL;
  yamop *p_code = NULL;
  StaticClause *cl = NULL;
  Functor f = NULL;

  while (atom == NIL) {
    atom = Yap_FullLookupAtom(Name);
    if (atom == NIL && !Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (Arity) {
    while (!f) {
      f = Yap_MkFunctor(atom, Arity);
      if (!f && !Yap_growheap(FALSE, 0L, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
        return;
      }
    }
  }
  while (pe == NULL) {
    if (Arity)
      pe = RepPredProp(PredPropByFunc(f, CurrentModule));
    else
      pe = RepPredProp(PredPropByAtom(atom, CurrentModule));
    if (!pe && !Yap_growheap(FALSE, sizeof(PredEntry), NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (pe->PredFlags & BinaryPredFlag) {
    flags = update_flags_from_prolog(flags, pe);
    p_code = pe->CodeOfPred;
    /* already exists */
  } else {
    while (!cl) {
      UInt sz = sizeof(StaticClause) +
                (CELL)NEXTOP(NEXTOP(NEXTOP(((yamop *)NULL), plxxs), p), l);
      cl = (StaticClause *)Yap_AllocCodeSpace(sz);
      if (!cl) {
        if (!Yap_growheap(FALSE, sz, NULL)) {
          Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s",
                    Name);
          return;
        }
      } else {
        Yap_ClauseSpace += sz;
        cl->ClFlags = StaticMask | StandardPredFlag;
        cl->ClNext = NULL;
        cl->ClSize = sz;
        cl->usc.ClLine = Yap_source_line_no();
        p_code = cl->ClCode;
        break;
      }
    }
  }
  // pe->PredFlags = flags | StandardPredFlag;
  pe->TrueCodeOfPred = p_code;
    pe->FirstClause = pe->LastClause = p_code;
    pe->NOfClauses = 1;
  pe->CodeOfPred = p_code;
  pe->cs.d_code = cmp_code;
  pe->ModuleOfPred = CurrentModule;
  p_code->opc = pe->OpcodeOfPred = Yap_opcode(_call_bfunc_xx);
  p_code->y_u.plxxs.p = pe;
  p_code->y_u.plxxs.f = FAILCODE;
  p_code->y_u.plxxs.x1 = Yap_emit_x(1);
  p_code->y_u.plxxs.x2 = Yap_emit_x(2);
  p_code->y_u.plxxs.flags = Yap_compile_cmp_flags(pe);
  p_code = NEXTOP(p_code, plxxs);
  p_code->opc = Yap_opcode(_procceed);
  p_code->y_u.p.p = pe;
  p_code = NEXTOP(p_code, p);
  p_code->opc = Yap_opcode(_Ystop);
  p_code->y_u.l.l = cl->ClCode;
}

void Yap_InitAsmPred(const char *Name, arity_t Arity, int code, CPredicate def,
                     pred_flags_t flags) {
  CACHE_REGS
  Atom atom = NIL;
  PredEntry *pe = NULL;
  Functor f = NULL;
    StaticClause *cl;
    yamop *p_code;
    
  while (atom == NIL) {
    atom = Yap_FullLookupAtom(Name);
    if (atom == NIL && !Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (Arity) {
    while (!f) {
      f = Yap_MkFunctor(atom, Arity);
      if (!f && !Yap_growheap(FALSE, 0L, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
        return;
      }
    }
  }
  bool exists;

  while (pe == NULL) {
    if (Arity)
      pe = RepPredProp(PredPropByFunc(f, CurrentModule));
    else
      pe = RepPredProp(PredPropByAtom(atom, CurrentModule));
    if (!pe && !Yap_growheap(FALSE, sizeof(PredEntry), NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  exists = pe->OpcodeOfPred != UNDEF_OPCODE;
  flags |= AsmPredFlag | StandardPredFlag | (code);
  if (exists) {
    flags = update_flags_from_prolog(flags, pe);
    /* already exists */
  }
  pe->PredFlags = flags;
  pe->cs.f_code = def;
  pe->ModuleOfPred = CurrentModule;
  if (def != NULL && !exists) {
    p_code = ((StaticClause *)NULL)->ClCode;

      if (flags & SafePredFlag) {
        cl = (StaticClause *)Yap_AllocCodeSpace(
            (CELL)NEXTOP(NEXTOP(NEXTOP(((yamop *)p_code), Osbpp), p), l));
      } else {
        cl = (StaticClause *)Yap_AllocCodeSpace((CELL)NEXTOP(
            NEXTOP(NEXTOP(NEXTOP(NEXTOP(((yamop *)p_code), e), Osbpp), p), p),
            l));
      if (!cl) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "No Heap Space in InitAsmPred");
        return;
      }
      Yap_ClauseSpace +=
          (CELL)NEXTOP(NEXTOP(NEXTOP(((yamop *)p_code), Osbpp), p), l);
      }
    } else {
      cl = ClauseCodeToStaticClause(pe->CodeOfPred);
    }
    cl->ClFlags = StaticMask;
    cl->ClNext = NULL;
    if (flags & SafePredFlag) {
      cl->ClSize = (CELL)NEXTOP(NEXTOP(NEXTOP(((yamop *)p_code), Osbpp), e), e);
    } else {
      cl->ClSize = (CELL)NEXTOP(
          NEXTOP(NEXTOP(NEXTOP(NEXTOP(((yamop *)p_code), e), Osbpp), p), e), e);
    }
    cl->usc.ClLine = Yap_source_line_no();
    p_code = cl->ClCode;
  pe->TrueCodeOfPred = p_code;
    pe->FirstClause = pe->LastClause = p_code;
    pe->NOfClauses = 1;
    pe->CodeOfPred = p_code;
    if (!(flags & SafePredFlag)) {
      p_code->opc = Yap_opcode(_allocate);
      p_code = NEXTOP(p_code, e);
    }
    p_code->opc = Yap_opcode(_call_cpred);
    p_code->y_u.Osbpp.bmap = NULL;
    p_code->y_u.Osbpp.s = -Signed(RealEnvSize);
    p_code->y_u.Osbpp.p = p_code->y_u.Osbpp.p0 = pe;
    p_code = NEXTOP(p_code, Osbpp);
    if (!(flags & SafePredFlag)) {
      p_code->opc = Yap_opcode(_deallocate);
      p_code->y_u.p.p = pe;
      p_code = NEXTOP(p_code, p);
    }
    p_code->opc = Yap_opcode(_procceed);
    p_code->y_u.p.p = pe;
    p_code = NEXTOP(p_code, p);
    p_code->opc = Yap_opcode(_Ystop);
    p_code->y_u.l.l = cl->ClCode;
    pe->OpcodeOfPred = pe->CodeOfPred->opc;

}

static void CleanBack(PredEntry *pe, CPredicate Start, CPredicate Cont,
                      CPredicate Cut) {
  yamop *code;
  if (pe->FirstClause != pe->LastClause ||
      pe->TrueCodeOfPred != pe->FirstClause ||
      pe->CodeOfPred != pe->FirstClause) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil,
              "initiating a C Pred with backtracking");
    return;
  }
  code = (yamop *)(pe->FirstClause);
  code->y_u.OtapFs.p = pe;
  if (pe->PredFlags & UserCPredFlag)
    code->opc = Yap_opcode(_try_userc);
  else
    code->opc = Yap_opcode(_try_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 2);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  code->y_u.OtapFs.f = Start;
  code = NEXTOP(code, OtapFs);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = Yap_opcode(_retry_userc);
  else
    code->opc = Yap_opcode(_retry_c);
#ifdef YAPOR
  INIT_YAMOP_LTT(code, 1);
  PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
  code->y_u.OtapFs.f = Cont;
  code = NEXTOP(code, OtapFs);
  if (pe->PredFlags & UserCPredFlag)
    code->opc = Yap_opcode(_cut_c);
  else
    code->opc = Yap_opcode(_cut_userc);
  code->y_u.OtapFs.p = pe;
  code->y_u.OtapFs.f = Cut;
}

void Yap_InitCPredBack(const char *Name, arity_t Arity, arity_t Extra,
                       CPredicate Call, CPredicate Retry, pred_flags_t flags) {
  Yap_InitCPredBack_(Name, Arity, Extra, Call, Retry, NULL, flags);
}

void Yap_InitCPredBackCut(const char *Name, arity_t Arity, arity_t Extra,
                          CPredicate Start, CPredicate Cont, CPredicate Cut,
                          pred_flags_t flags) {
  Yap_InitCPredBack_(Name, Arity, Extra, Start, Cont, Cut, flags);
}

void Yap_InitCPredBack_(const char *Name, arity_t Arity, arity_t Extra,
                        CPredicate Start, CPredicate Cont, CPredicate Cut,
                        pred_flags_t flags) {
  CACHE_REGS
  PredEntry *pe = NULL;
  Atom atom = NIL;
  Functor f = NULL;

  while (atom == NIL) {
    atom = Yap_FullLookupAtom(Name);
    if (atom == NIL && !Yap_growheap(FALSE, 0L, NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (Arity) {
    while (!f) {
      f = Yap_MkFunctor(atom, Arity);
      if (!f && !Yap_growheap(FALSE, 0L, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
        return;
      }
    }
  }
  while (pe == NULL) {
    if (Arity)
      pe = RepPredProp(PredPropByFunc(f, CurrentModule));
    else
      pe = RepPredProp(PredPropByAtom(atom, CurrentModule));
    if (!pe && !Yap_growheap(FALSE, sizeof(PredEntry), NULL)) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "while initializing %s", Name);
      return;
    }
  }
  if (pe->FirstClause != NIL) {
    flags = update_flags_from_prolog(flags, pe);
    CleanBack(pe, Start, Cont, Cut);
  } else {
    StaticClause *cl;
    yamop *code = ((StaticClause *)NULL)->ClCode;
    UInt sz =
        (CELL)NEXTOP(NEXTOP(NEXTOP(NEXTOP(code, OtapFs), OtapFs), OtapFs), l);
    if (flags & UserCPredFlag)
      pe->PredFlags = UserCPredFlag | BackCPredFlag | CompiledPredFlag | flags;
    else
      pe->PredFlags = CompiledPredFlag | StandardPredFlag | BackCPredFlag;

#ifdef YAPOR
    pe->PredFlags |= SequentialPredFlag;
#endif /* YAPOR */

    cl = (StaticClause *)Yap_AllocCodeSpace(sz);

    if (cl == NULL) {
      Yap_Error(RESOURCE_ERROR_HEAP, TermNil, "No Heap Space in InitCPredBack");
      return;
    }
    cl->ClFlags = StaticMask;
    cl->ClNext = NULL;
    Yap_ClauseSpace += sz;
    cl->ClSize =
        (CELL)NEXTOP(NEXTOP(NEXTOP(NEXTOP(code, OtapFs), OtapFs), OtapFs), e);
    cl->usc.ClLine = Yap_source_line_no();

    code = cl->ClCode;
    pe->TrueCodeOfPred = pe->CodeOfPred = pe->FirstClause =
        pe->LastClause = code;
    if (flags & UserCPredFlag)
      pe->OpcodeOfPred = code->opc = Yap_opcode(_try_userc);
    else
      pe->OpcodeOfPred = code->opc = Yap_opcode(_try_c);
    code->y_u.OtapFs.f = Start;
    code->y_u.OtapFs.p = pe;
    code->y_u.OtapFs.s = Arity;
    code->y_u.OtapFs.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 2);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code, OtapFs);
    if (flags & UserCPredFlag)
      code->opc = Yap_opcode(_retry_userc);
    else
      code->opc = Yap_opcode(_retry_c);
    code->y_u.OtapFs.f = Cont;
    code->y_u.OtapFs.p = pe;
    code->y_u.OtapFs.s = Arity;
    code->y_u.OtapFs.extra = Extra;
#ifdef YAPOR
    INIT_YAMOP_LTT(code, 1);
    PUT_YAMOP_SEQ(code);
#endif /* YAPOR */
    code = NEXTOP(code, OtapFs);
    if (flags & UserCPredFlag)
      code->opc = Yap_opcode(_cut_userc);
    else
      code->opc = Yap_opcode(_cut_c);
    code->y_u.OtapFs.f = Cut;
    code->y_u.OtapFs.p = pe;
    code->y_u.OtapFs.s = Arity;
    code->y_u.OtapFs.extra = Extra;
    code = NEXTOP(code, OtapFs);
    code->opc = Yap_opcode(_Ystop);
    code->y_u.l.l = cl->ClCode;
  }
}

static void InitStdPreds(struct yap_boot_params *yapi)
{
  CurrentModule = PROLOG_MODULE;
  Yap_InitCPreds();
  Yap_InitBackCPreds();
  BACKUP_MACHINE_REGS();
  Yap_InitFlags(false);
  Yap_InitPlIO(yapi);
#if HAVE_MPE
  Yap_InitMPE();
#endif
}

static void InitPredHash(void) {
  UInt i;

  PredHash = (PredEntry **)Yap_AllocAtomSpace(sizeof(PredEntry **) *
                                              PredHashInitialSize);
  PredHashTableSize = PredHashInitialSize;
  if (PredHash == NULL) {
    Yap_Error(SYSTEM_ERROR_FATAL, MkIntTerm(0),
              "allocating initial predicate hash table");
  }
  for (i = 0; i < PredHashTableSize; ++i) {
    PredHash[i] = NULL;
  }
  INIT_RWLOCK(PredHashRWLock);
}

static void InitEnvInst(yamop start[2], yamop **instp, op_numbers opc,
                        PredEntry *pred) {
  yamop *ipc = start;

  /* make it look like the instruction is preceeded by a call instruction */
  ipc->opc = Yap_opcode(_call);
  ipc->y_u.Osbpp.s = -Signed(RealEnvSize);
  ipc->y_u.Osbpp.bmap = NULL;
  ipc->y_u.Osbpp.p = pred;
  ipc->y_u.Osbpp.p0 = pred;
  ipc = NEXTOP(ipc, Osbpp);
  ipc->opc = Yap_opcode(opc);
  *instp = ipc;
}

static void InitOtaplInst(yamop start[1], OPCODE opc, PredEntry *pe) {
  yamop *ipc = start;

  /* this is a place holder, it should not really be used */
  ipc->opc = Yap_opcode(opc);
  ipc->y_u.Otapl.s = 0;
  ipc->y_u.Otapl.p = pe;
  ipc->y_u.Otapl.d = NULL;
#ifdef YAPOR
  INIT_YAMOP_LTT(ipc, 1);
#endif /* YAPOR */
#ifdef TABLING
  ipc->y_u.Otapl.te = NULL;
#endif /* TABLING */
}

static void InitDBErasedMarker(void) {
  DBErasedMarker = (DBRef)Yap_AllocCodeSpace(sizeof(DBStruct));
  Yap_LUClauseSpace += sizeof(DBStruct);
  DBErasedMarker->id = FunctorDBRef;
  DBErasedMarker->Flags = ErasedMask;
  DBErasedMarker->Code = NULL;
  DBErasedMarker->DBT.DBRefs = NULL;
  DBErasedMarker->Parent = NULL;
}

static void InitLogDBErasedMarker(void) {
  LogDBErasedMarker = (LogUpdClause *)Yap_AllocCodeSpace(
      sizeof(LogUpdClause) + (UInt)NEXTOP((yamop *)NULL, e));
  Yap_LUClauseSpace += sizeof(LogUpdClause) + (UInt)NEXTOP((yamop *)NULL, e);
  LogDBErasedMarker->Id = FunctorDBRef;
  LogDBErasedMarker->ClFlags = ErasedMask | LogUpdMask;
  LogDBErasedMarker->lusl.ClSource = NULL;
  LogDBErasedMarker->ClRefCount = 0;
  LogDBErasedMarker->ClExt = NULL;
  LogDBErasedMarker->ClPrev = NULL;
  LogDBErasedMarker->ClNext = NULL;
  LogDBErasedMarker->ClSize = (UInt)NEXTOP(((LogUpdClause *)NULL)->ClCode, e);
  LogDBErasedMarker->ClCode->opc = Yap_opcode(_op_fail);
  INIT_CLREF_COUNT(LogDBErasedMarker);
}

static void InitEmptyWakeups(void) {}

static void InitAtoms(void) {
  int i;
  AtomHashTableSize = MaxHash;
  HashChain =
      (AtomHashEntry *)Yap_AllocAtomSpace(sizeof(AtomHashEntry) * MaxHash);
  if (HashChain == NULL) {
    Yap_Error(SYSTEM_ERROR_FATAL, MkIntTerm(0),
              "allocating initial atom table");
  }
  for (i = 0; i < MaxHash; ++i) {
    INIT_RWLOCK(HashChain[i].AERWLock);
    HashChain[i].Entry = NIL;
  }
  NOfAtoms = 0;
#if 0 && OLD_STYLE_INITIAL_ATOMS
  Yap_LookupAtomWithAddress("**", (AtomEntry *)&(SF_STORE->AtFoundVar));
  Yap_ReleaseAtom(AtomFoundVar);
  Yap_LookupAtomWithAddress("?", (AtomEntry *)&(SF_STORE->AtFreeTerm));
  Yap_ReleaseAtom(AtomFreeTerm);
  Yap_LookupAtomWithAddress("[]", (AtomEntry *)&(SF_STORE->AtNil));
  Yap_LookupAtomWithAddress(".", (AtomEntry *)&(SF_STORE->AtDot));
#else
  AtomFoundVar = Yap_LookupAtom("**");
  Yap_ReleaseAtom(AtomFoundVar);
  AtomFreeTerm = Yap_LookupAtom("?");
  Yap_ReleaseAtom(AtomFreeTerm);
  AtomNil = Yap_LookupAtom("[]");
  AtomDot = Yap_LookupAtom(".");
#endif
}

static void InitWideAtoms(void) {
  int i;

  WideAtomHashTableSize = MaxWideHash;
  WideHashChain =
      (AtomHashEntry *)Yap_AllocAtomSpace(sizeof(AtomHashEntry) * MaxWideHash);
  if (WideHashChain == NULL) {
    Yap_Error(SYSTEM_ERROR_FATAL, MkIntTerm(0), "allocating wide atom table");
  }
  for (i = 0; i < MaxWideHash; ++i) {
    INIT_RWLOCK(WideHashChain[i].AERWLock);
    WideHashChain[i].Entry = NIL;
  }
  NOfWideAtoms = 0;
}

static void InitInvisibleAtoms(void) {
  /* initialize invisible chain */
  INVISIBLECHAIN.Entry = NIL;
  INIT_RWLOCK(INVISIBLECHAIN.AERWLock);
}

#ifdef YAPOR
void Yap_init_yapor_workers(void) {
  CACHE_REGS
  int proc;
#ifdef YAPOR_THREADS
  return;
#endif /* YAPOR_THREADS */
#ifdef YAPOR_COW
  GLOBAL_master_worker = getpid();
  if (GLOBAL_number_workers > 1) {
    int son;
    son = fork();
    if (son == -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "fork error (Yap_init_yapor_workers)");
    if (son > 0) {
      /* I am the father, I must stay here and wait for my children to all die
       */
      struct sigaction sigact;
      sigact.sa_handler = SIG_DFL;
      sigemptyset(&sigact.sa_mask);
      sigact.sa_flags = SA_RESTART;
      sigaction(SIGINT, &sigact, NULL);
      pause();
      exit(0);
    } else
      GLOBAL_worker_pid(0) = getpid();
  }
#endif /* YAPOR_COW */
  for (proc = 1; proc < GLOBAL_number_workers; proc++) {
    int son;
    son = fork();
    if (son == -1)
      Yap_Error(SYSTEM_ERROR_FATAL, TermNil,
                "fork error (Yap_init_yapor_workers)");
    if (son == 0) {
      /* new worker */
      worker_id = proc;
      Yap_remap_yapor_memory();
      LOCAL = REMOTE(worker_id);
      memmove(REMOTE(worker_id), REMOTE(0), sizeof(struct worker_local));
      InitWorker(worker_id);
      break;
    } else
      GLOBAL_worker_pid(proc) = son;
  }
}
#endif /* YAPOR */

#ifdef THREADS
static void InitThreadHandle(int wid) {
  REMOTE_ThreadHandle(wid).in_use = FALSE;
  REMOTE_ThreadHandle(wid).zombie = FALSE;
  REMOTE_ThreadHandle(wid).local_preds = NULL;
#ifdef LOW_LEVEL_TRACER
  REMOTE_ThreadHandle(wid).thread_inst_count = 0LL;
#endif
  pthread_mutex_init(&(REMOTE_ThreadHandle(wid).tlock), NULL);
  pthread_mutex_init(&(REMOTE_ThreadHandle(wid).tlock_status), NULL);
  REMOTE_ThreadHandle(wid).tdetach = (CELL)0;
  REMOTE_ThreadHandle(wid).cmod = (CELL)0;
  {
    mbox_t *mboxp = &REMOTE_ThreadHandle(wid).mbox_handle;
    pthread_mutex_t *mutexp;
    pthread_cond_t *condp;
    struct idb_queue *msgsp;

    mboxp->name = MkIntTerm(0);
    condp = &mboxp->cond;
    pthread_cond_init(condp, NULL);
    mutexp = &mboxp->mutex;
    pthread_mutex_init(mutexp, NULL);
    msgsp = &mboxp->msgs;
    mboxp->nmsgs = 0;
    mboxp->nclients = 0;
    mboxp->open = true;
    Yap_init_tqueue(msgsp);
  }
}

int Yap_InitThread(int new_id) {
  struct worker_local *new_s;
  if (new_id) {
    if (!(new_s =
              (struct worker_local *)calloc(sizeof(struct worker_local), 1)))
      return FALSE;
    Yap_local[new_id] = new_s;
    if (!((REGSTORE *)pthread_getspecific(Yap_yaamregs_key))) {
      REGSTORE *rs = (REGSTORE *)calloc(sizeof(REGSTORE), 1);
      pthread_setspecific(Yap_yaamregs_key, (const void *)rs);
      REMOTE_ThreadHandle(new_id).default_yaam_regs = rs;
      REMOTE_ThreadHandle(new_id).current_yaam_regs = rs;
      rs->worker_id_ = new_id;
      rs->worker_local_ = REMOTE(new_id);
    }
  }
  InitWorker(new_id);
  return TRUE;
}
#endif

static void InitScratchPad(int wid) {
  REMOTE_ScratchPad(wid).ptr = NULL;
  REMOTE_ScratchPad(wid).sz = SCRATCH_START_SIZE;
  REMOTE_ScratchPad(wid).msz = SCRATCH_START_SIZE;
}

static CELL *InitHandles(int wid) {
  size_t initial_slots = 1024;
  CELL *handles;

  REMOTE_CurSlot(wid) = 1;
  REMOTE_NSlots(wid) = initial_slots;
  handles = calloc(initial_slots, sizeof(CELL));

  if (handles == NULL) {
    Yap_Error(SYSTEM_ERROR_INTERNAL, 0 /* TermNil */,
              "No space for handles at " __FILE__ " : %d", __LINE__);
  }

  RESET_VARIABLE(handles);
  return handles;
}

void Yap_CloseScratchPad(void) {
  CACHE_REGS
  Yap_FreeCodeSpace(LOCAL_ScratchPad.ptr);
  LOCAL_ScratchPad.sz = SCRATCH_START_SIZE;
  LOCAL_ScratchPad.msz = SCRATCH_START_SIZE;
}

#include "iglobals.h"

#include "ilocals.h"

#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
struct global_data *Yap_global;
long Yap_worker_area_size;

#endif

#if defined(THREADS)
struct worker_local *Yap_local[MAX_THREADS];
#elif defined(YAPOR)
struct worker_local *Yap_local;
#else /* !THREADS && !YAPOR */
struct worker_local Yap_local;
#endif

static void InitCodes(struct yap_boot_params *yapi)
{
  CACHE_REGS
#if THREADS
  int wid;
  for (wid = 1; wid < MAX_THREADS; wid++) {
    Yap_local[wid] = NULL;
  }
#endif
#include "ihstruct.h"
#if THREADS
  Yap_InitThread(0);
#endif /* THREADS */
  InitGlobal();
#if !THREADS
  InitWorker(0);
#endif /* THREADS */
  Yap_InitFirstWorkerThreadHandle();
  /* make sure no one else can use these two atoms */
  LOCAL_SourceModule = CurrentModule = 0;
  Yap_ReleaseAtom(AtomOfTerm(TermRefoundVar));
  /* flags require atom table done, but must be done as soon as possible,
     definitely before any predicate initialization */
  // Yap_InitFlags(); moved to HEAPFIELDS
  /* make sure we have undefp defined */
  /* predicates can only be defined after this point */
  {
    /* make sure we know about the module predicate */
    PredEntry *modp = RepPredProp(PredPropByFunc(FunctorModule, PROLOG_MODULE));
    modp->PredFlags |= MetaPredFlag;
  }
#ifdef YAPOR
  Yap_heap_regs->getwork_code->y_u.Otapl.p =
      RepPredProp(PredPropByAtom(AtomGetwork, PROLOG_MODULE));
  Yap_heap_regs->getwork_seq_code->y_u.Otapl.p =
      RepPredProp(PredPropByAtom(AtomGetworkSeq, PROLOG_MODULE));
#endif /* YAPOR */
}

static void InitVersion(void) {
  Yap_PutValue(AtomVersionNumber, MkAtomTerm(Yap_LookupAtom(YAP_FULL_VERSION)));
}

const char *Yap_version(void) {
  Term t = Yap_GetValue(AtomVersionNumber);
  return RepAtom(AtomOfTerm(t))->StrOfAE;
}

void Yap_InitWorkspace(struct yap_boot_params *yapi,
          UInt Heap, size_t Stack, size_t Trail, size_t Atts,
                       size_t max_table_size, int n_workers, int sch_loop,
                       int delay_load)
{
  CACHE_REGS

/* initialize system stuff */
#if PUSH_REGS
#ifdef THREADS
  if (!(Yap_local[0] =
            (struct worker_local *)calloc(sizeof(struct worker_local), 1)))
    return;
  pthread_key_create(&Yap_yaamregs_key, NULL);
  pthread_setspecific(Yap_yaamregs_key, (const void *)&Yap_standard_regs);
  GLOBAL_master_thread = pthread_self();
#else
  /* In this case we need to initialize the abstract registers */
  Yap_regp = &Yap_standard_regs;
/* the emulator will eventually copy them to its own local
   register array, but for now they exist */
#endif
#endif /* PUSH_REGS */

#ifdef THREADS
  Yap_regp = ((REGSTORE *)pthread_getspecific(Yap_yaamregs_key));
  LOCAL = REMOTE(0);
#endif /* THREADS */
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
  LOCAL = REMOTE(0);
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */
  if (Heap < MinHeapSpace)
    Heap = MinHeapSpace;
  Heap = AdjustPageSize(Heap * K);
  Heap /= (K);
  /* sanity checking for data areas */
  if (Trail < MinTrailSpace)
    Trail = MinTrailSpace;
  Trail = AdjustPageSize(Trail * K);
  Trail /= (K);
  if (Stack < MinStackSpace)
    Stack = MinStackSpace;
  Stack = AdjustPageSize(Stack * K);
  Stack /= (K);
  Atts = 0;
#if defined(THREADS) || defined(YAPOR)
  worker_id = 0;
#endif /* YAPOR || THREADS */
#ifdef YAPOR
  if (n_workers > MAX_WORKERS)
    Yap_Error(SYSTEM_ERROR_INTERNAL, TermNil, "excessive number of workers");
#ifdef YAPOR_COPY
  INFORMATION_MESSAGE("YapOr: copy model with %d worker%s", n_workers,
                      n_workers == 1 ? "" : "s");
#elif YAPOR_COW
  INFORMATION_MESSAGE("YapOr: acow model with %d worker%s", n_workers,
                      n_workers == 1 ? "" : "s");
#elif YAPOR_SBA
  INFORMATION_MESSAGE("YapOr: sba model with %d worker%s", n_workers,
                      n_workers == 1 ? "" : "s");
#elif YAPOR_THREADS
  INFORMATION_MESSAGE("YapOr: threads model with %d worker%s", n_workers,
                      n_workers == 1 ? "" : "s");
#endif /* YAPOR_COPY - YAPOR_COW - YAPOR_SBA - YAPOR_THREADS */
#endif /* YAPOR */
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
  Yap_init_yapor_stacks_memory(Trail, Heap, Stack + Atts, n_workers);
#else
  Yap_InitMemory(Trail, Heap, Stack + Atts);
#endif
#if defined(YAPOR) || defined(TABLING)
  Yap_init_global_optyap_data(max_table_size, n_workers, sch_loop, delay_load);
#endif /* YAPOR || TABLING */

  Yap_AttsSize = Atts;
/* InitAbsmi must be done before InitCodes */
/* This must be done before initializing predicates */
#ifdef MPW
  Yap_InitAbsmi(REGS, FunctorList);
#else
  Yap_InitAbsmi();
#endif
  InitCodes(yapi);
  InitOps();
  InitDebug();
  InitVersion();
#if THREADS
  /* make sure we use the correct value of regcache */
  regcache = ((REGSTORE *)pthread_getspecific(Yap_yaamregs_key));
#endif
#if USE_SYSTEM_MALLOC
  if (Trail < MinTrailSpace)
    Trail = MinTrailSpace;
  if (Stack < MinStackSpace)
    Stack = MinStackSpace;
  if (!(LOCAL_GlobalBase = (ADDR)malloc((Trail + Stack) * 1024))) {
    Yap_Error(RESOURCE_ERROR_HEAP, 0,
              "could not allocate stack space for main thread");
    Yap_exit(1);
  }
#if THREADS
  /* don't forget this is a thread */
  LOCAL_ThreadHandle.stack_address = LOCAL_GlobalBase;
  LOCAL_ThreadHandle.tsize = Trail;
  LOCAL_ThreadHandle.ssize = Stack;
#endif
#endif
  GLOBAL_AllowGlobalExpansion = true;
  GLOBAL_AllowLocalExpansion = true;
  GLOBAL_AllowTrailExpansion = true;
  Yap_InitExStacks(0, Trail, Stack);
    Yap_InitYaamRegs(0, true);
  InitStdPreds(yapi);
  /* make sure tmp area is available */
  { Yap_ReleasePreAllocCodeSpace(Yap_PreAllocCodeSpace()); }
}

int Yap_HaltRegisterHook(HaltHookFunc f, void *env) {
  struct halt_hook *h;

  if (!(h = (struct halt_hook *)Yap_AllocCodeSpace(sizeof(struct halt_hook))))
    return FALSE;
  h->environment = env;
  h->hook = f;
  LOCK(GLOBAL_BGL);
  h->next = GLOBAL_HaltHooks;
  GLOBAL_HaltHooks = h;
  UNLOCK(GLOBAL_BGL);
  return TRUE;
}

static void run_halt_hooks(int code) {
  struct halt_hook *hooke = GLOBAL_HaltHooks;

  while (hooke) {
    hooke->hook(code, hooke->environment);
    hooke = hooke->next;
  }
}

void Yap_exit(int value) {
  CACHE_REGS
  void closeFiles(int all);
#if defined(YAPOR_COPY) || defined(YAPOR_COW) || defined(YAPOR_SBA)
  Yap_unmap_yapor_memory();
#endif /* YAPOR_COPY || YAPOR_COW || YAPOR_SBA */

  if (!(LOCAL_PrologMode & BootMode)) {
#ifdef LOW_PROF
    remove("PROFPREDS");
    remove("PROFILING");
#endif
    run_halt_hooks(value);
    Yap_ShutdownLoadForeign();
  }
  Yap_CloseStreams();
  Yap_CloseReadline();
#if USE_SYSTEM_MALLOC
#endif
  exit(value);
}
