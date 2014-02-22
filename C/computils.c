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
* File:		computils.c						 *
* comments:	some useful routines for YAP's compiler			 *
*									 *
* Last rev:     $Date: 2007-11-26 23:43:08 $							 *
* $Log: not supported by cvs2svn $
* Revision 1.31  2007/11/06 17:02:12  vsc
* compile ground terms away.
*
* Revision 1.30  2006/09/20 20:03:51  vsc
* improve indexing on floats
* fix sending large lists to DB
*
* Revision 1.29  2005/12/05 17:16:10  vsc
* write_depth/3
* overflow handlings and garbage collection
* Several ipdates to CLPBN
* dif/2 could be broken in the presence of attributed variables.
*
* Revision 1.28  2005/09/08 22:06:44  rslopes
* BEAM for YAP update...
*
* Revision 1.27  2005/07/06 15:10:04  vsc
* improvements to compiler: merged instructions and fixes for ->
*
* Revision 1.26  2005/01/04 02:50:21  vsc
* - allow MegaClauses with blobs
* - change Diffs to be thread specific
* - include Christian's updates
*
* Revision 1.25  2004/11/19 17:14:13  vsc
* a few fixes for 64 bit compiling.
*
* Revision 1.24  2004/04/16 19:27:31  vsc
* more bug fixes
*
* Revision 1.23  2004/03/10 14:59:55  vsc
* optimise -> for type tests
*									 *
*									 *
*************************************************************************/
#ifdef SCCS
static char SccsId[] = "%W% %G%";
#endif

/*
 * This file includes a set of utilities, useful to the several compilation
 * modules 
 */

#include "Yap.h"
#include "Yatom.h"
#include "YapHeap.h"
#include "compile.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef DEBUG
static void ShowOp(char *, struct PSEUDO *);
#endif /* DEBUG */

/*
 * The compiler creates an instruction chain which will be assembled after
 * afterwards 
 */



typedef struct mem_blk {
  union {
    struct mem_blk *next;
    double fill;
  } ublock;
  char contents[1];
} MemBlk;

#define CMEM_BLK_SIZE (4*4096)
#define FIRST_CMEM_BLK_SIZE (16*4096)

static char *
AllocCMem (UInt size, struct intermediates *cip)
{
#if SIZEOF_INT_P==8
  size = (size + 7) & ((UInt)-8);
#else
  size = (size + 3) & ((UInt)0xfffffffc);
#endif
#if USE_SYSTEM_MALLOC
  if (!cip->blks || cip->blk_cur+size > cip->blk_top) {
    UInt blksz;
    struct mem_blk *p;

    if (size > CMEM_BLK_SIZE)
      blksz = size+sizeof(struct mem_blk);
    else
      blksz = CMEM_BLK_SIZE;
    if (!cip->blks) {
      CACHE_REGS
      if (LOCAL_CMemFirstBlock) {
	p = LOCAL_CMemFirstBlock;
	blksz = LOCAL_CMemFirstBlockSz;
	p->ublock.next = NULL;
      } else {
	if (blksz < FIRST_CMEM_BLK_SIZE)
	  blksz = FIRST_CMEM_BLK_SIZE;
	p = (struct mem_blk *)Yap_AllocCodeSpace(blksz);
	if (!p) {
	  LOCAL_Error_Size = size;
	  save_machine_regs();
	  siglongjmp(cip->CompilerBotch, OUT_OF_HEAP_BOTCH);
	}
	LOCAL_CMemFirstBlock = p;
	LOCAL_CMemFirstBlockSz = blksz;
      }
    } else {
      p = (struct mem_blk *)Yap_AllocCodeSpace(blksz);
      if (!p) {
	CACHE_REGS
	LOCAL_Error_Size = size;
	save_machine_regs();
	siglongjmp(cip->CompilerBotch, OUT_OF_HEAP_BOTCH);
      }
    }
    p->ublock.next = cip->blks;
    cip->blks = p;
    cip->blk_cur = p->contents;
    cip->blk_top = (char *)p+blksz;
  }
  {
    char *out = cip->blk_cur;
    cip->blk_cur += size;
    return out;
  }
#else
  char *p;
  if (ASP <= CellPtr (cip->freep) + 256) {
    CACHE_REGS
    LOCAL_Error_Size = 256+((char *)cip->freep - (char *)HR);
    save_machine_regs();
    siglongjmp(cip->CompilerBotch, OUT_OF_STACK_BOTCH);
  } 
  p = cip->freep;
  cip->freep += size;
  return p;
#endif
}

void
Yap_ReleaseCMem (struct intermediates *cip)
{
#if USE_SYSTEM_MALLOC
  CACHE_REGS
  struct mem_blk *p = cip->blks;
  while (p) {
    struct mem_blk *nextp = p->ublock.next;
    if (p != LOCAL_CMemFirstBlock)
      Yap_FreeCodeSpace((ADDR)p);
    p = nextp;
  }
  cip->blks = NULL;
  if (cip->label_offset &&
      cip->label_offset != LOCAL_LabelFirstArray) {
    Yap_FreeCodeSpace((ADDR)cip->label_offset);
  }
#endif
  cip->label_offset = NULL;
}

char *
Yap_AllocCMem (UInt size, struct intermediates *cip)
{
  return AllocCMem(size, cip);
}

static int
is_a_test(Term arg, Term mod)
{
  if (IsVarTerm (arg)) {
    return FALSE;
  }
  if (IsVarTerm (arg) || !IsAtomTerm(mod)) {
    return FALSE;
  }
  if (IsAtomTerm (arg)) {
      Atom At = AtomOfTerm (arg);
      PredEntry *pe = RepPredProp(PredPropByAtom(At, mod));
      if (EndOfPAEntr(pe))
	return FALSE;
      return pe->PredFlags & TestPredFlag;
  }
  if (IsApplTerm (arg)) {
    Functor f = FunctorOfTerm (arg);

    if (f == FunctorModule) {
      return is_a_test(ArgOfTerm(2,arg), ArgOfTerm(1,arg));
    } else if (f == FunctorComma) {
      return
	is_a_test(ArgOfTerm(1,arg), mod) &&
	is_a_test(ArgOfTerm(2,arg), mod);
    } else {
      PredEntry *pe = RepPredProp(PredPropByFunc(f, mod));

      if (EndOfPAEntr(pe))
	return FALSE;
      if (pe->PredFlags & AsmPredFlag) {
	int op = pe->PredFlags & 0x7f;
	if (op >= _atom && op <= _eq) {
	  return TRUE;
	}
	return FALSE;
      }      
      return pe->PredFlags & (TestPredFlag|BinaryPredFlag);
    }
  }
  return FALSE;
}

int
Yap_is_a_test_pred (Term arg, Term mod)
{
  return is_a_test(arg, mod);
}

void
Yap_emit (compiler_vm_op o, Int r1, CELL r2, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p), cip);
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->nextInst = NULL;
  if (cip->cpc == NIL) {
    cip->cpc = cip->CodeStart = p;
  } else {
    cip->cpc->nextInst = p;
    cip->cpc = p;
  }
}

void
Yap_emit_3ops (compiler_vm_op o, CELL r1, CELL r2, CELL r3, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p)+sizeof(CELL), cip);
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->rnd3 = r3;
  p->nextInst = NIL;
  if (cip->cpc == NIL)
    cip->cpc = cip->CodeStart = p;
  else
    {
      cip->cpc->nextInst = p;
      cip->cpc = p;
    }
}

void
Yap_emit_4ops (compiler_vm_op o, CELL r1, CELL r2, CELL r3, CELL r4, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p)+2*sizeof(CELL), cip);
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->rnd3 = r3;
  p->rnd4 = r4;
  p->nextInst = NIL;
  if (cip->cpc == NIL)
    cip->cpc = cip->CodeStart = p;
  else
    {
      cip->cpc->nextInst = p;
      cip->cpc = p;
    }
}

CELL *
Yap_emit_extra_size (compiler_vm_op o, CELL r1, int size, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p) + size - CellSize, cip);
  p->op = o;
  p->rnd1 = r1;
  p->nextInst = NIL;
  if (cip->cpc == NIL)
    cip->cpc = cip->CodeStart = p;
  else
    {
      cip->cpc->nextInst = p;
      cip->cpc = p;
    }
  return p->arnds;
}

static void
bip_name(Int op, char *s)
{
  switch (op) {
  case _atom:
    strcpy(s,"atom");
    break;
  case _atomic:
    strcpy(s,"atomic");
    break;
  case _integer:
    strcpy(s,"integer");
    break;
  case _nonvar:
    strcpy(s,"nonvar");
    break;
  case _number:
    strcpy(s,"number");
    break;
  case _var:
    strcpy(s,"var");
    break;
  case _cut_by:
    strcpy(s,"cut_by");
    break;
  case _save_by:
    strcpy(s,"save_by");
    break;
  case _db_ref:
    strcpy(s,"db_ref");
    break;
  case _compound:
    strcpy(s,"compound");
    break;
  case _float:
    strcpy(s,"float");
    break;
  case _primitive:
    strcpy(s,"primitive");
    break;
  case _equal:
    strcpy(s,"equal");
    break;
  case _dif:
    strcpy(s,"dif");
    break;
  case _eq:
    strcpy(s,"eq");
    break;
  case _functor:
    strcpy(s,"functor");
    break;
  case _plus:
    strcpy(s,"plus");
    break;
  case _minus:
    strcpy(s,"minus");
    break;
  case _times:
    strcpy(s,"times");
    break;
  case _div:
    strcpy(s,"div");
    break;
  case _and:
    strcpy(s,"and");
    break;
  case _or:
    strcpy(s,"or");
    break;
  case _sll:
    strcpy(s,"sll");
    break;
  case _slr:
    strcpy(s,"slr");
    break;
  case _arg:
    strcpy(s,"arg");
    break;
  default:
    strcpy(s,"");
    break;
  }
}

void
Yap_bip_name(Int op, char *s) {
  bip_name(op,s);
}

#ifdef DEBUG

static void
write_address(CELL address)
{
  if (address < (CELL)AtomBase) {
    Yap_DebugErrorPutc('L');
    Yap_DebugPlWrite(MkIntTerm (address));
  } else if (address == (CELL) FAILCODE) {
    Yap_DebugPlWrite (MkAtomTerm (AtomFail));
  } else {
    char buf[32], *p = buf;

#if HAVE_SNPRINTF
    snprintf(buf,32,"%p",(void *)address);
#else
    sprintf(buf,"%p",(void *)address);
#endif
    p[31] = '\0'; /* so that I don't have to worry */
    Yap_DebugErrorPutc('0');
    Yap_DebugErrorPutc('x');
    while (*p != '\0') {
      Yap_DebugErrorPutc(*p++);
    }
  }
}

static void
write_functor(Functor f)
{
  if (IsExtensionFunctor(f)) {
    if (f == FunctorDBRef) {
      Yap_DebugPlWrite(MkAtomTerm(AtomDBREF));
    } else if (f == FunctorLongInt) {
      Yap_DebugPlWrite(MkAtomTerm(AtomLONGINT));
    } else if (f == FunctorBigInt) {
      Yap_DebugPlWrite(MkAtomTerm(AtomLONGINT));
    } else if (f == FunctorDouble) {
      Yap_DebugPlWrite(MkAtomTerm(AtomDOUBLE));
    } else if (f == FunctorString) {
      Yap_DebugPlWrite(MkAtomTerm(AtomSTRING));
    }
  } else {
    Yap_DebugPlWrite(MkAtomTerm(NameOfFunctor (f)));
    Yap_DebugErrorPutc ('/');
    Yap_DebugPlWrite(MkIntTerm(ArityOfFunctor (f)));
  }
}

static void
ShowOp (char *f, struct PSEUDO *cpc)
{
  char ch;
  Int arg = cpc->rnd1;
  Int rn = cpc->rnd2;
  CELL *cptr = cpc->arnds;

  while ((ch = *f++) != 0)
    {
      if (ch == '%')
	switch (ch = *f++)
	  {
#ifdef BEAM
	case '1':
		Yap_DebugPlWrite(MkIntTerm(rn));
		break;
	case '4':
		Yap_DebugPlWrite(MkIntTerm(arg));
		break;
#endif
	  case 'a':
	  case 'n':
	    Yap_DebugPlWrite ((Term) arg);
	    break;
	  case 'b':
	    /* write a variable bitmap for a call */
	    {
	      CACHE_REGS
	      int max = arg/(8*sizeof(CELL)), i;
	      CELL *ptr = cptr;
	      for (i = 0; i <= max; i++) {
		Yap_DebugPlWrite(MkIntegerTerm((Int)(*ptr++)));
	      }
	    }
	    break;		
	  case 'l':
	    write_address (arg);
	    break;
	  case 'B':
	    {
	      char s[32];

	      bip_name(rn,s);
	      Yap_DebugPlWrite (MkAtomTerm(Yap_LookupAtom(s)));
	    }
	    break;
	  case 'd':
	    {
	      CACHE_REGS
	      Yap_DebugPlWrite (MkIntegerTerm (arg));
	    }
	    break;
	  case 'z':
	    Yap_DebugPlWrite (MkIntTerm (cpc->rnd3));
	    break;
	  case 'v':
	    {
	      Ventry *v = (Ventry *) arg;
	      Yap_DebugErrorPutc (v->KindOfVE == PermVar ? 'Y' : 'X');
	      Yap_DebugPlWrite (MkIntTerm ((v->NoOfVE) & MaskVarAdrs));
	    }
	    break;	
	  case 'N':
	    {
	      Ventry *v;

	      cpc = cpc->nextInst;
	      arg = cpc->rnd1;
	      v = (Ventry *) arg;
	      Yap_DebugErrorPutc (v->KindOfVE == PermVar ? 'Y' : 'X');
	      Yap_DebugPlWrite (MkIntTerm ((v->NoOfVE) & MaskVarAdrs));
	    }
	    break;
	  case 'm':
	    Yap_DebugPlWrite (MkAtomTerm ((Atom) arg));
	    Yap_DebugErrorPutc ('/');
	    Yap_DebugPlWrite (MkIntTerm (rn));
	    break;
	  case 'p':
	    {
	      PredEntry *p = RepPredProp ((Prop) arg);
	      Functor f = p->FunctorOfPred;
	      UInt arity = p->ArityOfPE;
	      Term mod;

	      if (p->ModuleOfPred)
		mod = p->ModuleOfPred;
	      else
		mod = TermProlog;
	      Yap_DebugPlWrite (mod);
	      Yap_DebugErrorPutc (':');
	      if (arity == 0)
		Yap_DebugPlWrite (MkAtomTerm ((Atom)f));
	      else
		Yap_DebugPlWrite (MkAtomTerm (NameOfFunctor (f)));
	      Yap_DebugErrorPutc ('/');
	      Yap_DebugPlWrite (MkIntTerm (arity));
	    }
	    break;
	  case 'P':
	    {
	      PredEntry *p = RepPredProp((Prop) rn);
	      Functor f = p->FunctorOfPred;
	      UInt arity = p->ArityOfPE;
	      Term mod = TermProlog;

	      if (p->ModuleOfPred) mod = p->ModuleOfPred;
	      Yap_DebugPlWrite (mod);
	      Yap_DebugErrorPutc (':');
	      if (arity == 0)
		Yap_DebugPlWrite (MkAtomTerm ((Atom)f));
	      else
		Yap_DebugPlWrite (MkAtomTerm (NameOfFunctor (f)));
	      Yap_DebugErrorPutc ('/');
	      Yap_DebugPlWrite (MkIntTerm (arity));
	    }
	    break;
	  case 'f':
	    write_functor((Functor)arg);
	    break;
	  case 'r':
	    Yap_DebugErrorPutc ('A');
	    Yap_DebugPlWrite (MkIntTerm (rn));
	    break;
	  case 'h':
	    {
	      CELL my_arg = *cptr++;
	      write_address(my_arg);
	    }
	    break;
	  case 'g':
	    write_address(arg);
	    break;
	  case 'i':
	    write_address (arg);
	    break;
	  case 'j':
	    {
	      Functor fun = (Functor)*cptr++;
	      if (IsExtensionFunctor(fun)) {
		if (fun == FunctorDBRef) {
		  Yap_DebugPlWrite(MkAtomTerm(AtomDBREF));
		} else if (fun == FunctorLongInt) {
		  Yap_DebugPlWrite(MkAtomTerm(AtomLONGINT));
		} else if (fun == FunctorDouble) {
		  Yap_DebugPlWrite(MkAtomTerm(AtomDOUBLE));
		} else if (fun == FunctorString) {
		  Yap_DebugPlWrite(MkAtomTerm(AtomSTRING));
		}
	      } else {
		Yap_DebugPlWrite (MkAtomTerm(NameOfFunctor(fun)));
		Yap_DebugErrorPutc ('/');
		Yap_DebugPlWrite (MkIntTerm(ArityOfFunctor(fun)));
	      }
	    }
	    break;
	  case 'O':
	    Yap_DebugPlWrite(AbsAppl(cptr));
	    break;
	  case 'x':
	    Yap_DebugPlWrite (MkIntTerm (rn >> 1));
	    Yap_DebugErrorPutc ('\t');
	    Yap_DebugPlWrite (MkIntTerm (rn & 1));
	    break;
	  case 'w':
	    Yap_DebugPlWrite (arg);
	    break;
	  case 'o':
	    Yap_DebugPlWrite ((Term) * cptr++);
	  case 'c':
	    {
	      int i;
	      CELL *ptr = (CELL *)cptr[0];
	      for (i = 0; i < arg; ++i) {
		CELL my_arg;
		Yap_DebugErrorPutc('\t');
		if (*ptr) {
		  Yap_DebugPlWrite ((Term) *ptr++);
		} else {
		  Yap_DebugPlWrite (MkIntTerm (0));
		  ptr++;
		}
		Yap_DebugErrorPutc ('\t');
		my_arg = *ptr++;
		write_address (my_arg);
		if (i+1 < arg)
		  Yap_DebugErrorPutc ('\n');
	      }
	    }
	    break;
	  case 'e':
	    {
	      int i;
	      CELL *ptr = (CELL *)cptr[0];
	      for (i = 0; i < arg; ++i)	{
		CELL my_arg = ptr[0], lbl = ptr[1];
		Yap_DebugErrorPutc('\t');
		if (my_arg) {
		  write_functor((Functor)my_arg);
		} else {
		  Yap_DebugPlWrite(MkIntTerm (0));
		}
		Yap_DebugErrorPutc('\t');
		write_address(lbl);
		ptr += 2;
		if (i+1 < arg)
		  Yap_DebugErrorPutc('\n');
	      }
	    }
	    break;
	  default:
	    Yap_DebugErrorPutc ('%');
	    Yap_DebugErrorPutc (ch);
	  }
      else
	Yap_DebugErrorPutc (ch);
    }
  Yap_DebugErrorPutc ('\n');
}

static char *opformat[] =
{
  "nop",
  "get_var\t\t%v,%r",
  "put_var\t\t%v,%r",
  "get_val\t\t%v,%r",
  "put_val\t\t%v,%r",
  "get_atom\t%a,%r",
  "put_atom\t%a,%r",
  "get_num\t\t%n,%r",
  "put_num\t\t%n,%r",
  "get_float\t\t%w,%r",
  "put_float\t\t%w,%r",
  "get_dbterm\t%w,%r",
  "put_dbterm\t%w,%r",
  "get_longint\t\t%w,%r",
  "put_longint\t\t%w,%r",
  "get_bigint\t\t%l,%r",
  "put_bigint\t\t%l,%r",
  "get_list\t%r",
  "put_list\t%r",
  "get_struct\t%f,%r",
  "put_struct\t%f,%r",
  "put_unsafe\t%v,%r",
  "unify_var\t%v",
  "write_var\t%v",
  "unify_val\t%v",
  "write_val\t%v",
  "unify_atom\t%a",
  "write_atom\t%a",
  "unify_num\t%n",
  "write_num\t%n",
  "unify_float\t%w",
  "write_float\t%w",
  "unify_dbterm\t%w",
  "write_dbterm\t%w",
  "unify_longint\t%w",
  "write_longint\t%w",
  "unify_bigint\t%l",
  "write_bigint\t%l",
  "unify_list",
  "write_list",
  "unify_struct\t%f",
  "write_struct\t%f",
  "write_unsafe\t%v",
  "unify_local\t%v",
  "write local\t%v",
  "unify_last_list",
  "write_last_list",
  "unify_last_struct\t%f",
  "write_last_struct\t%f",
  "unify_last_var\t%v",
  "unify_last_val\t%v",
  "unify_last_local\t%v",
  "unify_last_atom\t%a",
  "unify_last_num\t%n",
  "unify_last_float\t%w", 
  "unify_last_dbterm\t%w",
  "unify_last_longint\t%w",
  "unify_last_bigint\t%l",
  "ensure_space",
  "native_code",
  "function_to_var\t%v,%B",
  "function_to_val\t%v,%B",
  "function_to_0\t%B",
  "align_float",
  "fail",
  "cut",
  "cutexit",
  "allocate",
  "deallocate",
  "try_me_else\t\t%l\t%x",
  "jump\t\t%l",
  "jump\t\t%l",
  "proceed",
  "call\t\t%p,%d,%z",
  "execute\t\t%p",
  "sys\t\t%p",
  "%l:",
  "name\t\t%m,%d",
  "pop\t\t%l",
  "retry_me_else\t\t%l\t%x",
  "trust_me_else_fail\t%x",
  "either_me\t\t%l,%d,%z",
  "or_else\t\t%l,%z",
  "or_last",
  "push_or",
  "pushpop_or",
  "pop_or",
  "save_by\t\t%v",
  "commit_by\t\t%v",
  "patch_by\t\t%v",
  "try\t\t%g\t%x",
  "retry\t\t%g\t%x",
  "trust\t\t%g\t%x",
  "try_in\t\t%g\t%x",
  "jump_if_var\t\t%g",
  "jump_if_nonvar\t\t%g",
  "cache_arg\t%r",
  "cache_sub_arg\t%d",
  "user_index",
  "switch_on_type\t%h\t%h\t%h\t%h",
  "switch_on_constant\t%i\n%c",
  "if_constant\t%i\n%c",
  "switch_on_functor\t%i\n%e",
  "if_functor\t%i\n%e",
  "if_not_then\t%i\t%h\t%h\t%h",
  "index_on_dbref",
  "index_on_blob",
  "index_on_long",
  "check_var\t %r",
  "save_pair\t%v",
  "save_appl\t%v",
  "pvar_bitmap\t%l,%b",
  "pvar_live_regs\t%l,%b",
  "fetch_reg1_reg2\t%N,%N",
  "fetch_constant_reg\t%l,%N",
  "fetch_reg_constant\t%l,%N",
  "fetch_integer_reg\t%d,%N",
  "fetch_reg_integer\t%d,%N",
  "enter_profiling\t\t%g",
  "retry_profiled\t\t%g",
  "count_call_op\t\t%g",
  "count_retry_op\t\t%g",
  "restore_temps\t\t%l",
  "restore_temps_and_skip\t\t%l",
  "enter_lu",
  "empty_call\t\t%l,%d",
#ifdef YAPOR
  "sync",
#endif /* YAPOR */
#ifdef TABLING
  "table_new_answer",
  "table_try_single\t%g\t%x",
#endif /* TABLING */
#ifdef TABLING_INNER_CUTS
  "clause_with_cut",
#endif /* TABLING_INNER_CUTS */
#ifdef BEAM
  "run_op %1,%4",
  "body_op %1",
  "endgoal_op",
  "try_me_op %1,%4",
  "retry_me_op %1,%4",
  "trust_me_op %1,%4",
  "only_1_clause_op %1,%4",
  "create_first_box_op %1,%4",
  "create_box_op %1,%4",
  "create_last_box_op %1,%4",
  "remove_box_op %1,%4",
  "remove_last_box_op %1,%4",
  "prepare_tries",
  "std_base_op %1,%4",
  "direct_safe_call",
  "skip_while_var_op",
  "wait_while_var_op",
  "force_wait_op",
  "write_op",
  "is_op",
  "equal_op",
  "exit",
#endif
  "fetch_args_for_bccall\t%v",
  "binary_cfunc\t\t%v,%P",
  "blob\t%O",
  "label_control\t"
#ifdef SFUNC
  ,
  "get_s_f_op\t%f,%r",
  "put_s_f_op\t%f,%r",
  "unify_s_f_op\t%f",
  "write_s_f_op\t%f",
  "unify_s_var\t%v,%r",
  "write_s_var\t%v,%r",
  "unify_s_val\t%v,%r",
  "write_s_val\t%v,%r",
  "unify_s_a\t%a,%r",
  "write_s_a\t%a,%r",
  "get_s_end",
  "put_s_end",
  "unify_s_end",
  "write_s_end"
#endif
};


void
Yap_ShowCode (struct intermediates *cint)
{
  CACHE_REGS
  CELL *oldH = HR;
  struct PSEUDO *cpc;

  cpc = cint->CodeStart;
  /* MkIntTerm and friends may build terms in the global stack */
  HR = (CELL *)cint->freep;
  while (cpc) {
    compiler_vm_op ic = cpc->op;
    if (ic != nop_op) {
      ShowOp (opformat[ic], cpc);
    }
    cpc = cpc->nextInst;
  }
  Yap_DebugErrorPutc ('\n');
  HR = oldH;
}

#endif /* DEBUG */

