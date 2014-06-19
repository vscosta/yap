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
static void ShowOp(const char *, struct PSEUDO *);
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
ShowOp (const char *f, struct PSEUDO *cpc)
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
	  case 'S':
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

static const char *
getFormat(compiler_vm_op ic) {
  switch( ic ) {
  case nop_op:
    return "nop";
  case get_var_op:
    return "get_var\t\t%v,%r";
  case put_var_op:
    return  "put_var\t\t%v,%r";
  case get_val_op:
    return "get_val\t\t%v,%r";
  case put_val_op:
    return "put_val\t\t%v,%r";
  case get_atom_op:
    return "get_atom\t%a,%r";
  case put_atom_op:
    return "put_atom\t%a,%r";
  case get_num_op:
    return "get_num\t\t%n,%r";
  case put_num_op:
    return "put_num\t\t%n,%r";
  case get_float_op:
    return "get_float\t\t%w,%r";
  case put_float_op:
    return "put_float\t\t%w,%r";
  case get_string_op:
    return "get_string\t\t%w,%S";
  case put_string_op:
    return "put_string\t\t%w,%S";
  case get_dbterm_op:
    return "get_dbterm\t%w,%r";
  case put_dbterm_op:
    return "put_dbterm\t%w,%r";
  case get_longint_op:
    return "get_longint\t\t%w,%r";
  case put_longint_op:
    return "put_longint\t\t%w,%r";
  case get_bigint_op:
    return "get_bigint\t\t%l,%r";
  case put_bigint_op:
    return "put_bigint\t\t%l,%r";
  case get_list_op:
    return "get_list\t%r";
  case put_list_op:
    return "put_list\t%r";
  case get_struct_op:
    return "get_struct\t%f,%r";
  case put_struct_op:
    return "put_struct\t%f,%r";
  case put_unsafe_op:
    return "put_unsafe\t%v,%r";
  case unify_var_op:
    return "unify_var\t%v";
  case write_var_op:
    return "write_var\t%v";
  case unify_val_op:
    return "unify_val\t%v";
  case write_val_op:
    return "write_val\t%v";
  case unify_atom_op:
    return "unify_atom\t%a";
  case write_atom_op:
    return "write_atom\t%a";
  case unify_num_op:
    return "unify_num\t%n";
  case write_num_op:
    return "write_num\t%n";
  case unify_float_op:
    return "unify_float\t%w";
  case write_float_op:
    return "write_float\t%w";
  case unify_string_op:
    return "unify_string\t%S";
  case write_string_op:
    return "write_string\t%S";
  case unify_dbterm_op:
    return "unify_dbterm\t%w";
  case write_dbterm_op:
    return "write_dbterm\t%w";
  case unify_longint_op:
    return "unify_longint\t%w";
  case write_longint_op:
    return "write_longint\t%w";
  case unify_bigint_op:
    return "unify_bigint\t%l";
  case write_bigint_op:
    return "write_bigint\t%l";
  case unify_list_op:
    return "unify_list";
  case write_list_op:
    return "write_list";
  case unify_struct_op:
    return "unify_struct\t%f";
  case write_struct_op:
    return "write_struct\t%f";
  case write_unsafe_op:
    return "write_unsafe\t%v";
  case unify_local_op:
    return "unify_local\t%v";
  case write_local_op:
    return "write local\t%v";
  case unify_last_list_op:
    return "unify_last_list";
  case write_last_list_op:
    return "write_last_list";
  case unify_last_struct_op:
    return "unify_last_struct\t%f";
  case write_last_struct_op:
    return "write_last_struct\t%f";
  case unify_last_var_op:
    return "unify_last_var\t%v";
  case unify_last_val_op:
    return "unify_last_val\t%v";
  case unify_last_local_op:
    return "unify_last_local\t%v";
  case unify_last_atom_op:
    return "unify_last_atom\t%a";
  case unify_last_num_op:
    return "unify_last_num\t%n";
  case unify_last_float_op:
     return "unify_last_float\t%w";
  case unify_last_string_op:
     return "unify_last_string\t%S";
  case unify_last_dbterm_op:
    return "unify_last_dbterm\t%w";
  case unify_last_longint_op:
    return "unify_last_longint\t%w";
  case unify_last_bigint_op:
    return "unify_last_bigint\t%l";
  case ensure_space_op:
    return "ensure_space";
  case native_op:
    return "native_code";
  case f_var_op:
    return "function_to_var\t%v,%B";
  case f_val_op:
    return "function_to_val\t%v,%B";
  case f_0_op:
    return "function_to_0\t%B";
  case align_float_op:
    return "align_float";
  case fail_op:
    return "fail";
  case cut_op:
    return "cut";
  case cutexit_op:
    return "cutexit";
  case allocate_op:
    return "allocate";
  case deallocate_op:
    return "deallocate";
  case tryme_op:
    return "try_me_else\t\t%l\t%x";
  case jump_op:
    return "jump\t\t%l";
  case jumpi_op:
    return "jump_in_indexing\t\t%i";
  case procceed_op:
    return "proceed";
  case call_op:
    return "call\t\t%p,%d,%z";
  case execute_op:
    return "execute\t\t%p";
  case safe_call_op:
    return "sys\t\t%p";
  case label_op:
    return "%l:";
  case name_op:
    return "name\t\t%m,%d";
  case pop_op:
    return "pop\t\t%l";
  case retryme_op:
    return "retry_me_else\t\t%l\t%x";
  case trustme_op:
    return "trust_me_else_fail\t%x";
  case either_op:
    return "either_me\t\t%l,%d,%z";
  case orelse_op:
    return "or_else\t\t%l,%z";
  case orlast_op:
    return "or_last";
  case push_or_op:
    return "push_or";
  case pop_or_op:
    return "pop_or";
  case pushpop_or_op:
    return "pushpop_or";
  case save_b_op:
    return "save_by\t\t%v";
  case commit_b_op:
    return "commit_by\t\t%v";
  case patch_b_op:
    return "patch_by\t\t%v";
  case try_op:
    return "try\t\t%g\t%x";
  case retry_op:
    return "retry\t\t%g\t%x";
  case trust_op:
    return "trust\t\t%g\t%x";
  case try_in_op:
    return "try_in\t\t%g\t%x";
  case jump_v_op:
    return "jump_if_var\t\t%g";
  case jump_nv_op:
    return "jump_if_nonvar\t\t%g";
  case cache_arg_op:
    return "cache_arg\t%r";
  case cache_sub_arg_op:
    return "cache_sub_arg\t%d";
  case user_switch_op:
    return "user_switch";
  case switch_on_type_op:
    return "switch_on_type\t%h\t%h\t%h\t%h";
  case switch_c_op:
    return "switch_on_constant\t%i\n%c";
  case if_c_op:
    return "if_constant\t%i\n%c";
  case switch_f_op:
    return "switch_on_functor\t%i\n%e";
  case if_f_op:
    return "if_functor\t%i\n%e";
  case if_not_op:
    return "if_not_then\t%i\t%h\t%h\t%h";
  case index_dbref_op:
    return "index_on_dbref";
  case index_blob_op:
    return "index_on_blob";
  case index_long_op:
    return "index_on_blob";
  case index_string_op:
    return "index_on_string";
  case 	if_nonvar_op:
    return "check_var\t %r";
  case save_pair_op:
    return "save_pair\t%v";
  case save_appl_op:
    return "save_appl\t%v";
  case mark_initialised_pvars_op:
    return "pvar_bitmap\t%l,%b";
  case mark_live_regs_op:
    return "pvar_live_regs\t%l,%b";
  case fetch_args_vv_op:
    return "fetch_reg1_reg2\t%N,%N";
  case fetch_args_cv_op:
    return "fetch_constant_reg\t%l,%N";
  case fetch_args_vc_op:
    return "fetch_reg_constant\t%l,%N";
  case fetch_args_iv_op:
    return "fetch_integer_reg\t%d,%N";
  case fetch_args_vi_op:
    return "fetch_reg_integer\t%d,%N";
  case enter_profiling_op:
    return "enter_profiling\t\t%g";
  case retry_profiled_op:
    return "retry_profiled\t\t%g";
  case count_call_op:
    return "count_call_op\t\t%g";
  case count_retry_op:
    return "count_retry_op\t\t%g";
  case restore_tmps_op:
    return "restore_temps\t\t%l";
  case restore_tmps_and_skip_op:
    return "restore_temps_and_skip\t\t%l";
  case enter_lu_op:
    return "enter_lu";
  case empty_call_op:
    return "empty_call\t\t%l,%d";
#ifdef YAPOR
  case sync_op:
    return "sync";
#endif /* YAPOR */
#ifdef TABLING
  case table_new_answer_op:
    return "table_new_answer";
  case table_try_single_op:
    return "table_try_single\t%g\t%x";
#endif /* TABLING */
#ifdef TABLING_INNER_CUTS
  case "clause_with_cut":
    return clause_with_cut_op;
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
  case fetch_args_for_bccall_op:
    return "fetch_args_for_bccall\t%v";
  case bccall_op:
    return "binary_cfunc\t\t%v,%P";
  case blob_op:
    return "blob\t%O";
  case string_op:
    return "string\t%O";
  case label_ctl_op:
    return "label_control\t";
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
    }
  return NULL;
}

void
Yap_ShowCode (struct intermediates *cint)
{
  CACHE_REGS
  struct PSEUDO *cpc;

  cpc = cint->CodeStart;
  /* MkIntTerm and friends may build terms in the global stack */
  HR = (CELL *)cint->freep;
  while (cpc) {
    compiler_vm_op ic = cpc->op;
    if (ic != nop_op) {
      }
    ShowOp (getFormat(ic), cpc);
    cpc = cpc->nextInst;
  }
  Yap_DebugErrorPutc ('\n');
}

#endif /* DEBUG */

