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

void
Yap_emit_5ops (compiler_vm_op o, CELL r1, CELL r2, CELL r3, CELL r4, CELL r5, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p)+3*sizeof(CELL), cip);
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->rnd3 = r3;
  p->rnd4 = r4;
  p->rnd5 = r5;
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
Yap_emit_6ops (compiler_vm_op o, CELL r1, CELL r2, CELL r3, CELL r4, CELL r5, CELL r6, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p)+4*sizeof(CELL), cip);
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->rnd3 = r3;
  p->rnd4 = r4; 
  p->rnd5 = r5;
  p->rnd6 = r6;
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
Yap_emit_7ops (compiler_vm_op o, CELL r1, CELL r2, CELL r3, CELL r4, CELL r5, CELL r6, CELL r7, struct intermediates *cip)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p)+5*sizeof(CELL), cip);
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->rnd3 = r3;
  p->rnd4 = r4; 
  p->rnd5 = r5;
  p->rnd6 = r6;
  p->rnd7 = r7;
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
    //Yap_DebugErrorPutc('0');
    //Yap_DebugErrorPutc('x');
    while (*p != '\0') {
      Yap_DebugErrorPutc(*p++);
    }
  }
}

static void
write_special_label(special_label_op arg, special_label_id rn, UInt lab)
{
  switch (arg) {
  case SPECIAL_LABEL_INIT:
    Yap_DebugErrorPuts("init,");
    switch (rn) {
    case SPECIAL_LABEL_EXCEPTION:
      Yap_DebugErrorPuts("exception,");
      break;
    case SPECIAL_LABEL_SUCCESS:
       Yap_DebugErrorPuts("success,");
      break;
    case SPECIAL_LABEL_FAILURE:
      Yap_DebugErrorPuts("fail,");
      break;
    }
    write_address(lab);
  case SPECIAL_LABEL_SET:
    Yap_DebugErrorPuts("set,");
    break;
  case SPECIAL_LABEL_CLEAR:
    Yap_DebugErrorPuts("clear,");
    switch (rn) {
    case SPECIAL_LABEL_EXCEPTION:
      Yap_DebugErrorPuts("exception");
      break;
    case SPECIAL_LABEL_SUCCESS:
       Yap_DebugErrorPuts("success");
      break;
    case SPECIAL_LABEL_FAILURE:
      Yap_DebugErrorPuts("fail");
      break;
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

char *opDesc[] = { mklist(f_arr) };

static void send_pred(PredEntry *p)
{
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


static void
ShowOp (compiler_vm_op ic, const char *f, struct PSEUDO *cpc)
{
  CACHE_REGS
  char ch;
  Int arg = cpc->rnd1;
  Int rn = cpc->rnd2;
  CELL *cptr = cpc->arnds;

  if (ic != label_op && ic != label_ctl_op && ic != name_op) {
    Yap_DebugErrorPutc ('\t');
  }
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
	  case '2':
	    {
	      Ventry *v = (Ventry *) cpc->rnd3;
	      Yap_DebugErrorPutc (v->KindOfVE == PermVar ? 'Y' : 'X');
	      Yap_DebugPlWrite (MkIntTerm ((v->NoOfVE) & MaskVarAdrs));
	      Yap_DebugErrorPutc (',');
	      Yap_DebugErrorPutc ('A');
	      Yap_DebugPlWrite (MkIntegerTerm (cpc->rnd4));
	      Yap_DebugErrorPutc (',');
	      send_pred( RepPredProp((Prop)(cpc->rnd5)) );
	    }
	    break;

	  case 'a':
	  case 'n':
	  case 'S':
	    Yap_DebugPlWrite ((Term) arg);
	    break;
	  case 'b':
	    /* write a variable bitmap for a call */
	    {
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
	  case 'L':
	    write_special_label (arg, rn, cpc->rnd3);
	    break;
	  case 'B':
	    {
	      char s[32];

	      bip_name(rn,s);
	      Yap_DebugPlWrite (MkAtomTerm(Yap_LookupAtom(s)));
	    }
	    break;
	  case 'd':
	    Yap_DebugPlWrite (MkIntegerTerm (arg));
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
	   send_pred( RepPredProp((Prop)(arg) ));
           break;
         case 'P':
	   send_pred( RepPredProp((Prop)(rn) ));
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
      ShowOp (ic, opDesc[ic], cpc);
    }
    cpc = cpc->nextInst;
  }
  Yap_DebugErrorPutc ('\n');
}

#endif /* DEBUG */

