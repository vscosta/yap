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
* Last rev:								 *
* mods:									 *
* comments:	some useful routines for YAP's compiler			 *
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
#include "Heap.h"
#include "compile.h"
#include "yapio.h"
#if HAVE_STRING_H
#include <string.h>
#endif

#ifdef DEBUG
STATIC_PROTO (void ShowOp, (char *));
#endif /* DEBUG */

/*
 * The compiler creates an instruction chain which will be assembled after
 * afterwards 
 */

#ifdef DEBUG
static Int arg, rn;

static compiler_vm_op ic;

static CELL *cptr;

char            _YAP_Option[20];

YP_FILE *_YAP_logfile;
#endif

static char *
AllocCMem (int size)
{
  char *p;
  p = freep;
#if SIZEOF_INT_P==8
  size = (size + 7) & 0xfffffffffffffff8L;
#else
  size = (size + 3) & 0xfffffffcL;
#endif
  freep += size;
  if (ASP <= CellPtr (freep) + 256) {
    save_machine_regs();
    longjmp(_YAP_CompilerBotch,3);
  }
  return (p);
}

char *
_YAP_AllocCMem (int size)
{
  return(AllocCMem(size));
}

int
_YAP_is_a_test_pred (Term arg, SMALLUNSGN mod)
{
  if (IsVarTerm (arg))
    return (FALSE);
  else if (IsAtomTerm (arg))
    {
      Atom At = AtomOfTerm (arg);
      PredEntry *pe = RepPredProp(PredPropByAtom(At, mod));
      if (EndOfPAEntr(pe))
	return (FALSE);
      return (pe->PredFlags & TestPredFlag);
    }
  else if (IsApplTerm (arg))
    {
      Functor f = FunctorOfTerm (arg);
      PredEntry *pe = RepPredProp(PredPropByFunc(f, mod));
      if (EndOfPAEntr(pe))
	return (FALSE);
      return (pe->PredFlags & TestPredFlag);
    }
  else
    return (FALSE);
}

void
_YAP_emit (compiler_vm_op o, Int r1, CELL r2)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p));
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->nextInst = NIL;
  if (cpc == NIL)
    cpc = CodeStart = p;
  else
    {
      cpc->nextInst = p;
      cpc = p;
    }
}

void
_YAP_emit_3ops (compiler_vm_op o, CELL r1, CELL r2, CELL r3)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p)+sizeof(CELL));
  p->op = o;
  p->rnd1 = r1;
  p->rnd2 = r2;
  p->rnd3 = r3;
  p->nextInst = NIL;
  if (cpc == NIL)
    cpc = CodeStart = p;
  else
    {
      cpc->nextInst = p;
      cpc = p;
    }
}

CELL *
_YAP_emit_extra_size (compiler_vm_op o, CELL r1, int size)
{
  PInstr *p;
  p = (PInstr *) AllocCMem (sizeof (*p) + size - CellSize);
  p->op = o;
  p->rnd1 = r1;
  p->nextInst = NIL;
  if (cpc == NIL)
    cpc = CodeStart = p;
  else
    {
      cpc->nextInst = p;
      cpc = p;
    }
  return (p->arnds);
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
_YAP_bip_name(Int op, char *s) {
  bip_name(op,s);
}

#ifdef DEBUG

static void
ShowOp (f)
     char *f;
{
  char ch;
  while ((ch = *f++) != 0)
    {
      if (ch == '%')
	switch (ch = *f++)
	  {
	  case 'a':
	  case 'n':
	    _YAP_plwrite ((Term) arg, _YAP_DebugPutc, 0);
	    break;
	  case 'b':
	    /* write a variable bitmap for a call */
	    {
	      int max = arg/(8*sizeof(CELL)), i;
	      CELL *ptr = cptr;
	      for (i = 0; i <= max; i++) {
		_YAP_plwrite(MkIntegerTerm((Int)(*ptr++)), _YAP_DebugPutc, 0);
	      }
	    }
	    break;		
	  case 'l':
	    _YAP_plwrite (MkIntTerm (arg), _YAP_DebugPutc, 0);
	    break;
	  case 'B':
	    {
	      char s[32];

	      bip_name(rn,s);
	      _YAP_plwrite (MkAtomTerm(_YAP_LookupAtom(s)), _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'd':
	    _YAP_plwrite (MkIntTerm (rn), _YAP_DebugPutc, 0);
	    break;
	  case 'z':
	    _YAP_plwrite (MkIntTerm (cpc->rnd3), _YAP_DebugPutc, 0);
	    break;
	  case 'v':
	    {
	      Ventry *v = (Ventry *) arg;
	      _YAP_DebugPutc (_YAP_c_error_stream,v->KindOfVE == PermVar ? 'Y' : 'X');
	      _YAP_plwrite (MkIntTerm ((v->NoOfVE) & MaskVarAdrs), _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'N':
	    {
	      Ventry *v;

	      cpc = cpc->nextInst;
	      arg = cpc->rnd1;
	      v = (Ventry *) arg;
	      _YAP_DebugPutc (_YAP_c_error_stream,v->KindOfVE == PermVar ? 'Y' : 'X');
	      _YAP_plwrite (MkIntTerm ((v->NoOfVE) & MaskVarAdrs), _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'm':
	    _YAP_plwrite (MkAtomTerm ((Atom) arg), _YAP_DebugPutc, 0);
	    _YAP_DebugPutc (_YAP_c_error_stream,'/');
	    _YAP_plwrite (MkIntTerm (rn), _YAP_DebugPutc, 0);
	    break;
	  case 'p':
	    {
	      PredEntry *p = RepPredProp ((Prop) arg);
	      Functor f = p->FunctorOfPred;
	      UInt arity = p->ArityOfPE;
	      SMALLUNSGN mod = 0;

	      if (p->ModuleOfPred) mod = IntOfTerm(p->ModuleOfPred);
	      _YAP_plwrite (ModuleName[mod], _YAP_DebugPutc, 0);
	      _YAP_DebugPutc (_YAP_c_error_stream,':');
	      if (arity == 0)
		_YAP_plwrite (MkAtomTerm ((Atom)f), _YAP_DebugPutc, 0);
	      else
		_YAP_plwrite (MkAtomTerm (NameOfFunctor (f)), _YAP_DebugPutc, 0);
	      _YAP_DebugPutc (_YAP_c_error_stream,'/');
	      _YAP_plwrite (MkIntTerm (arity), _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'P':
	    {
	      PredEntry *p = RepPredProp((Prop) rn);
	      Functor f = p->FunctorOfPred;
	      UInt arity = p->ArityOfPE;
	      SMALLUNSGN mod = 0;

	      if (p->ModuleOfPred) mod = IntOfTerm(p->ModuleOfPred);
	      _YAP_plwrite (ModuleName[mod], _YAP_DebugPutc, 0);
	      _YAP_DebugPutc (_YAP_c_error_stream,':');
	      if (arity == 0)
		_YAP_plwrite (MkAtomTerm ((Atom)f), _YAP_DebugPutc, 0);
	      else
		_YAP_plwrite (MkAtomTerm (NameOfFunctor (f)), _YAP_DebugPutc, 0);
	      _YAP_DebugPutc (_YAP_c_error_stream,'/');
	      _YAP_plwrite (MkIntTerm (arity), _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'f':
	    if (IsExtensionFunctor((Functor)arg)) {
	      if ((Functor)arg == FunctorDBRef) {
		_YAP_plwrite(MkAtomTerm(_YAP_LookupAtom("DBRef")), _YAP_DebugPutc, 0);
	      } else if ((Functor)arg == FunctorLongInt) {
		_YAP_plwrite(MkAtomTerm(_YAP_LookupAtom("LongInt")), _YAP_DebugPutc, 0);
	      } else if ((Functor)arg == FunctorDouble) {
		_YAP_plwrite(MkAtomTerm(_YAP_LookupAtom("Double")), _YAP_DebugPutc, 0);
	      }
	    } else {
	      _YAP_plwrite(MkAtomTerm(NameOfFunctor ((Functor) arg)), _YAP_DebugPutc, 0);
	      _YAP_DebugPutc (_YAP_c_error_stream,'/');
	      _YAP_plwrite(MkIntTerm(ArityOfFunctor ((Functor) arg)), _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'r':
	    _YAP_DebugPutc (_YAP_c_error_stream,'A');
	    _YAP_plwrite (MkIntTerm (rn), _YAP_DebugPutc, 0);
	    break;
	  case 'h':
	    {
	      CELL my_arg = *cptr++;
	      if (my_arg & 1)
		_YAP_plwrite (MkIntTerm (my_arg),
			 _YAP_DebugPutc, 0);
	      else if (my_arg == (CELL) FAILCODE)
		_YAP_plwrite (MkAtomTerm (AtomFail), _YAP_DebugPutc, 0);
	      else
		_YAP_plwrite (MkIntegerTerm ((Int) my_arg),
			 _YAP_DebugPutc, 0);
	    }
	    break;
	  case 'g':
	    if (arg & 1)
	      _YAP_plwrite (MkIntTerm (arg),
		       _YAP_DebugPutc, 0);
	    else if (arg == (CELL) FAILCODE)
	      _YAP_plwrite (MkAtomTerm (AtomFail), _YAP_DebugPutc, 0);
	    else
	      _YAP_plwrite (MkIntegerTerm ((Int) arg), _YAP_DebugPutc, 0);
	    break;
	  case 'i':
	    _YAP_plwrite (MkIntTerm (arg), _YAP_DebugPutc, 0);
	    break;
	  case 'j':
	    {
	      Functor fun = (Functor)*cptr++;
	      if (IsExtensionFunctor(fun)) {
		if (fun == FunctorDBRef) {
		  _YAP_plwrite(MkAtomTerm(_YAP_LookupAtom("DBRef")), _YAP_DebugPutc, 0);
		} else if (fun == FunctorLongInt) {
		  _YAP_plwrite(MkAtomTerm(_YAP_LookupAtom("LongInt")), _YAP_DebugPutc, 0);
		} else if (fun == FunctorDouble) {
		  _YAP_plwrite(MkAtomTerm(_YAP_LookupAtom("Double")), _YAP_DebugPutc, 0);
		}
	      } else {
		_YAP_plwrite (MkAtomTerm(NameOfFunctor(fun)), _YAP_DebugPutc, 0);
		_YAP_DebugPutc (_YAP_c_error_stream,'/');
		_YAP_plwrite (MkIntTerm(ArityOfFunctor(fun)), _YAP_DebugPutc, 0);
	      }
	    }
	    break;
	  case 'O':
	    _YAP_plwrite(AbsAppl(cptr), _YAP_DebugPutc, 0);
	    break;
	  case 'x':
	    _YAP_plwrite (MkIntTerm (rn >> 1), _YAP_DebugPutc, 0);
	    _YAP_DebugPutc (_YAP_c_error_stream,'\t');
	    _YAP_plwrite (MkIntTerm (rn & 1), _YAP_DebugPutc, 0);
	    break;
	  case 'o':
	    _YAP_plwrite ((Term) * cptr++, _YAP_DebugPutc, 0);
	  case 'c':
	    {
	      int i;
	      for (i = 0; i < arg; ++i)
		{
		  CELL my_arg;
		  if (*cptr)
		    {
		      _YAP_plwrite ((Term) * cptr++, _YAP_DebugPutc, 0);
		    }
		  else
		    {
		      _YAP_plwrite (MkIntTerm (0), _YAP_DebugPutc, 0);
		      cptr++;
		    }
		  _YAP_DebugPutc (_YAP_c_error_stream,'\t');
		  my_arg = *cptr++;
		  if (my_arg & 1)
		    _YAP_plwrite (MkIntTerm (my_arg),
			     _YAP_DebugPutc, 0);
		  else if (my_arg == (CELL) FAILCODE)
		    _YAP_plwrite (MkAtomTerm (AtomFail), _YAP_DebugPutc, 0);
		  else
		    _YAP_plwrite (MkIntegerTerm ((Int) my_arg), _YAP_DebugPutc, 0);
		  _YAP_DebugPutc (_YAP_c_error_stream,'\n');
		}
	    }
	    break;
	  case 'e':
	    {
	      int i;
	      for (i = 0; i < arg; ++i)
		{
		  CELL my_arg;
		  if (*cptr)
		    {
		      _YAP_plwrite (MkAtomTerm (NameOfFunctor ((Functor) * cptr)), _YAP_DebugPutc, 0);
		      _YAP_DebugPutc (_YAP_c_error_stream,'/');
		      _YAP_plwrite (MkIntTerm (ArityOfFunctor ((Functor) * cptr++)), _YAP_DebugPutc, 0);
		    }
		  else
		    {
		      _YAP_plwrite (MkIntTerm (0), _YAP_DebugPutc, 0);
		      cptr++;
		    }
		  _YAP_DebugPutc (_YAP_c_error_stream,'\t');
		  my_arg = *cptr++;
		  if (my_arg & 1)
		    _YAP_plwrite (MkIntTerm (my_arg),
			     _YAP_DebugPutc, 0);
		  else if (my_arg == (CELL) FAILCODE)
		    _YAP_plwrite (MkAtomTerm (AtomFail), _YAP_DebugPutc, 0);
		  else
		    _YAP_plwrite (MkIntegerTerm ((Int) my_arg), _YAP_DebugPutc, 0);
		  _YAP_DebugPutc (_YAP_c_error_stream,'\n');
		}
	    }
	    break;
	  default:
	    _YAP_DebugPutc (_YAP_c_error_stream,'%');
	    _YAP_DebugPutc (_YAP_c_error_stream,ch);
	  }
      else
	_YAP_DebugPutc (_YAP_c_error_stream,ch);
    }
  _YAP_DebugPutc (_YAP_c_error_stream,'\n');
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
  "get_float\t\t%l,%r",
  "put_float\t\t%l,%r",
  "get_longint\t\t%l,%r",
  "put_longint\t\t%l,%r",
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
  "unify_float\t%l",
  "write_float\t%l",
  "unify_longint\t%l",
  "write_longint\t%l",
  "unify_bigint\t%l",
  "write_bigint\t%l",
  "unify_list",
  "write_list",
  "unify_struct\t%f",
  "write_struct\t%f",
  "write_unsafe\t%v",
  "fail",
  "cut",
  "cutexit",
  "allocate",
  "deallocate",
  "try_me_else\t\t%l\t%x",
  "jump\t\t%l",
  "procceed",
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
  "comit_by\t\t%v",
  "patch_by\t\t%v",
  "try\t\t%g\t%x",
  "retry\t\t%g\t%x",
  "trust\t\t%g\t%x",
  "try_in\t\t%g\t%x",
  "retry_in\t\t%g\t%x",
  "trust_in\t\t%g\t%x",
  "try_first\t\t%g\t%x",
  "retry_first\t\t%g\t%x",
  "trust_first\t\t%g\t%x",
  "try_first in\t\t%g\t%x",
  "retry_first in\t\t%g\t%x",
  "trust_first in\t\t%g\t%x",
  "try_tail\t\t%g\t%x",
  "retry_tail\t\t%g\t%x",
  "trust_tail\t\t%g\t%x",
  "try_tail_in\t\t%g\t%x",
  "retry_tail_in\t\t%g\t%x",
  "trust_tail_in\t\t%g\t%x",
  "try_head\t\t%g\t%x",
  "retry_head\t\t%g\t%x",
  "trust_head\t\t%g\t%x",
  "try_head_in\t\t%g\t%x",
  "retry_head_in\t\t%g\t%x",
  "trust_head_in\t\t%g\t%x",
  "try_last_first\t\t%g\t%x",
  "try_last_head\t\t%g\t%x",
  "jump_if_var\t\t%g",
  "switch_on_type\t%h\t%h\t%h\t%h",
  "switch_on_type_if_nonvar\t%h\t%h\t%h",
  "switch_on_type_of_last\t%h\t%h\t%h",
  "switch_on_type_of_head\t%h\t%h\t%h\t%h",
  "switch_on_list_or_nil\t%h\t%h\t%h\t%h",
  "switch_if_list_or_nil\t%h\t%h\t%h",
  "switch_on_last_list_or_nil\t%h\t%h\t%h",
  "switch_on_constant\t%i\n%c",
  "if_a_constant\t%i\t%h\n%c",
  "go_if_ equals_constant\t%o\t%h\t%h",
  "switch_on_functor\t%i\n%e",
  "if_a_functor\t%i\t%h\n%e",
  "go_if_equals_functor\t%j\t%h\t%h",
  "if_not_then\t%i\t%h\t%h\t%h",
  "save_pair\t%v",
  "save_appl\t%v",
  "fail_label\t%l",
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
  "unify_last_float\t%l",
  "unify_last_longint\t%l",
  "unify_last_bigint\t%l",
  "pvar_bitmap\t%l,%b",
  "pvar_live_regs\t%l,%b",
  "fetch_reg1_reg2\t%N,%N",
  "fetch_constant_reg\t%l,%N",
  "fetch_reg_constant\t%l,%N",
  "function_to_var\t%v,%B",
  "function_to_al\t%v,%B",
  "enter_profiling\t\t%g",
  "retry_profiled\t\t%g",
  "count_call_op\t\t%g",
  "count_retry_op\t\t%g",
  "restore_temps\t\t%l",
  "restore_temps_and_skip\t\t%l",
  "empty_call\t\t%l,%d",
#ifdef TABLING
  "table_new_answer",
#endif /* TABLING */
#ifdef YAPOR
  "sync",
#endif /* YAPOR */
  "fetch_args_for_bccall\t%v",
  "binary_cfunc\t\t%v,%P",
  "blob\t%O"
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
_YAP_ShowCode ()
{
  CELL *OldH = H;

  cpc = CodeStart;
  /* MkIntTerm and friends may build terms in the global stack */
  H = (CELL *)freep;
  while (cpc)
    {
      ic = cpc->op;
      arg = cpc->rnd1;
      rn = cpc->rnd2;
      cptr = cpc->arnds;
      if (ic != nop_op)
	ShowOp (opformat[ic]);
      cpc = cpc->nextInst;
    }
  _YAP_DebugPutc (_YAP_c_error_stream,'\n');
  H = OldH;
}

#endif /* DEBUG */

