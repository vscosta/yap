/*************************************************************************
*									 *
*	 Yap Prolog 							 *
*									 *
*	Yap Prolog Was Developed At Nccup - Universidade Do Porto	 *
*									 *
* Copyright L.Damas, V.S.Costa And Universidade Do Porto 1985-1997	 *
*									 *
**************************************************************************
*									 *
* File:		errors.c						 *
* Last Rev:								 *
* Mods:									 *
* Comments:	Yap'S error handlers					 *
*									 *
*************************************************************************/

#include "absmi.h"
#include "yapio.h"
#if HAVE_STDARG_H
#include <stdarg.h>
#endif
#include <stdlib.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_STRING_H
#include <string.h>
#endif
#include "Foreign.h"


#ifdef DEBUG
STATIC_PROTO (int hidden, (Atom));
STATIC_PROTO (int legal_env, (CELL *));
void STD_PROTO (DumpActiveGoals, (void));
STATIC_PROTO (void detect_bug_location, (char *, int));

#define ONHEAP(ptr) (CellPtr(ptr) >= CellPtr(HeapBase)  && CellPtr(ptr) < CellPtr(HeapTop))

#define ONLOCAL(ptr) (CellPtr(ptr) > CellPtr(H)  && CellPtr(ptr) < CellPtr(LocalBase))

static int
hidden (Atom at)
{
  AtomEntry *chain;
  
  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  while (!EndOfPAEntr (chain) && AbsAtom (chain) != at)
    chain = RepAtom(chain->NextOfAE);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  if (EndOfPAEntr (chain))
    return (FALSE);
  return (TRUE);
}

static int
legal_env (CELL *ep)
{
  CELL cp, ps;
  PredEntry *pe;
  if (!ONLOCAL (ep) || Unsigned (ep) & 3)
    return (FALSE);
  cp = ep[E_CP];
  if (!ONHEAP (cp))
    return (FALSE);
  ps = *((CELL *) (Addr (cp) - CellSize));
  pe = (PredEntry *) (ps - sizeof (OPREG) - sizeof (Prop));
  READ_LOCK(pe->PRWLock);
  if (!ONHEAP (pe) || Unsigned (pe) & 3 || pe->KindOfPE & 0xff00) {
    READ_UNLOCK(pe->PRWLock);
    return (FALSE);
  }
  READ_UNLOCK(pe->PRWLock);
  return (TRUE);
}

void
DumpActiveGoals (void)
{
  /* try to dump active goals */
  CELL *ep = YENV;		/* and current environment		  */
  choiceptr b_ptr = B;
  CELL cp;
  PredEntry *pe;
  int first = 1;
  if (legal_env (YENV) && YENV < ENV)
    ep = YENV;
  else if (legal_env (ENV))
    ep = ENV;
  while (TRUE)
    {
      if (!ONLOCAL (ep) || (Unsigned (ep) & (sizeof(CELL)-1)))
	break;
      cp = ep[E_CP];
      if (!ONHEAP (cp) || (Unsigned (cp) & (sizeof(CELL)-1)))
	break;
      pe = EnvPreg(cp);
      if (!ONHEAP (pe) || Unsigned (pe) & (sizeof(CELL)-1))
	break;
      READ_LOCK(pe->PRWLock);
      if (pe->KindOfPE & 0xff00) {
	READ_UNLOCK(pe->PRWLock);
	break;
      }
      if (pe->PredFlags & (CompiledPredFlag | DynamicPredFlag | FastPredFlag))
	{
	  Functor f;
	  SMALLUNSGN mod = 0;

	  f = pe->FunctorOfPred;
	  if (pe->KindOfPE && hidden (NameOfFunctor (f)))
	    goto next;
	  if (first++ == 1)
	    fprintf(stderr,"Active ancestors:\n");
	  if (pe->ModuleOfPred) mod = IntOfTerm(pe->ModuleOfPred);
	  plwrite (ModuleName[mod], DebugPutc, 0);
	  DebugPutc (c_output_stream,':');
	  if (pe->ArityOfPE == 0) {
	    plwrite (MkAtomTerm ((Atom)f), DebugPutc, 0);
	  } else {
	    plwrite (MkAtomTerm (NameOfFunctor (f)), DebugPutc, 0);
	    DebugPutc (c_output_stream,'/');
	    plwrite (MkIntTerm (ArityOfFunctor (f)), DebugPutc, 0);
	  }
	  DebugPutc (c_output_stream,'\n');
	}
    next:
      READ_UNLOCK(pe->PRWLock);
      ep = (CELL *) ep[E_E];
    }
  first = 1;
  fprintf(stderr,"Active Choice-Points:\n");
  while (TRUE)
    {
      PredEntry *pe;
      op_numbers opnum;
      
      if (!ONLOCAL (b_ptr) || b_ptr->cp_b == NULL)
	break;
      opnum = op_from_opcode(b_ptr->cp_ap->opc);
    restart_cp:
      switch(opnum) {
      case _or_else:
	if (b_ptr->cp_ap == (yamop *)(b_ptr->cp_ap->u.sla.l))
	  {
	    plwrite(MkAtomTerm(LookupAtom("repeat ")), DebugPutc, 0);
	  }
      case _or_last:
#ifdef YAPOR
	pe = PredFromOr(b_ptr->cp_cp->u.ldl.bl);
#else
	pe = b_ptr->cp_cp->u.sla.p0;
#endif /* YAPOR */
	break;
      case _retry_profiled:
	opnum = op_from_opcode(NEXTOP(b_ptr->cp_ap,l)->opc);
	goto restart_cp;
      default:
	pe = (PredEntry *)(b_ptr->cp_ap->u.ld.p);
      }
      READ_LOCK(pe->PRWLock);
      {
	Functor f;
	SMALLUNSGN mod = 0;

	f = pe->FunctorOfPred;
	if (pe->ModuleOfPred) mod = IntOfTerm(pe->ModuleOfPred);
	plwrite (ModuleName[mod], DebugPutc, 0);
	DebugPutc (c_output_stream,':');
	if (pe->ArityOfPE == 0) {
	  plwrite (MkAtomTerm (NameOfFunctor(f)), DebugPutc, 0);
	} else {
	  Int i = 0, arity = pe->ArityOfPE;
	  Term *args = &(b_ptr->cp_a1);
	  plwrite (MkAtomTerm (NameOfFunctor (f)), DebugPutc, 0);
	  DebugPutc (c_output_stream,'(');
	  for (i= 0; i < arity; i++) {
	    if (i > 0) DebugPutc (c_output_stream,',');
	    plwrite(args[i], DebugPutc, 4);
	  }
	  DebugPutc (c_output_stream,')');
	}
	DebugPutc (c_output_stream,'\n');
      }
      READ_UNLOCK(pe->PRWLock);
      b_ptr = b_ptr->cp_b;
    }
}
#endif /* DEBUG */

void
exit_yap (int value)
{
#if defined(YAPOR)
  unmap_memory();
#endif /* YAPOR || TABLING */
  if (! (PrologMode & BootMode) )
    ShutdownLoadForeign();
  exit(value);
}


static void
detect_bug_location(char *tp, int psize)
{
  Atom pred_name;
  Int pred_arity;
  SMALLUNSGN pred_module;
  Int cl;

  if ((cl = PredForCode((CODEADDR)P, &pred_name, &pred_arity, &pred_module))
      == 0) {
    /* system predicate */
#if   HAVE_SNPRINTF
    snprintf(tp, psize, "%s",
	     "meta-call");
#else
    sprintf(tp, "%s",
	    "meta-call");
#endif	
  } else if (pred_module == 0) {
    /* don't give info on system predicates */
#if   HAVE_SNPRINTF
#if SHORT_INTS
    snprintf(tp, psize, "%s:%s/%ld",
	     RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    snprintf(tp, psize, "%s:%s/%d",
	     RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#else
#if SHORT_INTS
    sprintf(tp, "in %s:%s/%ld",
	    RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    sprintf(tp, "in %s:%s/%d",
	    RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#endif
  } else if (cl < 0) {
#if   HAVE_SNPRINTF
#if SHORT_INTS
    snprintf(tp, psize, "indexing code of %s:%s/%ld",
	     RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    snprintf(tp, psize, "indexing code of %s:%s/%d",
	     RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#else
#if SHORT_INTS
    sprintf(tp, "indexing code of %s:%s/%ld",
	    RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#else 
    sprintf(tp, "indexing code of %s:%s/%d",
	    RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#endif
  } else {
#if   HAVE_SNPRINTF
#if SHORT_INTS
    snprintf(tp, psize, "clause %ld of %s:%s/%ld", cl,
	     RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    snprintf(tp, psize, "clause %d of %s:%s/%d", cl,
	     RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	     RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#else
#if SHORT_INTS
    sprintf(tp, "clause %ld of %s:%s/%ld", cl,
	    RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#else
    sprintf(tp, "clause %d of %s:%s/%d", cl,
	    RepAtom(AtomOfTerm(ModuleName[pred_module]))->StrOfAE,
	    RepAtom(pred_name)->StrOfAE, pred_arity);
#endif
#endif
  }
}

#ifdef DEBUG

#include <stdio.h>

void
bug_location(yamop *pc)
{
  yamop *oldp = pc;
  P = pc;
  detect_bug_location((char *)H, 256);
  P = oldp;
  fprintf(stderr,"%s\n",(char *)H);
}
#endif

/* This needs to be a static because I can't trust the stack (WIN32), and
   I can't trust the Yap stacks  (error) */
#define YAP_BUF_SIZE 512

static char tmpbuf[YAP_BUF_SIZE];

yamop *
Error (yap_error_number type, Term where, char *format,...)
{
  va_list ap;
  CELL nt[2];
  Functor fun;
  int serious;
  char *tp = tmpbuf;
  int psize = YAP_BUF_SIZE;
 
  /* disallow recursive error handling */ 
  if (PrologMode & InErrorMode) {
    /* error within error */
    va_start (ap, format);
    /* now build the error string */
    if (format != NULL) {
#if   HAVE_VSNPRINTF
      (void) vsnprintf(tmpbuf, 512, format, ap);
#else
      (void) vsprintf(tmpbuf, format, ap);
#endif
    } else {
      tmpbuf[0] = '\0';
    }
    va_end (ap);
    fprintf(stderr,"[ ERROR WITHIN ERROR: %s ]\n", tmpbuf);
    exit(1);
  }
  /* must do this here */
  if (type == FATAL_ERROR) {
    va_start (ap, format);
    /* now build the error string */
    if (format != NULL) {
#if   HAVE_VSNPRINTF
      (void) vsnprintf(tmpbuf, YAP_BUF_SIZE, format, ap);
#else
      (void) vsprintf(tmpbuf, format, ap);
#endif
    }  else {
      tmpbuf[0] = '\0';
    }
    va_end (ap);
    fprintf(stderr,"[ Fatal YAP Error: %s exiting.... ]\n",tmpbuf);
    exit_yap (1);
  }
  if (P == (yamop *)(FAILCODE))
   return(P);
  /* PURE_ABORT may not have set where correctly, BootMode may not have the data terms ready */
  if (type == PURE_ABORT || PrologMode & BootMode) {
    where = TermNil;
    PrologMode &= ~AbortMode;
    PrologMode |= InErrorMode;
  } else {
    if (type != SYNTAX_ERROR)
      where = CopyTerm(Deref(where));
    if (IsVarTerm(where)) {
      /* we must be careful someone gave us a copy to a local variable */
      Term t = MkVarTerm();
      unify(t, where);
      where = Deref(where);
    }
    /* Exit Abort Mode, if we were there */
    PrologMode &= ~AbortMode;
    PrologMode |= InErrorMode;
    where = CopyTerm(where);
  }
  va_start (ap, format);
  /* now build the error string */
  if (format != NULL)
    {
#if   HAVE_VSNPRINTF
      (void) vsnprintf(tmpbuf, YAP_BUF_SIZE, format, ap);
#else
      (void) vsprintf(tmpbuf, format, ap);
#endif
    }
  else
    tmpbuf[0] = '\0';
  va_end (ap);
  if (PrologMode & BootMode) {
    /* crash in flames! */
    fprintf(stderr,"[ Fatal Error: %s exiting.... ]\n",tmpbuf);
    exit_yap (1);
  }
#ifdef DEBUGX
  DumpActiveGoals();
#endif /* DEBUG */
  switch (type) {
  case INTERNAL_ERROR:
    {
      fprintf(stderr,"[ Internal YAP Error: %s exiting.... ]\n",tmpbuf);
      serious = TRUE;
      detect_bug_location(tmpbuf, YAP_BUF_SIZE);
      fprintf(stderr,"[ Bug found while executing %s ]\n",tmpbuf);
      exit_yap (1);
    }
  case FATAL_ERROR:
    {
      fprintf(stderr,"[ Fatal YAP Error: %s exiting.... ]\n",tmpbuf);
      exit_yap (1);
    }
  case PURE_ABORT:
    nt[0] = MkAtomTerm(LookupAtom(tmpbuf));
    fun = MkFunctor(LookupAtom("abort"),2);
    serious = TRUE;
    break;
  case DOMAIN_ERROR_ARRAY_OVERFLOW:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("array_overflow"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_ARRAY_TYPE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("array_type"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_IO_MODE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("io_mode"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_MUTABLE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("mutable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NON_EMPTY_LIST:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("non_empty_list"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_LESS_THAN_ZERO:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("not_less_than_zero"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_NL:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("not_newline"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_NOT_ZERO:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("not_zero"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OUT_OF_RANGE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("out_of_range"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OPERATOR_PRIORITY:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("operator_priority"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_OPERATOR_SPECIFIER:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("operator_specifier"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_RADIX:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("radix"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SHIFT_COUNT_OVERFLOW:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("shift_count_overflow"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SOURCE_SINK:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("source_sink"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("stream"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_OR_ALIAS:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("stream_or_alias"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_STREAM_POSITION:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("stream_position"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_SYNTAX_ERROR_HANDLER:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("syntax_error_handler"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case DOMAIN_ERROR_TIMEOUT_SPEC:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("time_out_spec"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_SOURCE_SINK:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("source_sink"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("existence_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_ARRAY:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("array"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("existence_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EXISTENCE_ERROR_STREAM:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("stream"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("existence_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_FLOAT_OVERFLOW:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("float_overflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_INT_OVERFLOW:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("int_overflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_UNDEFINED:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("undefined"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_FLOAT_UNDERFLOW:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("float_underflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case EVALUATION_ERROR_UNDERFLOW:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("underflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break; 
 case EVALUATION_ERROR_ZERO_DIVISOR:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("zero_divisor"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case INSTANTIATION_ERROR:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(LookupAtom("instantiation_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_ACCESS_PRIVATE_PROCEDURE:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("access"));
      ti[1] = MkAtomTerm(LookupAtom("private_procedure"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_CREATE_ARRAY:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("create"));
      ti[1] = MkAtomTerm(LookupAtom("array"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_CREATE_OPERATOR:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("create"));
      ti[1] = MkAtomTerm(LookupAtom("operator"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_BINARY_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("binary_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_PAST_END_OF_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("past_end_of_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_INPUT_TEXT_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("text_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_MODIFY_STATIC_PROCEDURE:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("modify"));
      ti[1] = MkAtomTerm(LookupAtom("static_procedure"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_NEW_ALIAS_FOR_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("new"));
      ti[1] = MkAtomTerm(LookupAtom("alias"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OPEN_SOURCE_SINK:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("open"));
      ti[1] = MkAtomTerm(LookupAtom("source_sink"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_BINARY_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("output"));
      ti[1] = MkAtomTerm(LookupAtom("binary_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("output"));
      ti[1] = MkAtomTerm(LookupAtom("stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_OUTPUT_TEXT_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("output"));
      ti[1] = MkAtomTerm(LookupAtom("text_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_REPOSITION_STREAM:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("reposition"));
      ti[1] = MkAtomTerm(LookupAtom("stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case PERMISSION_ERROR_RESIZE_ARRAY:
    {
      int i;
      Term ti[3];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("resize"));
      ti[1] = MkAtomTerm(LookupAtom("array"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_CHARACTER:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("character"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("representation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_CHARACTER_CODE:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("character_code"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("representation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case REPRESENTATION_ERROR_MAX_ARITY:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("max_arity"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("representation_error"),1), 1, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case SYNTAX_ERROR:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      nt[0] = where;
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case SYSTEM_ERROR:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(LookupAtom("system_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ARRAY:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("array"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ATOM:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("atom"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_ATOMIC:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("atomic"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_BYTE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("byte"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CALLABLE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("callable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_CHARACTER:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("character"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_COMPOUND:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("compound"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_DBREF:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("db_reference"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_DBTERM:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("db_term"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_EVALUABLE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("evaluable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_FLOAT:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("float"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_INTEGER:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("integer"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_KEY:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("key"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_LIST:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("list"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_NUMBER:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("number"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_PREDICATE_INDICATOR:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("predicate_indicator"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_PTR:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("pointer"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_UBYTE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("unsigned_byte"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case TYPE_ERROR_VARIABLE:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      ti[0] = MkAtomTerm(LookupAtom("variable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  default:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(tmpbuf, " in ", psize);
#else
      strcat(tmpbuf, " in ");
#endif
      i = strlen(tmpbuf);
      nt[0] = MkAtomTerm(LookupAtom("system_error"));
      tp = tmpbuf+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
  }
  if (type != PURE_ABORT) {
    /* This is used by some complex procedures to detect there was an error */
    ErrorMessage = RepAtom(AtomOfTerm(nt[0]))->StrOfAE;
    detect_bug_location(tp, psize);
  }
  nt[1] = MkAtomTerm(LookupAtom(tmpbuf));
  if (serious) {
    if (type == PURE_ABORT)
      JumpToEnv(MkAtomTerm(LookupAtom("abort")));
    else
      JumpToEnv(MkApplTerm(fun, 2, nt));
    P = (yamop *)FAILCODE;
  }
  PrologMode &= ~InErrorMode;
  return(P);
}

