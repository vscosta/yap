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

	  f = pe->FunctorOfPred;
	  if (pe->KindOfPE == 0 && hidden ((Atom)f))
	    goto next;
	  if (pe->KindOfPE && hidden (NameOfFunctor (f)))
	    goto next;
	  if (first++ == 1)
	    YP_fprintf(YP_stderr,"Active ancestors:\n");
	  plwrite (ModuleName[pe->ModuleOfPred], DebugPutc, 0);
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
  YP_fprintf(YP_stderr,"Active Choice-Points:\n");
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
	pe = PredFromOr(b_ptr->cp_cp->u.sla.l2);
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

	f = pe->FunctorOfPred;
	plwrite (ModuleName[pe->ModuleOfPred], DebugPutc, 0);
	DebugPutc (c_output_stream,':');
	if (pe->ArityOfPE == 0) {
	  plwrite (MkAtomTerm ((Atom)f), DebugPutc, 0);
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
exit_yap (int value, char *msg)
{
  if (msg != NIL) {
    if (value == 0) 
      /* no error */
      YP_fprintf(YP_stderr, msg);
    else
      /* error */
      YP_fprintf(YP_stderr, "[ YAP unrecoverable error: %s ]\n", msg);
  }
#if defined(YAPOR)
  unmap_memory();
#endif /* YAPOR || TABLING */
  exit(value);
}

void
Abort (char *format,...)
{
  va_list ap;
  va_start (ap, format);

  if (format)
    {
      char ch;
      while ((ch = *format++)!=0)
	if (ch != '%')
	  YP_putc (ch, YP_stderr);
	else
	  {
	    switch (*format++)
	      {
	      case 'c':
		YP_putc (va_arg (ap, int), YP_stderr);
		break;
	      case 's':
		YP_fprintf(YP_stderr, "%s", va_arg (ap, char *));
		break;
	      case 'd':
#if SHORT_INTS
		YP_fprintf(YP_stderr, "%ld", va_arg (ap, Int));
#else
		YP_fprintf(YP_stderr, "%d", va_arg (ap, Int));
#endif
		break;
	      case 'x':
#if SHORT_INTS
		YP_fprintf(YP_stderr, "%lx", va_arg (ap, Int));
#else
		YP_fprintf(YP_stderr, "%x", va_arg (ap, Int));
#endif
		break;
	      }
	  }
      YP_putc ('\n', YP_stderr);
    }
  va_end (ap);
#ifdef DEBUGX
  DumpActiveGoals();
#endif /* DEBUG */
  if (PrologMode & BootMode)
    {
      exit_yap (1, NIL);
    }
  else
    {
      CreepFlag = CalculateStackGap();
#if PUSH_REGS
      restore_absmi_regs(&standard_regs);
#endif
#if defined(__GNUC__) && defined(hppa)
      /* siglongjmp resets the TR hardware register */
      save_TR();
#endif
      siglongjmp (RestartEnv, 1);
    }
}


static void detect_bug_location(char *tp, int psize)
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

void 
ShutdownYAP(int value)
{
  ShutdownLoadForeign();
  exit(value);
}

static int in_error = FALSE;

yamop *
Error (yap_error_number type, Term where, char *format,...)
{
  va_list ap;
  char p[512];
  CELL nt[2];
  Functor fun;
  int serious;
  char *tp = p;
  int psize = 512;
  
  if (in_error)
    return(P);
  va_start (ap, format);
  /* now build the error string */
  if (format != NULL)
    {
#if   HAVE_VSNPRINTF
      (void) vsnprintf(p, 512, format, ap);
#else
      (void) vsprintf(p, format, ap);
#endif
    }
  else
    p[0] = '\0';
  va_end (ap);
  if (PrologMode & BootMode) {
    /* crash in flames! */
    exit_yap (1, p);
  }
#ifdef DEBUGX
  DumpActiveGoals();
#endif /* DEBUG */
  switch (type) {
  case INTERNAL_ERROR:
    {
      YP_fprintf(YP_stderr,"[ Internal YAP Error: %s exiting.... ]\n",p);
      serious = TRUE;
      detect_bug_location(tp, psize);
      YP_fprintf(YP_stderr,"[ Bug found while executing %s ]\n",tp);
      ShutdownYAP (1);
    }
  case FATAL_ERROR:
    {
      YP_fprintf(YP_stderr,"[ Fatal YAP Error: %s exiting.... ]\n",p);
      ShutdownYAP (1);
    }
  case PURE_ABORT:
    nt[0] = MkAtomTerm(LookupAtom(p));
    fun = MkFunctor(LookupAtom("abort"),2);
    serious = TRUE;
    break;
  case DOMAIN_ERROR_ARRAY_OVERFLOW:
    {
      int i;
      Term ti[2];

#if   HAVE_STRNCAT
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("array_overflow"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("array_type"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("io_mode"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("mutable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("non_empty_list"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("not_less_than_zero"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("not_newline"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("not_zero"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("out_of_range"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("operator_priority"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("operator_specifier"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("radix"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("shift_count_overflow"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("source_sink"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("stream"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("stream_or_alias"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("stream_position"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("syntax_error_handler"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("time_out_spec"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("domain_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("source_sink"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("existence_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("array"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("existence_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("stream"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("existence_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("float_overflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("int_overflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("undefined"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("float_underflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("underflow"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("zero_divisor"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("evaluation_error"),1), 1, ti);
      tp = p+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case INSTANTIATION_ERROR:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      nt[0] = MkAtomTerm(LookupAtom("instantiation_error"));
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("access"));
      ti[1] = MkAtomTerm(LookupAtom("private_procedure"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("create"));
      ti[1] = MkAtomTerm(LookupAtom("array"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("create"));
      ti[1] = MkAtomTerm(LookupAtom("operator"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("binary_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("past_end_of_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("input"));
      ti[1] = MkAtomTerm(LookupAtom("text_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("modify"));
      ti[1] = MkAtomTerm(LookupAtom("static_procedure"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("new"));
      ti[1] = MkAtomTerm(LookupAtom("alias"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("open"));
      ti[1] = MkAtomTerm(LookupAtom("source_sink"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("output"));
      ti[1] = MkAtomTerm(LookupAtom("binary_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("output"));
      ti[1] = MkAtomTerm(LookupAtom("stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("output"));
      ti[1] = MkAtomTerm(LookupAtom("text_stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("reposition"));
      ti[1] = MkAtomTerm(LookupAtom("stream"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("resize"));
      ti[1] = MkAtomTerm(LookupAtom("array"));
      ti[2] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("permission_error"),3), 3, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("character"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("representation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("character_code"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("representation_error"),1), 1, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("max_arity"));
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("representation_error"),1), 1, ti);
      tp = p+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case SYNTAX_ERROR:
    {
      int i;
      Term ti[1];

#if   HAVE_STRNCAT
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("syntax_error"),1), 1, ti);
      tp = p+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  case SYSTEM_ERROR:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      nt[0] = MkAtomTerm(LookupAtom("system_error"));
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("array"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("atom"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("atomic"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("byte"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("callable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("character"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("compound"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("db_reference"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("db_term"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("evaluable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("float"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("integer"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("key"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("list"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("number"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("predicate_indicator"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("pointer"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("unsigned_byte"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
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
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      ti[0] = MkAtomTerm(LookupAtom("variable"));
      ti[1] = where;
      nt[0] = MkApplTerm(MkFunctor(LookupAtom("type_error"),2), 2, ti);
      tp = p+i;
      psize -= i;
      fun = MkFunctor(LookupAtom("error"),2);
      serious = TRUE;
    }
    break;
  default:
    {
      int i;

#if   HAVE_STRNCAT
      strncat(p, " in ", psize);
#else
      strcat(p, " in ");
#endif
      i = strlen(p);
      nt[0] = MkAtomTerm(LookupAtom("system_error"));
      tp = p+i;
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
  nt[1] = MkAtomTerm(LookupAtom(p));
  if (serious) {
    Int depth;

    CreepFlag = CalculateStackGap();
    if (type == PURE_ABORT)
      depth = SetDBForThrow(MkAtomTerm(LookupAtom("abort")));
    else
      depth = SetDBForThrow(MkApplTerm(fun, 2, nt));
    if (depth == -1) {
      /* if we did not find an error already */
      if (P != (yamop *)FAILCODE)
	/* oops, we lost our trap handler, backtrack until the root or
           until an instance of do_goal */
	while (B->cp_b != NULL && B->cp_ap != (yamop *) NOCODE)
	  B = B->cp_b;
      P = (yamop *)FAILCODE;
      in_error = FALSE;
      return(P);
    }
    /* make the abstract machine jump where we want them to jump to */
#ifdef YAPOR
#if SBA
    CUT_prune_to((choiceptr)depth);
#else
    CUT_prune_to((choiceptr)(LCL0-depth));
#endif
#else
    B = (choiceptr)(LCL0-depth);
#endif	/* YAPOR */
    P = (yamop *)FAILCODE;
  }
  in_error = FALSE;
  return(P);
}

