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
* File:		stdpreds.c						 *
* Last rev:								 *
* mods:									 *
* comments:	General-purpose C implemented system predicates		 *
*									 *
*************************************************************************/
#ifdef SCCS
static char     SccsId[] = "%W% %G%";
#endif


/*
 * This file includes the definition of a miscellania of standard predicates
 * for yap refering to: Consulting, Executing a C predicate from call,
 * Comparisons (both general and numeric), Structure manipulation, Direct
 * access to atoms and predicates, Basic support for the debugger 
 *
 * It also includes a table where all C-predicates are initializated 
 *
 */

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"
#include "yapio.h"
#include <stdio.h>
#if HAVE_STRING_H
#include <string.h>
#endif

STD_PROTO(static Int p_setval, (void));
STD_PROTO(static Int p_value, (void));
STD_PROTO(static Int p_values, (void));
STD_PROTO(static Int p_flipflop, (void));
STD_PROTO(static Int p_setflop, (void));
#ifdef undefined
STD_PROTO(static CODEADDR *FindAtom, (CODEADDR, int *));
#endif /* undefined */
STD_PROTO(static Int p_opdec, (void));
STD_PROTO(static Term get_num, (char *));
STD_PROTO(static Int p_name, (void));
STD_PROTO(static Int p_atom_chars, (void));
STD_PROTO(static Int p_atom_codes, (void));
STD_PROTO(static Int p_atom_length, (void));
STD_PROTO(static Int p_atom_split, (void));
STD_PROTO(static Int p_number_chars, (void));
STD_PROTO(static Int p_number_codes, (void));
STD_PROTO(static Int p_univ, (void));
STD_PROTO(static Int p_abort, (void));
STD_PROTO(static Int p_halt, (void));
STD_PROTO(static Int p_halt0, (void));
STD_PROTO(static Int init_current_atom, (void));
STD_PROTO(static Int cont_current_atom, (void));
STD_PROTO(static Int init_current_predicate, (void));
STD_PROTO(static Int cont_current_predicate, (void));
STD_PROTO(static Int init_current_predicate_for_atom, (void));
STD_PROTO(static Int cont_current_predicate_for_atom, (void));
STD_PROTO(static OpEntry *NextOp, (OpEntry *));
STD_PROTO(static Int init_current_op, (void));
STD_PROTO(static Int cont_current_op, (void));
#ifdef DEBUG
STD_PROTO(static Int p_debug, (void));
#endif
STD_PROTO(static Int p_flags, (void));
STD_PROTO(static int AlreadyHidden, (char *));
STD_PROTO(static Int p_hide, (void));
STD_PROTO(static Int p_hidden, (void));
STD_PROTO(static Int p_unhide, (void));
STD_PROTO(static Int TrailMax, (void));
STD_PROTO(static Int GlobalMax, (void));
STD_PROTO(static Int LocalMax, (void));
STD_PROTO(static Int p_statistics_heap_max, (void));
STD_PROTO(static Int p_statistics_global_max, (void));
STD_PROTO(static Int p_statistics_local_max, (void));
STD_PROTO(static Int p_statistics_heap_info, (void));
STD_PROTO(static Int p_statistics_stacks_info, (void));
STD_PROTO(static Int p_statistics_trail_info, (void));
STD_PROTO(static Term mk_argc_list, (void));
STD_PROTO(static Int p_argv, (void));
STD_PROTO(static Int p_cputime, (void));
STD_PROTO(static Int p_runtime, (void));
STD_PROTO(static Int p_walltime, (void));
STD_PROTO(static Int p_access_yap_flags, (void));
STD_PROTO(static Int p_set_yap_flags, (void));


static Int 
p_setval(void)
{				/* '$set_value'(+Atom,+Atomic) */
	Term            t1 = Deref(ARG1), t2 = Deref(ARG2);
	if (!IsVarTerm(t1) && IsAtomTerm(t1) &&
	    (!IsVarTerm(t2) && (IsAtomTerm(t2) || IsNumTerm(t2)))) {
		_YAP_PutValue(AtomOfTerm(t1), t2);
		return (TRUE);
	}
	return (FALSE);
}

static Int 
p_value(void)
{				/* '$get_value'(+Atom,?Val) */
  Term t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    _YAP_Error(INSTANTIATION_ERROR,t1,"get_value/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    _YAP_Error(TYPE_ERROR_ATOM,t1,"get_value/2");
    return (FALSE);
  }
  return (_YAP_unify_constant(ARG2, _YAP_GetValue(AtomOfTerm(t1))));
}


static Int 
p_values(void)
{				/* '$values'(Atom,Old,New) */
  Term            t1 = Deref(ARG1), t3 = Deref(ARG3);

  if (IsVarTerm(t1)) {
    _YAP_Error(INSTANTIATION_ERROR,t1,"set_value/2");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    _YAP_Error(TYPE_ERROR_ATOM,t1,"set_value/2");
    return (FALSE);
  }
  if (!_YAP_unify_constant(ARG2, _YAP_GetValue(AtomOfTerm(t1))))
    return (FALSE);
  if (!IsVarTerm(t3)) {
    if (IsAtomTerm(t3) || IsNumTerm(t3)) {
      _YAP_PutValue(AtomOfTerm(t1), t3);
    } else
      return (FALSE);
  }
  return (TRUE);
}

static Int 
p_flipflop(void)
{				/* '$flipflop'		 */
  Atom            at;
  PredEntry      *pred;

  at = _YAP_FullLookupAtom("$spy");
  pred = RepPredProp(PredPropByFunc(_YAP_MkFunctor(at, 1),0));
  SpyCode = pred;
  return ((int) (FlipFlop = (1 - FlipFlop)));
}

static Int 
p_setflop(void)
{				/* '$setflop'(N)	 */
  Term            t = Deref(ARG1);

  if (IsIntTerm(t)) {
    FlipFlop = IntOfTerm(t) & 1;
    return (TRUE);
  }
  return (FALSE);
}

static Int 
p_creep(void)
{
  Atom            at;
  PredEntry      *pred;

  at = _YAP_FullLookupAtom("$creep");
  pred = RepPredProp(PredPropByFunc(_YAP_MkFunctor(at, 1),0));
  CreepCode = pred;
  CreepFlag = Unsigned(LCL0)-Unsigned(H0);
  return (TRUE);
}

Int 
_YAP_creep(void)
{
  return p_creep();
}

#ifdef undefined

/*
 * Returns where some particular piece of code is, it may take its time but
 * then you only need it while creeping, so why bother ? 
 */
static CODEADDR *
FindAtom(codeToFind, arity)
     CODEADDR        codeToFind;
     unsigned int   *arityp;
{
  Atom            a;
  int             i;

  for (i = 0; i < MaxHash; ++i) {
    READ_LOCK(HashChain[i].AeRWLock);
    a = HashChain[i].Entry;
    READ_UNLOCK(HashChain[i].AeRWLock);
    while (a != NIL) {
      register PredEntry *pp;
      AtomEntry *ae = RepAtom(a);
      READ_LOCK(ae->ARWLock);
      pp = RepPredProp(RepAtom(a)->PropsOfAE);
      while (!EndOfPAEntr(pp) && ((pp->KindOfPE & 0x8000)
				  || (pp->CodeOfPred != codeToFind)))
	pp = RepPredProp(pp->NextOfPE);
      if (pp != NIL) {
	CODEADDR *out;
	READ_LOCK(pp->PRWLock);
	out = &(pp->CodeOfPred)
	*arityp = pp->ArityOfPE;
	READ_UNLOCK(pp->PRWLock);
	READ_UNLOCK(ae->ARWLock);
	return (out);
      }
      a = RepAtom(a)->NextOfAE;
      READ_UNLOCK(ae->ARWLock);
    }
  }
  *arityp = 0;
  return (0);
}

/*
 * This is called when you want to creep a C-predicate or a predicate written
 * in assembly 
 */
CELL 
FindWhatCreep(toCreep)
     CELL            toCreep;
{
  unsigned int    arity;
  Atom            at;
  CODEADDR       *place;

  if (toCreep > 64) {	/* written in C */
    int             i;
    place = FindAtom((CODEADDR) toCreep, &arity);
    *--ASP = Unsigned(P);
    *--ASP = N = arity;
    for (i = 1; i <= arity; ++i)
      *--ASP = X[i];
    /* P = CellPtr(CCREEPCODE);		 */
    return (Unsigned(place));
  }
}

#endif				/* undefined */

static Int 
p_opdec(void)
{				/* '$opdec'(p,type,atom)		 */
  /* we know the arguments are integer, atom, atom */
  Term            p = Deref(ARG1), t = Deref(ARG2), at = Deref(ARG3);
  return (_YAP_OpDec((int) IntOfTerm(p), RepAtom(AtomOfTerm(t))->StrOfAE,
		AtomOfTerm(at)));
}


#ifdef NO_STRTOD

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

double 
strtod(s, pe)
	char           *s, **pe;
{
	double          r = atof(s);
	*pe = s;
	while (*s == ' ')
		++s;
	if (*s == '+' || *s == '-')
		++s;
	if (!isdigit(*s))
		return (r);
	while (isdigit(*s))
		++s;
	if (*s == '.')
		++s;
	while (isdigit(*s))
		++s;
	if (*s == 'e' || *s == 'E')
		++s;
	if (*s == '+' || *s == '-')
		++s;
	while (isdigit(*s))
		++s;
	*pe = s;
	return (r);
}

#else

#include <stdlib.h>

#endif

static char *cur_char_ptr;

static int
get_char_from_string(int sno)
{
  if (cur_char_ptr[0] == '\0')
    return(-1);
  cur_char_ptr++;
  return((int)(cur_char_ptr[-1]));
}

    
static Term 
get_num(char *t)
{
  Term out;

  cur_char_ptr = t;
  out = _YAP_scan_num(get_char_from_string);
  /* not ever iso */
  if (out == TermNil && yap_flags[LANGUAGE_MODE_FLAG] != 1) {
    int sign = 1;
    if (t[0] == '+') {
      t++;
    }
    if (t[0] == '-') {
      t++;
      sign = -1;
    }
    if(strcmp(t,"inf") == 0) {
      Term ta[1];
      ta[0] = MkAtomTerm(_YAP_LookupAtom("inf"));
      if (sign > 0) {
	return(_YAP_MkApplTerm(_YAP_MkFunctor(AtomPlus, 1), 1, ta));
      }
      return(_YAP_MkApplTerm(_YAP_MkFunctor(AtomMinus, 1), 1, ta));
    }
    if(strcmp(t,"nan") == 0) {
      Term ta[1];
      ta[0] = MkAtomTerm(_YAP_LookupAtom("nan"));
      if (sign > 0) {
	return(_YAP_MkApplTerm(_YAP_MkFunctor(AtomPlus, 1), 1, ta));
      }
      return(_YAP_MkApplTerm(_YAP_MkFunctor(AtomMinus, 1), 1, ta));
    }
  }
  if (cur_char_ptr[0] == '\0')
    return(out);
  else
    return(TermNil);
}

static Int 
runtime(void)
{
  return(_YAP_cputime()-_YAP_total_gc_time()-_YAP_total_stack_shift_time());
}

Int last_gc_time = 0;
Int last_ss_time = 0;

/* $runtime(-SinceInterval,-SinceStart)	 */
static Int 
p_runtime(void)
{
  Int now, interval,
    gc_time,
    ss_time;

  _YAP_cputime_interval(&now, &interval);
  gc_time = _YAP_total_gc_time();
  ss_time = _YAP_total_stack_shift_time();
  now -= gc_time+ss_time;
  interval -= (gc_time-last_gc_time)+(ss_time-last_ss_time);
  last_gc_time = gc_time;
  last_ss_time = ss_time;
  return( _YAP_unify_constant(ARG1, MkIntegerTerm(now)) && 
	 _YAP_unify_constant(ARG2, MkIntegerTerm(interval)) );
}

/* $cputime(-SinceInterval,-SinceStart)	 */
static Int 
p_cputime(void)
{
  Int now, interval;
  _YAP_cputime_interval(&now, &interval);
  return( _YAP_unify_constant(ARG1, MkIntegerTerm(now)) && 
	 _YAP_unify_constant(ARG2, MkIntegerTerm(interval)) );
}

static Int 
p_walltime(void)
{
  Int now, interval;
  _YAP_walltime_interval(&now, &interval);
  return( _YAP_unify_constant(ARG1, MkIntegerTerm(now)) && 
	 _YAP_unify_constant(ARG2, MkIntegerTerm(interval)) );
}

static Int 
p_char_code(void)
{
  Int t0 = Deref(ARG1);
  if (IsVarTerm(t0)) {
    Term t1 = Deref(ARG2);
    if (IsVarTerm(t1)) {
      _YAP_Error(INSTANTIATION_ERROR,t0,"char_code/2");
      return(FALSE);
    } else if (!IsIntegerTerm(t1)) {
      _YAP_Error(TYPE_ERROR_INTEGER,t1,"char_code/2");
      return(FALSE);
    } else {
      Int code = IntegerOfTerm(t1);
      char codes[2];
      Term tout;

      if (code < 0 || code > 256) {
	_YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,t1,"char_code/2");
	return(FALSE);
      }
      codes[0] = code;
      codes[1] = '\0';
      tout = MkAtomTerm(_YAP_LookupAtom(codes));
      return(_YAP_unify(ARG1,tout));
    }
  } else if (!IsAtomTerm(t0)) {
    _YAP_Error(TYPE_ERROR_CHARACTER,t0,"char_code/2");
    return(FALSE);
  } else {
    char *c = RepAtom(AtomOfTerm(t0))->StrOfAE;
    if (c[1] != '\0') {
      _YAP_Error(TYPE_ERROR_CHARACTER,t0,"char_code/2");
      return(FALSE);
    }
    return(_YAP_unify(ARG2,MkIntTerm((Int)(c[0]))));
  }
}

static Int 
p_name(void)
{				/* name(?Atomic,?String)		 */
  char            *String = (char *)TR, *s; /* alloc temp space on trail */
  Term            t, NewT, AtomNameT = Deref(ARG1);

  ARG2 = Deref(ARG2);
  if (!IsVarTerm(AtomNameT)) {
    if (IsAtomTerm(AtomNameT)) {
      s = RepAtom(AtomOfTerm(AtomNameT))->StrOfAE;
      NewT = _YAP_StringToList(s);
      if (!IsVarTerm(ARG2) && !IsPairTerm(ARG2)) {
	_YAP_Error(TYPE_ERROR_LIST,ARG2,
	      "name/2");
	return(FALSE);
      }
      return (_YAP_unify(NewT, ARG2));
    } else if (IsIntTerm(AtomNameT)) {
#if SHORT_INTS
      sprintf(String, "%ld", IntOfTerm(AtomNameT));
#else
      sprintf(String, "%d", IntOfTerm(AtomNameT));
#endif
      NewT = _YAP_StringToList(String);
      if (!IsVarTerm(ARG2) && !IsPairTerm(ARG2)) {
	_YAP_Error(TYPE_ERROR_LIST,ARG2,"name/2");
	return(FALSE);
      }
      return (_YAP_unify(NewT, ARG2));
    } else if (IsFloatTerm(AtomNameT)) {
      sprintf(String, "%f", FloatOfTerm(AtomNameT));
      NewT = _YAP_StringToList(String);
      if (!IsVarTerm(ARG2) && !IsPairTerm(ARG2)) {
	_YAP_Error(TYPE_ERROR_LIST,ARG2,"name/2");
	return(FALSE);
      }
      return (_YAP_unify(NewT, ARG2));
    } else if (IsLongIntTerm(AtomNameT)) {
#if SHORT_INTS
      sprintf(String, "%ld", LongIntOfTerm(AtomNameT));
#else
      sprintf(String, "%d", LongIntOfTerm(AtomNameT));
#endif
      NewT = _YAP_StringToList(String);
      if (!IsVarTerm(ARG2) && !IsPairTerm(ARG2)) {
	_YAP_Error(TYPE_ERROR_LIST,ARG2,"name/2");
	return(FALSE);
      }
      return (_YAP_unify(NewT, ARG2));
    } else {
      _YAP_Error(TYPE_ERROR_ATOMIC,AtomNameT,"name/2");
      return(FALSE);
    }
  }
  t = ARG2;
  s = String;
  if (!IsVarTerm(t) && t == MkAtomTerm(AtomNil)) {
    return (_YAP_unify_constant(ARG1, MkAtomTerm(_YAP_LookupAtom(""))));
  }
  while (!IsVarTerm(t) && IsPairTerm(t)) {
    Term            Head;
    Int             i;
    Head = HeadOfTerm(t);
    if (IsVarTerm(Head)) {
      _YAP_Error(INSTANTIATION_ERROR,Head,"name/2");
      return(FALSE);
    }
    if (!IsIntTerm(Head)) {
      _YAP_Error(TYPE_ERROR_INTEGER,Head,"name/2");
      return(FALSE);
    }
    i = IntOfTerm(Head);
    if (i < 0 || i > 255) {
      if (i<0)
	_YAP_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO,Head,"name/2");
      return(FALSE);
    }
    if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
      _YAP_growtrail(sizeof(CELL) * 16 * 1024L);
    }
    *s++ = i;
    t = TailOfTerm(t);
  }
  *s = '\0';
  if (IsVarTerm(t)) {
    _YAP_Error(INSTANTIATION_ERROR,t,"name/2");
    return(FALSE);
  }
  if (IsAtomTerm(t) && AtomOfTerm(t) == AtomNil) {
    if ((NewT = get_num(String)) == TermNil) {
      NewT = MkAtomTerm(_YAP_LookupAtom(String));
    }
    return (_YAP_unify_constant(ARG1, NewT));
  } else {
    _YAP_Error(TYPE_ERROR_LIST,t,"name/2");
    return(FALSE);
  }
}

static Int 
p_atom_chars(void)
{
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term            NewT;
    if (!IsAtomTerm(t1)) {
      _YAP_Error(TYPE_ERROR_ATOM, t1, "atom_chars/2");
      return(FALSE);
    }
    if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
      NewT = _YAP_StringToList(RepAtom(AtomOfTerm(t1))->StrOfAE);
    } else {
      NewT = _YAP_StringToListOfAtoms(RepAtom(AtomOfTerm(t1))->StrOfAE);
    }
    return (_YAP_unify(NewT, ARG2));
  } else {
    /* ARG1 unbound */
    char           *String = (char *)TR; /* alloc temp space on trail */
    register Term   t = Deref(ARG2);
    register char  *s = String;

    if (IsVarTerm(t)) {
      _YAP_Error(INSTANTIATION_ERROR, t1, "atom_chars/2");
      return(FALSE);		
    }
    if (t == TermNil) {
      return (_YAP_unify_constant(t1, MkAtomTerm(_YAP_LookupAtom(""))));
    }
    if (!IsPairTerm(t)) {
      _YAP_Error(TYPE_ERROR_LIST, t, "atom_chars/2");
      return(FALSE);		
    }
    if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
      while (t != TermNil) {
	register Term   Head;
	register Int    i;
	Head = HeadOfTerm(t);
	if (IsVarTerm(Head)) {
	  _YAP_Error(INSTANTIATION_ERROR,Head,"atom_chars/2");
	  return(FALSE);
	} else if (!IsIntTerm(Head)) {
	  _YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_chars/2");
	  return(FALSE);		
	}
	i = IntOfTerm(Head);
	if (i < 0 || i > 255) {
	  _YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_chars/2");
	  return(FALSE);		
	}
	if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
	  _YAP_growtrail(sizeof(CELL) * 16 * 1024L);
	}
	*s++ = i;
	t = TailOfTerm(t);
	if (IsVarTerm(t)) {
	  _YAP_Error(INSTANTIATION_ERROR,t,"atom_chars/2");
	  return(FALSE);
	} else if (!IsPairTerm(t) && t != TermNil) {
	  _YAP_Error(TYPE_ERROR_LIST, t, "atom_chars/2");
	  return(FALSE);
	}
      }
    } else {
      /* ISO Prolog Mode */
      while (t != TermNil) {
	register Term   Head;
	register char   *is;

	Head = HeadOfTerm(t);
	if (IsVarTerm(Head)) {
	  _YAP_Error(INSTANTIATION_ERROR,Head,"atom_chars/2");
	  return(FALSE);
	} else if (!IsAtomTerm(Head)) {
	  _YAP_Error(TYPE_ERROR_CHARACTER,Head,"atom_chars/2");
	  return(FALSE);		
	}
	is = RepAtom(AtomOfTerm(Head))->StrOfAE;
	if (is[1] != '\0') {
	  _YAP_Error(TYPE_ERROR_CHARACTER,Head,"atom_chars/2");
	  return(FALSE);		
	}
	if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
	  _YAP_growtrail(sizeof(CELL) * 16 * 1024L);
	}
	*s++ = is[0];
	t = TailOfTerm(t);
	if (IsVarTerm(t)) {
	  _YAP_Error(INSTANTIATION_ERROR,t,"atom_chars/2");
	  return(FALSE);
	} else if (!IsPairTerm(t) && t != TermNil) {
	  _YAP_Error(TYPE_ERROR_LIST, t, "atom_chars/2");
	  return(FALSE);
	}
      }
    }
    *s++ = '\0';
    return (_YAP_unify_constant(ARG1, MkAtomTerm(_YAP_LookupAtom(String))));
  }
}

static Int 
p_atom_concat(void)
{
  Term t1 = Deref(ARG1);
  char *cptr = ((AtomEntry *)_YAP_PreAllocCodeSpace())->StrOfAE, *cpt0;
  char *top = (char *)AuxSp;
  char *atom_str;
  UInt sz;

 restart:
  cpt0 = cptr;
  /* we need to have a list */
  if (IsVarTerm(t1)) {
    _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
    _YAP_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
    return(FALSE);
  }
  while (IsPairTerm(t1)) {
    Term thead = HeadOfTerm(t1);
    if (IsVarTerm(thead)) {
      _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
      _YAP_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
      return(FALSE);
    }
    if (!IsAtomTerm(thead)) {
      _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
      _YAP_Error(TYPE_ERROR_ATOM, ARG1, "atom_concat/2");
      return(FALSE);
    }
    atom_str = RepAtom(AtomOfTerm(thead))->StrOfAE;
    /* check for overflows */
    sz = strlen(atom_str);
    if (cptr+sz >= top-1024) {
      _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
      if (!_YAP_growheap(FALSE)) {
	_YAP_Error(SYSTEM_ERROR, TermNil, _YAP_ErrorMessage);
	return(FALSE);
      }
      goto restart;
    }
    memcpy((void *)cptr, (void *)atom_str, sz);
    cptr += sz;
    t1 = TailOfTerm(t1);
    if (IsVarTerm(t1)) {
      _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
      _YAP_Error(INSTANTIATION_ERROR, ARG1, "atom_concat/2");
      return(FALSE);
    }
  }
  if (t1 == TermNil) {
    Term tout;
    cptr[0] = '\0';
    tout = MkAtomTerm(_YAP_LookupAtom(cpt0));
    _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
    return(_YAP_unify(ARG2, tout));
  }
  _YAP_ReleasePreAllocCodeSpace((ADDR)cpt0);
  _YAP_Error(TYPE_ERROR_LIST, ARG1, "atom_concat/2");
  return(FALSE);
}

static Int 
p_atom_codes(void)
{
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    Term            NewT;
    if (!IsAtomTerm(t1)) {
      _YAP_Error(TYPE_ERROR_ATOM, t1, "atom_codes/2");
      return(FALSE);
    }
    NewT = _YAP_StringToList(RepAtom(AtomOfTerm(t1))->StrOfAE);
    return (_YAP_unify(NewT, ARG2));
  } else {
    /* ARG1 unbound */
    char           *String = (char *)TR; /* alloc temp space on trail */
    register Term   t = Deref(ARG2);
    register char  *s = String;

    if (IsVarTerm(t)) {
      _YAP_Error(INSTANTIATION_ERROR, t1, "atom_codes/2");
      return(FALSE);		
    }
    if (t == TermNil) {
      return (_YAP_unify_constant(t1, MkAtomTerm(_YAP_LookupAtom(""))));
    }
    if (!IsPairTerm(t)) {
      _YAP_Error(TYPE_ERROR_LIST, t, "atom_codes/2");
      return(FALSE);		
    }
    while (t != TermNil) {
      register Term   Head;
      register Int    i;
      Head = HeadOfTerm(t);
      if (IsVarTerm(Head)) {
	_YAP_Error(INSTANTIATION_ERROR,Head,"atom_codes/2");
	return(FALSE);
      } else if (!IsIntTerm(Head)) {
	_YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_codes/2");
	return(FALSE);		
      }
      i = IntOfTerm(Head);
      if (i < 0 || i > 255) {
	_YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"atom_codes/2");
	return(FALSE);		
      }
      if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
	_YAP_growtrail(sizeof(CELL) * 16 * 1024L);
      }
      *s++ = i;
      t = TailOfTerm(t);
      if (IsVarTerm(t)) {
	_YAP_Error(INSTANTIATION_ERROR,t,"atom_codes/2");
	return(FALSE);
      } else if (!IsPairTerm(t) && t != TermNil) {
	_YAP_Error(TYPE_ERROR_LIST, t, "atom_codes/2");
	return(FALSE);
      }
    }
    *s++ = '\0';
    return (_YAP_unify_constant(ARG1, MkAtomTerm(_YAP_LookupAtom(String))));
  }
}

static Int 
p_atom_length(void)
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Int len;

  if (IsVarTerm(t1)) {
    _YAP_Error(INSTANTIATION_ERROR, t1, "atom_length/2");
    return(FALSE);		
  }
  if (!IsAtomTerm(t1)) {
    _YAP_Error(TYPE_ERROR_ATOM, t1, "atom_length/2");
    return(FALSE);
  }
  if (!IsVarTerm(t2)) {
    if (!IsIntTerm(t2)) {
      _YAP_Error(TYPE_ERROR_INTEGER, t2, "atom_length/2");
      return(FALSE);
    }
    if ((len = IntOfTerm(t2)) < 0) {
      _YAP_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "atom_length/2");
      return(FALSE);
    }
    return((Int)strlen(RepAtom(AtomOfTerm(t1))->StrOfAE) == len);
  } else {
    Term tj = MkIntTerm(strlen(RepAtom(AtomOfTerm(t1))->StrOfAE));
    return(_YAP_unify_constant(t2,tj));
  }
}


/* split an atom into two sub-atoms */
static Int 
p_atom_split(void)
{
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  Int len;
  char *s, *s1;
  int i;
  Term to1, to2;

  s1 = (char *)H;
  if (IsVarTerm(t1)) {
    _YAP_Error(INSTANTIATION_ERROR, t1, "$atom_split/4");
    return(FALSE);		
  }
  if (!IsAtomTerm(t1)) {
    _YAP_Error(TYPE_ERROR_ATOM, t1, "$atom_split/4");
    return(FALSE);
  }
  if (IsVarTerm(t2)) {
    _YAP_Error(INSTANTIATION_ERROR, t2, "$atom_split/4");
    return(FALSE);		
  }
  if (!IsIntTerm(t2)) {
    _YAP_Error(TYPE_ERROR_INTEGER, t2, "$atom_split/4");
    return(FALSE);
  }
  if ((len = IntOfTerm(t2)) < 0) {
    _YAP_Error(DOMAIN_ERROR_NOT_LESS_THAN_ZERO, t2, "$atom_split/4");
    return(FALSE);
  }
  s = RepAtom(AtomOfTerm(t1))->StrOfAE;
  if (len > (Int)strlen(s)) return(FALSE);
  for (i = 0; i< len; i++) {
    if (s1 > (char *)LCL0-1024)
      _YAP_Error(SYSTEM_ERROR,t1,"$atom_split/4");
    s1[i] = s[i];
  }
  s1[len] = '\0';
  to1 = MkAtomTerm(_YAP_LookupAtom(s1));
  to2 = MkAtomTerm(_YAP_LookupAtom(s+len)); 
  return(_YAP_unify_constant(ARG3,to1) && _YAP_unify_constant(ARG4,to2));
}

static Term
gen_syntax_error(char *s)
{
  Term ts[6], ti[2];
  ti[0] = ARG1;
  ti[1] = ARG2;
  ts[0] = _YAP_MkApplTerm(_YAP_MkFunctor(_YAP_LookupAtom(s),2),2,ti);
  ts[1] = ts[4] = ts[5] = MkIntTerm(0);
  ts[2] = MkAtomTerm(_YAP_LookupAtom("number syntax"));
  ts[3] = TermNil;
  return(_YAP_MkApplTerm(_YAP_MkFunctor(_YAP_LookupAtom("syntax_error"),6),6,ts));
}

static Int 
p_number_chars(void)
{
  char   *String = (char *)TR; /* alloc temp space on Trail */
  register Term   t = Deref(ARG2), t1 = Deref(ARG1);
  Term NewT;
  register char  *s = String;

  if (IsNonVarTerm(t1)) {
    Term            NewT;
    if (!IsNumTerm(t1)) {
      _YAP_Error(TYPE_ERROR_NUMBER, t1, "number_chars/2");
      return(FALSE);
    } else if (IsIntTerm(t1)) {
#if SHORT_INTS
      sprintf(String, "%ld", IntOfTerm(t1));
#else
      sprintf(String, "%d", IntOfTerm(t1));
#endif
      if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
	NewT = _YAP_StringToList(String);
      } else {
	NewT = _YAP_StringToListOfAtoms(String);
      }
      return (_YAP_unify(NewT, ARG2));
    } else if (IsFloatTerm(t1)) {
      sprintf(String, "%f", FloatOfTerm(t1));
      if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
	NewT = _YAP_StringToList(String);
      } else {
	NewT = _YAP_StringToListOfAtoms(String);
      }
      return (_YAP_unify(NewT, ARG2));
    } else if (IsLongIntTerm(t1)) {
#if SHORT_INTS
      sprintf(String, "%ld", LongIntOfTerm(t1));
#else
      sprintf(String, "%d", LongIntOfTerm(t1));
#endif
      if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
	NewT = _YAP_StringToList(String);
      } else {
	NewT = _YAP_StringToListOfAtoms(String);
      }
      return (_YAP_unify(NewT, ARG2));
#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      mpz_get_str(String, 10, _YAP_BigIntOfTerm(t1));
      if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
	NewT = _YAP_StringToList(String);
      } else {
	NewT = _YAP_StringToListOfAtoms(String);
      }
      return (_YAP_unify(NewT, ARG2));
#endif
    }
  }
  if (IsVarTerm(t)) {
    _YAP_Error(INSTANTIATION_ERROR, t1, "number_chars/2");
    return(FALSE);		
  }
  if (!IsPairTerm(t) && t != TermNil) {
    _YAP_Error(TYPE_ERROR_LIST, t, "number_chars/2");
    return(FALSE);		
  }
  if (yap_flags[YAP_TO_CHARS_FLAG] == QUINTUS_TO_CHARS) {
    while (t != TermNil) {
      register Term   Head;
      register Int    i;
      Head = HeadOfTerm(t);
      if (IsVarTerm(Head)) {
	_YAP_Error(INSTANTIATION_ERROR,Head,"number_chars/2");
	return(FALSE);
      } else if (!IsIntTerm(Head)) {
	_YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"number_chars/2");
	return(FALSE);		
      }
      i = IntOfTerm(Head);
      if (i < 0 || i > 255) {
	_YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"number_chars/2");
	return(FALSE);		
      }
      if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
	_YAP_growtrail(sizeof(CELL) * 16 * 1024L);
      }
      *s++ = i;
      t = TailOfTerm(t);
      if (IsVarTerm(t)) {
	_YAP_Error(INSTANTIATION_ERROR,t,"number_chars/2");
	return(FALSE);
      } else if (!IsPairTerm(t) && t != TermNil) {
	_YAP_Error(TYPE_ERROR_LIST, t, "number_chars/2");
	return(FALSE);
      }
    }
  } else {
    /* ISO code */
    while (t != TermNil) {
      register Term   Head;
      register char   *is;
      
      Head = HeadOfTerm(t);
      if (IsVarTerm(Head)) {
	_YAP_Error(INSTANTIATION_ERROR,Head,"number_chars/2");
	return(FALSE);
      } else if (!IsAtomTerm(Head)) {
	_YAP_Error(TYPE_ERROR_CHARACTER,Head,"number_chars/2");
	return(FALSE);		
      }
      is = RepAtom(AtomOfTerm(Head))->StrOfAE;
      if (is[1] != '\0') {
	_YAP_Error(TYPE_ERROR_CHARACTER,Head,"number_chars/2");
	return(FALSE);		
      }
      if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
	_YAP_growtrail(sizeof(CELL) * 16 * 1024L);
      }
      *s++ = is[0];
      t = TailOfTerm(t);
      if (IsVarTerm(t)) {
	_YAP_Error(INSTANTIATION_ERROR,t,"number_chars/2");
	return(FALSE);
      } else if (!IsPairTerm(t) && t != TermNil) {
	_YAP_Error(TYPE_ERROR_LIST, t, "number_chars/2");
	return(FALSE);
      }
    }
  }
  *s++ = '\0';
  if ((NewT = get_num(String)) == TermNil) {
    _YAP_Error(SYNTAX_ERROR, gen_syntax_error("number_chars"), "while scanning %s", String);
    return (FALSE);
  }
  return (_YAP_unify(ARG1, NewT));
}

static Int 
p_number_atom(void)
{
  char   *String = (char *)TR; /* alloc temp space on Trail */
  register Term   t = Deref(ARG2), t1 = Deref(ARG1);
  Term NewT;
  register char  *s = String;

  if (IsNonVarTerm(t1)) {
    if (IsIntTerm(t1)) {
      Term            NewT;
#if SHORT_INTS
      sprintf(String, "%ld", IntOfTerm(t1));
#else
      sprintf(String, "%d", IntOfTerm(t1));
#endif
      NewT = MkAtomTerm(_YAP_LookupAtom(String));
      return (_YAP_unify(NewT, ARG2));
    } else if (IsFloatTerm(t1)) {
      Term            NewT;
      sprintf(String, "%f", FloatOfTerm(t1));
      NewT = MkAtomTerm(_YAP_LookupAtom(String));
      return (_YAP_unify(NewT, ARG2));
    } else if (IsLongIntTerm(t1)) {
      Term            NewT;
#if SHORT_INTS
      sprintf(String, "%ld", LongIntOfTerm(t1));
#else
      sprintf(String, "%d", LongIntOfTerm(t1));
#endif
      NewT = MkAtomTerm(_YAP_LookupAtom(String));
      return (_YAP_unify(NewT, ARG2));
#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      Term            NewT;
      mpz_get_str(String, 10, _YAP_BigIntOfTerm(t1));
      NewT = MkAtomTerm(_YAP_LookupAtom(String));
      return (_YAP_unify(NewT, ARG2));
#endif
    } else {
      _YAP_Error(TYPE_ERROR_NUMBER, t1, "number_atom/2");
      return(FALSE);
    }
  }
  if (IsVarTerm(t)) {
      _YAP_Error(INSTANTIATION_ERROR, t, "number_chars/2");
      return(FALSE);		
  }
  if (!IsAtomTerm(t)) {
    _YAP_Error(TYPE_ERROR_LIST, t, "number_atom/2");
    return(FALSE);		
  }
  s = RepAtom(AtomOfTerm(t))->StrOfAE;
  if ((NewT = get_num(s)) == TermNil) {
    _YAP_Error(SYNTAX_ERROR, Deref(ARG2), "number_atom/2", String);
    return (FALSE);
  }
  return (_YAP_unify(ARG1, NewT));
}

static Int 
p_number_codes(void)
{
  char   *String = (char *)TR; /* alloc temp space on Trail */
  register Term   t = Deref(ARG2), t1 = Deref(ARG1);
  Term NewT;
  register char  *s = String;

  if (IsNonVarTerm(t1)) {
    if (IsIntTerm(t1)) {
#if SHORT_INTS
      sprintf(String, "%ld", IntOfTerm(t1));
#else
      sprintf(String, "%d", IntOfTerm(t1));
#endif
      NewT = _YAP_StringToList(String);
      return (_YAP_unify(NewT, ARG2));
    } else if (IsFloatTerm(t1)) {
      sprintf(String, "%f", FloatOfTerm(t1));
      NewT = _YAP_StringToList(String);
      return (_YAP_unify(NewT, ARG2));
    } else if (IsLongIntTerm(t1)) {
#if SHORT_INTS
      sprintf(String, "%ld", LongIntOfTerm(t1));
#else
      sprintf(String, "%d", LongIntOfTerm(t1));
#endif
      NewT = _YAP_StringToList(String);
      return (_YAP_unify(NewT, ARG2));
#if USE_GMP
    } else if (IsBigIntTerm(t1)) {
      mpz_get_str(String, 10, _YAP_BigIntOfTerm(t1));
      NewT = _YAP_StringToList(String);
      return (_YAP_unify(NewT, ARG2));
#endif
    } else {
      _YAP_Error(TYPE_ERROR_NUMBER, t1, "number_codes/2");
      return(FALSE);
    }
  }
  if (IsVarTerm(t)) {
    _YAP_Error(INSTANTIATION_ERROR, t, "number_codes/2");
  }
  if (!IsPairTerm(t) && t != TermNil) {
    _YAP_Error(TYPE_ERROR_LIST, t, "number_codes/2");
    return(FALSE);		
  }
  while (t != TermNil) {
    register Term   Head;
    register Int    i;

    Head = HeadOfTerm(t);
    if (IsVarTerm(Head)) {
      _YAP_Error(INSTANTIATION_ERROR,Head,"number_codes/2");
      return(FALSE);
    } else if (!IsIntTerm(Head)) {
      _YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"number_codes/2");
      return(FALSE);		
    }
    i = IntOfTerm(Head);
    if (i < 0 || i > 255) {
      _YAP_Error(REPRESENTATION_ERROR_CHARACTER_CODE,Head,"number_codes/2");
      return(FALSE);		
    }
    if (Unsigned(_YAP_TrailTop) - Unsigned(s) < MinTrailGap) {
      _YAP_growtrail(sizeof(CELL) * 16 * 1024L);
    }
    *s++ = i;
    t = TailOfTerm(t);
    if (IsVarTerm(t)) {
      _YAP_Error(INSTANTIATION_ERROR,t,"number_codes/2");
      return(FALSE);
    } else if (!IsPairTerm(t) && t != TermNil) {
      _YAP_Error(TYPE_ERROR_LIST, t, "number_codes/2");
      return(FALSE);
    }
  }
  *s++ = '\0';
  if ((NewT = get_num(String)) == TermNil) {
    _YAP_Error(SYNTAX_ERROR, gen_syntax_error("number_codes"), "while scanning %s", String);
    return (FALSE);
  }
  return (_YAP_unify(ARG1, NewT));
}

static Int 
p_univ(void)
{				/* A =.. L			 */
  unsigned int    arity;
  register Term   tin;
  Term            twork, t2;
  Atom            at;

  tin = Deref(ARG1);
  t2 = Deref(ARG2);
  if (IsVarTerm(tin)) {
    /* we need to have a list */
    Term           *Ar;
    if (IsVarTerm(t2)) {
      _YAP_Error(INSTANTIATION_ERROR, t2, "(=..)/2");
      return(FALSE);
    }
    if (!IsPairTerm(t2)) {
      if (t2 == TermNil)
	_YAP_Error(DOMAIN_ERROR_NON_EMPTY_LIST, t2, "(=..)/2");
      else
	_YAP_Error(TYPE_ERROR_LIST, ARG2, "(=..)/2");
      return (FALSE);
    }
    twork = HeadOfTerm(t2);
    if (IsVarTerm(twork)) {
      _YAP_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
      return(FALSE);
    }
    if (IsNumTerm(twork)) {
      Term tt = TailOfTerm(t2);
      if (IsVarTerm(tt) || tt != MkAtomTerm(AtomNil)) {
	_YAP_Error(TYPE_ERROR_ATOM, twork, "(=..)/2");
	return (FALSE);
      }
      return (_YAP_unify_constant(ARG1, twork));
    }
    if (!IsAtomTerm(twork)) {
      _YAP_Error(TYPE_ERROR_ATOM, twork, "(=..)/2");
      return (FALSE);
    }      
    at = AtomOfTerm(twork);
    twork = TailOfTerm(t2);
    if (IsVarTerm(twork)) {
      _YAP_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
      return(FALSE);
    } else if (!IsPairTerm(twork)) {
      if (twork != TermNil) {
	_YAP_Error(TYPE_ERROR_LIST, ARG2, "(=..)/2");
	return(FALSE);
      }
      return (_YAP_unify_constant(ARG1, MkAtomTerm(at)));
    }
  build_compound:
    /* build the term directly on the heap */
    Ar = H;
    H++;
    
    while (!IsVarTerm(twork) && IsPairTerm(twork)) {
      *H++ = HeadOfTerm(twork);
      if (H > ASP - 1024) {
	/* restore space */
	H = Ar;
	if (!_YAP_gc(2, ENV, P)) {
	  _YAP_Error(OUT_OF_STACK_ERROR, TermNil, _YAP_ErrorMessage);
	  return(FALSE);
	}
	twork = TailOfTerm(Deref(ARG2));
	goto build_compound;
      }
      twork = TailOfTerm(twork);
    }
    if (IsVarTerm(twork)) {
      _YAP_Error(INSTANTIATION_ERROR, twork, "(=..)/2");
      return(FALSE);
    }
    if (twork != TermNil) {
      _YAP_Error(TYPE_ERROR_LIST, ARG2, "(=..)/2");
      return (FALSE);
    }
#ifdef SFUNC
    DOES_NOT_WORK();
    {
      SFEntry        *pe = (SFEntry *) _YAP_GetAProp(at, SFProperty);
      if (pe)
	twork = MkSFTerm(_YAP_MkFunctor(at, SFArity),
			 arity, CellPtr(TR), pe->NilValue);
      else
	twork = _YAP_MkApplTerm(_YAP_MkFunctor(at, arity),
			   arity, CellPtr(TR));
    }
#else
    arity = H-Ar-1;
    if (at == AtomDot && arity == 2) {
      Ar[0] = Ar[1];
      Ar[1] = Ar[2];
      H --;
      twork = AbsPair(Ar);
    } else {      
      *Ar = (CELL)(_YAP_MkFunctor(at, arity));
      twork = AbsAppl(Ar);
    }
#endif
    return (_YAP_unify(ARG1, twork));
  }
  if (IsAtomicTerm(tin)) {
    twork = MkPairTerm(tin, MkAtomTerm(AtomNil));
    return (_YAP_unify(twork, ARG2));
  }
  if (IsRefTerm(tin))
    return (FALSE);
  if (IsApplTerm(tin)) {
    Functor         fun = FunctorOfTerm(tin);
    arity = ArityOfFunctor(fun);
    at = NameOfFunctor(fun);
#ifdef SFUNC
    if (arity == SFArity) {
      CELL           *p = CellPtr(TR);
      CELL           *q = ArgsOfSFTerm(tin);
      int             argno = 1;
      while (*q) {
	while (*q > argno++)
	  *p++ = MkVarTerm();
	++q;
	*p++ = Deref(*q++);
      }
      twork = _YAP_ArrayToList(CellPtr(TR), argno - 1);
      while (IsIntTerm(twork)) {
	if (!_YAP_gc(2, ENV, P)) {
	  _YAP_Error(OUT_OF_STACK_ERROR, TermNil, _YAP_ErrorMessage);
	  return(FALSE);
	}    
	twork = _YAP_ArrayToList(CellPtr(TR), argno - 1);
      }
    } else
#endif
      {
	while (H+arity*2 > ASP-1024) {
	  if (!_YAP_gc(2, ENV, P)) {
	    _YAP_Error(OUT_OF_STACK_ERROR, TermNil, _YAP_ErrorMessage);
	    return(FALSE);
	  }
	  tin = Deref(ARG1);
	}
	twork = _YAP_ArrayToList(RepAppl(tin) + 1, arity);
      }
  } else {
    /* We found a list */
    at = AtomDot;
    twork = _YAP_ArrayToList(RepPair(tin), 2);
  }
  twork = MkPairTerm(MkAtomTerm(at), twork);
  return (_YAP_unify(ARG2, twork));
}

static Int 
p_abort(void)
{				/* abort			 */
  /* make sure we won't go creeping around */
  _YAP_Error(PURE_ABORT, TermNil, "");
  return(FALSE);
}

static Int 
p_halt(void)
{				/* halt				 */
  Term t = Deref(ARG1);
  Int out;

  if (IsVarTerm(t)) {
    _YAP_Error(INSTANTIATION_ERROR,t,"halt/1");
    return(FALSE);
  }
  if (!IsIntegerTerm(t)) {
    _YAP_Error(TYPE_ERROR_INTEGER,t,"halt/1");
    return(FALSE);
  }
  out = IntegerOfTerm(t);
  if (yap_flags[HALT_AFTER_CONSULT_FLAG]) {
    _YAP_exit(out);
  } else {
    fprintf(_YAP_stderr, "\n\n[ Prolog execution halted ]\n");
    _YAP_exit(out);
  }
  return (TRUE);
}


static Int 
p_halt0(void)
{				/* halt				 */
  if (yap_flags[HALT_AFTER_CONSULT_FLAG]) {
    _YAP_exit(0);
  } else {
    fprintf(_YAP_stderr, "\n\n[ Prolog execution halted ]\n");
    _YAP_exit(0);
  }
  return (TRUE);
}


static Int 
cont_current_atom(void)
{
  Atom            catom;
  Int             i = IntOfTerm(EXTRA_CBACK_ARG(1,2));
  AtomEntry       *ap; /* nasty hack for gcc on hpux */

  /* protect current hash table line */
  if (IsAtomTerm(EXTRA_CBACK_ARG(1,1)))
    catom = AtomOfTerm(EXTRA_CBACK_ARG(1,1));
  else
    catom = NIL;
  if (catom == NIL){
    i++;
    /* move away from current hash table line */
    while (i < MaxHash) {
      READ_LOCK(HashChain[i].AERWLock);
      catom = HashChain[i].Entry;
      if (catom != NIL) {
	break;
      }
      READ_UNLOCK(HashChain[i].AERWLock);
      i++;
    }
    if (i == MaxHash) {
      cut_fail();
    } else {
      READ_UNLOCK(HashChain[i].AERWLock);
    }
  }
  ap = RepAtom(catom);
  if (_YAP_unify_constant(ARG1, MkAtomTerm(catom))) {
    if (ap->NextOfAE == NIL) {
      i++;
      while (i < MaxHash) {
	READ_LOCK(HashChain[i].AERWLock);
	catom = HashChain[i].Entry;
	READ_UNLOCK(HashChain[i].AERWLock);
	if (catom != NIL) {
	  break;
	}
	i++;
      }
      if (i == MaxHash) {
	cut_succeed();
      } else {
	EXTRA_CBACK_ARG(1,1) = MkAtomTerm(catom);
      }
    } else {
      READ_LOCK(ap->ARWLock);
      EXTRA_CBACK_ARG(1,1) = MkAtomTerm(ap->NextOfAE);
      READ_UNLOCK(ap->ARWLock);
    }
    EXTRA_CBACK_ARG(1,2) = MkIntTerm(i);
    return(TRUE);
  } else {
    return(FALSE);
  }
}

static Int 
init_current_atom(void)
{				/* current_atom(?Atom)		 */
  Term t1 = Deref(ARG1);
  if (!IsVarTerm(t1)) {
    if (IsAtomTerm(t1))
      cut_succeed();
    else
      cut_fail();
  }
  READ_LOCK(HashChain[0].AERWLock);
  if (HashChain[0].Entry != NIL) {
    EXTRA_CBACK_ARG(1,1) = MkAtomTerm(HashChain[0].Entry);
  } else {
    EXTRA_CBACK_ARG(1,1) = MkIntTerm(0);
  }
  READ_UNLOCK(HashChain[0].AERWLock);
  EXTRA_CBACK_ARG(1,2) = MkIntTerm(0);
  return (cont_current_atom());
}

static Int 
cont_current_predicate(void)
{
  PredEntry      *pp = (PredEntry *)IntegerOfTerm(EXTRA_CBACK_ARG(3,1));
  UInt Arity;
  Atom name;

  while (pp != NULL) {
    if (pp->PredFlags & HiddenPredFlag)
      pp = pp->NextPredOfModule;
    else
      break;
  }
  if (pp == NULL)
    cut_fail();
  EXTRA_CBACK_ARG(3,1) = (CELL)MkIntegerTerm((Int)(pp->NextPredOfModule));
  if (pp->FunctorOfPred == FunctorModule)
    return(FALSE);
  Arity = pp->ArityOfPE;
  if (Arity)
    name = NameOfFunctor(pp->FunctorOfPred);
  else
    name = (Atom)pp->FunctorOfPred;
  return (_YAP_unify(ARG2,MkAtomTerm(name)) &&
	  _YAP_unify(ARG3, MkIntegerTerm((Int)Arity)));
}

static Int 
init_current_predicate(void)
{
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1) || !IsAtomTerm(t1)) cut_fail();
  EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)ModulePred[_YAP_LookupModule(t1)]);
  return (cont_current_predicate());
}

static Int 
cont_current_predicate_for_atom(void)
{
  Prop pf = (Prop)IntegerOfTerm(EXTRA_CBACK_ARG(3,1));
  SMALLUNSGN mod = _YAP_LookupModule(Deref(ARG2));

  while (pf != NIL) {
    FunctorEntry *pp = RepFunctorProp(pf);
    if (IsFunctorProperty(pp->KindOfPE)) {
      Prop p0 = pp->PropsOfFE;
      while (p0) {
	PredEntry *p = RepPredProp(p0);
	if (p->ModuleOfPred == mod ||
	    p->ModuleOfPred == 0) {
	  /* we found the predicate */
	  EXTRA_CBACK_ARG(3,1) = (CELL)MkIntegerTerm((Int)(pp->NextOfPE));
	  return(_YAP_unify(ARG3,_YAP_MkNewApplTerm(p->FunctorOfPred,p->ArityOfPE)));
	}
	p0 = p->NextOfPE;
      }
    } else if (pp->KindOfPE == PEProp) {
      PredEntry *pe = RepPredProp(pf);
      if (pe->ModuleOfPred == mod ||
	  pe->ModuleOfPred == 0) {
	/* we found the predicate */
	EXTRA_CBACK_ARG(3,1) = (CELL)MkIntegerTerm((Int)(pp->NextOfPE));
	return(_YAP_unify(ARG3,MkAtomTerm((Atom)(pe->FunctorOfPred))));
      }
    }
    pf = pp->NextOfPE;
  }
  cut_fail();
}

static Int 
init_current_predicate_for_atom(void)
{
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1) || !IsAtomTerm(t1)) cut_fail();
  EXTRA_CBACK_ARG(3,1) = MkIntegerTerm((Int)RepAtom(AtomOfTerm(t1))->PropsOfAE);
  return (cont_current_predicate_for_atom());
}

static OpEntry *
NextOp(OpEntry *pp)
{
  while (!EndOfPAEntr(pp) && pp->KindOfPE != OpProperty)
    pp = RepOpProp(pp->NextOfPE);
  return (pp);
}


static Int 
cont_current_op(void)
{
  int             prio;
  Atom            a = AtomOfTerm(EXTRA_CBACK_ARG(3,1));
  Int             i = IntOfTerm(EXTRA_CBACK_ARG(3,2));
  Int             fix = IntOfTerm(EXTRA_CBACK_ARG(3,3));
  Term            TType;
  OpEntry        *pp = NIL;
    /* fix hp gcc bug */
  AtomEntry *at = RepAtom(a);

  if (fix > 3) {
    a = AtomOfTerm(Deref(ARG3));
    READ_LOCK(RepAtom(a)->ARWLock);
    if (EndOfPAEntr(pp = NextOp(RepOpProp(RepAtom(a)->PropsOfAE)))) {
      READ_UNLOCK(RepAtom(a)->ARWLock);
      cut_fail();
    }
    READ_LOCK(pp->OpRWLock);
    READ_UNLOCK(RepAtom(a)->ARWLock);
    if (fix == 4 && pp->Prefix == 0)
      fix = 5;
    if (fix == 5 && pp->Posfix == 0)
      fix = 6;
    if (fix == 6 && pp->Infix == 0)
      cut_fail();
    TType = MkAtomTerm(_YAP_GetOp(pp, &prio, (int) (fix - 4)));
    fix++;
    if (fix == 5 && pp->Posfix == 0)
      fix = 6;
    if (fix == 6 && pp->Infix == 0)
      fix = 7;
    READ_UNLOCK(pp->OpRWLock);
    EXTRA_CBACK_ARG(3,3) = (CELL) MkIntTerm(fix);
    if (fix < 7)
      return (_YAP_unify_constant(ARG1, MkIntTerm(prio))
	      && _YAP_unify_constant(ARG2, TType));
    if (_YAP_unify_constant(ARG1, MkIntTerm(prio)) && _YAP_unify_constant(ARG2, TType))
      cut_succeed();
    else
      cut_fail();
  }
  if (fix == 3) {
    do {
      if ((a = at->NextOfAE) == NIL) {
	i++;
	while (TRUE) {
	  READ_LOCK(HashChain[i].AERWLock);
	  a = HashChain[i].Entry;
	  READ_UNLOCK(HashChain[i].AERWLock);
	  if (a != NIL) {
	    break;
	  }
	  i++;
	}
	if (i == MaxHash)
	  cut_fail();
	EXTRA_CBACK_ARG(3,2) = (CELL) MkIntTerm(i);
      }
      at = RepAtom(a);
      READ_LOCK(at->ARWLock);
      pp = NextOp(RepOpProp(at->PropsOfAE));
      READ_UNLOCK(at->ARWLock);
    } while (EndOfPAEntr(pp));
    fix = 0;
    EXTRA_CBACK_ARG(3,1) = (CELL) MkAtomTerm(a);
  } else {
    pp = NextOp(RepOpProp(at->PropsOfAE));
  }
  READ_LOCK(pp->OpRWLock);
  if (fix == 0 && pp->Prefix == 0)
    fix = 1;
  if (fix == 1 && pp->Posfix == 0)
    fix = 2;
  TType = MkAtomTerm(_YAP_GetOp(pp, &prio, (int) fix));
  fix++;
  if (fix == 1 && pp->Posfix == 0)
    fix = 2;
  if (fix == 2 && pp->Infix == 0)
    fix = 3;
  READ_UNLOCK(pp->OpRWLock);
  EXTRA_CBACK_ARG(3,3) = (CELL) MkIntTerm(fix);
  return (_YAP_unify_constant(ARG1, MkIntTerm(prio)) &&
	  _YAP_unify_constant(ARG2, TType) &&
	  _YAP_unify_constant(ARG3, MkAtomTerm(a)));
}

static Int 
init_current_op(void)
{				/* current_op(-Precedence,-Type,-Atom)		 */
  Int             i = 0;
  Atom            a;
  Term            tprio = Deref(ARG1);
  Term            topsec = Deref(ARG2);
  Term            top = Deref(ARG3);

  if (!IsVarTerm(tprio)) {
    Int prio;
    if (!IsIntTerm(tprio)) {
      _YAP_Error(DOMAIN_ERROR_OPERATOR_PRIORITY,tprio,"current_op/3");
      return(FALSE);
    }
    prio = IntOfTerm(tprio);
    if (prio < 1 || prio > 1200) {
      _YAP_Error(DOMAIN_ERROR_OPERATOR_PRIORITY,tprio,"current_op/3");
      return(FALSE);
    }
  }
  if (!IsVarTerm(topsec)) {
    char *opsec;
    if (!IsAtomTerm(topsec)) {
      _YAP_Error(DOMAIN_ERROR_OPERATOR_SPECIFIER,topsec,"current_op/3");
      return(FALSE);
    }
    opsec = RepAtom(AtomOfTerm(topsec))->StrOfAE;
    if (!_YAP_IsOpType(opsec)) {
      _YAP_Error(DOMAIN_ERROR_OPERATOR_SPECIFIER,topsec,"current_op/3");
      return(FALSE);
    }
  }
  if (!IsVarTerm(top)) {
    if (!IsAtomTerm(top)) {
      _YAP_Error(TYPE_ERROR_ATOM,top,"current_op/3");
      return(FALSE);
    }
  }
  while (TRUE) {
    READ_LOCK(HashChain[i].AERWLock);
    a = HashChain[i].Entry;
    READ_UNLOCK(HashChain[i].AERWLock);
    if (a != NIL) {
      break;
    }
    i++;
  }
  EXTRA_CBACK_ARG(3,1) = (CELL) MkAtomTerm(a);
  EXTRA_CBACK_ARG(3,2) = (CELL) MkIntTerm(i);
  if (IsVarTerm(top))
    EXTRA_CBACK_ARG(3,3) = (CELL) MkIntTerm(3);
  else if (IsAtomTerm(top))
    EXTRA_CBACK_ARG(3,3) = (CELL) MkIntTerm(4);
  else
    cut_fail();
  return (cont_current_op());
}

#ifdef DEBUG
static Int 
p_debug()
{				/* $debug(+Flag) */
  int             i = IntOfTerm(Deref(ARG1));

  if (i >= 'a' && i <= 'z')
    _YAP_Option[i - 96] = !_YAP_Option[i - 96];
  return (1);
}
#endif

static Int 
p_flags(void)
{				/* $flags(+Functor,+Mod,?OldFlags,?NewFlags) */
  PredEntry      *pe;
  Int             newFl;
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  int mod;

  if (IsVarTerm(t1))
    return (FALSE);
  if (!IsAtomTerm(t2)) {
    return(FALSE);
  }
  mod = _YAP_LookupModule(t2);
  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1)) {
    pe = RepPredProp(PredPropByAtom(AtomOfTerm(t1), mod));
  } else if (IsApplTerm(t1)) {
    Functor         funt = FunctorOfTerm(t1);
    pe = RepPredProp(PredPropByFunc(funt, mod));
  } else
    return (FALSE);
  if (EndOfPAEntr(pe))
    return (FALSE);
  WRITE_LOCK(pe->PRWLock);
  if (!_YAP_unify_constant(ARG3, MkIntTerm(pe->PredFlags))) {
    WRITE_UNLOCK(pe->PRWLock);
    return(FALSE);
  }
  ARG4 = Deref(ARG4);
  if (IsVarTerm(ARG4)) {
    WRITE_UNLOCK(pe->PRWLock);
    return (TRUE);
  } else if (!IsIntTerm(ARG4)) {
    union arith_ret v;

    if (_YAP_Eval(ARG4, &v) == long_int_e) {
	newFl = v.Int;
    } else {
      WRITE_UNLOCK(pe->PRWLock);
      _YAP_Error(TYPE_ERROR_INTEGER, ARG4, "flags");
      return(FALSE);
    }
  } else
    newFl = IntOfTerm(ARG4);
  pe->PredFlags = (SMALLUNSGN) newFl;
  WRITE_UNLOCK(pe->PRWLock);
  return (TRUE);
}

static int 
AlreadyHidden(char *name)
{
  AtomEntry      *chain;

  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  while (!EndOfPAEntr(chain) && strcmp(chain->StrOfAE, name) != 0)
    chain = RepAtom(chain->NextOfAE);
  if (EndOfPAEntr(chain))
    return (FALSE);
  return (TRUE);
}

static Int 
p_hide(void)
{				/* hide(+Atom)		 */
  Atom            atomToInclude;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    _YAP_Error(INSTANTIATION_ERROR,t1,"hide/1");
    return(FALSE);
  }
  if (!IsAtomTerm(t1)) {
    _YAP_Error(TYPE_ERROR_ATOM,t1,"hide/1");
    return(FALSE);
  }
  atomToInclude = AtomOfTerm(t1);
  if (AlreadyHidden(RepAtom(atomToInclude)->StrOfAE)) {
    _YAP_Error(SYSTEM_ERROR,t1,"an atom of name %s was already hidden",
	  RepAtom(atomToInclude)->StrOfAE);
    return(FALSE);
  }
  _YAP_ReleaseAtom(atomToInclude);
  WRITE_LOCK(INVISIBLECHAIN.AERWLock);
  WRITE_LOCK(RepAtom(atomToInclude)->ARWLock);
  RepAtom(atomToInclude)->NextOfAE = INVISIBLECHAIN.Entry;
  WRITE_UNLOCK(RepAtom(atomToInclude)->ARWLock);
  INVISIBLECHAIN.Entry = atomToInclude;
  WRITE_UNLOCK(INVISIBLECHAIN.AERWLock);
  return (TRUE);
}

static Int 
p_hidden(void)
{				/* '$hidden'(+F)		 */
  Atom            at;
  AtomEntry      *chain;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1))
    return (FALSE);
  if (IsAtomTerm(t1))
    at = AtomOfTerm(t1);
  else if (IsApplTerm(t1))
    at = NameOfFunctor(FunctorOfTerm(t1));
  else
    return (FALSE);
  READ_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  while (!EndOfPAEntr(chain) && AbsAtom(chain) != at)
    chain = RepAtom(chain->NextOfAE);
  READ_UNLOCK(INVISIBLECHAIN.AERWLock);
  if (EndOfPAEntr(chain))
    return (FALSE);
  return (TRUE);
}


static Int 
p_unhide(void)
{				/* unhide(+Atom)		 */
  AtomEntry      *atom, *old, *chain;
  Term t1 = Deref(ARG1);

  if (IsVarTerm(t1)) {
    _YAP_Error(INSTANTIATION_ERROR,t1,"unhide/1");
    return(FALSE);
  }
  if (!IsAtomTerm(t1)) {
    _YAP_Error(TYPE_ERROR_ATOM,t1,"unhide/1");
    return(FALSE);
  }
  atom = RepAtom(AtomOfTerm(t1));
  WRITE_LOCK(atom->ARWLock);
  if (atom->PropsOfAE != NIL) {
    _YAP_Error(SYSTEM_ERROR,t1,"cannot unhide an atom in use");
    return(FALSE);
  }
  WRITE_LOCK(INVISIBLECHAIN.AERWLock);
  chain = RepAtom(INVISIBLECHAIN.Entry);
  old = NIL;
  while (!EndOfPAEntr(chain) && strcmp(chain->StrOfAE, atom->StrOfAE) != 0) {
    old = chain;
    chain = RepAtom(chain->NextOfAE);
  }
  if (EndOfPAEntr(chain))
    return (FALSE);
  atom->PropsOfAE = chain->PropsOfAE;
  if (old == NIL)
    INVISIBLECHAIN.Entry = chain->NextOfAE;
  else
    old->NextOfAE = chain->NextOfAE;
  WRITE_UNLOCK(INVISIBLECHAIN.AERWLock);
  WRITE_UNLOCK(atom->ARWLock);
  return (TRUE);
}

void
_YAP_show_statistics(void)
{
  unsigned long int heap_space_taken = 
    (unsigned long int)(Unsigned(HeapTop)-Unsigned(_YAP_HeapBase));
  double frag = (100.0*(heap_space_taken-HeapUsed))/heap_space_taken;

  fprintf(_YAP_stderr, "Code Space:  %ld (%ld bytes needed, %ld bytes used, fragmentation %.3f%%).\n", 
	     (unsigned long int)(Unsigned (H0) - Unsigned (_YAP_HeapBase)),
	     (unsigned long int)(Unsigned(HeapTop)-Unsigned(_YAP_HeapBase)),
	     (unsigned long int)(HeapUsed),
	     frag);
  fprintf(_YAP_stderr, "Stack Space: %ld (%ld for Global, %ld for local).\n", 
	     (unsigned long int)(sizeof(CELL)*(LCL0-H0)),
	     (unsigned long int)(sizeof(CELL)*(H-H0)),
	     (unsigned long int)(sizeof(CELL)*(LCL0-ASP)));
  fprintf(_YAP_stderr, "Trail Space: %ld (%ld used).\n", 
	     (unsigned long int)(sizeof(tr_fr_ptr)*(Unsigned(_YAP_TrailTop)-Unsigned(_YAP_TrailBase))),
	     (unsigned long int)(sizeof(tr_fr_ptr)*(Unsigned(TR)-Unsigned(_YAP_TrailBase))));
  fprintf(_YAP_stderr, "Runtime: %lds.\n", (unsigned long int)(runtime ()));
  fprintf(_YAP_stderr, "Cputime: %lds.\n", (unsigned long int)(_YAP_cputime ()));
  fprintf(_YAP_stderr, "Walltime: %lds.\n", (unsigned long int)(_YAP_walltime ()));
}

static Int
p_statistics_heap_max(void)
{
  Term tmax = MkIntegerTerm(HeapMax);

  return(_YAP_unify(tmax, ARG1));
}

/* The results of the next routines are not to be trusted too */
/* much. Basically, any stack shifting will seriously confuse the */
/* results */

static Int    TrailTide = -1, LocalTide = -1, GlobalTide = -1;

/* maximum Trail usage */
static Int
TrailMax(void)
{
  Int i;
  Int TrWidth = Unsigned(_YAP_TrailTop) - Unsigned(_YAP_TrailBase);
  CELL *pt;

  if (TrailTide != TrWidth) {
    pt = (CELL *)TR;
    while (pt+2 < (CELL *)_YAP_TrailTop) {
      if (pt[0] == 0 &&
	  pt[1] == 0 &&
	  pt[2] == 0)
	break;
      else
	pt++;
    }
    if (pt+2 < (CELL *)_YAP_TrailTop)
      i = Unsigned(pt) - Unsigned(_YAP_TrailBase);
    else
      i = TrWidth;
  } else
    return(TrWidth);
  if (TrailTide > i)
    i = TrailTide;
  else
    TrailTide = i;
  return(i);
}

static Int
p_statistics_trail_max(void)
{
  Term tmax = MkIntegerTerm(TrailMax());

  return(_YAP_unify(tmax, ARG1));
  
}

/* maximum Global usage */
static Int
GlobalMax(void)
{
  Int i;
  Int StkWidth = Unsigned(LCL0) - Unsigned(H0);
  CELL *pt;

  if (GlobalTide != StkWidth) {
    pt = H;
    while (pt+2 < ASP) {
      if (pt[0] == 0 &&
	  pt[1] == 0 &&
	  pt[2] == 0)
	break;
      else
	pt++;
    }
    if (pt+2 < ASP)
      i = Unsigned(pt) - Unsigned(H0);
    else
      /* so that both Local and Global have reached maximum width */
      GlobalTide = LocalTide = i = StkWidth;
  } else
    return(StkWidth);
  if (GlobalTide > i)
    i = GlobalTide;
  else
    GlobalTide = i;
  return(i);
}

static Int 
p_statistics_global_max(void)
{
  Term tmax = MkIntegerTerm(GlobalMax());

  return(_YAP_unify(tmax, ARG1));
  
}


static Int
LocalMax(void)
{
  Int i;
  Int StkWidth = Unsigned(LCL0) - Unsigned(H0);
  CELL *pt;

  if (LocalTide != StkWidth) {
    pt = LCL0;
    while (pt-3 > H) {
      if (pt[-1] == 0 &&
	  pt[-2] == 0 &&
	  pt[-3] == 0)
	break;
      else
	--pt;
    }
    if (pt-3 > H)
      i = Unsigned(LCL0) - Unsigned(pt);
    else
      /* so that both Local and Global have reached maximum width */
      GlobalTide = LocalTide = i = StkWidth;
  } else
    return(StkWidth);
  if (LocalTide > i)
    i = LocalTide;
  else
    LocalTide = i;
  return(i);
}

static Int 
p_statistics_local_max(void)
{
  Term tmax = MkIntegerTerm(LocalMax());

  return(_YAP_unify(tmax, ARG1));
  
}



static Int 
p_statistics_heap_info(void)
{
  Term tmax = MkIntegerTerm(Unsigned(H0) - Unsigned(_YAP_HeapBase));
  Term tusage = MkIntegerTerm(HeapUsed);

  return(_YAP_unify(tmax, ARG1) && _YAP_unify(tusage,ARG2));
  
}


static Int 
p_statistics_stacks_info(void)
{
  Term tmax = MkIntegerTerm(Unsigned(LCL0) - Unsigned(H0));
  Term tgusage = MkIntegerTerm(Unsigned(H) - Unsigned(H0));
  Term tlusage = MkIntegerTerm(Unsigned(LCL0) - Unsigned(ASP));

  return(_YAP_unify(tmax, ARG1) && _YAP_unify(tgusage,ARG2) && _YAP_unify(tlusage,ARG3));
  
}


static Int 
p_statistics_trail_info(void)
{
  Term tmax = MkIntegerTerm(Unsigned(_YAP_TrailTop) - Unsigned(_YAP_TrailBase));
  Term tusage = MkIntegerTerm(Unsigned(TR) - Unsigned(_YAP_TrailBase));

  return(_YAP_unify(tmax, ARG1) && _YAP_unify(tusage,ARG2));
  
}

static Term
mk_argc_list(void)
{
  int i =0;
  Term t = TermNil;
  while (i < _YAP_argc) {
    char *arg = _YAP_argv[i];
    /* check for -L -- */
    if (arg[0] == '-' && arg[1] == 'L') {
      arg += 2;
      while (*arg != '\0' && (*arg == ' ' || *arg == '\t'))
	arg++;
      if (*arg == '-' && arg[1] == '-' && arg[2] == '\0') {
	/* we found the separator */
	int j;
	for (j = _YAP_argc-1; j > i+1; --j) {
	  t = MkPairTerm(MkAtomTerm(_YAP_LookupAtom(_YAP_argv[j])),t);
	}
      return(t);
      }
    }
    if (arg[0] == '-' && arg[1] == '-' && arg[2] == '\0') {
      /* we found the separator */
      int j;
      for (j = _YAP_argc-1; j > i; --j) {
	t = MkPairTerm(MkAtomTerm(_YAP_LookupAtom(_YAP_argv[j])),t);
      }
      return(t);
    }
    i++;
  } 
  return(t);
}

static Int 
p_argv(void)
{
  Term t = mk_argc_list();
  return(_YAP_unify(t, ARG1));
}

static Int
p_access_yap_flags(void)
{
  Term tflag = Deref(ARG1);
  Int flag;
  Term tout;

  if (IsVarTerm(tflag)) {
    _YAP_Error(INSTANTIATION_ERROR, tflag, "access_yap_flags/2");
    return(FALSE);		
  }
  if (!IsIntTerm(tflag)) {
    _YAP_Error(TYPE_ERROR_INTEGER, tflag, "access_yap_flags/2");
    return(FALSE);		
  }
  flag = IntOfTerm(tflag);
  if (flag < 0 || flag > NUMBER_OF_YAP_FLAGS) {
    return(FALSE);
  }
  tout = MkIntegerTerm(yap_flags[flag]);
  return(_YAP_unify(ARG2, tout));
}

static Int
p_host_type(void)
{
  return(_YAP_unify(ARG1,MkAtomTerm(_YAP_LookupAtom(HOST_ALIAS))));
}

static Int 
p_has_yap_or(void)
{
#ifdef YAPOR
  return(TRUE);
#else
  return(FALSE);
#endif
}


static Int
p_set_yap_flags(void)
{
  Term tflag = Deref(ARG1);
  Term tvalue = Deref(ARG2);
  Int flag, value;

  if (IsVarTerm(tflag)) {
    _YAP_Error(INSTANTIATION_ERROR, tflag, "set_yap_flags/2");
    return(FALSE);		
  }
  if (!IsIntTerm(tflag)) {
    _YAP_Error(TYPE_ERROR_INTEGER, tflag, "set_yap_flags/2");
    return(FALSE);		
  }
  flag = IntOfTerm(tflag);
  if (IsVarTerm(tvalue)) {
    _YAP_Error(INSTANTIATION_ERROR, tvalue, "set_yap_flags/2");
    return(FALSE);		
  }
  if (!IsIntTerm(tvalue)) {
    _YAP_Error(TYPE_ERROR_INTEGER, tvalue, "set_yap_flags/2");
    return(FALSE);		
  }
  value = IntOfTerm(tvalue);
  /* checking should have been performed */
  switch(flag) {
  case CHAR_CONVERSION_FLAG:
    if (value != 0 && value != 1)
      return(FALSE);
    yap_flags[CHAR_CONVERSION_FLAG] = value;
    break;
  case YAP_DOUBLE_QUOTES_FLAG:
    if (value < 0 || value > 2)
      return(FALSE);
    yap_flags[YAP_DOUBLE_QUOTES_FLAG] = value;
    break;
  case YAP_TO_CHARS_FLAG:
    if (value != 0 && value != 1)
      return(FALSE);
    yap_flags[YAP_TO_CHARS_FLAG] = value;
    break;
  case LANGUAGE_MODE_FLAG:
    if (value < 0 || value > 2)
      return(FALSE);
    if (value == 1) {
      heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(_YAP_MkFunctor(AtomMetaCall,4),0));
    } else {
      heap_regs->pred_meta_call = RepPredProp(PredPropByFunc(_YAP_MkFunctor(AtomMetaCall,4),0));
    }
    yap_flags[LANGUAGE_MODE_FLAG] = value;
    break;
  case STRICT_ISO_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[STRICT_ISO_FLAG] = value;
    break;
  case SPY_CREEP_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[SPY_CREEP_FLAG] = value;
    break;
  case SOURCE_MODE_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[SOURCE_MODE_FLAG] = value;
    break;
  case CHARACTER_ESCAPE_FLAG:
    if (value != ISO_CHARACTER_ESCAPES
	&& value != CPROLOG_CHARACTER_ESCAPES
	&& value != SICSTUS_CHARACTER_ESCAPES)
      return(FALSE);
    yap_flags[CHARACTER_ESCAPE_FLAG] = value;
    break;
  case WRITE_QUOTED_STRING_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[WRITE_QUOTED_STRING_FLAG] = value;
    break;
  case ALLOW_ASSERTING_STATIC_FLAG:
    if (value != 0 && value !=  1)
      return(FALSE);
    yap_flags[ALLOW_ASSERTING_STATIC_FLAG] = value;
    break;
  default:
    return(FALSE);
  }
  return(TRUE);
}

#ifndef YAPOR
static Int
p_default_sequential(void) {
  return(TRUE);
}
#endif

#ifdef DEBUG
extern void DumpActiveGoals(void);

static Int
p_dump_active_goals(void) {
  DumpActiveGoals();
  return(TRUE);
}
#endif

#ifdef INES
static Int
p_euc_dist(void) {
  Term t1 = Deref(ARG1);
  Term t2 = Deref(ARG2);
  double d1 = (double)(IntegerOfTerm(ArgOfTerm(1,t1))-IntegerOfTerm(ArgOfTerm(1,t2)));
  double d2 = (double)(IntegerOfTerm(ArgOfTerm(2,t1))-IntegerOfTerm(ArgOfTerm(2,t2)));
  double d3 = (double)(IntegerOfTerm(ArgOfTerm(3,t1))-IntegerOfTerm(ArgOfTerm(3,t2)));
  Int result = (Int)sqrt(d1*d1+d2*d2+d3*d3);
  return(_YAP_unify(ARG3,MkIntegerTerm(result)));
}

volatile int loop_counter = 0;

static Int
p_loop(void) {
  while (loop_counter == 0);
  return(TRUE);
}
#endif

void 
_YAP_InitBackCPreds(void)
{
  _YAP_InitCPredBack("$current_atom", 1, 2, init_current_atom, cont_current_atom,
		SafePredFlag|SyncPredFlag);
  _YAP_InitCPredBack("$current_predicate", 3, 1, init_current_predicate, cont_current_predicate,
		SafePredFlag|SyncPredFlag);
  _YAP_InitCPredBack("$current_predicate_for_atom", 3, 1, init_current_predicate_for_atom, cont_current_predicate_for_atom,
		SafePredFlag|SyncPredFlag);
  _YAP_InitCPredBack("current_op", 3, 3, init_current_op, cont_current_op,
		SafePredFlag|SyncPredFlag);
  _YAP_InitBackIO();
  _YAP_InitBackDB();
  _YAP_InitUserBacks();
}

typedef void (*Proc)(void);

Proc E_Modules[]= {/* init_fc,*/ (Proc) 0 };

void 
_YAP_InitCPreds(void)
{
  /* numerical comparison */
  _YAP_InitCPred("$set_value", 2, p_setval, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$get_value", 2, p_value, TestPredFlag|SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$values", 3, p_values, SafePredFlag|SyncPredFlag);
  /* The flip-flop */
  _YAP_InitCPred("$flipflop", 0, p_flipflop, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$setflop", 1, p_setflop, SafePredFlag|SyncPredFlag);
  /* general purpose */
  _YAP_InitCPred("$opdec", 3, p_opdec, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("name", 2, p_name, SafePredFlag);
  _YAP_InitCPred("char_code", 2, p_char_code, SafePredFlag);
  _YAP_InitCPred("atom_chars", 2, p_atom_chars, SafePredFlag);
  _YAP_InitCPred("atom_codes", 2, p_atom_codes, SafePredFlag);
  _YAP_InitCPred("atom_length", 2, p_atom_length, SafePredFlag);
  _YAP_InitCPred("$atom_split", 4, p_atom_split, SafePredFlag);
  _YAP_InitCPred("number_chars", 2, p_number_chars, SafePredFlag);
  _YAP_InitCPred("number_atom", 2, p_number_atom, SafePredFlag);
  _YAP_InitCPred("number_codes", 2, p_number_codes, SafePredFlag);
  _YAP_InitCPred("atom_concat", 2, p_atom_concat, SafePredFlag);
  _YAP_InitCPred("=..", 2, p_univ, 0);
  _YAP_InitCPred("$statistics_trail_max", 1, p_statistics_trail_max, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$statistics_heap_max", 1, p_statistics_heap_max, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$statistics_global_max", 1, p_statistics_global_max, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$statistics_local_max", 1, p_statistics_local_max, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$statistics_heap_info", 2, p_statistics_heap_info, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$statistics_stacks_info", 3, p_statistics_stacks_info, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$statistics_trail_info", 2, p_statistics_trail_info, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$argv", 1, p_argv, SafePredFlag);
  _YAP_InitCPred("$runtime", 2, p_runtime, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$cputime", 2, p_cputime, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$walltime", 2, p_walltime, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$access_yap_flags", 2, p_access_yap_flags, SafePredFlag);
  _YAP_InitCPred("$set_yap_flags", 2, p_set_yap_flags, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("abort", 0, p_abort, SyncPredFlag);
  _YAP_InitCPred("halt", 1, p_halt, SyncPredFlag);
  _YAP_InitCPred("halt", 0, p_halt0, SyncPredFlag);
  _YAP_InitCPred("$host_type", 1, p_host_type, SyncPredFlag);
  /* basic predicates for the prolog machine tracer */
  /* they are defined in analyst.c */
  /* Basic predicates for the debugger */
  _YAP_InitCPred("$creep", 0, p_creep, SafePredFlag|SyncPredFlag);
#ifdef DEBUG
  _YAP_InitCPred("$debug", 1, p_debug, SafePredFlag|SyncPredFlag);
#endif
  /* Accessing and changing the flags for a predicate */
  _YAP_InitCPred("$flags", 4, p_flags, SafePredFlag|SyncPredFlag);
  /* hiding and unhiding some predicates */
  _YAP_InitCPred("hide", 1, p_hide, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("unhide", 1, p_unhide, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$hidden", 1, p_hidden, SafePredFlag|SyncPredFlag);
  _YAP_InitCPred("$has_yap_or", 0, p_has_yap_or, SafePredFlag|SyncPredFlag);
#ifndef YAPOR
  _YAP_InitCPred("$default_sequential", 1, p_default_sequential, SafePredFlag|SyncPredFlag);
#endif
#ifdef INES
  _YAP_InitCPred("euc_dist", 3, p_euc_dist, SafePredFlag);
  _YAP_InitCPred("loop", 0, p_loop, SafePredFlag);
#endif
#ifdef DEBUG
  _YAP_InitCPred("dump_active_goals", 0, p_dump_active_goals, SafePredFlag|SyncPredFlag);
#endif

  _YAP_InitUnify();
  _YAP_InitInlines();
  _YAP_InitCdMgr();
  _YAP_InitExecFs();
  _YAP_InitIOPreds();
  _YAP_InitCmpPreds();
  _YAP_InitDBPreds();
  _YAP_InitBBPreds();
  _YAP_InitBigNums();
  _YAP_InitSysPreds();
  _YAP_InitSavePreds();
  _YAP_InitCoroutPreds();
  _YAP_InitArrayPreds();
  _YAP_InitLoadForeign();

  _YAP_InitUserCPreds();
  _YAP_InitUtilCPreds();
  _YAP_InitSortPreds();
  _YAP_InitMaVarCPreds();
#ifdef DEPTH_LIMIT
  _YAP_InitItDeepenPreds();
#endif
#ifdef ANALYST
  _YAP_InitAnalystPreds();
#endif
#ifdef LOW_LEVEL_TRACER
  _YAP_InitLowLevelTrace();
#endif
  _YAP_InitEval();
  _YAP_InitGrowPreds();
#if defined(YAPOR) || defined(TABLING)
  _YAP_init_optyap_preds();
#endif
  {
    void            (*(*(p))) (void) = E_Modules;
    while (*p)
      (*(*p++)) ();
  }
#if CAMACHO
  {
    extern void InitForeignPreds(void);
  
    _YAP_InitForeignPreds();
  }
#endif
#if APRIL
  {
    extern void init_ol(void), init_time(void);
  
    init_ol();
    init_time();
  }
#endif

}


