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
* File:		arrays.c						 *
* Last rev:								 *
* mods:									 *
* comments:	Array Manipulation Routines	                         *
*									 *
*************************************************************************/

#include "Yap.h"
#include "Yatom.h"
#include "Heap.h"
#include "eval.h"
#include "heapgc.h"
#if HAVE_ERRNO_H
#include <errno.h>
#else
extern int errno;
#endif
#if HAVE_STRING_H
#include <string.h>
#endif

#if __simplescalar__
#ifdef HAVE_MMAP
#undef HAVE_MMAP
#endif
#endif

STATIC_PROTO(Int  p_compile_array_refs, (void));
STATIC_PROTO(Int  p_array_refs_compiled, (void));
STATIC_PROTO(Int  p_sync_mmapped_arrays, (void));

/*
 * 
 * This file works together with pl/arrays.yap and arrays.h.
 * 
 * YAP now supports a very simple notion of arrays. Arrays may be
 * allocated dynamically or statically:
 * 
 * o anonymous arrays are created during execution and allocated
 * in the heap. They have the lifetime of any other other heap
 * object. Any term can be an argument to a dynamic array.
 * 
 * Dynamic arrays are named as a free variable and are
 * initialised with free variables. 
 * 
 * o named arrays are created during execution but allocated
 * in the code space. They have the lifetime of an heap
 * object. Any term can be an argument to a dynamic array.
 * 
 * Named arrays are named with atoms and are initialised with
 * free variables.
 * 
 * o static arrays are allocated in the heap. Their space is
 * never recovered unless explictly said so by the
 * program. Arguments to these arrays must have fixed size,
 * and can only be atomic (at least for now).
 * 
 * Static arrays can be named through an  atom. They are
 * initialised with [].
 * 
 * Users create arrays by a declaration X array Arity. If X is an atom
 * A, then this it is a static array and A's the array name, otherwise
 * X refers to a dynamic array.
 * 
 * As in C, arrays start counting from 0.
 * 
 * Users access arrays by a token X[I] or a[I], this token can appear
 * anywhere within the computation, so a[2] = X[3*4] means that the
 * second element of global array a should unify with the 12th element
 * of array X. The mechanism used to implement this is the same
 * mechanism used to implement suspension variables.
 * 
 * Representation:
 * 
 * Dynamic Arrays are represented as a compound term of arity N, where
 * N is the size of the array. Even so, I will not include array bound
 * checking for now.
 * 
 * |--------------------------------------------------------------|
 * | $ARRAY/N|....
 * |______________________________________________________________
 * 
 * 
 * Unbound Var is used as a place to point to.
 * 
 * Static Arrays are represented as a special property for an atom,
 * with field size and 
 * 
 * A term of the form X[I] is represented as a Reference pointing to
 * the compound term:
 * 
 * '$array_arg'(X,I)
 * 
 * Dereferecing will automatically find X[I].
 * 
 * The only exception is the compiler, which uses a different
 * dereferencing routine. The clause cl(a[2], Y[X], Y) will be
 * compiled as:
 * 
 * cl(A, B, Y) :- '$access_array'(a, A, 2), '$access_array'(Y, B, X).
 * 
 * There are three operations to access arrays:
 * 
 * X[I] = A, This is normal unification.
 * 
 * X[I] := A, This is multiassignment, and therefore
 * backtrackable.
 * 
 * X[I] ::= A, This is non-backtrackable multiassignment, ans most
 * useful for static arrays.
 * 
 * The LHS of := and of ::= must be an array element!
 * 
 */

STATIC_PROTO(Term AccessNamedArray, (Atom, Int));
STATIC_PROTO(void InitNamedArray, (ArrayEntry *, Int));
STATIC_PROTO(void CreateNamedArray, (PropEntry *, Int, AtomEntry *));
STATIC_PROTO(void ResizeStaticArray, (StaticArrayEntry *, Int));
#if HAVE_MMAP
STATIC_PROTO(Int  CloseMmappedArray, (StaticArrayEntry *, void *));
STATIC_PROTO(void ResizeMmappedArray, (StaticArrayEntry *, Int, void *));
#endif
STATIC_PROTO(Int  p_create_array, (void));
STATIC_PROTO(Int  p_create_mmapped_array, (void));
STATIC_PROTO(void replace_array_references_complex, (CELL *, CELL *, CELL *, Term));
STATIC_PROTO(Term replace_array_references, (Term));
STATIC_PROTO(Int  p_array_references, (void));
STATIC_PROTO(Int  p_create_static_array, (void));
STATIC_PROTO(Int  p_resize_static_array, (void));
STATIC_PROTO(Int  p_close_static_array, (void));
STATIC_PROTO(Int  p_access_array, (void));
STATIC_PROTO(Int  p_assign_static, (void));

static Term
AccessNamedArray(Atom a, Int indx)
{
  AtomEntry *ae = RepAtom(a);
  ArrayEntry *pp; 

  READ_LOCK(ae->ARWLock);
  pp = RepArrayProp(ae->PropsOfAE);
  while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
    pp = RepArrayProp(pp->NextOfPE);
  READ_UNLOCK(ae->ARWLock);

  if (!EndOfPAEntr(pp)) {
    if (ArrayIsDynamic(pp)) {
      Term out;
      READ_LOCK(pp->ArRWLock);
      if (IsVarTerm(pp->ValueOfVE)) {
	READ_UNLOCK(pp->ArRWLock);
	return(MkAtomTerm(AtomFoundVar));
      }
      out = RepAppl(pp->ValueOfVE)[indx+1];
      READ_UNLOCK(pp->ArRWLock);
      return(out);
    } else {
      StaticArrayEntry *ptr = (StaticArrayEntry *)pp;

      READ_LOCK(ptr->ArRWLock);
      if (-(pp->ArrayEArity) <= indx || indx < 0) {
	/*	Error(DOMAIN_ERROR_ARRAY_OVERFLOW, MkIntegerTerm(indx), "access_array");*/
	READ_UNLOCK(ptr->ArRWLock);
	P = (yamop *)FAILCODE;
	return(TermNil);
      }
      switch (ptr->ArrayType) {

      case array_of_ints:
	{
	  Term out;
	  out = MkIntegerTerm(ptr->ValueOfVE.ints[indx]);
	  READ_UNLOCK(ptr->ArRWLock);
	  return (out);
	}
      case array_of_doubles:
	{
	  Term out;
	  out = MkEvalFl(ptr->ValueOfVE.floats[indx]);
	  READ_UNLOCK(ptr->ArRWLock);
	  return (out);
	}
      case array_of_ptrs:
	{
	  Term out;
	  out = MkIntegerTerm((Int)(ptr->ValueOfVE.ptrs[indx]));
	  READ_UNLOCK(ptr->ArRWLock);
	  return (out);
	}
      case array_of_atoms:
	{
	  Term out;
	  out = ptr->ValueOfVE.atoms[indx];
	  READ_UNLOCK(ptr->ArRWLock);
	  if (out == 0L)
	    out = TermNil;
	  else
	    return(out);
	}
	/* just return the atom */
      case array_of_chars:
	{
	  Term out;
	  out = MkIntegerTerm((Int)(ptr->ValueOfVE.chars[indx]));
	  READ_UNLOCK(ptr->ArRWLock);
	  return (out);
	}
      case array_of_uchars:
	{
	  Term out;
	  out = MkIntegerTerm((Int)(ptr->ValueOfVE.uchars[indx]));
	  READ_UNLOCK(ptr->ArRWLock);
	  return (out);
	}
      case array_of_dbrefs:
	{
	  /* The object is now in use */
	  Term TRef = ptr->ValueOfVE.dbrefs[indx];

	  READ_UNLOCK(ptr->ArRWLock);
	  if (TRef != 0L) {
	    DBRef ref = DBRefOfTerm(TRef);
#if defined(YAPOR) || defined(THREADS)
	    LOCK(ref->lock);
	    INC_DBREF_COUNT(ref);
	    TRAIL_REF(ref);	/* So that fail will erase it */
	    UNLOCK(ref->lock);
#else
	    if (!(ref->Flags & InUseMask)) {
	      ref->Flags |= InUseMask;
	      TRAIL_REF(ref);	/* So that fail will erase it */
	    }
#endif
	  } else {
	    P = (yamop *)FAILCODE;
	    TRef = TermNil;
	  }
	  return (TRef);
	}
      case array_of_terms:
	{
	  /* The object is now in use */
	  DBRef ref = ptr->ValueOfVE.terms[indx];
	  Term TRef;

	  READ_UNLOCK(ptr->ArRWLock);
	  if (ref != NULL) {
	    TRef = FetchTermFromDB(ref,3);
	  } else {
	    P = (yamop *)FAILCODE;
	    TRef = TermNil;
	  }
	  return (TRef);
	}
      default:
	READ_UNLOCK(ptr->ArRWLock);
	return(TermNil);
      }
    }      
  }
  else {
    Error(EXISTENCE_ERROR_ARRAY,MkAtomTerm(a),"named array");
    return (TermNil);
  }

}

static Int 
p_access_array(void)
{
  Term t = Deref(ARG1);
  Term ti = Deref(ARG2);
  Term tf;
  Int indx;

  if (IsNonVarTerm(ti)) {
    union arith_ret v;
    if (IsIntTerm(ti))
      indx = IntOfTerm(ti);
    else if (Eval(ti, &v) == long_int_e)
      indx = v.Int;
    else {
      Error(TYPE_ERROR_INTEGER,ti,"access_array");
      return (FALSE);
    }
  }
  else {
    Error(INSTANTIATION_ERROR,ti,"access_array");
    return (TermNil);
  }

  if (IsNonVarTerm(t)) {
    if (IsApplTerm(t)) {
      if (indx >= ArityOfFunctor(FunctorOfTerm(t))) {
	/*	Error(DOMAIN_ERROR_ARRAY_OVERFLOW, MkIntegerTerm(indx), "access_array");*/
	P = (yamop *)FAILCODE;
	return(FALSE);
      }
      tf = (RepAppl(t))[indx + 1];
    } else if (IsAtomTerm(t)) {
      tf = AccessNamedArray(AtomOfTerm(t), indx);
      if (tf == MkAtomTerm(AtomFoundVar)) {
	return(FALSE);
      }
    } else {
      Error(TYPE_ERROR_ARRAY,t,"access_array");
      return(FALSE);
    }    
  } else {
    Error(INSTANTIATION_ERROR,t,"access_array");
    return(FALSE);
  }
  return (unify(tf, ARG3));
}

static Int 
p_array_arg(void)
{
  register Term ti = Deref(ARG3), t;
  register Int indx;

  if (IsNonVarTerm(ti)) {
    union arith_ret v;
    if (IsIntTerm(ti))
      indx = IntOfTerm(ti);
    else if (Eval(ti, &v) == long_int_e)
      indx =  v.Int;
    else {
      Error(TYPE_ERROR_INTEGER,ti,"array_arg");
      return (FALSE);
    }
  }
  else {
    Error(INSTANTIATION_ERROR,ti,"array_arg");
    return (FALSE);
  }

  t = Deref(ARG2);
  if (IsNonVarTerm(t)) {
    if (IsApplTerm(t)) {
      return (unify(((RepAppl(t))[indx + 1]), ARG1));
    }
    else if (IsAtomTerm(t)) {
      Term tf = AccessNamedArray(AtomOfTerm(t), indx);
      if (tf == MkAtomTerm(AtomFoundVar)) {
	return(FALSE);
      }
      return (unify(tf, ARG1));
    }
    else
      Error(TYPE_ERROR_ARRAY,t,"array_arg");
  }
  else
    Error(INSTANTIATION_ERROR,t,"array_arg");

  return (FALSE);

}

static void
InitNamedArray(ArrayEntry * p, Int dim)
{
  Term *tp;

  WRITE_LOCK(p->ArRWLock);
  /* Leave a pointer so that we can reclaim array space when
   * we backtrack or when we abort */
  /* place terms in reverse order */
  Bind_Global(&(p->ValueOfVE),AbsAppl(H));
  tp = H;
  tp[0] =  (CELL)MkFunctor(AtomArray, dim);
  tp++;
  p->ArrayEArity = dim;
  /* Initialise the array as a set of variables */
  H = tp+dim;
  for (; tp < H; tp++) {
    RESET_VARIABLE(tp);
  }
  WRITE_UNLOCK(p->ArRWLock);
}

/* we assume the atom ae is already locked */
static void
CreateNamedArray(PropEntry * pp, Int dim, AtomEntry *ae)
{
  ArrayEntry *p;

  p = (ArrayEntry *) AllocAtomSpace(sizeof(*p));
  p->KindOfPE = ArrayProperty;
  p->NextOfPE = ae->PropsOfAE;
  INIT_RWLOCK(p->ArRWLock);
  ae->PropsOfAE = AbsArrayProp(p);

  InitNamedArray(p, dim);

}

static void
AllocateStaticArraySpace(StaticArrayEntry *p, static_array_types atype, Int array_size)
{
  Int asize = 0;
  switch (atype) {
  case array_of_doubles:
    asize = array_size*sizeof(Float);
    break;
  case array_of_ints:
    asize = array_size*sizeof(Int);
    break;
  case array_of_chars:
    asize = array_size*sizeof(char);
    break;
  case array_of_uchars:
    asize = array_size*sizeof(unsigned char);
    break;
  case array_of_ptrs:
    asize = array_size*sizeof(AtomEntry *);
    break;
  case array_of_dbrefs:
  case array_of_atoms:
    asize = array_size*sizeof(Term);
    break;
  case array_of_terms:
    asize = array_size*sizeof(DBRef);
    break;
  }
  while ((p->ValueOfVE.floats = (Float *) AllocAtomSpace(asize) ) == NULL) {
    YAPLeaveCriticalSection();
    if (!growheap(FALSE)) {
      Error(SYSTEM_ERROR, TermNil, "YAP failed to reserve space in growheap");
      return;
    }
    YAPEnterCriticalSection();
  }
}

/* ae and p are assumed to be locked, if they exist */
static void
CreateStaticArray(AtomEntry *ae, Int dim, static_array_types type, CODEADDR start_addr, StaticArrayEntry *p)
{
  if (EndOfPAEntr(p)) {
    p = (StaticArrayEntry *) AllocAtomSpace(sizeof(*p));
    p->KindOfPE = ArrayProperty;
    p->NextOfPE = ae->PropsOfAE;
    INIT_RWLOCK(p->ArRWLock);
    WRITE_LOCK(p->ArRWLock);
  }
  p->ArrayEArity = -dim;
  p->ArrayType = type;
  ae->PropsOfAE = AbsArrayProp((ArrayEntry *)p);
  WRITE_UNLOCK(ae->ARWLock);
  if (start_addr == NULL) {
    int i;

    AllocateStaticArraySpace(p, type, dim);
    switch(type) {
    case array_of_ints:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.ints[i] = 0;
      break;
    case array_of_chars:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.chars[i] = '\0';
      break;
    case array_of_uchars:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.uchars[i] = '\0';
      break;
    case array_of_doubles:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.floats[i] = 0.0;
      break;
    case array_of_ptrs:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.ptrs[i] = NULL;
      break;
    case array_of_atoms:
    case array_of_dbrefs:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.atoms[i] = 0L;
      break;
    case array_of_terms:
      for (i = 0; i < dim; i++)
	p->ValueOfVE.terms[i] = NULL;
      break;
    }
  } else {
    /* external array */
    p->ValueOfVE.chars = (char *)start_addr;
  }
  WRITE_UNLOCK(p->ArRWLock);
}

static void
ResizeStaticArray(StaticArrayEntry *pp, Int dim)
{
  statarray_elements old_v = pp->ValueOfVE;
  static_array_types type = pp->ArrayType;
  Int old_dim = - pp->ArrayEArity;
  Int mindim = (dim < old_dim ? dim : old_dim), i;

  WRITE_LOCK(pp->ArRWLock);
  /* change official size */
  if (pp->ArrayEArity >= 0)
    return;
  pp->ArrayEArity = -dim;
#if HAVE_MMAP
  if (pp->ValueOfVE.chars < (char *)HeapBase || 
      pp->ValueOfVE.chars > (char *)HeapTop) {
    ResizeMmappedArray(pp, dim, (void *)(pp->ValueOfVE.chars));
    return;
  }
#endif
  AllocateStaticArraySpace(pp, type, dim);
  switch(type) {
  case array_of_ints:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.ints[i] = old_v.ints[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.ints[i] = 0;
    break;
  case array_of_chars:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.chars[i] = old_v.chars[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.chars[i] = '\0';
    break;
  case array_of_uchars:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.uchars[i] = old_v.uchars[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.uchars[i] = '\0';
    break;
  case array_of_doubles:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.floats[i] = old_v.floats[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.floats[i] = 0.0;
    break;
  case array_of_ptrs:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.ptrs[i] = old_v.ptrs[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.ptrs[i] = NULL;
    break;
  case array_of_atoms:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.atoms[i] = old_v.atoms[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.atoms[i] = TermNil;
    break;
  case array_of_dbrefs:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.dbrefs[i] = old_v.dbrefs[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.dbrefs[i] = 0L;
    break;
  case array_of_terms:
    for (i = 0; i <mindim; i++)
      pp->ValueOfVE.terms[i] = old_v.terms[i];
    for (i = mindim; i<dim; i++)
      pp->ValueOfVE.terms[i] = NULL;
    break;
  }
  WRITE_UNLOCK(pp->ArRWLock);
}

CELL * 
ClearNamedArray(CELL *pt0)
{
  /* given a key to an array, just take it off-line */
  PropEntry *pp;
  AtomEntry *ae = (AtomEntry *)RepAppl(pt0[-1]);

  READ_LOCK(ae->ARWLock);
  pp = RepProp(ae->PropsOfAE);  
  while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty) {
    pp = RepProp(pp->NextOfPE);
  }
  READ_UNLOCK(ae->ARWLock);
  WRITE_LOCK(((ArrayEntry *)pp)->ArRWLock);
  if (!EndOfPAEntr(pp)) {
    ((ArrayEntry *) pp)->ArrayEArity = 0;
    /* tell backtracking to skip two cells */
    WRITE_UNLOCK(((ArrayEntry *)pp)->ArRWLock);
    return(pt0-2);
  } else {
    WRITE_UNLOCK(((ArrayEntry *)pp)->ArRWLock);
    Error(EXISTENCE_ERROR_ARRAY,TermNil,"clear array");
    return(pt0); /* just make GCC happy */
  }
}

/* create an array (?Name, + Size) */
static Int 
p_create_array(void)
{
  Term ti;
  Term t;
  Int size;

 restart:
  ti = Deref(ARG2);
  t = Deref(ARG1);
  {
    union arith_ret v;
    if (IsIntTerm(ti))
      size = IntOfTerm(ti);
    else if (Eval(ti, &v) == long_int_e)
      size = v.Int;
    else {
      Error(TYPE_ERROR_INTEGER,ti,"create_array");
      return (FALSE);
    }
  }

  if (IsVarTerm(t)) {
    /* Create an anonymous array */
    Functor farray;

    farray = MkFunctor(AtomArray, size);
    if (H+1+size > ASP-1024) {
      if (!gc(2, ENV, P)) {
	Error(SYSTEM_ERROR,TermNil,"YAP could not grow stack in array/2");
	return(FALSE);
      } else {
	if (H+1+size > ASP-1024) {
	  growstack( sizeof(CELL) * (size+1-(H-ASP-1024)));
	}
      }
      goto restart;
    }
    t = AbsAppl(H);
    *H++ = (CELL) farray;
    for (; size >= 0; size--) {
      RESET_VARIABLE(H);
      H++;
    }
    return (unify(t, ARG1));
  }
  else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    PropEntry *pp;

    WRITE_LOCK(ae->ARWLock);
    pp = RepProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepProp(pp->NextOfPE);
    if (EndOfPAEntr(pp)) {
      if (H+1+size > ASP-1024) {
	WRITE_UNLOCK(ae->ARWLock);
	if (!gc(2, ENV, P)) {
	  Error(SYSTEM_ERROR,TermNil,"YAP could not grow stack in array/2");
	  return(FALSE);
	} else
	  goto restart;
      }
      CreateNamedArray(pp, size, ae);
      WRITE_UNLOCK(ae->ARWLock);
      return (TRUE);
    } else {
      ArrayEntry *app = (ArrayEntry *) pp;

      WRITE_UNLOCK(ae->ARWLock);
      if (!IsVarTerm(app->ValueOfVE) || !IsUnboundVar(app->ValueOfVE))
	Error(PERMISSION_ERROR_CREATE_ARRAY,t,"create_array",
	      ae->StrOfAE);
      else {
	if (H+1+size > ASP-1024) {
	  if (!gc(2, ENV, P)) {
	    Error(SYSTEM_ERROR,TermNil,"YAP could not grow stack in array/2");
	    return(FALSE);
	  } else
	    goto restart;
	}
	InitNamedArray(app, size);
	return (TRUE);
      }
    }
  }
  return (FALSE);
}

/* create an array (+Name, + Size, +Props) */
static Int 
p_create_static_array(void)
{
  Term ti = Deref(ARG2);
  Term t = Deref(ARG1);
  Term tprops = Deref(ARG3);
  Int size;
  static_array_types props;

  if (IsVarTerm(ti)) {
    Error(INSTANTIATION_ERROR,ti,"create static array");
    return (FALSE);
  } else if (IsIntTerm(ti))
    size = IntOfTerm(ti);
  else {
    union arith_ret v;
    if (Eval(ti, &v) == long_int_e) {
      size = v.Int;
    }
    else {
      Error(TYPE_ERROR_INTEGER,ti,"create static array");
      return (FALSE);
    }
  }

  if (IsVarTerm(tprops)) {
    Error(INSTANTIATION_ERROR,tprops,"create static array");
    return (FALSE);
  } else if (IsAtomTerm(tprops)) {
    char *atname = RepAtom(AtomOfTerm(tprops))->StrOfAE;
    if (!strcmp(atname, "int"))
      props = array_of_ints;
    else if (!strcmp(atname, "dbref"))
      props = array_of_dbrefs;
    else if (!strcmp(atname, "float"))
      props = array_of_doubles;
    else if (!strcmp(atname, "ptr"))
      props = array_of_ptrs;
    else if (!strcmp(atname, "atom"))
      props = array_of_atoms;
    else if (!strcmp(atname, "byte"))
      props = array_of_chars;
    else if (!strcmp(atname, "unsigned_byte"))
      props = array_of_uchars;
    else if (!strcmp(atname, "term"))
      props = array_of_terms;
    else {
      Error(DOMAIN_ERROR_ARRAY_TYPE,tprops,"create static array");
      return(FALSE);
    }
  } else {
    Error(TYPE_ERROR_ATOM,tprops,"create static array");
    return (FALSE);
  }

  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t,"create static array");
    return (FALSE);
  }
  else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    StaticArrayEntry *pp;
    ArrayEntry *app = (ArrayEntry *) pp;

    WRITE_LOCK(ae->ARWLock);
    pp = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);

    app = (ArrayEntry *) pp;
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      CreateStaticArray(ae, size, props, NULL, pp);
      return (TRUE);
    } else if (ArrayIsDynamic(app)) {
      if (IsVarTerm(app->ValueOfVE) && IsUnboundVar(app->ValueOfVE)) {
	CreateStaticArray(ae, size, props, NULL, pp);
	return (TRUE);
      } else {
	Error(PERMISSION_ERROR_CREATE_ARRAY,t,"cannot create static array over dynamic array");
	return (FALSE);
      }
    } else {
      Error(PERMISSION_ERROR_CREATE_ARRAY,t,"cannot create static array over static array");
      return (FALSE);
    }
  }
  Error(TYPE_ERROR_ATOM,t,"create static array");
  return (FALSE);
}

/* has a static array associated (+Name) */
static Int 
p_has_static_array(void)
{
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    return (FALSE);
  }
  else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    StaticArrayEntry *pp;

    READ_LOCK(ae->ARWLock);
    pp = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      READ_UNLOCK(ae->ARWLock);
      return (FALSE);
    } else {
      READ_UNLOCK(ae->ARWLock);
      return(TRUE);
    }
  } else {
    return (FALSE);
  }
}

/* resize a static array (+Name, + Size, +Props) */
/* does not work for mmap arrays yet */
static Int 
p_resize_static_array(void)
{
  Term ti = Deref(ARG3);
  Term t = Deref(ARG1);
  Int size;

  if (IsVarTerm(ti)) {
    Error(INSTANTIATION_ERROR,ti,"resize a static array");
    return (FALSE);
  } else if (IsIntTerm(ti))
    size = IntOfTerm(ti);
  else {
    union arith_ret v;
    if (Eval(ti, &v) == long_int_e) {
      size = v.Int;
    }
    else {
      Error(TYPE_ERROR_INTEGER,ti,"resize a static array");
      return (FALSE);
    }
  }

  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t,"resize a static array");
    return (FALSE);
  }
  else if (IsAtomTerm(t)) {
    /* resize a named array */
    Atom a = AtomOfTerm(t);
    StaticArrayEntry *pp = RepStaticArrayProp(RepAtom(a)->PropsOfAE);

    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      Error(PERMISSION_ERROR_RESIZE_ARRAY,t,"resize a static array");
      return(FALSE);
    } else {
      Int osize =  - pp->ArrayEArity;
      ResizeStaticArray(pp, size);
      return(unify(ARG2,MkIntegerTerm(osize)));
    }
  } else {
    Error(TYPE_ERROR_ATOM,t,"resize a static array");
    return (FALSE);
  }
}

/* Close a named array (+Name) */
static Int 
p_close_static_array(void)
{
/* does not work for mmap arrays yet */
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t,"close static array");
    return (FALSE);
  }
  else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    PropEntry *pp;

    READ_LOCK(ae->ARWLock);
    pp = RepProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepProp(pp->NextOfPE);
    READ_UNLOCK(ae->ARWLock);
    if (EndOfPAEntr(pp)) {
      return (FALSE);
    } else {
      StaticArrayEntry *ptr = (StaticArrayEntry *)pp;
      if (ptr->ValueOfVE.ints != NULL) {
#if HAVE_MMAP
	if (ptr->ValueOfVE.chars < (char *)HeapBase || 
	    ptr->ValueOfVE.chars > (char *)HeapTop) {
	  return(CloseMmappedArray(ptr, (void *)ptr->ValueOfVE.chars));
	}
#endif
	FreeAtomSpace((char *)(ptr->ValueOfVE.ints));
	ptr->ValueOfVE.ints = NULL;
	ptr->ArrayEArity = 0;
	return(TRUE);
      } else {
	return(FALSE);
      }
    }
  } else {
    Error(TYPE_ERROR_ATOM,t,"close static array");
    return (FALSE);
  }
}

#if HAVE_MMAP

#if HAVE_UNISTD_H
#include <unistd.h>
#endif
#if HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

STATIC_PROTO(void  ResizeMmappedArray, (StaticArrayEntry *,Int ,void *));

/* keep a list of mmaped blocks to synch on exit */

typedef struct MMAP_ARRAY_BLOCK {
  Atom name;
  void *start;
  size_t  size;
  Int items;
  int  fd;
  struct MMAP_ARRAY_BLOCK *next;
} mmap_array_block;

static mmap_array_block *mmap_arrays = NULL;

static Int
CloseMmappedArray(StaticArrayEntry *pp, void *area)
{
  mmap_array_block *ptr = mmap_arrays, *optr = mmap_arrays;

  while (ptr != NULL && ptr->start != area) {
    ptr = ptr->next;
    optr = ptr;
  }
  if (ptr == NULL) {
    Error(SYSTEM_ERROR,ARG1,"close_mmapped_array (array chain incoherent)", strerror(errno));
    return(FALSE);
  }
  if (munmap(ptr->start, ptr->size) == -1) {
      Error(SYSTEM_ERROR,ARG1,"close_mmapped_array (munmap: %s)", strerror(errno));
      return(FALSE);
  }
  optr->next = ptr->next;
  pp->ValueOfVE.ints = NULL;
  pp->ArrayEArity = 0;
  if (close(ptr->fd) < 0) {
    Error(SYSTEM_ERROR,ARG1,"close_mmapped_array (close: %s)", strerror(errno));
    return(FALSE);
  }
  FreeAtomSpace((char *)ptr);
  return(TRUE);
}

static void
ResizeMmappedArray(StaticArrayEntry *pp, Int dim, void *area)
{
  mmap_array_block *ptr = mmap_arrays;
  size_t total_size; 
  while (ptr != NULL && ptr->start != area) {
    ptr = ptr->next;
  }
  if (ptr == NULL)
    return;
  /* This is a very stupid algorithm to change size for an array.

     First, we unmap it, then we actually change the size for the file,
     and last we initialise again
  */
  if (munmap(ptr->start, ptr->size) == -1) {
      Error(SYSTEM_ERROR,ARG1,"resize_mmapped_array (munmap: %s)", strerror(errno));
      return;
  }
  total_size = (ptr->size / ptr->items)*dim;
  if (ftruncate(ptr->fd, total_size) < 0) {
    Error(SYSTEM_ERROR,ARG1,"resize_mmapped_array (ftruncate: %s)", strerror(errno));
    return;
  }
  if (lseek(ptr->fd, total_size-1, SEEK_SET) < 0) {
    Error(SYSTEM_ERROR,ARG1,"resize_mmapped_array (lseek: %s)", strerror(errno));
    return;
  }
  if (write(ptr->fd, "", 1) < 0) {
    Error(SYSTEM_ERROR,ARG1,"resize_mmapped_array (write: %s)", strerror(errno));
    return;
  }
  if ((ptr->start = (void *)mmap(0, (size_t) total_size, PROT_READ | PROT_WRITE, MAP_SHARED, ptr->fd, 0)) == (void *) - 1) {
    Error(SYSTEM_ERROR,ARG1,"resize_mmapped_array (mmap: %s)", strerror(errno));
    return;
  }
  ptr->size = total_size;
  ptr->items = dim;
  pp->ValueOfVE.chars = ptr->start;
}

#endif

/* create an array (+Name, + Size, +Props) */
static Int 
p_create_mmapped_array(void)
{
#ifdef HAVE_MMAP
  Term ti = Deref(ARG2);
  Term t = Deref(ARG1);
  Term tprops = Deref(ARG3);
  Term tfile = Deref(ARG4);
  Int size;
  static_array_types props;
  size_t total_size;
  CODEADDR array_addr;
  int fd;

  if (IsVarTerm(ti)) {
    Error(INSTANTIATION_ERROR,ti,"create_mmapped_array");
    return (FALSE);
  } else if (IsIntTerm(ti))
    size = IntOfTerm(ti);
  else {
    union arith_ret v;
    if (Eval(ti, &v) == long_int_e) {
      size = v.Int;
    }
    else {
      Error(TYPE_ERROR_INTEGER,ti,"create_mmapped_array");
      return (FALSE);
    }
  }

  if (IsVarTerm(tprops)) {
    Error(INSTANTIATION_ERROR,tprops,"create_mmapped_array");
    return (FALSE);
  } else if (IsAtomTerm(tprops)) {
    char *atname = RepAtom(AtomOfTerm(tprops))->StrOfAE;
    if (!strcmp(atname, "int")) {
      props = array_of_ints;
      total_size = size*sizeof(Int);
    } else if (!strcmp(atname, "dbref")) {
      props = array_of_dbrefs;
      total_size = size*sizeof(Int);
    } else if (!strcmp(atname, "float")) {
      props = array_of_doubles;
      total_size = size*sizeof(Float);
    } else if (!strcmp(atname, "ptr")) {
      props = array_of_ptrs;
      total_size = size*sizeof(AtomEntry *);
    } else if (!strcmp(atname, "atom")) {
      props = array_of_atoms;
      total_size = size*sizeof(Term);
    } else if (!strcmp(atname, "byte")) {
      props = array_of_chars;
      total_size = size*sizeof(char);
    } else if (!strcmp(atname, "unsigned_byte")) {
      props = array_of_uchars;
      total_size = size*sizeof(unsigned char);
    } else {
      Error(DOMAIN_ERROR_ARRAY_TYPE,tprops,"create_mmapped_array");
      return(FALSE);
    }
  } else {
    Error(TYPE_ERROR_ATOM,tprops,"create_mmapped_array");
    return (FALSE);
  }

  if (IsVarTerm(tfile)) {
    Error(INSTANTIATION_ERROR,tfile,"create_mmapped_array");
    return (FALSE);
  } else if (IsAtomTerm(tfile)) {
    char *filename = RepAtom(AtomOfTerm(tfile))->StrOfAE;
    

    fd = open(filename, O_RDWR|O_CREAT, S_IRUSR|S_IWUSR);
    if (fd == -1) {
      Error(SYSTEM_ERROR,ARG1,"create_mmapped_array (open: %s)", strerror(errno));
      return(FALSE);
    }
    if (lseek(fd, total_size-1, SEEK_SET) < 0)
      Error(SYSTEM_ERROR,tfile,"create_mmapped_array (lseek: %s)", strerror(errno));
    if (write(fd, "", 1) < 0)
      Error(SYSTEM_ERROR,tfile,"create_mmapped_array (write: %s)", strerror(errno));
    /*
      if (ftruncate(fd, total_size) < 0)
      Error(SYSTEM_ERROR,tfile,"create_mmapped_array");
    */
    if ((array_addr = (CODEADDR)mmap(0, (size_t) total_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) == (CODEADDR) - 1)
      Error(SYSTEM_ERROR,tfile,"create_mmapped_array (mmap: %s)", strerror(errno));
  } else {
    Error(TYPE_ERROR_ATOM,tfile,"create_mmapped_array");
    return (FALSE);
  }

  if (IsVarTerm(t)) {
    Error(INSTANTIATION_ERROR,t,"create_mmapped_array");
    return (FALSE);
  }
  else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    StaticArrayEntry *pp;

    WRITE_LOCK(ae->ARWLock);
    pp = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (!EndOfPAEntr(pp)) {
      WRITE_LOCK(pp->ArRWLock);
    }
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      mmap_array_block *ptr;

      CreateStaticArray(ae, size, props, array_addr, pp);
      ptr = (mmap_array_block *)AllocAtomSpace(sizeof(mmap_array_block));
      ptr->name = AbsAtom(ae);
      ptr->size = total_size;
      ptr->items = size;
      ptr->start = (void *)array_addr;
      ptr->fd = fd;
      ptr->next = mmap_arrays;
      mmap_arrays = ptr;
      return(TRUE);
    } else {
      WRITE_UNLOCK(pp->ArRWLock);
      WRITE_UNLOCK(ae->ARWLock);
      Error(DOMAIN_ERROR_ARRAY_TYPE,t,"create_mmapped_array", ae->StrOfAE);
      return(FALSE);
    }
  } else {
    Error(TYPE_ERROR_ATOM,t,"create_mmapped_array");
    return (FALSE);
  }
#else
  Error(SYSTEM_ERROR,ARG1,"create_mmapped_array (mmap)");
  return (FALSE);
#endif
}

/* This routine verifies whether a complex has variables. */
static void 
replace_array_references_complex(register CELL *pt0,
				 register CELL *pt0_end,
				 register CELL *ptn,
				 Term Var)
{

  register CELL **to_visit = (CELL **) PreAllocCodeSpace();
  CELL **to_visit_base = to_visit;

loop:
  while (pt0 < pt0_end) {
    register CELL d0;

    ++pt0;
    d0 = Derefa(pt0);
    if (IsVarTerm(d0)) {
      *ptn++ = d0;
    }
    else if (IsPairTerm(d0)) {
      /* store the terms to visit */
      *ptn++ = AbsPair(H);
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = ptn;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = TermNil;
#else
      if (pt0 < pt0_end) {
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = ptn;
	to_visit += 3;
      }
#endif
      pt0 = RepPair(d0) - 1;
      pt0_end = RepPair(d0) + 1;
      /* write the head and tail of the list */
      ptn = H;
      H += 2;
    }
    else if (IsApplTerm(d0)) {
      register Functor f;

      f = FunctorOfTerm(d0);
      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
	{
	  *ptn++ = d0;
	  continue;
	}
      }
      *ptn++ = AbsAppl(H);
      /* store the terms to visit */
#ifdef RATIONAL_TREES
      to_visit[0] = pt0;
      to_visit[1] = pt0_end;
      to_visit[2] = ptn;
      to_visit[3] = (CELL *)*pt0;
      to_visit += 4;
      *pt0 = TermNil;
#else
      if (pt0 < pt0_end) {
	to_visit[0] = pt0;
	to_visit[1] = pt0_end;
	to_visit[2] = ptn;
	to_visit += 3;
      }
#endif
      pt0 = RepAppl(d0);
      d0 = ArityOfFunctor(f);
      pt0_end = pt0 + d0;
      /* start writing the compound term */
      ptn = H;
      *ptn++ = (CELL) f;
      H += d0 + 1;
    }
    else {			/* AtomOrInt */
      *ptn++ = d0;
    }
    /* just continue the loop */
  }

  /* Do we still have compound terms to visit */
  if (to_visit > (CELL **) to_visit_base) {
#ifdef RATIONAL_TREES
    to_visit -= 4;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptn = to_visit[2];
    *pt0 = (CELL)to_visit[3];
#else
    to_visit -= 3;
    pt0 = to_visit[0];
    pt0_end = to_visit[1];
    ptn = to_visit[2];
#endif
    goto loop;
  }

  Bind_Global(PtrOfTerm(Var), TermNil);
  ReleasePreAllocCodeSpace((ADDR)to_visit);
}

/*
 * 
 * Given a term t0, build a new term tf of the form ta+tb, where ta is
 * obtained by replacing the array references in t0 by empty
 * variables, and tb is a list of array references and corresponding
 * variables.
 */
static Term 
replace_array_references(Term t0)
{
  Term t;

  t = Deref(t0);
  if (IsVarTerm(t)) {
    /* we found a variable */
    return (MkPairTerm(t, TermNil));
  } else if (IsAtomOrIntTerm(t)) {
    return (MkPairTerm(t, TermNil));
  } else if (IsPairTerm(t)) {
    Term VList = MkVarTerm();
    CELL *h0 = H;

    H += 2;
    replace_array_references_complex(RepPair(t) - 1, RepPair(t) + 1, h0,
				     VList);
    return (MkPairTerm(AbsPair(h0), VList));
  } else {
    Term VList = MkVarTerm();
    CELL *h0 = H;
    Functor f = FunctorOfTerm(t);

    *H++ = (CELL) (f);
    H += ArityOfFunctor(f);
    replace_array_references_complex(RepAppl(t),
				     RepAppl(t) + ArityOfFunctor(FunctorOfTerm(t)), h0 + 1,
				     VList);
    return (MkPairTerm(AbsAppl(h0), VList));
  }
}

static Int 
p_array_references(void)
{
  Term t = replace_array_references(ARG1);
  Term t1 = HeadOfTerm(t);
  Term t2 = TailOfTerm(t);

  return (unify(t1, ARG2) && unify(t2, ARG3));
}

static Int 
p_assign_static(void)
{
  Term t1, t2, t3;
  StaticArrayEntry *ptr;
  Int indx;

  t2 = Deref(ARG2);
  if (IsNonVarTerm(t2)) {
    if (IsIntTerm(t2))
      indx = IntOfTerm(t2);
    else  {
      union arith_ret v;
      if (Eval(t2, &v) == long_int_e) {
	indx = v.Int;
      } else {
	Error(TYPE_ERROR_INTEGER,t2,"update_array");
	return (FALSE);
      }
    }
  } else {
    Error(INSTANTIATION_ERROR,t2,"update_array");
    return (FALSE);
  }
  t3 = Deref(ARG3);

  t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Error(INSTANTIATION_ERROR,t1,"update_array");
    return(FALSE);
  }
  if (!IsAtomTerm(t1)) {
    if (IsApplTerm(t1)) {
      CELL *ptr;
      Functor f = FunctorOfTerm(t1);
      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
	Error(TYPE_ERROR_ARRAY,t1,"update_array");
	return(FALSE);
      }
      if (indx > 0 && indx > ArityOfFunctor(f)) {
	Error(DOMAIN_ERROR_ARRAY_OVERFLOW,t2,"update_array");
	return(FALSE);
      }
      ptr = RepAppl(t1)+indx+1;
#ifdef MULTI_ASSIGNMENT_VARIABLES
      MaBind(ptr, t3);
      return(TRUE);
#else
      Error(SYSTEM_ERROR,t2,"update_array");
      return(FALSE);
#endif
    } else {
      Error(TYPE_ERROR_ATOM,t1,"update_array");
      return(FALSE);
    }
  }
  {
    AtomEntry *ae = RepAtom(AtomOfTerm(t1));

    READ_LOCK(ae->ARWLock);
    ptr =  RepStaticArrayProp(ae->PropsOfAE);    
    while (!EndOfPAEntr(ptr) && ptr->KindOfPE != ArrayProperty)
      ptr = RepStaticArrayProp(ptr->NextOfPE);
    READ_UNLOCK(ae->ARWLock);
  }

  if (EndOfPAEntr(ptr)) {
    Error(EXISTENCE_ERROR_ARRAY,t1,"assign_static %s", RepAtom(AtomOfTerm(t1))->StrOfAE);
    return(FALSE);
  }

  WRITE_LOCK(ptr->ArRWLock);
  if (ArrayIsDynamic((ArrayEntry *)ptr)) {
    ArrayEntry *pp = (ArrayEntry *)ptr;
    CELL *pt;
    if (indx < 0 || indx >= pp->ArrayEArity) {
      Error(DOMAIN_ERROR_ARRAY_OVERFLOW,t2,"assign_static");
      READ_UNLOCK(((ArrayEntry *)ptr)->ArRWLock);
      return(FALSE);
    }
    pt = RepAppl(pp->ValueOfVE) + indx + 1;
    WRITE_UNLOCK(((ArrayEntry *)ptr)->ArRWLock);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    /* the evil deed is to be done now */
    MaBind(pt, t3);
    return(TRUE);
#else
    Error(SYSTEM_ERROR,t2,"update_array");
    return(FALSE);
#endif
  }

  /* a static array */
  if (IsVarTerm(t3)) {
    WRITE_UNLOCK(ptr->ArRWLock);
    Error(INSTANTIATION_ERROR,t3,"assign_static");
    return (FALSE);
  }
   if (indx < 0 || indx >= - ptr->ArrayEArity) {
    WRITE_UNLOCK(ptr->ArRWLock);
    Error(DOMAIN_ERROR_ARRAY_OVERFLOW,t2,"assign_static");
    return(FALSE);
  }
  switch (ptr->ArrayType) {
  case array_of_ints:
    {
      Int i;
      union arith_ret v;
      
      if (IsIntTerm(t3))
	i = IntOfTerm(t3);
      else if (Eval(t3, &v) == long_int_e)
	i = v.Int;
      else {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_INTEGER,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.ints[indx]= i;
    }
    break;

  case array_of_chars:
    {
      Int i;
      union arith_ret v;
      
      if (IsIntTerm(t3))
	i = IntOfTerm(t3);
      else if (Eval(t3, &v) == long_int_e)
	i = v.Int;
      else {
	Error(TYPE_ERROR_INTEGER,t3,"assign_static");
	return (FALSE);
      }
      if (i > 127 || i < -128) {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_BYTE,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.chars[indx]= i;
    }
    break;

  case array_of_uchars:
    {
      Int i;
      union arith_ret v;
      
      if (IsIntTerm(t3))
	i = IntOfTerm(t3);
      else if (Eval(t3, &v) == long_int_e)
	i = v.Int;
      else {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_INTEGER,t3,"assign_static");
	return (FALSE);
      }
      if (i > 255 || i < 0) {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_UBYTE,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.chars[indx]= i;
    }
    break;

  case array_of_doubles:
    {
      Float f;
      union arith_ret v;

      if (IsFloatTerm(t3))
	f = FloatOfTerm(t3);
      else if (Eval(t3, &v) == double_e)
	f = v.dbl;
      else {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_FLOAT,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.floats[indx]= f;
    }
    break;

  case array_of_ptrs:
    {
      Int r;

      if (IsIntegerTerm(t3))
	r = IntegerOfTerm(t3);
      else {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_PTR,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.ptrs[indx]= (AtomEntry *)r;
    }
    break;

  case array_of_atoms:
    {
      if (!IsAtomTerm(t3)) {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_ATOM,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.atoms[indx]= t3;
    }
    break;

  case array_of_dbrefs:
    {
      
      Term t0 = ptr->ValueOfVE.dbrefs[indx];
      
      if (!IsDBRefTerm(t3)) {
	WRITE_UNLOCK(ptr->ArRWLock);
	Error(TYPE_ERROR_DBREF,t3,"assign_static");
	return (FALSE);
      }
      ptr->ValueOfVE.dbrefs[indx]= t3;
      if (t0 != 0L)
	DBRefOfTerm(t0)->NOfRefsTo--;
      DBRefOfTerm(t3)->NOfRefsTo++;
    }
    break;

  case array_of_terms:
    {
      
      DBRef ref = ptr->ValueOfVE.terms[indx];

      if (ref != NULL) {
	ReleaseTermFromDB(ref);
      }
      ptr->ValueOfVE.terms[indx] = StoreTermInDB(3,3);
      if (ptr->ValueOfVE.terms[indx] == NULL){
	WRITE_UNLOCK(ptr->ArRWLock);
	return(FALSE);
      }
    }
    break;
  }
  WRITE_UNLOCK(ptr->ArRWLock);
  return(TRUE);
}

int compile_arrays = FALSE;


static Int 
p_compile_array_refs(void)
{
  compile_arrays = TRUE;
  return (TRUE);
}

static Int 
p_array_refs_compiled(void)
{
  return (compile_arrays);
}

static Int
p_sync_mmapped_arrays(void)
{
#ifdef HAVE_MMAP
  mmap_array_block *ptr = mmap_arrays;
  while (ptr != NULL) {
    msync(ptr->start, ptr->size, MS_SYNC);
    ptr = ptr->next;
  }
#endif
  return(TRUE);
}

void 
InitArrayPreds(void)
{
  InitCPred("$create_array", 2, p_create_array, SyncPredFlag);
  InitCPred("$array_references", 3, p_array_references, SafePredFlag);
  InitCPred("$array_arg", 3, p_array_arg, SafePredFlag);
  InitCPred("static_array", 3, p_create_static_array, SafePredFlag|SyncPredFlag);
  InitCPred("resize_static_array", 3, p_resize_static_array, SafePredFlag|SyncPredFlag);
  InitCPred("mmapped_array", 4, p_create_mmapped_array, SafePredFlag|SyncPredFlag);
  InitCPred("update_array", 3, p_assign_static, SafePredFlag);
  InitCPred("array_element", 3, p_access_array, 0);
  InitCPred("close_static_array", 1, p_close_static_array, SafePredFlag);
  InitCPred("$sync_mmapped_arrays", 0, p_sync_mmapped_arrays, SafePredFlag);
  InitCPred("$compile_array_refs", 0, p_compile_array_refs, SafePredFlag);
  InitCPred("$array_refs_compiled", 0, p_array_refs_compiled, SafePredFlag);
  InitCPred("$has_static_array", 1, p_has_static_array, TestPredFlag|SafePredFlag);
}

