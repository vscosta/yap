/******************************************************************""*******
 *									 *
 *	 YAP Prolog 							 *
 *									 *
 *	Yap Prolog was developed at NCCUP - Universidade do Porto	 *
 *									 *
 * Copyright L.Damas, V.S.Costa and Universidade do Porto 1985-1997	 *
 *									 *
 **************************************************************************
 *									 *
 * File:		arrays.c * Last rev:
 ** mods: * comments:	Array Manipulation Routines *
 *									 *
 *************************************************************************/

/**

@file arrays.c

@namespace prolog

@addtogroup YAPArrays Named Arrays
@ingroup extensions
@{

The YAP system includes experimental support for arrays. The
support is enabled with the option `YAP_ARRAYS`.

There are two very distinct forms of arrays in YAP. The
<em>dynamic arrays</em> are a different way to access compound terms
created during the execution. Like any other terms, any bindings to
these terms and eventually the terms themselves will be destroyed during
backtracking. Our goal in supporting dynamic arrays is twofold. First,
they provide an alternative to the standard arg/3
built-in. Second, because dynamic arrays may have name that are globally
visible, a dynamic array can be visible from any point in the
program. In more detail, the clause

~~~~~
g(X) :- array_element(a,2,X).
~~~~~
will succeed as long as the programmer has used the built-in <tt>array/2</tt>
to create an array term with at least 3 elements in the current
environment, and the array was associated with the name `a`.  The
element `X` is a Prolog term, so one can bind it and any such
bindings will be undone when backtracking. Note that dynamic arrays do
not have a type: each element may be any Prolog term.

The <em>static arrays</em> are an extension of the database. They provide
a compact way for manipulating data-structures formed by characters,
integers, or floats imperatively. They can also be used to provide
two-way communication between YAP and external programs through
shared memory.

In order to efficiently manage space elements in a static array must
have a type. Currently, elements of static arrays in YAP should
have one of the following predefined types:

+ `byte`: an 8-bit signed character.
+ `unsigned_byte`: an 8-bit unsigned character.
+ `int`: Prolog integers. Size would be the natural size for
the machine's architecture.
+ `float`: Prolog floating point number. Size would be equivalent
to a double in `C`.
+ `atom`: a Prolog atom.
+ `dbref`: an internal database reference.
+ `term`: a generic Prolog term. Note that this will term will
not be stored in the array itself, but instead will be stored in the
Prolog internal database.


Arrays may be <em>named</em> or <em>anonymous</em>. Most arrays will be
<em>named</em>, that is associated with an atom that will be used to find
the array. Anonymous arrays do not have a name, and they are only of
interest if the `TERM_EXTENSIONS` compilation flag is enabled. In
this case, the unification and parser are extended to replace
occurrences of Prolog terms of the form `X[I]` by run-time calls to
array_element/3, so that one can use array references instead of
extra calls to arg/3. As an example:

~~~~~
g(X,Y,Z,I,J) :- X[I] is Y[J]+Z[I].
~~~~~
should give the same results as:

~~~~~
G(X,Y,Z,I,J) :-
        array_element(X,I,E1),
        array_element(Y,J,E2),
        array_element(Z,I,E3),
        E1 is E2+E3.
~~~~~

Note that the only limitation on array size are the stack size for
dynamic arrays; and, the heap size for static (not memory mapped)
arrays. Memory mapped arrays are limited by available space in the file
system and in the virtual memory space.

The following predicates manipulate arrays:




*/

#include "Yap.h"
#include "YapEval.h"
#include "Yatom.h"
#include "clause.h"
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

static Int compile_array_refs(USES_REGS1);
static Int array_refs_compiled(USES_REGS1);
static Int sync_mmapped_arrays(USES_REGS1);

/**
 * === Implementation Notes
 *
 * This file works together with pl/arrays.yap and arrays.h.
 *
 * YAP supports a very simple notion of arrays. Arrays may be
 * allocated dynamically or statically:
 *
 * o anonymous arrays are created during execution and allocated
 * in the heap. They have the lifetime of any other other heap
 * object. Any term can be an argument to a dynamic array.
 *
 * Dynamic arrays are named as a free variable and are
 * initialized with free variables.
 *
 * o named arrays are created during execution but allocated
 * in the code space. They have the lifetime of an heap
 * object. Any term can be an argument to a dynamic array.
 *
 * Named arrays are named with atoms and are initialized with
 * free variables.
 *
 * + static arrays are allocated in the heap. Their space is
 * never recovered unless explicitly said so by the
 * program. Arguments to these arrays must have fixed size,
 * and can only be atomic (at least for now).
 *
 * Static arrays can be named through an  atom. They are
 * initialized with [].
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
 * ==== Representation:
 *
 * Dynamic Arrays are represented as a compound term of arity N, where
 * N is the size of the array. Even so, I will not include array bound
 * checking for now.
 *
 * ~~~~
 * |--------------------------------------------------------------|
 * | $ARRAY/N|....
 * |______________________________________________________________
 * ~~~~
 *
 * Unbound Var is used as a place to point to.
 *
 * Static Arrays are represented as a special property for an atom,
 * with field size and
 *
 * A term of the form X[I] is represented as a Reference pointing to
 * the compound term:
 *
 * []([I],X)
 *
 */

static Int create_array(USES_REGS1);
static Int create_mmapped_array(USES_REGS1);
static Int array_references(USES_REGS1);
static Int create_static_array(USES_REGS1);
static Int resize_static_array(USES_REGS1);
static Int close_static_array(USES_REGS1);
static Int access_array(USES_REGS1);
static Int assign_static(USES_REGS1);
static Int assign_dynamic(USES_REGS1);

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

/* keep a list of mmaped blocks to synch on exit */

typedef struct MMAP_ARRAY_BLOCK {
  Atom name;
  void *start;
  size_t size;
  Int items;
  int fd;
  struct MMAP_ARRAY_BLOCK *next;
} mmap_array_block;

static Int CloseMmappedArray(StaticArrayEntry *pp, void *area USES_REGS) {
  mmap_array_block *ptr = GLOBAL_mmap_arrays, *optr = GLOBAL_mmap_arrays;

  while (ptr != NULL && ptr->start != area) {
    ptr = ptr->next;
    optr = ptr;
  }
  if (ptr == NULL) {
#if !defined(USE_SYSTEM_MALLOC)
    Yap_FullError(SYSTEM_ERROR_INTERNAL, ARG1,
                  "close_mmapped_array (array chain incoherent)",
                  strerror(errno));
#endif
    return FALSE;
  }
  if (munmap(ptr->start, ptr->size) == -1) {
    Yap_FullError(SYSTEM_ERROR_INTERNAL, ARG1,
                  "close_mmapped_array (munmap: %s)", strerror(errno));
    return (FALSE);
  }
  optr->next = ptr->next;
  pp->ValueOfVE.ints = NULL;
  pp->ArrayEArity = 0;
  if (close(ptr->fd) < 0) {
    Yap_FullError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1,
                  "close_mmapped_array (close: %s)", strerror(errno));
    return (FALSE);
  }
  Yap_FreeAtomSpace((char *)ptr);
  return (TRUE);
}

static void ResizeMmappedArray(StaticArrayEntry *pp, Int dim,
                               void *area USES_REGS) {
  mmap_array_block *ptr = GLOBAL_mmap_arrays;
  size_t total_size;
  while (ptr != NULL && ptr->start != area) {
    ptr = ptr->next;
  }
  if (ptr == NULL)
    return;
  /* This is a very stupid algorithm to change size for an array.

     First, we unmap it, then we actually change the size for the file,
     and last we initialize again
  */
  if (munmap(ptr->start, ptr->size) == -1) {
    Yap_FullError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1,
                  "resize_mmapped_array (munmap: %s)", strerror(errno));
    return;
  }
  total_size = (ptr->size / ptr->items) * dim;
  if (ftruncate(ptr->fd, total_size) < 0) {
    Yap_FullError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1,
                  "resize_mmapped_array (ftruncate: %s)", strerror(errno));
    return;
  }
  if (lseek(ptr->fd, total_size - 1, SEEK_SET) < 0) {
    Yap_Error(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1,
              "resize_mmapped_array (lseek: %s)", strerror(errno));
    return;
  }
  if (write(ptr->fd, "", 1) < 0) {
    Yap_FullError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1,
                  "resize_mmapped_array (write: %s)", strerror(errno));
    return;
  }
  if ((ptr->start = (void *)mmap(0, (size_t)total_size, PROT_READ | PROT_WRITE,
                                 MAP_SHARED, ptr->fd, 0)) == (void *)-1) {
    Yap_FullError(SYSTEM_ERROR_OPERATING_SYSTEM, ARG1,
                  "resize_mmapped_array (mmap: %s)", ___LINE__, __FUNCTION__,
                  -__FILE__, strerror(errno));
    return;
  }
  ptr->size = total_size;
  ptr->items = dim;
  pp->ValueOfVE.chars = ptr->start;
}

#endif

static Term GetTermFromArray(DBTerm *ref USES_REGS) {
  if (ref != NULL) {
    Term TRef;

    while ((TRef = Yap_FetchTermFromDB(ref)) == 0L) {
      if (!Yap_gcl(LOCAL_Error_Size, 3, ENV, Yap_gcP())) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return 0;
      }
    }
    return TRef;
  } else {
    Yap_Error(DOMAIN_ERROR_NOT_ZERO, ARG1, "Null reference.");
    return 0;
  }
}

static Term GetNBTerm(live_term *ar, Int indx USES_REGS) {
  /* The object is now in use */
  Term livet = ar[indx].tlive;

  if (!IsVarTerm(livet)) {
    if (!IsApplTerm(livet)) {
      return livet;
    } else if (FunctorOfTerm(livet) == FunctorAtFoundOne) {
      return Yap_ReadTimedVar(livet);
    } else {
      return livet;
    }
  } else {
    Term termt = ar[indx].tstore;

    if (!IsUnboundVar(&(ar[indx].tlive))) {
      return livet;
    }
    if (IsVarTerm(termt)) {
      livet = MkVarTerm();
    } else if (IsAtomicTerm(termt)) {
      livet = termt;
    } else {
      DBTerm *ref = (DBTerm *)RepAppl(termt);
      if ((livet = GetTermFromArray(ref PASS_REGS)) == 0) {
        return 0;
      }
    }
    YapBind(&(ar[indx].tlive), livet);
    return livet;
  }
}

static ArrayEntry *GetArrayEntry(Atom at, int owner) {
  CACHE_REGS
  ArrayEntry *pp;
  AtomEntry *ae = RepAtom(at);

  READ_LOCK(ae->ARWLock);
  pp = RepArrayProp(ae->PropsOfAE);
  while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty
#if THREADS
         && pp->owner_id != worker_id
#endif
  )
    pp = RepArrayProp(pp->NextOfPE);
  READ_UNLOCK(ae->ARWLock);
  return pp;
}

static Term AccessNamedArray(Atom a, Int indx USES_REGS) {
  ArrayEntry *pp;
  AtomEntry *ae = RepAtom(a);

  pp = GetArrayEntry(ae, worker_id);

  if (!EndOfPAEntr(pp)) {
    if (ArrayIsDynamic(pp)) {
      Term out;
      READ_LOCK(pp->ArRWLock);
      if (IsVarTerm(pp->ValueOfVE)) {
        READ_UNLOCK(pp->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, ARG1, "unbound static array", indx);
      }
      if (pp->ArrayEArity <= indx || indx < 0) {
        READ_UNLOCK(pp->ArRWLock);
        Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, ARG1, "bad index %ld", indx);
      }
      out = RepAppl(pp->ValueOfVE)[indx + 1];
      READ_UNLOCK(pp->ArRWLock);
      return (out);
    } else {
      StaticArrayEntry *ptr = (StaticArrayEntry *)pp;

      READ_LOCK(ptr->ArRWLock);
      if (pp->ArrayEArity <= indx || indx < 0) {
        Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, ARG1, "bad index %ld", indx);
      }
      switch (ptr->ArrayType) {

      case array_of_ints: {
        Term out;
        out = MkIntegerTerm(ptr->ValueOfVE.ints[indx]);
        READ_UNLOCK(ptr->ArRWLock);
        return out;
      }
      case array_of_doubles: {
        Term out;
        out = MkEvalFl(ptr->ValueOfVE.floats[indx]);
        READ_UNLOCK(ptr->ArRWLock);
        return out;
      }
      case array_of_ptrs: {
        Term out;
        out = MkIntegerTerm((Int)(ptr->ValueOfVE.ptrs[indx]));
        READ_UNLOCK(ptr->ArRWLock);
        return out;
      }
      case array_of_atoms: {
        Term out;
        out = ptr->ValueOfVE.atoms[indx];
        READ_UNLOCK(ptr->ArRWLock);
        if (out == 0L)
          return TermNil;
        else
          return out;
      }
      /* just return the atom */
      case array_of_chars: {
        Term out;
        out = MkIntegerTerm((Int)(ptr->ValueOfVE.chars[indx]));
        READ_UNLOCK(ptr->ArRWLock);
        return out;
      }
      case array_of_uchars: {
        Term out;
        out = MkIntegerTerm((Int)(ptr->ValueOfVE.uchars[indx]));
        READ_UNLOCK(ptr->ArRWLock);
        return out;
      }
      case array_of_dbrefs: {
        /* The object is now in use */
        Term TRef = ptr->ValueOfVE.dbrefs[indx];

        READ_UNLOCK(ptr->ArRWLock);
        if (TRef != 0L) {
          DBRef ref = DBRefOfTerm(TRef);

#if MULTIPLE_STACKS
          LOCK(ref->lock);
          INC_DBREF_COUNT(ref);
          TRAIL_REF(ref); /* So that fail will erase it */
          UNLOCK(ref->lock);
#else
          if (ref->Flags & LogUpdMask) {
            LogUpdClause *cl = (LogUpdClause *)ref;

            if (!(cl->ClFlags & InUseMask)) {
              cl->ClFlags |= InUseMask;
              TRAIL_CLREF(cl);
            }
          } else {
            if (!(ref->Flags & InUseMask)) {
              ref->Flags |= InUseMask;
              TRAIL_REF(ref); /* So that fail will erase it */
            }
          }
#endif
        } else {
          P = (yamop *)FAILCODE;
          TRef = TermNil;
        }
        return TRef;
      }
      case array_of_nb_terms: {
        /* The object is now in use */
        Term out = GetNBTerm(ptr->ValueOfVE.lterms, indx PASS_REGS);
        READ_UNLOCK(ptr->ArRWLock);
        if (out == 0)
          return TermNil;
      }
      case array_of_terms: {
        /* The object is now in use */
        DBTerm *ref = ptr->ValueOfVE.terms[indx];

        READ_UNLOCK(ptr->ArRWLock);
        return GetTermFromArray(ref PASS_REGS);
      }
      default:
        READ_UNLOCK(ptr->ArRWLock);
        return TermNil;
      }
    }
  } else {
    Yap_Error(EXISTENCE_ERROR_ARRAY, MkAtomTerm(a), "named array");
    return (TermNil);
  }
}

/** @pred  array_element(+ _Name_, + _Index_, ? _Element_)


Unify  _Element_ with  _Name_[ _Index_]. It works for both
static and dynamic arrays, but it is read-only for static arrays, while
it can be used to unify with an element of a dynamic array.


*/

/// @memberof array_element/3
static Int access_array(USES_REGS1) {
  Term t = Deref(ARG1);
  Term ti = Deref(ARG2);
  Term tf;
  Int indx;

  if (IsNonVarTerm(ti)) {
    Term nti;
    if (IsIntegerTerm(nti = Yap_Eval(ti)))
      indx = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, ti, "access_array");
      return (FALSE);
    }
  } else {
    Yap_Error(INSTANTIATION_ERROR, ti, "access_array");
    return (TermNil);
  }

  if (IsNonVarTerm(t)) {
    if (IsApplTerm(t)) {
      if (indx >= ArityOfFunctor(FunctorOfTerm(t)) || indx < 0) {
        /*	Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, MkIntegerTerm(indx),
         * "access_array");*/
        P = (yamop *)FAILCODE;
        return (FALSE);
      }
      tf = (RepAppl(t))[indx + 1];
    } else if (IsAtomTerm(t)) {
      tf = AccessNamedArray(AtomOfTerm(t), indx PASS_REGS);
      if (tf == MkAtomTerm(AtomFoundVar)) {
        return (FALSE);
      }
    } else {
      Yap_Error(TYPE_ERROR_ARRAY, t, "access_array");
      return (FALSE);
    }
  } else {
    Yap_Error(INSTANTIATION_ERROR, t, "access_array");
    return (FALSE);
  }
  return Yap_unify(tf, ARG3);
}

static Int array_arg(USES_REGS1) {
  register Term ti = Deref(ARG3), t;
  register Int indx;

  if (IsNonVarTerm(ti)) {
    Term nti;
    if (IsIntegerTerm(nti = Yap_Eval(ti)))
      indx = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, ti, "access_array");
      return (FALSE);
    }
  } else {
    Yap_Error(INSTANTIATION_ERROR, ti, "array_arg");
    return (FALSE);
  }

  t = Deref(ARG2);
  if (IsNonVarTerm(t)) {
    if (IsApplTerm(t)) {
      return (Yap_unify(((RepAppl(t))[indx + 1]), ARG1));
    } else if (IsAtomTerm(t)) {
      Term tf = AccessNamedArray(AtomOfTerm(t), indx PASS_REGS);
      if (tf == MkAtomTerm(AtomFoundVar)) {
        return (FALSE);
      }
      return (Yap_unify(tf, ARG1));
    } else
      Yap_Error(TYPE_ERROR_ARRAY, t, "array_arg");
  } else
    Yap_Error(INSTANTIATION_ERROR, t, "array_arg");

  return (FALSE);
}

static void InitNamedArray(ArrayEntry *p, Int dim USES_REGS) {
  Term *tp;

  WRITE_LOCK(p->ArRWLock);
  /* Leave a pointer so that we can reclaim array space when
   * we backtrack or when we abort */
  /* place terms in reverse order */
  Bind_Global(&(p->ValueOfVE), AbsAppl(HR));
  tp = HR;
  tp[0] = (CELL)Yap_MkFunctor(AtomArray, dim);
  tp++;
  p->ArrayEArity = dim;
  /* Initialize the array as a set of variables */
  HR = tp + dim;
  for (; tp < HR; tp++) {
    RESET_VARIABLE(tp);
  }
  WRITE_UNLOCK(p->ArRWLock);
}

/* we assume the atom ae is already locked */
static void CreateNamedArray(PropEntry *pp, Int dim, AtomEntry *ae USES_REGS) {
  ArrayEntry *p;

  p = (ArrayEntry *)Yap_AllocAtomSpace(sizeof(*p));
  p->KindOfPE = ArrayProperty;
  p->TypeOfAE = DYNAMIC_ARRAY;
  AddPropToAtom(ae, (PropEntry *)p);
  INIT_RWLOCK(p->ArRWLock);
#if THREADS
  p->owner_id = worker_id;
#endif
  p->NextAE = LOCAL_DynamicArrays;
  LOCAL_DynamicArrays = p;
  InitNamedArray(p, dim PASS_REGS);
}

static void AllocateStaticArraySpace(StaticArrayEntry *p,
                                     static_array_types atype, void *old,
                                     size_t array_size USES_REGS) {
  size_t asize = 0;
  switch (atype) {
  case array_of_doubles:
    asize = array_size * sizeof(Float);
    break;
  case array_of_ints:
    asize = array_size * sizeof(Int);
    break;
  case array_of_chars:
    asize = array_size * sizeof(char);
    break;
  case array_of_uchars:
    asize = array_size * sizeof(unsigned char);
    break;
  case array_of_ptrs:
    asize = array_size * sizeof(AtomEntry *);
    break;
  case array_of_atoms:
  case array_of_terms:
  case array_of_nb_terms:
    asize = array_size * sizeof(live_term);
    break;
  case array_of_dbrefs:
    asize = array_size * sizeof(DBRef);
    break;
  }
  if (old == NULL) {
    while ((p->ValueOfVE.floats = (Float *)Yap_AllocCodeSpace(asize)) == NULL) {
      YAPLeaveCriticalSection();
      if (!Yap_growheap(FALSE, asize, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
        return;
      }
      YAPEnterCriticalSection();
    }
  } else {
    while ((p->ValueOfVE.floats = (Float *)Yap_ReallocCodeSpace(old, asize)) ==
           NULL) {
      YAPLeaveCriticalSection();
      if (!Yap_growheap(FALSE, asize, NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
        return;
      }
      YAPEnterCriticalSection();
    }
  }
}

/* ae and p are assumed to be locked, if they exist */
static StaticArrayEntry *CreateStaticArray(AtomEntry *ae, size_t dim,
                                           static_array_types type,
                                           CODEADDR start_addr,
                                           StaticArrayEntry *p USES_REGS) {
  if (EndOfPAEntr(p)) {
    while ((p = (StaticArrayEntry *)Yap_AllocCodeSpace(sizeof(*p))) == NULL) {
      if (!Yap_growheap(FALSE, sizeof(*p), NULL)) {
        Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
        return NULL;
      }
    }
    p->KindOfPE = ArrayProperty;
    p->ValueOfVE.ints = NULL;
    INIT_RWLOCK(p->ArRWLock);
    AddPropToAtom(ae, (PropEntry *)p);
    p->NextAE = LOCAL_StaticArrays;
    LOCAL_StaticArrays = p;
  }
  WRITE_LOCK(p->ArRWLock);
  p->ArrayEArity = dim;
  p->ArrayType = type;
  p->TypeOfAE = STATIC_ARRAY;
  if (start_addr == NULL) {
    size_t i;
    AllocateStaticArraySpace(p, type, NULL, dim PASS_REGS);
    if (p->ValueOfVE.ints == NULL) {
      WRITE_UNLOCK(p->ArRWLock);
      return p;
    }
    switch (type) {
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
    case array_of_nb_terms:
      for (i = 0; i < dim; i++) {
        RESET_VARIABLE(&(p->ValueOfVE.lterms[i].tlive));
        p->ValueOfVE.lterms[i].tstore = TermNil;
      }
      break;
    }
  } else {
    /* external array */
    p->TypeOfAE |= MMAP_ARRAY;
    p->ValueOfVE.chars = (char *)start_addr;
  }
  WRITE_UNLOCK(p->ArRWLock);
  return p;
}

/* ae and p are assumed to be locked, if they exist */
StaticArrayEntry *Yap_StaticArray(Atom na, size_t dim, static_array_types type,
                                  CODEADDR start_addr, StaticArrayEntry *p) {
  CACHE_REGS
  StaticArrayEntry *e;
  ArrayEntry *e0 = GetArrayEntry(RepAtom(na), worker_id);
  if (e0 && ArrayIsDynamic(e0)) {
    e = NULL;
  } else {
    // initial version for e
    e = RepStaticArrayProp(AbsArrayProp(e0));
  }
  e = CreateStaticArray(RepAtom(na), dim, type, NULL, e PASS_REGS);
  return e;
}

static void ResizeStaticArray(StaticArrayEntry *pp, size_t dim USES_REGS) {
  statarray_elements old_v = pp->ValueOfVE;
  static_array_types type = pp->ArrayType;
  size_t old_dim = pp->ArrayEArity;
  size_t mindim = (dim < old_dim ? dim : old_dim), i;

  /* change official size */
  if (pp->ArrayEArity == 0) {
    return;
  }
  WRITE_LOCK(pp->ArRWLock);
  pp->ArrayEArity = dim;
#if HAVE_MMAP
  if (pp->TypeOfAE & MMAP_ARRAY) {
    ResizeMmappedArray(pp, dim, (void *)(pp->ValueOfVE.chars)PASS_REGS);
    WRITE_UNLOCK(pp->ArRWLock);
    return;
  }
#endif
  AllocateStaticArraySpace(pp, type, old_v.chars, dim PASS_REGS);
  switch (type) {
  case array_of_ints:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.ints[i] = 0;
    break;
  case array_of_chars:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.chars[i] = '\0';
    break;
  case array_of_uchars:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.uchars[i] = '\0';
    break;
  case array_of_doubles:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.floats[i] = 0.0;
    break;
  case array_of_ptrs:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.ptrs[i] = NULL;
    break;
  case array_of_atoms:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.atoms[i] = TermNil;
    break;
  case array_of_dbrefs:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.dbrefs[i] = 0L;
    break;
  case array_of_terms:
    for (i = mindim; i < dim; i++)
      pp->ValueOfVE.terms[i] = NULL;
    break;
  case array_of_nb_terms:
    for (i = mindim; i < dim; i++) {
      RESET_VARIABLE(&(pp->ValueOfVE.lterms[i].tlive));
      pp->ValueOfVE.lterms[i].tstore = TermNil;
    }
    break;
  }
  WRITE_UNLOCK(pp->ArRWLock);
}

static void ClearStaticArray(StaticArrayEntry *pp) {
  statarray_elements old_v = pp->ValueOfVE;
  static_array_types type = pp->ArrayType;
  Int dim = pp->ArrayEArity, i;

  /* change official size */
  if (pp->ArrayEArity == 0) {
    return;
  }
  WRITE_LOCK(pp->ArRWLock);
  switch (type) {
  case array_of_ints:
    memset((void *)pp->ValueOfVE.ints, 0, sizeof(Int) * dim);
    break;
  case array_of_chars:
    memset((void *)pp->ValueOfVE.chars, 0, sizeof(char) * dim);
    break;
  case array_of_uchars:
    memset((void *)pp->ValueOfVE.uchars, 0, sizeof(unsigned char) * dim);
    break;
  case array_of_doubles:
    memset((void *)pp->ValueOfVE.floats, 0, sizeof(double) * dim);
    break;
  case array_of_ptrs:
    memset((void *)pp->ValueOfVE.ptrs, 0, sizeof(void *) * dim);
    break;
  case array_of_atoms:
    for (i = 0; i < dim; i++)
      pp->ValueOfVE.atoms[i] = TermNil;
    break;
  case array_of_dbrefs:
    for (i = 0; i < dim; i++) {
      Term t0 = pp->ValueOfVE.dbrefs[i];
      if (t0 != 0L) {
        DBRef ptr = DBRefOfTerm(t0);

        if (ptr->Flags & LogUpdMask) {
          LogUpdClause *lup = (LogUpdClause *)ptr;
          //	  LOCK(lup->ClLock);
          lup->ClRefCount--;
          if (lup->ClRefCount == 0 && (lup->ClFlags & ErasedMask) &&
              !(lup->ClFlags & InUseMask)) {
            //	    UNLOCK(lup->ClLock);
            Yap_ErLogUpdCl(lup);
          } else {
            //	    UNLOCK(lup->ClLock);
          }
        } else {
          ptr->NOfRefsTo--;
          if (ptr->NOfRefsTo == 0 && (ptr->Flags & ErasedMask) &&
              !(ptr->Flags & InUseMask)) {
            Yap_ErDBE(ptr);
          }
        }
      }
      pp->ValueOfVE.dbrefs[i] = 0L;
    }
    break;
  case array_of_terms:
    for (i = 0; i < dim; i++) {
      DBTerm *ref = pp->ValueOfVE.terms[i];

      if (ref != NULL) {
        Yap_ReleaseTermFromDB(ref);
      }
      pp->ValueOfVE.terms[i] = NULL;
    }
    break;
  case array_of_nb_terms:
    for (i = 0; i < dim; i++) {
      Term told = pp->ValueOfVE.lterms[i].tstore;
      CELL *livep = &(pp->ValueOfVE.lterms[i].tlive);

      RESET_VARIABLE(livep);
      /* recover space */
      if (IsApplTerm(told)) {
        Yap_ReleaseTermFromDB((DBTerm *)RepAppl(told));
      }
      pp->ValueOfVE.lterms[i].tstore = old_v.lterms[i].tstore;
    }
    break;
  }
  WRITE_UNLOCK(pp->ArRWLock);
}

/* create an array (?Name, + Size) */
static Int create_array(USES_REGS1) {
  Term ti;
  Term t;
  Int size;

restart:
  ti = Deref(ARG2);
  t = Deref(ARG1);
  {
    Term nti;
    if (IsVarTerm(ti)) {
      Yap_Error(INSTANTIATION_ERROR, ti, "create_array");
      return (FALSE);
    }
    if (IsIntegerTerm(nti = Yap_Eval(ti)))
      size = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, ti, "create_array");
      return (FALSE);
    }
  }

  if (IsVarTerm(t)) {
    /* Create an anonymous array */
    Functor farray;

    farray = Yap_MkFunctor(AtomArray, size);
    if (HR + 1 + size > ASP - 1024) {
      if (!Yap_gcl((1 + size) * sizeof(CELL), 2, ENV, Yap_gcP())) {
        Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
        return (FALSE);
      } else {
        if (HR + 1 + size > ASP - 1024) {
          if (!Yap_growstack(sizeof(CELL) * (size + 1 - (HR - ASP - 1024)))) {
            Yap_Error(RESOURCE_ERROR_HEAP, TermNil, LOCAL_ErrorMessage);
            return FALSE;
          }
        }
      }
      goto restart;
    }
    t = AbsAppl(HR);
    *HR++ = (CELL)farray;
    for (; size >= 0; size--) {
      RESET_VARIABLE(HR);
      HR++;
    }
    return (Yap_unify(t, ARG1));
  } else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    PropEntry *pp;

    WRITE_LOCK(ae->ARWLock);
    pp = RepProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty
#if THREADS
           && ((ArrayEntry *)pp)->owner_id != worker_id
#endif
    )
      pp = RepProp(pp->NextOfPE);
    if (EndOfPAEntr(pp)) {
      if (HR + 1 + size > ASP - 1024) {
        WRITE_UNLOCK(ae->ARWLock);
        if (!Yap_gcl((1 + size) * sizeof(CELL), 2, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return (FALSE);
        } else
          goto restart;
      }
      CreateNamedArray(pp, size, ae PASS_REGS);
      WRITE_UNLOCK(ae->ARWLock);
      return (TRUE);
    } else {
      ArrayEntry *app = (ArrayEntry *)pp;

      WRITE_UNLOCK(ae->ARWLock);
      if (!IsVarTerm(app->ValueOfVE) || !IsUnboundVar(&app->ValueOfVE)) {
        if (size == app->ArrayEArity)
          return TRUE;
        Yap_Error(PERMISSION_ERROR_CREATE_ARRAY, t, "create_array",
                  ae->StrOfAE);
      } else {
        if (HR + 1 + size > ASP - 1024) {
          if (!Yap_gcl((1 + size) * sizeof(CELL), 2, ENV, gc_P(P, CP))) {
            Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
            return (FALSE);
          } else
            goto restart;
        }
        InitNamedArray(app, size PASS_REGS);
        return (TRUE);
      }
    }
  }
  return (FALSE);
}

#define CREATE_ARRAY_DEFS()                                                    \
  PAR("type", isatom, CREATE_ARRAY_TYPE),                                      \
      PAR("address", filler, CREATE_ARRAY_ADDRESS),                            \
      PAR("int", filler, CREATE_ARRAY_INT),                                    \
      PAR("dbref", filler, CREATE_ARRAY_DBREF),                                \
      PAR("float", filler, CREATE_ARRAY_FLOAT),                                \
      PAR("ptr", filler, CREATE_ARRAY_PTR),                                    \
      PAR("atom", filler, CREATE_ARRAY_ATOM),                                  \
      PAR("char", filler, CREATE_ARRAY_CHAR),                                  \
      PAR("unsigned_char", filler, CREATE_ARRAY_UNSIGNED_CHAR),                      \
      PAR("term", filler, CREATE_ARRAY_TERM),                                  \
      PAR("nb_term", filler, CREATE_ARRAY_NB_TERM)

#define PAR(x, y, z) z

typedef enum create_array_enum_choices {
  CREATE_ARRAY_DEFS()
} create_array_choices_t;

#undef PAR

#define PAR(x, y, z)                                                           \
  { x, y, z }

static const param_t create_array_defs[] = {CREATE_ARRAY_DEFS()};
#undef PAR

/* create an array (+Name, + Size, +Props) */
/** @pred  static_array(+ _Name_, + _Size_, + _Type_)


Create a new static array with name  _Name_. Note that the  _Name_
must be an atom (named array). The  _Size_ must evaluate to an
integer.  The  _Type_ must be bound to one of types mentioned
previously.
*/
static Int create_static_array(USES_REGS1) {
  Term ti = Deref(ARG2);
  Term t = Deref(ARG1);
  Term tprops = Deref(ARG3);
  Int size;
  static_array_types props;
  void *address = NULL;
  

  if (IsVarTerm(ti)) {
    Yap_Error(INSTANTIATION_ERROR, ti, "create static array");
    return (FALSE);
  } else {
    Term nti;

    if (IsIntegerTerm(nti = Yap_Eval(ti)))
      size = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, ti, "create static array");
      return (FALSE);
    }
  }
  xarg *args =
      Yap_ArgListToVector(tprops, create_array_defs, CREATE_ARRAY_NB_TERM,
                          DOMAIN_ERROR_CREATE_ARRAY_OPTION);
  if (args == NULL) {
    if (LOCAL_Error_TYPE != YAP_NO_ERROR) {
      Yap_Error(LOCAL_Error_TYPE, tprops, NULL);
    }
    return false;
  }
  if (args[CREATE_ARRAY_TYPE].used) {
    tprops = args[CREATE_ARRAY_TYPE].tvalue;
    {
      char *atname = (char *)RepAtom(AtomOfTerm(tprops))->StrOfAE;
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
      else if (!strcmp(atname, "char"))
        props = array_of_chars;
      else if (!strcmp(atname, "unsigned_char"))
        props = array_of_uchars;
      else if (!strcmp(atname, "term"))
        props = array_of_terms;
      else if (!strcmp(atname, "nb_term"))
        props = array_of_nb_terms;
    }
  }
  if (args[CREATE_ARRAY_ADDRESS].used) {
    address = AddressOfTerm(args[CREATE_ARRAY_ADDRESS].tvalue);
  }
  if (args[CREATE_ARRAY_INT].used)
    props = array_of_ints;
  if (args[CREATE_ARRAY_DBREF].used)
    props = array_of_dbrefs;
  if (args[CREATE_ARRAY_FLOAT].used)
    props = array_of_doubles;
  if (args[CREATE_ARRAY_PTR].used)
    props = array_of_ptrs;
  if (args[CREATE_ARRAY_ATOM].used)
    props = array_of_atoms;
  if (args[CREATE_ARRAY_CHAR].used)
    props = array_of_chars;
  if (args[CREATE_ARRAY_UNSIGNED_CHAR].used)
    props = array_of_uchars;
  if (args[CREATE_ARRAY_TERM].used)
    props = array_of_terms;
  if (args[CREATE_ARRAY_NB_TERM].used)
    props = array_of_nb_terms;
  /*  if (args[CREATE_ARRAY_MATRIX].used) {
    tprops = args[CREATE_ARRAY_TYPE].tvalue;
    
    if (tprops == TermTrue) {
        in_matrix = true;
	size += sizeof(MP_INT)/sizeof(CELL);
    }
    }
  */
  StaticArrayEntry *pp;
  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "create static array");
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    ArrayEntry *app;

    WRITE_LOCK(ae->ARWLock);
    pp = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);

    app = (ArrayEntry *)pp;
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      pp = CreateStaticArray(ae, size, props, address, pp PASS_REGS);
      if (pp == NULL || pp->ValueOfVE.ints == NULL) {
        return TRUE;
      }
    } else if (ArrayIsDynamic(app)) {
      if (IsVarTerm(app->ValueOfVE) && IsUnboundVar(&app->ValueOfVE)) {
        pp = CreateStaticArray(ae, size, props, NULL, pp PASS_REGS);
      } else {
        Yap_Error(PERMISSION_ERROR_CREATE_ARRAY, t,
                  "cannot create static array over dynamic array");
      }
    } else {
      if (pp->ArrayType != props) {
        Yap_Error(TYPE_ERROR_ATOM, t, "create static array %d/%d %d/%d",
                  pp->ArrayEArity, size, pp->ArrayType, props);
        pp = NULL;
      } else {
        AllocateStaticArraySpace(pp, props, pp->ValueOfVE.ints, size PASS_REGS);
      }
    }
    WRITE_UNLOCK(ae->ARWLock);
    if (!pp) {
      return false;
    }
    return true;
  }
  return false;
}

/// create a new vector in a given name Name. If one exists, destroy prrexisting
/// onr
StaticArrayEntry *Yap_StaticVector(Atom Name, size_t size,
                                   static_array_types props) {
  CACHE_REGS
  AtomEntry *ae = RepAtom(Name);

  WRITE_LOCK(ae->ARWLock);
  StaticArrayEntry *pp =
      RepStaticArrayProp(AbsArrayProp(GetArrayEntry(ae, worker_id)));
  if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
    pp = CreateStaticArray(ae, size, props, NULL, pp PASS_REGS);
    if (pp == NULL || pp->ValueOfVE.ints == NULL) {
      WRITE_UNLOCK(ae->ARWLock);
      return FALSE;
    }
    WRITE_UNLOCK(ae->ARWLock);
    return pp;
  }
  return NULL;
}

/* has a static array associated (+Name) */
static Int static_array_properties(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    return (FALSE);
  } else if (IsAtomTerm(t)) {
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
      static_array_types tp = pp->ArrayType;
      Int dim = pp->ArrayEArity;

      READ_UNLOCK(ae->ARWLock);
      if (dim <= 0 || !Yap_unify(ARG2, MkIntegerTerm(dim)))
        return (FALSE);
      switch (tp) {
      case array_of_ints:
        return (Yap_unify(ARG3, MkAtomTerm(AtomInt)));
      case array_of_dbrefs:
        return (Yap_unify(ARG3, MkAtomTerm(AtomDBref)));
      case array_of_doubles:
        return (Yap_unify(ARG3, MkAtomTerm(AtomFloat)));
      case array_of_ptrs:
        return (Yap_unify(ARG3, TermPointer));
      case array_of_chars:
        return (Yap_unify(ARG3, MkAtomTerm(AtomChar)));
      case array_of_uchars:
        return (Yap_unify(ARG3, MkAtomTerm(AtomUnsignedChar)));
      case array_of_terms:
        return (Yap_unify(ARG3, TermTerm));
      case array_of_nb_terms:
        return (Yap_unify(ARG3, TermTerm));
      case array_of_atoms:
        return (Yap_unify(ARG3, MkAtomTerm(AtomAtom)));
      }
    }
  }
  return (FALSE);
}

/* resize a static array (+Name, + Size, +Props) */
/* does not work for mmap arrays yet */
static Int resize_static_array(USES_REGS1) {
  Term ti = Deref(ARG3);
  Term t = Deref(ARG1);
  Int size;

  if (IsVarTerm(ti)) {
    Yap_Error(INSTANTIATION_ERROR, ti, "resize a static array");
    return (FALSE);
  } else {
    Term nti;

    if (IsIntegerTerm(nti = Yap_Eval(ti)))
      size = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, ti, "resize a static array");
      return (FALSE);
    }
  }

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "resize a static array");
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    /* resize a named array */
    Atom a = AtomOfTerm(t);
    StaticArrayEntry *pp = RepStaticArrayProp(RepAtom(a)->PropsOfAE);

    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      Yap_Error(PERMISSION_ERROR_RESIZE_ARRAY, t, "resize a static array");
      return (FALSE);
    } else {
      size_t osize = pp->ArrayEArity;
      ResizeStaticArray(pp, size PASS_REGS);
      return (Yap_unify(ARG2, MkIntegerTerm(osize)));
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t, "resize a static array");
    return (FALSE);
  }
}

/* resize a static array (+Name, + Size, +Props) */
/* does not work for mmap arrays yet */
/** @pred  reset_static_array(+ _Name_)


Reset static array with name  _Name_ to its initial value.


*/
static Int clear_static_array(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "clear a static array");
    return FALSE;
  } else if (IsAtomTerm(t)) {
    /* resize a named array */
    Atom a = AtomOfTerm(t);
    StaticArrayEntry *pp = RepStaticArrayProp(RepAtom(a)->PropsOfAE);

    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      Yap_Error(PERMISSION_ERROR_RESIZE_ARRAY, t, "clear a static array");
      return FALSE;
    } else {
      ClearStaticArray(pp);
      return TRUE;
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t, "clear a static array");
    return FALSE;
  }
}

/* Close a named array (+Name) */
/** @pred  close_static_array(+ _Name_)


Close an existing static array of name  _Name_. The  _Name_ must
be an atom (named array). Space for the array will be recovered and
further accesses to the array will return an error.


*/
static Int close_static_array(USES_REGS1) {
  /* does not work for mmap arrays yet */
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "close static array");
    return (FALSE);
  } else if (IsAtomTerm(t)) {
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
        Int val =
            CloseMmappedArray(ptr, (void *)ptr->ValueOfVE.chars PASS_REGS);
#if USE_SYSTEM_MALLOC
        if (val) {
#endif
          return (val);
#if USE_SYSTEM_MALLOC
        }
#endif
#endif
        Yap_FreeAtomSpace((char *)(ptr->ValueOfVE.ints));
        ptr->ValueOfVE.ints = NULL;
        ptr->ArrayEArity = 0;
        return (TRUE);
      } else {
        return (FALSE);
      }
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t, "close static array");
    return (FALSE);
  }
}

/** @pred  mmapped_array(+ _Name_, + _Size_, + _Type_, + _File_)


Similar to static_array/3, but the array is memory mapped to file
 _File_. This means that the array is initialized from the file, and
that any changes to the array will also be stored in the file.

This built-in is only available in operating systems that support the
system call `mmap`. Moreover, mmapped arrays do not store generic
terms (type `term`).


*/
static Int create_mmapped_array(USES_REGS1) {
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
    Yap_Error(INSTANTIATION_ERROR, ti, "create_mmapped_array");
    return (FALSE);
  } else {
    Term nti;

    if (IsIntegerTerm(nti = Yap_Eval(ti)))
      size = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, ti, "create_mmapped_array");
      return (FALSE);
    }
  }

  if (IsVarTerm(tprops)) {
    Yap_Error(INSTANTIATION_ERROR, tprops, "create_mmapped_array");
    return (FALSE);
  } else if (IsAtomTerm(tprops)) {
    char *atname = RepAtom(AtomOfTerm(tprops))->StrOfAE;
    if (!strcmp(atname, "int")) {
      props = array_of_ints;
      total_size = size * sizeof(Int);
    } else if (!strcmp(atname, "dbref")) {
      props = array_of_dbrefs;
      total_size = size * sizeof(Int);
    } else if (!strcmp(atname, "float")) {
      props = array_of_doubles;
      total_size = size * sizeof(Float);
    } else if (!strcmp(atname, "ptr")) {
      props = array_of_ptrs;
      total_size = size * sizeof(AtomEntry *);
    } else if (!strcmp(atname, "atom")) {
      props = array_of_atoms;
      total_size = size * sizeof(Term);
    } else if (!strcmp(atname, "char")) {
      props = array_of_chars;
      total_size = size * sizeof(char);
    } else if (!strcmp(atname, "unsigned_char")) {
      props = array_of_uchars;
      total_size = size * sizeof(unsigned char);
    } else {
      Yap_Error(DOMAIN_ERROR_ARRAY_TYPE, tprops, "create_mmapped_array");
      return (FALSE);
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, tprops, "create_mmapped_array");
    return (FALSE);
  }

  if (IsVarTerm(tfile)) {
    Yap_Error(INSTANTIATION_ERROR, tfile, "create_mmapped_array");
    return (FALSE);
  } else if (IsAtomTerm(tfile)) {
    char *filename = RepAtom(AtomOfTerm(tfile))->StrOfAE;

    fd = open(filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
    if (fd == -1) {
      Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1, "create_mmapped_array (open: %s)",
                strerror(errno));
      return (FALSE);
    }
    if (lseek(fd, total_size - 1, SEEK_SET) < 0)
      Yap_Error(SYSTEM_ERROR_INTERNAL, tfile,
                "create_mmapped_array (lseek: %s)", strerror(errno));
    if (write(fd, "", 1) < 0)
      Yap_Error(SYSTEM_ERROR_INTERNAL, tfile,
                "create_mmapped_array (write: %s)", strerror(errno));
    /*
      if (ftruncate(fd, total_size) < 0)
      Yap_Error(SYSTEM_ERROR_INTERNAL,tfile,"create_mmapped_array");
    */
    if ((array_addr =
             (CODEADDR)mmap(0, (size_t)total_size, PROT_READ | PROT_WRITE,
                            MAP_SHARED, fd, 0)) == (CODEADDR)-1)
      Yap_Error(SYSTEM_ERROR_INTERNAL, tfile, "create_mmapped_array (mmap: %s)",
                strerror(errno));
  } else {
    Yap_Error(TYPE_ERROR_ATOM, tfile, "create_mmapped_array");
    return (FALSE);
  }

  if (IsVarTerm(t)) {
    Yap_Error(INSTANTIATION_ERROR, t, "create_mmapped_array");
    return (FALSE);
  } else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    StaticArrayEntry *pp;

    WRITE_LOCK(ae->ARWLock);
    pp = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      mmap_array_block *ptr;

      if (EndOfPAEntr(pp)) {
        WRITE_UNLOCK(ae->ARWLock);
        return FALSE;
      } else {
        WRITE_LOCK(pp->ArRWLock);
      }
      CreateStaticArray(ae, size, props, array_addr, pp PASS_REGS);
      ptr = (mmap_array_block *)Yap_AllocAtomSpace(sizeof(mmap_array_block));
      ptr->name = AbsAtom(ae);
      ptr->size = total_size;
      ptr->items = size;
      ptr->start = (void *)array_addr;
      ptr->fd = fd;
      ptr->next = GLOBAL_mmap_arrays;
      GLOBAL_mmap_arrays = ptr;
      WRITE_UNLOCK(pp->ArRWLock);
      WRITE_UNLOCK(ae->ARWLock);
      return TRUE;
    } else {
      WRITE_UNLOCK(ae->ARWLock);
      Yap_Error(DOMAIN_ERROR_ARRAY_TYPE, t, "create_mmapped_array",
                ae->StrOfAE);
      return (FALSE);
    }
  } else {
    Yap_Error(TYPE_ERROR_ATOM, t, "create_mmapped_array");
    return FALSE;
  }
#else
  Yap_Error(SYSTEM_ERROR_INTERNAL, ARG1, "create_mmapped_array (mmap)");
  return (FALSE);
#endif
}
static Int array_references(USES_REGS1) {
  return false;
  Term t = replace_array_references(ARG1 PASS_REGS);
  Term t1 = HeadOfTerm(t);
  Term t2 = TailOfTerm(t);

  return (Yap_unify(t1, ARG2) && Yap_unify(t2, ARG3));
}

/** @pred  update_array(+ _Name_, + _Index_, ? _Value_)

Attribute value  _Value_ to  _Name_[ _Index_]. Type
restrictions must be respected for static arrays. This operation is
available for dynamic arrays if `MULTI_ASSIGNMENT_VARIABLES` is
enabled (true by default). Backtracking undoes  _update_array/3_ for
dynamic arrays, but not for static arrays.

Note that update_array/3 actually uses `setarg/3` to update
elements of dynamic arrays, and `setarg/3` spends an extra cell for
every update. For intensive operations we suggest it may be less
expensive to unify each element of the array with a mutable terms and
to use the operations on mutable terms.


*/
static Int assign_static(USES_REGS1) {
  Term t1, t2, t3;
  StaticArrayEntry *ptr;
  Int indx;

  t2 = Deref(ARG2);
  if (IsNonVarTerm(t2)) {
    Term nti;

    if (IsIntegerTerm(nti = Yap_Eval(t2)))
      indx = IntegerOfTerm(nti);
    else {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "update_array");
      return (FALSE);
    }
  } else {
    Yap_Error(INSTANTIATION_ERROR, t2, "update_array");
    return (FALSE);
  }
  t3 = Deref(ARG3);

  t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "update_array");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    if (IsApplTerm(t1)) {
      CELL *ptr;
      Functor f = FunctorOfTerm(t1);
      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
        Yap_Error(TYPE_ERROR_ARRAY, t1, "update_array");
        return (FALSE);
      }
      if (indx > 0 && indx > ArityOfFunctor(f)) {
        Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "update_array");
        return (FALSE);
      }
      ptr = RepAppl(t1) + indx + 1;
#ifdef MULTI_ASSIGNMENT_VARIABLES
      MaBind(ptr, t3);
      return (TRUE);
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, t2, "update_array");
      return (FALSE);
#endif
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t1, "update_array");
      return (FALSE);
    }
  }
  {
    AtomEntry *ae = RepAtom(AtomOfTerm(t1));

    READ_LOCK(ae->ARWLock);
    ptr = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(ptr) && ptr->KindOfPE != ArrayProperty)
      ptr = RepStaticArrayProp(ptr->NextOfPE);

    if (EndOfPAEntr(ptr)) {
      READ_UNLOCK(ae->ARWLock);
      Yap_Error(EXISTENCE_ERROR_ARRAY, t1, "assign_static %s",
                RepAtom(AtomOfTerm(t1))->StrOfAE);
      return FALSE;
    }

    if (ArrayIsDynamic((ArrayEntry *)ptr)) {
      ArrayEntry *pp = (ArrayEntry *)ptr;
      CELL *pt;

      WRITE_LOCK(pp->ArRWLock);
      READ_UNLOCK(ae->ARWLock);
      if (indx < 0 || indx >= pp->ArrayEArity) {
        Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "assign_static");
        WRITE_UNLOCK(pp->ArRWLock);
        return FALSE;
      }
      pt = RepAppl(pp->ValueOfVE) + indx + 1;
      WRITE_UNLOCK(pp->ArRWLock);
#ifdef MULTI_ASSIGNMENT_VARIABLES
      /* the evil deed is to be done now */
      MaBind(pt, t3);
      return TRUE;
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, t2, "update_array");
      return FALSE;
#endif
    }

    WRITE_LOCK(ptr->ArRWLock);
    READ_UNLOCK(ae->ARWLock);
    /* a static array */
    if (indx < 0 || indx >= ptr->ArrayEArity) {
      WRITE_UNLOCK(ptr->ArRWLock);
      Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "assign_static");
      return FALSE;
    }
    switch (ptr->ArrayType) {
    case array_of_ints: {
      Int i;
      Term nti;

      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }

      if (IsIntegerTerm(nti = Yap_Eval(t3)))
        i = IntegerOfTerm(nti);
      else {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_INTEGER, t3, "assign_static");
        return (FALSE);
      }
      ptr->ValueOfVE.ints[indx] = i;
    } break;

    case array_of_chars: {
      Int i;
      Term nti;

      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }
      if (IsIntegerTerm(nti = Yap_Eval(t3)))
        i = IntegerOfTerm(nti);
      else {
        Yap_Error(TYPE_ERROR_INTEGER, t3, "assign_static");
        return (FALSE);
      }
      if (i > 127 || i < -128) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_CHAR, t3, "assign_static");
        return FALSE;
      }
      ptr->ValueOfVE.chars[indx] = i;
    } break;

    case array_of_uchars: {
      Int i;
      Term nti;

      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }
      if (IsIntegerTerm(nti = Yap_Eval(t3)))
        i = IntegerOfTerm(nti);
      else {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_INTEGER, t3, "assign_static");
        return FALSE;
      }
      if (i > 255 || i < 0) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_UCHAR, t3, "assign_static");
        return FALSE;
      }
      ptr->ValueOfVE.chars[indx] = i;
    } break;

    case array_of_doubles: {
      Float f;
      Term nti;

      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }
      if (IsFloatTerm(nti = Yap_Eval(t3)))
        f = FloatOfTerm(nti);
      else if (IsIntegerTerm(nti))
        f = IntegerOfTerm(nti);
      else {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_FLOAT, t3, "assign_static");
        return FALSE;
      }
      ptr->ValueOfVE.floats[indx] = f;
    } break;

    case array_of_ptrs: {
      Int r;

      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }
      if (IsIntegerTerm(t3))
        r = IntegerOfTerm(t3);
      else {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_PTR, t3, "assign_static");
        return FALSE;
      }
      ptr->ValueOfVE.ptrs[indx] = (AtomEntry *)r;
    } break;

    case array_of_atoms: {
      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }
      if (!IsAtomTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_ATOM, t3, "assign_static");
        return FALSE;
      }
      ptr->ValueOfVE.atoms[indx] = t3;
    } break;

    case array_of_dbrefs: {

      Term t0 = ptr->ValueOfVE.dbrefs[indx];
      DBRef p = DBRefOfTerm(t3);

      if (IsVarTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(INSTANTIATION_ERROR, t3, "assign_static");
        return FALSE;
      }
      if (!IsDBRefTerm(t3)) {
        WRITE_UNLOCK(ptr->ArRWLock);
        Yap_Error(TYPE_ERROR_DBREF, t3, "assign_static");
        return FALSE;
      }
      ptr->ValueOfVE.dbrefs[indx] = t3;
      if (t0 != 0L) {
        DBRef ptr = DBRefOfTerm(t0);

        if (ptr->Flags & LogUpdMask) {
          LogUpdClause *lup = (LogUpdClause *)ptr;
          //	    LOCK(lup->ClLock);
          lup->ClRefCount--;
          if (lup->ClRefCount == 0 && (lup->ClFlags & ErasedMask) &&
              !(lup->ClFlags & InUseMask)) {
            //	      UNLOCK(lup->ClLock);
            Yap_ErLogUpdCl(lup);
          } else {
            //	      UNLOCK(lup->ClLock);
          }
        } else {
          ptr->NOfRefsTo--;
          if (ptr->NOfRefsTo == 0 && (ptr->Flags & ErasedMask) &&
              !(ptr->Flags & InUseMask)) {
            Yap_ErDBE(ptr);
          }
        }
      }

      if (p->Flags & LogUpdMask) {
        LogUpdClause *lup = (LogUpdClause *)p;
        //	  LOCK(lup->ClLock);
        lup->ClRefCount++;
        //	  UNLOCK(lup->ClLock);
      } else {
        p->NOfRefsTo++;
      }
    } break;

    case array_of_nb_terms:

    {
      Term told = ptr->ValueOfVE.lterms[indx].tstore;

      CELL *livep = &(ptr->ValueOfVE.lterms[indx].tlive);
      RESET_VARIABLE(livep);
      /* recover space */
      if (IsApplTerm(told)) {
        Yap_ReleaseTermFromDB((DBTerm *)RepAppl(told));
      }
      if (IsVarTerm(t3)) {
        RESET_VARIABLE(&(ptr->ValueOfVE.lterms[indx].tstore));
      } else if (IsAtomicTerm(t3)) {
        ptr->ValueOfVE.lterms[indx].tstore = t3;
      } else {
        DBTerm *new = Yap_StoreTermInDB(t3, 3);
        if (!new) {
          WRITE_UNLOCK(ptr->ArRWLock);
          return FALSE;
        }
        ptr->ValueOfVE.lterms[indx].tstore = AbsAppl((CELL *)new);
      }
    } break;

    case array_of_terms: {

      DBTerm *ref = ptr->ValueOfVE.terms[indx];

      if (ref != NULL) {
        Yap_ReleaseTermFromDB(ref);
      }
      ptr->ValueOfVE.terms[indx] = Yap_StoreTermInDB(t3, 3);
      if (ptr->ValueOfVE.terms[indx] == NULL) {
        WRITE_UNLOCK(ptr->ArRWLock);
        return FALSE;
      }
    } break;
    }
    WRITE_UNLOCK(ptr->ArRWLock);
    return TRUE;
  }
}

static Int assign_dynamic(USES_REGS1) {
  Term t1, t2, t3;
  StaticArrayEntry *ptr;
  Int indx;

  t2 = Deref(ARG2);
  if (IsNonVarTerm(t2)) {
    Term nti;
    if (IsIntegerTerm(nti = Yap_Eval(t2))) {
      indx = IntegerOfTerm(nti);
    } else {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "update_array");
      return (FALSE);
    }
  } else {
    Yap_Error(INSTANTIATION_ERROR, t2, "update_array");
    return (FALSE);
  }
  t3 = Deref(ARG3);

  t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "update_array");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    if (IsApplTerm(t1)) {
      CELL *ptr;
      Functor f = FunctorOfTerm(t1);
      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
        Yap_Error(TYPE_ERROR_ARRAY, t1, "update_array");
        return (FALSE);
      }
      if (indx > 0 && indx > ArityOfFunctor(f)) {
        Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "update_array");
        return (FALSE);
      }
      ptr = RepAppl(t1) + indx + 1;
#ifdef MULTI_ASSIGNMENT_VARIABLES
      MaBind(ptr, t3);
      return (TRUE);
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, t2, "update_array");
      return (FALSE);
#endif
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t1, "update_array");
      return (FALSE);
    }
  }
  {
    AtomEntry *ae = RepAtom(AtomOfTerm(t1));

    READ_LOCK(ae->ARWLock);
    ptr = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(ptr) && ptr->KindOfPE != ArrayProperty)
      ptr = RepStaticArrayProp(ptr->NextOfPE);
    READ_UNLOCK(ae->ARWLock);
  }

  if (EndOfPAEntr(ptr)) {
    Yap_Error(EXISTENCE_ERROR_ARRAY, t1, "assign_static %s",
              RepAtom(AtomOfTerm(t1))->StrOfAE);
    return (FALSE);
  }

  if (ArrayIsDynamic((ArrayEntry *)ptr)) {
    ArrayEntry *pp = (ArrayEntry *)ptr;
    CELL *pt;
    WRITE_LOCK(pp->ArRWLock);
    if (indx < 0 || indx >= pp->ArrayEArity) {
      Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "assign_static");
      WRITE_UNLOCK(pp->ArRWLock);
      return (FALSE);
    }
    pt = RepAppl(pp->ValueOfVE) + indx + 1;
    WRITE_UNLOCK(pp->ArRWLock);
#ifdef MULTI_ASSIGNMENT_VARIABLES
    /* the evil deed is to be done now */
    MaBind(pt, t3);
    return TRUE;
#else
    Yap_Error(SYSTEM_ERROR_INTERNAL, t2, "update_array");
    return FALSE;
#endif
  }

  WRITE_LOCK(ptr->ArRWLock);
  /* a static array */
  if (indx < 0 || indx >= ptr->ArrayEArity) {
    WRITE_UNLOCK(ptr->ArRWLock);
    Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "assign_static");
    return FALSE;
  }
  switch (ptr->ArrayType) {
  case array_of_ints:
  case array_of_chars:
  case array_of_uchars:
  case array_of_doubles:
  case array_of_ptrs:
  case array_of_atoms:
  case array_of_dbrefs:
  case array_of_terms:
    WRITE_UNLOCK(ptr->ArRWLock);
    Yap_Error(DOMAIN_ERROR_ARRAY_TYPE, t3, "assign_static");
    return FALSE;

  case array_of_nb_terms:
#ifdef MULTI_ASSIGNMENT_VARIABLES
  {
    Term t = ptr->ValueOfVE.lterms[indx].tlive;
    Functor f;
    /* we have a mutable term there */

    if (IsVarTerm(t) || !IsApplTerm(t) ||
        (f = FunctorOfTerm(t)) != FunctorAtFoundOne) {
      Term tn = Yap_NewTimedVar(t3);
      CELL *sp = RepAppl(tn);
      *sp = (CELL)FunctorAtFoundOne;
      YapBind(&(ptr->ValueOfVE.lterms[indx].tlive), tn);
    } else {
      Yap_UpdateTimedVar(t, t3);
    }
  }
    WRITE_UNLOCK(ptr->ArRWLock);
    return TRUE;
#else
    WRITE_UNLOCK(ptr->ArRWLock);
    Yap_Error(SYSTEM_ERROR_INTERNAL, t2, "update_array");
    return FALSE;
#endif
  }
  WRITE_UNLOCK(ptr->ArRWLock);
  return TRUE;
}

/** @pred  add_to_array_element(+ _Name_, + _Index_, + _Number_, ? _NewValue_)


Add  _Number_  _Name_[ _Index_] and unify  _NewValue_ with
the incremented value. Observe that  _Name_[ _Index_] must be an
number. If  _Name_ is a static array the type of the array must be
`int` or `float`. If the type of the array is `int` you
only may add integers, if it is `float` you may add integers or
floats. If  _Name_ corresponds to a dynamic array the array element
must have been previously bound to a number and `Number` can be
any kind of number.

The `add_to_array_element/3` built-in actually uses
`setarg/3` to update elements of dynamic arrays. For intensive
operations we suggest it may be less expensive to unify each element
of the array with a mutable terms and to use the operations on mutable
terms.




 */

static Int add_to_array_element(USES_REGS1) {
  Term t1, t2, t3;
  StaticArrayEntry *ptr;
  Int indx;

  t2 = Deref(ARG2);
  if (IsNonVarTerm(t2)) {
    Term nti;
    if (IsIntegerTerm(nti = Yap_Eval(t2))) {
      indx = IntegerOfTerm(nti);
    } else {
      Yap_Error(TYPE_ERROR_INTEGER, t2, "add_to_array_element");
      return (FALSE);
    }
  } else {
    Yap_Error(INSTANTIATION_ERROR, t2, "add_to_array_element");
    return (FALSE);
  }

  t1 = Deref(ARG1);
  if (IsVarTerm(t1)) {
    Yap_Error(INSTANTIATION_ERROR, t1, "add_to_array_element");
    return (FALSE);
  }
  t3 = Deref(ARG3);
  if (IsVarTerm(t3)) {
    Yap_Error(INSTANTIATION_ERROR, t3, "add_to_array_element");
    return (FALSE);
  }
  if (!IsAtomTerm(t1)) {
    if (IsApplTerm(t1)) {
      CELL *ptr;
      Functor f = FunctorOfTerm(t1);
      Term ta;

      /* store the terms to visit */
      if (IsExtensionFunctor(f)) {
        Yap_Error(TYPE_ERROR_ARRAY, t1, "add_to_array_element");
        return (FALSE);
      }
      if (indx > 0 && indx > ArityOfFunctor(f)) {
        Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "add_to_array_element");
        return (FALSE);
      }
      ptr = RepAppl(t1) + indx + 1;
      ta = RepAppl(t1)[indx + 1];
      if (IsIntegerTerm(ta)) {
        if (IsIntegerTerm(t3)) {
          ta = MkIntegerTerm(IntegerOfTerm(ta) + IntegerOfTerm(t3));
        } else if (IsFloatTerm(t3)) {
          ta = MkFloatTerm(IntegerOfTerm(ta) + FloatOfTerm(t3));
        } else {
          Yap_Error(TYPE_ERROR_NUMBER, t3, "add_to_array_element");
          return (FALSE);
        }
      } else if (IsFloatTerm(ta)) {
        if (IsFloatTerm(t3)) {
          ta = MkFloatTerm(FloatOfTerm(ta) + IntegerOfTerm(t3));
        } else if (IsFloatTerm(t3)) {
          ta = MkFloatTerm(FloatOfTerm(ta) + FloatOfTerm(t3));
        } else {
          Yap_Error(TYPE_ERROR_NUMBER, t3, "add_to_array_element");
          return (FALSE);
        }
      } else {
        Yap_Error(TYPE_ERROR_NUMBER, ta, "add_to_array_element");
        return (FALSE);
      }
#ifdef MULTI_ASSIGNMENT_VARIABLES
      MaBind(ptr, ta);
      return (Yap_unify(ARG4, ta));
#else
      Yap_Error(SYSTEM_ERROR_INTERNAL, t2, "add_to_array_element");
      return (FALSE);
#endif
    } else {
      Yap_Error(TYPE_ERROR_ATOM, t1, "add_to_array_element");
      return (FALSE);
    }
  }
  {
    AtomEntry *ae = RepAtom(AtomOfTerm(t1));

    READ_LOCK(ae->ARWLock);
    ptr = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(ptr) && ptr->KindOfPE != ArrayProperty)
      ptr = RepStaticArrayProp(ptr->NextOfPE);
    READ_UNLOCK(ae->ARWLock);
  }

  if (EndOfPAEntr(ptr)) {
    Yap_Error(EXISTENCE_ERROR_ARRAY, t1, "add_to_array_element %s",
              RepAtom(AtomOfTerm(t1))->StrOfAE);
    return (FALSE);
  }

  if (ArrayIsDynamic((ArrayEntry *)ptr)) {
    ArrayEntry *pp = (ArrayEntry *)ptr;
    CELL *pt;
    Term ta;

    WRITE_LOCK(pp->ArRWLock);
    if (indx < 0 || indx >= pp->ArrayEArity) {
      Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "add_to_array_element");
      READ_UNLOCK(pp->ArRWLock);
      return FALSE;
    }
    pt = RepAppl(pp->ValueOfVE) + indx + 1;
    ta = RepAppl(pp->ValueOfVE)[indx + 1];
    if (IsIntegerTerm(ta)) {
      if (IsIntegerTerm(t3)) {
        ta = MkIntegerTerm(IntegerOfTerm(ta) + IntegerOfTerm(t3));
      } else if (IsFloatTerm(t3)) {
        ta = MkFloatTerm(IntegerOfTerm(ta) + FloatOfTerm(t3));
      } else {
        WRITE_UNLOCK(pp->ArRWLock);
        Yap_Error(TYPE_ERROR_NUMBER, t3, "add_to_array_element");
        return FALSE;
      }
    } else if (IsFloatTerm(ta)) {
      if (IsFloatTerm(t3)) {
        ta = MkFloatTerm(FloatOfTerm(ta) + IntegerOfTerm(t3));
      } else if (IsFloatTerm(t3)) {
        ta = MkFloatTerm(FloatOfTerm(ta) + FloatOfTerm(t3));
      } else {
        WRITE_UNLOCK(pp->ArRWLock);
        Yap_Error(TYPE_ERROR_NUMBER, t3, "add_to_array_element");
        return FALSE;
      }
    } else {
      WRITE_UNLOCK(pp->ArRWLock);
      Yap_Error(TYPE_ERROR_NUMBER, ta, "add_to_array_element");
      return FALSE;
    }
    /* the evil deed is to be done now */
    MaBind(pt, ta);
    WRITE_UNLOCK(pp->ArRWLock);
    return Yap_unify(ARG4, t3);
  }

  WRITE_LOCK(ptr->ArRWLock);
  /* a static array */
  if (indx < 0 || indx >= ptr->ArrayEArity) {
    WRITE_UNLOCK(ptr->ArRWLock);
    Yap_Error(DOMAIN_ERROR_ARRAY_OVERFLOW, t2, "add_to_array_element");
    return FALSE;
  }
  switch (ptr->ArrayType) {
  case array_of_ints: {
    Int i = ptr->ValueOfVE.ints[indx];
    if (!IsIntegerTerm(t3)) {
      WRITE_UNLOCK(ptr->ArRWLock);
      Yap_Error(TYPE_ERROR_INTEGER, t3, "add_to_array_element");
      return FALSE;
    }
    i += IntegerOfTerm(t3);
    ptr->ValueOfVE.ints[indx] = i;
    WRITE_UNLOCK(ptr->ArRWLock);
    return Yap_unify(ARG4, MkIntegerTerm(i));
  } break;
  case array_of_doubles: {
    Float fl = ptr->ValueOfVE.floats[indx];

    if (IsFloatTerm(t3)) {
      fl += FloatOfTerm(t3);
    } else if (IsIntegerTerm(t3)) {
      fl += IntegerOfTerm(t3);
    } else {
      WRITE_UNLOCK(ptr->ArRWLock);
      Yap_Error(TYPE_ERROR_NUMBER, t3, "add_to_array_element");
      return FALSE;
    }
    ptr->ValueOfVE.floats[indx] = fl;
    WRITE_UNLOCK(ptr->ArRWLock);
    return Yap_unify(ARG4, MkFloatTerm(fl));
  } break;
  default:
    WRITE_UNLOCK(ptr->ArRWLock);
    Yap_Error(TYPE_ERROR_NUMBER, t2, "add_to_array_element");
    return FALSE;
  }
}

static Int compile_array_refs(USES_REGS1) {
  compile_arrays = TRUE;
  return (TRUE);
}

static Int array_refs_compiled(USES_REGS1) { return compile_arrays; }

static Int sync_mmapped_arrays(USES_REGS1) {
#ifdef HAVE_MMAP
  mmap_array_block *ptr = GLOBAL_mmap_arrays;
  while (ptr != NULL) {
    msync(ptr->start, ptr->size, MS_SYNC);
    ptr = ptr->next;
  }
#endif
  return (TRUE);
}

/** @pred  static_array_to_term(? _Name_, ? _Term_)


Convert a static array with name
 _Name_ to a compound term of name  _Name_.

This built-in will silently fail if the there is no static array with
that name.


*/
static Int static_array_to_term(USES_REGS1) {
  Term t = Deref(ARG1);

  if (IsVarTerm(t)) {
    return FALSE;
  } else if (IsAtomTerm(t)) {
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
      static_array_types tp = pp->ArrayType;
      Int dim = pp->ArrayEArity, indx;
      CELL *base;

      while (HR + 1 + dim > ASP - 1024) {
        if (!Yap_gcl((1 + dim) * sizeof(CELL), 2, ENV, gc_P(P, CP))) {
          Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
          return (FALSE);
        } else {
          if (HR + 1 + dim > ASP - 1024) {
            if (!Yap_growstack(sizeof(CELL) * (dim + 1 - (HR - ASP - 1024)))) {
              Yap_Error(RESOURCE_ERROR_STACK, TermNil, LOCAL_ErrorMessage);
              return FALSE;
            }
          }
        }
      }
      READ_LOCK(pp->ArRWLock);
      READ_UNLOCK(ae->ARWLock);
      base = HR;
      *HR++ = (CELL)Yap_MkFunctor(AbsAtom(ae), dim);
      switch (tp) {
      case array_of_ints: {
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          *sptr++ = MkIntegerTerm(pp->ValueOfVE.ints[indx]);
        }
      } break;
      case array_of_dbrefs:
        for (indx = 0; indx < dim; indx++) {
          /* The object is now in use */
          Term TRef = pp->ValueOfVE.dbrefs[indx];

          if (TRef != 0L) {
            DBRef ref = DBRefOfTerm(TRef);
            LOCK(ref->lock);
#if MULTIPLE_STACKS
            INC_DBREF_COUNT(ref);
            TRAIL_REF(ref); /* So that fail will erase it */
#else
            if (!(ref->Flags & InUseMask)) {
              ref->Flags |= InUseMask;
              TRAIL_REF(ref); /* So that fail will erase it */
            }
#endif
            UNLOCK(ref->lock);
          } else {
            TRef = TermNil;
          }
          *HR++ = TRef;
        }
        break;
      case array_of_doubles: {
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          *sptr++ = MkEvalFl(pp->ValueOfVE.floats[indx]);
        }
      } break;
      case array_of_ptrs: {
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          *sptr++ = MkAddressTerm(pp->ValueOfVE.ptrs[indx]);
        }
      } break;
      case array_of_chars: {
        CACHE_REGS
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          *sptr++ = MkIntTerm(pp->ValueOfVE.chars[indx]);
        }
      } break;
      case array_of_uchars: {
        CACHE_REGS
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          *sptr++ = MkIntTerm(pp->ValueOfVE.uchars[indx]);
        }
      } break;
      case array_of_terms: {
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          /* The object is now in use */
          DBTerm *ref = pp->ValueOfVE.terms[indx];

          Term TRef = GetTermFromArray(ref PASS_REGS);

          if (P == FAILCODE) {
            return FALSE;
          }

          *sptr++ = TRef;
        }
      } break;
      case array_of_nb_terms: {
        CELL *sptr = HR;
        HR += dim;
        for (indx = 0; indx < dim; indx++) {
          /* The object is now in use */
          Term To = GetNBTerm(pp->ValueOfVE.lterms, indx PASS_REGS);

          if (P == FAILCODE) {
            return FALSE;
          }

          *sptr++ = To;
        }
      } break;
      case array_of_atoms:
        for (indx = 0; indx < dim; indx++) {
          Term out;
          out = pp->ValueOfVE.atoms[indx];
          if (out == 0L)
            out = TermNil;
          *HR++ = out;
        }
        break;
      }
      READ_UNLOCK(pp->ArRWLock);
      return Yap_unify(AbsAppl(base), ARG2);
    }
  }
  Yap_Error(TYPE_ERROR_ATOM, t, "add_to_array_element");
  return FALSE;
}

/** @pred  static_array_location(+ _Name_, - _Ptr_)


Give the location or memory address for  a static array with name
 _Name_. The result is observed as an integer.
*/
static Int static_array_location(USES_REGS1) {
  Term t = Deref(ARG1);
  Int *ptr;

  if (IsVarTerm(t)) {
    return FALSE;
  } else if (IsAtomTerm(t)) {
    /* Create a named array */
    AtomEntry *ae = RepAtom(AtomOfTerm(t));
    StaticArrayEntry *pp;

    READ_LOCK(ae->ARWLock);
    pp = RepStaticArrayProp(ae->PropsOfAE);
    while (!EndOfPAEntr(pp) && pp->KindOfPE != ArrayProperty)
      pp = RepStaticArrayProp(pp->NextOfPE);
    if (EndOfPAEntr(pp) || pp->ValueOfVE.ints == NULL) {
      READ_UNLOCK(ae->ARWLock);
      return FALSE;
    } else {
      ptr = pp->ValueOfVE.ints;
      READ_UNLOCK(ae->ARWLock);
    }
    return Yap_unify(ARG2, MkAddressTerm(ptr));
  }
  return FALSE;
}

void Yap_InitArrayPreds(void) {
  Yap_InitCPred("$create_array", 2, create_array, SyncPredFlag);
  Yap_InitCPred("$array_references", 3, array_references, SafePredFlag);
  Yap_InitCPred("$array_arg", 3, array_arg, SafePredFlag);
  Yap_InitCPred("static_array", 3, create_static_array,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("resize_static_array", 3, resize_static_array,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("mmapped_array", 4, create_mmapped_array,
                SafePredFlag | SyncPredFlag);
  Yap_InitCPred("update_array", 3, assign_static, SafePredFlag);
  Yap_InitCPred("dynamic_update_array", 3, assign_dynamic, SafePredFlag);
  Yap_InitCPred("add_to_array_element", 4, add_to_array_element, SafePredFlag);
  Yap_InitCPred("array_element", 3, access_array, 0);
  Yap_InitCPred("reset_static_array", 1, clear_static_array, SafePredFlag);
  Yap_InitCPred("close_static_array", 1, close_static_array, SafePredFlag);
  Yap_InitCPred("$sync_mmapped_arrays", 0, sync_mmapped_arrays, SafePredFlag);
  Yap_InitCPred("$compile_array_refs", 0, compile_array_refs, SafePredFlag);
  Yap_InitCPred("$array_refs_compiled", 0, array_refs_compiled, SafePredFlag);
  Yap_InitCPred("$static_array_properties", 3, static_array_properties,
                SafePredFlag);
  Yap_InitCPred("static_array_to_term", 2, static_array_to_term, 0L);
  Yap_InitCPred("static_array_location", 2, static_array_location, 0L);
}

/**
@}
*/
