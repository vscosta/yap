#ifndef PL_YAP_H
#define PL_YAP_H

#ifdef __YAP_PROLOG__

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

#define SIZE_VOIDP SIZEOF_INT_P

#if SIZE_DOUBLE==SIZEOF_INT_P
#define WORDS_PER_DOUBLE 1
#else
#define WORDS_PER_DOUBLE 2
#endif

#if SIZEOF_LONG_INT==4
#define INT64_FORMAT "%lld"
#define INTBITSIZE 32
#else
#define INT64_FORMAT "%ld"
#define INTBITSIZE 64
#endif

typedef uintptr_t		word;		/* Anonymous 4 byte object */
typedef YAP_Term	        Word;		/* Anonymous 4 byte object */
typedef YAP_Atom		Atom;
//move this to SWI

typedef uintptr_t	PL_atomic_t;	/* same a word */


#ifdef __SWI_PROLOG__

/* just to make clear how it would look in SWI */

#define INIT_DEF(Type, Name, Size) \
  static void init_ ## Name (void) {}		\
  static const Type Name[] {

#define ADD_DEF2(Atom, Type) \
  { Atom, Type },
#define ADD_DEF5(Atom, Type, Reverse, Arity, Ctx) \
  { Atom, Type, Reverse, Arity, Ctx },
			     \
#define END_DEFS(Atom, F) \
  { Atom, F }			 
}

#endif

#define INIT_DEF(Type, Name, Size) \
  static Type Name[Size];	     \
  static void init_ ## Name (void) { \
  int i = 0;

#define ADD_DEF2(Atom, Type) \
  char_types[i].name = Atom; \
  char_types[i].test = Type; \
  i++;
#define ADD_DEF5(Atom, Type, Reverse, Arity, Ctx) \
  char_types[i].name = Atom; \
  char_types[i].test = Type; \
  char_types[i].reverse = Reverse; \
  char_types[i].arity = Arity; \
  char_types[i].ctx_type = Ctx; \
  i++;
#define END_DEFS(Atom, F) \
  char_types[i].name = Atom; \
  char_types[i].test = F; \
}

#define ADD_ENCODING(Atom, Type) \
  encoding_names[i].code = Atom;	 \
  encoding_names[i].name = Type; \
  i++;
#define END_ENCODINGS(Atom, F) \
  encoding_names[i].code = Atom; \
  encoding_names[i].name = F; \
}

#define ADD_OPEN4_OPT(Atom, Type) \
  open4_options[i].name = Atom;	 \
  open4_options[i].type = Type; \
  i++;
#define END_OPEN4_DEFS(Atom, F) \
  open4_options[i].name = Atom; \
  open4_options[i].type = F; \
}

#define ADD_CLOSE2_OPT(Atom, Type) \
  close2_options[i].name = Atom;	 \
  close2_options[i].type = Type; \
  i++;
#define END_CLOSE2_DEFS(Atom, F) \
  close2_options[i].name = Atom; \
  close2_options[i].type = F; \
}

#define ADD_SPROP(F1, F2) \
  sprop_list[i].functor = F1;	 \
  sprop_list[i].function = F2; \
  i++;
#define END_SPROP_DEFS(F1, F2) \
  sprop_list[i].functor = F1;	 \
  sprop_list[i].function = F2; \
}

#define ADD_STDSTREAM(Atom) \
  standardStreams[i] = Atom;	 \
  i++;
#define END_STDSTREAMS(Atom) \
  standardStreams[i] = Atom; \
}


#define MK_ATOM(X) ((atom_t)YAP_LookupAtom(X))
#define MKFUNCTOR(X,Y) ((functor_t)YAP_MkFunctor((YAP_Atom)(X),Y))

/*** memory allocation stuff: SWI wraps around malloc  */

#define allocHeap(X) YAP_AllocSpaceFromYap(X)

#define freeHeap(X,Size) YAP_FreeSpaceFromYap(X)


#define stopItimer()

/* TBD */

extern atom_t codeToAtom(int chrcode);

static inline word
INIT_SEQ_CODES(size_t n)
{
  return (word)YAP_OpenList(n);
}

static inline word
EXTEND_SEQ_CODES(word gstore, int c) {
  return (word)YAP_ExtendList((YAP_Term)gstore, YAP_MkIntTerm(c));
}

static inline word
EXTEND_SEQ_ATOMS(word gstore, int c) {
  return (word)YAP_ExtendList((YAP_Term)gstore, codeToAtom(c));
}

static inline int 
CLOSE_SEQ_OF_CODES(word gstore, word lp, word arg2, word arg3) {
  if (arg3 == (word)ATOM_nil) {
    if (!YAP_CloseList((YAP_Term)gstore, YAP_TermNil()))
      return FALSE;
  } else {
    if (!YAP_CloseList((YAP_Term)gstore, YAP_GetFromSlot(arg2)))
      return FALSE;
  }
  return YAP_Unify(YAP_GetFromSlot(arg3), lp);
}

static inline Word
valHandle(term_t tt)
{
  return (word)YAP_GetFromSlot(tt);
}
#define arityFunctor(f) YAP_ArityOfFunctor((YAP_Functor)f)

#define stringAtom(w)	YAP_AtomName((YAP_Atom)(w))
#define isInteger(A) (YAP_IsIntTerm((A)) && YAP_IsBigNumTerm((A)))
#define isString(A) FALSE
#define isAtom(A) YAP_IsAtomTerm((A))
#define isList(A) YAP_IsPairTerm((A))
#define isNil(A) ((A) == YAP_TermNil())
#define isReal(A)YAP_IsFloatTerm((A))
#define isVar(A) YAP_IsVarTerm((A))
#define varName(l, buf) buf
#define valReal(w) YAP_FloatOfTerm((w))
#define AtomLength(w) YAP_AtomNameLength(w)
#define atomValue(atom) ((YAP_Atom)atom)
#define argTermP(w,i) ((Word)((YAP_ArgsOfTerm(w)+(i))))
#define deRef(t) (t = YAP_Deref(t))

#define clearNumber(n)

#endif /* __YAP_PROLOG__ */


static int
stripostfix(const char *s, const char *e)
{ size_t ls = strlen(s);
  size_t le = strlen(e);

  if ( ls >= le )
    return strcasecmp(&s[ls-le], e) == 0;

  return FALSE;
} 


#endif /* PL_YAP_H */
