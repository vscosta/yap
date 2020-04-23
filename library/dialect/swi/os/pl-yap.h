#ifndef PL_YAP_H
#define PL_YAP_H

#ifdef __YAP_PROLOG__

#include "Yatom.h"

#include <libgen.h>

/* depends on tag schema, but 4 should always do */
#define LMASK_BITS 4 /* total # mask bits */

#if HAVE_CTYPE_H
#include <ctype.h>
#endif

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#define SIZE_VOIDP SIZEOF_INT_P

#if SIZEOF_LONG_INT == 4
#define INT64_FORMAT "%lld"
#else
#define INT64_FORMAT "%ld"
#endif
#define INTBITSIZE (sizeof(int) * 8)

typedef module_t Module;
typedef Term (*Func)(term_t); /* foreign functions */

extern const char *Yap_GetCurrentPredName(void);
extern Int Yap_GetCurrentPredArity(void);
extern term_t Yap_fetch_module_for_format(term_t args, Term *modp);
extern void Yap_setCurrentSourceLocation(void *rd);
extern void *Yap_GetStreamHandle(Atom at);
extern void Yap_WriteAtom(IOSTREAM *s, Atom atom);

extern atom_t codeToAtom(int chrcode);

#define valTermRef(t) ((Word)Yap_AddressFromSlot(t))

#include "pl-codelist.h"

// move this to SWI

#define GP_CREATE 2 /* create (in this module) */

#ifndef HAVE_MBSCOLL
COMMON(int) mbscoll(const char *s1, const char *s2);
#endif

#ifndef HAVE_MBSCASECOLL
COMMON(int) mbscasecoll(const char *s1, const char *s2);
#endif

COMMON(atom_t) TemporaryFile(const char *id, int *fdp);
COMMON(char *) Getenv(const char *, char *buf, size_t buflen);

/*** memory allocation stuff: SWI wraps around malloc  */

#define stopItimer()

COMMON(word) pl_print(term_t term);
COMMON(word) pl_write(term_t term);
COMMON(word) pl_write_canonical(term_t term);
COMMON(word) pl_write_term(term_t term, term_t options);
COMMON(word) pl_writeq(term_t term);

static inline int get_procedure(term_t descr, predicate_t *proc, term_t he,
                                int f) {
  CACHE_REGS
  Term t = Yap_GetFromSlot(descr);

  if (IsVarTerm(t))
    return FALSE;
  if (IsAtomTerm(t))
    *proc = RepPredProp(Yap_GetPredPropByAtom(AtomOfTerm(t), CurrentModule));
  else if (IsApplTerm(t)) {
    Functor f = FunctorOfTerm(t);
    if (IsExtensionFunctor(f)) {
      return FALSE;
    }
    *proc = RepPredProp(Yap_GetPredPropByFunc(f, CurrentModule));
  }
  return TRUE;
}

/* TBD */

extern word globalString(size_t size, char *s);
extern word globalWString(size_t size, wchar_t *s);

#define allocHeap(n) allocHeap__LD(n PASS_LD)

#define valHandle(r) valHandle__LD(r PASS_LD)

Int YAP_PLArityOfSWIFunctor(functor_t f);
struct PL_blob_t *YAP_find_blob_type(Atom at);

void PL_license(const char *license, const char *module);

#define arityFunctor(f) YAP_PLArityOfSWIFunctor(f)

#define stringAtom(w) (YAP_AtomFromSWIAtom(w)->StrOfAE)
#define isInteger(A)                                                           \
  (!IsVarTerm(A) && (IsIntegerTerm((A)) || YAP_IsBigNumTerm((A))))
#define isString(A) (!IsVarTerm(A) && IsStringTerm(A))
#define isAtom(A) (!IsVarTerm(A) && IsAtomTerm((A)))
#define isList(A) (!IsVarTerm(A) && IsPairTerm((A)))
#define isNil(A) ((A) == TermNil)
#define isReal(A) (!IsVarTerm(A) && IsFloatTerm((A)))
#define isFloat(A) (!IsVarTerm(A) && IsFloatTerm((A)))
#define isVar(A) IsVarTerm((A))
#define valReal(w) FloatOfTerm((w))
#define valFloat(w) FloatOfTerm((w))
#define atomValue(atom) AtomOfTerm(atom)
#define atomFromTerm(term) YAP_SWIAtomFromAtom(AtomOfTerm(term))

inline static char *atomName(Atom atom) {
  if (IsWideAtom(atom))
    return (char *)(atom->WStrOfAE);
  return atom->StrOfAE;
}

#define nameOfAtom(atom) nameOfAtom(atom)

#define atomBlobType(at) YAP_find_blob_type(at)
#define argTermP(w, i) ((Word)((YAP_ArgsOfTerm(w) + (i))))
#define deRef(t)                                                               \
  while (IsVarTerm(*(t)) && !IsUnboundVar(t)) {                                \
    t = (CELL *)(*(t));                                                        \
  }
#define canBind(t) FALSE // VSC: to implement
#define _PL_predicate(A, B, C, D) PL_predicate(A, B, C)
#define predicateHasClauses(pe) ((pe)->NOfClauses != 0)
#define lookupModule(A) Yap_GetModuleEntry(MkAtomTerm(YAP_AtomFromSWIAtom(A)))

Procedure resolveProcedure(functor_t f, Module module);

#define charEscapeWriteOption(A) FALSE // VSC: to implement
#define wordToTermRef(A) Yap_InitSlot(*(A))
#define isTaggedInt(A) IsIntegerTerm(A)
#define valInt(A) IntegerOfTerm(A)

#define MODULE_user Yap_GetModuleEntry(MkAtomTerm(Yap_LookupAtom("user")))
#define MODULE_system Yap_GetModuleEntry(MkAtomTerm(Yap_LookupAtom("system")))
#define MODULE_parse Yap_GetModuleEntry(LOCAL_SourceModule)

extern term_t Yap_CvtTerm(term_t ts);

#define clearNumber(n)

wchar_t *nameOfWideAtom(atom_t atom);
int isWideAtom(atom_t atom);

inline static int charCode(Term w) {
  if (IsAtomTerm(w)) {
    Atom a = atomValue(w);

    if (IsWideAtom(a)) {
      if (wcslen(a->WStrOfAE) == 1)
        return a->WStrOfAE[0];
      return -1;
    }
    if (strlen(a->StrOfAE) == 1)
      return ((unsigned char *)(a->StrOfAE))[0];
    return -1;
  }
  return -1;
}

#define PL_get_atom(t, a) PL_get_atom__LD(t, a PASS_LD)
#define PL_get_atom_ex(t, a) PL_get_atom_ex__LD(t, a PASS_LD)
#define PL_get_text(l, t, f) PL_get_text__LD(l, t, f PASS_LD)
#define PL_is_atom(t) PL_is_atom__LD(t PASS_LD)
#define PL_is_variable(t) PL_is_variable__LD(t PASS_LD)
#define PL_new_term_ref() PL_new_term_ref__LD(PASS_LD1)
#define PL_put_atom(t, a) PL_put_atom__LD(t, a PASS_LD)
#define PL_put_term(t1, t2) PL_put_term__LD(t1, t2 PASS_LD)
#define PL_unify_atom(t, a) PL_unify_atom__LD(t, a PASS_LD)
#define PL_unify_integer(t, i) PL_unify_integer__LD(t, i PASS_LD)

#define _PL_get_arg(i, t, a) _PL_get_arg__LD(i, t, a PASS_LD);

#endif /* __YAP_PROLOG__ */

unsigned int getUnknownModule(module_t m);

#if IN_PL_OS_C
static int stripostfix(const char *s, const char *e) {
  size_t ls = strlen(s);
  size_t le = strlen(e);

  if (ls >= le)
    return strcasecmp(&s[ls - le], e) == 0;

  return FALSE;
}
#endif

#if HAVE_SIGPROCMASK
#if HAVE_SIGNAL_H
#include <signal.h>
#endif

static inline void unblockSignal(int sig) {
  sigset_t set;

  sigemptyset(&set);
  sigaddset(&set, sig);

  sigprocmask(SIG_UNBLOCK, &set, NULL);
  //  DEBUG(1, Sdprintf("Unblocked signal %d\n", sig));
}
#else
static inline void unblockSignal(int sig) {}
#endif

#define suspendTrace(x)

atom_t ATOM_;

#if THREADS
intptr_t system_thread_id(void);
#endif

#endif /* PL_YAP_H */
