
#define YAP_CPP_INTERFACE 1

#include <stdlib.h>

#include <gmp.h>

extern "C" {

#include "Yap.h"

#include "Yatom.h"

#include "YapHeap.h"

#include "pl-shared.h"

#include "clause.h"

#include "yapio.h"

#include "Foreign.h"

#include "attvar.h"

#include "SWI-Stream.h"

#include "YapText.h"

#if HAVE_STDARG_H
#include <stdarg.h>
#endif

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#endif

#if _MSC_VER || defined(__MINGW32__) 
#include <windows.h>
#endif

  // taken from yap_structs.h
#include "iopreds.h"

// we cannot consult YapInterface.h, that conflicts with what we declare, though
// it shouldn't
}

class YAPTermHandle;
class YAPAtom;
class YAPFunctor;
class YAPQuery;

class YAPTerm {
  friend class YAPPredicate;
  friend class YAPTermHandle;
  friend class YAPApplTerm;
  friend class YAPPairTerm;
protected:
  Term t;
public:
  YAPTerm() {} // do nothing constructor
  YAPTerm(int i) { CACHE_REGS t = MkIntegerTerm( i ); }
  YAPTerm(int64_t i) { CACHE_REGS t = MkIntegerTerm( i ); }
  YAPTerm(long int i)  { CACHE_REGS t = MkIntegerTerm( i ); }
  YAPTerm(void *ptr)  { CACHE_REGS t = MkIntegerTerm( (Int)ptr ); }
  YAPTerm(Term tn) { t = tn; }
  YAPTerm(char *s) { Term tp ; t = YAP_ReadBuffer(s,&tp); }

  YAP_tag_t  tag();
  YAPTerm  deepCopy();
  bool exactlyEqual(YAPTerm t1);
  bool unify(YAPTerm t1);
  bool unifiable(YAPTerm t1);
  bool variant(YAPTerm t1);
  intptr_t hash(size_t sz, size_t depth, bool variant);
  bool isVar() { return IsVarTerm(t); }
  bool isAtom() { return IsAtomTerm(t); }
  bool isInteger() { return IsIntegerTerm(t); }
  bool isFloat() { return IsFloatTerm(t); }
  bool isCompound() { return !(IsVarTerm(t) || IsNumTerm(t)); }
  bool isAppl() { return IsApplTerm(t); }
  bool isPair() { return IsPairTerm(t); }
  bool isGround() { return Yap_IsGroundTerm(t); }
  bool isList() { return Yap_IsListTerm(t); }
  bool isString() { return IsStringTerm(t); }
};

class YAPVarTerm: private YAPTerm {
public:
  YAPVarTerm(): YAPTerm() { CACHE_REGS t = MkVarTerm(); }
  CELL *getVar() { return VarOfTerm(t); }
  bool unbound() { return IsUnboundVar(VarOfTerm(t)); }
};

class YAPApplTerm: private YAPTerm {
public:
  YAPApplTerm(YAPFunctor f, YAPTerm ts[]);
  YAPApplTerm(YAPFunctor f);
  YAPFunctor getFunctor();
  YAPTerm getArg(unsigned int i);
};

class YAPPairTerm: private YAPTerm {
public:
  YAPPairTerm(YAPTerm hd, YAPTerm tl);
  YAPPairTerm();
  YAPTerm getHead() { return  YAPTerm(HeadOfTerm(t)); }
  YAPTerm getTail() { return  YAPTerm(TailOfTerm(t)); }
};

class YAPAtom {
  friend class YAPPredicate;
  friend class YAPFunctor;
  friend class YAPAtomTerm;
  Atom a;
public:
  YAPAtom( Atom at ) { a = at; }
  YAPAtom( char * s) { a = Yap_LookupAtom( s ); }
  YAPAtom( wchar_t * s) { a = Yap_LookupMaybeWideAtom( s ); }
  YAPAtom( char * s, size_t len) { a = Yap_LookupAtomWithLength( s, len ); }
  YAPAtom( wchar_t * s, size_t len) { a = Yap_LookupMaybeWideAtomWithLength( s, len ); }
  char *name(void);
};

class YAPStringTerm: private YAPTerm {
public:
  YAPStringTerm(char *s) ;
  YAPStringTerm(char *s, size_t len);
  YAPStringTerm(wchar_t *s) ;
  YAPStringTerm(wchar_t *s, size_t len);
  const char *getString() { return StringOfTerm(t); }
};

class YAPAtomTerm: private YAPTerm {
public:
  YAPAtomTerm(YAPAtom a): YAPTerm() { t = MkAtomTerm(a.a); }
  YAPAtomTerm(Atom a): YAPTerm()  { t = MkAtomTerm(a); }
  YAPAtomTerm(char *s) ;
  YAPAtomTerm(char *s, size_t len);
  YAPAtomTerm(wchar_t *s) ;
  YAPAtomTerm(wchar_t *s, size_t len);
  YAPAtom getAtom() { return YAPAtom(AtomOfTerm(t)); }
};

class YAPFunctor {
  friend class YAPApplTerm;
  friend class YAPPredicate;
  Functor f;
public:
  YAPFunctor( char * s, unsigned int arity) { f = Yap_MkFunctor( Yap_LookupAtom( s ), arity ); }
  YAPFunctor( wchar_t * s, unsigned int arity) { f = Yap_MkFunctor( Yap_LookupWideAtom( s ), arity ); }
  YAPFunctor( YAPAtom at, unsigned int arity) { f = Yap_MkFunctor( at.a, arity ); }

  Atom name(void) {
    return NameOfFunctor( f );
  }

  unsigned int arity(void) {
    return ArityOfFunctor( f );
  }
};

class YAPTermHandle {
  long int handle;
public:
  YAPTermHandle(Term t) {   
   CACHE_REGS
   handle = Yap_InitSlot(t PASS_REGS);
 }
  ~YAPTermHandle(void) {   
   CACHE_REGS
     Yap_RecoverSlots(1 PASS_REGS);
 }

  YAPTerm get() {   
   CACHE_REGS
     return new YAPTerm( Yap_GetFromSlot(handle PASS_REGS) );
  }
  
  void set(YAPTerm t) {   
   CACHE_REGS
     Yap_PutInSlot(handle, t.t PASS_REGS);
  }
};
  
class YAPPredicate {
  friend class YAPQuery;
  PredEntry *ap;
public:
  YAPPredicate(PredEntry *pe) {   
    ap = pe;
  }
  YAPPredicate(YAPFunctor f) {   
   CACHE_REGS
   ap = RepPredProp(PredPropByFunc(f.f,CurrentModule));
  }
  YAPPredicate(YAPFunctor f, YAPTerm mod) {   
   CACHE_REGS
   ap = RepPredProp(PredPropByFunc(f.f,mod.t));
  }
  YAPPredicate(YAPAtom at, YAPTerm mod) {   
   CACHE_REGS
   ap = RepPredProp(PredPropByAtom(at.a,mod.t));
  }
  YAPPredicate(YAPAtom at) {   
   CACHE_REGS
   ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
  }
  YAPPredicate(YAPAtom at, unsigned int arity, YAPTerm mod) {   
    CACHE_REGS
    if (arity) {
      Functor f = Yap_MkFunctor(at.a, arity);
      ap = RepPredProp(PredPropByFunc(f,mod.t));
    } else {
      ap = RepPredProp(PredPropByAtom(at.a,mod.t));
    }
  }
  YAPPredicate(YAPAtom at, unsigned int arity) {   
    CACHE_REGS
    if (arity) {
      Functor f = Yap_MkFunctor(at.a, arity);
      ap = RepPredProp(PredPropByFunc(f,CurrentModule));
    } else {
      ap = RepPredProp(PredPropByAtom(at.a,CurrentModule));
    }
  }

  int call(YAPTerm ts[]);
};
 
/// interface to a YAP Query
/// uses an SWI-like status info internally
class YAPQuery: private YAPPredicate {
  int q_open;
  int q_state;
  Term *q_g;
  yamop *q_p, *q_cp;
  jmp_buf q_env;
  int q_flags;
  YAP_dogoalinfo q_h;
  YAPQuery *oq;
public:
  /// full constructor, is given a functor, module, and an array of terms that must hav at least 
  /// the same arity as the functor.
  YAPQuery(YAPFunctor f, YAPTerm mod, YAPTerm t[]);
  YAPQuery(YAPFunctor f, YAPTerm t[]);
  YAPQuery(YAPPredicate p, YAPTerm t[]);
  YAPQuery(YAPTerm t[]);
  int next();
  void cut();
  void close();
};
